'''
Created on Sun 05 12:33:18 2026

Obtención de coeficientes Poisson municipales (Inferencia Bayesiana completa)
Incluye efectos de panel (municipio y fecha) y modelos para IMAE total y sectorial.

@author: emer
'''
#%% librerias

'''
si no tiene libreria instale pip install numpy pandas bambi arviz sklearn
'''
import numpy as np
import pandas as pd
import bambi as bmb
import arviz as az
from sklearn.preprocessing import StandardScaler

# %% cargar
df = pd.read_csv("csv/panel_final.csv")
df['fecha'] = df['fecha'].astype(str)

df['log_imae'] = np.log(df['imae'])
df['log_area'] = np.log(df['area_km2'])
df['interaccion_log']  = df['densid_vial'] * df['log_imae']
df['interaccion_area'] = df['log_area'] * df['log_imae']

for sector, col_imae in [('primario', 'imae_primario'),
                         ('secundario', 'imae_secundario'),
                         ('terciario', 'imae_terciario')]:
    df[f'log_imae_{sector}'] = np.log(df[col_imae])
    df[f'interaccion_log_{sector}'] = df['densid_vial'] * df[f'log_imae_{sector}']
    df[f'interaccion_area_{sector}'] = df['log_area'] * df[f'log_imae_{sector}']

scaler = StandardScaler()
df[['interaccion_log_std', 'interaccion_area_std']] = scaler.fit_transform(
    df[['interaccion_log', 'interaccion_area']]
)
# %% modelado
formula_completa = (
    "luces_nocturnas ~ interaccion_log_std + interaccion_area_std + "
    "(1|fecha) + "
    "(1 + interaccion_log_std + interaccion_area_std | municipio)"
)

modelo = bmb.Model(formula_completa, data=df, family="poisson")
resultado = modelo.fit(
    draws=1000, tune=1000, chains=4, cores=4, random_seed=42,
    target_accept=0.95
)

#diagnostico
summary = az.summary(resultado, var_names=[
    "interaccion_log_std", "interaccion_area_std",
    "1|municipio_sigma", "1|fecha_sigma"
])
print(summary)
print("Divergencias totales:", resultado.sample_stats.diverging.sum().values)

#%%% extraer coeficientes
posterior = resultado.posterior


beta_vial_std = posterior['interaccion_log_std'].values.flatten()
beta_area_std = posterior['interaccion_area_std'].values.flatten()

efecto_vial_std = posterior['interaccion_log_std|municipio']
efecto_area_std = posterior['interaccion_area_std|municipio']

#municipios en orden alfabetico
municipios_ordenados = sorted(df['municipio'].unique())

# Recuperar el scaler usado
std_vial = scaler.scale_[0] #desviación estandar de interaccion_log original
std_area = scaler.scale_[1]

filas = []
for idx, muni in enumerate(municipios_ordenados):
    #desviacion aleatoria para este municipio
    b_vial_std = efecto_vial_std.isel(municipio__factor_dim=idx).values.flatten()
    b_area_std = efecto_area_std.isel(municipio__factor_dim=idx).values.flatten()
    
    #coeficiente total (fijo + aleatorio) en escala estandarizada
    coef_vial_std_total = beta_vial_std + b_vial_std
    coef_area_std_total = beta_area_std + b_area_std
    
    #quitar estandarizacion: coef_original = coef_std / std
    coef_vial_total = coef_vial_std_total / std_vial
    coef_area_total = coef_area_std_total / std_area
    
    filas.append({
        "municipio": muni,
        "coef_vial": coef_vial_total.mean(),
        "se_vial": coef_vial_total.std(),
        "ci_vial_lower": az.hdi(coef_vial_total, 0.95)[0],
        "ci_vial_upper": az.hdi(coef_vial_total, 0.95)[1],
        "coef_area": coef_area_total.mean(),
        "se_area": coef_area_total.std(),
        "ci_area_lower": az.hdi(coef_area_total, 0.95)[0],
        "ci_area_upper": az.hdi(coef_area_total, 0.95)[1]
    })

df_bayes = pd.DataFrame(filas)
df_bayes.to_csv("csv/bayes_mejorado_total.csv", index=False)
print(df_bayes.head())

'''
rhat esta en el umbral de 1.1 esto eso aceptable es complementario al TWFE Poisson 
principal, solo descriptiva y exploratori, el rhat al limite se podria mejorar pero la pc 
me explotaria, ess esta bien y solo 14 divegencias de 4000 iteraciones post-tuning

formila 
luces_nocturnas ~ interaccion_log_std + interaccion_area_std 
                    + (1|fecha) 
                    + (1 + interaccion_log_std + interaccion_area_std | municipio)

es modelo bayesiano jerarquico 
'''