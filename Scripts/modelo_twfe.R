if(F){
  "
  
  pregunta de invetigacion: como condiciona la dotación de infraestructura vial 
  la transmision de los shocks macroeconomicos a las economias locales 
  en Nicaragua, utilizando las luces nocturnas como proxy de actividad?
  
  hipotesis: Los municipios con mayor densidad vial presentan una mayor 
  elasticidad de sus luces nocturnas respecto al IMAE, es decir, reaccionan más
  intensamente a los ciclos macro
  
  Recuerde se el el imae trabajado es tendencia ciclo
  
  interaccion_log = log_densidad_vial * log_imae,
  densidad_vial es km cuadrado pero para carreteras
  interaccion_area = log_area * log_imae
  area es km cuadraro osea el area de la superficie en el municipio
  
  luces nocturnas estan estan medida en unidades fisicas depues verifico si:
  nW, cm cuadrado o sr
  
  en densidad_vial significa por ejemlo un valor promedio de 0.31 en niveles
  que hay 0.31 km carretera transitable
  desidadad_vial = km/km cuadrado
  
  por ejemplo managua tiene un valor de 2, se puede concluir dos cosas
  en promedio hay 2 caminos posibles para llegar al destino y que no existen
  comunidades aisladas, tambien que la longitud lineal de carretera es el doble
  que su area en carretera
  
  interpretacion del interaccion_causal (densidad_vial * imae):
  representa la dosis de ciclo economico nacional promedio a la que esta 
  expuesta el municipio a traves de su carretera, si el imae sube, los 
  municipios con mas carretaras subem de forma exponencial. en otras palabras
  es un indice de exposicion fisica
  
  interaccion_ area_nominal (area * imae):
  mide una escala de superficie expuesta al ciclo economico, representa que
  entra mas grande el municipio es un motor enomico potencialmente mas grande
  
  otras interacciones interpretatcion:
  
  interacion_vial_primario (densidad_vial * imae_primario):
  representa la exposicion de las zonas productoras de materias primas al 
  ciclo agricola nacional a traves del transporte
  
  interacion_vial_secundario (densidad_vial * imae_secundario):
  representa capacidad física de transporte y procesamiento industrial ante un 
  shock de demanda
  
  interacion_vial_terciario (densid_vial * imae_terciario):
  representa flujo de transporte y de consumo de la economía urbana
  
  las 3 interacciones son indices 
  
  
  otra cosa se habia estimado la poblacion pero hacer la interaccion con el 
  imae hay una multicolinealidad severa potenten mejor usar solo 2 variables
  
  "
}


#cargar librerias ---- 
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(fixest,
               tidyr,
               dplyr,
               lmtest,
               usethis,
               ggplot2,
               lme4,
               sf,
               viridis,
               readxl,
               stringr,
               patchwork
               )

# importar datos si no existen en el entorno ----
if (!exists("panel_final")) {
  if (file.exists("csv/panel_final.csv")) {
    panel_final <- readr::read_csv("csv/panel_final.csv") %>%
      dplyr::mutate(fecha = as.Date(fecha))
  } else {
    source("Scripts/obtener_datos.R")
  }
}

#modelo twfe ----

panel_final <- panel_final %>%
  dplyr::mutate(
    log_area = log(area_km2),
    log_luces = log(luces_nocturnas), 
    log_imae = log(imae),
    log_poblacion = log(poblacion_estimada),
    log_densidad_vial = log(densid_vial), 
    interaccion_log = log_densidad_vial * log_imae,
    interaccion_area = log_area * log_imae,
    interaccion_pob = log_poblacion * log_imae
  ) 

#modelo que elimina 19 obs
modelo_twfe_log <- fixest::feols(
  log_luces ~ interaccion_log + interaccion_area | municipio + fecha,
  data = panel_final,
  cluster = ~municipio
)

summary(modelo_twfe_log)

#modelo poisson log-log 
#recuerdase que la variable y poisoon lo hace log igualmente
modelo_twfe_poisson <- fixest::fepois(
  luces_nocturnas ~ interaccion_log + interaccion_area | municipio + fecha,
  data = panel_final,
  cluster = ~municipio
)

summary(modelo_twfe_poisson)

#pisson como poblacion constante
modelo_twfe_poisson_poblacion <- fixest::fepois(
  luces_nocturnas ~ interaccion_log + interaccion_area | municipio + fecha + poblacion_estimada,
  data = panel_final,
  cluster = ~municipio
)
#las constante son lo mismo en realidad es peor el modelo el bic = 13,349
#mas alto que 11,935 es mejor sin poblacion estimada con constante

summary(modelo_twfe_poisson_poblacion)

#poisson pero cambiado area por poblacion
modelo_twfe_poisson_pob <- fixest::fepois(
  luces_nocturnas ~ interaccion_log + interaccion_pob | municipio + fecha,
  data = panel_final,
  cluster = ~municipio
)

summary(modelo_twfe_poisson_pob)

#modelo sin cluster
modelo_twfe_base <- fixest::feols(
  log_luces ~ interaccion_log + interaccion_area | municipio + fecha,
  data = panel_final
)

#tablas comparativas para demostrar robustez
fixest::etable(
  modelo_twfe_poisson, 
  vcov = list(
    "iid", #errores clasicos 
    "hetero", #errores Robustos simples (solo corrige heterocedasticidad tipo White)
    ~municipio #errores clustered (corrige heterocedasticidad y autocorrelacion serial) modelo principal
  ),
  headers = c("clasicos", "robustos (White)", "clustered (Arellano)")
)

#grafico de efectos marginales
panel_grafico <- panel_final %>% 
  dplyr::filter(is.finite(log_densidad_vial))

rango_log_densidad <- seq(min(panel_grafico$log_densidad_vial, na.rm = TRUE), 
                          max(panel_grafico$log_densidad_vial, na.rm = TRUE), 
                          length.out = 100)

beta_interaccion <- stats::coef(modelo_twfe_poisson)["interaccion_log"]
se_interaccion   <- modelo_twfe_poisson$se["interaccion_log"]

df_marginal <- data.frame(
  log_density = rango_log_densidad,
  efecto_marginal = rango_log_densidad * beta_interaccion,
  se_marginal = rango_log_densidad * se_interaccion
) %>%
  dplyr::mutate(
    ci_inf = efecto_marginal - 1.96 * se_marginal,
    ci_sup = efecto_marginal + 1.96 * se_marginal,
    densid_vial_real = exp(log_density)
  )

grafico_marginal <- ggplot(df_marginal, aes(x = densid_vial_real, y = efecto_marginal)) +
  geom_ribbon(aes(ymin = ci_inf, ymax = ci_sup), fill = "#3B82F6", alpha = 0.15) +
  geom_line(color = "#1D4ED8", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#DC2626", linewidth = 0.8) +
  scale_x_log10() + 
  labs(
    title = "A. Efecto Marginal del Shock (Poisson PPML)",
    subtitle = "Elasticidad neta de luces según la Densidad Vial",
    x = "Densidad Vial Real (km/km², Escala Log)",
    y = "Elasticidad estimada (Beta neto)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", color = "#1E293B"),
    plot.subtitle = element_text(size = 8, color = "#64748B"),
    panel.grid.minor = element_blank()
  )

df_municipios_unicos <- panel_grafico %>% 
  dplyr::distinct(municipio, .keep_all = TRUE)

grafico_dispersion <- ggplot(df_municipios_unicos, aes(x = area_km2, y = densid_vial)) +
  geom_point(alpha = 0.6, color = "#10B981", size = 2) +
  geom_smooth(method = "lm", color = "#059669", fill = "#10B981", alpha = 0.1, linewidth = 0.8) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "B. Ortogonalidad Funcional",
    subtitle = "Dispersión Municipal: Área vs. Densidad Vial",
    x = "Área del Municipio (km², Escala Log)",
    y = "Densidad Vial (km de carreteras / km², Escala Log)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", color = "#1E293B"),
    plot.subtitle = element_text(size = 8, color = "#64748B"),
    panel.grid.minor = element_blank()
  )

panel_combinado <- grafico_marginal + grafico_dispersion

print(panel_combinado)
ggsave("Graficos/panel_efectos_y_controles_1x2.pdf", plot = panel_combinado, width = 11, height = 4.5, dpi = 300)


#graficos de residuos vs valores ajustados
residuos_df <- data.frame(
  Ajustados = fitted(modelo_twfe_poisson),
  Residuos = residuals(modelo_twfe_poisson)
)

dist_residuo_grafico <- ggplot(residuos_df, aes(x = Ajustados, y = Residuos)) +
  geom_point(alpha = 0.2, color = "#2563EB") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Distribución de Residuos del Modelo TWFE Poisson",
    # subtitle = "Ausencia de patrones no lineales severos tras controlar por Efectos Fijos",
    x = "Valores Ajustados (Luces)",
    y = "Residuos del Modelo"
  ) 

print(dist_residuo_grafico)
ggsave("Graficos/distribucion_residuos_twfe.pdf", plot = dist_residuo_grafico, width = 9, height = 4, dpi = 300)

if(F){
  "
  
  interpretacion del primer grafico en estaccion:
  el panel a el grafico en todo momento es significativo se ve la tendencia
  del beta con su densidad vial es decir en municipios con infraestructura vial
  casi nula, el efecto neto es positivo entonces ante un shock del 
  imae (ya sea positivo o negativo), la economaa local amplifica el ciclo de 
  forma lineal y descontrolada
  
  hay que ver las sonas donde estan desprotegido y la economia puede mejorar 
  pero un ejemplo rapido el el municipio del sauce tiene un area de 700.26
  km cuadrado y ellos solo tiene una densidadl vial de 0.31 km sobre km
  cuadrado ellos tienen rango para mejorar su economia local teoricamente 
  para mejorar su economia deberia diversificar su area para no que 
  las comunidades aisladas aporten positivamente a su economia *nota:
  hay que hacer un estudio aparte para saber como distrubuir correctamente
  la carretera para que conecta totalmente el area y la densidad vial es decir
  que la densidad vial sea 1
  
  panel b: aqui se ve una linea te tendencia clara osea que se aisla
  el efecto de carretera (desindad vial) y el tamaño geografico (area municipio) 
  tambien entre mas area menos densidad vial 
  
  aqui las bandade de confianza es la incertidumbre de la relacion geometrica
  entre las dos variables geonetricas 
  
  aqui se ve asimetria de area es decir hay areas muy enormes con poca o media densidad
  vial, ademas si el tamaño fuera un motor economico los municipios mas grande deberias
  ser ma iluminados pero no es significativo, el efecto multiplicador economico
  es la densidad y conectividad vial 
  
  interpretacion de distribucion de residuos TWFE
  no le entiendo muy bien hay que ver pero se ve como la mayoria esta en casi 0
  y existe mucho outlier
  
  "
}

#otro modelo para outliers mediante residuos studentizados

#corregir el desajuste de tamaños
panel_model <- panel_final %>%
  dplyr::filter(
    is.finite(log_luces),
    is.finite(interaccion_log),
    is.finite(interaccion_area)
  )

#observaciones talvez atipicas
#obtener residuos estandarizados
modelo_ols_dummies_full <- lm(
  log_luces ~ interaccion_log + interaccion_area +
    as.factor(municipio) + as.factor(fecha),
  data = panel_model
)

panel_model$rstudent <- rstudent(modelo_ols_dummies_full)

#identificar outliers
outliers <- panel_model %>% dplyr::filter(abs(rstudent) > 3)

print(outliers %>% dplyr::count(municipio, sort = TRUE))

panel_clean <- panel_model %>% dplyr::filter(abs(rstudent) <= 3)

#modelo
modelo_clean <- fixest::feols(
  log_luces ~ interaccion_log + interaccion_area | municipio + fecha,
  data = panel_clean,
  cluster = ~municipio
)
summary(modelo_clean)

#tabla de comparacion

fixest::etable(
  modelo_twfe_base, #log-lineal 
  modelo_twfe_log, #log-lineal (con cluster)
  modelo_twfe_poisson, #poisson (PPML)
  modelo_clean, #log-lineal (sin outliers)
  headers = c("OLS", "TWFE Principal", "Poisson (PPML)", "TWFE (Sin Outliers)")
)

if(F){
  "
  
  interpretacion del tablas comparativas:
  la primera columna son supuesto clasiscos, en la segunda columna contiene
  errores robusto white corrige heterocedasticidad y la ultima columan que
  el modelo principal este corrige heterocedaticidad y autocorrelacion, en la 
  cuarta columa es usa los errores de la 2ra columan pero sin outlier, los 
  resuldatos de los betas la 1ra, 2da y 4ta columan son de hecho casi lo mismo
  pero para este estudio se usara el modelo poisson 
  
  el valor -0.8472 es amortiguador antes shocks macroeconomicos del pais,
  es decir ante el aumento del 1% en la intensidad de la interaccion vial de 
  un municipio reduce el 0.8472% en la sensibilidad (volatilidad) de las luces 
  nocturnas ante el imae. De manera mas sencilla, más carreteras, la economía
  local se vuelve más autónoma, estable y protegida contra las crisis nacionales.
  
  Por ejemplo Managua, Estelí o Masaya tienen alta densidad vial entonces los
  mercados estan super interconectados. este coeficiente negativo dice que gracias
  a esta infraestructura, su economia interna son mas eficiente de las que tienen menos
  densidad vial como siuna que estan expuesto a sufrir mas si el transporte nacional
  se encarece
  
  ante esto me pregunte, si ante este valor negativo entonces la economia esta 
  protegida pero ante shock posivos tambien experimente un crecimento 
  restrictivo? si
  
  este coeficiente si demuestra que las carreteras general estabilidad estructural
  y resilencia, pero no potencia aqui ejemplos con escenerios:
  
  La economia esta en recesion (shocks negativo en el imae):
  significa que si el imae nacional se desploma un 10%, un municipio bien 
  conectado (alta densidad vial) no se desploma 10% completo, sino mucho menos 
  (cae alrededor de 8.47%) las carreteras protegen el empleo local, permiten 
  que los productos sigan fluyendo a mercados vecinos. Es un amortiguador
  
  si el imae sube un 10% las zonas, el municipio bien conectado igual solo 
  al rededor del 8.472%, crece por su propio ritmo estructural
  
  "
}

# el mae afecta a las luces de nicaragua? ----

#filtro para los inf
panel_validacion <- panel_final %>%
  dplyr::filter(is.finite(log_luces), is.finite(log_imae))

#modelo de elasticidad
modelo_validacion_proxy <- fixest::feols(
  log_luces ~ log_imae | municipio,
  data = panel_validacion,
  cluster = ~municipio
)

summary(modelo_validacion_proxy)

#crear tendencia para ver si hay regresión espuria
panel_validacion <- panel_final %>%
  dplyr::filter(is.finite(log_luces), is.finite(log_imae)) %>%
  dplyr::mutate(
    tendencia_lineal = as.numeric(fecha) - min(as.numeric(fecha)),
    mes_del_anio = as.factor(lubridate::month(fecha))
  )

#modelo aislando la tendencia a largo plazo y estacionalidad de las luces
modelo_validacion_estricto <- fixest::feols(
  log_luces ~ log_imae + tendencia_lineal | municipio + mes_del_anio,
  data = panel_validacion,
  cluster = ~municipio 
)

summary(modelo_validacion_estricto)


#tablas comparativas
fixest::etable(
  modelo_validacion_proxy,
  modelo_validacion_estricto,
  headers = c("validacion basica", "validacion con tendencia de meses")
  #drop = "tendencia_lineal" #ocultar la tendencia para enfocarse en el imae
)

if(F){
  "
  interpretacion de columna izquierda:
  por cada 1% que crece en el imae, las luces nocturnas en los municipios 
  incrementa en promedio un 2.105%
  
  interpretacion columna derecha:
  aqui se limpia la estacionariedad (mes_del_anio) del aumento de luces en 
  diciembre y se agrego una tendencia (el satelite sufre de calibraciones, 
  esta variable busca si existe tendenca por parte del satelite)
  una vez limpio
  
  un incremento del 1% en el ime genera un incremento del 6.141% en el 
  brillo de las luces y en la tendencia es un efecto muy pequeño pero todas
  las variables significativa al 1% entonces la tendencia es decreciente en la
  radiancia promedio 
  
  la intencion de esto era demostrar que es un buen proxy las luces_nocturnas
  recuerde que esto solo es correlacion
  "
}


#placebo permutacion de densid_vial entre municipios ----

#si el resultado es genuino entonces el coeficiente real debe estar
#en los extremos de la distribucion placebo
set.seed(42)
n_perm <- 1000

panel_placebo <- panel_final %>%
  dplyr::mutate(log_imae = log(imae))

#mapa de municipio
mapa_densidad <- panel_placebo %>%
  dplyr::distinct(municipio, densid_vial)

#comparar coeficientes
coef_real <- stats::coef(modelo_twfe_poisson)["interaccion_log"]
coefs_placebo_poisson <- numeric(n_perm)

message("permutando densid_vial entre municipios (", n_perm, " iteraciones)")

for (i in seq_len(n_perm)) {

  #reasignar densid_vial al azar entre municipios
  mapa_shuffled <- mapa_densidad %>%
    dplyr::mutate(densid_vial_falsa = sample(densid_vial, replace = FALSE))

  panel_p <- panel_placebo %>%
    dplyr::select(-densid_vial) %>%
    dplyr::left_join(mapa_shuffled, by = "municipio") %>%
    dplyr::mutate(
      interaccion_log_p = log(densid_vial_falsa) * log_imae 
    )

  mod_p <- fixest::fepois(
    luces_nocturnas ~ interaccion_log_p + interaccion_area | municipio + fecha,
    data = panel_p,
    cluster = ~municipio,
    warn = FALSE, notes = FALSE
  )

  coefs_placebo_poisson[i] <- stats::coef(mod_p)["interaccion_log_p"]
  
  if (i %% 100 == 0) message("   Progreso: ", i, "/", n_perm)
}

#valor p
p_placebo_poisson <- mean(abs(coefs_placebo_poisson) >= abs(coef_real))

message("coeficiente real") 
print(round(coef_real,4))
message("media del placebo") 
print(round(mean(coefs_placebo), 4))
message("desviacion (sd)")
print(round(sd(coefs_placebo), 4))
message("valor p-value")
print(p_placebo)

if(F){
  "
  
  interpretacion de la preuba placebo por permutacion de matriz espacial:
  la prueba placebo sirva para demostrar si el resultado beta es 
  pura casualidad matematica o ruido geografico, entonces la prueba asigna
  al azar carreteras y municipios equivocados (iteraciones igual a 1000)
  
  aqui se busca que los resultados del beta sean los mas cercanos a 0 en este
  caso la media es de -0.0062 y una sd 0.34 
  
  el p-value arroja 0.066 es decir el 6.6% de la iteraciones arrejo un valor
  causal de beta de -0.8472, o de otra manera en 934 de cada 1,000 intentos el 
  coeficiente falso cercana a cero
  
  el graifco solo es una distribucion de coeficientes tiene forma normal  
  
  el test placebo se hizo con poisson
  
  "
}

#grafico
#calcular la altura maxima de la densidad 
altura_texto <- max(density(coefs_placebo)$y) * 0.5

placebo_df <- data.frame(coef_placebo = coefs_placebo)

grafico_placebo <- ggplot(placebo_df, aes(x = coef_placebo)) +
  geom_histogram(aes(y = after_stat(density)), fill = "#94A3B8", color = "white", alpha = 0.85, bins = 30) +
  geom_density(color = "#475569", linewidth = 0.8, linetype = "dashed") +
  geom_vline(xintercept = coef_real, color = "#DC2626", linetype = "solid", linewidth = 1.2) +
  annotate("text", x = coef_real, y = altura_texto, 
           label = paste0("Coeficiente Real: ", round(coef_real, 4)), 
           color = "#DC2626", angle = 90, vjust = -0.5, fontface = "bold", size = 3.5) +
  labs(
    title = "Distribución del Coeficiente de Placebo vs. Efecto Real",
    subtitle = paste0("Simulación de Monte Carlo con ", n_perm, " permutaciones espaciales de la Red Vial (p-valor = ", round(p_placebo, 4), ")"),
    x = "Estimación del Coeficiente de Interacción Simulado",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1E293B"),
    plot.subtitle = element_text(size = 9, color = "#64748B"),
    panel.grid.minor = element_blank()
  )

print(grafico_placebo)
ggsave("Graficos/placebo_permutacion_histograma.pdf", plot = grafico_placebo, width = 8, height = 4.5, dpi = 300)

#modelos segmentados (estratos) por terciles de liminosidad ----

#clasificar municipios en terciles 
municipios_clasificados <- panel_final %>%
  dplyr::group_by(municipio) %>%
  dplyr::summarise(luz_historica = mean(luces_nocturnas, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    tercil = dplyr::ntile(luz_historica, 3),
    categoria = dplyr::case_when(
      tercil == 3 ~ "Estrato Alto",
      tercil == 2 ~ "Estrato Medio",
      tercil == 1 ~ "Estrato Bajo"
    )
  )

panel_estratos <- panel_model %>%
  dplyr::left_join(dplyr::select(municipios_clasificados, municipio, categoria),
                   by = "municipio")

#pre-filtrar cada estrato
panel_alto  <- dplyr::filter(panel_estratos, categoria == "Estrato Alto")
panel_medio <- dplyr::filter(panel_estratos, categoria == "Estrato Medio")
panel_bajo  <- dplyr::filter(panel_estratos, categoria == "Estrato Bajo")

modelos_estrato_poi <- list(
  Alto  = fixest::fepois(luces_nocturnas ~ interaccion_log + interaccion_area | 
                           municipio + fecha, data = panel_alto, cluster = ~municipio),
  Medio = fixest::fepois(luces_nocturnas ~ interaccion_log + interaccion_area | 
                           municipio + fecha, data = panel_medio, cluster = ~municipio),
  Bajo  = fixest::fepois(luces_nocturnas ~ interaccion_log + interaccion_area | 
                           municipio + fecha, data = panel_bajo, cluster = ~municipio)
)

#modelo de estratos
modelo_triple_poisson <- fixest::fepois(
  luces_nocturnas ~ interaccion_log + interaccion_area +
    i(categoria, interaccion_log, ref = "Estrato Alto") +
    i(categoria, interaccion_area, ref = "Estrato Alto") | 
    municipio + fecha,
  data = panel_estratos,
  cluster = ~municipio
)

message("como referencia el estrato alto")
summary(modelo_triple_poisson)

if(F){
  "
  
  "
}

#analisis de robustez del placebo ----
#no hacer caso esto esta sesgado tiene autocorrelacion serial por lo que
#no es concluyente

#placebo imae adenlantado (lead t+1) 

panel_lead <- panel_model %>%
  dplyr::arrange(municipio, fecha) %>%
  dplyr::group_by(municipio) %>%
  dplyr::mutate(
    log_imae_lead = dplyr::lead(log_imae),
    interaccion_log_lead  = densid_vial * log_imae_lead,
    interaccion_area_lead = log_area * log_imae_lead
  ) %>%
  dplyr::ungroup() %>%
  tidyr::drop_na(log_imae_lead)

modelo_lead <- fixest::feols(
  log_luces ~ interaccion_log_lead + interaccion_area_lead | municipio + fecha,
  data = panel_lead,
  cluster = ~municipio
)

summary(modelo_lead)

panel_lead %>% summarise(cor = cor(log_imae, log_imae_lead, use = "complete.obs"))

# el placebo lead no es concluyente por la autocorrealcion man

fixest::etable(
  modelo_twfe_log,
  modelo_lead,
  headers = c("Modelo Original (t)", "Placebo Lead (t+1)")
)

#mapa de coeficientes municipales (pendientes aleatorias) ----

message("mapa de coeficiente municipales")
message("modelo de pendientes aleatorias con lme4...")

modelo_random <- lme4::lmer(
  log_luces ~ interaccion_log + interaccion_area +
    (1 + interaccion_log | municipio) + (1 | fecha),
  data = panel_model,
  control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

#se extraye BLUPs

#extraer pendientes aleatorias 
blups_municipio <- lme4::ranef(modelo_random, condVar = TRUE)$municipio
blups_municipio$municipio <- rownames(blups_municipio)

efecto_fijo_interaccion <- lme4::fixef(modelo_random)["interaccion_log"]
blups_municipio$coef_municipal <- efecto_fijo_interaccion +
  blups_municipio$`interaccion_log`

#cargar shapefile para el mapa
ruta_mapa <- here::here("dataset", "geoBoundaries-NIC-ADM2.geojson")
nic_map <- sf::st_read(ruta_mapa, quiet = TRUE) %>%
  dplyr::rename(municipio = shapeName)

#unir coeficientes
nic_map <- nic_map %>%
  dplyr::left_join(
    dplyr::select(blups_municipio, municipio, coef_municipal),
    by = "municipio"
  )

# Mapa coroplético
mapa_coef <- ggplot(nic_map) +
  geom_sf(aes(fill = coef_municipal), color = "white", size = 0.15) +
  scale_fill_viridis_c(option = "plasma",
                       name = expression(beta[1]~municipal)) +
  labs(
    title = "Sensibilidad municipal de luces al IMAE: efecto moderador vial",
    subtitle = paste0(
      "Coeficiente municipal de interaccion_log (EF fijo + BLUP). ",
      "EF fijo global: ", round(efecto_fijo_interaccion, 3)
    ),
    caption = "Fuente: VIIRS-NASA, OSM, BCN. Modelo: lmer con pendiente aleatoria."
  ) +
  theme_void(base_size = 10) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 8))

ggsave(here::here("Graficos", "mapa_coeficientes_municipales.pdf"),
       plot = mapa_coef, width = 8, height = 7, dpi = 300)


#sensibilidad a saturacion de luces (umbral percentil 95) ----

umbral_p95 <- quantile(panel_model$luces_nocturnas, probs = 0.95, na.rm = TRUE)
message("umbral de saturacion (P95 luces_nocturnas): ",
        round(umbral_p95, 3))

panel_nosat <- panel_model %>%
  dplyr::filter(luces_nocturnas <= umbral_p95)

message("observaciones eliminadas por saturacion: ",
        nrow(panel_model) - nrow(panel_nosat), " de ", nrow(panel_model))

modelo_nosat <- fixest::feols(
  log_luces ~ interaccion_log + interaccion_area | municipio + fecha,
  data = panel_nosat,
  cluster = ~municipio
)

summary(modelo_nosat)

#tabla de robustez
fixest::etable(
  modelo_twfe_log,
  modelo_nosat,
  modelo_lead,
  headers = c("Original", "Sin Saturación (P95)", "Placebo Lead (t+1)")
)

#analisis sectorial ----

panel_final <- panel_final %>%
  mutate(
    log_imae_primario = log(imae_primario),
    interaccion_log_primario = densid_vial * log_imae_primario,
    interaccion_area_primario = log_area * log_imae_primario,
    
    log_imae_secundario = log(imae_secundario),
    interaccion_log_secundario = densid_vial * log_imae_secundario,
    interaccion_area_secundario = log_area * log_imae_secundario,
    
    log_imae_terciario = log(imae_terciario),
    interaccion_log_terciario = densid_vial * log_imae_terciario,
    interaccion_area_terciario = log_area * log_imae_terciario
  )

panel_model <- panel_final %>%
  filter(
    is.finite(log_luces),
    is.finite(interaccion_log_primario),
    is.finite(interaccion_area_primario),
    is.finite(interaccion_log_secundario),
    is.finite(interaccion_area_secundario),
    is.finite(interaccion_log_terciario),
    is.finite(interaccion_area_terciario)
  )

#modelos
modelo_primario <- feols(
  log_luces ~ interaccion_log_primario + interaccion_area_primario | municipio + fecha,
  data = panel_model, cluster = ~municipio
)

modelo_secundario <- feols(
  log_luces ~ interaccion_log_secundario + interaccion_area_secundario | municipio + fecha,
  data = panel_model, cluster = ~municipio
)

modelo_terciario <- feols(
  log_luces ~ interaccion_log_terciario + interaccion_area_terciario | municipio + fecha,
  data = panel_model, cluster = ~municipio
)
#tabla comparativa
etable(modelo_twfe_log, #modelo con imae total
       modelo_primario, 
       modelo_secundario, 
       modelo_terciario,
       headers = c("Total", "Primario", "Secundario", "Terciario"))



#grafico
#extraer coeficientes
df_dual <- data.frame(
  Sector = c(
    "Total (IMAE)", "Total (IMAE)",
    "Sector Primario", "Sector Primario",
    "Sector Secundario", "Sector Secundario",
    "Sector Terciario", "Sector Terciario"
  ),
  Variable = c(
    "Interacción Vial (Densidad)", "Control de Escala (Área)",
    "Interacción Vial (Densidad)", "Control de Escala (Área)",
    "Interacción Vial (Densidad)", "Control de Escala (Área)",
    "Interacción Vial (Densidad)", "Control de Escala (Área)"
  ),
  Coeficiente = c(
    stats::coef(modelo_twfe_log)["interaccion_log"],
    stats::coef(modelo_twfe_log)["interaccion_area"],
    stats::coef(modelo_primario)["interaccion_log_primario"],
    stats::coef(modelo_primario)["interaccion_area_primario"],
    stats::coef(modelo_secundario)["interaccion_log_secundario"],
    stats::coef(modelo_secundario)["interaccion_area_secundario"],
    stats::coef(modelo_terciario)["interaccion_log_terciario"],
    stats::coef(modelo_terciario)["interaccion_area_terciario"]
  ),
  ErrorEstandar = c(
    modelo_twfe_log$se["interaccion_log"],
    modelo_twfe_log$se["interaccion_area"],
    modelo_primario$se["interaccion_log_primario"],
    modelo_primario$se["interaccion_area_primario"],
    modelo_secundario$se["interaccion_log_secundario"],
    modelo_secundario$se["interaccion_area_secundario"],
    modelo_terciario$se["interaccion_log_terciario"],
    modelo_terciario$se["interaccion_area_terciario"]
  )
)

df_dual <- df_dual %>%
  dplyr::mutate(
    ci_inf = Coeficiente - 1.96 * ErrorEstandar,
    ci_sup = Coeficiente + 1.96 * ErrorEstandar,
    Sector = factor(Sector, levels = c("Sector Terciario", "Sector Secundario", "Sector Primario", "Total (IMAE)"))
  )
grafico_dual_sectores <- ggplot(df_dual, aes(x = Sector, y = Coeficiente, color = Variable)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#94A3B8", linewidth = 0.8) + 
  geom_errorbar(aes(ymin = ci_inf, ymax = ci_sup), 
                width = 0.2, linewidth = 0.9, 
                position = position_dodge(width = 0.5)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.5)) + 
  coord_flip() +
  scale_color_manual(values = c(
    "Control de Escala (Área)" = "#64748B",
    "Interacción Vial (Densidad)" = "#2563EB"
  )) +
  labs(
    title = "Análisis Sectorial: Efecto Moderador Vial vs. Control por Área",
    subtitle = "Comparación de elasticidades de luces nocturnas ante shocks macroeconómicos (IC 95%)",
    x = "Modelo / Indicador del IMAE",
    y = "Coeficiente Estimado (Beta)",
    color = "Variable del Modelo"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1E293B"),
    plot.subtitle = element_text(size = 9, color = "#64748B"),
    axis.text.y = element_text(face = "bold", size = 11, color = "#334155"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9)
  )

print(grafico_dual_sectores)
ggsave("Graficos/coeficientes_vial_vs_area.pdf", plot = grafico_dual_sectores, width = 8, height = 4.5, dpi = 300)

