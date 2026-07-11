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
  
  las unidades de medida son combinas y una sola es :
  nw(nanovatios), cm cuadraro y sr(estereorradian)
  
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
               patchwork,
               car,
               purrr,
               zoo
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

#### modelo principal ----
modelo_twfe_poisson <- fixest::fepois(
  luces_nocturnas ~ interaccion_log + interaccion_area | municipio + fecha,
  data = panel_final,
  cluster = ~municipio
)

summary(modelo_twfe_poisson)

####

#demas modelos poisson ----
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
    #title = "Distribución de Residuos del Modelo TWFE Poisson",
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

#modelo outliers para poisson

modelo_clean_poisson <- fixest::fepois(
  luces_nocturnas ~ interaccion_log + interaccion_area | municipio + fecha,
  data = panel_clean,
  cluster = ~municipio
)
summary(modelo_clean_poisson)

#tabla de comparacion

fixest::etable(
  modelo_twfe_base, #log-lineal 
  modelo_twfe_log, #log-lineal (con cluster)
  modelo_twfe_poisson, #poisson (PPML)
  modelo_clean_poisson,# poisson sin outliers
  modelo_clean, #log-lineal (sin outliers)
  headers = c("OLS", "TWFE Principal", "Poisson (PPML)", "Poisson (PPML - Sin Outliers)", "TWFE (Sin Outliers)")
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

# analisis imae y luces nocturnas ----

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
  
  por ejemplo si el imae tendencia ciclo crece un 10% el las luces deberias 
  deberia aumentar un 61% en prmedio pero en el grafico se observa un desface
  en el año 2020 y 2021, se deberia hacer un analisis mejor 
  
  la intencion de esto era demostrar que es un buen proxy las luces_nocturnas
  recuerde que esto solo es correlacion, es mejor analizar mas afondo la 
  este proxy para futuras investigaciones
  "
}

#graficos
panel_stl <- panel_final %>%
  dplyr::group_by(fecha) %>%
  dplyr::summarise(
    luz_nacional = mean(luces_nocturnas, na.rm = TRUE),
    imae_nacional = mean(imae, na.rm = TRUE)
  ) %>%
  dplyr::arrange(fecha)

luces_ts <- ts(panel_stl$luz_nacional, frequency = 12, start = c(2020, 1))

# filtro STL (descomposicion por Loess)
descomposicion_stl <- stl(luces_ts, s.window = "periodic")

panel_stl$luz_tendencia_stl <- as.numeric(descomposicion_stl$time.series[, "trend"])

coef_escala <- max(panel_stl$luz_tendencia_stl, na.rm = TRUE) / 
  max(panel_stl$imae_nacional, na.rm = TRUE)

imae_luces_stl <- ggplot(panel_stl, aes(x = fecha)) +
  geom_line(aes(y = luz_tendencia_stl, color = "Luces (Tendencia STL)"), linewidth = 1.2) +
  geom_line(aes(y = imae_nacional * coef_escala, color = "IMAE (Tendencia-Ciclo)"), linewidth = 1.2, linetype = "dashed") +
  
  scale_y_continuous(
    name = "Radiancia Promedio (Luces Nocturnas)",
    sec.axis = sec_axis(~ . / coef_escala, name = "Índice IMAE")
  ) +
  labs(
    #title = "Evolución Macroeconómica: IMAE vs Luces Nocturnas",
    #subtitle = "Serie de luces filtrada mediante Descomposición Estacional Loess (STL)",
    x = "Año",
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Luces (Tendencia STL)" = "#d35400", 
                                "IMAE (Tendencia-Ciclo)" = "#2980b9")) +
  theme(
    legend.position = "bottom", 
    plot.title = element_text(face = "bold"),
    axis.title.y.left = element_text(color = "#d35400", face = "bold"),
    axis.title.y.right = element_text(color = "#2980b9", face = "bold")
  )

print(imae_luces_stl)

ggplot2::ggsave(
  filename = "Graficos/imae_luces_filtro_stl.pdf",
  plot = imae_luces_stl,
  width = 10,       
  height = 6,       
  dpi = 300         
)


#placebo permutacion de densid_vial entre municipios ----

#si el resultado es genuino entonces el coeficiente real debe estar
#en los extremos de la distribucion placebo
set.seed(42)
n_perm <- 1000

panel_placebo <- panel_final %>%
  dplyr::mutate(log_imae = log(imae))

#mapa de municipio
mapa_densidad <- panel_placebo %>%
  dplyr::distinct(municipio, log_densidad_vial)

#comparar coeficientes
coef_real <- stats::coef(modelo_twfe_poisson)["interaccion_log"]
coefs_placebo_poisson <- numeric(n_perm)

message("permutando densid_vial entre municipios (", n_perm, " iteraciones)")

for (i in seq_len(n_perm)) {
  
  mapa_shuffled <- mapa_densidad %>%
    dplyr::mutate(log_densidad_vial_falsa = sample(log_densidad_vial, replace = FALSE))
  
  panel_p <- panel_placebo %>%
    dplyr::select(-log_densidad_vial) %>%
    dplyr::left_join(mapa_shuffled, by = "municipio") %>%
    dplyr::mutate(
      interaccion_log_p = log_densidad_vial_falsa * log_imae   
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
message("media del placebo_poisson") 
print(round(mean(coefs_placebo_poisson), 4))
message("desviacion (sd)")
print(round(sd(coefs_placebo_poisson), 4))
message("valor p-value")
print(p_placebo_poisson)

if(F){
  "
  
  interpretacion de la preuba placebo por permutacion de matriz espacial:
  la prueba placebo sirva para demostrar si el resultado beta es 
  pura casualidad matematica o ruido geografico, entonces la prueba asigna
  al azar carreteras y municipios equivocados (iteraciones igual a 1000)
  
  aqui se busca que los resultados del beta sean los mas cercanos a 0 en este
  caso la media es de -0.0147 y una sd 0.19 
  
  el p-value arroja 0.001 es decir el 0.1% de la iteraciones arrejo un valor
  causal de beta de -0.8472, o de otra manera en 999 de cada 1,000 intentos el 
  coeficiente falso es cercano a cero
  
  el graifco solo es una distribucion de coeficientes tiene forma casi normal  
  
  el test placebo se hizo con poisson
  
  "
}

#grafico
#calcular la altura maxima de la densidad 
altura_texto <- max(density(coefs_placebo_poisson)$y) * 0.5

placebo_df <- data.frame(coef_placebo = coefs_placebo_poisson)

grafico_placebo <- ggplot(placebo_df, aes(x = coef_placebo)) +
  geom_histogram(aes(y = after_stat(density)), fill = "#94A3B8", color = "white", alpha = 0.85, bins = 30) +
  geom_density(color = "#475569", linewidth = 0.8, linetype = "dashed") +
  geom_vline(xintercept = coef_real, color = "#DC2626", linetype = "solid", linewidth = 1.2) +
  annotate("text", x = coef_real, y = altura_texto, 
           label = paste0("Coeficiente Real: ", round(coef_real, 4)), 
           color = "#DC2626", angle = 90, vjust = -0.5, fontface = "bold", size = 3.5) +
  labs(
    title = "Distribución del Coeficiente de Placebo vs. Efecto Real",
    subtitle = paste0("Simulación de Monte Carlo con ", n_perm, " permutaciones espaciales de la Red Vial (p-valor = ", 
                      round(p_placebo_poisson, 4), ")"),
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

matriz_vcov <- vcov(modelo_triple_poisson)
names(coef(modelo_triple_poisson))

test_bajo_vial <- wald(modelo_triple_poisson, "categoria::Estrato Bajo:interaccion_log")
print(test_bajo_vial)
test_medio_vial <- wald(modelo_triple_poisson, "categoria::Estrato Medio:interaccion_log")
print(test_medio_vial)

#este test mira la covarianza (test de wald)
#h0: los betas son distintos de 0

#interpretacion mas directa
fixest::etable(
  modelos_estrato_poi[["Alto"]],
  modelos_estrato_poi[["Medio"]],
  modelos_estrato_poi[["Bajo"]],
  headers = c("Estrato Alto", "Estrato Medio", "Estrato Bajo")
)

#crear mapa de estratos 
#extraer coeficientes
coefs_estratos <- map_dfr(modelos_estrato_poi, function(mod) {
  ct <- coeftable(mod, keep = c("interaccion_log", "interaccion_area"))
  data.frame(
    term = rownames(ct),
    estimate = ct[, "Estimate"],
    std.error = ct[, "Std. Error"],
    p.value  = ct[, "Pr(>|z|)"]
  )
}, .id = "estrato")

df_forest <- coefs_estratos %>%
  mutate(
    categoria = case_when(
      estrato == "Alto" ~ "Estrato Alto",
      estrato == "Medio" ~ "Estrato Medio",
      estrato == "Bajo" ~ "Estrato Bajo"
    ),
    categoria = factor(categoria, levels = c("Estrato Bajo", "Estrato Medio", "Estrato Alto")),
    variable = case_when(
      term == "interaccion_log" ~ "Canal Vial",
      term == "interaccion_area" ~ "Canal Área"
    ),
    ci_lower = estimate - 1.96 * std.error,
    ci_upper = estimate + 1.96 * std.error,
    significancia = case_when(
      p.value < 0.01 ~ "Significativo al 1%",
      p.value < 0.05 ~ "Significativo al 5%",
      TRUE ~ "No Significativo"
    )
  )

#mapa
ruta_mapa <- here::here("dataset", "geoBoundaries-NIC-ADM2.geojson")
nic_map_raw <- sf::st_read(ruta_mapa, quiet = TRUE) %>% 
  dplyr::rename(municipio = shapeName)

mapa_estratos_df <- nic_map_raw %>%
  dplyr::left_join(municipios_clasificados, by = "municipio") %>%
  dplyr::mutate(categoria = factor(categoria, levels = c("Estrato Alto", "Estrato Medio", "Estrato Bajo")))

colores_estratos <- c("Estrato Alto"  = "#2C3E50",
                      "Estrato Medio" = "#F39C12",
                      "Estrato Bajo"  = "#E74C3C")

#mapa
p1_mapa <- ggplot(mapa_estratos_df) +
  geom_sf(aes(fill = categoria), color = "white", size = 0.08) +
  scale_fill_manual(values = colores_estratos, name = NULL, na.translate = FALSE) +
  theme_void(base_size = 9) +
  labs(
    # title = "Distribución Espacial de Terciles",
    # subtitle = "Clasificación por promedio histórico de luces"
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  )

ggplot2::ggsave(
  filename = here::here("Graficos", "mapa_estratos_terciles.pdf"),
  plot = p1_mapa, 
  width = 7, 
  height = 6, 
  dpi = 300
)

p2_forest <- ggplot(df_forest, aes(x = estimate, y = categoria)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#AEB6BF", size = 0.5) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.15, color = "#34495E", size = 0.7) +
  geom_point(aes(color = categoria), size = 3.5) +
  geom_text(aes(label = sprintf("%.4f", estimate)), vjust = -1, size = 2.5, fontface = "bold", color = "#2C3E50") +
  scale_color_manual(values = colores_estratos) +
  facet_wrap(~variable, scales = "free_x", ncol = 1) +
  labs(
    title = "B. Estimaciones y Precisión (IC 95%)",
    subtitle = expression("Coeficientes estructurales (" * beta * ") por Modelo de Estrato"),
    x = "Elasticidad de Transmisión",
    y = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", color = "#1C2833", hjust = 0.5),     
    plot.subtitle = element_text(size = 8, color = "#5D6D7E", hjust = 0.5),       
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#F2F4F4"),
    strip.background = element_rect(fill = "#F8F9F9", color = NA),
    strip.text = element_text(face = "bold", color = "#2C3E50"),
    axis.text.y = element_text(face = "bold"),
    legend.position = "none"
  )

mapa_panel_final <- p1_mapa + p2_forest + 
  plot_layout(ncol = 2, widths = c(1.2, 1)) +
  plot_annotation(
    #title = "Análisis de Resiliencia Macroeconómica por Estratos de Luminosidad",
    #subtitle = "Izquierda: Delimitación de clústeres económicos locales. Derecha: Coeficientes de interacción Poisson TWFE con intervalos de confianza empíricos.",
    #caption = "Fuente: Estimaciones propias basadas en VIIRS-NASA, OpenStreetMap y Banco Central de Nicaragua. Errores estándar clusterizados por municipio.",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 11, color = "#17202A"),
      plot.subtitle = element_text(hjust = 0.5, size = 8, color = "#566573"),
      plot.background = element_rect(fill = "white", color = NA) 
    )
  )

ggsave(
  filename = here::here("Graficos", "mapa_y_coeficientes_estratos_consolidado.pdf"),
  plot = mapa_panel_final, 
  width = 11, 
  height = 6, 
  dpi = 300
)

if(F){
  "
  
  usar la ultima tabla que es la interpretacion mas directa
  
  estrato alto:
  solo la interaccion de la densidad vial es significativa al 5% es decir
  municipios como nagagua tienen un valor beta de -0.6666 de amortiguador
  causal pero aqui el area no importa mucho como motor economico
  
  estrato medio:
  la densidad vial y el area son  no significativo indica neutralidad ciclica
  ante los shocks del imae.
  
  estrato bajo:
  ambas variables significativas la densiadad vial al 5% y el area al 1%,
  las variables son positivas, la densidad vial tiene un efecto multiplicador
  de 0.74%, encuanto el area tiene un efecto multiplicador de vulnerabilidad
  de 0.79% por ejemplo su el imae sube ellos brillan un poco mas, pero si 
  el imae cae (una crisis), ellos sufren un apagón económico mucho más severo 
  y desproporcionado 
  
  
  
  "
}

#analisis de robustez del placebo ----
#no hacer caso esto esta sesgado tiene autocorrelacion serial por lo que
#no es concluyente

#placebo imae adenlantado (lead t+1) 
#agregar log aqui las densidad esta en niveles ;(

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
#nota: esto se hizo en python, la razon r es lento es mejor hacerlo en python
#para obtener los coeficientes y aqui procesarlo

#modelo bayesiano jerárquico (Poisson GLMM)
resultados_poisson <- read.csv("csv/bayes_mejorado_total.csv")

#filtro de significancia bayesiana, el intervalo de credibilidad al 95% no incluye el cero
mapa_data_vial <- resultados_poisson %>%
  dplyr::mutate(
    coef_vial_filtrado = dplyr::if_else(ci_vial_lower > 0 | ci_vial_upper < 0, coef_vial, NA_real_),
    coef_area_filtrado = dplyr::if_else(ci_area_lower > 0 | ci_area_upper < 0, coef_area, NA_real_)
  )

#mapa
ruta_mapa <- here::here("dataset", "geoBoundaries-NIC-ADM2.geojson")
nic_map_raw <- sf::st_read(ruta_mapa, quiet = TRUE) %>% 
  dplyr::rename(municipio = shapeName)

#unir datos con el mapa
nic_map_completo <- nic_map_raw %>%
  dplyr::left_join(mapa_data_vial, by = "municipio")

color_no_significativo <- "#D5D8DC" 
color_bordes <- "#AEB6BF"

#densidad vial
p1 <- ggplot(nic_map_completo) +
  geom_sf(fill = color_no_significativo, color = color_bordes, size = 0.08) +
  geom_sf(aes(fill = coef_vial_filtrado), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "plasma", 
    na.value = "transparent", 
    name = expression(beta[vial])
  ) +
  labs(
    title = "A. Canal Infraestructura Vial", 
    subtitle = "Moderación del Escudo Carreteras"
  ) +
  theme_void(base_size = 9) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "#1C2833"),
    plot.subtitle = element_text(hjust = 0.5, size = 7, color = "#5D6D7E"),
    legend.position = "bottom"
  )

#area
p2 <- ggplot(nic_map_completo) +
  geom_sf(fill = color_no_significativo, color = color_bordes, size = 0.08) +
  geom_sf(aes(fill = coef_area_filtrado), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "viridis", 
    na.value = "transparent", 
    name = expression(beta[área])
  ) +
  labs(
    title = "B. Canal Dimensión Territorial", 
    subtitle = "Multiplicador de Fricción Espacial"
  ) +
  theme_void(base_size = 9) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "#1C2833"),
    plot.subtitle = element_text(hjust = 0.5, size = 7, color = "#5D6D7E"),
    legend.position = "bottom"
  )

mapa_panel_1x2 <- p1 + p2 + 
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Heterogeneidad Espacial Continua de la Resiliencia Macroeconómica",
    subtitle = "Efectos marginales específicos (modelo bayesiano jerárquico Poisson). Municipios en gris claro: intervalo de credibilidad del 95% incluye el cero.",
    caption = "Fuente: VIIRS-NASA, OpenStreetMap, Banco Central de Nicaragua. Procesamiento dual Python-R.",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 11, color = "#17202A"),
      plot.subtitle = element_text(hjust = 0.5, size = 8, color = "#566573"),
      plot.background = element_rect(fill = "white", color = NA) 
    )
  )

ggsave(
  here::here("Graficos", "mapa_panel_heterogeneidad_bayesiana_1x2.pdf"),
  plot = mapa_panel_1x2, 
  width = 11, 
  height = 6, 
  dpi = 300
)

#ver script en python ahi esta los que son significativos

#aqui se ven las variables significativas desglosadas
betas_municipales <- read.csv("csv/bayes_mejorado_total.csv")


if(F){
  "
  
  aqui se hizo modelo bayesiano jerárquico (Poisson GLMM)
  
  esto es util para politicas publicas de manera preliminar recuerdese
  no es causal
  
  los unicos no significativo son Corinto (Municipio), 
  San Juan de Cinco Pinos (Municipio),
  Municipio San Dionisio
  Municipio San Rafael del Norte
  para la variable densidad_vial 
  las coeficiente de densidad_vial son significativos y positivos
  nota: existe algunos na el 
  coeficiente_vial_flitrado es normal porque son no significativos
  
  en la variable area todos son significativos y negativos :0
  
  recuerdese: se hizo estandarizacion para que diera mejor la convergencia 
  sirvio y despues se hizo lo que es un reesccalado 
  
  
  "
}


#sensibilidad a saturacion de luces (umbral percentil 95) ----

umbral_p95 <- quantile(panel_model$luces_nocturnas, probs = 0.95, na.rm = TRUE)
message("umbral de saturacion (P95 luces_nocturnas): ",
        round(umbral_p95, 3))

panel_nosat <- panel_model %>%
  dplyr::filter(luces_nocturnas <= umbral_p95)

message("observaciones eliminadas por saturacion: ",
        nrow(panel_model) - nrow(panel_nosat), " de ", nrow(panel_model))

modelo_nosat <- fixest::fepois(
  luces_nocturnas ~ interaccion_log + interaccion_area | municipio + fecha,
  data = panel_nosat,
  cluster = ~municipio
)

summary(modelo_nosat)

#tabla de robustez
fixest::etable(
  modelo_twfe_poisson,
  modelo_nosat,
  headers = c("Original Poisson", "Sin Saturación (P95) Poisson")
)

if(F){
  "
  
  en esta seccion los resultados no se diferencian mucho densidad vial solo es
  mas ancho pero con intervalo de confianza del 1% y el area entra al 10%
  
  resulatado possion: interaccion_log  -0.8472*** (0.1927) y interaccion_area 
  0.0459 (0.0721)
  
  resultado possion sin saturacion p95: interaccion_log -0.8585*** (0.2599)
  interaccion_area 0.2019. (0.1160)
  
  "
}

#analisis sectorial ----

panel_final <- panel_final %>%
  mutate(
    log_imae_primario = log(imae_primario),
    interaccion_log_primario = log_densidad_vial * log_imae_primario,
    interaccion_area_primario = log_area * log_imae_primario,
    
    log_imae_secundario = log(imae_secundario),
    interaccion_log_secundario = log_densidad_vial * log_imae_secundario,
    interaccion_area_secundario = log_area * log_imae_secundario,
    
    log_imae_terciario = log(imae_terciario),
    interaccion_log_terciario = log_densidad_vial * log_imae_terciario,
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
modelo_twfe_poisson <- fixest::fepois(
  luces_nocturnas ~ interaccion_log + interaccion_area | municipio + fecha,
  data = panel_model, cluster = ~municipio
)

modelo_primario <- fixest::fepois(
  luces_nocturnas ~ interaccion_log_primario + interaccion_area_primario | municipio + fecha,
  data = panel_model, cluster = ~municipio
)

modelo_secundario <- fixest::fepois(
  luces_nocturnas ~ interaccion_log_secundario + interaccion_area_secundario | municipio + fecha,
  data = panel_model, cluster = ~municipio
)

modelo_terciario <- fixest::fepois(
  luces_nocturnas ~ interaccion_log_terciario + interaccion_area_terciario | municipio + fecha,
  data = panel_model, cluster = ~municipio
)
#tabla comparativa
fixest::etable(modelo_twfe_poisson, #modelo con imae total
       modelo_primario, 
       modelo_secundario, 
       modelo_terciario,
       headers = c("Total", "Primario", "Secundario", "Terciario"))

if(F){
  "
  
  se realizo regressiones poisson donde la densidad_vial es significativa y 
  todas negativas, el area no es significativo, en el sector primario el beta
  es de -1.05 el mas bajo de todos, en el secundario y terciario el valor 
  es  muy similar -0.754 y -0.744 respectivamente
  
  "
}


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
    stats::coef(modelo_twfe_poisson)["interaccion_log"],
    stats::coef(modelo_twfe_poisson)["interaccion_area"],
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

