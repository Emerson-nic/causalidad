if(FALSE){
  "
  
  se hara un moodelo de panel con efectos fijos bidireccionales 
  (Two-Way Fixed Effects - TWFE) con un termino de interaccion 
  (interaccion_causal = densid_vial * imae)
  
  
  "
}

#cargar librerias ---- 
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(ggeffects, 
               ggplot2,
               tidyr,
               dplyr,
               stringr,
               usethis
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

if(FALSE){
  "
  
  separacion de categorias:
  se usa dplyr::ntile(luz_historica, 3) quedivide los 153 municipios 3 grupos 
  iguales en tamaño (terciles) que se basa en su promedio historico de brillo 
  satelital:
  1. Estrato Alto: El 33% de los municipios con mayor actividad economica y urbana 
  2. Estrato Medio: El 33% intermedio con desarrollo moderado o cabeceras productivas
  3. Estrato Bajo: El 33% con menor luminosidad, representando zonas rurales 
  "
}

#grafico de imae y estratos ----

#funcion para limpiar nombres
limpiar_nombres <- function(nombres_sucios) {
  nombres_sucios %>%
    stringr::str_remove_all("(?i)\\(Municipio\\)|Municipio de |Municipio ") %>%
    stringr::str_trim() %>% 
    unique() %>%
    sort() %>%
    paste(collapse = ", ")
}

#calsificacion de mayor a menor
municipios_clasificados <- panel_final %>%
  dplyr::group_by(municipio) %>%
  dplyr::summarise(
    luz_historica = mean(luces_nocturnas, na.rm = TRUE),
    vial_historica = mean(densid_vial, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    tercil = dplyr::ntile(luz_historica, 3),
    categoria = case_when(
      tercil == 3 ~ "Estrato Alto",
      tercil == 2 ~ "Estrato Medio",
      tercil == 1 ~ "Estrato Bajo"
    )
  )

#lista de nombres
for(cat in c("Estrato Alto", "Estrato Medio", "Estrato Bajo")) {
  nombres <- municipios_clasificados %>% dplyr::filter(categoria == cat) %>% dplyr::pull(municipio)
  cat(paste0("\n", cat, " Completo:\n", limpiar_nombres(nombres), "\n"))
}

#unir estratos
panel_graficos <- panel_final %>%
  dplyr::left_join(municipios_clasificados %>% dplyr::select(municipio, categoria, vial_historica), 
                   by = "municipio")

#promedios
df_promedios_mensuales <- panel_graficos %>%
  dplyr::group_by(categoria, fecha) %>%
  dplyr::summarise(luces_grupo = mean(luces_nocturnas, na.rm = TRUE), .groups = "drop")

#grafico de imae
ddf_imae <- panel_final %>%
  dplyr::group_by(fecha) %>%
  dplyr::summarise(`IMAE Nacional` = mean(imae, na.rm = TRUE), .groups = "drop")

plot_imae <- ggplot(df_imae, aes(x = fecha, y = `IMAE Nacional`)) +
  geom_line(linewidth = 1.2, color = "#B91C1C") + 
  labs(
    title = "Evolución del Ciclo Macroeconómico Nacional (IMAE)",
    subtitle = "Comportamiento del shock exógeno común, transversal a todos los municipios",
    x = "Línea de Tiempo Mensual", 
    y = "Índice Mensual de Actividad Económica",
    caption = "Fuente: Elaboración propia con datos del Banco Central de Nicaragua."
  ) +
  theme_bw(base_size = 11) + 
  theme(panel.grid.minor = element_blank())

ggsave("Graficos/grafico_0_imae_nacional.pdf", plot = plot_imae, width = 9, height = 4, dpi = 300)

#estrato alto
df_alto_municipios <- panel_graficos %>% dplyr::filter(categoria == "Estrato Alto")
df_alto_promedios <- df_promedios_mensuales %>% dplyr::filter(categoria == "Estrato Alto")
densidad_alto <- round(mean(df_alto_municipios$vial_historica, na.rm=TRUE), 3)

caption_alto <- stringr::str_wrap(paste0("Municipios Integrantes: ", limpiar_nombres(df_alto_municipios$municipio)), width = 135)

plot_alto <- ggplot() +
  geom_line(data = df_alto_municipios, aes(x = fecha, y = luces_nocturnas, group = municipio), 
            color = "#94A3B8", alpha = 0.35, linewidth = 0.4) + 
  geom_line(data = df_alto_promedios, aes(x = fecha, y = `luces_grupo`), linewidth = 1.2, color = "#0F172A") + 
  labs(
    title = "Estrato Alto (Mayor Desarrollo Económico)",
    subtitle = paste0("Evolución temporal de la luminosidad local. Densidad Vial Promedio: ", densidad_alto),
    x = "Línea de Tiempo Mensual", y = "Luminosidad Promedio (NASA)",
    caption = caption_alto
  ) +
  theme_bw(base_size = 11) + 
  theme(
    panel.grid.minor = element_blank(), 
    plot.caption = element_text(size = 7.5, hjust = 0, color = "#475569", margin = margin(t = 10))
  )

ggsave("Graficos/grafico_1_estrato_alto.pdf", plot = plot_alto, width = 9, height = 6, dpi = 300)

#estrato medio
df_medio_municipios <- panel_graficos %>% dplyr::filter(categoria == "Estrato Medio")
df_medio_promedios <- df_promedios_mensuales %>% dplyr::filter(categoria == "Estrato Medio")
densidad_medio <- round(mean(df_medio_municipios$vial_historica, na.rm=TRUE), 3)

caption_medio <- stringr::str_wrap(paste0("Municipios Integrantes: ", limpiar_nombres(df_medio_municipios$municipio)), width = 135)

plot_medio <- ggplot() +
  geom_line(data = df_medio_municipios, aes(x = fecha, y = luces_nocturnas, group = municipio), 
            color = "#93C5FD", alpha = 0.4, linewidth = 0.4) +
  geom_line(data = df_medio_promedios, aes(x = fecha, y = `luces_grupo`), linewidth = 1.2, color = "#2563EB") + 
  labs(
    title = "Estrato Medio (Desarrollo Económico Intermedio)",
    subtitle = paste0("Evolución temporal de la luminosidad local. Densidad Vial Promedio: ", densidad_medio),
    x = "Línea de Tiempo Mensual", y = "Luminosidad Promedio (NASA)",
    caption = caption_medio
  ) +
  theme_bw(base_size = 11) + 
  theme(
    panel.grid.minor = element_blank(), 
    plot.caption = element_text(size = 7.5, hjust = 0, color = "#475569", margin = margin(t = 10))
  )

ggsave("Graficos/grafico_2_estrato_medio.pdf", plot = plot_medio, width = 9, height = 6, dpi = 300)

#estrato bajo
df_bajo_municipios <- panel_graficos %>% dplyr::filter(categoria == "Estrato Bajo")
df_bajo_promedios <- df_promedios_mensuales %>% dplyr::filter(categoria == "Estrato Bajo")
densidad_bajo <- round(mean(df_bajo_municipios$vial_historica, na.rm=TRUE), 3)

caption_bajo <- stringr::str_wrap(paste0("Municipios Integrantes: ", limpiar_nombres(df_bajo_municipios$municipio)), width = 135)

plot_bajo <- ggplot() +
  geom_line(data = df_bajo_municipios, aes(x = fecha, y = luces_nocturnas, group = municipio), 
            color = "#CBD5E1", alpha = 0.4, linewidth = 0.4) +
  geom_line(data = df_bajo_promedios, aes(x = fecha, y = `luces_grupo`), linewidth = 1.2, color = "#475569") + 
  labs(
    title = "Estrato Bajo (Zonas Estructuralmente Aisladas)",
    subtitle = paste0("Evolución temporal de la luminosidad local. Densidad Vial Promedio: ", densidad_bajo),
    x = "Línea de Tiempo Mensual", y = "Luminosidad Promedio (NASA)",
    caption = paste0(caption_bajo, "\nFuente: Elaboración propia con microdatos de OSM y NASA.")
  ) +
  theme_bw(base_size = 11) + 
  theme(
    panel.grid.minor = element_blank(), 
    plot.caption = element_text(size = 7.5, hjust = 0, color = "#475569", margin = margin(t = 10))
  )

ggsave("Graficos/grafico_3_estrato_bajo.pdf", plot = plot_bajo, width = 9, height = 6, dpi = 300)

# efectos <- ggeffects::ggemmeans(modelo_causal, terms = c("imae", "densid_vial"))
# 
# ggplot(efectos, aes(x = x, y = predicted, color = group)) +
#   geom_line(size = 1.2) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
#   labs(title = "Efecto de la Infraestructura en la Actividad Económica",
#        subtitle = "Respuesta de la luminosidad al shock del IMAE por nivel de densidad vial",
#        x = "Ciclo Económico (IMAE Nacional)",
#        y = "Luminosidad Predicha",
#        color = "Densidad Vial",
#        fill = "Densidad Vial") +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         panel.grid.minor = element_blank())
# 
# ggsave("graficos/impacto_vial_economico.png", width = 8, height = 6, dpi = 300)