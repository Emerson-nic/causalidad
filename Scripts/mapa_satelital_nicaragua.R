if(F){
  "
 mapa de nic vista nocturna desde NASA VIIRS
 producto mensual VNP46A3: diciembre 2025
  "
}
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")
pacman::p_load(blackmarbler,
               terra,
               sf,
               ggplot2,
               dplyr,
               viridis,
               ggrastr) 

#limites de nicaragua
ruta_mapa <- here::here("dataset", "geoBoundaries-NIC-ADM2.geojson")
nic_sf <- sf::st_read(ruta_mapa, quiet = TRUE)
nic_union <- sf::st_union(nic_sf)

#proyectar a UTM Zona 16N (Nicaragua)
nic_sf_utm <- sf::st_transform(nic_sf, "EPSG:32616")
nic_union_utm <- sf::st_transform(nic_union, "EPSG:32616")
nic_union_utm_sf <- sf::st_as_sf(nic_union_utm)

#producto mensual: diciembre 2025
r <- blackmarbler::bm_raster(
  roi_sf = nic_sf,
  product_id = "VNP46A3",
  date = "2025-12-01",
  bearer = Sys.getenv("NASA_EARTHDATA_TOKEN"),
  output_dir = here::here("dataset"),
  quiet = FALSE
)

#recortes iniciales
r_nic <- terra::crop(r, terra::vect(nic_union))
r_nic <- terra::mask(r_nic, terra::vect(nic_union))

r_utm <- terra::project(r_nic, "EPSG:32616", method = "bilinear")

gauss_k <- terra::focalMat(r_utm, d = 1200, type = "Gauss")
r_smooth <- terra::focal(r_utm, w = gauss_k, na.rm = TRUE)

umbral_ruido <- 0.3
r_smooth[r_smooth < umbral_ruido] <- 0

#pixeles mas pequeños
r_fine <- terra::disagg(r_smooth, fact = 2, method = "bilinear")
r_fine <- terra::mask(r_fine, terra::vect(nic_union_utm))

#dataframe completo
df <- as.data.frame(r_fine, xy = TRUE, na.rm = FALSE)
names(df) <- c("x", "y", "radiancia")
df <- df %>% dplyr::filter(!is.na(radiancia))

#redondeo 
df <- df %>%
  dplyr::mutate(
    x = round(x, 2),
    y = round(y, 2)
  )

#winsorizar al 99%
positivos <- df$radiancia[df$radiancia > 0]
if(length(positivos) > 0){
  max_val <- quantile(positivos, 0.99, na.rm = TRUE)
  df <- df %>% dplyr::mutate(radiancia = ifelse(radiancia > max_val, max_val, radiancia))
}

#grafico final de super resolucion
p <- ggplot() +
  ggrastr::rasterise(
    geom_raster(data = df, aes(x = x, y = y, fill = radiancia), interpolate = TRUE),
    dpi = 500
  ) +
  geom_sf(data = nic_sf_utm, fill = NA, color = "grey50", linewidth = 0.04, alpha = 0.5) +
  geom_sf(data = nic_union_utm_sf, fill = NA, color = "grey30", linewidth = 0.3) +
  scale_fill_viridis(
    option = "inferno",
    name = "Radiancia\n(nW/cm²/sr)",
    na.value = "white",
    trans = "sqrt" 
  ) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    legend.text = element_text(color = "black", size = 8),
    legend.title = element_text(color = "black", size = 9, face = "bold"),
    legend.position = "right"
  )

print(p)

ggplot2::ggsave(
  here::here("Graficos", "mapa_satelital_nicaragua.pdf"),
  plot = p, 
  width = 8, 
  height = 6, 
  dpi = 500,
  device = cairo_pdf,
  bg = "white"
)