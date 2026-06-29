if(FALSE){
  "
  
  Este documenta se intenta buscar causalidad en la economia y red vial,
  osea si construir mas carreretas acelera la economia de manera cuasal
  
  se trabajara con datos de panal por lo que todas las variables seran
  proxy, se intento incluir el imae pero no hay informacion desagregada
  
  las proxys son:
  Y: luces_nocturnas como sust del imae
  x: densidad_vial (cerreteras).
  z (control): poblacion_estimada 
  
  para desindad_vial se usara OpenStreetMap (OSM) y su libreria
  luces_nocturnas se obtendra del paquete blackmarbler del satelite VIIRS 
  de la NASA
  poblacion_estimada se usa el paquete de WorldPop de la universidad de 
  Southampton
  
  "
}

#cargar librerias ---- 
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(usethis, #para .Renviron igual que .env de python
               geodata, #interfaz de mapas 
               sf, #para manejar vectores por municipios
               terra, #para manejar raters poblacion
               exactextractr, #para resumir rasters 
               dplyr
               )

#obtener poblacion estimada y mapa ----
#municipos de managua 
#level = 2 es la division por municipios 
municipios_raw <- gadm(country = "NIC", 
                       level = 2, 
                       path = tempdir(),
                       version = "latest")

municipios_sf <- st_as_sf(municipios_raw)

#datos de WorldPop 
message("descargando poblacion de WorldPop")
poblacion_raster <- population(year = 2020, 
                               country = "NIC", 
                               path = tempdir())

# exact_extract lee los pixeles de WorldPop que caen dentro de 
#cada municipio y los suma
municipios_sf$poblacion_estimada <- exact_extract(
  poblacion_raster, 
  municipios_sf, 
  fun = "sum",
  progress = TRUE
)

#limpieza
tabla_poblacion_municipal <- municipios_sf %>%
  st_drop_geometry() %>%
  select(
    departamento = NAME_1,
    municipio = NAME_2,
    poblacion_estimada
  ) %>%
  arrange(departamento, municipio)

head(tabla_poblacion_municipal, 15)


