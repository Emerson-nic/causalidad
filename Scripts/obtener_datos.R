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
  Southampton, paquete no me funciona no responde dice 
  'The geodata server seems to be temporary out of service. 
  Please try again later.'
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
               dplyr,
               blackmarbler, #conectarse a la api de la nasa
               here #para gestionar rutas relativas desde la raiz del proyecto de forma segura
)

#obtener poblacion estimada y mapa ----
#municipos de managua 

#mapa descargado manualmente en la repo

message("Fuente limites geograficos (ADM2):")
message("Proyecto: geoBoundaries (SNA Lab) - Universidad William & Mary (EE.UU.)")
message("Repositorio de datos de código abierto de alta disponibilidad")
message("Link oficial de descarga:")
message("https://media.githubusercontent.com/media/wmgeolab/geoBoundaries/main/releaseData/gbOpen/NIC/ADM2/geoBoundaries-NIC-ADM2.geojson")
destino_geojson <- here::here("dataset", "geoBoundaries-NIC-ADM2.geojson")
municipios_sf <- sf::st_read(destino_geojson, quiet = TRUE) %>%
  dplyr::select(
    NAME_1 = shapeID, 
    NAME_2 = shapeName
  )

#datos de WorldPop 
message("obtner poblacion de WorldPop ajustado por la onu")

# Ruta local directa al raster de poblacion ajustado con el nombre real del archivo
destino_raster <- here::here("dataset", "nic_ppp_2020_UNadj.tif")

message("Fuente de datos WorldPop")
message("Universidad de Southampton (Reino Unido)")
message("Link de descarga directa para el navegador:")
message("https://data.worldpop.org/GIS/Population/Global_2000_2020/2020/NIC/nic_ppp_2020_UNadj.tif")

message("Cargando malla de poblacion de forma local...")
poblacion_raster <- terra::rast(destino_raster)

# exact_extract lee los pixeles de WorldPop que caen dentro de 
#cada municipio y los suma
municipios_sf$poblacion_estimada <- exactextractr::exact_extract(
  poblacion_raster, 
  municipios_sf, 
  fun = "sum",
  progress = TRUE
)

#limpieza
tabla_poblacion_municipal <- municipios_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    departamento = NAME_1,
    municipio = NAME_2,
    poblacion_estimada
  ) %>%
  dplyr::arrange(departamento, municipio)

head(tabla_poblacion_municipal, 15)

print(head(tabla_poblacion_municipal, 15))

print('La poblacion es exacta')
print('año 2020 del dataset')

#expandir mensualmente para un dataset mas grande ----
# secuencia de tiempo mensual 5 años
meses_panel <- dplyr::tibble(
  fecha = seq(from = as.Date("2020-01-01"), to = as.Date("2024-12-01"), by = "month")
)

# Multiplicamos los 153 municipios por los 60 meses para generar la estructura grande
dataset_mensual <- tabla_poblacion_municipal %>%
  tidyr::crossing(meses_panel)

print(paste("total de filas generadas:", nrow(dataset_mensual)))
print(head(dataset_mensual, 15))

if(FALSE){
  "
  
  Los numeros repetidos no es un error, esto de debe a que la poblacion
  cambia de forma identica y muy lenta año con año, el efecto 
  fijo de Municipio absorbe la escala estructural del territorio, tambien
  se evita multicolinalidad artificial
  "
  
}

#obtener luces_nocturnas del 2020 a 2025 mensualmente ----
message("Fuente de datos:")
message("Satélite: Suomi-NPP / VIIRS (Producto: Black Marble VNP46A3/VNP46A4)")
message("Institución: NASA (National Aeronautics and Space Administration) - EE.UU.")
message("Se necesita una api les recomiendo usar un archivo .Renviron y pegar su api con el mismo nonbre que se usa en el script")
message("Link para el api/token(es gratis): https://urs.earthdata.nasa.gov/")
message("llenar todas la casillas y en Approved Applications agregar Approved Applications")

# blackmarbler::bm_extract descarga y calcula el promedio de luz por municipio 
#si no lee probar con este comando de abajo
#install.packages(c("sf", "terra"), type = "source")
# blackmarbler::bm_extract descarga y calcula el promedio de luz por municipio 

ruta_csv_final <- here::here("csv", "luces_nocturnas_municipales.csv")
if (!file.exists(ruta_csv_final)) {
  
  message("no existe el cvs, cargar datos")
  
  secuencia_fechas <- seq(from = as.Date("2020-01-01"), to = as.Date("2025-12-01"), by = "month")
  
  #lista
  lista_meses <- list()
  
  #bucle procesa mes a mes de forma independiente para evitar colapsos
  for (i in seq_along(secuencia_fechas)) {
    fecha_i <- secuencia_fechas[i]
    
    #tryCatch evita que un error 401/404 de la NASA detenga todo el script
    lista_meses[[i]] <- tryCatch({
      
      extrac_mes <- blackmarbler::bm_extract(
        roi_sf = municipios_sf, #mapa de municipios de geoBoundaries
        product_id = "VNP46A3", #identificador de VIIRS para datos mensuales
        date = fecha_i, 
        bearer = Sys.getenv("NASA_EARTHDATA_TOKEN"), # token en  .Renviron (usethis)
        output_dir = here::here("dataset"), 
        aggregation_fun = "mean", #promedio de brillo luminics del municipio
        keep_downloaded_files = TRUE #datos esticos y crudos en el disco duro
      )
      
      #si el mes no existe o da NULL, devolvemos una estructura vacía controlada
      if (is.null(extrac_mes)) {
        stop("Mes sin datos")
      }
      
      extrac_mes
      
    }, error = function(e) {
      #si falla el mes se genera la estructura vacía con NA para no romper el panel
      df_error <- data.frame(
        shapeID = municipios_sf$shapeID,
        date = fecha_i,
        nft_mean = NA
      )
      return(df_error)
    })
  }
  
  #unir todos los meses 
  luces_mensuales_panel <- dplyr::bind_rows(lista_meses)
  
  luces_limpias <- luces_mensuales_panel %>%
    sf::st_drop_geometry() %>%
    dplyr::rename(
      id_municipio = shapeID,
      luces_nocturnas = nft_mean
    ) %>%
    dplyr::arrange(id_municipio, date)
  
  utils::write.csv(luces_limpias, file = ruta_csv_final, row.names = FALSE)
  
  #rm(luces_mensuales_panel, luces_limpias)
}

luces_limpias <- utils::read.csv(ruta_csv_final)

head(luces_limpias, 15)
#ver un grafico turkudito

capa_luces <- terra::rast(here::here("dataset", "VNP46A3.A2020001.h09v07.002.2025133143743.h5"), subds = 1)

terra::plot(capa_luces, main = "Luminosidad Satelital VIIRS", col = terrain.colors(100))

