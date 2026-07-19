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
  imae (tendencia ciclo): se utiliza para hacer una serie de tiempo la densidad_vial
  
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
               janitor,
               readxl,
               blackmarbler, #conectarse a la api de la nasa
               osmdata, #para las carreteras 
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

if (!file.exists(destino_raster)) {
  message("Descargando raster de WorldPop (~58 MB)...")
  download.file(
    "https://data.worldpop.org/GIS/Population/Global_2000_2020/2020/NIC/nic_ppp_2020_UNadj.tif",
    destino_raster,
    mode = "wb"
  )
}
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
  
  # Creamos la secuencia de meses
  meses_nasa <- seq(from = as.Date("2020-01-01"), to = as.Date("2025-12-01"), by = "month")
  lista_luces <- list() # Lista vacia para guardar mes a mes
  
  # Bucle for para blindar la descarga contra caidas 401/404
  for (i in seq_along(meses_nasa)) {
    mes_actual <- meses_nasa[i]
    message(paste("procesando mes:", mes_actual))
    
    # tryCatch protege el codigo: si un mes falla, devuelve NULL pero no detiene el script
    resultado_mes <- tryCatch({
      blackmarbler::bm_extract(
        roi_sf = municipios_sf, #mapa de municipios de geoBoundaries
        product_id = "VNP46A3", #identificador de VIIRS para datos mensuales
        date = mes_actual, # Procesamos solo un mes a la vez
        bearer = Sys.getenv("NASA_EARTHDATA_TOKEN"), # token en  .Renviron (usethis)
        output_dir = here::here("dataset"), 
        aggregation_fun = "mean", #promedio de brillo luminics del municipio
        keep_downloaded_files = TRUE #datos esticos y crudos en el disco duro
      )
    }, error = function(e) {
      message(paste("error o sin datos satelitales en:", mes_actual, "- saltando al siguiente"))
      return(NULL)
    })
    
    if (!is.null(resultado_mes)) {
      lista_luces[[i]] <- resultado_mes
    }
  }
  
  # Unimos todos los meses procesados en una sola tabla
  luces_mensuales_panel <- dplyr::bind_rows(lista_luces)
  
  #formato de la tabla resultante para integrarla a tu panel final
  luces_limpias <- luces_mensuales_panel %>%
    sf::st_drop_geometry() %>%
    dplyr::rename(
      id_municipio = NAME_1, #cambiamos shapeID por NAME_1 que es su nombre actual
      luces_nocturnas = ntl_mean
    ) %>%
    dplyr::arrange(id_municipio, date)
  
  utils::write.csv(luces_limpias, file = ruta_csv_final, row.names = FALSE)
  
  #rm(luces_mensuales_panel, luces_limpias)
}

luces_limpias <- utils::read.csv(ruta_csv_final)

head(luces_limpias, 15)

#densidad vial ----
message("Fuente de datos: OpenStreetMap con exportacion manual desde overpass-turbo.eu")

# La red vial se descargó manualmente desde Overpass Turbo.
# Consulta Overpass utilizada:
#   [out:json][timeout:60];
#   area["ISO3166-1"="NI"]->.searchArea;
#   (
#     way["highway"~"motorway|trunk|primary|secondary|tertiary|unclassified"](area.searchArea);
#   );
#   out body;
#   >;
#   out skel qt;
# El archivo export.geojson (~80 MB) se descargó y guardó en dataset/
# El script no ejecuta la consulta Overpass; usa el GeoJSON precargado.

#cargar mapas
ruta_municipios <- "dataset/geoBoundaries-NIC-ADM2.geojson"
ruta_csv_vial <- here::here("csv", "densidad_vial_municipales.csv")

if (!file.exists(ruta_csv_vial)) {
  ruta_raw_geojson <- "dataset/export.geojson"
  ruta_vial_optimizada <- "dataset/nicaragua_carreteras.rds"
  
  #cargar municipios
  municipios_sf <- sf::st_read(ruta_municipios, quiet = TRUE)
  
  #optimizacion crear archivo .rds si no existe
  if (!file.exists(ruta_vial_optimizada)) {
    message("de GeoJSON a formato binario .rds")
    carreteras_raw <- sf::st_read(ruta_raw_geojson, quiet = TRUE)
    carreteras_sf <- carreteras_raw %>% 
      sf::st_transform(crs = sf::st_crs(municipios_sf)) %>% 
      sf::st_make_valid() 
    saveRDS(carreteras_sf, ruta_vial_optimizada)
    rm(carreteras_raw)
  }
  
  message("este comando de abajo tarda bastante")
  carreteras_sf <- readRDS(ruta_vial_optimizada)
  
  #calcular densidad vial por municipio
  carreteras_municipio <- sf::st_intersection(carreteras_sf, municipios_sf)
  
  #calcular longitud en km por segmento y luego sumar por municipio
  message("igual este tarda mucho")
  carreteras_resumen <- carreteras_municipio %>%
    dplyr::mutate(longitud_km = sf::st_length(.) / 1000) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(shapeName) %>%
    dplyr::summarise(km_vial = sum(as.numeric(longitud_km)))
  
  #unir
  municipios_sf <- municipios_sf %>%
    dplyr::mutate(area_km2 = as.numeric(sf::st_area(.) / 1e6)) %>%
    dplyr::left_join(carreteras_resumen, by = "shapeName") %>%
    dplyr::mutate(densid_vial = km_vial / area_km2) %>%
    tidyr::replace_na(list(densid_vial = 0, km_vial = 0))
  
  #guardar
  densidad_vial_csv <- municipios_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::select(municipio = shapeName, area_km2, km_vial, densid_vial) 
  
  head(municipios_sf %>% dplyr::select(shapeName, densid_vial), 15)
  head(densidad_vial_csv, 15)
  
  readr::write_csv(densidad_vial_csv, ruta_csv_vial)
} else {
  message("csv/densidad_vial_municipales.csv ya existe, saltando OSM...")
}

#IMAE  ----
message("Fuente: BCN como archivo xlsx")

ruta_imae_excel <- "dataset/Cuadros_de_salida_IMAE.xlsx"

imae_raw <- readxl::read_excel(ruta_imae_excel, 
                               sheet = "IMAE", 
                               skip = 30, 
                               col_names = FALSE) %>%
  setNames(c("anio", "mes", 
             "orig_m", "orig_ia","orig_acum", "orig_prom", "_", 
             "sa_m", "sa_im" ,"sa_ia", "__", 
             "tc_m", "tc_im","tc_ia", "tc_acum_anual"))

#se usa la serie tendencia ciclo
imae_ts_df <- imae_raw %>%
  dplyr::select(tc_m) %>% 
  dplyr::mutate(
    imae = as.numeric(tc_m),
    fecha = seq(from = as.Date("2006-01-01"), by = "month", length.out = dplyr::n())
  ) %>%
  dplyr::select(fecha, imae) %>%
  tidyr::drop_na()


print("nombres imae:")
print(names(imae_ts_df))

print("imae")
print(head(imae_ts_df, 12))


#imae degagregado ---- 
imae_desagregado_raw <- readxl::read_excel(
  ruta_imae_excel,
  sheet = "Activ TC",
  range = "B8:IK27",
  col_names = FALSE   
)

nombres_sectores <- imae_desagregado_raw[[1]]
datos_numericos <- imae_desagregado_raw[, -1]
datos_t <- as.data.frame(t(datos_numericos))
names(datos_t) <- nombres_sectores
datos_t <- datos_t %>% select(-IMAE)
datos_t <- datos_t %>%
  mutate(fecha = seq.Date(from = as.Date("2006-01-01"), 
                          by = "month", 
                          length.out = nrow(datos_t))) %>%
  relocate(fecha) %>%
  clean_names()

#crear variables 
datos_t <- datos_t %>%
  mutate(
    # primario: 5 sectores
    imae_primario = (agricultura + pecuario + silvicultura_y_extraccion_de_madera +
                       pesca_y_acuicultura + explotacion_de_minas_y_canteras) / 5,
    
    # secundario: 3 sectores
    imae_secundario = (industra_manufactura + construccion + energia_y_agua) / 3,
    
    # terciario: 9 sectores
    imae_terciario = (comercio + hoteles_y_restaurantes + transporte_y_comunicaciones +
                        intermediacion_financiera_y_servicios_conexos + propiedad_de_vivenda +
                        administracion_publica_y_defensa + ensenanza + salud + otros_servicios) / 9
  )


#unirficar todo ----
message("unificar todo dog")

df_luces <- readr::read_csv("csv/luces_nocturnas_municipales.csv")
df_vial <- readr::read_csv("csv/densidad_vial_municipales.csv")

#convertir la variable luces a clase date
df_luces <- df_luces %>% 
  dplyr::mutate(fecha = as.Date(date))

#cruzar las luces con la densidad_vial 
panel_acumulado <- df_luces %>%
  dplyr::select(!dplyr::any_of("poblacion_estimada")) %>%
  dplyr::left_join(df_vial, by = c("NAME_2" = "municipio"))

panel_acumulado <- panel_acumulado %>%
  dplyr::left_join(
    dplyr::select(tabla_poblacion_municipal, municipio, poblacion_estimada),
    by = c("NAME_2" = "municipio")
  )

#cruzar el panel 
panel_completo <- panel_acumulado %>%
  dplyr::left_join(imae_ts_df, by = "fecha") %>%
  tidyr::drop_na(imae) 

panel_completo <- panel_completo %>%
  dplyr::left_join(datos_t, by = "fecha") %>%
  tidyr::drop_na()

#multiplicar la densidad_vial fija por el IMAE  del mes correspondiente
panel_completo <- panel_completo %>%
  dplyr::mutate(
    interaccion_causal = densid_vial * imae
  )

readr::write_csv(panel_completo, "csv/panel_completo.csv")

#cvs limpio ----

#recuerdece que interaccion_causal = densid_vial * imae
panel_final <- panel_completo %>%
  dplyr::select(
    municipio = NAME_2, #nombre del municipio 
    fecha,                
    luces_nocturnas, #y
    densid_vial, #infraestructura 
    imae, #shock temporal (tendencia ciclo)
    interaccion_causal, #Tu variable de interés (densidad_vial)
    area_km2, #control geografico
    imae_primario,
    imae_secundario,
    imae_terciario,
    poblacion_estimada
  )

print(names(panel_final))
head(panel_final, 10)

readr::write_csv(panel_final, "csv/panel_final.csv")
