# Infraestructura Vial como Amortiguador Cíclico

Este repositorio contiene el código y los datos utilizados en el trabajo *"Infraestructura vial como amortiguador cíclico: evidencia municipal para Nicaragua a partir de imágenes satelitales"*.

## Resumen

El estudio estima cómo la densidad de la red vial modula la transmisión de los shocks macroeconómicos hacia la actividad económica local, medida a través de la luminosidad nocturna satelital (VIIRS Black Marble). Se construye un panel mensual para los 153 municipios de Nicaragua entre enero de 2020 y diciembre de 2025 (11,015 observaciones). La estrategia de identificación combina un diseño shift-share (Bartik) con efectos fijos municipio-mes en un modelo Poisson pseudo-maximum likelihood (PPML), complementado con un modelo jerárquico bayesiano (Bambi) que estima coeficientes municipales individuales.

## Resultados principales

- Un aumento de una desviación estándar en el producto nacional (IMAE) se asocia con un incremento de 9.7 % en la luminosidad municipal.
- Cada desviación estándar adicional en la densidad vial reduce el impacto del shock en 1.5 puntos porcentuales (de 9.7 % a 8.2 %), consistente con un efecto amortiguador.
- El Canal Área (capturado por la interacción IMAE - área municipal) no es estadísticamente significativo, lo que sugiere que el efecto amortiguador opera a través de la conectividad y no de la extensión territorial.
- El efecto es heterogéneo: los municipios de baja densidad vial (estratos históricos 1 y 2) absorben una proporción mayor del shock, mientras que los de alta densidad muestran menor transmisión.
- Los resultados son robustos a la exclusión de observaciones influyentes, a especificaciones log-lineales, a la censura por saturación del sensor y a una prueba de placebo con permutación espacial.

## Requisitos

- R (>= 4.2) con los paquetes: `fixest`, `lfe`, `broom`, `modelsummary`, `tidyverse`, `sf`, `geodata`, `zoo`.
- Python (>= 3.10) con: `pandas`, `numpy`, `bambi`, `arviz`, `matplotlib`, `seaborn`, `blackmarbler`.
- La compilación del documento requiere Quarto y una distribución de LaTeX (TeX Live o MikTeX).

## Reproducir los resultados

1.  Clonar el repositorio:

        git clone https://github.com/Emerson-nic/road-infrastructure-nicaragua-viirs.git
        cd road-infrastructure-nicaragua-viirs

2.  Abrir el proyecto en RStudio o establecer el directorio de trabajo en la raíz del repositorio.

3.  Ejecutar los scripts en orden:

    - `Scripts/obtener_datos.R`: construye el panel a partir de VIIRS Black Marble (API de NASA), red vial de OSM y el IMAE del BCN. Requiere una clave de API de NASA (`bearer` en `.Renviron`).
    - `Scripts/modelo_twfe.R`: estima los modelos TWFE, PPML y placebo; genera las tablas de resultados y las figuras.
    - `Scripts/coeficientes_municipales.py`: estima el modelo jerárquico bayesiano con Bambi y produce los mapas de coeficientes municipales.

4.  Compilar el documento:

    quarto render paper/paper_densidad_vial.qmd --to pdf

    El PDF se generará en `paper/paper_densidad_vial.pdf`.

## Estructura del repositorio

    actividad_economica_via/
    ├── csv/ # Datos procesados (panel_final.csv)
    ├── dataset/ # Fuentes originales descargadas
    ├── Graficos/ # Figuras generadas por los scripts
    ├── paper/ # Documento .qmd, referencias .bib y PDF
    ├── Scripts/ # Código de estimación y procesamiento
    └── README.md

## Citación

Lopez, E. (2026). *Infraestructura vial como amortiguador cíclico: evidencia municipal para Nicaragua a partir de imágenes satelitales*. 
