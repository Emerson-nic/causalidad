# %% librerias
#para cargar dashboard en la terminal usar streamlit run dashboard/app.py
import streamlit as st
import pandas as pd
import numpy as np
import altair as alt
import folium
import json
from streamlit_folium import st_folium
from scipy.stats import linregress, pearsonr
from pathlib import Path


st.set_page_config(
    page_title="Dashboard - Resiliencia Vial Nicaragua",
    page_icon="\U0001F3D4",
    layout="wide"
)

# %% cargar datos
@st.cache_data
def cargar_panel():
    rutas = [
        Path("csv/panel_final_limpio.csv"),
        Path("../csv/panel_final_limpio.csv"),
    ]
    for ruta in rutas:
        if ruta.exists():
            return pd.read_csv(ruta, parse_dates=["fecha"])
    raise FileNotFoundError(
        "no se encontro 'csv/panel_final_limpio.csv'. "
        "ejecuta primero 'python limpieza.py'"
    )

@st.cache_data
def cargar_geojson():
    rutas = ["dataset/geoBoundaries-NIC-ADM2.geojson",
             "../dataset/geoBoundaries-NIC-ADM2.geojson"]
    for r in rutas:
        if Path(r).exists():
            with open(r) as f:
                return json.load(f)
    raise FileNotFoundError("no se encontro el GeoJSON en ninguna ruta")

try:
    df = cargar_panel()
    geo = cargar_geojson()
except FileNotFoundError as e:
    st.error(f"\U0000274C {e}")
    st.stop()

# %%helpers geo
def sacar_centroide(geometry):
    if geometry["type"] == "Polygon":
        coords = geometry["coordinates"][0]
    elif geometry["type"] == "MultiPolygon":
        coords = max(geometry["coordinates"], key=lambda x: len(x[0]))[0]
    else:
        return None, None
    xs = [c[0] for c in coords]
    ys = [c[1] for c in coords]
    return np.mean(ys), np.mean(xs)

def encontrar_municipio(lat, lon, centroides):
    mejor = None
    mejor_dist = float("inf")
    for nombre, (clat, clon) in centroides.items():
        dist = (lat - clat) ** 2 + (lon - clon) ** 2
        if dist < mejor_dist:
            mejor_dist = dist
            mejor = nombre
    return mejor

centroides = {}
for feat in geo["features"]:
    nombre = feat["properties"]["shapeName"]
    lat, lon = sacar_centroide(feat["geometry"])
    if lat is not None:
        centroides[nombre] = (lat, lon)

# %%session state
municipios = sorted(df["municipio"].unique())

if "municipio" not in st.session_state or st.session_state.municipio not in municipios:
    st.session_state.municipio = municipios[0]

#titulo
st.title("Infraestructura Vial como Amortiguador Cíclico en Nicaragua")
st.markdown(
    "Dashboard de panel espacial: luces nocturnas (NASA VIIRIS) "
    "vs ciclo macro (IMAE) en 153 municipios de Nicaragua. "
    "Haz clic en el mapa para seleccionar un municipio."
)

#sidebar
st.sidebar.header("Filtros y Navegacion")
st.sidebar.markdown("**Autor:** Kuin")
st.sidebar.markdown("**Metodologia:** Datos de Panel Espacial / NASA VIIRS")

def on_cambio_municipio():
    st.session_state.municipio = st.session_state.dropdown_muni

st.session_state["dropdown_muni"] = st.session_state.municipio
municipio_sel = st.sidebar.selectbox(
    "Selecciona un Municipio:", municipios, key="dropdown_muni",
    on_change=on_cambio_municipio
)

fecha_min = df["fecha"].min().date()
fecha_max = df["fecha"].max().date()
fecha_rango = st.sidebar.slider(
    "Ventana temporal",
    min_value=fecha_min, max_value=fecha_max,
    value=(fecha_min, fecha_max), format="YYYY-MM"
)

with st.sidebar.expander("Municipios por Estrato"):
    st.markdown("Clasificacion por terciles de radiancia media.")
    st.markdown("")
    for estrato in ["Estrato Alto", "Estrato Medio", "Estrato Bajo"]:
        lista = sorted(df[df["categoria"] == estrato]["municipio"].unique())
        st.markdown(f"**{estrato}** ({len(lista)}):")
        st.markdown(", ".join(lista))
        st.markdown("")

#filtro
mask = (
    (df["municipio"] == municipio_sel)
    & (df["fecha"].dt.date >= fecha_rango[0])
    & (df["fecha"].dt.date <= fecha_rango[1])
)
df_mun = df.loc[mask].copy().sort_values("fecha")

if df_mun.empty:
    st.warning("Sin datos para este municipio en esa ventana temporal.")
    st.stop()

# %% metricas
fila0 = df_mun.iloc[0]
densidad_vial = fila0["densid_vial"]
poblacion = fila0["poblacion_estimada"]
area = fila0["area_km2"]
densidad_demo = poblacion / area if area > 0 else 0
categoria = fila0["categoria"]

if len(df_mun) > 1:
    r_pearson, p_valor = pearsonr(df_mun["luces_nocturnas"], df_mun["imae"])
    if p_valor < 0.01:
        estrellas = "***"
    elif p_valor < 0.05:
        estrellas = "**"
    elif p_valor < 0.1:
        estrellas = "*"
    else:
        estrellas = ""
else:
    r_pearson = np.nan
    p_valor = np.nan
    estrellas = ""

col1, col2, col3, col4 = st.columns(4)

with col1:
    st.metric("Densidad Vial", f"{densidad_vial:.4f} km/km2")

with col2:
    st.metric(
        "Densidad Demografica",
        f"{densidad_demo:,.1f} hab/km2",
        help=f"Poblacion: {poblacion:,.0f}  |  Area: {area:,.2f} km2"
    )

with col3:
    if not np.isnan(r_pearson):
        st.metric(
            "Correlacion Luces vs IMAE",
            f"{r_pearson:.4f}{estrellas}",
            help=f"r = {r_pearson:.4f}  |  p-valor = {p_valor:.4f}  |  n = {len(df_mun)} meses"
        )
    else:
        st.metric("Correlacion Luces vs IMAE", "N/D")

with col4:
    st.metric("Estrato de Desarrollo", categoria)

# %% tabs
tab1, tab2, tab3, tab4 = st.tabs([
    "Mapa Interactivo",
    "Evolucion Temporal",
    "Elasticidad Ciclica",
    "Datos Crudos"
])

#mapa
with tab1:
    st.markdown("**Mapa de Municipios de Nicaragua — haz clic en cualquier municipio**")

    if municipio_sel in centroides:
        lat_m, lon_m = centroides[municipio_sel]
        m = folium.Map(location=[lat_m, lon_m], zoom_start=9, tiles="cartodbpositron")
    else:
        m = folium.Map(location=[12.8654, -85.2072], zoom_start=7, tiles="cartodbpositron")

    def estilo(feature):
        name = feature["properties"]["shapeName"]
        if name == municipio_sel:
            return {"fillColor": "#e67e22", "color": "#e67e22", "weight": 2.5, "fillOpacity": 0.4}
        return {"fillColor": "white", "color": "gray", "weight": 1, "fillOpacity": 0.15}

    def resaltar(feature):
        return {"fillColor": "#e67e22", "color": "#e67e22", "weight": 2, "fillOpacity": 0.5}

    folium.GeoJson(
        geo,
        style_function=estilo,
        highlight_function=resaltar,
        tooltip=folium.GeoJsonTooltip(
            fields=["shapeName"],
            aliases=["Municipio:"],
            style="font-size: 13px; font-weight: bold;"
        )
    ).add_to(m)

    if municipio_sel in centroides:
        lat_c, lon_c = centroides[municipio_sel]
        folium.Marker(
            location=[lat_c, lon_c],
            popup=(
                f"<b>{municipio_sel}</b><br>"
                f"Densidad Vial: {densidad_vial:.4f} km/km2<br>"
                f"Poblacion: {poblacion:,.0f}<br>"
                f"Correlacion: {r_pearson:.4f}{estrellas}<br>"
                f"Estrato: {categoria}"
            ),
            tooltip=municipio_sel,
            icon=folium.Icon(color="red", icon="info-sign")
        ).add_to(m)

    folium.LatLngPopup().add_to(m)

    output = st_folium(m, width=None, height=550)

    clicked = None
    if output and output.get("last_object_clicked"):
        props = output["last_object_clicked"]
        if "shapeName" in props:
            clicked = props["shapeName"]
    if not clicked and output and output.get("last_clicked"):
        lat = output["last_clicked"]["lat"]
        lng = output["last_clicked"]["lng"]
        clicked = encontrar_municipio(lat, lng, centroides)
    if clicked and clicked != st.session_state.municipio:
        st.session_state.municipio = clicked
        st.rerun()

#tab 2 serie temporal
with tab2:
    base = alt.Chart(df_mun).encode(
        x=alt.X("fecha:T", title="", axis=alt.Axis(format="%Y-%m"))
    )

    l1 = base.mark_line(color="#1f77b4", strokeWidth=2, point=True).encode(
        y=alt.Y("luces_nocturnas:Q", title="Luces Nocturnas (radiancia)",
                axis=alt.Axis(titleColor="#1f77b4", titlePadding=10))
    )

    l2 = base.mark_line(color="#d62728", strokeWidth=2, point=True).encode(
        y=alt.Y("imae:Q", title="IMAE Nacional",
                axis=alt.Axis(titleColor="#d62728", titlePadding=10))
    )

    dual = alt.layer(l1, l2).resolve_scale(y="independent").properties(
        title=f"Evolucion Mensual - {municipio_sel}", height=450
    ).interactive()

    st.altair_chart(dual, use_container_width=True)

#ols log-log normal este esta desegradado a diferencia al global
#el de r usa todas la obs y datos de panel aqui no
with tab3:
    valido = df_mun[(df_mun["luces_nocturnas"] > 0) & (df_mun["imae"] > 0)].copy()

    if len(valido) > 2:
        valido["log_luces"] = np.log(valido["luces_nocturnas"])
        valido["log_imae"] = np.log(valido["imae"])

        slope, intercept, r_val, p_ols, _ = linregress(valido["log_luces"], valido["log_imae"])

        if p_ols < 0.01:
            sig_ols = "***"
        elif p_ols < 0.05:
            sig_ols = "**"
        elif p_ols < 0.1:
            sig_ols = "*"
        else:
            sig_ols = ""

        x_vals = np.linspace(valido["log_luces"].min(), valido["log_luces"].max(), 100)
        line_df = pd.DataFrame({"log_luces": x_vals, "log_imae": intercept + slope * x_vals})

        pts = alt.Chart(valido).mark_circle(size=50, opacity=0.6, color="#2ca02c").encode(
            x=alt.X("log_luces:Q", title="log(Luces Nocturnas)"),
            y=alt.Y("log_imae:Q", title="log(IMAE)")
        )

        reg = alt.Chart(line_df).mark_line(color="red", strokeWidth=2.5).encode(
            x="log_luces:Q", y="log_imae:Q"
        )

        scatter = (pts + reg).properties(
            title=f"Elasticidad - {municipio_sel}  (beta = {slope:.4f}{sig_ols}, R2 = {r_val**2:.4f})",
            height=450
        ).interactive()

        st.altair_chart(scatter, use_container_width=True)
    else:
        st.info("No hay datos suficientes (>2 obs con luces>0) para el scatter.")

#tab 4 tablas
with tab4:
    with st.expander("Ver datos filtrados", expanded=False):
        cols_show = [
            "municipio", "fecha", "luces_nocturnas", "imae",
            "densid_vial", "area_km2", "poblacion_estimada", "categoria"
        ]
        df_mostrar = df_mun[cols_show].copy()
        df_mostrar.columns = [
            "Municipio", "Fecha", "Luces Nocturnas", "IMAE",
            "Densidad Vial", "Area (km2)", "Poblacion", "Estrato"
        ]
        st.dataframe(df_mostrar, use_container_width=True, hide_index=True)

        csv_data = df_mostrar.to_csv(index=False).encode("utf-8")
        st.download_button(
            label="Descargar CSV",
            data=csv_data,
            file_name=f"{municipio_sel.replace(' ', '_')}_datos.csv",
            mime="text/csv"
        )

#notas
with st.expander("Notas metodologicas"):
    st.markdown(
        "**Estratos de desarrollo:** Se construyeron dividiendo los 153 municipios "
        "en tres grupos del mismo tamano (terciles) segun su radiancia nocturna "
        "promedio historica. No son categorias oficiales."
    )
    st.markdown(
        "**Correlacion de Pearson (r):** Mide la asociacion lineal entre "
        "las luces nocturnas del municipio y el IMAE nacional en la ventana "
        "de tiempo seleccionada. Las estrellas indican significancia estadistica: "
        "*** p<0.01, ** p<0.05, * p<0.1."
    )
    st.markdown(
        "**Filtro de ventana temporal:** Permite aislar periodos "
        "especificos (crisis, recuperacion, estabilidad) para "
        "observar como cambia la relacion en distintos contextos macro."
    )

with st.expander("Fuentes de Datos y Especificaciones", expanded=False):
    st.markdown(
        """
        **Panel Espacial construido con las siguientes fuentes:**

        * **Limites Geograficos (ADM2):** Proyecto *geoBoundaries* (SNA Lab), Universidad William & Mary (EE.UU.).
        * **Demografia (Poblacion Estimada):** Proyecto *WorldPop* (nic_ppp_2020_UNadj), ajustado por la ONU. Universidad de Southampton (Reino Unido).
        * **Luces Nocturnas (Variable proxy Y):** Satelite Suomi-NPP / VIIRS (Producto: Black Marble VNP46A3/VNP46A4). Extraido mediante la API de la NASA (EE.UU.).
        * **Densidad Vial (Variable X):** Datos extraidos de *OpenStreetMap (OSM)* calculando los kilometros lineales de carretera sobre el area del municipio.
        * **IMAE (Control Macroeconomico):** Banco Central de Nicaragua (BCN). Serie de Tendencia Ciclo global y desagregada.
        """
    )

with st.expander("Notas Metodologicas"):
    st.markdown(
        """
        * **Estratos de desarrollo:** Se construyeron dividiendo los 153 municipios en tres grupos del mismo tamano (terciles) segun su radiancia nocturna promedio historica. No son categorias oficiales.
        * **Regresion OLS (log-log):** Estima la elasticidad local entre log(luces) y log(IMAE) por MCO.. *Nota: Este análisis es de series de tiempo univariada para el municipio seleccionado, a diferencia del modelo principal del paper que utiliza la estructura completa de datos de panel espacial.*
        * **Filtro temporal:** Permite aislar periodos para observar la elasticidad ciclica en distintos contextos macroeconomicos.
        """
    )