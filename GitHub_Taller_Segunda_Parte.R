# ==============================================================================
# Ciencia de Datos Espaciales Aplicada al Sector Público: Visualización de Mapas sobre Seguridad, Justicia y Negocios. Parte 2.
# ==============================================================================

# -------------------------------------------------------------------------
# 1. CARGA DE LIBRERÍAS
# -------------------------------------------------------------------------
library(tidyverse)      # Ecosistema para manipulación de datos (incluye dplyr, stringr y readr)
library(leaflet)        # Motor principal para crear mapas interactivos
library(leaflet.extras) # Extensión de leaflet (necesaria para la función addHeatmap)
library(jsonlite)       # Permite leer datos desde la web (APIs) en formato JSON

# -------------------------------------------------------------------------
# 2. DESCARGA Y PREPARACIÓN DE DATOS (FGJ CDMX)
# -------------------------------------------------------------------------
# read_csv descarga y lee el archivo directamente desde el portal de datos abiertos
fgj = read_csv("https://archivo.datos.cdmx.gob.mx/FGJ/victimas/victimasFGJ_2024.csv")

# Creamos una base más ligera (fgj_corto) conservando solo 3 columnas clave
fgj_corto = fgj %>% 
  select(delito, latitud, longitud) # select() elige columnas específicas

# count() hace un conteo de frecuencias. sort = TRUE ordena de mayor a menor.
fgj_corto %>% 
  count(delito, sort = TRUE)


# ========================================================================================================
# SECCIÓN A: 1. MAPAS EXPLORATORIOS Y DE CALOR DE ROBO A CASA HABITACIÓN, VIOLENCIA FAMILIAR Y DELITOS SEXUALES
# ========================================================================================================

# -------------------------------------------------------------------------
# 1.1. ROBO CASA HABITACIÓN
# -------------------------------------------------------------------------

# 1.1.1. Filtramos la base para crear nuestro primer set de datos espaciales 

d1 = fgj_corto %>% 
  filter(delito == "ROBO A CASA HABITACION SIN VIOLENCIA") %>% # Filtramos texto exacto
  filter(!is.na(latitud)) %>%  # Excluimos filas donde latitud sea nula (NA)
  filter(!is.na(longitud))     # Excluimos filas donde longitud sea nula (NA)

# 1.1.2. Mapa con puntos simples

leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% # Agrega un mapa base limpio en grises
  addCircles(                             # Dibuja un círculo por cada fila de la tabla
    data = d1,                            # Indicamos qué base de datos usar
    lng = ~longitud,                      # La tilde (~) le dice a R que busque esa columna en la base
    lat = ~latitud                        # Asignamos la columna de latitud
  )

# 1.1.3. Mapa con agrupación de clusters

leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addCircleMarkers(                       # Usamos marcadores de círculo (tienen mejor interactividad)
    data = d1,
    lng = ~longitud, 
    lat = ~latitud,
    clusterOptions = markerClusterOptions() # Agrupa los puntos cercanos en un globo numérico dinámico
  )

# 1.1.4. Mapa multicapa de calor con clusters

leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addHeatmap(                             # Capa 1: Mapa de calor (requiere leaflet.extras)
    data = d1,
    lng = ~longitud,
    lat = ~latitud,
    radius = 60                           # Define qué tan grande se dibuja la mancha de calor por cada punto
  ) %>% 
  addCircleMarkers(                       # Capa 2: Agregamos los clusters encima del calor
    data = d1,
    lng = ~longitud,
    lat = ~latitud,
    clusterOptions = markerClusterOptions() 
  )

# -------------------------------------------------------------------------
# 2. VIOLENCIA FAMILIAR
# -------------------------------------------------------------------------

# 2,1, Filtramos la base para crear nuestro segundo set de datos espaciales 

d2 = fgj_corto %>% 
  filter(delito == "VIOLENCIA FAMILIAR") %>% # Filtramos texto exacto
  filter(!is.na(latitud)) %>%  # Excluimos filas donde latitud sea nula (NA)
  filter(!is.na(longitud))     # Excluimos filas donde longitud sea nula (NA)

# 2.2. Mapa básico con puntos simples

leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% # Agrega un mapa base limpio en grises
  addCircles(                             # Dibuja un círculo por cada fila de la tabla
    data = d2,                            # Indicamos qué base de datos usar
    lng = ~longitud,                      # La tilde (~) le dice a R que busque esa columna en la base
    lat = ~latitud                        # Asignamos la columna de latitud
  )

# 2.3. Mapa con agrupación de clusters

leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addCircleMarkers(                       # Usamos marcadores de círculo (tienen mejor interactividad)
    data = d2,
    lng = ~longitud, 
    lat = ~latitud,
    clusterOptions = markerClusterOptions() # Agrupa los puntos cercanos en un globo numérico dinámico
  )

# 2.4. Mapa de calor con clusters

leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addHeatmap(                             # Capa 1: Mapa de calor (requiere leaflet.extras)
    data = d2,
    lng = ~longitud,
    lat = ~latitud,
    radius = 60                           # Define qué tan grande se dibuja la mancha de calor por cada punto
  ) %>% 
  addCircleMarkers(                       # Capa 2: Agregamos los clusters encima del calor
    data = d2,
    lng = ~longitud,
    lat = ~latitud,
    clusterOptions = markerClusterOptions() 
  )

# -------------------------------------------------------------------------
# 3. DELITOS SEXUALES
# -------------------------------------------------------------------------

# 3.1. Preparamos una nueva base enfocada en delitos sexuales
base_limpia <- fgj %>% 
  select(delito, latitud, longitud) %>% 
  filter(str_detect(delito, "SEX")) %>%   # str_detect() busca si la palabra "SEX" está en alguna parte del texto
  na.omit()                               # na.omit() borra cualquier fila incompleta rápidamente

# 3.2. Mapa básico con puntos simples

leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% # Agrega un mapa base limpio en grises
  addCircles(                             # Dibuja un círculo por cada fila de la tabla
    data = base_limpia,                            # Indicamos qué base de datos usar
    lng = ~longitud,                      # La tilde (~) le dice a R que busque esa columna en la base
    lat = ~latitud                        # Asignamos la columna de latitud
  )

# 3.3. Mapa con agrupación de clusters

leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addCircleMarkers(                       # Usamos marcadores de círculo (tienen mejor interactividad)
    data = base_limpia,
    lng = ~longitud, 
    lat = ~latitud,
    clusterOptions = markerClusterOptions() # Agrupa los puntos cercanos en un globo numérico dinámico
  )


# 3.4. Mapa de calor con clusters

leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addHeatmap(                             # Capa 1: Mapa de calor (requiere leaflet.extras)
    data = base_limpia,
    lng = ~longitud,
    lat = ~latitud,
    radius = 60                           # Define qué tan grande se dibuja la mancha de calor por cada punto
  ) %>% 
  addCircleMarkers(                       # Capa 2: Agregamos los clusters encima del calor
    data = base_limpia,
    lng = ~longitud,
    lat = ~latitud,
    clusterOptions = markerClusterOptions() 
  )


# ==============================================================================
# SECCIÓN B: 1. MAPA SATELITAL DE NEGOCIOS CON LA API DEL DENUE DE INEGI
# ==============================================================================

# 1.1. Descarga de datos desde la API del DENUE (Directorio Estadístico Nacional de Unidades Económicas)

# fromJSON() hace una petición web a la API del INEGI y convierte el resultado en una tabla de R
# En este caso, buscamos bancos en un radio de 500 metros alrededor del Museo Soumaya en CDMX.

soumaya <- fromJSON("https://www.inegi.org.mx/app/api/denue/v1/consulta/Buscar/bancos/19.4408544561198,-99.20399200125259/500/5782893c-bfe7-42da-8ee4-02d141d52797")

# 1.2. Preparación de datos para el mapa

# as.numeric() los convierte a formato matemático para que el mapa pueda leerlos.

soumaya$Longitud <- as.numeric(soumaya$Longitud)
soumaya$Latitud <- as.numeric(soumaya$Latitud)

# 1.3 Creamos un mapa con vista satelital para ubicar los negocios económicamente
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>% # Cambiamos el mapa base a fotografía satelital
  addCircles(
    data = soumaya, 
    lng = ~Longitud,     
    lat = ~Latitud,      
    color = "red",       # Pintamos los puntos de rojo para generar contraste con el satélite
    label = ~paste(Nombre, "-", Clase_actividad) # Usamos paste() para unir la columna Nombre y Clase_actividad separadas por un guion, al pasar el cursor, mostrará el nombre y la actividad económica del negocio
  )

# ==============================================================================
# NOTAS PARA EXPERIMENTACIÓN: OTRAS COORDENADAS Y RADIOS
# ==============================================================================
# Los siguientes enlaces y coordenadas son parámetros distintos para la API. 
# Si quieres ver otras zonas, solo tienes que copiar la URL (o los números) 
# y reemplazarlos en la función fromJSON() de la Parte 3. 
# La estructura de la URL del INEGI es: .../Buscar/palabra_clave/latitud,longitud/metros_de_radio/token

# Coordenadas de referencia para el Estadio BBVA en Monterrey:
# 25.669295827488895, -100.24388500476128

# Farmacias alrededor del Ángel de la Independencia con un radio de 750 metros:
# https://www.inegi.org.mx/app/api/denue/v1/consulta/Buscar/farmacia/19.427141997797538,-99.16727748910374/2500/5782893c-bfe7-42da-8ee4-02d141d52797

# Restaurantes alrededor de la Piramide de Chichen Itzá en Yucatán con un radio de 1000 metros:
# https://www.inegi.org.mx/app/api/denue/v1/consulta/Buscar/restaurante/20.68319666510934,-88.56821995095045/5000/5782893c-bfe7-42da-8ee4-02d141d52797


# -------------------------------------------------------------------------
# Desplegar en PositCloud
# -------------------------------------------------------------------------

# rsconnect::connectCloudUser()
# rsconnect::deployApp("archivo.html")
  
