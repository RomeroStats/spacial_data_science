# ==============================================================================
# Ciencia de Datos Espaciales Aplicada al Sector Público: Visualización. Parte 1.
# ==============================================================================

# -------------------------------------------------------------------------
# 1. CARGA DE LIBRERÍAS
# -------------------------------------------------------------------------
library(tidyverse) # Ecosistema de manipulación de datos
library(leaflet)   # Mapas interactivos
library(readxl)    # Para leer archivos de Excel (.xlsx)

# ==============================================================================
# SECCIÓN A: 1. DATOS ABIERTOS FGJ CDMX
# ==============================================================================

# 1.1. Descarga de datos abiertos de la FGJ CDMX (2024)
fgj <- read_csv("https://archivo.datos.cdmx.gob.mx/FGJ/victimas/victimasFGJ_2024.csv")

# 1.2. Selección de las columnas de interés
fgj_corto <- fgj %>% 
  select(delito, latitud, longitud)

# 1.3. Exploración los delitos más frecuentes 

fgj_corto %>% count(delito, sort = TRUE)

# 1.4. Filtro para hacer un mapa específico
d1 <- fgj_corto %>% 
  filter(delito == "ROBO DE ACCESORIOS DE AUTO") %>% 
  filter(!is.na(latitud), !is.na(longitud))

# 1.4.1. Mapa A.1: Básico con puntos simples
leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addCircles(
    data = d1,
    lng = ~longitud,
    lat = ~latitud
  )

# 1.4.2. Mapa A.2: Avanzado con agrupación de clusters
leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addCircleMarkers(
    data = d1,
    lng = ~longitud,
    lat = ~latitud,
    clusterOptions = markerClusterOptions() 
  )


# ==============================================================================
# SECCIÓN B: 1. DATOS OCVED
# Base de datos original: Organized Crime Violence in Mexico (OCVED)
# Créditos y fuente: Javier Osorio (https://github.com/javierosorio/OCVED_2.0)
# ==============================================================================

# 1.1. Cargar base de datos desde la ruta local
ocved <- read_xlsx("~/Desktop/Rstudio Spacial Data Analysis Lab/OCVED_2.0.xlsx") %>% 
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

# 1.2. Creamos la paleta de colores con colorFactor para variables categóricas
paleta_actores <- colorFactor(
  palette = "Set1",          
  domain = ocved$actor_main  
)

# -------------------------------------------------------------------------
# 2. VISUALIZACIÓN AVANZADA CON LEAFLET
# -------------------------------------------------------------------------
# 2.1. Mapa interactivo con mejoras estéticas
leaflet(data = ocved) %>% 
  
  # Fondo oscuro para resaltar los colores categóricos
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    group = ~actor_main,
    
    # Aplicación de la paleta de colores
    color = ~paleta_actores(actor_main), 
    fillColor = ~paleta_actores(actor_main), 
    
    radius = 5,          
    
    # Borde blanco delgado para separar puntos empalmados
    stroke = TRUE,       
    weight = 1,          
    opacity = 1,         
    fillOpacity = 0.8,
    
    # Etiquetas interactivas al pasar el cursor
    label = ~paste("Actor:", actor_main) 
  ) %>%
  
  # Controles de capas para filtrar visualmente en vivo
  addLayersControl(
    overlayGroups = unique(ocved$actor_main), 
    options = layersControlOptions(collapsed = FALSE) 
  ) %>%
  
  # Agregamos la leyenda en la esquina inferior derecha
  addLegend(
    position = "bottomleft",
    pal = paleta_actores,      
    values = ~actor_main,      
    title = "Tipo de Actor (OCVED)",   
    opacity = 1
  )

# -------------------------------------------------------------------------
# Desplegar en PositCloud
# -------------------------------------------------------------------------

# rsconnect::connectCloudUser()
# rsconnect::deployApp("archivo.html")