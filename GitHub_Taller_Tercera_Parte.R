# ==============================================================================
# Ciencia de Datos Espaciales Aplicada al Sector Público: Ruteo, Inteligencia Artificial y Autocorrelación Espacial. Parte 3.
# ==============================================================================

# -------------------------------------------------------------------------
# 1. CARGA DE LIBRERÍAS
# -------------------------------------------------------------------------
# 1.1. Primero cargo mis herramientas
library(leaflet)            # Para armar los mapas web interactivos
library(tidyverse)          # Para limpiar y manipular los datos (dplyr, ggplot, etc.)
library(sf)                 # La librería principal para manejar los shapefiles y geometrías
library(readxl)             # Para poder leer las bases de datos en Excel
library(osrm)               # Para conectarme a la API de rutas y calcular distancias/tiempos
library(bivariateLeaflet)   # Para cruzar dos variables en un solo mapa de colores
library(mapedit)            # Para poder dar clic en el mapa y sacar coordenadas a mano
library(usethis)            # Para editar mis variables de entorno (como las contraseñas)
library(curl)               # Para que no truene la conexión de red con la IA
library(ellmer)             # Para traerme a los LLMs (Gemini/GitHub) directo a la consola
library(sfdep)              # Para hacer estadística espacial y sacar el Índice de Moran
library(htmlwidgets)        # Para guardar el mapa como HTML en mi computadora
library(markdown)           # Para poder subir el mapa a RPubs
library(rsconnect)          # Para poder subir el mapa a Posit Connect
library(patchwork)          # Para pegar mapas de ggplot uno al lado del otro

# -------------------------------------------------------------------------
# 2. PROCESAMIENTO DE POLÍGONOS A TRAVÉS DE MUNICIPIOS Y CONAPO
# -------------------------------------------------------------------------
# 2.1. Lectura del Marco Geoestadístico completo que bajé del INEGI
mun = st_read("00mun.shp")

# 2.2. Se filtran solo los municipios de Guanajuato a través de la clave 11 para no saturar la memoria
est_interes = mun %>% 
  filter(CVE_ENT==11)

# 2.3. Se carga el índice de marginación 2020 de CONAPO desde mi Excel
conapo <- read_excel("IMM_2020.xlsx", sheet = "IMM_2020")

# 2.4. Se coloca la tabla de CONAPO a los polígonos del mapa usando la clave municipal.
final = est_interes %>% 
  left_join(y = conapo, by=c("CVEGEO"="CVE_MUN"))

# 2.5. Se pasan las coordenadas de INEGI en metros al estándar global de grados WGS84.
final = final %>% st_transform(4326)


# -------------------------------------------------------------------------
# 3. PRIMER MAPA: POLÍGONOS COLOREADOS Y MAPA BIVARIADO
# -------------------------------------------------------------------------
# 3.1. Se crea un mapa bivariado para ver dónde se cruza el analfabetismo con la falta de agua.
mi_mapa_bivariado <- create_bivariate_map(
  data = final,
  var_1 = "ANALF",
  var_2 = "OVSAE"
)

# 3.2. Se usa saveWidget para crear físicamente el archivo HTML en mi carpeta de trabajo.
saveWidget(mi_mapa_bivariado, file = "mapa_vi.html")


# 3.3. Se crea la paleta de colores "viridis" basada en la población.
colorea = colorNumeric(palette = "viridis",
                       domain = final$POB_TOT)

# 3.4. Relleno los polígonos según cuánta gente vive ahí.
leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addPolygons(data=final,
              color = ~colorea(POB_TOT), 
              fillOpacity = .6,          
              weight = 1)                

# -------------------------------------------------------------------------
# 4. PROCESAMIENTO DE LA UBICACIÓN DE LAS CLÍNICAS Y SPATIAL JOIN
# -------------------------------------------------------------------------
# 4.1. Se lee padrón de establecimientos de salud (CLUES), se quedan únicamente con coordenadas.
clues <- read_excel("ESTABLECIMIENTO_SALUD_202602.xlsx") %>% 
  select(LONGITUD,LATITUD)

# 4.2. Se convierte la tabla en un objeto espacial 
clues_espacial = clues %>% 
  mutate(LONGITUD=as.numeric(LONGITUD),
         LATITUD=as.numeric(LATITUD)) %>% 
  na.omit() %>% 
  st_as_sf(coords = c("LONGITUD","LATITUD"),crs=4326)

# 4.3. Se cruzan los puntos con los polígonos para saber en qué municipio cayó cada clínica mediante un spacial joint.
final_2 = st_join(clues_espacial,final,left=T) %>% filter(!is.na(CVEGEO))

# -------------------------------------------------------------------------
# 5. MAPA CON CLUSTERS DE PUNTOS
# -------------------------------------------------------------------------
# 5.1. Se dibuja el mapa con los polígonos coloreados por población y las clínicas como puntos agrupados en clusters.
leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addPolygons(data=final,
              color = ~colorea(POB_TOT),
              fillOpacity = .6,
              weight = 1) %>% 
  addCircleMarkers(data=final_2,clusterOptions = T)

# -------------------------------------------------------------------------
# 6. RUTEO CON OSRM: ¿CÓMO LLEGA LA GENTE A LA CLÍNICA?
# -------------------------------------------------------------------------
# 6.1. Se calcula el centro geográfico exacto de cada municipio, es decir, mi punto de salida.
origen = final %>% 
  select(NOM_MUN,GM_2020) %>% 
  st_centroid()

# 6.2. Las clínicas que son el punto de destino se les pone un ID único para no perderlas en el join.
destino = final_2 %>% mutate(id=1:n()) %>% 
  select(id)

# 6.3. Se cruzan los centroides con clínicas buscando cuál le queda más cerca a cada quien, siendo esta la distancia lineal.
para_mapa = origen %>% 
  st_join(y = destino,join = st_nearest_feature)

# 6.4. Prueba piloto de la ruta
or = para_mapa %>% head(1)
de = destino %>% filter(id==or$id)

# 6.5. Cálculo la ruta caminando por la calle usando OSRM
ruta = osrmRoute(src = or,dst = de,osrm.profile = "foot")

# 6.6. Se dibuja el resultado con la línea negra marcando la ruta peatonal
leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addCircles(data=or,color="red") %>% 
  addCircles(data=de,color="green") %>% 
  addPolylines(data=ruta,color = "black")

# -------------------------------------------------------------------------
# 7. EL LOOP: RUTEO MASIVO
# -------------------------------------------------------------------------
# 7.1. Se crea un data frame vacío para ir guardando todas las rutas que calcule.
contenedor= data.frame()

# 7.2. Se hace un for loop para iterar sobre todos los municipios.
for (i in para_mapa$NOM_MUN) {
  or = para_mapa %>% filter(NOM_MUN==i)
  de = destino %>% filter(id==or$id)
  
  ruta = osrmRoute(src = or,dst = de,osrm.profile = "foot") 
  contenedor=rbind(contenedor,ruta)
}

# 7.3. Se visualiza el mapa la telaraña de todas las rutas juntas sobre OpenStreetMap.
leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addCircles(data = final_2,color = "red") %>% 
  addCircles(data=para_mapa,color = "blue") %>% 
  addPolylines(data=contenedor, color = "black")

# 7.4. Se hace una gráfica de densidad para ver cómo se distribuyen los tiempos de traslado.
contenedor %>% 
  ggplot(aes(duration))+
  geom_density()

# -------------------------------------------------------------------------
# 8. ISOCRONAS E ISODISTANCIAS
# -------------------------------------------------------------------------
# 8.1. Isocrona básica con el origen anterior en Guanajuato.
iso1 = or %>% 
  osrmIsochrone(breaks = c(5,10,15),
                osrm.profile = "foot",res = 30)

# 8.2. Se plasman las manchas de tiempo en el mapa.
leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(data=iso1,weight = 1,opacity = .5) %>% 
  addCircles(data=or)

# --- 8.3. ISODISTANCIA INTERACTIVA Con mapedit ---
# 8.3.1. Se abre un mapa interactivo para dar un clic donde yo quiera y guardar ese punto.

angel_independencia = mapedit::editMap()

# 8.3.2. Se calculan las manchas físicas de hasta dónde puedo caminar 100, 500 y 1000 metros desde ahí.
iso_dist = angel_independencia %>% 
  osrmIsodistance(
    breaks = c(100, 500, 1000), 
    osrm.profile = "foot",  
    res = 30                  
  )

# 8.3.3. Se visualiza el resultado.
leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(data=iso_dist,weight = 1,opacity = .5) %>% 
  addCircles(data=angel_independencia) %>% 
  addMeasure(primaryLengthUnit = "meters")

# --- 8.4. ISOCRONA: MUSEO NACIONAL DE ANTROPOLOGÍA ---
# 8.4.1. Se fijan las coordenadas del Museo Nacional de Antropología.
museo_antropologia <- data.frame(lon = -99.1863, lat = 19.4260) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# 8.4.2. Se calculan las isocronas.
iso_tiempo <- museo_antropologia %>% 
  osrmIsochrone(
    breaks = c(5, 10, 15),
    osrm.profile = "foot",
    res = 30
  )

# 8.4.3. Se dibujan las manchas de tiempo en el mapa con Leaflet.
leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(data = iso_tiempo, weight = 1, opacity = 0.5, color = "purple") %>% 
  addCircleMarkers(data = museo_antropologia, color = "red", radius = 5, label = "Museo Nacional de Antropología")

# -------------------------------------------------------------------------
# 9. JUGANDO CON LOS LLMs GEMINI Y GITHUB DESDE RSTUDIO
# -------------------------------------------------------------------------
# usethis::edit_r_environ()

# 9.1. Se llama a Gemini y se le da un prompt para que nos lleve la contraria de forma sarcástica.
ayudante = chat_google_gemini(
  system_prompt = "tu tarea es llevarme la contraria y ser un poco sarcástico, pero sin ofenderme. Responde a mis preguntas con chistes, memes o referencias culturales. No me des respuestas serias, quiero divertirme."
)
ayudante$chat("Creo que la democracia es una buena forma para elegir gobernantes porque nos permite participar a todos")

p1 = chat_google_gemini(system_prompt = "tu tarea es contar chistes cortos de la educación pública en México")
p1$chat("Cuéntame un chiste corto de los profesores en México")

# usethis::create_github_token()
prueba = chat_github("cuentame algo de la SEP")

# -------------------------------------------------------------------------
# 10. EXTRACCIÓN DE DATOS ESTRUCTURADOS CON IA DESDE UN PDF
# -------------------------------------------------------------------------
# 10.1. Se sube un PDF a la nube de Google para que la IA pueda leerlo.
archivo = google_upload("hola.pdf")

# 10.2. Se le da un prompt a la IA para que extraiga información estructurada del PDF, como nombres, géneros, gustos, orígenes y coordenadas.

p1_extractor = chat_google_gemini(
  system_prompt = "identifica los nombres, en función de los nombres asigna el genero, obten el gusto y el origen de la persona, integra las coordenadas de donde viene la persona en lat y lng"
)

# 10.3. Se le dice a la IA que la información que extraiga la organice en un tipo de datos específico, con campos para nombre, género, origen, latitud, longitud, nivel educativo y gusto.

type_datos = type_object(
  nombre=type_string("nombre de la persona",required = FALSE),
  genero=type_string("genero de la persona"),
  origen=type_string("de donde viene la persona"),
  lat=type_number("latitud de donde es"),
  lng=type_number("longitud de donde es"),
  nivel_educa=type_string("Agrega el nivel educativo en, básico o superior de la formación acádemica de la persona"),
  gusto=type_string("algo que le gusta a la persona")
)

# 10.4. Se le dice a la IA que el tipo de datos que queremos es un array, es decir, una lista de personas con esos campos.

tipo_final = type_array(type_datos)

# 10.5. Se le pide a la IA que extraiga la información del PDF y la organice en el tipo de datos que le dijimos, es decir, una lista de personas con nombre, género, origen, latitud, longitud, nivel educativo y gusto.

resultado = p1_extractor$chat_structured(archivo,type = tipo_final,"Extraer la información")

# 10.6. Se visualiza la información que extrajo la IA en un mapa de Leaflet, usando las coordenadas que sacó del PDF para colocar los puntos y los nombres como etiquetas.

leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addCircleMarkers(data=resultado,label = ~nombre)

# -------------------------------------------------------------------------
# 11. ESTADÍSTICA ESPACIAL: ÍNDICE DE MORAN Y AUTOCORRELACIÓN
# -------------------------------------------------------------------------
# 11.1. Para este análisis espacial cambio de estado a Puebla (CVE_ENT 21).
est_interes_puebla = mun %>% 
  filter(CVE_ENT==21)

# 11.2. Se hace el mismo proceso de unir la tabla de CONAPO con los municipios de Puebla para tener las variables de interés.

final_puebla = est_interes_puebla %>% 
  left_join(y = conapo,by=c("CVEGEO"="CVE_MUN"))

# 11.3. Se seleccionan las variables que queremos analizar, en este caso el analfabetismo y la falta de agua, junto con el nombre del municipio.

moran_p1 = final_puebla %>% select(NOMGEO,ANALF,SBASC)

# 11.4. Se calcula el índice de Moran para el analfabetismo, creando una nueva variable que es el "lag" o promedio de los vecinos.

m2 = moran_p1 %>% 
  mutate(id=1:n()) %>% 
  mutate(nb = st_contiguity(geometry),
         wt=st_weights(nb),
         lag=st_lag(nb = nb,wt = wt,ANALF))

# 11.5. Se hace un scatter plot para ver la relación entre el analfabetismo de cada municipio y el promedio de analfabetismo de sus vecinos, con una línea de tendencia para ver si hay autocorrelación positiva o negativa.

m2 %>% 
  select(NOMGEO,ANALF,lag) %>% 
  as_tibble() %>% 
  ggplot(aes(lag,ANALF))+
  geom_point() +
  geom_smooth(method = "lm") 

# 11.6. Se calcula el índice de Moran global para ver si hay autocorrelación espacial en el analfabetismo a nivel general.

global_moran(x = m2$ANALF,nb = m2$nb,wt = m2$wt)

# 11.7. Se hace una regresión lineal para ver si el analfabetismo de un municipio se explica por el analfabetismo de sus vecinos, lo que confirmaría la autocorrelación espacial.

lm(m2$lag~m2$ANALF) %>% 
  summary()

# 11.8. Se visualiza el mapa de Puebla con los municipios coloreados por su nivel de analfabetismo y las líneas que conectan a cada municipio con sus vecinos para ver la red de autocorrelación espacial.
ggplot()+
  geom_sf(data=moran_p1)+
  geom_sf(data=m2 %>% 
            st_as_graph(nb = nb,wt = wt) %>% 
            sf::st_as_sf("edges"))+
  theme_bw()

# 11.9. Se hace una visualización comparativa: "Mapa de Calor" vs "Clústeres Espaciales"

mapa_1 <- ggplot(m2) +
  geom_sf(aes(fill = ANALF), lwd = 0.2, color = "white") +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "%") +
  theme_void() +
  labs(title = "1. Niveles de Analfabetismo",
       subtitle = "Distribución cruda por municipio")

mapa_2 <- m2 %>%
  mutate(moran = local_moran(ANALF, nb, wt)) %>% 
  tidyr::unnest(moran) %>% 
  mutate(pysal = ifelse(p_folded_sim <= 0.1, as.character(pysal), NA)) %>% 
  ggplot(aes(fill = pysal)) +
  geom_sf(lwd = 0.2, color = "white") +
  theme_void() +
  scale_fill_manual(values = c("#B20016", "#E27C7C", "#94C9A9", "#24975E"), 
                    na.value = "grey90", 
                    name = "Clústeres LISA") +
  labs(title = "2. Autocorrelación Espacial",
       subtitle = "Focos Rojos y Contagio Territorial",
       caption = "Áreas en gris no presentan autocorrelación significativa")

# 11.10. Se despliegan ambos mapas con la librería patchwork

mapa_1 + mapa_2

# -------------------------------------------------------------------------
# Desplegar en PositCloud
# -------------------------------------------------------------------------

# rsconnect::connectCloudUser()
# rsconnect::deployApp("archivo.html")