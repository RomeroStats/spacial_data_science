# Ciencia de Datos Espaciales Aplicada al Sector Público
Este repositorio contiene código que forma parte del taller de RStudio con Enfoque Espacial del seminario interinstitucional: Instituciones de seguridad y justicia del Instituto Mora, CIDE, IIS - UNAM y el Colegio Mexiquense.

---

##  Agradecimientos
Expreso un agradecimiento profundo al **Mtro. Noé Osorio García**, instructor de este taller, por compartir su metodología y experiencia en el uso de herramientas aplicadas al análisis geográfico para las políticas públicas. Su guía fue fundamental para el desarrollo de cada una de las fases presentadas en este repositorio.

---

## Archivos y Bases de Datos
Para facilitar la replicabilidad de los análisis, en este repositorio podrás encontrar:
* **Código Fuente:** Subí los archivos de script en formato `.R` correspondientes a la Parte 1, Parte 2 y Parte 3 para su consulta directa y ejecución.
* **Datos:** Se han agregado algunas de las bases de datos necesarias para correr los ejercicios prácticos.
* *Nota sobre OCVED:* La base de datos *Organized Crime Violence in Mexico* (OCVED 2.0) no está alojada en estas carpetas, pero la puedes consultar y descargar a través de su enlace oficial en la sección de Créditos.

---

## Estructura del Proyecto

El taller lo dividí en tres partes:

### [Parte 1: Cartografía e Incidencia Delictiva](https://019d5020-dcc1-6298-8851-14a0b5586fea.share.connect.posit.cloud/)
* **Enfoque:** Procesamiento de datos abiertos de la FGJ CDMX y de la base de datos OCVED 2.0.
* **Técnicas:** Limpieza con `tidyverse`, visualización de puntos simples y agrupación dinámica mediante clústeres.
* **Producto:** Mapa interactivo de delitos patrimoniales 2024 y de presencia de grupos armados en territorio mexicano.

### [Parte 2: Seguridad, Justicia y Negocios](https://019d4736-3ddf-959e-853a-fad1ca68134f.share.connect.posit.cloud/)
* **Enfoque:** Detección de focos rojos y entorno económico.
* **Técnicas:** Mapas de calor, capas satelitales (`Esri.WorldImagery`) y conexión en tiempo real con la **API DENUE de INEGI**.
* **Producto:** Análisis multicapa de riesgos y establecimientos comerciales.

### [Parte 3: Accesibilidad, Brechas Sociales e Inteligencia Artificial](https://019d4739-bb16-891e-d299-0e622342683c.share.connect.posit.cloud/)
* **Enfoque:** Geoestadística avanzada y Modelos de Lenguaje LLMs.
* **Técnicas:** Ruteo peatonal con **OSRM**, Isocronas de tiempo, Isodistancias, extracción de datos estructurados con **Gemini AI** y Autocorrelación Espacial (**LISA - Índice de Moran**).
* **Producto:** Evaluación de accesibilidad a servicios de salud y detección de clústeres de vulnerabilidad.

---

## Créditos
El conjunto de datos especializados OCVED 2.0 utilizados en la parte 1 de este taller se fundamentan en el trabajo de:

* **Dr. Javier Osorio**
* **Proyecto:** *Organized Crime Violence in Mexico* (OCVED 2.0).
* [Repositorio Oficial OCVED](https://github.com/javierosorio/OCVED_2.0)

---

## Autor
**Mtro. José César Romero Galván** *Científico de Datos y Especialista en Política Pública*

Si tienes dudas sobre el código o quieres colaborar en proyectos de Ciencia de Datos y Seguridad, puedes contactarme a través de:

* **GitHub:** [RomeroStats](https://github.com/RomeroStats)
* **LinkedIn:** [César Romero](https://www.linkedin.com/in/c%C3%A9sar-romero-ba09b3163/)
* **cesar_romero@politicas.unam.mx**

---
