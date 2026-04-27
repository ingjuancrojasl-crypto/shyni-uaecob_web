# Incidentes UAECOB Bogotá 2020 — Shiny App

## Descripción

Aplicación web interactiva desarrollada con **Shiny**, **shinydashboard** y **Plotly para R** que visualiza los incidentes atendidos por la Unidad Administrativa Especial Cuerpo Oficial de Bomberos de Bogotá (UAECOB) entre enero y agosto de 2020.

Proyecto desarrollado para el curso **Herramientas y Visualización de Datos** — Fundación Universitaria Los Libertadores.

## Dataset

- **Fuente**: Datos Abiertos del Gobierno Colombiano
- **URL**: https://www.datos.gov.co/
- **Nombre**: Incidentes atendidos por UAECOB — Corte 31 agosto 2020
- **Descripción**: 20.228 registros de incidentes atendidos por el Cuerpo de Bomberos de Bogotá con 42 variables: fechas, localización geográfica, tipo de servicio, causa del incidente y conteos de personas expuestas/afectadas/rescatadas.

## Hallazgos Principales

1. **Prevenciones y activaciones dominan**: Más del 30% de los servicios son prevenciones o activaciones — los incendios reales representan menos del 4% del total.
2. **Tiempo de respuesta mediano: 9 minutos**: Distribución sesgada a la derecha; la mayoría se atienden en menos de 15 minutos.
3. **Estratos 2 y 3 concentran la mayoría**: Coherente con su mayor población. Quemas e incidentes con animales son más frecuentes en estratos bajos.
4. **Caída de incidentes en marzo–abril por COVID-19**: La cuarentena redujo significativamente la actividad registrada.
5. **Suba y Kennedy lideran geográficamente**: Concentran ~20% de todos los incidentes. El perfil de riesgo varía por zona.

## Visualizaciones Implementadas

1. **Barras horizontales** (Viz 1): Comparación de tipos de incidente con selector de top N y orden.
2. **Histograma + Boxplot** (Viz 2): Distribución del tiempo de respuesta con filtro por tipo de servicio.
3. **Heatmap** (Viz 3): Relación estrato socioeconómico × tipo de incidente, con selector de paleta de color.
4. **Línea de tiempo** (Viz 4): Evolución mensual con banda COVID-19 y filtro por tipo de servicio.
5. **Barras apiladas** (Viz 5): Composición de incidentes por localidad y causa, con opción de porcentaje.

## Tecnologías Utilizadas

- **Framework**: Shiny + shinydashboard
- **Lenguaje**: R 4.x
- **Bibliotecas**:
  - `shiny` — framework web reactivo
  - `shinydashboard` — diseño de dashboard
  - `plotly` — visualizaciones interactivas
  - `ggplot2` — gráficos base
  - `dplyr` — manipulación de datos
  - `tidyr` — transformación de datos
  - `readr` — lectura de archivos
  - `stringr` — manejo de texto
  - `lubridate` — manejo de fechas

## Instalación y Ejecución Local

### Requisitos Previos

- R 4.0 o superior
- RStudio (recomendado)

### Instrucciones

```r
# 1. Instalar paquetes (solo la primera vez)
install.packages(c(
  "shiny", "shinydashboard", "dplyr", "ggplot2",
  "plotly", "readr", "stringr", "tidyr", "lubridate"
))

# 2. Abrir app.R en RStudio y hacer clic en "Run App"
# O desde la consola:
shiny::runApp("app.R")
```

Asegúrate de que el CSV `incidentes_uaecob_2020.csv` esté en la **misma carpeta** que `app.R`.

## Despliegue

URL en producción: *(agregar después del deploy en shinyapps.io)*

### Cómo desplegar en shinyapps.io

```r
# Instalar rsconnect
install.packages("rsconnect")

# Configurar cuenta (tokens disponibles en shinyapps.io → Account → Tokens)
rsconnect::setAccountInfo(
  name   = "TU_USUARIO",
  token  = "TU_TOKEN",
  secret = "TU_SECRET"
)

# Desplegar
rsconnect::deployApp(appFiles = c("app.R", "incidentes_uaecob_2020.csv"))
```

## Estructura del Repositorio

```
uaecob-shiny/
├── app.R                          # Aplicación Shiny (UI + Server)
├── incidentes_uaecob_2020.csv     # Dataset
└── README.md                      # Este archivo
```

## Autores

- Nombre Apellido 1
- Nombre Apellido 2

*Fundación Universitaria Los Libertadores — Herramientas y Visualización de Datos*
