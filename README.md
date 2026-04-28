# 🚒 UAECOB Bogotá — Dashboard de Incidentes 2020 (Shiny R)

Dashboard interactivo profesional desarrollado en R Shiny para visualizar los incidentes atendidos por el Cuerpo Oficial de Bomberos de Bogotá D.C. durante el periodo enero–agosto 2020.

## Dataset

- **Fuente:** Unidad Administrativa Especial Cuerpo Oficial de Bomberos Bogotá (UAECOB)
- **URL:** https://datosabiertos.bogota.gov.co/dataset/incidente-atendido-por-bomberos
- **Descripción:** 20.228 registros de incidentes con 41 variables: fecha, localidad, estrato, tipo de servicio, víctimas (heridos, rescatados), tiempo de respuesta y coordenadas geográficas.
- **Periodo:** Enero – Agosto 2020 (junio sin registros en el dataset fuente)

## Hallazgos Principales

1. **Patrón semanal con pico el viernes:** Los viernes concentran la mayor carga operativa (3.094 incidentes, +15% vs domingo). Miércoles a viernes representan el período de mayor demanda.
2. **Brecha territorial en tiempos de respuesta:** Los Mártires responde en 6 min (mediana); Ciudad Bolívar y Rafael Uribe tardan 11 min. La brecha de 5 minutos evidencia inequidad en cobertura de estaciones.
3. **55% atendido en menos de 10 minutos:** El desempeño general es satisfactorio, pero el 4,6% con tiempos > 30 min (~930 eventos) requiere atención con alertas automáticas de apoyo.
4. **B-5 Kennedy bajo máxima presión:** Con 1.905 incidentes, casi duplica la carga de la estación de menor demanda. Se requiere estudio de capacidad vs. demanda.
5. **Suba y Kennedy lideran en heridos:** Concentran el 20,4% de los 1.316 heridos del periodo. Requieren dotación prioritaria de paramédicos bomberos.

## Visualizaciones Implementadas

| Graf. | Tipo | Descripción |
|-------|------|-------------|
| G2 | Barras verticales | Incidentes por día de la semana |
| G4 | Lollipop (puntos + segmento) | Tiempo de respuesta mediano por localidad |
| G5 | Histograma / Densidad | Distribución del tiempo de respuesta |
| G7 | Barras horizontales | Top 12 estaciones por carga operativa |
| G11 | Barras horiz. / Lollipop / Acumulado | Personas heridas por localidad |

Cada visualización incluye: Contexto · Análisis · Interpretación · Conclusión

## Tecnologías Utilizadas

- **Framework:** R Shiny + bslib (Bootstrap 5)
- **Visualización:** ggplot2 + plotly
- **Interactividad:** selectInput, sliderInput, checkboxGroupInput, radioButtons
- **Fuentes web:** Google Fonts (Montserrat + Source Sans Pro)
- **Despliegue:** shinyapps.io

## Instalación y Ejecución Local

### Requisitos Previos
- R ≥ 4.2
- RStudio (recomendado)

### Instrucciones

```r
# 1. Instalar paquetes necesarios
install.packages(c(
  "shiny", "bslib", "plotly", "dplyr",
  "tidyr", "ggplot2", "scales", "stringr"
))

# 2. Clonar el repositorio y abrir app.R en RStudio

# 3. Colocar el CSV en la misma carpeta que app.R:
#    incidentes-atendidos-por-uaecob-corte-31-agosto-2020.csv

# 4. Ejecutar la app
shiny::runApp("app.R")
```

> **Nota:** Si el CSV no se encuentra, la app funciona con datos embebidos de demostración mostrando los valores exactos del dataset original.

## Despliegue en shinyapps.io

```r
# Instalar rsconnect si no está instalado
install.packages("rsconnect")

# Configurar cuenta (tokens desde shinyapps.io > Account > Tokens)
rsconnect::setAccountInfo(
  name   = "TU_USUARIO",
  token  = "TU_TOKEN",
  secret = "TU_SECRET"
)

# Desplegar
rsconnect::deployApp(
  appDir  = ".",
  appName = "uaecob-bogota-2020"
)
```

**URL en producción:** *(completar después del despliegue)*

## Estructura del Repositorio

```
uaecob-shiny/
├── app.R                                                       # App completa (ui + server)
├── incidentes-atendidos-por-uaecob-corte-31-agosto-2020.csv   # Dataset
└── README.md                                                   # Este archivo
```

## Autores

- Juan Carlos Rojas Lizarazo
- Brayan Andres Sierra Zambrano

**Curso:** Herramientas y Visualización de Datos  
**Institución:** Fundación Universitaria Los Libertadores  
**Año:** 2026
