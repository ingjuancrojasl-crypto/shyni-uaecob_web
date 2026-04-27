library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(stringr)
library(tidyr)
library(lubridate)

# ══════════════════════════════════════════════════════════════
# CARGA Y LIMPIEZA DE DATOS
# ══════════════════════════════════════════════════════════════

cargar_datos <- function() {
  df <- read_delim(
    "incidentes_uaecob_2020.csv",
    delim = ";",
    locale = locale(encoding = "latin1"),
    show_col_types = FALSE
  )

  # Extraer mes desde texto en español
  meses_map <- c(
    "enero" = 1, "febrero" = 2, "marzo" = 3, "abril" = 4,
    "mayo" = 5, "junio" = 6, "julio" = 7, "agosto" = 8,
    "septiembre" = 9, "octubre" = 10, "noviembre" = 11, "diciembre" = 12
  )
  meses_nombre <- c(
    "1" = "Enero", "2" = "Febrero", "3" = "Marzo", "4" = "Abril",
    "5" = "Mayo", "6" = "Junio", "7" = "Julio", "8" = "Agosto"
  )

  get_mes <- function(fecha) {
    fecha <- tolower(fecha)
    for (nm in names(meses_map)) {
      if (grepl(nm, fecha)) return(meses_map[[nm]])
    }
    return(NA_integer_)
  }

  df$MES <- sapply(df$`FECHA DEL EVENTO`, get_mes)
  df$MES_NOMBRE <- meses_nombre[as.character(df$MES)]
  df$MES_NOMBRE <- factor(df$MES_NOMBRE,
    levels = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto"))

  # Servicio limpio (sin número inicial)
  df$SERVICIO_SIMPLE <- str_extract(df$SERVICIO, "(?<=\\d\\.\\s).*")
  df$SERVICIO_SIMPLE[is.na(df$SERVICIO_SIMPLE)] <- df$SERVICIO[is.na(df$SERVICIO_SIMPLE)]

  # Tiempo de respuesta en minutos
  parse_tiempo <- function(t) {
    parts <- str_split(t, ":")[[1]]
    if (length(parts) == 3) {
      return(as.numeric(parts[1]) * 60 + as.numeric(parts[2]) + as.numeric(parts[3]) / 60)
    }
    return(NA_real_)
  }
  df$TIEMPO_MIN <- sapply(df$`Tiempo de Respuesta`, parse_tiempo)
  df$TIEMPO_MIN[df$TIEMPO_MIN > 180] <- NA  # Eliminar outliers extremos

  # Estrato numérico válido
  df$ESTRATO_NUM <- suppressWarnings(as.integer(df$ESTRATO))

  # Causas limpias
  df$CAUSAS_LIMPIA <- str_trim(toupper(df$CAUSAS))
  df$CAUSAS_LIMPIA[!df$CAUSAS_LIMPIA %in%
    c("ACCIDENTAL","NATURAL","PROVOCADA","INDETERMINADA","ORDEN","NO APLICA","CONDICIÓN HUMANA")] <- "OTRA"

  # Total víctimas
  df$TOTAL_EXPUESTOS <- rowSums(df[, c("HOMBRES EXPUESTOS","MUJERES EXPUESTAS",
    "MENORES NIÑAS EXPUESTAS","MENORES NIÑOS EXPUESTOS")], na.rm = TRUE)

  return(df)
}

df <- cargar_datos()

# Paleta de colores institucional
COLOR_ROJO   <- "#E63946"
COLOR_AZUL   <- "#1D3557"
COLOR_CLARO  <- "#457B9D"

# ══════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════

ui <- dashboardPage(
  skin = "red",

  dashboardHeader(
    title = "🚒 UAECOB Bogotá 2020",
    titleWidth = 280
  ),

  dashboardSidebar(
    width = 260,
    sidebarMenu(
      menuItem("Inicio",              tabName = "inicio",    icon = icon("home")),
      menuItem("Viz 1 · Comparación", tabName = "viz1",      icon = icon("chart-bar")),
      menuItem("Viz 2 · Distribución",tabName = "viz2",      icon = icon("chart-area")),
      menuItem("Viz 3 · Relación",    tabName = "viz3",      icon = icon("th")),
      menuItem("Viz 4 · Temporal",    tabName = "viz4",      icon = icon("calendar")),
      menuItem("Viz 5 · Composición", tabName = "viz5",      icon = icon("chart-pie"))
    ),
    hr(),
    # Filtros globales
    tags$div(style = "padding: 0 15px;",
      tags$h5("⚙️ Filtros globales", style = "color:#aaa; margin-bottom:10px;"),
      checkboxGroupInput("filtro_mes", "Mes:",
        choices  = setNames(1:8, c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto")),
        selected = 1:8
      ),
      selectInput("filtro_estrato", "Estrato:",
        choices  = c("Todos", "1","2","3","4","5","6"),
        selected = "Todos"
      )
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background: #f4f6f9; }
      .insight-box {
        background: #fff8e1;
        border-left: 4px solid #ffc107;
        border-radius: 6px;
        padding: 12px 16px;
        margin-top: 12px;
        font-size: 14px;
      }
      .box-title { font-weight: 700; }
      .small-box .icon { font-size: 60px !important; top: 10px !important; }
    "))),

    tabItems(

      # ── INICIO ──────────────────────────────────────────────
      tabItem(tabName = "inicio",
        fluidRow(
          valueBoxOutput("kpi_total",   width = 3),
          valueBoxOutput("kpi_tiempo",  width = 3),
          valueBoxOutput("kpi_localidad", width = 3),
          valueBoxOutput("kpi_servicio", width = 3)
        ),
        fluidRow(
          box(width = 12, status = "danger", solidHeader = TRUE,
            title = "📋 Sobre este dashboard",
            p("Análisis interactivo de los", strong("20.228 incidentes"), "atendidos por el",
              strong("Cuerpo Oficial de Bomberos de Bogotá (UAECOB)"),
              "entre enero y agosto de 2020."),
            p("Usa el menú lateral para navegar entre las 5 visualizaciones.
               Los filtros globales (mes y estrato) se aplican a todas las vistas."),
            tags$ul(
              tags$li(strong("Viz 1:"), " Comparación de tipos de incidente (barras)"),
              tags$li(strong("Viz 2:"), " Distribución del tiempo de respuesta (histograma + boxplot)"),
              tags$li(strong("Viz 3:"), " Relación estrato × tipo de incidente (heatmap)"),
              tags$li(strong("Viz 4:"), " Evolución temporal mensual (línea de tiempo)"),
              tags$li(strong("Viz 5:"), " Composición por causa (gráfico de barras apiladas)")
            ),
            hr(),
            p(em("Fuente: Datos Abiertos Bogotá · datos.gov.co — Corte 31 agosto 2020"),
              style = "color:#888; font-size:12px;")
          )
        )
      ),

      # ── VIZ 1: COMPARACIÓN ──────────────────────────────────
      tabItem(tabName = "viz1",
        fluidRow(
          box(width = 3, status = "danger", solidHeader = TRUE, title = "⚙️ Controles",
            sliderInput("v1_top", "Número de tipos:", min = 5, max = 15, value = 10),
            radioButtons("v1_orden", "Ordenar por:",
              choices = c("Mayor a menor" = "desc", "Menor a mayor" = "asc"),
              selected = "desc"
            )
          ),
          box(width = 9, status = "danger", solidHeader = TRUE,
            title = "📊 Viz 1 · Comparación: tipos de incidente atendidos",
            plotlyOutput("plot_v1", height = "420px"),
            div(class = "insight-box",
              "💡 ", strong("Hallazgo:"), " La mayoría de los servicios corresponden a ",
              strong("prevenciones, activaciones y continuaciones"),
              " — los incendios reales representan menos del 4% del total,
              reflejando la labor preventiva del cuerpo de bomberos."
            )
          )
        )
      ),

      # ── VIZ 2: DISTRIBUCIÓN ─────────────────────────────────
      tabItem(tabName = "viz2",
        fluidRow(
          box(width = 3, status = "danger", solidHeader = TRUE, title = "⚙️ Controles",
            selectInput("v2_tipo", "Filtrar por tipo:",
              choices = c("Todos los tipos", sort(unique(na.omit(df$SERVICIO_SIMPLE))))
            ),
            sliderInput("v2_bins", "Número de barras:", min = 10, max = 60, value = 35),
            checkboxInput("v2_boxplot", "Mostrar boxplot debajo", value = TRUE)
          ),
          box(width = 9, status = "danger", solidHeader = TRUE,
            title = "⏱️ Viz 2 · Distribución: tiempo de respuesta (minutos)",
            plotlyOutput("plot_v2", height = "420px"),
            div(class = "insight-box",
              "💡 ", strong("Hallazgo:"), " El tiempo de respuesta mediano es de ",
              strong("~9 minutos"), ". La distribución está sesgada a la derecha —
              la mayoría de incidentes se atienden en menos de 15 minutos,
              pero algunos casos complejos elevan el promedio a ~11 min."
            )
          )
        )
      ),

      # ── VIZ 3: RELACIÓN ─────────────────────────────────────
      tabItem(tabName = "viz3",
        fluidRow(
          box(width = 3, status = "danger", solidHeader = TRUE, title = "⚙️ Controles",
            sliderInput("v3_top_serv", "Top tipos de servicio:", min = 4, max = 12, value = 8),
            selectInput("v3_escala", "Escala de color:",
              choices = c("Amarillo-Rojo" = "YlOrRd", "Azul-Rojo" = "RdBu",
                          "Verde-Rojo" = "RdYlGn", "Naranja" = "Oranges"),
              selected = "YlOrRd"
            )
          ),
          box(width = 9, status = "danger", solidHeader = TRUE,
            title = "🔥 Viz 3 · Relación: estrato socioeconómico × tipo de incidente",
            plotlyOutput("plot_v3", height = "420px"),
            div(class = "insight-box",
              "💡 ", strong("Hallazgo:"), " Los estratos 2 y 3 concentran la mayor cantidad de
              incidentes en casi todas las categorías — coherente con su mayor población en Bogotá.
              Las quemas prohibidas e incidentes con animales son relativamente más frecuentes
              en estratos bajos."
            )
          )
        )
      ),

      # ── VIZ 4: TEMPORAL ─────────────────────────────────────
      tabItem(tabName = "viz4",
        fluidRow(
          box(width = 3, status = "danger", solidHeader = TRUE, title = "⚙️ Controles",
            selectInput("v4_tipo", "Ver evolución de:",
              choices = c("Todos los tipos", sort(unique(na.omit(df$SERVICIO_SIMPLE))))
            ),
            checkboxInput("v4_covid", "Marcar período COVID-19", value = TRUE),
            checkboxInput("v4_puntos", "Mostrar valores en puntos", value = TRUE)
          ),
          box(width = 9, status = "danger", solidHeader = TRUE,
            title = "📅 Viz 4 · Evolución temporal: incidentes por mes (enero–agosto 2020)",
            plotlyOutput("plot_v4", height = "420px"),
            div(class = "insight-box",
              "💡 ", strong("Hallazgo:"), " Se observa una ",
              strong("reducción notable en marzo y abril"), ",
              coincidiendo con el inicio de la cuarentena obligatoria por COVID-19.
              Los meses de enero y febrero muestran picos relacionados con el verano bogotano
              (temporada seca con más quemas e incendios)."
            )
          )
        )
      ),

      # ── VIZ 5: COMPOSICIÓN ──────────────────────────────────
      tabItem(tabName = "viz5",
        fluidRow(
          box(width = 3, status = "danger", solidHeader = TRUE, title = "⚙️ Controles",
            sliderInput("v5_top_loc", "Top localidades:", min = 5, max = 15, value = 10),
            selectInput("v5_var", "Variable de composición:",
              choices = c(
                "Causa del incidente"  = "CAUSAS_LIMPIA",
                "Clase de servicio"    = "CLASE DE SERVICIO",
                "Origen de la causa"   = "ORIGEN DE LA CAUSA"
              ),
              selected = "CAUSAS_LIMPIA"
            ),
            checkboxInput("v5_pct", "Ver como porcentaje (%)", value = FALSE)
          ),
          box(width = 9, status = "danger", solidHeader = TRUE,
            title = "🗺️ Viz 5 · Composición: distribución por localidad y causa",
            plotlyOutput("plot_v5", height = "460px"),
            div(class = "insight-box",
              "💡 ", strong("Hallazgo:"), " ",
              strong("Suba y Kennedy"), " concentran casi el 20% de todos los incidentes.
              La causa más frecuente en casi todas las localidades es 'No aplica' o 'Accidental',
              pero zonas industriales como Fontibón y Puente Aranda muestran mayor proporción
              de causas de Orden (emergencias industriales/MATPEL)."
            )
          )
        )
      )

    ) # fin tabItems
  ) # fin dashboardBody
) # fin ui

# ══════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Datos filtrados reactivos ──────────────────────────────
  datos_filtrados <- reactive({
    d <- df %>% filter(MES %in% as.integer(input$filtro_mes))
    if (input$filtro_estrato != "Todos") {
      d <- d %>% filter(ESTRATO == input$filtro_estrato)
    }
    d
  })

  # ── KPIs INICIO ───────────────────────────────────────────
  output$kpi_total <- renderValueBox({
    valueBox(
      value = format(nrow(datos_filtrados()), big.mark = "."),
      subtitle = "Total incidentes",
      icon = icon("fire"),
      color = "red"
    )
  })

  output$kpi_tiempo <- renderValueBox({
    med <- median(datos_filtrados()$TIEMPO_MIN, na.rm = TRUE)
    valueBox(
      value = paste0(round(med, 0), " min"),
      subtitle = "Tiempo respuesta mediano",
      icon = icon("clock"),
      color = "yellow"
    )
  })

  output$kpi_localidad <- renderValueBox({
    top <- datos_filtrados() %>%
      count(LOCALIDAD, sort = TRUE) %>%
      slice(1) %>% pull(LOCALIDAD)
    valueBox(
      value = str_to_title(top),
      subtitle = "Localidad con más incidentes",
      icon = icon("map-marker"),
      color = "blue"
    )
  })

  output$kpi_servicio <- renderValueBox({
    top <- datos_filtrados() %>%
      count(SERVICIO_SIMPLE, sort = TRUE) %>%
      slice(1) %>% pull(SERVICIO_SIMPLE)
    valueBox(
      value = str_to_title(top),
      subtitle = "Tipo más frecuente",
      icon = icon("list"),
      color = "green"
    )
  })

  # ── VIZ 1: BARRAS HORIZONTALES ────────────────────────────
  output$plot_v1 <- renderPlotly({
    d <- datos_filtrados() %>%
      count(SERVICIO_SIMPLE, sort = TRUE) %>%
      slice_head(n = input$v1_top) %>%
      na.omit()

    if (input$v1_orden == "asc") d <- d %>% arrange(n)
    else d <- d %>% arrange(desc(n))

    d$SERVICIO_SIMPLE <- factor(d$SERVICIO_SIMPLE, levels = d$SERVICIO_SIMPLE)

    p <- ggplot(d, aes(x = n, y = SERVICIO_SIMPLE, fill = n,
                       text = paste0(SERVICIO_SIMPLE, ": ", format(n, big.mark="."), " incidentes"))) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = format(n, big.mark=".")), hjust = -0.1, size = 3.2, color = "#333") +
      scale_fill_gradient(low = "#f4a261", high = COLOR_ROJO) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Número de incidentes", y = NULL,
           title = paste("Top", input$v1_top, "tipos de incidente — UAECOB Bogotá 2020")) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.title = element_text(face = "bold", size = 13, color = COLOR_AZUL),
        axis.text  = element_text(color = "#333")
      )

    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l = 10, r = 40, t = 50, b = 30))
  })

  # ── VIZ 2: HISTOGRAMA ─────────────────────────────────────
  output$plot_v2 <- renderPlotly({
    d <- datos_filtrados() %>% filter(!is.na(TIEMPO_MIN))

    if (input$v2_tipo != "Todos los tipos") {
      d <- d %>% filter(SERVICIO_SIMPLE == input$v2_tipo)
    }

    med_val <- median(d$TIEMPO_MIN, na.rm = TRUE)
    avg_val <- mean(d$TIEMPO_MIN, na.rm = TRUE)

    if (input$v2_boxplot) {
      p1 <- plot_ly(d, x = ~TIEMPO_MIN, type = "histogram",
        nbinsx = input$v2_bins,
        marker = list(color = COLOR_ROJO, opacity = 0.8, line = list(color = "white", width = 0.5)),
        name = "Frecuencia"
      ) %>%
        add_segments(x = med_val, xend = med_val, y = 0, yend = nrow(d) * 0.25,
          line = list(color = COLOR_AZUL, dash = "dash", width = 2),
          name = paste0("Mediana: ", round(med_val, 1), " min")
        ) %>%
        layout(xaxis = list(title = ""), yaxis = list(title = "Frecuencia"),
               showlegend = TRUE)

      p2 <- plot_ly(d, x = ~TIEMPO_MIN, type = "box",
        marker  = list(color = COLOR_ROJO),
        line    = list(color = COLOR_AZUL),
        fillcolor = "rgba(230,57,70,0.3)",
        name = "Distribución"
      ) %>%
        layout(xaxis = list(title = "Minutos hasta la llegada"), yaxis = list(showticklabels = FALSE))

      subplot(p1, p2, nrows = 2, shareX = TRUE, heights = c(0.75, 0.25)) %>%
        layout(title = list(text = "Distribución del tiempo de respuesta", font = list(size = 14)),
               showlegend = TRUE)
    } else {
      plot_ly(d, x = ~TIEMPO_MIN, type = "histogram",
        nbinsx = input$v2_bins,
        marker = list(color = COLOR_ROJO, opacity = 0.8)
      ) %>%
        add_segments(x = med_val, xend = med_val, y = 0, yend = nrow(d) * 0.3,
          line = list(color = COLOR_AZUL, dash = "dash", width = 2),
          name = paste0("Mediana: ", round(med_val, 1), " min")
        ) %>%
        layout(title = "Distribución del tiempo de respuesta",
               xaxis = list(title = "Minutos hasta la llegada"),
               yaxis = list(title = "Frecuencia"))
    }
  })

  # ── VIZ 3: HEATMAP ────────────────────────────────────────
  output$plot_v3 <- renderPlotly({
    top_serv <- datos_filtrados() %>%
      count(SERVICIO_SIMPLE, sort = TRUE) %>%
      slice_head(n = input$v3_top_serv) %>%
      pull(SERVICIO_SIMPLE)

    d <- datos_filtrados() %>%
      filter(!is.na(ESTRATO_NUM), ESTRATO_NUM %in% 1:6,
             SERVICIO_SIMPLE %in% top_serv) %>%
      count(ESTRATO_NUM, SERVICIO_SIMPLE) %>%
      complete(ESTRATO_NUM = 1:6, SERVICIO_SIMPLE = top_serv, fill = list(n = 0))

    pivot <- d %>%
      tidyr::pivot_wider(names_from = ESTRATO_NUM, values_from = n, values_fill = 0)

    mat <- as.matrix(pivot[, -1])
    rownames(mat) <- pivot$SERVICIO_SIMPLE
    colnames(mat) <- paste("Estrato", 1:6)

    plot_ly(
      x = colnames(mat),
      y = rownames(mat),
      z = mat,
      type = "heatmap",
      colorscale = input$v3_escala,
      hoverongaps = FALSE,
      hovertemplate = "<b>%{y}</b><br>%{x}<br>Incidentes: %{z}<extra></extra>"
    ) %>%
      layout(
        title = list(text = "Incidentes por tipo de servicio y estrato socioeconómico",
                     font = list(size = 14)),
        xaxis = list(title = "Estrato socioeconómico"),
        yaxis = list(title = ""),
        margin = list(l = 160, r = 20, t = 60, b = 50)
      )
  })

  # ── VIZ 4: LÍNEA TEMPORAL ─────────────────────────────────
  output$plot_v4 <- renderPlotly({
    d <- datos_filtrados()

    if (input$v4_tipo != "Todos los tipos") {
      d <- d %>% filter(SERVICIO_SIMPLE == input$v4_tipo)
    }

    d_mes <- d %>%
      filter(!is.na(MES)) %>%
      count(MES, MES_NOMBRE) %>%
      arrange(MES)

    mode_line <- if (input$v4_puntos) "lines+markers+text" else "lines"

    p <- plot_ly(d_mes, x = ~MES_NOMBRE, y = ~n, type = "scatter", mode = mode_line,
      line   = list(color = COLOR_ROJO, width = 3),
      marker = list(size = 10, color = COLOR_AZUL),
      text   = ~n, textposition = "top center",
      name   = "Incidentes",
      hovertemplate = "<b>%{x}</b><br>Incidentes: %{y}<extra></extra>"
    )

    if (input$v4_covid) {
      p <- p %>%
        add_segments(
          x = "Marzo", xend = "Abril",
          y = max(d_mes$n) * 0.05, yend = max(d_mes$n) * 0.05,
          line = list(color = COLOR_CLARO, width = 20, opacity = 0.2),
          name = "Inicio cuarentena COVID-19",
          showlegend = TRUE
        )
    }

    p %>% layout(
      title = list(
        text = if (input$v4_tipo == "Todos los tipos")
          "Total de incidentes por mes — UAECOB 2020"
        else
          paste("Tipo:", input$v4_tipo, "— evolución mensual"),
        font = list(size = 14)
      ),
      xaxis = list(title = "Mes", categoryorder = "array",
        categoryarray = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto")),
      yaxis = list(title = "Número de incidentes"),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.15)
    )
  })

  # ── VIZ 5: BARRAS APILADAS ────────────────────────────────
  output$plot_v5 <- renderPlotly({
    top_locs <- datos_filtrados() %>%
      count(LOCALIDAD, sort = TRUE) %>%
      slice_head(n = input$v5_top_loc) %>%
      pull(LOCALIDAD)

    var_sel <- input$v5_var

    d <- datos_filtrados() %>%
      filter(LOCALIDAD %in% top_locs, !is.na(.data[[var_sel]])) %>%
      count(LOCALIDAD, .data[[var_sel]]) %>%
      rename(GRUPO = 2)

    if (input$v5_pct) {
      d <- d %>%
        group_by(LOCALIDAD) %>%
        mutate(pct = n / sum(n) * 100) %>%
        ungroup()
      y_var  <- "pct"
      y_lab  <- "Porcentaje (%)"
      barnorm <- "percent"
    } else {
      d$pct  <- d$n
      y_var  <- "n"
      y_lab  <- "Número de incidentes"
      barnorm <- NULL
    }

    # Ordenar localidades por total
    orden_loc <- datos_filtrados() %>%
      filter(LOCALIDAD %in% top_locs) %>%
      count(LOCALIDAD, sort = TRUE) %>%
      pull(LOCALIDAD)

    d$LOCALIDAD <- factor(d$LOCALIDAD, levels = rev(orden_loc))

    p <- plot_ly(d, x = ~LOCALIDAD, y = ~get(y_var), color = ~GRUPO,
      type = "bar", barmode = "stack",
      hovertemplate = "<b>%{x}</b><br>%{fullData.name}<br>%{y:.0f}<extra></extra>"
    ) %>%
      layout(
        title = list(
          text = paste("Composición de incidentes —", input$v5_top_loc, "localidades principales"),
          font = list(size = 14)
        ),
        xaxis = list(title = "Localidad", tickangle = -30),
        yaxis = list(title = y_lab),
        barmode = "stack",
        legend = list(title = list(text = str_to_title(gsub("_", " ", var_sel))),
                      orientation = "v"),
        margin = list(b = 80)
      )

    if (!is.null(barnorm) && input$v5_pct) {
      p <- p %>% layout(barnorm = "percent")
    }
    p
  })

} # fin server

# ══════════════════════════════════════════════════════════════
shinyApp(ui = ui, server = server)
