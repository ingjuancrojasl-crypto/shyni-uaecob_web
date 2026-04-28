# ══════════════════════════════════════════════════════════════════════════════
# UAECOB Bogotá D.C. — Dashboard de Incidentes 2020
# App Shiny profesional con 5 visualizaciones interactivas
#
# Autores  : Juan Carlos Rojas Lizarazo · Brayan Andres Sierra Zambrano
# Fuente   : UAECOB / Datos Abiertos Bogotá
# Dataset  : https://datosabiertos.bogota.gov.co/dataset/incidente-atendido-por-bomberos
# Periodo  : Enero – Agosto 2020
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. LIBRERÍAS ──────────────────────────────────────────────────────────────
library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)

# ── 2. DATOS PREPROCESADOS ───────────────────────────────────────────────────
# Nota: carga desde CSV si existe; de lo contrario usa datos embebidos.
# Para producción, colocar el CSV en la misma carpeta que app.R

cargar_datos <- function() {
  csv_path <- "incidentes-atendidos-por-uaecob-corte-31-agosto-2020.csv"

  if (file.exists(csv_path)) {
    # ── Carga real desde CSV ──────────────────────────────────────────────────
    df_raw <- read.csv2(csv_path, fileEncoding = "latin1",
                        stringsAsFactors = FALSE, check.names = FALSE)

    # Normalizar nombre de columnas
    names(df_raw) <- trimws(names(df_raw))

    # Parsear fecha
    meses_es <- c(enero=1,febrero=2,marzo=3,abril=4,mayo=5,
                  junio=6,julio=7,agosto=8,septiembre=9,
                  octubre=10,noviembre=11,diciembre=12)

    parse_fecha <- function(s) {
      tryCatch({
        partes <- strsplit(trimws(s), " de ")[[1]]
        dia    <- as.integer(tail(strsplit(trimws(partes[1]), " ")[[1]], 1))
        mes    <- meses_es[tolower(trimws(partes[2]))]
        anio   <- as.integer(trimws(partes[3]))
        as.Date(paste(anio, mes, dia, sep = "-"))
      }, error = function(e) NA)
    }

    df_raw$FECHA   <- as.Date(sapply(df_raw[["FECHA DEL EVENTO"]], parse_fecha),
                              origin = "1970-01-01")
    df_raw$MES     <- as.integer(format(df_raw$FECHA, "%m"))
    df_raw$DIA_SEM <- weekdays(df_raw$FECHA, abbreviate = FALSE)

    # Unificar localidad (quitar prefijo numérico)
    unif_loc <- function(x) {
      x <- trimws(x)
      if (grepl("^[0-9]+\\s", x)) sub("^[0-9]+\\s+", "", x) else x
    }
    df_raw$LOCALIDAD_L <- sapply(df_raw$LOCALIDAD, unif_loc)

    # Tiempo de respuesta a minutos
    parse_tr <- function(x) {
      tryCatch({
        parts <- strsplit(trimws(x), ":")[[1]]
        as.numeric(parts[1])*60 + as.numeric(parts[2]) + as.numeric(parts[3])/60
      }, error = function(e) NA_real_)
    }
    df_raw$TR_min   <- sapply(df_raw[["Tiempo de Respuesta"]], parse_tr)
    df_raw$TR_limpio <- ifelse(df_raw$TR_min <= 120, df_raw$TR_min, NA_real_)

    # Heridos y rescatados
    cols_her <- c("HOMBRES HERIDOS","MUJERES HERIDAS",
                  "MENORES NIÑAS HERIDAS","MENORES NIÑOS HERIDOS")
    cols_res <- c("HOMBRES RESCATADOS","MUJERES RESCATADAS",
                  "MENORES NIÑAS RESCATADAS","MENORES NIÑOS RESCATADOS")
    for (col in c(cols_her, cols_res)) {
      df_raw[[col]] <- suppressWarnings(as.numeric(df_raw[[col]]))
      df_raw[[col]][is.na(df_raw[[col]])] <- 0
    }
    df_raw$TOTAL_HERIDOS    <- rowSums(df_raw[, cols_her])
    df_raw$TOTAL_RESCATADOS <- rowSums(df_raw[, cols_res])

    return(df_raw)
  }

  # ── Datos embebidos (fallback si no hay CSV) ──────────────────────────────
  message("CSV no encontrado — usando datos de demostración embebidos.")
  NULL
}

df_global <- cargar_datos()

# ── Datos derivados para cada gráfica ────────────────────────────────────────

# G2 — Incidentes por día de la semana
ORDEN_DIAS <- c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo")

datos_g2_base <- if (!is.null(df_global)) {
  dias_map <- c("Monday"="Lunes","Tuesday"="Martes","Wednesday"="Miércoles",
                "Thursday"="Jueves","Friday"="Viernes",
                "Saturday"="Sábado","Sunday"="Domingo")
  df_global %>%
    mutate(DIA = dias_map[DIA_SEM]) %>%
    group_by(MES, DIA) %>%
    summarise(n = n(), .groups = "drop")
} else {
  # Datos embebidos exactos del notebook
  tibble(
    DIA = factor(c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"),
                 levels = ORDEN_DIAS),
    n   = c(2744, 2875, 2935, 3040, 3094, 2847, 2693),
    MES = 0L   # marcador — modo demo
  )
}

# G4 — Tiempo de respuesta mediano por localidad
datos_g4_base <- if (!is.null(df_global)) {
  top20 <- df_global %>% count(LOCALIDAD_L) %>% top_n(20, n) %>% pull(LOCALIDAD_L)
  df_global %>%
    filter(LOCALIDAD_L %in% top20) %>%
    group_by(MES, LOCALIDAD_L) %>%
    summarise(tr_med = median(TR_limpio, na.rm = TRUE), .groups = "drop")
} else {
  tibble(
    LOCALIDAD_L = c("LOS MÁRTIRES","ANTONIO NARIÑO","LA CANDELARIA",
                    "ENGATIVÁ","PUENTE ARANDA","TEUSAQUILLO","CHAPINERO",
                    "SANTA FE","BARRIOS UNIDOS","FONTIBÓN","TUNJUELITO",
                    "USME","KENNEDY","SUBA","RAFAEL URIBE URIBE","USAQUÉN",
                    "SAN CRISTÓBAL","BOSA","RAFAEL URIBE","CIUDAD BOLÍVAR"),
    tr_med = c(6.00,6.98,7.68,8.00,8.00,8.00,8.57,8.68,8.87,
               9.00,9.00,9.00,9.15,9.30,9.67,9.68,9.81,10.00,11.00,11.00),
    MES = 0L
  )
}

# G5 — Distribución del tiempo de respuesta
datos_g5_base <- if (!is.null(df_global)) {
  df_global %>%
    filter(!is.na(TR_limpio)) %>%
    select(MES, TR_limpio)
} else {
  tibble(
    MES      = 0L,
    TR_limpio = c(
      rep(2.5, 3589), rep(7.5, 7557), rep(12.5, 4692),
      rep(17.5, 1941), rep(25.0, 1496), rep(45.0, 798), rep(90.0, 129)
    )
  )
}

# G7 — Top 12 estaciones
datos_g7_base <- if (!is.null(df_global)) {
  df_global %>%
    group_by(MES, ESTACIÓN) %>%
    summarise(n = n(), .groups = "drop")
} else {
  tibble(
    ESTACIÓN = c("B-5 KENNEDY","B-1 CHAPINERO","B-13 CAOBOS SALAZAR",
                 "B-3 SUR","B-2 CENTRAL","B-11 CANDELARIA",
                 "B-4 PUENTE ARANDA","B-7 FERIAS","B-6 FONTIBÓN",
                 "B-9 BELLAVISTA","B-12 SUBA","B-15 GARCES NAVAS"),
    n   = c(1905,1800,1481,1262,1256,1232,1187,1167,1148,1110,1064,1018),
    MES = 0L
  )
}

# G11 — Heridos por localidad
datos_g11_base <- if (!is.null(df_global)) {
  df_global %>%
    filter(LOCALIDAD_L != "FUERA D.C.", LOCALIDAD_L != "SUMAPAZ",
           !is.na(LOCALIDAD_L)) %>%
    group_by(MES, LOCALIDAD_L) %>%
    summarise(heridos = sum(TOTAL_HERIDOS), .groups = "drop")
} else {
  tibble(
    LOCALIDAD_L = c("SUBA","KENNEDY","TEUSAQUILLO","ENGATIVÁ","CIUDAD BOLÍVAR",
                    "SAN CRISTÓBAL","USAQUÉN","FONTIBÓN","PUENTE ARANDA","BOSA",
                    "USME","RAFAEL URIBE URIBE","CHAPINERO","BARRIOS UNIDOS",
                    "SANTA FE","LOS MÁRTIRES","ANTONIO NARIÑO","TUNJUELITO","LA CANDELARIA"),
    heridos = c(140,129,111,107,102,87,86,80,70,66,53,51,48,43,42,34,28,23,12),
    MES = 0L
  )
}

# Meses disponibles
MESES_DISP <- if (!is.null(df_global)) {
  sort(unique(df_global$MES))
} else {
  1:8
}
NOMBRE_MES <- c("1"="Ene","2"="Feb","3"="Mar","4"="Abr",
                 "5"="May","6"="Jun","7"="Jul","8"="Ago")

# ── Paleta institucional ──────────────────────────────────────────────────────
COL <- list(
  azul   = "#1a5ea8",
  rojo   = "#c0392b",
  verde  = "#1D9E75",
  ambar  = "#d68910",
  morado = "#6c3483",
  teal   = "#148f77",
  gris   = "#707b7c",
  bg     = "#f7f9fc"
)

# ── Tema ggplot2 personalizado ────────────────────────────────────────────────
tema_uaecob <- function() {
  theme_minimal(base_family = "sans", base_size = 12) +
    theme(
      plot.background    = element_rect(fill = "white", color = NA),
      panel.grid.major   = element_line(color = "#ebebeb", linewidth = 0.5),
      panel.grid.minor   = element_blank(),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.title         = element_text(size = 13, face = "bold", color = "#0d2b5e",
                                        margin = margin(b = 6)),
      plot.subtitle      = element_text(size = 10, color = "#707b7c",
                                        margin = margin(b = 10)),
      axis.title         = element_text(size = 10, color = "#555555"),
      axis.text          = element_text(size = 9, color = "#555555"),
      legend.position    = "bottom",
      legend.title       = element_text(size = 9),
      legend.text        = element_text(size = 9),
      plot.caption       = element_text(size = 8, color = "#95a5a6",
                                        hjust = 0, margin = margin(t = 8)),
      plot.margin        = margin(12, 16, 8, 12)
    )
}

CAPTION_BASE <- "Fuente: UAECOB · Datos Abiertos Bogotá · Periodo: Ene–Ago 2020 · Autores: J.C. Rojas · B.A. Sierra"

# ══════════════════════════════════════════════════════════════════════════════
# 3. UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- page_navbar(
  title = tags$span(
    tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1a/Escudo_de_Bogot%C3%A1.svg/40px-Escudo_de_Bogot%C3%A1.svg.png",
             height = "28px", style = "margin-right:8px; vertical-align:middle;"),
    "UAECOB · Bogotá D.C. 2020"
  ),
  theme = bs_theme(
    version      = 5,
    primary      = "#1a5ea8",
    secondary    = "#6c3483",
    success      = "#1D9E75",
    danger       = "#c0392b",
    warning      = "#d68910",
    bg           = "#f7f9fc",
    fg           = "#1a1a2e",
    base_font    = font_google("Source Sans Pro"),
    heading_font = font_google("Montserrat"),
    code_font    = font_google("JetBrains Mono"),
    bootswatch   = "flatly"
  ),
  window_title  = "UAECOB Dashboard 2020",
  bg            = "#0d2b5e",
  inverse       = TRUE,
  fillable      = FALSE,

  # ── CSS extra ──────────────────────────────────────────────────────────────
  header = tags$head(tags$style(HTML("
    body { background-color: #f7f9fc; }

    /* KPI cards */
    .kpi-card {
      background: #fff;
      border-left: 4px solid #1a5ea8;
      border-radius: 10px;
      padding: 18px 22px;
      box-shadow: 0 2px 8px rgba(0,0,0,.07);
      margin-bottom: 12px;
      transition: transform .15s;
    }
    .kpi-card:hover { transform: translateY(-2px); box-shadow: 0 4px 14px rgba(0,0,0,.12); }
    .kpi-num   { font-size: 2.1rem; font-weight: 700; color: #1a5ea8; line-height:1; }
    .kpi-label { font-size: .78rem; color: #707b7c; margin-top: 4px; }
    .kpi-red   { border-color: #c0392b; }
    .kpi-red .kpi-num { color: #c0392b; }
    .kpi-green { border-color: #1D9E75; }
    .kpi-green .kpi-num { color: #1D9E75; }
    .kpi-amber { border-color: #d68910; }
    .kpi-amber .kpi-num { color: #d68910; }

    /* Section cards */
    .viz-card {
      background: #fff;
      border-radius: 12px;
      padding: 24px 26px;
      box-shadow: 0 2px 8px rgba(0,0,0,.06);
      margin-bottom: 24px;
    }
    .viz-title {
      font-size: 1.1rem; font-weight: 700; color: #0d2b5e;
      border-left: 4px solid #1a5ea8; padding-left: 12px;
      margin-bottom: 6px;
    }
    .viz-subtitle { font-size: .85rem; color: #707b7c; margin-bottom: 16px; }

    /* Analisis pills */
    .analisis-pill {
      border-radius: 8px; padding: 12px 16px;
      margin-bottom: 10px; font-size: .85rem;
    }
    .pill-contexto      { background: #eaf2fb; border-left: 3px solid #1a5ea8; }
    .pill-analisis      { background: #fdfefe; border-left: 3px solid #707b7c; }
    .pill-interpretacion{ background: #fef9e7; border-left: 3px solid #d68910; }
    .pill-conclusion    { background: #eafaf1; border-left: 3px solid #1D9E75; }
    .pill-label { font-weight: 700; font-size: .78rem; text-transform: uppercase;
                  letter-spacing: .04em; margin-bottom: 4px; }
    .pill-contexto .pill-label       { color: #1a5ea8; }
    .pill-analisis .pill-label       { color: #555; }
    .pill-interpretacion .pill-label { color: #d68910; }
    .pill-conclusion .pill-label     { color: #1D9E75; }

    /* Sidebar filtros */
    .filtros-panel {
      background: #fff;
      border-radius: 10px;
      padding: 18px;
      box-shadow: 0 1px 5px rgba(0,0,0,.06);
    }

    /* Tabs */
    .nav-pills .nav-link.active { background-color: #1a5ea8 !important; }
    .nav-pills .nav-link { color: #1a5ea8; }

    /* Footer */
    .footer-bar {
      background: #0d2b5e; color: #aec6e8;
      text-align: center; padding: 14px;
      font-size: .78rem; margin-top: 32px;
      border-radius: 8px;
    }
    .footer-bar a { color: #7fb3e0; }
  "))),

  # ── TAB 0: Inicio / KPIs ───────────────────────────────────────────────────
  nav_panel(
    title = "🏠 Inicio",
    value = "inicio",

    div(class = "container-fluid py-3",

      # Encabezado hero
      div(class = "viz-card",
        style = "background: linear-gradient(135deg,#0d2b5e 0%,#1a5ea8 100%); color:white; border-radius:14px; padding:32px;",
        fluidRow(
          column(8,
            h2("🚒 Incidentes Atendidos por la UAECOB",
               style = "color:white; font-weight:700; margin-bottom:8px;"),
            p("Análisis exploratorio de los incidentes registrados por el Cuerpo Oficial de Bomberos de Bogotá D.C. durante el periodo enero – agosto de 2020.",
              style = "color:#c8d8ed; font-size:.95rem; margin-bottom:0;")
          ),
          column(4, style = "text-align:right;",
            p(icon("calendar"), strong(" Ene – Ago 2020", style="color:white;"),
              br(),
              icon("database"), span(" 20.228 registros", style="color:#c8d8ed;"),
              br(),
              icon("map-marker-alt"), span(" Bogotá D.C.", style="color:#c8d8ed;"),
              style = "font-size:.88rem; color:white; margin-top:8px;"
            )
          )
        )
      ),

      # Filtros globales
      div(class = "filtros-panel mb-3",
        fluidRow(
          column(4,
            selectInput("filtro_mes_global", "Mes:",
                        choices = c("Todos los meses" = 0, setNames(MESES_DISP, NOMBRE_MES[as.character(MESES_DISP)])),
                        selected = 0, width = "100%")
          ),
          column(4,
            checkboxGroupInput("filtro_dias", "Días de la semana:",
                               choices  = ORDEN_DIAS,
                               selected = ORDEN_DIAS,
                               inline   = TRUE)
          ),
          column(4,
            sliderInput("filtro_tr", "Rango tiempo de respuesta (min):",
                        min = 0, max = 120, value = c(0, 120),
                        step = 5, width = "100%")
          )
        )
      ),

      # KPI cards
      fluidRow(
        column(3,
          div(class = "kpi-card",
            div(class = "kpi-num", textOutput("kpi_total")),
            div(class = "kpi-label", icon("fire"), " Total incidentes")
          )
        ),
        column(3,
          div(class = "kpi-card kpi-red",
            div(class = "kpi-num", textOutput("kpi_heridos")),
            div(class = "kpi-label", icon("user-injured"), " Personas heridas")
          )
        ),
        column(3,
          div(class = "kpi-card kpi-green",
            div(class = "kpi-num", textOutput("kpi_rescatados")),
            div(class = "kpi-label", icon("hands-helping"), " Personas rescatadas")
          )
        ),
        column(4,
          div(class = "kpi-card kpi-amber",
            div(class = "kpi-num", textOutput("kpi_tr")),
            div(class = "kpi-label", icon("clock"), " Tiempo de respuesta mediano")
          )
        )
      ),

      # Insight banner
      div(class = "viz-card",
        style = "border-left: 4px solid #d68910; background:#fef9e7;",
        h6(icon("lightbulb"), " Insight principal del periodo", style="color:#d68910; font-weight:700; margin-bottom:6px;"),
        p("Los viernes concentran la mayor demanda operativa (+15% vs domingos). El 55% de los incidentes se atendió en menos de 10 minutos. Suba, Kennedy y Engativá concentran más del 35% de todos los incidentes, evidenciando la necesidad de una distribución territorial más equitativa de recursos.",
          style="margin-bottom:0; font-size:.88rem; color:#555;")
      ),

      # Footer
      div(class = "footer-bar",
        HTML("📊 <strong>Fuente:</strong> Unidad Administrativa Especial Cuerpo Oficial de Bomberos Bogotá (UAECOB) ·
              <a href='https://datosabiertos.bogota.gov.co/dataset/incidente-atendido-por-bomberos' target='_blank'>Datos Abiertos Bogotá</a> ·
              <strong>Autores:</strong> Juan Carlos Rojas Lizarazo · Brayan Andres Sierra Zambrano ·
              Fundación Universitaria Los Libertadores 2026")
      )
    )
  ),

  # ── TAB 1: G2 — Días de la semana ─────────────────────────────────────────
  nav_panel(
    title = "📅 Días Semana",
    value = "g2",

    div(class = "container-fluid py-3",
      div(class = "viz-card",
        div(class = "viz-title", "📅 Gráfica 2 · Incidentes por día de la semana"),
        div(class = "viz-subtitle",
            "Distribución de los 20.228 incidentes según el día de la semana en que ocurrieron. Filtros activos desde la pestaña Inicio."),

        fluidRow(
          column(3,
            div(class = "filtros-panel",
              h6("⚙️ Filtros", style="font-weight:700; color:#0d2b5e; margin-bottom:12px;"),
              checkboxGroupInput("g2_dias", "Días a mostrar:",
                                 choices  = ORDEN_DIAS,
                                 selected = ORDEN_DIAS),
              hr(),
              selectInput("g2_color", "Paleta de color:",
                          choices = c("Azul institucional" = "azul",
                                      "Calor (intensidad)"  = "calor",
                                      "Monocromático gris"  = "gris"),
                          selected = "calor"),
              hr(),
              checkboxInput("g2_mostrar_valores", "Mostrar valores en barras", value = TRUE)
            )
          ),
          column(9,
            plotlyOutput("plot_g2", height = "420px")
          )
        ),

        hr(style="margin:20px 0;"),

        fluidRow(
          column(6,
            div(class = "analisis-pill pill-contexto",
              div(class="pill-label","📌 Contexto"),
              "La distribución semanal permite planificar turnos, rotación de personal y disponibilidad de unidades. Es un insumo clave para la gestión eficiente de recursos humanos de la UAECOB."
            ),
            div(class = "analisis-pill pill-analisis",
              div(class="pill-label","🔍 Análisis"),
              "Viernes lidera con 3.094 incidentes, seguido de jueves (3.040) y miércoles (2.935). Domingo registra el valor más bajo (2.693). La variación entre el día más alto y el más bajo es del 15%."
            )
          ),
          column(6,
            div(class = "analisis-pill pill-interpretacion",
              div(class="pill-label","💡 Interpretación"),
              "El aumento progresivo de lunes a viernes refleja la acumulación de actividad laboral, comercial e industrial. La reducción del fin de semana puede explicarse por el cierre de industrias y restricciones dominicales en la ciudad."
            ),
            div(class = "analisis-pill pill-conclusion",
              div(class="pill-label","✅ Conclusión"),
              "Existe un patrón semanal claro con mayor demanda de miércoles a viernes. Se recomienda reforzar turnos y disponibilidad de unidades esos días, especialmente los viernes, con esquemas de guardia diferenciados."
            )
          )
        )
      )
    )
  ),

  # ── TAB 2: G4 — Tiempo de respuesta por localidad ─────────────────────────
  nav_panel(
    title = "⏱️ Resp. Localidad",
    value = "g4",

    div(class = "container-fluid py-3",
      div(class = "viz-card",
        div(class = "viz-title", "⏱️ Gráfica 4 · Tiempo de respuesta mediano por localidad"),
        div(class = "viz-subtitle", "Mediana del tiempo en minutos entre el reporte y la llegada de la unidad. Outliers > 120 min excluidos."),

        fluidRow(
          column(3,
            div(class = "filtros-panel",
              h6("⚙️ Filtros", style="font-weight:700; color:#0d2b5e; margin-bottom:12px;"),
              sliderInput("g4_n_loc", "N.º de localidades:",
                          min = 5, max = 20, value = 20, step = 1),
              hr(),
              radioButtons("g4_orden", "Ordenar por:",
                           choices  = c("Menor tiempo primero" = "asc",
                                        "Mayor tiempo primero"  = "desc"),
                           selected = "asc"),
              hr(),
              checkboxInput("g4_linea_ref", "Mostrar línea de referencia (9 min)", value = TRUE),
              hr(),
              div(style="background:#eaf2fb; border-radius:8px; padding:10px;",
                strong("Referencia OMS", style="font-size:.82rem; color:#1a5ea8;"),
                br(),
                span("El estándar internacional para respuesta a emergencias es ≤ 8 minutos.",
                     style="font-size:.8rem; color:#555;")
              )
            )
          ),
          column(9,
            plotlyOutput("plot_g4", height = "520px")
          )
        ),

        hr(style="margin:20px 0;"),

        fluidRow(
          column(6,
            div(class = "analisis-pill pill-contexto",
              div(class="pill-label","📌 Contexto"),
              "El tiempo de respuesta mide el intervalo entre el reporte del incidente y la llegada de la unidad. Se usa la mediana para evitar distorsión por valores extremos. Se excluyeron registros con tiempos superiores a 120 minutos."
            ),
            div(class = "analisis-pill pill-analisis",
              div(class="pill-label","🔍 Análisis"),
              "Los Mártires (6 min) y Antonio Nariño (7 min) tienen los mejores tiempos. Ciudad Bolívar y Rafael Uribe registran los más altos (11 min). La brecha entre el mejor y el peor tiempo es de 5 minutos."
            )
          ),
          column(6,
            div(class = "analisis-pill pill-interpretacion",
              div(class="pill-label","💡 Interpretación"),
              "Las localidades centrales se benefician de mayor densidad de estaciones y mejor malla vial. Las periféricas del sur presentan mayores tiempos por distancias más largas y mayor congestión vehicular."
            ),
            div(class = "analisis-pill pill-conclusion",
              div(class="pill-label","✅ Conclusión"),
              "Existe una brecha de cobertura entre localidades centrales y periféricas del sur. Se recomienda evaluar la apertura de subestaciones en Ciudad Bolívar, Rafael Uribe y Bosa para reducir esta inequidad territorial."
            )
          )
        )
      )
    )
  ),

  # ── TAB 3: G5 — Distribución tiempo de respuesta ──────────────────────────
  nav_panel(
    title = "📊 Distribución TR",
    value = "g5",

    div(class = "container-fluid py-3",
      div(class = "viz-card",
        div(class = "viz-title", "📊 Gráfica 5 · Distribución del tiempo de respuesta"),
        div(class = "viz-subtitle", "Histograma de frecuencias por rango de tiempo en minutos. Mediana general: 9,0 min · Promedio: 11,2 min."),

        fluidRow(
          column(3,
            div(class = "filtros-panel",
              h6("⚙️ Filtros", style="font-weight:700; color:#0d2b5e; margin-bottom:12px;"),
              sliderInput("g5_rango", "Rango de tiempo a mostrar (min):",
                          min = 0, max = 120, value = c(0, 60), step = 5),
              hr(),
              radioButtons("g5_tipo", "Tipo de visualización:",
                           choices  = c("Histograma (conteo)"     = "hist",
                                        "Histograma (porcentaje)" = "pct",
                                        "Densidad"                = "dens"),
                           selected = "hist"),
              hr(),
              checkboxInput("g5_mediana", "Línea de mediana", value = TRUE),
              checkboxInput("g5_promedio", "Línea de promedio", value = TRUE),
              hr(),
              div(style="background:#eafaf1; border-radius:8px; padding:10px;",
                strong("Dato clave", style="font-size:.82rem; color:#1D9E75;"),
                br(),
                span("55,2% de los incidentes se atendió en menos de 10 minutos.",
                     style="font-size:.8rem; color:#555;")
              )
            )
          ),
          column(9,
            plotlyOutput("plot_g5", height = "450px")
          )
        ),

        hr(style="margin:20px 0;"),

        fluidRow(
          column(6,
            div(class = "analisis-pill pill-contexto",
              div(class="pill-label","📌 Contexto"),
              "Muestra cómo se distribuyen los incidentes según el tiempo de llegada de la unidad. Se excluyeron 25 registros con tiempos superiores a 120 minutos. Mediana general: 9,0 min. Promedio: 11,2 min."
            ),
            div(class = "analisis-pill pill-analisis",
              div(class="pill-label","🔍 Análisis"),
              "El rango 5–10 minutos es el más frecuente (37,4%). El 55,2% se atendió en menos de 10 minutos. Solo el 4,6% tardó más de 30 minutos. Los tiempos de 60–120 min representan apenas el 0,6%."
            )
          ),
          column(6,
            div(class = "analisis-pill pill-interpretacion",
              div(class="pill-label","💡 Interpretación"),
              "El tiempo de respuesta general de la UAECOB es satisfactorio. El 4,6% con tiempos superiores a 30 minutos merece atención especial ya que en incendios activos ese tiempo puede ser determinante para el resultado de la intervención."
            ),
            div(class = "analisis-pill pill-conclusion",
              div(class="pill-label","✅ Conclusión"),
              "El desempeño es positivo, pero el 4,6% con tiempos > 30 min representa ~930 eventos en 8 meses. Se recomienda implementar alertas automáticas cuando el tiempo supere los 20 minutos para activar unidades de apoyo desde estaciones vecinas."
            )
          )
        )
      )
    )
  ),

  # ── TAB 4: G7 — Top estaciones ────────────────────────────────────────────
  nav_panel(
    title = "🏢 Estaciones",
    value = "g7",

    div(class = "container-fluid py-3",
      div(class = "viz-card",
        div(class = "viz-title", "🏢 Gráfica 7 · Top 12 estaciones de bomberos"),
        div(class = "viz-subtitle", "Carga operativa por estación durante el periodo enero – agosto 2020."),

        fluidRow(
          column(3,
            div(class = "filtros-panel",
              h6("⚙️ Filtros", style="font-weight:700; color:#0d2b5e; margin-bottom:12px;"),
              sliderInput("g7_n", "N.º de estaciones a mostrar:",
                          min = 3, max = 12, value = 12, step = 1),
              hr(),
              radioButtons("g7_orden", "Ordenar:",
                           choices  = c("Mayor carga primero" = "desc",
                                        "Menor carga primero" = "asc"),
                           selected = "desc"),
              hr(),
              selectInput("g7_color", "Color de las barras:",
                          choices = c("Azul institucional" = "#1a5ea8",
                                      "Rojo emergencia"    = "#c0392b",
                                      "Verde operativo"    = "#1D9E75",
                                      "Morado"             = "#6c3483"),
                          selected = "#1a5ea8"),
              hr(),
              checkboxInput("g7_label", "Mostrar etiquetas de valor", value = TRUE)
            )
          ),
          column(9,
            plotlyOutput("plot_g7", height = "460px")
          )
        ),

        hr(style="margin:20px 0;"),

        fluidRow(
          column(6,
            div(class = "analisis-pill pill-contexto",
              div(class="pill-label","📌 Contexto"),
              "La carga operativa por estación es un indicador directo de la demanda territorial de servicios y un insumo clave para decisiones de inversión en infraestructura y dotación de equipos."
            ),
            div(class = "analisis-pill pill-analisis",
              div(class="pill-label","🔍 Análisis"),
              "B-5 Kennedy lidera con 1.905 incidentes, seguida de B-1 Chapinero (1.800) y B-13 Caobos Salazar (1.481). La diferencia entre la primera y la duodécima (B-15 Garcés Navas, 1.018) es de 887 incidentes — casi el doble de carga."
            )
          ),
          column(6,
            div(class = "analisis-pill pill-interpretacion",
              div(class="pill-label","💡 Interpretación"),
              "La concentración de carga en pocas estaciones puede generar saturación operativa, desgaste del personal y deterioro del parque automotor. Se requiere cruzar con capacidad instalada para identificar desequilibrios reales."
            ),
            div(class = "analisis-pill pill-conclusion",
              div(class="pill-label","✅ Conclusión"),
              "B-5 Kennedy y B-1 Chapinero están bajo presión operativa significativamente mayor. Se recomienda un estudio de capacidad vs. demanda y explorar la creación de subestaciones en las zonas de mayor presión."
            )
          )
        )
      )
    )
  ),

  # ── TAB 5: G11 — Heridos por localidad ────────────────────────────────────
  nav_panel(
    title = "🏥 Heridos Localidad",
    value = "g11",

    div(class = "container-fluid py-3",
      div(class = "viz-card",
        div(class = "viz-title", "🏥 Gráfica 11 · Personas heridas por localidad"),
        div(class = "viz-subtitle", "Distribución de víctimas heridas por atención de bomberos según localidad de Bogotá D.C. Total periodo: 1.316 heridos."),

        fluidRow(
          column(3,
            div(class = "filtros-panel",
              h6("⚙️ Filtros", style="font-weight:700; color:#0d2b5e; margin-bottom:12px;"),
              sliderInput("g11_n_loc", "N.º de localidades:",
                          min = 5, max = 20, value = 20, step = 1),
              hr(),
              radioButtons("g11_orden", "Ordenar por:",
                           choices  = c("Mayor heridos primero" = "desc",
                                        "Menor heridos primero" = "asc"),
                           selected = "desc"),
              hr(),
              selectInput("g11_tipo", "Tipo de gráfico:",
                          choices = c("Barras horizontales"   = "barras",
                                      "Lollipop (puntos)"     = "lollipop",
                                      "Barras + % acumulado"  = "acum",
                                      "Mapa de burbujas"      = "mapa"),
                          selected = "barras"),
              hr(),
              div(style="background:#fef0f0; border-radius:8px; padding:10px;",
                strong("Total heridos", style="font-size:.82rem; color:#c0392b;"),
                br(),
                span("1.316 personas heridas durante Ene–Ago 2020",
                     style="font-size:.8rem; color:#555;")
              )
            )
          ),
          column(9,
            plotlyOutput("plot_g11", height = "540px")
          )
        ),

        hr(style="margin:20px 0;"),

        fluidRow(
          column(6,
            div(class = "analisis-pill pill-contexto",
              div(class="pill-label","📌 Contexto"),
              "Los mapas de calor y ranking de localidades muestran la distribución espacial de heridos en las 20 localidades de Bogotá D.C. Los datos corresponden a víctimas directamente relacionadas con incidentes atendidos por bomberos."
            ),
            div(class = "analisis-pill pill-analisis",
              div(class="pill-label","🔍 Análisis"),
              "Suba (140), Kennedy (129) y Teusaquillo (111) concentran el mayor número de heridos. Las cinco primeras localidades acumulan el 45,2% del total. Sumapaz registra 0 heridos por su condición rural y lejanía del casco urbano."
            )
          ),
          column(6,
            div(class = "analisis-pill pill-interpretacion",
              div(class="pill-label","💡 Interpretación"),
              "La concentración de heridos en Suba y Kennedy está relacionada con su alta densidad poblacional y actividad industrial. Teusaquillo, pese a ser pequeña, tiene alta incidencia por su zona comercial y de entretenimiento (Av. El Dorado y alrededores)."
            ),
            div(class = "analisis-pill pill-conclusion",
              div(class="pill-label","✅ Conclusión"),
              "Las localidades con mayor número de heridos requieren dotación específica de paramédicos bomberos y coordinación reforzada con el SAMU. Suba y Kennedy deben tener prioridad en los planes de inversión de la UAECOB para el siguiente periodo."
            )
          )
        )
      )
    )
  ),

  # ── TAB 6: Acerca de ──────────────────────────────────────────────────────
  nav_panel(
    title = "ℹ️ Acerca de",
    value = "about",

    div(class = "container py-4",
      div(class = "viz-card",
        h4("📋 Acerca de esta aplicación", style="color:#0d2b5e; font-weight:700;"),
        hr(),
        fluidRow(
          column(6,
            h6("🗂️ Dataset", style="color:#1a5ea8; font-weight:700;"),
            tags$ul(
              tags$li(strong("Fuente: "), "Unidad Administrativa Especial Cuerpo Oficial de Bomberos Bogotá (UAECOB)"),
              tags$li(strong("URL: "),
                tags$a("datosabiertos.bogota.gov.co",
                       href="https://datosabiertos.bogota.gov.co/dataset/incidente-atendido-por-bomberos",
                       target="_blank")),
              tags$li(strong("Periodo: "), "Enero – Agosto 2020 (junio sin registros en el dataset fuente)"),
              tags$li(strong("Registros: "), "20.228 incidentes"),
              tags$li(strong("Variables: "), "41 columnas (fecha, localidad, estrato, tipo servicio, víctimas, tiempos, coordenadas)")
            ),
            h6("👥 Autores", style="color:#1a5ea8; font-weight:700; margin-top:20px;"),
            tags$ul(
              tags$li("Juan Carlos Rojas Lizarazo"),
              tags$li("Brayan Andres Sierra Zambrano")
            ),
            h6("🏛️ Institución", style="color:#1a5ea8; font-weight:700; margin-top:20px;"),
            p("Fundación Universitaria Los Libertadores · 2026"),
            p("Curso: Herramientas y Visualización de Datos — Proyecto 2")
          ),
          column(6,
            h6("📊 Visualizaciones", style="color:#1a5ea8; font-weight:700;"),
            tags$table(class="table table-sm table-hover",
              tags$thead(tags$tr(tags$th("Graf."), tags$th("Tipo"), tags$th("Descripción"))),
              tags$tbody(
                tags$tr(tags$td("G2"), tags$td("Barras"), tags$td("Incidentes por día de la semana")),
                tags$tr(tags$td("G4"), tags$td("Lollipop"), tags$td("Tiempo de respuesta mediano por localidad")),
                tags$tr(tags$td("G5"), tags$td("Histograma"), tags$td("Distribución del tiempo de respuesta")),
                tags$tr(tags$td("G7"), tags$td("Barras horiz."), tags$td("Top 12 estaciones de bomberos")),
                tags$tr(tags$td("G11"), tags$td("Barras / Lollipop"), tags$td("Personas heridas por localidad"))
              )
            ),
            h6("🛠️ Tecnologías", style="color:#1a5ea8; font-weight:700; margin-top:20px;"),
            tags$ul(
              tags$li(strong("Framework: "), "R Shiny + bslib (Bootstrap 5)"),
              tags$li(strong("Visualización: "), "ggplot2 + plotly"),
              tags$li(strong("Interactividad: "), "sliderInput, selectInput, checkboxGroupInput"),
              tags$li(strong("Despliegue: "), "shinyapps.io")
            )
          )
        )
      )
    )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# 4. SERVER
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ── Filtros reactivos globales ─────────────────────────────────────────────
  df_filtrado <- reactive({
    if (is.null(df_global)) return(NULL)
    d <- df_global
    if (!is.null(input$filtro_mes_global) && input$filtro_mes_global != 0)
      d <- d[d$MES == as.integer(input$filtro_mes_global), ]
    if (!is.null(input$filtro_tr))
      d <- d[!is.na(d$TR_limpio) & d$TR_limpio >= input$filtro_tr[1] & d$TR_limpio <= input$filtro_tr[2], ]
    d
  })

  # ── KPIs ──────────────────────────────────────────────────────────────────
  output$kpi_total <- renderText({
    if (!is.null(df_filtrado())) format(nrow(df_filtrado()), big.mark=",")
    else "20,228"
  })
  output$kpi_heridos <- renderText({
    if (!is.null(df_filtrado())) format(sum(df_filtrado()$TOTAL_HERIDOS), big.mark=",")
    else "1,316"
  })
  output$kpi_rescatados <- renderText({
    if (!is.null(df_filtrado())) format(sum(df_filtrado()$TOTAL_RESCATADOS), big.mark=",")
    else "1,046"
  })
  output$kpi_tr <- renderText({
    if (!is.null(df_filtrado())) {
      med <- median(df_filtrado()$TR_limpio, na.rm=TRUE)
      if (!is.na(med)) paste0(round(med,1)," min") else "N/D"
    } else "9,0 min"
  })

  # ── G2 — Días de la semana ─────────────────────────────────────────────────
  output$plot_g2 <- renderPlotly({
    mes_sel <- if (!is.null(input$filtro_mes_global)) as.integer(input$filtro_mes_global) else 0

    d <- datos_g2_base
    if (!is.null(df_global) && mes_sel != 0)
      d <- d[d$MES == mes_sel, ]

    d_agg <- if (!is.null(df_global)) {
      dias_map <- c("Monday"="Lunes","Tuesday"="Martes","Wednesday"="Miércoles",
                    "Thursday"="Jueves","Friday"="Viernes","Saturday"="Sábado","Sunday"="Domingo")
      df_use <- if (mes_sel != 0) df_global[df_global$MES == mes_sel, ] else df_global
      df_use %>%
        mutate(DIA = dias_map[DIA_SEM]) %>%
        filter(DIA %in% input$g2_dias) %>%
        group_by(DIA) %>%
        summarise(n = n(), .groups="drop") %>%
        mutate(DIA = factor(DIA, levels = ORDEN_DIAS)) %>%
        arrange(DIA)
    } else {
      datos_g2_base %>%
        filter(DIA %in% input$g2_dias) %>%
        group_by(DIA) %>%
        summarise(n = sum(n), .groups="drop") %>%
        mutate(DIA = factor(DIA, levels = ORDEN_DIAS)) %>%
        arrange(DIA)
    }

    pal <- switch(input$g2_color,
      azul  = COL$azul,
      calor = NULL,
      gris  = "#909497"
    )

    p <- ggplot(d_agg, aes(x=DIA, y=n,
                            fill = if (input$g2_color=="calor") n else DIA,
                            text = paste0("<b>", DIA, "</b><br>", format(n,big.mark=","), " incidentes"))) +
      geom_col(width = 0.7, show.legend = FALSE) +
      tema_uaecob() +
      labs(title = "Incidentes por día de la semana — UAECOB 2020",
           x = NULL, y = "N.º de incidentes",
           caption = CAPTION_BASE) +
      scale_y_continuous(labels = comma) +
      theme(axis.text.x = element_text(size=10))

    if (input$g2_color == "calor") {
      p <- p + scale_fill_gradient(low="#aec6e8", high="#1a5ea8")
    } else {
      p <- p + scale_fill_manual(values = rep(pal, 7))
    }

    if (input$g2_mostrar_valores) {
      p <- p + geom_text(aes(label=format(n,big.mark=",")),
                         vjust=-0.4, size=3.2, color="#333")
    }

    ggplotly(p, tooltip="text") %>%
      layout(hoverlabel = list(bgcolor="white", font=list(size=12)),
             margin = list(t=50)) %>%
      config(displayModeBar=FALSE)
  })

  # ── G4 — Tiempo de respuesta por localidad ─────────────────────────────────
  output$plot_g4 <- renderPlotly({
    mes_sel <- if (!is.null(input$filtro_mes_global)) as.integer(input$filtro_mes_global) else 0

    d <- if (!is.null(df_global)) {
      df_use <- if (mes_sel != 0) df_global[df_global$MES == mes_sel, ] else df_global
      top20 <- df_use %>% count(LOCALIDAD_L) %>% top_n(input$g4_n_loc, n) %>% pull(LOCALIDAD_L)
      df_use %>%
        filter(LOCALIDAD_L %in% top20) %>%
        group_by(LOCALIDAD_L) %>%
        summarise(tr_med = median(TR_limpio, na.rm=TRUE), .groups="drop") %>%
        filter(!is.na(tr_med))
    } else {
      datos_g4_base %>%
        group_by(LOCALIDAD_L) %>%
        summarise(tr_med = mean(tr_med), .groups="drop") %>%
        slice_head(n = input$g4_n_loc)
    }

    d <- d %>%
      mutate(COLOR = ifelse(tr_med <= 9, "≤ 9 min (bueno)", "> 9 min (revisar)"),
             LOCALIDAD_L = if (input$g4_orden=="asc") reorder(LOCALIDAD_L, tr_med)
                           else reorder(LOCALIDAD_L, -tr_med))

    p <- ggplot(d, aes(y=LOCALIDAD_L, x=tr_med, color=COLOR,
                       text=paste0("<b>", LOCALIDAD_L,"</b><br>Mediana: ",
                                   round(tr_med,1)," min"))) +
      geom_segment(aes(x=0, xend=tr_med, y=LOCALIDAD_L, yend=LOCALIDAD_L),
                   color="#dddddd", linewidth=1.2) +
      geom_point(size=4.5) +
      scale_color_manual(values = c("≤ 9 min (bueno)"   = COL$verde,
                                    "> 9 min (revisar)"  = COL$rojo),
                         name="") +
      tema_uaecob() +
      labs(title="Tiempo de respuesta mediano por localidad",
           x="Minutos (mediana)", y=NULL, caption=CAPTION_BASE) +
      scale_x_continuous(limits=c(0, NA), labels=function(x) paste0(x," min")) +
      theme(legend.position="top")

    if (input$g4_linea_ref)
      p <- p + geom_vline(xintercept=9, linetype="dashed",
                          color="#d68910", linewidth=0.8) +
               annotate("text", x=9.15, y=1, label="Ref: 9 min",
                        color="#d68910", size=3, hjust=0)

    ggplotly(p, tooltip="text") %>%
      layout(legend=list(orientation="h", y=1.05),
             margin=list(t=50, l=150)) %>%
      config(displayModeBar=FALSE)
  })

  # ── G5 — Distribución tiempo de respuesta (plotly nativo) ──────────────────
  output$plot_g5 <- renderPlotly({
    mes_sel <- if (!is.null(input$filtro_mes_global)) as.integer(input$filtro_mes_global) else 0

    tr_vals <- if (!is.null(df_global)) {
      df_use <- if (mes_sel != 0) df_global[df_global$MES == mes_sel, ] else df_global
      df_use$TR_limpio[!is.na(df_use$TR_limpio) &
                       df_use$TR_limpio >= input$g5_rango[1] &
                       df_use$TR_limpio <= input$g5_rango[2]]
    } else {
      datos_g5_base$TR_limpio[datos_g5_base$TR_limpio >= input$g5_rango[1] &
                               datos_g5_base$TR_limpio <= input$g5_rango[2]]
    }

    tr_vals  <- tr_vals[!is.na(tr_vals)]
    med_val  <- median(tr_vals)
    prom_val <- mean(tr_vals)

    # Calcular bins manualmente para control total (evita bug ggplotly -> torta)
    n_bins <- 24
    h      <- hist(tr_vals, breaks = n_bins, plot = FALSE)
    mids   <- h$mids
    counts <- h$counts
    total  <- sum(counts)
    pcts   <- counts / total * 100

    if (input$g5_tipo == "dens") {
      dens <- density(tr_vals, from = input$g5_rango[1], to = input$g5_rango[2], n = 200)
      fig  <- plot_ly(x = dens$x, y = dens$y, type = "scatter", mode = "lines",
                      fill = "tozeroy", fillcolor = paste0(COL$azul,"55"),
                      line = list(color = COL$azul, width = 2), name = "Densidad")
      if (input$g5_mediana)
        fig <- fig %>% add_segments(x=med_val, xend=med_val, y=0, yend=max(dens$y),
                                    line=list(color=COL$rojo, dash="dash", width=2),
                                    name=paste0("Mediana: ",round(med_val,1)," min"))
      if (input$g5_promedio)
        fig <- fig %>% add_segments(x=prom_val, xend=prom_val, y=0, yend=max(dens$y),
                                    line=list(color=COL$ambar, dash="dot", width=2),
                                    name=paste0("Promedio: ",round(prom_val,1)," min"))
      return(fig %>% layout(
        title  = list(text="Distribución del tiempo de respuesta — Densidad", font=list(size=13)),
        xaxis  = list(title="Tiempo de respuesta (min)"),
        yaxis  = list(title="Densidad"),
        legend = list(orientation="h", y=-0.2),
        margin = list(t=55), plot_bgcolor="white", paper_bgcolor="white"
      ) %>% config(displayModeBar=FALSE))
    }

    if (input$g5_tipo == "pct") {
      y_vals  <- pcts
      y_title <- "% del total"
      titulo  <- "Distribución del tiempo de respuesta — Porcentaje"
      color   <- COL$teal
      hover   <- paste0(round(mids,1)," min<br>", round(pcts,1),"%")
    } else {
      y_vals  <- counts
      y_title <- "N.º de incidentes"
      titulo  <- "Distribución del tiempo de respuesta — Histograma de frecuencias"
      color   <- COL$verde
      hover   <- paste0(round(mids,1)," min<br>Incidentes: ", counts)
    }

    fig <- plot_ly(x=mids, y=y_vals, type="bar",
                   marker=list(color=color, line=list(color="white", width=0.5)),
                   text=hover, hoverinfo="text", name=y_title)

    if (input$g5_mediana)
      fig <- fig %>% add_segments(x=med_val, xend=med_val, y=0, yend=max(y_vals)*1.1,
                                  line=list(color=COL$rojo, dash="dash", width=2),
                                  name=paste0("Mediana: ",round(med_val,1)," min"))
    if (input$g5_promedio)
      fig <- fig %>% add_segments(x=prom_val, xend=prom_val, y=0, yend=max(y_vals)*1.1,
                                  line=list(color=COL$ambar, dash="dot", width=2),
                                  name=paste0("Promedio: ",round(prom_val,1)," min"))

    fig %>% layout(
      title  = list(text=titulo, font=list(size=13)),
      xaxis  = list(title="Tiempo de respuesta (min)"),
      yaxis  = list(title=y_title),
      bargap = 0.05, showlegend=TRUE,
      legend = list(orientation="h", y=-0.2),
      margin = list(t=55), plot_bgcolor="white", paper_bgcolor="white"
    ) %>% config(displayModeBar=FALSE)
  })

  # ── G7 — Top estaciones ────────────────────────────────────────────────────
  output$plot_g7 <- renderPlotly({
    mes_sel <- if (!is.null(input$filtro_mes_global)) as.integer(input$filtro_mes_global) else 0

    d <- if (!is.null(df_global)) {
      df_use <- if (mes_sel != 0) df_global[df_global$MES == mes_sel, ] else df_global
      df_use %>%
        count(ESTACIÓN) %>%
        filter(!is.na(ESTACIÓN)) %>%
        top_n(input$g7_n, n)
    } else {
      datos_g7_base %>%
        group_by(ESTACIÓN) %>%
        summarise(n=sum(n), .groups="drop") %>%
        top_n(input$g7_n, n)
    }

    d <- d %>%
      mutate(ESTACIÓN = if(input$g7_orden=="desc") reorder(ESTACIÓN, n)
                        else reorder(ESTACIÓN, -n))

    p <- ggplot(d, aes(x=n, y=ESTACIÓN, fill=n,
                       text=paste0("<b>", ESTACIÓN,"</b><br>", format(n,big.mark=",")," incidentes"))) +
      geom_col(width=0.7, show.legend=FALSE) +
      scale_fill_gradient(low=paste0(input$g7_color,"88"), high=input$g7_color) +
      tema_uaecob() +
      labs(title="Top estaciones de bomberos por carga operativa — UAECOB 2020",
           x="N.º de incidentes", y=NULL, caption=CAPTION_BASE) +
      scale_x_continuous(labels=comma)

    if (input$g7_label)
      p <- p + geom_text(aes(label=format(n,big.mark=",")),
                         hjust=-0.15, size=3.2, color="#333")

    ggplotly(p, tooltip="text") %>%
      layout(margin=list(t=50, l=160, r=80)) %>%
      config(displayModeBar=FALSE)
  })

  # ── G11 — Heridos por localidad (barras + mapa de burbujas) ─────────────────
  output$plot_g11 <- renderPlotly({
    mes_sel <- if (!is.null(input$filtro_mes_global)) as.integer(input$filtro_mes_global) else 0

    d <- if (!is.null(df_global)) {
      df_use <- if (mes_sel != 0) df_global[df_global$MES == mes_sel, ] else df_global
      df_use %>%
        filter(!LOCALIDAD_L %in% c("FUERA D.C.","SUMAPAZ"), !is.na(LOCALIDAD_L)) %>%
        group_by(LOCALIDAD_L) %>%
        summarise(heridos=sum(TOTAL_HERIDOS), .groups="drop") %>%
        filter(heridos > 0) %>%
        top_n(input$g11_n_loc, heridos)
    } else {
      datos_g11_base %>%
        group_by(LOCALIDAD_L) %>%
        summarise(heridos=sum(heridos), .groups="drop") %>%
        filter(heridos > 0) %>%
        top_n(input$g11_n_loc, heridos)
    }

    d <- d %>%
      mutate(
        pct = heridos / sum(heridos) * 100,
        LOCALIDAD_L = if(input$g11_orden=="desc") reorder(LOCALIDAD_L, heridos)
                      else reorder(LOCALIDAD_L, -heridos),
        TOOLTIP = paste0("<b>", LOCALIDAD_L,"</b><br>",
                         heridos," heridos (",round(pct,1),"%)")
      )

    # Coordenadas centroides de localidades de Bogotá
    coords_loc <- data.frame(
      LOCALIDAD_L = c("USAQUÉN","CHAPINERO","SANTA FE","SAN CRISTÓBAL","USME",
                      "TUNJUELITO","BOSA","KENNEDY","FONTIBÓN","ENGATIVÁ",
                      "SUBA","BARRIOS UNIDOS","TEUSAQUILLO","LOS MÁRTIRES",
                      "ANTONIO NARIÑO","PUENTE ARANDA","LA CANDELARIA",
                      "RAFAEL URIBE URIBE","CIUDAD BOLÍVAR","SUMAPAZ"),
      lat = c(4.7016,4.6549,4.5988,4.5683,4.4855,4.5757,4.6194,4.6276,
              4.6741,4.7101,4.7404,4.6637,4.6277,4.6043,4.5858,4.6219,
              4.5961,4.5703,4.5124,4.2765),
      lon = c(-74.0308,-74.0560,-74.0803,-74.0896,-74.1318,-74.1261,
              -74.1872,-74.1503,-74.1418,-74.1066,-74.0834,-74.0830,
              -74.0972,-74.0949,-74.1128,-74.1231,-74.0760,-74.1126,
              -74.1710,-74.3544),
      stringsAsFactors = FALSE
    )

    if (input$g11_tipo == "mapa") {
      d_map <- d %>%
        mutate(LOCALIDAD_L = as.character(LOCALIDAD_L)) %>%
        left_join(coords_loc, by = "LOCALIDAD_L") %>%
        filter(!is.na(lat))

      return(plot_ly(d_map,
        lat        = ~lat,
        lon        = ~lon,
        type       = "scattermapbox",
        mode       = "markers",
        marker     = list(
          size     = ~sqrt(heridos) * 3,
          color    = ~heridos,
          colorscale = list(c(0,"#f5b7b1"), c(1, COL$rojo)),
          showscale  = TRUE,
          colorbar   = list(title = "Heridos"),
          opacity  = 0.8
        ),
        text       = ~TOOLTIP,
        hoverinfo  = "text"
      ) %>% layout(
        title      = list(text="Mapa de heridos por localidad — UAECOB 2020", font=list(size=13)),
        mapbox     = list(
          style    = "carto-positron",
          center   = list(lat=4.6097, lon=-74.0817),
          zoom     = 10
        ),
        margin     = list(t=55, l=0, r=0, b=0)
      ) %>% config(displayModeBar=FALSE))
    }

    if (input$g11_tipo == "lollipop") {
      p <- ggplot(d, aes(y=LOCALIDAD_L, x=heridos, color=heridos, text=TOOLTIP)) +
        geom_segment(aes(x=0, xend=heridos, y=LOCALIDAD_L, yend=LOCALIDAD_L),
                     color="#dddddd", linewidth=1) +
        geom_point(size=4) +
        scale_color_gradient(low="#f5b7b1", high=COL$rojo, guide="none") +
        tema_uaecob() +
        labs(title="Personas heridas por localidad — Lollipop",
             x="N.º de heridos", y=NULL, caption=CAPTION_BASE) +
        scale_x_continuous(labels=comma)

    } else if (input$g11_tipo == "acum") {
      d_acum <- d %>%
        arrange(desc(heridos)) %>%
        mutate(cum_pct = cumsum(heridos)/sum(heridos)*100,
               LOCALIDAD_L = factor(LOCALIDAD_L, levels=rev(levels(LOCALIDAD_L))))
      p <- ggplot(d_acum, aes(x=heridos, y=LOCALIDAD_L, fill=heridos,
                               text=paste0(TOOLTIP,"<br>Acum: ",round(cum_pct,1),"%"))) +
        geom_col(width=0.7, show.legend=FALSE) +
        scale_fill_gradient(low="#f5b7b1", high=COL$rojo) +
        geom_text(aes(label=paste0(round(cum_pct,0),"%")),
                  hjust=-0.1, size=2.8, color=COL$gris) +
        tema_uaecob() +
        labs(title="Heridos por localidad con % acumulado — UAECOB 2020",
             x="N.º de heridos", y=NULL, caption=CAPTION_BASE) +
        scale_x_continuous(labels=comma, expand=expansion(mult=c(0,.18)))

    } else {
      p <- ggplot(d, aes(x=heridos, y=LOCALIDAD_L, fill=heridos, text=TOOLTIP)) +
        geom_col(width=0.7, show.legend=FALSE) +
        scale_fill_gradient(low="#f5b7b1", high=COL$rojo) +
        geom_text(aes(label=paste0(heridos," (",round(pct,1),"%)")),
                  hjust=-0.1, size=3, color="#333") +
        tema_uaecob() +
        labs(title="Personas heridas por localidad — UAECOB 2020",
             x="N.º de heridos", y=NULL, caption=CAPTION_BASE) +
        scale_x_continuous(labels=comma, expand=expansion(mult=c(0,.18)))
    }

    ggplotly(p, tooltip="text") %>%
      layout(margin=list(t=50, l=150, r=80)) %>%
      config(displayModeBar=FALSE)
  })
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. ARRANCAR APP
# ══════════════════════════════════════════════════════════════════════════════
shinyApp(ui = ui, server = server)
