library(shiny)
library(bslib)

options(shiny.maxRequestSize = 500 * 1024^2) ## 500 MB
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)
library(scales)
library(plotly)
library(DT)
library(readr)

library(staat1cho)

## Fonts ----

if (
  requireNamespace("showtext", quietly = TRUE) &&
    requireNamespace("sysfonts", quietly = TRUE)
) {
  sysfonts::font_add_google("Plus Jakarta Sans", "plus_jakarta")
  showtext::showtext_auto()
  FONT_FMLY <- "plus_jakarta"
} else {
  FONT_FMLY <- "sans"
}

## Npuls kleuren ----

NPULS_BLAUW <- "#3D68EC"
NPULS_ORANJE <- "#DD784B"
NPULS_GROEN <- "#00AF81"
NPULS_GEEL <- "#F4D74B"
NPULS_ROZE <- "#F4D9DC"
NPULS_ZWART <- "#000000"
NPULS_WIT <- "#FFFFFF"
NPULS_LICHT_GROEN <- "#CCEEE6"
KLEUR_NEUTRAAL <- "#6B7280"

KLEUREN_TERMIJN <- c(
  "3 jaar" = NPULS_ORANJE,
  "5 jaar" = NPULS_BLAUW,
  "8 jaar" = NPULS_GROEN
)

KLEUREN_UITVAL <- c(
  "binnen 1 jaar" = NPULS_ORANJE,
  "binnen 3 jaar" = NPULS_BLAUW
)

KLEUREN_WISSEL <- c(
  "binnen 1 jaar" = NPULS_GEEL,
  "binnen 3 jaar" = NPULS_GROEN
)

KLEUREN <- c(
  ## Status
  "Diploma behaald" = NPULS_GROEN,
  "Zittend" = NPULS_BLAUW,
  "Uitgevallen" = NPULS_ORANJE,
  ## Geslacht
  "man" = NPULS_BLAUW,
  "vrouw" = "#D96EA8",
  ## Opleidingsvorm
  "voltijd" = NPULS_BLAUW,
  "deeltijd" = NPULS_ORANJE,
  ## Int student
  "geen internationale student" = NPULS_BLAUW,
  "internationale student" = NPULS_GROEN,
  ## Sectoren
  "gezondheidszorg" = NPULS_GROEN,
  "economie" = NPULS_BLAUW,
  "gedrag & maatschappij" = NPULS_ORANJE,
  "gedrag en maatschappij" = NPULS_ORANJE,
  "recht" = NPULS_GEEL,
  "natuur" = "#007A5B",
  "techniek" = "#1A3FA0",
  "onderwijs" = NPULS_GROEN,
  "taal & cultuur" = "#B5570D",
  "taal en cultuur" = "#B5570D",
  ## Rendement
  "Diploma binnen 3 jaar" = NPULS_GROEN,
  "Diploma na 3 jaar" = NPULS_GEEL,
  "Diploma binnen 5 jaar" = NPULS_BLAUW,
  "Diploma na 5 jaar" = NPULS_ORANJE,
  "Diploma binnen 5-8 jaar" = "#00796B",
  "Diploma binnen 8 jaar" = "#1A3FA0",
  "Diploma na 8 jaar" = "#B5570D",
  "Geen diploma" = "#D1D5DB",
  "Onbekend" = "#E5E7EB",
  "Onbekend (diplomajaar voor instroomjaar)" = "#E5E7EB",
  ## Uitval
  "Niet uitgevallen" = NPULS_GROEN,
  "Uitgevallen binnen 1 jaar" = NPULS_ORANJE,
  "Uitgevallen in 2e of 3e jaar" = "#DD9E4B",
  "Uitgevallen na 3 jaar" = "#B5570D",
  "Na 1 jaar nog ingeschreven of diploma behaald" = NPULS_GROEN,
  "Na 3 jaar nog ingeschreven of diploma behaald" = NPULS_LICHT_GROEN,
  "Uitgevallen binnen 3 jaar" = "#DD9E4B",
  ## Studiewissel
  "Niet gewisseld" = "#9CA3AF",
  "Gewisseld binnen 1 jaar" = NPULS_GEEL,
  "Gewisseld binnen 3 jaar" = NPULS_GROEN,
  "Gewisseld in het 2e of 3e jaar" = NPULS_GROEN,
  "Niet gewisseld binnen 1 jaar" = "#D1D5DB",
  "Niet gewisseld binnen 3 jaar" = "#E5E7EB",
  "Geen switch bepaald" = "#F3F4F6"
)

## Datadictionary ----

DICT <- data.frame(
  Categorie = c(
    rep("Studentkenmerken", 12),
    rep("Status", 2),
    rep("Rendement", 4),
    rep("Uitval", 4),
    rep("Studiewissel", 11)
  ),
  Kolom = c(
    "inschrijvingsjaar",
    "geslacht",
    "locatie",
    "opleidingscode",
    "opleidingsvorm",
    "opleidingsniveau",
    "int_student",
    "indicatie_EER",
    "sector",
    "leeftijd_bij_instroom",
    "postcode4_student_1okt",
    "postcode4_vooropleiding_voorHO",
    "status",
    "soortdiploma",
    "rendement_3jr",
    "rendement_5jr",
    "rendement_8jr",
    "rendement",
    "uitval_xjr",
    "uitval_1jr",
    "uitval_3jr",
    "uitval",
    "studiewissel_1jr",
    "studiewissel_3jr",
    "opleidingscode_na_switch1jr",
    "opleidingsvorm_na_switch1jr",
    "opleidingsniveau_na_switch1jr",
    "sector_na_switch1jr",
    "opleidingscode_na_switch3jr",
    "opleidingsvorm_na_switch3jr",
    "opleidingsniveau_na_switch3jr",
    "sector_na_switch3jr",
    "studiewissel"
  ),
  Omschrijving = c(
    "Jaar van eerste inschrijving (cohortjaar)",
    "Geslacht van de student",
    "Vestigingslocatie van de opleiding",
    "CROHO-opleidingscode",
    "Opleidingsvorm",
    "Niveau van de opleiding",
    "Indicator internationale student",
    "Indicator EER-student (Europese Economische Ruimte)",
    "Sector van de opleiding (CROHO-onderdeel)",
    "Leeftijd van de student bij aanvang opleiding",
    "Postcode (4-cijferig) van de student op 1 oktober",
    "Postcode (4-cijferig) van vooropleiding voor HO",
    "Status na afloop van de observatieperiode",
    "Soort behaald diploma",
    "Diplomaresultaat binnen 3 jaar na instroom",
    "Diplomaresultaat binnen 5 jaar na instroom",
    "Diplomaresultaat binnen 8 jaar na instroom",
    "Samengevatte rendementsuitkomst",
    "Uitvalstatus binnen een variabele termijn",
    "Uitvalstatus na 1 jaar",
    "Uitvalstatus na 3 jaar",
    "Samengevatte uitvaluitkomst",
    "Studiewissel binnen 1 jaar na instroom",
    "Studiewissel binnen 3 jaar na instroom",
    "Opleidingscode na wissel (1 jaar)",
    "Opleidingsvorm na wissel (1 jaar)",
    "Opleidingsniveau na wissel (1 jaar)",
    "Sector na wissel (1 jaar)",
    "Opleidingscode na wissel (3 jaar)",
    "Opleidingsvorm na wissel (3 jaar)",
    "Opleidingsniveau na wissel (3 jaar)",
    "Sector na wissel (3 jaar)",
    "Samengevatte studiewisseluitkomst"
  ),
  stringsAsFactors = FALSE
)

## Vereiste 1CHO-kolommen ----

VEREISTE_KOLOMMEN <- c(
  "persoonsgebonden_nummer",
  "inschrijvingsjaar",
  "verblijfsjaar_actuele_instelling",
  "diplomajaar",
  "soort_hoger_onderwijs",
  "soort_inschrijving_actuele_instelling",
  "geslacht",
  "opleidingsvorm",
  "indicatie_internationale_student",
  "indicatie_eer_actueel",
  "croho_onderdeel_actuele_opleiding",
  "soort_diploma_instelling",
  "opleiding_actueel_equivalent",
  "type_hoger_onderwijs_binnen_soort_hoger_onderwijs",
  "leeftijd_per_peildatum_1_oktober",
  "postcodecijfers_student_op_1_oktober",
  "postcodecijfers_van_de_hoogste_vooropl_voor_het_ho",
  "opleidingscode_naam_opleiding",
  "vestigingsnummer_gemeentenaam_volgens_rio"
)

## Hulpfuncties ----

kleur_voor <- function(waarden) {
  waarden <- as.character(waarden)
  gevonden <- KLEUREN[waarden]
  ontbrekend <- is.na(gevonden)
  if (any(ontbrekend)) {
    extra <- scales::hue_pal()(sum(ontbrekend))
    gevonden[ontbrekend] <- extra
  }
  names(gevonden) <- waarden
  gevonden
}

thema <- function() {
  theme_minimal(base_size = 12, base_family = FONT_FMLY) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 12),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      axis.text = element_text(color = "#374151"),
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

leeg_plot <- function(bericht = "Geen data voor deze selectie") {
  ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = bericht,
      color = KLEUR_NEUTRAAL,
      size = 5
    ) +
    theme_void()
}

pct_bar <- function(data, x, titel = NULL) {
  if (nrow(data) == 0) {
    return(leeg_plot())
  }
  tel <- data |>
    count(waarde = as.character({{ x }})) |>
    filter(!is.na(waarde)) |>
    mutate(pct = n / sum(n))
  if (nrow(tel) == 0) {
    return(leeg_plot())
  }
  kleuren <- kleur_voor(tel$waarde)
  ggplot(tel, aes(x = fct_reorder(waarde, pct), y = pct, fill = waarde)) +
    geom_col(show.legend = FALSE) +
    geom_text(
      aes(label = paste0(round(pct * 100), "% (n=", n, ")")),
      hjust = -0.05,
      size = 3.3
    ) +
    scale_y_continuous(labels = percent, limits = c(0, 1.2)) +
    scale_fill_manual(values = kleuren, breaks = names(kleuren)) +
    coord_flip() +
    labs(x = NULL, y = NULL, title = titel) +
    thema() +
    theme(legend.position = "none")
}

trend_lijn <- function(
  data,
  y_col,
  kleur_col,
  kleuren_schaal,
  titel,
  y_label = percent
) {
  if (nrow(data) == 0 || nrow(data |> filter(!is.na(.data[[y_col]]))) == 0) {
    return(leeg_plot())
  }
  lijn_data <- data |>
    group_by(.data[[kleur_col]]) |>
    filter(n() >= 2) |>
    ungroup()
  ggplot(
    data,
    aes(x = inschrijvingsjaar, y = .data[[y_col]], color = .data[[kleur_col]])
  ) +
    geom_line(data = lijn_data, linewidth = 1.3) +
    geom_point(size = 3) +
    scale_y_continuous(labels = y_label) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
    scale_color_manual(values = kleuren_schaal) +
    labs(x = "Instroomjaar", y = NULL, title = titel) +
    thema()
}

instroom_trend <- function(data, titel = "Instroom per cohortjaar") {
  if (nrow(data) == 0) {
    return(leeg_plot())
  }
  agg <- data |> count(inschrijvingsjaar)
  if (nrow(agg) == 0) {
    return(leeg_plot())
  }
  lijn_data <- if (nrow(agg) >= 2) agg else agg[0, ]
  ggplot(agg, aes(x = inschrijvingsjaar, y = n)) +
    geom_line(data = lijn_data, color = NPULS_BLAUW, linewidth = 1.3) +
    geom_point(color = NPULS_BLAUW, size = 3) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
    labs(x = "Instroomjaar", y = "Studenten", title = titel) +
    thema()
}

rendement_trend <- function(data, titel = "Rendement per cohortjaar") {
  if (nrow(data) == 0) {
    return(leeg_plot())
  }
  agg <- data |>
    mutate(
      `3 jaar` = rendement_3jr == "Diploma binnen 3 jaar",
      `5 jaar` = rendement_5jr == "Diploma binnen 5 jaar",
      `8 jaar` = rendement_8jr == "Diploma binnen 8 jaar"
    ) |>
    group_by(inschrijvingsjaar) |>
    summarise(
      `3 jaar` = mean(`3 jaar`, na.rm = TRUE),
      `5 jaar` = mean(`5 jaar`, na.rm = TRUE),
      `8 jaar` = mean(`8 jaar`, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) |>
    filter(n >= 5) |>
    pivot_longer(
      c(`3 jaar`, `5 jaar`, `8 jaar`),
      names_to = "termijn",
      values_to = "pct"
    ) |>
    filter(!is.na(pct))
  if (nrow(agg) == 0) {
    return(leeg_plot())
  }
  trend_lijn(agg, "pct", "termijn", KLEUREN_TERMIJN, titel)
}

uitval_trend <- function(data, titel = "Uitval per cohortjaar") {
  if (nrow(data) == 0) {
    return(leeg_plot())
  }
  agg <- data |>
    mutate(
      `binnen 1 jaar` = uitval_1jr == "Uitgevallen binnen 1 jaar",
      `binnen 3 jaar` = uitval_3jr == "Uitgevallen binnen 3 jaar"
    ) |>
    group_by(inschrijvingsjaar) |>
    summarise(
      `binnen 1 jaar` = mean(`binnen 1 jaar`, na.rm = TRUE),
      `binnen 3 jaar` = mean(`binnen 3 jaar`, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) |>
    filter(n >= 5) |>
    pivot_longer(
      c(`binnen 1 jaar`, `binnen 3 jaar`),
      names_to = "termijn",
      values_to = "pct"
    ) |>
    filter(!is.na(pct))
  if (nrow(agg) == 0) {
    return(leeg_plot())
  }
  trend_lijn(agg, "pct", "termijn", KLEUREN_UITVAL, titel)
}

wissel_trend <- function(data, titel = "Studiewissel per cohortjaar") {
  if (nrow(data) == 0) {
    return(leeg_plot())
  }
  agg <- data |>
    mutate(
      `binnen 1 jaar` = studiewissel_1jr == "Gewisseld binnen 1 jaar",
      `binnen 3 jaar` = studiewissel_3jr == "Gewisseld binnen 3 jaar"
    ) |>
    group_by(inschrijvingsjaar) |>
    summarise(
      `binnen 1 jaar` = mean(`binnen 1 jaar`, na.rm = TRUE),
      `binnen 3 jaar` = mean(`binnen 3 jaar`, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) |>
    filter(n >= 5) |>
    pivot_longer(
      c(`binnen 1 jaar`, `binnen 3 jaar`),
      names_to = "termijn",
      values_to = "pct"
    ) |>
    filter(!is.na(pct))
  if (nrow(agg) == 0) {
    return(leeg_plot())
  }
  trend_lijn(agg, "pct", "termijn", KLEUREN_WISSEL, titel)
}

vb <- function(label, waarde, bg = NPULS_BLAUW, fg = NPULS_GEEL) {
  value_box(
    title = label,
    value = waarde,
    theme = value_box_theme(bg = bg, fg = fg)
  )
}

pct_label <- function(data, conditie) {
  if (nrow(data) == 0) {
    return("—")
  }
  paste0(round(mean(conditie(data), na.rm = TRUE) * 100), "%")
}

n_label <- function(n) {
  formatC(n, format = "d", big.mark = ".")
}

## CSS ----

npuls_css <- "
@import url('https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;600&display=swap');

*, body { font-family: 'Plus Jakarta Sans', sans-serif !important; }

/* Upload scherm */
.upload-achtergrond {
  min-height: 100vh;
  display: flex;
  align-items: center;
  justify-content: center;
  background: #FFFFFF;
  padding: 2rem;
}
.upload-card {
  width: 480px;
  max-width: 100%;
  box-shadow: 0 4px 32px rgba(61,104,236,0.12);
}
.upload-card .card-header {
  background-color: #3D68EC !important;
  color: #F4D74B !important;
  border-bottom: none !important;
  padding: 1.25rem 1.5rem;
}
.upload-card .card-header .npuls-payoff { color: #F4D74B; }
.upload-card .card-body { padding: 1.75rem 1.5rem; }
.upload-verwerk-btn {
  background-color: #3D68EC !important;
  color: #F4D74B !important;
  border: none !important;
  font-weight: 600 !important;
  width: 100%;
  margin-top: 0.5rem;
  padding: 0.65rem;
  font-size: 0.95rem;
}
.upload-verwerk-btn:hover {
  background-color: #2850C8 !important;
}
/* Dashboard header */
.dashboard-header {
  background-color: #F4D9DC;
  border-bottom: 3px solid #DD784B;
  padding: 0.5rem 1.25rem;
  display: flex;
  align-items: center;
  position: sticky;
  top: 0;
  z-index: 100;
}
.dashboard-brand {
  color: #3D68EC;
  font-weight: 600;
  font-size: 1.05rem;
  display: flex;
  flex-direction: column;
  line-height: 1.2;
}

/* Pay-off */
.npuls-payoff {
  display: block;
  color: #DD784B;
  font-size: 0.6rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.12em;
  margin-top: -0.05rem;
}

/* Sidebar */
.sidebar-title-box {
  color: #3D68EC;
  font-weight: 600;
  font-size: 0.75rem;
  text-transform: uppercase;
  letter-spacing: 0.1em;
  margin-bottom: 0.75rem;
}
.sidebar .form-label,
.sidebar label,
.sidebar .shiny-input-container > label {
  color: #3D68EC !important;
  font-size: 0.75rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.06em;
}
.sidebar .selectize-input,
.sidebar .form-select,
.sidebar .form-control {
  background-color: #FFFFFF !important;
  border: 1px solid #3D68EC !important;
  color: #000000 !important;
  border-radius: 4px;
}
.sidebar .selectize-dropdown { background-color: #FFFFFF; color: #000000; }
.sidebar .selectize-dropdown .option:hover { background-color: #D6E2FD; }
.sidebar .irs-bar { background: #3D68EC; border-color: #3D68EC; }
.sidebar .irs-single { background: #3D68EC; }
.sidebar .irs-handle { background: #DD784B; border-color: #DD784B; }
.sidebar .irs-line { background: #E8C4C8; }
.sidebar .irs-grid-text { color: #374151; }
.sidebar .irs-min, .sidebar .irs-max { color: #374151; background: transparent; }
.sidebar .form-check-label { color: #000000 !important; font-size: 0.85rem; font-weight: 400; text-transform: none; letter-spacing: 0; }
.sidebar .form-check-input:checked { background-color: #3D68EC; border-color: #3D68EC; }
.sidebar hr { border-color: #E0B4BA !important; }
.sidebar .n-students { color: #374151; font-size: 0.85rem; }

/* Card headers */
.card-header {
  background-color: #D6E2FD !important;
  color: #000000 !important;
  font-weight: 600 !important;
  font-size: 0.78rem !important;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  border-bottom: 2px solid #3D68EC !important;
}

/* Nav tabs */
.nav-tabs .nav-link.active {
  background-color: #3D68EC !important;
  color: #F4D74B !important;
  border-color: #3D68EC !important;
  font-weight: 600;
}
.nav-tabs .nav-link:hover:not(.active) {
  color: #3D68EC;
  border-bottom-color: #3D68EC;
}
.nav-tabs .nav-link { font-weight: 500; font-size: 0.9rem; }

/* Value boxes */
.value-box .value-box-title {
  font-size: 0.72rem;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  opacity: 0.9;
  font-weight: 600;
}
.value-box .value-box-value { font-size: 1.9rem; font-weight: 600; }
"

## UI ----

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = "#000000",
    primary = NPULS_BLAUW,
    success = NPULS_GROEN,
    warning = NPULS_GEEL,
    danger = NPULS_ORANJE,
    base_font = font_google("Plus Jakarta Sans"),
    heading_font = font_google("Plus Jakarta Sans")
  ),
  tags$head(tags$style(npuls_css)),
  style = "padding: 0; margin: 0;",
  uiOutput("scherm")
)

## Server ----

server <- function(input, output, session) {
  fase <- reactiveVal("upload")
  df_data <- reactiveVal(NULL)

  ## Scherm switch ----

  output$scherm <- renderUI({
    if (fase() == "upload") {
      tags$div(
        class = "upload-achtergrond",
        card(
          class = "upload-card",
          card_header(
            tags$div(
              style = "display:flex;align-items:center;gap:0.85rem;",
              HTML(
                '<svg width="32" height="32" viewBox="0 0 32 32">
                  <circle cx="16" cy="16" r="14" fill="none" stroke="#F4D74B" stroke-width="1.5"/>
                  <circle cx="16" cy="16" r="9"  fill="none" stroke="#FFFFFF" stroke-width="1.5"/>
                  <circle cx="16" cy="16" r="4"  fill="#F4D74B"/>
                </svg>'
              ),
              tags$div(
                tags$div(
                  "Staat van Onderwijsinstelling",
                  style = "font-size:1.1rem;font-weight:600;color:#F4D74B;"
                ),
                tags$span("Onderwijs bewegen.", class = "npuls-payoff")
              )
            )
          ),
          card_body(
            fileInput(
              "bestand_upload",
              "1CHO CSV-bestand",
              accept = ".csv",
              buttonLabel = "Bladeren...",
              placeholder = "Geen bestand geselecteerd"
            ),
            uiOutput("btn_verwerk_ui")
          )
        )
      )
    } else {
      req(df_data())
      d <- df_data()
      jaren_d <- sort(unique(d$inschrijvingsjaar))
      locaties_d <- sort(unique(as.character(d$locatie)))
      sectoren_d <- sort(na.omit(unique(as.character(d$sector))))
      niveaus_d <- sort(na.omit(unique(as.character(d$opleidingsniveau))))
      vormen_d <- sort(na.omit(unique(as.character(d$opleidingsvorm))))
      opleidingen_d <- sort(na.omit(unique(as.character(d$opleidingsnaam))))

      tagList(
        tags$div(
          class = "dashboard-header",
          tags$div(
            class = "dashboard-brand",
            "Staat van Onderwijsinstelling",
            tags$span("Onderwijs bewegen.", class = "npuls-payoff")
          ),
          tags$div(
            style = "margin-left:auto;",
            actionButton(
              "btn_opnieuw",
              "Nieuw bestand",
              class = "btn btn-sm",
              style = "background:#FFFFFF;border:1.5px solid #3D68EC;color:#3D68EC;font-size:0.75rem;font-weight:600;"
            )
          )
        ),
        layout_sidebar(
          style = "min-height: calc(100vh - 52px);",
          sidebar = sidebar(
            width = 260,
            bg = NPULS_ROZE,
            fg = NPULS_ZWART,
            tags$div(
              style = "margin-bottom:0.5rem;",
              HTML(
                '<svg width="32" height="32" viewBox="0 0 32 32" style="display:block;margin-bottom:0.5rem">
                  <circle cx="16" cy="16" r="14" fill="none" stroke="#3D68EC" stroke-width="1.5"/>
                  <circle cx="16" cy="16" r="9"  fill="none" stroke="#DD784B" stroke-width="1.5"/>
                  <circle cx="16" cy="16" r="4"  fill="#DD784B"/>
                </svg>'
              ),
              tags$div(class = "sidebar-title-box", "Filters")
            ),
            sliderInput(
              "jaar_filter",
              "Instroomjaar",
              min = min(jaren_d),
              max = max(jaren_d),
              value = c(min(jaren_d), max(jaren_d)),
              sep = "",
              step = 1
            ),
            selectInput(
              "locatie_filter",
              "Locatie",
              choices = c("Alle locaties" = "", locaties_d),
              selected = ""
            ),
            selectInput(
              "sector_filter",
              "Sector",
              choices = c("Alle sectoren" = "", sectoren_d),
              selected = ""
            ),
            selectInput(
              "opleiding_filter",
              "Opleiding",
              choices = c("Alle opleidingen" = "", opleidingen_d),
              selected = ""
            ),
            selectInput(
              "niveau_filter",
              "Opleidingsniveau",
              choices = c("Alle niveaus" = "", niveaus_d),
              selected = ""
            ),
            radioButtons(
              "vorm_filter",
              "Opleidingsvorm",
              choices = c(
                "Alle" = "",
                setNames(vormen_d, tools::toTitleCase(vormen_d))
              ),
              selected = "",
              inline = TRUE
            ),
            radioButtons(
              "geslacht_filter",
              "Geslacht",
              choices = c("Alle" = "", "Man" = "man", "Vrouw" = "vrouw"),
              selected = "",
              inline = TRUE
            ),
            tags$hr(),
            uiOutput("n_label"),
            downloadButton(
              "download_csv",
              "Download als CSV",
              style = "width:100%;margin-top:0.5rem;background:#3D68EC;color:#F4D74B;border:none;font-size:0.78rem;font-weight:600;"
            )
          ),
          navset_card_tab(
            ## Overzicht ----
            nav_panel(
              "Overzicht",
              layout_columns(
                col_widths = c(3, 3, 3, 3),
                uiOutput("kpi_n"),
                uiOutput("kpi_diploma"),
                uiOutput("kpi_uitval_ov"),
                uiOutput("kpi_wissel_ov")
              ),
              layout_columns(
                col_widths = c(4, 4, 4),
                card(
                  card_header("Status"),
                  plotlyOutput("plot_status", height = "260px")
                ),
                card(
                  card_header("Geslacht"),
                  plotlyOutput("plot_ov_geslacht", height = "260px")
                ),
                card(
                  card_header("Opleidingsvorm"),
                  plotlyOutput("plot_ov_vorm", height = "260px")
                )
              ),
              card(
                card_header("Instroom per cohortjaar"),
                plotlyOutput("plot_overzicht_instroom", height = "340px")
              )
            ),

            ## Instroom ----
            nav_panel(
              "Instroom",
              card(
                card_header("Instroom per cohortjaar"),
                plotlyOutput("plot_instroom_trend", height = "360px")
              ),
              layout_columns(
                col_widths = c(4, 4, 4),
                card(
                  card_header("Sector"),
                  plotlyOutput("plot_instroom_sector", height = "280px")
                ),
                card(
                  card_header("Internationale studenten"),
                  plotlyOutput("plot_instroom_int", height = "280px")
                ),
                card(
                  card_header("Geslacht"),
                  plotlyOutput("plot_instroom_geslacht", height = "280px")
                )
              ),
              card(
                card_header("Leeftijdsverdeling bij instroom"),
                plotlyOutput("plot_instroom_leeftijd", height = "260px")
              )
            ),

            ## Rendement ----
            nav_panel(
              "Rendement",
              layout_columns(
                col_widths = c(4, 4, 4),
                uiOutput("kpi_rend3"),
                uiOutput("kpi_rend5"),
                uiOutput("kpi_rend8")
              ),
              card(
                card_header("Rendement per cohortjaar"),
                plotlyOutput("plot_rendement_trend", height = "420px")
              ),
              layout_columns(
                col_widths = c(4, 4, 4),
                card(
                  card_header("Rendement 3 jaar"),
                  plotlyOutput("plot_rend3", height = "240px")
                ),
                card(
                  card_header("Rendement 5 jaar"),
                  plotlyOutput("plot_rend5", height = "240px")
                ),
                card(
                  card_header("Rendement 8 jaar"),
                  plotlyOutput("plot_rend8", height = "240px")
                )
              )
            ),

            ## Uitval ----
            nav_panel(
              "Uitval",
              layout_columns(
                col_widths = c(4, 4, 4),
                uiOutput("kpi_uitval_kpi"),
                uiOutput("kpi_uitval1"),
                uiOutput("kpi_uitval3")
              ),
              card(
                card_header("Uitval per cohortjaar"),
                plotlyOutput("plot_uitval_trend", height = "420px")
              ),
              layout_columns(
                col_widths = c(6, 6),
                card(
                  card_header("Uitval samengevat"),
                  plotlyOutput("plot_uitval_detail", height = "260px")
                ),
                card(
                  card_header("Uitval per sector"),
                  plotlyOutput("plot_uitval_sector", height = "260px")
                )
              )
            ),

            ## Studiewissel ----
            nav_panel(
              "Studiewissel",
              layout_columns(
                col_widths = c(6, 6),
                uiOutput("kpi_wissel1"),
                uiOutput("kpi_wissel3")
              ),
              card(
                card_header("Studiewissel per cohortjaar"),
                plotlyOutput("plot_wissel_trend", height = "420px")
              ),
              layout_columns(
                col_widths = c(4, 4, 4),
                card(
                  card_header("Studiewissel samengevat"),
                  plotlyOutput("plot_wissel_samen", height = "260px")
                ),
                card(
                  card_header("Sector na wissel (1 jaar)"),
                  plotlyOutput("plot_wissel_sector1", height = "260px")
                ),
                card(
                  card_header("Sector na wissel (3 jaar)"),
                  plotlyOutput("plot_wissel_sector3", height = "260px")
                )
              )
            ),

            ## Data ----
            nav_panel(
              "Data",
              card(
                card_header("Datadictionary"),
                DTOutput("tabel_dict")
              ),
              card(
                card_header("Voorbeeld dataset (eerste 20 rijen)"),
                DTOutput("tabel_voorbeeld")
              )
            )
          )
        )
      )
    }
  })

  ## Verwerken ----

  toon_fout <- function(bericht) {
    showNotification(bericht, type = "error", duration = NULL)
  }

  output$btn_verwerk_ui <- renderUI({
    btn <- actionButton(
      "btn_verwerk",
      "Data verwerken",
      class = "upload-verwerk-btn"
    )
    if (is.null(input$bestand_upload)) {
      tagAppendAttributes(
        btn,
        disabled = "disabled",
        style = "opacity: 0.5; cursor: not-allowed;"
      )
    } else {
      btn
    }
  })
  outputOptions(output, "btn_verwerk_ui", suspendWhenHidden = FALSE)

  observeEvent(input$btn_verwerk, {
    if (is.null(input$bestand_upload)) {
      toon_fout("Selecteer eerst een CSV-bestand.")
      return()
    }

    kolomnamen <- names(readr::read_csv2(
      input$bestand_upload$datapath,
      n_max = 0,
      show_col_types = FALSE
    ))

    ontbrekend <- setdiff(VEREISTE_KOLOMMEN, kolomnamen)
    if (length(ontbrekend) > 0) {
      toon_fout(paste0(
        "Ontbrekende kolom(men): ",
        paste(ontbrekend, collapse = ", ")
      ))
      return()
    }

    withProgress(message = "Data wordt verwerkt...", value = 0, {
      tryCatch(
        {
          setProgress(0.10, detail = "Basisbestand inlezen")
          basisbestand <- maak_basisbestand(input$bestand_upload$datapath)
          jaar <- max(basisbestand$inschrijvingsjaar, na.rm = TRUE) + 1L

          soort_ho <- unique(basisbestand$soort_hoger_onderwijs)

          setProgress(0.25, detail = "Instroomcohort aanmaken")
          cohorten <- maak_instroom_cohort(basisbestand, soort_ho)

          setProgress(0.40, detail = "Diploma bepalen")
          diploma <- maak_diploma_behaald(basisbestand)

          setProgress(0.55, detail = "Rendement berekenen")
          rendement <- bereken_rendement(cohorten, diploma)

          setProgress(0.70, detail = "Uitval berekenen")
          uitval <- bereken_uitval(basisbestand, diploma, cohorten, jaar)

          setProgress(0.85, detail = "Studiewissel berekenen")
          wissel <- bereken_studiewissel(
            basisbestand,
            cohorten,
            diploma,
            uitval
          )

          setProgress(0.95, detail = "Combineren")
          result <- combineer_indicatoren(cohorten, rendement, uitval, wissel)

          naam_tabel <- dplyr::distinct(
            basisbestand,
            opleidingscode = as.character(opleiding_actueel_equivalent),
            opleidingsnaam = opleidingscode_naam_opleiding
          )
          result <- dplyr::left_join(
            result,
            naam_tabel,
            by = c("opleidingscode" = "opleidingscode")
          )

          df_data(result)
          fase("dashboard")
        },
        error = function(e) {
          toon_fout(conditionMessage(e))
        }
      )
    })
  })

  ## Terug naar upload ----

  observeEvent(input$btn_opnieuw, {
    df_data(NULL)
    fase("upload")
  })

  ## Gefilterde data ----

  df <- reactive({
    req(fase() == "dashboard", df_data(), !is.null(input$jaar_filter))
    d <- df_data()
    d <- filter(
      d,
      inschrijvingsjaar >= input$jaar_filter[1],
      inschrijvingsjaar <= input$jaar_filter[2]
    )
    if (!is.null(input$locatie_filter) && nchar(input$locatie_filter) > 0) {
      d <- filter(d, as.character(locatie) == input$locatie_filter)
    }
    if (!is.null(input$sector_filter) && nchar(input$sector_filter) > 0) {
      d <- filter(d, as.character(sector) == input$sector_filter)
    }
    if (!is.null(input$opleiding_filter) && nchar(input$opleiding_filter) > 0) {
      d <- filter(d, as.character(opleidingsnaam) == input$opleiding_filter)
    }
    if (!is.null(input$niveau_filter) && nchar(input$niveau_filter) > 0) {
      d <- filter(d, as.character(opleidingsniveau) == input$niveau_filter)
    }
    if (!is.null(input$vorm_filter) && nchar(input$vorm_filter) > 0) {
      d <- filter(d, as.character(opleidingsvorm) == input$vorm_filter)
    }
    if (!is.null(input$geslacht_filter) && nchar(input$geslacht_filter) > 0) {
      d <- filter(d, as.character(geslacht) == input$geslacht_filter)
    }
    d
  })

  ## Sidebar teller ----

  output$n_label <- renderUI({
    tags$p(tags$strong(n_label(nrow(df()))), " studenten", class = "n-students")
  })

  ## KPI's overzicht ----

  output$kpi_n <- renderUI(vb(
    "Studenten",
    n_label(nrow(df())),
    bg = NPULS_BLAUW,
    fg = NPULS_GEEL
  ))
  output$kpi_diploma <- renderUI(vb(
    "Diploma behaald",
    pct_label(df(), \(d) d$status == "Diploma behaald"),
    bg = NPULS_GROEN,
    fg = NPULS_ZWART
  ))
  output$kpi_uitval_ov <- renderUI(vb(
    "Uitgevallen",
    pct_label(df(), \(d) d$status == "Uitgevallen"),
    bg = NPULS_ORANJE,
    fg = NPULS_ZWART
  ))
  output$kpi_wissel_ov <- renderUI(vb(
    "Gewisseld binnen 1 jaar",
    pct_label(df(), \(d) d$studiewissel_1jr == "Gewisseld binnen 1 jaar"),
    bg = NPULS_GEEL,
    fg = NPULS_ZWART
  ))

  ## KPI's rendement ----

  output$kpi_rend3 <- renderUI(vb(
    "Diploma binnen 3 jaar",
    pct_label(df(), \(d) d$rendement_3jr == "Diploma binnen 3 jaar"),
    bg = NPULS_ORANJE,
    fg = NPULS_ZWART
  ))
  output$kpi_rend5 <- renderUI(vb(
    "Diploma binnen 5 jaar",
    pct_label(df(), \(d) d$rendement_5jr == "Diploma binnen 5 jaar"),
    bg = NPULS_BLAUW,
    fg = NPULS_GEEL
  ))
  output$kpi_rend8 <- renderUI(vb(
    "Diploma binnen 8 jaar",
    pct_label(df(), \(d) d$rendement_8jr == "Diploma binnen 8 jaar"),
    bg = NPULS_GROEN,
    fg = NPULS_ZWART
  ))

  ## KPI's uitval ----

  output$kpi_uitval_kpi <- renderUI(vb(
    "% uitgevallen",
    pct_label(df(), \(d) d$status == "Uitgevallen"),
    bg = NPULS_ORANJE,
    fg = NPULS_ZWART
  ))
  output$kpi_uitval1 <- renderUI(vb(
    "Uitval binnen 1 jaar",
    pct_label(df(), \(d) d$uitval_1jr == "Uitgevallen binnen 1 jaar"),
    bg = NPULS_ORANJE,
    fg = NPULS_ZWART
  ))
  output$kpi_uitval3 <- renderUI(vb(
    "Uitval binnen 3 jaar",
    pct_label(df(), \(d) d$uitval_3jr == "Uitgevallen binnen 3 jaar"),
    bg = NPULS_BLAUW,
    fg = NPULS_GEEL
  ))

  ## KPI's studiewissel ----

  output$kpi_wissel1 <- renderUI(vb(
    "Gewisseld binnen 1 jaar",
    pct_label(df(), \(d) d$studiewissel_1jr == "Gewisseld binnen 1 jaar"),
    bg = NPULS_GEEL,
    fg = NPULS_ZWART
  ))
  output$kpi_wissel3 <- renderUI(vb(
    "Gewisseld binnen 3 jaar",
    pct_label(df(), \(d) d$studiewissel_3jr == "Gewisseld binnen 3 jaar"),
    bg = NPULS_GROEN,
    fg = NPULS_ZWART
  ))

  ## Plots overzicht ----

  output$plot_status <- renderPlotly(ggplotly(pct_bar(df(), status)))
  output$plot_ov_geslacht <- renderPlotly(ggplotly(pct_bar(df(), geslacht)))
  output$plot_ov_vorm <- renderPlotly(ggplotly(pct_bar(df(), opleidingsvorm)))
  output$plot_overzicht_instroom <- renderPlotly(
    ggplotly(instroom_trend(df()), tooltip = c("x", "y")) |>
      layout(
        legend = list(
          x = 0.01,
          y = 0.99,
          xanchor = "left",
          yanchor = "top",
          bgcolor = "rgba(255,255,255,0.85)",
          bordercolor = "#D6E2FD",
          borderwidth = 1,
          font = list(size = 11)
        )
      )
  )

  ## Plots instroom ----

  output$plot_instroom_trend <- renderPlotly(
    ggplotly(instroom_trend(df()), tooltip = c("x", "y")) |>
      layout(
        legend = list(
          x = 0.01,
          y = 0.99,
          xanchor = "left",
          yanchor = "top",
          bgcolor = "rgba(255,255,255,0.85)",
          bordercolor = "#D6E2FD",
          borderwidth = 1,
          font = list(size = 11)
        )
      )
  )
  output$plot_instroom_sector <- renderPlotly(ggplotly(pct_bar(
    df(),
    sector
  )))
  output$plot_instroom_int <- renderPlotly(ggplotly(pct_bar(df(), int_student)))
  output$plot_instroom_geslacht <- renderPlotly(ggplotly(pct_bar(
    df(),
    geslacht
  )))
  output$plot_instroom_leeftijd <- renderPlotly({
    d <- df()
    if (nrow(d) == 0) {
      return(ggplotly(leeg_plot()))
    }
    leeftijd <- as.integer(d$leeftijd_bij_instroom)
    leeftijd <- leeftijd[!is.na(leeftijd) & leeftijd > 0 & leeftijd < 80]
    if (length(leeftijd) == 0) {
      return(ggplotly(leeg_plot()))
    }
    p <- ggplot(data.frame(leeftijd = leeftijd), aes(x = leeftijd)) +
      geom_histogram(
        fill = NPULS_BLAUW,
        alpha = 0.85,
        binwidth = 1,
        boundary = 0
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
      labs(x = "Leeftijd bij instroom", y = "Studenten") +
      thema() +
      theme(
        panel.grid.major.y = element_line(color = "#F3F4F6"),
        panel.grid.major.x = element_blank()
      )
    ggplotly(p) |> layout(showlegend = FALSE)
  })

  ## Plots rendement ----

  output$plot_rendement_trend <- renderPlotly(
    ggplotly(rendement_trend(df()), tooltip = c("x", "y", "colour")) |>
      layout(
        legend = list(
          x = 0.01,
          y = 0.99,
          xanchor = "left",
          yanchor = "top",
          bgcolor = "rgba(255,255,255,0.85)",
          bordercolor = "#D6E2FD",
          borderwidth = 1,
          font = list(size = 11)
        )
      )
  )
  output$plot_rend3 <- renderPlotly(ggplotly(pct_bar(
    df(),
    rendement_3jr,
    "Rendement 3 jaar"
  )))
  output$plot_rend5 <- renderPlotly(ggplotly(pct_bar(
    df(),
    rendement_5jr,
    "Rendement 5 jaar"
  )))
  output$plot_rend8 <- renderPlotly(ggplotly(pct_bar(
    df(),
    rendement_8jr,
    "Rendement 8 jaar"
  )))

  ## Plots uitval ----

  output$plot_uitval_trend <- renderPlotly(
    ggplotly(uitval_trend(df()), tooltip = c("x", "y", "colour")) |>
      layout(
        legend = list(
          x = 0.01,
          y = 0.99,
          xanchor = "left",
          yanchor = "top",
          bgcolor = "rgba(255,255,255,0.85)",
          bordercolor = "#D6E2FD",
          borderwidth = 1,
          font = list(size = 11)
        )
      )
  )
  output$plot_uitval_detail <- renderPlotly(ggplotly(pct_bar(df(), uitval)))
  output$plot_uitval_sector <- renderPlotly({
    d <- df() |> filter(!is.na(sector))
    if (nrow(d) == 0) {
      return(ggplotly(leeg_plot()))
    }
    agg <- d |>
      group_by(sector = as.character(sector)) |>
      summarise(
        pct = mean(status == "Uitgevallen", na.rm = TRUE),
        .groups = "drop"
      )
    kleuren <- kleur_voor(agg$sector)
    p <- ggplot(
      agg,
      aes(x = fct_reorder(sector, pct), y = pct, fill = sector)
    ) +
      geom_col(show.legend = FALSE) +
      geom_text(
        aes(label = paste0(round(pct * 100), "%")),
        hjust = -0.1,
        size = 3.3
      ) +
      scale_y_continuous(labels = percent, limits = c(0, 1.1)) +
      scale_fill_manual(values = kleuren, breaks = names(kleuren)) +
      coord_flip() +
      labs(x = NULL, y = NULL, title = "% uitgevallen per sector") +
      thema()
    ggplotly(p) |> layout(showlegend = FALSE)
  })

  ## Plots studiewissel ----

  output$plot_wissel_trend <- renderPlotly(
    ggplotly(wissel_trend(df()), tooltip = c("x", "y", "colour")) |>
      layout(
        legend = list(
          x = 0.01,
          y = 0.99,
          xanchor = "left",
          yanchor = "top",
          bgcolor = "rgba(255,255,255,0.85)",
          bordercolor = "#D6E2FD",
          borderwidth = 1,
          font = list(size = 11)
        )
      )
  )
  output$plot_wissel_samen <- renderPlotly(ggplotly(pct_bar(
    df(),
    studiewissel
  )))
  output$plot_wissel_sector1 <- renderPlotly(ggplotly(pct_bar(
    df(),
    sector_na_switch1jr
  )))
  output$plot_wissel_sector3 <- renderPlotly(ggplotly(pct_bar(
    df(),
    sector_na_switch3jr
  )))

  ## Data tab ----

  output$tabel_dict <- renderDT({
    datatable(
      DICT,
      rownames = FALSE,
      options = list(pageLength = 33, dom = "t", ordering = FALSE),
      class = "compact stripe"
    ) |>
      formatStyle(
        "Categorie",
        backgroundColor = styleEqual(
          c(
            "Studentkenmerken",
            "Status",
            "Rendement",
            "Uitval",
            "Studiewissel"
          ),
          c("#CCEEE6", "#D6E2FD", "#FFE4D8", "#FFE4D8", "#FCF5D4")
        ),
        fontWeight = "600"
      )
  })

  output$tabel_voorbeeld <- renderDT({
    req(df_data())
    datatable(
      head(df_data(), 20),
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 10, dom = "tp"),
      class = "compact"
    )
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("Indicatoren_1cHO_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(df_data(), file)
    }
  )
}

shinyApp(ui, server)
