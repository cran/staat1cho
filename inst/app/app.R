library(shiny)
library(bslib)

## Een verrijkt EV-bestand van een grote instelling is enkele gigabytes. Het
## werkgeheugen van de computer is daarbij de echte grens, niet Shiny.
options(shiny.maxRequestSize = 10 * 1024^3) ## 10 GB
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
  "Geen switch bepaald" = "#F3F4F6",
  ## Onvolledige cohorten
  "Nog niet waarneembaar" = "#F9FAFB"
)

## Datadictionary ----

DICT <- data.frame(
  Categorie = c(
    rep("Studentkenmerken", 14),
    rep("Status", 4),
    rep("Rendement", 4),
    rep("Uitval", 4),
    rep("Studiewissel", 11),
    rep("Vooropleiding (VAKHAVW)", 3),
    rep("Bekostiging (VLPBEK)", 4)
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
    "vooropleiding",
    "eerstejaars_ho",
    "postcode4_student_1okt",
    "postcode4_vooropleiding_voorHO",
    "status",
    "soortdiploma",
    "opleidingscode_diploma",
    "diploma_in_instroomopleiding",
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
    "studiewissel",
    "vakhawv_gemiddeld_eindcijfer",
    "vakhawv_wiskundecijfer",
    "vakhawv_aantal_vakken",
    "indicatie_bekostigd",
    "indicatie_hoofdinschrijving",
    "reden_niet_bekostigd",
    "indicatie_herstelbaar"
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
    "Hoogste vooropleiding voor het HO (havo, vwo, mbo, ho, buitenlands, overig, onbekend)",
    "Of het instroomjaar ook het eerste jaar in het hoger onderwijs is",
    "Postcode (4-cijferig) van de student op 1 oktober",
    "Postcode (4-cijferig) van vooropleiding voor HO",
    "Status na afloop van de observatieperiode",
    "Soort behaald diploma",
    "CROHO-opleidingscode waarin het (eerste) diploma is behaald",
    "Of het diploma in de instroomopleiding is behaald (FALSE = na een wissel elders in de instelling)",
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
    "Samengevatte studiewisseluitkomst",
    "Gemiddeld eindcijfer vooropleiding (uit VAKHAVW, schaal 1-10)",
    "Gemiddeld centraal examencijfer wiskunde (uit VAKHAVW, schaal 1-10)",
    "Aantal unieke vakken op de eindlijst (uit VAKHAVW)",
    "Of DUO de inschrijving bekostigt (uit VLPBEK)",
    "DUO-Bekostigingsindicatie: of de inschrijving is aangeleverd als in aanmerking komend voor bekostiging (uit VLPBEK)",
    "Toelichting als niet bekostigd, gedecodeerd uit de DUO-redencode (uit VLPBEK)",
    "Of de reden voor niet-bekostiging hersteld kan worden door tijdig opnieuw aan te leveren (uit VLPBEK)"
  ),
  stringsAsFactors = FALSE
)

## Vereiste 1CHO-kolommen ----

VEREISTE_KOLOMMEN <- c(
  "persoonsgebonden_nummer",
  "inschrijvingsjaar",
  "verblijfsjaar_actuele_instelling",
  "verblijfsjaar_actuele_opleiding_instelling",
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

## Constanten ----

LEGEND_LAYOUT <- list(
  x = 0.01,
  y = 0.99,
  xanchor = "left",
  yanchor = "top",
  bgcolor = "rgba(255,255,255,0.85)",
  bordercolor = "#D6E2FD",
  borderwidth = 1,
  font = list(size = 11)
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
    ## Weinig ticks: de kaarten zijn smal en 25%-stappen overlappen
    scale_y_continuous(labels = percent, limits = c(0, 1.2), breaks = c(0, 0.5, 1)) +
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
  ## Een geom_line() met een lege data-frame (elke groep heeft maar 1 punt,
  ## bijv. een enkel instroomjaar) laat ggplotly() crashen ("Error in order:
  ## argument 1 is not a vector"). Voeg de lijnlaag daarom alleen toe als er
  ## daadwerkelijk lijndata is.
  p <- ggplot(
    data,
    aes(x = inschrijvingsjaar, y = .data[[y_col]], color = .data[[kleur_col]])
  ) +
    geom_point(size = 3) +
    scale_y_continuous(labels = y_label) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
    scale_color_manual(values = kleuren_schaal) +
    labs(x = "Instroomjaar", y = NULL, title = titel) +
    thema()
  if (nrow(lijn_data) > 0) {
    p <- p + geom_line(data = lijn_data, linewidth = 1.3)
  }
  p
}

instroom_trend <- function(
  data,
  titel = "Instroom per cohortjaar",
  y_label = "Studenten"
) {
  if (nrow(data) == 0) {
    return(leeg_plot())
  }
  agg <- data |> count(inschrijvingsjaar)
  if (nrow(agg) == 0) {
    return(leeg_plot())
  }
  ## Een geom_line() met een lege data-frame (maar 1 instroomjaar in de
  ## data) laat ggplotly() crashen ("Error in order: argument 1 is not a
  ## vector"). Voeg de lijnlaag daarom alleen toe als er >= 2 punten zijn.
  p <- ggplot(agg, aes(x = inschrijvingsjaar, y = n)) +
    geom_point(color = NPULS_BLAUW, size = 3) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
    labs(x = "Instroomjaar", y = y_label, title = titel) +
    thema()
  if (nrow(agg) >= 2) {
    p <- p + geom_line(data = agg, color = NPULS_BLAUW, linewidth = 1.3)
  }
  p
}

rendement_trend <- function(data, titel = "Rendement per cohortjaar") {
  if (nrow(data) == 0) {
    return(leeg_plot())
  }
  agg <- data |>
    mutate(
      `3 jaar` = waargenomen(rendement_3jr, "Diploma binnen 3 jaar"),
      `5 jaar` = waargenomen(rendement_5jr, "Diploma binnen 5 jaar"),
      `8 jaar` = waargenomen(rendement_8jr, "Diploma binnen 8 jaar")
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
      `binnen 1 jaar` = waargenomen(uitval_1jr, "Uitgevallen binnen 1 jaar"),
      `binnen 3 jaar` = waargenomen(uitval_3jr, "Uitgevallen binnen 3 jaar")
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
      `binnen 1 jaar` = waargenomen(studiewissel_1jr, "Gewisseld binnen 1 jaar"),
      `binnen 3 jaar` = waargenomen(studiewissel_3jr, "Gewisseld binnen 3 jaar")
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

## Vraag-badge met tooltip naast een label
def_icon <- function(definitie) {
  tooltip(
    span(
      "?",
      style = paste0(
        "display:inline-flex;align-items:center;justify-content:center;",
        "width:1.1em;height:1.1em;margin-left:0.4em;",
        "background:rgba(255,255,255,0.35);border:1.5px solid rgba(255,255,255,0.7);",
        "border-radius:50%;font-size:0.72em;font-weight:700;",
        "cursor:help;vertical-align:middle;line-height:1;"
      )
    ),
    definitie,
    placement = "right"
  )
}

vb <- function(label, waarde, bg = NPULS_BLAUW, fg = NPULS_GEEL, definitie = NULL) {
  title_ui <- if (!is.null(definitie)) {
    tagList(label, def_icon(definitie))
  } else {
    label
  }
  value_box(
    title = title_ui,
    value = waarde,
    theme = value_box_theme(bg = bg, fg = fg)
  )
}

pct_label <- function(data, conditie) {
  if (nrow(data) == 0) {
    return("—")
  }
  waarde <- mean(conditie(data), na.rm = TRUE)
  ## NaN: niemand in de selectie is waarneembaar voor deze indicator
  if (is.nan(waarde)) {
    return("—")
  }
  paste0(round(waarde * 100), "%")
}

## TRUE/FALSE of de indicator gelijk is aan `label`; NA voor studenten in een
## cohort waarvan het meetvenster nog niet in de data zit. Met
## mean(na.rm = TRUE) tellen die rijen zo niet mee in de noemer.
NIET_WAARNEEMBAAR <- "Nog niet waarneembaar"
waargenomen <- function(x, label) {
  x <- as.character(x)
  dplyr::if_else(x == NIET_WAARNEEMBAAR, NA, x == label)
}

## Percentage per groep (bijv. vooropleiding) over de waarneembare rijen.
## `conditie` is een functie zoals bij pct_label(); NA telt niet mee.
pct_per_groep <- function(data, groep, conditie, titel) {
  if (nrow(data) == 0) {
    return(leeg_plot())
  }
  agg <- data |>
    mutate(groep = droplevels(as.factor({{ groep }})), treffer = conditie(data)) |>
    filter(!is.na(treffer), !is.na(groep)) |>
    group_by(groep) |>
    summarise(pct = mean(treffer), n = n(), .groups = "drop")
  if (nrow(agg) == 0) {
    return(leeg_plot())
  }
  ggplot(agg, aes(x = fct_rev(groep), y = pct)) +
    geom_col(fill = NPULS_BLAUW, show.legend = FALSE) +
    geom_text(
      aes(label = paste0(round(pct * 100), "% (n=", n, ")")),
      hjust = -0.05,
      size = 3.3
    ) +
    scale_y_continuous(labels = percent, limits = c(0, 1.2), breaks = c(0, 0.5, 1)) +
    coord_flip() +
    labs(x = NULL, y = NULL, title = titel) +
    thema()
}

n_label <- function(n) {
  formatC(n, format = "d", big.mark = ".")
}

## Op studentniveau filteren opleiding, sector en locatie op het instroomjaar,
## terwijl uitkomsten instellingsbreed zijn. Het label maakt dat zichtbaar.
filter_label <- function(tekst, niveau) {
  if (niveau != "student") {
    return(tekst)
  }
  tagList(
    paste(tekst, "bij instroom"),
    tooltip(
      span("?", class = "filter-help"),
      DEFINITIES[["student"]]$filter_instroom,
      placement = "right"
    )
  )
}

## CSS ----

npuls_css <- "
@import url('https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;600&display=swap');

*, body { font-family: 'Plus Jakarta Sans', sans-serif !important; }

/* Uitleg bij filters op studentniveau */
.filter-help {
  display: inline-flex; align-items: center; justify-content: center;
  width: 1.1em; height: 1.1em; margin-left: 0.35em;
  border: 1.5px solid #3D68EC; color: #3D68EC; border-radius: 50%;
  font-size: 0.7em; font-weight: 700; cursor: help; vertical-align: middle;
}
.filter-uitleg {
  background: #FFFFFF; border-left: 3px solid #3D68EC; border-radius: 4px;
  padding: 0.5rem 0.6rem; font-size: 0.75rem; line-height: 1.35;
  margin: 0.25rem 0 0.5rem 0;
}

/* Uitleg onder de uploadvelden */
.upload-hint {
  font-size: 0.75rem; line-height: 1.4; color: #4B5563;
  margin: -0.25rem 0 0.75rem 0;
}

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
  analyse_niveau <- reactiveVal("student")
  peildatum <- reactiveVal(NULL)
  heeft_vakhawv <- reactiveVal(FALSE)
  vakhawv_raw <- reactiveVal(NULL)
  heeft_bekostiging <- reactiveVal(FALSE)
  bek_peiljaar     <- reactiveVal(NULL)
  bek_jaren        <- reactiveVal(NULL)
  ## Shiny wist input$xxx_upload niet vanzelf als de fileInput opnieuw wordt
  ## getekend (bijv. na "Nieuw bestand"); de oude serverwaarde blijft anders
  ## hangen en wordt stilzwijgend hergebruikt. Door de input-ID's te laten
  ## meelopen met deze teller krijgt elke upload-poging gegarandeerd verse,
  ## nog nooit geziene input-ID's zonder oude waarde.
  upload_generation <- reactiveVal(0L)
  upload_ids <- function() {
    gen <- upload_generation()
    list(
      bestand     = paste0("bestand_upload_", gen),
      vakhawv     = paste0("vakhawv_upload_", gen),
      bekostiging = paste0("bekostiging_upload_", gen)
    )
  }

  ## Scherm switch ----

  output$scherm <- renderUI({
    if (fase() == "upload") {
      ids <- upload_ids()
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
                tags$span("Onderwijs in beweging.", class = "npuls-payoff")
              )
            )
          ),
          card_body(
            fileInput(
              ids$bestand,
              "1CHO CSV-bestand",
              accept = ".csv",
              buttonLabel = "Bladeren...",
              placeholder = "Geen bestand geselecteerd"
            ),
            radioButtons(
              "analyse_niveau",
              "Analyseniveau",
              choices = c(
                "Studentniveau" = "student",
                "Inschrijvingsniveau" = "inschrijving"
              ),
              selected = "student",
              inline = TRUE
            ),
            tags$hr(style = "border-color:#D6E2FD;margin:0.75rem 0;"),
            tags$p(
              "Optionele bestanden",
              style = paste0(
                "font-size:0.72rem;font-weight:600;text-transform:uppercase;",
                "letter-spacing:0.08em;color:#6B7280;margin-bottom:0.5rem;"
              )
            ),
            fileInput(
              ids$vakhawv,
              "VAKHAVW-bestand (vakcijfers vooropleiding)",
              accept = ".csv",
              buttonLabel = "Bladeren...",
              placeholder = "Optioneel"
            ),
            fileInput(
              ids$bekostiging,
              "VLPBEK-bestand (bekostigingsindicatie)",
              accept = ".csv",
              buttonLabel = "Bladeren...",
              placeholder = "Optioneel"
            ),
            ## VLPBEK gebruikt het BSN, het EV-bestand een eigen DUO-nummer
            tags$p(
              class = "upload-hint",
              tags$strong("VLPBEK nog niet gebruiken. "),
              "Het VLPBEK-bestand gebruikt het BSN, het 1CHO-bestand een eigen",
              "persoonsnummer van DUO, dus de koppeling vindt nog niets terug."
            ),
            uiOutput("btn_verwerk_ui")
          )
        )
      )
    } else {
      req(df_data())
      d <- df_data()

      if (nrow(d) == 0 || !"inschrijvingsjaar" %in% names(d)) {
        return(tags$div(
          class = "upload-achtergrond",
          tags$div(
            style = "text-align:center;color:#374151;",
            tags$h4("Geen data gevonden na verwerking."),
            tags$p(
              "Het cohort is leeg. Controleer of het bestand de juiste",
              "kolommen bevat en of het soort hoger onderwijs klopt."
            ),
            actionButton(
              "btn_opnieuw",
              "Nieuw bestand laden",
              class = "btn btn-sm",
              style = "background:#3D68EC;color:#F4D74B;border:none;font-weight:600;"
            )
          )
        ))
      }

      jaren_d <- sort(unique(d$inschrijvingsjaar))
      locaties_d <- sort(unique(as.character(d$locatie)))
      sectoren_d <- sort(na.omit(unique(as.character(d$sector))))
      niveaus_d <- sort(na.omit(unique(as.character(d$opleidingsniveau))))
      vormen_d <- sort(na.omit(unique(as.character(d$opleidingsvorm))))
      opleidingen_d <- sort(na.omit(unique(as.character(d$opleidingsnaam))))
      ## Vaste volgorde (havo, vwo, mbo, ...) in plaats van alfabetisch
      vooropleidingen_d <- as.character(unique(sort(d$vooropleiding)))

      tagList(
        tags$div(
          class = "dashboard-header",
          tags$div(
            class = "dashboard-brand",
            "Staat van Onderwijsinstelling",
            tags$span("Onderwijs in beweging.", class = "npuls-payoff")
          ),
          tags$span(
            if (analyse_niveau() == "inschrijving") {
              "Inschrijvingsniveau"
            } else {
              "Studentniveau"
            },
            style = "margin-left:1rem;background:#D6E2FD;color:#3D68EC;font-size:0.7rem;font-weight:600;padding:0.2rem 0.65rem;border-radius:999px;text-transform:uppercase;letter-spacing:0.06em;"
          ),
          if (!is.null(peildatum())) {
            tags$span(
              paste("Peildatum", format(peildatum(), "%d-%m-%Y")),
              title = paste(
                "Stand van de 1CHO-data. Cijfers van eerdere cohorten kunnen",
                "bij een latere peildatum veranderen."
              ),
              style = "margin-left:0.5rem;background:#FFFFFF;border:1px solid #D6E2FD;color:#374151;font-size:0.7rem;font-weight:600;padding:0.2rem 0.65rem;border-radius:999px;letter-spacing:0.04em;"
            )
          },
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
              filter_label("Locatie", analyse_niveau()),
              choices = c("Alle locaties" = "", locaties_d),
              selected = ""
            ),
            selectInput(
              "sector_filter",
              filter_label("Sector", analyse_niveau()),
              choices = c("Alle sectoren" = "", sectoren_d),
              selected = ""
            ),
            selectInput(
              "opleiding_filter",
              filter_label("Opleiding", analyse_niveau()),
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
            selectInput(
              "vooropleiding_filter",
              "Vooropleiding",
              choices = c("Alle vooropleidingen" = "", vooropleidingen_d),
              selected = ""
            ),
            radioButtons(
              "eerstejaars_ho_filter",
              "Eerder in het HO?",
              choices = c(
                "Alle" = "",
                "Eerstejaars HO" = "eerstejaars HO",
                "Eerder in HO" = "eerder in HO"
              ),
              selected = "",
              inline = TRUE
            ),
            uiOutput("filter_uitleg"),
            tags$hr(),
            uiOutput("n_label"),
            downloadButton(
              "download_csv",
              "Download als CSV",
              style = "width:100%;margin-top:0.5rem;background:#3D68EC;color:#F4D74B;border:none;font-size:0.78rem;font-weight:600;"
            ),
            tags$p(
              class = "upload-hint",
              style = "margin-top:0.3rem;",
              "Bevat persoonsnummers: alleen binnen de instelling gebruiken.",
              "Het benchmarkrapport bevat geen persoonsnummers."
            ),
            downloadButton(
              "download_benchmark",
              "Download benchmarkrapport",
              style = "width:100%;margin-top:0.4rem;background:#FFFFFF;color:#3D68EC;border:1.5px solid #3D68EC;font-size:0.78rem;font-weight:600;"
            )
          ),
          navset_card_tab(
            ## Overzicht ----
            nav_panel(
              "Overzicht",
              layout_columns(
                col_widths = c(3, 3, 3, 3),
                fill = FALSE,
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
              layout_columns(
                col_widths = c(6, 6),
                card(
                  card_header("Vooropleiding"),
                  plotlyOutput("plot_instroom_vooropleiding", height = "280px")
                ),
                card(
                  card_header("Eerstejaars HO of eerder in HO"),
                  plotlyOutput("plot_instroom_eerstejaars_ho", height = "280px")
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
                fill = FALSE,
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
              ),
              layout_columns(
                col_widths = c(6, 6),
                card(
                  card_header("Diploma binnen 5 jaar per vooropleiding"),
                  plotlyOutput("plot_rend5_vooropleiding", height = "280px")
                ),
                card(
                  card_header("Diploma binnen 5 jaar: eerstejaars HO of eerder in HO"),
                  plotlyOutput("plot_rend5_eerstejaars_ho", height = "280px")
                )
              )
            ),

            ## Uitval ----
            nav_panel(
              "Uitval",
              layout_columns(
                col_widths = c(4, 4, 4),
                fill = FALSE,
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
              ),
              layout_columns(
                col_widths = c(6, 6),
                card(
                  card_header("Uitval binnen 1 jaar per vooropleiding"),
                  plotlyOutput("plot_uitval1_vooropleiding", height = "280px")
                ),
                card(
                  card_header("Uitval binnen 1 jaar: eerstejaars HO of eerder in HO"),
                  plotlyOutput("plot_uitval1_eerstejaars_ho", height = "280px")
                )
              )
            ),

            ## Studiewissel (alleen bij studentniveau) ----
            if (analyse_niveau() == "student") {
              nav_panel(
                "Studiewissel",
                layout_columns(
                  col_widths = c(6, 6),
                  fill = FALSE,
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
              )
            } else {
              NULL
            },

            ## Bekostiging (alleen als VLPBEK geladen) ----
            if (heeft_bekostiging()) {
              nav_panel(
                "Bekostiging",
                uiOutput("bek_peiljaar_badge"),
                layout_columns(
                  col_widths = c(3, 3, 3, 3),
                  fill = FALSE,
                  uiOutput("kpi_pct_bekostigd"),
                  uiOutput("kpi_n_bekostigd"),
                  uiOutput("kpi_n_niet_bekostigd"),
                  uiOutput("kpi_pct_hoofdinschrijving")
                ),
                layout_columns(
                  col_widths = c(6, 6),
                  card(
                    card_header("Bekostigingsstatus"),
                    plotlyOutput("plot_bek_status", height = "280px")
                  ),
                  card(
                    card_header("% bekostigd per sector"),
                    plotlyOutput("plot_bek_sector", height = "280px")
                  )
                ),
                layout_columns(
                  col_widths = c(6, 6),
                  card(
                    card_header("% bekostigd per opleidingsvorm"),
                    plotlyOutput("plot_bek_vorm", height = "240px")
                  ),
                  card(
                    card_header("% bekostigd per opleidingsniveau"),
                    plotlyOutput("plot_bek_niveau", height = "240px")
                  )
                ),
                layout_columns(
                  col_widths = c(3, 9),
                  uiOutput("kpi_pct_herstelbaar"),
                  card(
                    card_header("Redenen niet bekostigd (groen = herstelbaar door tijdig opnieuw aan te leveren)"),
                    plotlyOutput("plot_bek_redenen", height = "320px")
                  )
                )
              )
            } else {
              NULL
            },

            ## Vooropleiding (alleen als VAKHAVW geladen) ----
            if (heeft_vakhawv()) {
              nav_panel(
                "Vooropleiding",
                layout_columns(
                  col_widths = c(4, 4, 4),
                  fill = FALSE,
                  uiOutput("kpi_gem_eindcijfer"),
                  uiOutput("kpi_aantal_vakken"),
                  uiOutput("kpi_pct_met_vakhawv")
                ),
                card(
                  card_header("Verdeling gemiddeld eindcijfer"),
                  plotlyOutput("plot_eindcijfer_hist", height = "300px")
                ),
                layout_columns(
                  col_widths = c(6, 6),
                  card(
                    card_header("Gemiddeld eindcijfer per sector (alleen sectoren met VAKHAVW-data)"),
                    plotlyOutput("plot_eindcijfer_sector", height = "280px")
                  ),
                  card(
                    card_header("Gemiddeld eindcijfer per rendement"),
                    plotlyOutput("plot_eindcijfer_rendement", height = "280px")
                  )
                ),
                card(
                  card_header("Vakcijfer per vak"),
                  layout_columns(
                    col_widths = c(4, 4, 4),
                    uiOutput("vak_keuze_ui"),
                    uiOutput("kpi_vak_cijfer"),
                    uiOutput("kpi_pct_vak")
                  ),
                  layout_columns(
                    col_widths = c(6, 6),
                    plotlyOutput("plot_vak_sector", height = "260px"),
                    plotlyOutput("plot_vak_rendement", height = "260px")
                  )
                )
              )
            } else {
              NULL
            },

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
              ),
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

  ## Koppelpercentage van VAKHAVW/VLPBEK tonen. Een lage koppeling wijst meestal
  ## op verschillende persoonsnummers (bijv. DUO-nummer vs. BSN) en is
  ## anders niet te onderscheiden van "niemand bekostigd".
  toon_koppeling <- function(resultaat) {
    k <- attr(resultaat, "koppeling")
    if (is.null(k)) {
      return(invisible())
    }
    laag <- isTRUE(k$laag)
    showNotification(
      paste0(
        k$bron, ": ", k$gekoppeld, " van ", k$n, " ", k$eenheid,
        " teruggevonden in het 1CHO-bestand (", k$pct, "%).",
        if (laag && k$bron == "VAKHAVW") {
          " Gebruik voor het 1CHO- en VAKHAVW-bestand de uitvoer van dezelfde 1cijferho-run."
        } else if (laag) {
          paste(
            " VLPBEK koppelt nog niet: het VLPBEK-bestand gebruikt het BSN,",
            "het 1CHO-bestand een eigen persoonsnummer van DUO."
          )
        } else {
          ""
        }
      ),
      type = if (laag) "warning" else "message",
      duration = if (laag) NULL else 8
    )
  }

  output$btn_verwerk_ui <- renderUI({
    btn <- actionButton(
      "btn_verwerk",
      "Data verwerken",
      class = "upload-verwerk-btn"
    )
    if (is.null(input[[upload_ids()$bestand]])) {
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
    ids <- upload_ids()
    bestand_upload     <- input[[ids$bestand]]
    vakhawv_upload     <- input[[ids$vakhawv]]
    bekostiging_upload <- input[[ids$bekostiging]]

    if (is.null(bestand_upload)) {
      toon_fout("Selecteer eerst een CSV-bestand.")
      return()
    }

    kolomnamen <- names(readr::read_csv2(
      bestand_upload$datapath,
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
          basisbestand <- maak_basisbestand(bestand_upload$datapath)
          jaar <- max(basisbestand$inschrijvingsjaar, na.rm = TRUE) + 1L

          soort_ho <- unique(basisbestand$soort_hoger_onderwijs)

          niv <- input$analyse_niveau
          analyse_niveau(niv)

          setProgress(0.25, detail = "Instroomcohort aanmaken")
          cohorten <- maak_instroom_cohort(basisbestand, soort_ho, niveau = niv)

          setProgress(0.40, detail = "Diploma bepalen")
          diploma <- maak_diploma_behaald(basisbestand, niveau = niv)

          setProgress(0.55, detail = "Rendement berekenen")
          rendement <- bereken_rendement(
            cohorten, diploma, niveau = niv, laatste_jaar = jaar - 1L
          )

          setProgress(0.70, detail = "Uitval berekenen")
          uitval <- bereken_uitval(
            basisbestand,
            diploma,
            cohorten,
            jaar,
            niveau = niv
          )

          wissel <- NULL
          if (niv == "student") {
            setProgress(0.85, detail = "Studiewissel berekenen")
            wissel <- bereken_studiewissel(
              basisbestand,
              cohorten,
              diploma,
              uitval
            )
          }

          setProgress(0.95, detail = "Combineren")
          result <- combineer_indicatoren(
            cohorten,
            rendement,
            uitval,
            wissel,
            niveau = niv
          )
          peildatum(attr(result, "peildatum"))

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

          ## VAKHAVW (optioneel) ----
          heeft_vakhawv(FALSE)
          if (!is.null(vakhawv_upload)) {
            setProgress(0.95, detail = "VAKHAVW koppelen")
            tryCatch(
              {
                vakhawv <- lees_vakhawv(vakhawv_upload$datapath)
                vakhawv_raw(vakhawv)
                result <- suppressWarnings(verrijk_met_vakhawv(result, vakhawv))
                toon_koppeling(result)
                heeft_vakhawv(TRUE)
              },
              error = function(e) {
                showNotification(
                  paste("VAKHAVW overgeslagen:", conditionMessage(e)),
                  type = "warning",
                  duration = 10
                )
              }
            )
          }

          ## VLPBEK (optioneel) ----
          heeft_bekostiging(FALSE)
          if (!is.null(bekostiging_upload)) {
            setProgress(0.98, detail = "Bekostiging koppelen")
            tryCatch(
              {
                bekostiging <- lees_bekostiging(bekostiging_upload$datapath)
                bek_peiljaar(attr(bekostiging, "peiljaar"))
                bek_jaren(sort(unique(bekostiging$inschrijvingsjaar)))
                result <- suppressWarnings(verrijk_met_bekostiging(result, bekostiging))
                toon_koppeling(result)
                heeft_bekostiging(TRUE)
              },
              error = function(e) {
                showNotification(
                  paste("VLPBEK overgeslagen:", conditionMessage(e)),
                  type = "warning",
                  duration = 10
                )
              }
            )
          }

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
    vakhawv_raw(NULL)
    fase("upload")
    heeft_vakhawv(FALSE)
    heeft_bekostiging(FALSE)
    bek_peiljaar(NULL)
    bek_jaren(NULL)
    ## Nieuwe generatie -> fileInput()s krijgen verse ID's, zodat oude
    ## input$..._upload-waarden niet stilzwijgend worden hergebruikt.
    upload_generation(upload_generation() + 1L)
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
    if (!is.null(input$vooropleiding_filter) && nchar(input$vooropleiding_filter) > 0) {
      d <- filter(d, as.character(vooropleiding) == input$vooropleiding_filter)
    }
    if (!is.null(input$eerstejaars_ho_filter) && nchar(input$eerstejaars_ho_filter) > 0) {
      d <- filter(d, as.character(eerstejaars_ho) == input$eerstejaars_ho_filter)
    }
    d
  })

  ## Uitleg filters op studentniveau (#45) ----

  output$filter_uitleg <- renderUI({
    req(analyse_niveau() == "student")
    actief <- vapply(
      c("opleiding_filter", "sector_filter", "locatie_filter"),
      function(id) !is.null(input[[id]]) && nchar(input[[id]]) > 0,
      logical(1)
    )
    if (!any(actief)) {
      return(NULL)
    }
    d <- df()
    n_diploma <- sum(d$status == "Diploma behaald", na.rm = TRUE)
    n_elders <- sum(
      d$status == "Diploma behaald" & !d$diploma_in_instroomopleiding,
      na.rm = TRUE
    )
    tags$div(
      class = "filter-uitleg",
      tags$strong("Studentniveau: "),
      "je ziet studenten die hier zijn ingestroomd. Uitkomsten gelden voor de",
      "hele instelling, ook na een wissel.",
      if (n_diploma > 0) {
        tags$span(
          tags$br(),
          sprintf(
            "%s van de %s diploma's in deze selectie is in een andere opleiding behaald.",
            n_label(n_elders),
            n_label(n_diploma)
          )
        )
      },
      tags$br(),
      tags$em("Kies inschrijvingsniveau om per opleiding te meten.")
    )
  })

  ## Sidebar teller ----

  output$n_label <- renderUI({
    eenheid <- if (analyse_niveau() == "inschrijving") {
      "inschrijvingen"
    } else {
      "studenten"
    }
    tags$p(
      tags$strong(n_label(nrow(df()))),
      paste0(" ", eenheid),
      class = "n-students"
    )
  })

  ## KPI's overzicht ----

  output$kpi_n <- renderUI({
    label <- if (analyse_niveau() == "inschrijving") {
      "Inschrijvingen"
    } else {
      "Studenten"
    }
    vb(
      label,
      n_label(nrow(df())),
      bg = NPULS_BLAUW,
      fg = NPULS_GEEL,
      definitie = DEFINITIES[[analyse_niveau()]]$instroom
    )
  })
  output$kpi_diploma <- renderUI(vb(
    "Diploma behaald",
    pct_label(df(), \(d) d$status == "Diploma behaald"),
    bg = NPULS_GROEN,
    fg = NPULS_ZWART,
    definitie = DEFINITIES[[analyse_niveau()]]$status
  ))
  output$kpi_uitval_ov <- renderUI(vb(
    "Uitgevallen",
    pct_label(df(), \(d) d$status == "Uitgevallen"),
    bg = NPULS_ORANJE,
    fg = NPULS_ZWART,
    definitie = DEFINITIES[[analyse_niveau()]]$status
  ))
  output$kpi_wissel_ov <- renderUI({
    if (analyse_niveau() == "inschrijving") {
      vb(
        "Gewisseld binnen 1 jaar",
        "—",
        bg = NPULS_GEEL,
        fg = NPULS_ZWART,
        definitie = DEFINITIES[[analyse_niveau()]]$studiewissel_1jr
      )
    } else {
      vb(
        "Gewisseld binnen 1 jaar",
        pct_label(df(), \(d) waargenomen(d$studiewissel_1jr, "Gewisseld binnen 1 jaar")),
        bg = NPULS_GEEL,
        fg = NPULS_ZWART,
        definitie = DEFINITIES[[analyse_niveau()]]$studiewissel_1jr
      )
    }
  })

  ## KPI's rendement ----

  output$kpi_rend3 <- renderUI(vb(
    "Diploma binnen 3 jaar",
    pct_label(df(), \(d) waargenomen(d$rendement_3jr, "Diploma binnen 3 jaar")),
    bg = NPULS_ORANJE,
    fg = NPULS_ZWART,
    definitie = DEFINITIES[[analyse_niveau()]]$rendement_3jr
  ))
  output$kpi_rend5 <- renderUI(vb(
    "Diploma binnen 5 jaar",
    pct_label(df(), \(d) waargenomen(d$rendement_5jr, "Diploma binnen 5 jaar")),
    bg = NPULS_BLAUW,
    fg = NPULS_GEEL,
    definitie = DEFINITIES[[analyse_niveau()]]$rendement_5jr
  ))
  output$kpi_rend8 <- renderUI(vb(
    "Diploma binnen 8 jaar",
    pct_label(df(), \(d) waargenomen(d$rendement_8jr, "Diploma binnen 8 jaar")),
    bg = NPULS_GROEN,
    fg = NPULS_ZWART,
    definitie = DEFINITIES[[analyse_niveau()]]$rendement_8jr
  ))

  ## KPI's uitval ----

  output$kpi_uitval_kpi <- renderUI(vb(
    "% uitgevallen",
    pct_label(df(), \(d) d$status == "Uitgevallen"),
    bg = NPULS_ORANJE,
    fg = NPULS_ZWART,
    definitie = DEFINITIES[[analyse_niveau()]]$status
  ))
  output$kpi_uitval1 <- renderUI(vb(
    "Uitval binnen 1 jaar",
    pct_label(df(), \(d) waargenomen(d$uitval_1jr, "Uitgevallen binnen 1 jaar")),
    bg = NPULS_ORANJE,
    fg = NPULS_ZWART,
    definitie = DEFINITIES[[analyse_niveau()]]$uitval_1jr
  ))
  output$kpi_uitval3 <- renderUI(vb(
    "Uitval binnen 3 jaar",
    pct_label(df(), \(d) waargenomen(d$uitval_3jr, "Uitgevallen binnen 3 jaar")),
    bg = NPULS_BLAUW,
    fg = NPULS_GEEL,
    definitie = DEFINITIES[[analyse_niveau()]]$uitval_3jr
  ))

  ## KPI's studiewissel ----

  output$kpi_wissel1 <- renderUI(vb(
    "Gewisseld binnen 1 jaar",
    pct_label(df(), \(d) waargenomen(d$studiewissel_1jr, "Gewisseld binnen 1 jaar")),
    bg = NPULS_GEEL,
    fg = NPULS_ZWART,
    definitie = DEFINITIES[[analyse_niveau()]]$studiewissel_1jr
  ))
  output$kpi_wissel3 <- renderUI(vb(
    "Gewisseld binnen 3 jaar",
    pct_label(df(), \(d) waargenomen(d$studiewissel_3jr, "Gewisseld binnen 3 jaar")),
    bg = NPULS_GROEN,
    fg = NPULS_ZWART,
    definitie = DEFINITIES[[analyse_niveau()]]$studiewissel_3jr
  ))

  ## Plots overzicht ----

  output$plot_status <- renderPlotly(ggplotly(pct_bar(df(), status)))
  output$plot_ov_geslacht <- renderPlotly(ggplotly(pct_bar(df(), geslacht)))
  output$plot_ov_vorm <- renderPlotly(ggplotly(pct_bar(df(), opleidingsvorm)))

  instroom_plotly <- reactive({
    y_label <- if (analyse_niveau() == "inschrijving") {
      "Inschrijvingen"
    } else {
      "Studenten"
    }
    ggplotly(instroom_trend(df(), y_label = y_label), tooltip = c("x", "y")) |>
      layout(legend = LEGEND_LAYOUT)
  })

  output$plot_overzicht_instroom <- renderPlotly(instroom_plotly())

  ## Plots instroom ----

  output$plot_instroom_trend <- renderPlotly(instroom_plotly())
  output$plot_instroom_sector <- renderPlotly(ggplotly(pct_bar(
    df(),
    sector
  )))
  output$plot_instroom_int <- renderPlotly(ggplotly(pct_bar(df(), int_student)))
  output$plot_instroom_geslacht <- renderPlotly(ggplotly(pct_bar(
    df(),
    geslacht
  )))
  output$plot_instroom_vooropleiding <- renderPlotly(ggplotly(pct_bar(
    df(),
    vooropleiding
  )))
  output$plot_instroom_eerstejaars_ho <- renderPlotly(ggplotly(pct_bar(
    df(),
    eerstejaars_ho
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
      layout(legend = LEGEND_LAYOUT)
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

  rend5 <- \(d) waargenomen(d$rendement_5jr, "Diploma binnen 5 jaar")
  output$plot_rend5_vooropleiding <- renderPlotly(ggplotly(pct_per_groep(
    df(), vooropleiding, rend5, "% diploma binnen 5 jaar"
  )))
  output$plot_rend5_eerstejaars_ho <- renderPlotly(ggplotly(pct_per_groep(
    df(), eerstejaars_ho, rend5, "% diploma binnen 5 jaar"
  )))

  ## Plots uitval ----

  uitval1 <- \(d) waargenomen(d$uitval_1jr, "Uitgevallen binnen 1 jaar")
  output$plot_uitval1_vooropleiding <- renderPlotly(ggplotly(pct_per_groep(
    df(), vooropleiding, uitval1, "% uitgevallen binnen 1 jaar"
  )))
  output$plot_uitval1_eerstejaars_ho <- renderPlotly(ggplotly(pct_per_groep(
    df(), eerstejaars_ho, uitval1, "% uitgevallen binnen 1 jaar"
  )))

  output$plot_uitval_trend <- renderPlotly(
    ggplotly(uitval_trend(df()), tooltip = c("x", "y", "colour")) |>
      layout(legend = LEGEND_LAYOUT)
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
      layout(legend = LEGEND_LAYOUT)
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

  ## Vooropleiding tab (VAKHAVW) ----

  output$kpi_gem_eindcijfer <- renderUI({
    d <- df()
    gem <- round(mean(d$vakhawv_gemiddeld_eindcijfer, na.rm = TRUE), 1)
    vb(
      "Gem. eindcijfer vooropleiding",
      if (is.nan(gem)) "—" else as.character(gem),
      bg = NPULS_BLAUW,
      fg = NPULS_GEEL,
      definitie = DEFINITIES[[analyse_niveau()]]$vakhawv_gemiddeld_eindcijfer
    )
  })

  output$kpi_aantal_vakken <- renderUI({
    d <- df()
    gem <- round(mean(d$vakhawv_aantal_vakken, na.rm = TRUE), 1)
    vb(
      "Gem. aantal vakken",
      if (is.nan(gem)) "—" else as.character(gem),
      bg = NPULS_ORANJE,
      fg = NPULS_ZWART,
      definitie = DEFINITIES[[analyse_niveau()]]$vakhawv_aantal_vakken
    )
  })

  output$kpi_pct_met_vakhawv <- renderUI({
    d <- df()
    pct <- round(mean(!is.na(d$vakhawv_gemiddeld_eindcijfer)) * 100)
    vb(
      "% met VAKHAVW-data",
      if (is.nan(pct)) "—" else paste0(pct, "%"),
      bg = NPULS_GROEN,
      fg = NPULS_ZWART,
      definitie = "Percentage studenten waarvoor vooropleidingsgegevens uit het geuploade VAKHAVW-bestand gekoppeld konden worden (op persoonsgebonden_nummer)."
    )
  })

  ## Vak selector en dynamische vakcijfer ----

  output$vak_keuze_ui <- renderUI({
    req(vakhawv_raw())
    vakken <- sort(unique(vakhawv_raw()$afkorting_vak))
    selectInput(
      "vak_keuze",
      "Vak",
      choices = vakken,
      selected = if ("wis" %in% vakken) "wis" else vakken[1]
    )
  })

  vak_per_student <- reactive({
    req(vakhawv_raw(), input$vak_keuze)
    vakhawv_raw() |>
      dplyr::filter(afkorting_vak == input$vak_keuze) |>
      dplyr::group_by(persoonsgebonden_nummer) |>
      dplyr::summarise(
        .vak_cijfer = mean(cijfer_eerste_centraal_examen, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        .vak_cijfer = dplyr::if_else(is.nan(.vak_cijfer), NA_real_, .vak_cijfer)
      )
  })

  df_met_vak <- reactive({
    req(vak_per_student())
    dplyr::left_join(
      dplyr::mutate(df(), persoonsgebonden_nummer = as.character(persoonsgebonden_nummer)),
      vak_per_student(),
      by = "persoonsgebonden_nummer"
    )
  })

  output$kpi_vak_cijfer <- renderUI({
    d <- df_met_vak()
    gem <- round(mean(d$.vak_cijfer, na.rm = TRUE), 1)
    vb(
      paste0("Gem. CE-cijfer ", toupper(input$vak_keuze)),
      if (is.nan(gem)) "—" else as.character(gem),
      bg = NPULS_BLAUW,
      fg = NPULS_GEEL,
      definitie = paste0(
        "Gemiddeld centraal examencijfer (schaal 1-10) voor het vak '",
        input$vak_keuze, "', afkomstig uit het VAKHAVW-bestand."
      )
    )
  })

  output$kpi_pct_vak <- renderUI({
    d <- df_met_vak()
    pct <- round(mean(!is.na(d$.vak_cijfer)) * 100)
    vb(
      paste0("% met ", toupper(input$vak_keuze)),
      if (is.nan(pct)) "—" else paste0(pct, "%"),
      bg = NPULS_GROEN,
      fg = NPULS_ZWART,
      definitie = paste0(
        "Percentage studenten met een centraal examencijfer voor '",
        input$vak_keuze, "' op de VAKHAVW-eindlijst."
      )
    )
  })

  output$plot_vak_sector <- renderPlotly({
    d <- df_met_vak() |>
      dplyr::filter(!is.na(sector), !is.na(.vak_cijfer))
    if (nrow(d) == 0) {
      return(ggplotly(leeg_plot()))
    }
    agg <- d |>
      dplyr::group_by(sector = as.character(sector)) |>
      dplyr::summarise(gem = mean(.vak_cijfer, na.rm = TRUE), .groups = "drop")
    kleuren <- kleur_voor(agg$sector)
    p <- ggplot(agg, aes(x = fct_reorder(sector, gem), y = gem, fill = sector)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(gem, 1)), hjust = -0.15, size = 3.3) +
      scale_y_continuous(limits = c(0, 10)) +
      scale_fill_manual(values = kleuren, breaks = names(kleuren)) +
      coord_flip() +
      labs(
        x = NULL,
        y = "Gem. CE-cijfer",
        title = paste0(toupper(input$vak_keuze), " per sector")
      ) +
      thema()
    ggplotly(p) |> layout(showlegend = FALSE)
  })

  output$plot_vak_rendement <- renderPlotly({
    d <- df_met_vak() |>
      dplyr::filter(!is.na(rendement), !is.na(.vak_cijfer))
    if (nrow(d) == 0) {
      return(ggplotly(leeg_plot()))
    }
    agg <- d |>
      dplyr::group_by(rendement) |>
      dplyr::summarise(gem = mean(.vak_cijfer, na.rm = TRUE), .groups = "drop")
    kleuren <- kleur_voor(agg$rendement)
    p <- ggplot(agg, aes(x = fct_reorder(rendement, gem), y = gem, fill = rendement)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(gem, 1)), hjust = -0.15, size = 3.3) +
      scale_y_continuous(limits = c(0, 10)) +
      scale_fill_manual(values = kleuren, breaks = names(kleuren)) +
      coord_flip() +
      labs(
        x = NULL,
        y = "Gem. CE-cijfer",
        title = paste0(toupper(input$vak_keuze), " per rendement")
      ) +
      thema()
    ggplotly(p) |> layout(showlegend = FALSE)
  })

  output$plot_eindcijfer_hist <- renderPlotly({
    d <- df()
    cijfers <- d$vakhawv_gemiddeld_eindcijfer
    cijfers <- cijfers[!is.na(cijfers)]
    if (length(cijfers) == 0) {
      return(ggplotly(leeg_plot()))
    }
    p <- ggplot(data.frame(cijfer = cijfers), aes(x = cijfer)) +
      geom_histogram(
        fill = NPULS_BLAUW,
        alpha = 0.85,
        binwidth = 0.5,
        boundary = 0
      ) +
      scale_x_continuous(breaks = seq(4, 10, 1), limits = c(4, 10)) +
      labs(x = "Gemiddeld eindcijfer", y = "Studenten") +
      thema()
    ggplotly(p) |> layout(showlegend = FALSE)
  })

  output$plot_eindcijfer_sector <- renderPlotly({
    d <- df() |>
      dplyr::filter(!is.na(sector), !is.na(vakhawv_gemiddeld_eindcijfer))
    if (nrow(d) == 0) {
      return(ggplotly(leeg_plot()))
    }
    agg <- d |>
      dplyr::group_by(sector = as.character(sector)) |>
      dplyr::summarise(
        gem = mean(vakhawv_gemiddeld_eindcijfer, na.rm = TRUE),
        .groups = "drop"
      )
    kleuren <- kleur_voor(agg$sector)
    p <- ggplot(agg, aes(x = fct_reorder(sector, gem), y = gem, fill = sector)) +
      geom_col(show.legend = FALSE) +
      geom_text(
        aes(label = round(gem, 1)),
        hjust = -0.15,
        size = 3.3
      ) +
      scale_y_continuous(limits = c(0, 10)) +
      scale_fill_manual(values = kleuren, breaks = names(kleuren)) +
      coord_flip() +
      labs(x = NULL, y = "Gem. eindcijfer") +
      thema()
    ggplotly(p) |> layout(showlegend = FALSE)
  })

  output$plot_eindcijfer_rendement <- renderPlotly({
    d <- df() |>
      dplyr::filter(!is.na(rendement), !is.na(vakhawv_gemiddeld_eindcijfer))
    if (nrow(d) == 0) {
      return(ggplotly(leeg_plot()))
    }
    agg <- d |>
      dplyr::group_by(rendement) |>
      dplyr::summarise(
        gem = mean(vakhawv_gemiddeld_eindcijfer, na.rm = TRUE),
        .groups = "drop"
      )
    kleuren <- kleur_voor(agg$rendement)
    p <- ggplot(agg, aes(x = fct_reorder(rendement, gem), y = gem, fill = rendement)) +
      geom_col(show.legend = FALSE) +
      geom_text(
        aes(label = round(gem, 1)),
        hjust = -0.15,
        size = 3.3
      ) +
      scale_y_continuous(limits = c(0, 10)) +
      scale_fill_manual(values = kleuren, breaks = names(kleuren)) +
      coord_flip() +
      labs(x = NULL, y = "Gem. eindcijfer") +
      thema()
    ggplotly(p) |> layout(showlegend = FALSE)
  })

  ## Bekostiging tab ----

  output$bek_peiljaar_badge <- renderUI({
    jaar  <- bek_peiljaar()
    jaren <- bek_jaren()
    if (is.null(jaar) || is.na(jaar)) return(NULL)
    jaren_tekst <- if (!is.null(jaren) && length(jaren) > 0) {
      paste0(
        " — bekostigde studenten met instroomjaar ",
        paste(jaren, collapse = " en ")
      )
    } else {
      ""
    }
    tags$p(
      paste0("Bekostigingsjaar ", jaar, jaren_tekst),
      style = paste0(
        "font-size:0.78rem;font-weight:600;color:#374151;",
        "background:#F3F4F6;border-radius:4px;padding:0.35rem 0.75rem;",
        "display:inline-block;margin-bottom:0.75rem;"
      )
    )
  })

  output$kpi_pct_bekostigd <- renderUI({
    d <- df() |> dplyr::filter(!is.na(indicatie_bekostigd))
    pct <- round(mean(d$indicatie_bekostigd, na.rm = TRUE) * 100)
    vb(
      "% bekostigd",
      if (length(pct) == 0 || is.nan(pct)) "—" else paste0(pct, "%"),
      bg = NPULS_GROEN,
      fg = NPULS_ZWART,
      definitie = DEFINITIES[[analyse_niveau()]]$indicatie_bekostigd
    )
  })

  output$kpi_n_bekostigd <- renderUI({
    d <- df()
    n <- sum(d$indicatie_bekostigd == TRUE, na.rm = TRUE)
    eenheid <- if (analyse_niveau() == "inschrijving") "inschrijvingen" else "studenten"
    vb(paste0("Bekostigd (", eenheid, ")"), n_label(n), bg = NPULS_BLAUW, fg = NPULS_GEEL)
  })

  output$kpi_n_niet_bekostigd <- renderUI({
    d <- df()
    n <- sum(d$indicatie_bekostigd == FALSE, na.rm = TRUE)
    eenheid <- if (analyse_niveau() == "inschrijving") "inschrijvingen" else "studenten"
    vb(paste0("Niet bekostigd (", eenheid, ")"), n_label(n), bg = NPULS_ORANJE, fg = NPULS_ZWART)
  })

  output$kpi_pct_hoofdinschrijving <- renderUI({
    d <- df() |> dplyr::filter(!is.na(indicatie_hoofdinschrijving))
    pct <- round(mean(d$indicatie_hoofdinschrijving, na.rm = TRUE) * 100)
    vb(
      "% hoofdinschrijving",
      if (length(pct) == 0 || is.nan(pct)) "—" else paste0(pct, "%"),
      bg = NPULS_GEEL,
      fg = NPULS_ZWART,
      definitie = DEFINITIES[[analyse_niveau()]]$indicatie_hoofdinschrijving
    )
  })

  output$kpi_pct_herstelbaar <- renderUI({
    d <- df() |> dplyr::filter(!is.na(indicatie_herstelbaar))
    pct <- round(mean(d$indicatie_herstelbaar, na.rm = TRUE) * 100)
    vb(
      "% herstelbaar (van niet bekostigd)",
      if (nrow(d) == 0 || is.nan(pct)) "—" else paste0(pct, "%"),
      bg = NPULS_ROZE,
      fg = NPULS_ZWART,
      definitie = DEFINITIES[[analyse_niveau()]]$indicatie_herstelbaar
    )
  })

  bek_pct_bar <- function(d, groep_col, titel) {
    d <- d |> dplyr::filter(!is.na(indicatie_bekostigd), !is.na(.data[[groep_col]]))
    if (nrow(d) == 0) return(ggplotly(leeg_plot()))
    agg <- d |>
      dplyr::group_by(groep = as.character(.data[[groep_col]])) |>
      dplyr::summarise(
        pct = mean(indicatie_bekostigd == TRUE, na.rm = TRUE),
        n   = dplyr::n(),
        .groups = "drop"
      )
    kleuren <- kleur_voor(agg$groep)
    p <- ggplot(agg, aes(x = fct_reorder(groep, pct), y = pct, fill = groep)) +
      geom_col(show.legend = FALSE) +
      geom_text(
        aes(label = paste0(round(pct * 100), "% (n=", n, ")")),
        hjust = -0.05, size = 3.3
      ) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1.2)) +
      scale_fill_manual(values = kleuren, breaks = names(kleuren)) +
      coord_flip() +
      labs(x = NULL, y = "% bekostigd", title = titel) +
      thema() +
      theme(legend.position = "none")
    ggplotly(p) |> layout(showlegend = FALSE)
  }

  output$plot_bek_status <- renderPlotly({
    d <- df() |> dplyr::filter(!is.na(indicatie_bekostigd))
    if (nrow(d) == 0) return(ggplotly(leeg_plot()))
    tel <- d |>
      dplyr::count(
        status = dplyr::if_else(indicatie_bekostigd, "Bekostigd", "Niet bekostigd")
      ) |>
      dplyr::mutate(pct = n / sum(n))
    kleuren <- c("Bekostigd" = NPULS_GROEN, "Niet bekostigd" = NPULS_ORANJE)
    p <- ggplot(tel, aes(x = fct_reorder(status, pct), y = pct, fill = status)) +
      geom_col(show.legend = FALSE) +
      geom_text(
        aes(label = paste0(round(pct * 100), "% (n=", n, ")")),
        hjust = -0.05, size = 3.3
      ) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1.2)) +
      scale_fill_manual(values = kleuren) +
      coord_flip() +
      labs(x = NULL, y = NULL) +
      thema() +
      theme(legend.position = "none")
    ggplotly(p) |> layout(showlegend = FALSE)
  })

  output$plot_bek_sector    <- renderPlotly(bek_pct_bar(df(), "sector", NULL))
  output$plot_bek_vorm      <- renderPlotly(bek_pct_bar(df(), "opleidingsvorm", NULL))
  output$plot_bek_niveau    <- renderPlotly(bek_pct_bar(df(), "opleidingsniveau", NULL))

  output$plot_bek_redenen <- renderPlotly({
    d <- df() |> dplyr::filter(!is.na(reden_niet_bekostigd))
    if (nrow(d) == 0) return(ggplotly(leeg_plot()))

    ## Een rij kan meerdere, met "; " samengevoegde redenen hebben (zie
    ## verrijk_met_bekostiging()); die worden hier los geteld.
    tel <- tibble::tibble(
      reden = unlist(strsplit(d$reden_niet_bekostigd, "; ", fixed = TRUE))
    ) |>
      dplyr::count(reden, name = "n", sort = TRUE) |>
      dplyr::left_join(
        dplyr::select(BEKOSTIGINGSTATUS_CODES, omschrijving, herstelbaar),
        by = c("reden" = "omschrijving")
      ) |>
      dplyr::mutate(
        categorie = dplyr::case_when(
          is.na(herstelbaar) ~ "Onbekend",
          herstelbaar        ~ "Herstelbaar",
          TRUE               ~ "Structureel"
        ),
        reden_kort = dplyr::if_else(
          nchar(reden) > 65,
          paste0(substr(reden, 1, 62), "..."),
          reden
        )
      ) |>
      dplyr::slice_max(n, n = 10, with_ties = FALSE)

    kleuren <- c(
      "Herstelbaar"  = NPULS_GROEN,
      "Structureel"  = NPULS_ORANJE,
      "Onbekend"     = KLEUR_NEUTRAAL
    )
    p <- ggplot(tel, aes(x = fct_reorder(reden_kort, n), y = n, fill = categorie)) +
      geom_col() +
      geom_text(aes(label = n), hjust = -0.15, size = 3.3) +
      scale_fill_manual(values = kleuren, breaks = names(kleuren)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      coord_flip() +
      labs(x = NULL, y = "Aantal", fill = NULL) +
      thema()
    ggplotly(p) |> layout(legend = LEGEND_LAYOUT)
  })

  ## Data tab ----

  output$tabel_dict <- renderDT({
    datatable(
      DICT,
      rownames = FALSE,
      options = list(pageLength = nrow(DICT), dom = "t", ordering = FALSE),
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
            "Studiewissel",
            "Vooropleiding (VAKHAVW)",
            "Bekostiging (VLPBEK)"
          ),
          c(
            "#CCEEE6", "#D6E2FD", "#FFE4D8", "#FFE4D8", "#FCF5D4", "#FFF3CD",
            NPULS_ROZE
          )
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

  output$download_benchmark <- downloadHandler(
    filename = function() {
      paste0("Benchmarkrapport_peildatum_", peildatum(), ".xlsx")
    },
    content = function(file) {
      rapport <- maak_benchmarkrapport(
        df_data(),
        niveau = analyse_niveau(),
        peildatum = peildatum()
      )
      schrijf_benchmarkrapport(rapport, file)
    }
  )
}

shinyApp(ui, server)
