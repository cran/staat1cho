#' Lees een VAKHAVW-bestand in
#'
#' Leest een semicolongescheiden VAKHAVW-bestand (DUO 1CHO-formaat) in,
#' controleert de aanwezigheid van de vereiste kolommen en converteert
#' cijferkolommen naar decimale waarden. DUO slaat cijfers op als gehele
#' getallen vermenigvuldigd met 10 (bijv. 69 = 6.9).
#'
#' @param pad Pad naar het semicolongescheiden VAKHAVW-bestand (UTF-8)
#'
#' @return Een tibble met de ruwe vakcijferdata per student per vak,
#'   gereed voor gebruik in [verrijk_met_vakhawv()]
#'
#' @examples
#' pad <- system.file("extdata/voorbeeld_vakhawv.csv", package = "staat1cho")
#' vakhawv <- lees_vakhawv(pad)
#' @export
lees_vakhawv <- function(pad) {
  vereist <- c(
    "persoonsgebonden_nummer",
    "afkorting_vak",
    "gemiddeld_cijfer_cijferlijst",
    "cijfer_eerste_centraal_examen",
    "cijfer_schoolexamen"
  )

  data <- readr::read_delim(
    pad,
    delim = ";",
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(encoding = "UTF-8")
  )

  ontbrekend <- setdiff(vereist, names(data))
  if (length(ontbrekend) > 0) {
    rlang::abort(paste0(
      "Ontbrekende kolommen in VAKHAVW-bestand: ",
      paste(ontbrekend, collapse = ", ")
    ))
  }

  ## DUO slaat cijfers op als gehele getallen x 10 (69 = 6.9). Een waarde
  ## van 0 betekent dat het onderdeel niet is afgelegd (geen CE of SE), niet
  ## een werkelijk cijfer. Die worden omgezet naar NA.
  data |>
    dplyr::mutate(
      dplyr::across(
        c(
          gemiddeld_cijfer_cijferlijst,
          cijfer_eerste_centraal_examen,
          cijfer_schoolexamen
        ),
        ~ {
          num <- suppressWarnings(as.numeric(.x))
          dplyr::if_else(num == 0, NA_real_, num / 10)
        }
      ),
      persoonsgebonden_nummer = as.character(persoonsgebonden_nummer),
      afkorting_vak = tolower(as.character(afkorting_vak))
    )
}

#' Verrijk indicatoren met VAKHAVW-vooropleidingsgegevens
#'
#' Aggregeert vakcijferdata per student en koppelt ze aan het
#' indicatorenbestand via `persoonsgebonden_nummer`. Berekent per student:
#'
#' - `vakhawv_gemiddeld_eindcijfer`: hoogste gemiddelde eindcijfer van de
#'   cijferlijst (max over meerdere opleidingsjaren als die voorkomen)
#' - `vakhawv_wiskundecijfer`: gemiddeld centraal examencijfer voor wiskunde
#'   (alle vakken waarvan de afkorting begint met `"wis"`)
#' - `vakhawv_aantal_vakken`: aantal unieke vakken op de cijferlijst
#'
#' Studenten zonder overeenkomst in de VAKHAVW-data krijgen `NA` voor alle
#' drie de kolommen. De functie meldt welk deel van de VAKHAVW-studenten is
#' teruggevonden (attribuut `koppeling`) en waarschuwt onder de 10%. Een
#' lager aandeel dan 100% is normaal: VAKHAVW bevat ook studenten die voor
#' het eerste cohort in de data begonnen.
#'
#' DUO vult het persoonsgebonden nummer in EV aan met spaties en in VAKHAVW
#' met nullen; de koppeling negeert voorloopnullen. Werkt op zowel student-
#' als inschrijvingsniveau: bij
#' inschrijvingsniveau worden de vooropleidingsgegevens van een student
#' voor elke opleiding herhaald.
#'
#' @param indicatoren Tibble zoals gemaakt door [combineer_indicatoren()],
#'   met een kolom `persoonsgebonden_nummer`
#' @param vakhawv Tibble zoals gemaakt door [lees_vakhawv()]
#'
#' @return De indicatoren-tibble uitgebreid met de kolommen
#'   `vakhawv_gemiddeld_eindcijfer`, `vakhawv_wiskundecijfer` en
#'   `vakhawv_aantal_vakken`
#'
#' @examples
#' indicatoren <- tibble::tibble(
#'   persoonsgebonden_nummer = c("S001", "S002", "S003"),
#'   inschrijvingsjaar = 2020L
#' )
#' vakhawv <- tibble::tibble(
#'   persoonsgebonden_nummer = c("S001", "S001", "S002"),
#'   afkorting_vak = c("ne", "wis", "ne"),
#'   gemiddeld_cijfer_cijferlijst = c(7.2, 7.2, 6.5),
#'   cijfer_eerste_centraal_examen = c(7.0, 6.5, 6.2),
#'   cijfer_schoolexamen = c(7.5, 6.8, 6.8)
#' )
#' verrijk_met_vakhawv(indicatoren, vakhawv)
#' @export
verrijk_met_vakhawv <- function(indicatoren, vakhawv) {
  if (!"persoonsgebonden_nummer" %in% names(indicatoren)) {
    rlang::abort(paste0(
      "Kolom `persoonsgebonden_nummer` niet gevonden in indicatoren. ",
      "Zorg dat `combineer_indicatoren()` is aangeroepen met de standaardinstellingen."
    ))
  }

  controleer_id_soort(
    indicatoren$persoonsgebonden_nummer,
    vakhawv$persoonsgebonden_nummer,
    "VAKHAVW",
    "Gebruik voor het 1CHO- en VAKHAVW-bestand dezelfde 1cijferho-uitvoer."
  )

  ## DUO vult het persoonsgebonden nummer in EV aan met spaties ("     2",
  ## na 1cijferho "2") en in VAKHAVW met nullen ("000000000002"). Koppel
  ## daarom op het nummer zonder voorloopnullen; de opgeslagen nummers
  ## blijven ongewijzigd.
  per_student <- vakhawv |>
    dplyr::mutate(persoonsgebonden_nummer = koppelsleutel(persoonsgebonden_nummer)) |>
    dplyr::group_by(persoonsgebonden_nummer) |>
    dplyr::summarise(
      vakhawv_gemiddeld_eindcijfer = max(
        gemiddeld_cijfer_cijferlijst,
        na.rm = TRUE
      ),
      vakhawv_wiskundecijfer = {
        wis <- cijfer_eerste_centraal_examen[grepl("^wis", afkorting_vak)]
        if (length(wis) == 0 || all(is.na(wis))) NA_real_
        else mean(wis, na.rm = TRUE)
      },
      vakhawv_aantal_vakken = dplyr::n_distinct(afkorting_vak, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(dplyr::across(
      c(vakhawv_gemiddeld_eindcijfer, vakhawv_wiskundecijfer),
      ~ dplyr::if_else(is.infinite(.x) | is.nan(.x), NA_real_, .x)
    ))

  sleutels <- koppelsleutel(indicatoren$persoonsgebonden_nummer)
  resultaat <- dplyr::left_join(
    dplyr::mutate(
      indicatoren,
      persoonsgebonden_nummer = as.character(persoonsgebonden_nummer),
      .koppelsleutel = sleutels
    ),
    dplyr::rename(per_student, .koppelsleutel = persoonsgebonden_nummer),
    by = ".koppelsleutel"
  ) |>
    dplyr::select(-.koppelsleutel)
  meld_koppeling(
    resultaat,
    bron_gevonden = per_student$persoonsgebonden_nummer %in% sleutels,
    n_verrijkt = sum(sleutels %in% per_student$persoonsgebonden_nummer),
    bron = "VAKHAVW",
    eenheid = "studenten",
    drempel = 0.1
  )
}
