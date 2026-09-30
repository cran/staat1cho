#' Lees het 1CHO-bestand in en voeg label-kolommen toe
#'
#' Leest een semicolongescheiden CSV-bestand (UTF-8) in en voegt
#' extra `_label`-kolommen toe voor gebruik in rapportages. Het pakket
#' bevat een klein synthetisch voorbeeldbestand zonder echte persoonsgegevens
#' (`inst/extdata/voorbeeld_1cho.csv`).
#'
#' @param pad_invoer Pad naar het semicolongescheiden CSV-bestand (UTF-8)
#'
#' @return Een tibble met alle 1CHO-regels plus extra `_label`-kolommen die de
#'   originele categorische waarden bewaren voor gebruik in rapportages
#'
#' @examples
#' # voorbeeld_1cho.csv is a small synthetic dataset bundled with the package
#' pad <- system.file("extdata/voorbeeld_1cho.csv", package = "staat1cho")
#' basis <- suppressMessages(maak_basisbestand(pad))
#' @export
maak_basisbestand <- function(pad_invoer) {
  ## Alles als tekst inlezen: readr raadt types op de eerste 1000 rijen, wat
  ## voorloopnullen in ID's en postcodes ("0010") weggooit en leeftijd ("021")
  ## soms als tekst en soms als getal oplevert. De numerieke kolommen worden
  ## hieronder expliciet omgezet.
  invoer <- readr::read_delim(
    pad_invoer,
    delim = ";",
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(encoding = "UTF-8")
  )

  if (!"vestigingsnummer_gemeentenaam_volgens_rio" %in% names(invoer)) {
    cli::cli_abort(
      "Kolom {.val vestigingsnummer_gemeentenaam_volgens_rio} niet gevonden in het invoerbestand."
    )
  }

  invoer <- zet_om_naar_integer(invoer, INTEGER_KOLOMMEN_1CHO)

  invoer |>
    dplyr::mutate(
      soort_inschrijving_actuele_instelling_label = soort_inschrijving_actuele_instelling,
      geslacht_label = geslacht,
      opleidingsvorm_label = opleidingsvorm,
      indicatie_internationale_student_label = indicatie_internationale_student,
      indicatie_eer_actueel_label = indicatie_eer_actueel,
      croho_onderdeel_actuele_opleiding_label = croho_onderdeel_actuele_opleiding,
      soort_diploma_instelling_label = soort_diploma_instelling,
      locatie_label = vestigingsnummer_gemeentenaam_volgens_rio
    )
}

#' Maak een eerstejaarscohort per instelling
#'
#' Filtert het basisbestand op het opgegeven soort hoger onderwijs,
#' hoofdinschrijvingen en eerste verblijfsjaar.
#'
#' @param basisbestand Tibble zoals gemaakt door [maak_basisbestand()]
#' @param soort_ho Character vector met toegestane waarden van
#'   `soort_hoger_onderwijs`, bijv. `c("hoger beroepsonderwijs", "hbo")`
#' @param niveau Analyseniveau: `"student"` (standaard) of `"inschrijving"`.
#'   Bij `"student"` is de sleutel `persoonsgebonden_nummer` en wordt gefilterd
#'   op het eerste jaar aan de instelling (`verblijfsjaar_actuele_instelling`).
#'   Bij `"inschrijving"` is de sleutel `persoonsgebonden_nummer` +
#'   `opleiding_actueel_equivalent` en wordt gefilterd op het eerste jaar in de
#'   specifieke opleiding (`verblijfsjaar_actuele_opleiding_instelling`), zodat
#'   wisselaars als eerstejaars in hun nieuwe opleiding worden meegenomen.
#'
#' @return Een tibble met een rij per student (bij `niveau = "student"`) of per
#'   student-opleidingcombinatie (bij `niveau = "inschrijving"`), aangevuld met
#'   kolom `eerstejaar_instelling` (= inschrijvingsjaar). Gooit een fout als er
#'   dubbele sleutelcombinaties zijn.
#'
#' @examples
#' basis <- tibble::tibble(
#'   persoonsgebonden_nummer = c("S1", "S2", "S3"),
#'   soort_hoger_onderwijs = c("hbo", "wo", "hbo"),
#'   soort_inschrijving_actuele_instelling_label =
#'     "hoofdinschrijving binnen het domein actuele instelling",
#'   verblijfsjaar_actuele_instelling = 1L,
#'   verblijfsjaar_actuele_opleiding_instelling = 1L,
#'   inschrijvingsjaar = 2020L,
#'   soort_diploma_instelling_label = NA_character_
#' )
#' maak_instroom_cohort(basis, "hbo")
#' @export
maak_instroom_cohort <- function(basisbestand, soort_ho, niveau = "student") {
  ## Bij studentniveau telt het eerste jaar aan de instelling (ongeacht
  ## opleiding). Bij inschrijvingsniveau telt het eerste jaar in de specifieke
  ## opleiding, waardoor wisselaars als eerstejaars in hun nieuwe opleiding
  ## worden meegenomen.
  verblijfsjaar_col <- if (niveau == "inschrijving") {
    "verblijfsjaar_actuele_opleiding_instelling"
  } else {
    "verblijfsjaar_actuele_instelling"
  }

  cohort <- basisbestand |>
    dplyr::filter(soort_hoger_onderwijs %in% soort_ho) |>
    dplyr::filter(
      soort_inschrijving_actuele_instelling_label ==
        "hoofdinschrijving binnen het domein actuele instelling",
      .data[[verblijfsjaar_col]] == 1
    ) |>
    dplyr::mutate(eerstejaar_instelling = inschrijvingsjaar)

  sleutels <- niveau_sleutels(niveau)
  if (anyDuplicated(cohort[sleutels]) > 0) {
    rlang::abort(
      "Dubbele sleutelcombinaties in het instroomcohortbestand gevonden!"
    )
  }

  cohort
}
