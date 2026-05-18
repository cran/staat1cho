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
  invoer <- readr::read_csv2(
    pad_invoer,
    locale = readr::locale(encoding = "UTF-8")
  )

  if (!"vestigingsnummer_gemeentenaam_volgens_rio" %in% names(invoer)) {
    cli::cli_abort(
      "Kolom {.val vestigingsnummer_gemeentenaam_volgens_rio} niet gevonden in het invoerbestand."
    )
  }

  invoer |>
    dplyr::mutate(
      verblijfsjaar_actuele_instelling = as.integer(
        verblijfsjaar_actuele_instelling
      ),
      diplomajaar = suppressWarnings(as.integer(diplomajaar)),
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
#'
#' @return Een tibble met één rij per student, aangevuld met kolom
#'   `eerstejaar_instelling` (= inschrijvingsjaar). Gooit een fout als er
#'   dubbele persoonsgebonden nummers zijn.
#'
#' @examples
#' basis <- tibble::tibble(
#'   persoonsgebonden_nummer = c("S1", "S2", "S3"),
#'   soort_hoger_onderwijs = c("hbo", "wo", "hbo"),
#'   soort_inschrijving_actuele_instelling_label =
#'     "hoofdinschrijving binnen het domein actuele instelling",
#'   verblijfsjaar_actuele_instelling = 1L,
#'   inschrijvingsjaar = 2020L,
#'   soort_diploma_instelling_label = NA_character_
#' )
#' maak_instroom_cohort(basis, "hbo")
#' @export
maak_instroom_cohort <- function(basisbestand, soort_ho) {
  cohort <- basisbestand |>
    dplyr::filter(soort_hoger_onderwijs %in% soort_ho) |>
    dplyr::filter(
      soort_inschrijving_actuele_instelling_label ==
        "hoofdinschrijving binnen het domein actuele instelling",
      verblijfsjaar_actuele_instelling == 1
    ) |>
    dplyr::mutate(eerstejaar_instelling = inschrijvingsjaar)

  if (any(duplicated(cohort$persoonsgebonden_nummer))) {
    rlang::abort(
      "Dubbele studentnummers in het instroomcohortbestand gevonden!"
    )
  }

  cohort
}
