#' Bereken uitvalindicatoren per cohort
#'
#' Bepaalt voor elke student in het instroomcohort of zij zijn uitgevallen,
#' zittend of gediplomeerd. Uitval wordt gemarkeerd als een student niet meer
#' ingeschreven staat en geen diploma heeft.
#'
#' @param basisbestand Tibble zoals gemaakt door [maak_basisbestand()]
#' @param diploma_behaald Tibble zoals gemaakt door [maak_diploma_behaald()]
#' @param cohorten_instroom Tibble zoals gemaakt door [maak_instroom_cohort()]
#' @param jaar Integer, peiljaar van de analyse (bijv. `2025`). Studenten die
#'   in `jaar - 1` nog ingeschreven staan, gelden als zittend.
#'
#' @return Een tibble met kolommen `persoonsgebonden_nummer`,
#'   `laatste_jaar_inschrijving`, `diploma`, `status` (factor:
#'   Diploma behaald / Zittend / Uitgevallen), `uitval_xjr` (jaar van uitval
#'   t.o.v. instroomjaar), `uitval_1jr` en `uitval_3jr` (factoren).
#'   Gooit een fout bij dubbele studenten of ontbrekende statussen.
#'
#' @examples
#' basis <- tibble::tibble(
#'   persoonsgebonden_nummer = c("S1", "S1", "S2"),
#'   inschrijvingsjaar = c(2020L, 2021L, 2020L),
#'   soort_inschrijving_actuele_instelling = "hoofdinschrijving"
#' )
#' diploma <- tibble::tibble(
#'   persoonsgebonden_nummer = "S1",
#'   jaar_eerste_diploma = 2022L,
#'   verblijfsjaar_eerste_diploma = 3L,
#'   diploma = "Diploma behaald (excl. propedeuse)"
#' )
#' cohort <- tibble::tibble(
#'   persoonsgebonden_nummer = c("S1", "S2"),
#'   eerstejaar_instelling = 2020L
#' )
#' bereken_uitval(basis, diploma, cohort, jaar = 2023L)
#' @export
bereken_uitval <- function(
  basisbestand,
  diploma_behaald,
  cohorten_instroom,
  jaar
) {
  ## Bepaal het laatste inschrijvingsjaar per student
  uitstroom <- basisbestand |>
    dplyr::group_by(persoonsgebonden_nummer) |>
    dplyr::arrange(
      dplyr::desc(inschrijvingsjaar),
      soort_inschrijving_actuele_instelling,
      .by_group = TRUE
    ) |>
    dplyr::distinct(persoonsgebonden_nummer, .keep_all = TRUE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      ## jaar - 1 is het meest recente studiejaar in de data; studenten die
      ## daar nog in staan zijn niet uitgevallen maar zittend
      laatste_jaar_inschrijving = dplyr::case_when(
        inschrijvingsjaar == jaar - 1 ~ NA_real_,
        is.na(inschrijvingsjaar) ~ NA_real_,
        TRUE ~ as.numeric(inschrijvingsjaar)
      )
    ) |>
    dplyr::select(
      persoonsgebonden_nummer,
      inschrijvingsjaar,
      laatste_jaar_inschrijving
    )

  if (any(duplicated(uitstroom$persoonsgebonden_nummer))) {
    rlang::abort("Dubbele studentnummers in het uitvalbestand gevonden!")
  }

  uitval <- uitstroom |>
    dplyr::left_join(diploma_behaald, by = "persoonsgebonden_nummer") |>
    dplyr::left_join(
      cohorten_instroom |>
        dplyr::select(persoonsgebonden_nummer, eerstejaar_instelling),
      by = "persoonsgebonden_nummer"
    ) |>
    dplyr::mutate(
      status = factor(dplyr::case_when(
        diploma == "Diploma behaald (excl. propedeuse)" ~ "Diploma behaald",
        inschrijvingsjaar == jaar - 1 ~ "Zittend",
        TRUE ~ "Uitgevallen"
      )),
      uitval_xjr = dplyr::case_when(
        status == "Uitgevallen" ~ laatste_jaar_inschrijving +
          1 -
          eerstejaar_instelling
      ),
      uitval_1jr = factor(dplyr::case_when(
        uitval_xjr == 1 ~ "Uitgevallen binnen 1 jaar",
        TRUE ~ "Na 1 jaar nog ingeschreven of diploma behaald"
      )),
      uitval_3jr = factor(dplyr::case_when(
        uitval_xjr <= 3 ~ "Uitgevallen binnen 3 jaar",
        TRUE ~ "Na 3 jaar nog ingeschreven of diploma behaald"
      ))
    ) |>
    dplyr::select(
      persoonsgebonden_nummer,
      laatste_jaar_inschrijving,
      diploma,
      status,
      dplyr::starts_with("uitval")
    )

  if (any(is.na(uitval$status))) {
    rlang::abort("Niet alle statussen zijn gevuld")
  }

  uitval
}
