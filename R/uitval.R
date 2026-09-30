#' Bereken uitvalindicatoren per cohort
#'
#' Bepaalt voor elke student (of inschrijving) in het instroomcohort of zij zijn
#' uitgevallen, zittend of gediplomeerd. Uitval wordt gemarkeerd als een student
#' niet meer ingeschreven staat en geen diploma heeft.
#'
#' @param basisbestand Tibble zoals gemaakt door [maak_basisbestand()]
#' @param diploma_behaald Tibble zoals gemaakt door [maak_diploma_behaald()]
#' @param cohorten_instroom Tibble zoals gemaakt door [maak_instroom_cohort()]
#' @param jaar Integer, peiljaar van de analyse (bijv. `2025`). Studenten die
#'   in `jaar - 1` nog ingeschreven staan, gelden als zittend. Standaard het
#'   jaar na het laatste inschrijvingsjaar in `basisbestand`. Een `jaar` waarbij
#'   de data inschrijvingen na `jaar - 1` bevat geeft een fout, omdat zittende
#'   studenten dan als uitgevallen zouden tellen.
#' @param niveau Analyseniveau: `"student"` (standaard) of `"inschrijving"`.
#'   Moet overeenkomen met het niveau waarop de andere invoertibbles zijn
#'   aangemaakt.
#'
#' @return Een tibble met de sleutelkolom(men), `laatste_jaar_inschrijving`,
#'   `diploma`, `status` (factor: Diploma behaald / Zittend / Uitgevallen),
#'   `uitval_xjr` (jaar van uitval t.o.v. instroomjaar), `uitval_1jr` en
#'   `uitval_3jr` (factoren). Cohorten waarvoor de periode nog niet volledig
#'   in de data zit (instroomjaar + 1 resp. + 3 na `jaar - 1`) krijgen
#'   `"Nog niet waarneembaar"`. Attribuut `laatste_jaar` is `jaar - 1`, het
#'   laatste inschrijvingsjaar in de data. Gooit een fout bij dubbele
#'   sleutelcombinaties of ontbrekende statussen.
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
  jaar = NULL,
  niveau = "student"
) {
  sleutels <- niveau_sleutels(niveau)

  laatste_in_data <- max(basisbestand$inschrijvingsjaar, na.rm = TRUE)
  if (is.null(jaar)) {
    jaar <- laatste_in_data + 1L
  } else if (laatste_in_data > jaar - 1) {
    cli::cli_abort(c(
      "De data bevat inschrijvingen in {laatste_in_data}, na het peiljaar {.arg jaar} - 1 = {jaar - 1}.",
      "i" = "Studenten die in {laatste_in_data} nog ingeschreven staan zouden als uitgevallen tellen.",
      "i" = "Gebruik {.code jaar = {laatste_in_data + 1}} of laat {.arg jaar} weg."
    ))
  }
  laatste_jaar <- jaar - 1

  ## Bepaal het laatste inschrijvingsjaar per sleutelcombinatie
  uitstroom <- basisbestand |>
    dplyr::group_by(dplyr::across(dplyr::all_of(sleutels))) |>
    dplyr::arrange(
      dplyr::desc(inschrijvingsjaar),
      soort_inschrijving_actuele_instelling,
      .by_group = TRUE
    ) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(sleutels)), .keep_all = TRUE) |>
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
      dplyr::all_of(sleutels),
      inschrijvingsjaar,
      laatste_jaar_inschrijving
    )

  if (anyDuplicated(uitstroom[sleutels]) > 0) {
    rlang::abort("Dubbele sleutelcombinaties in het uitvalbestand gevonden!")
  }

  uitval <- uitstroom |>
    dplyr::left_join(diploma_behaald, by = sleutels) |>
    dplyr::left_join(
      cohorten_instroom |>
        dplyr::select(dplyr::all_of(sleutels), eerstejaar_instelling),
      by = sleutels
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
      ## Uitval binnen x jaar is pas vast te stellen als instroomjaar + x in
      ## de data zit: anders weten we nog niet of de student terugkomt.
      uitval_1jr = factor(dplyr::case_when(
        eerstejaar_instelling + 1 > laatste_jaar ~ NIET_WAARNEEMBAAR,
        uitval_xjr == 1 ~ "Uitgevallen binnen 1 jaar",
        TRUE ~ "Na 1 jaar nog ingeschreven of diploma behaald"
      )),
      uitval_3jr = factor(dplyr::case_when(
        eerstejaar_instelling + 3 > laatste_jaar ~ NIET_WAARNEEMBAAR,
        uitval_xjr <= 3 ~ "Uitgevallen binnen 3 jaar",
        TRUE ~ "Na 3 jaar nog ingeschreven of diploma behaald"
      ))
    ) |>
    dplyr::select(
      dplyr::all_of(sleutels),
      laatste_jaar_inschrijving,
      diploma,
      status,
      dplyr::starts_with("uitval")
    )

  if (any(is.na(uitval$status))) {
    rlang::abort("Niet alle statussen zijn gevuld")
  }

  ## combineer_indicatoren() leidt hier de peildatum van af
  attr(uitval, "laatste_jaar") <- as.integer(laatste_jaar)
  uitval
}
