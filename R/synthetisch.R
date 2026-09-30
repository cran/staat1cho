#' Maak een synthetisch 1CHO-bestand met complete studieloopbanen
#'
#' Simuleert instroomcohorten van een fictieve hbo-instelling met vaste kansen
#' op uitval, studiewissel en diplomering, in het kolomformaat van de
#' 1cijferho-enriched output. Bedoeld voor demo's, de showcase en als
#' referentie bij validatie: omdat per student bekend is wat er gebeurde,
#' moet de pipeline die uitkomsten exact terugvinden.
#'
#' Per student wordt één loopbaan getrokken:
#'
#' - met kans `p_uitval_1jr` uitval na het eerste jaar;
#' - anders met kans `p_uitval_later` uitval na jaar 2 of 3;
#' - anders met kans `p_wissel` een wissel naar een andere opleiding in
#'   jaar 2, gevolgd door een diploma;
#' - anders een diploma na 4, 5 of 6 jaar (kansen 0,5 / 0,3 / 0,2). Een
#'   wisselaar doet er een jaar langer over.
#'
#' Het diploma staat op de inschrijvingsrij van het laatste studiejaar, met
#' `diplomajaar` gelijk aan dat `inschrijvingsjaar` (dezelfde conventie als
#' de DUO-data). Loopbanen worden afgekapt na het laatste jaar in `jaren`.
#'
#' @param n_per_jaar Aantal instromers per cohort
#' @param jaren Integer vector met instroomjaren; het laatste jaar is ook het
#'   laatste inschrijvingsjaar in de data
#' @param p_uitval_1jr,p_uitval_later,p_wissel Kansen, zie hierboven
#' @param seed Seed voor reproduceerbaarheid
#'
#' @return Een tibble met één rij per student per inschrijvingsjaar, klaar om
#'   met `readr::write_delim(x, pad, delim = ";", na = "")` weg te schrijven en
#'   in te lezen met [maak_basisbestand()]. Het attribuut `waarheid` bevat per
#'   student `persoonsgebonden_nummer`, `instroomjaar`, `uitval_na` (jaar van
#'   uitval of `NA`), `diploma_na` (studiejaar van diplomering of `NA`),
#'   `gewisseld` (`TRUE` bij een wissel in jaar 2), `vooropleiding` (havo,
#'   vwo, mbo, buitenlands of onbekend) en `eerder_in_ho` (`TRUE` als de
#'   student twee jaar voor instroom al in het HO stond), zonder afkapping.
#'
#' @examples
#' synth <- maak_synthetische_1cho(n_per_jaar = 20, jaren = 2018:2023)
#' head(attr(synth, "waarheid"))
#'
#' pad <- tempfile(fileext = ".csv")
#' readr::write_delim(synth, pad, delim = ";", na = "")
#' basis <- maak_basisbestand(pad)
#' @export
maak_synthetische_1cho <- function(
  n_per_jaar = 200L,
  jaren = 2012:2023,
  p_uitval_1jr = 0.15,
  p_uitval_later = 0.10,
  p_wissel = 0.10,
  seed = 1L
) {
  jaren <- as.integer(jaren)
  laatste_jaar <- max(jaren)
  opleidingen <- tibble::tibble(
    code = c("34401", "34402", "35501", "39110"),
    naam = c(
      "B Werktuigbouwkunde", "B Elektrotechniek",
      "B Verpleegkunde", "B Bedrijfskunde"
    ),
    sector = c("techniek", "techniek", "gezondheidszorg", "economie"),
    locatie = c("Breda", "Breda", "Den Bosch", "Den Bosch")
  )

  ## Gebruik een eigen RNG-stroom zodat de seed van de gebruiker niet wijzigt
  waarheid <- withr_seed(seed, {
    n <- n_per_jaar * length(jaren)
    uitkomst <- stats::runif(n)
    uitval_1 <- uitkomst < p_uitval_1jr
    uitval_later <- !uitval_1 & uitkomst < p_uitval_1jr + p_uitval_later
    rest <- !uitval_1 & !uitval_later
    gewisseld <- rest & stats::runif(n) < p_wissel
    duur_diploma <- sample(4:6, n, replace = TRUE, prob = c(0.5, 0.3, 0.2)) + gewisseld

    tibble::tibble(
      persoonsgebonden_nummer = sprintf("%09d", seq_len(n)),
      instroomjaar = rep(jaren, each = n_per_jaar),
      opleiding_start = sample(seq_len(nrow(opleidingen)), n, replace = TRUE),
      opleiding_wissel = (opleiding_start %% nrow(opleidingen)) + 1L,
      uitval_na = dplyr::case_when(
        uitval_1 ~ 1L,
        uitval_later ~ sample(2:3, n, replace = TRUE),
        TRUE ~ NA_integer_
      ),
      diploma_na = dplyr::if_else(rest, as.integer(duur_diploma), NA_integer_),
      gewisseld = gewisseld,
      geslacht = sample(c("man", "vrouw"), n, replace = TRUE),
      internationaal = stats::runif(n) < 0.08,
      postcode = sprintf("%04d", sample(c(10L, 4811:4838, 5211:5236), n, replace = TRUE)),
      leeftijd = sample(17:22, n, replace = TRUE, prob = c(0.2, 0.35, 0.2, 0.12, 0.08, 0.05)),
      ## Na de bestaande trekkingen, zodat die met dezelfde seed gelijk blijven
      vooropleiding = sample(
        names(SYNTH_VOOROPLEIDING), n, replace = TRUE,
        prob = c(0.45, 0.10, 0.35, 0.05, 0.05)
      ),
      eerder_in_ho = stats::runif(n) < 0.12
    )
  })

  ## Eén rij per studiejaar zolang de student ingeschreven is en het jaar in
  ## de data valt
  rijen <- waarheid |>
    dplyr::mutate(duur = dplyr::coalesce(uitval_na, diploma_na)) |>
    tidyr_uncount_jaren(laatste_jaar) |>
    dplyr::mutate(
      inschrijvingsjaar = instroomjaar + jaar_index - 1L,
      opl = dplyr::if_else(gewisseld & jaar_index >= 2L, opleiding_wissel, opleiding_start),
      verblijfsjaar_opleiding = dplyr::if_else(gewisseld & jaar_index >= 2L, jaar_index - 1L, jaar_index),
      diploma_rij = !is.na(diploma_na) & jaar_index == diploma_na
    )

  tibble::tibble(
    persoonsgebonden_nummer = rijen$persoonsgebonden_nummer,
    inschrijvingsjaar = rijen$inschrijvingsjaar,
    verblijfsjaar_actuele_instelling = rijen$jaar_index,
    verblijfsjaar_actuele_opleiding_instelling = rijen$verblijfsjaar_opleiding,
    diplomajaar = dplyr::if_else(rijen$diploma_rij, rijen$inschrijvingsjaar, NA_integer_),
    soort_hoger_onderwijs = "hoger beroepsonderwijs",
    soort_inschrijving_actuele_instelling = "hoofdinschrijving binnen het domein actuele instelling",
    geslacht = rijen$geslacht,
    opleidingsvorm = "voltijd",
    indicatie_internationale_student = dplyr::if_else(
      rijen$internationaal, "internationale student", "geen internationale student"
    ),
    indicatie_eer_actueel = dplyr::if_else(rijen$internationaal, "EER-student", "Nederlandse student"),
    croho_onderdeel_actuele_opleiding = opleidingen$sector[rijen$opl],
    soort_diploma_instelling = dplyr::if_else(
      rijen$diploma_rij,
      "Hoofd-bachelor-diploma binnen de actuele instelling",
      NA_character_
    ),
    opleiding_actueel_equivalent = opleidingen$code[rijen$opl],
    type_hoger_onderwijs_binnen_soort_hoger_onderwijs = "bachelor",
    leeftijd_per_peildatum_1_oktober = sprintf("%03d", rijen$leeftijd + rijen$jaar_index - 1L),
    postcodecijfers_student_op_1_oktober = rijen$postcode,
    postcodecijfers_van_de_hoogste_vooropl_voor_het_ho = rijen$postcode,
    opleidingscode_naam_opleiding = opleidingen$naam[rijen$opl],
    vestigingsnummer_gemeentenaam_volgens_rio = opleidingen$locatie[rijen$opl],
    hoogste_vooropleiding_voor_het_ho_omschrijving_vooropleiding =
      unname(SYNTH_VOOROPLEIDING[rijen$vooropleiding]),
    eerste_jaar_in_het_hoger_onderwijs = rijen$instroomjaar - 2L * rijen$eerder_in_ho
  ) |>
    structure(
      waarheid = dplyr::select(
        waarheid, persoonsgebonden_nummer, instroomjaar, uitval_na, diploma_na, gewisseld,
        vooropleiding, eerder_in_ho
      )
    )
}

## Voorbeeldomschrijvingen zoals 1cijferho ze levert, per hoofdcategorie
SYNTH_VOOROPLEIDING <- c(
  havo = "havo profiel economie & maatschappij",
  vwo = "vwo profiel natuur & gezondheid",
  mbo = "mbo economie niveau 4",
  buitenlands = "overig buitenlands diploma / Europees baccalaureaat",
  onbekend = "vooropleiding onbekend"
)

## Herhaal elke student voor jaar_index 1..duur, afgekapt op het laatste jaar
tidyr_uncount_jaren <- function(data, laatste_jaar) {
  duur <- pmin(data$duur, laatste_jaar - data$instroomjaar + 1L)
  idx <- rep(seq_len(nrow(data)), duur)
  uit <- data[idx, ]
  uit$jaar_index <- sequence(duur)
  uit
}

## Voer code uit met een tijdelijke seed en herstel daarna de RNG-staat
withr_seed <- function(seed, code) {
  oud <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  }
  on.exit(
    if (is.null(oud)) {
      rm(".Random.seed", envir = globalenv())
    } else {
      assign(".Random.seed", oud, envir = globalenv())
    },
    add = TRUE
  )
  set.seed(seed)
  code
}
