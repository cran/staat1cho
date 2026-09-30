## Vooropleiding en eerstejaars HO ----

test_that("categoriseer_vooropleiding vat DUO-omschrijvingen samen", {
  omschrijving <- c(
    "havo profiel economie & maatschappij",
    "vwo profiel natuur & gezondheid",
    "mbo techniek niveau 4",
    "hbo-p algemeen",
    "overig buitenlands diploma / Europees baccalaureaat",
    "vmbo gemengde leerweg",
    "overig toelatingsexamen",
    "vooropleiding onbekend",
    NA,
    ""
  )
  expect_equal(
    as.character(categoriseer_vooropleiding(omschrijving)),
    c("havo", "vwo", "mbo", "ho", "buitenlands", "overig", "overig",
      "onbekend", "onbekend", "onbekend")
  )
  expect_equal(levels(categoriseer_vooropleiding("havo algemeen")), VOOROPLEIDING_NIVEAUS)
})

test_that("bepaal_eerstejaars_ho vergelijkt eerste jaar HO met het instroomjaar", {
  expect_equal(
    as.character(bepaal_eerstejaars_ho(
      c(2020L, 2018L, NA, 2022L),
      c(2020L, 2020L, 2020L, 2020L)
    )),
    c("eerstejaars HO", "eerder in HO", "onbekend", "onbekend")
  )
})

## Via combineer_indicatoren ----

maak_minimale_input <- function(cohort_extra = list()) {
  cohort <- tibble::tibble(
    persoonsgebonden_nummer = c("A", "B"),
    inschrijvingsjaar = 2020L,
    eerstejaar_instelling = 2020L,
    geslacht_label = "vrouw",
    locatie_label = "Breda",
    opleiding_actueel_equivalent = "12345",
    opleidingsvorm_label = "voltijd",
    type_hoger_onderwijs_binnen_soort_hoger_onderwijs = "ba",
    indicatie_internationale_student_label = "geen internationale student",
    indicatie_eer_actueel_label = "eer",
    croho_onderdeel_actuele_opleiding_label = "techniek",
    leeftijd_per_peildatum_1_oktober = 19L,
    postcodecijfers_student_op_1_oktober = "4818",
    postcodecijfers_van_de_hoogste_vooropl_voor_het_ho = "4818",
    soort_diploma_instelling_label = NA_character_
  )
  for (kol in names(cohort_extra)) {
    cohort[[kol]] <- cohort_extra[[kol]]
  }
  rendement <- tibble::tibble(
    persoonsgebonden_nummer = c("A", "B"),
    eerstejaar_instelling = 2020L,
    jaar_eerste_diploma = NA_real_,
    verblijfsjaar_eerste_diploma = NA_integer_,
    diploma = NA_character_,
    rendement_xjaar = NA_real_,
    rendement_3jr = factor("Geen diploma"),
    rendement_5jr = factor("Geen diploma"),
    rendement_8jr = factor("Geen diploma")
  )
  uitval <- tibble::tibble(
    persoonsgebonden_nummer = c("A", "B"),
    laatste_jaar_inschrijving = NA_real_,
    diploma = NA_character_,
    status = factor("Zittend"),
    uitval_xjr = NA_real_,
    uitval_1jr = factor("Na 1 jaar nog ingeschreven of diploma behaald"),
    uitval_3jr = factor("Na 3 jaar nog ingeschreven of diploma behaald")
  )
  list(cohort = cohort, rendement = rendement, uitval = uitval)
}

test_that("combineer_indicatoren voegt vooropleiding en eerstejaars_ho toe", {
  inp <- maak_minimale_input(list(
    hoogste_vooropleiding_voor_het_ho_omschrijving_vooropleiding =
      c("havo algemeen", "mbo economie niveau 4"),
    eerste_jaar_in_het_hoger_onderwijs = c(2020L, 2019L)
  ))
  res <- suppressWarnings(combineer_indicatoren(inp$cohort, inp$rendement, inp$uitval))
  expect_equal(as.character(res$vooropleiding), c("havo", "mbo"))
  expect_equal(as.character(res$eerstejaars_ho), c("eerstejaars HO", "eerder in HO"))
})

test_that("zonder de 1CHO-kolommen zijn vooropleiding en eerstejaars_ho onbekend", {
  inp <- maak_minimale_input()
  res <- suppressWarnings(combineer_indicatoren(inp$cohort, inp$rendement, inp$uitval))
  expect_true(all(res$vooropleiding == "onbekend"))
  expect_true(all(res$eerstejaars_ho == "onbekend"))
})

test_that("peildatum komt uit het laatste jaar van bereken_uitval()", {
  inp <- maak_minimale_input()
  attr(inp$uitval, "laatste_jaar") <- 2024L
  res <- suppressWarnings(combineer_indicatoren(inp$cohort, inp$rendement, inp$uitval))
  expect_equal(attr(res, "peildatum"), as.Date("2024-10-01"))

  ## Zonder attribuut: laatste instroomjaar
  inp <- maak_minimale_input()
  res <- suppressWarnings(combineer_indicatoren(inp$cohort, inp$rendement, inp$uitval))
  expect_equal(attr(res, "peildatum"), as.Date("2020-10-01"))
})
