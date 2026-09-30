## End-to-end: synthetische data met bekende uitkomsten door de hele pipeline.
## Omdat per student bekend is wat er gebeurde, moeten de aantallen exact
## kloppen voor alle cohorten waarvan het meetvenster in de data zit.

synth <- maak_synthetische_1cho(n_per_jaar = 150L, jaren = 2012:2023, seed = 42L)
waarheid <- attr(synth, "waarheid")
L <- 2023L

pad <- tempfile(fileext = ".csv")
readr::write_delim(synth, pad, delim = ";", na = "")
basis <- maak_basisbestand(pad)

draai <- function(niveau) {
  cohort <- maak_instroom_cohort(basis, "hoger beroepsonderwijs", niveau = niveau)
  diploma <- maak_diploma_behaald(basis, niveau = niveau)
  rendement <- bereken_rendement(cohort, diploma, niveau = niveau, laatste_jaar = L)
  uitval <- bereken_uitval(basis, diploma, cohort, niveau = niveau)
  wissel <- if (niveau == "student") bereken_studiewissel(basis, cohort, diploma, uitval)
  combineer_indicatoren(cohort, rendement, uitval, wissel, niveau = niveau)
}
ind <- draai("student")

test_that("instroom per cohort is gelijk aan het aantal gesimuleerde studenten", {
  expect_equal(as.vector(table(ind$inschrijvingsjaar)), rep(150L, 12))
  ## persoonsgebonden_nummer behoudt voorloopnullen (#39)
  expect_true(all(nchar(ind$persoonsgebonden_nummer) == 9))
})

test_that("uitval binnen 1 jaar klopt exact voor waarneembare cohorten", {
  waarneembaar <- waarheid$instroomjaar + 1 <= L
  verwacht <- sum(waarheid$uitval_na[waarneembaar] == 1, na.rm = TRUE)
  expect_equal(sum(ind$uitval_1jr == "Uitgevallen binnen 1 jaar"), verwacht)
  expect_true(all(ind$uitval_1jr[ind$inschrijvingsjaar == L] == "Nog niet waarneembaar"))
})

test_that("uitval binnen 3 jaar klopt exact voor waarneembare cohorten", {
  waarneembaar <- waarheid$instroomjaar + 3 <= L
  verwacht <- sum(waarheid$uitval_na[waarneembaar] <= 3, na.rm = TRUE)
  expect_equal(sum(ind$uitval_3jr == "Uitgevallen binnen 3 jaar"), verwacht)
})

test_that("rendement binnen 5 jaar klopt exact voor waarneembare cohorten", {
  waarneembaar <- waarheid$instroomjaar + 5 - 1 <= L
  verwacht <- sum(waarheid$diploma_na[waarneembaar] <= 5, na.rm = TRUE)
  expect_equal(sum(ind$rendement_5jr == "Diploma binnen 5 jaar"), verwacht)
  expect_true(all(ind$rendement_5jr[ind$inschrijvingsjaar > L - 4] == "Nog niet waarneembaar"))
})

test_that("rendement binnen 8 jaar is alleen voor de oudste cohorten waarneembaar", {
  waarneembaar_ind <- ind$rendement_8jr != "Nog niet waarneembaar"
  expect_equal(sort(unique(ind$inschrijvingsjaar[waarneembaar_ind])), 2012:2016)
  ## Iedereen met een diploma haalt dat binnen 7 jaar
  verwacht <- sum(!is.na(waarheid$diploma_na[waarheid$instroomjaar <= 2016]))
  expect_equal(sum(ind$rendement_8jr == "Diploma binnen 8 jaar"), verwacht)
})

test_that("studiewissel binnen 1 jaar klopt exact", {
  waarneembaar <- waarheid$instroomjaar + 1 <= L
  expect_equal(
    sum(ind$studiewissel_1jr == "Gewisseld binnen 1 jaar"),
    sum(waarheid$gewisseld[waarneembaar])
  )
})

test_that("vooropleiding en eerstejaars HO komen overeen met de waarheid", {
  gekoppeld <- dplyr::left_join(
    ind,
    dplyr::rename(waarheid, vooropleiding_waar = vooropleiding),
    by = "persoonsgebonden_nummer"
  )
  expect_equal(as.character(gekoppeld$vooropleiding), gekoppeld$vooropleiding_waar)
  expect_equal(gekoppeld$eerstejaars_ho == "eerder in HO", gekoppeld$eerder_in_ho)
  expect_equal(attr(ind, "peildatum"), as.Date("2023-10-01"))
})

test_that("gemiddelde leeftijd staat in het benchmarkrapport (#39)", {
  rapport <- maak_benchmarkrapport(ind, drempel = 1L)
  expect_false(anyNA(rapport$gem_leeftijd_instroom))
  expect_equal(attr(rapport, "metadata")$niveau, "student")
})

test_that("op studentniveau tellen diploma's na een wissel mee bij de instroomopleiding (#45)", {
  gediplomeerde_wisselaars <- waarheid$gewisseld &
    !is.na(waarheid$diploma_na) &
    waarheid$instroomjaar + waarheid$diploma_na - 1 <= L
  expect_equal(
    sum(ind$diploma_in_instroomopleiding %in% FALSE),
    sum(gediplomeerde_wisselaars)
  )
})

test_that("op inschrijvingsniveau is een wissel uitval uit de oude opleiding (#45)", {
  ins <- draai("inschrijving")
  ## Wisselaars starten een tweede cohort in hun nieuwe opleiding
  nieuwe_cohorten <- sum(waarheid$gewisseld & waarheid$instroomjaar + 1 <= L)
  expect_equal(nrow(ins), nrow(waarheid) + nieuwe_cohorten)

  ## Uitval binnen 1 jaar = echte uitvallers + wisselaars (uit hun eerste opleiding)
  waarneembaar <- waarheid$instroomjaar + 1 <= L
  verwacht <- sum(waarheid$uitval_na[waarneembaar] == 1, na.rm = TRUE) +
    sum(waarheid$gewisseld[waarneembaar])
  expect_equal(sum(ins$uitval_1jr == "Uitgevallen binnen 1 jaar"), verwacht)
})

test_that("generator is reproduceerbaar en laat de RNG van de gebruiker ongemoeid", {
  set.seed(1)
  voor <- stats::runif(1)
  set.seed(1)
  a <- maak_synthetische_1cho(n_per_jaar = 5L, jaren = 2020:2021, seed = 7L)
  na <- stats::runif(1)
  b <- maak_synthetische_1cho(n_per_jaar = 5L, jaren = 2020:2021, seed = 7L)
  expect_identical(a, b)
  expect_identical(voor, na)
})
