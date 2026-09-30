## --- lees_vakhawv ---

test_that("leest het voorbeeldbestand zonder fouten in", {
  pad <- system.file("extdata/voorbeeld_vakhawv.csv", package = "staat1cho")
  result <- lees_vakhawv(pad)

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
})

test_that("converteert DUO-cijfers (x10-schaal) naar decimale waarden", {
  pad <- system.file("extdata/voorbeeld_vakhawv.csv", package = "staat1cho")
  result <- lees_vakhawv(pad)

  ## Ruw: 72, na conversie: 7.2
  expect_true(all(result$gemiddeld_cijfer_cijferlijst <= 10, na.rm = TRUE))
  expect_true(all(result$cijfer_eerste_centraal_examen <= 10, na.rm = TRUE))
  expect_true(all(result$cijfer_schoolexamen <= 10, na.rm = TRUE))
})

test_that("converteert afkorting_vak naar kleine letters", {
  pad <- system.file("extdata/voorbeeld_vakhawv.csv", package = "staat1cho")
  result <- lees_vakhawv(pad)

  expect_equal(result$afkorting_vak, tolower(result$afkorting_vak))
})

test_that("geeft fout bij ontbrekende verplichte kolommen", {
  onvolledig <- tibble(
    persoonsgebonden_nummer = "A",
    afkorting_vak = "ne"
    ## ontbrekend: gemiddeld_cijfer_cijferlijst, etc.
  )
  pad <- tempfile(fileext = ".csv")
  readr::write_csv2(onvolledig, pad)

  expect_error(lees_vakhawv(pad), "Ontbrekende kolommen")
})

test_that("bevat de vijf verplichte kolommen na inlezen", {
  pad <- system.file("extdata/voorbeeld_vakhawv.csv", package = "staat1cho")
  result <- lees_vakhawv(pad)

  vereist <- c(
    "persoonsgebonden_nummer",
    "afkorting_vak",
    "gemiddeld_cijfer_cijferlijst",
    "cijfer_eerste_centraal_examen",
    "cijfer_schoolexamen"
  )
  expect_true(all(vereist %in% names(result)))
})

## --- verrijk_met_vakhawv ---

maak_vakhawv_input <- function() {
  indicatoren <- tibble(
    persoonsgebonden_nummer = c("A", "B", "C"),
    inschrijvingsjaar = 2020L
  )

  ## A heeft wiskunde, B heeft geen wiskunde, C zit niet in VAKHAVW
  vakhawv <- tibble(
    persoonsgebonden_nummer = c("A", "A", "B"),
    afkorting_vak = c("ne", "wis", "ne"),
    gemiddeld_cijfer_cijferlijst = c(7.2, 7.2, 6.5),
    cijfer_eerste_centraal_examen = c(7.0, 6.5, 6.2),
    cijfer_schoolexamen = c(7.5, 6.8, 6.8)
  )

  list(indicatoren = indicatoren, vakhawv = vakhawv)
}

test_that("voegt drie vakhawv-kolommen toe aan indicatoren", {
  inp <- maak_vakhawv_input()
  result <- verrijk_met_vakhawv(inp$indicatoren, inp$vakhawv)

  expect_true("vakhawv_gemiddeld_eindcijfer" %in% names(result))
  expect_true("vakhawv_wiskundecijfer" %in% names(result))
  expect_true("vakhawv_aantal_vakken" %in% names(result))
})

test_that("behoudt alle rijen van indicatoren na de join", {
  inp <- maak_vakhawv_input()
  result <- verrijk_met_vakhawv(inp$indicatoren, inp$vakhawv)

  expect_equal(nrow(result), nrow(inp$indicatoren))
})

test_that("geeft NA voor studenten die niet in VAKHAVW staan", {
  inp <- maak_vakhawv_input()
  result <- verrijk_met_vakhawv(inp$indicatoren, inp$vakhawv)

  rij_c <- result[result$persoonsgebonden_nummer == "C", ]
  expect_true(is.na(rij_c$vakhawv_gemiddeld_eindcijfer))
  expect_true(is.na(rij_c$vakhawv_wiskundecijfer))
})

test_that("berekent vakhawv_wiskundecijfer correct voor student met wiskunde", {
  inp <- maak_vakhawv_input()
  result <- verrijk_met_vakhawv(inp$indicatoren, inp$vakhawv)

  rij_a <- result[result$persoonsgebonden_nummer == "A", ]
  expect_equal(rij_a$vakhawv_wiskundecijfer, 6.5)
})

test_that("geeft NA voor vakhawv_wiskundecijfer als student geen wiskunde had", {
  inp <- maak_vakhawv_input()
  result <- verrijk_met_vakhawv(inp$indicatoren, inp$vakhawv)

  rij_b <- result[result$persoonsgebonden_nummer == "B", ]
  expect_true(is.na(rij_b$vakhawv_wiskundecijfer))
})

test_that("telt het juiste aantal unieke vakken per student", {
  inp <- maak_vakhawv_input()
  result <- verrijk_met_vakhawv(inp$indicatoren, inp$vakhawv)

  rij_a <- result[result$persoonsgebonden_nummer == "A", ]
  expect_equal(rij_a$vakhawv_aantal_vakken, 2L)

  rij_b <- result[result$persoonsgebonden_nummer == "B", ]
  expect_equal(rij_b$vakhawv_aantal_vakken, 1L)
})

test_that("geeft fout als persoonsgebonden_nummer ontbreekt in indicatoren", {
  indicatoren_zonder_pgn <- tibble(inschrijvingsjaar = 2020L)
  vakhawv <- tibble(
    persoonsgebonden_nummer = "A",
    afkorting_vak = "ne",
    gemiddeld_cijfer_cijferlijst = 7.0,
    cijfer_eerste_centraal_examen = 6.8,
    cijfer_schoolexamen = 7.2
  )

  expect_error(
    verrijk_met_vakhawv(indicatoren_zonder_pgn, vakhawv),
    "persoonsgebonden_nummer"
  )
})

test_that("werkt op inschrijvingsniveau: herhaalt vakhawv-data per opleiding", {
  ## Zelfde student, twee opleidingen (inschrijvingsniveau)
  indicatoren <- tibble(
    persoonsgebonden_nummer = c("A", "A"),
    opleiding_actueel_equivalent = c("34401", "34402"),
    inschrijvingsjaar = 2020L
  )
  vakhawv <- tibble(
    persoonsgebonden_nummer = "A",
    afkorting_vak = "ne",
    gemiddeld_cijfer_cijferlijst = 7.2,
    cijfer_eerste_centraal_examen = 7.0,
    cijfer_schoolexamen = 7.5
  )

  result <- verrijk_met_vakhawv(indicatoren, vakhawv)

  expect_equal(nrow(result), 2)
  expect_equal(result$vakhawv_gemiddeld_eindcijfer, c(7.2, 7.2))
})

test_that("koppelt ondanks andere aanvulling van het DUO-nummer in EV en VAKHAVW", {
  ## EV: spaties (na 1cijferho "2"), VAKHAVW: nullen ("000000000002")
  indicatoren <- tibble(
    persoonsgebonden_nummer = c("2", "     7", "10"),
    inschrijvingsjaar = 2020L
  )
  vakhawv <- tibble(
    persoonsgebonden_nummer = c("000000000002", "000000000007", "000000000099"),
    afkorting_vak = "ne",
    gemiddeld_cijfer_cijferlijst = c(7.2, 6.5, 8.0),
    cijfer_eerste_centraal_examen = 7.0,
    cijfer_schoolexamen = 7.5
  )

  result <- suppressWarnings(verrijk_met_vakhawv(indicatoren, vakhawv))

  expect_equal(result$vakhawv_gemiddeld_eindcijfer, c(7.2, 6.5, NA))
  ## De nummers in het analysebestand blijven zoals ze waren
  expect_equal(result$persoonsgebonden_nummer, c("2", "     7", "10"))
  expect_equal(attr(result, "koppeling")$gekoppeld, 2L)
})

test_that("koppelsleutel verwijdert spaties en voorloopnullen, maar laat 0 staan", {
  expect_equal(koppelsleutel(c("000000000002", "     2", "2", "0", "000")), c("2", "2", "2", "0", "0"))
})
