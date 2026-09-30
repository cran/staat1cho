## staat1cho pseudonimiseert niet zelf. Deze tests bewaken dat een verkeerde
## combinatie van 1cijferho-uitvoer een duidelijke instructie oplevert in
## plaats van stil 0% koppeling.

PSEUDONIEM <- strrep("ab", 32)

vlpbek_regels <- c(
  "VLP|TEST|2025|20240115|||||||||||||||||||||",
  "BRD|700010001||TEST|1|J|pi|x|31001|HBO-BA|B|20230901|20240831|x|S|VT|x|x|ECONOMIE|BEKOSTIGD||||x|x"
)

test_that("is_gepseudonimiseerd herkent 1cijferho-pseudoniemen", {
  expect_true(is_gepseudonimiseerd(c(PSEUDONIEM, NA, "")))
  expect_false(is_gepseudonimiseerd(c("700010001", PSEUDONIEM)))
  expect_false(is_gepseudonimiseerd(c("012345678", "S12345")))
  expect_false(is_gepseudonimiseerd(character(0)))
})

test_that("gepseudonimiseerd 1CHO met VLPBEK geeft een fout met uitleg", {
  pad <- tempfile(fileext = ".csv")
  writeLines(vlpbek_regels, pad)
  indicatoren <- tibble::tibble(persoonsgebonden_nummer = PSEUDONIEM, opleidingscode = "31001")

  expect_error(
    verrijk_met_bekostiging(indicatoren, lees_bekostiging(pad)),
    "VLPBEK koppelt nog niet"
  )
})

test_that("1CHO met ander persoonsnummer koppelt niet en waarschuwt met uitleg", {
  pad <- tempfile(fileext = ".csv")
  writeLines(vlpbek_regels, pad)
  indicatoren <- tibble::tibble(persoonsgebonden_nummer = "S2023001", opleidingscode = "31001")

  expect_warning(
    result <- verrijk_met_bekostiging(indicatoren, lees_bekostiging(pad)),
    "VLPBEK koppelt nog niet"
  )
  expect_equal(attr(result, "koppeling")$pct, 0)
})

test_that("1CHO met BSN koppelt zonder waarschuwing", {
  pad <- tempfile(fileext = ".csv")
  writeLines(vlpbek_regels, pad)
  indicatoren <- tibble::tibble(persoonsgebonden_nummer = "700010001", opleidingscode = "31001")

  expect_no_warning(result <- suppressMessages(verrijk_met_bekostiging(indicatoren, lees_bekostiging(pad))))
  expect_true(result$indicatie_bekostigd)
})

test_that("gepseudonimiseerd 1CHO met niet-gepseudonimiseerd VAKHAVW geeft een fout", {
  indicatoren <- tibble::tibble(persoonsgebonden_nummer = PSEUDONIEM)
  vakhawv <- tibble::tibble(
    persoonsgebonden_nummer = "700010001", afkorting_vak = "ne",
    gemiddeld_cijfer_cijferlijst = 7, cijfer_eerste_centraal_examen = 7,
    cijfer_schoolexamen = 7
  )
  expect_error(verrijk_met_vakhawv(indicatoren, vakhawv), "dezelfde 1cijferho-uitvoer")
})
