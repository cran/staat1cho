test_that("lees_bekostiging verwerkt een VLPBEK-bestand correct", {
  pad <- system.file("extdata/voorbeeld_vlpbek.csv", package = "staat1cho")
  result <- lees_bekostiging(pad)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c(
    "persoonsgebonden_nummer", "opleidingscode", "inschrijvingsjaar",
    "indicatie_hoofdinschrijving", "opleidingsvorm", "sector", "bekostigingsstatus",
    "code_bekostigingstatus", "reden_niet_bekostigd", "indicatie_herstelbaar"
  ) %in% names(result)))
  expect_equal(nrow(result), 6L)
})

test_that("lees_bekostiging vult persoonsgebonden_nummer met het burgerservicenummer", {
  pad <- system.file("extdata/voorbeeld_vlpbek.csv", package = "staat1cho")
  result <- lees_bekostiging(pad)

  expect_equal(
    sort(result$persoonsgebonden_nummer),
    sort(as.character(100000001:100000006))
  )
})

test_that("lees_bekostiging decodeert code_bekostigingstatus naar reden_niet_bekostigd", {
  pad <- system.file("extdata/voorbeeld_vlpbek.csv", package = "staat1cho")
  result <- lees_bekostiging(pad)

  niet_bekostigd <- result[result$bekostigingsstatus == "niet bekostigd", ]
  expect_equal(niet_bekostigd$code_bekostigingstatus, c("nf", "ti"))
  expect_true(all(grepl("overschrijden maximaal aantal bekostigde inschrijvingen", niet_bekostigd$reden_niet_bekostigd[1])))
  expect_true(all(grepl("niet tijdig aangeleverd", niet_bekostigd$reden_niet_bekostigd[2])))

  bekostigd <- result[result$bekostigingsstatus == "bekostigd", ]
  expect_true(all(is.na(bekostigd$reden_niet_bekostigd)))
})

test_that("lees_bekostiging zet indicatie_herstelbaar op basis van CodeBekostigingstatus", {
  pad <- system.file("extdata/voorbeeld_vlpbek.csv", package = "staat1cho")
  result <- lees_bekostiging(pad)

  niet_bekostigd <- result[result$bekostigingsstatus == "niet bekostigd", ]
  expect_equal(niet_bekostigd$indicatie_herstelbaar, c(FALSE, TRUE))

  bekostigd <- result[result$bekostigingsstatus == "bekostigd", ]
  expect_true(all(is.na(bekostigd$indicatie_herstelbaar)))
})

test_that("lees_bekostiging leest het peiljaar uit de VLP-koptekstregel", {
  pad <- system.file("extdata/voorbeeld_vlpbek.csv", package = "staat1cho")
  result <- lees_bekostiging(pad)
  expect_equal(attr(result, "peiljaar"), 2025L)
})

test_that("lees_bekostiging converteert indicatie_hoofdinschrijving naar logical", {
  pad <- system.file("extdata/voorbeeld_vlpbek.csv", package = "staat1cho")
  result <- lees_bekostiging(pad)
  expect_type(result$indicatie_hoofdinschrijving, "logical")
  expect_true(all(result$indicatie_hoofdinschrijving == TRUE))
})

test_that("lees_bekostiging converteert bekostigingsstatus correct", {
  pad <- system.file("extdata/voorbeeld_vlpbek.csv", package = "staat1cho")
  result <- lees_bekostiging(pad)

  expect_true(all(result$bekostigingsstatus %in% c("bekostigd", "niet bekostigd")))
  expect_equal(sum(result$bekostigingsstatus == "bekostigd"), 4L)
  expect_equal(sum(result$bekostigingsstatus == "niet bekostigd"), 2L)
})

test_that("lees_bekostiging leidt inschrijvingsjaar af uit begindatum", {
  pad <- system.file("extdata/voorbeeld_vlpbek.csv", package = "staat1cho")
  result <- lees_bekostiging(pad)

  expect_type(result$inschrijvingsjaar, "integer")
  expect_true(all(result$inschrijvingsjaar %in% c(2022L, 2023L)))
})

test_that("lees_bekostiging normaliseert opleidingsvorm", {
  pad <- system.file("extdata/voorbeeld_vlpbek.csv", package = "staat1cho")
  result <- lees_bekostiging(pad)

  expect_true(all(result$opleidingsvorm %in% c("voltijd", "deeltijd")))
})

test_that("lees_bekostiging normaliseert sector naar lowercase", {
  pad <- system.file("extdata/voorbeeld_vlpbek.csv", package = "staat1cho")
  result <- lees_bekostiging(pad)

  expect_false(any(grepl("_", result$sector, fixed = TRUE)))
  expect_false(any(grepl("[A-Z]", result$sector)))
})

test_that("lees_bekostiging geeft foutmelding bij ontbrekende BRD-regels", {
  pad_leeg <- tempfile(fileext = ".csv")
  writeLines(c("VLP|TEST|2025|20240115|||||||||||||||||||||"), pad_leeg)
  expect_error(lees_bekostiging(pad_leeg), "Geen BRD-regels")
  unlink(pad_leeg)
})

test_that("lees_bekostiging converteert persoonsgebonden_nummer naar character", {
  pad <- system.file("extdata/voorbeeld_vlpbek.csv", package = "staat1cho")
  result <- lees_bekostiging(pad)

  expect_type(result$persoonsgebonden_nummer, "character")
})

## verrijk_met_bekostiging ----

bekostiging_fixture <- tibble::tibble(
  persoonsgebonden_nummer    = c("1", "2", "3"),
  opleidingscode             = c("31001", "31002", "31001"),
  inschrijvingsjaar          = c(2023L, 2023L, 2022L),
  indicatie_hoofdinschrijving = c(TRUE, TRUE, FALSE),
  opleidingsvorm             = c("voltijd", "deeltijd", "voltijd"),
  sector                     = c("gezondheidszorg", "economie", "gezondheidszorg"),
  bekostigingsstatus         = c("bekostigd", "niet bekostigd", "bekostigd"),
  code_bekostigingstatus     = c(NA, "nf", NA),
  reden_niet_bekostigd       = c(NA, "Niet bekostigd i.v.m. overschrijden maximaal aantal bekostigde inschrijvingen, rekening houdend met eerder behaalde graden.", NA),
  indicatie_herstelbaar      = c(NA, FALSE, NA)
)

test_that("verrijk_met_bekostiging voegt indicatie_bekostigd en indicatie_hoofdinschrijving toe", {
  indicatoren <- tibble::tibble(
    persoonsgebonden_nummer = c("1", "2", "3", "4"),
    opleidingscode          = c("31001", "31002", "31001", "31003"),
    inschrijvingsjaar       = c(2023L, 2023L, 2022L, 2023L)
  )

  result <- verrijk_met_bekostiging(indicatoren, bekostiging_fixture)

  expect_true("indicatie_bekostigd" %in% names(result))
  expect_true("indicatie_hoofdinschrijving" %in% names(result))
  expect_equal(nrow(result), 4L)
})

test_that("verrijk_met_bekostiging koppelt bekostiging correct", {
  indicatoren <- tibble::tibble(
    persoonsgebonden_nummer = c("1", "2", "3"),
    opleidingscode          = c("31001", "31002", "31001")
  )

  result <- verrijk_met_bekostiging(indicatoren, bekostiging_fixture)

  expect_true(result$indicatie_bekostigd[result$persoonsgebonden_nummer == "1"])
  expect_false(result$indicatie_bekostigd[result$persoonsgebonden_nummer == "2"])
})

test_that("verrijk_met_bekostiging voegt reden_niet_bekostigd en indicatie_herstelbaar toe", {
  indicatoren <- tibble::tibble(
    persoonsgebonden_nummer = c("1", "2", "3"),
    opleidingscode          = c("31001", "31002", "31001")
  )

  result <- verrijk_met_bekostiging(indicatoren, bekostiging_fixture)

  expect_true(is.na(result$reden_niet_bekostigd[result$persoonsgebonden_nummer == "1"]))
  expect_match(
    result$reden_niet_bekostigd[result$persoonsgebonden_nummer == "2"],
    "overschrijden maximaal aantal bekostigde inschrijvingen"
  )
  expect_false(result$indicatie_herstelbaar[result$persoonsgebonden_nummer == "2"])
  expect_true(is.na(result$indicatie_herstelbaar[result$persoonsgebonden_nummer == "1"]))
})

test_that("verrijk_met_bekostiging geeft NA voor studenten zonder match", {
  indicatoren <- tibble::tibble(
    persoonsgebonden_nummer = c("1", "99"),
    opleidingscode          = c("31001", "99999")
  )

  result <- verrijk_met_bekostiging(indicatoren, bekostiging_fixture)

  expect_true(is.na(result$indicatie_bekostigd[result$persoonsgebonden_nummer == "99"]))
})

test_that("verrijk_met_bekostiging: bekostigd wint bij meerdere rijen per pgn+opleiding", {
  bek_dubbel <- tibble::tibble(
    persoonsgebonden_nummer    = c("1", "1"),
    opleidingscode             = c("31001", "31001"),
    inschrijvingsjaar          = c(2023L, 2023L),
    indicatie_hoofdinschrijving = c(FALSE, TRUE),
    opleidingsvorm             = c("voltijd", "voltijd"),
    sector                     = c("gezondheidszorg", "gezondheidszorg"),
    bekostigingsstatus         = c("niet bekostigd", "bekostigd"),
    code_bekostigingstatus     = c("ti", NA),
    reden_niet_bekostigd       = c("De inschrijving is niet tijdig aangeleverd.", NA),
    indicatie_herstelbaar      = c(TRUE, NA)
  )

  indicatoren <- tibble::tibble(
    persoonsgebonden_nummer = "1",
    opleidingscode          = "31001"
  )

  result <- verrijk_met_bekostiging(indicatoren, bek_dubbel)
  expect_true(result$indicatie_bekostigd)
})

test_that("verrijk_met_bekostiging accepteert numeriek persoonsgebonden_nummer", {
  indicatoren <- tibble::tibble(
    persoonsgebonden_nummer = c(1, 2),
    opleidingscode          = c("31001", "31002")
  )

  result <- verrijk_met_bekostiging(indicatoren, bekostiging_fixture)
  expect_equal(nrow(result), 2L)
  expect_true("indicatie_bekostigd" %in% names(result))
})

test_that("verrijk_met_bekostiging geeft foutmelding bij ontbrekende sleutelkolom", {
  indicatoren_zonder_opl <- tibble::tibble(
    persoonsgebonden_nummer = "1"
  )

  expect_error(
    verrijk_met_bekostiging(indicatoren_zonder_opl, bekostiging_fixture),
    "opleidingscode"
  )
})

## BEKOSTIGINGSTATUS_CODES ----

test_that("BEKOSTIGINGSTATUS_CODES bevat alle DUO-redencodes zonder duplicaten", {
  expect_s3_class(BEKOSTIGINGSTATUS_CODES, "tbl_df")
  expect_true(all(c("code", "omschrijving", "herstelbaar") %in% names(BEKOSTIGINGSTATUS_CODES)))
  expect_equal(anyDuplicated(BEKOSTIGINGSTATUS_CODES$code), 0L)
  expect_true(all(nchar(BEKOSTIGINGSTATUS_CODES$code) == 2L))
})

test_that("lees_bekostiging voegt meerdere komma-gescheiden codes samen", {
  pad_dubbel <- tempfile(fileext = ".csv")
  writeLines(c(
    "VLP|TEST|2025|20240115|||||||||||||||||||||",
    "BRD|100000009||TEST|1|N|na,ti|x|31001|HBO-BA|B|20230901|20240831|x|S|VT|x|x|GEZONDHEIDSZORG|||||x|x",
    "SLR|1|1|0|||||||||||||||||||||"
  ), pad_dubbel)

  result <- lees_bekostiging(pad_dubbel)

  expect_equal(result$code_bekostigingstatus, "na,ti")
  expect_match(result$reden_niet_bekostigd, "woonplaatsvereiste")
  expect_match(result$reden_niet_bekostigd, "niet tijdig aangeleverd")
  ## na (woonplaatsvereiste) is niet herstelbaar, ook al is ti dat wel
  expect_false(result$indicatie_herstelbaar)

  unlink(pad_dubbel)
})

## --- robuustheid VLPBEK (#42) ---

schrijf_vlpbek <- function(brd) {
  pad <- tempfile(fileext = ".csv")
  writeLines(c("VLP|TEST|2025|20240115|||||||||||||||||||||", brd), pad)
  pad
}

test_that("lege BSN valt terug op het onderwijsnummer in plaats van een lege sleutel", {
  pad <- schrijf_vlpbek(c(
    "BRD||800000001|TEST|1|J|pi|x|31001|HBO-BA|B|20230901|20240831|x|S|VT|x|x|ECONOMIE|BEKOSTIGD||||x|x",
    "BRD||800000002|TEST|2|J|pi|x|31001|HBO-BA|B|20230901|20240831|x|S|VT|x|x|ECONOMIE|BEKOSTIGD||||x|x"
  ))
  result <- lees_bekostiging(pad)
  expect_equal(result$persoonsgebonden_nummer, c("800000001", "800000002"))
})

test_that("geeft fout bij meer dan 25 velden per BRD-regel", {
  pad <- schrijf_vlpbek(
    "BRD|1||TEST|1|J|pi|x|31001|HBO-BA|B|20230901|20240831|x|S|VT|x|x|ECONOMIE|BEKOSTIGD||||x|x|extra"
  )
  expect_error(lees_bekostiging(pad), "meer dan 25 velden")
})

test_that("geeft fout als de kolomindeling verschoven is", {
  pad <- schrijf_vlpbek(
    "BRD|1||TEST|1|J|pi|x|31001|20230901|20240831|x|S|VT|x|x|ECONOMIE|BEKOSTIGD||||x|x"
  )
  expect_error(lees_bekostiging(pad), "begindatum")
})

test_that("redencode pd geeft status deels bekostigd en telt als bekostigd", {
  pad <- schrijf_vlpbek(
    "BRD|1||TEST|1|J|pd|x|31001|HBO-BA|B|20230901|20240831|x|S|VT|x|x|ECONOMIE|||||x|x"
  )
  bek <- lees_bekostiging(pad)
  expect_equal(bek$bekostigingsstatus, "deels bekostigd")

  ind <- tibble::tibble(persoonsgebonden_nummer = "1", opleidingscode = "31001")
  result <- suppressMessages(verrijk_met_bekostiging(ind, bek))
  expect_true(result$indicatie_bekostigd)
  expect_equal(result$bekostiging_jaar, 2023L)
})

test_that("waarschuwt bij minder dan 50% gekoppeld en legt koppeling vast", {
  bek <- tibble::tibble(
    persoonsgebonden_nummer = "1", opleidingscode = "31001", inschrijvingsjaar = 2023L,
    indicatie_hoofdinschrijving = TRUE, opleidingsvorm = "voltijd", sector = "economie",
    bekostigingsstatus = "bekostigd", code_bekostigingstatus = "pi",
    reden_niet_bekostigd = NA_character_, indicatie_herstelbaar = NA
  )
  ## Drie VLPBEK-inschrijvingen, waarvan er maar een in 1CHO voorkomt
  bek <- dplyr::bind_rows(bek, dplyr::mutate(bek, persoonsgebonden_nummer = "8"),
                          dplyr::mutate(bek, persoonsgebonden_nummer = "9"))
  ind <- tibble::tibble(persoonsgebonden_nummer = c("1", "2", "3"), opleidingscode = "31001")

  expect_warning(result <- verrijk_met_bekostiging(ind, bek), "1 van 3 inschrijvingen")
  expect_equal(attr(result, "koppeling")$pct, 33)
  expect_equal(attr(result, "koppeling")$n_verrijkt, 1L)
})

test_that("waarschuwt niet als VLPBEK volledig terugkomt, ook al beslaat het weinig cohortrijen", {
  bek <- tibble::tibble(
    persoonsgebonden_nummer = "1", opleidingscode = "31001", inschrijvingsjaar = 2023L,
    indicatie_hoofdinschrijving = TRUE, opleidingsvorm = "voltijd", sector = "economie",
    bekostigingsstatus = "bekostigd", code_bekostigingstatus = "pi",
    reden_niet_bekostigd = NA_character_, indicatie_herstelbaar = NA
  )
  ## VLPBEK beslaat één jaar: van de tien cohortrijen kan er maar een koppelen
  ind <- tibble::tibble(persoonsgebonden_nummer = as.character(1:10), opleidingscode = "31001")
  expect_no_warning(result <- suppressMessages(verrijk_met_bekostiging(ind, bek)))
  expect_equal(attr(result, "koppeling")$pct, 100)
})
