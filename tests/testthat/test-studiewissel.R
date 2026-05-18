maak_wissel_rijen <- function(
  pgn,
  verblijfsjaren,
  opleidingen,
  inschrijvingsjaren
) {
  tibble(
    persoonsgebonden_nummer = pgn,
    verblijfsjaar_actuele_instelling = as.integer(verblijfsjaren),
    soort_inschrijving_actuele_instelling_label = HOOFD_INSCHRIJVING,
    opleiding_actueel_equivalent = opleidingen,
    inschrijvingsjaar = inschrijvingsjaren,
    opleidingsvorm = "voltijd",
    type_hoger_onderwijs_binnen_soort_hoger_onderwijs = "bachelor",
    croho_onderdeel_actuele_opleiding = "techniek"
  )
}

## --- bereken_wissel_xjr: wissels ---

test_that("detecteert opleidingswissel correct bij 1jr", {
  basisbestand <- bind_rows(
    maak_wissel_rijen(c("A", "A"), c(1, 2), c("opl1", "opl2"), c(2019, 2020)),
    maak_wissel_rijen(c("B", "B"), c(1, 2), c("opl1", "opl1"), c(2019, 2020))
  )
  zittend <- tibble(persoonsgebonden_nummer = c("A", "B"))

  result <- bereken_wissel_xjr(
    basisbestand,
    zittend,
    verblijfsjaren = c(1, 2),
    doeljaar = 2,
    label_gewisseld = "Gewisseld binnen 1 jaar",
    label_niet = "Niet gewisseld binnen 1 jaar",
    suffix = "1jr"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$persoonsgebonden_nummer, "A")
  expect_equal(as.character(result$studiewissel_1jr), "Gewisseld binnen 1 jaar")
})

test_that("detecteert opleidingswissel correct bij 3jr", {
  basisbestand <- bind_rows(
    maak_wissel_rijen(c("A", "A"), c(1, 4), c("opl1", "opl2"), c(2015, 2018)),
    maak_wissel_rijen(c("B", "B"), c(1, 4), c("opl1", "opl1"), c(2015, 2018))
  )
  zittend <- tibble(persoonsgebonden_nummer = c("A", "B"))

  result <- bereken_wissel_xjr(
    basisbestand,
    zittend,
    verblijfsjaren = c(1, 4),
    doeljaar = 4,
    label_gewisseld = "Gewisseld binnen 3 jaar",
    label_niet = "Niet gewisseld binnen 3 jaar",
    suffix = "3jr"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$persoonsgebonden_nummer, "A")
  expect_equal(as.character(result$studiewissel_3jr), "Gewisseld binnen 3 jaar")
})

test_that("geeft lege tibble terug als niemand gewisseld is", {
  basisbestand <- bind_rows(
    maak_wissel_rijen(c("A", "A"), c(1, 2), c("opl1", "opl1"), c(2019, 2020)),
    maak_wissel_rijen(c("B", "B"), c(1, 2), c("opl2", "opl2"), c(2019, 2020))
  )
  zittend <- tibble(persoonsgebonden_nummer = c("A", "B"))

  result <- bereken_wissel_xjr(
    basisbestand,
    zittend,
    verblijfsjaren = c(1, 2),
    doeljaar = 2,
    label_gewisseld = "Gewisseld binnen 1 jaar",
    label_niet = "Niet gewisseld binnen 1 jaar",
    suffix = "1jr"
  )

  expect_equal(nrow(result), 0)
})

## --- bereken_wissel_xjr: populatiefilter ---

test_that("negeert studenten die niet in zittend zitten", {
  basisbestand <- bind_rows(
    maak_wissel_rijen(c("A", "A"), c(1, 2), c("opl1", "opl2"), c(2019, 2020)),
    maak_wissel_rijen(c("B", "B"), c(1, 2), c("opl1", "opl2"), c(2019, 2020))
  )
  zittend <- tibble(persoonsgebonden_nummer = "A")

  result <- bereken_wissel_xjr(
    basisbestand,
    zittend,
    verblijfsjaren = c(1, 2),
    doeljaar = 2,
    label_gewisseld = "Gewisseld binnen 1 jaar",
    label_niet = "Niet gewisseld binnen 1 jaar",
    suffix = "1jr"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$persoonsgebonden_nummer, "A")
})

test_that("geeft lege tibble als zittend leeg is", {
  basisbestand <- maak_wissel_rijen(
    c("A", "A"),
    c(1, 2),
    c("opl1", "opl2"),
    c(2019, 2020)
  )
  zittend <- tibble(persoonsgebonden_nummer = character(0))

  result <- bereken_wissel_xjr(
    basisbestand,
    zittend,
    verblijfsjaren = c(1, 2),
    doeljaar = 2,
    label_gewisseld = "Gewisseld binnen 1 jaar",
    label_niet = "Niet gewisseld binnen 1 jaar",
    suffix = "1jr"
  )

  expect_equal(nrow(result), 0)
})

## --- bereken_wissel_xjr: kalenderjaarfilter ---

test_that("sluit studenten uit met verkeerd verschil in kalenderjaren", {
  ## Verschil is 2 jaar terwijl doeljaar - 1 = 1 verwacht
  basisbestand <- maak_wissel_rijen(
    c("A", "A"),
    c(1, 2),
    c("opl1", "opl2"),
    c(2018, 2020)
  )
  zittend <- tibble(persoonsgebonden_nummer = "A")

  result <- bereken_wissel_xjr(
    basisbestand,
    zittend,
    verblijfsjaren = c(1, 2),
    doeljaar = 2,
    label_gewisseld = "Gewisseld binnen 1 jaar",
    label_niet = "Niet gewisseld binnen 1 jaar",
    suffix = "1jr"
  )

  expect_equal(nrow(result), 0)
})

## --- bereken_wissel_xjr: uitvoerkolommen ---

test_that("bevat de juiste switchkolommen in de output", {
  basisbestand <- maak_wissel_rijen(
    c("A", "A"),
    c(1, 2),
    c("opl1", "opl2"),
    c(2019, 2020)
  )
  zittend <- tibble(persoonsgebonden_nummer = "A")

  result <- bereken_wissel_xjr(
    basisbestand,
    zittend,
    verblijfsjaren = c(1, 2),
    doeljaar = 2,
    label_gewisseld = "Gewisseld binnen 1 jaar",
    label_niet = "Niet gewisseld binnen 1 jaar",
    suffix = "1jr"
  )

  expect_true("studiewissel_1jr" %in% names(result))
  expect_true("opleidingscode_na_switch1jr" %in% names(result))
  expect_true("opleidingsvorm_na_switch1jr" %in% names(result))
  expect_true("opleidingsniveau_na_switch1jr" %in% names(result))
  expect_true("sector_na_switch1jr" %in% names(result))
})

test_that("vult opleidingscode_na_switch in bij wissel", {
  basisbestand <- maak_wissel_rijen(
    c("A", "A"),
    c(1, 2),
    c("opl1", "opl2"),
    c(2019, 2020)
  )
  zittend <- tibble(persoonsgebonden_nummer = "A")

  result <- bereken_wissel_xjr(
    basisbestand,
    zittend,
    verblijfsjaren = c(1, 2),
    doeljaar = 2,
    label_gewisseld = "Gewisseld binnen 1 jaar",
    label_niet = "Niet gewisseld binnen 1 jaar",
    suffix = "1jr"
  )

  expect_equal(as.character(result$opleidingscode_na_switch1jr), "opl2")
})

## --- bereken_wissel_xjr: foutcondities ---

test_that("geeft fout bij meer dan 2 rijen per student", {
  basisbestand <- maak_wissel_rijen(
    c("A", "A", "A"),
    c(1, 1, 2),
    c("opl1", "opl1", "opl2"),
    c(2019, 2019, 2020)
  )
  zittend <- tibble(persoonsgebonden_nummer = "A")

  expect_error(bereken_wissel_xjr(
    basisbestand,
    zittend,
    verblijfsjaren = c(1, 2),
    doeljaar = 2,
    label_gewisseld = "Gewisseld binnen 1 jaar",
    label_niet = "Niet gewisseld binnen 1 jaar",
    suffix = "1jr"
  ))
})

test_that("geeft fout bij ongelijk aantal verblijfsjaar 1 en 2 rijen", {
  ## Student A heeft twee rijen met verblijfsjaar 1 en geen met verblijfsjaar 2
  basisbestand <- maak_wissel_rijen(
    c("A", "A"),
    c(1, 1),
    c("opl1", "opl2"),
    c(2019, 2019)
  )
  zittend <- tibble(persoonsgebonden_nummer = "A")

  expect_error(bereken_wissel_xjr(
    basisbestand,
    zittend,
    verblijfsjaren = c(1, 2),
    doeljaar = 2,
    label_gewisseld = "Gewisseld binnen 1 jaar",
    label_niet = "Niet gewisseld binnen 1 jaar",
    suffix = "1jr"
  ))
})
