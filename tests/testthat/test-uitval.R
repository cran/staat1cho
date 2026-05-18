maak_basis <- function(pgn, inschrijvingsjaar) {
  tibble(
    persoonsgebonden_nummer = pgn,
    inschrijvingsjaar = inschrijvingsjaar,
    soort_inschrijving_actuele_instelling = "hoofdinschrijving"
  )
}

maak_cohort <- function(pgn, eerstejaar) {
  tibble(
    persoonsgebonden_nummer = pgn,
    eerstejaar_instelling = eerstejaar
  )
}

geen_diplomas <- tibble(
  persoonsgebonden_nummer = character(0),
  jaar_eerste_diploma = integer(0),
  verblijfsjaar_eerste_diploma = integer(0),
  diploma = character(0)
)

met_diploma <- function(pgn) {
  tibble(
    persoonsgebonden_nummer = pgn,
    jaar_eerste_diploma = 2022,
    verblijfsjaar_eerste_diploma = 3L,
    diploma = "Diploma behaald (excl. propedeuse)"
  )
}

## --- status ---

test_that("markeert zittende student correct", {
  jaar <- 2025

  result <- bereken_uitval(
    maak_basis("A", jaar - 1),
    geen_diplomas,
    maak_cohort("A", 2022),
    jaar
  )

  expect_equal(as.character(result$status), "Zittend")
})

test_that("markeert gediplomeerde student correct", {
  jaar <- 2025

  result <- bereken_uitval(
    maak_basis("A", 2022),
    met_diploma("A"),
    maak_cohort("A", 2020),
    jaar
  )

  expect_equal(as.character(result$status), "Diploma behaald")
})

test_that("markeert uitgevallen student correct", {
  jaar <- 2025

  result <- bereken_uitval(
    maak_basis("A", 2021),
    geen_diplomas,
    maak_cohort("A", 2020),
    jaar
  )

  expect_equal(as.character(result$status), "Uitgevallen")
})

test_that("diploma heeft voorrang boven zittend als student ook ingeschreven is", {
  ## Student staat in jaar - 1 ingeschreven maar heeft ook een diploma
  jaar <- 2025

  result <- bereken_uitval(
    maak_basis("A", jaar - 1),
    met_diploma("A"),
    maak_cohort("A", 2020),
    jaar
  )

  expect_equal(as.character(result$status), "Diploma behaald")
})

test_that("status is een factor", {
  jaar <- 2025

  result <- bereken_uitval(
    maak_basis("A", 2021),
    geen_diplomas,
    maak_cohort("A", 2020),
    jaar
  )

  expect_true(is.factor(result$status))
})

## --- uitval_xjr ---

test_that("berekent uitval_xjr correct", {
  jaar <- 2025

  result <- bereken_uitval(
    maak_basis("A", 2021),
    geen_diplomas,
    maak_cohort("A", 2020),
    jaar
  )

  ## 2021 + 1 - 2020 = 2
  expect_equal(result$uitval_xjr, 2)
})

test_that("uitval_xjr is NA voor zittende student", {
  jaar <- 2025

  result <- bereken_uitval(
    maak_basis("A", jaar - 1),
    geen_diplomas,
    maak_cohort("A", 2022),
    jaar
  )

  expect_true(is.na(result$uitval_xjr))
})

test_that("uitval_xjr is NA voor gediplomeerde student", {
  jaar <- 2025

  result <- bereken_uitval(
    maak_basis("A", 2022),
    met_diploma("A"),
    maak_cohort("A", 2020),
    jaar
  )

  expect_true(is.na(result$uitval_xjr))
})

## --- uitval_1jr en uitval_3jr ---

test_that("categoriseert uitval_1jr correct", {
  jaar <- 2025
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "B"),
    inschrijvingsjaar = c(2020, 2021),
    soort_inschrijving_actuele_instelling = "hoofdinschrijving"
  )
  cohorten_instroom <- tibble(
    persoonsgebonden_nummer = c("A", "B"),
    eerstejaar_instelling = 2020
  )

  result <- bereken_uitval(basisbestand, geen_diplomas, cohorten_instroom, jaar)

  ## A: xjr = 1 -> uitgevallen binnen 1 jaar
  expect_equal(
    as.character(result$uitval_1jr[result$persoonsgebonden_nummer == "A"]),
    "Uitgevallen binnen 1 jaar"
  )
  ## B: xjr = 2 -> niet binnen 1 jaar
  expect_equal(
    as.character(result$uitval_1jr[result$persoonsgebonden_nummer == "B"]),
    "Na 1 jaar nog ingeschreven of diploma behaald"
  )
})

test_that("categoriseert uitval_3jr correct inclusief grenswaarde", {
  jaar <- 2025
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "B", "C"),
    ## A: xjr = 2020 + 1 - 2020 = 1, B: xjr = 2022 + 1 - 2020 = 3, C: xjr = 2015 + 1 - 2010 = 6
    inschrijvingsjaar = c(2020, 2022, 2015),
    soort_inschrijving_actuele_instelling = "hoofdinschrijving"
  )
  cohorten_instroom <- tibble(
    persoonsgebonden_nummer = c("A", "B", "C"),
    eerstejaar_instelling = c(2020, 2020, 2010)
  )

  result <- bereken_uitval(basisbestand, geen_diplomas, cohorten_instroom, jaar)

  ## A: xjr = 1 -> uitgevallen binnen 3 jaar
  expect_equal(
    as.character(result$uitval_3jr[result$persoonsgebonden_nummer == "A"]),
    "Uitgevallen binnen 3 jaar"
  )
  ## B: xjr = 3 (exact op grens) -> uitgevallen binnen 3 jaar
  expect_equal(
    as.character(result$uitval_3jr[result$persoonsgebonden_nummer == "B"]),
    "Uitgevallen binnen 3 jaar"
  )
  ## C: xjr = 6 -> na 3 jaar
  expect_equal(
    as.character(result$uitval_3jr[result$persoonsgebonden_nummer == "C"]),
    "Na 3 jaar nog ingeschreven of diploma behaald"
  )
})

test_that("verwerkt meerdere studenten tegelijk correct", {
  jaar <- 2025
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "B", "C"),
    inschrijvingsjaar = c(jaar - 1, 2022, 2021),
    soort_inschrijving_actuele_instelling = "hoofdinschrijving"
  )
  cohorten_instroom <- tibble(
    persoonsgebonden_nummer = c("A", "B", "C"),
    eerstejaar_instelling = 2020
  )

  result <- bereken_uitval(
    basisbestand,
    met_diploma("B"),
    cohorten_instroom,
    jaar
  )

  statussen <- setNames(
    as.character(result$status),
    result$persoonsgebonden_nummer
  )
  expect_equal(statussen[["A"]], "Zittend")
  expect_equal(statussen[["B"]], "Diploma behaald")
  expect_equal(statussen[["C"]], "Uitgevallen")
})

test_that("behoudt student in uitval die niet in cohorten_instroom zit", {
  ## basisbestand bevat meer studenten dan cohorten_instroom
  jaar <- 2025
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "B"),
    inschrijvingsjaar = c(2021, 2021),
    soort_inschrijving_actuele_instelling = "hoofdinschrijving"
  )
  ## Alleen A in cohort
  cohorten_instroom <- maak_cohort("A", 2020)

  result <- bereken_uitval(basisbestand, geen_diplomas, cohorten_instroom, jaar)

  expect_equal(nrow(result), 2)
  ## B heeft geen eerstejaar_instelling (NA na left_join) -> uitval_xjr wordt NA
  expect_true(is.na(result$uitval_xjr[result$persoonsgebonden_nummer == "B"]))
})
