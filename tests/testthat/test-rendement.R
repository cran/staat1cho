## --- maak_diploma_behaald ---

test_that("behoudt alleen afgeronde diploma's en niet propedeuse", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "B"),
    soort_diploma_instelling = c(
      BACHELOR_DIPLOMA,
      "Propedeusediploma binnen de actuele instelling"
    ),
    diplomajaar = c(2022L, 2020L),
    verblijfsjaar_actuele_instelling = c(3L, 1L)
  )

  result <- maak_diploma_behaald(basisbestand)

  expect_equal(nrow(result), 1)
  expect_equal(result$persoonsgebonden_nummer, "A")
})

test_that("accepteert alle 12 diplomasoorten", {
  alle_soorten <- c(
    "Hoofd-bachelor-diploma binnen de actuele instelling",
    "Neven-bachelor-diploma binnen de actuele instelling",
    "Hoofd-master-diploma binnen de actuele instelling",
    "Neven-master-diploma binnen de actuele instelling",
    "Hoofd-doctoraal-diploma binnen de actuele instelling",
    "Neven-doctoraal-diploma binnen de actuele instelling",
    "Hoofddiploma beroepsfase/voortgezet binnen de actuele instelling",
    "Nevendiploma beroepsfase/voortgezet binnen de actuele instelling",
    "Hoofddiploma associate degree binnen de actuele instelling",
    "Nevendiploma associate degree binnen de actuele instelling",
    "Hoofddiploma postinitiele master binnen de actuele instelling",
    "Nevendiploma postinitiele master binnen de actuele instelling"
  )

  basisbestand <- tibble(
    persoonsgebonden_nummer = as.character(seq_along(alle_soorten)),
    soort_diploma_instelling = alle_soorten,
    diplomajaar = 2022L,
    verblijfsjaar_actuele_instelling = 3L
  )

  result <- maak_diploma_behaald(basisbestand)

  expect_equal(nrow(result), 12)
})

test_that("sluit studenten zonder diploma uit", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = "A",
    soort_diploma_instelling = NA_character_,
    diplomajaar = NA_integer_,
    verblijfsjaar_actuele_instelling = 3L
  )

  result <- maak_diploma_behaald(basisbestand)

  expect_equal(nrow(result), 0)
})

test_that("zet diplomajaar 0 om naar NA", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = "A",
    soort_diploma_instelling = BACHELOR_DIPLOMA,
    diplomajaar = 0L,
    verblijfsjaar_actuele_instelling = 3L
  )

  result <- maak_diploma_behaald(basisbestand)

  expect_true(is.na(result$jaar_eerste_diploma))
})

test_that("kiest het vroegste diploma bij meerdere rijen", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "A"),
    soort_diploma_instelling = BACHELOR_DIPLOMA,
    diplomajaar = c(2024L, 2022L),
    verblijfsjaar_actuele_instelling = c(5L, 3L)
  )

  result <- maak_diploma_behaald(basisbestand)

  expect_equal(nrow(result), 1)
  expect_equal(result$jaar_eerste_diploma, 2022)
  expect_equal(result$verblijfsjaar_eerste_diploma, 3L)
})

test_that("bevat de juiste uitvoerkolommen", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = "A",
    soort_diploma_instelling = BACHELOR_DIPLOMA,
    diplomajaar = 2022L,
    verblijfsjaar_actuele_instelling = 3L
  )

  result <- maak_diploma_behaald(basisbestand)

  expect_named(
    result,
    c(
      "persoonsgebonden_nummer",
      "jaar_eerste_diploma",
      "verblijfsjaar_eerste_diploma",
      "diploma"
    )
  )
  expect_equal(result$diploma, "Diploma behaald (excl. propedeuse)")
})


## --- bereken_rendement ---

test_that("berekent rendement_xjaar correct", {
  cohorten_instroom <- tibble(
    persoonsgebonden_nummer = "A",
    eerstejaar_instelling = 2019
  )
  diploma_behaald <- tibble(
    persoonsgebonden_nummer = "A",
    jaar_eerste_diploma = 2022,
    verblijfsjaar_eerste_diploma = 3L,
    diploma = "Diploma behaald (excl. propedeuse)"
  )

  result <- bereken_rendement(cohorten_instroom, diploma_behaald)

  ## 2022 - 2019 + 1 = 4
  expect_equal(as.numeric(as.character(result$rendement_xjaar)), 4)
})

test_that("categoriseert rendement_3jr correct inclusief grenswaarden", {
  cohorten_instroom <- tibble(
    persoonsgebonden_nummer = c("A", "B", "C"),
    eerstejaar_instelling = 2015
  )
  diploma_behaald <- tibble(
    persoonsgebonden_nummer = c("A", "B"),
    ## A: xjaar = 2017 - 2015 + 1 = 3 (exact op grens -> binnen 3 jaar)
    ## B: xjaar = 2018 - 2015 + 1 = 4 (net over grens -> na 3 jaar)
    jaar_eerste_diploma = c(2017, 2018),
    verblijfsjaar_eerste_diploma = c(3L, 4L),
    diploma = "Diploma behaald (excl. propedeuse)"
  )

  result <- bereken_rendement(cohorten_instroom, diploma_behaald)

  expect_equal(
    as.character(result$rendement_3jr[result$persoonsgebonden_nummer == "A"]),
    "Diploma binnen 3 jaar"
  )
  expect_equal(
    as.character(result$rendement_3jr[result$persoonsgebonden_nummer == "B"]),
    "Diploma na 3 jaar"
  )
  expect_equal(
    as.character(result$rendement_3jr[result$persoonsgebonden_nummer == "C"]),
    "Geen diploma"
  )
})

test_that("categoriseert rendement_5jr correct inclusief grenswaarden", {
  cohorten_instroom <- tibble(
    persoonsgebonden_nummer = c("A", "B"),
    eerstejaar_instelling = 2015
  )
  diploma_behaald <- tibble(
    persoonsgebonden_nummer = c("A", "B"),
    ## A: xjaar = 5 (exact op grens)
    ## B: xjaar = 6 (net over grens)
    jaar_eerste_diploma = c(2019, 2020),
    verblijfsjaar_eerste_diploma = c(5L, 6L),
    diploma = "Diploma behaald (excl. propedeuse)"
  )

  result <- bereken_rendement(cohorten_instroom, diploma_behaald)

  expect_equal(
    as.character(result$rendement_5jr[result$persoonsgebonden_nummer == "A"]),
    "Diploma binnen 5 jaar"
  )
  expect_equal(
    as.character(result$rendement_5jr[result$persoonsgebonden_nummer == "B"]),
    "Diploma na 5 jaar"
  )
})

test_that("categoriseert rendement_8jr correct inclusief grenswaarden", {
  cohorten_instroom <- tibble(
    persoonsgebonden_nummer = c("A", "B"),
    eerstejaar_instelling = 2015
  )
  diploma_behaald <- tibble(
    persoonsgebonden_nummer = c("A", "B"),
    jaar_eerste_diploma = c(2022, 2023),
    verblijfsjaar_eerste_diploma = c(8L, 9L),
    diploma = "Diploma behaald (excl. propedeuse)"
  )

  result <- bereken_rendement(cohorten_instroom, diploma_behaald)

  expect_equal(
    as.character(result$rendement_8jr[result$persoonsgebonden_nummer == "A"]),
    "Diploma binnen 8 jaar"
  )
  expect_equal(
    as.character(result$rendement_8jr[result$persoonsgebonden_nummer == "B"]),
    "Diploma na 8 jaar"
  )
})

test_that("geeft 'Geen diploma' voor studenten zonder diploma", {
  cohorten_instroom <- tibble(
    persoonsgebonden_nummer = "A",
    eerstejaar_instelling = 2019
  )
  diploma_behaald <- tibble(
    persoonsgebonden_nummer = character(0),
    jaar_eerste_diploma = integer(0),
    verblijfsjaar_eerste_diploma = integer(0),
    diploma = character(0)
  )

  result <- bereken_rendement(cohorten_instroom, diploma_behaald)

  expect_equal(as.character(result$rendement_3jr), "Geen diploma")
  expect_equal(as.character(result$rendement_5jr), "Geen diploma")
  expect_equal(as.character(result$rendement_8jr), "Geen diploma")
})

test_that("geeft factorkolommen terug voor alle rendement-indicatoren", {
  cohorten_instroom <- tibble(
    persoonsgebonden_nummer = "A",
    eerstejaar_instelling = 2019
  )
  diploma_behaald <- tibble(
    persoonsgebonden_nummer = "A",
    jaar_eerste_diploma = 2022,
    verblijfsjaar_eerste_diploma = 3L,
    diploma = "Diploma behaald (excl. propedeuse)"
  )

  result <- bereken_rendement(cohorten_instroom, diploma_behaald)

  expect_true(is.factor(result$rendement_3jr))
  expect_true(is.factor(result$rendement_5jr))
  expect_true(is.factor(result$rendement_8jr))
})

test_that("markeert als 'Onbekend' als diplomajaar voor instroomjaar ligt", {
  cohorten_instroom <- tibble(
    persoonsgebonden_nummer = "A",
    eerstejaar_instelling = 2020
  )
  diploma_behaald <- tibble(
    persoonsgebonden_nummer = "A",
    jaar_eerste_diploma = 2018,
    verblijfsjaar_eerste_diploma = 1L,
    diploma = "Diploma behaald (excl. propedeuse)"
  )

  result <- bereken_rendement(cohorten_instroom, diploma_behaald)

  expect_true(grepl(
    "Onbekend",
    as.character(result$rendement_3jr)
  ))
})
