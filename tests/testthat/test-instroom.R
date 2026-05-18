## --- maak_instroom_cohort ---

test_that("filtert op soort_ho", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "B", "C"),
    soort_hoger_onderwijs = c("hbo", "wo", "hbo"),
    soort_inschrijving_actuele_instelling_label = HOOFD_INSCHRIJVING,
    verblijfsjaar_actuele_instelling = 1L,
    inschrijvingsjaar = 2020
  )

  result <- maak_instroom_cohort(basisbestand, "hbo")

  expect_setequal(result$persoonsgebonden_nummer, c("A", "C"))
})

test_that("accepteert meerdere soort_ho-waarden", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "B", "C"),
    soort_hoger_onderwijs = c("hoger beroepsonderwijs", "hbo", "wo"),
    soort_inschrijving_actuele_instelling_label = HOOFD_INSCHRIJVING,
    verblijfsjaar_actuele_instelling = 1L,
    inschrijvingsjaar = 2020
  )

  result <- maak_instroom_cohort(
    basisbestand,
    c("hoger beroepsonderwijs", "hbo")
  )

  expect_setequal(result$persoonsgebonden_nummer, c("A", "B"))
})

test_that("behoudt alleen verblijfsjaar 1", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "A", "A"),
    soort_hoger_onderwijs = "hbo",
    soort_inschrijving_actuele_instelling_label = HOOFD_INSCHRIJVING,
    verblijfsjaar_actuele_instelling = c(1L, 2L, 3L),
    inschrijvingsjaar = c(2020, 2021, 2022)
  )

  result <- maak_instroom_cohort(basisbestand, "hbo")

  expect_equal(nrow(result), 1)
  expect_equal(result$verblijfsjaar_actuele_instelling, 1L)
})

test_that("filtert niet-hoofdinschrijvingen eruit", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "B", "C"),
    soort_hoger_onderwijs = "hbo",
    soort_inschrijving_actuele_instelling_label = c(
      HOOFD_INSCHRIJVING,
      "neveninschrijving binnen het domein actuele instelling",
      "neveninschrijving binnen het domein hoger onderwijs"
    ),
    verblijfsjaar_actuele_instelling = 1L,
    inschrijvingsjaar = 2020
  )

  result <- maak_instroom_cohort(basisbestand, "hbo")

  expect_equal(nrow(result), 1)
  expect_equal(result$persoonsgebonden_nummer, "A")
})

test_that("zet eerstejaar_instelling gelijk aan inschrijvingsjaar", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = "A",
    soort_hoger_onderwijs = "hbo",
    soort_inschrijving_actuele_instelling_label = HOOFD_INSCHRIJVING,
    verblijfsjaar_actuele_instelling = 1L,
    inschrijvingsjaar = 2021
  )

  result <- maak_instroom_cohort(basisbestand, "hbo")

  expect_equal(result$eerstejaar_instelling, 2021)
})

test_that("geeft fout bij dubbele persoonsgebonden_nummers", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "A"),
    soort_hoger_onderwijs = "hbo",
    soort_inschrijving_actuele_instelling_label = HOOFD_INSCHRIJVING,
    verblijfsjaar_actuele_instelling = 1L,
    inschrijvingsjaar = 2020
  )

  expect_error(maak_instroom_cohort(basisbestand, "hbo"))
})

test_that("geeft lege tibble terug bij geen matches", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = "A",
    soort_hoger_onderwijs = "wo",
    soort_inschrijving_actuele_instelling_label = HOOFD_INSCHRIJVING,
    verblijfsjaar_actuele_instelling = 1L,
    inschrijvingsjaar = 2020
  )

  result <- maak_instroom_cohort(basisbestand, "hbo")

  expect_equal(nrow(result), 0)
})

test_that("behoudt alle originele kolommen in de output", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = "A",
    soort_hoger_onderwijs = "hbo",
    soort_inschrijving_actuele_instelling_label = HOOFD_INSCHRIJVING,
    verblijfsjaar_actuele_instelling = 1L,
    inschrijvingsjaar = 2020,
    extra_kolom = "x"
  )

  result <- maak_instroom_cohort(basisbestand, "hbo")

  expect_true("extra_kolom" %in% names(result))
  expect_true("eerstejaar_instelling" %in% names(result))
})

test_that("past tegelijk alle drie filters toe", {
  ## Student B valt af op soort_ho, C op verblijfsjaar, D op inschrijvingstype
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "B", "C", "D"),
    soort_hoger_onderwijs = c("hbo", "wo", "hbo", "hbo"),
    soort_inschrijving_actuele_instelling_label = c(
      HOOFD_INSCHRIJVING,
      HOOFD_INSCHRIJVING,
      HOOFD_INSCHRIJVING,
      "neveninschrijving"
    ),
    verblijfsjaar_actuele_instelling = c(1L, 1L, 2L, 1L),
    inschrijvingsjaar = c(2020, 2020, 2019, 2020)
  )

  result <- maak_instroom_cohort(basisbestand, "hbo")

  expect_equal(nrow(result), 1)
  expect_equal(result$persoonsgebonden_nummer, "A")
})
