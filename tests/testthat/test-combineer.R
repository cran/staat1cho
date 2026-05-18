## Bouwt een minimale set van vier indicatortibbles voor combineer_indicatoren
maak_input <- function(
  pgn = "A",
  inschrijvingsjaar = 2020,
  opleidingsvorm_label = "voltijd",
  croho_label = "techniek",
  postcode_student = "4818",
  postcode_vooropl = "4818",
  opleidingsniveau = "ba",
  status = "Zittend",
  uitval_xjr = NA_real_,
  uitval_1jr = "Na 1 jaar nog ingeschreven of diploma behaald",
  uitval_3jr = "Na 3 jaar nog ingeschreven of diploma behaald",
  studiewissel_1jr = "Niet gewisseld binnen 1 jaar",
  studiewissel_3jr = "Niet gewisseld binnen 3 jaar",
  rendement_3jr = "Geen diploma",
  rendement_5jr = "Geen diploma",
  rendement_8jr = "Geen diploma"
) {
  cohorten_instroom <- tibble(
    persoonsgebonden_nummer = pgn,
    inschrijvingsjaar = inschrijvingsjaar,
    eerstejaar_instelling = inschrijvingsjaar,
    geslacht_label = "vrouw",
    locatie_label = "Breda",
    opleiding_actueel_equivalent = "12345",
    opleidingsvorm_label = opleidingsvorm_label,
    type_hoger_onderwijs_binnen_soort_hoger_onderwijs = opleidingsniveau,
    indicatie_internationale_student_label = "geen internationale student",
    indicatie_eer_actueel_label = "eer",
    croho_onderdeel_actuele_opleiding_label = croho_label,
    leeftijd_per_peildatum_1_oktober = 19,
    postcodecijfers_student_op_1_oktober = postcode_student,
    postcodecijfers_van_de_hoogste_vooropl_voor_het_ho = postcode_vooropl,
    soort_diploma_instelling_label = NA_character_
  )

  rendement_indicatoren <- tibble(
    persoonsgebonden_nummer = pgn,
    eerstejaar_instelling = inschrijvingsjaar,
    jaar_eerste_diploma = NA_real_,
    verblijfsjaar_eerste_diploma = NA_integer_,
    diploma = NA_character_,
    rendement_xjaar = factor(NA_character_),
    rendement_3jr = factor(rendement_3jr),
    rendement_5jr = factor(rendement_5jr),
    rendement_8jr = factor(rendement_8jr)
  )

  uitval_indicatoren <- tibble(
    persoonsgebonden_nummer = pgn,
    laatste_jaar_inschrijving = NA_real_,
    diploma = NA_character_,
    status = factor(status),
    uitval_xjr = uitval_xjr,
    uitval_1jr = factor(uitval_1jr),
    uitval_3jr = factor(uitval_3jr)
  )

  studiewissel_indicatoren <- tibble(
    persoonsgebonden_nummer = pgn,
    studiewissel_1jr = factor(studiewissel_1jr),
    studiewissel_3jr = factor(studiewissel_3jr),
    opleidingscode_na_switch1jr = factor(NA_character_),
    opleidingsvorm_na_switch1jr = factor(NA_character_),
    opleidingsniveau_na_switch1jr = factor(NA_character_),
    sector_na_switch1jr = factor(NA_character_),
    opleidingscode_na_switch3jr = factor(NA_character_),
    opleidingsvorm_na_switch3jr = factor(NA_character_),
    opleidingsniveau_na_switch3jr = factor(NA_character_),
    sector_na_switch3jr = factor(NA_character_)
  )

  list(
    cohorten_instroom = cohorten_instroom,
    rendement_indicatoren = rendement_indicatoren,
    uitval_indicatoren = uitval_indicatoren,
    studiewissel_indicatoren = studiewissel_indicatoren
  )
}

roep_combineer <- function(inp) {
  ## suppressWarnings: fct_recode waarschuwt over ontbrekende niveaus in testdata
  ## die in productie wel aanwezig zijn omdat de factor alle mogelijke waarden bevat
  suppressWarnings(combineer_indicatoren(
    inp$cohorten_instroom,
    inp$rendement_indicatoren,
    inp$uitval_indicatoren,
    inp$studiewissel_indicatoren
  ))
}

## --- uitvoerstructuur ---

test_that("geeft één rij per student terug", {
  result <- roep_combineer(maak_input())
  expect_equal(nrow(result), 1)
})

test_that("bevat de verwachte kerncolomnamen", {
  result <- roep_combineer(maak_input())
  verwachte_kolommen <- c(
    "inschrijvingsjaar",
    "geslacht",
    "locatie",
    "opleidingscode",
    "opleidingsvorm",
    "opleidingsniveau",
    "status",
    "uitval",
    "studiewissel",
    "rendement"
  )
  expect_true(all(verwachte_kolommen %in% names(result)))
})

## --- locatie ---

test_that("neemt locatie_label direct over als locatie", {
  result <- roep_combineer(maak_input())
  expect_equal(as.character(result$locatie), "Breda")
})

## --- fct_recode: opleidingsvorm ---

test_that("hernoemt lange opleidingsvorm-label naar 'duaal'", {
  inp <- maak_input(
    opleidingsvorm_label = "coöp-student of duaal onderwijs (vanaf het studiejaar 1998-1999)"
  )
  result <- roep_combineer(inp)
  expect_equal(as.character(result$opleidingsvorm), "duaal")
})

test_that("laat andere opleidingsvorm-waarden ongewijzigd", {
  result <- roep_combineer(maak_input(opleidingsvorm_label = "voltijd"))
  expect_equal(as.character(result$opleidingsvorm), "voltijd")
})

## --- fct_recode: sector ---

test_that("hernoemt 'gedrag en maatschappij' naar 'gedrag & maatschappij'", {
  result <- roep_combineer(maak_input(croho_label = "gedrag en maatschappij"))
  expect_equal(as.character(result$sector), "gedrag & maatschappij")
})

test_that("hernoemt 'taal en cultuur' naar 'taal & cultuur'", {
  result <- roep_combineer(maak_input(croho_label = "taal en cultuur"))
  expect_equal(as.character(result$sector), "taal & cultuur")
})

test_that("laat andere sector-waarden ongewijzigd", {
  result <- roep_combineer(maak_input(croho_label = "techniek"))
  expect_equal(as.character(result$sector), "techniek")
})

## --- fct_recode: opleidingsniveau ---

test_that("hernoemt 'ba' naar 'bachelor'", {
  result <- roep_combineer(maak_input(opleidingsniveau = "ba"))
  expect_equal(as.character(result$opleidingsniveau), "bachelor")
})

test_that("hernoemt 'ma' naar 'master'", {
  result <- roep_combineer(maak_input(opleidingsniveau = "ma"))
  expect_equal(as.character(result$opleidingsniveau), "master")
})

test_that("verwijdert 'postinitiele master' uit opleidingsniveau", {
  result <- roep_combineer(maak_input(opleidingsniveau = "postinitiele master"))
  expect_true(is.na(result$opleidingsniveau))
})

## --- postcodefiltering ---

test_that("zet postcode 0010 om naar NA", {
  result <- roep_combineer(maak_input(postcode_student = "0010"))
  expect_true(is.na(result$postcode4_student_1okt))
})

test_that("zet alle vier ongeldige postcodes om naar NA", {
  for (postcode in c("0010", "0020", "0030", "0040")) {
    result <- roep_combineer(maak_input(postcode_student = postcode))
    expect_true(is.na(result$postcode4_student_1okt), label = postcode)
  }
})

test_that("laat geldige postcodes intact", {
  result <- roep_combineer(maak_input(postcode_student = "4818"))
  expect_equal(as.character(result$postcode4_student_1okt), "4818")
})

test_that("filtert ongeldige postcodes ook in vooropleidingspostcode", {
  result <- roep_combineer(maak_input(postcode_vooropl = "0030"))
  expect_true(is.na(result$postcode4_vooropleiding_voorHO))
})

## --- samengevatte indicatoren ---

test_that("bepaalt uitval correct op basis van uitval_xjr", {
  r1 <- roep_combineer(maak_input(uitval_xjr = 1, status = "Uitgevallen"))
  r2 <- roep_combineer(maak_input(uitval_xjr = 2, status = "Uitgevallen"))
  r3 <- roep_combineer(maak_input(uitval_xjr = 4, status = "Uitgevallen"))
  r4 <- roep_combineer(maak_input())

  expect_equal(r1$uitval, "Uitgevallen binnen 1 jaar")
  expect_equal(r2$uitval, "Uitgevallen in 2e of 3e jaar")
  expect_equal(r3$uitval, "Uitgevallen na 3 jaar")
  expect_equal(r4$uitval, "Niet uitgevallen")
})

test_that("bepaalt studiewissel correct op basis van studiewissel_1jr en _3jr", {
  r1 <- roep_combineer(maak_input(studiewissel_1jr = "Gewisseld binnen 1 jaar"))
  r2 <- roep_combineer(maak_input(studiewissel_3jr = "Gewisseld binnen 3 jaar"))
  r3 <- roep_combineer(maak_input())

  expect_equal(r1$studiewissel, "Gewisseld binnen 1 jaar")
  expect_equal(r2$studiewissel, "Gewisseld in het 2e of 3e jaar")
  expect_equal(r3$studiewissel, "Niet gewisseld")
})

test_that("geeft 'Onbekend' als rendement_8jr de onbekend-waarde heeft", {
  result <- roep_combineer(maak_input(
    rendement_5jr = "Onbekend (diplomajaar voor instroomjaar)",
    rendement_8jr = "Onbekend (diplomajaar voor instroomjaar)"
  ))
  expect_equal(result$rendement, "Onbekend")
})

test_that("bepaalt rendement correct op basis van rendement_5jr en _8jr", {
  r1 <- roep_combineer(maak_input(
    rendement_5jr = "Diploma binnen 5 jaar",
    rendement_8jr = "Diploma binnen 8 jaar"
  ))
  r2 <- roep_combineer(maak_input(
    rendement_5jr = "Diploma na 5 jaar",
    rendement_8jr = "Diploma binnen 8 jaar"
  ))
  r3 <- roep_combineer(maak_input(
    rendement_5jr = "Diploma na 5 jaar",
    rendement_8jr = "Diploma na 8 jaar"
  ))
  r4 <- roep_combineer(maak_input())

  expect_equal(r1$rendement, "Diploma binnen 5 jaar")
  expect_equal(r2$rendement, "Diploma binnen 5-8 jaar")
  expect_equal(r3$rendement, "Diploma na 8 jaar")
  expect_equal(r4$rendement, "Geen diploma")
})
