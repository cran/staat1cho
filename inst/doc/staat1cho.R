## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(staat1cho)

## ----basisbestand-------------------------------------------------------------
library(tibble)

## Drie studenten, twee cohortjaren
## S1: behaalt diploma in 2022 (binnen 3 jaar na instroom 2020)
## S2: valt uit na 1 jaar (verschijnt alleen in 2020)
## S3: wisselt opleiding na 1 jaar (andere opleidingscode in 2021)

hoofd <- "hoofdinschrijving binnen het domein actuele instelling"
bachelor_diploma <- "Hoofd-bachelor-diploma binnen de actuele instelling"

basisbestand <- tibble(
  persoonsgebonden_nummer = c("S1", "S1", "S2", "S3", "S3"),
  inschrijvingsjaar       = c(2020L, 2021L, 2020L, 2020L, 2021L),
  verblijfsjaar_actuele_instelling = c(1L, 2L, 1L, 1L, 2L),
  soort_hoger_onderwijs   = "hbo",
  soort_inschrijving_actuele_instelling = hoofd,
  soort_inschrijving_actuele_instelling_label = hoofd,
  diplomajaar             = c(NA_integer_, 2022L, NA_integer_, NA_integer_, NA_integer_),
  soort_diploma_instelling = c(NA, bachelor_diploma, NA, NA, NA),
  soort_diploma_instelling_label = c(NA, bachelor_diploma, NA, NA, NA),
  geslacht_label = "man",
  opleidingsvorm = "voltijd",
  opleidingsvorm_label = "voltijd",
  indicatie_internationale_student_label = "geen internationale student",
  indicatie_eer_actueel_label = "geen EER-student",
  croho_onderdeel_actuele_opleiding = "techniek",
  croho_onderdeel_actuele_opleiding_label = "techniek",
  opleiding_actueel_equivalent = c("34401", "34401", "34401", "34401", "39999"),
  type_hoger_onderwijs_binnen_soort_hoger_onderwijs = "ba",
  leeftijd_per_peildatum_1_oktober = 19L,
  postcodecijfers_student_op_1_oktober = "4818",
  postcodecijfers_van_de_hoogste_vooropl_voor_het_ho = "4818",
  locatie_label = "Breda"
)

## ----instroom-----------------------------------------------------------------
cohorten <- maak_instroom_cohort(basisbestand, soort_ho = "hbo")
cohorten[, c("persoonsgebonden_nummer", "inschrijvingsjaar", "eerstejaar_instelling")]

## ----diploma------------------------------------------------------------------
diploma <- maak_diploma_behaald(basisbestand)
diploma

## ----rendement----------------------------------------------------------------
rendement <- bereken_rendement(cohorten, diploma)
rendement[, c("persoonsgebonden_nummer", "rendement_3jr", "rendement_5jr", "rendement_8jr")]

## ----uitval-------------------------------------------------------------------
## peiljaar = het jaar na het laatste jaar in de data
uitval <- bereken_uitval(basisbestand, diploma, cohorten, jaar = 2023L)
uitval[, c("persoonsgebonden_nummer", "status", "uitval_1jr", "uitval_3jr")]

## ----wissel-------------------------------------------------------------------
wissel <- bereken_studiewissel(basisbestand, cohorten, diploma, uitval)
wissel[, c("persoonsgebonden_nummer", "studiewissel_1jr", "studiewissel_3jr")]

## ----combineer, warning = FALSE-----------------------------------------------
indicatoren <- combineer_indicatoren(cohorten, rendement, uitval, wissel)
indicatoren[, c("inschrijvingsjaar", "opleidingscode", "status", "rendement", "uitval", "studiewissel")]

## ----dashboard, eval = FALSE--------------------------------------------------
# start_dashboard()

