## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(staat1cho)

## ----data---------------------------------------------------------------------
synth <- maak_synthetische_1cho(n_per_jaar = 100, jaren = 2014:2023)
pad <- tempfile(fileext = ".csv")
readr::write_delim(synth, pad, delim = ";", na = "")

basis <- maak_basisbestand(pad)
laatste_jaar <- max(basis$inschrijvingsjaar)
laatste_jaar

## ----instroom-----------------------------------------------------------------
cohorten <- maak_instroom_cohort(basis, soort_ho = "hoger beroepsonderwijs")
table(cohorten$inschrijvingsjaar)

## ----rendement----------------------------------------------------------------
diploma <- maak_diploma_behaald(basis)
rendement <- bereken_rendement(cohorten, diploma, laatste_jaar = laatste_jaar)
table(rendement$eerstejaar_instelling, rendement$rendement_5jr)

## ----uitval-------------------------------------------------------------------
uitval <- bereken_uitval(basis, diploma, cohorten)
table(uitval$status)

## ----wissel-------------------------------------------------------------------
wissel <- bereken_studiewissel(basis, cohorten, diploma, uitval)
table(wissel$studiewissel_1jr)

## ----combineer----------------------------------------------------------------
indicatoren <- combineer_indicatoren(cohorten, rendement, uitval, wissel)
head(indicatoren[, c("inschrijvingsjaar", "opleidingscode", "status", "rendement", "uitval", "studiewissel")])

## ----diploma-elders-----------------------------------------------------------
table(indicatoren$diploma_in_instroomopleiding, useNA = "ifany")

## ----inschrijving-------------------------------------------------------------
cohort_ins <- maak_instroom_cohort(basis, "hoger beroepsonderwijs", niveau = "inschrijving")
nrow(cohort_ins) - nrow(cohorten)  # extra cohorten door wissels

## ----benchmark----------------------------------------------------------------
rapport <- maak_benchmarkrapport(indicatoren)
rapport[rapport$sector == "totaal", c("inschrijvingsjaar", "n", "pct_uitval_1jr", "pct_rendement_5jr")]

## ----dashboard, eval = FALSE--------------------------------------------------
# start_dashboard()

## ----pipeline, eval = FALSE---------------------------------------------------
# pad_1cho    <- "C:/Tools/1cijferho/data/02-output/EV21PL24_enriched.csv"
# niveau      <- "student"    # of "inschrijving"
# vakhawv_pad <- ""           # pad naar VAKHAVW..._decoded.csv, of leeg
# vlpbek_pad  <- ""           # nog leeg laten

