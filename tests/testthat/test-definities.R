test_that("beide niveaus leggen de filterlogica uit", {
  expect_type(DEFINITIES[["student"]]$filter_instroom, "character")
  expect_type(DEFINITIES[["inschrijving"]]$filter_instroom, "character")
})

test_that("definities van tijdgebonden indicatoren noemen onvolledige cohorten", {
  for (niveau in c("student", "inschrijving")) {
    for (ind in c("rendement_3jr", "rendement_5jr", "rendement_8jr", "uitval_1jr", "uitval_3jr")) {
      expect_match(DEFINITIES[[niveau]][[ind]], "Nog niet waarneembaar", info = paste(niveau, ind))
    }
  }
})

test_that("alle packages die de app laadt staan in DASHBOARD_PACKAGES of Imports", {
  app <- readLines(system.file("app", "app.R", package = "staat1cho"))
  geladen <- sub("^library[(]([A-Za-z0-9.]+)[)].*$", "\\1", grep("^library[(]", app, value = TRUE))
  imports <- c("shiny", "dplyr", "forcats", "readr", "staat1cho")
  expect_true(all(geladen %in% c(DASHBOARD_PACKAGES, imports)), info = paste(setdiff(geladen, c(DASHBOARD_PACKAGES, imports)), collapse = ", "))
})
