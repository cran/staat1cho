#' Maak een bestand met het vroegst behaalde diploma per student (of inschrijving)
#'
#' Filtert het basisbestand op diplomasoorten die gelden als afgeronde opleiding
#' en behoudt per sleutel alleen het eerste diploma op basis van diplomajaar.
#'
#' @param basisbestand Tibble zoals gemaakt door [maak_basisbestand()]
#' @param niveau Analyseniveau: `"student"` (standaard) of `"inschrijving"`.
#'   Bepaalt de sleutel waarop gededupliceerd wordt.
#'
#' @return Een tibble met één rij per student (bij `niveau = "student"`) of per
#'   student-opleidingcombinatie (bij `niveau = "inschrijving"`), met kolommen
#'   voor de sleutel(s), `jaar_eerste_diploma`, `verblijfsjaar_eerste_diploma`
#'   (verblijfsjaar aan de instelling resp. in de opleiding), `diploma`,
#'   `soort_diploma` en `opleidingscode_diploma` (de opleiding waarin het
#'   diploma behaald is). Gooit een fout bij dubbele sleutelcombinaties.
#'
#' @examples
#' basis <- tibble::tibble(
#'   persoonsgebonden_nummer = c("S1", "S2"),
#'   soort_diploma_instelling = c(
#'     "Hoofd-bachelor-diploma binnen de actuele instelling",
#'     NA_character_
#'   ),
#'   diplomajaar = c(2022L, NA_integer_),
#'   verblijfsjaar_actuele_instelling = c(3L, 1L)
#' )
#' maak_diploma_behaald(basis)
#' @export
maak_diploma_behaald <- function(basisbestand, niveau = "student") {
  diplomas <- c(
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

  sleutels <- niveau_sleutels(niveau)
  ## Op inschrijvingsniveau telt het verblijfsjaar in de opleiding, niet aan
  ## de instelling (anders telt een wisselaar zijn eerdere opleiding mee)
  verblijfsjaar_col <- if (niveau == "inschrijving") {
    "verblijfsjaar_actuele_opleiding_instelling"
  } else {
    "verblijfsjaar_actuele_instelling"
  }

  diploma_behaald <- basisbestand |>
    dplyr::filter(soort_diploma_instelling %in% diplomas) |>
    ## diplomajaar == 0 betekent geen jaar geregistreerd in de 1CHO-data
    dplyr::mutate(diplomajaar = dplyr::na_if(diplomajaar, 0)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(sleutels))) |>
    dplyr::arrange(diplomajaar, .by_group = TRUE) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(sleutels)), .keep_all = TRUE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      jaar_eerste_diploma = diplomajaar,
      verblijfsjaar_eerste_diploma = .data[[verblijfsjaar_col]],
      diploma = "Diploma behaald (excl. propedeuse)",
      soort_diploma = soort_diploma_instelling,
      opleidingscode_diploma = if ("opleiding_actueel_equivalent" %in% names(basisbestand)) {
        as.character(opleiding_actueel_equivalent)
      } else {
        NA_character_
      }
    ) |>
    dplyr::select(
      dplyr::all_of(sleutels),
      jaar_eerste_diploma,
      verblijfsjaar_eerste_diploma,
      diploma,
      soort_diploma,
      opleidingscode_diploma
    )

  if (anyDuplicated(diploma_behaald[sleutels]) > 0) {
    rlang::abort("Dubbele sleutelcombinaties in het diplomabestand gevonden!")
  }

  diploma_behaald
}

#' Bereken rendementsindicatoren per cohort
#'
#' Koppelt diplomagegevens aan het instroomcohort en berekent of een student
#' binnen 3, 5 of 8 jaar een diploma heeft behaald.
#'
#' @param cohorten_instroom Tibble zoals gemaakt door [maak_instroom_cohort()]
#' @param diploma_behaald Tibble zoals gemaakt door [maak_diploma_behaald()]
#' @param niveau Analyseniveau: `"student"` (standaard) of `"inschrijving"`.
#'   Moet overeenkomen met het niveau waarop `cohorten_instroom` en
#'   `diploma_behaald` zijn aangemaakt.
#' @param laatste_jaar Integer, het laatste inschrijvingsjaar in de data.
#'   Standaard het laatste instroomjaar in `cohorten_instroom`. Cohorten
#'   waarvoor instroomjaar + x - 1 na dit jaar valt, krijgen voor rendement
#'   binnen x jaar `"Nog niet waarneembaar"`: die studenten hebben nog geen x
#'   jaar kunnen studeren.
#'
#' @return Een tibble met kolommen voor de sleutel(s), `eerstejaar_instelling`,
#'   `jaar_eerste_diploma`, `verblijfsjaar_eerste_diploma`, `diploma`,
#'   `soort_diploma`, `opleidingscode_diploma`, `rendement_xjaar`, en
#'   factorkolommen `rendement_3jr`, `rendement_5jr`, `rendement_8jr`
#'
#' @examples
#' cohort <- tibble::tibble(
#'   persoonsgebonden_nummer = c("S1", "S2"),
#'   eerstejaar_instelling = 2020L
#' )
#' diploma <- tibble::tibble(
#'   persoonsgebonden_nummer = "S1",
#'   jaar_eerste_diploma = 2022L,
#'   verblijfsjaar_eerste_diploma = 3L,
#'   diploma = "Diploma behaald (excl. propedeuse)"
#' )
#' bereken_rendement(cohort, diploma, laatste_jaar = 2025L)
#' @export
bereken_rendement <- function(
  cohorten_instroom,
  diploma_behaald,
  niveau = "student",
  laatste_jaar = NULL
) {
  sleutels <- niveau_sleutels(niveau)
  if (is.null(laatste_jaar)) {
    laatste_jaar <- max(cohorten_instroom$eerstejaar_instelling, na.rm = TRUE)
  }

  ## Label per cohort: rendement binnen x jaar is pas bekend als het hele
  ## venster in de data zit. Een cohort half meetellen (alleen de snelle
  ## afstudeerders) zou het rendement juist overschatten.
  rendement_label <- function(x, rendement_xjaar, jaar_eerste_diploma, eerstejaar) {
    dplyr::case_when(
      eerstejaar + x - 1 > laatste_jaar ~ NIET_WAARNEEMBAAR,
      is.na(jaar_eerste_diploma) ~ "Geen diploma",
      ## diplomajaar voor instroomjaar kan voorkomen door data-inconsistentie
      jaar_eerste_diploma < eerstejaar ~ "Onbekend (diplomajaar voor instroomjaar)",
      rendement_xjaar <= x ~ paste("Diploma binnen", x, "jaar"),
      rendement_xjaar > x ~ paste("Diploma na", x, "jaar")
    )
  }

  cohorten_instroom |>
    dplyr::left_join(diploma_behaald, by = sleutels) |>
    dplyr::select(
      dplyr::all_of(sleutels),
      eerstejaar_instelling,
      jaar_eerste_diploma,
      verblijfsjaar_eerste_diploma,
      diploma,
      dplyr::any_of(c("soort_diploma", "opleidingscode_diploma"))
    ) |>
    dplyr::mutate(
      ## diplomajaar gebruikt dezelfde conventie als inschrijvingsjaar (startjaar
      ## van het academisch jaar), dus het verschil is het aantal academische
      ## jaren dat is verstreken. +1 omdat jaar 1 = 0 verschil zou geven.
      rendement_xjaar = jaar_eerste_diploma - eerstejaar_instelling + 1,

      rendement_3jr = rendement_label(3, rendement_xjaar, jaar_eerste_diploma, eerstejaar_instelling),
      rendement_5jr = rendement_label(5, rendement_xjaar, jaar_eerste_diploma, eerstejaar_instelling),
      rendement_8jr = rendement_label(8, rendement_xjaar, jaar_eerste_diploma, eerstejaar_instelling)
    ) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("rendement"), as.factor))
}
