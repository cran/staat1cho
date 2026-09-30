#' Combineer alle indicatoren tot een analysebestand
#'
#' Voegt rendement-, uitval- en (optioneel) studiewisselindicatoren samen met
#' het instroomcohort. Past kolomnamen en factorniveaus aan voor gebruik in
#' rapportages.
#'
#' @param cohorten_instroom Tibble zoals gemaakt door [maak_instroom_cohort()]
#' @param rendement_indicatoren Tibble zoals gemaakt door [bereken_rendement()]
#' @param uitval_indicatoren Tibble zoals gemaakt door [bereken_uitval()]
#' @param studiewissel_indicatoren Tibble zoals gemaakt door
#'   [bereken_studiewissel()], of `NULL`. Studiewissel is een
#'   studentniveau-concept en alleen van toepassing bij
#'   `niveau = "student"`. Geef `NULL` door bij inschrijvingsniveau.
#' @param niveau Analyseniveau: `"student"` (standaard) of `"inschrijving"`.
#'   Moet overeenkomen met het niveau waarop de andere invoertibbles zijn
#'   aangemaakt.
#'
#' @return Een tibble met gecombineerde indicatorkolommen, klaar voor
#'   rapportage. Bevat o.a. `status`, `rendement`, `uitval` en alle
#'   onderliggende deelscores. Bij `niveau = "student"` zijn ook
#'   studiewisselkolommen aanwezig als `studiewissel_indicatoren` is meegegeven.
#'
#'   `vooropleiding` vat de hoogste vooropleiding voor het HO samen (havo,
#'   vwo, mbo, ho, buitenlands, overig of onbekend). `eerstejaars_ho` geeft
#'   aan of het instroomjaar ook het eerste jaar in het hoger onderwijs is
#'   (`"eerstejaars HO"`) of dat de student al eerder in het HO stond
#'   (`"eerder in HO"`). Beide zijn `"onbekend"` als de 1CHO-kolommen
#'   `hoogste_vooropleiding_voor_het_ho_omschrijving_vooropleiding` resp.
#'   `eerste_jaar_in_het_hoger_onderwijs` ontbreken.
#'
#'   Attributen: `niveau` en `peildatum` (1 oktober van het laatste
#'   inschrijvingsjaar in de data, zie [maak_benchmarkrapport()]).
#'
#' @examples
#' cohort <- tibble::tibble(
#'   persoonsgebonden_nummer = "S1",
#'   inschrijvingsjaar = 2020L,
#'   eerstejaar_instelling = 2020L,
#'   geslacht_label = "man",
#'   locatie_label = "Breda",
#'   opleiding_actueel_equivalent = "34401",
#'   opleidingsvorm_label = "voltijd",
#'   type_hoger_onderwijs_binnen_soort_hoger_onderwijs = "ba",
#'   indicatie_internationale_student_label = "geen internationale student",
#'   indicatie_eer_actueel_label = "geen EER-student",
#'   croho_onderdeel_actuele_opleiding_label = "techniek",
#'   leeftijd_per_peildatum_1_oktober = 19L,
#'   postcodecijfers_student_op_1_oktober = "4818",
#'   postcodecijfers_van_de_hoogste_vooropl_voor_het_ho = "4818",
#'   soort_diploma_instelling_label = NA_character_
#' )
#' rendement <- tibble::tibble(
#'   persoonsgebonden_nummer = "S1",
#'   eerstejaar_instelling = 2020L,
#'   jaar_eerste_diploma = NA_real_,
#'   verblijfsjaar_eerste_diploma = NA_integer_,
#'   diploma = NA_character_,
#'   rendement_xjaar = factor(NA_character_),
#'   rendement_3jr = factor("Geen diploma"),
#'   rendement_5jr = factor("Geen diploma"),
#'   rendement_8jr = factor("Geen diploma")
#' )
#' uitval <- tibble::tibble(
#'   persoonsgebonden_nummer = "S1",
#'   laatste_jaar_inschrijving = NA_real_,
#'   diploma = NA_character_,
#'   status = factor("Zittend"),
#'   uitval_xjr = NA_real_,
#'   uitval_1jr = factor("Na 1 jaar nog ingeschreven of diploma behaald"),
#'   uitval_3jr = factor("Na 3 jaar nog ingeschreven of diploma behaald")
#' )
#' wissel <- tibble::tibble(
#'   persoonsgebonden_nummer = "S1",
#'   studiewissel_1jr = factor("Niet gewisseld binnen 1 jaar"),
#'   studiewissel_3jr = factor("Niet gewisseld binnen 3 jaar"),
#'   opleidingscode_na_switch1jr = factor(NA_character_),
#'   opleidingsvorm_na_switch1jr = factor(NA_character_),
#'   opleidingsniveau_na_switch1jr = factor(NA_character_),
#'   sector_na_switch1jr = factor(NA_character_),
#'   opleidingscode_na_switch3jr = factor(NA_character_),
#'   opleidingsvorm_na_switch3jr = factor(NA_character_),
#'   opleidingsniveau_na_switch3jr = factor(NA_character_),
#'   sector_na_switch3jr = factor(NA_character_)
#' )
#' suppressWarnings(combineer_indicatoren(cohort, rendement, uitval, wissel))
#' @export
combineer_indicatoren <- function(
  cohorten_instroom,
  rendement_indicatoren,
  uitval_indicatoren,
  studiewissel_indicatoren = NULL,
  niveau = "student"
) {
  sleutels <- niveau_sleutels(niveau)

  result <- cohorten_instroom |>
    dplyr::left_join(rendement_indicatoren, by = sleutels) |>
    dplyr::left_join(uitval_indicatoren, by = sleutels)

  if (!is.null(studiewissel_indicatoren)) {
    result <- dplyr::left_join(
      result,
      studiewissel_indicatoren,
      by = "persoonsgebonden_nummer"
    )
  }

  ## Soort en opleiding van het behaalde diploma komen uit het diplomabestand
  ## (via rendement). Oudere aanroepen zonder die kolommen blijven werken.
  for (kol in c("soort_diploma", "opleidingscode_diploma")) {
    if (!kol %in% names(result)) {
      result[[kol]] <- NA_character_
    }
  }

  ## Vooropleiding en eerste jaar HO zijn optionele 1CHO-kolommen; zonder
  ## die kolommen wordt alles "onbekend".
  kolom_of_na <- function(kol) {
    if (kol %in% names(result)) result[[kol]] else rep(NA, nrow(result))
  }
  result$vooropleiding <- categoriseer_vooropleiding(kolom_of_na(VOOROPLEIDING_KOLOM))
  result$eerstejaars_ho <- bepaal_eerstejaars_ho(
    suppressWarnings(as.integer(kolom_of_na(EERSTE_JAAR_HO_KOLOM))),
    result$inschrijvingsjaar
  )

  result <- result |>
    dplyr::select(
      persoonsgebonden_nummer,
      inschrijvingsjaar,
      geslacht = geslacht_label,
      locatie = locatie_label,
      opleidingscode = opleiding_actueel_equivalent,
      opleidingsvorm = opleidingsvorm_label,
      opleidingsniveau = type_hoger_onderwijs_binnen_soort_hoger_onderwijs,
      int_student = indicatie_internationale_student_label,
      indicatie_EER = indicatie_eer_actueel_label,
      sector = croho_onderdeel_actuele_opleiding_label,
      leeftijd_bij_instroom = leeftijd_per_peildatum_1_oktober,
      vooropleiding,
      eerstejaars_ho,
      postcode4_student_1okt = postcodecijfers_student_op_1_oktober,
      postcode4_vooropleiding_voorHO = postcodecijfers_van_de_hoogste_vooropl_voor_het_ho,
      status,
      soortdiploma = soort_diploma,
      opleidingscode_diploma,
      rendement_3jr:rendement_8jr,
      uitval_xjr:uitval_3jr,
      dplyr::any_of(c(
        "studiewissel_1jr",
        "studiewissel_3jr",
        "opleidingscode_na_switch1jr",
        "opleidingsvorm_na_switch1jr",
        "opleidingsniveau_na_switch1jr",
        "sector_na_switch1jr",
        "opleidingscode_na_switch3jr",
        "opleidingsvorm_na_switch3jr",
        "opleidingsniveau_na_switch3jr",
        "sector_na_switch3jr"
      ))
    ) |>

    dplyr::mutate(
      opleidingscode = factor(opleidingscode),
      locatie = factor(locatie),
      opleidingsvorm = factor(opleidingsvorm),
      postcode4_student_1okt = factor(postcode4_student_1okt),
      postcode4_vooropleiding_voorHO = factor(postcode4_vooropleiding_voorHO)
    ) |>

    ## Lange categorielabels inkorten en ongeldige postcodes verwijderen
    dplyr::mutate(
      opleidingsvorm = hercodeer(
        opleidingsvorm,
        c("duaal" = "co\u00f6p-student of duaal onderwijs (vanaf het studiejaar 1998-1999)")
      ),
      sector = hercodeer(
        sector,
        c(
          "gedrag & maatschappij" = "gedrag en maatschappij",
          "taal & cultuur" = "taal en cultuur"
        )
      ),
      ## 0010-0040 zijn onbekende postcodewaarden in de 1CHO-data
      postcode4_student_1okt = hercodeer(
        postcode4_student_1okt,
        verwijder = ONBEKENDE_POSTCODES
      ),
      postcode4_vooropleiding_voorHO = hercodeer(
        postcode4_vooropleiding_voorHO,
        verwijder = ONBEKENDE_POSTCODES
      ),
      opleidingsniveau = hercodeer(
        opleidingsniveau,
        c(bachelor = "ba", master = "ma"),
        verwijder = "postinitiele master"
      )
    ) |>

    ## Samengevatte indicatoren op basis van de gedetailleerde xjr-waarden
    dplyr::mutate(
      uitval = dplyr::case_when(
        uitval_xjr == 1 ~ "Uitgevallen binnen 1 jaar",
        uitval_xjr %in% 2:3 ~ "Uitgevallen in 2e of 3e jaar",
        uitval_xjr > 3 ~ "Uitgevallen na 3 jaar",
        TRUE ~ "Niet uitgevallen"
      ),
      rendement = dplyr::case_when(
        rendement_5jr == "Diploma binnen 5 jaar" ~ "Diploma binnen 5 jaar",
        rendement_8jr == "Diploma binnen 8 jaar" ~ "Diploma binnen 5-8 jaar",
        rendement_8jr == "Diploma na 8 jaar" ~ "Diploma na 8 jaar",
        rendement_8jr == "Geen diploma" ~ "Geen diploma",
        rendement_8jr == "Onbekend (diplomajaar voor instroomjaar)" ~ "Onbekend",
        rendement_8jr == NIET_WAARNEEMBAAR ~ NIET_WAARNEEMBAAR
      )
    )

  if (!is.null(studiewissel_indicatoren)) {
    result <- result |>
      dplyr::mutate(
        studiewissel = dplyr::case_when(
          studiewissel_1jr ==
            "Gewisseld binnen 1 jaar" ~ "Gewisseld binnen 1 jaar",
          studiewissel_3jr ==
            "Gewisseld binnen 3 jaar" ~ "Gewisseld in het 2e of 3e jaar",
          studiewissel_3jr == NIET_WAARNEEMBAAR ~ NIET_WAARNEEMBAAR,
          TRUE ~ "Niet gewisseld"
        )
      )
  }

  ## Op studentniveau worden opleiding, sector en locatie van het instroomjaar
  ## getoond, maar uitkomsten gelden instellingsbreed. Deze kolom maakt
  ## zichtbaar of het diploma in de instroomopleiding is behaald (#45).
  result <- result |>
    dplyr::mutate(
      diploma_in_instroomopleiding = dplyr::if_else(
        is.na(opleidingscode_diploma),
        NA,
        as.character(opleidingscode_diploma) == as.character(opleidingscode)
      ),
      .after = opleidingscode_diploma
    )

  laatste_jaar <- attr(uitval_indicatoren, "laatste_jaar")
  if (is.null(laatste_jaar)) {
    laatste_jaar <- max(cohorten_instroom$inschrijvingsjaar, na.rm = TRUE)
  }

  attr(result, "niveau") <- niveau
  attr(result, "peildatum") <- peildatum_1cho(laatste_jaar)
  result
}
