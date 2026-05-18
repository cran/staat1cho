#' Bereken studiewisselindicatoren per cohort
#'
#' Bepaalt per student of zij binnen 1 of 3 jaar van opleiding zijn gewisseld.
#' Studenten die al zijn uitgevallen of gediplomeerd in de meetperiode worden
#' buiten beschouwing gelaten.
#'
#' @param basisbestand Tibble zoals gemaakt door [maak_basisbestand()]
#' @param cohorten_instroom Tibble zoals gemaakt door [maak_instroom_cohort()]
#' @param diploma_behaald Tibble zoals gemaakt door [maak_diploma_behaald()]
#' @param uitval_indicatoren Tibble zoals gemaakt door [bereken_uitval()]
#'
#' @return Een tibble met kolommen `persoonsgebonden_nummer`,
#'   `studiewissel_1jr`, `studiewissel_3jr` (factoren) en aanvullende
#'   switch-kolommen met de opleiding, opleidingsvorm, niveau en sector na
#'   de wissel
#'
#' @examples
#' basis <- tibble::tibble(
#'   persoonsgebonden_nummer = c("S1", "S1"),
#'   verblijfsjaar_actuele_instelling = c(1L, 2L),
#'   soort_inschrijving_actuele_instelling_label =
#'     "hoofdinschrijving binnen het domein actuele instelling",
#'   opleiding_actueel_equivalent = "34401",
#'   inschrijvingsjaar = c(2020L, 2021L),
#'   opleidingsvorm = "voltijd",
#'   type_hoger_onderwijs_binnen_soort_hoger_onderwijs = "bachelor",
#'   croho_onderdeel_actuele_opleiding = "techniek"
#' )
#' cohort <- tibble::tibble(
#'   persoonsgebonden_nummer = "S1",
#'   inschrijvingsjaar = 2020L,
#'   eerstejaar_instelling = 2020L,
#'   soort_diploma_instelling_label = NA_character_,
#'   opleidingsvorm_label = "voltijd"
#' )
#' diploma <- tibble::tibble(
#'   persoonsgebonden_nummer = character(0),
#'   jaar_eerste_diploma = integer(0),
#'   verblijfsjaar_eerste_diploma = integer(0),
#'   diploma = character(0)
#' )
#' uitval <- tibble::tibble(
#'   persoonsgebonden_nummer = "S1",
#'   uitval_xjr = NA_real_
#' )
#' bereken_studiewissel(basis, cohort, diploma, uitval)
#' @export
bereken_studiewissel <- function(
  basisbestand,
  cohorten_instroom,
  diploma_behaald,
  uitval_indicatoren
) {
  ## Uitval binnen 1 of 3 jaar per student bepalen voor exclusie
  uitval_1_3_jaar <- uitval_indicatoren |>
    dplyr::select(persoonsgebonden_nummer, uitval_xjr) |>
    dplyr::mutate(
      uitval_een_drie = factor(dplyr::case_when(
        uitval_xjr == 1 ~ "binnen 1 jaar",
        uitval_xjr > 1 & uitval_xjr <= 3 ~ "binnen 2- en 3de jaar",
        TRUE ~ "niet uitgevallen eerste drie jaar"
      ))
    )

  ## Koppel diploma en uitvalinfo aan cohort voor zittend-bepaling
  zittend_per_cohort <- cohorten_instroom |>
    dplyr::select(-soort_diploma_instelling_label) |>
    dplyr::left_join(diploma_behaald, by = "persoonsgebonden_nummer") |>
    dplyr::select(
      persoonsgebonden_nummer,
      inschrijvingsjaar,
      eerstejaar_instelling,
      jaar_eerste_diploma,
      opleidingsvorm_label
    ) |>
    dplyr::mutate(
      diploma_na_x_jaar = jaar_eerste_diploma - eerstejaar_instelling + 1,
      diploma_een_drie = factor(dplyr::case_when(
        diploma_na_x_jaar <= 1 ~ "binnen 1 jaar of daarvoor",
        diploma_na_x_jaar > 1 &
          diploma_na_x_jaar <= 3 ~ "binnen 2- en 3de jaar",
        TRUE ~ "geen diploma in eerste drie jaar"
      ))
    ) |>
    dplyr::left_join(uitval_1_3_jaar, by = "persoonsgebonden_nummer")

  ## Bepaal de populatie die een wissel kon maken in het 1e resp. 3e jaar
  zittend_1jr <- zittend_per_cohort |>
    dplyr::filter(diploma_een_drie != "binnen 1 jaar of daarvoor") |>
    dplyr::filter(uitval_een_drie != "binnen 1 jaar")

  zittend_3jr <- zittend_per_cohort |>
    dplyr::filter(diploma_een_drie != "binnen 1 jaar of daarvoor") |>
    dplyr::filter(diploma_een_drie != "binnen 2- en 3de jaar") |>
    dplyr::filter(uitval_een_drie != "binnen 1 jaar") |>
    dplyr::filter(uitval_een_drie != "binnen 2- en 3de jaar")

  wissel_1jr <- bereken_wissel_xjr(
    basisbestand = basisbestand,
    zittend = zittend_1jr,
    verblijfsjaren = c(1, 2),
    doeljaar = 2,
    label_gewisseld = "Gewisseld binnen 1 jaar",
    label_niet = "Niet gewisseld binnen 1 jaar",
    suffix = "1jr"
  )

  wissel_3jr <- bereken_wissel_xjr(
    basisbestand = basisbestand,
    zittend = zittend_3jr,
    verblijfsjaren = c(1, 4),
    doeljaar = 4,
    label_gewisseld = "Gewisseld binnen 3 jaar",
    label_niet = "Niet gewisseld binnen 3 jaar",
    suffix = "3jr"
  )

  cohorten_instroom |>
    dplyr::left_join(wissel_1jr, by = "persoonsgebonden_nummer") |>
    dplyr::left_join(wissel_3jr, by = "persoonsgebonden_nummer") |>
    dplyr::mutate(
      studiewissel_1jr = dplyr::case_when(
        !(persoonsgebonden_nummer %in%
          zittend_1jr$persoonsgebonden_nummer) ~ "Geen switch bepaald",
        TRUE ~ studiewissel_1jr
      ),
      studiewissel_3jr = dplyr::case_when(
        !(persoonsgebonden_nummer %in%
          zittend_3jr$persoonsgebonden_nummer) ~ "Geen switch bepaald",
        TRUE ~ studiewissel_3jr
      )
    ) |>
    dplyr::mutate(
      studiewissel_1jr = forcats::fct_na_value_to_level(
        studiewissel_1jr,
        "Niet gewisseld binnen 1 jaar"
      ),
      studiewissel_3jr = forcats::fct_na_value_to_level(
        studiewissel_3jr,
        "Niet gewisseld binnen 3 jaar"
      )
    ) |>
    dplyr::select(
      persoonsgebonden_nummer,
      dplyr::starts_with("studiewissel"),
      dplyr::contains("switch")
    )
}

#' Bereken studiewissel voor een specifiek verblijfsjaar-paar
#'
#' Hulpfunctie die de switchlogica hergebruikt voor zowel de 1jr- als
#' 3jr-meting. Een wissel wordt vastgesteld als een student in jaar `doeljaar`
#' een andere opleidingscode heeft dan in jaar 1, en het verschil in
#' kalenderjaren gelijk is aan `doeljaar - 1`.
#'
#' @param basisbestand Tibble zoals gemaakt door [maak_basisbestand()]
#' @param zittend Tibble met de populatie waarvoor de wissel bepaald wordt
#' @param verblijfsjaren Integer vector van lengte 2: begin- en eindjaar van
#'   het meetvenster, bijv. `c(1, 2)` of `c(1, 4)`
#' @param doeljaar Integer, het verblijfsjaar waarop de wissel wordt gemeten
#' @param label_gewisseld Character, label als student gewisseld is
#' @param label_niet Character, label als student niet gewisseld is
#' @param suffix Character, achtervoegsel voor de kolomnamen (`"1jr"` of
#'   `"3jr"`)
#'
#' @return Een tibble met alleen de studenten die gewisseld zijn, met kolommen
#'   voor wissel, nieuwe opleidingscode, opleidingsvorm, niveau en sector
bereken_wissel_xjr <- function(
  basisbestand,
  zittend,
  verblijfsjaren,
  doeljaar,
  label_gewisseld,
  label_niet,
  suffix
) {
  bewerkt <- basisbestand |>
    dplyr::filter(
      persoonsgebonden_nummer %in% zittend$persoonsgebonden_nummer
    ) |>
    dplyr::filter(
      verblijfsjaar_actuele_instelling %in% verblijfsjaren,
      soort_inschrijving_actuele_instelling_label ==
        "hoofdinschrijving binnen het domein actuele instelling"
    )

  freq <- bewerkt |> dplyr::count(persoonsgebonden_nummer)

  if (any(freq$n > 2)) {
    rlang::abort(
      "Meer dan twee regels per student in het switchcohortbestand gevonden!"
    )
  }

  persnr_2jr <- freq |> dplyr::filter(.data$n == 2)

  dubbel <- bewerkt |>
    dplyr::filter(
      persoonsgebonden_nummer %in% persnr_2jr$persoonsgebonden_nummer
    )

  if (
    sum(dubbel$verblijfsjaar_actuele_instelling == verblijfsjaren[1]) !=
      sum(dubbel$verblijfsjaar_actuele_instelling == verblijfsjaren[2])
  ) {
    rlang::abort(paste(
      "Aantal verblijfsjaren",
      verblijfsjaren[1],
      "en",
      verblijfsjaren[2],
      "is niet gelijk"
    ))
  }

  wissel_col <- paste0("studiewissel_", suffix)
  opl_col <- paste0("opleidingscode_na_switch", suffix)
  vorm_col <- paste0("opleidingsvorm_na_switch", suffix)
  niveau_col <- paste0("opleidingsniveau_na_switch", suffix)
  sector_col <- paste0("sector_na_switch", suffix)

  dubbel |>
    dplyr::arrange(persoonsgebonden_nummer, verblijfsjaar_actuele_instelling) |>
    dplyr::mutate(
      persnr_dubbel = dplyr::if_else(
        persoonsgebonden_nummer == dplyr::lag(persoonsgebonden_nummer),
        "zelfde student",
        "andere student"
      ),
      opl_dubbel = dplyr::if_else(
        opleiding_actueel_equivalent ==
          dplyr::lag(opleiding_actueel_equivalent),
        "zelfde opleiding",
        "andere opleiding"
      ),
      !!wissel_col := dplyr::if_else(
        verblijfsjaar_actuele_instelling == doeljaar &
          persnr_dubbel == "zelfde student" &
          opl_dubbel == "andere opleiding",
        label_gewisseld,
        label_niet
      ),
      verschil_kalenderjaren = inschrijvingsjaar -
        dplyr::lag(inschrijvingsjaar),
      !!opl_col := dplyr::if_else(
        .data[[wissel_col]] == label_gewisseld,
        as.character(opleiding_actueel_equivalent),
        NA_character_
      ),
      !!vorm_col := dplyr::if_else(
        .data[[wissel_col]] == label_gewisseld,
        as.character(opleidingsvorm),
        NA_character_
      ),
      !!niveau_col := dplyr::if_else(
        .data[[wissel_col]] == label_gewisseld,
        as.character(type_hoger_onderwijs_binnen_soort_hoger_onderwijs),
        NA_character_
      ),
      !!sector_col := dplyr::if_else(
        .data[[wissel_col]] == label_gewisseld,
        as.character(croho_onderdeel_actuele_opleiding),
        NA_character_
      )
    ) |>
    dplyr::mutate(dplyr::across(
      c(!!wissel_col, !!opl_col, !!vorm_col, !!niveau_col, !!sector_col),
      factor
    )) |>
    dplyr::select(
      persoonsgebonden_nummer,
      verschil_kalenderjaren,
      !!wissel_col,
      !!opl_col,
      !!vorm_col,
      !!niveau_col,
      !!sector_col
    ) |>
    dplyr::filter(
      .data[[wissel_col]] == label_gewisseld,
      verschil_kalenderjaren == (doeljaar - 1)
    )
}
