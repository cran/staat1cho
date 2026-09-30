#' Maak een geaggregeerd benchmarkrapport
#'
#' Aggregeert het indicatorenbestand per sector, opleidingsvorm,
#' opleidingsniveau en instroomjaar. Berekent percentages voor de
#' kernuitkomsten en voegt een totaalrij per jaar toe.
#'
#' ## Onvolledige cohorten
#'
#' Percentages worden berekend over de waarneembare rijen: studenten met
#' `"Nog niet waarneembaar"` (cohort te recent voor het meetvenster) tellen
#' niet mee in teller of noemer. Is niemand in een groep waarneembaar, dan is
#' het percentage `NA`.
#'
#' ## Privacyonderdrukking
#'
#' - **Primair:** groepen met minder dan `drempel` studenten (standaard 30)
#'   krijgen `NA` voor alle uitkomsten, inclusief `n`.
#' - **Secundair:** is in een instroomjaar precies één groep primair
#'   onderdrukt, dan is die groep terug te rekenen uit de totaalrij min de
#'   zichtbare groepen. Daarom wordt dan ook de kleinste zichtbare groep in
#'   dat jaar onderdrukt.
#' - **Cellen:** een percentage wordt `NA` als de teller of het complement
#'   (noemer - teller) kleiner is dan `min_cel` (standaard 5), zodat
#'   bijvoorbeeld "0% uitval" in een groep niets over iedereen in die groep
#'   verraadt. Zet `min_cel = 0` om dit uit te zetten.
#' - **Secundair per cel:** is in een instroomjaar precies één groep leeg
#'   voor een percentage, dan is dat percentage terug te rekenen uit de
#'   totaalrij. Dan wordt hetzelfde percentage ook leeggemaakt in de kleinste
#'   groep in dat jaar waar het nog zichtbaar is.
#' - Percentages worden afgerond op hele procenten, gemiddelden op één
#'   decimaal.
#'
#' De drempel van 30 is een eigen keuze van het project, geen voorgeschreven
#' norm. Laat een privacy officer of FG van de deelnemende instellingen
#' `drempel` en `min_cel` toetsen voordat rapporten gedeeld worden.
#'
#' De kolom `onderdrukt` maakt zichtbaar welke rijen zijn onderdrukt
#' (primair of secundair) zonder iets te onthullen over de werkelijke omvang.
#'
#' ## Peildatum
#'
#' Elke rij krijgt de `peildatum` van de data: standaard 1 oktober van het
#' laatste inschrijvingsjaar (het attribuut `peildatum` dat
#' [combineer_indicatoren()] zet). Cijfers van eerdere cohorten kunnen bij een
#' latere peildatum veranderen, bijvoorbeeld als een student na een tussenjaar
#' terugkeert en dan niet meer als uitgevallen telt. Vergelijk daarom alleen
#' rapporten met dezelfde peildatum, en vermeld de peildatum bij elk gedeeld
#' cijfer.
#'
#' ## Samenstelling instroom
#'
#' Als het analysebestand `vooropleiding` en `eerstejaars_ho` bevat (zie
#' [combineer_indicatoren()]), geeft het rapport per groep het aandeel havo-,
#' vwo- en mbo-instromers en het aandeel eerstejaars HO. Onbekende waarden
#' tellen niet mee in de noemer. Verschillen in rendement en uitval tussen
#' instellingen hangen sterk samen met deze samenstelling.
#'
#' ## Gebruik als validatiemiddel
#'
#' CEDA kan met dit rapport controleren of de tool correct werkt:
#'
#' - **Totalen vs. DUO-publicaties**: `n` in de totaalrij per instroomjaar
#'   moet overeenkomen met het aantal studenten dat DUO voor die instelling
#'   en dat jaar publiceert. Een grote afwijking wijst op een verwerkingsfout.
#' - **Plausibele marges**: uitvalpercentages buiten circa 5-40 % of
#'   rendement van 0 % zijn een signaal om de pipelinestap te controleren.
#' - **Reproduceerbaarheid**: hetzelfde invoerbestand moet op elk moment
#'   identieke cijfers opleveren.
#' - **Optionele bestanden**: als VLPBEK of VAKHAVW is geladen, moeten de
#'   bijbehorende kolommen (`pct_bekostigd`, `gem_eindcijfer`) voor de meeste
#'   rijen gevuld zijn. Kolommen die volledig leeg zijn duiden op een
#'   koppelfout.
#'
#' @param indicatoren Tibble zoals gemaakt door [combineer_indicatoren()],
#'   eventueel aangevuld via [verrijk_met_vakhawv()] of
#'   [verrijk_met_bekostiging()].
#' @param drempel Minimale groepsgrootte. Groepen kleiner dan deze waarde
#'   krijgen NA voor alle kolommen inclusief `n`. Standaard 30.
#' @param min_cel Minimale celgrootte voor percentages (zie hierboven).
#'   Standaard 5; 0 zet celonderdrukking uit.
#' @param niveau Analyseniveau waarop `indicatoren` is gemaakt (`"student"`
#'   of `"inschrijving"`). Standaard het attribuut `niveau` dat
#'   [combineer_indicatoren()] zet. Wordt vastgelegd in de metadata, zodat
#'   rapporten van verschillende instellingen vergelijkbaar blijven.
#' @param peildatum Peildatum van de data (`Date` of `"JJJJ-MM-DD"`).
#'   Standaard het attribuut `peildatum` dat [combineer_indicatoren()] zet;
#'   ontbreekt dat, dan 1 oktober van het laatste instroomjaar in
#'   `indicatoren`.
#'
#' @return Een tibble met kolommen `peildatum`, `sector`, `opleidingsvorm`,
#'   `opleidingsniveau`, `inschrijvingsjaar`, `onderdrukt`, `n`,
#'   `pct_uitval_1jr`, `pct_uitval_3jr`, `pct_rendement_3jr`,
#'   `pct_rendement_5jr`, `pct_rendement_8jr`, `pct_studiewissel_1jr`,
#'   `pct_studiewissel_3jr`, `pct_int_student`, `gem_leeftijd_instroom`,
#'   plus optioneel `pct_eerstejaars_ho`, `pct_vooropl_havo`,
#'   `pct_vooropl_vwo`, `pct_vooropl_mbo`, `gem_eindcijfer`,
#'   `gem_wiskundecijfer`,
#'   `gem_aantal_vakken`, `pct_bekostigd`, `pct_hoofdinschrijving` en
#'   `pct_herstelbaar` (van de niet-bekostigde rijen: percentage waarvan de
#'   reden een te late aanlevering is en dus hersteld kan worden, zie
#'   [BEKOSTIGINGSTATUS_CODES]). Attributen: `gegenereerd_op` (POSIXct) en
#'   `metadata` (lijst met peildatum, niveau, drempel, min_cel, laatste
#'   inschrijvingsjaar, geladen optionele bestanden, packageversie en
#'   tijdstip).
#'
#' @seealso [schrijf_benchmarkrapport()] om het rapport met toelichting als
#'   Excel-bestand op te slaan.
#'
#' @examples
#' df <- tibble::tibble(
#'   sector          = "gezondheidszorg",
#'   opleidingsvorm  = "voltijd",
#'   opleidingsniveau = "bachelor",
#'   inschrijvingsjaar = 2022L,
#'   uitval_1jr      = factor("Na 1 jaar nog ingeschreven of diploma behaald"),
#'   uitval_3jr      = factor("Na 3 jaar nog ingeschreven of diploma behaald"),
#'   rendement_3jr   = factor("Geen diploma"),
#'   rendement_5jr   = factor("Geen diploma"),
#'   rendement_8jr   = factor("Geen diploma"),
#'   int_student     = "geen internationale student",
#'   leeftijd_bij_instroom = 19L
#' )
#' maak_benchmarkrapport(df, drempel = 1L, niveau = "student")
#' @export
maak_benchmarkrapport <- function(
  indicatoren,
  drempel = 30L,
  min_cel = 5L,
  niveau = attr(indicatoren, "niveau"),
  peildatum = attr(indicatoren, "peildatum")
) {
  if (is.null(niveau)) {
    niveau <- "onbekend"
  }
  if (is.null(peildatum)) {
    peildatum <- peildatum_1cho(max(indicatoren$inschrijvingsjaar, na.rm = TRUE))
  }
  peildatum <- as.Date(peildatum)
  heeft_studiewissel  <- "studiewissel_1jr"             %in% names(indicatoren)
  heeft_vooropl       <- "vooropleiding"                 %in% names(indicatoren)
  heeft_eerstejaars   <- "eerstejaars_ho"                %in% names(indicatoren)
  heeft_vakhawv       <- "vakhawv_gemiddeld_eindcijfer"  %in% names(indicatoren)
  heeft_wiskunde      <- "vakhawv_wiskundecijfer"        %in% names(indicatoren)
  heeft_aantal_vakken <- "vakhawv_aantal_vakken"         %in% names(indicatoren)
  heeft_bekostiging   <- "indicatie_bekostigd"           %in% names(indicatoren)
  heeft_hoofdinschr   <- "indicatie_hoofdinschrijving"   %in% names(indicatoren)
  heeft_herstelbaar   <- "indicatie_herstelbaar"         %in% names(indicatoren)

  ## Aandeel TRUE over de niet-NA waarden. NA betekent "telt niet mee in de
  ## noemer" (niet waarneembaar, niet gekoppeld of niet van toepassing).
  ## Met min_cel > 0 wordt een aandeel NA als teller of complement te klein
  ## is om nog iets over individuele studenten te zeggen.
  .aandeel <- function(treffer) {
    noemer <- sum(!is.na(treffer))
    if (noemer == 0) {
      return(NA_real_)
    }
    teller <- sum(treffer, na.rm = TRUE)
    if (min_cel > 0 && (teller < min_cel || noemer - teller < min_cel)) {
      return(NA_real_)
    }
    teller / noemer * 100
  }

  ## Percentage over de waarneembare rijen: studenten in cohorten waarvan het
  ## meetvenster nog niet in de data zit tellen niet mee in de noemer.
  .pct <- function(x, label) {
    x <- as.character(x)
    .aandeel(dplyr::if_else(x == NIET_WAARNEEMBAAR, NA, x == label))
  }

  ## Aandeel in de samenstelling: "onbekend" telt niet mee in de noemer
  .samenstelling <- function(x, label) {
    x <- as.character(x)
    .aandeel(dplyr::if_else(x == "onbekend", NA, x == label))
  }

  .groepeer <- function(data) {
    data |>
      dplyr::summarise(
        n                    = dplyr::n(),
        pct_uitval_1jr       = .pct(uitval_1jr,  "Uitgevallen binnen 1 jaar"),
        pct_uitval_3jr       = .pct(uitval_3jr,  "Uitgevallen binnen 3 jaar"),
        pct_rendement_3jr    = .pct(rendement_3jr, "Diploma binnen 3 jaar"),
        pct_rendement_5jr    = .pct(rendement_5jr, "Diploma binnen 5 jaar"),
        pct_rendement_8jr    = .pct(rendement_8jr, "Diploma binnen 8 jaar"),
        pct_studiewissel_1jr = if (heeft_studiewissel) .pct(studiewissel_1jr, "Gewisseld binnen 1 jaar") else NA_real_,
        pct_studiewissel_3jr = if (heeft_studiewissel) .pct(studiewissel_3jr, "Gewisseld binnen 3 jaar") else NA_real_,
        pct_int_student      = .pct(int_student, "internationale student"),
        gem_leeftijd_instroom = mean(leeftijd_bij_instroom, na.rm = TRUE),
        pct_eerstejaars_ho    = if (heeft_eerstejaars) .samenstelling(eerstejaars_ho, "eerstejaars HO") else NA_real_,
        pct_vooropl_havo      = if (heeft_vooropl)     .samenstelling(vooropleiding, "havo") else NA_real_,
        pct_vooropl_vwo       = if (heeft_vooropl)     .samenstelling(vooropleiding, "vwo")  else NA_real_,
        pct_vooropl_mbo       = if (heeft_vooropl)     .samenstelling(vooropleiding, "mbo")  else NA_real_,
        gem_eindcijfer       = if (heeft_vakhawv)      mean(vakhawv_gemiddeld_eindcijfer, na.rm = TRUE) else NA_real_,
        gem_wiskundecijfer    = if (heeft_wiskunde)      mean(vakhawv_wiskundecijfer,       na.rm = TRUE) else NA_real_,
        gem_aantal_vakken     = if (heeft_aantal_vakken) mean(vakhawv_aantal_vakken,        na.rm = TRUE) else NA_real_,
        pct_bekostigd         = if (heeft_bekostiging)   .aandeel(indicatie_bekostigd)         else NA_real_,
        pct_hoofdinschrijving = if (heeft_hoofdinschr)   .aandeel(indicatie_hoofdinschrijving) else NA_real_,
        ## indicatie_herstelbaar is NA voor bekostigde rijen, dus dit beperkt
        ## zich automatisch tot de niet-bekostigde rijen.
        pct_herstelbaar       = if (heeft_herstelbaar)   .aandeel(indicatie_herstelbaar)       else NA_real_,
        .groups = "drop"
      )
  }

  ## Per sector x opleidingsvorm x opleidingsniveau x instroomjaar
  per_groep <- indicatoren |>
    dplyr::group_by(sector, opleidingsvorm, opleidingsniveau, inschrijvingsjaar) |>
    .groepeer()

  ## Totaalrij per instroomjaar voor validatie tegen DUO-publicaties
  per_jaar <- indicatoren |>
    dplyr::group_by(inschrijvingsjaar) |>
    .groepeer() |>
    dplyr::mutate(
      sector           = "totaal",
      opleidingsvorm   = NA_character_,
      opleidingsniveau = NA_character_,
      .before = inschrijvingsjaar
    )

  resultaat <- dplyr::bind_rows(per_groep, per_jaar) |>
    dplyr::arrange(inschrijvingsjaar, sector, opleidingsvorm, opleidingsniveau)

  ## Privacyonderdrukking: groepen kleiner dan drempel krijgen nergens een
  ## waarde, ook niet voor n. De kolom `onderdrukt` laat zien welke rijen
  ## zijn weggelaten zonder iets over de werkelijke omvang te onthullen.
  primair <- resultaat$n < drempel

  ## Secundair: met precies één onderdrukte groep in een jaar is die groep
  ## terug te rekenen als totaalrij min zichtbare groepen. Onderdruk dan ook
  ## de kleinste zichtbare groep in dat jaar.
  secundair <- rep(FALSE, nrow(resultaat))
  is_groep <- resultaat$sector != "totaal"
  for (j in unique(resultaat$inschrijvingsjaar)) {
    in_jaar <- resultaat$inschrijvingsjaar %in% j
    groepen <- which(is_groep & in_jaar)
    totaal_zichtbaar <- any(!primair[!is_groep & in_jaar])
    if (totaal_zichtbaar && length(groepen) > 1 && sum(primair[groepen]) == 1) {
      zichtbaar <- groepen[!primair[groepen]]
      secundair[zichtbaar[which.min(resultaat$n[zichtbaar])]] <- TRUE
    }
  }
  te_klein <- primair | secundair

  uitkomstkolommen <- c(
    "n",
    "pct_uitval_1jr", "pct_uitval_3jr",
    "pct_rendement_3jr", "pct_rendement_5jr", "pct_rendement_8jr",
    "pct_studiewissel_1jr", "pct_studiewissel_3jr",
    "pct_int_student", "gem_leeftijd_instroom",
    "pct_eerstejaars_ho", "pct_vooropl_havo", "pct_vooropl_vwo", "pct_vooropl_mbo",
    "gem_eindcijfer", "gem_wiskundecijfer", "gem_aantal_vakken",
    "pct_bekostigd", "pct_hoofdinschrijving", "pct_herstelbaar"
  )
  aanwezige_uitkomsten <- intersect(uitkomstkolommen, names(resultaat))

  resultaat <- resultaat |>
    dplyr::mutate(
      onderdrukt = te_klein,
      dplyr::across(
        dplyr::all_of(aanwezige_uitkomsten),
        ~ dplyr::if_else(te_klein, NA_real_, as.double(.x))
      ),
      .before = "n"
    )

  ## Secundair per cel: hetzelfde terugrekenen werkt per percentage. Een
  ## percentage dat door min_cel (of rijonderdrukking) leeg is in precies één
  ## groep, volgt uit totaalrij min zichtbare groepen. Maak dan ook de
  ## kleinste groep met een zichtbare waarde leeg.
  pct_kolommen <- grep("^pct_", aanwezige_uitkomsten, value = TRUE)
  for (j in unique(resultaat$inschrijvingsjaar)) {
    in_jaar <- resultaat$inschrijvingsjaar %in% j
    totaal <- which(!is_groep & in_jaar)
    groepen <- which(is_groep & in_jaar)
    for (kol in pct_kolommen) {
      waarden <- resultaat[[kol]]
      if (length(totaal) == 0 || all(is.na(waarden[totaal]))) {
        next
      }
      leeg <- groepen[is.na(waarden[groepen])]
      gevuld <- groepen[!is.na(waarden[groepen])]
      if (length(leeg) == 1 && length(gevuld) > 0) {
        resultaat[[kol]][gevuld[which.min(resultaat$n[gevuld])]] <- NA_real_
      }
    }
  }

  ## Verwijder optionele kolommen die volledig leeg zijn (optioneel bestand
  ## was niet geladen of koppeling leverde geen matches op)
  altijd_na  <- function(col) all(is.na(resultaat[[col]]))
  optioneel  <- c(
    "pct_studiewissel_1jr", "pct_studiewissel_3jr",
    "pct_eerstejaars_ho", "pct_vooropl_havo", "pct_vooropl_vwo", "pct_vooropl_mbo",
    "gem_eindcijfer", "gem_wiskundecijfer", "gem_aantal_vakken",
    "pct_bekostigd", "pct_hoofdinschrijving", "pct_herstelbaar"
  )
  resultaat  <- dplyr::select(
    resultaat, -dplyr::all_of(Filter(altijd_na, optioneel))
  )

  ## Afronden: hele procenten en gemiddelden op één decimaal. Meer precisie
  ## voegt bij groepen van 30+ niets toe en maakt terugrekenen makkelijker.
  resultaat <- resultaat |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("pct_"), ~ round(.x)),
      dplyr::across(dplyr::starts_with("gem_"), ~ round(.x, 1))
    )

  ## Peildatum als eerste kolom, zodat die meegaat als rapporten van
  ## verschillende instellingen worden samengevoegd
  resultaat <- dplyr::mutate(resultaat, peildatum = peildatum, .before = 1)

  optionele_bestanden <- c(
    VAKHAVW = heeft_vakhawv,
    VLPBEK = heeft_bekostiging
  )
  gegenereerd_op <- Sys.time()
  attr(resultaat, "gegenereerd_op") <- gegenereerd_op
  attr(resultaat, "metadata") <- list(
    peildatum = peildatum,
    niveau = niveau,
    drempel = drempel,
    min_cel = min_cel,
    laatste_inschrijvingsjaar = suppressWarnings(
      max(indicatoren$inschrijvingsjaar, na.rm = TRUE)
    ),
    optionele_bestanden = names(optionele_bestanden)[optionele_bestanden],
    packageversie = as.character(utils::packageVersion("staat1cho")),
    gegenereerd_op = gegenereerd_op
  )
  resultaat
}

#' Sla een benchmarkrapport op als Excel-bestand
#'
#' Schrijft het rapport uit [maak_benchmarkrapport()] naar een `.xlsx` met
#' vier tabbladen: `Rapport`, `Toelichting` (per kolom, afgestemd op
#' analyseniveau en drempel), `Validatie` (controlepunten voor CEDA) en
#' `Metadata` (peildatum, niveau, drempel, packageversie, laatste
#' inschrijvingsjaar).
#' Vereist het package `writexl`.
#'
#' @param rapport Tibble zoals gemaakt door [maak_benchmarkrapport()]
#' @param pad Pad van het uitvoerbestand (`.xlsx`)
#'
#' @return `pad`, onzichtbaar
#'
#' @examples
#' df <- tibble::tibble(
#'   sector = "gezondheidszorg", opleidingsvorm = "voltijd",
#'   opleidingsniveau = "bachelor", inschrijvingsjaar = 2022L,
#'   uitval_1jr = factor("Uitgevallen binnen 1 jaar"),
#'   uitval_3jr = factor("Uitgevallen binnen 3 jaar"),
#'   rendement_3jr = factor("Geen diploma"),
#'   rendement_5jr = factor("Geen diploma"),
#'   rendement_8jr = factor("Geen diploma"),
#'   int_student = "geen internationale student",
#'   leeftijd_bij_instroom = 19L
#' )
#' if (requireNamespace("writexl", quietly = TRUE)) {
#'   rapport <- maak_benchmarkrapport(df, drempel = 1L, niveau = "student")
#'   schrijf_benchmarkrapport(rapport, tempfile(fileext = ".xlsx"))
#' }
#' @export
schrijf_benchmarkrapport <- function(rapport, pad) {
  rlang::check_installed("writexl", reason = "om het benchmarkrapport als Excel op te slaan.")
  writexl::write_xlsx(
    list(
      Rapport = rapport,
      Toelichting = benchmark_toelichting(rapport),
      Validatie = benchmark_validatie(rapport),
      Metadata = benchmark_metadata(rapport)
    ),
    pad
  )
  invisible(pad)
}

## Eenheid in teksten: op inschrijvingsniveau gaat het om inschrijvingen
benchmark_eenheid <- function(rapport) {
  niveau <- attr(rapport, "metadata")$niveau
  if (identical(niveau, "inschrijving")) "inschrijvingen" else "studenten"
}

benchmark_toelichting <- function(rapport) {
  meta <- attr(rapport, "metadata")
  eenheid <- benchmark_eenheid(rapport)
  drempel <- if (is.null(meta$drempel)) 30L else meta$drempel
  waarneembaar <- paste0(
    "Berekend over ", eenheid, " in cohorten waarvan het meetvenster al in ",
    "de data zit; recentere cohorten tellen niet mee"
  )

  toelichting <- tibble::tribble(
    ~Kolom, ~Omschrijving, ~Opmerking,
    "peildatum", "Peildatum van de 1CHO-data waarop het rapport is gebaseerd",
    "Cijfers van eerdere cohorten kunnen bij een latere peildatum veranderen; vergelijk alleen rapporten met dezelfde peildatum",
    "sector","Onderwijs-CROHO-sector (bijv. gezondheidszorg, techniek)",
    "Rij met sector = 'totaal' is het totaal over alle groepen voor dat instroomjaar",
    "opleidingsvorm", "Voltijd, deeltijd of duaal", "",
    "opleidingsniveau", "Associate degree, bachelor of master", "",
    "inschrijvingsjaar", "Instroomjaar (cohort)", "",
    "onderdrukt",
    paste0("TRUE = rij is onderdrukt wegens een te kleine groep (n < ", drempel,
           ") of om terugrekenen via de totaalrij te voorkomen; alle uitkomsten zijn leeg"),
    "",
    "n", paste("Aantal", eenheid, "in de groep; leeg bij onderdrukking"), "",
    "pct_uitval_1jr", paste("%", eenheid, "uitgevallen binnen 1 jaar na instroom"), waarneembaar,
    "pct_uitval_3jr", paste("%", eenheid, "uitgevallen binnen 3 jaar na instroom"), waarneembaar,
    "pct_rendement_3jr", paste("%", eenheid, "met diploma binnen 3 jaar"), waarneembaar,
    "pct_rendement_5jr", paste("%", eenheid, "met diploma binnen 5 jaar"), waarneembaar,
    "pct_rendement_8jr", paste("%", eenheid, "met diploma binnen 8 jaar"), waarneembaar,
    "pct_studiewissel_1jr", "% studenten gewisseld van opleiding binnen 1 jaar",
    paste0("Alleen op studentniveau. ", waarneembaar),
    "pct_studiewissel_3jr", "% studenten gewisseld van opleiding binnen 3 jaar",
    paste0("Alleen op studentniveau. ", waarneembaar),
    "pct_int_student", paste("% internationale", eenheid, "in de groep"), "",
    "gem_leeftijd_instroom", "Gemiddelde leeftijd bij instroom", "",
    "pct_eerstejaars_ho", paste("%", eenheid, "waarvoor het instroomjaar ook het eerste jaar in het hoger onderwijs is"),
    "Onbekend telt niet mee in de noemer",
    "pct_vooropl_havo", paste("%", eenheid, "met havo als hoogste vooropleiding voor het HO"),
    "Onbekend telt niet mee in de noemer",
    "pct_vooropl_vwo", paste("%", eenheid, "met vwo als hoogste vooropleiding voor het HO"),
    "Onbekend telt niet mee in de noemer",
    "pct_vooropl_mbo", paste("%", eenheid, "met mbo als hoogste vooropleiding voor het HO"),
    "Onbekend telt niet mee in de noemer",
    "gem_eindcijfer", "Gemiddeld eindcijfer vooropleiding",
    "Alleen als een VAKHAVW-bestand is geladen; gemiddelde over het hoogste eindcijfer per student",
    "gem_wiskundecijfer", "Gemiddeld centraal examencijfer wiskunde",
    "Alleen als een VAKHAVW-bestand is geladen",
    "gem_aantal_vakken", "Gemiddeld aantal vakken op de eindlijst",
    "Alleen als een VAKHAVW-bestand is geladen",
    "pct_bekostigd", paste("% gekoppelde", eenheid, "met bekostigingsstatus 'bekostigd' of 'deels bekostigd'"),
    "Alleen als een VLPBEK-bestand is geladen; berekend over de gekoppelde rijen",
    "pct_hoofdinschrijving", paste("% gekoppelde", eenheid, "met DUO-bekostigingsindicatie 'J'"),
    "Alleen als een VLPBEK-bestand is geladen",
    "pct_herstelbaar", "% van de niet-bekostigde inschrijvingen waarvan alle redenen herstelbaar zijn (te late aanlevering)",
    "Alleen als een VLPBEK-bestand is geladen. Zie BEKOSTIGINGSTATUS_CODES in het R-pakket"
  )
  ## Celonderdrukking geldt voor alle percentages
  min_cel <- if (is.null(meta$min_cel)) 0L else meta$min_cel
  if (min_cel > 0) {
    cel <- paste0(
      "Leeg als minder dan ", min_cel, " ", eenheid, " het wel of juist niet betreft, ",
      "of om terugrekenen via de totaalrij te voorkomen"
    )
    is_pct <- startsWith(toelichting$Kolom, "pct_")
    toelichting$Opmerking[is_pct] <- ifelse(
      toelichting$Opmerking[is_pct] == "",
      cel,
      paste0(toelichting$Opmerking[is_pct], ". ", cel)
    )
  }
  toelichting[toelichting$Kolom %in% names(rapport), ]
}

benchmark_validatie <- function(rapport) {
  eenheid <- benchmark_eenheid(rapport)
  meta <- attr(rapport, "metadata")
  tijdstip <- if (is.null(meta$gegenereerd_op)) attr(rapport, "gegenereerd_op") else meta$gegenereerd_op
  tibble::tibble(
    Controlepunt = c(
      "Totaal n per instroomjaar",
      "Uitvalpercentages",
      "Rendementpercentages",
      "Recente cohorten",
      "Optionele kolommen",
      "Peildatum",
      "Reproduceerbaarheid"
    ),
    Toelichting = c(
      paste0(
        "De rij met sector = 'totaal' geeft het totaal aantal ", eenheid,
        " per instroomjaar. Vergelijk dit met de DUO-open-data voor dezelfde instelling ",
        "en hetzelfde jaar. Een afwijking van meer dan 1% wijst op een verwerkingsfout."
      ),
      "Uitval binnen 1 jaar ligt voor de meeste hbo-instellingen tussen 5% en 30%. Waarden buiten dit bereik rechtvaardigen een controle van de uitvalstap in de pipeline.",
      "Rendement binnen 5 jaar ligt doorgaans tussen 40% en 80% voor bachelor. Waarden van 0% of 100% zijn verdacht.",
      "Lege percentages bij de laatste cohorten zijn verwacht: die cohorten zitten nog niet lang genoeg in de data voor het meetvenster.",
      "Als VLPBEK of VAKHAVW is geladen, moeten pct_bekostigd resp. gem_eindcijfer aanwezig en voor de meeste rijen gevuld zijn. Kolommen die volledig leeg zijn duiden op een koppelfout (bijv. andere ID's of onjuiste opleidingscode).",
      paste0(
        "Peildatum: ", if (is.null(meta$peildatum)) "onbekend" else format(meta$peildatum, "%Y-%m-%d"),
        ". Vergelijk alleen rapporten met dezelfde peildatum: bij een latere peildatum kunnen cijfers van ",
        "eerdere cohorten veranderen, bijvoorbeeld als een student na een tussenjaar terugkeert."
      ),
      paste0(
        "Gegenereerd op: ", format(tijdstip, "%Y-%m-%d %H:%M:%S"),
        ". Hetzelfde invoerbestand moet altijd dezelfde uitkomsten geven."
      )
    )
  )
}

benchmark_metadata <- function(rapport) {
  meta <- attr(rapport, "metadata")
  if (is.null(meta)) {
    meta <- list()
  }
  waarde <- function(x) if (is.null(x)) "onbekend" else as.character(x)
  tibble::tibble(
    Kenmerk = c(
      "Peildatum", "Analyseniveau", "Drempel groepsgrootte", "Minimale celgrootte",
      "Laatste inschrijvingsjaar in de data", "Optionele bestanden", "Versie staat1cho",
      "Gegenereerd op", "Onderdrukkingsregels"
    ),
    Waarde = c(
      if (is.null(meta$peildatum)) "onbekend" else format(meta$peildatum, "%Y-%m-%d"),
      waarde(meta$niveau), waarde(meta$drempel), waarde(meta$min_cel),
      waarde(meta$laatste_inschrijvingsjaar),
      if (length(meta$optionele_bestanden) == 0) "geen" else paste(meta$optionele_bestanden, collapse = ", "),
      waarde(meta$packageversie),
      if (is.null(meta$gegenereerd_op)) "onbekend" else format(meta$gegenereerd_op, "%Y-%m-%d %H:%M:%S"),
      paste(
        "Drempel en minimale celgrootte zijn een eigen keuze van het project, geen",
        "voorgeschreven norm. Groepen onder de drempel en percentages onder de",
        "celgrootte zijn leeg; blijft daardoor in een jaar maar 1 groep of cel leeg,",
        "dan wordt ook de kleinste zichtbare groep of cel leeggemaakt."
      )
    )
  )
}
