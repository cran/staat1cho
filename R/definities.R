#' Beschrijvingen van de studie-indicatoren
#'
#' Korte definities van alle indicatoren die het package berekent, uitgesplitst
#' naar analyseniveau. Afgeleid uit de berekeningslogica in
#' [maak_instroom_cohort()], [bereken_rendement()], [bereken_uitval()] en
#' [bereken_studiewissel()]. Bedoeld voor gebruik als tooltip-tekst in
#' dashboards.
#'
#' Gebruik `DEFINITIES[["student"]]$instroom` of
#' `DEFINITIES[["inschrijving"]]$instroom` om de juiste definitie op te halen.
#'
#' @format Een nested named list met sub-lijsten `student` en `inschrijving`
#' @export
DEFINITIES <- list(
  student = list(
    filter_instroom =
      "Op studentniveau filteren opleiding, sector en locatie op de
      opleiding waarin de student is ingestroomd. Alle uitkomsten gelden
      voor de hele instelling: een student die wisselt en in een andere
      opleiding een diploma haalt, telt bij de instroomopleiding als
      'Diploma behaald' en niet als uitgevallen. Studenten die in een
      opleiding instromen na een wissel zijn daar niet te zien. Kies
      inschrijvingsniveau om per opleiding te meten.",

    instroom =
      "Eerstejaars studenten aan de instelling: ingeschreven als
      hoofdinschrijving en voor het eerst aan deze instelling
      (verblijfsjaar aan de instelling = 1). Een student telt slechts
      eenmaal mee, ongeacht hoeveel opleidingen zij volgen.",

    status =
      "Eindstatus van de student na de observatieperiode.
      'Diploma behaald': behaalde een hoofd- of nevendiploma aan de
      instelling (associate degree, bachelor, master, doctoraal,
      beroepsfase of postinitiele master; excl. propedeuse), in welke
      opleiding dan ook. 'Zittend': nog ingeschreven,
      geen diploma behaald. 'Uitgevallen': niet meer ingeschreven en
      geen diploma behaald.",

    rendement_3jr =
      "Percentage studenten dat een diploma behaalde binnen 3 academische
      jaren na instroom aan de instelling. Berekend als:
      diplomajaar - instroomjaar + 1 <= 3.
      Cohorten die nog geen 3 jaar in de data zitten zijn
      'Nog niet waarneembaar' en tellen niet mee in het percentage.",

    rendement_5jr =
      "Percentage studenten dat een diploma behaalde binnen 5 academische
      jaren na instroom aan de instelling.
      Cohorten die nog geen 5 jaar in de data zitten zijn
      'Nog niet waarneembaar' en tellen niet mee in het percentage.",

    rendement_8jr =
      "Percentage studenten dat een diploma behaalde binnen 8 academische
      jaren na instroom. Dit is de maximale observatietermijn.
      Cohorten die nog geen 8 jaar in de data zitten zijn
      'Nog niet waarneembaar' en tellen niet mee in het percentage.",

    uitval_1jr =
      "Percentage studenten dat na het eerste jaar niet meer ingeschreven
      is aan de instelling en geen diploma heeft behaald. Studenten die
      naar een andere opleiding binnen de instelling zijn overgestapt
      tellen niet mee als uitgevallen.
      Uitval binnen 1 jaar is pas vast te stellen als instroomjaar + 1
      in de data zit; recentere cohorten zijn 'Nog niet waarneembaar'
      en tellen niet mee.",

    uitval_3jr =
      "Percentage studenten dat binnen 3 jaar na instroom aan de
      instelling niet meer ingeschreven is en geen diploma heeft behaald.
      Telt cumulatief: ook studenten die al in jaar 1 uitvielen.
      Uitval binnen 3 jaar is pas vast te stellen als instroomjaar + 3
      in de data zit; recentere cohorten zijn 'Nog niet waarneembaar'
      en tellen niet mee.",

    studiewissel_1jr =
      "Percentage studenten dat na jaar 1 een andere opleiding volgt dan
      bij instroom. Vastgesteld door de opleiding in verblijfsjaar 2 te
      vergelijken met verblijfsjaar 1.
      Cohorten waarvan verblijfsjaar 2 nog niet in de data zit zijn
      'Nog niet waarneembaar' en tellen niet mee.",

    studiewissel_3jr =
      "Percentage studenten dat uiterlijk in jaar 4 naar een andere
      opleiding is overgestapt, gemeten bij verblijfsjaar 4 ten opzichte
      van verblijfsjaar 1.
      Cohorten waarvan verblijfsjaar 4 nog niet in de data zit zijn
      'Nog niet waarneembaar' en tellen niet mee.",

    vooropleiding =
      "Hoogste vooropleiding voor het hoger onderwijs volgens DUO,
      samengevat tot havo, vwo, mbo, ho, buitenlands (incl. Europees
      baccalaureaat), overig (o.a. vmbo, toelatingsexamen, beschikking)
      of onbekend.",

    eerstejaars_ho =
      "'eerstejaars HO': het instroomjaar aan de instelling is ook het
      eerste jaar in het hoger onderwijs. 'eerder in HO': de student stond
      al eerder in het hoger onderwijs ingeschreven, bijvoorbeeld bij een
      andere instelling. Kies 'eerstejaars HO' om te vergelijken met
      landelijke cijfers over eerstejaars in het hoger onderwijs.",

    ## VAKHAVW (identiek op beide niveaus: beschrijft de student)
    vakhawv_gemiddeld_eindcijfer =
      "Gemiddeld eindcijfer van de vooropleiding (schaal 1-10), afkomstig
      uit het VAKHAVW-bestand. Gebaseerd op het hoogste gemiddelde eindcijfer
      van de cijferlijst. Alleen beschikbaar als een VAKHAVW-bestand is
      geupload.",

    vakhawv_wiskundecijfer =
      "Gemiddeld centraal examencijfer voor wiskunde (schaal 1-10), afkomstig
      uit het VAKHAVW-bestand. NA als de student geen wiskunde had op de
      eindlijst.",

    vakhawv_aantal_vakken =
      "Aantal unieke vakken op de eindlijst van de vooropleiding, afkomstig
      uit het VAKHAVW-bestand.",

    ## Bekostiging (identiek op beide niveaus: beschrijft de koppeling via
    ## persoonsgebonden_nummer + opleidingscode)
    indicatie_bekostigd =
      "Of DUO deze inschrijving bekostigt, afkomstig uit het VLPBEK-bestand.
      Heeft de student meerdere BRD-regels voor dezelfde opleiding, dan
      geldt: bekostigd zodra minstens een regel bekostigd is. Alleen
      beschikbaar als een VLPBEK-bestand is geupload.",

    indicatie_hoofdinschrijving =
      "DUO-veld 'Bekostigingsindicatie' uit het VLPBEK-bestand: geeft aan of
      de instelling deze inschrijving heeft aangeleverd als in aanmerking
      komend voor bekostiging. Alleen beschikbaar als een VLPBEK-bestand is
      geupload.",

    reden_niet_bekostigd =
      "Toelichting op de DUO-bekostigingsstatus als de inschrijving niet
      bekostigd is, gedecodeerd uit de CodeBekostigingstatus van het
      VLPBEK-bestand (zie BEKOSTIGINGSTATUS_CODES). Leeg als de inschrijving
      wel bekostigd is.",

    indicatie_herstelbaar =
      "Of de reden voor niet-bekostiging een te late aanlevering door de
      instelling is ('ti'/'tg') en dus hersteld kan worden door tijdig
      opnieuw aan te leveren. 'Onwaar' bij een structurele of wettelijke
      reden; leeg als de inschrijving wel bekostigd is."
  ),

  inschrijving = list(
    filter_instroom =
      "Op inschrijvingsniveau is elke opleiding een eigen cohort: een
      wissel telt als uitval uit de oude opleiding en als instroom in de
      nieuwe. Filters op opleiding, sector en locatie meten dus per
      opleiding.",

    instroom =
      "Eerstejaars inschrijvingen per opleiding: een student telt mee
      zodra zij voor het eerst in een specifieke opleiding aan deze
      instelling staan (verblijfsjaar in de opleiding = 1). Een student
      die van opleiding wisselt start een nieuw cohort bij de nieuwe
      opleiding.",

    status =
      "Eindstatus van de inschrijving na de observatieperiode.
      'Diploma behaald': behaalde een hoofd- of nevendiploma voor deze
      opleiding (associate degree, bachelor, master, doctoraal, beroepsfase
      of postinitiele master; excl. propedeuse). 'Zittend': nog ingeschreven
      in de opleiding, geen diploma behaald. 'Uitgevallen': niet meer
      ingeschreven in de opleiding en geen diploma behaald.",

    rendement_3jr =
      "Percentage inschrijvingen met een diploma binnen 3 academische
      jaren na eerste inschrijving in de opleiding. Berekend als:
      diplomajaar - instroomjaar + 1 <= 3.
      Cohorten die nog geen 3 jaar in de data zitten zijn
      'Nog niet waarneembaar' en tellen niet mee in het percentage.",

    rendement_5jr =
      "Percentage inschrijvingen met een diploma binnen 5 academische
      jaren na eerste inschrijving in de opleiding.
      Cohorten die nog geen 5 jaar in de data zitten zijn
      'Nog niet waarneembaar' en tellen niet mee in het percentage.",

    rendement_8jr =
      "Percentage inschrijvingen met een diploma binnen 8 academische
      jaren na eerste inschrijving in de opleiding. Dit is de maximale
      observatietermijn.
      Cohorten die nog geen 8 jaar in de data zitten zijn
      'Nog niet waarneembaar' en tellen niet mee in het percentage.",

    uitval_1jr =
      "Percentage inschrijvingen waarbij de student na het eerste jaar
      niet meer in deze opleiding ingeschreven is en geen diploma heeft
      behaald. Een overstap naar een andere opleiding telt hier als uitval
      uit de opleiding.
      Uitval binnen 1 jaar is pas vast te stellen als instroomjaar + 1
      in de data zit; recentere cohorten zijn 'Nog niet waarneembaar'
      en tellen niet mee.",

    uitval_3jr =
      "Percentage inschrijvingen waarbij de student binnen 3 jaar na
      instroom in de opleiding niet meer ingeschreven is en geen diploma
      heeft behaald. Telt cumulatief.
      Uitval binnen 3 jaar is pas vast te stellen als instroomjaar + 3
      in de data zit; recentere cohorten zijn 'Nog niet waarneembaar'
      en tellen niet mee.",

    ## Studiewissel is niet beschikbaar op inschrijvingsniveau
    studiewissel_1jr = NULL,
    studiewissel_3jr = NULL,

    vooropleiding =
      "Hoogste vooropleiding voor het hoger onderwijs volgens DUO,
      samengevat tot havo, vwo, mbo, ho, buitenlands (incl. Europees
      baccalaureaat), overig (o.a. vmbo, toelatingsexamen, beschikking)
      of onbekend.",

    eerstejaars_ho =
      "'eerstejaars HO': het eerste jaar in deze opleiding is ook het
      eerste jaar in het hoger onderwijs. 'eerder in HO': de student stond
      al eerder in het hoger onderwijs ingeschreven, bij een andere
      instelling of bij een andere opleiding van deze instelling.",

    ## VAKHAVW (identiek op beide niveaus: beschrijft de student, niet de inschrijving)
    vakhawv_gemiddeld_eindcijfer =
      "Gemiddeld eindcijfer van de vooropleiding (schaal 1-10), afkomstig uit
      het VAKHAVW-bestand. Gebaseerd op het hoogste gemiddelde eindcijfer van
      de cijferlijst als een student meerdere jaren in de data staat.
      Alleen beschikbaar als een VAKHAVW-bestand is geupload.",

    vakhawv_wiskundecijfer =
      "Gemiddeld centraal examencijfer voor wiskunde (schaal 1-10), afkomstig
      uit het VAKHAVW-bestand. Berekend over alle wiskundevarianten (wis, wis A,
      wis B, wis D). NA als de student geen wiskunde had op de eindlijst.",

    vakhawv_aantal_vakken =
      "Aantal unieke vakken op de eindlijst van de vooropleiding, afkomstig
      uit het VAKHAVW-bestand.",

    ## Bekostiging (identiek op beide niveaus: beschrijft de koppeling via
    ## persoonsgebonden_nummer + opleidingscode)
    indicatie_bekostigd =
      "Of DUO deze inschrijving bekostigt, afkomstig uit het VLPBEK-bestand.
      Heeft de student meerdere BRD-regels voor dezelfde opleiding, dan
      geldt: bekostigd zodra minstens een regel bekostigd is. Alleen
      beschikbaar als een VLPBEK-bestand is geupload.",

    indicatie_hoofdinschrijving =
      "DUO-veld 'Bekostigingsindicatie' uit het VLPBEK-bestand: geeft aan of
      de instelling deze inschrijving heeft aangeleverd als in aanmerking
      komend voor bekostiging. Alleen beschikbaar als een VLPBEK-bestand is
      geupload.",

    reden_niet_bekostigd =
      "Toelichting op de DUO-bekostigingsstatus als de inschrijving niet
      bekostigd is, gedecodeerd uit de CodeBekostigingstatus van het
      VLPBEK-bestand (zie BEKOSTIGINGSTATUS_CODES). Leeg als de inschrijving
      wel bekostigd is.",

    indicatie_herstelbaar =
      "Of de reden voor niet-bekostiging een te late aanlevering door de
      instelling is ('ti'/'tg') en dus hersteld kan worden door tijdig
      opnieuw aan te leveren. 'Onwaar' bij een structurele of wettelijke
      reden; leeg als de inschrijving wel bekostigd is."
  )
)

## Hulpfunctie om VAKHAVW-definities op te halen
## (identiek voor student en inschrijving, dus apart gedocumenteerd)
vakhawv_definitie <- function(naam) {
  DEFINITIES[["student"]][[naam]]
}
