#' Redencodes bekostigingstatus (VLPBEK/DEFBEK)
#'
#' Decodeertabel voor de kolom `CodeBekostigingstatus` uit het analysebestand
#' voorlopige/definitieve bekostiging (DUO PvE HO-instelling, bijlage 8,
#' par. 17.7.3/19.7.5). Een inschrijving of graad kan meerdere codes
#' tegelijk hebben (komma-gescheiden in het brondata, bijv. `"na,ti"`).
#'
#' `herstelbaar` geeft aan of de reden voortkomt uit een registratiefout van
#' de instelling die te herstellen is door tijdig opnieuw aan te leveren
#' (`ti`/`tg`, waarde `TRUE`), een structurele/wettelijke reden is die niet
#' te herstellen is (waarde `FALSE`), of helemaal geen "niet bekostigd"-reden
#' betreft maar juist aangeeft dat er (deels) bekostigd wordt (waarde `NA`).
#'
#' @format Een tibble met kolommen `code`, `omschrijving` en `herstelbaar`
#' @export
BEKOSTIGINGSTATUS_CODES <- tibble::tribble(
  ~code, ~omschrijving, ~herstelbaar,
  "ex", "Het betreft een extraneus-inschrijving.", FALSE,
  "jk", "Er zijn voor deze student meerdere eerste inschrijvingen aangeleverd voor verschillende opleidingen.", FALSE,
  "jl", "Het betreft geen opleiding van 'eerste inschrijving' ('eerste inschrijving' is 'N').", FALSE,
  "jm", "Er zijn voor deze student meerdere eerste inschrijvingen aangeleverd voor dezelfde opleiding aan dezelfde instelling.", FALSE,
  "mt", "De graad is behaald na de peilperiode voor het actuele bekostigingsjaar.", FALSE,
  "mu", "De graad is behaald voor de peilperiode voor het actuele bekostigingsjaar.", FALSE,
  "mv", "De inschrijving is niet geldig op de peildatum voor het actuele bekostigingsjaar, of valt niet binnen het actuele bekostigingsjaar (OU).", FALSE,
  "mw", "Graad zonder bijbehorende, op datum diploma geldige inschrijving.", FALSE,
  "na", "De student voldoet niet aan de woonplaatsvereiste.", FALSE,
  "nb", "Niet bekostigd i.v.m. eerder behaalde graad/graden.", FALSE,
  "nc", "Niet bekostigd vanuit de conversie naar HORS.", FALSE,
  "nd", "De student heeft al eerder een MA-graad behaald.", FALSE,
  "ne", "Voor bekostiging geen relevante graad, of een AD-graad.", FALSE,
  "nf", "Niet bekostigd i.v.m. overschrijden maximaal aantal bekostigde inschrijvingen, rekening houdend met eerder behaalde graden.", FALSE,
  "ng", "Niet bekostigd i.v.m. overschrijden maximaal aantal bekostigde inschrijvingen (nog geen graad behaald).", FALSE,
  "nh", "Ongedeelde opleiding waarvan het aantal toegestane te bekostigen jaren MA is verbruikt.", FALSE,
  "ni", "Ongedeelde opleiding waarvan het aantal toegestane te bekostigen jaren MA in een uitzonderingscategorie is verbruikt.", FALSE,
  "nk", "De student heeft het toegestane aantal te bekostigen jaren c.q. studielast MA verbruikt.", FALSE,
  "nl", "Aangeleverd met de markering om niet te bekostigen.", FALSE,
  "no", "Ongedeelde opleiding waarvan de behaalde graden (BA en MA) beide niet bekostigd worden.", FALSE,
  "np", "Behaalde graad die niet voor bekostiging in aanmerking komt (opleidingsfase = P, D of A).", FALSE,
  "nr", "De student voldoet niet aan de nationaliteitsvereiste.", FALSE,
  "ns", "De student heeft aan verschillende instellingen op dezelfde dag eenzelfde soort graad behaald.", FALSE,
  "nt", "De student heeft meerdere graden behaald aan dezelfde instelling; slechts een wordt bekostigd.", FALSE,
  "ob", "'Datum diploma' valt niet in de accreditatieperiode.", FALSE,
  "oc", "Het betreft geen geaccrediteerde opleiding.", FALSE,
  "pb", "Graad in een ongedeelde opleiding waarvan alleen de (impliciete) BA-graad bekostigd wordt.", NA,
  "pd", "De inschrijving wordt deels bekostigd.", NA,
  "pg", "De graad wordt bekostigd.", NA,
  "pi", "De inschrijving wordt bekostigd.", NA,
  "pm", "Graad in een ongedeelde opleiding waarvan alleen de MA-graad bekostigd wordt.", NA,
  "po", "Graad in een ongedeelde opleiding waarvan zowel de (impliciete) BA als de MA bekostigd wordt.", NA,
  "tg", "De graad is niet tijdig aangeleverd.", TRUE,
  "ti", "De inschrijving is niet tijdig aangeleverd.", TRUE
)

## Interne hulpfuncties om (eventueel samengestelde, komma-gescheiden)
## CodeBekostigingstatus-waarden te vertalen naar leesbare tekst resp. een
## herstelbaar-vlag, aan de hand van BEKOSTIGINGSTATUS_CODES.
decodeer_bekostigingstatus <- function(codes) {
  vapply(codes, function(code) {
    if (is.na(code)) {
      return(NA_character_)
    }
    delen <- trimws(strsplit(code, ",", fixed = TRUE)[[1]])
    omschrijvingen <- vapply(delen, function(deel) {
      idx <- match(deel, BEKOSTIGINGSTATUS_CODES$code)
      if (is.na(idx)) deel else BEKOSTIGINGSTATUS_CODES$omschrijving[idx]
    }, character(1))
    paste(omschrijvingen, collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
}

## Herstelbaar alleen als *alle* redenen herstelbaar zijn: bij "na,ti" blijft
## de woonplaatsvereiste (na) staan, ook als de inschrijving alsnog tijdig
## wordt aangeleverd. Positieve codes (pi, pd, ...) zijn geen reden en tellen
## niet mee.
bekostiging_is_herstelbaar <- function(codes) {
  positief <- BEKOSTIGINGSTATUS_CODES$code[is.na(BEKOSTIGINGSTATUS_CODES$herstelbaar)]
  vapply(codes, function(code) {
    if (is.na(code)) {
      return(NA)
    }
    delen <- trimws(strsplit(code, ",", fixed = TRUE)[[1]])
    redenen <- setdiff(delen, positief)
    length(redenen) > 0 && all(redenen %in% c("ti", "tg"))
  }, logical(1), USE.NAMES = FALSE)
}

## Bevat de (samengestelde) code een bepaalde deelcode?
bevat_code <- function(codes, zoek) {
  vapply(codes, function(code) {
    !is.na(code) && zoek %in% trimws(strsplit(code, ",", fixed = TRUE)[[1]])
  }, logical(1), USE.NAMES = FALSE)
}

#' Lees een VLPBEK-bestand in
#'
#' Leest een pipegescheiden VLPBEK-bestand (DUO-formaat, Voorlopige Bekostiging)
#' in en extraheert de relevante kolommen per inschrijving. Alleen BRD-regels
#' worden verwerkt; de koptekstregel (VLP), totaalregel (BLB) en sluitregel
#' (SLR) worden genegeerd. Het peiljaar wordt uit de VLP-koptekstregel gelezen.
#'
#' ## Koppelt nog niet aan het EV-bestand
#'
#' Het VLPBEK-bestand bevat het BSN of onderwijsnummer. Het persoonsgebonden
#' nummer in het 1CHO-bestand (EV) is een eigen DUO-nummer en geen BSN, dus
#' [verrijk_met_bekostiging()] vindt bij echte leveringen (vrijwel) geen
#' inschrijvingen terug en waarschuwt. De koppeling wordt nog aangepast.
#'
#' @param pad Pad naar het VLPBEK-bestand (latin-1 gecodeerd, pipegescheiden)
#'
#' @return Een tibble met de kolommen `persoonsgebonden_nummer`,
#'   `opleidingscode`, `inschrijvingsjaar`, `indicatie_hoofdinschrijving`,
#'   `opleidingsvorm`, `sector`, `bekostigingsstatus` (`"bekostigd"`,
#'   `"deels bekostigd"` (redencode `pd`) of `"niet bekostigd"`), `code_bekostigingstatus` (ruwe DUO-redencode(s)),
#'   `reden_niet_bekostigd` (leesbare toelichting, zie
#'   [BEKOSTIGINGSTATUS_CODES]) en `indicatie_herstelbaar` (`TRUE` als de
#'   redenen allemaal een te late aanlevering door de instelling zijn en dus
#'   hersteld kunnen worden, `FALSE` bij een structurele reden, `NA` als de inschrijving wel
#'   bekostigd is), plus het attribuut `peiljaar` (integer) dat aangeeft
#'   voor welk bekostigingsjaar het bestand geldt. Gereed voor gebruik in
#'   [verrijk_met_bekostiging()]
#'
#' @examples
#' pad <- system.file("extdata/voorbeeld_vlpbek.csv", package = "staat1cho")
#' lees_bekostiging(pad)
#' @export
lees_bekostiging <- function(pad) {
  regels <- readLines(pad, encoding = "latin1", warn = FALSE)

  ## Peiljaar uit de VLP-koptekstregel (veld 3)
  vlp <- regels[startsWith(regels, "VLP")]
  peiljaar <- if (length(vlp) > 0) {
    as.integer(strsplit(vlp[1], "|", fixed = TRUE)[[1]][3])
  } else {
    NA_integer_
  }

  brd <- regels[startsWith(regels, "BRD")]
  if (length(brd) == 0) {
    rlang::abort("Geen BRD-regels gevonden in het VLPBEK-bestand.")
  }

  ## De BRD-regels zijn pipegescheiden en hebben 25 velden.
  kolommen <- c(
    "recordtype",
    "burgerservicenummer",
    "onderwijsnummer",
    "instellingscode",
    "inschrijvingvolgnummer",
    "indicatie_hoofdinschrijving",
    "code_bekostigingstatus", "col8",
    "opleidingscode",
    "soort_hoger_onderwijs",
    "col11",
    "begindatum_inschrijving",
    "einddatum_inschrijving",
    "col14", "col15",
    "opleidingsvorm",
    "col17", "col18",
    "sector",
    "bekostigingsstatus",
    "col21", "col22", "col23", "col24", "col25"
  )

  ## Kolommen worden op positie gelezen. Controleer dat de layout klopt, zodat
  ## een ander formaat (bijv. MBO, zie #34) niet stil verkeerde kolommen geeft.
  ## strsplit laat lege velden aan het eind weg, dus minder dan 25 mag.
  velden <- strsplit(brd, "|", fixed = TRUE)
  te_veel <- lengths(velden) > length(kolommen)
  if (any(te_veel)) {
    cli::cli_abort(c(
      "{sum(te_veel)} BRD-regel{?s} {?heeft/hebben} meer dan {length(kolommen)} velden.",
      "i" = "Dit lijkt geen VLPBEK-bestand in het HO-formaat."
    ))
  }
  m <- do.call(rbind, lapply(velden, function(p) {
    length(p) <- length(kolommen)
    p
  }))
  begindatum <- m[, match("begindatum_inschrijving", kolommen)]
  if (mean(grepl("^[0-9]{8}$", begindatum)) < 0.9) {
    cli::cli_abort(c(
      "Veld 12 (begindatum inschrijving) bevat geen datums (JJJJMMDD).",
      "i" = "De kolomindeling van dit bestand wijkt af van het verwachte VLPBEK-formaat."
    ))
  }

  result <- tibble::as_tibble(m, .name_repair = "minimal") |>
    rlang::set_names(kolommen) |>
    ## Een leeg veld is "" en geen NA; zonder deze stap kiest coalesce() een
    ## lege BSN boven het onderwijsnummer en krijgen alle studenten zonder
    ## BSN dezelfde sleutel "".
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::na_if(trimws(.x), ""))) |>
    dplyr::mutate(
      persoonsgebonden_nummer    = as.character(dplyr::coalesce(burgerservicenummer, onderwijsnummer)),
      opleidingscode             = as.character(opleidingscode),
      inschrijvingsjaar          = as.integer(substr(begindatum_inschrijving, 1, 4)),
      indicatie_hoofdinschrijving = indicatie_hoofdinschrijving == "J",
      opleidingsvorm             = dplyr::recode(
        tolower(opleidingsvorm),
        "vt" = "voltijd",
        "dt" = "deeltijd"
      ),
      sector             = tolower(gsub("_", " ", sector)),
      code_bekostigingstatus = tolower(code_bekostigingstatus),
      bekostigingsstatus = dplyr::case_when(
        bekostigingsstatus %in% "BEKOSTIGD" ~ "bekostigd",
        bevat_code(code_bekostigingstatus, "pd") ~ "deels bekostigd",
        TRUE ~ "niet bekostigd"
      ),
      reden_niet_bekostigd = dplyr::if_else(
        bekostigingsstatus == "niet bekostigd",
        decodeer_bekostigingstatus(code_bekostigingstatus),
        NA_character_
      ),
      indicatie_herstelbaar = dplyr::if_else(
        bekostigingsstatus == "niet bekostigd",
        bekostiging_is_herstelbaar(code_bekostigingstatus),
        NA
      )
    ) |>
    dplyr::select(
      persoonsgebonden_nummer,
      opleidingscode,
      inschrijvingsjaar,
      indicatie_hoofdinschrijving,
      opleidingsvorm,
      sector,
      bekostigingsstatus,
      code_bekostigingstatus,
      reden_niet_bekostigd,
      indicatie_herstelbaar
    )

  attr(result, "peiljaar") <- peiljaar
  result
}

## Interne hulpfunctie om unieke, niet-NA redenen van meerdere BRD-regels
## (voor dezelfde persoon + opleiding) samen te voegen tot een leesbare tekst.
samenvoegen_redenen <- function(redenen) {
  redenen <- unique(redenen[!is.na(redenen)])
  if (length(redenen) == 0) NA_character_ else paste(redenen, collapse = "; ")
}

#' Verrijk indicatoren met bekostigingsinformatie uit een VLPBEK-bestand
#'
#' Koppelt bekostigingsstatus per student per opleiding aan het
#' indicatorenbestand via `persoonsgebonden_nummer` en `opleidingscode`.
#' Wanneer een student meerdere BRD-regels heeft voor dezelfde opleiding
#' geldt: als ten minste een regel de status `"bekostigd"` of `"deels
#' bekostigd"` heeft, is de student bekostigd voor die opleiding.
#'
#' Een VLPBEK-bestand beschrijft de inschrijvingen van één bekostigingsjaar.
#' De koppeling legt dus de huidige bekostigingsstatus naast elk
#' instroomcohort; studenten die in dat jaar niet (meer) ingeschreven stonden
#' krijgen `NA`. De kolom `bekostiging_jaar` geeft aan op welk
#' inschrijvingsjaar de status betrekking heeft.
#'
#' De functie meldt hoeveel rijen gekoppeld zijn en waarschuwt als dat minder
#' dan de helft is: dan gebruiken de bestanden verschillende persoonsnummers.
#' Dat is bij echte leveringen nu altijd zo, zie [lees_bekostiging()]. Het
#' attribuut `koppeling` bevat de aantallen.
#'
#' Studenten zonder overeenkomst in het VLPBEK-bestand krijgen `NA` voor
#' `indicatie_bekostigd` en `indicatie_hoofdinschrijving`.
#'
#' @param indicatoren Tibble zoals gemaakt door [combineer_indicatoren()],
#'   met kolommen `persoonsgebonden_nummer` en `opleidingscode`
#' @param bekostiging Tibble zoals gemaakt door [lees_bekostiging()]
#'
#' @return De indicatoren-tibble uitgebreid met `indicatie_bekostigd`
#'   (`TRUE`/`FALSE`/`NA`), `indicatie_hoofdinschrijving` (`TRUE`/`FALSE`/`NA`),
#'   `reden_niet_bekostigd` (leesbare toelichting(en), `NA` als wel bekostigd)
#'   , `indicatie_herstelbaar` (`TRUE`/`FALSE`/`NA`, zie
#'   [BEKOSTIGINGSTATUS_CODES]) en `bekostiging_jaar`. Heeft een student meerdere BRD-regels met
#'   verschillende redenen voor dezelfde opleiding, dan worden de unieke
#'   redenen samengevoegd.
#'
#' @examples
#' indicatoren <- tibble::tibble(
#'   persoonsgebonden_nummer = c("1", "2", "3"),
#'   opleidingscode          = c("31001", "31002", "31003"),
#'   inschrijvingsjaar       = 2023L
#' )
#' bekostiging <- tibble::tibble(
#'   persoonsgebonden_nummer    = c("1", "2"),
#'   opleidingscode             = c("31001", "31002"),
#'   inschrijvingsjaar          = c(2023L, 2023L),
#'   indicatie_hoofdinschrijving = c(TRUE, TRUE),
#'   opleidingsvorm             = c("voltijd", "voltijd"),
#'   sector                     = c("gezondheidszorg", "economie"),
#'   bekostigingsstatus         = c("bekostigd", "niet bekostigd"),
#'   code_bekostigingstatus     = c(NA, "nf"),
#'   reden_niet_bekostigd       = c(NA, "Maximaal aantal bekostigde inschrijvingen overschreden."),
#'   indicatie_herstelbaar      = c(NA, FALSE)
#' )
#' verrijk_met_bekostiging(indicatoren, bekostiging)
#' @export
verrijk_met_bekostiging <- function(indicatoren, bekostiging) {
  for (kol in c("persoonsgebonden_nummer", "opleidingscode")) {
    if (!kol %in% names(indicatoren)) {
      rlang::abort(paste0("Kolom `", kol, "` niet gevonden in indicatoren."))
    }
  }

  controleer_id_soort(
    indicatoren$persoonsgebonden_nummer,
    bekostiging$persoonsgebonden_nummer,
    "VLPBEK",
    HINT_VLPBEK
  )

  per_inschrijving <- bekostiging |>
    dplyr::group_by(persoonsgebonden_nummer, opleidingscode) |>
    dplyr::summarise(
      indicatie_bekostigd         = any(bekostigingsstatus %in% c("bekostigd", "deels bekostigd")),
      indicatie_hoofdinschrijving = any(indicatie_hoofdinschrijving, na.rm = TRUE),
      reden_niet_bekostigd = dplyr::if_else(
        indicatie_bekostigd,
        NA_character_,
        samenvoegen_redenen(reden_niet_bekostigd)
      ),
      indicatie_herstelbaar = dplyr::if_else(
        indicatie_bekostigd,
        NA,
        all(indicatie_herstelbaar, na.rm = TRUE) && any(!is.na(indicatie_herstelbaar))
      ),
      bekostiging_jaar = suppressWarnings(max(inschrijvingsjaar, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(bekostiging_jaar = dplyr::if_else(
      is.finite(bekostiging_jaar), as.integer(bekostiging_jaar), NA_integer_
    ))

  resultaat <- dplyr::left_join(
    indicatoren |>
      dplyr::mutate(
        persoonsgebonden_nummer = as.character(persoonsgebonden_nummer),
        opleidingscode          = as.character(opleidingscode)
      ),
    per_inschrijving,
    by = c("persoonsgebonden_nummer", "opleidingscode")
  )
  sleutel_ind <- paste(resultaat$persoonsgebonden_nummer, resultaat$opleidingscode)
  sleutel_bek <- paste(per_inschrijving$persoonsgebonden_nummer, per_inschrijving$opleidingscode)
  meld_koppeling(
    resultaat,
    bron_gevonden = sleutel_bek %in% sleutel_ind,
    n_verrijkt = sum(!is.na(resultaat$indicatie_bekostigd)),
    bron = "VLPBEK",
    eenheid = "inschrijvingen"
  )
}

## Meldt hoe goed een verrijking koppelt en legt de aantallen vast in het
## attribuut `koppeling`. Het percentage kijkt vanuit het bronbestand: welk
## deel van de VLPBEK-inschrijvingen of VAKHAVW-studenten is in het
## 1CHO-bestand teruggevonden? Vanuit het 1CHO-bestand is een laag percentage
## normaal (VLPBEK beslaat maar één jaar, niet iedereen heeft VAKHAVW); vanuit
## de bron wijst het op verschillende persoonsnummers.
##
## `drempel` is het aandeel waaronder gewaarschuwd wordt. VAKHAVW bevat ook
## studenten die voor de eerste cohort in de data begonnen, dus daar is een
## laag aandeel normaal en wijst pas een heel laag aandeel op een fout.
meld_koppeling <- function(resultaat, bron_gevonden, n_verrijkt, bron, eenheid, drempel = 0.5) {
  n <- length(bron_gevonden)
  gevonden <- sum(bron_gevonden)
  pct <- if (n > 0) round(100 * gevonden / n) else NA_real_
  laag <- n > 0 && gevonden / n < drempel
  attr(resultaat, "koppeling") <- list(
    bron = bron, eenheid = eenheid, n = n, gekoppeld = gevonden, pct = pct,
    n_verrijkt = n_verrijkt, n_rijen = nrow(resultaat), laag = laag
  )
  tekst <- "{bron}: {gevonden} van {n} {eenheid} teruggevonden in het 1CHO-bestand ({pct}%); {n_verrijkt} van {nrow(resultaat)} rijen verrijkt."
  if (laag) {
    cli::cli_warn(c(
      tekst,
      "i" = "Controleer of beide bestanden dezelfde persoonsnummers en opleidingscodes gebruiken.",
      "i" = if (bron == "VLPBEK") HINT_VLPBEK else "Gebruik voor het 1CHO- en VAKHAVW-bestand dezelfde 1cijferho-uitvoer."
    ))
  } else {
    cli::cli_inform(tekst)
  }
  resultaat
}
