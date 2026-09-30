niveau_sleutels <- function(niveau) {
  niveau <- rlang::arg_match0(niveau, c("student", "inschrijving"))
  if (niveau == "inschrijving") {
    c("persoonsgebonden_nummer", "opleiding_actueel_equivalent")
  } else {
    "persoonsgebonden_nummer"
  }
}

## 1CHO-kolommen die als geheel getal gebruikt worden. Alle andere kolommen
## blijven tekst (zie maak_basisbestand()).
INTEGER_KOLOMMEN_1CHO <- c(
  "inschrijvingsjaar",
  "verblijfsjaar_actuele_instelling",
  "verblijfsjaar_actuele_opleiding_instelling",
  "diplomajaar",
  "leeftijd_per_peildatum_1_oktober",
  "eerste_jaar_in_het_hoger_onderwijs"
)

## Zet de opgegeven (aanwezige) kolommen om naar integer en waarschuwt als
## daarbij waarden verloren gaan, zodat een afwijkende aanlevering niet stil
## tot NA's leidt.
zet_om_naar_integer <- function(data, kolommen) {
  for (kol in intersect(kolommen, names(data))) {
    ruw <- trimws(as.character(data[[kol]]))
    ruw[ruw == ""] <- NA_character_
    ## 1cijferho zet "niet van toepassing" (code 0/00/0000) om naar een label
    ## als "geen examen geregistreerd > 0000 voor overige inschrijvingen".
    ## Dat is een lege waarde, geen fout.
    ruw[grepl(">\\s*0+\\b", ruw)] <- NA_character_
    omgezet <- suppressWarnings(as.integer(ruw))
    verloren <- sum(!is.na(ruw) & is.na(omgezet))
    if (verloren > 0) {
      voorbeelden <- unique(ruw[!is.na(ruw) & is.na(omgezet)])
      voorbeelden <- voorbeelden[seq_len(min(3, length(voorbeelden)))]
      cli::cli_warn(c(
        "{verloren} waarde{?n} in kolom {.field {kol}} {?is/zijn} geen geheel getal en {?wordt/worden} NA.",
        "i" = "Voorbeelden: {.val {voorbeelden}}"
      ))
    }
    data[[kol]] <- omgezet
  }
  data
}

## Sleutel om persoonsnummers uit verschillende DUO-bestanden te koppelen:
## zonder spaties en voorloopnullen, omdat DUO het nummer per bestand anders
## aanvult ("     2" in EV, "000000000002" in VAKHAVW).
koppelsleutel <- function(x) {
  sub("^0+(?=.)", "", trimws(as.character(x)), perl = TRUE)
}

ONBEKENDE_POSTCODES <-c("0010", "0020", "0030", "0040")

## Hercodeer factorniveaus die daadwerkelijk voorkomen. In tegenstelling tot
## forcats::fct_recode() geeft dit geen waarschuwing als een niveau in deze
## aanlevering niet voorkomt. `nieuw_oud` is een named vector (nieuw = oud);
## niveaus in `verwijder` worden NA.
hercodeer <- function(x, nieuw_oud = character(), verwijder = character()) {
  if (!is.factor(x)) {
    x <- factor(x)
  }
  aanwezig <- nieuw_oud[nieuw_oud %in% levels(x)]
  if (length(aanwezig) > 0) {
    x <- forcats::fct_recode(x, !!!aanwezig)
  }
  if (any(verwijder %in% levels(x))) {
    x[x %in% verwijder] <- NA
    x <- droplevels(x)
  }
  x
}

## Label voor indicatoren waarvan het meetvenster voor een cohort nog niet
## volledig in de data zit. Telt niet mee in teller of noemer.
NIET_WAARNEEMBAAR <- "Nog niet waarneembaar"

## Peildatum van de 1CHO-data: 1 oktober van het laatste inschrijvingsjaar.
## Inschrijvingen in 1CHO gelden per peildatum 1 oktober; alle uitkomsten
## beschrijven de stand van de data op dat moment.
peildatum_1cho <- function(laatste_jaar) {
  as.Date(sprintf("%d-10-01", as.integer(laatste_jaar)))
}

## Kolommen voor vooropleiding en eerste jaar in het HO. Beide zijn optioneel:
## oudere aanleveringen zonder deze kolommen geven "onbekend".
VOOROPLEIDING_KOLOM <- "hoogste_vooropleiding_voor_het_ho_omschrijving_vooropleiding"
EERSTE_JAAR_HO_KOLOM <- "eerste_jaar_in_het_hoger_onderwijs"

VOOROPLEIDING_NIVEAUS <- c("havo", "vwo", "mbo", "ho", "buitenlands", "overig", "onbekend")
EERSTEJAARS_HO_NIVEAUS <- c("eerstejaars HO", "eerder in HO", "onbekend")

## Vat de DUO-omschrijving van de hoogste vooropleiding voor het HO samen tot
## een hoofdcategorie. De omschrijvingen beginnen met de soort ("havo profiel
## economie & maatschappij", "mbo techniek niveau 4"); vmbo, vbo,
## toelatingsexamens en beschikkingen vallen onder "overig".
categoriseer_vooropleiding <- function(omschrijving) {
  x <- tolower(trimws(as.character(omschrijving)))
  categorie <- dplyr::case_when(
    is.na(x) | x == "" | grepl("onbekend", x) ~ "onbekend",
    grepl("^havo", x) ~ "havo",
    grepl("^vwo", x) ~ "vwo",
    grepl("^mbo", x) ~ "mbo",
    grepl("^(hbo|wo)-", x) ~ "ho",
    grepl("buitenlands", x) ~ "buitenlands",
    TRUE ~ "overig"
  )
  factor(categorie, levels = VOOROPLEIDING_NIVEAUS)
}

## Eerstejaars HO: het instroomjaar is ook het eerste jaar in het hoger
## onderwijs. Anders stond de student eerder ergens in het HO ingeschreven
## (bij een andere instelling, of bij een andere opleiding van deze
## instelling op inschrijvingsniveau).
bepaal_eerstejaars_ho <- function(eerste_jaar_ho, instroomjaar) {
  categorie <- dplyr::case_when(
    is.na(eerste_jaar_ho) | is.na(instroomjaar) ~ "onbekend",
    eerste_jaar_ho == instroomjaar ~ "eerstejaars HO",
    eerste_jaar_ho < instroomjaar ~ "eerder in HO",
    ## Eerste jaar HO na het instroomjaar is een data-inconsistentie
    TRUE ~ "onbekend"
  )
  factor(categorie, levels = EERSTEJAARS_HO_NIVEAUS)
}
