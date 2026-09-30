## Persoonsnummers in 1cijferho-uitvoer
##
## 1cijferho kan de persoonsnummers in de EV- en VAKHAVW-bestanden op drie
## manieren uitleveren: met het BSN behouden, omgezet naar studentnummer, of
## gepseudonimiseerd. Het VLPBEK-bestand komt rechtstreeks van DUO en bevat
## het BSN of onderwijsnummer. Het persoonsgebonden nummer in het EV-bestand
## is een eigen DUO-nummer en geen BSN, dus VLPBEK koppelt nog niet aan het
## 1CHO-bestand. staat1cho pseudonimiseert of vertaalt zelf niets.

## Melding die in foutmeldingen, het dashboard en de documentatie terugkomt
HINT_VLPBEK <- paste(
  "VLPBEK koppelt nog niet: het VLPBEK-bestand gebruikt het BSN of",
  "onderwijsnummer, het 1CHO-bestand het eigen persoonsgebonden nummer van DUO.",
  "Laat VLPBEK voorlopig weg; de rest van staat1cho werkt zonder."
)

#' Is een ID-kolom gepseudonimiseerd door 1cijferho?
#'
#' Herkent de pseudoniemen die 1cijferho kan maken: 64 hexadecimale tekens.
#' Een gepseudonimiseerd 1CHO-bestand kan niet gekoppeld worden aan een
#' niet-gepseudonimiseerd VAKHAVW- of VLPBEK-bestand.
#'
#' @param x Vector met persoonsnummers, bijv. `basis$persoonsgebonden_nummer`
#'
#' @return `TRUE` als alle niet-lege waarden pseudoniemen zijn, anders `FALSE`
#'
#' @examples
#' is_gepseudonimiseerd(c("123456789", "987654321"))
#' is_gepseudonimiseerd(strrep("a1", 32))
#' @export
is_gepseudonimiseerd <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & x != ""]
  length(x) > 0 && all(grepl("^[0-9a-f]{64}$", x))
}

## Controleer dat beide kanten van een koppeling hetzelfde soort ID gebruiken.
## Een mismatch geeft altijd 0% koppeling; beter meteen een duidelijke fout.
controleer_id_soort <- function(indicatoren_ids, bron_ids, bron, hint) {
  links <- is_gepseudonimiseerd(indicatoren_ids)
  rechts <- is_gepseudonimiseerd(bron_ids)
  if (links && !rechts) {
    cli::cli_abort(c(
      "Het 1CHO-bestand is gepseudonimiseerd, het {bron}-bestand niet.",
      "i" = hint
    ))
  }
  if (!links && rechts) {
    cli::cli_abort(c(
      "Het {bron}-bestand is gepseudonimiseerd, het 1CHO-bestand niet.",
      "i" = "Gebruik voor het 1CHO- en {bron}-bestand dezelfde 1cijferho-uitvoer."
    ))
  }
  invisible(TRUE)
}
