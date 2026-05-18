#' Start het Staat van Onderwijsinstelling dashboard
#'
#' Opent de interactieve Shiny app waarmee je een 1CHO CSV-bestand kunt
#' uploaden en studie-indicatoren kunt verkennen.
#'
#' @return No return value, called for side effects.
#' @examples
#' if (interactive()) {
#'   start_dashboard()
#' }
#' @export
start_dashboard <- function() {
  app_pad <- system.file("app", package = "staat1cho")
  if (!nzchar(app_pad)) {
    cli::cli_abort(
      "De app kon niet gevonden worden. Zorg dat staat1cho correct is geinstalleerd."
    )
  }
  shiny::runApp(app_pad)
}
