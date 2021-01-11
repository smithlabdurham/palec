#' @importFrom shiny fluidPage
#' @importFrom shinyjs useShinyjs
#' @importFrom colourpicker colourInput
#' @importFrom moments skewness
#' Palaeoecological apps
#'
#' Applications for palaeontological analysis
#'
#' These applications accompany the GEOL2031 course; an introduction to their
#' use is available in the [course documents](https://smithlabdurham.github.io/GEOL2031/2S.html)
#'
#' @template MRS
#'
#' @name palec

#' @rdname palec
#' @export
Survivorship <- function () {
  shiny::runApp(system.file('Survivorship', package = 'palec'))
}

#' @rdname palec
#' @export
Histogram <- function () {
  shiny::runApp(system.file('Histogram', package = 'palec'))
}

#' @rdname palec
#' @export
Diversity <- function () {
  shiny::runApp(system.file('Diversity', package = 'palec'))
}
