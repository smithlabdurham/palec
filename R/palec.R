# Here's a good place to put your top-level package documentation
#' @importFrom shiny fluidPage
#' @importFrom shinyjs useShinyjs
#' @importFrom readxl read_excel
#' @importFrom moments skewness
#' @importFrom Ternary TernaryPlot
#' @importFrom readxl read_excel

.onAttach <- function (lib, pkgname = "palec") {
  packageStartupMessage("Begin palec analysis by typing Survivorship(), Histogram() or Diversity()")
  invisible()
}

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
  shiny::runApp(system.file("Survivorship", package = "palec"))
}

#' @rdname palec
#' @export
Histogram <- function () {
  shiny::runApp(system.file("Histogram", package = "palec"))
}

#' @rdname palec
#' @export
Diversity <- function () {
  shiny::runApp(system.file("Diversity", package = "palec"))
}

#' Assign species to octave
#'
#' For details of octave plots, see [course material](https://smithlabdurham.github.io/GEOL2031/2A.html#Octave)
#'
#' @param counts Vector containing number of individuals in each species.
#' Zero entries will be ignored.
#'
#' @return Vector listing which octave each non-zero species belongs to,
#' suitable for the constrution of a histogram (with [`hist()`]) or
#' tabulation (with [`table()`]).
#'
#' @template MRS
#' @export
Octaves <- function (counts) {
  counts <- counts[!is.na(counts)]
  biggestBinMax <- ceiling(log2(max(counts + 1L)))
  as.integer(
    cut(counts[counts > 0],
        breaks = 2L ^ seq.int(from = 0, to = biggestBinMax,
                              by = 1L),
        right = FALSE)
  )
}
