#' @include utils.R
NULL

#' Run imagefluency app
#'
#' Launches a Shiny app that shows a demo of what can be done with
#' the \code{imagefluency} package.
#'
#' @export
#'
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   run_imagefluency()
#' }
run_imagefluency <- function() {
  appDir <- system.file("imagefluencyApp", package = "imagefluency")
  if (appDir == "") {
    stop("Could not find shiny app directory. Try re-installing `imagefluency`.", call. = FALSE)
  }

  if (requireNamespace("shiny", quietly = TRUE)) {
    shiny::runApp(appDir, display.mode = "normal")
  } else {
    stop("Package 'shiny' is required but not installed on your system.", call. = FALSE)
  }
}
