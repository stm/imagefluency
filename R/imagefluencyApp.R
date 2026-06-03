#' @include utils.R
NULL

#' Interactive dashboard for \code{imagefluency}
#'
#' Launches a dashboard to interactively use the functions of the
#' \code{imagefluency} package. The dashboard is built using the \code{shiny}
#' package and provides a user-friendly interface for uploading images,
#' analyzing them, and exporting the results. The dashboard supports
#' multi-image uploads and side-by-side comparisons of image fluency scores.
#'
#' @param max_images Integer. Maximum number of images that can be uploaded.
#'   Defaults to 100.
#'
#' @export
#'
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   run_imagefluency()
#'
#'   # increase the maximum number of uploadable images to 150
#'   run_imagefluency(max_images = 150)
#' }
run_imagefluency <- function(max_images = 100) {
  appDir <- system.file("imagefluencyApp", package = "imagefluency")
  if (appDir == "") {
    stop("Could not find shiny app directory. Try re-installing `imagefluency`.", call. = FALSE)
  }

  if (!is.numeric(max_images) || max_images < 1) {
    stop("`max_images` must be a positive number.", call. = FALSE)
  }

  if (requireNamespace("shiny", quietly = TRUE)) {
    options(imagefluency.max_images = as.integer(max_images))
    on.exit(options(imagefluency.max_images = NULL), add = TRUE)
    shiny::runApp(appDir, display.mode = "normal")
  } else {
    stop("Package 'shiny' is required but not installed on your system.", call. = FALSE)
  }
}
