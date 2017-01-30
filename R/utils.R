#' .check_input
#'
#' \code{.check_input} is a helper function of the \code{rquantae} package that checks whether the input is a matrix of numeric or integer values. Error message are thrown if that is not the case.
#'
#' @param img An object that needs to checked.
#' @param f_call The name of the function inside which the \code{.check_input} is called.
#'
#' @return An error message if the check fails.
#' @keywords internal
.check_input <- function(img, f_call = NULL){
  if (is.null(f_call)) stop("You have to specify function for the f_call argument.", call. = FALSE)
  if (f_call == "symmetry" | f_call == "contrast" | f_call == "typicality" | f_call == "self-similarity") {
    # input must be a matrix of numeric or integer values
    if (!is.matrix(img)) {
      stop("Input img has to be a *matrix* of numeric or integer values", call. = FALSE)
    }
    if (!(is.numeric(img) | is.integer(img))) {
      stop("Input img has to be a matrix of *numeric* or *integer* values", call. = FALSE)
    }
  } else stop("unknown input to f_call argument", call. = FALSE)
}


#' RGB to Gray Conversion
#'
#' \code{rgb2gray} transforms colors from RGB space (red/green/blue) into an matrix of grayscale values.
#'
#' @param img 3-dimensional array of numeric or integer values
#'
#' @return A matrix of grayscale values.
#' @details The function takes a 3-dimensional array of numeric or integer values as input (\code{img}) and returns a matrix of grayscale values as output. The grayscale values are computed as \code{GRAY = 0.2989 * RED + 0.5870 * GREEN + 0.1140 * BLUE}.
#' @export
#' @examples
#' # construct a sample RGB image as array of random integers
#' imgRed <- matrix(runif(100, min = 0, max = 255), 10, 10)
#' imgGreen <- matrix(runif(100, min = 0, max = 255), 10, 10)
#' imgBlue <- matrix(runif(100, min = 0, max = 255), 10, 10)
#' imgColor <- array(c(imgRed, imgGreen, imgBlue), dim = c(10, 10, 3))
#'
#' # convert to gray
#' img <- rgb2gray(imgColor)
rgb2gray <- function(img) {
  if (requireNamespace("OpenImageR", quietly = TRUE)) {
    OpenImageR::rgb_2gray(img)
  } else {
    if (!is.array(img) | !is.numeric(img) | length(dim(img)) != 3 | dim(img)[3] != 3) {
      stop("Invalid input (has to be a 3-dimensional array of numeric or integer values)")
    }
    redChannel <- img[, , 1]
    greenChannel <- img[, , 2]
    blueChannel <- img[, , 3]
    out <- 0.2989 * redChannel + 0.5870 * greenChannel + 0.1140 * blueChannel
    return(out)
  }
}
