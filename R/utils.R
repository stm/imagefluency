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
  if (f_call == "symmetry" | f_call == "contrast" | f_call == "typicality" | f_call == "complexity" | f_call == "self-similarity") {
    # input must be a matrix of numeric or integer values
    if (!is.matrix(img)) {
      stop("Input has to be a *matrix* of numeric or integer values", call. = FALSE)
    }
    if (!(is.numeric(img) | is.integer(img))) {
      stop("Input has to be a matrix of *numeric* or *integer* values", call. = FALSE)
    }
  } else stop("unknown input to f_call argument", call. = FALSE)
}

rgb2gray <- function(img) {
  if (requireNamespace("OpenImageR", quietly = TRUE)) {
    OpenImageR::rgb_2gray(img)
  } else {
    #0.2989 *     0.5870    0.1140
    #redChannel = color(:, :, 1);
    #greenChannel = color(:, :, 2);
    #blueChannel = color(:, :, 3);
    # gray = .299*redChannel + .587*greenChannel + .114*blueChannel
  }
}
