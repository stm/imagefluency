#' @keywords internal
.check_input <- function(img, f_call = NULL){
  if (f_call == "symmetry" | f_call == "contrast" | f_call == "typicality" | f_call == "self-similarity") {
    # input must be a matrix of numeric or integer values
    if (!is.matrix(img)) {
      stop("Input has to be a *matrix* of numeric or integer values", call. = FALSE)
    }
    if (!(is.numeric(img) | is.integer(img))) {
      stop("Input has to be a matrix of *numeric* or *integer* values", call. = FALSE)
    }
  }
}
