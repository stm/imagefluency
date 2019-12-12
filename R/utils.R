#' Read bitmap image (bmp, jpg, png, tiff)
#'
#' Wrapper for readbitmap's \code{\link[readbitmap]{read.bitmap}} function. The
#' function currently allows reading in images in \code{bmp}, \code{jpg} /
#' \code{jpeg}, \code{png}, or \code{tif} / \code{tiff} format.
#'
#' @details For details, see the \code{\link[readbitmap]{read.bitmap}}
#'   documentation.
#'
#'
#' @param path Path to the image file.
#' @param ... Additional parameters that are passed to
#'   \code{\link[readbitmap]{read.bitmap}} and the underlying image reader
#'   packages.
#'
#' @return Objects returned by \code{\link[bmp]{read.bmp}},
#'   \code{\link[jpeg]{readJPEG}}, \code{\link[png]{readPNG}}, or
#'   \code{\link[tiff]{readTIFF}}. See their documentation for details.
#' @export
#'
#' @seealso \code{\link[readbitmap]{read.bitmap}}, \code{\link[bmp]{read.bmp}},
#'   \code{\link[jpeg]{readJPEG}}, \code{\link[png]{readPNG}},
#'   \code{\link[tiff]{readTIFF}}
#'
#' @examples
#' ## Example image with high vertical symmetry: rails
#' rails <- img_read(system.file("example_images", "rails.jpg", package = "imagefluency"))
img_read <- function(path, ...){
  readbitmap::read.bitmap(f = path, ...)
}

#' RGB to Gray Conversion
#'
#' \code{rgb2gray} transforms colors from RGB space
#' (red/green/blue) into an matrix of grayscale values.
#'
#' @param img 3-dimensional array of numeric or integer
#'   values
#'
#' @return A matrix of grayscale values.
#'
#' @details The function takes a 3-dimensional array of
#'   numeric or integer values as input (\code{img}) and
#'   returns a matrix of grayscale values as output. The
#'   grayscale values are computed as \code{GRAY = 0.2989 *
#'   RED + 0.5870 * GREEN + 0.1140 * BLUE}. If the array has
#'   a fourth dimension (i.e., alpha channel), the fourth
#'   dimension is ignored.
#'
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
  if (is.null(dim(img))) {
    stop("Invalid input (should be a 3-dimensional array of numeric or integer values)", call. = FALSE)
  }
  if (!is.array(img) | !is.numeric(img) | length(dim(img)) != 3 | dim(img)[3] < 3 | dim(img)[3] > 4) {
    stop("Invalid input (should be a 3-dimensional array of numeric or integer values)", call. = FALSE)
  }
  if (dim(img)[3] == 4) {
    warning("Array with 4 dimensions, presumably with alpha channel. 4th dimension is ignored ...", call. = FALSE)
  }

  redChannel <- img[, , 1]
  greenChannel <- img[, , 2]
  blueChannel <- img[, , 3]
  out <- 0.2989 * redChannel + 0.5870 * greenChannel + 0.1140 * blueChannel
  return(out)
}

#' Matrix or Array Rotation by 90 Degrees
#'
#' @param img an array or a matrix
#' @param direction The direction of rotation by 90 degrees.
#'   The value can be \code{"positive"} (default) or
#'   \code{"negative"}. Aliases are
#'   \code{"counterclockwise"} and \code{"clockwise"},
#'   respectively.
#'
#' @details The function takes an array or matrix as input
#'   object (\code{img}) and returns the object rotated by
#'   90 degrees. Per default, the rotation is done in the
#'   mathematically positive direction (i.e.,
#'   counterclockwise). Clockwise rotation (i.e.,
#'   mathematically negative) can be specified by passing
#'   the value \code{"negative"} to the \code{direction}
#'   argument.
#'
#' @return an array or a matrix (rotated by 90 degrees)
#' @export
#'
#' @examples
#' # sample matrix
#' img <- matrix(1:6, ncol = 2)
#' img
#'
#' rotate90(img) # counterclockwise
#' rotate90(img, direction = "negative") # clockwise
rotate90 <- function(img, direction = "positive") {
  rot90 <- function(A, dir = "positive") {
    height <- dim(A)[1] # nrows / height
    width <- dim(A)[2] # ncols / width
    A <- t(A) # transpose matrix
    if (dir == "positive" | dir == "counterclockwise") {
      return(A[width:1, ]) # flip matrix rows
    }
    if (dir == "negative" | dir == "clockwise") {
      return(A[, height:1]) # flip matrix rows
    }
  }
  #
  if (!(direction == "positive" | direction == "counterclockwise" | direction == "negative" | direction == "clockwise")) {
    stop(paste0("'",direction,"' is an unknown input to parameter 'direction'. Try 'direction = positive' or 'direction = negative'."))
  }
  if (inherits(img, "matrix")) {
    return(rot90(img, direction))
  } else if (inherits(img, "array")) {
    # create array with same number of arrays but flipped dimensions
    out <- array(NA, dim = c(dim(img)[2], dim(img)[1], dim(img)[3]))
    #
    for (i in seq_len( dim(img)[3] )) {
      # for each array dimension
      out[, , i] <- rot90(img[, , i], direction)
    }
    return(out)
  }
  else {
    stop(paste0("Unknown input of type '", class(img),"' (has to be of type 'matrix' or 'array')"), call. = FALSE)
  }
}


#' .check_input
#'
#' \code{.check_input} is a helper function of the
#' \code{rquantae} package that checks whether the input is
#' a matrix of numeric or integer values. Error message are
#' thrown if that is not the case.
#'
#' @param img An object that needs to checked.
#' @param f_call The name of the function inside which the
#'   \code{.check_input} is called.
#'
#' @return An error message if the check fails.
#' @keywords internal
.check_input <- function(img, f_call = NULL){
  if (is.null(f_call)) stop("You have to specify a function for the f_call argument.", call. = FALSE)
  if (f_call == "symmetry" | f_call == "contrast" | f_call == "self-similarity") {
    # input not matrix or array?
    if (is.null(dim(img))) {
      stop("Invalid input (should be a matrix or a 3-dimensional array of numeric or integer values)", call. = FALSE)
    }
    # input a matrix?
    if (is.matrix(img)) {
      # must be numeric or integer
      if (!(is.numeric(img) | is.integer(img))) {
        stop("Input img has to be a matrix or array of *numeric* or *integer* values", call. = FALSE)
      }
      return("gray")
    } else if (is.array(img)) {
      # must be 3-dimensional array of integers or numeric values
      if (!is.numeric(img) | length(dim(img)) != 3 | !(dim(img)[3] == 3 | dim(img)[3] == 4)) {
        stop("Invalid array (should be a 3-dimensional array of numeric or integer values)", call. = FALSE)
      }
      if (dim(img)[3] == 4) {
        warning("Array with 4 dimensions, presumably with alpha channel. 4th dimension is ignored ...", call. = FALSE)
      }
      return("rgb")
    }
  } else stop("unknown input to f_call argument", call. = FALSE)
}


#' @keywords internal
.rand_string <- function(n = 6){
  paste0(letters[round(stats::runif(n, min = 1, max = 26))], collapse = "")
}
