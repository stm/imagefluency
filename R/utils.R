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
#' (red/green/blue) into a matrix of grayscale values.
#'
#' @param img 3-dimensional array of numeric or integer
#'   values
#'
#' @return A matrix of grayscale values.
#'
#' @details The function takes a 3-dimensional array of
#'   numeric or integer values as input (\code{img}) and
#'   returns a matrix of grayscale values as output. The
#'   grayscale values are computed as \code{GRAY = 0.299 *
#'   RED + 0.587 * GREEN + 0.114 * BLUE} (as recommended in
#'   ITU-R BT.601). If the array has a fourth dimension
#'   (i.e., alpha channel), the fourth dimension is ignored.
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
  # Rec. 601 weights; written so that equal channels return their value exactly
  out <- blueChannel + 0.299 * (redChannel - blueChannel) + 0.587 * (greenChannel - blueChannel)
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
#'   90 degrees. By default, the rotation is done in the
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
      return(A[width:1, , drop = FALSE]) # flip matrix rows
    }
    if (dir == "negative" | dir == "clockwise") {
      return(A[, height:1, drop = FALSE]) # flip matrix columns
    }
  }
  #
  if (length(direction) != 1 || !(direction %in% c("positive", "counterclockwise", "negative", "clockwise"))) {
    stop(paste0("'", paste(direction, collapse = ", "), "' is an unknown input to parameter 'direction'. Try 'direction = positive' or 'direction = negative'."))
  }
  if (inherits(img, "matrix")) {
    return(rot90(img, direction))
  } else if (inherits(img, "array")) {
    if (length(dim(img)) != 3) {
      stop("Unknown input (has to be a matrix or a 3-dimensional array)", call. = FALSE)
    }
    # create array with same number of arrays but flipped dimensions
    out <- array(NA, dim = c(dim(img)[2], dim(img)[1], dim(img)[3]))
    #
    for (i in seq_len( dim(img)[3] )) {
      # for each array dimension
      out[, , i] <- rot90(matrix(img[, , i], nrow = dim(img)[1]), direction)
    }
    return(out)
  }
  else {
    stop(paste0("Unknown input of type '", class(img),"' (has to be of type 'matrix' or 'array')"), call. = FALSE)
  }
}


#' .prepare_img
#'
#' \code{.prepare_img} is a helper function of the \code{imagefluency} package
#' that checks whether the input is a valid image and returns the image in a
#' standard form. All \code{img_*} functions use this function so that the same
#' input is handled in the same way by every function.
#'
#' The following is checked: (1) the input is a matrix or an array (not, e.g.,
#' a vector or a data frame), (2) the values are numeric or integer, (3) the
#' input is a matrix (grayscale image) or a 3-dimensional array with 1
#' (grayscale), 2 (grayscale with alpha channel), 3 (color), or 4 (color with
#' alpha channel) layers in the third dimension, and (4) the input contains no
#' missing or non-finite values. An alpha channel is removed with a warning.
#'
#' @param img An object that needs to be checked.
#'
#' @return A list with the elements \code{img} (a matrix for grayscale images
#'   or a 3-dimensional array with 3 layers for color images) and \code{type}
#'   (\code{"gray"} or \code{"rgb"}). An error is thrown if the check fails.
#' @keywords internal
.prepare_img <- function(img){
  # input not matrix or array?
  if (is.null(dim(img)) || is.data.frame(img)) {
    stop("Invalid input (should be a matrix or a 3-dimensional array of numeric or integer values)", call. = FALSE)
  }
  n_dim <- length(dim(img))

  if (is.matrix(img)) {
    # must be numeric or integer
    if (!is.numeric(img)) {
      stop("Input img has to be a matrix or array of *numeric* or *integer* values", call. = FALSE)
    }
    type <- "gray"
  } else {
    # must be 3-dimensional array of integers or numeric values
    if (!is.array(img) || !is.numeric(img) || n_dim != 3 || !(dim(img)[3] %in% 1:4)) {
      stop("Invalid array (should be a 3-dimensional array of numeric or integer values)", call. = FALSE)
    }
    n_layers <- dim(img)[3]
    if (n_layers == 4) {
      warning("Array with 4 dimensions, presumably with alpha channel. 4th dimension is ignored ...", call. = FALSE)
      img <- img[, , 1:3, drop = FALSE]
    }
    if (n_layers == 2) {
      warning("Array with 2 dimensions, presumably grayscale with alpha channel. 2nd dimension is ignored ...", call. = FALSE)
    }
    if (n_layers <= 2) {
      # grayscale image stored as array: convert to matrix
      img <- matrix(img[, , 1], nrow = dim(img)[1], ncol = dim(img)[2])
      type <- "gray"
    } else {
      type <- "rgb"
    }
  }

  # missing or non-finite values?
  if (anyNA(img) || !all(is.finite(range(img)))) {
    stop("Input image contains missing or non-finite values (NA, NaN, or Inf)", call. = FALSE)
  }

  return(list(img = img, type = type))
}


#' .check_flag
#'
#' \code{.check_flag} is a helper function of the \code{imagefluency} package
#' that checks whether an optional parameter is a single logical value
#' (\code{TRUE} or \code{FALSE}). If that is not the case, a warning is thrown
#' and the default value is returned.
#'
#' @param x The value of the parameter.
#' @param name The name of the parameter.
#' @param default The default value that is used if the check fails.
#' @param action Character string. End of the warning message.
#'
#' @return \code{x} if the check is passed, otherwise \code{default}.
#' @keywords internal
.check_flag <- function(x, name, default = FALSE,
                        action = paste0("Automatically set to ", default, " ...")){
  if (is.logical(x) && length(x) == 1 && !is.na(x)) {
    return(x)
  }
  warning(paste0(name, " = '", paste(x, collapse = ", "), "' is not a logical value (TRUE/FALSE). ", action), call. = FALSE)
  return(default)
}


#' .img_names
#'
#' Returns the names of the images in a list: the names of the list elements,
#' and \code{img1}, \code{img2}, ... for list elements without a name.
#'
#' @param imglist A list of images.
#'
#' @return a character vector
#' @keywords internal
.img_names <- function(imglist){
  imgnames <- paste0("img", seq_along(imglist))
  if (!is.null(names(imglist))) {
    has_name <- !is.na(names(imglist)) & names(imglist) != ""
    imgnames[has_name] <- names(imglist)[has_name]
  }
  return(imgnames)
}


#' @keywords internal
.pkg_avail <- function(pkg){
  requireNamespace(pkg, quietly = TRUE) == TRUE
}


#' @keywords internal
.normalize_img <- function(img) {
  # compute range
  rng <- range(img, na.rm = TRUE)
  # check for non-finite values
  if (!all(is.finite(rng))) {
    stop("Input image contains non-finite values.", call. = FALSE)
  }
  # if values are already in [0, 1] range, return image
  if (rng[1] >= 0 && rng[2] <= 1) {
    return(img)
  }
  # check if values are in 0-255 range (8-bit format)
  if (rng[1] >= 0 && rng[2] <= 255) {
    return(img / 255)
  }
  # check for constant image
  if (rng[1] == rng[2]) {
    fill <- if (rng[1] <= 0) 0 else 1
    return(array(fill, dim = dim(img), dimnames = dimnames(img)))
  }
  # normalize to [0, 1] (min-max)
  (img - rng[1]) / diff(rng)
}
