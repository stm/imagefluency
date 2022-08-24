#' @include utils.R
NULL

#' Image contrast
#'
#' \code{img_contrast} returns the RMS contrast of an image \code{img}. A higher
#' value indicates higher contrast.
#'
#' @details The function returns the RMS contrast of an image \code{img}. The
#'   RMS contrast is defined as the standard deviation of the normalized pixel
#'   intensity values. A higher value indicates higher contrast. The image is
#'   automatically normalized if necessary (i.e., normalization into range [0,
#'   1]).
#'
#'   For color images, the weighted average between each color channel's values
#'   is computed.
#'
#'
#' @param img An image in form of a matrix or array of numeric values. Use e.g.
#'   \code{\link{img_read}()} to read an image file into \code{R}.
#'
#' @return a numeric value (RMS contrast)
#' @export
#'
#' @examples
#' # Example image with relatively high contrast: berries
#' berries <- img_read(system.file("example_images", "berries.jpg", package = "imagefluency"))
#' #
#' # display image
#' grid::grid.raster(berries)
#' #
#' # get contrast
#' img_contrast(berries)
#'
#' # Example image with relatively low contrast: bike
#' bike <- img_read(system.file("example_images", "bike.jpg", package = "imagefluency"))
#' #
#' # display image
#' grid::grid.raster(bike)
#' #
#' # get contrast
#' img_contrast(bike)
#'
#' @references Peli, E. (1990). Contrast in complex images. \emph{Journal of the
#'   Optical Society of America A}, \emph{7}, 2032--2040.
#'   \doi{10.1364/JOSAA.7.002032}
#'
#'
#' @seealso \code{\link{img_read}}, \code{\link{img_complexity}},
#'   \code{\link{img_self_similarity}}, \code{\link{img_simplicity}},
#'   \code{\link{img_symmetry}}, \code{\link{img_typicality}},
#'
#'
img_contrast <- function(img){

  # check input
  imgtype <- .check_input(img, f_call = "contrast")

  # compute and return contrast
  if (imgtype == "rgb") {
    # split image into channels
    redChannel <- img[, , 1]
    greenChannel <- img[, , 2]
    blueChannel <- img[, , 3]
    #
    out <- 0.2989 * .contr(redChannel) + 0.5870 * .contr(greenChannel) + 0.1140 * .contr(blueChannel)
    return(out)
    #
  } else return(.contr(img))
}

#' .contr
#'
#' Returns the RMS contrast of an image matrix.
#'
#' @param img A matrix of numeric values or integer values.
#'
#' @return a numeric value (RMS contrast)
#' @keywords internal
.contr <- function(img) {

  ## -----------------------
  ##      rms contrast
  ## -----------------------

  pixAll <- as.vector(img)

  # check range of input values
  if (min(pixAll) < 0) {
    warning("Negative pixel intensity values in your image. Corresponding pixels set to 0.", call. = FALSE)
    pixAll[pixAll < 0] <- 0
  }
  if (max(pixAll) > 255) {
    warning("Pixel intensity values > 255 in your image. Corresponding pixels set to 255.", call. = FALSE)
    pixAll[pixAll > 255] <- 255
  }

  # normalize image if necessary
  if (max(pixAll) > 1) {
    pixAll <- pixAll / 255
  }

  # RMS via built-in sd function
  return(stats::sd(pixAll))

  # # alternative 1: via normalization
  # pixAllMean <- pixAll - mean(pixAll)
  # norm(pixAllMean,'2')/sqrt(length(pixAllMean))

  # # alternative 2: direct formula
  # sqrt(sum((pixAll - mean(pixAll))^2)/length(pixAll))
}
