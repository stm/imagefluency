#' @include utils.R
NULL

#' RMS Contrast of an Image
#'
#' \code{quantify_contrast} returns the RMS contrast of an
#' image matrix \code{img}. A higher value indicates higher
#' contrast.
#'
#' @details The function returns the RMS contrast of an
#'   image matrix \code{img}. The RMS contrast is defined as
#'   the standard deviation of the normalized pixel
#'   intensity values. A higher value indicates higher
#'   contrast. The image is automatically normalized if
#'   necessary (i.e., normalization into range [0, 1]).
#'
#'
#' @param img A matrix of numeric values or integer values.
#'   Color images have to be converted to grayscale in
#'   advance (function \code{rgb2gray}) or each color
#'   channel has to be analyzed seperately.
#'
#' @return a numeric value (RMS contrast)
#' @export
#'
#' @examples
#' # Example image with high contrast: img_berries
#' #
#' # display image
#' grid::grid.raster(img_berries)
#' # convert to grayscale
#' berries_grayscale <- rgb2gray(img_berries)
#' # get contrast
#' quantify_contrast(berries_grayscale)
#'
#' # Example image with low contrast: img_bike
#' #
#' # display image
#' grid::grid.raster(img_bike)
#' # convert to grayscale
#' bike_grayscale <- rgb2gray(img_bike)
#' # get contrast
#' quantify_contrast(bike_grayscale)
#'
#' @references Peli, E. (1990). Contrast in complex images.
#'   \emph{Journal of the Optical Society of America A},
#'   \emph{7}, 2032--2040.
#'   doi:\href{https://doi.org/10.1364/JOSAA.7.002032}{10.1364/JOSAA.7.002032}
#'
#'
#' @seealso \code{\link{rgb2gray}},
#'   \code{\link{quantify_symmetry}},
#'   \code{\link{quantify_complexity}},
#'   \code{\link{quantify_typicality}},
#'   \code{\link{quantify_self_similarity}}
#'
#' @importFrom stats sd
quantify_contrast <- function(img){

  # check input
  .check_input(img, f_call = "contrast")

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
  return(sd(pixAll))

  # # alternative 1: via normalization
  # pixAllMean <- pixAll - mean(pixAll)
  # norm(pixAllMean,'2')/sqrt(length(pixAllMean))

  # # alternative 2: direct formula
  # sqrt(sum((pixAll - mean(pixAll))^2)/length(pixAll))
}
