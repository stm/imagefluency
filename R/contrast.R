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
#'   contrast.
#'
#'   As the function assumes that the pixel intensity values
#'   of the image \code{img} are in the range [0, 255],
#'   pixel normalization into [0, 1] is done by default. If
#'   \code{normalize} is set to \code{FALSE} no
#'   normalization is performed.
#'
#'
#' @param img A matrix of numeric values or integer values.
#'   Color images have to be converted to grayscale in
#'   advance (function \code{rgb2gray}) or each color
#'   channel has to be analyzed seperately. The image is
#'   assumed to have its pixel intensities \strong{not}
#'   normalized but to be in the range [0, 255]
#' @param normalize logical. Should pixel intensity
#'   normalization into range [0, 1] be performed?
#'
#' @return a list of a numeric value (RMS contrast)
#' @export
#'
#' @examples
#' # construct sample image
#' img <- matrix(0, nrow=100, ncol=100)
#' img[21:80, 21:80] <- 255
#'
#' # if you want to inspect the image
#' # OpenImageR::imageShow(img)
#'
#' # get contrast
#' quantify_contrast(img)
#'
#' @references Peli, E. (1990). Contrast in complex images.
#'   \emph{Journal of the Optical Society of America A},
#'   \emph{7}, 2032--2040.
#'   doi:\href{https://doi.org/10.1364/JOSAA.7.002032}{10.1364/JOSAA.7.002032}
#'
#' @seealso \code{\link{rgb2gray}},
#'   \code{\link{quantify_symmetry}},
#'   \code{\link{quantify_complexity}},
#'   \code{\link{quantify_typicality}},
#'   \code{\link{quantify_self_similarity}}
#'
#' @importFrom stats sd
quantify_contrast <- function(img, normalize = TRUE){

  # check input
  .check_input(img, f_call = "contrast")
  if (!is.logical(normalize)) stop("parameter 'normalize' has to be a logical value (TRUE/FALSE)", call. = FALSE)

  ## -----------------------
  ##      rms contrast
  ## -----------------------


  pixAll <- as.vector(img)

  # check if input is already normalized (or negative values)
  if (min(pixAll) < 0) warning("Negative pixel intensity values in your image. Result might not be meaningful.", call. = FALSE)
  if (min(pixAll) >= 0 & max(pixAll) <= 1 & normalize == TRUE) {
    warning("Input image might already be normalized (all pixel intensity values between [0, 1]). Consider turning option 'normalize' to FALSE.", call. = FALSE)
  }

  # normalize image by default
  if (normalize) pixAll <- pixAll / 255

  # via built-in sd function
  return(list(contrast = sd(pixAll)))

  # # alternative 1: via normalization
  # pixAllMean <- pixAll - mean(pixAll)
  # norm(pixAllMean,'2')/sqrt(length(pixAllMean))

  # # alternative 2: direct formula
  # sqrt(sum((pixAll - mean(pixAll))^2)/length(pixAll))
}
