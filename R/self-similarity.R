#' @include utils.R
NULL

## -----------------------
##    self-similarity
## -----------------------
# the degree to which the log-log power spectrum
# falls with a slope of -2
#

#' Self-Similarity of an Image
#'
#' \code{quantify_self-similarity} returns the
#' self-similarity of an image (i.e., the degree to which
#' the log-log power spectrum of the image falls with a
#' slope of -2).
#'
#' @details The function takes a matrix of numeric or
#'   integer values representing an image as input and
#'   returns the self-similarity of the image.
#'   Self-similarity is computed via the slope of the
#'   log-log power spectrum using OLS. A slope near
#'   \code{-2} indicates fractal-like properties. Thus,
#'   value for self-similarity that is return by the
#'   function calculated as \code{self-similarity =
#'   abs(slope + 2) * (-1)}. That is, the measure reaches
#'   its maximum value of 0 for a slope of -2, and any
#'   deviation from -2 results in negative values that are
#'   more negative the higher the deviation from -2.
#'
#'   Per default, only the frequency range betwen 10 and 256
#'   cycles per image is used for interpolation. Computation
#'   for the full range can be set via the parameter
#'   \code{full = TRUE}.
#'
#'   If \code{logplot} is set to \code{TRUE} then a log-log
#'   plot of the power spectrum is performed. If the package
#'   \code{ggplot2} is installed the plot includes the slope
#'   of the OLS regression.
#'
#'
#'
#' @param img A matrix of numeric values or integer values.
#'   Color images have to be converted to grayscale in
#'   advance (function \code{rgb2gray}) or each color
#'   channel has to be analyzed seperately.
#' @param full logical. Should the full frequency range be
#'   used for interpolation?
#' @param logplot logical. Should the log-log power spectrum
#'   of the image be plotted?
#'
#' @return a numeric value (self-similarity)
#' @export
#'
#' @note The function inspired by Matlab's sfPlot (by
#'   Diederick C. Niehorster) see
#'   \url{http://www.mathworks.com/matlabcentral/newsreader/view_original/799264}
#'
#'
#' @importFrom stats coefficients fft lm
#' @importFrom graphics plot
#'
#' @references Redies, C., Hasenstein, J., & Denzler, J.
#'   (2007). Fractal-like image statistics in visual art:
#'   Similarity to natural scenes. \emph{Spatial Vision},
#'   \emph{21}, 137--148.
#'   doi:\href{https://doi.org/10.1163/156856807782753921}{10.1163/156856807782753921}
#'
#'
#' @examples
#' # construct sample image
#' img <- matrix(0, nrow=100, ncol=100)
#' img[11:90,11:90] <- 0.25
#' img[21:80, 21:80] <- 0.5
#' img[31:70, 31:70] <- 0.75
#' img[41:60, 41:60] <- 1
#'
#' # show image
#' grid::grid.raster(img)
#'
#' # get self-similarity
#' quantify_self_similarity(img)
#'
#' # ------------------------------
#' # construct another sample image
#' img2 <- matrix(runif(100*100), nrow=100, ncol=100)
#'
#' # show image
#' grid::grid.raster(img2)
#'
#' # get self-similarity
#' quantify_self_similarity(img2)
#'
#' @seealso \code{\link{rgb2gray}},
#'   \code{\link{quantify_symmetry}},
#'   \code{\link{quantify_complexity}},
#'   \code{\link{quantify_contrast}},
#'   \code{\link{quantify_typicality}}
quantify_self_similarity <- function(img, full = FALSE, logplot = FALSE){

  # check input
  .check_input(img, f_call = "self-similarity")
  if (!is.logical(full)) {
    warning(paste0("full = '", full, "' is not a logical value (TRUE/FALSE). Automatically set to FALSE ..."), call. = FALSE)
    full <- FALSE
  }
  if (!is.logical(logplot)) {
    warning(paste0("logplot = '", logplot, "' is not a logical value (TRUE/FALSE). Skipping log-log plot ..."), call. = FALSE)
    logplot <- FALSE
  }

  if (min(dim(img)) < 22) {
    stop(paste0("Image has to be at least 22 pixels in each dimension. Input image has ",
                dim(img)[2], " (width) x ", dim(img)[1], " (height)."),
         call. = FALSE)
  }
  xs <- dim(img)[1] # image height
  ys <- dim(img)[2] # image width

  # apply fft2
  img_fft <- fft(img)
  # shift center
  img_fft_shifted <- pracma::circshift(img_fft, floor(dim(img_fft)/2))
  img_fft_shifted <- abs(img_fft_shifted)^2

  f2 <- seq(-xs/2, xs/2 - 1)
  f1 <- seq(-ys/2, ys/2 - 1)

  # create meshgrid
  XXYY <- pracma::meshgrid(f1, f2)
  XX <- XXYY[[1]]
  YY <- XXYY[[2]]

  # transform coordinates
  rtmatrix <- pracma::cart2pol(cbind(as.vector(XX), as.vector(YY)))
  # tmatrix <- matrix(rtmatrix[, 1], nrow = xs, ncol = ys)
  rmatrix <- matrix(rtmatrix[, 2], nrow = xs, ncol = ys)

  if (xs %% 2 == 1 || ys %% 2 == 1) {
    rmatrix <- round(rmatrix) - 1
  } else {
    rmatrix <- round(rmatrix)
  }

  # compute rotational average (power spectrum)
  power_spec <- pracma::accumarray(as.vector(rmatrix) + 1, as.vector(img_fft_shifted)) / pracma::accumarray(as.vector(rmatrix) + 1, rep(1, length(as.vector(rmatrix))))
  power_spec <- power_spec[seq(2, floor(min(xs,ys)/2) + 1)]


  if (!full) {
    if (min(dim(img))/2 < 256) {
      lmrange <- c( 10, floor( min(dim(img)) / 2 ) )
    } else {
      lmrange <- c( 10, 256 )
    }
  } else {
    lmrange <- c( 1, floor( min(dim(img)) / 2 ) )
  }

  # overall possible frequencies (i.e., all frequencies)
  spat_freq <-  seq( 1, floor( min(dim(img)) / 2 ) )

  Xt <- log(spat_freq[seq(lmrange[1], lmrange[2])])
  Yt <- log(power_spec[seq(lmrange[1], lmrange[2])])

  # calculate slope of regression
  slope <- unname(coefficients(lm(Yt ~ Xt))[2])
  # sprintf("%.4f",slope)

  if (logplot) {
    xvals <- spat_freq
    yvals <- power_spec
    if (requireNamespace("ggplot2", quietly = TRUE) & requireNamespace("scales", quietly = TRUE)) {
      g <- ggplot2::ggplot(data = data.frame(xvals, yvals), ggplot2::aes(xvals, yvals)) +
        ggplot2::geom_line() +
        ggplot2::geom_smooth(method = lm, se = FALSE) +
        ggplot2::scale_x_log10(
          breaks = scales::trans_breaks("log10", function(x) 10^x),
          labels = scales::trans_format("log10", scales::math_format(10^.x))
        ) +
        ggplot2::scale_y_log10(
          breaks = scales::trans_breaks("log10", function(x) 10^x),
          labels = scales::trans_format("log10", scales::math_format(10^.x))
        ) + ggplot2::annotation_logticks() +
        ggplot2::labs(x = "Spatial frequency (cycles/image)", y = "Energy") +
        ggplot2::theme_bw()
      print(g)
    } else {
      plot(xvals, yvals, log = "xy",
           type = "l",
           xlab = "Spatial frequency (cycles/image)",
           ylab = "Energy")
    }
  }

  # self-similarity is the degree to which slope is -2
  self_sim <- abs(slope + 2)*(-1)

  # return results
  return(self_sim)
}
