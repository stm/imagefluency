#' @include utils.R
NULL

## -----------------------
##    self-similarity
## -----------------------
# the degree to which the log-log power spectrum
# falls with a slope of -2
#

#' Image self-similarity
#'
#' \code{img_self_similarity} returns the self-similarity of an image (i.e., the
#' degree to which the log-log power spectrum of the image falls with a slope of
#' -2). Higher values indicate higher image self-similarity.
#'
#' @details The function takes a (square) array or matrix of numeric or integer
#'   values representing an image as input and returns the self-similarity of
#'   the image. Self-similarity is computed via the slope of the log-log power
#'   spectrum using OLS. A slope near \code{-2} indicates fractal-like
#'   properties (see Redies et al., 2007; Simoncelli & Olshausen, 2001). Thus,
#'   value for self-similarity that is return by the function calculated as
#'   \code{self-similarity = abs(slope + 2) * (-1)}. That is, the measure
#'   reaches its maximum value of 0 for a slope of -2, and any deviation from -2
#'   results in negative values that are more negative the higher the deviation
#'   from -2. For color images, the weighted average between each color channel's
#'   values is computed (cf. Mayer & Landwehr 2018).
#'
#'   Per default, only the frequency range betwen 10 and 256 cycles per image is
#'   used for interpolation. Computation for the full range can be set via the
#'   parameter \code{full = TRUE}.
#'
#'   If \code{logplot} is set to \code{TRUE} then a log-log plot of the power
#'   spectrum is additionally shown. If the package \code{ggplot2} is installed
#'   the plot includes the slope of the OLS regression. Note that this option is
#'   currently implemented for grayscale images.
#'
#'   It is possible to get the raw regression slope (instead of the transformed
#'   value which indicates self-similarity) by using the option \code{raw =
#'   TRUE}.
#'
#'   For color images, the weighed average between each color channel's values
#'   is computed.
#'
#'
#' @param img An image in form of a matrix or array of numeric values,
#'   preferably by square size. If the input is not square, bilinear resizing to
#'   a square size is performed using the
#'   \code{\link[OpenImageR:resizeImage]{OpenImageR}} package. Use e.g.
#'   \code{\link{img_read}()} to read an image file into \code{R}.
#' @param full logical. Should the full frequency range be used for
#'   interpolation? (default: \code{FALSE})
#' @param logplot logical. Should the log-log power spectrum of the image be
#'   plotted? (default: \code{FALSE})
#' @param raw logical. Should the raw value of the regression slope be returned?
#'   (default: \code{FALSE})
#'
#' @return a numeric value (self-similarity)
#' @export
#'
#' @note The function inspired by Matlab's sfPlot (by Diederick C. Niehorster).
#'
#'
#' @references Mayer, S. & Landwehr, J, R. (2018). Quantifying Visual Aesthetics
#'   Based on Processing Fluency Theory: Four Algorithmic Measures for
#'   Antecedents of Aesthetic Preferences. \emph{Psychology of Aesthetics,
#'   Creativity, and the Arts}, \emph{12}(4), 399--431.
#'   \doi{10.1037/aca0000187}
#'
#' @references Redies, C., Hasenstein, J., & Denzler, J. (2007). Fractal-like
#'   image statistics in visual art: Similarity to natural scenes. \emph{Spatial
#'   Vision}, \emph{21}, 137--148.
#'   \doi{10.1163/156856807782753921}
#'
#' @references Simoncelli, E. P., & Olshausen, B. A. (2001). Natural image
#'   statistics and neural representation. \emph{Annual Review of Neuroscience},
#'   \emph{24}, 1193--1216.
#'   \doi{10.1146/annurev.neuro.24.1.1193}
#'
#'
#' @examples
#' # Example image with high self-similarity: romanesco
#' romanesco <- img_read(system.file("example_images", "romanesco.jpg", package = "imagefluency"))
#' #
#' # display image
#' grid::grid.raster(romanesco)
#' #
#' # get self-similarity
#' img_self_similarity(romanesco)
#'
#' # Example image with low self-similarity: office
#' office <- img_read(system.file("example_images", "office.jpg", package = "imagefluency"))
#' #
#' # display image
#' grid::grid.raster(office)
#' #
#' # get self-similarity
#' img_self_similarity(office)
#'
#' @seealso \code{\link{img_read}}, \code{\link{img_contrast}},
#'   \code{\link{img_complexity}}, \code{\link{img_simplicity}},
#'   \code{\link{img_symmetry}}, \code{\link{img_typicality}},
img_self_similarity <- function(img, full = FALSE, logplot = FALSE, raw=FALSE){

  # check input
  imgtype <- .check_input(img, f_call = "self-similarity")
  if (!is.logical(full)) {
    warning(paste0("full = '", full, "' is not a logical value (TRUE/FALSE). Automatically set to FALSE ..."), call. = FALSE)
    full <- FALSE
  }
  if (!is.logical(logplot)) {
    warning(paste0("logplot = '", logplot, "' is not a logical value (TRUE/FALSE). Skipping log-log plot ..."), call. = FALSE)
    logplot <- FALSE
  }
  if (!is.logical(raw)) {
    warning(paste0("raw = '", raw, "' is not a logical value (TRUE/FALSE). Automatically set to FALSE ..."), call. = FALSE)
    raw <- FALSE
  }

  # compute self-similarity
  if (imgtype == "rgb") {
    # split image into channels
    redChannel <- img[, , 1]
    greenChannel <- img[, , 2]
    blueChannel <- img[, , 3]
    #
    outR <- .selfsim(redChannel, full, raw, logplot)
    outG <- .selfsim(greenChannel, full, raw, logplot)
    outB <- .selfsim(blueChannel, full, raw, logplot)

    out <- 0.2989 * outR$self_sim + 0.5870 * outG$self_sim + 0.1140 * outB$self_sim
  } else {
    outGray <- .selfsim(img, full, raw, logplot)
    out <- outGray$self_sim
  }

  # plot
  if (logplot) {
    # currently only supported for grayscale images
    if (imgtype == "gray") {
      xvals <- outGray$xvals
      yvals <- outGray$yvals
      coord1 <- outGray$coord1
      coord2 <- outGray$coord2
      if (requireNamespace("ggplot2", quietly = TRUE) & requireNamespace("scales", quietly = TRUE)) {
        g <- ggplot2::ggplot(data = data.frame(xvals, yvals), ggplot2::aes(xvals, yvals)) +
          ggplot2::geom_line() +
          # ggplot2::geom_smooth(method = stats::lm, se = FALSE) +
          ggplot2::geom_segment(ggplot2::aes(x=coord1[1], y=coord1[2], xend=coord2[1], yend=coord2[2]), color = "red") +
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
        suppressWarnings(print(g))
      } else {
        graphics::plot(xvals, yvals, log = "xy",
             type = "l",
             xlab = "Spatial frequency (cycles/image)",
             ylab = "Energy")
        graphics::segments(x0=coord1[1], y0=coord1[2], x1=coord2[1], y1=coord2[2], col="red")
      }
    } else warning("Plot option currently only supports grayscale images. No plot is shown.", call. = FALSE)
  }

  # return self-similarity
  return(out)
}

# fix "no visible binding for global variable '.x'" NOTE (from scales package)
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".x"))


#' .selfsim
#'
#' Returns the self-similarity of an image matrix as the degree to which the slope of the log-log power spectrum falls with power according to the value of 2.
#'
#' @param img A matrix of numeric values or integer values, preferably
#'   by square size.
#' @param full logical. Should the full frequency range be used for
#'   interpolation?
#' @param logplot logical. Should the log-log power spectrum of the image be
#'   plotted?
#' @param raw logical. Should the raw value of the regression slope be returned?
#'
#'
#' @return a numeric value (self-similarity)
#' @keywords internal
.selfsim <- function(img, full, raw, logplot){
  if (min(dim(img)) < 22) {
    stop(paste0("Image has to be at least 22 pixels in each dimension. Input image has ",
                dim(img)[2], " (width) x ", dim(img)[1], " (height)."),
         call. = FALSE)
  }
  xs <- dim(img)[1] # image height
  ys <- dim(img)[2] # image width

  # check for squared matrix
  if (xs != ys) {
    if (requireNamespace("OpenImageR", quietly = TRUE)) {
      square_dim <- min(xs, ys)
      if (square_dim %% 2 == 1) { # odd size
        square_dim <- square_dim - 1
      }
      img <- OpenImageR::resizeImage(img, width = square_dim,
                                     height = square_dim,
                                     method = "bilinear")
      xs <- dim(img)[1]
      ys <- dim(img)[2]

      # warning("The input image was not squared. Rescaling to a squared matrix was performed.",
      #        call. = FALSE)
    } else {
      stop("Package 'OpenImageR' is required but not installed on your system.", call. = FALSE)
    }
  }

  # apply fft2
  img_fft <- stats::fft(img)
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

  Xt <- spat_freq[seq(lmrange[1], lmrange[2])]
  Xt_log <- log(Xt)
  Yt <- power_spec[seq(lmrange[1], lmrange[2])]
  Yt_log <- log(Yt)

  # calculate slope of regression
  regr <- stats::lm(Yt_log ~ Xt_log)
  slope <- unname(stats::coefficients(regr)[2])
  # sprintf("%.4f",slope)
  coord1 <- unname(c(lmrange[1], exp(stats::fitted(regr)[1])))
  coord2 <- unname(c(lmrange[2], exp(utils::tail(stats::fitted(regr),1))))

  # self-similarity is the degree to which slope is -2
  if (raw == TRUE) {
    self_sim <- slope
  } else {
    self_sim <- abs(slope + 2)*(-1)
  }

  # return results
  if(logplot) return(list(self_sim = self_sim, xvals=spat_freq, yvals=power_spec, coord1=coord1, coord2=coord2))
  if(!logplot) return(list(self_sim = self_sim))
}
