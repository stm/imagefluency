#' @include utils.R
NULL

## -----------------------
##    self-similarity
## -----------------------
# the degree to which the log-log power spectrum
# falls with a slope of -2
#
# sfPlot inspired by Diederick C. Niehorster
# see http://www.mathworks.com/matlabcentral/newsreader/view_original/799264
#
# NOTE: this has to be a squared dimension (or at least a quadratic matrix)

quantify_self_similarity <- function(img, full = FALSE, logplot = FALSE){

  # check input
  .check_input(img, f_call = "contrast")

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
    if (requireNamespace("ggplot2", quietly = TRUE)) {
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
  return(list(self_similarity = self_sim))
}
