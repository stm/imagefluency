#' @include utils.R
NULL

#' Image symmetry
#'
#' \code{img_symmetry} returns the vertical and horizontal mirror symmetry of an
#' image. Higher values indicate higher image symmetry.
#'
#' @details The function returns the vertical and horizontal mirror symmetry of
#'   an image \code{img}. Symmetry values can range between 0 (not symmetrical)
#'   and 1 (fully symmetrical). If \code{vertical} or \code{horizontal} is set
#'   to \code{FALSE} then vertical or horizontal symmetry is not computed,
#'   respectively.
#'
#'   As the perceptual mirror axis is not necessarily exactly in the middle of a
#'   picture, the function estimates in a first step several symmetry values
#'   with different positions for the mirror axis. To this end, the mirror axis
#'   is automatically shifted up to 5\% (default) of the image width to the left
#'   and to the right (in the case of vertical symmetry; analogously for
#'   horizontal symmetry). In the second step, the overall symmetry score is
#'   computed as the maximum of the symmetry scores given the different mirror
#'   axes. See Mayer & Landwehr (2018) for details.
#'
#'   Advanced users can change the shift range with the optional parameter
#'   \code{shift_range}, which takes a numeric decimal as input. The default
#'   \code{shift_range = 0.05} (i.e., 5\%).
#'
#'   Symmetry cannot be computed if one of the image halves has no variation
#'   (e.g., an image half that has only one color; for color images, this
#'   applies to each color channel). In this case, the function returns
#'   \code{NA} for the respective symmetry and throws a warning. For color
#'   images with a color channel that has no variation (e.g., an image that
#'   only consists of black and red), consider converting the image into
#'   grayscale first (see \code{\link{rgb2gray}}).
#'
#'   For color images, the default is that first a maximal symmetry score (as
#'   explained above) is obtained per color channel (parameter \code{per_channel
#'   = TRUE}). Subsequently, a weighted average between each color channel's
#'   maximal score is computed as the image's overall symmetry. Advanced users
#'   can reverse this order by setting \code{per_channel = FALSE}. This results
#'   in first computing the weighted averages for each position of the mirror
#'   axis separately, and afterwards finding the maximal overall symmetry score.
#'
#'   To keep the computation fast, the best position of the mirror axis is
#'   located with a fast Fourier transform (FFT), which yields the symmetry
#'   values of all positions of the mirror axis at once. The symmetry score
#'   for this best position is then computed directly as the correlation
#'   between the two image halves. The result is the same as when the
#'   correlation is computed directly for every position of the mirror axis,
#'   unless the symmetry values of two positions are practically tied
#'   (difference smaller than about 1e-10). In this rare case, the score might
#'   differ by a correspondingly small amount. Setting \code{exact = TRUE}
#'   computes the correlation directly for every position of the mirror axis
#'   (the method used up to version 1.0.1), which avoids this at the expense of
#'   speed (the computation takes several times longer; for large images, more
#'   than ten times longer).
#'
#'   A perfectly symmetric image (i.e., the two image halves are identical
#'   mirror images of each other) always receives a score of exactly 1.
#'
#'
#' @param img An image in form of a matrix or array of numeric values. Use e.g.
#'   \code{\link{img_read}()} to read an image file into \code{R}.
#' @param vertical logical. Should the vertical symmetry be computed? (default:
#'   TRUE)
#' @param horizontal logical. Should the horizontal symmetry be computed?
#'   (default: TRUE)
#' @param exact logical. Should the correlation be computed directly for every
#'   position of the mirror axis instead of locating the best position with a
#'   fast Fourier transform? This is considerably slower and only rarely leads
#'   to a (minimally) different result (see details). (default: FALSE)
#' @param ... Further options: \code{shift_range} to shift the mirror axis,
#'   \code{per_channel} to switch between a maximal per channel vs. per image
#'   symmetry (see details).
#'
#' @return a named vector of numeric values (vertical and horizontal symmetry)
#' @export
#'
#' @examples
#' # Example image with high vertical symmetry: rails
#' rails <- img_read(system.file("example_images", "rails.jpg", package = "imagefluency"))
#' #
#' # display image
#' grid::grid.raster(rails)
#' #
#' # get symmetry
#' img_symmetry(rails)
#'
#' # Example image with low vertical symmetry: bridge
#' bridge <- img_read(system.file("example_images", "bridge.jpg", package = "imagefluency"))
#' #
#' # display image
#' grid::grid.raster(bridge)
#' #
#' # get symmetry
#' img_symmetry(bridge)
#'
#' # compute the correlation directly for every position of the mirror axis
#' # (slower)
#' img_symmetry(bridge, exact = TRUE)
#'
#' @seealso \code{\link{img_read}}, \code{\link{img_complexity}},
#'   \code{\link{img_contrast}}, \code{\link{img_self_similarity}},
#'   \code{\link{img_simplicity}}, \code{\link{img_typicality}}
#'
#' @references Mayer, S. & Landwehr, J. R. (2018). Quantifying Visual Aesthetics
#'   Based on Processing Fluency Theory: Four Algorithmic Measures for
#'   Antecedents of Aesthetic Preferences. \emph{Psychology of Aesthetics,
#'   Creativity, and the Arts}, \emph{12}(4), 399--431.
#'   \doi{10.1037/aca0000187}
img_symmetry <- function(img, vertical = TRUE, horizontal = TRUE, exact = FALSE, ...) {

  # check options
  vertical <- .check_flag(vertical, "vertical", default = TRUE)
  horizontal <- .check_flag(horizontal, "horizontal", default = TRUE)
  if (!vertical & !horizontal) {
    warning("Both optional arguments cannot be FALSE. Try setting option 'vertical' or 'horizontal' to TRUE. Returning NA.", call. = FALSE)
    return(NA)
  }
  exact <- .check_flag(exact, "exact", default = FALSE)

  # check input
  img <- .prepare_img(img)
  color <- img$type == "rgb"
  img <- img$img

  # Note: the default is vertical symmetry.
  # If horizontal symmetry should also be computed, the image is simply rotated
  # by 90 degrees and then again "horizontal" symmetry is computed.

  # compute symmetry
  out <- c()
  if (vertical) {
    out <- c(out, vertical = .sym(img, color = color, exact = exact, ...))
  }
  if (horizontal) {
    out <- c(out, horizontal = .sym(rotate90(img), color = color, exact = exact, ...))
  }

  # symmetry cannot be computed without variation in the image halves
  for (direction in names(out)[is.na(out)]) {
    warning(paste0("No variation in at least one image half",
                   if (color) " of at least one color channel" else "",
                   ". Computation of ", direction, " symmetry not possible. Returning NA."),
            call. = FALSE)
  }

  # return symmetry
  return(out)
}

#' .sym
#'
#' Calculates the mirror symmetry of an
#' image by correlating image halves.
#'
#' @param img A matrix of numeric values or integer values.
#' @param color logical. Color image?
#' @param per_channel logical. Channel-wise maximum symmetry?
#' @param shift_range numeric. Percentage of axis shift.
#' @param exact logical. Compute the correlation directly for every position
#'   of the mirror axis?
#'
#' @return one numeric value as the symmetry (\code{NA} if there is no
#'   variation in the image halves)
#' @keywords internal
.sym <- function(img, color, per_channel = TRUE, shift_range = 0.05, exact = FALSE) {

  # check options
  if (!is.numeric(shift_range) || length(shift_range) != 1 || is.na(shift_range) ||
      shift_range < 0 || shift_range >= 1) {
    stop("'shift_range' has to be a single number between 0 and 1 (e.g., 0.05 for 5%)", call. = FALSE)
  }
  per_channel <- .check_flag(per_channel, "per_channel", default = TRUE)

  # image dimensions
  imgH <- dim(img)[1] # image height
  imgW <- dim(img)[2] # image width

  # symmetry will be computed around the respective centerline
  #
  # symmetry shift range (default: 5%)
  xrange <- 0:floor(imgW * shift_range)

  if (color == FALSE) {
    # grayscale image
    #
    # find symmetries
    return(.sym_max(list(img), imgW, xrange, exact))
  } else {
    # colored image
    channels <- lapply(1:3, function(i) matrix(img[, , i], nrow = imgH, ncol = imgW))

    #
    if (per_channel) {
      # maximal symmetry per channel, then weighted average
      sym_channels <- vapply(channels,
                             function(x) .sym_max(list(x), imgW, xrange, exact),
                             numeric(1))
      return(.weight_channels(sym_channels[1], sym_channels[2], sym_channels[3]))
    } else {
      # weighted average per axis position, then maximal symmetry
      return(.sym_max(channels, imgW, xrange, exact))
    }
  }
}

#' .weight_channels
#'
#' Weighted average of the values of the three color channels (Rec. 601
#' weights), written so that equal values return their value exactly.
#'
#' @param red,green,blue Numeric vectors with the values of the red, green,
#'   and blue channel.
#'
#' @return a numeric vector
#' @keywords internal
.weight_channels <- function(red, green, blue) {
  blue + 0.299 * (red - blue) + 0.587 * (green - blue)
}

#' .sym_max
#'
#' Returns the maximal mirror symmetry over all positions of the mirror axis.
#' By default, the best position of the mirror axis is located via
#' \code{.shift_axis_fft} and its symmetry is then computed directly via
#' \code{.sym_mirror}. With \code{exact = TRUE}, the symmetry is computed
#' directly for every position of the mirror axis via \code{.shift_axis}.
#'
#' @param channels A list with one matrix (grayscale image) or three matrices
#'   (color channels that share the position of the mirror axis).
#' @param imgW Image width (numeric).
#' @param xrange Shift range (numeric vector).
#' @param exact logical. Compute the correlation directly for every position
#'   of the mirror axis?
#'
#' @return one numeric value as the symmetry (\code{NA} if the symmetry cannot
#'   be computed for any position of the mirror axis)
#' @keywords internal
.sym_max <- function(channels, imgW, xrange, exact) {
  combine <- function(x) {
    if (length(x) == 1) x[[1]] else .weight_channels(x[[1]], x[[2]], x[[3]])
  }

  # symmetries for all positions of the mirror axis
  shift_fun <- if (exact) .shift_axis else .shift_axis_fft
  sym_all <- combine(lapply(channels, shift_fun, imgW = imgW, xrange = xrange))

  # positions without variation in an image half are ignored (NA)
  if (all(is.na(sym_all))) {
    return(NA_real_)
  }
  if (exact) {
    return(max(sym_all, na.rm = TRUE))
  }

  # best position of the mirror axis ...
  best <- which.max(sym_all)
  if (best <= length(xrange)) {
    cols <- 1:(imgW - xrange[best]) # axis moved to the left
  } else {
    cols <- (1 + xrange[best - length(xrange)]):imgW # axis moved to the right
  }
  # ... for which the symmetry is computed directly
  as.numeric(combine(lapply(channels, function(x) .sym_mirror(x[, cols, drop = FALSE]))))
}

#' .shift_axis
#'
#' .shift_axis shifts the mirror axis by xrange and returns
#' the symmetries at each axis position by calling the
#' .sym_mirror function.
#'
#' @param img A matrix of numeric values or integer values.
#' @param imgW Image width (numeric).
#' @param xrange Shift range (numeric vector).
#'
#' @return a vector of mirror symmetry values
#' @keywords internal
.shift_axis <- function(img, imgW, xrange) {
  return(
    c(
      # move axis to the left and compute symmetries
      vapply(xrange, function(x) {.sym_mirror(img[, 1:(imgW - x), drop = FALSE])}, numeric(1)),
      # move axis to the right and compute symmetries
      vapply(xrange, function(x) {.sym_mirror(img[, (1 + x):imgW, drop = FALSE])}, numeric(1))
    )
  )
}

#' .shift_axis_fft
#'
#' Fast version of \code{.shift_axis}: returns the symmetries at each axis
#' position (in the same order), but computes them for all axis positions at
#' once instead of correlating the image halves separately for each position.
#' The results equal those of \code{.shift_axis} up to floating-point error
#' (about 1e-10).
#'
#' The correlation between the left image half and the mirrored right image
#' half only requires the sums of the pixel values, of their squares, and of
#' the products of each pixel with its mirrored counterpart. The first two are
#' obtained from cumulative column sums. The sum of the products for all axis
#' positions is the convolution of each image row with itself, which is
#' computed via a fast Fourier transform.
#'
#' @param img A matrix of numeric values or integer values.
#' @param imgW Image width (numeric).
#' @param xrange Shift range (numeric vector).
#'
#' @return a vector of mirror symmetry values
#' @keywords internal
.shift_axis_fft <- function(img, imgW, xrange) {
  imgH <- dim(img)[1] # image height

  # first (a) and last (b) column of the image part that is analyzed for each
  # position of the mirror axis (first: axis moved to the left, then: to the
  # right), width of this part (w), and width of its halves (h)
  a <- c(rep(1, length(xrange)), 1 + xrange)
  b <- c(imgW - xrange, rep(imgW, length(xrange)))
  w <- b - a + 1
  h <- w %/% 2

  # same checks as in .sym_mirror
  if (any(w < 4)) stop("Image too small. Try an image with at least 4 pixels in both image dimensions.", call. = FALSE)
  # (an image half has no variation if all of its columns are constant and
  # have the same value)
  first_row <- img[1, ]
  col_const <- colSums(img != rep(first_row, each = imgH)) == 0
  no_variation <- function(cols) {
    all(col_const[cols]) && min(first_row[cols]) == max(first_row[cols])
  }
  no_var <- vapply(seq_along(a), function(i) {
    no_variation(a[i]:(a[i] + h[i] - 1)) || no_variation((b[i] - h[i] + 1):b[i])
  }, logical(1))
  if (all(no_var)) {
    return(rep(NA_real_, length(a)))
  }

  # centering does not change the correlations but reduces rounding errors
  img <- img - mean(img)

  # cumulative sums of the pixel values and of the squared pixel values
  col_sq <- colSums(img^2)
  cum_sum <- c(0, cumsum(colSums(img)))
  cum_sq <- c(0, cumsum(col_sq))

  # sums of the products of all pairs of columns j and k with j + k = m + 1
  # (i.e., of all columns with their mirrored column for a given position of
  # the mirror axis) via the convolution of each image row with itself;
  # rows are processed in chunks to limit memory usage
  n_fft <- stats::nextn(2 * imgW - 1)
  chunk_size <- max(1, floor(2^20 / n_fft))
  conv_fft <- complex(n_fft)
  for (chunk_start in seq(1, imgH, by = chunk_size)) {
    rows <- chunk_start:min(imgH, chunk_start + chunk_size - 1)
    padded <- matrix(0, nrow = n_fft, ncol = length(rows))
    padded[seq_len(imgW), ] <- t(img[rows, , drop = FALSE])
    padded <- stats::mvfft(padded)
    conv_fft <- conv_fft + rowSums(padded * padded)
  }
  conv <- Re(stats::fft(conv_fft, inverse = TRUE)) / n_fft

  # sums for the left (L) and (mirrored) right (R) image half
  n <- imgH * h
  # each pair of columns is included twice in the convolution; a middle column
  # (odd width) is included once and is not part of the image halves
  mid_col <- ifelse(w %% 2 == 1, col_sq[(a + b) %/% 2], 0)
  sum_LR <- (conv[a + b - 1] - mid_col) / 2
  sum_L <- cum_sum[a + h] - cum_sum[a]
  sum_R <- cum_sum[b + 1] - cum_sum[b - h + 1]
  sum_LL <- cum_sq[a + h] - cum_sq[a]
  sum_RR <- cum_sq[b + 1] - cum_sq[b - h + 1]

  # correlation of image halves (NA without variation in an image half)
  corrLR <- suppressWarnings(
    (sum_LR - sum_L * sum_R / n) /
      sqrt((sum_LL - sum_L^2 / n) * (sum_RR - sum_R^2 / n))
  )
  corrLR[no_var] <- NA

  # fall back to the direct computation if rounding errors prevent a result
  if (!all(is.finite(corrLR[!no_var]))) {
    return(.shift_axis(img, imgW, xrange))
  }

  # symmetries (absolute correlations)
  return(pmin(abs(corrLR), 1))
}



#' .sym_mirror
#'
#' sym_mirror returns the mirror symmetry of a grayscale
#' image matrix. To this end, the left and right image
#' halves are correlated.
#'
#' @param img A matrix of numeric values or integer values.
#'
#' @return a numeric value between 0 and 1 (\code{NA} if there is no variation
#'   in one of the image halves)
#' @keywords internal
.sym_mirror <- function(img) {
  ## -------------------------------
  ##   (vertical) mirror symmetry
  ## -------------------------------

  # image dimensions
  # imgH <- dim(img)[1] # image height
  imgW <- dim(img)[2] # image width

  if (imgW < 4) stop("Image too small. Try an image with at least 4 pixels in both image dimensions.", call. = FALSE)

  # Cut image into 2 equal pieces (row-wise / vertically,
  # that means across the y axis) with flipped right half.
  # Note: floor and ceiling control for images with a width
  # that is not a multiple of 2; if that's the case, the
  # syntax below just eliminates the most middle column;
  # otherwise, all data points are analyzed.
  stimL <- img[, 1:floor(imgW / 2)]
  stimR <- img[, imgW:(1 + ceiling(imgW / 2))]

  # vectorize matrices
  dim(stimL) <- NULL
  dim(stimR) <- NULL

  # check whether sd in one of the halves is zero
  # (computation not possible without variation)
  if (stats::sd(stimL) == 0 || stats::sd(stimR) == 0) {
    return(NA_real_)
  }

  # perfect symmetry (identical image halves): the correlation is exactly 1,
  # which is not guaranteed by the computation below due to rounding errors
  if (identical(stimL, stimR)) {
    return(1)
  }

  # correlation of image halves
  # (use = "pairwise.complete.obs" is considerably faster than the default;
  # the image contains no missing values at this point)
  corrLR <- stats::cor(stimL, stimR, use = "pairwise.complete.obs")

  # final symmetry: vertical (absolute correlation)
  return(abs(corrLR))
}
