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
#'   For color images, the default is that first a maximal symmetry score (as
#'   explained above) is obtained per color channel (parameter \code{per_channel
#'   = TRUE}). Subsequently, a weighted average between each color channel's
#'   maximal score is computed as the image's overall symmetry. Advanced users
#'   can reverse this order by setting \code{per_channel = FALSE}. This results
#'   in first computing the weighted averages for each position of the mirror
#'   axis separately, and afterwards finding the maximal overall symmetry score.
#'
#'
#' @param img An image in form of a matrix or array of numeric values. Use e.g.
#'   \code{\link{img_read}()} to read an image file into \code{R}.
#' @param vertical logical. Should the vertical symmetry be computed? (default:
#'   TRUE)
#' @param horizontal logical. Should the horizontal symmetry be computed?
#'   (default: TRUE)
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
#' @seealso \code{\link{img_read}}, \code{\link{img_complexity}},
#'   \code{\link{img_contrast}}, \code{\link{img_self_similarity}}
#'   \code{\link{img_simplicity}}, \code{\link{img_typicality}}
#'
#' @references Mayer, S. & Landwehr, J, R. (2018). Quantifying Visual Aesthetics
#'   Based on Processing Fluency Theory: Four Algorithmic Measures for
#'   Antecedents of Aesthetic Preferences. \emph{Psychology of Aesthetics,
#'   Creativity, and the Arts}, \emph{12}(4), 399--431.
#'   \doi{10.1037/aca0000187}
img_symmetry <- function(img, vertical = TRUE, horizontal = TRUE, ...) {

  # check options
  if (vertical == FALSE & horizontal == FALSE) {
    warning("Both optional arguments cannot be FALSE. Try setting option 'vertical' or 'horizontal' to TRUE. Returning NA.", call. = FALSE)
    return(NA)
  }

  # check input
  imgtype <- .check_input(img, f_call = "symmetry")

  # Note: the default is vertical symmetry.
  # If horizontal symmetry should also be computed, the image is simply rotated
  # by 90 degrees and then again "horizontal" symmetry is computed.

  # compute and return symmetry
  if (imgtype == "rgb") {
    # colored image
    #
    # only vertical symmetry
    if (!horizontal) {
      return(c(vertical = .sym(img, color=TRUE, ...)))
    }
    # only horizontal symmetry
    if (!vertical) {
      return(c(horizontal = .sym(rotate90(img), color=TRUE, ...)))
    }
    # both horizontal and vertical symmetry
    if (vertical & horizontal) {
      return(c(vertical = c(.sym(img, color=TRUE, ...)),
               horizontal = .sym(rotate90(img), color=TRUE, ...)))
    }
  } else {
    # grayscale image
    #
    # only vertical symmetry
    if (!horizontal) {
      return(c(vertical = .sym(img, color=FALSE, ...)))
    }
    # only horizontal symmetry
    if (!vertical) {
      return(c(horizontal = .sym(rotate90(img), color=FALSE, ...)))
    }
    # both horizontal and vertical symmetry
    if (vertical & horizontal) {
      return(c(vertical = c(.sym(img, color=FALSE, ...)),
               horizontal = .sym(rotate90(img), color=FALSE, ...)))
    }
  }
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
#'
#' @return one numeric value as the symmetry
#' @keywords internal
.sym <- function(img, color, per_channel = TRUE, shift_range = 0.05) {

  # image dimensions
  # imgH <- dim(img)[1] # image height
  imgW <- dim(img)[2] # image width

  # symmetry will be computed around the respective centerline
  #
  # symmetry shift range (default: 5%)
  xrange <- 0:floor(imgW * shift_range)

  if (color == FALSE) {
    # grayscale image
    #
    # find symmetries
    return(max(.shift_axis(img, imgW, xrange)))
  } else {
    # colored image
    redChannel <- img[, , 1]
    greenChannel <- img[, , 2]
    blueChannel <- img[, , 3]

    #
    if (per_channel) {
      return(#
        0.2989 * max(.shift_axis(redChannel, imgW, xrange)) +
          0.5870 * max(.shift_axis(greenChannel, imgW, xrange)) +
          0.1140 * max(.shift_axis(blueChannel, imgW, xrange))
      )
    } else {
      return(#
        max(#
          0.2989 * .shift_axis(redChannel, imgW, xrange) +
            0.5870 * .shift_axis(greenChannel, imgW, xrange) +
            0.1140 * .shift_axis(blueChannel, imgW, xrange)
        )
      )
    }
  }
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
      vapply(xrange, function(x) {.sym_mirror(img[, 1:(imgW - x)])}, numeric(1)),
      # move axis to the right and compute symmetries
      vapply(xrange, function(x) {.sym_mirror(img[, (1 + x):imgW])}, numeric(1))
    )
  )
}



#' .sym_mirror
#'
#' sym_mirror returns the mirror symmetry of a grayscale
#' image matrix. To this end, the left and right image
#' halves are correlated.
#'
#' @param img A matrix of numeric values or integer values.
#'
#' @return a numeric value between 0 and 1
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
  # Note: floor and ceiling control for images with a height
  # that is not a multiple of 2; if that's the case, the
  # syntax below just eliminates the most middle row;
  # otherwise, all data points are analyzed.
  stimL <- img[, 1:floor(imgW / 2)]
  stimR <- img[, imgW:(1 + ceiling(imgW / 2))]

  # vectorize matrices
  pixL <- as.vector(stimL)
  pixR <- as.vector(stimR)

  # check whether sd in one of the halves is zero
  if (stats::sd(pixL) == 0) {
    stop("No variation in left image half. Computation not possible.", call. = FALSE)
    # return(NA)
  }
  if (stats::sd(pixR) == 0) {
    stop("No variation in right image half. Computation not possible.", call. = FALSE)
    # return(NA)
  }

  # correlation of image halves
  corrLR <- stats::cor(pixL, pixR)

  # final symmetry: vertical (absolute correlation)
  return(abs(corrLR))
}
