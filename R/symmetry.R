#' @include utils.R
NULL

#' Symmetry of an Image
#'
#' \code{quantify_symmetry} returns the vertical and
#' horizontal symmetry of an image matrix \code{img}.
#'
#' @details The function returns the vertical and horizontal
#'   symmetry of an image matrix \code{img}. Values can
#'   range between 0 (not symmmetrical) and 1 (fully
#'   symmetrical). If \code{vertical} or \code{horizontal}
#'   is set to \code{FALSE} then vertical or horizontal
#'   symmetry is not computed, respectively.
#'
#' @param img A matrix of numeric values or integer values.
#'   Color images have to be converted to grayscale in
#'   advance (function \code{rgb2gray}) or each color
#'   channel has to be analyzed seperately.
#' @param vertical logical. Should the vertical symmetry be
#'   computed?
#' @param horizontal logical. Should the horizontal symmetry
#'   be computed?
#'
#' @return a named vector of numeric values (vertical and horizontal
#'   symmetry)
#' @export
#'
#' @examples
#' # construct sample image
#' img <- matrix(0, nrow=100, ncol=100)
#' img[21:60,31:70] <- 0.5
#' img[71:90, 21:80] <- 1
#'
#' # if you want to inspect the image
#' # OpenImageR::imageShow(img)
#'
#' # get both vertical and horizontal symmetry
#' quantify_symmetry(img)
#'
#' @seealso \code{\link{rgb2gray}},
#'   \code{\link{quantify_complexity}},
#'   \code{\link{quantify_contrast}},
#'   \code{\link{quantify_typicality}},
#'   \code{\link{quantify_self_similarity}}
#'
#' @references Mayer, S. & Landwehr, J. R. (2014). When
#'   Complexity is Symmetric: The Interplay of Two Core
#'   Determinants of Visual Aesthetics. \emph{Advances in
#'   Consumer Research}, \emph{42}, 608--609.
quantify_symmetry <- function(img, vertical = TRUE, horizontal = TRUE) {

  # check input
  .check_input(img, f_call = "symmetry")

  # compute symmetry
  if (vertical) sym_v <- sym_ver(img)
  if (horizontal) sym_h <- sym_hor(img)

  # return symmetry values
  if (vertical == FALSE & horizontal == FALSE) {
    warning("Both optional arguments cannot be FALSE. Try setting option 'vertical' or 'horizontal' to TRUE. Returning NA.", call. = FALSE)
    return(NA)
  } else {
    if (!vertical) return(c(horizontal = sym_h))
    if (!horizontal) return(c(vertical = sym_v))
    return(c(vertical = sym_v, horizontal = sym_h))
  }
}


#' sym_ver
#'
#' sym_ver returns the vertical symmetry of a grayscale
#' image matrix. To this end, the left and right image
#' halves are correlated.
#'
#' @param img A matrix of numeric values or integer values.
#'
#' @return a numeric value between 0 and 1
#' @importFrom stats sd cor
#' @keywords internal
#' @export
sym_ver <- function(img) {
  ## -----------------------
  ##   vertical symmetry
  ## -----------------------

  # image dimensions
  imgH <- dim(img)[1] # image height
  imgW <- dim(img)[2] # image width

  if (imgW < 4) stop("Image too small. Try an image with at least 4 pixels in width.", call. = FALSE)

  # Cut image into 2 equal pieces (row-wise / vertically,
  # that means across the y axis) with flipped right half.
  # Note: floor and ceiling control for images with a height
  # that is not a multiple of 2; if that's the cas, the
  # syntax below just eliminates the most middle row;
  # otherwise, all data points are analyzed.
  stimL <- img[, 1:floor(imgW / 2)]
  stimR <- img[, imgW:(1 + ceiling(imgW / 2))]

  # vectorize matrices
  pixL <- as.vector(stimL)
  pixR <- as.vector(stimR)

  # check whether sd in one of the halves is zero
  if (sd(pixL) == 0) stop("No variation in left image half. Computation not possible.", call. = FALSE)
  if (sd(pixR) == 0) stop("No variation in right image half. Computation not possible.", call. = FALSE)

  # correlation of image halves
  corrLR <- cor(pixL, pixR)

  # final symmetry: vertical (absolute correlation)
  return(abs(corrLR))
}

#' sym_hor
#'
#' sym_hor returns the horizontal symmetry of a grayscale
#' image matrix. To this end, the upper and lower image
#' halves are correlated.
#'
#' @param img A matrix of numeric values or integer values.
#'
#' @return a numeric value between 0 and 1
#' @importFrom stats sd cor
#' @keywords internal
#' @export
sym_hor <- function(img) {
  ## -----------------------
  ##   horizontal symmetry
  ## -----------------------

  # image dimensions
  imgH <- dim(img)[1] # image height
  imgW <- dim(img)[2] # image width

  if (imgH < 4) stop("Image too small. Try an image with at least 4 pixels in height.", call. = FALSE)

  # Cut image into 2 equal pieces (column-wise /
  # horizontally, that means across the x axis) with flipped
  # lower half.
  # Note: floor and ceiling control for images with a height
  # that is not a multiple of 2; if that's the cas, the
  # syntax below just eliminates the most middle row;
  # otherwise, all data points are analyzed.
  stimU <- img[1:floor(imgH / 2),]
  stimD <- img[imgH:(1 + ceiling(imgH / 2)),]

  # vectorize matrices
  pixU <- as.vector(stimU)
  pixD <- as.vector(stimD)

  # check whether sd in one of the halves is zero
  if (sd(pixU) == 0) stop("No variation in upper image half. Computation not possible.", call. = FALSE)
  if (sd(pixD) == 0) stop("No variation in lower image half. Computation not possible.", call. = FALSE)

  # correlation of image halves
  corrUD <- cor(pixU, pixD)

  # final symmetry: horizontal (absolute correlation)
  return(abs(corrUD))
}
