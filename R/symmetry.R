# 2do: what if image width / height cannot be divided by 2?
# 2do: what if sd is zero (no corr possible because no variation due to the fact that there is no variation in one half of the image?)


#' quantify_symmetry
#'
#' quantify_symmetry returns the vertical and horizontal
#' symmetry of an image. Values can range between 0 (not
#' symmmetrical) and 1 (fully symmetrical).
#'
#' @param img A matrix of numeric values or integer values.
#'   Color images have to be converted to grayscale in
#'   advance or each color channel has to be analyzed
#'   seperately.
#'
#' @return a list of numeric values
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
quantify_symmetry <- function(img){
  sym_v <- sym_ver(img)
  sym_h <- sym_hor(img)
  results <- list(vertical = sym_v, horizontal = sym_h)
    return(results)
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
sym_ver <- function(img){
  ## -----------------------
  ##   vertical symmetry
  ## -----------------------

  # image dimensions
  imgH <- dim(img)[1] # image height
  imgW <- dim(img)[2] # image width

  # cut image into 2 equal pieces (row-wise / vertically, that means across the y axis)
  stimL <- img[ , 1:(imgW/2)]
  stimR <- img[ , (1 + imgW/2):imgW ]

  # flip right image half
  stimRfl <- OpenImageR::flipImage(stimR, mode = "horizontal")

  # vectorize matrices
  pixL <- as.vector(stimL)
  pixR <- as.vector(stimRfl)

  # correlation of image halves
  corrLR <- cor(pixL,pixR)

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
sym_hor <- function(img){
  ## -----------------------
  ##   horizontal symmetry
  ## -----------------------

  # image dimensions
  imgH <- dim(img)[1] # image height
  imgW <- dim(img)[2] # image width

  # cut image into 2 equal pieces (column-wise / horizontally, that means across the x axis)
  stimU <- img[ 1:(imgH/2), ]
  stimD <- img[ (1 + imgH/2):imgH, ]

  # flip lower image half
  stimDfl <- OpenImageR::flipImage(stimD, mode = "vertical")

  # vectorize matrices
  pixU <- as.vector(stimU)
  pixD <- as.vector(stimDfl)

  # correlation of image halves
  corrUD <- cor(pixU,pixD)

  # final symmetry: horizontal (absolute correlation)
  return(abs(corrUD))
}

