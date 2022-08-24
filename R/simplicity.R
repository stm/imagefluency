#' @include utils.R
NULL

#' Image simplicity
#'
#' \code{img_simplicity} returns the simplicity of an image as 1 minus the
#' complexity of the image. Higher values indicated higher image simplicity.
#'
#' @details Image simplicity is calculated as 1 minus the ratio between the
#'   compressed and uncompressed file size (i.e., the compression rate). Values
#'   can range between 0 (no compression possible, thus extremely complex image)
#'   and almost 1 (virtually completely compressed image, thus extremly simple
#'   image). Different compression algorithms are implemented. For details, see
#'   \code{\link{img_complexity}}.
#'
#' @param imgfile Either a character string containing the path to the image
#'   file (or URL) or an an image in form of a matrix (grayscale image) or array
#'   (color image) of numeric values representing the pre-loaded image (e.g. by
#'   using \code{\link{img_read}()}).
#' @param algorithm Character string that specifies which image compression
#'   algorithm to use. Currently implemented are \code{zip} with deflate
#'   compression, \code{jpg}, \code{gif}, and \code{png}.
#' @param rotate logical. Should the compressed file size of the rotated image
#'   also be computed? (see details)
#'
#' @return a numeric value: 1 minus the ratio of compressed divided by
#'   uncompressed file size (i.e., the compression rate)
#' @export
#'
#' @examples
#' # Example image with low simplicity: trees
#' trees <- img_read(system.file("example_images", "trees.jpg", package = "imagefluency"))
#' #
#' # display image
#' grid::grid.raster(trees)
#' #
#' # get complexity
#' img_simplicity(trees)
#'
#' # Example image with high simplicity: sky
#' sky <- img_read(system.file("example_images", "sky.jpg", package = "imagefluency"))
#' #
#' # display image
#' grid::grid.raster(sky)
#' #
#' # get complexity
#' img_simplicity(sky)
#'
#' @references Donderi, D. C. (2006). Visual complexity: A Review.
#'   \emph{Psychological Bulletin}, \emph{132}, 73--97.
#'   \doi{10.1037/0033-2909.132.1.73}
#'
#' @references Forsythe, A., Nadal, M., Sheehy, N., Cela-Conde, C. J., & Sawey,
#'   M. (2011). Predicting Beauty: Fractal Dimension and Visual Complexity in
#'   Art. \emph{British Journal of Psychology}, \emph{102}, 49--70.
#'   \doi{10.1348/000712610X498958}
#'
#' @references Mayer, S. & Landwehr, J, R. (2018). Quantifying Visual Aesthetics
#'   Based on Processing Fluency Theory: Four Algorithmic Measures for
#'   Antecedents of Aesthetic Preferences. \emph{Psychology of Aesthetics,
#'   Creativity, and the Arts}, \emph{12}(4), 399--431.
#'   \doi{10.1037/aca0000187}
#'
#'
#' @seealso \code{\link{img_read}}, \code{\link{img_complexity}},
#'   \code{\link{img_contrast}}, \code{\link{img_self_similarity}},
#'   \code{\link{img_symmetry}}, \code{\link{img_typicality}},
img_simplicity <- function(imgfile, algorithm = "zip", rotate = FALSE){
  1 - img_complexity(imgfile, algorithm, rotate)
}
