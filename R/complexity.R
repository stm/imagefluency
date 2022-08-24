#' @include utils.R
NULL

# 2do: idea / test: image compression via PCA (for images that are not in an uncompressed format)?
# see http://www.aaronschlegel.com/image-compression-principal-component-analysis/
#
# alternative: entropy as measure

#' Image complexity
#'
#' \code{img_complexity} returns the complexity of an image via image
#' compression. Higher values indicate higher image complexity.
#'
#' @details The function returns the visual complexity of an image. Visual
#'   complexity is calculated as ratio between the compressed and uncompressed
#'   image file size. Preferably, the original image is an uncompressed image
#'   file.
#'
#'   The function takes the file path of an image file (or URL) or a pre-loaded
#'   image as input argument (\code{imgfile}) and returns the ratio of the
#'   compressed divided by the uncompressed image file size. Values can range
#'   between almost 0 (virtually completely compressed image, thus extremely
#'   simple image) and 1 (no compression possible, thus extremely complex
#'   image).
#'
#'   You can choose between different image compression algorithms. Currently
#'   implemented are \code{zip} with deflate compression (default), \code{jpg},
#'   \code{gif}, and \code{png}. See Mayer & Landwehr (2018) for a discussion of
#'   different image compression algorithms for measuring visual complexity.
#'
#'   As most compression algorithms do not depict horizontal and vertical
#'   redundancies equally, the function includes an optional \code{rotate}
#'   parameter (default: \code{FALSE}). Setting this parameter to \code{TRUE}
#'   has the following effects: first, the image is rotated by 90 degrees.
#'   Second, a compressed version of the rotated image is created. Finally,
#'   the overall compressed image's file size is computed as the minimum of
#'   the original image's file size and the file size of the rotated image.
#'
#'   As \code{R}'s built-in \code{bmp} device creates (a) indexed instead of
#'   True Color images and (b) creates files with different file sizes depending
#'   on the operating system, the function relies on the \code{magick} package
#'   to write (and read) images.
#'
#' @param imgfile Either a character string containing the path to the image
#'   file (or URL) or an an image in form of a matrix (grayscale image) or array
#'   (color image) of numeric values representing the pre-loaded image (e.g. by
#'   using \code{\link{img_read}()}).
#' @param algorithm Character string that specifies which image compression
#'   algorithm to use. Currently implemented are \code{zip} with deflate
#'   compression (default), \code{jpg}, \code{gif}, and \code{png}.
#' @param rotate logical. Should the compressed file size of the rotated image
#'   also be computed? (see details)
#'
#' @return a numeric value: the ratio of the compressed divided by the
#'   uncompressed image file size
#' @export
#'
#' @examples
#' # Example image with high complexity: trees
#' trees <- img_read(system.file("example_images", "trees.jpg", package = "imagefluency"))
#' #
#' # display image
#' grid::grid.raster(trees)
#' #
#' # get complexity
#' img_complexity(trees)
#'
#'
#' # Example image with low complexity: sky
#' sky <- img_read(system.file("example_images", "sky.jpg", package = "imagefluency"))
#' #
#' # display image
#' grid::grid.raster(sky)
#' #
#' # get complexity
#' img_complexity(sky)
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
#' @seealso \code{\link{img_read}}, \code{\link{img_contrast}},
#'   \code{\link{img_self_similarity}}, \code{\link{img_simplicity}},
#'   \code{\link{img_symmetry}}, \code{\link{img_typicality}},
img_complexity <- function(imgfile, algorithm = "zip", rotate = FALSE){

  if (!(algorithm %in% c("zip", "jpg", "gif", "png"))) {
    stop("Unknown image compression algorithm requested. Use one of the following: zip, jpg, gif, png", call. = FALSE)
  }

  if (is.character(imgfile) & !is.array(imgfile)) {
    if (length(imgfile) == 1) {
      input <- "image_path"
    } else {
      stop("Multiple filenames. Function can only handle one image at a time.", call. = FALSE)
    }
  } else if (is.array(imgfile) & (is.numeric(imgfile) | is.integer(imgfile))) {
    input <- "image"
    # normalize image if necessary
    if (max(imgfile) > 1) {
      imgfile <- imgfile / 255
    }
  } else {
    stop("Wrong type of input: has to be a filename (character string) or an image (3-dimensional array of numeric or integer values)", call. = FALSE)
  }

  if (!requireNamespace("magick", quietly = TRUE)) stop("Package 'magick' not found but needed. Please install the package first.", call. = FALSE)

  img <- tryCatch(magick::image_read(imgfile), error = function(err) err)
  if (inherits(img, "error")) {
    if (input == "image_path") {
      stop(paste0("File not found or invalid path (could not resolve '", imgfile ,"')"), call. = FALSE)
    } else {
      stop(paste0("File not found or invalid path (could not resolve input)"), call. = FALSE)
    }
  } else {
    .compl(img, algorithm, rotate)
  }
}


#' .compl
#'
#' Returns the complexity of an image array / matrix or path.
#'
#' @param imgfile An array or matrix of numeric values or integer values, or the file path to the image.
#' @param algorithm character. Which compression algorithm to use.
#' @param rotate logical. Image rotation by 90 degrees?
#'
#' @return a numeric value (ratio compressed/uncompressed file size).
#' @keywords internal
.compl <- function(img, algorithm, rotate) {
  flname <- file.path(tempdir(), .rand_string())

  # imginfo <- magick::image_info(img)

  # isBMP <- grepl("^BMP", as.character(imginfo[1]))
  # if (!isBMP) warning("Input image might not be uncompressed. Interpret results with caution.", call. = FALSE)

  # write bmp image
  magick::image_write(img, path = paste0(flname, ".bmp"), format = "bmp")
  orig_size <- file.size(paste0(flname, ".bmp"))

  # compress file
  if (algorithm == "zip") {
    if (!requireNamespace("R.utils", quietly = TRUE)) stop("Package 'R.utils' not found but needed. Please install the package first.", call. = FALSE)
    R.utils::gzip(filename = paste0(flname, ".bmp"))
  } else if (algorithm == "jpg") {
    magick::image_write(img, path = paste0(flname, ".jpg"), format = "jpg")
  } else if (algorithm == "gif") {
    magick::image_write(img, path = paste0(flname, ".gif"), format = "gif")
  } else if (algorithm == "png") {
    magick::image_write(img, path = paste0(flname, ".png"), format = "png")
  }

  # read file sizes
  if (algorithm == "zip") compressed_size <- file.size(paste0(flname,".bmp.gz"))
  if (algorithm == "jpg") compressed_size <- file.size(paste0(flname,".jpg"))
  if (algorithm == "gif") compressed_size <- file.size(paste0(flname,".gif"))
  if (algorithm == "png") compressed_size <- file.size(paste0(flname,".png"))

  # remove newly created files
  # Sys.sleep(0.25) # wait a moment
  if (algorithm == "zip") file.remove(paste0(flname, ".bmp.gz"))
  if (algorithm == "jpg") file.remove(paste0(flname, ".jpg"))
  if (algorithm == "gif") file.remove(paste0(flname, ".gif"))
  if (algorithm == "png") file.remove(paste0(flname, ".png"))
  # remove original file (if not zip, where this is done automatically)
  if (algorithm != "zip")   file.remove(paste0(flname, ".bmp"))

  if (!is.logical(rotate)) {
    warning(paste0("rotate = '", rotate, "' is not a logical value (TRUE/FALSE). Skipping rotation ..."), call. = FALSE)
    rotate <- FALSE
  }

  if (rotate) {
    flname_rot <- paste0(flname, "_rot")

    # message("... trying to rotate the image ...")
    # write rotated image
    magick::image_write(magick::image_rotate(img, degrees = 90), path = paste0(flname_rot, ".bmp"), format = "bmp")

    # compress rotated image
    if (algorithm == "zip") {
      R.utils::gzip(filename = paste0(flname_rot, ".bmp"))
    } else if (algorithm == "jpg") {
      magick::image_write(magick::image_rotate(img, degrees = 90), path = paste0(flname_rot, ".jpg"), format = "jpg")
    } else if (algorithm == "gif") {
      magick::image_write(magick::image_rotate(img, degrees = 90), path = paste0(flname_rot, ".gif"), format = "gif")
    } else if (algorithm == "png") {
      magick::image_write(magick::image_rotate(img, degrees = 90), path = paste0(flname_rot, ".png"), format = "png")
    }

    # read file size
    if (algorithm == "zip") compressed_size_rot <- file.size(paste0(flname_rot,".bmp.gz"))
    if (algorithm == "jpg") compressed_size_rot <- file.size(paste0(flname_rot,".jpg"))
    if (algorithm == "gif") compressed_size_rot <- file.size(paste0(flname_rot,".gif"))
    if (algorithm == "png") compressed_size_rot <- file.size(paste0(flname_rot,".png"))

    # remove rotated files
    # Sys.sleep(0.25) # wait a moment
    if (algorithm == "zip") file.remove(paste0(flname_rot,".bmp.gz"))
    if (algorithm == "jpg") file.remove(paste0(flname_rot,".jpg"))
    if (algorithm == "gif") file.remove(paste0(flname_rot,".gif"))
    if (algorithm == "png") file.remove(paste0(flname_rot,".png"))
    # remove original file (if not zip, where this is done automatically)
    if (algorithm != "zip") file.remove(paste0(flname_rot, ".bmp"))

    # update result: minimum of both compressed file sizes of original and rotated img
    compressed_size <- min(compressed_size, compressed_size_rot)
  }

  # remove initial tempfile


  # return ratio between compressed and original (bmp) size (i.e., compression rate)
  return(compressed_size/orig_size)
}
