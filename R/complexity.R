#' @include utils.R
NULL

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
#' @examplesIf .Platform$OS.type != "windows"
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
      img_from_path <- tryCatch(img_read(imgfile), error = function(err) err)
      if (!inherits(img_from_path, "error")) {
        imgfile <- img_from_path
      }
    } else {
      stop("Multiple filenames. Function can only handle one image at a time.", call. = FALSE)
    }
  }

  if (is.array(imgfile) & (is.numeric(imgfile) | is.integer(imgfile))) {
    # normalize image if necessary
    if (min(imgfile) < 0 || max(imgfile) > 1) {
      imgfile <- .normalize_img(imgfile)
    }
  } else if (!is.character(imgfile) || is.array(imgfile)) {
    stop("Wrong type of input: has to be a filename (character string) or an image (3-dimensional array of numeric or integer values)", call. = FALSE)
  }

  if (!.pkg_avail("magick")) stop("Package 'magick' not found but needed. Please install the package first.", call. = FALSE)

  img <- tryCatch(magick::image_read(imgfile), error = function(err) err)
  if (inherits(img, "error")) {
    stop("File not found or invalid path", call. = FALSE)
  }

  .compl(img, algorithm, rotate)
}


#' .compl
#'
#' Returns the complexity of an image array / matrix or path.
#'
#' @param img An array or matrix of numeric values or integer values, or the file path to the image.
#' @param algorithm character. Which compression algorithm to use.
#' @param rotate logical. Image rotation by 90 degrees?
#'
#' @return a numeric value (ratio compressed/uncompressed file size)
#' @keywords internal
.compl <- function(img, algorithm, rotate) {
  flname <- tempfile()

  # write bmp image
  bmp_path <- paste0(flname, ".bmp")
  magick::image_write(img, path = bmp_path, format = "bmp")
  # remove original file on exit
  # (for zip this is done automatically, which is why we suppress warnings here)
  on.exit(suppressWarnings(file.remove(bmp_path)))
  # get original file size
  orig_size <- file.size(bmp_path)

  # compress file
  compressed_size <- .compress_and_get_size(img, flname, algorithm, use_existing_bmp = TRUE)

  if (!is.logical(rotate)) {
    warning(paste0("rotate = '", rotate, "' is not a logical value (TRUE/FALSE). Skipping rotation ..."), call. = FALSE)
    rotate <- FALSE
  }

  if (rotate) {
    # rotate image, compress, and get size
    img_rot <- magick::image_rotate(img, degrees = 90)
    compressed_size_rot <- .compress_and_get_size(img_rot, flname, algorithm, use_existing_bmp = FALSE)
    # update result: minimum of both compressed file sizes of original and rotated img
    compressed_size <- min(compressed_size, compressed_size_rot)
  }

  # return ratio between compressed and original (bmp) size (i.e., compression rate)
  return(compressed_size/orig_size)
}


#' .compress_and_get_size
#'
#' Compresses an image and returns the compressed file size.
#'
#' @param img An array or matrix of numeric values or integer values.
#' @param flname Character string (file name without extension).
#' @param algorithm Character string (which compression algorithm to use).
#' @param use_existing_bmp Logical (use existing bmp file).
#'
#' @returns a numeric value (compressed file size)
#' @keywords internal
.compress_and_get_size <- function(img, flname, algorithm, use_existing_bmp) {
  # zip algorithm
  if (algorithm == "zip") {
    bmp_path <- paste0(flname, ".bmp")
    if (!use_existing_bmp) {
      magick::image_write(img, path = bmp_path, format = "bmp")
    }
    if (!.pkg_avail("R.utils")) stop("Package 'R.utils' not found but needed. Please install the package first.", call. = FALSE)
    R.utils::gzip(filename = bmp_path)
    on.exit(file.remove(paste0(bmp_path, ".gz")), add = TRUE)
    return(file.size(paste0(bmp_path, ".gz")))
  }

  # all other algorithms
  format_path <- paste0(flname, ".", algorithm)
  magick::image_write(img, path = format_path, format = algorithm)
  on.exit(file.remove(format_path), add = TRUE)
  return(file.size(format_path))
}
