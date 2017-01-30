# 2do: idea / test: image compression via PCA (for images that are not in an uncompressed format)?
# see http://www.aaronschlegel.com/image-compression-principal-component-analysis/

#' @include utils.R
NULL

# In the current version, the messages that are displayed by the zip
# command are printed when calling the function (because zip calls system2, and
# system2 uses stdout = "" and stderr = "" by default). Maybe something along
# the lines of patching system2 would do the trick (see [1]), but for now, the
# user just has to live with it.
# [1] https://stackoverflow.com/questions/12416076/suppress-install-outputs-in-r/12504606


#' Complexity of an image
#'
#' \code{quantify_complexity} returns the complexity of an
#' image as the size of the zip-compressed image file. In
#' the current version, only \code{.bmp} images are
#' supported.
#'
#' @details The function returns the visual complexity of an
#'   image. Visual complexity is calculated as the size of
#'   the zip-compressed image file. Thus, it is important
#'   that the original image is in an uncompressed format.
#'
#'   The function takes the file path \code{flname} of an
#'   uncompressed image file as input argument and returns
#'   the filesize of the zip-compressed image file.
#'   Additionally, the uncompressed filesize and the
#'   compression rate are returned.
#'
#'   The zip compression algorithm works line by line and
#'   hence does not depict horizontal and vertical
#'   redundancies equally. Thus, the function includes an
#'   optional \code{rotate} parameter (default:
#'   \code{FALSE}). Setting this parameter to \code{TRUE}
#'   has the following effects: first, the image is loaded
#'   via the \code{readbitmap} package (if \code{img} is
#'   \code{NULL}, otherwise see below). Second, the image is
#'   rotated by 90 degrees. Third, the rotated image is
#'   written in the current working directory and a
#'   zip-compressed version is created. Finally, the
#'   complexity of the image is copmuted as the minimum of
#'   the original image's filesize and the filesize of the
#'   rotated image. All files that are created while the
#'   function runs are automatically deleted once the
#'   complexity score is calculated.
#'
#'   If the parameter \code{img} is not \code{NULL} but
#'   contains an array or matrix of numeric or integer
#'   values, this \code{img} is used for the rotation
#'   instead of the original image.
#'
#'   In the current version, \strong{only \code{.bmp}
#'   images} are supported.
#'
#' @param flname A character string containing the file path
#'   of the image. If the image file is in the same
#'   directory as the current working directory, it is
#'   sufficient to pass just the filename as input to the
#'   function.
#' @param rotate logical. Should the compressed filesize of
#'   the rotated image also be computed? (see details)
#' @param img An array or matrix of numeric or integer
#'   values representing an image.
#'
#' @return a list of numeric values: compressed filesize,
#'   original filesize, ratio of compressed / original
#'   filesize (i.e., the compression rate)
#' @export
#'
#' @examples
#' # Download Lena sample image (see http://eeweb.poly.edu/~yao/EL5123/SampleData.html)
#' download.file("http://eeweb.poly.edu/~yao/EL5123/image/lena_gray.bmp", "Lena.bmp")
#'
#' # get complexity
#' quantify_complexity("Lena.bmp")
#'
#' # get complexity (including rotated version)
#' quantify_complexity("Lena.bmp", rotate = TRUE)
#'
#' # # get complexity (including rotated version)
#' # # but specify image that to be rotated directly
#' # img_lena <- readbitmap::read.bitmap("Lena.bmp")
#' # quantify_complexity("Lena.bmp", rotate = TRUE, img = img_lena)
#'
#' # compute simplicity score
#' lena_results <- quantify_complexity("Lena.bmp", rotate = TRUE)
#' lena_results$original / lena_results$compressed # simplicity ratio
#'
#' @references Donderi, D. C. (2006). Visual complexity: A
#'   Review. \emph{Psychological Bulletin}, \emph{132},
#'   73--97.
#'   doi:\href{https://doi.org/10.1037/0033-2909.132.1.73}{10.1037/0033-2909.132.1.73}
#' @references Forsythe, A., Nadal, M., Sheehy, N.,
#'   Cela-Conde, C. J., & Sawey, M. (2011). Predicting
#'   Beauty: Fractal Dimension and Visual Complexity in Art.
#'   \emph{British Journal of Psychology}, \emph{102},
#'   49--70.
#'   doi:\href{https://doi.org/10.1348/000712610X498958}{10.1348/000712610X498958}
#'
#' @importFrom grDevices bmp dev.off
#' @importFrom graphics plot
#' @importFrom utils zip
quantify_complexity <- function(flname, rotate = FALSE, img = NULL){

  if (!is.character(flname)) {
    stop("Wrong type of input ('flname' has to be a character string)")
  }

  if (length(flname) > 1) {
    stop("Multiple filenames. Function can only handle one image at a time.")
  }

  try_err_file = inherits(tryCatch(normalizePath(flname, mustWork = TRUE), error = function(err) err), "error")
  if (!try_err_file) {
    flname <- normalizePath(flname)
  } else {
    stop(paste0("Invalid path or filename (could not resolve '", flname ,"')"))
  }

  # original file size
  orig_size <- file.size(flname)
  if (is.na(orig_size)) stop("File not found. Did you forget to specify the filename extension?")

  # original file name without file extension (.bmp)
  file_wo_ext <- gsub("(.*)\\.bmp$", "\\1", flname)
  if (file_wo_ext == flname) stop("Unsupported filename extension. Currently only the following image types are supported: .bmp")

  # zip-compress file and read file size of compressed file
  suppressWarnings(zip(zipfile = file_wo_ext, files = flname))
  compressed_size <- file.size(paste0(file_wo_ext,".zip"))
  # remove newly created .zip file
  file.remove(paste0(file_wo_ext,".zip"))

  if (rotate) {

    message("... trying to rotate the image ...")

    # no img as parameter given, try to load original image
    if (is.null(img)) {
      # flnme is character (checked above)
      if (requireNamespace("readbitmap", quietly = TRUE)) {
        img <- readbitmap::read.bitmap(flname)
      } else {
        stop("Package 'readbitmap' not found but needed to load the .bmp image for rotation")
      }

    } else {
      # check input
      if (!(is.matrix(img) || is.array(img))) {
        stop("Input 'img' has to be an *array* or a *matrix* of numeric or integer values", call. = FALSE)
      }
      if (!(is.numeric(img) | is.integer(img))) {
        stop("Input 'img' has to be an array or a matrix of *numeric* or *integer* values", call. = FALSE)
      }
    }

    # image dimensions
    img_h <- dim(img)[1] # image height
    img_w <- dim(img)[2] # image width

    # rotate image
    if (requireNamespace("OpenImageR", quietly = TRUE)) {
      img_rot <- OpenImageR::rotateFixed(img,90)
    } else {
      img_rot <- rotate90(img)
    }
    # write as uncompressed image file
    bmp(filename = paste0(file_wo_ext, "_rot.bmp"), width = img_w, height = img_h) # height and width switched because of rotation
    if (requireNamespace("OpenImageR", quietly = TRUE)) {
      OpenImageR::imageShow(img_rot)
    } else if (requireNamespace("grid", quietly = TRUE)) {
      if (max(img_rot) > 1) {
        img_rot <- img_rot / 255
      }
      grid::grid.raster(img_rot)
    } else {
      stop("Package 'OpenImageR' or 'grid' needed to render the rotated image.")
    }
    dev.off()

    # zip-compress rotated image and read file size
    zip(zipfile = paste0(file_wo_ext,"_rot"), files = paste0(file_wo_ext, "_rot.bmp"))
    compressed_size_rot <- file.size(paste0(file_wo_ext,"_rot.zip"))
    # remove rotated files
    file.remove(paste0(file_wo_ext,"_rot.zip"))
    file.remove(paste0(file_wo_ext, "_rot.bmp"))

    # update result: minimum of both compressed file sizes of original and rotated img
    compressed_size <- min(compressed_size, compressed_size_rot)
  }

  # return results
  return(list(compressed = compressed_size, original = orig_size, ratio = compressed_size/orig_size))
}

