# 2do: idea / test: image compression via PCA (for images that are not in an uncompressed format)?
# see http://www.aaronschlegel.com/image-compression-principal-component-analysis/
#
# alternative: entropy as measure

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
#' image as the size of the zip-compressed image file.
#'
#' @details The function returns the visual complexity of an
#'   image. Visual complexity is calculated as the size of
#'   the zip-compressed image file. Preferably, the input is
#'   an uncompressed image file. If the input image is not
#'   an uncompressed \code{.bmp} file, the function tries to
#'   create an uncompressed version of the image.
#'
#'   The function takes the file path of an image file (or URL) or a
#'   pre-loaded image as input argument (\code{imgfile}) and
#'   returns the filesize of the zip-compressed image file.
#'   Additionally, the uncompressed filesize and the
#'   compression rate are returned. Note that permission to
#'   read and write files in the specified directory path /
#'   working directory is needed for this function to work
#'   properly.
#'
#'   The zip compression algorithm works line by line and
#'   hence does not depict horizontal and vertical
#'   redundancies equally (see Example). Thus, the function
#'   includes an optional \code{rotate} parameter (default:
#'   \code{FALSE}). Setting this parameter to \code{TRUE}
#'   has the following effects: first, the image is rotated
#'   by 90 degrees. Second, the rotated image is written in
#'   the working directory and a zip-compressed version is
#'   created. Finally, the complexity of the image is
#'   computed as the minimum of the original image's
#'   filesize and the filesize of the rotated image. All
#'   files that are created while the function runs are
#'   automatically deleted once the complexity score is
#'   calculated.
#'
#'   As \code{R}'s built-in \code{bmp} device creates (a)
#'   indexed instead of True Color images and (b) creates
#'   files with different file sizes depending on the
#'   operating system, the function relies on the
#'   \code{magick} package to write (and read) images.
#'
#' @param imgfile Either a character string containing the
#'   file name of the image (or URL) or an 3-dimensional
#'   array of numeric values representing the pre-loaded
#'   image.
#' @param rotate logical. Should the compressed filesize of
#'   the rotated image also be computed? (see details)
#'
#' @return a list of numeric values: compressed filesize,
#'   original filesize, ratio of compressed / original
#'   filesize (i.e., the compression rate)
#' @export
#'
#' @examples
#' # Example: URL as input
#' # sample image is a Lenna (http://www.lenna.org)
#' # an uncompressed version can be found at http://www.hlevkin.com/TestImages/classic.htm
#' quantify_complexity("http://www.hlevkin.com/TestImages/lenna.bmp")
#' quantify_complexity("http://www.hlevkin.com/TestImages/lenna.bmp", rotate = TRUE)
#'
#' # example image with high complexity
#' # http://www.hlevkin.com/TestImages/barbara.bmp
#'
#' # example image with low complexity
#' # http://www.hlevkin.com/TestImages/baboon.bmp
#'
#' @references Donderi, D. C. (2006). Visual complexity: A
#'   Review. \emph{Psychological Bulletin}, \emph{132},
#'   73--97.
#'   doi:\href{https://doi.org/10.1037/0033-2909.132.1.73}{10.1037/0033-2909.132.1.73}
#'
#'
#' @references Forsythe, A., Nadal, M., Sheehy, N.,
#'   Cela-Conde, C. J., & Sawey, M. (2011). Predicting
#'   Beauty: Fractal Dimension and Visual Complexity in Art.
#'   \emph{British Journal of Psychology}, \emph{102},
#'   49--70.
#'   doi:\href{https://doi.org/10.1348/000712610X498958}{10.1348/000712610X498958}
#'
#'
#' @seealso \code{\link{quantify_symmetry}},
#'   \code{\link{quantify_contrast}},
#'   \code{\link{quantify_typicality}},
#'   \code{\link{quantify_self_similarity}}
#'
#' @importFrom utils zip
quantify_complexity <- function(imgfile, rotate = FALSE){

  if (is.character(imgfile) & !is.array(imgfile)) {
    if (length(imgfile) == 1) {
      input <- "image_path"
    } else {
      stop("Multiple filenames. Function can only handle one image at a time.", call. = FALSE)
    }
  } else if (is.array(imgfile) & (is.numeric(imgfile) | is.integer(imgfile))) {
    if (is.array(imgfile) & (length(dim(imgfile)) != 3 | dim(imgfile)[3] != 3)) {
      stop("Wrong array dimensions (has to be a 3-dimensional array)", call. = FALSE)
    } else {
      input <- "image"
    }
  } else {
    stop("Wrong type of input: has to be a filename (character string) or an image (3-dimensional array of numeric or integer values)", call. = FALSE)
  }

  if (!requireNamespace("magick", quietly = TRUE)) stop("Package 'magick' not found but needed. Please install the package first.", call. = FALSE)

  # actual function
  quant_compl <- function(imgfile, rotate) {
    flname <- .rand_string()
    img <- magick::image_read(imgfile)
    imginfo <- magick::image_info(img)

    isBMP <- grepl("^BMP", as.character(imginfo[1]))
    if (!isBMP) warning("Input image might not be uncompressed. Interpret results with caution.", call. = FALSE)

    # write bmp image
    magick::image_write(img, path = paste0(flname, ".bmp"), format = "bmp")

    # zip-compress file
    Sys.sleep(0.25) # wait a moment
    suppressWarnings(zip(zipfile = flname, files = paste0(flname, ".bmp")))

    # read file sizes
    orig_size <- file.size(paste0(flname, ".bmp"))
    compressed_size <- file.size(paste0(flname,".zip"))

    # remove newly created files
    Sys.sleep(0.25) # wait a moment
    file.remove(paste0(flname, ".bmp"))
    file.remove(paste0(flname, ".zip"))

    if (!is.logical(rotate)) {
      warning(paste0("rotate = '", rotate, "' is not a logical value (TRUE/FALSE). Skipping rotation ..."), call. = FALSE)
      rotate <- FALSE
    }

    if (rotate) {
      flname_rot <- paste0(flname, "_rot")

      message("... trying to rotate the image ...")
      # write rotated image
      magick::image_write(magick::image_rotate(img, degrees = 90), path = paste0(flname_rot, ".bmp"), format = "bmp")

      # zip-compress rotated image and read file size
      Sys.sleep(0.25) # wait a moment
      zip(zipfile = flname_rot, files = paste0(flname_rot, ".bmp"))
      compressed_size_rot <- file.size(paste0(flname_rot,".zip"))

      # remove rotated files
      Sys.sleep(0.25) # wait a moment
      file.remove(paste0(flname_rot,".zip"))
      file.remove(paste0(flname_rot, ".bmp"))

      # update result: minimum of both compressed file sizes of original and rotated img
      compressed_size <- min(compressed_size, compressed_size_rot)
    }

    # return results
    return(list(compressed = compressed_size, original = orig_size, ratio = compressed_size/orig_size))
  }

  try_err_file <- inherits(tryCatch(magick::image_read(imgfile), error = function(err) err), "error")
  if (try_err_file) {
    if (input == "image_path") {
      stop(paste0("File not found or invalid path (could not resolve input)"), call. = FALSE)
    } else {
      stop(paste0("File not found or invalid path (could not resolve '", imgfile ,"')"), call. = FALSE)
    }
  } else {
    quant_compl(imgfile, rotate)
  }
}


# # not in use yet
# quantify_entropy <- function(img) {
#
#   binMin <- 0
#   binMax <- 1
#   if (max(img) > 1) binMax <- 255
#
#   # calculate histogram (with 256 bins as in Matlab)
#   imgHist <- hist(as.vector(img), breaks = seq(binMin, binMax, length.out = 256), plot = FALSE)$density
#   #imgHist <- hist(img, breaks = seq(binMin, binMax, length.out = 256), plot = FALSE)$counts
#
#   # remove zero entries
#   imgHist[imgHist==0] <- NA
#   imgHist <- na.omit(imgHist)
#
#   # normalize
#   imgNorm <- imgHist
#   #imgNorm <- imgHist / sum(imgHist)
#
#   # entropy
#   entr <- -sum(imgNorm * log2(imgNorm))
#   return(entr)
# }
