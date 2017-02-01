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
#' Supported images types are \code{.bmp}, \code{.png},
#' \code{.jpg}, and \code{.jpeg}.
#'
#' @details The function returns the visual complexity of an
#'   image. Visual complexity is calculated as the size of
#'   the zip-compressed image file. Preferably, the input
#'   is an uncompressed image file. If the input image is not an
#'   uncompressed \code{.bmp} file, the function tries to
#'   create an uncompressed version of the image.
#'
#'   The function takes the file path of an image file or a pre-loaded image as input argument (\code{imgfile}) and returns
#'   the filesize of the zip-compressed image file.
#'   Additionally, the uncompressed filesize and the
#'   compression rate are returned. Note that permission to
#'   read and write files in the specified directory path / working directory is
#'   needed for this function to work properly.
#'
#'   The zip compression algorithm works line by line and
#'   hence does not depict horizontal and vertical
#'   redundancies equally (see Example 1). Thus, the function includes an
#'   optional \code{rotate} parameter (default:
#'   \code{FALSE}). Setting this parameter to \code{TRUE}
#'   has the following effects: first, the image is loaded
#'   via the \code{readbitmap} package (if \code{imgfile} is
#'   not a pre-loaded image). Second, the image is
#'   rotated by 90 degrees. Third, the rotated image is
#'   written in the same directory as the input file (or the working directory) and a
#'   zip-compressed version is created. Finally, the
#'   complexity of the image is copmuted as the minimum of
#'   the original image's filesize and the filesize of the
#'   rotated image. All files that are created while the
#'   function runs are automatically deleted once the
#'   complexity score is calculated.
#'
#'   In the current version, only \code{.bmp}, \code{.png},
#'   \code{.jpg}, and \code{.jpeg} images are supported.
#'
#' @param imgfile A character string containing the file name of the image or an array or a matrix of numeric or integer values representing the pre-loaded image.
#' @param rotate logical. Should the compressed filesize of
#'   the rotated image also be computed? (see details)
#'
#' @return a list of numeric values: compressed filesize,
#'   original filesize, ratio of compressed / original
#'   filesize (i.e., the compression rate)
#' @export
#'
#' @examples
#' # Example 1: pre-loaded image as input
#' #
#' # create sample image
#' img <- matrix(0, nrow = 500, ncol = 500)
#' img[, 201:300] <- 1
#'
#' # get complexity
#' quantify_complexity(img)
#' quantify_complexity(img, rotate = TRUE) # rotated file smaller (i.e., zip works line by line)
#'
#'
#' # Example 2: image path as input
#' #
#' # Download Lena sample image (see http://eeweb.poly.edu/~yao/EL5123/SampleData.html)
#' download.file("http://eeweb.poly.edu/~yao/EL5123/image/lena_gray.bmp",
#'    "Lena.bmp", mode = "wb")
#'
#' # get complexity
#' quantify_complexity("Lena.bmp")
#'
#' # get complexity (including rotated version)
#' quantify_complexity("Lena.bmp", rotate = TRUE)
#'
#' # compute simplicity score
#' lena_results <- quantify_complexity("Lena.bmp", rotate = TRUE)
#' lena_results$original / lena_results$compressed # simplicity ratio
#'
#' # delete image of Lena
#' file.remove("Lena.bmp")
#'
#' @references Donderi, D. C. (2006). Visual complexity: A
#'   Review. \emph{Psychological Bulletin}, \emph{132},
#'   73--97.
#'   doi:\href{https://doi.org/10.1037/0033-2909.132.1.73}{10.1037/0033-2909.132.1.73}
#'
#' @references Forsythe, A., Nadal, M., Sheehy, N.,
#'   Cela-Conde, C. J., & Sawey, M. (2011). Predicting
#'   Beauty: Fractal Dimension and Visual Complexity in Art.
#'   \emph{British Journal of Psychology}, \emph{102},
#'   49--70.
#'   doi:\href{https://doi.org/10.1348/000712610X498958}{10.1348/000712610X498958}
#'
#' @seealso \code{\link{quantify_symmetry}},
#'   \code{\link{quantify_contrast}},
#'   \code{\link{quantify_typicality}},
#'   \code{\link{quantify_self_similarity}}
#'
#' @importFrom grDevices bmp dev.off
#' @importFrom graphics plot
#' @importFrom utils zip
quantify_complexity <- function(imgfile, rotate = FALSE){

  if (is.character(imgfile) & !is.matrix(imgfile) & !is.array(imgfile)) {
    if (length(imgfile) == 1) {
      input <- "image_path"
    } else {
      stop("Multiple filenames. Function can only handle one image at a time.", call. = FALSE)
    }
  } else if ((is.matrix(imgfile) || is.array(imgfile)) & (is.numeric(imgfile) | is.integer(imgfile))) {
    if (is.array(imgfile) & !is.matrix(imgfile) & (length(dim(imgfile)) != 3 | dim(imgfile)[3] != 3)) {
      stop("Wrong array dimensions (has to be a 3-dimensional array)", call. = FALSE)
    } else {
      input <- "image"
    }
  } else {
    stop("Wrong type of input: has to be a filename (character string) or an image (array or matrix of numeric or integer values)", call. = FALSE)
  }


  if (input == "image_path") {

    try_err_file = inherits(tryCatch(normalizePath(imgfile, mustWork = TRUE), error = function(err) err), "error")
    if (!try_err_file) {
      imgfile <- normalizePath(imgfile)
    } else {
      stop(paste0("File not found or invalid path (could not resolve '", imgfile ,"')"), call. = FALSE)
    }

    # original file name without file extension
    # #file_wo_ext <- gsub("(.*)\\.bmp$", "\\1", imgfile)
    flext <- sub(".*\\.([^.]+)$", "\\1", imgfile)
    if (!(tolower(flext) %in% c("bmp", "png", "jpeg", "jpg"))) {
      stop(paste0("Unsupported filename extension .", flext,
                  ". Currently only the following image types are supported: .bmp, .png, .jpg, .jpeg"), call. = FALSE)
    }
    file_wo_ext <- gsub(paste0("(.*)\\.",flext,"$"), "\\1", imgfile)

    # new filename with random string to make sure nothing is overwritten
    flname_uncompressed <- paste0(file_wo_ext, "_", .rand_string())
  } else if (input == "image") {
    flext <- "none"
    flname_uncompressed <- .rand_string()
  }


  if (tolower(flext) != "bmp") {

    if (input != "image") {
      warning("Input image is not an uncompressed bitmap. Conversion to .bmp was performed. Results might not be meaningful.", call. = FALSE)
      # flnme is character (checked above)
      if (requireNamespace("readbitmap", quietly = TRUE)) {
        img <- readbitmap::read.bitmap(imgfile)
      } else {
        stop("Package 'readbitmap' not found but needed load the image.")
      }
    } else {
      img <- imgfile
    }

    # image dimensions
    img_h <- dim(img)[1] # image height
    img_w <- dim(img)[2] # image width

    # write bmp image
    bmp(filename = paste0(flname_uncompressed, ".bmp"), width = img_h, height = img_w)
    if (requireNamespace("OpenImageR", quietly = TRUE)) {
      OpenImageR::imageShow(img)
    } else if (requireNamespace("grid", quietly = TRUE)) {
      if (max(img) > 1) {
        img <- img / 255
      }
      grid::grid.raster(img)
    } else {
      stop("Package 'OpenImageR' or 'grid' needed to render the uncompressed image.")
    }
    dev.off()

    # original file size
    orig_size <- file.size(paste0(flname_uncompressed, ".bmp"))

    # zip-compress file
    Sys.sleep(0.5) # wait for half a second
    suppressWarnings(zip(zipfile = flname_uncompressed, files = paste0(flname_uncompressed, ".bmp")))

    # remove newly created .bmp file
    Sys.sleep(0.5) # wait for half a second
    file.remove(paste0(flname_uncompressed, ".bmp"))

  } else {
    # original file size
    orig_size <- file.size(imgfile)

    # zip-compress file
    Sys.sleep(0.5) # wait for half a second
    suppressWarnings(zip(zipfile = flname_uncompressed, files = imgfile))
  }

  # read file size of compressed file
  compressed_size <- file.size(paste0(flname_uncompressed,".zip"))
  # remove newly created .zip file
  Sys.sleep(0.5) # wait for half a second
  file.remove(paste0(flname_uncompressed,".zip"))

  if (!is.logical(rotate)) {
    warning(paste0("rotate = '", rotate, "' is not a logical value (TRUE/FALSE). Skipping rotation ..."), call. = FALSE)
    rotate <- FALSE
  }

  if (rotate) {

    message("... trying to rotate the image ...")

    # no img as parameter given but image path, try to load original image
    if (input == "image_path") {
      # flnme is character (checked above)
      if (requireNamespace("readbitmap", quietly = TRUE)) {
        img <- readbitmap::read.bitmap(imgfile)
      } else {
        stop("Package 'readbitmap' not found but needed to load the .bmp image for rotation")
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
    flname_rot <- paste0(flname_uncompressed, "_rot")
    bmp(filename = paste0(flname_rot, ".bmp"), width = img_w, height = img_h) # height and width switched because of rotation
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
    Sys.sleep(0.5) # wait for half a second
    zip(zipfile = flname_rot, files = paste0(flname_rot, ".bmp"))
    compressed_size_rot <- file.size(paste0(flname_rot,".zip"))
    # remove rotated files
    Sys.sleep(0.5) # wait for half a second
    file.remove(paste0(flname_rot,".zip"))
    file.remove(paste0(flname_rot, ".bmp"))

    # update result: minimum of both compressed file sizes of original and rotated img
    compressed_size <- min(compressed_size, compressed_size_rot)
  }

  # return results
  return(list(compressed = compressed_size, original = orig_size, ratio = compressed_size/orig_size))
}

