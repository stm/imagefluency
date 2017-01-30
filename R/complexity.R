# 2do: idea / test: image compression via PCA (for images that are not in an uncompressed format)?
# see http://www.aaronschlegel.com/image-compression-principal-component-analysis/

#' @include utils.R
NULL

## -----------------------
##      complexity
## -----------------------
# visual complexity simply is the size of the zip-compressed image file.
# Thus, it is important that the original image is in an uncompressed format.
#
# The function takes the file path of an uncompressed image file as
# input argument and returns the file size of the compressed image file.
# Additionally, the uncompressed file size and the compression rate are returned.
#
# NOTE: If the file is in the same directory as the current working directory,
# it is sufficient to pass just the file name as input to the function.
#
# Further, in the current version, the messages that are displayed by the zip
# command are printed when calling the function (because zip calls system2, and
# system2 uses stdout = "" and stderr = "" by default). Maybe something along
# the lines of patching system2 would do the trick (see [1]), but for now, the
# user just has to live with it.
# [1] https://stackoverflow.com/questions/12416076/suppress-install-outputs-in-r/12504606

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
        img <- readbitmap::read.bitmap(path)
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

