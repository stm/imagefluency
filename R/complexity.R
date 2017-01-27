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

  # original file size
  orig_size <- file.size(flname)

  # original file name without file extension (.bmp)
  file_wo_ext <- gsub("(.*)\\.bmp$", "\\1", flname)

  # zip-compress file and read file size of compressed file
  suppressWarnings(zip(zipfile = file_wo_ext, files = flname))
  compressed_size <- file.size(paste0(file_wo_ext,".zip"))
  # remove newly created .zip file
  file.remove(paste0(file_wo_ext,".zip"))

  if (rotate) {

    # check input
    .check_input(img, f_call = "complexity")

    # image dimensions
    img_h <- dim(img)[1] # image height
    img_w <- dim(img)[2] # image width

    # rotate image
    img_rot <- OpenImageR::rotateFixed(img,90)
    # write as uncompressed image file
    bmp(filename = paste0(file_wo_ext, "_rot.bmp"), width = img_w, height = img_h) # height and width switched because of rotation
    OpenImageR::imageShow(img_rot)
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

