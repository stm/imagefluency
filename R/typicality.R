#' @include utils.R
NULL

#' Typicality of images relative to each other
#'
#' \code{img_typicality} returns the visual typicality of a list of images
#' relative to each other. Higher values indicate larger typicality.
#'
#' @details The function returns the visual typicality of a \emph{list} of image
#'   arrays or matrices \code{imglist} relative to each other. Values can range
#'   from -1 (inversely typical) through 0 (not typical) to 1 (perfectly typical).
#'   That is, higher absolute values indicate a larger typicality.
#'
#'   The typicality score is computed as the correlation of a particular image
#'   with the average representation of all images, i.e., the mean of all images.
#'   For color images, the weighted average between each color channel's values
#'   is computed. If the images have different dimensions they are automatically
#'   resized to the smallest height and width. If the list contains both
#'   grayscale and color images, the grayscale images are treated as color
#'   images with three identical color channels (with a warning).
#'
#'   Rescaling of the images prior to computing the typicality scores can be
#'   specified with the optional rescaling parameter (must be a numeric value).
#'   Most users won't need any rescaling and can use the default (\code{rescale
#'   = NULL}). See Mayer & Landwehr (2018) for more details.
#'
#'
#' @param imglist A \emph{list} of arrays or matrices with numeric values. Use
#'   e.g. \code{\link{img_read}()} to read image files into \code{R} (see
#'   example).
#' @param rescale numeric. Rescales the images prior to computing the typicality
#'   scores (by default, no rescaling is performed). Rescaling is performed by
#'   \code{OpenImageR}'s \code{\link[OpenImageR]{resizeImage}} function
#'   (bilinear rescaling).
#'
#' @return a matrix of numeric values (typicality scores) with one row per
#'   image and one column. The rows are named after the names of the input list
#'   (\code{img1}, \code{img2}, ... for images without a name). If the list
#'   contains fewer than two images, the scores are \code{NA}.
#' @export
#'
#' @examples
#' # Example images depicting valleys: valley_green, valley_white
#' # Example image depicting fireworks: fireworks
#' valley_green <- img_read(
#'     system.file("example_images", "valley_green.jpg", package = "imagefluency")
#'   )
#' valley_white <- img_read(
#'     system.file("example_images", "valley_white.jpg", package = "imagefluency")
#'   )
#' fireworks <- img_read(
#'     system.file("example_images", "fireworks.jpg", package = "imagefluency")
#'   )
#' #
#' # display images
#' grid::grid.raster(valley_green)
#' grid::grid.raster(valley_white)
#' grid::grid.raster(fireworks)
#'
#' # create image set as list
#' imglist <- list(fireworks, valley_green, valley_white)
#'
#' # get typicality
#' img_typicality(imglist)
#'
#' # named list: scores are labeled with the names of the images
#' img_typicality(list(fireworks = fireworks, valley_green = valley_green,
#'                     valley_white = valley_white))
#'
#' @references Mayer, S. & Landwehr, J. R. (2018). Objective measures of design
#'   typicality. \emph{Design Studies}, \emph{54}, 146--161.
#'   \doi{10.1016/j.destud.2017.09.004}
#'
#'
#'
#' @seealso \code{\link{img_read}}, \code{\link{img_contrast}},
#'    \code{\link{img_complexity}}, \code{\link{img_self_similarity}},
#'   \code{\link{img_simplicity}}, \code{\link{img_symmetry}}
img_typicality <- function(imglist, rescale = NULL){

  # check input
  #
  # input list?
  if (!is.list(imglist) || is.data.frame(imglist)) {
    stop("Input has to be a *list* of image matrices", call. = FALSE)
  }
  #
  # names of the images (names of the list, otherwise img1, img2, ...)
  imgnames <- paste0("img", seq_along(imglist))
  if (!is.null(names(imglist))) {
    has_name <- !is.na(names(imglist)) & names(imglist) != ""
    imgnames[has_name] <- names(imglist)[has_name]
  }
  #
  # at least 2 images?
  if (length(imglist) < 2) {
    warning("The function needs at least 2 images in the input list. Returning NA.", call. = FALSE)
    return(matrix(NA_real_, nrow = length(imglist), ncol = 1,
                  dimnames = list(imgnames, NULL)))
  }
  #
  # check each image (same check as in all other functions)
  imgtypes <- character(length(imglist))
  for (elmnt in seq_along(imglist)) {
    checked <- withCallingHandlers(
      tryCatch(.prepare_img(imglist[[elmnt]]),
               error = function(err) {
                 stop(paste0("List element ", elmnt, ": ", conditionMessage(err)), call. = FALSE)
               }),
      warning = function(wrn) {
        warning(paste0("List element ", elmnt, ": ", conditionMessage(wrn)), call. = FALSE)
        invokeRestart("muffleWarning")
      })
    imglist[[elmnt]] <- checked$img
    imgtypes[elmnt] <- checked$type
  }
  #
  # grayscale and color images in the same list?
  imgtype <- if (any(imgtypes == "rgb")) "rgb" else "gray"
  if (imgtype == "rgb" && any(imgtypes == "gray")) {
    warning("The input list contains both grayscale and color images. Grayscale images are treated as color images with three identical color channels.", call. = FALSE)
    imglist[imgtypes == "gray"] <- lapply(imglist[imgtypes == "gray"],
                                          function(x) array(x, dim = c(dim(x), 3)))
  }

  # resize images to the same image dimension
  # using the smallest width and height over all pics
  if (imgtype == "gray") {
    dims <- vapply(imglist, dim, numeric(2))
  } else {
    dims <- vapply(imglist, dim, numeric(3))
  }
  minH <- min(dims[1,])
  minW <- min(dims[2,])
  # check whether all images already have the same dimension, otherwise resize
  if (!(all(dims[-3, ] == c(minH, minW)))) {
    # Resizing requires package "OpenImageR"
    if (.pkg_avail("OpenImageR")) {
      # note: in OpenImageR, 'width' = number of rows and 'height' = number of
      # columns, i.e., the opposite of the image's width and height
      imglist <- lapply(imglist, OpenImageR::resizeImage,
                                  width = minH, height = minW, method = "bilinear")
    } else {
      stop("Package 'OpenImageR' is required but not installed on your system.", call. = FALSE)
    }
  }

  # original resolution or different scaling level?
  if (!is.null(rescale)) {
    if (!is.numeric(rescale)) {
      stop("parameter 'rescale' must be numeric", call. = FALSE)
    }
    if (length(rescale) != 1 || is.na(rescale) || rescale <= 0) {
      stop("parameter 'rescale' must be a single positive number", call. = FALSE)
    }
    if (rescale != 1) {
      # Rescaling requires package "OpenImageR"
      if (.pkg_avail("OpenImageR")) {
        # image dimensions of first element (assumes the same for all elements)
        img_h <- dim(imglist[[1]])[1] # image height
        img_w <- dim(imglist[[1]])[2] # image width
        # note: OpenImageR's 'width' = rows, 'height' = columns (see above)
        imglist <- lapply(imglist, OpenImageR::resizeImage,
                          width = max(1, rescale*img_h),
                          height = max(1, rescale*img_w),
                          method = "bilinear")
      } else {
        stop("Package 'OpenImageR' is required for rescaling but not installed on your system.", call. = FALSE)
      }
    }
  }

  if (imgtype == "rgb") {
    # split image into channels
    redChannels <- lapply(imglist, function(x) x[, , 1])
    greenChannels <- lapply(imglist, function(x) x[, , 2])
    blueChannels <- lapply(imglist, function(x) x[, , 3])
    #
    out <- 0.299 * .typ(redChannels) + 0.587 * .typ(greenChannels) + 0.114 * .typ(blueChannels)
  } else {
    out <- .typ(imglist)
  }

  rownames(out) <- imgnames
  return(out)
}


#' .typ
#'
#' Returns the typicality of a list of images as the correlation with the mean image.
#'
#' @param imglist  A list of matrices with numeric values or
#'   integer values.
#'
#' @return a named matrix of numeric values (typicality scores)
#' @keywords internal
.typ <- function(imglist){
  imglist <- matrix(unlist(imglist), ncol = length(imglist), byrow = FALSE)
  img_mean <- rowMeans(imglist)
  # (use = "pairwise.complete.obs" is considerably faster than the default;
  # the images contain no missing values at this point)
  output <- stats::cor(imglist, img_mean, use = "pairwise.complete.obs")
  rownames(output) <- paste0("img", seq_len(nrow(output)))
  return(output)
}
