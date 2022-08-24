#' @include utils.R
NULL

#' Typicality of images relative to each other
#'
#' \code{img_typicality} returns the visual typicality of a list of images
#' relative to each other. Higher values indicate larger typicality.
#'
#' @details The function returns the visual typicality of a \emph{list} of image
#'   arrays or matrices \code{imglist} relative to each other. Values can range
#'   between -1 (inversely typical) over 0 (not typical) to 1 (perfectly typical).
#'   That is, higher absolute values indicate a larger typicality.
#'
#'   The typicality score is computed as the correlation of a particular image
#'   with the average representation of all images, i.e. the mean of all images.
#'   For color images, the weighted average between each color channel's values
#'   is computed. If the images have different dimensions they are automatically
#'   resized to the smallest height and width.
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
#'   scores (per default no rescaling is performed). Rescaling is performed by
#'   \code{OpenImageR}'s \code{\link[OpenImageR]{resizeImage}} function
#'   (bilinear rescaling)
#'
#' @return a named matrix of numeric values (typicality scores)
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
#' @references Mayer, S. & Landwehr, J. R. (2018). Objective measures of design
#'   typicality. \emph{Design Studies}, \emph{54}, 146--161.
#'   \doi{10.1016/j.destud.2017.09.004}
#'
#'
#'
#' @seealso \code{\link{img_read}}, \code{\link{img_contrast}},
#'    \code{\link{img_complexity}}, \code{\link{img_self_similarity}}
#'   \code{\link{img_simplicity}}, \code{\link{img_symmetry}}
img_typicality <- function(imglist, rescale = NULL){

  # check input
  #
  # input list?
  if (!is.list(imglist)) {
    stop("Input has to be a *list* of image matrices", call. = FALSE)
  }
  #
  # at least 2 images?
  if (length(imglist) < 2) {
    warning("The function needs at least 2 images in the input list. Returning NA.", call. = FALSE)
    return(list(typicality = NA))
  }
  #tryCatch(lapply(imglist, .check_input, "typicality"), error = function(err) {stop("errorinside", call. = FALSE)})
  #
  #
  for (elmnt in seq_along(imglist)) {
    # input not matrix or array?
    if (is.null(dim( imglist[[elmnt]] ))) {
      stop(paste("List element",elmnt,"has to be a *matrix* or a 3-dimensional *array* of numeric or integer values"))
    }
    # input a matrix?
    if (is.matrix( imglist[[elmnt]] )) {
      # must be numeric or integer
      if (!(is.numeric( imglist[[elmnt]] ) | is.integer( imglist[[elmnt]] ))) {
        stop(paste("List element",elmnt,"has to be a matrix or a 3-dimensional array of *numeric* or *integer* values"))
      }
      imgtype <- "gray"
    } else if (is.array( imglist[[elmnt]] )) {
      # must be 3-dimensional array of integers or numeric values
      if (!is.numeric( imglist[[elmnt]] ) | length(dim( imglist[[elmnt]] )) != 3 | !(dim( imglist[[elmnt]] )[3] == 3 | dim( imglist[[elmnt]] )[3] == 4)) {
        stop(paste("List element",elmnt,"is an invalid array (should be a 3-dimensional array of numeric or integer values)"))
      }
      if (dim( imglist[[elmnt]] )[3] == 4) {
        warning(paste("List element",elmnt,"is an array with 4 dimensions, presumably with alpha channel. 4th dimension is ignored ..."), call. = FALSE)
        imglist[[elmnt]] <- imglist[[elmnt]][,,-4]
      }
      imgtype <- "rgb"
    }
  }

  # resize images same image dimension
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
    if (requireNamespace("OpenImageR", quietly = TRUE)) {
      imglist <- lapply(imglist, OpenImageR::resizeImage,
                                  width = minW, height = minH, method = "bilinear")
    } else {
      stop("Package 'OpenImageR' is required but not installed on your system.", call. = FALSE)
    }
  }

  # original resolution or different scaling level?
  if (!is.null(rescale)) {
    if (!is.numeric(rescale)) {
      stop("parameter 'rescale' must be numeric", call. = FALSE)
    }
    if (rescale != 1) {
      # Rescaling requires package "OpenImageR"
      if (requireNamespace("OpenImageR", quietly = TRUE)) {
        # image dimensions of first element (assumes the same for all elements)
        img_h <- dim(imglist[[1]])[1] # image height
        img_w <- dim(imglist[[1]])[2] # image width
        imglist <- lapply(imglist, OpenImageR::resizeImage,
                          width = rescale*img_w,
                          height = rescale*img_h,
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
    out <- 0.2989 * .typ(redChannels) + 0.5870 * .typ(greenChannels) + 0.1140 * .typ(blueChannels)
    return(out)
    #
  } else return(.typ(imglist))
}


#' .typ
#'
#' Returns the typicality of a list of images as the correlation with the mean image.
#'
#' @param imglist  A list of matrices with numeric values or
#'   integer values.
#'
#' @return a numeric value (RMS contrast)
#' @keywords internal
.typ <- function(imglist){
  imglist <- matrix(unlist(imglist), ncol = length(imglist), byrow = FALSE)
  img_mean <- rowMeans(imglist)
  output <- stats::cor(imglist, img_mean)
  rownames(output) <- paste0("img", seq_len(nrow(output)))
  return(typicality = output)
}

