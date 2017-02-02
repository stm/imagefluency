# idea: typicality(img, method = "correlation | feature_points | phash", ...)

#' Typicality of Images relative to each other
#'
#' \code{quantify_typicality} returns the visual typicality
#' of a list of image matrices \code{imglist} relative to
#' each other. Higher values indicate larger typicality.
#'
#' @details The function returns the visual typicality of a
#'   list of image matrices \code{imglist} relative to each
#'   other, while higher values are indicating larger
#'   typicality.
#'
#'   The typicality score is computed as the correlation of
#'   a particular image with the average representation of
#'   all images, i.e. the mean of all images.
#'
#'   Rescaling of the images prior to computing the
#'   typicality scores can be specified with the optional
#'   rescaling parameter (must be a numeric value).
#'
#'
#' @param imglist A list of matrices with numeric values or
#'   integer values. Color images have to be converted to
#'   grayscale in advance (function \code{rgb2gray}) or each
#'   color channel has to be analyzed seperately.
#' @param rescale numeric. Rescales the images prior to
#'   computing the typicality scores (per default no
#'   rescaling is performed). Rescaling is performed by the
#'   \code{resizeImage} function from the \code{OpenImageR}
#'   package (bilinear rescaling)
#'
#' @return a named matrix of numeric values (typicality
#'   scores)
#' @export
#'
#' @examples
#' # construct a list of 20 sample images
#' imgs <- replicate(20,
#'   matrix(runif(100, min = 0, max = 255), nrow = 10, ncol = 10),
#'   simplify = FALSE)
#'
#' # get typicality scores
#' quantify_typicality(imgs)
#'
#' @references Mayer, S. & Landwehr, J. R. (2016). Measuring
#'   design typicality -- a comparison of objective and
#'   subjective approaches. \emph{Proceedings of DRS 2016,
#'   Design Research Society 50th Anniversary Conference}.
#'   Brighton, UK, 27--30 June 2016.
#'
#' @seealso \code{\link{rgb2gray}},
#'   \code{\link{quantify_symmetry}},
#'   \code{\link{quantify_complexity}},
#'   \code{\link{quantify_contrast}},
#'   \code{\link{quantify_self_similarity}}
#' @importFrom stats cor
quantify_typicality <- function(imglist, rescale = NULL){

  # check input
  if (!is.list(imglist)) {
    stop("Input has to be a *list* of image matrices", call. = FALSE)
  }
  if (length(imglist) < 2) {
    warning("The function needs at least 2 images in the input list. Returning NA.", call. = FALSE)
    return(list(typicality = NA))
  }
  #tryCatch(lapply(imglist, .check_input, "typicality"), error = function(err) {stop("errorinside", call. = FALSE)})
  for (elmnt in seq_along(imglist)) {
    if (!is.matrix( imglist[[elmnt]] )) {
      stop(paste("List element",elmnt,"has to be a *matrix* of numeric or integer values"))
    }
    if (!(is.numeric( imglist[[elmnt]] ) | is.integer( imglist[[elmnt]] ))) {
      stop(paste("List element",elmnt,"has to be a matrix of *numeric* or *integer* values"), call. = FALSE)
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
  # create matrix of vectorized intensity values
  imglist <- matrix(unlist(imglist), ncol = length(imglist), byrow = FALSE)
  img_mean <- rowMeans(imglist)
  output <- cor(imglist, img_mean)
  rownames(output) <- paste0("img", 1:nrow(output))
  return(typicality = output)
}

