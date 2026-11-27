#' @include utils.R complexity.R contrast.R self-similarity.R simplicity.R symmetry.R typicality.R
NULL

#' Image fluency scores for several images at once
#'
#' \code{img_fluency} computes the image fluency scores for several images at
#' once and returns the results as a data frame with one row per image.
#'
#' @details The function reads one image after the other and computes the
#'   requested scores with the respective functions of the package using their
#'   default settings:
#'
#'   \itemize{
#'     \item \code{"contrast"}: \code{\link{img_contrast}()}
#'     \item \code{"self_similarity"}: \code{\link{img_self_similarity}()}
#'     \item \code{"simplicity"}: \code{\link{img_simplicity}()}
#'     \item \code{"symmetry"}: vertical symmetry, i.e.,
#'       \code{\link{img_symmetry}()} with \code{horizontal = FALSE}
#'     \item \code{"typicality"}: \code{\link{img_typicality}()}
#'   }
#'
#'   Typicality is not computed by default (but can be added to
#'   \code{metrics}) for two reasons. First, typicality is not a
#'   characteristic of a single image, but of an image relative to all other
#'   images that are analyzed. Thus, the typicality scores change if images are
#'   added or removed. Second, all images have to be kept in memory at the same
#'   time to compute typicality, which might be a problem if you analyze many
#'   or large images. For all other scores, only one image is kept in memory at
#'   a time.
#'
#'   The function does not stop if an image file cannot be read or if a score
#'   cannot be computed for an image (e.g., symmetry for an image without any
#'   variation in one image half). Instead, the respective scores in the
#'   results are \code{NA}, and a warning at the end lists the images concerned.
#'   Warnings of the individual functions are not shown. Use the individual
#'   functions if you need other than the default settings or want to find out
#'   why a score could not be computed for an image.
#'
#' @param imgs Either a character vector with the paths to the image files
#'   (e.g., created by \code{\link[base]{list.files}()} with \code{full.names =
#'   TRUE}) or a \emph{list} of pre-loaded images (matrices or arrays of numeric
#'   values, e.g. by using \code{\link{img_read}()}).
#' @param metrics Character vector that specifies which scores are computed.
#'   One or more of \code{"contrast"}, \code{"self_similarity"},
#'   \code{"simplicity"}, \code{"symmetry"}, and \code{"typicality"}. By
#'   default, all scores except typicality are computed (see details).
#' @param gray logical. Should color images be converted into grayscale (using
#'   \code{\link{rgb2gray}()}) before the scores are computed? This speeds up
#'   the computation. (default: \code{FALSE})
#' @param progress logical. Should a progress bar be shown? (default:
#'   \code{TRUE} in interactive sessions)
#'
#' @return a data frame with one row per image (in the order of \code{imgs}).
#'   The first column \code{filename} contains the file names of the images (for
#'   a list of pre-loaded images: the names of the list; \code{img1},
#'   \code{img2}, ... for images without a name). The other columns contain the
#'   scores and are named after \code{metrics}.
#' @export
#'
#' @examples
#' # paths to some of the example images
#' files <- system.file("example_images", c("rails.jpg", "sky.jpg", "trees.jpg"),
#'                      package = "imagefluency")
#'
#' # contrast and symmetry scores of all images
#' img_fluency(files, metrics = c("contrast", "symmetry"))
#'
#' # add typicality (of the images relative to each other)
#' img_fluency(files, metrics = c("contrast", "symmetry", "typicality"))
#'
#' @examplesIf .Platform$OS.type != "windows"
#' # all scores except typicality (default)
#' img_fluency(files)
#'
#' @seealso \code{\link{img_read}}, \code{\link{img_contrast}},
#'   \code{\link{img_self_similarity}}, \code{\link{img_simplicity}},
#'   \code{\link{img_symmetry}}, \code{\link{img_typicality}}
img_fluency <- function(imgs,
                        metrics = c("contrast", "self_similarity", "simplicity", "symmetry"),
                        gray = FALSE, progress = interactive()){

  # check input
  all_metrics <- c("contrast", "self_similarity", "simplicity", "symmetry", "typicality")
  if (!is.character(metrics) || length(metrics) < 1 || !all(metrics %in% all_metrics)) {
    stop(paste0("Unknown input to parameter 'metrics'. Use one or more of the following: ",
                paste(all_metrics, collapse = ", ")), call. = FALSE)
  }
  metrics <- all_metrics[all_metrics %in% metrics]
  gray <- .check_flag(gray, "gray", default = FALSE)
  progress <- .check_flag(progress, "progress", default = FALSE)

  from_path <- is.character(imgs) && is.null(dim(imgs))
  if (!from_path && (!is.list(imgs) || is.data.frame(imgs))) {
    stop("Input has to be a character vector of file paths or a *list* of images", call. = FALSE)
  }
  n_imgs <- length(imgs)
  if (n_imgs < 1) {
    stop("Input does not contain any images", call. = FALSE)
  }

  # one row per image
  filenames <- if (from_path) basename(imgs) else .img_names(imgs)
  results <- data.frame(filename = filenames, stringsAsFactors = FALSE)
  results[metrics] <- NA_real_

  # computes a score; NA if the score cannot be computed
  score <- function(expr) {
    tryCatch(suppressWarnings(as.numeric(expr)), error = function(err) NA_real_)
  }

  # all images are only kept in memory if typicality is requested
  typicality <- "typicality" %in% metrics
  if (typicality) all_imgs <- vector("list", n_imgs)
  not_readable <- logical(n_imgs)

  show_progress <- progress && n_imgs > 1
  if (show_progress) {
    progress_bar <- utils::txtProgressBar(min = 0, max = n_imgs, style = 3)
    on.exit(close(progress_bar), add = TRUE)
  }

  for (i in seq_len(n_imgs)) {
    # read and check image (same check as in all other functions)
    img <- tryCatch({
      img <- if (from_path) img_read(imgs[i]) else imgs[[i]]
      img <- suppressWarnings(.prepare_img(img))
      if (gray && img$type == "rgb") rgb2gray(img$img) else img$img
    }, error = function(err) NULL)

    if (is.null(img)) {
      not_readable[i] <- TRUE
    } else {
      # compute scores
      if ("contrast" %in% metrics) {
        results$contrast[i] <- score(img_contrast(img))
      }
      if ("self_similarity" %in% metrics) {
        results$self_similarity[i] <- score(img_self_similarity(img))
      }
      if ("simplicity" %in% metrics) {
        results$simplicity[i] <- score(img_simplicity(img))
      }
      if ("symmetry" %in% metrics) {
        results$symmetry[i] <- score(img_symmetry(img, horizontal = FALSE))
      }
      if (typicality) all_imgs[[i]] <- img
    }

    if (show_progress) utils::setTxtProgressBar(progress_bar, i)
  }

  # typicality of the images relative to each other (needs at least 2 images)
  if (typicality && sum(!not_readable) >= 2) {
    results$typicality[!not_readable] <- score(img_typicality(all_imgs[!not_readable]))
  }

  # summary of problems
  if (any(not_readable)) {
    warning(paste0(sum(not_readable), " of ", n_imgs,
                   " images could not be read or are not valid images (all scores are NA): ",
                   paste(filenames[not_readable], collapse = ", ")), call. = FALSE)
  }
  not_computed <- !not_readable & rowSums(is.na(results[metrics])) > 0
  if (any(not_computed)) {
    warning(paste0("Not all scores could be computed for ", sum(not_computed), " of ", n_imgs,
                   " images (the respective scores are NA): ",
                   paste(filenames[not_computed], collapse = ", ")), call. = FALSE)
  }

  return(results)
}
