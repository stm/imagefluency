#' @details \emph{The main functions are:}
#' \itemize{
#'   \item \code{\link[imagefluency]{img_contrast}} to get the visual contrast of an
#'   image
#'   \item \code{\link[imagefluency]{img_complexity}} to get the visual complexity of
#'   an image (equals 1 minus image simplicity)
#'   \item \code{\link[imagefluency]{img_self_similarity}} to get the visual
#'   self-similarity of an image
#'   \item \code{\link[imagefluency]{img_simplicity}} to get the visual simplicity
#'   of an image (equals 1 minus image complexity)
#'   \item \code{\link[imagefluency]{img_symmetry}} to get the vertical and
#'   horizontal symmetry of an image
#'   \item \code{\link[imagefluency]{img_typicality}} to get the visual typicality
#'   of a list of images relative to each other
#' }
#' \emph{Other helpful functions are:}
#' \itemize{
#'   \item \code{\link[imagefluency]{img_read}} wrapper function
#'   to read images using \code{\link[readbitmap:read.bitmap]{readbitmap::read.bitmap}}
#'   \item \code{\link[imagefluency]{run_imagefluency}} to launch a Shiny app
#'   for an interactive demo of the main functions
#'   \item \code{\link[imagefluency]{rgb2gray}} to convert images from
#'   RGB into grayscale
#' }
#'
#'
#' @references Mayer, S. & Landwehr, J, R. (2018). Quantifying Visual Aesthetics
#'   Based on Processing Fluency Theory: Four Algorithmic Measures for
#'   Antecedents of Aesthetic Preferences. \emph{Psychology of Aesthetics,
#'   Creativity, and the Arts}, \emph{12}(4), 399--431.
#'   \doi{10.1037/aca0000187}
#'
#' @references Mayer, S. & Landwehr, J. R. (2018). Objective measures of design
#'   typicality. \emph{Design Studies}, \emph{54}, 146--161.
#'   \doi{10.31219/osf.io/gtbhw}
#'
#' @keywords internal
"_PACKAGE"
