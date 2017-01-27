#' @include utils.R
NULL

## -----------------------
##      rms contrast
## -----------------------
# RMS contrast is defined as the standard deviation of the
# normalized (grayscale) intensity values in the range
# between [0, 1].

quantify_contrast <- function(img, normalize = TRUE){

  # check input
  .check_input(img, f_call = "contrast")

  pixAll <- as.vector(img)
  if (normalize) pixAll <- pixAll / 255

  # via built-in sd function
  return(list(contrast = sd(pixAll)))

  # # alternative 1: via normalization
  # pixAllMean <- pixAll - mean(pixAll)
  # norm(pixAllMean,'2')/sqrt(length(pixAllMean))

  # # alternative 2: direct formula
  # sqrt(sum((pixAll - mean(pixAll))^2)/length(pixAll))
}
