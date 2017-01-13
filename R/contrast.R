## -----------------------
##      rms contrast
## -----------------------
# RMS contrast is defined as the standard deviation of the pixel intensities
# NOTE: function assumes that the image is matrix of normalized (grayscale)
#       intensity values in the range between [0, 1].

quantify_contrast <- function(img){
  pixAll <- as.vector(img)
  # pixNorm <- pixAll / 255 # this is assumed by default
  # via built-in sd function
  return(list(contrast = sd(pixAll)))

  # # alternative 1: vial normalization
  # pixAllMean <- pixAll - mean(pixAll)
  # norm(pixAllMean,'2')/sqrt(length(pixAllMean))

  # # alternative 2: direct formula
  # sqrt(sum((pixAll - mean(pixAll))^2)/length(pixAll))
}
