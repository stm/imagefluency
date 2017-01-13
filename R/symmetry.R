## -----------------------
##   vertical symmetry
## -----------------------

# 2do: what if image width / height cannot be divided by 2?
#
quantify_symmetry <- function(img){
  # cut image into 2 equal pieces (horizontally, that means across the x axis)
  stimL <- img[ , 1:(stimW/2)]
  stimR <- img[ , (1 + stimW/2):stimW ]

  # flip right image half
  stimRfl <- OpenImageR::flipImage(stimR, mode = "horizontal")

  # vectorize matrices
  pixL <- as.vector(stimL)
  pixR <- as.vector(stimRfl)

  # correlation of image halves
  corrLR <- cor(pixL,pixR)

  # final symmetry: vertical (absolute correlation)
  sym_v <- abs(corrLR)


  ## -----------------------
  ##   horizontal symmetry
  ## -----------------------

  # cut image into 2 equal pieces (vertically, that means across the y axis)
  stimU <- img[ 1:(stimH/2), ]
  stimD <- img[ (1 + stimH/2):stimH, ]

  # flip lower image half
  stimDfl <- OpenImageR::flipImage(stimD, mode = "vertical")

  # vectorize matrices
  pixU <- as.vector(stimU)
  pixD <- as.vector(stimDfl)

  # correlation of image halves
  corrUD <- cor(pixU,pixD)

  # final symmetry: horizontal (absolute correlation)
  sym_h <- abs(corrUD)

  ## -----------------------
  ##   return results
  ## -----------------------
  return(list(vertical = sym_v, horizontal = sym_h))
}

