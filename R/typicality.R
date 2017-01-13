## -----------------------
##      typicality
## -----------------------
# visual typicality is the correlation of a particular image with the average
# representation, i.e. the mean of all images
#
# NOTE: The function takes a list of grayscale images as argument.

quantify_typicality <- function(imglist, rescale = 1){
  # original resolution or different scaling level?
  if (rescale != 1) {
    # image dimensions of first element (assumes the same for all elements)
    img_h <- dim(imglist[[1]])[1] # image height
    img_w <- dim(imglist[[1]])[2] # image width
    imglist <- lapply(imglist, OpenImageR::resizeImage,
                      width = rescale*img_w,
                      height = rescale*img_h,
                      method = "bilinear")
  }
  # create matrix of vectorized intensity values
  imglist <- matrix(unlist(imglist), ncol = length(imglist), byrow = FALSE)
  img_mean <- rowMeans(imglist)
  return(list(typicality = cor(imglist, img_mean)))
}

