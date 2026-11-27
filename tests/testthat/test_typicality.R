test_that("img_typicality only handles lists of matrices with numeric values", {
  expect_error(img_typicality(matrix("foo", nrow = 10, ncol = 10)),
               "Input has to be a \\*list\\* of image matrices")
  expect_error(img_typicality(matrix(1, nrow = 10, ncol = 10)),
               "Input has to be a \\*list\\* of image matrices")
  expect_error(img_typicality(1:10),
               "Input has to be a \\*list\\* of image matrices")
  expect_error(img_typicality("image.bmp"),
               "Input has to be a \\*list\\* of image matrices")
})

test_that("img_typicality warns if only 1 image is in the list, returns NA", {
  expect_warning(x <- img_typicality(list(matrix(1, 10))),
                 "The function needs at least 2 images in the input list\\. Returning NA\\.")
  expect_equal(x, matrix(NA_real_, nrow = 1, ncol = 1, dimnames = list("img1", NULL)))
  expect_warning(x <- img_typicality(list(only = matrix(1, 10))),
                 "The function needs at least 2 images in the input list\\. Returning NA\\.")
  expect_equal(rownames(x), "only")
})


test_that("input is matrix or array, numeric or integer", {
  set.seed(2787)
  imgs <- replicate(3, matrix(runif(100, min = 0, max = 255), nrow = 10, ncol = 10),
                    simplify = FALSE)
  # not a matrix
  imgs[[2]] <- "I'm not a matrix"
  expect_error(img_typicality(imgs),
               "List element 2: Invalid input \\(should be a matrix or a 3-dimensional array of numeric or integer values\\)")

  # not numeric
  imgs[[2]] <- matrix(letters[round(stats::runif(100, min = 1, max = 26))]
                      , nrow = 10, ncol = 10)
  expect_error(img_typicality(imgs),
               "List element 2: Input img has to be a matrix or array of \\*numeric\\* or \\*integer\\* values")

  # construct RGB image
  imgs <- replicate(3, array(runif(100*3, min = 0, max = 255), dim = c(100, 100, 3)),
                    simplify = FALSE)

  # RGB not numeric
  imgs[[2]] <- array("0", dim = c(100, 100, 3))
  expect_error(img_typicality(imgs),
               "List element 2: Invalid array \\(should be a 3-dimensional array of numeric or integer values\\)")

  # RGB wrong dimensions of array
  imgs[[2]] <- array(0, dim = c(100, 100, 5))
  expect_error(img_typicality(imgs),
               "List element 2: Invalid array \\(should be a 3-dimensional array of numeric or integer values\\)")
})


test_that("input presumably has alpha channel", {
  set.seed(2787)
  imgs <- replicate(3, array(runif(100*3, min = 0, max = 255), dim = c(100, 100, 3)),
                    simplify = FALSE)
  imgs[[2]] <- array(1:100, c(100, 100, 4))
  expect_warning(img_typicality(imgs),
                 "List element 2: Array with 4 dimensions, presumably with alpha channel\\. 4th dimension is ignored \\.\\.\\.")
})


test_that("img_typicality checks whether parameter rescale is numeric and has OpenImageR installed", {
  expect_error(img_typicality(list(matrix(1, 10), matrix(1, 10)), rescale = TRUE),
               "parameter 'rescale' must be numeric")

  imgs <- replicate(3, array(0:255, dim = c(100, 100, 3)),
                    simplify = FALSE)

  # rescaling works if OpenImageR is installed
  expect_error(img_typicality(imgs, rescale = .5), NA)

  # OpenImageR package has to be installed
  mockery::stub(img_typicality, '.pkg_avail', FALSE)
  expect_error(img_typicality(imgs, rescale = .5),
               "Package \\'OpenImageR\\' is required for rescaling but not installed on your system\\.")
})


test_that("img_typicality gives results you'd expect", {
  set.seed(2787)
  imgs <- replicate(3, matrix(runif(100, min = 0, max = 255), nrow = 10, ncol = 10),
                    simplify = FALSE)
  imgs[[2]] <- -imgs[[1]]
  imgs[[3]] <- -imgs[[1]]
  results <- img_typicality(imgs)
  expect_equal(results[1], -results[2])

  imgs <- replicate(3, matrix(runif(100, min = 0, max = 255), nrow = 10, ncol = 10),
                    simplify = FALSE)
  imgs[[2]] <- imgs[[1]]
  results <- img_typicality(imgs)
  expect_equal(results[1], results[2])

  # RGB
  imgs <- replicate(3, array(runif(100*3, min = 0, max = 255), dim = c(100, 100, 3)),
                    simplify = FALSE)
  imgs[[2]] <- -imgs[[1]]
  results <- img_typicality(imgs)
  expect_equal(results[1], -results[2])

  imgs[[2]] <- imgs[[1]]
  results <- img_typicality(imgs)
  expect_equal(results[1], results[2])
})

test_that("Different image sizes are resized using OpenImageR", {
  imgs <- replicate(3, array(0:255, dim = c(100, 100, 3)),
                    simplify = FALSE)
  res1 <-  img_typicality(imgs)

  # different dimension for image 1
  imgs[[1]] <- array(0:255, c(150, 150, 3))
  res2 <- img_typicality(imgs)

  expect_false(isTRUE(all.equal(res1,res2)))


  # OpenImageR package has to be installed
  mockery::stub(img_typicality, '.pkg_avail', FALSE)
  expect_error(img_typicality(imgs),
               "Package \\'OpenImageR\\' is required but not installed on your system\\.")
})

test_that(".typ returns the correlation with the mean image", {
  set.seed(2787)
  imgs <- replicate(
    3,
    matrix(runif(100, min = 0, max = 255), nrow = 10, ncol = 10),
    simplify = FALSE
  )
  img_mean <- (imgs[[1]] + imgs[[2]] + imgs[[3]]) / 3

  out <- .typ(imgs)
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(3, 1))
  expect_equal(as.vector(out),
               vapply(imgs, function(x) cor(as.vector(x), as.vector(img_mean)), numeric(1)))
})

test_that("images are resized to the smallest height and width", {
  set.seed(2787)
  # non-square images with different image sizes
  imgs <- list(matrix(runif(40 * 60), nrow = 40, ncol = 60),
               matrix(runif(50 * 80), nrow = 50, ncol = 80),
               matrix(runif(40 * 60), nrow = 40, ncol = 60))
  # resized by hand to height (rows) = 40 and width (columns) = 60
  # (OpenImageR's 'width' is the number of rows, 'height' the number of columns)
  imgs_resized <- lapply(imgs, OpenImageR::resizeImage,
                         width = 40, height = 60, method = "bilinear")
  expect_equal(dim(imgs_resized[[2]]), c(40, 60))
  expect_equal(img_typicality(imgs), img_typicality(imgs_resized))

  # same for color images
  imgsRGB <- list(array(runif(40 * 60 * 3), dim = c(40, 60, 3)),
                  array(runif(50 * 80 * 3), dim = c(50, 80, 3)))
  imgsRGB_resized <- lapply(imgsRGB, OpenImageR::resizeImage,
                            width = 40, height = 60, method = "bilinear")
  expect_equal(img_typicality(imgsRGB), img_typicality(imgsRGB_resized))
})

test_that("rescale keeps the aspect ratio of the images", {
  set.seed(2787)
  imgs <- replicate(3, matrix(runif(40 * 60), nrow = 40, ncol = 60), simplify = FALSE)
  # rescaled by hand to height (rows) = 20 and width (columns) = 30
  imgs_rescaled <- lapply(imgs, OpenImageR::resizeImage,
                          width = 20, height = 30, method = "bilinear")
  expect_equal(img_typicality(imgs, rescale = 0.5), img_typicality(imgs_rescaled))
})

test_that("img_typicality checks the input list and the parameter rescale", {
  set.seed(2787)
  imgs <- replicate(3, matrix(runif(100), nrow = 10, ncol = 10), simplify = FALSE)

  expect_error(img_typicality(as.data.frame(imgs[[1]])),
               "Input has to be a \\*list\\* of image matrices")
  expect_error(img_typicality(list(imgs[[1]], as.data.frame(imgs[[2]]))),
               "List element 2: Invalid input")
  imgs_na <- imgs
  imgs_na[[3]][1, 1] <- NA
  expect_error(img_typicality(imgs_na),
               "List element 3: Input image contains missing or non-finite values")

  expect_error(img_typicality(imgs, rescale = c(0.5, 0.5)),
               "parameter 'rescale' must be a single positive number")
  expect_error(img_typicality(imgs, rescale = -1),
               "parameter 'rescale' must be a single positive number")
})

test_that("img_typicality returns a plain matrix named after the images", {
  set.seed(2787)
  imgs <- replicate(3, matrix(runif(100), nrow = 10, ncol = 10), simplify = FALSE)

  out <- img_typicality(imgs)
  expect_identical(class(out), c("matrix", "array"))
  expect_equal(dimnames(out), list(c("img1", "img2", "img3"), NULL))

  # names of the list are used
  names(imgs) <- c("first", "second", "third")
  expect_equal(rownames(img_typicality(imgs)), c("first", "second", "third"))
  names(imgs) <- c("first", "", NA)
  expect_equal(rownames(img_typicality(imgs)), c("first", "img2", "img3"))
  expect_equal(unname(img_typicality(imgs)), unname(out))
})

test_that("img_typicality handles lists with grayscale and color images", {
  set.seed(2787)
  img_gray <- matrix(runif(100), nrow = 10, ncol = 10)
  imgs <- list(img_gray,
               array(runif(300), dim = c(10, 10, 3)),
               array(runif(300), dim = c(10, 10, 3)))

  expect_warning(out <- img_typicality(imgs),
                 "The input list contains both grayscale and color images\\. Grayscale images are treated as color images with three identical color channels\\.")
  # same result as with the grayscale image converted by hand
  imgs[[1]] <- array(img_gray, dim = c(10, 10, 3))
  expect_equal(out, img_typicality(imgs))

  # grayscale images stored as arrays with one layer
  imgs_gray <- replicate(3, matrix(runif(100), nrow = 10, ncol = 10), simplify = FALSE)
  imgs_array <- lapply(imgs_gray, function(x) array(x, dim = c(10, 10, 1)))
  expect_equal(img_typicality(imgs_array), img_typicality(imgs_gray))
})
