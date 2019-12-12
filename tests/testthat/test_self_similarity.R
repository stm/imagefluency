context("self_similarity")

test_that("img_self_similarity only handles numeric matrices", {
  expect_error(img_self_similarity(matrix("foo", nrow = 10, ncol = 10)),
               "Input img has to be a matrix or array of \\*numeric\\* or \\*integer\\* values")
  expect_error(img_self_similarity(1:10),
               "Invalid input \\(should be a matrix or a 3-dimensional array of numeric or integer values\\)")
})

test_that("img_self_similarity checks whether optional parameter are logical", {
  set.seed(2787)
  img <- matrix(runif(100*100, min = 0, max = 255), nrow = 100, ncol = 100)
  expect_warning(img_self_similarity(img, full = "yes"),
                 "full = 'yes' is not a logical value \\(TRUE\\/FALSE\\)\\. Automatically set to FALSE \\.\\.\\.")
  expect_warning(img_self_similarity(img, logplot = "yes"),
                 "logplot = 'yes' is not a logical value \\(TRUE\\/FALSE\\)\\. Skipping log-log plot \\.\\.\\.")
  expect_warning(img_self_similarity(img, raw = "yes"),
                 "raw = 'yes' is not a logical value \\(TRUE\\/FALSE\\)\\. Automatically set to FALSE \\.\\.\\.")
  expect_warning(img_self_similarity(img, full = "yes", logplot = "yes", raw = 1))
})

test_that("img_self_similarity checks whether image is at least 22 pixels in each dimensions, throws error otherwise", {
  img <- matrix(1, nrow = 21, ncol = 100)
  expect_error(img_self_similarity(img),
               "Image has to be at least 22 pixels in each dimension\\. Input image has 100 \\(width\\) x 21 \\(height\\)\\.")
})

test_that("img_self_similarity gives results you'd expect (grayscale)", {
  set.seed(2787)
  # result must be 0 or less than zero
  img <- matrix(runif(599*599, min = 0, max = 255), nrow = 599, ncol = 599)
  expect_lte(img_self_similarity(img), 0)
})

test_that("img_self_similarity gives results you'd expect", {
  set.seed(2787)
  # result must be 0 or less than zero
  red <- matrix(runif(50*50, min = 0, max = 255), nrow = 50, ncol = 50)
  green <- matrix(runif(50*50, min = 0, max = 255), nrow = 50, ncol = 50)
  blue <- matrix(runif(50*50, min = 0, max = 255), nrow = 50, ncol = 50)
  imgRGB <- array(c(red, green, blue), dim = c(50, 50, 3))
  expect_lte(img_self_similarity(imgRGB), 0)

  #
  imgRGB <- array(0, dim = c(100, 100, 3))
  imgRGB[1:50, 1:50, ] <- 255
  expect_equal(img_self_similarity(imgRGB), expected = -0.5552913, tolerance = .00001, scale = 1)
})


test_that("img_self_similarity plots only grayscale images so far", {
  imgRGB <- array(runif(100*100*3, min = 0, max = 255), dim = c(100, 100, 3))
  expect_warning(img_self_similarity(imgRGB, logplot = TRUE),
                 "Plot option currently only supports grayscale images\\. No plot is shown\\.")
})


test_that("img_self_similarity logplot works as intended
          (i.e., plots something without any errors)", {
  set.seed(2787)
  img <- matrix(runif(50*50, min = 0, max = 255), nrow = 50, ncol = 50)

  # should return no errors when plotting succeeds
  #
  # use case: all packages are installed (i.e., ggplot2 is used
  expect_silent(img_self_similarity(img, logplot = TRUE))
  expect_error(img_self_similarity(img, logplot = TRUE), NA)

  # use case: base plot is used (requireNamespace returns false)
  mockery::stub(img_self_similarity, 'requireNamespace', FALSE)
  expect_error(img_self_similarity(img, logplot = TRUE), NA)
})

test_that("Non-squared image is resized using OpenImageR", {
  set.seed(2787)
  # non-squared but even sizes
  img <- matrix(runif(50*48, min = 0, max = 255), nrow = 50, ncol = 48)
  expect_equal(img_self_similarity(img), -1.558845, tolerance = .00001, scale = 1)

  # non-squared and odd sizes
  img <- matrix(runif(50*49, min = 0, max = 255), nrow = 50, ncol = 49)
  expect_equal(img_self_similarity(img), -0.8992694, tolerance = .00001, scale = 1)

  # OpenImageR package has to be installed
  mockery::stub(.selfsim, 'requireNamespace', FALSE)
  expect_error(.selfsim(img, full = FALSE, logplot = FALSE, raw=FALSE),
               "Package \\'OpenImageR\\' is required but not installed on your system\\.")
})

test_that("Image size >= 22 pix", {
  set.seed(2787)
  img <- matrix(runif(20*20, min = 0, max = 255), nrow = 20, ncol = 20)
  expect_error(img_self_similarity(img),
               "Image has to be at least 22 pixels in each dimension\\. Input image has 20 \\(width\\) x 20 \\(height\\)\\.")
})

test_that("Get raw log-log slope", {
  set.seed(2787)
  img <- matrix(runif(50*50, min = 0, max = 255), nrow = 50, ncol = 50)
  expect_equal(img_self_similarity(img, raw = FALSE), -2 - img_self_similarity(img, raw = TRUE))
})


test_that("Option to compute sel-similarity on full range", {
  set.seed(2787)
  img <- matrix(runif(50*50, min = 0, max = 255), nrow = 50, ncol = 50)
  expect_equal(img_self_similarity(img, full = TRUE), -2.135299, tolerance = .00001, scale = 1)
})
