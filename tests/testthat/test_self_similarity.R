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
  expect_warning(img_self_similarity(img, full = "yes", logplot = "yes"))
})

test_that("img_self_similarity checks whether image is at least 22 pixels in each dimensions, throws error otherwise", {
  img <- matrix(1, nrow = 21, ncol = 100)
  expect_error(img_self_similarity(img),
               "Image has to be at least 22 pixels in each dimension\\. Input image has 100 \\(width\\) x 21 \\(height\\)\\.")
})

test_that("img_self_similarity gives results you'd expect", {
  set.seed(2787)
  # result must be 0 or less than zero
  img <- matrix(runif(50*50, min = 0, max = 255), nrow = 50, ncol = 50)
  expect_lte(img_self_similarity(img), 0)

  #
  img <- matrix(0, nrow = 100, ncol = 100)
  img[, 1:50] <- 255
  expect_equal(img_self_similarity(img), expected = -1.05, tolerance = .025, scale = 1)
})

