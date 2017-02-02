context("self_similarity")

test_that("quantify_self_similarity only handles numeric matrices", {
  expect_error(quantify_self_similarity(matrix("foo", nrow = 10, ncol = 10)),
               "Input img has to be a matrix of \\*numeric\\* or \\*integer\\* values")
  expect_error(quantify_self_similarity(1:10),
               "Input img has to be a \\*matrix\\* of numeric or integer values")
})

test_that("quantify_self_similarity checks whether optional parameter are logical", {
  img <- matrix(runif(100*100, min = 0, max = 255), nrow = 100, ncol = 100)
  expect_warning(quantify_self_similarity(img, full = "yes"),
                 "full = 'yes' is not a logical value \\(TRUE\\/FALSE\\)\\. Automatically set to FALSE \\.\\.\\.")
  expect_warning(quantify_self_similarity(img, logplot = "yes"),
                 "logplot = 'yes' is not a logical value \\(TRUE\\/FALSE\\)\\. Skipping log-log plot \\.\\.\\.")
  expect_warning(quantify_self_similarity(img, full = "yes", logplot = "yes"))
})

test_that("quantify_self_similarity checks whether image is at least 22 pixels in each dimensions, throws error otherwise", {
  img <- matrix(1, nrow = 21, ncol = 100)
  expect_error(quantify_self_similarity(img),
               "Image has to be at least 22 pixels in each dimension\\. Input image has 100 \\(width\\) x 21 \\(height\\)\\.")
})

test_that("quantify_self_similarity gives results you'd expect", {
  # result must be 0 or less than zero
  img <- matrix(runif(50*50, min = 0, max = 255), nrow = 50, ncol = 50)
  expect_lte(quantify_self_similarity(img), 0)

  #
  img <- matrix(0, nrow = 100, ncol = 100)
  img[, 1:50] <- 255
  expect_equal(quantify_self_similarity(img), expected = -1.047962, tolerance = .00001, scale = 1)
})

