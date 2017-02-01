context("contrast")

test_that("quantify_contrast only handles numeric matrices", {
  expect_error(quantify_contrast(matrix("foo", nrow = 10, ncol = 10)),
               "Input img has to be a matrix of \\*numeric\\* or \\*integer\\* values")
  expect_error(quantify_contrast(1:10),
               "Input img has to be a \\*matrix\\* of numeric or integer values")
})

test_that("quantify_contrast warns if input contains negative values", {
  img <- matrix(-1, nrow = 100, ncol = 100)
  expect_warning(quantify_contrast(img),
                 "Negative pixel intensity values in your image\\. Result might not be meaningful\\.")
})

test_that("quantify_contrast warns if image is presumably already normalized", {
  img <- matrix(runif(100, min = 0, max = 1), nrow = 10, ncol = 10)
  expect_warning(quantify_contrast(img),
                 "Input image might already be normalized \\(all pixel intensity values between \\[0, 1\\]\\)\\. Consider turning option 'normalize' to FALSE\\.")
})

test_that("quantify_contrast checks whether parameter normalize is logical", {
  img <- matrix(runif(100, min = 0, max = 255), nrow = 10, ncol = 10)
  expect_error(quantify_contrast(img, normalize = "all"),
               "parameter 'normalize' has to be a logical value \\(TRUE/FALSE\\)")
})

test_that("quantify_contrast gives results you'd expect", {
  img <- matrix(0, nrow = 100, ncol = 100)
  expect_equal(quantify_contrast(img, normalize = FALSE), expected = list(contrast = 0))

  img <- matrix(rnorm(1000*1000) + 100, nrow = 1000, ncol = 1000)
  expect_equal(quantify_contrast(img, normalize = FALSE), expected = list(contrast = 1), tolerance = .01, scale = 1)
})

