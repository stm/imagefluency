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
                 "Negative pixel intensity values in your image\\. Corresponding pixels set to 0\\.")
})

test_that("quantify_contrast warns if input contains values > 255", {
  img <- matrix(256, nrow = 100, ncol = 100)
  expect_warning(quantify_contrast(img),
                 "Pixel intensity values > 255 in your image\\. Corresponding pixels set to 255\\.")
})

test_that("quantify_contrast gives results you'd expect", {
  img <- matrix(0, nrow = 100, ncol = 100)
  expect_equal(quantify_contrast(img), expected = 0)

  img <- matrix(rnorm(1000*1000) + 100, nrow = 1000, ncol = 1000)
  expect_equal(quantify_contrast(img), expected = 1/255, tolerance = .01, scale = 1)
})

