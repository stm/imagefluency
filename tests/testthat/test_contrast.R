context("contrast")

test_that("img_contrast only handles numeric matrices", {
  expect_error(img_contrast(matrix("foo", nrow = 10, ncol = 10)),
               "Input img has to be a matrix or array of \\*numeric\\* or \\*integer\\* values")
  expect_error(img_contrast(1:10),
               "Invalid input \\(should be a matrix or a 3-dimensional array of numeric or integer values\\)")
})

test_that("img_contrast warns if input contains negative values", {
  img <- matrix(-1, nrow = 100, ncol = 100)
  expect_warning(img_contrast(img),
                 "Negative pixel intensity values in your image\\. Corresponding pixels set to 0\\.")
})

test_that("img_contrast warns if input contains values > 255", {
  img <- matrix(256, nrow = 100, ncol = 100)
  expect_warning(img_contrast(img),
                 "Pixel intensity values > 255 in your image\\. Corresponding pixels set to 255\\.")
})

test_that("img_contrast gives results you'd expect", {
  img <- matrix(0, nrow = 100, ncol = 100)
  expect_equal(img_contrast(img), expected = 0)

  img <- matrix(rnorm(1000*1000) + 100, nrow = 1000, ncol = 1000)
  expect_equal(img_contrast(img), expected = 1/255, tolerance = .01, scale = 1)
})

