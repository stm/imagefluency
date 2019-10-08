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

test_that("img_contrast gives results you'd expect (grayscale)", {
  set.seed(2787)
  img <- matrix(0, nrow = 100, ncol = 100)
  expect_equal(img_contrast(img), expected = 0)

  img <- matrix(rnorm(1000*1000) + 100, nrow = 1000, ncol = 1000)
  expect_equal(img_contrast(img), expected = 1/255, tolerance = .01, scale = 1)
})

test_that("img_contrast gives results you'd expect", {
  # RGB image
  red <- matrix(0, nrow = 20, ncol = 20)
  green <- matrix(100, nrow = 20, ncol = 20)
  blue <- matrix(255, nrow = 20, ncol = 20)
  imgRGB <- array(c(red, green, blue), dim = c(20, 20, 3))
  expect_equal(img_contrast(imgRGB), expected = 0)

  set.seed(2787)
  red <- matrix(c(rnorm(5*5)+50,rnorm(5*5)+200), nrow = 50, ncol = 50)
  green <- matrix(c(rnorm(5*5)+50,rnorm(5*5)+200), nrow = 50, ncol = 50)
  blue <- matrix(c(rnorm(5*5)+50,rnorm(5*5)+200), nrow = 50, ncol = 50)
  imgRGB <- array(c(red, green, blue), dim = c(50, 20, 3))
  expect_equal(img_contrast(imgRGB), expected = 0.2936195, tolerance = .0001, scale = 1)
})
