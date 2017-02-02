context("complexity")

test_that("quantify_complexity handles only *numeric* matrices or arrays", {
  expect_error(quantify_complexity(array(1, dim = c(10, 10, 2))),
               "Wrong array dimensions \\(has to be a 3-dimensional array\\)")
  expect_error(quantify_complexity(matrix("foo", nrow = 10, ncol = 10)),
               "Wrong type of input\\: has to be a filename \\(character string\\) or an image \\(3-dimensional array of numeric or integer values\\)")
  expect_error(quantify_complexity(1:10),
               "Wrong type of input\\: has to be a filename \\(character string\\) or an image \\(3-dimensional array of numeric or integer values\\)")
})

test_that("quantify_complexity warns if rotate is not a logical value", {
  expect_warning(quantify_complexity(array(0, dim = c(10, 10, 3)), rotate = "yes"),
                 "rotate = 'yes' is not a logical value \\(TRUE\\/FALSE\\)\\. Skipping rotation \\.\\.\\.")
})

test_that("quantify_complexity warns if input is not a bmp image", {
  expect_warning(quantify_complexity(array(0, dim = c(10, 10, 3))),
                 "Input image might not be uncompressed\\. Interpret results with caution\\.")
})


test_that("quantify_complexity works as intended", {
  set.seed(2787)
  redChannel <- matrix(round(runif(250*250, min = 0, max = 1)), nrow = 250, ncol = 250)
  greenChannel <- matrix(round(runif(250*250, min = 0, max = 1)), nrow = 250, ncol = 250)
  blueChannel <- matrix(round(runif(250*250, min = 0, max = 1)), nrow = 250, ncol = 250)
  img <- array(c(redChannel, greenChannel, blueChannel), dim = c(dim(redChannel), 3))
  # magick::image_read(img)
  expect_warning(results <- quantify_complexity(img),
                 "Input image might not be uncompressed\\. Interpret results with caution\\.")

  expect_equal(results$compressed, 28990, tolerance = 100, scale = 1)
  expect_equal(results$original, 188138, tolerance = 100, scale = 1)
  expect_equal(results$ratio, 0.154089, tolerance = 0.01, scale = 1)


  redChannel <- matrix(0, nrow = 300, ncol = 300)
  redChannel[101:300, ] <- 1
  greenChannel <- matrix(0, nrow = 300, ncol = 300)
  greenChannel[201:300, ] <- 1
  blueChannel <- matrix(0, nrow = 300, ncol = 300)
  img <- array(c(redChannel, greenChannel, blueChannel), dim = c(dim(redChannel), 3))
  # magick::image_read(img)
  expect_warning(results <- quantify_complexity(img),
                 "Input image might not be uncompressed\\. Interpret results with caution\\.")

  expect_equal(results$compressed, 552, tolerance = 10, scale = 1)
  expect_equal(results$original, 270138, tolerance = 100, scale = 1)
  expect_equal(results$ratio, 0.0020434, tolerance = 0.001, scale = 1)
})
