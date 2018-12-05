context("complexity")

test_that("img_complexity handles only *numeric* matrices or arrays", {
  expect_error(img_complexity(matrix("foo", nrow = 10, ncol = 10)),
               "Wrong type of input\\: has to be a filename \\(character string\\) or an image \\(3-dimensional array of numeric or integer values\\)")
  expect_error(img_complexity(1:10),
               "Wrong type of input\\: has to be a filename \\(character string\\) or an image \\(3-dimensional array of numeric or integer values\\)")
})

test_that("img_complexity warns if rotate is not a logical value", {
  expect_warning(img_complexity(array(0, dim = c(10, 10, 3)), rotate = "yes"),
                 "rotate = 'yes' is not a logical value \\(TRUE\\/FALSE\\)\\. Skipping rotation \\.\\.\\.")
})

test_that("img_complexity works as intended (gives correct / consistent results)", {
  set.seed(2787)
  redChannel <- matrix(round(runif(250*250, min = 0, max = 1)), nrow = 250, ncol = 250)
  greenChannel <- matrix(round(runif(250*250, min = 0, max = 1)), nrow = 250, ncol = 250)
  blueChannel <- matrix(round(runif(250*250, min = 0, max = 1)), nrow = 250, ncol = 250)
  img <- array(c(redChannel, greenChannel, blueChannel), dim = c(dim(redChannel), 3))
  # magick::image_read(img)
  results <- img_complexity(img)

  expect_equal(results, 0.1600421, tolerance = 0.01, scale = 1)


  redChannel <- matrix(0, nrow = 300, ncol = 300)
  redChannel[101:300, ] <- 1
  greenChannel <- matrix(0, nrow = 300, ncol = 300)
  greenChannel[201:300, ] <- 1
  blueChannel <- matrix(0, nrow = 300, ncol = 300)
  img <- array(c(redChannel, greenChannel, blueChannel), dim = c(dim(redChannel), 3))
  # magick::image_read(img)
  results <- img_complexity(img)

  expect_equal(results, 0.001673219, tolerance = 0.001, scale = 1)
})

context("simplicity")

test_that("img_simplicity complements img_complexity", {
  set.seed(2787)
  redChannel <- matrix(round(runif(250*250, min = 0, max = 1)), nrow = 250, ncol = 250)
  greenChannel <- matrix(round(runif(250*250, min = 0, max = 1)), nrow = 250, ncol = 250)
  blueChannel <- matrix(round(runif(250*250, min = 0, max = 1)), nrow = 250, ncol = 250)
  img <- array(c(redChannel, greenChannel, blueChannel), dim = c(dim(redChannel), 3))
  results <- img_simplicity(img)

  expect_equal(results, 1-0.1600421, tolerance = 0.01, scale = 1)
})


