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


test_that("img_complexity checks whether algorithm is known", {
  expect_error(img_complexity(array(0, dim = c(10, 10, 3)), algorithm = "foo"),
               "Unknown image compression algorithm requested\\. Use one of the following: zip, jpg, gif, png")

  expect_error(img_complexity(array(0, dim = c(10, 10, 3)), algorithm = "zip", rotate = TRUE), NA)
  expect_error(img_complexity(array(0, dim = c(10, 10, 3)), algorithm = "jpg", rotate = TRUE), NA)
  expect_error(img_complexity(array(0, dim = c(10, 10, 3)), algorithm = "gif", rotate = TRUE), NA)
  expect_error(img_complexity(array(0, dim = c(10, 10, 3)), algorithm = "png", rotate = TRUE), NA)
})


test_that("img_complexity checks and handles file names as input", {
  expect_error(img_complexity(c("foo", "bar")),
               "Multiple filenames\\. Function can only handle one image at a time\\.")

  img_file <- system.file("example_images", "sky.jpg", package = "imagefluency")
  expect_equal(img_complexity(img_file), img_complexity(img_read(img_file)), tolerance = .1, scale = 1)
})

test_that("img_complexity normalizes input image if necessary", {
  expect_equal(img_complexity(array(100:500, dim = c(50, 50, 3))), 0.1111398, tolerance = 0.01, scale = 1)
})


test_that("img_complexity needs packge magick", {
  mockery::stub(img_complexity, 'requireNamespace', FALSE)
  expect_error(img_complexity(array(0, dim = c(10, 10, 3))),
               "Package \\'magick\\' not found but needed\\. Please install the package first\\.")
})

test_that("img_complexity throws error if image cannot be loaded",{
  expect_error(img_complexity("foo.jpg"),
               "File not found or invalid path \\(could not resolve \\'foo\\.jpg\\'\\)")

  expect_error(img_complexity(array(0, dim = c(10, 10, 5))),
               "File not found or invalid path \\(could not resolve input\\)")
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

  expect_equal(results, 0.001673219, tolerance = 0.01, scale = 1)
})


test_that(".compl checks for package R.utils", {
  mockery::stub(.compl, 'requireNamespace', FALSE)
  expect_error(.compl(magick::image_read(array(0, dim = c(10, 10, 3))), algorithm = "zip", rotate = FALSE),
               "Package \\'R.utils\\' not found but needed\\. Please install the package first\\.")
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


