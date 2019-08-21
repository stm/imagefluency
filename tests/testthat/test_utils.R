context("rgb2gray")

test_that("rgb2gray only handles numeric arrays", {
  expect_error(rgb2gray(matrix("foo", nrow = 10, ncol = 10)),
               "Invalid input \\(should be a 3-dimensional array of numeric or integer values\\)")
  expect_error(rgb2gray(1:10),
               "Invalid input \\(should be a 3-dimensional array of numeric or integer values\\)")
  expect_error(rgb2gray(array(c("1", "2", "3"), dim = c(10, 10, 3))),
               "Invalid input \\(should be a 3-dimensional array of numeric or integer values\\)")
})

test_that("rgb2gray warns that alpha channel is ignored", {
  expect_warning(rgb2gray(array(c(1, 2, 3), dim = c(10, 10, 4))),
                 "Array with 4 dimensions, presumably with alpha channel\\. 4th dimension is ignored \\.\\.\\.")
})

test_that("rgb2gray correctly converts RGB values to Gray", {
  # RGB image
  red <- matrix(0, nrow = 20, ncol = 20)
  green <- matrix(100, nrow = 20, ncol = 20)
  blue <- matrix(255, nrow = 20, ncol = 20)
  imgRGB <- array(c(red, green, blue), dim = c(20, 20, 3))
  # resulting grayscale image
  imgGray <- 0.2989 * red + 0.5870 * green + 0.1140 * blue
  # check
  expect_equal(rgb2gray(imgRGB), imgGray)
})


context("rotate90")

test_that("rotate90 only handles arrays or matrices", {
  expect_error(rotate90(1:10),
               "Unknown input of type 'integer' \\(has to be of type 'matrix' or 'array'\\)")
  expect_error(rotate90(c("a", "b", "c")),
               "Unknown input of type 'character' \\(has to be of type 'matrix' or 'array'\\)")
  expect_error(rotate90(cbind.data.frame(1:10, 1:10, "A")),
               "Unknown input of type 'data.frame' \\(has to be of type 'matrix' or 'array'\\)")
})

test_that("rotate90 only knows direction parameters 'positive' and 'negative'", {
  expect_error(rotate90(matrix(1:4, 2), direction = "right"),
               "'right' is an unknown input to parameter 'direction'\\. Try 'direction = positive' or 'direction = negative'\\.")
  expect_error(rotate90(matrix(1:4, 2), direction = "left"),
               "'left' is an unknown input to parameter 'direction'\\. Try 'direction = positive' or 'direction = negative'\\.")
})

test_that("rotate90 correctly rotates clockwise and counterclockwise", {
  # sample matrix
  mat <- matrix(1:4, ncol = 2)
  # sample array
  arr <- array(c(matrix(1:4, 2), matrix(5:8, 2), matrix(9:12, 2)), dim = c(2, 2, 3))

  # rotated matrix: counterclockwise (default)
  expect_equal(rotate90(mat), matrix(c(3, 1, 4, 2), ncol = 2))
  expect_equal(rotate90(mat, direction = "positive"), matrix(c(3, 1, 4, 2), ncol = 2))
  expect_equal(rotate90(mat, direction = "counterclockwise"), matrix(c(3, 1, 4, 2), ncol = 2))
  # rotated matrix: clockwise
  expect_equal(rotate90(mat, direction = "negative"), matrix(c(2, 4, 1, 3), ncol = 2))
  expect_equal(rotate90(mat, direction = "clockwise"), matrix(c(2, 4, 1, 3), ncol = 2))

  # rotated array
  expect_equal(rotate90(arr), array(c(
    rotate90(matrix(1:4, 2)), rotate90(matrix(5:8, 2)), rotate90(matrix(9:12, 2)))
    , dim = c(2, 2, 3)))
})

context("img_read")

test_that("img_read loads demo image without error", {
  expect_error({
    path <- system.file("example_images", "rails.jpg", package = "imagefluency")
    img_read(path)
  }, NA)
})


context(".check_input")

test_that(".check_input gives results only for known function calls", {
  img <- matrix(1:4, 2)
  expect_error(
    .check_input(img),
    "You have to specify a function for the f_call argument\\."
  )
  expect_error(
    .check_input(img, f_call = "foo"),
    "unknown input to f_call argument"
  )
})



test_that(".check_input returns error for non 3-dimensional arrays", {
  img <- array(1:24, dim = c(2, 2, 3, 2))
  expect_error(
    .check_input(img, f_call = "contrast"),
    "Invalid array \\(should be a 3-dimensional array of numeric or integer values\\)"
  )
})

test_that(".check_input warns if presumed alpha channel is present", {
  img <- array(1:16, dim = c(2, 2, 4))
  expect_warning(
    .check_input(img, f_call = "contrast"),
    "Array with 4 dimensions, presumably with alpha channel\\. 4th dimension is ignored \\.\\.\\."
  )
})


test_that(".check_input returns rgb for arrays", {
  img <- array(c(matrix(1:4, 2), matrix(5:8, 2), matrix(9:12, 2)), dim = c(2, 2, 3))
  expect_equal(
    .check_input(img, f_call="contrast"),
    "rgb"
    )
})
