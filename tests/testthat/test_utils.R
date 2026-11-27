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
  imgGray <- 0.299 * red + 0.587 * green + 0.114 * blue
  # check
  expect_equal(rgb2gray(imgRGB), imgGray)
})


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

test_that("rotate90 keeps the dimensions of images with one row or column", {
  expect_equal(rotate90(matrix(1:5, ncol = 1)), matrix(1:5, nrow = 1))
  expect_equal(rotate90(matrix(1:5, nrow = 1)), matrix(5:1, ncol = 1))
  expect_equal(rotate90(matrix(1:5, nrow = 1), direction = "negative"), matrix(1:5, ncol = 1))
  expect_equal(dim(rotate90(array(1:6, dim = c(1, 2, 3)))), c(2, 1, 3))
})

test_that("rotate90 only handles 3-dimensional arrays and a single direction", {
  expect_error(rotate90(array(1:16, dim = c(2, 2, 2, 2))),
               "Unknown input \\(has to be a matrix or a 3-dimensional array\\)")
  expect_error(rotate90(matrix(1:4, 2), direction = c("positive", "negative")),
               "'positive, negative' is an unknown input to parameter 'direction'")
})

test_that("img_read loads demo image without error", {
  expect_error({
    path <- system.file("example_images", "rails.jpg", package = "imagefluency")
    img_read(path)
  }, NA)
})


test_that(".prepare_img only accepts numeric matrices or 3-dimensional arrays", {
  msg_input <- "Invalid input \\(should be a matrix or a 3-dimensional array of numeric or integer values\\)"
  msg_array <- "Invalid array \\(should be a 3-dimensional array of numeric or integer values\\)"

  expect_error(.prepare_img(1:10), msg_input)
  expect_error(.prepare_img("image.jpg"), msg_input)
  expect_error(.prepare_img(as.data.frame(matrix(1:4, 2))), msg_input)
  expect_error(.prepare_img(matrix("foo", nrow = 2, ncol = 2)),
               "Input img has to be a matrix or array of \\*numeric\\* or \\*integer\\* values")
  expect_error(.prepare_img(array(1:24, dim = c(2, 2, 3, 2))), msg_array)
  expect_error(.prepare_img(array(1:20, dim = c(2, 2, 5))), msg_array)
  expect_error(.prepare_img(array("1", dim = c(2, 2, 3))), msg_array)
})

test_that(".prepare_img does not accept missing or non-finite values", {
  msg <- "Input image contains missing or non-finite values \\(NA, NaN, or Inf\\)"
  expect_error(.prepare_img(matrix(c(1, NA, 3, 4), 2)), msg)
  expect_error(.prepare_img(matrix(c(1, NaN, 3, 4), 2)), msg)
  expect_error(.prepare_img(array(c(1, Inf, 3), dim = c(2, 2, 3))), msg)
})

test_that(".prepare_img returns the image in a standard form", {
  # grayscale: matrix
  img <- matrix(1:4, 2)
  expect_equal(.prepare_img(img), list(img = img, type = "gray"))
  # grayscale: array with one layer
  expect_equal(.prepare_img(array(1:4, dim = c(2, 2, 1))), list(img = img, type = "gray"))
  # color
  img <- array(1:12, dim = c(2, 2, 3))
  expect_equal(.prepare_img(img), list(img = img, type = "rgb"))
})

test_that(".prepare_img removes a presumed alpha channel with a warning", {
  # color with alpha channel
  img <- array(1:16, dim = c(2, 2, 4))
  expect_warning(
    out <- .prepare_img(img),
    "Array with 4 dimensions, presumably with alpha channel\\. 4th dimension is ignored \\.\\.\\."
  )
  expect_equal(out, list(img = img[, , 1:3], type = "rgb"))

  # grayscale with alpha channel
  img <- array(1:8, dim = c(2, 2, 2))
  expect_warning(
    out <- .prepare_img(img),
    "Array with 2 dimensions, presumably grayscale with alpha channel\\. 2nd dimension is ignored \\.\\.\\."
  )
  expect_equal(out, list(img = matrix(1:4, 2), type = "gray"))
})

test_that(".check_flag returns the default with a warning for invalid values", {
  expect_true(.check_flag(TRUE, "foo"))
  expect_false(.check_flag(FALSE, "foo", default = TRUE))

  expect_warning(out <- .check_flag("yes", "foo"),
                 "foo = 'yes' is not a logical value \\(TRUE\\/FALSE\\)\\. Automatically set to FALSE \\.\\.\\.")
  expect_false(out)
  expect_warning(out <- .check_flag(NA, "foo", default = TRUE),
                 "foo = 'NA' is not a logical value \\(TRUE\\/FALSE\\)\\. Automatically set to TRUE \\.\\.\\.")
  expect_true(out)
  expect_warning(.check_flag(c(TRUE, FALSE), "foo", action = "Skipping ..."),
                 "foo = 'TRUE, FALSE' is not a logical value \\(TRUE\\/FALSE\\)\\. Skipping \\.\\.\\.")
})

test_that(".normalize_img handles edge cases and all normalization branches", {
  expect_error(
    .normalize_img(matrix(c(0, 1, Inf, 2), nrow = 2)),
    "Input image contains non-finite values\\."
  )

  img_01 <- matrix(c(0, 0.25, 0.5, 1), nrow = 2)
  expect_identical(.normalize_img(img_01), img_01)

  img_255 <- matrix(c(0, 64, 128, 255), nrow = 2)
  expect_equal(.normalize_img(img_255), img_255 / 255)

  img_const_negative <- matrix(-2, nrow = 2, ncol = 2)
  expect_equal(.normalize_img(img_const_negative), matrix(0, nrow = 2, ncol = 2))

  img_const_high <- matrix(300, nrow = 2, ncol = 2)
  expect_equal(.normalize_img(img_const_high), matrix(1, nrow = 2, ncol = 2))
})
