test_that("img_symmetry checks all options to be correct", {
  expect_warning(img_symmetry(0, vertical = FALSE, horizontal = FALSE),
               "Both optional arguments cannot be FALSE\\. Try setting option \\'vertical\\' or \\'horizontal\\' to TRUE\\. Returning NA\\.")


  expect_error(img_symmetry(1:10),
               "Invalid input \\(should be a matrix or a 3-dimensional array of numeric or integer values\\)")
})



test_that("img_symmetry only handles numeric matrices", {
  expect_error(img_symmetry(matrix("foo", nrow = 10, ncol = 10)),
               "Input img has to be a matrix or array of \\*numeric\\* or \\*integer\\* values")
  expect_error(img_symmetry(1:10),
               "Invalid input \\(should be a matrix or a 3-dimensional array of numeric or integer values\\)")
})


test_that("img_symmetry needs at least 4x4 pixels",{
  img <- matrix(0, nrow = 3, ncol = 3)
  expect_error(img_symmetry(img),
               "Image too small\\. Try an image with at least 4 pixels in both image dimensions\\.")
})


test_that("img_symmetry returns NA with a warning if an image half has no variation",{
  img <- matrix(1:100, nrow = 10, ncol = 10)
  msg_vertical <- "No variation in at least one image half\\. Computation of vertical symmetry not possible\\. Returning NA\\."

    # no variation in left half
    img_left <- img
    img_left[, 1:5] <- 0
    expect_warning(result <- img_symmetry(img_left, horizontal = FALSE), msg_vertical)
    expect_identical(result, c(vertical = NA_real_))

    # no variation in right half
    img_right <- img
    img_right[, 6:10] <- 0
    expect_warning(result <- img_symmetry(img_right, horizontal = FALSE), msg_vertical)
    expect_identical(result, c(vertical = NA_real_))

    # no variation in upper half (horizontal symmetry); vertical symmetry is computed
    img_upper <- matrix(runif(100), nrow = 10, ncol = 10)
    img_upper[1:5, ] <- 0
    expect_warning(result <- img_symmetry(img_upper),
                   "No variation in at least one image half\\. Computation of horizontal symmetry not possible\\. Returning NA\\.")
    expect_false(is.na(result[["vertical"]]))
    expect_true(is.na(result[["horizontal"]]))

  # color image with a color channel without variation
  imgRGB <- array(0, dim = c(20, 20, 3))
  imgRGB[5:15, 3:9, 1] <- 1
  expect_warning(result <- img_symmetry(imgRGB, horizontal = FALSE),
                 "No variation in at least one image half of at least one color channel\\. Computation of vertical symmetry not possible\\. Returning NA\\.")
  expect_identical(result, c(vertical = NA_real_))
})


test_that("img_symmetry ignores positions of the mirror axis without variation in an image half", {
  set.seed(2787)
  # no variation in the left image half only if the mirror axis is not shifted
  img <- matrix(runif(50 * 50), nrow = 50, ncol = 50)
  img[, 1:24] <- 0
  expect_warning(result <- img_symmetry(img, horizontal = FALSE, shift_range = 0.1), NA)
  expect_false(is.na(result))
})


test_that("img_symmetry detects perfect symmetry", {
  # construct sample image
  # img1: perfectly symmetric image (both horiz. & vert.)
  img1 <- matrix(0, nrow = 100, ncol = 100)
  img1[41:60, 41:60] <- 1

  # img perfectly symmetric
  expect_equal(img_symmetry(img1), c(vertical = 1, horizontal = 1))
  expect_equal(img_symmetry(img1, horizontal = FALSE), c(vertical = 1))
  expect_equal(img_symmetry(img1, vertical = FALSE), c(horizontal = 1))

  # RGB
  img1 <- array(0, dim = c(9, 9, 3))
  img1[3:7, 3:7, ] <- 1
  expect_equal(img_symmetry(img1),
               c(vertical = 1,
               horizontal = 1))
  expect_equal(img_symmetry(img1, horizontal = FALSE),
               c(vertical = 1))
  expect_equal(img_symmetry(img1, vertical = FALSE),
               c(horizontal = 1))
  expect_equal(img_symmetry(img1, per_channel = FALSE),
               c(vertical = 1,
                 horizontal = 1))
})


test_that("img_symmetry detects horizontal symmetry", {
  # construct sample image
  # img2: horizontally symmetric image
  img2 <- matrix(0, nrow = 50, ncol = 50)
  img2[21:30, 16:30] <- 1
  img2[11:40, 31:40] <- 1

  # img perfectly horizontally symmetric
  expect_lt(img_symmetry(img2)[1], c(vertical = 1))
  expect_identical(img_symmetry(img2)[2], c(horizontal = 1))
})


test_that("img_symmetry detects vertical symmetry", {
  # construct sample image
  # img3: vertically symmetric image
  img3 <- matrix(0, nrow = 50, ncol = 50)
  img3[16:30, 21:30] <- 1
  img3[31:40, 11:40] <- 1

  # img perfectly vertically symmetric
  expect_identical(img_symmetry(img3)[1], c(vertical = 1))
  expect_lt(img_symmetry(img3)[2], c(horizontal = 1))
})


test_that("img_symmetry detects symmetry for inverted image", {
  # img1
  img1 <- matrix(0, nrow = 100, ncol = 100)
  img1[41:60, 41:60] <- 1

    # img2: inverse of image 1 (should have the same symmetry)
  img2 <- 1 - img1

  expect_equal(img_symmetry(img1), img_symmetry(img2))
})

test_that(".sym_mirror returns the absolute correlation of the image halves", {
  img <- matrix(c(1, 5, 2, 8, 3, 9, 4, 7, 6, 2, 9, 1), nrow = 2)
  expect_equal(.sym_mirror(img), abs(cor(as.vector(img[, 1:3]), as.vector(img[, 6:4]))))

  # no variation in an image half
  img[, 1:3] <- 0
  expect_identical(.sym_mirror(img), NA_real_)
})

test_that("img_symmetry checks the options", {
  img <- matrix(0, nrow = 20, ncol = 20)
  img[5:15, 8:13] <- 1

  msg_shift <- "'shift_range' has to be a single number between 0 and 1"
  expect_error(img_symmetry(img, shift_range = -0.1), msg_shift)
  expect_error(img_symmetry(img, shift_range = 1), msg_shift)
  expect_error(img_symmetry(img, shift_range = "a"), msg_shift)
  expect_error(img_symmetry(img, shift_range = c(0.05, 0.1)), msg_shift)

  expect_warning(result <- img_symmetry(img, vertical = NA),
                 "vertical = 'NA' is not a logical value \\(TRUE\\/FALSE\\)\\. Automatically set to TRUE \\.\\.\\.")
  expect_equal(result, img_symmetry(img))
  expect_warning(img_symmetry(img, horizontal = "no"),
                 "horizontal = 'no' is not a logical value \\(TRUE\\/FALSE\\)\\. Automatically set to TRUE \\.\\.\\.")
  expect_warning(img_symmetry(array(img, dim = c(20, 20, 3)), horizontal = FALSE, per_channel = "no"),
                 "per_channel = 'no' is not a logical value \\(TRUE\\/FALSE\\)\\. Automatically set to TRUE \\.\\.\\.")

  expect_error(img_symmetry(as.data.frame(img)),
               "Invalid input \\(should be a matrix or a 3-dimensional array of numeric or integer values\\)")
})
