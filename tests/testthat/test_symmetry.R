context("quantify_symmetry")

test_that("quantify_symmetry detects perfect symmetry", {
  # construct sample image
  # img1: perfectly symmetric image (both horiz. & vert.)
  img1 <- matrix(0, nrow = 100, ncol = 100)
  img1[41:60, 41:60] <- 1

  # img perfectly symmetric
  expect_equal(quantify_symmetry(img1)$horizontal, 1)
  expect_equal(quantify_symmetry(img1)$vertical, 1)
})

test_that("quantify_symmetry detects horizontal symmetry", {
  # construct sample image
  # img2: horizontally symmetric image
  img2 <- matrix(0, nrow = 50, ncol = 50)
  img2[21:30, 16:30] <- 1
  img2[11:40, 31:40] <- 1

  # img perfectly horizontally symmetric
  expect_identical(quantify_symmetry(img2)$horizontal, 1)
  expect_lt(quantify_symmetry(img2)$vertical, 1)
})

test_that("quantify_symmetry detects vertical symmetry", {
  # construct sample image
  # img3: vertically symmetric image
  img3 <- matrix(0, nrow = 50, ncol = 50)
  img3[16:30, 21:30] <- 1
  img3[31:40, 11:40] <- 1

  # img perfectly vertically symmetric
  expect_identical(quantify_symmetry(img3)$vertical, 1)
  expect_lt(quantify_symmetry(img3)$horizontal, 1)
})

test_that("quantify_symmetry detects symmetry for inverted image", {
  # img1
  img1 <- matrix(0, nrow = 100, ncol = 100)
  img1[41:60, 41:60] <- 1

    # img2: inverse of image 1 (should have the same symmetry)
  img2 <- 1 - img1

  expect_equal(quantify_symmetry(img1), quantify_symmetry(img2))
})

test_that("quantify_symmetry only handles numeric matrices", {
  expect_error(quantify_symmetry(matrix("foo", nrow = 10, ncol = 10)),
               "Input img has to be a matrix of \\*numeric\\* or \\*integer\\* values")
  expect_error(quantify_symmetry(1:10),
               "Input img has to be a \\*matrix\\* of numeric or integer values")
})
