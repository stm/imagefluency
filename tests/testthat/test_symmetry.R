context("img_symmetry")

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


test_that("img_symmetry needs variance in the image",{
  img <- matrix(1:100, nrow = 10, ncol = 10)

  # no variation in left half
  img[,1:5]<-0
  expect_error(img_symmetry(img),
               "No variation in left image half\\. Computation not possible\\.")

  # no variation in left half
  img[,1:5]<-img[,6:10]
  img[,6:10]<-0
  expect_error(img_symmetry(img),
               "No variation in right image half\\. Computation not possible\\.")

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
               c(vertical = 0.2989 * 1 + 0.5870 * 1 + 0.1140 * 1,
               horizontal = 0.2989 * 1 + 0.5870 * 1 + 0.1140 * 1))
  expect_equal(img_symmetry(img1, horizontal = FALSE),
               c(vertical = 0.2989 * 1 + 0.5870 * 1 + 0.1140 * 1))
  expect_equal(img_symmetry(img1, vertical = FALSE),
               c(horizontal = 0.2989 * 1 + 0.5870 * 1 + 0.1140 * 1))
  expect_equal(img_symmetry(img1, per_channel = FALSE),
               c(vertical = 0.2989 * 1 + 0.5870 * 1 + 0.1140 * 1,
                 horizontal = 0.2989 * 1 + 0.5870 * 1 + 0.1140 * 1))
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

