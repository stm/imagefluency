context("complexity")

test_that("quantify_complexity handles only *numeric* matrices or arrays", {
  expect_error(quantify_complexity(array(1, dim = c(10, 10, 2))),
               "Wrong array dimensions \\(has to be a 3-dimensional array\\)")
  expect_error(quantify_complexity(matrix("foo", nrow = 10, ncol = 10)),
               "Wrong type of input\\: has to be a filename \\(character string\\) or an image \\(array or matrix of numeric or integer values\\)")
  expect_error(quantify_complexity(1:10),
               "Wrong type of input\\: has to be a filename \\(character string\\) or an image \\(array or matrix of numeric or integer values\\)")
})

test_that("quantify_complexity warns if rotate is not a logical value", {
  expect_warning(quantify_complexity(matrix(1, 100, 100), rotate = "yes"),
                 "rotate = 'yes' is not a logical value \\(TRUE\\/FALSE\\)\\. Skipping rotation \\.\\.\\.")
})

# test_that("quantify_complexity works as intended", {
#   set.seed(2787)
#   img <- matrix(round(runif(200*200, min = 0, max = 255)), nrow = 100, ncol = 100)
#   results <- quantify_complexity(img)
#   expect_equal(results$original, 1198, tolerance = 100, scale = 1)
#   expect_equal(results$compressed, 581, tolerance = 100, scale = 1)
#   expect_equal(results$ratio, 0.484975, tolerance = 0.01, scale = 1)
#
#   img1 <- matrix(0, nrow = 100, ncol = 100)
#   img1[c(1:10, 21:30, 41:50, 61:70, 81:90), ] <- 1
#   img2 <- rotate90(img1, direction = "negative")
#   expect_lte(quantify_complexity(img1)$compressed, quantify_complexity(img2)$compressed)
#   expect_equal(quantify_complexity(img1)$compressed, quantify_complexity(img2, rotate = TRUE)$compressed, tolerance = 10, scale = 1)
# })
