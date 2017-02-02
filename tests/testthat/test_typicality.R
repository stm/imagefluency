context("typicality")

test_that("quantify_typicality only handles lists of matrices with numeric values", {
  expect_error(quantify_typicality(matrix("foo", nrow = 10, ncol = 10)),
               "Input has to be a \\*list\\* of image matrices")
  expect_error(quantify_typicality(matrix(1, nrow = 10, ncol = 10)),
               "Input has to be a \\*list\\* of image matrices")
  expect_error(quantify_typicality(1:10),
               "Input has to be a \\*list\\* of image matrices")
  expect_error(quantify_typicality("image.bmp"),
               "Input has to be a \\*list\\* of image matrices")
})

test_that("quantify_typicality warns if only 1 image is in the list, returns NA", {
  expect_warning(x <- quantify_typicality(list(matrix(1, 10))),
                 "The function needs at least 2 images in the input list\\. Returning NA\\.")
  expect_equal(x, list(typicality = NA))
})


test_that("quantify_typicality checks whether parameter rescale is numeric", {
  expect_error(quantify_typicality(list(matrix(1, 10), matrix(1, 10)), rescale = TRUE),
               "parameter 'rescale' must be numeric")
})


test_that("quantify_typicality gives results you'd expect", {
  imgs <- replicate(3, matrix(runif(100, min = 0, max = 255), nrow = 10, ncol = 10),
                    simplify = FALSE)
  imgs[[2]] <- -imgs[[1]]
  imgs[[3]] <- -imgs[[1]]
  results <- quantify_typicality(imgs)
  expect_equal(results[1], -results[2])

  imgs <- replicate(3, matrix(runif(100, min = 0, max = 255), nrow = 10, ncol = 10),
                    simplify = FALSE)
  imgs[[2]] <- imgs[[1]]
  results <- quantify_typicality(imgs)
  expect_equal(results[1], results[2])
})

