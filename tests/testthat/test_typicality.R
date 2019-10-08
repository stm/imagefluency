context("typicality")

test_that("img_typicality only handles lists of matrices with numeric values", {
  expect_error(img_typicality(matrix("foo", nrow = 10, ncol = 10)),
               "Input has to be a \\*list\\* of image matrices")
  expect_error(img_typicality(matrix(1, nrow = 10, ncol = 10)),
               "Input has to be a \\*list\\* of image matrices")
  expect_error(img_typicality(1:10),
               "Input has to be a \\*list\\* of image matrices")
  expect_error(img_typicality("image.bmp"),
               "Input has to be a \\*list\\* of image matrices")
})

test_that("img_typicality warns if only 1 image is in the list, returns NA", {
  expect_warning(x <- img_typicality(list(matrix(1, 10))),
                 "The function needs at least 2 images in the input list\\. Returning NA\\.")
  expect_equal(x, list(typicality = NA))
})


test_that("input is matrix or array, numeric or integer", {
  set.seed(2787)
  imgs <- replicate(3, matrix(runif(100, min = 0, max = 255), nrow = 10, ncol = 10),
                    simplify = FALSE)
  # not a matrix
  imgs[[2]] <- "I'm not a matrix"
  expect_error(img_typicality(imgs),
               "List element 2 has to be a \\*matrix\\* or a 3-dimensional \\*array\\* of numeric or integer values")

  # not numeric
  imgs[[2]] <- matrix(letters[round(stats::runif(100, min = 1, max = 26))]
                      , nrow = 10, ncol = 10)
  expect_error(img_typicality(imgs),
               "List element 2 has to be a matrix or a 3-dimensional array of \\*numeric\\* or \\*integer\\* values")

  # construct RGB image
  imgs <- replicate(3, array(runif(100*3, min = 0, max = 255), dim = c(100, 100, 3)),
                    simplify = FALSE)

  # RGB not numeric
  imgs[[2]] <- array("0", dim = c(100, 100, 3))
  expect_error(img_typicality(imgs),
               "List element 2 is an invalid array \\(should be a 3-dimensional array of numeric or integer values\\)")

  # RGB wrong dimensions of array
  imgs[[2]] <- array(0, dim = c(100, 100, 2))
  expect_error(img_typicality(imgs),
               "List element 2 is an invalid array \\(should be a 3-dimensional array of numeric or integer values\\)")
})


test_that("input presumably has alpha channel", {
  set.seed(2787)
  imgs <- replicate(3, array(runif(100*3, min = 0, max = 255), dim = c(100, 100, 3)),
                    simplify = FALSE)
  imgs[[2]] <- array(1:100, c(100, 100, 4))
  expect_warning(img_typicality(imgs),
                 "List element 2 is an array with 4 dimensions, presumably with alpha channel\\. 4th dimension is ignored \\.\\.\\.")
})


test_that("img_typicality checks whether parameter rescale is numeric and has OpenImageR installed", {
  expect_error(img_typicality(list(matrix(1, 10), matrix(1, 10)), rescale = TRUE),
               "parameter 'rescale' must be numeric")

  imgs <- replicate(3, array(0:255, dim = c(100, 100, 3)),
                    simplify = FALSE)

  # rescaling works if OpenImageR is installed
  expect_error(img_typicality(imgs, rescale = .5), NA)

  # OpenImageR package has to be installed
  mockery::stub(img_typicality, 'requireNamespace', FALSE)
  expect_error(img_typicality(imgs, rescale = .5),
               "Package \\'OpenImageR\\' is required for rescaling but not installed on your system\\.")
})


test_that("img_typicality gives results you'd expect", {
  set.seed(2787)
  imgs <- replicate(3, matrix(runif(100, min = 0, max = 255), nrow = 10, ncol = 10),
                    simplify = FALSE)
  imgs[[2]] <- -imgs[[1]]
  imgs[[3]] <- -imgs[[1]]
  results <- img_typicality(imgs)
  expect_equal(results[1], -results[2])

  imgs <- replicate(3, matrix(runif(100, min = 0, max = 255), nrow = 10, ncol = 10),
                    simplify = FALSE)
  imgs[[2]] <- imgs[[1]]
  results <- img_typicality(imgs)
  expect_equal(results[1], results[2])

  # RGB
  imgs <- replicate(3, array(runif(100*3, min = 0, max = 255), dim = c(100, 100, 3)),
                    simplify = FALSE)
  imgs[[2]] <- -imgs[[1]]
  results <- img_typicality(imgs)
  expect_equal(results[1], -results[2])

  imgs[[2]] <- imgs[[1]]
  results <- img_typicality(imgs)
  expect_equal(results[1], results[2])
})

test_that("Different image sizes are resized using OpenImageR", {
  imgs <- replicate(3, array(0:255, dim = c(100, 100, 3)),
                    simplify = FALSE)
  res1 <-  img_typicality(imgs)

  # different dimension for image 1
  imgs[[1]] <- array(0:255, c(150, 150, 3))
  res2 <- img_typicality(imgs)

  expect_false(isTRUE(all.equal(res1,res2)))


  # OpenImageR package has to be installed
  mockery::stub(img_typicality, 'requireNamespace', FALSE)
  expect_error(img_typicality(imgs),
               "Package \\'OpenImageR\\' is required but not installed on your system\\.")
})
