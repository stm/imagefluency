example_image <- function(name) system.file("example_images", name, package = "imagefluency")

test_that("img_fluency returns the scores of the individual functions", {
  files <- example_image(c("rails.jpg", "sky.jpg", "trees.jpg"))
  imgs <- lapply(files, img_read)

  results <- img_fluency(files, progress = FALSE)

  expect_s3_class(results, "data.frame")
  expect_equal(names(results),
               c("filename", "contrast", "self_similarity", "simplicity", "symmetry"))
  expect_equal(results$filename, c("rails.jpg", "sky.jpg", "trees.jpg"))
  expect_equal(results$contrast, vapply(imgs, img_contrast, numeric(1)))
  expect_equal(results$self_similarity, vapply(imgs, img_self_similarity, numeric(1)))
  expect_equal(results$simplicity, vapply(imgs, img_simplicity, numeric(1)))
  expect_equal(results$symmetry,
               vapply(imgs, function(x) unname(img_symmetry(x, horizontal = FALSE)), numeric(1)))
})

test_that("img_fluency only computes the requested metrics", {
  files <- example_image(c("rails.jpg", "sky.jpg", "trees.jpg"))

  # columns are always in the same order
  results <- img_fluency(files, metrics = c("symmetry", "contrast"), progress = FALSE)
  expect_equal(names(results), c("filename", "contrast", "symmetry"))

  # typicality is computed relative to all images
  results <- img_fluency(files, metrics = "typicality", progress = FALSE)
  expect_equal(names(results), c("filename", "typicality"))
  expect_equal(results$typicality, as.vector(img_typicality(lapply(files, img_read))))

  expect_error(img_fluency(files, metrics = c("contrast", "foo")),
               "Unknown input to parameter 'metrics'\\. Use one or more of the following: contrast, self_similarity, simplicity, symmetry, typicality")
  expect_error(img_fluency(files, metrics = character(0)),
               "Unknown input to parameter 'metrics'")
})

test_that("img_fluency handles a list of pre-loaded images", {
  set.seed(2787)
  imgs <- list(first = matrix(runif(50 * 50), nrow = 50, ncol = 50),
               array(runif(50 * 50 * 3), dim = c(50, 50, 3)))

  results <- img_fluency(imgs, metrics = c("contrast", "symmetry"), progress = FALSE)
  expect_equal(results$filename, c("first", "img2"))
  expect_equal(results$contrast, c(img_contrast(imgs[[1]]), img_contrast(imgs[[2]])))

  # conversion into grayscale
  results_gray <- img_fluency(imgs, metrics = c("contrast", "symmetry"), gray = TRUE, progress = FALSE)
  expect_equal(results_gray$contrast[1], results$contrast[1])
  expect_equal(results_gray$contrast[2], img_contrast(rgb2gray(imgs[[2]])))
  expect_equal(results_gray$symmetry[2],
               unname(img_symmetry(rgb2gray(imgs[[2]]), horizontal = FALSE)))
})

test_that("img_fluency does not stop if an image cannot be read or a score cannot be computed", {
  broken <- tempfile(fileext = ".jpg")
  on.exit(unlink(broken), add = TRUE)
  writeLines("this is not an image", broken)
  files <- c(example_image("rails.jpg"), broken, "does_not_exist.jpg", example_image("sky.jpg"))

  expect_warning(
    results <- img_fluency(files, metrics = c("contrast", "typicality"), progress = FALSE),
    paste0("2 of 4 images could not be read or are not valid images \\(all scores are NA\\): ",
           basename(broken), ", does_not_exist\\.jpg")
  )
  expect_equal(nrow(results), 4)
  expect_equal(is.na(results$contrast), c(FALSE, TRUE, TRUE, FALSE))
  # typicality is computed for the two readable images
  expect_equal(results$typicality[c(1, 4)],
               as.vector(img_typicality(lapply(files[c(1, 4)], img_read))))
  expect_true(all(is.na(results$typicality[2:3])))

  # score cannot be computed: no variation in the left image half
  set.seed(2787)
  img <- matrix(runif(50 * 50), nrow = 50, ncol = 50)
  img_blank <- img
  img_blank[, 1:30] <- 0
  expect_warning(
    results <- img_fluency(list(ok = img, blank = img_blank),
                           metrics = c("contrast", "symmetry"), progress = FALSE),
    "Not all scores could be computed for 1 of 2 images \\(the respective scores are NA\\): blank"
  )
  expect_equal(is.na(results$symmetry), c(FALSE, TRUE))
  expect_equal(is.na(results$contrast), c(FALSE, FALSE))

  # invalid list element
  expect_warning(
    results <- img_fluency(list(ok = img, invalid = "foo"), metrics = "contrast", progress = FALSE),
    "1 of 2 images could not be read or are not valid images \\(all scores are NA\\): invalid"
  )
  expect_equal(is.na(results$contrast), c(FALSE, TRUE))

  # typicality needs at least two images
  expect_warning(results <- img_fluency(list(img), metrics = "typicality", progress = FALSE),
                 "Not all scores could be computed for 1 of 1 images")
  expect_true(is.na(results$typicality))
})

test_that("img_fluency checks the input", {
  expect_error(img_fluency(matrix(1:100, 10, 10)),
               "Input has to be a character vector of file paths or a \\*list\\* of images")
  expect_error(img_fluency(42),
               "Input has to be a character vector of file paths or a \\*list\\* of images")
  expect_error(img_fluency(character(0)), "Input does not contain any images")
  expect_error(img_fluency(list()), "Input does not contain any images")

  img <- matrix(runif(50 * 50), nrow = 50, ncol = 50)
  expect_warning(img_fluency(list(img), metrics = "contrast", gray = "yes", progress = FALSE),
                 "gray = 'yes' is not a logical value \\(TRUE\\/FALSE\\)\\. Automatically set to FALSE \\.\\.\\.")
})

test_that("img_fluency shows a progress bar if requested", {
  set.seed(2787)
  imgs <- replicate(2, matrix(runif(50 * 50), nrow = 50, ncol = 50), simplify = FALSE)
  expect_output(img_fluency(imgs, metrics = "contrast", progress = TRUE), "100%")
  expect_silent(img_fluency(imgs, metrics = "contrast", progress = FALSE))
})
