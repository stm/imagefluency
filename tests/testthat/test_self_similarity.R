test_that("img_self_similarity only handles numeric matrices", {
  expect_error(img_self_similarity(matrix("foo", nrow = 10, ncol = 10)),
               "Input img has to be a matrix or array of \\*numeric\\* or \\*integer\\* values")
  expect_error(img_self_similarity(1:10),
               "Invalid input \\(should be a matrix or a 3-dimensional array of numeric or integer values\\)")
})

test_that("img_self_similarity checks whether optional parameters are logical", {
  set.seed(2787)
  img <- matrix(runif(100*100, min = 0, max = 255), nrow = 100, ncol = 100)
  expect_warning(img_self_similarity(img, full = "yes"),
                 "full = 'yes' is not a logical value \\(TRUE\\/FALSE\\)\\. Automatically set to FALSE \\.\\.\\.")
  expect_warning(img_self_similarity(img, logplot = "yes"),
                 "logplot = 'yes' is not a logical value \\(TRUE\\/FALSE\\)\\. Skipping log-log plot \\.\\.\\.")
  expect_warning(img_self_similarity(img, raw = "yes"),
                 "raw = 'yes' is not a logical value \\(TRUE\\/FALSE\\)\\. Automatically set to FALSE \\.\\.\\.")
})

test_that("img_self_similarity checks whether image is at least 22 pixels in each dimension, throws error otherwise", {
  img <- matrix(1, nrow = 21, ncol = 100)
  expect_error(img_self_similarity(img),
               "Image has to be at least 22 pixels in each dimension\\. Input image has 100 \\(width\\) x 21 \\(height\\)\\.")
})

test_that("img_self_similarity gives results you'd expect (grayscale)", {
  set.seed(2787)
  # result must be 0 or less than zero
  img <- matrix(runif(599*599, min = 0, max = 255), nrow = 599, ncol = 599)
  expect_lte(img_self_similarity(img), 0)
})

test_that("img_self_similarity gives results you'd expect", {
  set.seed(2787)
  # result must be 0 or less than zero
  red <- matrix(runif(50*50, min = 0, max = 255), nrow = 50, ncol = 50)
  green <- matrix(runif(50*50, min = 0, max = 255), nrow = 50, ncol = 50)
  blue <- matrix(runif(50*50, min = 0, max = 255), nrow = 50, ncol = 50)
  imgRGB <- array(c(red, green, blue), dim = c(50, 50, 3))
  expect_lte(img_self_similarity(imgRGB), 0)

  #
  imgRGB <- array(0, dim = c(100, 100, 3))
  imgRGB[1:50, 1:50, ] <- 255
  expect_equal(img_self_similarity(imgRGB), expected = -0.5553468, tolerance = .00001)
})


test_that("img_self_similarity plots only grayscale images so far", {
  imgRGB <- array(runif(100*100*3, min = 0, max = 255), dim = c(100, 100, 3))
  expect_warning(img_self_similarity(imgRGB, logplot = TRUE),
                 "Plot option currently only supports grayscale images\\. No plot is shown\\.")
})


test_that("img_self_similarity logplot works as intended
          (i.e., plots something without any errors)", {
  set.seed(2787)
  img <- matrix(runif(50*50, min = 0, max = 255), nrow = 50, ncol = 50)

  # should return no errors when plotting succeeds
  #
  # use case: all packages are installed (i.e., ggplot2 is used
  expect_silent(img_self_similarity(img, logplot = TRUE))
  expect_error(img_self_similarity(img, logplot = TRUE), NA)

  # use case: base plot is used (requireNamespace returns false)
  mockery::stub(img_self_similarity, 'requireNamespace', FALSE)
  expect_error(img_self_similarity(img, logplot = TRUE), NA)
})

test_that("Non-squared image is resized using OpenImageR", {
  set.seed(2787)
  # non-squared but even sizes
  img <- matrix(runif(50*48, min = 0, max = 255), nrow = 50, ncol = 48)
  expect_equal(img_self_similarity(img), -1.558845, tolerance = .00001)

  # non-squared and odd sizes
  img <- matrix(runif(50*49, min = 0, max = 255), nrow = 50, ncol = 49)
  expect_equal(img_self_similarity(img), -0.8992694, tolerance = .00001)

  # OpenImageR package has to be installed
  mockery::stub(.selfsim, '.pkg_avail', FALSE)
  expect_error(.selfsim(img, full = FALSE, logplot = FALSE, raw=FALSE),
               "Package \\'OpenImageR\\' is required but not installed on your system\\.")
})

test_that("Image size >= 22 pix", {
  set.seed(2787)
  img <- matrix(runif(20*20, min = 0, max = 255), nrow = 20, ncol = 20)
  expect_error(img_self_similarity(img),
               "Image has to be at least 22 pixels in each dimension\\. Input image has 20 \\(width\\) x 20 \\(height\\)\\.")
})

test_that("Get raw log-log slope", {
  set.seed(2787)
  img <- matrix(runif(50*50, min = 0, max = 255), nrow = 50, ncol = 50)
  expect_equal(img_self_similarity(img, raw = FALSE), -2 - img_self_similarity(img, raw = TRUE))
})


test_that("Option to compute self-similarity on full range", {
  set.seed(2787)
  img <- matrix(runif(50*50, min = 0, max = 255), nrow = 50, ncol = 50)
  expect_equal(img_self_similarity(img, full = TRUE), -2.135299, tolerance = .00001)
})

test_that("img_self_similarity falls back to base plotting when ggplot2/scales are unavailable", {
  set.seed(2787)
  img <- matrix(runif(50 * 50, min = 0, max = 255), nrow = 50, ncol = 50)
  mockery::stub(img_self_similarity, ".pkg_avail", FALSE)

  expect_error(img_self_similarity(img, logplot = TRUE), NA)
})

test_that("img_self_similarity checks the input", {
  img <- matrix(runif(50 * 50), nrow = 50, ncol = 50)
  expect_error(img_self_similarity(as.data.frame(img)),
               "Invalid input \\(should be a matrix or a 3-dimensional array of numeric or integer values\\)")
  expect_warning(result <- img_self_similarity(img, full = NA),
                 "full = 'NA' is not a logical value \\(TRUE\\/FALSE\\)\\. Automatically set to FALSE \\.\\.\\.")
  expect_equal(result, img_self_similarity(img))
  img[1, 1] <- NA
  expect_error(img_self_similarity(img),
               "Input image contains missing or non-finite values")
})

test_that("img_self_similarity returns NA with a warning for images without variation", {
  expect_warning(result <- img_self_similarity(matrix(0.5, nrow = 64, ncol = 64)),
                 "No variation in the image\\. Computation of self-similarity not possible\\. Returning NA\\.")
  expect_identical(result, NA_real_)

  # color image with a color channel without variation
  set.seed(2787)
  imgRGB <- array(c(runif(64 * 64 * 2), rep(0, 64 * 64)), dim = c(64, 64, 3))
  expect_warning(result <- img_self_similarity(imgRGB, logplot = TRUE),
                 "No variation in the image or in at least one color channel\\. Computation of self-similarity not possible\\. Returning NA\\.")
  expect_identical(result, NA_real_)
})

test_that("img_self_similarity warns if resize is not a logical value", {
  set.seed(2787)
  img <- matrix(runif(50 * 60, min = 0, max = 255), nrow = 50, ncol = 60)
  expect_warning(result <- img_self_similarity(img, resize = "no"),
                 "resize = 'no' is not a logical value \\(TRUE\\/FALSE\\)\\. Automatically set to TRUE \\.\\.\\.")
  expect_equal(result, img_self_similarity(img))
})

test_that("resize = FALSE has no effect for square images", {
  set.seed(2787)
  img <- matrix(runif(50 * 50, min = 0, max = 255), nrow = 50, ncol = 50)
  expect_identical(img_self_similarity(img, resize = FALSE), img_self_similarity(img))

  imgRGB <- array(runif(49 * 49 * 3, min = 0, max = 255), dim = c(49, 49, 3))
  expect_identical(img_self_similarity(imgRGB, resize = FALSE), img_self_similarity(imgRGB))
})

test_that("resize = FALSE analyzes non-square images without resizing", {
  # image with a known slope of the log-log power spectrum of -2
  set.seed(2787)
  n <- 256
  freq <- c(0:(n / 2), -(n / 2 - 1):-1)
  radius <- sqrt(outer(freq^2, freq^2, "+"))
  radius[1, 1] <- 1
  phase <- matrix(runif(n * n, min = 0, max = 2 * pi), nrow = n, ncol = n)
  img <- Re(stats::fft(radius^(-1) * exp(1i * phase), inverse = TRUE))

  # landscape, portrait, and odd image sizes
  img_landscape <- img[1:120, 1:180]
  expect_equal(img_self_similarity(img_landscape, raw = TRUE, resize = FALSE), -2,
               tolerance = 0.05)
  expect_equal(img_self_similarity(t(img_landscape), raw = TRUE, resize = FALSE), -2,
               tolerance = 0.05)
  expect_equal(img_self_similarity(img[1:121, 1:179], raw = TRUE, resize = FALSE), -2,
               tolerance = 0.05)

  # resizing makes the slope steeper
  expect_lt(img_self_similarity(img_landscape, raw = TRUE),
            img_self_similarity(img_landscape, raw = TRUE, resize = FALSE))

  # OpenImageR is not needed
  mockery::stub(.selfsim, ".pkg_avail", function(pkg) !identical(pkg, "OpenImageR") && requireNamespace(pkg, quietly = TRUE))
  expect_error(.selfsim(img_landscape, full = FALSE, raw = TRUE, logplot = FALSE, resize = FALSE), NA)

  # log-log plot and color images work as well
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(img_self_similarity(img_landscape, logplot = TRUE, resize = FALSE), NA)
  imgRGB <- array(img_landscape, dim = c(120, 180, 3))
  expect_equal(img_self_similarity(imgRGB, raw = TRUE, resize = FALSE),
               img_self_similarity(img_landscape, raw = TRUE, resize = FALSE))
})

test_that("img_self_similarity warns if the slope is based on only a few frequencies", {
  set.seed(2787)
  img <- matrix(runif(22 * 22), nrow = 22, ncol = 22)
  expect_warning(img_self_similarity(img),
                 "Small image: the slope of the log-log power spectrum is estimated from only 2 spatial frequencies\\. The result might be unreliable \\(consider using 'full = TRUE'\\)\\.")
  expect_warning(img_self_similarity(img, full = TRUE), NA)

  img <- matrix(runif(38 * 38), nrow = 38, ncol = 38)
  expect_warning(img_self_similarity(img), NA)
})
