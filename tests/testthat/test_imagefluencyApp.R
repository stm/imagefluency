test_that("shiny package is installed", {
  mockery::stub(run_imagefluency, 'requireNamespace', FALSE)
  expect_error(run_imagefluency(),
               "Package \\'shiny\\' is required but not installed on your system\\.")
})

test_that("empty shiny app directory (likely error on install)", {
  mockery::stub(run_imagefluency, 'system.file', "")
  expect_error(run_imagefluency(),
               "Could not find shiny app directory\\. Try re-installing \\`imagefluency\\`\\.")
})

test_that("run_imagefluency launches shiny app when available", {
  mockery::stub(run_imagefluency, "system.file", "/tmp/imagefluencyApp")
  mockery::stub(run_imagefluency, "requireNamespace", TRUE)
  mockery::stub(run_imagefluency, "shiny::runApp", "launched")

  expect_identical(run_imagefluency(), "launched")
})

test_that("run_imagefluency errors on invalid max_images", {
  expect_error(run_imagefluency(max_images = 0),
               "`max_images` must be a positive number\\.")
  expect_error(run_imagefluency(max_images = "ten"),
               "`max_images` must be a positive number\\.")
})

# ---- server logic of the dashboard ------------------------------------------

app_dir <- function() system.file("imagefluencyApp", package = "imagefluency")
example_image <- function(name) system.file("example_images", name, package = "imagefluency")
# mimics the data frame that shiny::fileInput() returns
upload <- function(paths, names = basename(paths)) {
  data.frame(name = names, size = file.size(paths), type = "image/jpeg",
             datapath = paths, stringsAsFactors = FALSE)
}

test_that("dashboard computes the metrics for the demo image", {
  skip_if_not_installed("shiny")
  rails <- img_read(example_image("rails.jpg"))

  shiny::testServer(app_dir(), {
    session$setInputs(select_all_metrics = FALSE,
                      statistic = c("contr", "selfsim", "simpl", "sym"))
    session$setInputs(calcfluency = 1)

    expect_equal(nrow(rv$results), 1)
    expect_equal(rv$results$Contrast, img_contrast(rails))
    expect_equal(rv$results$Self_Similarity, img_self_similarity(rails))
    expect_equal(rv$results$Simplicity, img_simplicity(rails), tolerance = 0.01)
    expect_equal(rv$results$Symmetry, unname(img_symmetry(rails, horizontal = FALSE)))
  })
})

test_that("dashboard keeps results and images aligned if an image cannot be read", {
  skip_if_not_installed("shiny")
  broken <- tempfile(fileext = ".jpg")
  on.exit(unlink(broken), add = TRUE)
  writeLines("this is not an image", broken)
  files <- upload(c(example_image("rails.jpg"), broken, example_image("sky.jpg"), example_image("trees.jpg")),
                  c("rails.jpg", "broken.jpg", "sky.jpg", "trees.jpg"))

  shiny::testServer(app_dir(), {
    session$setInputs(file = files)
    session$setInputs(select_all_metrics = FALSE, statistic = c("contr", "typ"))
    session$setInputs(calcfluency = 1)

    # one row per uploaded image, in the same order
    expect_equal(rv$results$Image, rv$image_names)
    expect_equal(is.na(rv$results$Contrast), c(FALSE, TRUE, FALSE, FALSE))
    expect_equal(rv$results$Contrast[3], img_contrast(img_read(example_image("sky.jpg"))))

    # typicality is computed for the readable images
    typicality <- img_typicality(lapply(example_image(c("rails.jpg", "sky.jpg", "trees.jpg")), img_read))
    expect_equal(rv$results$Typicality, c(typicality[1], NA, typicality[2:3]))
  })
})

test_that("dashboard handles grayscale images", {
  skip_if_not_installed("shiny")
  gray_file <- tempfile(fileext = ".png")
  on.exit(unlink(gray_file), add = TRUE)
  gray <- rgb2gray(img_read(example_image("rails.jpg")))
  magick::image_write(magick::image_read(array(gray, dim = c(dim(gray), 1))),
                      path = gray_file, format = "png")
  files <- upload(c(example_image("rails.jpg"), gray_file), c("rails.jpg", "rails_gray.png"))

  shiny::testServer(app_dir(), {
    session$setInputs(file = files)
    session$setInputs(select_all_metrics = TRUE,
                      statistic = c("contr", "selfsim", "simpl", "sym", "typ"))
    session$setInputs(calcfluency = 1)

    expect_equal(dim(rv$results), c(2, 6))
    expect_false(anyNA(rv$results))
  })
})
