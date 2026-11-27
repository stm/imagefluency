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
