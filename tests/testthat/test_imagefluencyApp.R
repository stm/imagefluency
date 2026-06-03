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
