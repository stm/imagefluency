context("shiny app")

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
