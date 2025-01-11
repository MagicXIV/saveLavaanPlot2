## valid fit object
test_that("saveLavaanPlot2 throws an error for invalid fit object", {
  expect_error(saveLavaanPlot2(fit = "not_a_model", filePath = "test.png"))
})


## invalid file path
test_that("saveLavaanPlot2 throws an error for invalid file path", {
  library(lavaan)
  model <- ' visual  =~ x1 + x2 + x3 '
  data <- lavaan::HolzingerSwineford1939
  fit <- cfa(model, data = HolzingerSwineford1939)
  expect_error(saveLavaanPlot2(fit = fit, filePath = "test"))
})


## Directory creation
test_that("saveLavaanPlot2 creates a directory if it does not exist", {
  temp_dir <- tempfile()
  temp_file <- file.path(temp_dir, "test.png")

  library(lavaan)
  model <- ' visual  =~ x1 + x2 + x3 '
  fit <- cfa(model, data = HolzingerSwineford1939)

  saveLavaanPlot2(fit = fit, filePath = temp_file)

  expect_true(file.exists(temp_dir))
  expect_true(file.exists(temp_file))

  # Clean-up
  unlink(temp_dir, recursive = TRUE)
})


## compatibility with other packages
test_that("saveLavaanPlot2 works with valid inputs", {
  library(lavaan)
  model <- ' visual  =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed   =~ x7 + x8 + x9 '
  fit <- cfa(model, data = HolzingerSwineford1939)

  temp_file <- tempfile(fileext = ".png")
  expect_error(saveLavaanPlot2(fit, temp_file), NA)
  expect_true(file.exists(temp_file))
})

