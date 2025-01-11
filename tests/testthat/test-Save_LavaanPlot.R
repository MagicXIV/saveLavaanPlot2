## Saved file
test_that("saveLavaanPlot generates a valid plot", {
  library(lavaan)

  # Fit the model
  model <- ' visual  =~ x1 + x2 + x3 '
  data <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa(model, data = data)

  # Call the function
  sem_plot <- saveLavaanPlot(fit = fit, filePath = tempfile(fileext = ".png"))

  # Check if sem_plot is a grViz object
  expect_true(inherits(sem_plot, "grViz"))

  # Optionally render the plot to ensure it's correct
  return(sem_plot)
})


## valid fit object
test_that("saveLavaanPlot throws an error for invalid fit object", {
  expect_error(saveLavaanPlot(fit = "not_a_model", filePath = "test.png"))
})


## invalid file path
test_that("saveLavaanPlot throws an error for invalid file path", {
  library(lavaan)
  model <- ' visual  =~ x1 + x2 + x3 '
  fit <- cfa(model, data = HolzingerSwineford1939)
  expect_error(saveLavaanPlot(fit = fit, filePath = "test"))
})



## Directory creation
test_that("saveLavaanPlot creates a directory if it does not exist", {
  temp_dir <- tempfile()
  temp_file <- file.path(temp_dir, "test.png")

  library(lavaan)
  model <- ' visual  =~ x1 + x2 + x3 '
  fit <- cfa(model, data = HolzingerSwineford1939)

  saveLavaanPlot(fit = fit, filePath = temp_file)

  expect_true(file.exists(temp_dir))
  expect_true(file.exists(temp_file))

  # Clean-up
  unlink(temp_dir, recursive = TRUE)
})


## compatibility with other packages
test_that("saveLavaanPlot works with valid inputs", {
  library(lavaan)
  model <- ' visual  =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed   =~ x7 + x8 + x9 '
  fit <- cfa(model, data = HolzingerSwineford1939)

  temp_file <- tempfile(fileext = ".png")
  expect_error(saveLavaanPlot(fit, temp_file), NA)
  expect_true(file.exists(temp_file))
})

