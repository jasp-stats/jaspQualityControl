test_that("Worksheet export preserves responses and existing files", {
  export <- jaspQualityControl:::.doeRsmExportDesign
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  options <- list(exportDesignFile = path, actualExporter = TRUE)
  design <- data.frame(run.order = 1:2, std.order = 1:2, Temperature = c(-1, 1))
  expect_identical(export(options, design), "created")
  existing <- read.csv(path, check.names = FALSE)
  existing$Response <- c(12, 15)
  write.csv(existing, path, row.names = FALSE)
  before <- readBin(path, "raw", n = file.info(path)$size)
  expect_identical(export(options, design), "unchanged")
  expect_identical(readBin(path, "raw", n = file.info(path)$size), before)
  design$Temperature <- c(-2, 2)
  expect_identical(export(options, design), "exists")
  expect_identical(readBin(path, "raw", n = file.info(path)$size), before)
  options$exportDesignFile <- file.path(tempfile(), "missing.csv")
  expect_identical(export(options, design), "failed")
})

test_that("Exclusive copy never overwrites its destination", {
  copy <- jaspQualityControl:::.doeCopyExclusive
  source <- tempfile()
  destination <- tempfile()
  on.exit(unlink(c(source, destination)))
  writeBin(as.raw(c(0, 1, 255)), source)
  expect_true(copy(source, destination))
  expect_identical(readBin(destination, "raw", n = 3), as.raw(c(0, 1, 255)))
  writeBin(as.raw(c(9, 8, 7)), source)
  expect_false(copy(source, destination))
  expect_identical(readBin(destination, "raw", n = 3), as.raw(c(0, 1, 255)))
})

test_that("A factor named Response does not lose its values", {
  export <- jaspQualityControl:::.doeRsmExportDesign
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  options <- list(exportDesignFile = path, actualExporter = TRUE)
  design <- data.frame(run.order = 1:2, std.order = 1:2, Response = c("low, first", "high"))
  expect_identical(export(options, design), "created")
  actual <- read.csv(path, check.names = FALSE)
  expect_identical(names(actual), c("RunOrder", "StandardOrder", "Response", "Response.1"))
  expect_identical(actual$Response, c("low, first", "high"))
})
