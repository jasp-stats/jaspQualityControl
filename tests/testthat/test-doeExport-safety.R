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
  expect_identical(copy(source, destination), "created")
  expect_identical(readBin(destination, "raw", n = 3), as.raw(c(0, 1, 255)))
  writeBin(as.raw(c(9, 8, 7)), source)
  expect_identical(copy(source, destination), "open_failed")
  expect_identical(readBin(destination, "raw", n = 3), as.raw(c(0, 1, 255)))
})

test_that("Failed exclusive copying leaves the created destination for inspection", {
  copy <- jaspQualityControl:::.doeCopyExclusive
  source <- tempfile()
  destination <- tempfile()
  on.exit(unlink(c(source, destination)))
  writeBin(rep(as.raw(42), 65537), source)
  local_mocked_bindings(.doeCopyChunks = function(input, output) {
    writeBin(readBin(input, "raw", n = 65536), output)
    stop("simulated copy failure")
  }, .package = "jaspQualityControl")
  expect_identical(copy(source, destination), "incomplete")
  expect_true(file.exists(destination))
  expect_equal(file.info(destination)$size, 65536)
  writeBin(as.raw(c(17, 23)), destination)
  expect_identical(copy(source, destination), "open_failed")
  expect_identical(readBin(destination, "raw", n = 2), as.raw(c(17, 23)))
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

test_that("A failed fallback copy is reported as incomplete", {
  local_mocked_bindings(.doeLinkDesign = function(...) FALSE,
                        .doeCopyExclusive = function(...) "incomplete", .package = "jaspQualityControl")
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  options <- list(exportDesignFile = path, actualExporter = TRUE)
  design <- data.frame(run.order = 1:2, std.order = 1:2, x = c(-1, 1))
  expect_identical(jaspQualityControl:::.doeRsmExportDesign(options, design), "incomplete")
})

test_that("An incomplete worksheet is identified in the analysis results", {
  local_mocked_bindings(.doeLinkDesign = function(...) FALSE,
                        .doeCopyChunks = function(...) stop("simulated copy failure"), .package = "jaspQualityControl")
  options <- analysisOptions("doeFactorial")
  options$numberOfCategorical <- 3
  options$categoricalVariables <- list(
    list(levels = paste("Row", 0:2), name = "data 1", values = c("A", "B", "C")),
    list(levels = paste("Row", 0:2), name = "data 2", values = rep("a", 3)),
    list(levels = paste("Row", 0:2), name = "data 3", values = rep("b", 3)))
  options$selectedDesign2 <- list(list(levels = c("Row 0", "Row 1"), name = "data 1", values = c(4, 8)),
                                  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(0, 0)))
  options$selectedRow <- 1
  options$displayDesign <- TRUE
  options$actualExporter <- TRUE
  options$exportDesignFile <- tempfile(fileext = ".csv")
  on.exit(unlink(options$exportDesignFile))
  result <- runAnalysis("doeFactorial", "debug.csv", options)
  expect_match(result$results$exportStatus$text, "worksheet may be incomplete")
})
