context("[Quality Control] Variables Charts for Subgroups - wide format dependencies")

# jasp-stats/INTERNAL-jasp#3412: the outputs depended on the non-existent option "variables" instead of
# "measurementsWideFormat", so changing the wide-format Measurements kept the previous chart, limits and tests.
# jaspTools runs are stateless (no state is reused between runs), so the dependencies are checked directly,
# and the fresh-run results for the changed column selections are checked below.

test_that("All outputs depend on the wide-format measurements and data format, and only on existing options", {
  getDependencies <- jaspQualityControl:::.variablesChartsSubgroupsDependencies
  # option names in the QML source, incl. the imported common/ControlChartTests.qml (analysisOptions() misses some)
  qmlFiles   <- system.file("qml", c("variablesChartsSubgroups.qml", "common/ControlChartTests.qml"), package = "jaspQualityControl")
  expect_length(qmlFiles[nzchar(qmlFiles)], 2)
  qmlSource  <- unlist(lapply(qmlFiles, readLines))
  qmlOptions <- sub('.*name:\\s*"([^"]+)".*', "\\1", grep('name:\\s*"[^"]+"', qmlSource, value = TRUE))
  # rule9 / rule9Value are in the shared rule helper but not in this form
  qmlOptions <- c(qmlOptions, "rule9", "rule9Value")
  for (element in c("notReadyPlot", "subgroupSizeOnePlot", "controlCharts", "report")) {
    dependencies <- getDependencies(element)
    expect_true(all(c("measurementsWideFormat", "dataFormat", "measurementLongFormat") %in% dependencies), label = element)
    expect_false("variables" %in% dependencies, label = element)
    expect_true(all(dependencies %in% qmlOptions), label = paste(element, ":", paste(setdiff(dependencies, qmlOptions), collapse = ", ")))
  }
  expect_true("manualSubgroupSizeValue" %in% getDependencies("subgroupSizeOnePlot"))
})

.wideFormatDataset <- function() {
  dataset <- read.csv(testthat::test_path("datasets", "variableChartsSubgroups", "variableChartsSubgroupsWideFormatDebug.csv"))
  dataset$dm6 <- dataset$dm3
  dataset$dm6[10] <- dataset$dm6[10] + 12
  dataset
}

.wideFormatResults <- function(measurements) {
  options <- jaspTools::analysisOptions("variablesChartsSubgroups")
  options$testSet <- "jaspDefault"
  options$dataFormat <- "wideFormat"
  options$measurementsWideFormat <- as.list(measurements)
  options$chartType <- "xBarAndR"
  set.seed(1)
  jaspTools::runAnalysis("variablesChartsSubgroups", .wideFormatDataset(), options, view = FALSE)
}

.wideFormatLimits <- function(results) {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  subplots <- results[["state"]][["figures"]][[plotName]][["obj"]]$subplots
  # first two layers are the center line and the upper control limit
  lapply(subplots, function(p) sapply(ggplot2::ggplot_build(p)$data[1:2], function(d) d$y[1]))
}

test_that("Removing measurement columns (dm1-dm5 -> dm1-dm3) gives the dm1-dm3 limits", {
  limits <- .wideFormatLimits(.wideFormatResults(c("dm1", "dm2", "dm3")))
  expect_equal(limits[[1]], c(-7.21333333, -4.08786667), tolerance = 1e-6) # x-bar chart CL, UCL
  expect_equal(limits[[2]], c(3.055, 7.86432393), tolerance = 1e-6)        # R chart CL (R-bar), UCL
})

test_that("Swapping a measurement column (dm3 -> dm6) gives the test 1 signal at point 10 in the R chart", {
  results <- .wideFormatResults(c("dm1", "dm2", "dm6"))
  limits  <- .wideFormatLimits(results)
  expect_equal(limits[[2]][2], 9.370259, tolerance = 1e-6)
  rTable <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_secondTable"]][["data"]]
  jaspTools::expect_equal_tables(rTable, list("Point 10"))
})
