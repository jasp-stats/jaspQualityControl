context("[Quality Control] Empty rows below a shorter measurement column")

# Empty rows after the last measurement (e.g. when other columns of the data set are longer) are not data.
# With a manual subgroup size or in wide format they used to become empty subgroups at the end of the chart.

set.seed(1)
.trailingValues <- round(rnorm(25, 10), 3)
.trailingLong   <- data.frame(y = c(.trailingValues, rep(NA, 15)))
.trailingWide   <- data.frame(a = c(.trailingValues[1:5 * 5 - 4], rep(NA, 7)), b = c(.trailingValues[1:5 * 5 - 3], rep(NA, 7)),
                              c = c(.trailingValues[1:5 * 5 - 2], rep(NA, 7)), d = c(.trailingValues[1:5 * 5 - 1], rep(NA, 7)),
                              e = c(.trailingValues[1:5 * 5], rep(NA, 7)))

# number of plotted subgroups (rows of the X-bar point layer, including empty ones)
.trailingNumberOfPoints <- function(results, key) {
  p <- results[["state"]][["figures"]][[key]][["obj"]]$subplots[[1]]
  isPoint <- vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))
  nrow(ggplot2::ggplot_build(p)$data[[which(isPoint)[1]]])
}

test_that("control charts do not add empty subgroups for trailing empty rows", {
  options <- jaspTools::analysisOptions("variablesChartsSubgroups")
  options$measurementLongFormat <- "y"
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 5
  options$chartType <- "xBarAndR"
  results <- jaspTools::runAnalysis("variablesChartsSubgroups", .trailingLong, options, view = FALSE)
  key <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  expect_identical(.trailingNumberOfPoints(results, key), 5L) # 8 before the fix: 3 empty subgroups at the end

  options$dataFormat <- "wideFormat"
  options$measurementsWideFormat <- c("a", "b", "c", "d", "e")
  resultsWide <- jaspTools::runAnalysis("variablesChartsSubgroups", .trailingWide, options, view = FALSE)
  keyWide <- resultsWide[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  expect_identical(.trailingNumberOfPoints(resultsWide, keyWide), 5L) # 12 before the fix
})

test_that("a subgroup column left over from grouping does not keep empty rows in manual mode", {
  # the form disables the subgroup field in manual mode, but keeps its column assigned
  options <- jaspTools::analysisOptions("variablesChartsSubgroups")
  options$measurementLongFormat <- "y"
  options$subgroup <- "lot"
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 5
  options$chartType <- "xBarAndR"
  data <- data.frame(y = c(.trailingValues, rep(NA, 15)), lot = sprintf("L%02d", 1:40))
  results <- jaspTools::runAnalysis("variablesChartsSubgroups", data, options, view = FALSE)
  key <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  expect_identical(.trailingNumberOfPoints(results, key), 5L)
})

test_that("missing values inside the data keep their subgroup", {
  values <- .trailingValues
  values[7] <- NA # subgroup 2 keeps 4 values, the later subgroups do not shift
  options <- jaspTools::analysisOptions("variablesChartsSubgroups")
  options$measurementLongFormat <- "y"
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 5
  options$chartType <- "xBarAndR"
  results <- jaspTools::runAnalysis("variablesChartsSubgroups", data.frame(y = c(values, rep(NA, 15))), options, view = FALSE)
  key <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  p <- results[["state"]][["figures"]][[key]][["obj"]]$subplots[[1]]
  built <- ggplot2::ggplot_build(p)
  isPoint <- vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))
  means <- built$data[[which(isPoint)[1]]]$y
  expected <- tapply(values, rep(1:5, each = 5), mean, na.rm = TRUE)
  expect_equal(means[!is.na(means)], unname(as.numeric(expected)))
})

test_that("the capability study note counts the observations without a subgroup", {
  options <- jaspTools::analysisOptions("processCapabilityStudies")
  options$measurementLongFormat <- "y"
  options$subgroupSizeType <- "groupingVariable"
  options$subgroup <- "lot"
  options$manualSubgroupSizeValue <- 5
  options$probabilityPlotRankMethod <- "bernard"
  data <- data.frame(y = c(.trailingValues, NA, NA), lot = c(rep(sprintf("L%d", 1:5), each = 5), NA, NA))
  results <- jaspTools::runAnalysis("processCapabilityStudies", data, options, view = FALSE)
  note <- results[["results"]][["xBar"]][["collection"]][["xBar_plotNote"]][["text"]]
  expect_match(note, "Removed 2 observation(s) that were not assigned to any subgroups.", fixed = TRUE)
})
