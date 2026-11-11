context("[Quality Control] Process Capability Moving Range Errors")

.pcMovingRangeData <- data.frame(
  First  = c(1.1, NA, 3.2, NA, 5.3, NA, 2.4, NA, 4.2),
  Second = c(2.1, NA, 4.2, NA, 6.3, NA, 3.5, NA, 5.7)
)

.pcMovingRangeOptions <- function() {
  options <- analysisOptions("processCapabilityStudies")
  options$dataFormat <- "wideFormat"
  options$measurementsWideFormat <- c("First", "Second")
  options$controlChartType <- "xBarMR"
  options$controlChartSdEstimationMethodGroupSizeLargerThanOne <- "rBar"
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- 0
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 10
  options
}

.pcNormalResults <- function(results) {
  results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]]
}

.pcPlotLabels <- function(plot) {
  if (!is.null(plot$subplots))
    return(unlist(lapply(plot$subplots, .pcPlotLabels)))
  if (!ggplot2::is_ggplot(plot))
    return(NULL)
  unlist(lapply(seq_along(plot$layers), function(i) as.character(ggplot2::layer_data(plot, i)[["label"]])))
}

options <- .pcMovingRangeOptions()
results <- runAnalysis("processCapabilityStudies", .pcMovingRangeData, options)
options$controlChart <- FALSE
withoutChart <- runAnalysis("processCapabilityStudies", .pcMovingRangeData, options)

test_that("Missing moving ranges do not discard process capability results", {
  expect_identical(results[["status"]], "complete")
  actual <- .pcNormalResults(results)
  expected <- .pcNormalResults(withoutChart)
  for (name in c("processSummaryTable", "capabilityTableWithin", "capabilityTableOverall", "capabilityTablePerformance")) {
    key <- paste0("capabilityAnalysis_normalCapabilityAnalysis_", name)
    expect_identical(actual[[key]][["status"]], "complete")
    expect_equal(actual[[key]][["data"]], expected[[key]][["data"]])
  }
  # outside the report, the X-bar chart is drawn alone and the moving range error is noted below it
  chart <- results[["results"]][["xBar"]][["collection"]]
  expect_identical(chart[["xBar_plot"]][["status"]], "complete")
  expect_true(ggplot2::is_ggplot(results[["state"]][["figures"]][[chart[["xBar_plot"]][["data"]]]][["obj"]]))
  expect_match(chart[["xBar_plotNote"]][["text"]], "Moving range chart not shown. Cannot estimate the standard deviation from moving ranges of length 2: the data have fewer than 2 consecutive non-missing observations.", fixed = TRUE)
})

options <- .pcMovingRangeOptions()
options$report <- TRUE
results <- runAnalysis("processCapabilityStudies", .pcMovingRangeData, options)

test_that("Process capability report retains tables and the moving-range error", {
  expect_identical(results[["status"]], "complete")
  report <- results[["results"]][["report"]]
  expect_identical(report[["status"]], "complete")
  labels <- .pcPlotLabels(results[["state"]][["figures"]][[report[["data"]]]][["obj"]])
  expect_true(any(grepl("the data have fewer than 2 consecutive", labels, fixed = TRUE)))
  expect_true(any(grepl("Process capability", labels, fixed = TRUE)))
})

options <- .pcMovingRangeOptions()
options$dataFormat <- "longFormat"
options$measurementLongFormat <- "First"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 1
options$controlChartType <- "xmr"
results <- runAnalysis("processCapabilityStudies", .pcMovingRangeData, options)

test_that("Unavailable within SD leaves overall capability estimates available", {
  expect_identical(results[["status"]], "complete")
  normal <- .pcNormalResults(results)
  summary <- normal[["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]]
  expect_true(any(grepl("within std. dev. could not be calculated", unlist(summary[["footnotes"]]), fixed = TRUE)))
  overall <- normal[["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]]
  expect_identical(overall[["status"]], "complete")
  expect_false(is.null(overall[["data"]]))
  within <- normal[["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]]
  expect_true(all(unlist(within[["data"]]) == "*"))
  expect_true(is.na(jaspQualityControl:::.qcCapabilityMovingRangeSd(.pcMovingRangeData["First"], 2)))
})

test_that("X-mR chart without a complete moving range shows the error on the plot", {
  chart <- results[["results"]][["xmr"]][["collection"]]
  expect_identical(chart[["xmr_plot"]][["status"]], "error")
  expect_match(chart[["xmr_plot"]][["error"]][["errorMessage"]], "Cannot estimate the standard deviation from moving ranges of length 2: the data have fewer than 2 consecutive non-missing observations.", fixed = TRUE)
})

# moving ranges of length 2 (individuals chart) but not of length 3 (moving range chart)
options <- .pcMovingRangeOptions()
options$dataFormat <- "longFormat"
options$measurementLongFormat <- "First"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 1
options$controlChartType <- "xmr"
options$xmrChartMovingRangeLength <- 3
results <- runAnalysis("processCapabilityStudies", data.frame(First = c(1.1, 2.3, NA, 3.2, 4.1, NA, 5.3, 2.4, NA, 4.2)), options)

test_that("X-mR chart shows the individuals chart alone if only the moving range chart fails", {
  chart <- results[["results"]][["xmr"]][["collection"]]
  expect_identical(results[["status"]], "complete")
  expect_identical(chart[["xmr_plot"]][["status"]], "complete")
  expect_true(ggplot2::is_ggplot(results[["state"]][["figures"]][[chart[["xmr_plot"]][["data"]]]][["obj"]]))
  expect_false(is.null(chart[["xmr_tableIndividual"]]))
  expect_match(chart[["xmr_plotNote"]][["text"]], "Moving range chart not shown. Cannot estimate the standard deviation from moving ranges of length 3: the data have fewer than 3 consecutive non-missing observations.", fixed = TRUE)
})
