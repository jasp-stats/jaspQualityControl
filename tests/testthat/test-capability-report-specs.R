reportSummaryTable <- function(data, options) {
  results <- runAnalysis("processCapabilityStudies", data, options, view = FALSE)
  plotName <- results[["results"]][["report"]][["data"]]
  report <- results[["state"]][["figures"]][[plotName]][["obj"]]
  tables <- unlist(lapply(report$subplots, function(plot) {
    lapply(plot$layers, function(layer) if ("tb" %in% names(layer$data)) layer$data$tb else NULL)
  }), recursive = FALSE)
  tables <- unlist(tables, recursive = FALSE)
  Filter(function(table) is.data.frame(table) && "Target" %in% names(table), tables)[[1]]
}

test_that("The non-normal report distinguishes an unset target from zero", {
  options <- analysisOptions("processCapabilityStudies")
  options$probabilityPlotRankMethod <- "bernard"
  options$measurementLongFormat <- "x"
  options$manualSubgroupSizeValue <- 1
  options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
  options$nonNormalDistribution <- "logistic"
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- -2
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 2
  options$report <- TRUE
  options$reportProcessStability <- FALSE
  options$reportProbabilityPlot <- FALSE
  options$reportProcessCapabilityPlot <- FALSE
  options$reportMetaData <- FALSE
  data <- data.frame(x = c(-3, -1, 0, 1, 2, 3))
  summaryTarget <- function(options) reportSummaryTable(data, options)$Target[[1]]
  expect_identical(summaryTarget(options), "*")
  options$target <- TRUE
  options$targetValue <- 0
  expect_equal(summaryTarget(options), 0)
})

test_that("The normal report shows unset specification limits and target as asterisks", {
  options <- analysisOptions("processCapabilityStudies")
  options$probabilityPlotRankMethod <- "bernard"
  options$measurementLongFormat <- "x"
  options$manualSubgroupSizeValue <- 1
  options$capabilityStudyType <- "normalCapabilityAnalysis"
  options$lowerSpecificationLimit <- FALSE
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 5
  options$report <- TRUE
  options$reportProcessStability <- FALSE
  options$reportProbabilityPlot <- FALSE
  options$reportProcessCapabilityPlot <- FALSE
  options$reportMetaData <- FALSE
  data <- data.frame(x = c(-3, -1, 0, 1, 2, 3))
  summary <- reportSummaryTable(data, options)
  expect_identical(summary[[1]][[1]], "*")
  expect_identical(summary$Target[[1]], "*")
  expect_equal(summary[[3]][[1]], 5)
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- 0
  options$target <- TRUE
  options$targetValue <- 0
  summary <- reportSummaryTable(data, options)
  expect_equal(summary[[1]][[1]], 0)
  expect_equal(summary$Target[[1]], 0)
})
