test_that("A diagnostic distribution does not suppress a valid logistic capability fit", {
  options <- analysisOptions("processCapabilityStudies")
  options$probabilityPlotRankMethod <- "bernard"
  options$measurementLongFormat <- "x"
  options$manualSubgroupSizeValue <- 1
  options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
  options$nonNormalDistribution <- "logistic"
  options$nullDistribution <- "weibull"
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- -2
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 2
  options$controlChart <- FALSE
  options$histogram <- FALSE
  options$processCapabilityPlot <- FALSE
  data <- data.frame(x = c(-3, -1, 0, 1, 2, 3))
  results <- runAnalysis("processCapabilityStudies", data, options, view = FALSE)
  tables <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]]
  expect_equal(tables[["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]][["data"]][[1]]$ppk, 0.203)
  expect_match(results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["error"]][["errorMessage"]], "negative")
})

test_that("A diagnostic distribution does not suppress a valid logistic capability report", {
  options <- analysisOptions("processCapabilityStudies")
  options$probabilityPlotRankMethod <- "bernard"
  options$measurementLongFormat <- "x"
  options$manualSubgroupSizeValue <- 1
  options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
  options$nonNormalDistribution <- "logistic"
  options$nullDistribution <- "weibull"
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- -2
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 2
  options$report <- TRUE
  data <- data.frame(x = c(-3, -1, 0, 1, 2, 3))
  results <- runAnalysis("processCapabilityStudies", data, options, view = FALSE)
  expect_identical(results[["status"]], "complete")
  report <- results[["results"]][["report"]]
  expect_null(report[["error"]])
  expect_true(inherits(results[["state"]][["figures"]][[report[["data"]]]][["obj"]], "jaspMatrixPlot"))
})
