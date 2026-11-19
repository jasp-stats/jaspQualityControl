context("[Quality Control] Process Capability Study - historical threshold at or above the smallest observation")

# jasp-stats/jasp-issues#4691: the Process summary showed the raw try-error text ("Error : ...\n") and blamed the
# distribution, while a historical threshold at or above the smallest observation gives a likelihood of zero.

thresholdErrorResults <- function(distribution, threshold, shape = NULL) {
  diameter <- read.csv("datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv")[["Diameter"]]
  diameter[which.min(diameter)] <- 0 # row 8, smallest observation 0
  options <- analysisOptions("processCapabilityStudies")
  options$measurementLongFormat <- "Diameter"
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 5
  options$controlChartType <- "xBarR"
  options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
  options$nonNormalDistribution <- distribution
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- 2
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 12
  options$historicalThreshold <- TRUE
  options$historicalThresholdValue <- threshold
  if (!is.null(shape)) {
    options$historicalShape <- TRUE
    options$historicalShapeValue <- shape
  }
  set.seed(1)
  runAnalysis("processCapabilityStudies", data.frame(Diameter = diameter), options, view = FALSE)
}

thresholdErrorSummary <- function(results) {
  results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]]
}

expectThresholdError <- function(results, threshold) {
  expect_equal(results[["status"]], "complete")
  message <- thresholdErrorSummary(results)[["error"]][["errorMessage"]]
  expect_type(message, "character")
  expect_false(startsWith(message, "Error"))
  expect_false(endsWith(message, "\n"))
  expect_match(message, sprintf("historical threshold (%s) must be smaller than the smallest observation (0)", threshold), fixed = TRUE)
  expect_no_match(message, "far away from the selected distribution", fixed = TRUE)
  message
}

test_that("3-parameter Weibull: historical threshold at the smallest observation names the threshold", {
  message <- expectThresholdError(thresholdErrorResults("3ParameterWeibull", 0), 0)
  expect_match(message, "historical shape of 1", fixed = TRUE)
})

test_that("3-parameter Weibull: historical threshold above the smallest observation names the threshold", {
  message <- expectThresholdError(thresholdErrorResults("3ParameterWeibull", 1), 1)
  expect_no_match(message, "historical shape of 1", fixed = TRUE)
})

test_that("3-parameter log-normal: historical threshold at the smallest observation names the threshold", {
  expectThresholdError(thresholdErrorResults("3ParameterLognormal", 0), 0)
})

test_that("3-parameter Weibull: historical threshold at the smallest observation with historical shape 1 fits a shifted exponential", {
  results <- thresholdErrorResults("3ParameterWeibull", 0, shape = 1)
  summaryTable <- thresholdErrorSummary(results)
  expect_null(summaryTable[["error"]])
  row <- summaryTable[["data"]][[1]]
  expect_equal(row[["beta"]], 1)
  expect_equal(row[["threshold"]], 0)
  # the MLE of the scale of an exponential shifted by 0 is the sample mean (both shown with 3 decimals)
  expect_equal(row[["theta"]], row[["mean"]], tolerance = 1e-3)
})
