context("[Quality Control] Process Capability Study - non-normal distributions with negative data")

nonNormalNegativeResults <- function(distribution, offset) {
  dataset <- read.csv("datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv")
  dataset[["Diameter"]] <- dataset[["Diameter"]] - offset
  options <- analysisOptions("processCapabilityStudies")
  options$measurementLongFormat <- "Diameter"
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 5
  options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
  options$nonNormalDistribution <- distribution
  options$nullDistribution <- distribution
  options$controlChartType <- "xBarR"
  options$lowerSpecificationLimit <- TRUE
  options$target <- TRUE
  options$upperSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- 0 - offset
  options$targetValue <- 6 - offset
  options$upperSpecificationLimitValue <- 12 - offset
  set.seed(1)
  runAnalysis("processCapabilityStudies", dataset, options, view = FALSE)[["results"]][["capabilityAnalysis"]][["collection"]]
}

nonNormalTable <- function(collection, name) {
  collection[["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][[paste0("capabilityAnalysis_nonNormalCapabilityAnalysis_", name)]][["data"]]
}

test_that("Logistic capability analysis accepts negative data and is shift equivariant", {
  original <- nonNormalNegativeResults("logistic", 0)
  shifted  <- nonNormalNegativeResults("logistic", 10) # most measurements below zero
  expect_null(shifted[["capabilityAnalysis_p"]])

  # the logistic distribution is a location-scale family: shifting data and limits leaves capability unchanged
  expect_equal(nonNormalTable(shifted, "overallCapabilityNonNormal"),
               nonNormalTable(original, "overallCapabilityNonNormal"), tolerance = 1e-3)
  ppm <- function(collection) as.numeric(vapply(nonNormalTable(collection, "PerformanceNonNormal"), `[[`, "", "expOverall1"))
  expect_equal(ppm(shifted), ppm(original), tolerance = 1e-3)

  summaryShifted  <- nonNormalTable(shifted, "summaryTableNonNormal")[[1]]
  summaryOriginal <- nonNormalTable(original, "summaryTableNonNormal")[[1]]
  expect_equal(summaryShifted[["beta"]], summaryOriginal[["beta"]] - 10, tolerance = 1e-3)
  expect_equal(summaryShifted[["theta"]], summaryOriginal[["theta"]], tolerance = 1e-3)
})

test_that("Positive distributions still reject negative data", {
  shifted <- nonNormalNegativeResults("lognormal", 10)
  expect_identical(shifted[["capabilityAnalysis_p"]][["error"]][["errorMessage"]],
                   "Dataset contains negative numbers. Not compatible with the selected distribution.")
})
