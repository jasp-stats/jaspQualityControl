context("[Quality Control] Process Capability Study - Box-Cox (auto) lambda by subgroup")

# Box-Cox (auto) with Log-Lik / SD pools the variation within subgroups (one row of the wide data each),
# not within positions of the subgroup (jasp-stats/INTERNAL-jasp#3424).
# Reference: car::powerTransform(Diameter ~ factor(Time)) = MASS::boxcox(Diameter ~ factor(Time)) = 0.5683

.bcGroupingDataset <- function() {
  read.csv(testthat::test_path("datasets", "processCapabilityStudy", "processCapabilityAnalysisLongFormatDebug.csv"))
}

.bcGroupingOptions <- function(method = "loglik") {
  options <- analysisOptions("processCapabilityStudies")
  options$testSet <- "jaspDefault"
  options$measurementLongFormat <- "Diameter"
  options$subgroupSizeType <- "groupingVariable"
  options$subgroup <- "Time"
  options$capabilityStudyType <- "normalCapabilityAnalysis"
  options$controlChartType <- "xBarR"
  options$controlChart <- FALSE
  options$histogram <- FALSE
  options$probabilityPlot <- FALSE
  options$processCapabilityPlot <- FALSE
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- 2
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 12
  options$target <- FALSE
  options$dataTransformation <- "boxCoxAuto"
  options$dataTransformationShift <- 0
  options$dataTransformationMethod <- method
  options$dataTransformationContinuityAdjustment <- FALSE
  return(options)
}

.bcGroupingResults <- function(dataset, options) {
  results <- runAnalysis("processCapabilityStudies", dataset, options)
  collection <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]]
  list(
    lambda      = results[["results"]][["dataTransformationContainer"]][["collection"]][["dataTransformationContainer_table"]][["data"]][[1]][["value"]],
    within      = collection[["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]],
    overall     = collection[["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]],
    performance = collection[["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  )
}

test_that("Box-Cox (auto) estimates lambda within subgroups (long format, grouping variable)", {
  dataset <- .bcGroupingDataset()
  res <- .bcGroupingResults(dataset, .bcGroupingOptions())

  expect_equal(res[["lambda"]], 0.5683, tolerance = 1e-3)
  expect_equal(as.numeric(res[["within"]][[1]][["cpk"]]), 0.835, tolerance = 1e-3)
  expect_equal(as.numeric(res[["overall"]][[1]][["ppk"]]), 0.797, tolerance = 1e-3)
  expect_equal(as.numeric(res[["performance"]][[3]][["expOverall1"]]), 8791, tolerance = 1e-3)

  # SD method minimises the pooled within-subgroup SD of the scaled power transform: same lambda
  resSd <- .bcGroupingResults(dataset, .bcGroupingOptions("sd"))
  expect_equal(resSd[["lambda"]], 0.5683, tolerance = 1e-3)
})

test_that("Box-Cox (auto) lambda does not depend on the order of values within a subgroup", {
  dataset <- .bcGroupingDataset()
  set.seed(1)
  shuffled <- dataset[order(dataset[["Time"]], runif(nrow(dataset))), ]

  res         <- .bcGroupingResults(dataset, .bcGroupingOptions())
  resShuffled <- .bcGroupingResults(shuffled, .bcGroupingOptions())
  expect_equal(resShuffled[["lambda"]], res[["lambda"]], tolerance = 1e-4)
  expect_equal(resShuffled[["within"]], res[["within"]])
  expect_equal(resShuffled[["overall"]], res[["overall"]])
})

test_that("Box-Cox (auto) lambda is the same for wide format and manual subgroup size", {
  dataset <- .bcGroupingDataset()
  res <- .bcGroupingResults(dataset, .bcGroupingOptions())

  wide <- as.data.frame(matrix(dataset[["Diameter"]], ncol = 5, byrow = TRUE))
  names(wide) <- paste0("x", 1:5)
  optionsWide <- .bcGroupingOptions()
  optionsWide$dataFormat <- "wideFormat"
  optionsWide$measurementsWideFormat <- names(wide)
  optionsWide$axisLabels <- ""
  expect_equal(.bcGroupingResults(wide, optionsWide)[["lambda"]], res[["lambda"]], tolerance = 1e-6)

  optionsManual <- .bcGroupingOptions()
  optionsManual$subgroupSizeType <- "manual"
  optionsManual$manualSubgroupSizeValue <- 5
  expect_equal(.bcGroupingResults(dataset, optionsManual)[["lambda"]], res[["lambda"]], tolerance = 1e-6)
})

test_that("Box-Cox (auto) with subgroup size 1 uses the overall variation", {
  dataset <- .bcGroupingDataset()
  options <- .bcGroupingOptions()
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 1
  options$controlChartType <- "xmr"

  # car::powerTransform(Diameter ~ 1) = 0.5447
  expect_equal(.bcGroupingResults(dataset, options)[["lambda"]], 0.5447, tolerance = 1e-3)
})
