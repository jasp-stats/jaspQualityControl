context("[Quality Control] Process Capability Study - non-normal distributions with zero values")

# jasp-stats/INTERNAL-jasp#3417: zeros were replaced by half the smallest positive value for every non-normal distribution.

zeroValuesData <- function() {
  diameter <- read.csv("datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv")[["Diameter"]]
  list(
    deviations = round(diameter - 7),                     # gauge resolution: 21 zeros, both signs
    nonNegative = Filter(function(x) x >= 0, round(diameter - 4)), # 6 zeros, smallest positive value 1
    oneZero = replace(diameter, which.min(diameter), 0)   # positive data with a single zero
  )
}

zeroValuesResults <- function(distribution, data, lowerLimit, upperLimit, nullDistribution = distribution, historicalThreshold = NULL) {
  options <- analysisOptions("processCapabilityStudies")
  options$measurementLongFormat <- "Diameter"
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 5
  options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
  options$nonNormalDistribution <- distribution
  options$nullDistribution <- nullDistribution
  if (!is.null(historicalThreshold)) {
    options$historicalThreshold <- TRUE
    options$historicalThresholdValue <- historicalThreshold
  }
  options$controlChartType <- "xBarR"
  options$lowerSpecificationLimit <- TRUE
  options$upperSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- lowerLimit
  options$upperSpecificationLimitValue <- upperLimit
  set.seed(1)
  runAnalysis("processCapabilityStudies", data.frame(Diameter = data), options, view = FALSE)[["results"]]
}

zeroValuesTable <- function(results, name) {
  results[["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][[paste0("capabilityAnalysis_nonNormalCapabilityAnalysis_", name)]][["data"]]
}

zeroValuesSummary <- function(results) zeroValuesTable(results, "summaryTableNonNormal")[[1]]
zeroValuesIndices <- function(results) zeroValuesTable(results, "overallCapabilityNonNormal")[[1]]
zeroValuesPpm <- function(results, column) as.numeric(vapply(zeroValuesTable(results, "PerformanceNonNormal"), `[[`, "", column))

# Location(-scale) families: shifting data and limits must leave the capability unchanged, with zeros or without
expectShiftEquivariant <- function(distribution, data, lowerLimit, upperLimit, shift = 10, thresholdTolerance = 1e-3) {
  asEntered <- zeroValuesResults(distribution, data, lowerLimit, upperLimit)
  shifted   <- zeroValuesResults(distribution, data + shift, lowerLimit + shift, upperLimit + shift)

  expect_null(asEntered[["zeroWarning"]])
  # the indices are rounded to 3 decimals, so an optimizer difference can flip the last digit
  expect_equal(zeroValuesIndices(asEntered), zeroValuesIndices(shifted), tolerance = 2e-3)
  expect_equal(zeroValuesPpm(asEntered, "expOverall1"), zeroValuesPpm(shifted, "expOverall1"), tolerance = 1e-3)
  expect_equal(zeroValuesSummary(asEntered)[["mean"]], mean(data))
  expect_equal(zeroValuesSummary(asEntered)[["mean"]], zeroValuesSummary(shifted)[["mean"]] - shift)
  asEntered
}

test_that("Logistic capability analysis keeps zeros and is shift equivariant", {
  data <- zeroValuesData()$deviations
  results <- expectShiftEquivariant("logistic", data, -5, 5)
  expect_equal(zeroValuesSummary(results)[["sd"]], sd(data))
  expect_equal(zeroValuesSummary(results)[["n"]], 100)
})

test_that("3-parameter Weibull capability analysis keeps a zero and is shift equivariant", {
  data <- zeroValuesData()$oneZero
  results <- expectShiftEquivariant("3ParameterWeibull", data, 2, 12)
  shifted <- zeroValuesResults("3ParameterWeibull", data + 10, 12, 22)
  expect_equal(zeroValuesSummary(results)[["threshold"]], zeroValuesSummary(shifted)[["threshold"]] - 10, tolerance = 1e-3)
})

test_that("3-parameter log-normal capability analysis keeps zeros and is shift equivariant", {
  expectShiftEquivariant("3ParameterLognormal", zeroValuesData()$nonNegative, 0.4, 8)
})

test_that("Exponential capability analysis keeps zeros: the scale is the sample mean", {
  data <- zeroValuesData()$nonNegative
  results <- zeroValuesResults("exponential", data, 0.4, 8)
  expect_null(results[["zeroWarning"]])
  expect_equal(zeroValuesSummary(results)[["theta"]], mean(data), tolerance = 1e-6)
  expect_equal(zeroValuesSummary(results)[["mean"]], mean(data))
})

test_that("Changing the capability distribution preserves the normal diagnostic for recorded zeros", {
  x <- c(0, 3, 3, 3, 4, 4, 6, 10, 10)
  for (distribution in c("exponential", "weibull")) {
    results <- zeroValuesResults(distribution, x, 1, 9, nullDistribution = "normal")
    probability <- results[["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]][[1]]
    expect_equal(probability$n, 9)
    expect_equal(probability$mean, mean(x))
    expect_equal(probability$sd, sd(x))
    expect_equal(probability$ad, 0.594, tolerance = 0.001)
    expect_equal(probability$p, 0.085, tolerance = 0.001)
    if (distribution == "weibull") {
      capability <- results[["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]]
      expect_match(capability[["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]][["error"]][["errorMessage"]], "greater than zero")
      expect_null(capability[["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]])
    } else {
      expect_equal(zeroValuesSummary(results)$theta, mean(x), tolerance = 1e-6)
    }
  }
})

test_that("Positive-only distributions reject a recorded zero before estimating or using fixed parameters", {
  for (distribution in c("weibull", "lognormal", "gamma", "loglogistic")) {
    expect_error(jaspQualityControl:::.distributionParameters(c(0, 1, 2), distribution), "greater than zero")
  }
  expect_error(jaspQualityControl:::.distributionParameters(c(0, 1, 2), "weibull",
                                                            fix.arg = list(shape = 2, scale = 1)), "greater than zero")
})

test_that("A probability plot distribution that needs positive data shows an error instead of stopping the analysis", {
  # the capability distribution (logistic) is fitted to the zeros as entered, the log-normal of the probability plot cannot be
  results <- zeroValuesResults("logistic", zeroValuesData()$nonNegative, 0.4, 8, nullDistribution = "lognormal")
  expect_null(results[["errorMessage"]])
  expect_equal(zeroValuesSummary(results)[["n"]], 99)
  probabilityTable <- results[["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]]
  expect_false(is.null(probabilityTable[["error"]][["errorMessage"]]))
})

test_that("3-parameter Weibull with a historical threshold at the smallest observation gives an error, not parameters of another fit", {
  results <- zeroValuesResults("3ParameterWeibull", zeroValuesData()$oneZero, 2, 12, historicalThreshold = 0)
  summaryTable <- results[["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]]
  expect_match(summaryTable[["error"]][["errorMessage"]], "could not be estimated")
})
