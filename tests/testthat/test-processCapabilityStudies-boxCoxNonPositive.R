context("[Quality Control] Process Capability Study - Box-Cox with values <= -shift")

boxCoxNonPositiveResults <- function(transformation, zero, shift = 0) {
  dataset <- read.csv("datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv")
  if (zero)
    dataset[["Diameter"]][1] <- 0
  options <- analysisOptions("processCapabilityStudies")
  options$measurementLongFormat <- "Diameter"
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 5
  options$controlChartType <- "xBarR"
  options$lowerSpecificationLimit <- TRUE
  options$upperSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- 2
  options$upperSpecificationLimitValue <- 12
  options$dataTransformation <- transformation
  options$dataTransformationLambda <- 0.5
  options$dataTransformationShift <- shift
  set.seed(1)
  runAnalysis("processCapabilityStudies", dataset, options, view = FALSE)
}

normalTable <- function(results, name) {
  results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][[paste0("capabilityAnalysis_normalCapabilityAnalysis_", name)]][["data"]]
}

boxCoxNonPositiveError <- "The Box-Cox transformation requires measurement + shift > 0, but 1 measurement is smaller than or equal to -shift (0). Increase the shift or use the Yeo-Johnson or Johnson transformation."

test_that("Box-Cox with a zero measurement stops instead of dropping it", {
  results <- boxCoxNonPositiveResults("boxCox", zero = TRUE)
  expect_identical(results[["status"]], "validationError")
  expect_identical(results[["results"]][["errorMessage"]], boxCoxNonPositiveError)
})

test_that("Box-Cox (auto) with a zero measurement stops instead of dropping it", {
  results <- boxCoxNonPositiveResults("boxCoxAuto", zero = TRUE)
  expect_identical(results[["status"]], "validationError")
  expect_identical(results[["results"]][["errorMessage"]], boxCoxNonPositiveError)
})

test_that("Box-Cox with a zero measurement and a positive shift uses all measurements", {
  results <- boxCoxNonPositiveResults("boxCox", zero = TRUE, shift = 1)
  expect_identical(results[["status"]], "complete")
  expect_equal(normalTable(results, "processSummaryTable")[[1]][["n"]], 100)
  observed <- vapply(normalTable(results, "capabilityTablePerformance"), function(row) as.numeric(row[["observed1"]]), numeric(1))
  expect_equal(observed, c(10000, 10000, 20000))
})

test_that("Box-Cox with positive measurements is unchanged", {
  results <- boxCoxNonPositiveResults("boxCox", zero = FALSE)
  expect_identical(results[["status"]], "complete")
  expect_equal(normalTable(results, "processSummaryTable")[[1]][["n"]], 100)
  jaspTools::expect_equal_tables(normalTable(results, "capabilityTableOverall"),
                                 list(0.972, 0.783, 0.677, 0.89, 1.16, 0.857, 0.783, 1.084))
})
