test_that("A failed staged fit retains valid estimates and observed counts", {
  options <- analysisOptions("processCapabilityStudies")
  options$probabilityPlotRankMethod <- "bernard"
  options$measurementLongFormat <- "x"
  options$stagesLongFormat <- "stage"
  options$manualSubgroupSizeValue <- 1
  options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
  options$nonNormalDistribution <- "gamma"
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- 1
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 9
  options$controlChart <- FALSE
  options$histogram <- FALSE
  options$probabilityPlot <- FALSE
  options$processCapabilityPlot <- FALSE
  data <- data.frame(x = c(1, 2, 4, 6, 8, 10, rep(4, 6)), stage = rep(c("A", "B"), each = 6))
  results <- runAnalysis("processCapabilityStudies", data, options, view = FALSE)
  tables <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]]
  capability <- tables[["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]][["data"]]
  performance <- tables[["capabilityAnalysis_nonNormalCapabilityAnalysis_PerformanceNonNormal"]][["data"]]
  expect_true(is.finite(capability[[1]]$ppk))
  expect_identical(capability[[2]]$ppk, "")
  expect_equal(as.numeric(performance[[3]]$observed2), 0)
  notes <- tables[["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]][["footnotes"]]
  expect_true(any(grepl("stage B", vapply(notes, function(note) note$text, character(1)))))
})

test_that("The report identifies every stage with a failed fit", {
  options <- analysisOptions("processCapabilityStudies")
  options$nonNormalDistribution <- "gamma"
  data <- data.frame(x = c(1, 2, 4, 6, 8, 10, rep(4, 6), rep(5, 6)),
                     stage = rep(c("A", "B", "C"), each = 6))
  message <- jaspQualityControl:::.qcNonNormalFitError(options, data, "x", "stage")
  expect_match(message, "Stage B:")
  expect_match(message, "Stage C:")
})

test_that("The summary footnote names the distribution when the last stage cannot be fitted", {
  options <- analysisOptions("processCapabilityStudies")
  options$probabilityPlotRankMethod <- "bernard"
  options$measurementLongFormat <- "x"
  options$stagesLongFormat <- "stage"
  options$manualSubgroupSizeValue <- 1
  options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
  options$nonNormalDistribution <- "weibull"
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- 1
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 9
  options$controlChart <- FALSE
  options$histogram <- FALSE
  options$probabilityPlot <- FALSE
  options$processCapabilityPlot <- FALSE
  data <- data.frame(x = c(1, 2, 4, 6, 8, 10, rep(4, 6)), stage = rep(c("A", "B"), each = 6))
  results <- runAnalysis("processCapabilityStudies", data, options, view = FALSE)
  tables <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]]
  notes <- unlist(lapply(tables, function(table) vapply(table[["footnotes"]], function(note) note$text, character(1))))
  expect_true(any(grepl("stage B", notes)))
  expect_true(any(grepl("Calculations based on Weibull distribution.", notes, fixed = TRUE)))
})
