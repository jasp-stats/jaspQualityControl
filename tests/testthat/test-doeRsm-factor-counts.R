test_that("factor counts are checked before generating or exporting a design", {
  matches <- jaspQualityControl:::.doeRsmFactorCountsMatch
  table <- function(n) list(list(values = seq_len(n)), list(values = seq_len(n)))
  opts <- list(numberOfContinuous = 2, continuousVariables = table(2),
               numberOfCategorical = 2, categoricalVariables = table(2))
  expect_true(matches(opts))
  opts$categoricalVariables <- table(7)
  expect_false(matches(opts))
  opts$numberOfCategorical <- 0
  expect_true(matches(opts))
  opts$continuousVariables[[2]]$values <- 1
  expect_false(matches(opts))

  local_mocked_bindings(.doeRsmFactorCountsMatch = function(options) FALSE, .package = "jaspQualityControl")
  opts <- analysisOptions("doeResponseSurfaceMethodology")
  opts$selectedRow <- 0L
  opts$displayDesign <- TRUE
  opts$actualExporter <- TRUE
  path <- tempfile(fileext = ".csv")
  opts$exportDesignFile <- path
  result <- runAnalysis("doeResponseSurfaceMethodology", "debug.csv", opts)
  expect_match(result$results$doeRsmDesignSummaryTable$error$errorMessage, "number of factor definitions does not match")
  expect_false(file.exists(path))
})
