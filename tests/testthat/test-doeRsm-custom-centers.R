test_that("custom Box-Behnken centers update summary and generated run counts", {
  for (centers in c(1, 5)) {
    opts <- analysisOptions("doeResponseSurfaceMethodology")
    opts$numberOfContinuous <- 4L
    opts$continuousVariables <- list(
      list(levels = paste("Row", 0:3), name = "data 1", values = LETTERS[1:4]),
      list(levels = paste("Row", 0:3), name = "data 2", values = rep(-1, 4)),
      list(levels = paste("Row", 0:3), name = "data 3", values = rep(1, 4)))
    opts$numberOfCategorical <- 1L
    opts$categoricalVariables <- list(
      list(levels = "Row 0", name = "data 1", values = "G"),
      list(levels = "Row 0", name = "data 2", values = "a"),
      list(levels = "Row 0", name = "data 3", values = "b"))
    opts$designType <- "boxBehnkenDesign"
    opts$selectedRow <- 0L
    opts$centerPointType <- "custom"
    opts$customCubeBlock <- centers
    opts$replicates <- 2L
    opts$displayDesign <- TRUE
    opts$runOrder <- "runOrderStandard"
    result <- runAnalysis("doeResponseSurfaceMethodology", "debug.csv", opts)
    summary <- result$results$doeRsmDesignSummaryTable$data[[1]]
    expect_equal(summary$baseRuns, 24 + 3 * centers)
    expect_equal(summary$totalRuns, (24 + 3 * centers) * 4)
    expect_length(result$results$designTable$data, (24 + 3 * centers) * 4)
  }
})
