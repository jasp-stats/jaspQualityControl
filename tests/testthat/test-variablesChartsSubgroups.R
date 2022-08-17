context("[Quality Control] Variables Charts for Subgroups")

## Long
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$subgroup <- "Time"
options$warningLimits <- TRUE
set.seed(1)
results <- runAnalysis("variablesChartsSubgroups", "SPCSubgroups_Long.csv", options)

# R chart
test_that("X-bar & R Control Chart plot matches", {
  plotName <- results[["results"]][["XbarPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart")
})


# S chart
options$chartType <- "xBarAndS"
results <- runAnalysis("variablesChartsSubgroups", "SPCSubgroups_Long.csv", options)

test_that("X-bar & s Control Chart plot matches", {
  plotName <- results[["results"]][["SPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart")
})

## Wide
options$dataFormat <- "wideFormat"
options$chartType <- "xBarAndR"
options$measurementsWideFormat <- c("dm1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
set.seed(1)
results <- runAnalysis("variablesChartsSubgroups", "SPCSubgroups_Wide.csv", options)

test_that("X-bar & R Control Chart2 plot matches", {
  plotName <- results[["results"]][["XbarPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart2")
})

# S chart
options$chartType <- "xBarAndS"
results <- runAnalysis("variablesChartsSubgroups", "SPCSubgroups_Wide.csv", options)

test_that("X-bar & s Control Chart 2 plot matches", {
  plotName <- results[["results"]][["SPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart2")
})


### Unequal subgroup sizes
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 22
options$subgroupSizeUnequal <- "actualSizes"
results <- runAnalysis("variablesChartsSubgroups", "SPCSubgroups_Long.csv", options)

test_that("X-bar & R Control Chart3 plot matches", {
  plotName <- results[["results"]][["XbarPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart3")
})


### Known parameters x-bar & r-chart
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 22
options$subgroupSizeUnequal <- "actualSizes"
options$knownParameters <- TRUE
options$knownParametersMean <- -6
results <- runAnalysis("variablesChartsSubgroups", "SPCSubgroups_Long.csv", options)

test_that("Test results for R chart table results match", {
  table <- results[["results"]][["NelsonTables"]][["collection"]][["NelsonTables_NelsonTableR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(5))
})

test_that("X-bar & R Control Chart 4 plot matches", {
  plotName <- results[["results"]][["XbarPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart4")
})

### Known parameters x-bar & s-chart
options$chartType <- "xBarAndS"
results <- runAnalysis("variablesChartsSubgroups", "SPCSubgroups_Long.csv", options)

test_that("Test results for s chart table results match", {
  table <- results[["results"]][["NelsonTables"]][["collection"]][["NelsonTables_NelsonTableS"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 2, 3, 4, 5))
})

test_that("X-bar & s Control Chart 3 plot matches", {
  plotName <- results[["results"]][["SPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart3")
})
