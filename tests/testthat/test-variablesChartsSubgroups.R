context("[Quality Control] Variables Charts for Subgroups")

# Tests for Column data format
options <- analysisOptions("variablesChartsSubgroups")
options$CCDataFormat <- "CClongFormat"
options$variablesLong <- "Measurement3"
options$CCSubgroupSize <- 10
options$Wlimits <- TRUE
options$Wlimits2 <- TRUE
options$Xbarchart <- TRUE
options$Schart <- TRUE
set.seed(1)
results <- runAnalysis("variablesChartsSubgroups", "partOperatorData.csv", options)

test_that("Test result for X-bar chart table results match", {
  table <- results[["results"]][["NelsonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 7))
})

test_that("Xbar & s Control Chart plot matches", {
  plotName <- results[["results"]][["SPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "Xbar & s Plot")
})

test_that("X-bar & R Control Chart plot matches", {
  plotName <- results[["results"]][["XbarPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "Xbar & R Plot")
})

# Tests for Row data format
options$CCDataFormat <- "CCwideFormat"
options$variables <- c("Measurement1","Measurement2","Measurement3")
set.seed(1)
results <- runAnalysis("variablesChartsSubgroups", "partOperatorData.csv", options)

test_that("Test result for X-bar chart table results match", {
  table <- results[["results"]][["NelsonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 16, 2, 17, 3, 18, 9, 13, 14, 15, 22, 23, 28, 29, 30))
})

test_that("Test result for R chart table results match", {
  table <- results[["results"]][["NelsonTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 10))
})

test_that("Test result for s chart table results match", {
  table <- results[["results"]][["NelsonTableS"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 10))
})

test_that("Xbar & s Control Chart plot matches", {
  plotName <- results[["results"]][["SPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "Xbar & s Plot")
})

test_that("X-bar & R Control Chart plot matches", {
  plotName <- results[["results"]][["XbarPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "Xbar & R Plot")
})
