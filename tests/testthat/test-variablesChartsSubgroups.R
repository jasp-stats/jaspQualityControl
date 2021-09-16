context("[Quality Control] Variables Charts for Subgroups")

# Tests for Column data format
options <- analysisOptions("variablesChartsSubgroups")
options$CCDataFormat <- "CClongFormat"
options$variablesLong <- "Measurement2"
options$CCSubgroupSize <- 5
options$Wlimits <- TRUE
options$Wlimits2 <- TRUE
options$Xbarchart <- TRUE
options$Schart <- TRUE
set.seed(1)
results <- runAnalysis("variablesChartsSubgroups", "partOperatorData.csv", options)

test_that("Test result for X-bar chart table results match", {
  table <- results[["results"]][["NelsonTableX"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3))
})

test_that("Xbar & s Control Chart plot matches", {
  plotName <- results[["results"]][["SPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "xbar-s-control-chart")
})

test_that("X-bar & R Control Chart plot matches", {
  plotName <- results[["results"]][["XbarPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart")
})
# Tests for Row data format
options$CCDataFormat <- "CCwideFormat"
options$variables <- c("Measurement1","Measurement2","Measurement3")
options$subgroups <- "time"
set.seed(1)
results <- runAnalysis("variablesChartsSubgroups", "partOperatorData_Time.csv", options)

test_that("Test result for X-bar chart table results match2", {
  table <- results[["results"]][["NelsonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("00:05", "08:10", "08:15", "11:50", "15:40", "19:50", "20:30"
                                 ))
})

test_that("Xbar & s Control Chart plot matches2", {
  plotName <- results[["results"]][["SPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "xbar-s-control-chart2")
})

test_that("X-bar & R Control Chart plot matches2", {
  plotName <- results[["results"]][["XbarPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart2")
})
