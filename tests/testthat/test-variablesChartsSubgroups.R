context("[Quality Control] Variables Charts for Subgroups")

options <- analysisOptions("variablesChartsSubgroups")
options$variables <- c("Measurement1", "Measurement2", "Measurement3")
options$time <- "Part"
options$Xbarchart <- TRUE
options$Schart <- TRUE
set.seed(1)
results <- runAnalysis("variablesChartsSubgroups", "partOperatorData.csv", options)


test_that("Test Results for R Chart table results match", {
  table <- results[["results"]][["NelsonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 18, 10))
})

test_that("Test Results for XBAR Chart table results match", {
  table <- results[["results"]][["NelsonTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 16, 2, 16, 2, 17, 3, 3, 18, 14, 9, 15, 13, 22, 14, 23, 15,
                                      24, 22, 29, 23, 30, 28, 29, 30))
})

test_that("Test Results for S Chart table results match", {
  table <- results[["results"]][["NelsonTableS"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 10))
})

test_that("Test Results for XBAR Chart table results match", {
  table <- results[["results"]][["NelsonTableX"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 16, 2, 16, 2, 17, 3, 3, 18, 14, 9, 15, 13, 22, 14, 23, 15,
                                      24, 22, 29, 23, 30, 28, 29, 30))
})

test_that("Xbar & s Control Chart plot matches", {
  plotName <- results[["results"]][["SPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "xbar-s-control-chart", dir="variablesChartsSubgroups")
})

test_that("X-bar & R Control Chart plot matches", {
  plotName <- results[["results"]][["XbarPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart", dir="variablesChartsSubgroups")
})
