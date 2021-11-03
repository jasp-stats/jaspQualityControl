context("[Quality Control] Variables Charts for Subgroups")

# Tests for Column data format
options <- analysisOptions("variablesChartsSubgroups")
options$CCDataFormat <- "CClongFormat"
options$variablesLong <- "Ovality"
options$subgroups <- "Time"
options$Wlimits <- TRUE
options$TypeChart <- "Xbarchart"
results <- runAnalysis("variablesChartsSubgroups", "Ovality2.csv", options)

test_that("Test result for R chart table results match", {
  table <- results[["results"]][["NelsonTables"]][["collection"]][["NelsonTables_NelsonTableR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("00:20"))
})

test_that("X-bar & R Control Chart plot matches", {
  plotName <- results[["results"]][["XbarPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart")
})


