context("[Quality Control] Variables Charts for Individuals")

options <- analysisOptions("variablesChartsIndividuals")
options$variables <- "Measurement3"
options$CorPlot <- TRUE
set.seed(1)
results <- runAnalysis("variablesChartsIndividuals", "partOperatorData.csv", options)

test_that("Autocorrelation Plot matches", {
  plotName <- results[["results"]][["CorPlot"]][["collection"]][["CorPlot_Measurement3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "Autocorrelation Plot for Measurement3")
})

test_that("Individual and Moving Range Plot matches", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Measurement3"]][["collection"]][["Ichart_Measurement3_Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "Individual and MR Plot for Measurement3")
})


test_that("Test result for Measurement3 for Individual chart table results match", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Measurement3"]][["collection"]][["Ichart_Measurement3_Table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 16, 3, 9, 17, 14, 29, 18, 29, 30, 30))
})

test_that("Test result for Measurement3 for R chart table results match", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Measurement3"]][["collection"]][["Ichart_Measurement3_Table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(15, 12, 13, 26, 27))
})
