context("[Quality Control] Variables Charts for Individuals")

options <- analysisOptions("variablesChartsIndividuals")
options$variables <- "Diameter"
options$subgroups <- "Time"
options$CorPlot <- TRUE
set.seed(1)
results <- runAnalysis("variablesChartsIndividuals", "SPCSubgroups_Long.csv", options)


test_that("Diameter plot matches", {
  plotName <- results[["results"]][["CorPlot"]][["collection"]][["CorPlot_Diameter"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "diameter")
})

test_that("titleless-plot-1 matches", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Diameter"]][["collection"]][["Ichart_Diameter_Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "titleless-plot-1")
})

test_that("Test results for Diameter for Individuals chart table results match", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Diameter"]][["collection"]][["Ichart_Diameter_Table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("16:55"))
})

test_that("Test results for Diameter for Range chart table results match", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Diameter"]][["collection"]][["Ichart_Diameter_Table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("10:10", "16:10", "16:10", "16:10", "16:10", "20:10", "20:10",
                                      "20:10", "21:00", "21:00", "21:00"))
})
