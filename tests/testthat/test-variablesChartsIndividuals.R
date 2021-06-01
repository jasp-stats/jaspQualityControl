context("[Quality Control] Variables Charts for Individuals")

test_that("Variable: Measurement1 plot matches", {
  options <- analysisOptions("variablesChartsIndividuals")
  options$variables <- "Measurement1"
  set.seed(1)
  results <- runAnalysis("variablesChartsIndividuals", "partOperatorData.csv", options)
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Measurement1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "variable-measurement1", dir="variablesChartsIndividuals")
})
