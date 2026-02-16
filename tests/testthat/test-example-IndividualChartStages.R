context("Example: IndividualChartStages")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("variablesChartsIndividuals results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "IndividualChartStages.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("variablesChartsIndividuals", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_x-mr-control-chart")

})

