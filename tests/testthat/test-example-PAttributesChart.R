context("Example: PAttributesChart")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("attributesCharts results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "PAttributesChart.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("attributesCharts", dataset, opts)

  plotName <- results[["results"]][["PchartPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_p-chart")

})

