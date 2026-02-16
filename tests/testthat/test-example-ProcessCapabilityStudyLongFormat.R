context("Example: ProcessCapabilityStudyLongFormat")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("processCapabilityStudies results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "ProcessCapabilityStudyLongFormat.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("processCapabilityStudies", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_x-bar-r-control-chart")

})

