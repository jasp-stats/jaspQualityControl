context("Example: ProcessCapabilityStudyWideFormat")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("processCapabilityStudies results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "ProcessCapabilityStudyWideFormat.jasp")
  # A .jasp file only stores the options that existed when it was saved. JASP Desktop fills the rest
  # in from the QML defaults; jaspTools does not, so options added after the file was saved have to
  # be merged in here or the analysis is called with an incomplete options list.
  opts <- modifyList(jaspTools::analysisOptions("processCapabilityStudies"),
                     jaspTools::analysisOptions(jaspFile))
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("processCapabilityStudies", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_x-bar-s-control-chart")

})

