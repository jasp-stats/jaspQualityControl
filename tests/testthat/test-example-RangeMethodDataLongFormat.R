context("Example: RangeMethodDataLongFormat")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("msaTestRetest results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "RangeMethodDataLongFormat.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("msaTestRetest", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["ScatterOperatorParts"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_run-chart-of-parts")

  table <- results[["results"]][["rAndR2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.587716720540699, 0.195905573513566, 3.5263003232442, 3, 0.7,
     1.19105, 5, 1))

  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_range-chart-by-part")

})

