context("Example: Type1InstrumentCapability")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("msaType1Gauge results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Type1InstrumentCapability.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("msaType1Gauge", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["biasHistogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_bias-histogram")

  plotName <- results[["results"]][["biasRun"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_run-chart-of-jaspcolumn1")

  table <- results[["results"]][["biasTable"]][["collection"]][["biasTable_Basic"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.52008746203907, 0.0540000000000003, 0.360000000000002, -3.946,
     -4, 0.420014577006512, 15, -5.5, -2.5))

  table <- results[["results"]][["biasTable"]][["collection"]][["biasTable_Capability"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.19, 1.15, 16.8, 17.43))

  table <- results[["results"]][["biasTtest"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0540000000000001, 49, -0.0653668220476197, 0.367744349288335,
     0.909105737620188, 0.17336682204762))

})

