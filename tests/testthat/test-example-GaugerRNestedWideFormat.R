context("Example: GaugerRNestedWideFormat")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("msaGaugeRRnonrep results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "GaugerRNestedWideFormat.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("msaGaugeRRnonrep", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_components-of-variation")

  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2, 0.0255551572552495, 0.836111111111103, 1.67222222222221, 0.9747921598386,
     "jaspColumn4", 27, 256.053140096618, 32.7179012345679, 883.383333333333,
     2.00494441020682e-52, "jaspColumn5(jaspColumn4)", 60, "", 0.127777777777778,
     7.66666666666667, "", "Repeatability", 89, "", "", 892.722222222222,
     "", "Total"))

  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.127777777777778, 1.16255124773012, "Total gauge r&amp;R", 0.127777777777778,
     1.16255124773012, "Repeatability", 0, 0, "Reproducibility",
     10.8633744855967, 98.8374487522699, "Part-To-part", 10.9911522633745,
     100, "Total variation"))

  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.35746017649212, 10.7821669794625, "Total gauge r&amp;R", 2.14476105895272,
     0.35746017649212, 10.7821669794625, "Repeatability", 2.14476105895272,
     0, 0, "Reproducibility", 0, 3.29596336229587, 99.4170250773326,
     "Part-To-part", 19.7757801737752, 3.31529067554784, 100, "Total variation",
     19.8917440532871))

  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_range-chart-by-operator")

})

