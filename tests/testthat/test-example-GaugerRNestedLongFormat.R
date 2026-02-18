context("Example: GaugerRNestedLongFormat")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("msaGaugeRRnonrep results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "GaugerRNestedLongFormat.jasp")
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
    list(2, 0.092585866870024, 0.868619975635812, 1.73723995127162, 0.911859074638568,
     "jaspColumn2", 27, 9.99997705926056, 9.38177720855838, 253.307984631076,
     1.1128747856415e-13, "jaspColumn3(jaspColumn2)", 60, "", 0.93817987310984,
     56.2907923865904, "", "Repeatability", 89, "", "", 311.336016968938,
     "", "Total"))

  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.93817987310984, 25.0000477932985, "Total gauge r&amp;R", 0.93817987310984,
     25.0000477932985, "Repeatability", 0, 0, "Reproducibility",
     2.81453244514951, 74.9999522067015, "Part-To-part", 3.75271231825936,
     100, "Total variation"))

  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.968596857887656, 50.0000477932757, 581.158114732594, "Total gauge r&amp;R",
     5.81158114732594, 0.968596857887656, 50.0000477932757, 581.158114732594,
     "Repeatability", 5.81158114732594, 0, 0, 0, "Reproducibility",
     0, 1.67765683175956, 86.6025127849657, 1006.59409905573, "Part-To-part",
     10.0659409905573, 1.93719186408042, 100, 1162.31511844825, "Total variation",
     11.6231511844825))

  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_range-chart-by-operator")

})

