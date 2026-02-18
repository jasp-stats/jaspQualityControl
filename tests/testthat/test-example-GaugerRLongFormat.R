context("Example: GaugerRLongFormat")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("msaGaugeRR results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "GaugerRLongFormat.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("msaGaugeRR", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(22.56, "Total gauge r&amp;R", 0.848225424385098, 22.54, "Repeatability",
     0.8475221639879, 0.02, "Reproducibility", 0.000703260397198739,
     0.02, "jaspColumn2", 0.000703260397198739, 77.44, "Part-to-part",
     2.91190554016356, 100, "Total variation", 3.76013096454866
    ))

  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.920991544144189, 47.5, 55.26, "Total gauge r&amp;R", 5.52594926486514,
     0.920609669723222, 47.48, 55.24, "Repeatability", 5.52365801833933,
     0.0265190572456628, 1.37, 1.59, "Reproducibility", 0.159114343473977,
     0.0265190572456628, 1.37, 1.59, "jaspColumn2", 0.159114343473977,
     1.7064306432327, 88, 102.39, "Part-to-part", 10.2385838593962,
     1.93910571257698, 100, 116.35, "Total variation", 11.6346342754619
    ))

  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_components-of-variation")

  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9, 28.8374040004607, 27.05467202546, 6.66730220297081e-09, 243.49204822914,
     "jaspColumn3", 2, 0.925856545015139, 0.868619975903862, 0.414257745376511,
     1.73723995180772, "jaspColumn2", 18, 0.581263589155472, 0.545329800306983,
     0.899320321555707, 9.81593640552569, "jaspColumn3  *  jaspColumn2",
     60, 0.938179873092174, 56.2907923855304, "Repeatability", 89,
     311.336016972003, "Total"))

  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9, 31.9220820115876, 27.05467202546, 1.20668981418678e-22, 243.49204822914,
     "jaspColumn3", 2, 1.02489352233184, 0.868619975903862, 0.363615551623405,
     1.73723995180772, "jaspColumn2", 78, 0.8475221639879, 66.1067287910562,
     "Repeatability", 89, 311.336016972003, "Total"))

  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_range-chart-by-operator")

})

