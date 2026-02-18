context("Example: GaugerRWideFormat")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("msaGaugeRR results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "GaugerRWideFormat.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("msaGaugeRR", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.43, "Total gauge r&amp;R", 0.157929724596391, 1.22, "Repeatability",
     0.134544159544159, 0.21, "Reproducibility", 0.0233855650522318,
     0.21, "jaspColumn4", 0.0233855650522318, 98.57, "Part-to-part",
     10.8561068903661, 100, "Total variation", 11.0140366149625
    ))

  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.397403729972922, 11.97, 23.84, "Total gauge r&amp;R", 2.38442237983753,
     0.36680261659939, 11.05, 22.01, "Repeatability", 2.20081569959634,
     0.15292339602635, 4.61, 9.18, "Reproducibility", 0.917540376158099,
     0.15292339602635, 4.61, 9.18, "jaspColumn4", 0.917540376158099,
     3.29486067844547, 99.28, 197.69, "Part-to-part", 19.7691640706728,
     3.31874021504584, 100, 199.12, "Total variation", 19.9124412902751
    ))

  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_components-of-variation")

  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9, 765.700483091787, 97.8395061728395, 2.22178519889191e-21, 880.555555555556,
     "jaspColumn5", 2, 6.54347826086958, 0.836111111111114, 0.0073155788200197,
     1.67222222222223, "jaspColumn4", 18, 1.22946859903382, 0.157098765432099,
     0.268355463232599, 2.82777777777778, "jaspColumn5  *  jaspColumn4",
     60, 0.127777777777778, 7.66666666666667, "Repeatability", 89,
     892.722222222222, "Total"))

  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9, 727.192518087174, 97.8395061728395, 2.19585066080532e-71, 880.555555555556,
     "jaspColumn5", 2, 6.21439915299104, 0.836111111111114, 0.00313130866789578,
     1.67222222222223, "jaspColumn4", 78, 0.134544159544159, 10.4944444444444,
     "Repeatability", 89, 892.722222222222, "Total"))

  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_range-chart-by-operator")

})

