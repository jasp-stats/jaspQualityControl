context("Example: LinearityStudy")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("msaGaugeLinearity results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "LinearityStudy.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("msaGaugeLinearity", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["LB"]][["collection"]][["LB_plotBias"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_bias-and-linearity")

  plotName <- results[["results"]][["LB"]][["collection"]][["LB_plotProcessVar"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_percentage-process-variation-graph")

  table <- results[["results"]][["LB"]][["collection"]][["LB_tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Bias = 0.74 - 0.13 * Reference value"))

  table <- results[["results"]][["LB"]][["collection"]][["LB_tableGaugeBias"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.491666666666667, 2.49166666666667, 1, 8.19444444444444, 2.87233310444284e-08,
     2, 0.125, 4.125, 2, 2.08333333333333, 0.353991325267683, 4,
     0.0250000000000004, 6.025, 3, 0.416666666666673, 0.667130710762814,
     6, -0.291666666666667, 7.70833333333333, 4, 4.86111111111112,
     6.41948050554358e-07, 8, -0.616666666666667, 9.38333333333333,
     5, 10.2777777777778, 1.55444480038029e-08, 10, -0.0533333333333334,
     "Total", 0.888888888888889, 0.0899196050780288))

  table <- results[["results"]][["LB"]][["collection"]][["LB_tableGaugeLinearity"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.239539788646537, 0.79, 13.1666666666667, 0.714318415932242
    ))

  table <- results[["results"]][["LB"]][["collection"]][["LB_tableRegression"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0725242725916125, 10.1575188601321, 0.736666666666666, "Intercept",
     1.73379959546227e-14, 0.0109334454718107, -12.0425594114992,
     -0.131666666666667, "Slope", 2.03771558228573e-17))

})

