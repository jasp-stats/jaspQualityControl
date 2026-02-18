context("Example: AttributesAgreementAnalysisWideFormat")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("msaAttribute results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "AttributesAgreementAnalysisWideFormat.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("msaAttribute", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_AllVsStandard"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(21.267, 73.414, 15, 7, 46.67))

  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_Between"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(32.287, 83.664, 15, 9, 60))

  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_EachVsStandard"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("A", 51.911, 95.669, 15, 12, 80, "B", 44.9, 92.213, 15, 11, 73.33,
     "C", 32.287, 83.664, 15, 9, 60))

  plotName <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_PlotVs"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_each-appraiser-vs-standard")

  plotName <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_PlotWithin"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_within-appraisers")

  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_StudyEffectiveness"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("A", "80 (marginally acceptable)", "16.67 (unacceptable)", "20 (unacceptable)",
     "B", "73.33 (unacceptable)", "26.67 (unacceptable)", "20 (unacceptable)",
     "C", "60 (unacceptable)", "10 (marginally acceptable)", "66.67 (unacceptable)"
    ))

  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_Within"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("A", 68.052, 99.831, 15, 14, 93.33, "B", 68.052, 99.831, 15, 14,
     93.33, "C", 51.911, 95.669, 15, 12, 80))

  table <- results[["results"]][["cohensKappa"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("A", 0.612903225806452, "B", 0.492307692307692, "C", 0.264150943396226,
     "All", 0.466666666666667))

  table <- results[["results"]][["cohensKappaCor"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("-", 0.87, 0.6, "A", 0.87, "-", 0.52, "B", 0.6, 0.52, "-", "C"
    ))

  table <- results[["results"]][["fleissKappa"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("A", "", 0.612918660287081, 0.905462184873949, "B", "", 0.48608895977317,
     0.91, "C", "", 0.220151828847481, 0.543918918918919, "All",
     0.641666666666666, 0.439719816302578))

})

