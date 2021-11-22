context("Attribute Agreement Analysis")

options <- analysisOptions("msaAttribute")
options$AAAdataFormat <- "AAAlongFormat"
options$operators <- "Operator"
options$parts <- "Part"
options$measurementsLong <- c("Results")
options$standard <- "Reference"
options$AAAcohensKappa <- TRUE
options$AAAfleissKappa <- TRUE
options$PositiveRef <- "Yes"

results <- runAnalysis("msaAttribute", "AAALong.csv", options)

test_that("All Appraisers vs Standard table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_AllVsStandard"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(21.267, 73.414, 15, 7, 46.6666666666667))
})

test_that("Between Appraisers table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_Between"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(32.287, 83.664, 15, 9, 60))
})

test_that("Each Appraiser vs Standard table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_EachVsStandard"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("BL", 44.9, 92.213, 15, 11, 73.33, "EG", 51.911, 95.669, 15, 12,
                                      80, "MH", 32.287, 83.664, 15, 9, 60))
})

test_that("Each Appraiser vs Standard plot matches", {
  plotName <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_PlotVs"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "each-appraiser-vs-standard")
})

test_that("Within Appraisers plot matches", {
  plotName <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_PlotWithin"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "within-appraisers")
})

test_that("Study effectiveness summary table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_StudyEffectiveness"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("BL", "73.33 (Unacceptable)", "26.67 (Unacceptable)", "20 (Unacceptable)",
                                      "EG", "80 (Marginally acceptable)", "16.67 (Unacceptable)",
                                      "20 (Unacceptable)", "MH", "60 (Unacceptable)", "10 (Marginally acceptable)",
                                      "66.67 (Unacceptable)"))
})

test_that("Within Appraisers table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_Within"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("BL", 68.052, 99.831, 15, 14, 93.33, "EG", 68.052, 99.831, 15,
                                      14, 93.33, "MH", 51.911, 95.669, 15, 12, 80))
})

test_that("Cohen's Kappa for Appraiser vs Standard table results match", {
  table <- results[["results"]][["cohensKappa"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("BL", 0.492307692307692, "EG", 0.612903225806452, "MH", 0.264150943396226,
                                      "All", 0.466666666666667))
})

test_that("Cohen's Kappa correlations summary table results match", {
  table <- results[["results"]][["cohensKappaCor"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("-", 0.87, 0.52, "BL", 0.87, "-", 0.6, "EG", 0.52, 0.6, "-", "MH"
                                 ))
})

test_that("Fleiss' Kappa table results match", {
  table <- results[["results"]][["fleissKappa"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("BL", "", 0.48608895977317, 0.91, "EG", "", 0.612918660287081,
                                      0.905462184873949, "MH", "", 0.220151828847481, 0.543918918918919,
                                      "All", 0.641666666666666, 0.439719816302578))
})

# Wide
options$AAAdataFormat <- "AAAwideFormat"
options$operators <- "Operator"
options$parts <- "Part"
options$measurements <- c("Repeat.1", "Repeat.2", "Repeat.3")
options$standard <- "Reference"
results <- runAnalysis("msaAttribute", "AAARow.csv", options)

test_that("All Appraisers vs Standard table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_AllVsStandard"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(21.267, 73.414, 15, 7, 46.6666666666667))
})

test_that("Between Appraisers table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_Between"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(32.287, 83.664, 15, 9, 60))
})

test_that("Each Appraiser vs Standard table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_EachVsStandard"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("EG", 51.911, 95.669, 15, 12, 80, "BL", 44.9, 92.213, 15, 11,
                                      73.33, "MH", 32.287, 83.664, 15, 9, 60))
})

test_that("Each Appraiser vs Standard plot matches2", {
  plotName <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_PlotVs"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "each-appraiser-vs-standard2")
})

test_that("Within Appraisers plot matches2", {
  plotName <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_PlotWithin"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "within-appraisers2")
})

test_that("Study effectiveness summary table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_StudyEffectiveness"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("EG", "80 (Marginally acceptable)", "16.67 (Unacceptable)", "20 (Unacceptable)",
                                      "BL", "73.33 (Unacceptable)", "26.67 (Unacceptable)", "20 (Unacceptable)",
                                      "MH", "60 (Unacceptable)", "10 (Marginally acceptable)", "66.67 (Unacceptable)"
                                 ))
})

test_that("Within Appraisers table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_Within"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("EG", 68.052, 99.831, 15, 14, 93.33, "BL", 68.052, 99.831, 15,
                                      14, 93.33, "MH", 51.911, 95.669, 15, 12, 80))
})

test_that("Cohen's Kappa for Appraiser vs Standard table results match", {
  table <- results[["results"]][["cohensKappa"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("EG", 0.612903225806452, "BL", 0.492307692307692, "MH", 0.264150943396226,
                                      "All", 0.466666666666667))
})

test_that("Cohen's Kappa correlations summary table results match", {
  table <- results[["results"]][["cohensKappaCor"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.87, "-", 0.6, "EG", "-", 0.87, 0.52, "BL", 0.52, 0.6, "-", "MH"
                                 ))
})

test_that("Fleiss' Kappa table results match", {
  table <- results[["results"]][["fleissKappa"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("EG", "", 0.612918660287081, 0.905462184873949, "BL", "", 0.48608895977317,
                                      0.91, "MH", "", 0.220151828847481, 0.543918918918919, "All",
                                      0.641666666666666, 0.439719816302578))
})

# Tau
options$AAAdataFormat <- "AAAlongFormat"
options$operators <- "Operator"
options$parts <- "Sample"
options$measurementsLong <- c("Rating")
options$standard <- "Agreed"
options$AAAfleissKappa <- TRUE
options$PositiveRef <- ""
options$AAAkendallTau <- TRUE
results <- runAnalysis("msaAttribute", "msaTau.csv", options)

test_that("All Appraisers vs Standard table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_AllVsStandard"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3.755, 30.722, 30, 4, 13.3333333333333))
})

test_that("Between Appraisers table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_Between"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(25.461, 62.573, 30, 13, 43.3333333333333))
})

test_that("Each Appraiser vs Standard table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_EachVsStandard"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("A", 5.642, 34.721, 30, 5, 16.67, "B", 14.735, 49.396, 30, 9,
                                      30, "C", 7.714, 38.567, 30, 6, 20, "D", 12.279, 45.889, 30,
                                      8, 26.67))
})

test_that("Each Appraiser vs Standard plot matches3", {
  plotName <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_PlotVs"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "each-appraiser-vs-standard3")
})

test_that("Kendall's Tau table results match", {
  table <- results[["results"]][["KendallTau"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, -0.0576178502151195, 0.544077276765283, 0.603570260407359,
                                      0.784579319887982, "A", 0.544077276765283, -0.0320215763704757,
                                      1, 0.485871445981783, 0.604587661572944, "B", 0.603570260407359,
                                      0.0497740024706929, 0.485871445981783, 1, 0.474625719247113,
                                      "C", 0.784579319887982, -0.150970271219279, 0.604587661572944,
                                      0.474625719247113, 1, "D"))
})

test_that("Cohen's Kappa for Appraiser vs Standard table results match", {
  table <- results[["results"]][["cohensKappa"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("A", -0.085383502170767, "B", 0.0247678018575851, "C", -0.00278551532033424,
                                      "D", -0.0248447204968944, "All", -0.0318721375829516))
})

test_that("Cohen's Kappa correlations summary table results match", {
  table <- results[["results"]][["cohensKappaCor"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("-", 0.6, 0.63, 0.86, "A", 0.6, "-", 0.61, 0.63, "B", 0.63, 0.61,
                                      "-", 0.52, "C", 0.86, 0.63, 0.52, "-", "D"))
})

test_that("Fleiss' Kappa table results match", {
  table <- results[["results"]][["fleissKappa"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("A", "", -0.167315175097276, "NaN", "B", "", -0.0413223140495868,
                                      "NaN", "C", "", -0.0580455547391623, "NaN", "D", "", -0.113924050632911,
                                      "NaN", "All", 0.405850091407678, -0.0951517736297342))
})

