context("Attribute Agreement Analysis")

# Long format ####

## AAA and Fleiss' Kappa ####

options <- analysisOptions("msaAttribute")
options$dataFormat <- "longFormat"
options$operatorLongFormat <- "Operator"
options$partLongFormat <- "Part"
options$measurementLongFormat <- c("Results")
options$standardLongFormat <- "Reference"
options$cohensKappa <- TRUE
options$fleissKappa <- TRUE
options$positiveReference <- "Yes"
results <- runAnalysis("msaAttribute", "datasets/msaAttributeAgreement/msaAttributeKappa_long.csv", options)

test_that("LF1.1 AAA and Fleiss - All Appraisers vs Standard table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_AllVsStandard"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(21.267, 73.414, 15, 7, 46.67))
})

test_that("LF1.2 AAA and Fleiss - Between Appraisers table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_Between"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(32.287, 83.664, 15, 9, 60))
})

test_that("LF1.3 AAA and Fleiss - Each Appraiser vs Standard table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_EachVsStandard"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("BL", 44.9, 92.213, 15, 11, 73.33, "EG", 51.911, 95.669, 15, 12,
                                      80, "MH", 32.287, 83.664, 15, 9, 60))
})

test_that("LF1.4 AAA and Fleiss - Each Appraiser vs Standard plot matches", {
  plotName <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_PlotVs"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_each-appraiser-vs-standard")
})

test_that("LF1.5 AAA and Fleiss - Within Appraisers plot matches", {
  plotName <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_PlotWithin"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_within-appraisers")
})

test_that("LF1.6 AAA and Fleiss - Study effectiveness summary table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_StudyEffectiveness"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("BL", "73.33 (unacceptable)", "26.67 (unacceptable)", "20 (unacceptable)",
                                      "EG", "80 (marginally acceptable)", "16.67 (unacceptable)",
                                      "20 (unacceptable)", "MH", "60 (unacceptable)", "10 (marginally acceptable)",
                                      "66.67 (unacceptable)"))
})

test_that("LF1.7 AAA and Fleiss - Within Appraisers table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_Within"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("BL", 68.052, 99.831, 15, 14, 93.33, "EG", 68.052, 99.831, 15,
                                      14, 93.33, "MH", 51.911, 95.669, 15, 12, 80))
})

test_that("LF1.8 AAA and Fleiss - Cohen's Kappa for Appraiser vs Standard table results match", {
  table <- results[["results"]][["cohensKappa"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("BL", 0.492307692307692, "EG", 0.612903225806452, "MH", 0.264150943396226,
                                      "All", 0.466666666666667))
})

test_that("LF1.9 AAA and Fleiss - Cohen's Kappa correlations summary table results match", {
  table <- results[["results"]][["cohensKappaCor"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("-", 0.87, 0.52, "BL", 0.87, "-", 0.6, "EG", 0.52, 0.6, "-", "MH"
                                 ))
})

test_that("LF1.10 AAA and Fleiss - Fleiss' Kappa table results match", {
  table <- results[["results"]][["fleissKappa"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("BL", "", 0.48608895977317, 0.91, "EG", "", 0.612918660287081,
                                      0.905462184873949, "MH", "", 0.220151828847481, 0.543918918918919,
                                      "All", 0.641666666666666, 0.439719816302578))
})

## Kendall's Tau ####
options <- analysisOptions("msaAttribute")
options$dataFormat <- "longFormat"
options$operatorLongFormat <- "Operator"
options$partLongFormat <- "Sample"
options$measurementLongFormat <- c("Rating")
options$standardLongFormat <- "Agreed"
options$fleissKappa <- TRUE
options$cohensKappa <- TRUE
options$positiveReference <- ""
options$kendallsTau <- TRUE
results <- runAnalysis("msaAttribute", "datasets/msaAttributeAgreement/msaAttributeTau_long.csv", options)

test_that("LF2.1 Kendall Tau - All appraisers vs standard table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_AllVsStandard"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(25.461, 62.573, 30, 13, 43.33))
})

test_that("LF2.2 Kendall Tau - Between appraisers table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_Between"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(25.461, 62.573, 30, 13, 43.33))
})

test_that("LF2.3 Kendall Tau - Each appraiser vs standard table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_EachVsStandard"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("A", 50.604, 85.265, 30, 21, 70, "B", 50.604, 85.265, 30, 21,
                                      70, "C", 47.188, 82.713, 30, 20, 66.67, "D", 47.188, 82.713,
                                      30, 20, 66.67))
})

test_that("LF2.4 Kendall Tau - Each Appraiser vs Standard plot matches", {
  plotName <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_PlotVs"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF2_each-appraiser-vs-standard")
})

test_that("LF2.5 Kendall Tau - Kendall's tau table results match", {
  table <- results[["results"]][["KendallTau"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 0.790897539450037, 0.544077276765283, 0.603570260407359, 0.784579319887982,
                                      "A", 0.544077276765283, 0.818329173912156, 1, 0.485871445981783,
                                      0.604587661572944, "B", 0.603570260407359, 0.573358877643966,
                                      0.485871445981783, 1, 0.474625719247113, "C", 0.784579319887982,
                                      0.763623484279901, 0.604587661572944, 0.474625719247113, 1,
                                      "D"))
})

test_that("LF2.6 Kendall Tau - Cohen's kappa for appraiser vs standard table results match", {
  table <- results[["results"]][["cohensKappa"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("A", 0.540816326530612, "B", 0.537671232876712, "C", 0.523052464228935,
                                      "D", 0.456521739130435, "All", 0.515512112197195))
})

test_that("LF2.7 Kendall Tau - Cohen's kappa correlations summary table results match", {
  table <- results[["results"]][["cohensKappaCor"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("-", 0.6, 0.63, 0.86, "A", 0.6, "-", 0.61, 0.63, "B", 0.63, 0.61,
                                      "-", 0.52, "C", 0.86, 0.63, 0.52, "-", "D"))
})

test_that("LF2.8 Kendall Tau - Fleiss' kappa table results match", {
  table <- results[["results"]][["fleissKappa"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("A", "", 0.532871972318339, "NaN", "B", "", 0.529616724738676,
                                      "NaN", "C", "", 0.52, "NaN", "D", "", 0.440820130475303, "NaN",
                                      "All", 0.405850091407678, 0.505827206883079))
})

## Rows in run order ####
# Agreement between appraisers matches the appraisers' ratings by part, not by row position (INTERNAL-jasp#2812).
# Run order: each trial round rates the parts in a random order, appraisers interleaved. Trial k of a part
# stays the k-th row of that part, as the Cohen's kappa correlations pair trial k across appraisers.
options <- analysisOptions("msaAttribute")
options$dataFormat <- "longFormat"
options$operatorLongFormat <- "Operator"
options$partLongFormat <- "Part"
options$measurementLongFormat <- c("Results")
options$standardLongFormat <- "Reference"
options$cohensKappa <- TRUE
options$fleissKappa <- TRUE
options$positiveReference <- "Yes"
dataset <- read.csv("datasets/msaAttributeAgreement/msaAttributeKappa_long.csv")
dataset <- dataset[c("Part", "Operator", "Results", "Reference")]
trial <- ave(seq_len(nrow(dataset)), dataset$Operator, dataset$Part, FUN = seq_along)
set.seed(1)
runOrder <- order(trial, sample(nrow(dataset)))
results <- runAnalysis("msaAttribute", dataset[runOrder, ], options)

test_that("LF3.1 Rows in run order - All appraisers vs standard table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_AllVsStandard"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(21.267, 73.414, 15, 7, 46.67))
})

test_that("LF3.2 Rows in run order - Between appraisers table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_Between"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(32.287, 83.664, 15, 9, 60))
})

test_that("LF3.3 Rows in run order - Cohen's kappa correlations summary table results match", {
  table <- results[["results"]][["cohensKappaCor"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("-", 0.87, 0.52, "BL", 0.87, "-", 0.6, "EG", 0.52, 0.6, "-", "MH"
                                 ))
})

test_that("LF3.4 Rows in run order - Fleiss' kappa table results match", {
  table <- results[["results"]][["fleissKappa"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("BL", "", 0.48608895977317, 0.91, "EG", "", 0.612918660287081,
                                      0.905462184873949, "MH", "", 0.220151828847481, 0.543918918918919,
                                      "All", 0.641666666666666, 0.439719816302578))
})

## Invalid designs ####
options <- analysisOptions("msaAttribute")
options$dataFormat <- "longFormat"
options$operatorLongFormat <- "Operator"
options$partLongFormat <- "Part"
options$measurementLongFormat <- c("Results")
options$standardLongFormat <- "Reference"
options$fleissKappa <- TRUE
dataset <- read.csv("datasets/msaAttributeAgreement/msaAttributeKappa_long.csv")
dataset <- dataset[c("Part", "Operator", "Results", "Reference")]
rowsMH <- which(dataset$Operator == "MH")
rowsMHPart1 <- rowsMH[dataset$Part[rowsMH] == 1]
rowsMHPart2 <- rowsMH[dataset$Part[rowsMH] == 2]
unbalancedMessage <- "Each appraiser must rate each part, and each part the same number of times."

test_that("LF4.1 Appraiser rates some parts fewer times gives an error", {
  # MH rates parts 1 to 3 twice, the other parts three times.
  results <- runAnalysis("msaAttribute", dataset[-rowsMH[match(1:3, dataset$Part[rowsMH])], ], options)
  expect_identical(results[["status"]], "validationError")
  expect_identical(results[["results"]][["errorMessage"]], unbalancedMessage)
})

test_that("LF4.2 Appraiser rates a part fewer and another part more often gives an error", {
  # MH rates part 1 twice, part 2 four times.
  results <- runAnalysis("msaAttribute", dataset[c(setdiff(seq_len(nrow(dataset)), rowsMHPart1[1]), rowsMHPart2[1]), ], options)
  expect_identical(results[["status"]], "validationError")
  expect_identical(results[["results"]][["errorMessage"]], unbalancedMessage)
})

test_that("LF4.3 Appraiser rates a part more often gives an error", {
  # MH rates part 1 four times.
  results <- runAnalysis("msaAttribute", dataset[c(seq_len(nrow(dataset)), rowsMHPart1[1]), ], options)
  expect_identical(results[["status"]], "validationError")
  expect_identical(results[["results"]][["errorMessage"]], unbalancedMessage)
})

test_that("LF4.4 Reference that differs between trials gives an error", {
  differentReference <- dataset
  differentReference$Reference[rowsMHPart1[1]] <- "No"
  results <- runAnalysis("msaAttribute", differentReference, options)
  expect_identical(results[["status"]], "validationError")
  expect_identical(results[["results"]][["errorMessage"]], "Each part must have a single reference value.")
})

# Wide Format ####

## AAA and Fleiss' Kappa ####
options <- analysisOptions("msaAttribute")
options$dataFormat <- "wideFormat"
options$operatorWideFormat <- "Operator"
options$partWideFormat <- "Part"
options$measurementsWideFormat <- c("Repeat.1", "Repeat.2", "Repeat.3")
options$standardWideFormat <- "Reference"
options$fleissKappa <- TRUE
results <- runAnalysis("msaAttribute", "datasets/msaAttributeAgreement/msaAttributeKappa_wide.csv", options)

test_that("WF1.1 AAA and Fleiss - All appraisers vs standard table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_AllVsStandard"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(21.267, 73.414, 15, 7, 46.67))
})

test_that("WF1.2 AAA and Fleiss - Between appraisers table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_Between"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(32.287, 83.664, 15, 9, 60))
})

test_that("WF1.3 AAA and Fleiss - Each appraiser vs standard table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_EachVsStandard"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("EG", 51.911, 95.669, 15, 12, 80, "BL", 44.9, 92.213, 15, 11,
                                      73.33, "MH", 32.287, 83.664, 15, 9, 60))
})

test_that("WF1.4 AAA and Fleiss - Each Appraiser vs Standard plot matches", {
  plotName <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_PlotVs"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_each-appraiser-vs-standard")
})

test_that("WF1.5 AAA and Fleiss - Within Appraisers plot matches", {
  plotName <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_PlotWithin"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_within-appraisers")
})

test_that("WF1.6 AAA and Fleiss - Within appraisers table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_Within"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("EG", 68.052, 99.831, 15, 14, 93.33, "BL", 68.052, 99.831, 15,
                                      14, 93.33, "MH", 51.911, 95.669, 15, 12, 80))
})

test_that("WF1.7 AAA and Fleiss - Fleiss' kappa table results match", {
  table <- results[["results"]][["fleissKappa"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("EG", "", 0.612918660287081, 0.905462184873949, "BL", "", 0.48608895977317,
                                      0.91, "MH", "", 0.220151828847481, 0.543918918918919, "All",
                                      0.641666666666666, 0.439719816302578))
})

## Rows in run order ####
# Agreement between appraisers matches the appraisers' ratings by part, not by row position (INTERNAL-jasp#2812).
options <- analysisOptions("msaAttribute")
options$dataFormat <- "wideFormat"
options$operatorWideFormat <- "Operator"
options$partWideFormat <- "Part"
options$measurementsWideFormat <- c("Repeat.1", "Repeat.2", "Repeat.3")
options$standardWideFormat <- "Reference"
options$fleissKappa <- TRUE
dataset <- read.csv("datasets/msaAttributeAgreement/msaAttributeKappa_wide.csv")
dataset <- dataset[c("Operator", "Part", "Repeat.1", "Repeat.2", "Repeat.3", "Reference")]
runOrder <- c(17, 44, 3, 29, 8, 36, 21, 12, 40, 1, 33, 25, 6, 45, 19, 10, 38, 27, 14, 2, 31, 42, 23,
              9, 35, 16, 4, 28, 41, 11, 22, 37, 5, 30, 18, 43, 13, 26, 7, 39, 20, 34, 15, 24, 32)
results <- runAnalysis("msaAttribute", dataset[runOrder, ], options)

test_that("WF2.1 Rows in run order - All appraisers vs standard table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_AllVsStandard"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(21.267, 73.414, 15, 7, 46.67))
})

test_that("WF2.2 Rows in run order - Between appraisers table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_Between"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(32.287, 83.664, 15, 9, 60))
})

test_that("WF2.3 Rows in run order - Fleiss' kappa table results match", {
  table <- results[["results"]][["fleissKappa"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("BL", "", 0.48608895977317, 0.91, "MH", "", 0.220151828847481,
                                      0.543918918918919, "EG", "", 0.612918660287081, 0.905462184873949,
                                      "All", 0.641666666666666, 0.439719816302578))
})

## Invalid designs ####
options <- analysisOptions("msaAttribute")
options$dataFormat <- "wideFormat"
options$operatorWideFormat <- "Operator"
options$partWideFormat <- "Part"
options$measurementsWideFormat <- c("Repeat.1", "Repeat.2", "Repeat.3")
options$standardWideFormat <- "Reference"

test_that("WF3.1 Unbalanced design gives an error", {
  results <- runAnalysis("msaAttribute", dataset[-1, ], options)
  expect_identical(results[["status"]], "validationError")
  expect_identical(results[["results"]][["errorMessage"]],
                   "Each appraiser must rate each part, and each part the same number of times.")
})

test_that("WF3.2 More than one row per appraiser and part gives an error", {
  results <- runAnalysis("msaAttribute", rbind(dataset, dataset), options)
  expect_identical(results[["status"]], "validationError")
  expect_identical(results[["results"]][["errorMessage"]], "Each appraiser must have exactly one row per part.")
})

test_that("WF3.3 Reference that differs between appraisers gives an error", {
  differentReference <- dataset
  differentReference$Reference[differentReference$Operator == "MH" & differentReference$Part == 1] <- "No"
  results <- runAnalysis("msaAttribute", differentReference, options)
  expect_identical(results[["status"]], "validationError")
  expect_identical(results[["results"]][["errorMessage"]], "Each part must have a single reference value.")
})

## Kendall's tau, rows in run order ####
# One trial: each appraiser rates the samples in a random order (INTERNAL-jasp#2812).
options <- analysisOptions("msaAttribute")
options$dataFormat <- "wideFormat"
options$operatorWideFormat <- "Operator"
options$partWideFormat <- "Sample"
options$measurementsWideFormat <- "Rating"
options$standardWideFormat <- "Agreed"
options$fleissKappa <- TRUE
options$cohensKappa <- TRUE
options$positiveReference <- ""
options$kendallsTau <- TRUE
dataset <- read.csv("datasets/msaAttributeAgreement/msaAttributeTau_long.csv")
dataset <- dataset[c("Operator", "Sample", "Rating", "Agreed")]
set.seed(1)
runOrder <- order(dataset$Operator, sample(nrow(dataset)))
results <- runAnalysis("msaAttribute", dataset[runOrder, ], options)

test_that("WF4.1 Kendall Tau, rows in run order - Between appraisers table results match", {
  table <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_Between"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(25.461, 62.573, 30, 13, 43.33))
})

test_that("WF4.2 Kendall Tau, rows in run order - Kendall's tau table results match", {
  table <- results[["results"]][["KendallTau"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 0.790897539450037, 0.544077276765283, 0.603570260407359, 0.784579319887982,
                                      "A", 0.544077276765283, 0.818329173912156, 1, 0.485871445981783,
                                      0.604587661572944, "B", 0.603570260407359, 0.573358877643966,
                                      0.485871445981783, 1, 0.474625719247113, "C", 0.784579319887982,
                                      0.763623484279901, 0.604587661572944, 0.474625719247113, 1,
                                      "D"))
})

test_that("WF4.3 Kendall Tau, rows in run order - Cohen's kappa correlations summary table results match", {
  table <- results[["results"]][["cohensKappaCor"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("-", 0.6, 0.63, 0.86, "A", 0.6, "-", 0.61, 0.63, "B", 0.63, 0.61,
                                      "-", 0.52, "C", 0.86, 0.63, 0.52, "-", "D"))
})

test_that("WF4.4 Kendall Tau, rows in run order - Fleiss' kappa table results match", {
  table <- results[["results"]][["fleissKappa"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("A", "", 0.532871972318339, "NaN", "B", "", 0.529616724738676,
                                      "NaN", "C", "", 0.52, "NaN", "D", "", 0.440820130475303, "NaN",
                                      "All", 0.405850091407678, 0.505827206883079))
})
