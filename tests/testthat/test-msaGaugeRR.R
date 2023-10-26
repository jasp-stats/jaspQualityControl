context("[Quality Control] Gauge r&R")
.numDecimals <- 2

# Long format
options <- analysisOptions("msaGaugeRR")
options$operator <- "Operators"
options$part <- "Parts"
options$measurementLongFormat <- "Dm"
options$tolerance <- TRUE
options$toleranceValue <- 15
options$rChart <- TRUE
options$xBarChart <- TRUE
options$scatterPlot <- TRUE
options$scatterPlotFitLine <- TRUE
options$scatterPlotOriginLine <- TRUE
options$partMeasurementPlot <- TRUE
options$partMeasurementPlotAllValues <- TRUE
options$operatorMeasurementPlot <- TRUE
options$partByOperatorMeasurementPlot <- TRUE
options$trafficLightChart <- TRUE
set.seed(1)
results <- runAnalysis("msaGaugeRR", "msaGageRandr_long.csv", options)

test_that("Variance Components table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.43, "Total Gauge r&amp;R", 0.157929724596391, 1.22, "Repeatability",
                                      0.134544159544159, 0.21, "Reproducibility", 0.0233855650522318,
                                      0.21, "Operators", 0.0233855650522318, 98.57, "Part-to-Part",
                                      10.8561068903661, 100, "Total Variation", 11.0140366149625
                                 ))
})

test_that("Gauge Evaluation table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.397403729972922, 11.97, 15.9, "Total Gauge r&amp;R", 2.38442237983753,
                                      0.36680261659939, 11.05, 14.67, "Repeatability", 2.20081569959634,
                                      0.15292339602635, 4.61, 6.12, "Reproducibility", 0.917540376158099,
                                      0.15292339602635, 4.61, 6.12, "Operators", 0.917540376158099,
                                      3.29486067844547, 99.28, 131.79, "Part-to-Part", 19.7691640706728,
                                      3.31874021504585, 100, 132.75, "Total Variation", 19.9124412902751
                                 ))
})

test_that("Components of Variation plot matches", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "components-of-variation")
})

test_that("Two-way ANOVA Table with Interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 765.700483091787, 97.8395061728395, 2.22178519889191e-21, 880.555555555556,
                                      "Parts", 2, 6.54347826086958, 0.836111111111114, 0.0073155788200197,
                                      1.67222222222223, "Operators", 18, 1.22946859903382, 0.157098765432099,
                                      0.268355463232599, 2.82777777777778, "Parts  *  Operators",
                                      60, 0.127777777777778, 7.66666666666667, "Repeatability", 89,
                                      892.722222222222, "Total"))
})

test_that("Two-way ANOVA Table without Interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 727.192518087174, 97.8395061728395, 2.19585066080531e-71, 880.555555555556,
                                      "Parts", 2, 6.21439915299104, 0.836111111111114, 0.00313130866789578,
                                      1.67222222222223, "Operators", 78, 0.134544159544159, 10.4944444444444,
                                      "Repeatability", 89, 892.722222222222, "Total"))
})

test_that("Part by Operator Interaction plot matches", {
  plotName <- results[["results"]][["gaugeByInteraction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "part-by-operator-interaction")
})

test_that("Measurements by Operator plot matches", {
  plotName <- results[["results"]][["gaugeByOperator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "measurements-by-operator")
})

test_that("Measurements by Part plot matches", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "measurements-by-part")
})

test_that("Range chart by operator plot matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "range-chart-by-operator")
})

test_that("Matrix Plot for Operators matches", {
  plotName <- results[["results"]][["gaugeScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "matrix-plot-for-operators")
})

test_that("Average chart by operator plot matches", {
  plotName <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "average-chart-by-operator")
})

test_that("Test results for x-bar chart table results match", {
  table <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Operator A", 1, "", 2, "", 3, "", 4, "", 5, "", 6, "", 7, "",
                                      8, "", 9, "", 10, "Operator B", 1, "", 2, "", 3, "", 4, "",
                                      5, "", 6, "", 7, "", 8, "", 9, "", 10, "Operator C", 1, "",
                                      2, "", 3, "", 4, "", 5, "", 6, "", 7, "", 8, "", 9, "", 10
                                 ))
})

# Wide
options$dataFormat <- "wideFormat"
options$operator <- "Operator"
options$part <- "Part"
options$measurementsWideFormat <- c("Measurement1", "Measurement2", "Measurement3")
set.seed(1)
results <- runAnalysis("msaGaugeRR", "msaGageRandr_wide.csv", options)

test_that("Variance Components table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(22.56, "Total Gauge r&amp;R", 0.848225424491385, 22.54, "Repeatability",
                                      0.847522164107094, 0.02, "Reproducibility", 0.000703260384290564,
                                      0.02, "Operator", 0.000703260384290564, 77.44, "Part-to-Part",
                                      2.91190554000432, 100, "Total Variation", 3.7601309644957))
})

test_that("Gauge Evaluation table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.920991544201892, 47.5, 36.84, "Total Gauge r&amp;R", 5.52594926521135,
                                      0.920609669787959, 47.48, 36.82, "Repeatability", 5.52365801872775,
                                      0.0265190570022873, 1.37, 1.06, "Reproducibility", 0.159114342013724,
                                      0.0265190570022873, 1.37, 1.06, "Operator", 0.159114342013724,
                                      1.70643064318604, 88, 68.26, "Part-to-Part", 10.2385838591162,
                                      1.93910571256332, 100, 77.56, "Total Variation", 11.6346342753799
                                 ))
})

test_that("Components of Variation plot matches", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "components-of-variation-Wide")
})

test_that("Two-way ANOVA Table with Interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 28.837403998517, 27.0546720241459, 6.66730220666189e-09, 243.492048217314,
                                      "Part", 2, 0.925856544711989, 0.868619975635811, 0.41425774549038,
                                      1.73723995127162, "Operator", 18, 0.581263589632298, 0.545329800764601,
                                      0.89932032121579, 9.81593641376281, "Part  *  Operator", 60,
                                      0.938179873109842, 56.2907923865906, "Repeatability", 89, 311.336016968939,
                                      "Total"))
})

test_that("Two-way ANOVA Table without Interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 31.9220820055477, 27.0546720241459, 1.20668982102463e-22, 243.492048217314,
                                      "Part", 2, 1.02489352187142, 0.868619975635811, 0.363615551786533,
                                      1.73723995127162, "Operator", 78, 0.847522164107094, 66.1067288003534,
                                      "Repeatability", 89, 311.336016968939, "Total"))
})

test_that("Part by Operator Interaction plot matches", {
  plotName <- results[["results"]][["gaugeByInteraction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "part-by-operator-interaction-Wide")
})

test_that("Measurements by Operator plot matches", {
  plotName <- results[["results"]][["gaugeByOperator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "measurements-by-operator-Wide")
})

test_that("Measurements by Part plot matches", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "measurements-by-part-Wide")
})

test_that("Range chart by operator plot matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "range-chart-by-operator-Wide")
})

test_that("Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("C", 10))
})

test_that("Matrix Plot for Operators matches", {
  plotName <- results[["results"]][["gaugeScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "matrix-plot-for-operators-Wide")
})

test_that("Average chart by operator plot matches", {
  plotName <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "average-chart-by-operator-Wide")
})

test_that("Test results for x-bar chart table results match", {
  table <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("A", 1, "", 5, "", 8, "", 10, "B", 1, "", 5, "", 8, "", 10, "C",
                                      1, "", 3, "", 5, "", 10))
})

# Long: Type 3
options <- analysisOptions("msaGaugeRR")
data <- read.csv("msaGaugeRR_Type3_Long.csv")
options$dataFormat <- "longFormat"
names(data)[1] <- "Parts"
options$operators <- ""
options$parts <- "Parts"
options$measurementsLong <- "dm"
options$Type3 <- TRUE
options$gaugeToleranceEnabled <- TRUE
options$tolerance <- 12
options$TypeForFstat <- 'RandomEffects'
options$gaugeByPart <- TRUE
set.seed(1)
results <- runAnalysis("msaGaugeRR", data, options)

test_that("Variance Components table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4.53, "Total Gauge r&amp;R", 0.176833333333333, 4.53, "Repeatability",
                                      0.176833333333333, 95.47, "Part-to-Part", 3.73036549707602,
                                      100, "Total Variation", 3.90719883040936))
})

test_that("Gauge Evaluation table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.420515556588972, 21.27, 21.03, "Total Gauge r&amp;R", 2.52309333953384,
                                      0.420515556588972, 21.27, 21.03, "Repeatability", 2.52309333953384,
                                      1.93141541287109, 97.71, 96.57, "Part-to-Part", 11.5884924772266,
                                      1.97666356024726, 100, 98.83, "Total Variation", 11.8599813614835
                                 ))
})

test_that("Components of Variation plot matches", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "components-of-variation-LongType3")
})

test_that("One-way ANOVA Table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(19, 64.2861253038345, 11.3679298245614, 4.58619746938796e-24,
                                      215.990666666667, "Parts", 40, 0.176833333333333, 7.07333333333333,
                                      "Repeatability", 59, 223.064, "Total"))
})

test_that("Measurements by Part plot matches", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "measurements-by-part-LongType3")
})

# Type 3 WIDE
options$operator <- ""
options$dataFormat <- "wideFormat"
options$part <- "Part"
options$measurementsWideFormat <- c("Repeat.1", "Repeat.2", "Repeat.3")
options$type3 <- TRUE
options$anovaModelType <- "randomEffect"
options$scatterPlot <- FALSE
options$scatterPlotFitLine <- FALSE
set.seed(1)
results <- runAnalysis("msaGaugeRR", "msaGaugeRR_Type3_Wide.csv", options)

test_that("Variance Components table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4.53, "Total Gauge r&amp;R", 0.176833333333333, 4.53, "Repeatability",
                                      0.176833333333333, 95.47, "Part-to-Part", 3.73036549707602,
                                      100, "Total Variation", 3.90719883040936))
})

test_that("Components of Variation plot matches", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "components-of-variation-WideType3")
})

test_that("One-way ANOVA Table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(19, 64.2861253038345, 11.3679298245614, 4.58619746938796e-24,
                                      215.990666666667, "Part", 40, 0.176833333333333, 7.07333333333333,
                                      "Repeatability", 59, 223.064, "Total"))
})

test_that("Measurements by Part plot matches", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "measurements-by-part-WideType3")
})
