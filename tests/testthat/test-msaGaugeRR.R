context("[Quality Control] Gauge r&R")
.numDecimals <- 2

# Long format ####

## Default settings ####
options <- analysisOptions("msaGaugeRR")
options$operatorLongFormat <- "Operators"
options$partLongFormat <- "Parts"
options$measurementLongFormat <- "Dm"
options$tolerance <- TRUE
options$toleranceValue <- 10
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
options$anovaModelType <- "randomEffect"
set.seed(1)
results <- runAnalysis("msaGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options)

test_that("LF1.1 Default settings - Variance components table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.43, "Total gauge r&amp;R", 0.157929724596391, 1.22, "Repeatability",
                                      0.134544159544159, 0.21, "Reproducibility", 0.0233855650522318,
                                      0.21, "Operators", 0.0233855650522318, 98.57, "Part-to-part",
                                      10.8561068903661, 100, "Total variation", 11.0140366149625
                                 ))
})

test_that("LF1.2 Default settings - Gauge evaluation table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.397403729972922, 11.97, 23.84, "Total gauge r&amp;R", 2.38442237983753,
                                      0.36680261659939, 11.05, 22.01, "Repeatability", 2.20081569959634,
                                      0.15292339602635, 4.61, 9.18, "Reproducibility", 0.917540376158099,
                                      0.15292339602635, 4.61, 9.18, "Operators", 0.917540376158099,
                                      3.29486067844547, 99.28, 197.69, "Part-to-part", 19.7691640706728,
                                      3.31874021504584, 100, 199.12, "Total variation", 19.9124412902751
                                 ))
})

test_that("LF1.3 Default settings - Components of variation plot matches", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "1_components-of-variation")
})

test_that("LF1.4 Default settings - Two-way ANOVA table with interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 622.789783889979, 97.8395061728395, 1.41581280737464e-20, 880.555555555556,
                                      "Parts", 2, 5.32220039292731, 0.836111111111114, 0.0152789978971193,
                                      1.67222222222223, "Operators", 18, 1.22946859903382, 0.157098765432099,
                                      0.268355463232599, 2.82777777777778, "Parts  *  Operators",
                                      60, 0.127777777777778, 7.66666666666667, "Repeatability", 89,
                                      892.722222222222, "Total"))
})

test_that("LF1.5 Default settings - Two-way ANOVA table without interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 727.192518087174, 97.8395061728395, 2.19585066080532e-71, 880.555555555556,
                                      "Parts", 2, 6.21439915299104, 0.836111111111114, 0.00313130866789578,
                                      1.67222222222223, "Operators", 78, 0.134544159544159, 10.4944444444444,
                                      "Repeatability", 89, 892.722222222222, "Total"))
})

test_that("LF1.6 Default settings - Part by operator interaction plot matches", {
  plotName <- results[["results"]][["gaugeByInteraction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "1_part-by-operator-interaction")
})

test_that("LF1.7 Default settings - Measurements by operator plot matches", {
  plotName <- results[["results"]][["gaugeByOperator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "1_measurements-by-operator")
})

test_that("LF1.8 Default settings - Measurements by part plot matches", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "1_measurements-by-part")
})

test_that("LF1.9 Default settings - Matrix plot for operators matches", {
  plotName <- results[["results"]][["gaugeScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "1_matrix-plot-for-operators")
})

test_that("LF1.10 Default settings - Range chart by operator plot matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "1_range-chart-by-operator")
})

test_that("LF1.11 Default settings - Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("LF1.12 Default settings - Traffic plot matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "1_traffic-plot")
})

test_that("LF1.13 Default settings - Average chart by operator plot matches", {
  plotName <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "1_average-chart-by-operator")
})

test_that("LF1.14 Default settings - Test results for x-bar chart table results match", {
  table <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Operator A", "Point 1", "", "Point 2", "", "Point 3", "", "Point 4",
                                      "", "Point 5", "", "Point 6", "", "Point 7", "", "Point 8",
                                      "", "Point 9", "", "Point 10", "Operator B", "Point 1", "",
                                      "Point 2", "", "Point 3", "", "Point 4", "", "Point 5", "",
                                      "Point 6", "", "Point 7", "", "Point 8", "", "Point 9", "",
                                      "Point 10", "Operator C", "Point 1", "", "Point 2", "", "Point 3",
                                      "", "Point 4", "", "Point 5", "", "Point 6", "", "Point 7",
                                      "", "Point 8", "", "Point 9", "", "Point 10"))
})

## Historical std. dev ####

options <- analysisOptions("msaGaugeRR")
options$operatorLongFormat <- "Operators"
options$partLongFormat <- "Parts"
options$measurementLongFormat <- "Dm"
options$tolerance <- TRUE
options$toleranceValue <- 10
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
options$anovaModelType <- "randomEffect"
options$processVariationReference <- "historicalSd"
options$historicalSdValue <- 3
set.seed(1)
results <- runAnalysis("msaGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options)

test_that("LF2.1 Historical std. dev. - Variance components table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.75, "Total gauge r&amp;R", 0.157929724596391, 1.49, "Repeatability",
                                      0.134544159544159, 0.26, "Reproducibility", 0.0233855650522318,
                                      0.26, "Operators", 0.0233855650522318, 98.25, "Part-to-part",
                                      8.84207027540361, 100, "Total variation", 9))
})

test_that("LF2.2 Historical std. dev. - Gauge evaluation table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.397403729972922, 13.25, 23.84, "Total gauge r&amp;R", 2.38442237983753,
                                      0.36680261659939, 12.23, 22.01, "Repeatability", 2.20081569959634,
                                      0.15292339602635, 5.1, 9.18, "Reproducibility", 0.917540376158099,
                                      0.15292339602635, 5.1, 9.18, "Operators", 0.917540376158099,
                                      2.97356188356718, 99.12, 178.41, "Part-to-part", 17.8413713014031,
                                      3, 100, 180, "Total variation", 18))
})

test_that("LF2.3 Historical std. dev. - Components of variation plot matches", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "2_components-of-variation")
})

test_that("LF2.4 Historical std. dev. - Two-way ANOVA table with interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 622.789783889979, 97.8395061728395, 1.41581280737464e-20, 880.555555555556,
                                      "Parts", 2, 5.32220039292731, 0.836111111111114, 0.0152789978971193,
                                      1.67222222222223, "Operators", 18, 1.22946859903382, 0.157098765432099,
                                      0.268355463232599, 2.82777777777778, "Parts  *  Operators",
                                      60, 0.127777777777778, 7.66666666666667, "Repeatability", 89,
                                      892.722222222222, "Total"))
})

test_that("LF2.5 Historical std. dev. - Two-way ANOVA table without interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 727.192518087174, 97.8395061728395, 2.19585066080532e-71, 880.555555555556,
                                      "Parts", 2, 6.21439915299104, 0.836111111111114, 0.00313130866789578,
                                      1.67222222222223, "Operators", 78, 0.134544159544159, 10.4944444444444,
                                      "Repeatability", 89, 892.722222222222, "Total"))
})

test_that("LF2.6 Historical std. dev. - Part by operator interaction plot matches", {
  plotName <- results[["results"]][["gaugeByInteraction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "2_part-by-operator-interaction")
})

test_that("LF2.7 Historical std. dev. - Measurements by operator plot matches", {
  plotName <- results[["results"]][["gaugeByOperator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "2_measurements-by-operator")
})

test_that("LF2.8 Historical std. dev. - Measurements by part plot matches", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "2_measurements-by-part")
})

test_that("LF2.9 Historical std. dev. - Matrix plot for operators matches", {
  plotName <- results[["results"]][["gaugeScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "2_matrix-plot-for-operators")
})

test_that("LF2.10 Historical std. dev. - Range chart by operator plot matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "2_range-chart-by-operator")
})

test_that("LF2.11 Historical std. dev. - Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("LF2.12 Historical std. dev. - Traffic plot matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "2_traffic-plot")
})

test_that("LF2.13 Historical std. dev. - Average chart by operator plot matches", {
  plotName <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "2_average-chart-by-operator")
})

test_that("LF2.14 Historical std. dev. - Test results for x-bar chart table results match", {
  table <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Operator A", "Point 1", "", "Point 2", "", "Point 3", "", "Point 4",
                                      "", "Point 5", "", "Point 6", "", "Point 7", "", "Point 8",
                                      "", "Point 9", "", "Point 10", "Operator B", "Point 1", "",
                                      "Point 2", "", "Point 3", "", "Point 4", "", "Point 5", "",
                                      "Point 6", "", "Point 7", "", "Point 8", "", "Point 9", "",
                                      "Point 10", "Operator C", "Point 1", "", "Point 2", "", "Point 3",
                                      "", "Point 4", "", "Point 5", "", "Point 6", "", "Point 7",
                                      "", "Point 8", "", "Point 9", "", "Point 10"))
})

## Fixed effects model ####

options <- analysisOptions("msaGaugeRR")
options$operatorLongFormat <- "Operators"
options$partLongFormat <- "Parts"
options$measurementLongFormat <- "Dm"
options$tolerance <- TRUE
options$toleranceValue <- 10
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
options$anovaModelType <- "fixedEffect"
set.seed(1)
results <- runAnalysis("msaGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options)

test_that("LF3.1 Fixed effect ANOVA - Two-way ANOVA table with interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 765.700483091787, 97.8395061728395, 2.22178519889191e-21, 880.555555555556,
                                      "Parts", 2, 6.54347826086958, 0.836111111111114, 0.0073155788200197,
                                      1.67222222222223, "Operators", 18, 1.22946859903382, 0.157098765432099,
                                      0.268355463232599, 2.82777777777778, "Parts  *  Operators",
                                      60, 0.127777777777778, 7.66666666666667, "Repeatability", 89,
                                      892.722222222222, "Total"))
})

test_that("LF3.2 Fixed effect ANOVA - Two-way ANOVA table without interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 727.192518087174, 97.8395061728395, 2.19585066080532e-71, 880.555555555556,
                                      "Parts", 2, 6.21439915299104, 0.836111111111114, 0.00313130866789578,
                                      1.67222222222223, "Operators", 78, 0.134544159544159, 10.4944444444444,
                                      "Repeatability", 89, 892.722222222222, "Total"))
})

## Type 3 study ####
options <- analysisOptions("msaGaugeRR")
options$partLongFormat <- "Parts"
options$measurementLongFormat <- "Dm"
options$tolerance <- TRUE
options$toleranceValue <- 10
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
options$anovaModelType <- "randomEffect"
options$type3 <- TRUE
set.seed(1)
results <- runAnalysis("msaGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options)


test_that("LF4.1 Type 3 study - Variance components table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.38, "Total gauge r&amp;R", 0.152083333333333, 1.38, "Repeatability",
                                      0.152083333333333, 98.62, "Part-to-part", 10.8541580932785,
                                      100, "Total variation", 11.0062414266118))
})

test_that("LF4.2 Type 3 study - Gauge evaluation table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.389978631893253, 11.75, 23.4, "Total gauge r&amp;R", 2.33987179135952,
                                      0.389978631893253, 11.75, 23.4, "Repeatability", 2.33987179135952,
                                      3.29456493232088, 99.31, 197.67, "Part-to-part", 19.7673895939253,
                                      3.3175655873866, 100, 199.05, "Total variation", 19.9053935243196
                                 ))
})

test_that("LF4.3 Type 3 study - Components of variation plot matches", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "4_components-of-variation")
})

test_that("LF4.4 Type 3 study - One-way ANOVA table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 643.328259766616, 97.8395061728395, 9.59874187907134e-71, 880.555555555556,
                                      "Parts", 80, 0.152083333333333, 12.1666666666667, "Repeatability",
                                      89, 892.722222222222, "Total"))
})

test_that("LF4.5 Type 3 study - Part by operator interaction plot matches", {
  plotName <- results[["results"]][["gaugeByInteraction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "4_part-by-operator-interaction")
})

test_that("LF4.6 Type 3 study - Measurements by operator plot matches", {
  plotName <- results[["results"]][["gaugeByOperator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "4_measurements-by-operator")
})

test_that("LF4.7 Type 3 study - Measurements by part plot matches", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "4_measurements-by-part")
})

test_that("LF4.9 Type 3 study - Range chart by operator plot matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "4_range-chart-by-operator")
})

test_that("LF4.10 Type 3 study - Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Point 10", "Point 8", "", "", "Point 9"))
})

test_that("LF4.11 Type 3 study - traffic plot matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "4_traffic-plot")
})

test_that("LF4.12 Type 3 study - Average chart by operator plot matches", {
  plotName <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "4_average-chart-by-operator")
})

test_that("LF4.13 Type 3 study - Test results for x-bar chart table results match", {
  table <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Point 1", "", "Point 2", "", "Point 3", "", "Point 4", "",
                                      "Point 5", "", "Point 6", "", "Point 7", "", "Point 8", "",
                                      "Point 9", "", "Point 10"))
})

## Report ####

options <- analysisOptions("msaGaugeRR")
options$operatorLongFormat <- "Operators"
options$partLongFormat <- "Parts"
options$measurementLongFormat <- "Dm"
options$tolerance <- TRUE
options$toleranceValue <- 10
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
options$anovaModelType <- "randomEffect"
options$report <- TRUE
options$reportGaugeNameText <- "Name of the gauge study"
set.seed(1)
results <- runAnalysis("msaGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options)



test_that("LF5. Gauge r&R report plot matches", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "5_gauge-r-r-report")
})

# Wide format ####

## Default settings ####
options <- analysisOptions("msaGaugeRR")
options$operatorWideFormat <- "Operator"
options$partWideFormat <- "Part"
options$measurementsWideFormat <- list("Measurement1", "Measurement2", "Measurement3")
options$dataFormat <- "wideFormat"
options$tolerance <- TRUE
options$toleranceValue <- 10
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
options$anovaModelType <- "randomEffect"
set.seed(1)
results <- runAnalysis("msaGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_wide.csv", options)

test_that("WF1.1 Default Settings - Variance components table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(22.56, "Total gauge r&amp;R", 0.848225424491385, 22.54, "Repeatability",
                                      0.847522164107094, 0.02, "Reproducibility", 0.000703260384290564,
                                      0.02, "Operator", 0.000703260384290564, 77.44, "Part-to-part",
                                      2.91190554000432, 100, "Total variation", 3.7601309644957))
})

test_that("WF1.2 Default Settings - Gauge evaluation table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.920991544201892, 47.5, 55.26, "Total gauge r&amp;R", 5.52594926521135,
                                      0.920609669787959, 47.48, 55.24, "Repeatability", 5.52365801872775,
                                      0.0265190570022873, 1.37, 1.59, "Reproducibility", 0.159114342013724,
                                      0.0265190570022873, 1.37, 1.59, "Operator", 0.159114342013724,
                                      1.70643064318604, 88, 102.39, "Part-to-part", 10.2385838591162,
                                      1.93910571256332, 100, 116.35, "Total variation", 11.6346342753799
                                 ))
})

test_that("WF1.3 Default Settings - Components of variation plot matches", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_components-of-variation")
})

test_that("WF1.4 Default Settings - Two-way ANOVA table with interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 49.6115781426449, 27.0546720241459, 7.05242347812414e-11, 243.492048217313,
                                      "Part", 2, 1.59283423428892, 0.868619975635811, 0.23071349585535,
                                      1.73723995127162, "Operator", 18, 0.581263589632298, 0.545329800764601,
                                      0.89932032121579, 9.81593641376281, "Part  *  Operator", 60,
                                      0.938179873109842, 56.2907923865905, "Repeatability", 89, 311.336016968938,
                                      "Total"))
})

test_that("WF1.5 Default Settings - Two-way ANOVA table without interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 31.9220820055477, 27.0546720241459, 1.20668982102463e-22, 243.492048217313,
                                      "Part", 2, 1.02489352187142, 0.868619975635811, 0.363615551786533,
                                      1.73723995127162, "Operator", 78, 0.847522164107094, 66.1067288003534,
                                      "Repeatability", 89, 311.336016968938, "Total"))
})

test_that("WF1.6 Default Settings - Part by operator interaction plot matches", {
  plotName <- results[["results"]][["gaugeByInteraction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_part-by-operator-interaction")
})

test_that("WF1.7 Default Settings - Measurements by operator plot matches", {
  plotName <- results[["results"]][["gaugeByOperator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_measurements-by-operator")
})

test_that("WF1.8 Default Settings - Measurements by part plot matches", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_measurements-by-part")
})

test_that("WF1.9 Default Settings - Matrix plot for operators matches", {
  plotName <- results[["results"]][["gaugeScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_matrix-plot-for-operators")
})

test_that("WF1.10 Default Settings - Range chart by operator plot matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_range-chart-by-operator")
})

test_that("WF1.11 Default Settings - Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("C", "Point 10"))
})

test_that("WF1.12 Default Settings - Traffic plot matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_traffic-plot")
})

test_that("WF1.13 Default Settings - Average chart by operator plot matches", {
  plotName <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_average-chart-by-operator")
})

test_that("WF1.14 Default Settings - Test results for x-bar chart table results match", {
  table <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("A", "Point 1", "", "Point 5", "", "Point 8", "", "Point 10",
                                      "B", "Point 1", "", "Point 5", "", "Point 8", "", "Point 10",
                                      "C", "Point 1", "", "Point 3", "", "Point 5", "", "Point 10"
                                 ))
})

## Historical std. dev ####
options <- analysisOptions("msaGaugeRR")
options$operatorWideFormat <- "Operator"
options$partWideFormat <- "Part"
options$measurementsWideFormat <- list("Measurement1", "Measurement2", "Measurement3")
options$dataFormat <- "wideFormat"
options$tolerance <- TRUE
options$toleranceValue <- 10
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
options$anovaModelType <- "randomEffect"
options$processVariationReference <- "historicalSd"
options$historicalSdValue <- 3
set.seed(1)
results <- runAnalysis("msaGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_wide.csv", options)

test_that("WF2.1 Historical std. dev. - Variance components table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9.42, "Total gauge r&amp;R", 0.848225424491385, 9.42, "Repeatability",
                                      0.847522164107094, 0.01, "Reproducibility", 0.000703260384290564,
                                      0.01, "Operator", 0.000703260384290564, 90.58, "Part-to-part",
                                      8.15177457550861, 100, "Total variation", 9))
})

test_that("WF2.2 Historical std. dev. - Gauge evaluation table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.920991544201892, 30.7, 55.26, "Total gauge r&amp;R", 5.52594926521135,
                                      0.920609669787959, 30.69, 55.24, "Repeatability", 5.52365801872775,
                                      0.0265190570022873, 0.88, 1.59, "Reproducibility", 0.159114342013724,
                                      0.0265190570022873, 0.88, 1.59, "Operator", 0.159114342013724,
                                      2.85513127115175, 95.17, 171.31, "Part-to-part", 17.1307876269105,
                                      3, 100, 180, "Total variation", 18))
})

test_that("WF2.3 Historical std. dev. - Components of variation plot matches", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF2_components-of-variation")
})

test_that("WF2.4 Historical std. dev. - -way ANOVA table with interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 49.6115781426449, 27.0546720241459, 7.05242347812414e-11, 243.492048217313,
                                      "Part", 2, 1.59283423428892, 0.868619975635811, 0.23071349585535,
                                      1.73723995127162, "Operator", 18, 0.581263589632298, 0.545329800764601,
                                      0.89932032121579, 9.81593641376281, "Part  *  Operator", 60,
                                      0.938179873109842, 56.2907923865905, "Repeatability", 89, 311.336016968938,
                                      "Total"))
})

test_that("WF2.5 Historical std. dev. - Two-way ANOVA table without interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 31.9220820055477, 27.0546720241459, 1.20668982102463e-22, 243.492048217313,
                                      "Part", 2, 1.02489352187142, 0.868619975635811, 0.363615551786533,
                                      1.73723995127162, "Operator", 78, 0.847522164107094, 66.1067288003534,
                                      "Repeatability", 89, 311.336016968938, "Total"))
})

test_that("WF2.6 Historical std. dev. - Part by operator interaction plot matches", {
  plotName <- results[["results"]][["gaugeByInteraction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF2_part-by-operator-interaction")
})

test_that("WF2.7 Historical std. dev. - Measurements by operator plot matches", {
  plotName <- results[["results"]][["gaugeByOperator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF2_measurements-by-operator")
})

test_that("WF2.8 Historical std. dev. - Measurements by part plot matches", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF2_measurements-by-part")
})

test_that("WF2.9 Historical std. dev. - Matrix plot for operators matches", {
  plotName <- results[["results"]][["gaugeScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF2_matrix-plot-for-operators")
})

test_that("WF2.10 Historical std. dev. - Range chart by operator plot matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF2_range-chart-by-operator")
})

test_that("WF2.11 Historical std. dev. - Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("C", "Point 10"))
})

test_that("WF2.12 Historical std. dev. - Traffic plot matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF2_traffic-plot")
})

test_that("WF2.13 Historical std. dev. - Average chart by operator plot matches", {
  plotName <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF2_average-chart-by-operator")
})

test_that("WF2.14 Historical std. dev. - Test results for x-bar chart table results match", {
  table <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("A", "Point 1", "", "Point 5", "", "Point 8", "", "Point 10",
                                      "B", "Point 1", "", "Point 5", "", "Point 8", "", "Point 10",
                                      "C", "Point 1", "", "Point 3", "", "Point 5", "", "Point 10"
                                 ))
})

## Fixed effects model ####

options <- analysisOptions("msaGaugeRR")
options$operatorWideFormat <- "Operator"
options$partWideFormat <- "Part"
options$measurementsWideFormat <- list("Measurement1", "Measurement2", "Measurement3")
options$dataFormat <- "wideFormat"
options$tolerance <- TRUE
options$toleranceValue <- 10
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
options$anovaModelType <- "fixedEffect"
set.seed(1)
results <- runAnalysis("msaGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_wide.csv", options)

test_that("WF3.1 Fixed effct ANOVA - Two-way ANOVA table with interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 28.837403998517, 27.0546720241459, 6.66730220666189e-09, 243.492048217313,
                                      "Part", 2, 0.925856544711989, 0.868619975635811, 0.41425774549038,
                                      1.73723995127162, "Operator", 18, 0.581263589632298, 0.545329800764601,
                                      0.89932032121579, 9.81593641376281, "Part  *  Operator", 60,
                                      0.938179873109842, 56.2907923865905, "Repeatability", 89, 311.336016968938,
                                      "Total"))
})

test_that("WF3.2 Fixed effct ANOVA - Two-way ANOVA table without interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 31.9220820055477, 27.0546720241459, 1.20668982102463e-22, 243.492048217313,
                                      "Part", 2, 1.02489352187142, 0.868619975635811, 0.363615551786533,
                                      1.73723995127162, "Operator", 78, 0.847522164107094, 66.1067288003534,
                                      "Repeatability", 89, 311.336016968938, "Total"))
})

## Type 3 study ####

options <- analysisOptions("msaGaugeRR")
options$partWideFormat <- "Part"
options$measurementsWideFormat <- list("Measurement1", "Measurement2", "Measurement3")
options$dataFormat <- "wideFormat"
options$tolerance <- TRUE
options$toleranceValue <- 10
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
options$anovaModelType <- "randomEffect"
options$type3 <- TRUE
set.seed(1)
results <- runAnalysis("msaGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_wide.csv", options)


test_that("WF4.1 Type 3 study - Variance components table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(22.56, "Total gauge r&amp;R", 0.848049609395312, 22.56, "Repeatability",
                                      0.848049609395312, 77.44, "Part-to-part", 2.91184693497229,
                                      100, "Total variation", 3.7598965443676))
})

test_that("WF4.2 Type 3 study - Gauge evaluation table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.920896090444146, 47.49, 55.25, "Total gauge r&amp;R", 5.52537654266487,
                                      0.920896090444146, 47.49, 55.25, "Repeatability", 5.52537654266487,
                                      1.70641347128189, 88, 102.38, "Part-to-part", 10.2384808276913,
                                      1.9390452661987, 100, 116.34, "Total variation", 11.6342715971922
                                 ))
})

test_that("WF4.3 Type 3 study - Components of variation plot matches", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF4_components-of-variation")
})

test_that("WF4.4 Type 3 study - One-way ANOVA table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 31.9022280352642, 27.0546720241459, 6.20007551605074e-23, 243.492048217313,
                                      "Part", 80, 0.848049609395312, 67.843968751625, "Repeatability",
                                      89, 311.336016968938, "Total"))
})

test_that("WF4.6 Type 3 study - Measurements by operator plot matches", {
  plotName <- results[["results"]][["gaugeByOperator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF4_measurements-by-operator")
})

test_that("WF4.7 Type 3 study - Measurements by part plot matches", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF4_measurements-by-part")
})

test_that("WF4.9 Type 3 study - Range chart by operator plot matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF4_range-chart-by-operator")
})

test_that("WF4.10 Type 3 study - Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Point 9", "", "Point 10"))
})

test_that("WF4.11 Type 3 study - Traffic plot matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF4_traffic-plot")
})

test_that("WF4.12 Type 3 study - Average chart by operator plot matches", {
  plotName <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF4_average-chart-by-operator")
})

test_that("WF4.13 Type 3 study - Test results for x-bar chart table results match", {
  table <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Point 1", "Point 16", "", "Point 2", "Point 17", "", "Point 3",
                                      "Point 18", "", "Point 9", "", "", "Point 13", "", "", "Point 14",
                                      "", "", "Point 15", "", "", "Point 22", "", "", "Point 23",
                                      "", "", "Point 28", "", "", "Point 29", "", "", "Point 30",
                                      ""))
})

## Report ####

options <- analysisOptions("msaGaugeRR")
options$operatorWideFormat <- "Operator"
options$partWideFormat <- "Part"
options$measurementsWideFormat <- list("Measurement1", "Measurement2", "Measurement3")
options$dataFormat <- "wideFormat"
options$tolerance <- TRUE
options$toleranceValue <- 10
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
options$anovaModelType <- "randomEffect"
options$report <- TRUE
set.seed(1)
results <- runAnalysis("msaGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_wide.csv", options)

test_that("WF5 - Gauge r&R report plot matches", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "gauge-r-r-report")
})
