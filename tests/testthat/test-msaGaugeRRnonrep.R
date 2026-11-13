context("[Quality Control] MSA Gauge non-replicabe")

# Long format ####

## Default settings ####
options <- analysisOptions("msaGaugeRRnonrep")
options$testSet <- "jaspDefault"
options$partLongFormat <- "Batch"
options$operatorLongFormat <- "Operator"
options$measurementLongFormat <- "Result"
options$tolerance <- TRUE
options$toleranceValue <- 15
options$rChart <- TRUE
options$xBarChart <- TRUE
options$partMeasurementPlot <- TRUE
options$partMeasurementPlotAllValues <- TRUE
options$operatorMeasurementPlot <- TRUE
set.seed(1)

results <- runAnalysis("msaGaugeRRnonrep", "datasets/msaGaugeRRNested/msaGaugeRRNested_long.csv", options)


test_that("LF1.1 Default settings - Measurements by operator plot matches", {
  plotName <- results[["results"]][["NRoperatorGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_measurements-by-operator")
})

test_that("LF1.2 Default settings - Operator A plot matches", {
  plotName <- results[["results"]][["NRpartOperatorGraph"]][["collection"]][["NRpartOperatorGraph_A"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_operator-a")
})

test_that("LF1.3 Default settings - Operator B plot matches", {
  plotName <- results[["results"]][["NRpartOperatorGraph"]][["collection"]][["NRpartOperatorGraph_B"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_operator-b")
})

test_that("LF1.4 Default settings - Operator C plot matches", {
  plotName <- results[["results"]][["NRpartOperatorGraph"]][["collection"]][["NRpartOperatorGraph_C"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_operator-c")
})

test_that("LF1.5 Default settings - Components of variation plot matches", {
  plotName <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_components-of-variation")
})

test_that("LF1.6 Default settings - Gauge r&R (nested) table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2, 0.696059139499015, 2.18133333333333, 4.36266666666666, 0.517597080688392,
                                      "Operator", 12, 38.849173553719, 3.13383333333333, 37.606, 4.46224072581384e-09,
                                      "Batch(Operator)", 15, "", 0.0806666666666667, 1.21, "", "Repeatability",
                                      29, "", "", 43.1786666666667, "", "Total"))
})

test_that("LF1.7 Default settings - Variance components table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0806666666666667, 5.01892466428164, "Total gauge r&amp;R", 0.0806666666666667,
                                      5.01892466428164, "Repeatability", 0, 0, "Reproducibility",
                                      1.52658333333333, 94.9810753357184, "Part-To-part", 1.60725,
                                      100, "Total variation"))
})

test_that("LF1.8 Default settings - Gauge evaluation table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.284018778721877, 22.4029566447861, 11.3607511488751, "Total gauge r&amp;R",
                                      1.70411267233126, 0.284018778721877, 22.4029566447861, 11.3607511488751,
                                      "Repeatability", 1.70411267233126, 0, 0, 0, "Reproducibility",
                                      0, 1.23554981013852, 97.458234816622, 49.4219924055408, "Part-To-part",
                                      7.41329886083112, 1.26777363910124, 100, 50.7109455640496, "Total variation",
                                      7.60664183460744))
})

test_that("LF1.9 Default settings - Range chart by operator plot matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_range-chart-by-operator")
})

test_that("LF1.10 Default settings - Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("LF1.11 Default settings - Average chart by operator plot matches", {
  plotName <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_average-chart-by-operator")
})

test_that("LF1.12 Default settings - Test results for x-bar chart table results match", {
  table <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("A", "Point 2", "", "Point 3", "", "Point 4", "", "Point 5",
                                      "B", "Point 6", "", "Point 7", "", "Point 8", "", "Point 10",
                                      "C", "Point 11", "", "Point 12", "", "Point 13", "",
                                      "Point 15"))
})

## Historical std. dev. ####

options <- analysisOptions("msaGaugeRRnonrep")
options$testSet <- "jaspDefault"
options$partLongFormat <- "Batch"
options$operatorLongFormat <- "Operator"
options$measurementLongFormat <- "Result"
options$tolerance <- TRUE
options$toleranceValue <- 15
options$rChart <- TRUE
options$xBarChart <- TRUE
options$partMeasurementPlot <- TRUE
options$partMeasurementPlotAllValues <- TRUE
options$operatorMeasurementPlot <- TRUE
options$processVariationReference <- "historicalSd"
options$historicalSdValue <- 3
set.seed(1)

results <- runAnalysis("msaGaugeRRnonrep", "datasets/msaGaugeRRNested/msaGaugeRRNested_long.csv", options)


test_that("LF2.1 Historical std. dev. - Measurements by operator plot matches", {
  plotName <- results[["results"]][["NRoperatorGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF2_measurements-by-operator")
})

test_that("LF2.2 Historical std. dev. - Operator A plot matches", {
  plotName <- results[["results"]][["NRpartOperatorGraph"]][["collection"]][["NRpartOperatorGraph_A"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF2_operator-a")
})

test_that("LF2.3 Historical std. dev. - Operator B plot matches", {
  plotName <- results[["results"]][["NRpartOperatorGraph"]][["collection"]][["NRpartOperatorGraph_B"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF2_operator-b")
})

test_that("LF2.4 Historical std. dev. - Operator C plot matches", {
  plotName <- results[["results"]][["NRpartOperatorGraph"]][["collection"]][["NRpartOperatorGraph_C"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF2_operator-c")
})

test_that("LF2.5 Historical std. dev. - Components of variation plot matches", {
  plotName <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF2_components-of-variation")
})

test_that("LF2.6 Historical std. dev. - Gauge r&R (nested) table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2, 0.696059139499015, 2.18133333333333, 4.36266666666666, 0.517597080688392,
                                      "Operator", 12, 38.849173553719, 3.13383333333333, 37.606, 4.46224072581384e-09,
                                      "Batch(Operator)", 15, "", 0.0806666666666667, 1.21, "", "Repeatability",
                                      29, "", "", 43.1786666666667, "", "Total"))
})

test_that("LF2.7 Historical std. dev. - Variance components table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0806666666666667, 0.896296296296296, "Total gauge r&amp;R",
                                      0.0806666666666667, 0.896296296296296, "Repeatability", 0, 0,
                                      "Reproducibility", 8.91933333333333, 99.1037037037037, "Part-To-part",
                                      9, 100, "Total variation"))
})

test_that("LF2.8 Historical std. dev. - Gauge evaluation table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.284018778721877, 9.46729262406257, 11.3607511488751, "Total gauge r&amp;R",
                                      1.70411267233126, 0.284018778721877, 9.46729262406257, 11.3607511488751,
                                      "Repeatability", 1.70411267233126, 0, 0, 0, "Reproducibility",
                                      0, 2.98652529427315, 99.5508431424384, 119.461011770926, "Part-To-part",
                                      17.9191517656389, 3, 100, 120, "Total variation", 18))
})

test_that("LF2.9 Historical std. dev. - Range chart by operator plot matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF2_range-chart-by-operator")
})

test_that("LF2.10 Historical std. dev. - Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("LF2.11 Historical std. dev. - Average chart by operator plot matches", {
  plotName <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF2_average-chart-by-operator")
})

test_that("LF2.12 Historical std. dev. - Test results for x-bar chart table results match", {
  table <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("A", "Point 2", "", "Point 3", "", "Point 4", "", "Point 5",
                                      "B", "Point 6", "", "Point 7", "", "Point 8", "", "Point 10",
                                      "C", "Point 11", "", "Point 12", "", "Point 13", "",
                                      "Point 15"))
})

## Report ####

options <- analysisOptions("msaGaugeRRnonrep")
options$testSet <- "jaspDefault"
options$partLongFormat <- "Batch"
options$operatorLongFormat <- "Operator"
options$measurementLongFormat <- "Result"
options$tolerance <- TRUE
options$toleranceValue <- 15
options$rChart <- TRUE
options$xBarChart <- TRUE
options$partMeasurementPlot <- TRUE
options$partMeasurementPlotAllValues <- TRUE
options$operatorMeasurementPlot <- TRUE
options$report <- TRUE
options$reportGaugeName <- TRUE
options$reportGaugeNameText <- "Test Name"
set.seed(1)

results <- runAnalysis("msaGaugeRRnonrep", "datasets/msaGaugeRRNested/msaGaugeRRNested_long.csv", options)

test_that("LF3 Report - Gauge r&R (non-replicable) report plot matches", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF3_gauge-r-r-non-replicable-report")
})

# Wide format ####

## Default settings ####
options <- analysisOptions("msaGaugeRRnonrep")
options$testSet <- "jaspDefault"
options$dataFormat <- "wideFormat"
options$partWideFormat <- "Batch"
options$operatorWideFormat <- "Operator"
options$measurementsWideFormat <- c("Result1", "Result2")
options$tolerance <- TRUE
options$toleranceValue <- 15
options$rChart <- TRUE
options$xBarChart <- TRUE
options$partMeasurementPlot <- TRUE
options$partMeasurementPlotAllValues <- TRUE
options$operatorMeasurementPlot <- TRUE
set.seed(1)

results <- runAnalysis("msaGaugeRRnonrep", "datasets/msaGaugeRRNested/msaGaugeRRNested_wide.csv", options)


test_that("WF1.1 Default settings - Measurements by operator plot matches", {
  plotName <- results[["results"]][["NRoperatorGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_measurements-by-operator")
})

test_that("WF1.2 Default settings - Operator A plot matches", {
  plotName <- results[["results"]][["NRpartOperatorGraph"]][["collection"]][["NRpartOperatorGraph_A"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_operator-a")
})

test_that("WF1.3 Default settings - Operator B plot matches", {
  plotName <- results[["results"]][["NRpartOperatorGraph"]][["collection"]][["NRpartOperatorGraph_B"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_operator-b")
})

test_that("WF1.4 Default settings - Operator C plot matches", {
  plotName <- results[["results"]][["NRpartOperatorGraph"]][["collection"]][["NRpartOperatorGraph_C"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_operator-c")
})

test_that("WF1.5 Default settings - Components of variation plot matches", {
  plotName <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_components-of-variation")
})

test_that("WF1.6 Default settings - Gauge r&R (nested) table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2, 0.696059139499009, 2.18133333333331, 4.36266666666663, 0.517597080688394,
                                      "Operator", 12, 38.8491735537192, 3.13383333333334, 37.606,
                                      4.46224072581371e-09, "Batch(Operator)", 15, "", 0.0806666666666664,
                                      1.21, "", "Repeatability", 29, "", "", 43.1786666666667, "",
                                      "Total"))
})

test_that("WF1.7 Default settings - Variance components table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0806666666666664, 5.01892466428162, "Total gauge r&amp;R", 0.0806666666666664,
                                      5.01892466428162, "Repeatability", 0, 0, "Reproducibility",
                                      1.52658333333334, 94.9810753357184, "Part-To-part", 1.60725,
                                      100, "Total variation"))
})

test_that("WF1.8 Default settings - Gauge evaluation table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.284018778721877, 22.402956644786, 11.3607511488751, "Total gauge r&amp;R",
                                      1.70411267233126, 0.284018778721877, 22.402956644786, 11.3607511488751,
                                      "Repeatability", 1.70411267233126, 0, 0, 0, "Reproducibility",
                                      0, 1.23554981013852, 97.458234816622, 49.4219924055408, "Part-To-part",
                                      7.41329886083113, 1.26777363910124, 100, 50.7109455640496, "Total variation",
                                      7.60664183460744))
})

test_that("WF1.9 Default settings - Range chart by operator plot matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_range-chart-by-operator")
})

test_that("WF1.10 Default settings - Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("WF1.11 Default settings - Average chart by operator plot matches", {
  plotName <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_average-chart-by-operator")
})

test_that("WF1.12 Default settings - Test results for x-bar chart table results match", {
  table <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("A", "Point 2", "", "Point 3", "", "Point 4", "", "Point 5",
                                      "B", "Point 6", "", "Point 7", "", "Point 8", "", "Point 10",
                                      "C", "Point 11", "", "Point 12", "", "Point 13", "",
                                      "Point 15"))
})

## Historical std. dev. ####

options <- analysisOptions("msaGaugeRRnonrep")
options$testSet <- "jaspDefault"
options$dataFormat <- "wideFormat"
options$partWideFormat <- "Batch"
options$operatorWideFormat <- "Operator"
options$measurementsWideFormat <- c("Result1", "Result2")
options$tolerance <- TRUE
options$toleranceValue <- 15
options$rChart <- TRUE
options$xBarChart <- TRUE
options$partMeasurementPlot <- TRUE
options$partMeasurementPlotAllValues <- TRUE
options$operatorMeasurementPlot <- TRUE
options$processVariationReference <- "historicalSd"
options$historicalSdValue <- 3
set.seed(1)

results <- runAnalysis("msaGaugeRRnonrep", "datasets/msaGaugeRRNested/msaGaugeRRNested_wide.csv", options)

test_that("WF2.1 Historical std. dev. - Measurements by operator plot matches", {
  plotName <- results[["results"]][["NRoperatorGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF2_measurements-by-operator")
})

test_that("WF2.2 Historical std. dev. - Operator A plot matches", {
  plotName <- results[["results"]][["NRpartOperatorGraph"]][["collection"]][["NRpartOperatorGraph_A"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF2_operator-a")
})

test_that("WF2.3 Historical std. dev. - Operator B plot matches", {
  plotName <- results[["results"]][["NRpartOperatorGraph"]][["collection"]][["NRpartOperatorGraph_B"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF2_operator-b")
})

test_that("WF2.4 Historical std. dev. - Operator C plot matches", {
  plotName <- results[["results"]][["NRpartOperatorGraph"]][["collection"]][["NRpartOperatorGraph_C"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF2_operator-c")
})

test_that("WF2.5 Historical std. dev. - Components of variation plot matches", {
  plotName <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF2_components-of-variation")
})

test_that("WF2.6 Historical std. dev. - Gauge r&R (nested) table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2, 0.696059139499009, 2.18133333333331, 4.36266666666663, 0.517597080688394,
                                      "Operator", 12, 38.8491735537192, 3.13383333333334, 37.606,
                                      4.46224072581371e-09, "Batch(Operator)", 15, "", 0.0806666666666664,
                                      1.21, "", "Repeatability", 29, "", "", 43.1786666666667, "",
                                      "Total"))
})

test_that("WF2.7 Historical std. dev. - Variance components table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0806666666666664, 0.896296296296293, "Total gauge r&amp;R",
                                      0.0806666666666664, 0.896296296296293, "Repeatability", 0, 0,
                                      "Reproducibility", 8.91933333333333, 99.1037037037037, "Part-To-part",
                                      9, 100, "Total variation"))
})

test_that("WF2.8 Historical std. dev. - Gauge evaluation table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.284018778721877, 9.46729262406256, 11.3607511488751, "Total gauge r&amp;R",
                                      1.70411267233126, 0.284018778721877, 9.46729262406256, 11.3607511488751,
                                      "Repeatability", 1.70411267233126, 0, 0, 0, "Reproducibility",
                                      0, 2.98652529427315, 99.5508431424384, 119.461011770926, "Part-To-part",
                                      17.9191517656389, 3, 100, 120, "Total variation", 18))
})

test_that("WF2.9 Historical std. dev. - Range chart by operator plot matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF2_range-chart-by-operator")
})

test_that("WF2.10 Historical std. dev. - Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("WF2.11 Historical std. dev. - Average chart by operator plot matches", {
  plotName <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF2_average-chart-by-operator")
})

test_that("WF2.12 Historical std. dev. - Test results for x-bar chart table results match", {
  table <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("A", "Point 2", "", "Point 3", "", "Point 4", "", "Point 5",
                                      "B", "Point 6", "", "Point 7", "", "Point 8", "", "Point 10",
                                      "C", "Point 11", "", "Point 12", "", "Point 13", "",
                                      "Point 15"))
})

## Report ####

options <- analysisOptions("msaGaugeRRnonrep")
options$testSet <- "jaspDefault"
options$dataFormat <- "wideFormat"
options$partWideFormat <- "Batch"
options$operatorWideFormat <- "Operator"
options$measurementsWideFormat <- c("Result1", "Result2")
options$tolerance <- TRUE
options$toleranceValue <- 15
options$rChart <- TRUE
options$xBarChart <- TRUE
options$partMeasurementPlot <- TRUE
options$partMeasurementPlotAllValues <- TRUE
options$operatorMeasurementPlot <- TRUE
options$report <- TRUE
options$reportGaugeName <- TRUE
options$reportGaugeNameText <- "Test Name"
set.seed(1)

results <- runAnalysis("msaGaugeRRnonrep", "datasets/msaGaugeRRNested/msaGaugeRRNested_wide.csv", options)

test_that("WF3 Report - Gauge r&R (non-replicable) report plot matches", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF3_gauge-r-r-non-replicable-report")
})

# Operator variance larger than part(operator) variance ####
# 3 operators x 3 parts per operator x 2 replicates; reproducibility = (MS_O - MS_P(O)) / (3 * 2)
nestedData <- data.frame(
  Operator    = rep(c("A", "B", "C"), each = 6),
  Part        = rep(1:9, each = 2),
  PartReused  = rep(rep(1:3, each = 2), 3),
  Measurement = c(10.1, 10.3, 11.0, 10.8, 10.5, 10.6,
                  12.2, 12.5, 12.9, 13.0, 12.4, 12.1,
                  14.8, 15.0, 15.6, 15.3, 14.9, 15.2)
)

options <- analysisOptions("msaGaugeRRnonrep")
options$partLongFormat <- "Part"
options$operatorLongFormat <- "Operator"
options$measurementLongFormat <- "Measurement"
options$tolerance <- TRUE
options$toleranceValue <- 15
set.seed(1)
results <- runAnalysis("msaGaugeRRnonrep", nestedData, options)

test_that("LF4.1 Operator effect - Gauge r&R (nested) table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2, 137.257211538462, 31.7216666666667, 63.4433333333333, 9.78561585176688e-06,
                                      "Operator", 6, 8.32000000000006, 0.231111111111111, 1.38666666666666,
                                      0.00291404920796518, "Part(Operator)", 9, "", 0.0277777777777775,
                                      0.249999999999998, "", "Repeatability", 17, "", "", 65.08, "",
                                      "Total"))
})

test_that("LF4.2 Operator effect - Variance components table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(5.2762037037037, 98.109536681531, "Total gauge r&amp;R", 0.0277777777777775,
                                      0.516520032368585, "Repeatability", 5.24842592592592, 97.5930166491624,
                                      "Reproducibility", 0.101666666666667, 1.89046331846903, "Part-To-part",
                                      5.37787037037037, 100, "Total variation"))
  # moment estimators: (MS_O - MS_P(O)) / (b * r) and (MS_P(O) - MS_E) / r
  expect_equal(table[[3]][["Var"]], (31.7216666666667 - 0.231111111111111) / (3 * 2))
  expect_equal(table[[4]][["Var"]], (0.231111111111111 - 0.0277777777777775) / 2)
})

test_that("LF4.3 Operator effect - Gauge evaluation table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.2969988471272, 99.050258294227, 91.8799538850881, "Total gauge r&amp;R",
                                      13.7819930827632, 0.166666666666666, 7.18693281148909, 6.66666666666664,
                                      "Repeatability", 0.999999999999996, 2.29094433060385, 98.7891778734707,
                                      91.637773224154, "Reproducibility", 13.7456659836231, 0.318852107828483,
                                      13.74941205459, 12.7540843131393, "Part-To-part", 1.9131126469709,
                                      2.319023581245, 100, 92.7609432497999, "Total variation", 13.91414148747))
})

## Part labels reused across operators ####
options$partLongFormat <- "PartReused"
set.seed(1)
resultsReused <- runAnalysis("msaGaugeRRnonrep", nestedData, options)

test_that("LF4.4 Part labels reused across operators give the same variance components", {
  table <- resultsReused[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(5.2762037037037, 98.109536681531, "Total gauge r&amp;R", 0.0277777777777775,
                                      0.516520032368585, "Repeatability", 5.24842592592592, 97.5930166491624,
                                      "Reproducibility", 0.101666666666667, 1.89046331846903, "Part-To-part",
                                      5.37787037037037, 100, "Total variation"))
})

## Unequal number of parts per operator ####
options$partLongFormat <- "Part"
set.seed(1)
resultsUnbalanced <- runAnalysis("msaGaugeRRnonrep", nestedData[nestedData$Part != 9, ], options)

test_that("LF4.5 Unequal number of parts per operator gives an error", {
  expect_identical(resultsUnbalanced[["results"]][["gaugeRRNonRep"]][["error"]][["errorMessage"]],
                   "Unbalanced design. Some operators measured more parts than others.")
})

# Wide format, part labels reused across operators ####
# 2 operators x 3 parts per operator x 2 replicates; reproducibility = (MS_O - MS_P(O)) / (3 * 2)
nestedDataWide <- data.frame(
  Operator = rep(c("A", "B"), each = 3),
  Batch    = rep(1:3, 2),
  Result1  = c(10.1, 11.0, 10.5, 12.2, 12.9, 12.4),
  Result2  = c(10.3, 10.8, 10.6, 12.5, 13.0, 12.1)
)

options <- analysisOptions("msaGaugeRRnonrep")
options$dataFormat <- "wideFormat"
options$partWideFormat <- "Batch"
options$operatorWideFormat <- "Operator"
options$measurementsWideFormat <- c("Result1", "Result2")
options$tolerance <- TRUE
options$toleranceValue <- 15
set.seed(1)
results <- runAnalysis("msaGaugeRRnonrep", nestedDataWide, options)

test_that("WF4.1 Reused part labels - Gauge r&R (nested) table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 43.6489028213168, 11.6033333333333, 11.6033333333333, 0.00272033418242098,
                                      "Operator", 4, 11.3928571428572, 0.265833333333332, 1.06333333333333,
                                      0.00574955819130926, "Batch(Operator)", 6, "", 0.0233333333333331,
                                      0.139999999999998, "", "Repeatability", 11, "", "", 12.8066666666666,
                                      "", "Total"))
})

test_that("WF4.2 Reused part labels - Variance components table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.91291666666666, 94.0393281442032, "Total gauge r&amp;R", 0.0233333333333331,
                                      1.14707087259319, "Repeatability", 1.88958333333333, 92.89225727161,
                                      "Reproducibility", 0.121249999999999, 5.96067185579679, "Part-To-part",
                                      2.03416666666666, 100, "Total variation"))
  # moment estimators: (MS_O - MS_P(O)) / (b * r) and (MS_P(O) - MS_E) / r
  expect_equal(table[[3]][["Var"]], (11.6033333333333 - 0.265833333333332) / (3 * 2))
  expect_equal(table[[4]][["Var"]], (0.265833333333332 - 0.0233333333333331) / 2)
})
