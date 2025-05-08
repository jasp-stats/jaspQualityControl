context("[Quality Control] MSA Gauge non-replicabe")
.numDecimals <- 2

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
                                 list("A", "Point 2", "", "Point 3", "", "Point 4", "", "Point 5", "B",
                                      "Point 1", "", "Point 2", "", "Point 3", "", "Point 5", "C",
                                      "Point 1", "", "Point 2", "", "Point 3", "", "Point 5"
                                 ))
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
                                 list("A", "Point 2", "", "Point 3", "", "Point 4", "", "Point 5", "B",
                                      "Point 1", "", "Point 2", "", "Point 3", "", "Point 5", "C",
                                      "Point 1", "", "Point 2", "", "Point 3", "", "Point 5"
                                 ))
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
                                 list("A", "Point 2", "", "Point 3", "", "Point 4", "", "Point 5", "B",
                                      "Point 1", "", "Point 2", "", "Point 3", "", "Point 5", "C",
                                      "Point 1", "", "Point 2", "", "Point 3", "", "Point 5"
                                 ))
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
                                 list("A", "Point 2", "", "Point 3", "", "Point 4", "", "Point 5", "B",
                                      "Point 1", "", "Point 2", "", "Point 3", "", "Point 5", "C",
                                      "Point 1", "", "Point 2", "", "Point 3", "", "Point 5"
                                 ))
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
