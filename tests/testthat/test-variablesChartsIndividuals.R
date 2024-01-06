context("[Quality Control] Variables Charts for Individuals")
.numDecimals <- 2
set.seed(1)

# Basic tests ####

## X-mR chart (verified with Minitab) ####
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "Yield"
options$axisLabels <- "Month"
options$xmrChartMovingRangeLength <- 2
options$autocorrelationPlot <- TRUE
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("1.1 Basic test to create x-mR control chart - plot", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart1")
})

test_that("1.2 Basic test to create x-mR control chart - I table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 3 (Mar Year 1)", "Point 11 (Nov Year 1)", "Point 4 (Apr Year 1)",
                                      "Point 4 (Apr Year 1)", "Point 12 (Dec Year 1)", "Point 24 (Dec Year 2)",
                                      "Point 23 (Nov Year 2)", "Point 13 (Jan Year 2)", "", "Point 24 (Dec Year 2)",
                                      "Point 14 (Feb Year 2)", "", "", "Point 15 (Mar Year 2)", "",
                                      "", "Point 16 (Apr Year 2)", "", "", "Point 17 (May Year 2)",
                                      "", "", "Point 18 (Jun Year 2)", "", "", "Point 19 (Jul Year 2)",
                                      "", "", "Point 20 (Aug Year 2)", "", "", "Point 21 (Sep Year 2)",
                                      "", "", "Point 22 (Oct Year 2)", "", "", "Point 31 (Jul Year 3)",
                                      "", "", "Point 32 (Aug Year 3)", "", "", "Point 33 (Sep Year 3)",
                                      "", "", "Point 34 (Oct Year 3)", "", "", "Point 35 (Nov Year 3)",
                                      "", "", "Point 36 (Dec Year 3)", ""))
})

test_that("1.3 Basic test to create x-mR control chart - MR table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 3 (Mar Year 1)", "Point 12 (Dec Year 1)", "Point 5 (May Year 1)",
                                      "Point 13 (Jan Year 2)", "Point 23 (Nov Year 2)", "Point 14 (Feb Year 2)",
                                      "Point 25 (Jan Year 3)", "Point 15 (Mar Year 2)", "", "Point 32 (Aug Year 3)",
                                      "", "Point 33 (Sep Year 3)", "", "Point 34 (Oct Year 3)", "",
                                      "Point 35 (Nov Year 3)", "", "Point 36 (Dec Year 3)"))
})

## Autocorrelation plot (verified with Minitab) ####
test_that("2. Basic test to create autocorrelation chart", {
  plotName <- results[["results"]][["autocorrelationPlot"]][["collection"]][["autocorrelationPlot_Yield"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "autocorrelation-plot1")
})

## X-mR chart with different moving range length (verified with Minitab) ####
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "Yield"
options$xmrChartMovingRangeLength <- 4
results <- runAnalysis("variablesChartsIndividuals",
                       "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options, makeTests = T)

test_that("3.1 Basic test to create x-mR control chart with changed MR length - plot", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart2")
})

test_that("3.2 Basic test to create x-mR control chart with changed MR length - I table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 3", "Point 11", "Point 4", "Point 4", "Point 12", "Point 24",
                                      "Point 23", "Point 13", "", "Point 24", "Point 14", "", "",
                                      "Point 15", "", "", "Point 16", "", "", "Point 17", "", "",
                                      "Point 18", "", "", "Point 19", "", "", "Point 20", "", "",
                                      "Point 21", "", "", "Point 22", "", "", "Point 31", "", "",
                                      "Point 32", "", "", "Point 33", "", "", "Point 34", "", "",
                                      "Point 35", "", "", "Point 36", ""))
})

test_that("3.3 Basic test to create x-mR control chart with changed MR length - MR table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 4", "Point 14", "Point 5", "Point 15", "Point 6", "Point 16",
                                      "Point 7", "Point 17", "Point 23", "Point 18", "Point 24", "Point 19",
                                      "Point 25", "Point 20", "Point 26", "Point 21", "Point 27",
                                      "Point 22", "", "Point 34", "", "Point 35", "", "Point 36"
                                 ))
})

## X-mR chart with stages (verified with Minitab) ####
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "Yield"
options$stage <- "Stage"
options$xmrChartMovingRangeLength <- 2
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("4.1 Basic test to create x-mR control chart with stages - plot", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart3")
})

test_that("4.2 Basic test to create x-mR control chart with stages - I table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Zero", "Point 3", "Point 11", "Point 4", "", "Point 4", "Point 12",
                                      "", "One", "Point 23", "Point 19", "Point 24", "", "Point 24",
                                      "Point 20", "", "", "", "Point 21", "", "", "", "Point 22",
                                      ""))
})

test_that("4.3 Basic test to create x-mR control chart with stages - MR table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Zero", "Point 3", "Point 12", "", "Point 5", "", "One", "Point 23",
                                      "Point 20", "", "", "Point 21", "", "", "Point 22"))
})


## X-mR chart with large moving range length (verified with Minitab) ####
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "Yield"
options$xmrChartMovingRangeLength <- 36
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("5.1 Test X-mR control chart with large moving range length - plot", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart4")
})

test_that("5.2 Test X-mR control chart with large moving range length - I table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 3", "Point 11", "Point 4", "Point 19", "Point 4", "Point 12",
                                      "Point 24", "Point 20", "Point 23", "Point 13", "", "Point 21",
                                      "Point 24", "Point 14", "", "Point 22", "", "Point 15", "",
                                      "", "", "Point 16", "", "", "", "Point 17", "", "", "", "Point 18",
                                      "", "", "", "Point 19", "", "", "", "Point 20", "", "", "",
                                      "Point 21", "", "", "", "Point 22", "", "", "", "Point 31",
                                      "", "", "", "Point 32", "", "", "", "Point 33", "", "", "",
                                      "Point 34", "", "", "", "Point 35", "", "", "", "Point 36",
                                      "", ""))
})

test_that("5.3 Test X-mR control chart with large moving range length - MR table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

## X-mR chart with stages and large moving range length (verified with Minitab) ####
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "Yield"
options$stage <- "Stage"
options$xmrChartMovingRangeLength <- 12
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("6.1 Test X-mR control chart with stages and large moving range length - plot", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart5")
})

test_that("6.2 Test X-mR control chart with stages and large moving range length - I table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Zero", "Point 11", "Point 4", "", "Point 12", "", "One", "Point 19",
                                      "Point 24", "", "Point 20", "", "", "Point 21", "", "", "Point 22",
                                      ""))
})

test_that("6.3 Test X-mR control chart with stages and large moving range length - MR table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

## Autocorrelation plot with different number of lags ####
options <- analysisOptions("variablesChartsIndividuals")
options$xmrChart <- FALSE
options$measurement <- "Yield"
options$autocorrelationPlot <- TRUE
options$autocorrelationPlotLagsNumber <- 4
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("7. Autocorrelation plot with changed number of lags", {
  options <- analysisOptions("variablesChartsIndividuals")
  options$measurement <- "Yield"
  options$xmrChart <- FALSE
  options$autocorrelationPlot <- TRUE
  options$autocorrelationPlotLagsNumber <- 4
  set.seed(1)
  results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)
  plotName <- results[["results"]][["autocorrelationPlot"]][["collection"]][["autocorrelationPlot_Yield"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "autocorrelation-plot2")
})

## Report including everything (verified with Minitab) ####
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "Yield"
options$axisLabels <- "Month"
options$xmrChartMovingRangeLength <- 2
options$report <- TRUE
options$reportAutocorrelationChart <- TRUE
options$reportMetaData <- TRUE
options$reportIMRChart <- TRUE
options$reportDate <- "01.01.2000"
options$reportMeasurementName <- "Measurement name"
options$reportMiscellaneous <- "Various comments"
options$reportReportedBy <- "Your name"
options$reportTitle <- "Report title"
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("8. Basic test to create a report with all components", {
  plotName <- results[["results"]][["report"]][["collection"]][["report_report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "report1")
})

## Report including only x-mR chart ####
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "Yield"
options$axisLabels <- "Month"
options$xmrChartMovingRangeLength <- 2
options$report <- TRUE
options$reportAutocorrelationChart <- FALSE
options$reportMetaData <- TRUE
options$reportIMRChart <- TRUE
options$reportDate <- "01.01.2000"
options$reportMeasurementName <- "Measurement name"
options$reportMiscellaneous <- "Various comments"
options$reportReportedBy <- "Your name"
options$reportTitle <- "Report title"
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("9. Basic test to create a report with only an IMR chart", {
  plotName <- results[["results"]][["report"]][["collection"]][["report_report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "report2")
})

# Missing values ####

## Missing values in measurement ####

### Single missing value ####

#### X-mR chart (verified with Minitab) ####
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "YieldMissing1"
options$axisLabels <- "Month"
options$xmrChartMovingRangeLength <- 2
options$autocorrelationPlot <- TRUE
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("10.1 X-mR control chart with single missing value in measurements - plot", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart6")
})

test_that("10.2 X-mR control chart with single missing value in measurements - I table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 3 (Mar Year 1)", "Point 11 (Nov Year 1)", "Point 4 (Apr Year 1)",
                                      "Point 4 (Apr Year 1)", "Point 12 (Dec Year 1)", "Point 24 (Dec Year 2)",
                                      "Point 23 (Nov Year 2)", "Point 13 (Jan Year 2)", "", "Point 24 (Dec Year 2)",
                                      "Point 14 (Feb Year 2)", "", "", "Point 15 (Mar Year 2)", "",
                                      "", "Point 16 (Apr Year 2)", "", "", "Point 17 (May Year 2)",
                                      "", "", "Point 18 (Jun Year 2)", "", "", "Point 19 (Jul Year 2)",
                                      "", "", "Point 20 (Aug Year 2)", "", "", "Point 21 (Sep Year 2)",
                                      "", "", "Point 22 (Oct Year 2)", "", "", "Point 31 (Jul Year 3)",
                                      "", "", "Point 32 (Aug Year 3)", "", "", "Point 33 (Sep Year 3)",
                                      "", "", "Point 34 (Oct Year 3)", "", "", "Point 35 (Nov Year 3)",
                                      "", "", "Point 36 (Dec Year 3)", ""))
})

test_that("10.3 X-mR control chart with single missing value in measurements - MR table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 12 (Dec Year 1)", "Point 13 (Jan Year 2)", "Point 14 (Feb Year 2)",
                                      "Point 15 (Mar Year 2)", "Point 32 (Aug Year 3)", "Point 33 (Sep Year 3)",
                                      "Point 34 (Oct Year 3)", "Point 35 (Nov Year 3)", "Point 36 (Dec Year 3)"
                                 ))
})

## Missing values in axis label (verified with Minitab) ####
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "Yield"
options$axisLabels <- "MonthMissing5"
options$xmrChartMovingRangeLength <- 2
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("11.1 X-mR control chart with missing values in axis labels - plot", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart7")
})

test_that("11.2 X-mR control chart with missing values in axis labels - I table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 3 (Mar Year 1)", "Point 11 (Nov Year 1)", "Point 4 (Apr Year 1)",
                                      "Point 4 (Apr Year 1)", "Point 12 (Dec Year 1)", "Point 24 (Dec Year 2)",
                                      "Point 23 (Nov Year 2)", "Point 13 ()", "", "Point 24 (Dec Year 2)",
                                      "Point 14 (Feb Year 2)", "", "", "Point 15 (Mar Year 2)", "",
                                      "", "Point 16 (Apr Year 2)", "", "", "Point 17 (May Year 2)",
                                      "", "", "Point 18 (Jun Year 2)", "", "", "Point 19 (Jul Year 2)",
                                      "", "", "Point 20 (Aug Year 2)", "", "", "Point 21 (Sep Year 2)",
                                      "", "", "Point 22 ()", "", "", "Point 31 (Jul Year 3)", "",
                                      "", "Point 32 ()", "", "", "Point 33 (Sep Year 3)", "", "",
                                      "Point 34 (Oct Year 3)", "", "", "Point 35 (Nov Year 3)", "",
                                      "", "Point 36 (Dec Year 3)", ""))
})

test_that("11.3 X-mR control chart with missing values in axis labels - MR table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 3 (Mar Year 1)", "Point 12 (Dec Year 1)", "Point 5 (May Year 1)",
                                      "Point 13 ()", "Point 23 (Nov Year 2)", "Point 14 (Feb Year 2)",
                                      "Point 25 (Jan Year 3)", "Point 15 (Mar Year 2)", "", "Point 32 ()",
                                      "", "Point 33 (Sep Year 3)", "", "Point 34 (Oct Year 3)", "",
                                      "Point 35 (Nov Year 3)", "", "Point 36 (Dec Year 3)"))
})
