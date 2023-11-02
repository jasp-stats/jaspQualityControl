context("[Quality Control] Variables Charts for Individuals")
.numDecimals <- 2
set.seed(1)

# Basic tests

## X-mR chart (verified with Minitab)
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "Yield"
options$axisLabels <- "Month"
options$xmrChartMovingRangeLength <- 2
options$autocorrelationPlot <- TRUE
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("Basic test to create x-mR control chart - plot", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart1")
})

test_that("Basic test to create x-mR control chart - I table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Mar Year 1", "Nov Year 1", "Apr Year 1", "Apr Year 1", "Dec Year 1",
                                      "Dec Year 2", "Nov Year 2", "Jan Year 2", "", "Dec Year 2",
                                      "Feb Year 2", "", "", "Mar Year 2", "", "", "Apr Year 2", "",
                                      "", "May Year 2", "", "", "Jun Year 2", "", "", "Jul Year 2",
                                      "", "", "Aug Year 2", "", "", "Sep Year 2", "", "", "Oct Year 2",
                                      "", "", "Jul Year 3", "", "", "Aug Year 3", "", "", "Sep Year 3",
                                      "", "", "Oct Year 3", "", "", "Nov Year 3", "", "", "Dec Year 3",
                                      ""))
})

test_that("Basic test to create x-mR control chart - MR table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Mar Year 1", "Dec Year 1", "May Year 1", "Jan Year 2", "Nov Year 2",
                                      "Feb Year 2", "Jan Year 3", "Mar Year 2", "", "Aug Year 3",
                                      "", "Sep Year 3", "", "Oct Year 3", "", "Nov Year 3", "", "Dec Year 3"
                                 ))
})

## Autocorrelation plot (verified with Minitab)
test_that("Basic test to create autocorrelation chart", {
  plotName <- results[["results"]][["autocorrelationPlot"]][["collection"]][["autocorrelationPlot_Yield"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "autocorrelation-plot1")
})

## X-mR chart with different moving range length (verified with Minitab)
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "Yield"
options$xmrChartMovingRangeLength <- 4
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("Basic test to create x-mR control chart with changed MR length - plot", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart2")
})

test_that("Basic test to create x-mR control chart with changed MR length - I table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3, 11, 4, 4, 12, 24, 23, 13, "", 24, 14, "", "", 15, "", "", 16,
                                      "", "", 17, "", "", 18, "", "", 19, "", "", 20, "", "", 21,
                                      "", "", 22, "", "", 31, "", "", 32, "", "", 33, "", "", 34,
                                      "", "", 35, "", "", 36, ""))
})

test_that("Basic test to create x-mR control chart with changed MR length - MR table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4, 14, 5, 15, 6, 16, 7, 17, 23, 18, 24, 19, 25, 20, 26, 21, 27,
                                      22, "", 34, "", 35, "", 36))
})

## X-mR chart with stages (verified with Minitab)
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "Yield"
options$stage <- "Stage"
options$xmrChartMovingRangeLength <- 2
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("Basic test to create x-mR control chart with stages - plot", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart3")
})

test_that("Basic test to create x-mR control chart with stages - I table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Zero", 3, 11, 4, "", 4, 12, "", "One", 23, 19, 24, "", 24, 20,
                                      "", "", "", 21, "", "", "", 22, ""))
})

test_that("Basic test to create x-mR control chart with stages - MR table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Zero", 3, 12, "", 5, "", "One", 23, 20, "", "", 21, "", "", 22
                                 ))
})


## X-mR chart with large moving range length (verified with Minitab)
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "Yield"
options$xmrChartMovingRangeLength <- 36
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("Test X-mR control chart with large moving range length - plot", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart4")
})

test_that("Test X-mR control chart with large moving range length - I table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3, 11, 4, 19, 4, 12, 24, 20, 23, 13, "", 21, 24, 14, "", 22, "",
                                      15, "", "", "", 16, "", "", "", 17, "", "", "", 18, "", "",
                                      "", 19, "", "", "", 20, "", "", "", 21, "", "", "", 22, "",
                                      "", "", 31, "", "", "", 32, "", "", "", 33, "", "", "", 34,
                                      "", "", "", 35, "", "", "", 36, "", ""))
})

## X-mR chart with stages and large moving range length (verified with Minitab)
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "Yield"
options$stage <- "Stage"
options$xmrChartMovingRangeLength <- 12
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("Test X-mR control chart with stages and large moving range length - plot", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart5")
})

test_that("Test X-mR control chart with stages and large moving range length - I table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Zero", 11, 4, "", 12, "", "One", 19, 24, "", 20, "", "", 21,
                                      "", "", 22, ""))
})

## Autocorrelation plot with different number of lags
options <- analysisOptions("variablesChartsIndividuals")
options$xmrChart <- FALSE
options$measurement <- "Yield"
options$autocorrelationPlot <- TRUE
options$autocorrelationPlotLagsNumber <- 4
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("Autocorrelation plot with changed number of lags", {
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

## Report including everything (verified with Minitab)
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

test_that("Basic test to create a report with all components", {
  plotName <- results[["results"]][["report"]][["collection"]][["report_report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "report1")
})

## Report including only x-mR chart
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

test_that("Basic test to create a report with only an IMR chart", {
  plotName <- results[["results"]][["report"]][["collection"]][["report_report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "report2")
})

# Missing values

## Missing values in measurement

### Single missing value

#### X-mR chart (verified with Minitab)
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "YieldMissing1"
options$axisLabels <- "Month"
options$xmrChartMovingRangeLength <- 2
options$autocorrelationPlot <- TRUE
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("X-mR control chart with single missing value in measurements - plot", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart6")
})

test_that("X-mR control chart with single missing value in measurements - I table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Mar Year 1", "Nov Year 1", "Apr Year 1", "Apr Year 1", "Dec Year 1",
                                      "Dec Year 2", "Nov Year 2", "Jan Year 2", "", "Dec Year 2",
                                      "Feb Year 2", "", "", "Mar Year 2", "", "", "Apr Year 2", "",
                                      "", "May Year 2", "", "", "Jun Year 2", "", "", "Jul Year 2",
                                      "", "", "Aug Year 2", "", "", "Sep Year 2", "", "", "Oct Year 2",
                                      "", "", "Jul Year 3", "", "", "Aug Year 3", "", "", "Sep Year 3",
                                      "", "", "Oct Year 3", "", "", "Nov Year 3", "", "", "Dec Year 3",
                                      ""))
})

test_that("X-mR control chart with single missing value in measurements - MR table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Dec Year 1", "Jan Year 2", "Feb Year 2", "Mar Year 2", "Aug Year 3",
                                      "Sep Year 3", "Oct Year 3", "Nov Year 3", "Dec Year 3"))
})

## Missing values in axis label (verified with Minitab)
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "Yield"
options$axisLabels <- "MonthMissing5"
options$xmrChartMovingRangeLength <- 2
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("X-mR control chart with missing values in axis labels - plot", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart7")
})

test_that("X-mR control chart with missing values in axis labels - I table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Mar Year 1", "Nov Year 1", "Apr Year 1", "Apr Year 1", "Dec Year 1",
                                      "Dec Year 2", "Nov Year 2", "", "", "Dec Year 2", "Feb Year 2",
                                      "", "", "Mar Year 2", "", "", "Apr Year 2", "", "", "May Year 2",
                                      "", "", "Jun Year 2", "", "", "Jul Year 2", "", "", "Aug Year 2",
                                      "", "", "Sep Year 2", "", "", "", "", "", "Jul Year 3", "",
                                      "", "", "", "", "Sep Year 3", "", "", "Oct Year 3", "", "",
                                      "Nov Year 3", "", "", "Dec Year 3", ""))
})

test_that("X-mR control chart with missing values in axis labels - MR table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Mar Year 1", "Dec Year 1", "May Year 1", "", "Nov Year 2", "Feb Year 2",
                                      "Jan Year 3", "Mar Year 2", "", "", "", "Sep Year 3", "", "Oct Year 3",
                                      "", "Nov Year 3", "", "Dec Year 3"))
})
