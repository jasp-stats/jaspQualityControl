context("[Quality Control] Variables Charts for Individuals")
set.seed(1)

# Basic tests ####

## X-mR chart (verified with Minitab) ####
options <- analysisOptions("variablesChartsIndividuals")
options$testSet <- "jaspDefault"
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
options$testSet <- "jaspDefault"
options$measurement <- "Yield"
options$xmrChartMovingRangeLength <- 4
results <- runAnalysis("variablesChartsIndividuals",
                       "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

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
options$testSet <- "jaspDefault"
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
                                      "Point 20", "", "", "", "Point 21", "", "", "", "Point 22", ""
                                 ))
})

test_that("4.3 Basic test to create x-mR control chart with stages - MR table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Zero", "Point 3", "Point 12", "", "Point 5", "", "One", "Point 23",
                                      "Point 20", "", "", "Point 21", "", "", "Point 22"))
})


## X-mR chart with large moving range length (verified with Minitab) ####
options <- analysisOptions("variablesChartsIndividuals")
options$testSet <- "jaspDefault"
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
options$testSet <- "jaspDefault"
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

## X-mR chart with stages: test results use chart positions and timestamps (jasp-issues#4672) ####
# The second stage comes first alphabetically and has one out-of-control point (row 18)
stagedDataset <- data.frame(
  Viscosity = c(10, 11, 10, 11, 10, 11, 10, 11, 10, 11,
                20, 21, 20, 21, 20, 21, 20, 40, 20, 21),
  Stage     = rep(c("Before", "After"), each = 10),
  Day       = paste("Day", 1:20)
)
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "Viscosity"
options$stage <- "Stage"
options$axisLabels <- "Day"
options$xmrChartMovingRangeLength <- 2
options$testSet <- "custom"
options$rule1 <- TRUE
options$rule2 <- FALSE
options$rule3 <- FALSE
options$rule4 <- FALSE
options$rule5 <- FALSE
options$rule6 <- FALSE
options$rule7 <- FALSE
options$rule8 <- FALSE
results <- runAnalysis("variablesChartsIndividuals", stagedDataset, options)

test_that("6.4 X-mR chart with stages reports chart positions in later stages - I table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("After", "Point 18 (Day 18)"))
})

test_that("6.5 X-mR chart with stages reports chart positions in later stages - MR table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("After", "Point 18 (Day 18)", "", "Point 19 (Day 19)"))
})

## Autocorrelation plot with different number of lags ####
options <- analysisOptions("variablesChartsIndividuals")
options$testSet <- "jaspDefault"
options$xmrChart <- FALSE
options$measurement <- "Yield"
options$autocorrelationPlot <- TRUE
options$autocorrelationPlotLagsNumber <- 4
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("7. Autocorrelation plot with changed number of lags", {
  results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)
  plotName <- results[["results"]][["autocorrelationPlot"]][["collection"]][["autocorrelationPlot_Yield"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "autocorrelation-plot2")
})

## Report including everything (verified with Minitab) ####
options <- analysisOptions("variablesChartsIndividuals")
options$testSet <- "jaspDefault"
options$measurement <- "Yield"
options$axisLabels <- "Month"
options$xmrChartMovingRangeLength <- 2
options$report <- TRUE
options$reportAutocorrelationChart <- TRUE
options$reportIMRChart <- TRUE
options$reportMetaData <- TRUE
options$reportTitleText <- "new title test"
options$reportChartNameText <- "chart name test"
options$reportSubtitleText <- "subtitle test"
options$reportMeasurementNameText <- "measurement name test"
options$reportFootnoteText <- "footnote test"
options$reportLocationText <- "location test"
options$reportDateText <- "01.01.2000"
options$reportPerformedByText <- "Your name"
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("8. Basic test to create a report with all components", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "report1")
})

## Report including only x-mR chart ####
options <- analysisOptions("variablesChartsIndividuals")
options$testSet <- "jaspDefault"
options$measurement <- "Yield"
options$axisLabels <- "Month"
options$stage <- "Stage"
options$xmrChartMovingRangeLength <- 2
options$report <- TRUE
options$reportAutocorrelationChart <- FALSE
options$reportIMRChart <- TRUE
options$reportMetaData <- FALSE
options$reportTitle <- TRUE
results <- runAnalysis("variablesChartsIndividuals", "datasets/variableChartsIndividuals/variableChartsIndividualsDebug.csv", options)

test_that("9. Basic test to create a report with only an IMR chart", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "report2")
})

# Missing values ####

## Missing values in measurement ####

### Single missing value ####

#### X-mR chart (verified with Minitab) ####
options <- analysisOptions("variablesChartsIndividuals")
options$testSet <- "jaspDefault"
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
                                 list("Point 3 (Mar Year 1)", "Point 12 (Dec Year 1)", "Point 5 (May Year 1)",
                                      "Point 13 (Jan Year 2)", "Point 23 (Nov Year 2)", "Point 14 (Feb Year 2)",
                                      "Point 25 (Jan Year 3)", "Point 15 (Mar Year 2)", "", "Point 32 (Aug Year 3)",
                                      "", "Point 33 (Sep Year 3)", "", "Point 34 (Oct Year 3)", "",
                                      "Point 35 (Nov Year 3)", "", "Point 36 (Dec Year 3)"))
})

## Missing values in axis label (verified with Minitab) ####
options <- analysisOptions("variablesChartsIndividuals")
options$testSet <- "jaspDefault"
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
                                 list("Point 3 (Mar Year 1)", "Point 11 (Nov Year 1)",
                                      "Point 4 (Apr Year 1)", "Point 4 (Apr Year 1)",
                                      "Point 12 (Dec Year 1)", "Point 24 (Dec Year 2)",
                                      "Point 23 (Nov Year 2)", "Point 13 ()", "",
                                      "Point 24 (Dec Year 2)", "Point 14 (Feb Year 2)", "", "",
                                      "Point 15 (Mar Year 2)", "", "", "Point 16 (Apr Year 2)", "", "",
                                      "Point 17 (May Year 2)", "", "", "Point 18 (Jun Year 2)", "", "",
                                      "Point 19 (Jul Year 2)", "", "", "Point 20 (Aug Year 2)", "", "",
                                      "Point 21 (Sep Year 2)", "", "", "Point 22 ()", "", "",
                                      "Point 31 (Jul Year 3)", "", "", "Point 32 ()", "", "",
                                      "Point 33 (Sep Year 3)", "", "", "Point 34 (Oct Year 3)", "", "",
                                      "Point 35 (Nov Year 3)", "", "", "Point 36 (Dec Year 3)", ""))
})

test_that("11.3 X-mR control chart with missing values in axis labels - MR table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 3 (Mar Year 1)", "Point 12 (Dec Year 1)",
                                      "Point 5 (May Year 1)", "Point 13 ()", "Point 23 (Nov Year 2)",
                                      "Point 14 (Feb Year 2)", "Point 25 (Jan Year 3)",
                                      "Point 15 (Mar Year 2)", "", "Point 32 ()", "",
                                      "Point 33 (Sep Year 3)", "", "Point 34 (Oct Year 3)", "",
                                      "Point 35 (Nov Year 3)", "", "Point 36 (Dec Year 3)"))
})

# Out of control rules (verified with Minitab) ####
options <- analysisOptions("variablesChartsIndividuals")
options$measurement <- "V1"
options$xmrChartMovingRangeLength <- 2
options$testSet <- "custom"
options$rule1 <- TRUE
options$rule2 <- TRUE
options$rule2Value <- 7
options$rule3 <- TRUE
options$rule3Value <- 7
options$rule4 <- TRUE
options$rule4Value <- 2
options$rule5 <- TRUE
options$rule5Value <- 15
options$rule6 <- TRUE
options$rule6Value <- 8
options$rule7 <- TRUE
options$rule7Value <- 4
options$rule8 <- TRUE
options$rule8Value <- 14
results <- runAnalysis("variablesChartsIndividuals", "datasets/controlChartRules/violatingAllindividualRules.csv", options)

test_that("12.1 Test of all rules for x-mr chart", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart12")
})

test_that("12.2 Test of all rules for x-mr chart - I Table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 37", "Point 22", "Point 22", "Point 38", "Point 29", "Point 44",
                                      "Point 40", "Point 15", "Point 38", "Point 23", "", "Point 39",
                                      "Point 30", "Point 45", "Point 41", "Point 16", "Point 39",
                                      "Point 24", "", "Point 40", "Point 31", "", "Point 42", "",
                                      "Point 40", "Point 25", "", "Point 41", "Point 32", "", "Point 43",
                                      "", "Point 41", "Point 26", "", "Point 42", "Point 33", "",
                                      "Point 44", "", "Point 42", "Point 27", "", "Point 43", "Point 34",
                                      "", "", "", "Point 43", "Point 28", "", "Point 44", "Point 35",
                                      "", "", "", "Point 44", "Point 29", "", "", "Point 36", "",
                                      "", "", "Point 45", "Point 30", "", "", "", "", "", "", "",
                                      "Point 31", "", "", "", "", "", "", "", "Point 32", "", "",
                                      "", "", "", "", "", "Point 33", "", "", "", "", "", "", "",
                                      "Point 34", "", "", "", "", "", "", "", "Point 35", "", "",
                                      "", "", "", "", "", "Point 36", "", "", "", "", "", "", "",
                                      "Point 43", "", "", "", "", "", "", "", "Point 44", "", "",
                                      "", "", "", ""))
})

test_that("12.3 Test of all rules for x-mr chart - MR Table", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 37", "Point 8", "Point 45", "Point 9", "", "Point 10",
                                      "", "Point 11", "", "Point 12", "", "Point 13", "", "Point 14",
                                      "", "Point 15", "", "Point 16", "", "Point 23", "", "Point 24",
                                      "", "Point 25", "", "Point 26", "", "Point 27", "", "Point 28",
                                      "", "Point 29", "", "Point 30", "", "Point 31", "", "Point 32",
                                      "", "Point 33", "", "Point 34", "", "Point 35", "", "Point 36",
                                      "", "Point 44"))
})

# Std. dev. from moving ranges without a complete moving range ####
.imrOptions <- function(stage = "", movingRangeLength = 3) {
  options <- analysisOptions("variablesChartsIndividuals")
  options$measurement               <- "Measurement"
  options$stage                     <- stage
  options$xmrChartMovingRangeLength <- movingRangeLength
  options
}

results <- runAnalysis("variablesChartsIndividuals", data.frame(Measurement = c(1.2, NA, 2.3, NA)), .imrOptions(movingRangeLength = 2))

test_that("MR1 X-mR chart without stages errors when every moving range contains a missing value", {
  plot <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]]
  expect_identical(results[["status"]], "complete")
  expect_identical(plot[["status"]], "error")
  expect_match(plot[["error"]][["errorMessage"]], "Cannot estimate the standard deviation from moving ranges of length 2: the data have fewer than 2 consecutive non-missing observations.", fixed = TRUE)
})

data <- data.frame(Measurement = c(1.2, 2.3, 1.8, 2.9, 2.1, 1.7, NA, 2.4, NA), Stage = rep(c("A", "B"), c(6, 3)))
results <- runAnalysis("variablesChartsIndividuals", data, .imrOptions(stage = "Stage"))

test_that("MR2 X-mR chart errors for a stage whose moving ranges all contain a missing value", {
  plot <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]]
  expect_identical(results[["status"]], "complete")
  expect_identical(plot[["status"]], "error")
  expect_match(plot[["error"]][["errorMessage"]], "stage 'B' has fewer than 3 consecutive non-missing observations.", fixed = TRUE)
})

options <- .imrOptions(stage = "Stage")
options$report                     <- TRUE
options$reportAutocorrelationChart <- FALSE
results <- runAnalysis("variablesChartsIndividuals", data, options)

test_that("MR3 Report shows the X-mR chart error and keeps the meta data", {
  report <- results[["results"]][["report"]]
  expect_identical(results[["status"]], "complete")
  expect_identical(report[["status"]], "complete")
  subplots <- Filter(ggplot2::is_ggplot, results[["state"]][["figures"]][[report[["data"]]]][["obj"]]$subplots)
  labels   <- unlist(lapply(subplots, function(p) lapply(seq_along(p$layers), function(i) as.character(ggplot2::layer_data(p, i)[["label"]]))))
  expect_true(any(grepl("stage 'B' has fewer than 3 consecutive", labels, fixed = TRUE)))
  expect_true(any(grepl("Chart name:", labels, fixed = TRUE)))
})
