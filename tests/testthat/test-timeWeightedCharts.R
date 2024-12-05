context("[Quality Control] Time Weighted Charts")
.numDecimals <- 2

# Long Format ####

## Without Stages ####

### CUSUM (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$cumulativeSumChart <- TRUE
options$cumulativeSumChartSdSource <- "data"
options$cumulativeSumChartSdMethod <- "s"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/cumsumChartLong.csv", options)

test_that("LF1.1 Basic test of Cumulative sum chart", {
  plotName <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-sum-chart_LF1")
})

test_that("LF1.2 Basic test of Cumulative sum chart table", {
  table <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 4 - upper", "Point 5 - upper", "Point 6 - upper", "Point 7 - upper",
                                      "Point 8 - upper", "Point 9 - upper", "Point 10 - upper"))
})

### EWMA (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$cumulativeSumChart <- FALSE
options$exponentiallyWeightedMovingAverageChart <- TRUE
options$exponentiallyWeightedMovingAverageChartSdSource <- "data"
options$exponentiallyWeightedMovingAverageChartSdMethod <- "s"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/ewmaChartLong.csv", options)

test_that("LF1.3 Basic test of Exponentially weighted moving average chart", {
  plotName <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "exponentially-weighted-moving-average-chartLF1")
})

test_that("LF1.4 Basic test of Exponentially weighted moving average chart table", {
  table <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 5"))
})

## With Stages ####

### CUSUM (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$stagesLongFormat <- "Stage"
options$cumulativeSumChart <- TRUE
options$cumulativeSumChartSdSource <- "data"
options$cumulativeSumChartSdMethod <- "s"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/cumsumChartLong.csv", options)

test_that("LF2.1 Basic test of Cumulative sum chart with stages", {
  plotName <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-sum-chartLF2")
})

test_that("LF2.2 Basic test of Cumulative sum chart with stages table", {
  table <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Point 5 - upper", "", "Point 6 - upper", "", "Point 7 - upper",
                                      "", "Point 8 - upper"))
})

### EWMA (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "individual"
options$stagesLongFormat <- "Stage"
options$cumulativeSumChart <- FALSE
options$exponentiallyWeightedMovingAverageChart <- TRUE
options$exponentiallyWeightedMovingAverageChartSdSource <- "data"
options$exponentiallyWeightedMovingAverageChartSdMethod <- "s"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/ewmaChartLong.csv", options)

test_that("LF2.3 Basic test of Exponentially weighted moving average chart with stages", {
  plotName <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "exponentially-weighted-moving-average-chart")
})

test_that("LF2.4 Basic test of Exponentially weighted moving average chart with stages table", {
  table <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

## Subgrouping Mechanisms ####

### Grouping Variable ####

#### CUSUM (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "groupingVariable"
options$subgroup <- "Date"
options$cumulativeSumChart <- TRUE
options$cumulativeSumChartSdSource <- "data"
options$cumulativeSumChartSdMethod <- "s"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/cumsumChartLong.csv", options)

test_that("LF3.1 Test of Cumulative sum chart with subgroup variable", {
  plotName <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-sum-chartLF3")
})

test_that("LF3.2 Test of Cumulative sum chart with subgroup variable table", {
  table <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 4 - upper (10/Apr)", "Point 5 - upper (10/May)", "Point 6 - upper (10/Jun)",
                                      "Point 7 - upper (10/Jul)", "Point 8 - upper (10/Aug)", "Point 9 - upper (10/Nov)",
                                      "Point 10 - upper (10/Dec)"))
})

#### EWMA (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "groupingVariable"
options$subgroup <- "Subgroup"
options$cumulativeSumChart <- FALSE
options$exponentiallyWeightedMovingAverageChart <- TRUE
options$exponentiallyWeightedMovingAverageChartSdSource <- "data"
options$exponentiallyWeightedMovingAverageChartSdMethod <- "s"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/ewmaChartLong.csv", options)

test_that("LF3.3 Test of Exponentially weighted moving average chart with subgroup variable", {
  plotName <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "exponentially-weighted-moving-average-chartLF3")
})

test_that("LF3.4 Test of Exponentially weighted moving average chart with subgroup variable table", {
  table <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 30 (30)"))
})

### No Subgroups ####

#### CUSUM (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "individual"
options$cumulativeSumChart <- TRUE
options$cumulativeSumChartSdSource <- "data"
options$cumulativeSumChartSdMethod <- "s"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/cumsumChartLong.csv", options)

test_that("LF4.1 Test of Cumulative sum chart without subgroups", {
  plotName <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-sum-chartLF4")
})

test_that("LF4.2 Test of Cumulative sum chart without subgroups table", {
  table <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 8 - upper", "Point 9 - upper", "Point 10 - upper", "Point 11 - upper",
                                      "Point 13 - upper", "Point 14 - upper", "Point 16 - upper",
                                      "Point 17 - upper", "Point 18 - upper", "Point 19 - upper",
                                      "Point 20 - upper", "Point 21 - upper", "Point 22 - upper",
                                      "Point 23 - upper", "Point 24 - upper", "Point 25 - upper",
                                      "Point 26 - upper", "Point 27 - upper", "Point 28 - upper",
                                      "Point 29 - upper", "Point 30 - upper", "Point 31 - upper",
                                      "Point 32 - upper", "Point 33 - upper", "Point 34 - upper",
                                      "Point 35 - upper", "Point 36 - upper", "Point 37 - upper",
                                      "Point 38 - upper", "Point 40 - upper", "Point 41 - upper",
                                      "Point 42 - upper", "Point 43 - upper", "Point 46 - lower",
                                      "Point 47 - lower", "Point 88 - lower", "Point 89 - lower",
                                      "Point 90 - lower", "Point 91 - lower", "Point 92 - lower",
                                      "Point 93 - lower", "Point 97 - lower"))
})

#### EWMA (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "individual"
options$cumulativeSumChart <- FALSE
options$exponentiallyWeightedMovingAverageChart <- TRUE
options$exponentiallyWeightedMovingAverageChartSdSource <- "data"
options$exponentiallyWeightedMovingAverageChartSdMethod <- "s"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/ewmaChartLong.csv", options)

test_that("LF4.3 Test of Exponentially weighted moving average chart without subgroups", {
  plotName <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "exponentially-weighted-moving-average-chartLF4")
})

test_that("LF4.4 Test of Exponentially weighted moving average chart without subgroups table", {
  table <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 30"))
})

## Plotting Options ####

### Historical values ####

#### CUSUM (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$cumulativeSumChart <- TRUE
options$cumulativeSumChartSdSource <- "historical"
options$cumulativeSumChartSdValue <- 3
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/cumsumChartLong.csv", options)

test_that("LF5.1 Test of Cumulative sum chart with historical std. dev.", {
  plotName <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-sum-chartLF5")
})

test_that("LF5.2 Test of Cumulative sum chart with historical std. dev. table", {
  table <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 4 - upper", "Point 5 - upper", "Point 6 - upper", "Point 7 - upper",
                                      "Point 8 - upper", "Point 9 - upper", "Point 10 - upper"))
})

#### EWMA (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$cumulativeSumChart <- FALSE
options$exponentiallyWeightedMovingAverageChart <- TRUE
options$exponentiallyWeightedMovingAverageChartSdSource <- "historical"
options$exponentiallyWeightedMovingAverageChartSdValue <- 3
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/ewmaChartLong.csv", options)

test_that("LF5.3 Test of Exponentially weighted moving average chart with historical std. dev.", {
  plotName <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "exponentially-weighted-moving-average-chartLF5")
})

test_that("LF5.4 Test of Exponentially weighted moving average chart with historical std. dev. table", {
  table <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

### Alternative SD Estimation ####

#### CUSUM (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$cumulativeSumChart <- TRUE
options$cumulativeSumChartSdSource <- "data"
options$cumulativeSumChartSdMethod <- "r"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/cumsumChartLong.csv", options)

test_that("LF6.1 Test of Cumulative sum chart with r-bar std. dev.", {
  plotName <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-sum-chartLF6")
})

test_that("LF6.2 Test of Cumulative sum chart with r-bar std. dev. table", {
  table <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 4 - upper", "Point 5 - upper", "Point 6 - upper", "Point 7 - upper",
                                      "Point 8 - upper", "Point 9 - upper", "Point 10 - upper"))
})

#### EWMA (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$cumulativeSumChart <- FALSE
options$exponentiallyWeightedMovingAverageChart <- TRUE
options$exponentiallyWeightedMovingAverageChartSdSource <- "data"
options$exponentiallyWeightedMovingAverageChartSdMethod <- "r"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/ewmaChartLong.csv", options)

test_that("LF6.3 Test of Exponentially weighted moving average chart with r-bar std. dev.", {
  plotName <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "exponentially-weighted-moving-average-chartLF6")
})

test_that("LF6.4 Test of Exponentially weighted moving average chart with r-bar std. dev. table", {
  table <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 5"))
})

## Report ####

### CUSUM (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$cumulativeSumChart <- TRUE
options$cumulativeSumChartSdSource <- "data"
options$cumulativeSumChartSdMethod <- "s"
options$report <- TRUE
options$reportMeasurementName <- TRUE
options$reportMeasurementNameText <- "Test Name"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/cumsumChartLong.csv", options)

test_that("LF7.1 Test of Cumulative sum chart report", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "time-weighted-charts-reportLF7_1")
})

### EWMA (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$cumulativeSumChart <- FALSE
options$exponentiallyWeightedMovingAverageChart <- TRUE
options$exponentiallyWeightedMovingAverageChartSdSource <- "data"
options$exponentiallyWeightedMovingAverageChartSdMethod <- "s"
options$report <- TRUE
options$reportMeasurementName <- TRUE
options$reportMeasurementNameText <- "Test Name"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/ewmaChartLong.csv", options)

test_that("LF7.2 Test of Exponentially weighted moving average chart report", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "time-weighted-charts-report7_2")
})

# Wide Format ####

## Without Stages (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
options$cumulativeSumChart <- TRUE
options$cumulativeSumChartSdSource <- "data"
options$cumulativeSumChartSdMethod <- "s"
options$exponentiallyWeightedMovingAverageChart <- TRUE
options$exponentiallyWeightedMovingAverageChartSdSource <- "data"
options$exponentiallyWeightedMovingAverageChartSdMethod <- "s"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/cumsumChartWide.csv", options)

test_that("WF1.1 Basic test of Cumulative sum chart", {
  plotName <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-sum-chart_WF1")
})

test_that("WF1.2 Basic test of Cumulative sum chart table", {
  table <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 4 - upper", "Point 5 - upper", "Point 6 - upper", "Point 7 - upper",
                                      "Point 8 - upper", "Point 9 - upper", "Point 10 - upper"))
})

test_that("WF1.3 Basic test of Exponentially weighted moving average chart", {
  plotName <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "exponentially-weighted-moving-average-chart_WF1")
})

test_that("WF1.3 Basic test of Exponentially weighted moving average chart table", {
  table <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 5"))
})

## With Stages (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
options$stagesWideFormat <- "Stage"
options$cumulativeSumChart <- TRUE
options$cumulativeSumChartSdSource <- "data"
options$cumulativeSumChartSdMethod <- "s"
options$exponentiallyWeightedMovingAverageChart <- TRUE
options$exponentiallyWeightedMovingAverageChartSdSource <- "data"
options$exponentiallyWeightedMovingAverageChartSdMethod <- "s"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/cumsumChartWide.csv", options)

test_that("WF2.1 Basic test of Cumulative sum chart with stages", {
  plotName <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-sum-chart_WF2")
})

test_that("WF2.2 Basic test of Cumulative sum chart with stages table", {
  table <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "Point 5 - upper", "", "Point 6 - upper", "", "Point 7 - upper",
                                      "", "Point 8 - upper"))
})

test_that("WF2.3 Basic test of Exponentially weighted moving average chart with stages", {
  plotName <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "exponentially-weighted-moving-average-chart_WF2")
})

test_that("WF2.4 Basic test of Exponentially weighted moving average chart with stages table", {
  table <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

## Plotting Options (verified with Minitab) ####

### Historical values ####
options <- analysisOptions("timeWeightedCharts")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
options$cumulativeSumChart <- TRUE
options$cumulativeSumChartSdSource <- "historical"
options$cumulativeSumChartSdValue <- 3
options$exponentiallyWeightedMovingAverageChart <- TRUE
options$exponentiallyWeightedMovingAverageChartSdSource <- "historical"
options$exponentiallyWeightedMovingAverageChartSdValue <- 3
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/cumsumChartWide.csv", options)

test_that("WF3.1 Test of Cumulative sum chart with historical std. dev.", {
  plotName <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-sum-chart_WF3")
})

test_that("WF3.1 Test of Cumulative sum chart with historical std. dev. table", {
  table <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 4 - upper", "Point 5 - upper", "Point 6 - upper", "Point 7 - upper",
                                      "Point 8 - upper", "Point 9 - upper", "Point 10 - upper"))
})

test_that("WF3.3 Test of Exponentially weighted moving average chart with historical std. dev.", {
  plotName <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "exponentially-weighted-moving-average-chart_WF3")
})

test_that("WF3.4 Test of Exponentially weighted moving average chart with historical std. dev. table", {
  table <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 2", "Point 5", "Point 6"))
})

### Alternative SD Estimation (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
options$cumulativeSumChart <- TRUE
options$cumulativeSumChartSdSource <- "data"
options$cumulativeSumChartSdMethod <- "r"
options$exponentiallyWeightedMovingAverageChart <- TRUE
options$exponentiallyWeightedMovingAverageChartSdSource <- "data"
options$exponentiallyWeightedMovingAverageChartSdMethod <- "r"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/cumsumChartWide.csv", options)

test_that("WF4.1 Test of Cumulative sum chart with r-bar std. dev.", {
  plotName <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-sum-chart_WF4")
})

test_that("WF4.2 Test of Cumulative sum chart with r-bar std. dev. table", {
  table <- results[["results"]][["CusumChart"]][["collection"]][["CusumChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 4 - upper", "Point 5 - upper", "Point 6 - upper", "Point 7 - upper",
                                      "Point 8 - upper", "Point 9 - upper", "Point 10 - upper"))
})

test_that("WF4.3 Test of Exponentially weighted moving average chart with historical std. dev.", {
  plotName <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "exponentially-weighted-moving-average-chart_WF4")
})

test_that("WF4.4 Test of Exponentially weighted moving average chart with historical std. dev. table", {
  table <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 5"))
})

## Report (verified with Minitab) ####
options <- analysisOptions("timeWeightedCharts")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
options$cumulativeSumChart <- TRUE
options$cumulativeSumChartSdSource <- "data"
options$cumulativeSumChartSdMethod <- "s"
options$exponentiallyWeightedMovingAverageChart <- TRUE
options$exponentiallyWeightedMovingAverageChartSdSource <- "data"
options$exponentiallyWeightedMovingAverageChartSdMethod <- "s"
options$report <- TRUE
options$reportMeasurementName <- TRUE
options$reportMeasurementNameText <- "Test Name"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/cumsumChartWide.csv", options)

test_that("WF5 Test of Time weighted charts report", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "time-weighted-charts-report_WF5")
})
