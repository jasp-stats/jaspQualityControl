context("[Quality Control] Time Weighted Charts")

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
options$exponentiallyWeightedMovingAverageChartSdMethod <- "averageMovingRange"
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
options$cumulativeSumChartSdMethod <- "averageMovingRange"
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
options$exponentiallyWeightedMovingAverageChartSdMethod <- "averageMovingRange"
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

# Std. dev. from moving ranges without a complete moving range ####
# The moving range sigma is used for individual observations (subgroups of size <= 1). A stage (or the data) without a
# complete moving range gives an error on that chart only.

## Moving range helper ####
test_that("MR0.1 Moving range sigma skips ranges with missing values", {
  expect_equal(jaspQualityControl:::.movingRangeSigma(c(1, 3, 2, 5), k = 2)[["sigma"]], 2 / 1.128)
  expect_equal(jaspQualityControl:::.movingRangeSigma(c(1, NA, 2, 4), k = 2)[["sigma"]], 2 / 1.128)
  expect_equal(dim(jaspQualityControl:::.movingRangeSigma(c(1, 2, 4), k = 3)[["matrix"]]), c(1, 3))
})

test_that("MR0.2 Moving range sigma errors without a complete moving range", {
  expect_error(jaspQualityControl:::.movingRangeSigma(5, k = 2),
               "The data contain 1 non-missing observation. Estimating the standard deviation from moving ranges of length 2",
               class = "qcDataError", fixed = TRUE)
  expect_error(jaspQualityControl:::.movingRangeSigma(c(1, NA, 2, NA, 3), k = 2),
               "The data contain 3 non-missing observations.", class = "qcDataError", fixed = TRUE)
  expect_error(jaspQualityControl:::.movingRangeSigma(c(1, 2), k = 3, stage = "B"),
               "Stage 'B' contains 2 non-missing observations. Estimating the standard deviation from moving ranges of length 3",
               class = "qcDataError", fixed = TRUE)
})

.twStageData <- function(subgroupSize, lastStageSubgroups = 1) {
  set.seed(1)
  stageSubgroups <- c(A = 6, B = 6, C = lastStageSubgroups)
  data.frame(
    Measurement = rnorm(sum(stageSubgroups) * subgroupSize),
    Stage       = factor(rep(names(stageSubgroups), stageSubgroups * subgroupSize))
  )
}

.twStageOptions <- function(subgroupSizeType, sdSource = "data") {
  sdMethod <- if (subgroupSizeType == "individual") "averageMovingRange" else "s"
  options <- analysisOptions("timeWeightedCharts")
  options$measurementLongFormat                             <- "Measurement"
  options$stagesLongFormat                                  <- "Stage"
  options$subgroupSizeType                                  <- subgroupSizeType
  options$manualSubgroupSizeValue                           <- 3
  options$cumulativeSumChart                                <- TRUE
  options$cumulativeSumChartSdSource                        <- sdSource
  options$cumulativeSumChartSdMethod                        <- sdMethod
  options$cumulativeSumChartSdValue                         <- 1
  options$exponentiallyWeightedMovingAverageChart           <- TRUE
  options$exponentiallyWeightedMovingAverageChartSdSource   <- sdSource
  options$exponentiallyWeightedMovingAverageChartSdMethod   <- sdMethod
  options$exponentiallyWeightedMovingAverageChartSdValue    <- 1
  options
}

.twCusumPlot <- function(results) results[["results"]][["CusumChart"]][["collection"]][["CusumChart_plot"]]
.twEwmaPlot  <- function(results) results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_plot"]]

.twExpectChartDrawn <- function(results, plot) {
  expect_identical(results[["status"]], "complete")
  expect_identical(plot[["status"]], "complete")
  expect_false(is.null(results[["state"]][["figures"]][[plot[["data"]]]][["obj"]]))
}

.twExpectChartError <- function(results, plot, message) {
  expect_identical(results[["status"]], "complete")
  expect_identical(plot[["status"]], "error")
  expect_match(plot[["error"]][["errorMessage"]], message, fixed = TRUE)
}

# y values of the center line and control limits (step layers) of a drawn chart
.twControlLimits <- function(results, plot) {
  plotObject <- results[["state"]][["figures"]][[plot[["data"]]]][["obj"]]
  stepLayers <- which(vapply(plotObject$layers, function(layer) inherits(layer$geom, "GeomStep"), logical(1)))
  lapply(stepLayers, function(i) ggplot2::layer_data(plotObject, i)[["y"]])
}

## Individual observations, stage with a single observation -> error ####
results <- runAnalysis("timeWeightedCharts", .twStageData(subgroupSize = 1), .twStageOptions("individual"))

test_that("MR1.1 Cumulative sum chart errors for stage with single individual observation", {
  .twExpectChartError(results, .twCusumPlot(results), "Stage 'C' contains 1 non-missing observation. Estimating the standard deviation from moving ranges of length 2")
})

test_that("MR1.2 EWMA chart errors for stage with single individual observation", {
  .twExpectChartError(results, .twEwmaPlot(results), "Stage 'C' contains 1 non-missing observation. Estimating the standard deviation from moving ranges of length 2")
})

## Same stage counts with subgroup size > 1 -> charts drawn ####
results <- runAnalysis("timeWeightedCharts", .twStageData(subgroupSize = 3), .twStageOptions("manual"))

test_that("MR2.1 Cumulative sum chart is drawn for stage with single subgroup of size > 1", {
  .twExpectChartDrawn(results, .twCusumPlot(results))
})

test_that("MR2.2 EWMA chart is drawn for stage with single subgroup of size > 1", {
  .twExpectChartDrawn(results, .twEwmaPlot(results))
})

## Historical std. dev. -> charts drawn ####
results <- runAnalysis("timeWeightedCharts", .twStageData(subgroupSize = 1), .twStageOptions("individual", sdSource = "historical"))

test_that("MR3.1 Cumulative sum chart with historical std. dev. is drawn for stage with single observation", {
  .twExpectChartDrawn(results, .twCusumPlot(results))
})

test_that("MR3.2 EWMA chart with historical std. dev. is drawn for stage with single observation", {
  .twExpectChartDrawn(results, .twEwmaPlot(results))
})

## Report mode, only the EWMA chart fails -> report keeps the cumulative sum chart ####
options <- .twStageOptions("individual")
options$report                                                   <- TRUE
options$exponentiallyWeightedMovingAverageChartMovingRangeLength <- 3
results <- runAnalysis("timeWeightedCharts", .twStageData(subgroupSize = 1, lastStageSubgroups = 2), options)

test_that("MR4 Report shows the cumulative sum chart and the EWMA chart error", {
  report <- results[["results"]][["report"]]
  expect_identical(results[["status"]], "complete")
  expect_identical(report[["status"]], "complete")
  subplots <- Filter(ggplot2::is_ggplot, results[["state"]][["figures"]][[report[["data"]]]][["obj"]]$subplots)
  hasSteps <- vapply(subplots, function(p) any(vapply(p$layers, function(l) inherits(l$geom, "GeomStep"), logical(1))), logical(1))
  labels   <- unlist(lapply(subplots, function(p) lapply(seq_along(p$layers), function(i) as.character(ggplot2::layer_data(p, i)[["label"]]))))
  expect_true(any(hasSteps))
  expect_true("Exponentially weighted moving average chart" %in% labels)
  expect_true(any(grepl("Stage 'C' contains 2 non-missing", labels, fixed = TRUE)))
})

## No stages, a single observation -> error ####
options <- .twStageOptions("individual")
options$stagesLongFormat <- ""
results <- runAnalysis("timeWeightedCharts", data.frame(Measurement = 5.1), options)

test_that("MR5 Charts without stages error for a single individual observation", {
  .twExpectChartError(results, .twCusumPlot(results), "The data contain 1 non-missing observation.")
  .twExpectChartError(results, .twEwmaPlot(results), "The data contain 1 non-missing observation.")
})

## Missing value in individual observations -> EWMA sigma from moving ranges ####
.twMissingData <- data.frame(Measurement = c(1.2, 2.3, 1.8, 2.9, NA, 2.1, 1.7, 2.4, 2.0, 1.9), Subgroup = paste0("g", 1:10))
options <- .twStageOptions("individual")
options$stagesLongFormat <- ""
resultsIndividual <- runAnalysis("timeWeightedCharts", .twMissingData, options)
options <- .twStageOptions("groupingVariable")
options$stagesLongFormat <- ""
options$subgroup         <- "Subgroup"
resultsGrouping <- runAnalysis("timeWeightedCharts", .twMissingData, options)

test_that("MR6.1 EWMA chart of individual observations with a missing value is drawn", {
  .twExpectChartDrawn(resultsIndividual, .twEwmaPlot(resultsIndividual))
})

test_that("MR6.2 EWMA limits with subgroups of size 1 and a missing value are from moving ranges, not zero width", {
  limitsGrouping <- .twControlLimits(resultsGrouping, .twEwmaPlot(resultsGrouping))
  expect_equal(limitsGrouping, .twControlLimits(resultsIndividual, .twEwmaPlot(resultsIndividual)))
  expect_gt(max(unlist(limitsGrouping), na.rm = TRUE) - min(unlist(limitsGrouping), na.rm = TRUE), 0.5)
})

## Stage of missing values only -> error ####
data <- .twStageData(subgroupSize = 1, lastStageSubgroups = 2)
data$Measurement[data$Stage == "C"] <- NA
results <- runAnalysis("timeWeightedCharts", data, .twStageOptions("individual"))

test_that("MR7 Charts error for a stage without non-missing observations", {
  .twExpectChartError(results, .twCusumPlot(results), "Stage 'C' contains 0 non-missing observations.")
  .twExpectChartError(results, .twEwmaPlot(results), "Stage 'C' contains 0 non-missing observations.")
})

## Across rows with one measurement column (moving range length not shown) -> error ####
options <- analysisOptions("timeWeightedCharts")
options$dataFormat                                      <- "wideFormat"
options$measurementsWideFormat                          <- "Measurement"
options$stagesWideFormat                                <- "Stage"
options$cumulativeSumChartSdMethod                      <- "s"
options$exponentiallyWeightedMovingAverageChart         <- TRUE
options$exponentiallyWeightedMovingAverageChartSdMethod <- "s"
results <- runAnalysis("timeWeightedCharts", .twStageData(subgroupSize = 1), options)

test_that("MR8 Charts across rows with one column error for stage with single observation", {
  .twExpectChartError(results, .twCusumPlot(results), "Stage 'C' contains 1 non-missing observation.")
  .twExpectChartError(results, .twEwmaPlot(results), "Stage 'C' contains 1 non-missing observation.")
})
