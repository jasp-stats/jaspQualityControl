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
  plotName <- results[["results"]][["CusumChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-sum-chart_LF1")
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

test_that("LF1.2 Basic test of Exponentially weighted moving average chart", {
  plotName <- results[["results"]][["EWMAPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "exponentially-weighted-moving-average-chartLF1")
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
  plotName <- results[["results"]][["CusumChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-sum-chartLF2")
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

test_that("LF2.2 Basic test of Exponentially weighted moving average chart with stages", {
  plotName <- results[["results"]][["EWMAPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "exponentially-weighted-moving-average-chart")
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
  plotName <- results[["results"]][["CusumChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-sum-chartLF3")
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

test_that("LF3.2 Test of Exponentially weighted moving average chart with subgroup variable", {
  plotName <- results[["results"]][["EWMAPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "exponentially-weighted-moving-average-chartLF3")
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
  plotName <- results[["results"]][["CusumChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-sum-chartLF4")
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

test_that("LF4.2 Test of Exponentially weighted moving average chart without subgroups", {
  plotName <- results[["results"]][["EWMAPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "exponentially-weighted-moving-average-chartLF4")
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
  plotName <- results[["results"]][["CusumChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-sum-chartLF5")
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

test_that("LF5.2 Test of Exponentially weighted moving average chart with historical std. dev.", {
  plotName <- results[["results"]][["EWMAPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "exponentially-weighted-moving-average-chartLF5")
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
  plotName <- results[["results"]][["CusumChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-sum-chartLF6")
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

test_that("LF6.2 Test of Exponentially weighted moving average chart with r-bar std. dev.", {
  plotName <- results[["results"]][["EWMAPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "exponentially-weighted-moving-average-chartLF6")
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

## Without Stages ####
# options <- analysisOptions("timeWeightedCharts")
# options$measurementsWideFormat <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")
# options$cumulativeSumChart <- TRUE
# options$cumulativeSumChartSdSource <- "data"
# options$cumulativeSumChartSdMethod <- "s"
# options$exponentiallyWeightedMovingAverageChart <- TRUE
# options$exponentiallyWeightedMovingAverageChartSdSource <- "data"
# options$exponentiallyWeightedMovingAverageChartSdMethod <- "s"
# results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/cumsumChartWide.csv", options, makeTests = T)

## With Stages ####


## Plotting Options ####

### Historical values ####

### Alternative SD Estimation ####

## Report ####
