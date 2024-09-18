context("[Quality Control] Rare Event Charts")
.numDecimals <- 2

# Date input ####

## MD (g chart verified with Minitab, T chart differs because of zero handling) ####
options <- analysisOptions("rareEventCharts")
options$variable <- "MD"
options$dataType <- "dataTypeDates"
options$dataTypeDatesStructure <- "dateOnly"
options$dataTypeDatesFormatDate <- "md"
options$gChart <- TRUE
options$tChart <- TRUE
results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options)

test_that("1.1 Test of G chart with MD date format", {
  plotName <- results[["results"]][["gChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart1")
})

test_that("1.2 Test of T chart with MD date format", {
  plotName <- results[["results"]][["tChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart1")
})

## MDHM (t chart verified with Minitab, g chart not possible with this format in Minitab) ####
options <- analysisOptions("rareEventCharts")
options$variable <- "MDHM"
options$dataType <- "dataTypeDates"
options$dataTypeDatesStructure <- "dateTime"
options$dataTypeDatesFormatDate <- "md"
options$dataTypeDatesFormatTime <- "HM"
options$gChart <- TRUE
options$tChart <- TRUE
results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options)

test_that("2.1 Test of G chart with MDHM date format", {
  plotName <- results[["results"]][["gChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart2")
})

test_that("2.2 Test of T chart with MDHM date format", {
  plotName <- results[["results"]][["tChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart2")
})


## DM (verified with Minitab) ####
options <- analysisOptions("rareEventCharts")
options$variable <- "DM"
options$dataType <- "dataTypeDates"
options$dataTypeDatesStructure <- "dateOnly"
options$dataTypeDatesFormatDate <- "dm"
options$gChart <- TRUE
options$tChart <- TRUE
results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options)

test_that("3.1 Test of G chart with DM date format", {
  plotName <- results[["results"]][["gChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart3")
})

test_that("3.2 Test of T chart with DM date format", {
  plotName <- results[["results"]][["tChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart3")
})

## DMY (verified with Minitab) ####
options <- analysisOptions("rareEventCharts")
options$variable <- "DMY"
options$dataType <- "dataTypeDates"
options$dataTypeDatesStructure <- "dateOnly"
options$dataTypeDatesFormatDate <- "dmy"
options$gChart <- TRUE
options$tChart <- TRUE
results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options)

test_that("4.1 Test of G chart with DMY date format", {
  plotName <- results[["results"]][["gChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart4")
})

test_that("4.2 Test of T chart with DMY date format", {
  plotName <- results[["results"]][["tChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart4")
})

## MDY (verified with Minitab) ####
options <- analysisOptions("rareEventCharts")
options$variable <- "MDY"
options$dataType <- "dataTypeDates"
options$dataTypeDatesStructure <- "dateOnly"
options$dataTypeDatesFormatDate <- "mdy"
options$gChart <- TRUE
options$tChart <- TRUE
results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options)

test_that("5.1 Test of G chart with MDY date format", {
  plotName <- results[["results"]][["gChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart5")
})

test_that("5.2 Test of T chart with MDY date format", {
  plotName <- results[["results"]][["tChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart5")
})

## YMD (verified with Minitab) ####
options <- analysisOptions("rareEventCharts")
options$variable <- "YMD"
options$dataType <- "dataTypeDates"
options$dataTypeDatesStructure <- "dateOnly"
options$dataTypeDatesFormatDate <- "ymd"
options$gChart <- TRUE
options$tChart <- TRUE
results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options)

test_that("6.1 Test of G chart with YMD date format", {
  plotName <- results[["results"]][["gChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart6")
})

test_that("6.2 Test of T chart with YMD date format", {
  plotName <- results[["results"]][["tChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart6")
})

# Time input ####

## Decimal Hours (t chart verified with Minitab, g chart not possible with this format in Minitab) ####
options <- analysisOptions("rareEventCharts")
options$variable <- "decimalTime"
options$dataType <- "dataTypeInterval"
options$dataTypeIntervalType <- "hours"
options$gChart <- TRUE
options$tChart <- TRUE
results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options)

test_that("7.1 Test of G chart with decimal hour format", {
  plotName <- results[["results"]][["gChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart7")
})

test_that("7.2 Test of T chart with decimal hour format", {
  plotName <- results[["results"]][["tChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart7")
})

## Decimal Days (t chart verified with Minitab, g chart not possible with this format in Minitab) ####
options <- analysisOptions("rareEventCharts")
options$variable <- "decimalDays"
options$dataType <- "dataTypeInterval"
options$dataTypeIntervalType <- "days"
options$gChart <- TRUE
options$tChart <- TRUE
results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options)

test_that("8.1 Test of G chart with decimal day format", {
  plotName <- results[["results"]][["gChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart8")
})

test_that("8.2 Test of T chart with decimal day format", {
  plotName <- results[["results"]][["tChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart8")
})

# Opportunities input (verified with Minitab) ####
options <- analysisOptions("rareEventCharts")
options$variable <- "opportunities"
options$dataType <- "dataTypeInterval"
options$dataTypeIntervalType <- "opportunities"
options$gChart <- TRUE
options$tChart <- TRUE
results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options)

test_that("9.1 Test of G chart with opportunities format", {
  plotName <- results[["results"]][["gChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart9")
})

test_that("9.2 Test of T chart with opportunities format", {
  plotName <- results[["results"]][["tChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart9")
})

# Stages ####

## Date format (verified with Minitab) ####
options <- analysisOptions("rareEventCharts")
options$variable <- "DM"
options$stage <- "stages"
options$dataType <- "dataTypeDates"
options$dataTypeDatesStructure <- "dateOnly"
options$dataTypeDatesFormatDate <- "dm"
options$gChart <- TRUE
options$tChart <- TRUE
results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options)

test_that("10.1 Test of G chart with DM format and stages", {
  plotName <- results[["results"]][["gChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart10")
})

test_that("10.2 Test of T chart with DM format and stages", {
  plotName <- results[["results"]][["tChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart10")
})

## Interval format (verified with Minitab) ####
options <- analysisOptions("rareEventCharts")
options$variable <- "opportunities"
options$stage <- "stages"
options$dataType <- "dataTypeInterval"
options$dataTypeIntervalType <- "opportunities"
options$gChart <- TRUE
options$tChart <- TRUE
results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options)

test_that("11.1 Test of G chart with interval format and stages", {
  plotName <- results[["results"]][["gChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart11")
})

test_that("11.2 Test of G chart with interval format and stages", {
  plotName <- results[["results"]][["tChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart11")
})

# Historical options (verified with Minitab) ####
options <- analysisOptions("rareEventCharts")
options$variable <- "DM"
options$dataType <- "dataTypeDates"
options$dataTypeDatesStructure <- "dateOnly"
options$dataTypeDatesFormatDate <- "dm"
options$gChart <- TRUE
options$tChart <- TRUE
options$gChartProportionSource <- "historical"
options$gChartHistoricalProportion <- 0.5
options$tChartDistributionParameterSource <- "historical"
options$tChartHistoricalParametersScale <- 2
options$tChartHistoricalParametersWeibullShape <- 2
results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options)

test_that("12.1 Test of G chart with DM date format and historical parameters", {
  plotName <- results[["results"]][["gChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart12")
})

test_that("12.2 Test of T chart with DM date format and historical parameters", {
  plotName <- results[["results"]][["tChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart12")
})

# Distribution options (verified with Minitab) ####
options <- analysisOptions("rareEventCharts")
options$variable <- "DM"
options$dataType <- "dataTypeDates"
options$dataTypeDatesStructure <- "dateOnly"
options$dataTypeDatesFormatDate <- "dm"
options$gChart <- FALSE
options$tChart <- TRUE
options$tChartDistribution <- "exponential"
results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options)

test_that("13 Test of T chart with DM date format and exponential distribution", {
  plotName <- results[["results"]][["tChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart13")
})

# Report (verified with Minitab) ####
options <- analysisOptions("rareEventCharts")
options$variable <- "DM"
options$dataType <- "dataTypeDates"
options$dataTypeDatesStructure <- "dateOnly"
options$dataTypeDatesFormatDate <- "dm"
options$gChart <- TRUE
options$tChart <- TRUE
options$report <- TRUE
options$reportMeasurementName <- TRUE
options$reportMeasurementNameText <- "Test Name"
results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options)

test_that("14 Test of g and t chart report", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "rare-event-charts-report1")
})
