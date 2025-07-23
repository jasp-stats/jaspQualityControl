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
  plotName <- results[["results"]][["gChart"]][["collection"]][["gChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart1")
})

test_that("1.2 Test of G chart with MD date format table", {
  table <- results[["results"]][["gChart"]][["collection"]][["gChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 28", "Point 29", "Point 30", "Point 31"))
})


test_that("1.3 Test of T chart with MD date format", {
  plotName <- results[["results"]][["tChart"]][["collection"]][["tChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart1")
})

test_that("1.4 Test of T chart with MD date format table", {
  table <- results[["results"]][["tChart"]][["collection"]][["tChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
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
  plotName <- results[["results"]][["gChart"]][["collection"]][["gChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart2")
})

test_that("2.2 Test of G chart with MDHM date format table", {
  table <- results[["results"]][["gChart"]][["collection"]][["gChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("2.3 Test of T chart with MDHM date format", {
  plotName <- results[["results"]][["tChart"]][["collection"]][["tChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart2")
})

test_that("2.4 Test of T chart with MDHM date format table", {
  table <- results[["results"]][["tChart"]][["collection"]][["tChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
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
  plotName <- results[["results"]][["gChart"]][["collection"]][["gChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart3")
})

test_that("3.2 Test of G chart with DM date format table", {
  table <- results[["results"]][["gChart"]][["collection"]][["gChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("3.3 Test of T chart with DM date format", {
  plotName <- results[["results"]][["tChart"]][["collection"]][["tChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart3")
})

test_that("3.4 Test of T chart with DM date format table", {
  table <- results[["results"]][["tChart"]][["collection"]][["tChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
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
  plotName <- results[["results"]][["gChart"]][["collection"]][["gChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart4")
})

test_that("4.2 Test of G chart with DMY date format table", {
  table <- results[["results"]][["gChart"]][["collection"]][["gChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("4.3 Test of T chart with DMY date format", {
  plotName <- results[["results"]][["tChart"]][["collection"]][["tChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart4")
})

test_that("4.4 Test of T chart with DMY date format table", {
  table <- results[["results"]][["tChart"]][["collection"]][["tChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
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
  plotName <- results[["results"]][["gChart"]][["collection"]][["gChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart5")
})

test_that("5.2 Test of G chart with MDY date format table", {
  table <- results[["results"]][["gChart"]][["collection"]][["gChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("5.3 Test of T chart with MDY date format", {
  plotName <- results[["results"]][["tChart"]][["collection"]][["tChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart5")
})

test_that("5.4 Test of T chart with MDY date format table", {
  table <- results[["results"]][["tChart"]][["collection"]][["tChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
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
  plotName <- results[["results"]][["gChart"]][["collection"]][["gChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart6")
})

test_that("6.2 Test of G chart with YMD date format table", {
  table <- results[["results"]][["gChart"]][["collection"]][["gChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("6.3 Test of T chart with YMD date format", {
  plotName <- results[["results"]][["tChart"]][["collection"]][["tChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart6")
})

test_that("6.4 Test of T chart with YMD date format table", {
  table <- results[["results"]][["tChart"]][["collection"]][["tChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
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
  plotName <- results[["results"]][["gChart"]][["collection"]][["gChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart7")
})

test_that("7.2 Test of G chart with decimal hour format table", {
  table <- results[["results"]][["gChart"]][["collection"]][["gChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("7.3 Test of T chart with decimal hour format", {
  plotName <- results[["results"]][["tChart"]][["collection"]][["tChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart7")
})

test_that("7.4 Test of T chart with decimal hour format", {
  table <- results[["results"]][["tChart"]][["collection"]][["tChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
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
  plotName <- results[["results"]][["gChart"]][["collection"]][["gChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart8")
})

test_that("8.2 Test of G chart with decimal day format table", {
  table <- results[["results"]][["gChart"]][["collection"]][["gChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("8.3 Test of T chart with decimal day format", {
  plotName <- results[["results"]][["tChart"]][["collection"]][["tChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart8")
})

test_that("8.4 Test of T chart with decimal day format table", {
  table <- results[["results"]][["tChart"]][["collection"]][["tChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
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
  plotName <- results[["results"]][["gChart"]][["collection"]][["gChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart9")
})

test_that("9.2 Test of G chart with opportunities format table", {
  table <- results[["results"]][["gChart"]][["collection"]][["gChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("9.3 Test of T chart with opportunities format", {
  plotName <- results[["results"]][["tChart"]][["collection"]][["tChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart9")
})

test_that("9.4 Test of T chart with opportunities format table", {
  table <- results[["results"]][["tChart"]][["collection"]][["tChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
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
  plotName <- results[["results"]][["gChart"]][["collection"]][["gChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart10")
})

test_that("10.2 Test of G chart with DM format and stages table", {
  table <- results[["results"]][["gChart"]][["collection"]][["gChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("10.3 Test of T chart with DM format and stages", {
  plotName <- results[["results"]][["tChart"]][["collection"]][["tChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart10")
})

test_that("10.4 Test of T chart with DM format and stages table", {
  table <- results[["results"]][["tChart"]][["collection"]][["tChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
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
  plotName <- results[["results"]][["gChart"]][["collection"]][["gChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart11")
})

test_that("11.2 Test of G chart with interval format and stages", {
  table <- results[["results"]][["gChart"]][["collection"]][["gChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("11.3 Test of T chart with interval format and stages", {
  plotName <- results[["results"]][["tChart"]][["collection"]][["tChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  testthat::skip_on_os("mac")
  jaspTools::expect_equal_plots(testPlot, "t-chart11")
})

test_that("11.4 Test of T chart with interval format and stages table", {
  table <- results[["results"]][["tChart"]][["collection"]][["tChart_table"]][["data"]]
  testthat::skip_on_os("mac")
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
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
  plotName <- results[["results"]][["gChart"]][["collection"]][["gChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart12")
})

test_that("12.2 Test of G chart with DM date format and historical parameters table", {
  table <- results[["results"]][["gChart"]][["collection"]][["gChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 3", "Point 6", "Point 7", "Point 9", "Point 11", "Point 12",
                                      "Point 13", "Point 16", "Point 17", "Point 18", "Point 20",
                                      "Point 21", "Point 23", "Point 24", "Point 27", "Point 30",
                                      "Point 34", "Point 37"))
})

test_that("12.3 Test of T chart with DM date format and historical parameters", {
  plotName <- results[["results"]][["tChart"]][["collection"]][["tChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart12")
})

test_that("12.4 Test of T chart with DM date format and historical parameters", {
  table <- results[["results"]][["tChart"]][["collection"]][["tChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 2", "Point 3", "Point 5", "Point 6", "Point 7", "Point 9",
                                      "Point 11", "Point 12", "Point 13", "Point 15", "Point 16",
                                      "Point 17", "Point 18", "Point 20", "Point 21", "Point 23",
                                      "Point 24", "Point 26", "Point 27", "Point 28", "Point 30",
                                      "Point 34", "Point 37"))
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

test_that("13.1 Test of T chart with DM date format and exponential distribution", {
  plotName <- results[["results"]][["tChart"]][["collection"]][["tChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart13")
})

test_that("13.2 Test of T chart with DM date format and exponential distribution table", {
  table <- results[["results"]][["tChart"]][["collection"]][["tChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
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

# Out of control rules (verified with Minitab)
options <- analysisOptions("rareEventCharts")
options$variable <- "Opportunities"
options$dataType <- "dataTypeInterval"
options$dataTypeIntervalType <- "opportunities"
options$gChart <- TRUE
options$tChart <- FALSE
options$testSet <- "custom"
options$rule1 <- TRUE
options$rule2 <- TRUE
options$rule3 <- TRUE
options$rule8 <- TRUE
options$rule9 <- TRUE
results <- runAnalysis("rareEventCharts", "datasets/controlChartRules/violatingAllRareEventRules.csv", options)

test_that("15.1 Test of all rules for G chart", {
  plotName <- results[["results"]][["gChart"]][["collection"]][["gChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart15")
})

test_that("15.2 Test of all rules for G chart table", {
  table <- results[["results"]][["gChart"]][["collection"]][["gChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 2", "Point 9", "Point 11", "Point 26", "Point 4", "Point 3",
                                      "Point 18", "Point 12", "Point 27", "Point 5", "Point 4", "Point 19",
                                      "Point 13", "Point 28", "", "Point 5", "Point 20", "", "Point 29",
                                      "", "Point 32", "Point 21", "", "Point 30", "", "", "Point 22",
                                      "", "Point 31", "", "", "Point 23", "", "", "", "", "Point 24",
                                      "", "", "", "", "Point 25", "", "", "", "", "Point 26", "",
                                      "", "", "", "Point 27", "", "", "", "", "Point 28", "", "",
                                      "", "", "Point 29", "", "", "", "", "Point 30", "", "", "",
                                      "", "Point 31", "", "", "", "", "Point 32", "", "", ""))
})
