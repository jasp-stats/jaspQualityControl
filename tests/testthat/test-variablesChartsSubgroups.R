context("[Quality Control] Variables Charts for Subgroups")
.numDecimals <- 2
set.seed(1)

# Long / Column format

## Basic tests

### x-bar & r chart with manual subgroup size (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("1. Basic test to create X-bar & R control chart with manual subgroups", {
 plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart1")
})

### x-bar & s chart with manual subgroup size (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("2. Basic test to create X-bar & s control chart with manual subgroups", {
plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart1")
})

### x-bar & r chart with manual subgroup size and stages (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$stages <- "Stage"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)


test_that("3. Basic test to create X-bar & R control chart with manual subgroups and stages", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart2")
})

### x-bar & s chart with manual subgroup size and stages (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndS"
options$measurementLongFormat <- "Diameter"
options$stages <- "Stage"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("4. Basic test to create X-bar & s control chart with manual subgroups and stages", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart2")
})

### x-bar & r chart with subgroup variable (verfied with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$subgroup <- "Time"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("5. Basic test to create X-bar & R control chart with subgroup variable", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart3")
})

### x-bar & s chart with subgroup variable (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndS"
options$measurementLongFormat <- "Diameter"
options$subgroup <- "Time"
options$subgroupSizeType <- "groupingVariable"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("6. Basic test to create X-bar & s control chart with subgroup variable", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart3")
})

### x-bar & r chart with subgroup variable and stages (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$stages <- "Stage"
options$subgroup <- "Time"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("7. Basic test to create X-bar & R control chart with subgroup variable and stages", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart4")
})

### x-bar & s chart with subgroup variable and stages (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndS"
options$measurementLongFormat <- "Diameter"
options$stages <- "Stage"
options$subgroup <- "Time"
options$subgroupSizeType <- "groupingVariable"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("8. Basic test to create X-bar & R control chart with subgroup variable and stages", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart4")
})

### x-bar & r chart with warning limits (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndR"
options$measurementLongFormat <- "Diameter"
options$warningLimits <- TRUE
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("9. Basic test of adding warning limits to X-bar & R control chart", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart5")
})

### x-bar & s chart with warning limits (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndS"
options$measurementLongFormat <- "Diameter"
options$warningLimits <- TRUE
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("10. Basic test of adding warning limits to X-bar & s control chart", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart5")
})

### x-bar & r chart with known parameters (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndR"
options$measurementLongFormat <- "Diameter"
options$knownParameters <- TRUE
options$knownParametersMean <- 0
options$knownParametersSd <- 3
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("11.1 Basic test of adding known parameters to X-bar & r control chart - plot", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart6")
})

test_that("11.2 Basic test of adding known parameters to X-bar & r control chart - x-bar table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_secondTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
})

test_that("11.3 Basic test of adding known parameters to X-bar & r control chart - R table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_xBarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 7, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12, 7, 13, 8, 14, 9, 15, 10,
                                      16, 11, 17, 12, 18, 13, 19, 14, 20, 15, "", 16, "", 17, "",
                                      18, "", 19, "", 20, ""))
})

### x-bar & s chart with known parameters (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndS"
options$measurementLongFormat <- "Diameter"
options$knownParameters <- TRUE
options$knownParametersMean <- 0
options$knownParametersSd <- 3
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("12.1 Basic test of adding known parameters to X-bar & s control chart - plot", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart6")
})

test_that("12.2 Basic test of adding known parameters to X-bar & s control chart - x-bar table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_secondTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(6, 7, 8, 8, 10, 9, 11, 10, 15, 11, 16, 12, "", 13, "", 14, "",
                                      15, "", 16, "", 17, "", 18, "", 19, "", 20))
})

test_that("12.3 Basic test of adding known parameters to X-bar & s control chart - s table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_xBarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 7, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12, 7, 13, 8, 14, 9, 15, 10,
                                      16, 11, 17, 12, 18, 13, 19, 14, 20, 15, "", 16, "", 17, "",
                                      18, "", 19, "", 20, ""))
})

### x-bar & r chart with changed manual subgroup size value (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndR"
options$measurementLongFormat <- "Diameter"
options$manualSubgroupSizeValue <- 10
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("13. Basic test of changing manual subgroup size with X-bar & R control chart", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart7")
})

### x-bar & s chart with changed manual subgroup size value (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndS"
options$measurementLongFormat <- "Diameter"
options$manualSubgroupSizeValue <- 10
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("14. Basic test of changing manual subgroup size with X-bar & s control chart", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart7")
})

### Report function with x-bar & r chart (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndR"
options$measurementLongFormat <- "Diameter"
options$reportMeasurementName <- "Report name"
options$reportDate <- "01.01.2000"
options$reportSubtitle <- "Your report sub-title"
options$reportReportedBy <- "Operator name"
options$reportMiscellaneous <- "Various comments"
options$reportChartName <- "Name of chart"
options$report <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("15. Basic test to create report of X-bar & R control chart", {
  plotName <- results[["results"]][["report"]][["collection"]][["report_report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-report1")
})

### Report function with x-bar & s chart (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndS"
options$measurementLongFormat <- "Diameter"
options$reportMeasurementName <- "Report name"
options$reportDate <- "01.01.2000"
options$reportSubtitle <- "Your report sub-title"
options$reportReportedBy <- "Operator name"
options$reportMiscellaneous <- "Various comments"
options$reportChartName <- "Name of chart"
options$report <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("16. Basic test to create report of X-bar & s control chart", {
  plotName <- results[["results"]][["report"]][["collection"]][["report_report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-report1")
})


## Missing values handling

### Missing values in measurements variable

#### Single missing value

##### x-bar & r chart (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "DiameterMissing1"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("17. X-bar & R control chart with single missing value in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart8")
})

##### x-bar & s chart (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "DiameterMissing1"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("18. X-bar & s control chart with single missing value in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart8")
})

#### All but one value missing

##### x-bar & r chart

options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "DiameterMissing99"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("19. X-bar & R control chart with all but one missing value in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart9")
})

##### x-bar & s chart
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "DiameterMissing99"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("20. X-bar & s control chart with all but one missing value in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart9")
})

#### All values missing

##### x-bar & r chart
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "DiameterMissingAll"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("21. X-bar & R control chart with all missing value in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart10")
})

##### x-bar & s chart
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "DiameterMissingAll"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("22. X-bar & s control chart with all missing value in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart10")
})

### Missing values in subgroup variable

#### x-bar & r chart
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$subgroup <- "TimeMissing15"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("23. X-bar & R control with missing values in subgroup variable", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart11")
})

#### x-bar & s chart
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$subgroup <- "TimeMissing15"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("24. X-bar & s control chart  with missing values in subgroup variable", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart11")
})

### Missing values in stages variable

#### x-bar & r chart
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$stages <- "StageMissing15"
options$subgroup <- "Time"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("25. X-bar & R control with missing values in stages variable", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart12")
})

#### x-bar & s chart
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$stages <- "StageMissing15"
options$subgroup <- "Time"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("26. X-bar & s control with missing values in stages variable", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart12")
})

## Unequal subgroup sizes

### x-bar & r chart with actual sizes (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$subgroup <- "TimeUnequalGroups"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndR"
options$subgroupSizeUnequal <- "actualSizes"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("27. X-bar & R control chart with unequal subgroups and actual size calculation", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart13")
})

### x-bar & s chart with actual sizes (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$subgroup <- "TimeUnequalGroups"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndS"
options$subgroupSizeUnequal <- "actualSizes"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("28. X-bar & s control chart with unequal subgroups and actual size calculation", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart13")
})

### x-bar & r chart with fixed group calculation (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$subgroup <- "TimeUnequalGroups"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndR"
options$subgroupSizeUnequal <- "fixedSubgroupSize"
options$fixedSubgroupSizeValue <- 7
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("29. X-bar & R control chart with unequal subgroups and fixed size calculation", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart14")
})

### x-bar & s chart with fixed group calculation (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$subgroup <- "TimeUnequalGroups"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndS"
options$subgroupSizeUnequal <- "fixedSubgroupSize"
options$fixedSubgroupSizeValue <- 7
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("30. X-bar & s control chart with unequal subgroups and fixed size calculation", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart14")
})

## Edge cases

### Subgroup size larger than stage

#### x-bar & r chart
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$stages <- "Stage"
options$chartType <- "xBarAndR"
options$manualSubgroupSizeValue <- 44
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("31. Edge case of X-bar & R control chart with very large subgroup size", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart15")
})

#### x-bar & s chart
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$stages <- "Stage"
options$chartType <- "xBarAndS"
options$manualSubgroupSizeValue <- 44
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("32. Edge case of X-bar & R control chart with very large subgroup size", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart15")
})

### Multiple stages assigned within subgroup

#### x-bar & r chart
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$stages <- "StageMultiAssigned"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("33. Edge case of X-bar & R control chart with multiple assigned stages per subgroups", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart16")
})

#### x-bar & s chart
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$stages <- "StageMultiAssigned"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("34. Edge case of X-bar & s control chart with multiple assigned stages per subgroups", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart16")
})

# Wide / Row format

## Basic tests

### x-bar & r chart

### x-bar & s chart

### x-bar & r chart with stages

### x-bar & s chart with stages

### x-bar & r chart with warning limits

### x-bar & s chart with warning limits

### x-bar & r chart with known parameters

### x-bar & s chart with known parameters

### Report function with x-bar & r chart

### Report function with x-bar & s chart


## Missing values handling

### Missing values in measurements variable

#### Single missing value

##### x-bar & r chart

##### x-bar & s chart

#### All but one value missing

##### x-bar & r chart

##### x-bar & s chart

#### All values missing

##### x-bar & r chart

##### x-bar & s chart

### Missing values in stages variable

#### x-bar & r chart

#### x-bar & s chart


## Unequal subgroup sizes

### x-bar & r chart with actual sizes

### x-bar & s chart with actual sizes

### x-bar & r chart with fixed group calculation

### x-bar & s chart with fixed group calculation
