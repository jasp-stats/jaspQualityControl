context("[Quality Control] Variables Charts for Subgroups")
.numDecimals <- 2
set.seed(1)

# Long / Column format ####

## Basic tests ####

### x-bar & r chart with manual subgroup size (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF1. Basic test to create X-bar & R control chart with manual subgroups", {
 plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart1")
})

### x-bar & s chart with manual subgroup size (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF2. Basic test to create X-bar & s control chart with manual subgroups", {
plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart1")
})

### x-bar & r chart with manual subgroup size and stages (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$stagesLongFormat <- "Stage"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)


test_that("LF3. Basic test to create X-bar & R control chart with manual subgroups and stages", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart2")
})

### x-bar & s chart with manual subgroup size and stages (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndS"
options$measurementLongFormat <- "Diameter"
options$stagesLongFormat <- "Stage"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF4. Basic test to create X-bar & s control chart with manual subgroups and stages", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart2")
})

### x-bar & r chart with subgroup variable (verfied with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$subgroup <- "Time"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF5. Basic test to create X-bar & R control chart with subgroup variable", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart3")
})

### x-bar & s chart with subgroup variable (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndS"
options$measurementLongFormat <- "Diameter"
options$subgroup <- "Time"
options$subgroupSizeType <- "groupingVariable"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF6. Basic test to create X-bar & s control chart with subgroup variable", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart3")
})

### x-bar & r chart with subgroup variable and stages (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$stagesLongFormat <- "Stage"
options$subgroup <- "Time"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF7. Basic test to create X-bar & R control chart with subgroup variable and stages", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart4")
})

### x-bar & s chart with subgroup variable and stages (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndS"
options$measurementLongFormat <- "Diameter"
options$stagesLongFormat <- "Stage"
options$subgroup <- "Time"
options$subgroupSizeType <- "groupingVariable"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF8. Basic test to create X-bar & R control chart with subgroup variable and stages", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart4")
})

### x-bar & r chart with warning limits (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndR"
options$measurementLongFormat <- "Diameter"
options$warningLimits <- TRUE
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF9. Basic test of adding warning limits to X-bar & R control chart", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart5")
})

### x-bar & s chart with warning limits (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndS"
options$measurementLongFormat <- "Diameter"
options$warningLimits <- TRUE
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF10. Basic test of adding warning limits to X-bar & s control chart", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart5")
})

### x-bar & r chart with known parameters (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndR"
options$measurementLongFormat <- "Diameter"
options$knownParameters <- TRUE
options$knownParametersMean <- 0
options$knownParametersSd <- 3
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF11.1 Basic test of adding known parameters to X-bar & r control chart - plot", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart6")
})

test_that("LF11.2 Basic test of adding known parameters to X-bar & r control chart - x-bar table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_xBarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 1", "Point 7", "Point 2", "Point 8", "Point 3", "Point 9",
                                      "Point 4", "Point 10", "Point 5", "Point 11", "Point 6", "Point 12",
                                      "Point 7", "Point 13", "Point 8", "Point 14", "Point 9", "Point 15",
                                      "Point 10", "Point 16", "Point 11", "Point 17", "Point 12",
                                      "Point 18", "Point 13", "Point 19", "Point 14", "Point 20",
                                      "Point 15", "", "Point 16", "", "Point 17", "", "Point 18",
                                      "", "Point 19", "", "Point 20", ""))
})

test_that("LF11.3 Basic test of adding known parameters to X-bar & r control chart - R table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_secondTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 9", "Point 10", "Point 11", "Point 12", "Point 13", "Point 14",
                                      "Point 15", "Point 16", "Point 17", "Point 18", "Point 19",
                                      "Point 20"))
})


### x-bar & s chart with known parameters (verified with Minitab) ####
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

test_that("LF12.1 Basic test of adding known parameters to X-bar & s control chart - plot", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart6")
})

test_that("LF12.2 Basic test of adding known parameters to X-bar & s control chart - x-bar table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_xBarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 1", "Point 7", "Point 2", "Point 8", "Point 3", "Point 9",
                                      "Point 4", "Point 10", "Point 5", "Point 11", "Point 6", "Point 12",
                                      "Point 7", "Point 13", "Point 8", "Point 14", "Point 9", "Point 15",
                                      "Point 10", "Point 16", "Point 11", "Point 17", "Point 12",
                                      "Point 18", "Point 13", "Point 19", "Point 14", "Point 20",
                                      "Point 15", "", "Point 16", "", "Point 17", "", "Point 18",
                                      "", "Point 19", "", "Point 20", ""))
})


test_that("LF12.3 Basic test of adding known parameters to X-bar & s control chart - s table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_secondTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 7", "Point 8", "Point 9", "Point 10", "Point 11", "Point 12",
                                      "Point 13", "Point 14", "Point 15", "Point 16", "Point 17",
                                      "Point 18", "Point 19", "Point 20"))
})

### x-bar & r chart with changed manual subgroup size value (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndR"
options$measurementLongFormat <- "Diameter"
options$manualSubgroupSizeValue <- 10
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF13. Basic test of changing manual subgroup size with X-bar & R control chart", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart7")
})

### x-bar & s chart with changed manual subgroup size value (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndS"
options$measurementLongFormat <- "Diameter"
options$manualSubgroupSizeValue <- 10
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF14. Basic test of changing manual subgroup size with X-bar & s control chart", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart7")
})

### Report function with x-bar & r chart (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndR"
options$measurementLongFormat <- "Diameter"
options$report <- TRUE
options$reportMetaData <- TRUE
options$reportTitleText <- "newTitle"
options$reportChartNameText <- "Chart name test"
options$reportSubtitleText <- "Sub title test"
options$reportMeasurementNameText <- "Measurement name test"
options$reportFootnoteText <- "Footnote test"
options$reportLocationText <- "Place test"
options$reportDateText <- "01.01.2000"
options$reportSubtitleText <- "Your report sub-title"
options$reportPerformedByText <- "Operator name"
options$reportChartNameText <- "Name of chart"
options$reportPrintDateText <- "02.02.2002"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF15. Basic test to create report of X-bar & R control chart", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-report1")
})

### Report function with x-bar & s chart (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndS"
options$measurementLongFormat <- "Diameter"
options$report <- TRUE
options$reportMetaData <- TRUE
options$reportTitleText <- "newTitle"
options$reportChartNameText <- "Chart name test"
options$reportSubtitleText <- "Sub title test"
options$reportMeasurementNameText <- "Measurement name test"
options$reportFootnoteText <- "Footnote test"
options$reportLocationText <- "Place test"
options$reportDateText <- "01.01.2000"
options$reportSubtitleText <- "Your report sub-title"
options$reportPerformedByText <- "Operator name"
options$reportChartNameText <- "Name of chart"
options$reportPrintDateText <- "02.02.2002"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF16. Basic test to create report of X-bar & s control chart", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-report1")
})


## Missing values handling ####

### Missing values in measurements variable ####

#### Single missing value ####

##### x-bar & r chart (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "DiameterMissing1"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF17. X-bar & R control chart with single missing value in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart8")
})

##### x-bar & s chart (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "DiameterMissing1"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF18. X-bar & s control chart with single missing value in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart8")
})

#### All but one value missing ####

##### x-bar & r chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "DiameterMissing99"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF19. X-bar & R control chart with all but one missing value in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart9")
})

##### x-bar & s chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "DiameterMissing99"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF20. X-bar & s control chart with all but one missing value in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart9")
})

#### All values missing ####

##### x-bar & r chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "DiameterMissingAll"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF21. X-bar & R control chart with all missing value in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart10")
})

##### x-bar & s chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "DiameterMissingAll"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF22. X-bar & s control chart with all missing value in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart10")
})

### Missing values in subgroup variable ####

#### x-bar & r chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$subgroup <- "TimeMissing15"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF23. X-bar & R control with missing values in subgroup variable", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart11")
})

#### x-bar & s chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$subgroup <- "TimeMissing15"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF24. X-bar & s control chart  with missing values in subgroup variable", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart11")
})

### Missing values in stages variable ####

#### x-bar & r chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$stagesLongFormat <- "StageMissing15"
options$subgroup <- "Time"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF25. X-bar & R control with missing values in stages variable", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart12")
})

#### x-bar & s chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$stagesLongFormat <- "StageMissing15"
options$subgroup <- "Time"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF26. X-bar & s control with missing values in stages variable", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart12")
})

## Unequal subgroup sizes ####

### x-bar & r chart with actual sizes (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$subgroup <- "TimeUnequalGroups"
options$subgroupSizeType <- "groupingVariable"
options$chartType <- "xBarAndR"
options$subgroupSizeUnequal <- "actualSizes"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF27. X-bar & R control chart with unequal subgroups and actual size calculation", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart13")
})

### x-bar & s chart with actual sizes (verified with Minitab) ####
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

test_that("LF28. X-bar & s control chart with unequal subgroups and actual size calculation", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart13")
})

### x-bar & r chart with fixed group calculation (verified with Minitab) ####
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

test_that("LF29. X-bar & R control chart with unequal subgroups and fixed size calculation", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart14")
})

### x-bar & s chart with fixed group calculation (verified with Minitab) ####
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

test_that("LF30. X-bar & s control chart with unequal subgroups and fixed size calculation", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart14")
})

## Edge cases ####

### Subgroup size larger than stage ####

#### x-bar & r chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$stagesLongFormat <- "Stage"
options$chartType <- "xBarAndR"
options$manualSubgroupSizeValue <- 44
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF31. Edge case of X-bar & R control chart with very large subgroup size", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart15")
})

#### x-bar & s chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$stagesLongFormat <- "Stage"
options$chartType <- "xBarAndS"
options$manualSubgroupSizeValue <- 44
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF32. Edge case of X-bar & s control chart with very large subgroup size", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart15")
})

### Multiple stages assigned within subgroup ####

#### x-bar & r chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$stagesLongFormat <- "StageMultiAssigned"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF33. Edge case of X-bar & R control chart with multiple assigned stages per subgroups", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart16")
})

#### x-bar & s chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$measurementLongFormat <- "Diameter"
options$stagesLongFormat <- "StageMultiAssigned"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF34. Edge case of X-bar & s control chart with multiple assigned stages per subgroups", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart16")
})

## Options tests ####

### Report of x-bar & r chart with stages and some deselected meta data ####
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndR"
options$measurementLongFormat <- "Diameter"
options$stagesLongFormat <- "Stage"
options$report <- TRUE
options$reportMetaData <- TRUE
options$reportTitle <- FALSE
options$reportChartNameText <- "Chart name test"
options$reportSubtitleText <- "Sub title test"
options$reportMeasurementNameText <- "Measurement name test"
options$reportFootnote <- FALSE
options$reportLocationText <- "Place test"
options$reportDateText <- "01.01.2000"
options$reportSubtitle <- FALSE
options$reportPerformedByText <- "Operator name"
options$reportChartNameText <- "Name of chart"
options$reportPrintDateText <- "02.02.2002"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF35. Option test of creating a report with stages and less meta data", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-report2")
})

### Report of x-bar & r chart with no meta data ####
options <- analysisOptions("variablesChartsSubgroups")
options$chartType <- "xBarAndR"
options$measurementLongFormat <- "Diameter"
options$stagesLongFormat <- "Stage"
options$report <- TRUE
options$reportMetaData <- FALSE
options$reportTitle <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv",
                       options)

test_that("LF36. Option test of creating a report with no meta data", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-report3")
})

# Wide / Row format ####

## Basic tests ####

### x-bar & r chart (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF1. Basic test to create X-bar & R control chart", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart17")
})

### x-bar & s chart (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF2. Basic test to create X-bar & s control chart", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart17")
})

### x-bar & r chart with stages (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$stagesWideFormat <- "Stage"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF3. Basic test to create X-bar & R control chart with stages", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart18")
})

### x-bar & s chart with stages (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$stagesWideFormat <- "Stage"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF4. Basic test to create X-bar & s control chart with stages", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart18")
})

### x-bar & r chart with axis labels (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$axisLabels <- "Time"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF5. Basic test to create X-bar & R control chart with axis labels", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart19")
})

### x-bar & s chart with axis labels (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$axisLabels <- "Time"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF6. Basic test to create X-bar & s control chart with axis labels", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart19")
})

### x-bar & r chart with axis labels and stages (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
options$stagesWideFormat <- "Stage"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF7. Basic test to create X-bar & R control chart with stages and axis labels", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart20")
})

### x-bar & s chart with axis labels and stages (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
options$stagesWideFormat <- "Stage"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF8. Basic test to create X-bar & s control chart with stages and axis labels", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart20")
})

### x-bar & r chart with warning limits (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$chartType <- "xBarAndR"
options$warningLimits <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF9. Basic test to create X-bar & R control chart with warning limits", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart21")
})

### x-bar & s chart with warning limits (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$chartType <- "xBarAndS"
options$warningLimits <- TRUE
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF10. Basic test to create X-bar & s control chart with warning limits", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart21")
})

### x-bar & r chart with known parameters (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$chartType <- "xBarAndR"
options$knownParameters <- TRUE
options$knownParametersMean <- 0
options$knownParametersSd <- 3
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF11.1 Basic test of adding known parameters to X-bar & R control chart - plot", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart22")
})

test_that("WF11.2 Basic test of adding known parameters to X-bar & R control chart - R table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_secondTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 9", "Point 10", "Point 11", "Point 12", "Point 13", "Point 14",
                                      "Point 15", "Point 16", "Point 17", "Point 18", "Point 19",
                                      "Point 20"))
})

test_that("WF11.3 Basic test of adding known parameters to X-bar & R control chart - X-bar table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_xBarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 1", "Point 7", "Point 2", "Point 8", "Point 3", "Point 9",
                                      "Point 4", "Point 10", "Point 5", "Point 11", "Point 6", "Point 12",
                                      "Point 7", "Point 13", "Point 8", "Point 14", "Point 9", "Point 15",
                                      "Point 10", "Point 16", "Point 11", "Point 17", "Point 12",
                                      "Point 18", "Point 13", "Point 19", "Point 14", "Point 20",
                                      "Point 15", "", "Point 16", "", "Point 17", "", "Point 18",
                                      "", "Point 19", "", "Point 20", ""))
})

### x-bar & s chart with known parameters (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$knownParameters <- TRUE
options$knownParametersMean <- 0
options$knownParametersSd <- 3
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF12.1 Basic test of adding known parameters to X-bar & s control chart - plot", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart22")
})

test_that("WF12.2 Basic test of adding known parameters to X-bar & s control chart - s table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_secondTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 7", "Point 8", "Point 9", "Point 10", "Point 11", "Point 12",
                                      "Point 13", "Point 14", "Point 15", "Point 16", "Point 17",
                                      "Point 18", "Point 19", "Point 20"))
})

test_that("WF12.3 Basic test of adding known parameters to X-bar & s control chart - X-bar table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_xBarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 1", "Point 7", "Point 2", "Point 8", "Point 3", "Point 9",
                                      "Point 4", "Point 10", "Point 5", "Point 11", "Point 6", "Point 12",
                                      "Point 7", "Point 13", "Point 8", "Point 14", "Point 9", "Point 15",
                                      "Point 10", "Point 16", "Point 11", "Point 17", "Point 12",
                                      "Point 18", "Point 13", "Point 19", "Point 14", "Point 20",
                                      "Point 15", "", "Point 16", "", "Point 17", "", "Point 18",
                                      "", "Point 19", "", "Point 20", ""))
})

### Report function with x-bar & r chart (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$chartType <- "xBarAndR"
options$report <- TRUE
options$reportMetaData <- TRUE
options$reportTitleText <- "newTitle"
options$reportChartNameText <- "Chart name test"
options$reportSubtitleText <- "Sub title test"
options$reportMeasurementNameText <- "Measurement name test"
options$reportFootnoteText <- "Footnote test"
options$reportLocationText <- "Place test"
options$reportDateText <- "01.01.2000"
options$reportSubtitleText <- "Your report sub-title"
options$reportPerformedByText <- "Operator name"
options$reportChartNameText <- "Name of chart"
options$reportPrintDateText <- "02.02.2002"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF13. Basic test to create report of X-bar & R control chart", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-reportW1")
})

### Report function with x-bar & s chart (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
options$report <- TRUE
options$reportMetaData <- TRUE
options$reportTitleText <- "newTitle"
options$reportChartNameText <- "Chart name test"
options$reportSubtitleText <- "Sub title test"
options$reportMeasurementNameText <- "Measurement name test"
options$reportFootnoteText <- "Footnote test"
options$reportLocationText <- "Place test"
options$reportDateText <- "01.01.2000"
options$reportSubtitleText <- "Your report sub-title"
options$reportPerformedByText <- "Operator name"
options$reportChartNameText <- "Name of chart"
options$reportPrintDateText <- "02.02.2002"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF14. Basic test to create report of X-bar & s control chart", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-reportW1")
})


## Missing values handling ####

### Missing values in measurements variable ####

#### Single missing value ####

##### x-bar & r chart (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1Missing1", "dm2", "dm3", "dm4", "dm5")
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF15. X-bar & R control chart with single missing value in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart23")
})

##### x-bar & s chart (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1Missing1", "dm2", "dm3", "dm4", "dm5")
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF16. X-bar & s control chart with single missing value in measurement", {
  options <- analysisOptions("variablesChartsSubgroups")
  options$dataFormat <- "wideFormat"
  options$measurementsWideFormat <- list("dm1Missing1", "dm2", "dm3", "dm4", "dm5")
  options$chartType <- "xBarAndS"
  set.seed(1)
  results <- runAnalysis("variablesChartsSubgroups", "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv", options)
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart23")
})

#### All but one value missing ####

##### x-bar & r chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1Missing19", "dm2MissingAll", "dm3MissingAll",
                                       "dm4MissingAll", "dm5MissingAll")
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF17. X-bar & R control chart with all but one missing value in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart24")
})

##### x-bar & s chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1Missing19", "dm2MissingAll", "dm3MissingAll",
                                       "dm4MissingAll", "dm5MissingAll")
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF18. X-bar & s control chart with all but one missing value in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart24")
})

#### All values missing ####

##### x-bar & r chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1MissingAll", "dm2MissingAll", "dm3MissingAll",
                                       "dm4MissingAll", "dm5MissingAll")
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF19. X-bar & R control chart with all values missing in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart25")
})

##### x-bar & s chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1MissingAll", "dm2MissingAll", "dm3MissingAll",
                                       "dm4MissingAll", "dm5MissingAll")
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF20. X-bar & s control chart with all values missing in measurement", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart25")
})

### Missing values in stages variable ####

#### x-bar & r chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$stagesWideFormat <- "StageMissing7"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF21. X-bar & R control chart with missing values in stages variable", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart26")
})

#### x-bar & s chart ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$stagesWideFormat <- "StageMissing7"
options$chartType <- "xBarAndS"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF22. X-bar & s control chart with missing values in stages variable", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart26")
})

## Unequal subgroup sizes ####

### x-bar & r chart with actual sizes (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1Missing7", "dm2", "dm3", "dm4", "dm5")
options$subgroupSizeUnequal <- "actualSizes"
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF23. X-bar & R control chart with unequal subgroups and actual sizes", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart27")
})

### x-bar & s chart with actual sizes (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1Missing7", "dm2", "dm3", "dm4", "dm5")
options$chartType <- "xBarAndS"
options$subgroupSizeUnequal <- "actualSizes"
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF24. X-bar & s control chart with unequal subgroups and actual sizes", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart27")
})

### x-bar & r chart with fixed group calculation (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1Missing7", "dm2", "dm3", "dm4", "dm5")
options$subgroupSizeUnequal <- "fixedSubgroupSize"
options$fixedSubgroupSizeValue <- 7
options$chartType <- "xBarAndR"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF25. X-bar & R control chart with unequal subgroups and fixed sizes", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart28")
})

### x-bar & s chart with fixed group calculation (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1Missing7", "dm2", "dm3", "dm4", "dm5")
options$chartType <- "xBarAndS"
options$subgroupSizeUnequal <- "fixedSubgroupSize"
options$fixedSubgroupSizeValue <- 7
options$xBarAndSUnbiasingConstant <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF26. X-bar & s control chart with unequal subgroups and fixed sizes", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart28")
})

## Options tests ####

### Report of x-bar & r chart with stages and some deselected meta data ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$stagesWideFormat <- "Stage"
options$chartType <- "xBarAndR"
options$report <- TRUE
options$reportMetaData <- TRUE
options$reportTitle <- FALSE
options$reportChartNameText <- "Chart name test"
options$reportSubtitleText <- "Sub title test"
options$reportMeasurementNameText <- "Measurement name test"
options$reportFootnote <- FALSE
options$reportLocationText <- "Place test"
options$reportDate <- FALSE
options$reportSubtitleText <- "Your report sub-title"
options$reportPerformedByText <- "Operator name"
options$reportChartNameText <- "Name of chart"
options$reportPrintDateText <- "02.02.2002"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF27. Options test of creating a report with stages and less meta data", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-reportW2")
})

### Report of x-bar & r chart with no meta data ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("dm1", "dm2", "dm3", "dm4", "dm5")
options$chartType <- "xBarAndR"
options$report <- TRUE
options$reportMetaData <- FALSE
options$reportTitle <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsWideFormatDebug.csv",
                       options)

test_that("WF27. Options test of creating a report with stages and less meta data", {
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-reportW3")
})


# Out of control rules (verified with Minitab) ####
options <- analysisOptions("variablesChartsSubgroups")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("V1", "V2", "V3")
options$chartType <- "xBarAndR"
options$testSet <- "custom"
options$rule1 <- TRUE
options$rule2 <- TRUE
options$rule3 <- TRUE
options$rule4 <- TRUE
options$rule5 <- TRUE
options$rule6 <- TRUE
options$rule7 <- TRUE
options$rule8 <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/controlChartRules/violatingAllSubgroupRules.csv",
                       options)

test_that("WF28.1 Test of all rules for xbar & r chart", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chartW28")
})

test_that("WF28.2 Test of all rules for xbar & r chart - r table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_secondTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 7", "Point 8", "Point 9", "Point 10", "Point 11", "Point 12",
                                      "Point 13", "Point 14", "Point 15", "Point 16", "Point 17",
                                      "Point 18", "Point 19", "Point 20", "Point 21", "Point 22",
                                      "Point 23", "Point 24", "Point 25", "Point 26", "Point 27",
                                      "Point 28", "Point 29", "Point 30", "Point 31", "Point 32",
                                      "Point 33", "Point 34", "Point 35", "Point 36", "Point 37",
                                      "Point 38", "Point 39", "Point 40", "Point 47", "Point 48",
                                      "Point 49", "Point 50", "Point 51", "Point 52", "Point 53",
                                      "Point 54", "Point 55"))
})

test_that("WF28.3 Test of all rules for xbar & r chart - xbar table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_xBarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 1", "Point 7", "Point 24", "Point 2", "Point 55", "Point 8",
                                      "Point 4", "Point 37", "Point 2", "Point 8", "", "Point 3",
                                      "", "Point 9", "Point 5", "Point 38", "Point 3", "Point 9",
                                      "", "Point 4", "", "Point 10", "Point 6", "Point 39", "Point 4",
                                      "Point 10", "", "Point 5", "", "Point 11", "Point 7", "Point 40",
                                      "Point 5", "Point 17", "", "Point 6", "", "Point 12", "Point 8",
                                      "", "Point 6", "Point 18", "", "Point 7", "", "Point 13", "Point 9",
                                      "", "Point 7", "Point 19", "", "Point 8", "", "Point 14", "Point 10",
                                      "", "Point 8", "Point 20", "", "Point 9", "", "Point 15", "Point 14",
                                      "", "Point 9", "Point 21", "", "Point 10", "", "Point 16", "Point 15",
                                      "", "Point 10", "Point 22", "", "Point 12", "", "Point 17",
                                      "Point 16", "", "Point 11", "Point 23", "", "Point 13", "",
                                      "Point 18", "Point 17", "", "Point 12", "Point 24", "", "Point 14",
                                      "", "Point 19", "Point 18", "", "Point 13", "Point 25", "",
                                      "Point 15", "", "Point 20", "Point 19", "", "Point 14", "Point 26",
                                      "", "Point 16", "", "Point 21", "Point 20", "", "Point 15",
                                      "Point 27", "", "Point 17", "", "Point 22", "Point 21", "",
                                      "Point 16", "Point 28", "", "Point 18", "", "Point 23", "Point 22",
                                      "", "Point 17", "Point 29", "", "Point 19", "", "Point 24",
                                      "Point 23", "", "Point 18", "Point 30", "", "Point 20", "",
                                      "Point 25", "Point 24", "", "Point 19", "Point 31", "", "Point 26",
                                      "", "Point 26", "Point 25", "", "Point 20", "Point 32", "",
                                      "Point 27", "", "Point 27", "Point 26", "", "Point 21", "Point 33",
                                      "", "Point 28", "", "Point 28", "Point 27", "", "Point 22",
                                      "Point 34", "", "Point 29", "", "Point 29", "Point 28", "",
                                      "Point 23", "Point 35", "", "Point 30", "", "Point 30", "Point 29",
                                      "", "Point 24", "Point 36", "", "Point 31", "", "Point 31",
                                      "Point 30", "", "Point 25", "Point 37", "", "Point 32", "",
                                      "Point 32", "Point 31", "", "Point 26", "Point 38", "", "Point 33",
                                      "", "Point 33", "Point 32", "", "Point 27", "Point 39", "",
                                      "Point 34", "", "Point 34", "Point 33", "", "Point 28", "Point 40",
                                      "", "Point 35", "", "Point 35", "Point 34", "", "Point 29",
                                      "Point 47", "", "Point 36", "", "Point 36", "Point 35", "",
                                      "Point 30", "Point 48", "", "Point 37", "", "Point 37", "Point 36",
                                      "", "Point 31", "Point 49", "", "Point 38", "", "Point 38",
                                      "Point 37", "", "Point 32", "Point 50", "", "Point 39", "",
                                      "Point 39", "Point 38", "", "Point 33", "Point 51", "", "Point 40",
                                      "", "Point 40", "Point 39", "", "Point 34", "Point 52", "",
                                      "", "", "", "Point 40", "", "Point 35", "Point 53", "", "",
                                      "", "", "", "", "Point 36", "Point 54", "", "", "", "", "",
                                      "", "Point 37", "Point 55", "", "", "", "", "", "", "Point 38",
                                      "", "", "", "", "", "", "", "Point 39", "", "", "", "", "",
                                      "", "", "Point 40", "", "", "", "", "", "", ""))
})
