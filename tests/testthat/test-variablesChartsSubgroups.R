context("[Quality Control] Variables Charts for Subgroups")
.numDecimals <- 2
set.seed(1)

# Long / Column format

## Basic tests

### x-bar & r chart with manual subgroup size (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$variablesLong <- "Diameter"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormat.csv",
                       options)

test_that("Basic test to create X-bar & R control chart with manual subgroups", {
 plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart1")
})

### x-bar & s chart with manual subgroup size (verified with Minitab) Note: JASP does not use the unbiasing constant.
options <- analysisOptions("variablesChartsSubgroups")
options$variablesLong <- "Diameter"
options$TypeChart <- "xBarSchart"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormat.csv",
                       options)

test_that("Basic test to create X-bar & s control chart with manual subgroups", {
plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart1")
})

### x-bar & r chart with manual subgroup size and stages (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$variablesLong <- "Diameter"
options$stages <- "Stage"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatStages.csv",
                       options)


test_that("Basic test to create X-bar & R control chart with manual subgroups and stages", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart2")
})

### x-bar & s chart with manual subgroup size and stages (verified with Minitab) Note: JASP does not use the unbiasing constant.
options <- analysisOptions("variablesChartsSubgroups")
options$TypeChart <- "xBarSchart"
options$variablesLong <- "Diameter"
options$stages <- "Stage"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatStages.csv",
                       options)

test_that("Basic test to create X-bar & s control chart with manual subgroups and stages", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart2")
})

### x-bar & r chart with subgroup variable (verfied with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$variablesLong <- "Diameter"
options$subgroups <- "Time"
options$subgroupSizeType <- "groupingVariable"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormat.csv",
                       options)

test_that("Basic test to create X-bar & R control chart with subgroup variable", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart3")
})

### x-bar & s chart with subgroup variable (verified with Minitab) Note: JASP does not use the unbiasing constant.
options <- analysisOptions("variablesChartsSubgroups")
options$TypeChart <- "xBarSchart"
options$variablesLong <- "Diameter"
options$subgroups <- "Time"
options$subgroupSizeType <- "groupingVariable"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormat.csv",
                       options)

test_that("Basic test to create X-bar & s control chart with subgroup variable", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart3")
})

### x-bar & r chart with subgroup variable and stages (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$variablesLong <- "Diameter"
options$stages <- "Stage"
options$subgroups <- "Time"
options$subgroupSizeType <- "groupingVariable"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatStages.csv",
                       options)

test_that("Basic test to create X-bar & R control chart with subgroup variable and stages", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart4")
})

### x-bar & s chart with subgroup variable and stages (verified with Minitab) Note: JASP does not use the unbiasing constant.
options <- analysisOptions("variablesChartsSubgroups")
options$TypeChart <- "xBarSchart"
options$variablesLong <- "Diameter"
options$stages <- "Stage"
options$subgroups <- "Time"
options$subgroupSizeType <- "groupingVariable"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatStages.csv",
                       options)

test_that("Basic test to create X-bar & R control chart with subgroup variable and stages", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart4")
})

### x-bar & r chart with warning limits (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$variablesLong <- "Diameter"
options$Wlimits <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormat.csv",
                       options)

test_that("Basic test of adding warning limits to X-bar & R control chart", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart5")
})

### x-bar & s chart with warning limits Note: JASP does not use the unbiasing constant.
options <- analysisOptions("variablesChartsSubgroups")
options$TypeChart <- "xBarSchart"
options$variablesLong <- "Diameter"
options$Wlimits <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormat.csv",
                       options)

test_that("Basic test of adding warning limits to X-bar & s control chart", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart5")
})

### x-bar & r chart with known parameters (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$variablesLong <- "Diameter"
options$Phase2 <- TRUE
options$mean <- 0
options$SD <- 3
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormat.csv",
                       options)

test_that("Basic test of adding known parameters to X-bar & r control chart - plot", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart6")
})

test_that("Basic test of adding known parameters to X-bar & r control chart - x-bar table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_secondTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
})

test_that("Basic test of adding known parameters to X-bar & r control chart - R table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_xBarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 7, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12, 7, 13, 8, 14, 9, 15, 10,
                                      16, 11, 17, 12, 18, 13, 19, 14, 20, 15, "", 16, "", 17, "",
                                      18, "", 19, "", 20, ""))
})

### x-bar & s chart with known parameters (verified with Minitab) Note: JASP does not use the unbiasing constant
options <- analysisOptions("variablesChartsSubgroups")
options$TypeChart <- "xBarSchart"
options$variablesLong <- "Diameter"
options$Phase2 <- TRUE
options$mean <- 0
options$SD <- 3
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormat.csv",
                       options)

test_that("Basic test of adding known parameters to X-bar & s control chart - plot", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart6")
})

test_that("Basic test of adding known parameters to X-bar & s control chart - x-bar table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_secondTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(6, 7, 8, 8, 10, 9, 11, 10, 15, 11, 16, 12, "", 13, "", 14, "",
                                      15, "", 16, "", 17, "", 18, "", 19, "", 20))
})

test_that("Basic test of adding known parameters to X-bar & s control chart - s table", {
  table <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_xBarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 7, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12, 7, 13, 8, 14, 9, 15, 10,
                                      16, 11, 17, 12, 18, 13, 19, 14, 20, 15, "", 16, "", 17, "",
                                      18, "", 19, "", 20, ""))
})

### x-bar & r chart with changed manual subgroup size value (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$variablesLong <- "Diameter"
options$CCSubgroupSize <- 10
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormat.csv",
                       options)

test_that("Basic test of changing manual subgroup size with X-bar & R control chart", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart7")
})

### x-bar & s chart with changed manual subgroup size value (verified with Minitab) Note: JASP does not use the unbiasing constant
options <- analysisOptions("variablesChartsSubgroups")
options$TypeChart <- "xBarSchart"
options$variablesLong <- "Diameter"
options$CCSubgroupSize <- 10
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormat.csv",
                       options)

test_that("Basic test of changing manual subgroup size with X-bar & s control chart", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart7")
})

### Report function with x-bar & r chart (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$variablesLong <- "Diameter"
options$ccName <- "Report name"
options$ccDate <- "01.01.2000"
options$ccSubTitle <- "Your report sub-title"
options$ccReportedBy <- "Operator name"
options$ccMisc <- "Various comments"
options$CCReport <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormat.csv",
                       options)

test_that("Basic test to create report of X-bar & R control chart", {
  plotName <- results[["results"]][["CCReport"]][["collection"]][["CCReport_ccReport"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-report1")
})

### Report function with x-bar & s chart (verified with Minitab) Note: JASP does not use the unbiasing constant
options <- analysisOptions("variablesChartsSubgroups")
options$TypeChart <- "xBarSchart"
options$variablesLong <- "Diameter"
options$ccName <- "Report name"
options$ccDate <- "01.01.2000"
options$ccSubTitle <- "Your report sub-title"
options$ccReportedBy <- "Operator name"
options$ccMisc <- "Various comments"
options$CCReport <- TRUE
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormat.csv",
                       options)

test_that("Basic test to create report of X-bar & s control chart", {
  plotName <- results[["results"]][["CCReport"]][["collection"]][["CCReport_ccReport"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-report1")
})


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

### Missing values in subgroup variable

#### x-bar & r chart

#### x-bar & s chart

### Missing values in stages variable

#### x-bar & r chart

#### x-bar & s chart


## Unequal subgroup sizes

### x-bar & r chart with actual sizes

### x-bar & s chart with actual sizes

### x-bar & r chart with fixed group calculation

### x-bar & s chart with fixed group calculation


## Edge cases

### Subgroup size larger than data or stage

### Multiple stages assigned within subgroup




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
