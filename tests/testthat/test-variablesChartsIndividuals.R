context("[Quality Control] Variables Charts for Individuals")
.numDecimals <- 2
set.seed(1)

# Basic tests

## X-mR chart

## Autocorrelation plot

## X-mR chart with stages

## X-mR chart with large moving range length

## X-mR chart with stages and large moving range length

## Autocorrelation plot with large number of lags

## Report including everything

## Report including only x-mR chart


# Missing values

## Missing values in measurement

### Single missing value

### All but one missing

### All missing

### All of one axis label missing

## Missing value in axis label

## Missing value in stages


# Edge cases

## Moving range length larger than one of the stages















# basic test for IMR chart & table (verified with Minitab) and autocorrelation plot
# options <- analysisOptions("variablesChartsIndividuals")
# options$variables <- "Yield"
# options$subgroups <- "Month"
# options$CorPlot <- TRUE
# options$movingRangeLength <- 2
# set.seed(1)
# results <- runAnalysis("variablesChartsIndividuals", "IndividualChartStages.csv", options)
#
#
# test_that("Yield plot matches", {
#   plotName <- results[["results"]][["CorPlot"]][["collection"]][["CorPlot_Yield"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "yield")
# })
#
# test_that("titleless-plot-1 matches", {
#   plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Yield"]][["collection"]][["Ichart_Yield_Plot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "titleless-plot-1")
# })
#
# test_that("Test results for individuals chart table results match", {
#   table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Yield"]][["collection"]][["Ichart_Yield_Table1"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(3, 11, 4, 4, 12, 24, 23, 13, "", 24, 14, "", "", 15, "", "", 16,
#                                       "", "", 17, "", "", 18, "", "", 19, "", "", 20, "", "", 21,
#                                       "", "", 22, "", "", 31, "", "", 32, "", "", 33, ""))
# })
#
# test_that("Test results for range chart table results match", {
#   table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Yield"]][["collection"]][["Ichart_Yield_Table2"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(3, 12, 5, 13, 23, 14, 25, 15, "", 16, "", 32, "", 33))
# })
#
# # test for different moving range lengths (verified with Minitab)
# options$CorPlot <- FALSE
# options$movingRangeLength <- 5
# results <- runAnalysis("variablesChartsIndividuals", "IndividualChartStages.csv", options)
#
#
# test_that("titleless-plot-2 matches", {
#   plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Yield"]][["collection"]][["Ichart_Yield_Plot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "titleless-plot-2")
# })
#
# test_that("Test results for individuals chart table results match", {
#   table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Yield"]][["collection"]][["Ichart_Yield_Table1"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(3, 11, 4, 4, 12, 24, 23, 13, "", 24, 14, "", "", 15, "", "", 16,
#                                       "", "", 17, "", "", 18, "", "", 19, "", "", 20, "", "", 21,
#                                       "", "", 22, "", "", 31, "", "", 32, "", "", 33, ""))
# })
#
# test_that("Test results for range chart table results match", {
#   table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Yield"]][["collection"]][["Ichart_Yield_Table2"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(2, 12, 3, 13, 4, 14, 5, 15, 20, 16, 21, 17, 22, 18, 23, 19, 24,
#                                       "", 25, ""))
# })
#
# # test for more extreme moving range length (verified with Minitab)
# options$movingRangeLength <- 30
# results <- runAnalysis("variablesChartsIndividuals", "IndividualChartStages.csv", options)
# test_that("titleless-plot-3 matches", {
#   plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Yield"]][["collection"]][["Ichart_Yield_Plot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "titleless-plot-3")
# })
#
# test_that("Test results for individuals chart table results match", {
#   table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Yield"]][["collection"]][["Ichart_Yield_Table1"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(3, 11, 4, 19, 4, 12, 24, 20, 23, 13, "", 21, 24, 14, "", 22, "",
#                                       15, "", "", "", 16, "", "", "", 17, "", "", "", 18, "", "",
#                                       "", 19, "", "", "", 20, "", "", "", 21, "", "", "", 22, "",
#                                       "", "", 31, "", "", "", 32, "", "", "", 33, "", ""))
# })
#
# # test analysis of stages plot (verified with Minitab)
# options$split <- "Stage"
# options$movingRangeLength <- 2
# results <- runAnalysis("variablesChartsIndividuals", "IndividualChartStages.csv", options)
#
#
# test_that("titleless-plot-4 matches", {
#   plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Yield"]][["collection"]][["Ichart_Yield_Plot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "titleless-plot-4")
# })
#
# test_that("Test results for individuals chart table results match", {
#   table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Yield"]][["collection"]][["Ichart_Yield_Table1"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list("Zero", 3, 11, 4, "", 4, 12, "", "", "", 13, "", "", "", 14, "",
#                                       "Three", 23, 31, 24, "", 24, 32, "", "", "", 33, ""))
# })
#
# test_that("Test results for range chart table results match", {
#   table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Yield"]][["collection"]][["Ichart_Yield_Table2"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list("Zero", 3, 12, "", 5, 13, "", "", 14, "Three", 23, 32, "", 25,
#                                       33))
# })
