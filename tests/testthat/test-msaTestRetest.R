context("[Quality Control] MSA Test Retest")
.numDecimals <- 2

# Long Format ####

## Default Settings ####

options <- analysisOptions("msaTestRetest")
options$dataFormat <- "longFormat"
options$partLongFormat <- "Part"
options$measurementLongFormat <- "Measurement"
options$operator <- "Repetition"
options$manualProcessSd <- TRUE
options$manualProcessSdValue <- 2
options$tolerance <- TRUE
options$toleranceValue <- 0.5
options$repeatabilityAndReproducibilityTable <- TRUE
options$runChartPart <- TRUE
options$scatterPlotMeasurement <- TRUE
options$scatterPlotMeasurementFitLine <- TRUE
options$rChart <- TRUE
options$trafficLightChart <- TRUE

results <- runAnalysis("msaTestRetest", "datasets/msaTestRetest/msaTestRetest_long.csv", options)

test_that("LF1.1 Default Settings - Run chart of parts plot matches", {
  plotName <- results[["results"]][["ScatterOperatorParts"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_run-chart-of-parts")
})

test_that("LF1.2 Default Settings - Scatterplot of 1st measurement vs 2nd measurement matches", {
  plotName <- results[["results"]][["ScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_scatterplot-of-1st-measurement-vs-2nd-measurement")
})

test_that("LF1.3 Default Settings - Short gauge study table results match", {
  table <- results[["results"]][["rAndR2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0434914974122558, 0.0217457487061279, 0.52189796894707, 2, 0.0499999999999999,
                                      1.14965, 15, 0.5))
})

test_that("LF1.4 Default Settings - Range chart by part plot matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_range-chart-by-part")
})

test_that("LF1.5 Default Settings - Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("LF1.6 Default Settings - Traffic chart matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_trafficChart")
})

# Wide Format ####

## Default Settings ####

options <- analysisOptions("msaTestRetest")
options$dataFormat <- "wideFormat"
options$partWideFormat <- "Part"
options$measurementsWideFormat <- c("Measurement1", "Measurement2")
options$manualProcessSd <- TRUE
options$manualProcessSdValue <- 2
options$tolerance <- TRUE
options$toleranceValue <- 0.5
options$repeatabilityAndReproducibilityTable <- TRUE
options$runChartPart <- TRUE
options$scatterPlotMeasurement <- TRUE
options$scatterPlotMeasurementFitLine <- TRUE
options$rChart <- TRUE
options$trafficLightChart <- TRUE

results <- runAnalysis("msaTestRetest", "datasets/msaTestRetest/msaTestRetest_wide.csv", options)


test_that("WF1.1 Default Settings - Run chart of parts plot matches", {
  plotName <- results[["results"]][["ScatterOperatorParts"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_run-chart-of-parts")
})

test_that("WF1.2 Default Settings - Scatterplot of 1st measurement vs 2nd measurement matches", {
  plotName <- results[["results"]][["ScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_scatterplot-of-1st-measurement-vs-2nd-measurement")
})

test_that("WF1.3 Default Settings - Short gauge study table results match", {
  table <- results[["results"]][["rAndR2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0434914974122558, 0.0217457487061279, 0.52189796894707, 2, 0.0499999999999999,
                                      1.14965, 15, 0.5))
})

test_that("WF1.4 Default Settings - Range chart by part plot matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_range-chart-by-part")
})

test_that("WF1.5 Default Settings - Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("WF1.6 Default Settings - Traffic chart matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_traffiChart")
})

