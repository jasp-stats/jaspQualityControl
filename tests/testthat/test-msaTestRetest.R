context("[Quality Control] MSA Test Retest")

options <- analysisOptions("msaTestRetest")
options$dataFormat <- "wideFormat"
options$part <- "Part"
options$measurementsWideFormat <- c("X1", "X2")
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

results <- runAnalysis("msaTestRetest", "msaTestRetest.csv", options)

test_that("Run chart of parts plot matches", {
  plotName <- results[["results"]][["ScatterOperatorParts"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "run-chart-of-parts")
})

test_that("Scatterplot of 1st measurement vs 2nd measurement matches", {
  plotName <- results[["results"]][["ScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "scatterplot-of-1st-measurement-vs-2nd-measurement")
})

test_that("Short gauge study table results match", {
  table <- results[["results"]][["rAndR2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0434914974122558, 2.17457487061279, 0.52189796894707, 2, 0.0499999999999999,
                                      1.14965, 15, 0.5))
})

test_that("Range chart by part plot matches", {
  plotName <- results[["results"]][["rangeRchart"]][["collection"]][["rangeRchart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "range-chart-by-part")
})

test_that("Traffic light chart matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "Traffic-light-chart")
})

