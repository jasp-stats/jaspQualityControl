context("[Quality Control] MSA - Type 1 Instrument Capability")
.numDecimals <- 2
set.seed(1)

# Basic tests ####

## Standard settings (verified with Minitab) ####

options <- analysisOptions("msaType1Gauge")
options$measurement <- "dm"
options$referenceValue <- -4
options$toleranceRange <- 15
options$histogram <- TRUE
options$biasTable <- TRUE
options$percentToleranceForCg <- 20
results <- runAnalysis("msaType1Gauge", "datasets/msaType1InstrumentCapability/msaType1.csv", options)

test_that("1.1 Standard settings - Bias histogram plot matches", {
  plotName <- results[["results"]][["biasHistogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "1_bias-histogram")
})

test_that("1.2 Standard settings - Run chart of dm plot matches", {
  plotName <- results[["results"]][["biasRun"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "1_run-chart-of-dm")
})

test_that("1.3 Standard settings - Basic statistics table results match", {
  table <- results[["results"]][["biasTable"]][["collection"]][["biasTable_Basic"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.52008746203907, 0.0540000000000003, 0.360000000000002, -3.946,
                                      -4, 0.420014577006512, 15, -5.5, -2.5))
})

test_that("1.4 Standard settings - Capability table results match", {
  table <- results[["results"]][["biasTable"]][["collection"]][["biasTable_Capability"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.19, 1.15, 16.8, 17.43))
})

test_that("1.5 Standard settings - t-test of observed bias against 0 table results match", {
  table <- results[["results"]][["biasTtest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0540000000000001, 49, -0.0653668220476197, 0.367744349288335,
                                      0.909105737620188, 0.17336682204762))
})

## Alternative settings (verified with Minitab)  ####

options <- analysisOptions("msaType1Gauge")
options$measurement <- "dm"
options$referenceValue <- -4
options$toleranceRange <- 15
options$histogram <- TRUE
options$biasTable <- TRUE
options$percentToleranceForCg <- 15
options$studyVarianceMultiplier <- 4
results <- runAnalysis("msaType1Gauge", "datasets/msaType1InstrumentCapability/msaType1.csv", options)

test_that("2.1 Alternative settings - Bias histogram plot matches", {
  plotName <- results[["results"]][["biasHistogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "2_bias-histogram")
})

test_that("2.2 Alternative settings - Run chart of dm plot matches", {
  plotName <- results[["results"]][["biasRun"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "2_run-chart-of-dm")
})

test_that("2.3 Alternative settings - Basic statistics table results match", {
  table <- results[["results"]][["biasTable"]][["collection"]][["biasTable_Basic"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.68005830802605, 0.0540000000000003, 0.360000000000002, -3.946,
                                      -4, 0.420014577006512, 15, -5.125, -2.875))
})

test_that("2.4 Alternative settings - Capability table results match", {
  table <- results[["results"]][["biasTable"]][["collection"]][["biasTable_Capability"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.34, 1.27, 11.2, 11.77))
})

test_that("2.5 Alternative settings - t-test of observed bias against 0 table results match", {
  table <- results[["results"]][["biasTtest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0540000000000001, 49, -0.0653668220476197, 0.367744349288335,
                                      0.909105737620188, 0.17336682204762))
})

# Missing data ####

## One missing value (verified with Minitab)  ####

options <- analysisOptions("msaType1Gauge")
options$measurement <- "dmMissing1"
options$referenceValue <- -4
options$toleranceRange <- 15
options$histogram <- TRUE
options$biasTable <- TRUE
options$percentToleranceForCg <- 20
results <- runAnalysis("msaType1Gauge", "datasets/msaType1InstrumentCapability/msaType1.csv", options)

test_that("3.1 Missing 1 value - Bias histogram plot matches", {
  plotName <- results[["results"]][["biasHistogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "3_bias-histogram")
})

test_that("3.2 Missing 1 value - Run chart of dmMissing1 plot matches", {
  plotName <- results[["results"]][["biasRun"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "3_run-chart-of-dmmissing1")
})

test_that("3.3 Missing 1 value - Basic statistics table results match", {
  table <- results[["results"]][["biasTable"]][["collection"]][["biasTable_Basic"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.54588503515868, 0.0530612244897961, 0.353741496598641, -3.9469387755102,
                                      -4, 0.424314172526446, 15, -5.5, -2.5))
})

test_that("3.4 Missing 1 value - Capability table results match", {
  table <- results[["results"]][["biasTable"]][["collection"]][["biasTable_Capability"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.18, 1.14, 16.97, 17.59))
})

test_that("3.5 Missing 1 value - t-test of observed bias against 0 table results match", {
  table <- results[["results"]][["biasTtest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.053061224489796, 48, -0.0688160360008093, 0.385736270518345,
                                      0.875362161996656, 0.174938484980401))
})

## Half missing value (verified with Minitab)  ####

options <- analysisOptions("msaType1Gauge")
options$measurement <- "dmMissing25"
options$referenceValue <- -4
options$toleranceRange <- 15
options$histogram <- TRUE
options$biasTable <- TRUE
options$percentToleranceForCg <- 20
results <- runAnalysis("msaType1Gauge", "datasets/msaType1InstrumentCapability/msaType1.csv", options)

test_that("4.1 Missing half values - Bias histogram plot matches", {
  plotName <- results[["results"]][["biasHistogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "4_bias-histogram")
})

test_that("4.2 Missing half values - Run chart of dmMissing25 plot matches", {
  plotName <- results[["results"]][["biasRun"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "4_run-chart-of-dmmissing25")
})

test_that("4.3 Missing half values - Basic statistics table results match", {
  table <- results[["results"]][["biasTable"]][["collection"]][["biasTable_Basic"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.16189500386222, 0, 0, -4, -4, 0.193649167310371, 15, -5.5, -2.5
                                 ))
})

test_that("4.4 Missing half values - Capability table results match", {
  table <- results[["results"]][["biasTable"]][["collection"]][["biasTable_Capability"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.58, 2.58, 7.75, 7.75))
})

test_that("4.5 Missing half values - t-test of observed bias against 0 table results match", {
  table <- results[["results"]][["biasTtest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.06580965841124e-16, 24, -0.0799344475744677, 0.999999999999998,
                                      2.75190870483584e-15, 0.0799344475744679))
})

## All-but-one missing value ####

options <- analysisOptions("msaType1Gauge")
options$measurement <- "dmMissing49"
options$referenceValue <- -4
options$toleranceRange <- 15
options$histogram <- TRUE
options$biasTable <- TRUE
options$percentToleranceForCg <- 20
results <- runAnalysis("msaType1Gauge", "datasets/msaType1InstrumentCapability/msaType1.csv", options)

test_that("5.1 Missing all-but-one values - Run chart of dmMissing49 plot matches", {
  plotName <- results[["results"]][["biasRun"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "5_run-chart-of-dmmissing49")
})

test_that("5.2 Missing all-but-one values - Basic statistics table results match", {
  table <- results[["results"]][["biasTable"]][["collection"]][["biasTable_Basic"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", 0.5, 3.33333333333333, -3.5, -4, "", 15, -5.5, -2.5))
})

test_that("5.3 Missing all-but-one values - Capability table results match", {
  table <- results[["results"]][["biasTable"]][["collection"]][["biasTable_Capability"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", "", "", ""))
})
