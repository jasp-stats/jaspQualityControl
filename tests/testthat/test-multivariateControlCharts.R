context("[Quality Control] Multivariate Control Charts")
.numDecimals <- 2
set.seed(1)

# Montgomery Example 1: 2 variables, 20 observations ####
# Reference: Montgomery, D.C., Introduction to Statistical Quality Control, 6th Ed, Table 11.1

options <- analysisOptions("multivariateControlCharts")
options$testSet <- "jaspDefault"
options$variables <- c("Strength", "Diameter")
options$confidenceLevelAutomatic <- TRUE
options$centerTable <- TRUE
options$covarianceMatrixTable <- TRUE
options$tSquaredValuesTable <- TRUE
results <- runAnalysis("multivariateControlCharts",
                       "datasets/multivariateControlCharts/montgomeryExample1.csv", options)

test_that("1.1 Montgomery Ex1 - T² chart is created", {
  plotName <- results[["results"]][["tsqChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "hotelling-t2-chart-example1")
})

test_that("1.2 Montgomery Ex1 - Summary table", {
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.99460729, 0.000102579734072022, 0, 20, 2, 8.28592787636626))
})

test_that("1.3 Montgomery Ex1 - Center table", {
  table <- results[["results"]][["centerTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115.5875, "Strength",
                                      1.058, "Diameter"))
})

test_that("1.4 Montgomery Ex1 - Covariance matrix", {
  table <- results[["results"]][["covarianceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.00146842105263156, 0.147188157894736, "Strength",
                                      0.000711578947368422, -0.00146842105263156, "Diameter"))
})

test_that("1.5 Montgomery Ex1 - T² values table", {
  table <- results[["results"]][["tsqValuesTable"]][["data"]]
  expect_equal(length(table), 20)
  statuses <- sapply(table, function(x) x$status)
  expect_true(all(statuses == "In control"))
})

# Montgomery Example 3: 4 variables, 30 observations ####
# Reference: Montgomery, D.C., Introduction to Statistical Quality Control, 6th Ed, Table 11.6

options2 <- analysisOptions("multivariateControlCharts")
options2$testSet <- "jaspDefault"
options2$variables <- c("x1", "x2", "x3", "x4")
options2$confidenceLevelAutomatic <- TRUE
options2$covarianceMatrixTable <- FALSE
options2$tSquaredValuesTable <- TRUE
results2 <- runAnalysis("multivariateControlCharts",
                        "datasets/multivariateControlCharts/montgomeryExample3.csv", options2)

test_that("2.1 Montgomery Ex3 - T² chart is created", {
  plotName <- results2[["results"]][["tsqChart"]][["data"]]
  testPlot <- results2[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "hotelling-t2-chart-example3")
})

test_that("2.2 Montgomery Ex3 - Summary table", {
  table <- results2[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.989243661321144, 156.122662346298, 0, 30, 4, 11.1213684523803))
})

test_that("2.3 Montgomery Ex3 - Out of control points detected", {
  table <- results2[["results"]][["tsqValuesTable"]][["data"]]
  outOfControl <- sapply(table, function(x) x$status == "Out of control")
  expect_true(any(outOfControl))
})

# Custom confidence level ####

options3 <- analysisOptions("multivariateControlCharts")
options3$testSet <- "jaspDefault"
options3$variables <- c("Strength", "Diameter")
options3$confidenceLevelAutomatic <- FALSE
options3$confidenceLevel <- 0.999
options3$covarianceMatrixTable <- FALSE
options3$tSquaredValuesTable <- FALSE
results3 <- runAnalysis("multivariateControlCharts",
                        "datasets/multivariateControlCharts/montgomeryExample1.csv", options3)

test_that("3.1 Custom confidence level - Summary table", {
  table <- results3[["results"]][["summaryTable"]][["data"]]
  ucl <- table[[1]]$ucl
  expect_true(ucl > 8.286)
})

test_that("3.2 Custom confidence level - Chart is created", {
  plotName <- results3[["results"]][["tsqChart"]][["data"]]
  testPlot <- results3[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "hotelling-t2-chart-custom-cl")
})

# Edge case: not ready (no variables assigned) ####

options4 <- analysisOptions("multivariateControlCharts")
options4$testSet <- "jaspDefault"
options4$variables <- list()
results4 <- runAnalysis("multivariateControlCharts",
                        "datasets/multivariateControlCharts/montgomeryExample1.csv", options4)

test_that("4.1 No variables - analysis returns without error", {
  expect_true(results4$status != "fatalError" || is.null(results4$status))
})

# Phase I/II: Training-validation split ####

options5 <- analysisOptions("multivariateControlCharts")
options5$testSet <- "jaspDefault"
options5$variables <- c("Strength", "Diameter")
options5$stage <- "Phase"
options5$trainingLevel <- "Training"
options5$confidenceLevelAutomatic <- TRUE
options5$centerTable <- TRUE
options5$covarianceMatrixTable <- TRUE
options5$tSquaredValuesTable <- TRUE
results5 <- runAnalysis("multivariateControlCharts",
                        "datasets/multivariateControlCharts/phasedExample.csv", options5)

test_that("5.1 Phased - T² chart is created", {
  plotName <- results5[["results"]][["tsqChart"]][["data"]]
  testPlot <- results5[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "hotelling-t2-chart-phased")
})

test_that("5.2 Phased - Summary table has two rows with per-phase limits", {
  table <- results5[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.99460729, 0.000201350315680274, 0, 15, 2, "Training (Training)",
                                      7.59483590446819, 0.99460729, 0.000201350315680274, 0, 10, 2,
                                      "Test", 18.4177171929513))
})

test_that("5.3 Phased - Center table with training-only footnote", {
  table <- results5[["results"]][["centerTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115.784, "Strength",
                                      1.04853333333333, "Diameter"))
  footnotes <- results5[["results"]][["centerTable"]][["footnotes"]]
  footnoteText <- footnotes[[1]]$text
  expect_true(grepl("training phase", footnoteText, ignore.case = TRUE))
})

test_that("5.4 Phased - Covariance table with training-only footnote", {
  table <- results5[["results"]][["covarianceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.00189700000000003, 0.152154285714287, "Strength",
                                      0.00134698095238095, 0.00189700000000003, "Diameter"))
  footnotes <- results5[["results"]][["covarianceTable"]][["footnotes"]]
  footnoteText <- footnotes[[1]]$text
  expect_true(grepl("training phase", footnoteText, ignore.case = TRUE))
})

test_that("5.5 Phased - T² values table has phase column", {
  table <- results5[["results"]][["tsqValuesTable"]][["data"]]
  expect_equal(length(table), 25)
  phases <- sapply(table, function(x) x$phase)
  expect_true(all(phases[1:15] == "Training (Training)"))
  expect_true(all(phases[16:25] == "Test"))
  statuses <- sapply(table, function(x) x$status)
  expect_true(all(statuses == "In control"))
})

test_that("5.6 Phased - Training UCL uses Beta, Test UCL uses F prediction limits", {
  table <- results5[["results"]][["summaryTable"]][["data"]]
  trainingUCL <- table[[1]]$ucl
  testUCL <- table[[2]]$ucl
  # Test UCL (F prediction) should be larger than training UCL (Beta)
  expect_true(testUCL > trainingUCL)
})

# Optional timestamp / axis labels ####

options6 <- analysisOptions("multivariateControlCharts")
options6$testSet <- "jaspDefault"
options6$variables <- c("Strength", "Diameter")
options6$axisLabels <- "Timestamp"
options6$tSquaredValuesTable <- TRUE

dataset6 <- utils::read.csv("datasets/multivariateControlCharts/montgomeryExample1.csv")
dataset6$Timestamp <- paste0("t", seq_len(nrow(dataset6)))

results6 <- runAnalysis("multivariateControlCharts", dataset6, options6)

test_that("6.1 Optional timestamp - x axis title uses timestamp", {
  plotName <- results6[["results"]][["tsqChart"]][["data"]]
  testPlot <- results6[["state"]][["figures"]][[plotName]][["obj"]]
  xScale <- testPlot$scales$get_scales("x")
  expect_equal(xScale$name, "Timestamp")
})

test_that("6.2 Optional timestamp - T² values table includes timestamp column", {
  table <- results6[["results"]][["tsqValuesTable"]][["data"]]
  timestamps <- sapply(table, function(x) x$timestamp)
  expect_equal(timestamps[1], "t1")
  expect_equal(timestamps[length(timestamps)], "t20")
})
