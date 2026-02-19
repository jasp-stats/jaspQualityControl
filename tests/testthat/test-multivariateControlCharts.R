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
                                 list(2, 20, 0.994600729, 0, 8.28592806, 0.000102579726315789))
})

test_that("1.3 Montgomery Ex1 - Center table", {
  table <- results[["results"]][["centerTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Strength", 115.5875,
                                      "Diameter", 1.058))
})

test_that("1.4 Montgomery Ex1 - Covariance matrix", {
  table <- results[["results"]][["covarianceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Strength", 0.147188157894737, -0.00146842105263158,
                                      "Diameter", -0.00146842105263158, 0.000711578947368421))
})

test_that("1.5 Montgomery Ex1 - T² values table", {
  table <- results[["results"]][["tsqValuesTable"]][["data"]]
  # 20 rows, each with sample, tsq, status
  expect_equal(length(table), 20)
  # All points should be in control for this example
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
                                 list(4, 30, 0.989208119, 0, 11.121374, 156.12268))
})

test_that("2.3 Montgomery Ex3 - Out of control points detected", {
  table <- results2[["results"]][["tsqValuesTable"]][["data"]]
  outOfControl <- sapply(table, function(x) x$status == "Out of control")
  # At least one point should be out of control (sample 24 is known)
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
  # UCL should be higher with higher confidence level
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
