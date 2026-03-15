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
                       testthat::test_path("datasets/multivariateControlCharts/montgomeryExample1.csv"), options)

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

# Client case: simulated 5-variable dataset compared against qcc ####

clientN <- 250
clientMeans <- c(x1 = 10, x2 = 12, x3 = 15, x4 = 8, x5 = 20)
clientSds <- c(2, 1.5, 3, 1, 2.5)
clientCorMatrix <- matrix(
  c(1, .5, .3, .2, .1,
    .5, 1, .4, .2, .1,
    .3, .4, 1, .3, .2,
    .2, .2, .3, 1, .4,
    .1, .1, .2, .4, 1),
  nrow = 5,
  byrow = TRUE
)
clientSigmaMatrix <- diag(clientSds) %*% clientCorMatrix %*% diag(clientSds)

set.seed(123)
clientDataset <- as.data.frame(MASS::mvrnorm(n = clientN, mu = clientMeans, Sigma = clientSigmaMatrix))
colnames(clientDataset) <- names(clientMeans)

clientReference <- qcc::mqcc(clientDataset, type = "T2.single", plot = FALSE)

options7 <- analysisOptions("multivariateControlCharts")
options7$testSet <- "jaspDefault"
options7$variables <- names(clientMeans)
options7$confidenceLevelAutomatic <- TRUE
options7$centerTable <- TRUE
options7$covarianceMatrixTable <- TRUE
options7$tSquaredValuesTable <- TRUE
results7 <- runAnalysis("multivariateControlCharts", clientDataset, options7)

test_that("7.1 Simulated 5-variable case - summary matches qcc", {
  table <- results7[["results"]][["summaryTable"]][["data"]]
  row <- table[[1]]

  expect_equal(row$numVariables, length(clientMeans))
  expect_equal(row$numObservations, clientN)
  expect_equal(row$confidenceLevel, (1 - 0.0027)^length(clientMeans))
  expect_equal(row$lcl, as.numeric(clientReference$limits[, "LCL"]))
  expect_equal(row$ucl, as.numeric(clientReference$limits[, "UCL"]))
  expect_equal(row$detS, det(clientReference$cov), tolerance = 1e-10)
})

test_that("7.2 Simulated 5-variable case - centers and covariance match qcc", {
  centerTable <- results7[["results"]][["centerTable"]][["data"]]
  centerValues <- sapply(centerTable, function(x) x$center)
  names(centerValues) <- sapply(centerTable, function(x) x$variable)

  expect_equal(unname(centerValues[names(clientReference$center)]),
               unname(as.numeric(clientReference$center)),
               tolerance = 1e-10)

  covarianceTable <- results7[["results"]][["covarianceTable"]][["data"]]
  covarianceMatrix <- do.call(rbind, lapply(covarianceTable, function(row) unlist(row[names(clientMeans)])))
  rownames(covarianceMatrix) <- sapply(covarianceTable, function(row) row$variable)

  expect_equal(unname(covarianceMatrix[names(clientMeans), names(clientMeans)]),
               unname(clientReference$cov),
               tolerance = 1e-10)
})

test_that("7.3 Simulated 5-variable case - T² values and statuses match qcc", {
  table <- results7[["results"]][["tsqValuesTable"]][["data"]]
  jaspTsq <- sapply(table, function(x) x$tsq)
  expectedTsq <- as.numeric(clientReference$statistics)
  expectedUcl <- as.numeric(clientReference$limits[, "UCL"])
  expectedStatus <- ifelse(expectedTsq > expectedUcl, "Out of control", "In control")

  expect_equal(jaspTsq, expectedTsq, tolerance = 1e-10)
  expect_equal(sapply(table, function(x) x$status), expectedStatus)
  expect_equal(sum(expectedStatus == "Out of control"), 4)
})

# Client case: simulated 5-variable phased dataset compared against qcc ####

set.seed(123)
client2NPerState <- 100
client2P <- 5
client2States <- c("State1", "State2")

client2CorList <- list(
  State1 = matrix(c(1, .5, .3, .2, .1,
                    .5, 1, .4, .2, .1,
                    .3, .4, 1, .3, .2,
                    .2, .2, .3, 1, .4,
                    .1, .1, .2, .4, 1), 5, 5),
  State2 = matrix(0.8, nrow = client2P, ncol = client2P)
)
diag(client2CorList$State2) <- 1

client2Params <- list(
  State1 = list(mu = c(15, 18, 22.5, 12, 30), sd = c(2, 1.5, 3, 1, 2.5)),
  State2 = list(mu = c(12, 10, 18, 9, 22),    sd = c(1, 2.0, 2, 1.5, 3))
)

client2DatasetList <- lapply(client2States, function(state) {
  sigmaMatrix <- diag(client2Params[[state]]$sd) %*% client2CorList[[state]] %*% diag(client2Params[[state]]$sd)
  data <- MASS::mvrnorm(n = client2NPerState, mu = client2Params[[state]]$mu, Sigma = sigmaMatrix)
  tempDf <- as.data.frame(data)
  colnames(tempDf) <- paste0("x", 1:client2P)
  tempDf$State <- state
  tempDf
})

client2Dataset <- do.call(rbind, client2DatasetList)
client2Phase1Data <- client2Dataset[client2Dataset$State == "State1", 1:client2P]
client2Phase2Data <- client2Dataset[client2Dataset$State == "State2", 1:client2P]
client2Reference <- qcc::mqcc(
  client2Phase1Data,
  type = "T2.single",
  newdata = client2Phase2Data,
  pred.limits = TRUE,
  confidence.level = (1 - 0.0027)^client2P,
  plot = FALSE
)

options8 <- analysisOptions("multivariateControlCharts")
options8$testSet <- "jaspDefault"
options8$variables <- paste0("x", 1:client2P)
options8$stage <- "State"
options8$trainingLevel <- "State1"
options8$confidenceLevelAutomatic <- TRUE
options8$centerTable <- TRUE
options8$covarianceMatrixTable <- TRUE
options8$tSquaredValuesTable <- TRUE
results8 <- runAnalysis("multivariateControlCharts", client2Dataset, options8)

test_that("8.1 Simulated phased 5-variable case - summary matches qcc", {
  table <- results8[["results"]][["summaryTable"]][["data"]]

  expect_equal(length(table), 2)

  trainingRow <- table[[1]]
  testRow <- table[[2]]

  expect_equal(trainingRow$phase, "Training (State1)")
  expect_equal(trainingRow$numVariables, client2P)
  expect_equal(trainingRow$numObservations, client2NPerState)
  expect_equal(trainingRow$confidenceLevel, (1 - 0.0027)^client2P)
  expect_equal(trainingRow$lcl, as.numeric(client2Reference$limits[, "LCL"]))
  expect_equal(trainingRow$ucl, as.numeric(client2Reference$limits[, "UCL"]))
  expect_equal(trainingRow$detS, det(client2Reference$cov), tolerance = 1e-10)

  expect_equal(testRow$phase, "Test")
  expect_equal(testRow$numVariables, client2P)
  expect_equal(testRow$numObservations, client2NPerState)
  expect_equal(testRow$confidenceLevel, (1 - 0.0027)^client2P)
  expect_equal(testRow$lcl, as.numeric(client2Reference$pred.limits[, "LPL"]))
  expect_equal(testRow$ucl, as.numeric(client2Reference$pred.limits[, "UPL"]))
  expect_equal(testRow$detS, det(client2Reference$cov), tolerance = 1e-10)
})

test_that("8.2 Simulated phased 5-variable case - training centers and covariance match qcc", {
  centerTable <- results8[["results"]][["centerTable"]][["data"]]
  centerValues <- sapply(centerTable, function(x) x$center)
  names(centerValues) <- sapply(centerTable, function(x) x$variable)

  expect_equal(unname(centerValues[names(client2Reference$center)]),
               unname(as.numeric(client2Reference$center)),
               tolerance = 1e-10)

  covarianceTable <- results8[["results"]][["covarianceTable"]][["data"]]
  covarianceMatrix <- do.call(rbind, lapply(covarianceTable, function(row) unlist(row[paste0("x", 1:client2P)])))
  rownames(covarianceMatrix) <- sapply(covarianceTable, function(row) row$variable)

  expect_equal(unname(covarianceMatrix[paste0("x", 1:client2P), paste0("x", 1:client2P)]),
               unname(client2Reference$cov),
               tolerance = 1e-10)
})

test_that("8.3 Simulated phased 5-variable case - T² values, phases, and statuses match qcc", {
  table <- results8[["results"]][["tsqValuesTable"]][["data"]]
  expectedTsq <- c(as.numeric(client2Reference$statistics), as.numeric(client2Reference$newstats))
  expectedUcl <- c(rep(as.numeric(client2Reference$limits[, "UCL"]), client2NPerState),
                   rep(as.numeric(client2Reference$pred.limits[, "UPL"]), client2NPerState))
  expectedPhase <- c(rep("Training (State1)", client2NPerState), rep("Test", client2NPerState))
  expectedStatus <- ifelse(expectedTsq > expectedUcl, "Out of control", "In control")

  expect_equal(length(table), 2 * client2NPerState)
  expect_equal(sapply(table, function(x) x$phase), expectedPhase)
  expect_equal(sapply(table, function(x) x$tsq), expectedTsq, tolerance = 1e-10)
  expect_equal(sapply(table, function(x) x$status), expectedStatus)
  expect_equal(sum(expectedStatus[1:client2NPerState] == "Out of control"), 1)
  expect_equal(sum(expectedStatus[(client2NPerState + 1):(2 * client2NPerState)] == "Out of control"), 92)
})
