context("[Quality Control] Traffic light chart - values above 100%")

# The traffic light chart caps only the marker position at 100%; the label shows the real value.

.trafficLabels <- function(results, path) {
  node <- results[["results"]]
  for (p in path) node <- node[[p]]
  obj <- results[["state"]][["figures"]][[node[["data"]]]][["obj"]]
  subplots <- if (ggplot2::is_ggplot(obj)) list(obj) else Filter(ggplot2::is_ggplot, obj$subplots)
  out <- lapply(subplots, function(s) {
    layers <- ggplot2::ggplot_build(s)[["data"]]
    text  <- Filter(function(l) "label" %in% names(l), layers)[[1]]
    vline <- Filter(function(l) "xintercept" %in% names(l), layers)[[1]]
    list(label = as.character(text[["label"]]), labelX = text[["x"]], vline = vline[["xintercept"]])
  })
  list(label  = vapply(out, `[[`, "", "label"),
       labelX = vapply(out, `[[`, 0, "labelX"),
       vline  = vapply(out, `[[`, 0, "vline"))
}

.crossedOptions <- function(tol) {
  options <- analysisOptions("msaGaugeRR")
  options$testSet <- "jaspDefault"
  options$operatorLongFormat <- "Operators"
  options$partLongFormat <- "Parts"
  options$measurementLongFormat <- "Dm"
  options$tolerance <- TRUE
  options$toleranceValue <- tol
  options$trafficLightChart <- TRUE
  options$anovaModelType <- "randomEffect"
  options
}

test_that("Crossed study: %Tolerance above 100% is labelled with its real value", {
  set.seed(1)
  results <- runAnalysis("msaGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", .crossedOptions(2), view = FALSE)
  traffic <- .trafficLabels(results, c("trafficPlot", "collection", "trafficPlot_plot"))
  # subplots: %Study variation, %Tolerance (gauge evaluation table: 11.97, 119.22)
  expect_equal(traffic$label, c("11.97%", "119.22%"))
  expect_equal(traffic$vline, c(11.97, 100))
  expect_equal(traffic$labelX, c(16.97, 105))
})

test_that("Crossed study: values below 100% are unchanged", {
  set.seed(1)
  results <- runAnalysis("msaGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", .crossedOptions(10), view = FALSE)
  traffic <- .trafficLabels(results, c("trafficPlot", "collection", "trafficPlot_plot"))
  expect_equal(traffic$label, c("11.97%", "23.84%"))
  expect_equal(traffic$vline, c(11.97, 23.84))
})

test_that("Nested study: %Tolerance above 100% is labelled with its real value", {
  options <- analysisOptions("msaGaugeRRnonrep")
  options$testSet <- "jaspDefault"
  options$partLongFormat <- "Batch"
  options$operatorLongFormat <- "Operator"
  options$measurementLongFormat <- "Result"
  options$tolerance <- TRUE
  options$toleranceValue <- 0.5
  options$trafficLightChart <- TRUE
  set.seed(1)
  results <- runAnalysis("msaGaugeRRnonrep", "datasets/msaGaugeRRNested/msaGaugeRRNested_long.csv", options, view = FALSE)
  traffic <- .trafficLabels(results, "trafficLightChart")
  # gauge evaluation table: %Study variation 22.40, %Tolerance 340.82 (= 1.70411 / 0.5)
  expect_equal(traffic$label, c("22.40%", "340.82%"))
  expect_equal(traffic$vline, c(22.40296, 100), tolerance = 1e-4)
})

test_that("Test-retest: both values above 100% are labelled with their real values", {
  options <- analysisOptions("msaTestRetest")
  options$dataFormat <- "longFormat"
  options$partLongFormat <- "Part"
  options$measurementLongFormat <- "Measurement"
  options$operator <- "Repetition"
  options$manualProcessSd <- TRUE
  options$manualProcessSdValue <- 0.03
  options$tolerance <- TRUE
  options$toleranceValue <- 0.2
  options$trafficLightChart <- TRUE
  set.seed(1)
  results <- runAnalysis("msaTestRetest", "datasets/msaTestRetest/msaTestRetest_long.csv", options, view = FALSE)
  traffic <- .trafficLabels(results, c("trafficPlot", "collection", "trafficPlot_plot"))
  # short gauge study table: %GRR of process std. dev. 144.97, %GRR of tolerance 130.47
  expect_equal(traffic$label, c("144.97%", "130.47%"))
  expect_equal(traffic$vline, c(100, 100))
  expect_equal(traffic$labelX, c(105, 105))
})
