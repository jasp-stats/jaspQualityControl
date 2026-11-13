context("[Quality Control] Gauge r&R - charts by operator dependencies")

# jasp-stats/INTERNAL-jasp#3414: the range/average chart by operator containers are only built when
# absent, so they must depend on every option that changes their grouping. jaspTools runs are
# stateless (no old state is reused), so the dependency vector is tested directly.

test_that("Range/average charts by operator depend on data assignment, data format and Type 3", {
  required <- c("dataFormat", "type3", "report",
                "measurementLongFormat", "operatorLongFormat", "partLongFormat",
                "measurementsWideFormat", "operatorWideFormat", "partWideFormat",
                jaspQualityControl:::.getDependenciesControlChartRules())
  for (chart in c("rChart", "xBarChart")) {
    deps <- jaspQualityControl:::.msaGaugeRRChartByOperatorDependencies(chart)
    expect_true(chart %in% deps)
    expect_true(all(required %in% deps), info = chart)
    # non-existent option names
    expect_false(any(c("gaugeRRmethod", "anovaGaugeReport") %in% deps), info = chart)
  }
})

# AIAG MSA (4th ed.) crossed study: 10 parts x 3 appraisers x 3 trials
aiagMeasurements <- c(
  0.29, -0.56, 1.34, 0.47, -0.80, 0.02, 0.59, -0.31, 2.26, -1.36,
  0.41, -0.68, 1.17, 0.50, -0.92, -0.11, 0.75, -0.20, 1.99, -1.25,
  0.64, -0.58, 1.27, 0.64, -0.84, -0.21, 0.66, -0.17, 2.01, -1.31,
  0.08, -0.47, 1.19, 0.01, -0.56, -0.20, 0.47, -0.63, 1.80, -1.68,
  0.25, -1.22, 0.94, 1.03, -1.20, 0.22, 0.55, 0.08, 2.12, -1.62,
  0.07, -0.68, 1.34, 0.20, -1.28, 0.06, 0.83, -0.34, 2.19, -1.50,
  0.04, -1.38, 0.88, 0.14, -1.46, -0.29, 0.02, -0.46, 1.77, -1.49,
  -0.11, -1.13, 1.09, 0.20, -1.07, -0.67, 0.01, -0.56, 1.45, -1.77,
  -0.15, -0.96, 0.67, 0.11, -1.45, -0.49, 0.21, -0.49, 1.87, -2.16
)
aiag <- data.frame(
  Part        = rep(1:10, times = 9),
  Trial       = rep(rep(1:3, each = 10), times = 3),
  Operator    = rep(c("A", "B", "C"), each = 30),
  Measurement = aiagMeasurements
)

aiagOptions <- function(operator, type3 = FALSE) {
  options <- analysisOptions("msaGaugeRR")
  options$dataFormat <- "longFormat"
  options$operatorLongFormat <- operator
  options$partLongFormat <- "Part"
  options$measurementLongFormat <- "Measurement"
  options$testSet <- "jaspDefault"
  options$rChart <- TRUE
  options$xBarChart <- TRUE
  options$type3 <- type3
  options
}

test_that("Range chart by operator with Operator = Trial matches a fresh analysis", {
  set.seed(1)
  results <- runAnalysis("msaGaugeRR", aiag, aiagOptions("Trial"), view = FALSE)
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  plot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  built <- ggplot2::ggplot_build(plot)
  labels <- unlist(lapply(built$data, function(d) if ("label" %in% names(d)) as.character(d$label)))
  expect_true(all(c("1", "2", "3") %in% labels))
  expect_false(any(c("A", "B", "C") %in% labels))
  lineLabels <- grep("^(CL|UCL) = ", labels, value = TRUE)
  values <- as.numeric(sub("^.* = ", "", lineLabels))
  names(values) <- sub(" = .*$", "", lineLabels)
  expect_equal(values[["CL"]], 0.54967, tolerance = 1e-3)
  expect_equal(values[["UCL"]], 1.41498, tolerance = 1e-3)
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table, list("No test violations occurred."))
})

test_that("Range/average charts by operator are not shown for a Type 3 study", {
  set.seed(1)
  results <- runAnalysis("msaGaugeRR", aiag, aiagOptions("", type3 = TRUE), view = FALSE)
  expect_identical(results[["status"]], "complete")
  expect_null(results[["results"]][["rChart"]])
  expect_null(results[["results"]][["xBarChart"]])
})
