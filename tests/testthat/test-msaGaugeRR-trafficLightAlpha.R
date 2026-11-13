context("[Quality Control] Gauge r&R - traffic light chart and interaction alpha")

# jasp-stats/INTERNAL-jasp#3426: "Alpha interaction removal" decides whether the Part x Operator
# interaction is kept, so it changes the traffic light chart. jaspTools runs are stateless, so the
# stale-output bug is guarded by the container's dependency vector; the fresh-run numbers check
# that the chart follows the alpha (AIAG MSA 4th ed. crossed data, tolerance 8, interaction p = 0.974).

test_that("Traffic light chart depends on the interaction alpha", {
  expect_true("anovaAlphaForInteractionRemoval" %in% jaspQualityControl:::.gaugeRRTrafficLightDependencies())
})

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
aiag <- data.frame(Part = rep(1:10, times = 9), Operator = rep(c("A", "B", "C"), each = 30), Measurement = aiagMeasurements)

.trafficLightRun <- function(alpha) {
  options <- jaspTools::analysisOptions("msaGaugeRR")
  options$operatorLongFormat <- "Operator"
  options$partLongFormat <- "Part"
  options$measurementLongFormat <- "Measurement"
  options$tolerance <- TRUE
  options$toleranceValue <- 8
  options$trafficLightChart <- TRUE
  options$anovaModelType <- "randomEffect"
  options$anovaAlphaForInteractionRemoval <- alpha
  jaspTools::runAnalysis("msaGaugeRR", aiag, options, view = FALSE)
}

.trafficLightLabels <- function(results) {
  plot <- results$state$figures[[results$results$trafficPlot$collection$trafficPlot_plot$data]]$obj
  unlist(lapply(plot$subplots, function(p) unlist(lapply(ggplot2::ggplot_build(p)$data, function(layer) layer$label))))
}

.gaugeTotalRow <- function(results) {
  row <- results$results$gaugeANOVA$collection$gaugeANOVA_RRtable2$data[[1]]
  c(row$percentStudyVar, row$percentTolerance)
}

test_that("Traffic light chart matches the model without interaction (alpha 0.05)", {
  results <- .trafficLightRun(0.05)
  expect_equal(.trafficLightLabels(results), c("27.86%", "22.68%"))
  expect_equal(.gaugeTotalRow(results), c(27.86, 22.68))
})

test_that("Traffic light chart matches the model with interaction (alpha 0.99)", {
  results <- .trafficLightRun(0.99)
  expect_equal(.trafficLightLabels(results), c("28.75%", "23.49%"))
  expect_equal(.gaugeTotalRow(results), c(28.75, 23.49))
})
