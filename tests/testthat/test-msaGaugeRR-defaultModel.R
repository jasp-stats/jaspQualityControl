context("[Quality Control] Gauge r&R - default F-test model")

# AIAG MSA (4th ed.) crossed study: 10 parts x 3 operators x 3 trials (Minitab gageaiag)
aiagMeasurements <- c(
  0.29, -0.56, 1.34, 0.47, -0.80,  0.02, 0.59, -0.31, 2.26, -1.36,
  0.41, -0.68, 1.17, 0.50, -0.92, -0.11, 0.75, -0.20, 1.99, -1.25,
  0.64, -0.58, 1.27, 0.64, -0.84, -0.21, 0.66, -0.17, 2.01, -1.31,
  0.08, -0.47, 1.19, 0.01, -0.56, -0.20, 0.47, -0.63, 1.80, -1.68,
  0.25, -1.22, 0.94, 1.03, -1.20,  0.22, 0.55,  0.08, 2.12, -1.62,
  0.07, -0.68, 1.34, 0.20, -1.28,  0.06, 0.83, -0.34, 2.19, -1.50,
  0.04, -1.38, 0.88, 0.14, -1.46, -0.29, 0.02, -0.46, 1.77, -1.49,
 -0.11, -1.13, 1.09, 0.20, -1.07, -0.67, 0.01, -0.56, 1.45, -1.77,
 -0.15, -0.96, 0.67, 0.11, -1.45, -0.49, 0.21, -0.49, 1.87, -2.16)
aiagData <- data.frame(Part     = rep(1:10, 9),
                       Operator = rep(c("A", "B", "C"), each = 30),
                       Dm       = aiagMeasurements)

options <- analysisOptions("msaGaugeRR")
options$operatorLongFormat    <- "Operator"
options$partLongFormat        <- "Part"
options$measurementLongFormat <- "Dm"
options$tolerance             <- TRUE
options$toleranceValue        <- 8
# anovaModelType deliberately not set: the QML default is tested
set.seed(1)
results <- runAnalysis("msaGaugeRR", aiagData, options)

test_that("Default F-test model is random effects", {
  expect_identical(options$anovaModelType, "randomEffect")
})

test_that("Default ANOVA with interaction matches AIAG/Minitab F-tests", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  # Minitab: Part F 492.291, Operator F 79.406, Part * Operator F 0.434
  expect_equal(table[[1]][["F value"]], 492.291, tolerance = 1e-5)
  expect_equal(table[[2]][["F value"]], 79.406,  tolerance = 1e-5)
  expect_equal(table[[3]][["F value"]], 0.434,   tolerance = 1e-2)
})
