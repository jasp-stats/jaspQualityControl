context("[Quality Control] MSA - Gauge Linearity")
set.seed(1)

# Basic test  (verified with other software; Total p-value changed to pooled within-part SD, see test-msaGaugeLinearity-totalBias.R) ####

options <- analysisOptions("msaGaugeLinearity")
options$part <- "Part"
options$measurement <- "Measurement"
options$standard <- "Reference"
options$manualProcessVariation <- TRUE
options$manualProcessVariationValue <- 1
options$percentProcessVariationPlot <- TRUE
set.seed(1)
results <- runAnalysis("msaGaugeLinearity", "datasets/msaLinearityStudy/msaLinearity.csv", options)


test_that("1.1 Basic test - Bias and linearity plot matches", {
  plotName <- results[["results"]][["LB"]][["collection"]][["LB_plotBias"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "1_bias-and-linearity")
})

test_that("1.2 Basic test - Percentage process variation graph plot matches", {
  plotName <- results[["results"]][["LB"]][["collection"]][["LB_plotProcessVar"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "1_percentage-process-variation-graph")
})

test_that("1.3 Basic test - Gauge bias table results match", {
  table <- results[["results"]][["LB"]][["collection"]][["LB_tableGaugeBias"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.491666666666667, 2.49166666666667, 1, 49.1666666666667, 2.87233310444284e-08,
                                      2, 0.125, 4.125, 2, 12.5, 0.353991325267683, 4, 0.0250000000000004,
                                      6.025, 3, 2.50000000000004, 0.667130710762814, 6, -0.291666666666667,
                                      7.70833333333333, 4, 29.1666666666667, 6.41948050554358e-07,
                                      8, -0.616666666666667, 9.38333333333333, 5, 61.6666666666667,
                                      1.55444480038029e-08, 10, -0.0533333333333334, "Total", 5.33333333333333,
                                      0.0894220227213575))
})

test_that("1.4 Basic test - Regression model table results match", {
  table <- results[["results"]][["LB"]][["collection"]][["LB_tableRegression"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0725242725916125, 10.1575188601321, 0.736666666666666, "Intercept",
                                      1.73379959546227e-14, 0.0109334454718107, -12.0425594114992,
                                      -0.131666666666667, "Slope", 2.03771558228573e-17))
})

test_that("1.5 Basic test - Gauge linearity table results match", {
  table <- results[["results"]][["LB"]][["collection"]][["LB_tableGaugeLinearity"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.239539788646537, 0.131666666666667, 13.1666666666667, 0.714318415932242
                                 ))
})

test_that("1.6 Basic test - Regression equation table results match", {
  table <- results[["results"]][["LB"]][["collection"]][["LB_tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Bias = 0.74 - 0.13 * Reference value"))
})

# Missing values test (verified with other software, except the Total p-value: pooled within-part SD, checked analytically in test-msaGaugeLinearity-totalBias.R) ####

options <- analysisOptions("msaGaugeLinearity")
options$part <- "PartMissing5"
options$measurement <- "MeasurementMissing5"
options$standard <- "ReferenceMissing5"
options$manualProcessVariation <- TRUE
options$manualProcessVariationValue <- 1
options$percentProcessVariationPlot <- TRUE
set.seed(1)
results <- runAnalysis("msaGaugeLinearity", "datasets/msaLinearityStudy/msaLinearity.csv", options)


test_that("2.1 Missing values test - Bias and linearity plot matches", {
  plotName <- results[["results"]][["LB"]][["collection"]][["LB_plotBias"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "2_bias-and-linearity")
})

test_that("2.2 Missing values test - Percentage process variation graph plot matches", {
  plotName <- results[["results"]][["LB"]][["collection"]][["LB_plotProcessVar"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "2_percentage-process-variation-graph")
})

test_that("2.3 Missing values test - Gauge bias table results match", {
  table <- results[["results"]][["LB"]][["collection"]][["LB_tableGaugeBias"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.475, 2.475, 1, 47.5, 4.1784708324642e-05, 2, 0.225, 4.225, 2,
                                      22.5, 0.267640820562279, 4, -0.0222222222222221, 5.97777777777778,
                                      3, 2.22222222222221, 0.71883629907514, 6, -0.281818181818182,
                                      7.71818181818182, 4, 28.1818181818182, 2.48662323102535e-06,
                                      8, -0.6, 9.4, 5, 60, 4.65540714815082e-07, 10, -0.0804347826086957,
                                      "Total", 8.04347826086957, 0.0382677422161326))
})

test_that("2.4 Missing values test - Regression model table results match", {
  table <- results[["results"]][["LB"]][["collection"]][["LB_tableRegression"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0897638791625512, 8.45044628565873, 0.7585448392555, "Intercept",
                                      9.18217700323426e-11, 0.0130153805110511, -10.2247895183502,
                                      -0.133079526226734, "Slope", 3.35065357366737e-13))
})

test_that("2.5 Missing values test - Gauge linearity table results match", {
  table <- results[["results"]][["LB"]][["collection"]][["LB_tableGaugeLinearity"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.246859991507049, 0.133079526226734, 13.3079526226734, 0.703796096771246
                                 ))
})

test_that("2.6 Missing values test - Regression equation table results match", {
  table <- results[["results"]][["LB"]][["collection"]][["LB_tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Bias = 0.76 - 0.13 * Reference value"))
})

# Bias plot axis limits (jasp-stats/INTERNAL-jasp#3435) ####
# The y limits must never drop data (the fitted line is refitted to what remains) and must contain the zero line.

.linBiasPlotRun <- function(shift) {
  dataset <- read.csv(testthat::test_path("datasets", "msaLinearityStudy", "msaLinearity.csv"))[c("Part", "Reference", "Measurement")]
  dataset[["Part"]] <- factor(dataset[["Part"]])
  dataset[["Measurement"]] <- dataset[["Measurement"]] + shift
  options <- analysisOptions("msaGaugeLinearity")
  options$part <- "Part"
  options$measurement <- "Measurement"
  options$standard <- "Reference"
  options$linearityAndBiasPlot <- TRUE
  options$manualProcessVariation <- FALSE
  options$percentProcessVariationPlot <- FALSE
  set.seed(1)
  results <- runAnalysis("msaGaugeLinearity", dataset, options, view = FALSE)
  plotName <- results[["results"]][["LB"]][["collection"]][["LB_plotBias"]][["data"]]
  built <- ggplot2::ggplot_build(results[["state"]][["figures"]][[plotName]][["obj"]])
  smoothData <- built$data[[2]]
  list(biases = dataset[["Measurement"]] - dataset[["Reference"]],
       limits = built$layout$panel_scales_y[[1]]$get_limits(),
       zeroLine = built$data[[1]][["yintercept"]], # NA when the zero line is outside the limits
       rawPoints = built$data[[3]][["y"]],         # NA when a point is outside the limits
       meanPoints = built$data[[4]][["y"]],
       drawnSlope = unname(coef(lm(y ~ x, smoothData))[2]),
       tableSlope = results[["results"]][["LB"]][["collection"]][["LB_tableRegression"]][["data"]][[2]][["coefficient"]])
}

test_that("3.1 Bias plot shows all data, the zero line and the table's regression line for all-negative, all-positive and mixed biases", {
  for (shift in c(-2, 5, 0)) {
    run <- .linBiasPlotRun(shift)
    label <- paste("shift", shift)
    expect_true(all(is.finite(run$rawPoints)), label = paste(label, "raw biases drawn"))
    expect_length(run$rawPoints, 60)
    expect_true(all(is.finite(run$meanPoints)), label = paste(label, "mean biases drawn"))
    expect_length(run$meanPoints, 5)
    expect_true(is.finite(run$zeroLine), label = paste(label, "zero line drawn"))
    expect_lte(run$limits[1], min(run$biases, 0))
    expect_gte(run$limits[2], max(run$biases, 0))
    expect_equal(run$drawnSlope, run$tableSlope, tolerance = 1e-6, label = paste(label, "drawn slope"))
  }
})

test_that("3.2 Bias plot keeps its limits (min bias to 2 x max bias) when the biases straddle zero", {
  run <- .linBiasPlotRun(0)
  expect_equal(run$limits, c(min(run$biases), max(run$biases) * 2))
})
