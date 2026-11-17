context("[Quality Control] MSA Test Retest")

# Long Format ####

## Default Settings ####

options <- analysisOptions("msaTestRetest")
options$dataFormat <- "longFormat"
options$partLongFormat <- "Part"
options$measurementLongFormat <- "Measurement"
options$operator <- "Repetition"
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

results <- runAnalysis("msaTestRetest", "datasets/msaTestRetest/msaTestRetest_long.csv", options)

test_that("LF1.1 Default Settings - Run chart of parts plot matches", {
  plotName <- results[["results"]][["ScatterOperatorParts"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_run-chart-of-parts")
})

test_that("LF1.2 Default Settings - Scatterplot of 1st measurement vs 2nd measurement matches", {
  plotName <- results[["results"]][["ScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_scatterplot-of-1st-measurement-vs-2nd-measurement")
})

test_that("LF1.3 Default Settings - Short gauge study table results match", {
  table <- results[["results"]][["rAndR2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0434914974122558, 0.0217457487061279, 0.52189796894707, 2, 0.0499999999999999,
                                      1.14965, 15, 0.5))
})

test_that("LF1.4 Default Settings - Range chart by part plot matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_range-chart-by-part")
})

test_that("LF1.5 Default Settings - Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("LF1.6 Default Settings - Traffic chart matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "LF1_trafficChart")
})

# Wide Format ####

## Default Settings ####

options <- analysisOptions("msaTestRetest")
options$dataFormat <- "wideFormat"
options$partWideFormat <- "Part"
options$measurementsWideFormat <- c("Measurement1", "Measurement2")
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

results <- runAnalysis("msaTestRetest", "datasets/msaTestRetest/msaTestRetest_wide.csv", options)


test_that("WF1.1 Default Settings - Run chart of parts plot matches", {
  plotName <- results[["results"]][["ScatterOperatorParts"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_run-chart-of-parts")
})

test_that("WF1.2 Default Settings - Scatterplot of 1st measurement vs 2nd measurement matches", {
  plotName <- results[["results"]][["ScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_scatterplot-of-1st-measurement-vs-2nd-measurement")
})

test_that("WF1.3 Default Settings - Short gauge study table results match", {
  table <- results[["results"]][["rAndR2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0434914974122558, 0.0217457487061279, 0.52189796894707, 2, 0.0499999999999999,
                                      1.14965, 15, 0.5))
})

test_that("WF1.4 Default Settings - Range chart by part plot matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_range-chart-by-part")
})

test_that("WF1.5 Default Settings - Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("WF1.6 Default Settings - Traffic chart matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "WF1_traffiChart")
})


# Scatterplot axis limits with negative measurements (jasp-stats/INTERNAL-jasp#3436) ####
# The axis limits must never drop points (the fitted line is refitted to what remains).

.trScatterRun <- function(shift, allValues = FALSE) {
  dataset <- data.frame(Part = 1:5,
                        A = c(-10, -8, -6, -4, -2) + shift,
                        B = c(-9.5, -8.4, -6.2, -3.7, -2.2) + shift)
  options <- analysisOptions("msaTestRetest")
  options$dataFormat <- "wideFormat"
  options$partWideFormat <- "Part"
  options$measurementsWideFormat <- c("A", "B")
  options$repeatabilityAndReproducibilityTable <- FALSE
  options$runChartPart <- FALSE
  options$scatterPlotMeasurement <- TRUE
  options$scatterPlotMeasurementFitLine <- TRUE
  options$scatterPlotMeasurementAllValues <- allValues
  options$rChart <- FALSE
  options$trafficLightChart <- FALSE
  results <- runAnalysis("msaTestRetest", dataset, options, view = FALSE)
  plot <- results[["state"]][["figures"]][[results[["results"]][["ScatterOperators"]][["data"]]]][["obj"]]
  built <- ggplot2::ggplot_build(plot)
  smoothLayer <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomSmooth"), logical(1)))
  list(data = dataset,
       plot = plot,
       xLimits = built$layout$panel_scales_x[[1]]$get_limits(),
       yLimits = built$layout$panel_scales_y[[1]]$get_limits(),
       points = built$data[[1]], # x or y is NA when a point is outside the limits
       drawnSlope = unname(coef(lm(y ~ x, built$data[[smoothLayer]]))[2]))
}

test_that("WF2.1 Scatterplot shows all pairs and the lm line for negative, mixed and positive measurements", {
  for (shift in c(0, 6, 20)) {
    run <- .trScatterRun(shift)
    label <- paste("shift", shift)
    expect_equal(sum(is.finite(run$points[["x"]]) & is.finite(run$points[["y"]])), 5, label = paste(label, "points drawn"))
    allValues <- unlist(run$data[c("A", "B")])
    for (limits in list(run$xLimits, run$yLimits)) {
      expect_lte(limits[1], min(allValues))
      expect_gte(limits[2], max(allValues))
    }
    expect_equal(run$drawnSlope, unname(coef(lm(B ~ A, run$data))[2]), tolerance = 1e-6, label = paste(label, "drawn slope"))
  }
})

test_that("WF2.2 Scatterplot keeps its limits (0.9 x min to 1.1 x max) for non-negative measurements", {
  run <- .trScatterRun(20)
  allValues <- unlist(run$data[c("A", "B")])
  expect_equal(run$xLimits, c(min(allValues) * 0.9, max(allValues) * 1.1))
  expect_equal(run$yLimits, c(min(allValues) * 0.9, max(allValues) * 1.1))
})

test_that("WF2.3 Scatterplot with all measurements keeps every jittered copy inside the axis limits", {
  # shift 0: all negative, 6: mixed, 10: minimum exactly 0, 20: all positive
  for (shift in c(0, 6, 10, 20)) {
    run <- .trScatterRun(shift, allValues = TRUE)
    label <- paste("shift", shift)
    jitterLayer <- which(vapply(run$plot$layers, function(layer) inherits(layer$position, "PositionJitter"), logical(1)))
    expect_length(jitterLayer, 1)
    drawn <- vapply(1:20, function(seed) {
      set.seed(seed)
      jittered <- suppressWarnings(ggplot2::ggplot_build(run$plot))$data[[jitterLayer]]
      sum(is.finite(jittered[["x"]]) & is.finite(jittered[["y"]]))
    }, numeric(1))
    expect_equal(drawn, rep(5, 20), label = paste(label, "jittered copies drawn per seed"))
    expect_equal(run$drawnSlope, unname(coef(lm(B ~ A, run$data))[2]), tolerance = 1e-6, label = paste(label, "drawn slope"))
  }
})
