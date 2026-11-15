context("Test-retest traffic light reference values")

# AIAG MSA range method example (5 parts, 2 appraisers: R-bar 0.07, GRR 0.0588, process SD 0.0777),
# scaled x1000 so the reference values fit the default DoubleField decimals.
.trtlData <- data.frame(part = 1:5, A = c(850, 750, 1000, 450, 500), B = c(800, 700, 950, 550, 600))
.trtlRun <- function(processSd, tolerance, processSdValue = 1, toleranceValue = 1) {
  options <- jaspTools::analysisOptions("msaTestRetest")
  options$dataFormat                           <- "wideFormat"
  options$partWideFormat                       <- "part"
  options$measurementsWideFormat               <- c("A", "B")
  options$repeatabilityAndReproducibilityTable <- FALSE
  options$runChartPart                         <- FALSE
  options$scatterPlotMeasurement               <- FALSE
  options$rChart                               <- FALSE
  options$trafficLightChart                    <- TRUE
  options$manualProcessSd                      <- processSd
  options$manualProcessSdValue                 <- processSdValue
  options$tolerance                            <- tolerance
  options$toleranceValue                       <- toleranceValue
  jaspTools::runAnalysis("msaTestRetest", .trtlData, options, view = FALSE)
}
.trtlPanels <- function(results) {
  plot <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_plot"]]
  obj  <- results[["state"]][["figures"]][[plot[["data"]]]][["obj"]]
  panels <- if (inherits(obj, "ggplot")) list(obj) else obj$subplots
  lapply(panels, function(p) list(
    xlab  = p$scales$get_scales("x")$name,
    vline = ggplot2::ggplot_build(p)$data[[2]]$xintercept
  ))
}

test_that("no reference value ticked: no traffic light, error asks for process SD or tolerance", {
  for (sdValue in c(1, 77.7)) { # hidden default and value typed before unticking
    results <- .trtlRun(FALSE, FALSE, processSdValue = sdValue)
    plot <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_plot"]]
    expect_identical(plot[["status"]], "error")
    expect_match(plot[["error"]][["errorMessage"]], "process std. dev. or a tolerance", fixed = TRUE)
  }
})

test_that("only process SD ticked: single study-variation panel with AIAG %GRR", {
  panels <- .trtlPanels(.trtlRun(TRUE, FALSE, processSdValue = 77.7))
  expect_length(panels, 1L)
  expect_identical(panels[[1]]$xlab, "Percent study variation of GRR")
  expect_equal(panels[[1]]$vline, 75.639, tolerance = 1e-4)
})

test_that("only tolerance ticked: single tolerance panel, no hidden process SD", {
  panels <- .trtlPanels(.trtlRun(FALSE, TRUE, toleranceValue = 500))
  expect_length(panels, 1L)
  expect_identical(panels[[1]]$xlab, "Percent tolerance of GRR")
  expect_equal(panels[[1]]$vline, 70.526, tolerance = 1e-4)
})

test_that("both ticked: study-variation and tolerance panels", {
  panels <- .trtlPanels(.trtlRun(TRUE, TRUE, processSdValue = 77.7, toleranceValue = 500))
  expect_length(panels, 2L)
  expect_equal(vapply(panels, function(p) p$vline, numeric(1)), c(75.639, 70.526), tolerance = 1e-4)
})
