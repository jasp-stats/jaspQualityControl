context("[Quality Control] Recurring stage plot regressions")

set.seed(592)
plotStageData <- data.frame(
  y = rnorm(60, 10, 1),
  stage = rep(c("A", "B", "A"), each = 20),
  timestamp = c(paste0("t", 1:40), rep(NA_character_, 20))
)

test_that("Stage validation uses available measurements, independent of timestamps", {
  hasEmptyStage <- jaspQualityControl:::.qcAnyStageWithoutMeasurements
  expect_false(hasEmptyStage(plotStageData, "stage", "y"))
  partial <- plotStageData
  partial$y2 <- partial$y
  partial$y2[41:60] <- NA_real_
  expect_false(hasEmptyStage(partial, "stage", c("y", "y2")))
  partial$y[41:60] <- NA_real_
  expect_true(hasEmptyStage(partial, "stage", c("y", "y2")))
})

plotStageOptions <- jaspTools::analysisOptions("processCapabilityStudies")
plotStageOptions$dataFormat <- "wideFormat"
plotStageOptions$measurementsWideFormat <- "y"
plotStageOptions$stagesWideFormat <- "stage"
plotStageOptions$axisLabels <- "timestamp"
plotStageOptions$controlChart <- FALSE
plotStageOptions$lowerSpecificationLimit <- TRUE
plotStageOptions$lowerSpecificationLimitValue <- 7
plotStageOptions$upperSpecificationLimit <- TRUE
plotStageOptions$upperSpecificationLimitValue <- 13
plotStageOptions$processCapabilityTableCi <- FALSE
plotStageOptions$histogram <- TRUE
plotStageOptions$probabilityPlot <- TRUE
plotStageOptions$processCapabilityPlot <- TRUE
plotStageResults <- jaspTools::runAnalysis("processCapabilityStudies", plotStageData, plotStageOptions, view = FALSE)

plotStageOutputs <- list(
  histogram = plotStageResults$results$histogram,
  probability = plotStageResults$results$probabilityContainer$collection$probabilityContainer_ProbabilityPlot,
  capability = plotStageResults$results$capabilityAnalysis$collection$capabilityAnalysis_normalCapabilityAnalysis$collection$capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot
)

test_that("Missing timestamps do not suppress recurring-stage plots", {
  expect_identical(plotStageResults$status, "complete")
  for (output in plotStageOutputs) {
    expect_false(is.null(output))
    expect_null(output[["error"]]) # not output$error: it partial-matches the field errorType ("validationError" of non-editable matrix plots)
    expect_true(nzchar(output$data))
  }
})

test_that("Odd-stage histogram and probability plots convert to interactive plots", {
  for (name in c("histogram", "probability")) {
    plot <- plotStageResults$state$figures[[plotStageOutputs[[name]]$data]]$obj
    converted <- jaspGraphs::convertGgplotToPlotly(plot)
    expect_false(inherits(converted, "try-error"), info = name)
    expect_true(nzchar(converted), info = name)
  }
})
