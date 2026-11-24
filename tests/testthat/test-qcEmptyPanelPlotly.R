context("[Quality Control] Interactive version of matrix plots with blank panels")

# Empty panels in matrix plots need a (blank) layer: without one, plotly drops the panel and the
# interactive version of the whole plot fails with "The length of the widths argument must be equal
# to the number of columns".

expectPlotlyConversion <- function(results, plotName) {
  plot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  converted <- jaspGraphs::convertGgplotToPlotly(plot)
  expect_false(jaspBase::isTryError(converted), info = if (jaspBase::isTryError(converted)) as.character(converted))
}

test_that("A staged non-normal capability report has an interactive version", {
  set.seed(1)
  data <- data.frame(x = rnorm(30, 10), stage = rep(c("a", "b", "c"), each = 10))
  options <- analysisOptions("processCapabilityStudies")
  options$probabilityPlotRankMethod <- "bernard"
  options$measurementLongFormat <- "x"
  options$manualSubgroupSizeValue <- 1
  options$stagesLongFormat <- "stage"
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- 7
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 13
  options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
  options$nonNormalDistribution <- "weibull"
  options$report <- TRUE
  results <- runAnalysis("processCapabilityStudies", data, options, view = FALSE)
  expectPlotlyConversion(results, results[["results"]][["report"]][["data"]])
})

test_that("The Gauge r&R matrix plot for three operators has an interactive version", {
  options <- analysisOptions("msaGaugeRR")
  options$operatorLongFormat <- "Operators"
  options$partLongFormat <- "Parts"
  options$measurementLongFormat <- "Dm"
  options$scatterPlot <- TRUE
  set.seed(1)
  results <- runAnalysis("msaGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options, view = FALSE)
  expectPlotlyConversion(results, results[["results"]][["gaugeScatterOperators"]][["data"]])
})
