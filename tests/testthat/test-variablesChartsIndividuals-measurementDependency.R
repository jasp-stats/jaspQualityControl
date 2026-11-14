context("[Quality Control] Variables Charts for Individuals - Measurement dependency")

# jasp-stats/INTERNAL-jasp#3411: X-mR container and report did not depend on "measurement",
# so changing the Measurement variable kept the previous variable's chart and tables.
# jaspTools runs are stateless (no edited-analysis rerun), so the dependency vectors are checked directly.

test_that("X-mR chart and report depend on the Measurement variable", {
  xmrDeps    <- jaspQualityControl:::.variablesChartsIndividualsXmrDependencies()
  reportDeps <- jaspQualityControl:::.variablesChartsIndividualsReportDependencies()
  expect_true("measurement" %in% xmrDeps)
  expect_true("measurement" %in% reportDeps)
  expect_true("report" %in% reportDeps)
  expect_true("autocorrelationPlotCiLevel" %in% reportDeps)
})

# Two variables on different scales; a fresh y2 analysis gives y2's own limits and signals
dataset <- data.frame(
  y1 = c(10.74, 9.8, 10.57, 10.7, 10.36, 10.67, 10.9, 9.96, 9.55, 10.78, 9.11, 10.11, 10.11, 9.72, 9.86,
         12.52, 9.82, 9.55, 10.22, 10.53, 8.94, 9.39, 9.34, 10.89, 10.67, 9.14, 11.39, 9.62, 11.62, 10.39),
  y2 = c(22.17, 25.4, 20.96, 25.15, 24.08, 25.36, 27.15, 23.69, 23.19, 22.6, 26.9, 25.31, 23.07, 22.45, 26.02,
         24.64, 31.5, 25.99, 25.06, 24.68, 25.14, 24.67, 23.38, 26.04, 24.23, 25.3, 24.2, 24.29, 25.53, 25.97)
)

.individualsLimits <- function(results) {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
  matrixPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  lapply(matrixPlot$subplots, function(subplot) {
    layers <- ggplot2::ggplot_build(subplot)$data
    labelLayer <- layers[[which(vapply(layers, function(l) "label" %in% names(l), logical(1)))[1]]]
    setNames(labelLayer$y, sub(" =.*", "", labelLayer$label))
  })
}

options <- analysisOptions("variablesChartsIndividuals")
options$testSet <- "jaspDefault"
options$xmrChart <- TRUE
options$xmrChartMovingRangeLength <- 2
options$controlLimitsNumberOfSigmas <- 3

test_that("Fresh y2 analysis gives y2's X-mR limits and test results", {
  options$measurement <- "y2"
  results <- runAnalysis("variablesChartsIndividuals", dataset, options)
  expect_equal(results[["status"]], "complete")

  limits <- .individualsLimits(results)
  iLimits  <- limits[[which(vapply(limits, function(l) l[["CL"]] > 20, logical(1)))]]
  mrLimits <- limits[[which(vapply(limits, function(l) l[["CL"]] < 20, logical(1)))]]
  # CL = mean, limits = CL +/- 3 * MRbar / 1.128, MR UCL = 3.267 * MRbar
  expect_equal(unname(iLimits[c("CL", "UCL", "LCL")]), c(24.804, 30.17451, 19.43349), tolerance = 1e-5)
  expect_equal(unname(mrLimits[c("CL", "UCL", "LCL")]), c(2.01931, 6.597667, 0), tolerance = 1e-5)

  tableI  <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  tableMR <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(tableI, list("Point 17"))
  jaspTools::expect_equal_tables(tableMR, list("Point 17"))
})
