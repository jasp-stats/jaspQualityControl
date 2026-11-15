context("[Quality Control] Attributes charts: y-axis covers control limits and data")

# 30 subgroups of n = 1000 with about 100 defectives; subgroup 10 has 65 (below the LCL)
.yLimCounts <- local({
  others <- 101 + rep(c(4, -3, 2, -5, 3, -1, 0, 1, -2), length.out = 29)
  others[29] <- others[29] + (2936 - sum(others))
  append(others, 65, after = 9)
})

.yLimRun <- function(family, chart = NULL) {
  options <- jaspTools::analysisOptions("attributesCharts")
  options$defectiveOrDefect <- "count"
  options$total <- "size"
  options$attributesChart <- family
  if (family == "defectives")
    options$attributesChartDefectivesChartType <- chart
  if (family == "defects")
    options$attributesChartDefectsChartType <- chart
  jaspTools::runAnalysis("attributesCharts", data.frame(count = .yLimCounts, size = 1000), options, view = FALSE)
}

.yLimPlot <- function(results, key) {
  results$state$figures[[results$results[[key]]$data]]$obj
}

# every layer keeps all its y values (none removed for being outside the axis limits),
# and the axis covers the control limits and all data
.yLimExpectAllDrawn <- function(plot, limits, values, label) {
  built <- ggplot2::ggplot_build(plot)
  for (d in built$data) {
    y <- if ("yintercept" %in% names(d)) d$yintercept else d$y
    if (!is.null(y))
      expect_false(anyNA(y), info = label)
  }
  yRange <- built$layout$panel_scales_y[[1]]$limits
  expect_lte(yRange[1], min(limits, values), label = label)
  expect_gte(yRange[2], max(limits, values), label = label)
}

test_that("np chart draws the LCL and the point below it", {
  results <- .yLimRun("defectives", "npChart")
  expect_identical(results$status, "complete")
  plot <- .yLimPlot(results, "NPchartPlot")
  expect_equal(mean(.yLimCounts), 100.0333, tolerance = 1e-4)
  lcl <- 100.0333 - 3 * sqrt(100.0333 * (1 - 0.1000333))
  ucl <- 100.0333 + 3 * sqrt(100.0333 * (1 - 0.1000333))
  expect_equal(c(lcl, ucl), c(71.57, 128.50), tolerance = 1e-4)
  .yLimExpectAllDrawn(plot, c(lcl, ucl), .yLimCounts, "np")
})

test_that("c chart draws the LCL and the point below it", {
  results <- .yLimRun("defects", "cChart")
  expect_identical(results$status, "complete")
  plot <- .yLimPlot(results, "CchartPlot")
  lcl <- 100.0333 - 3 * sqrt(100.0333)
  ucl <- 100.0333 + 3 * sqrt(100.0333)
  .yLimExpectAllDrawn(plot, c(lcl, ucl), .yLimCounts, "c")
})

test_that("attributes X-mR chart draws its limits and all points", {
  results <- .yLimRun("xmr")
  expect_identical(results$status, "complete")
  subplots <- .yLimPlot(results, "IPlotA")$subplots
  p <- .yLimCounts / 1000
  mr <- abs(diff(p))
  xLimits <- mean(p) + c(-3, 3) * mean(mr) / 1.128
  mrUCL <- 3.267 * mean(mr)
  .yLimExpectAllDrawn(subplots[[1]], xLimits, p, "X")
  .yLimExpectAllDrawn(subplots[[2]], c(0, mrUCL), mr, "mR")
})
