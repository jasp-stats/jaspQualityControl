context("[Quality Control] Control chart signal colours")

# Exercise the shared helpers from this checkout independently of unrelated analyses.
controlChartSource <- new.env(parent = globalenv())
sys.source(testthat::test_path("..", "..", "R", "commonQualityControl.R"), envir = controlChartSource)
controlChartSource$.numDecimals <- 2

test_that("Subgroup signals stay red below, at and above half of the observations", {
  for (chartType in c("xBar", "R", "s")) {
    for (signalCount in c(0L, 4L, 5L, 6L, 10L)) {
      signalRows <- seq_len(signalCount)
      if (chartType == "xBar") {
        subgroupMean <- rep(0, 10)
        subgroupMean[signalRows] <- 10
        dataset <- data.frame(a = subgroupMean - 0.5, b = subgroupMean + 0.5)
      } else {
        subgroupRange <- rep(1, 10)
        subgroupRange[signalRows] <- 10
        dataset <- data.frame(a = -subgroupRange / 2, b = subgroupRange / 2)
      }
      chart <- controlChartSource$.controlChart_calculations(
        dataset, plotType = chartType, ruleList = list(rule1 = list(enabled = TRUE)),
        phase2 = TRUE, phase2Mu = 0, phase2Sd = 1
      )
      expectedColours <- rep("blue", 10)
      expectedColours[signalRows] <- "red"
      expect_equal(chart$pointData$dotColor, expectedColours, info = paste(chartType, signalCount))
      expect_equal(as.integer(chart$violationTable[[1]]$test1), signalRows)
    }
  }
})

test_that("A sustained in-limit shift retains all rule-2 signal endpoints", {
  dataset <- data.frame(a = rep(0.5, 20), b = rep(1.5, 20))
  chart <- controlChartSource$.controlChart_calculations(
    dataset, plotType = "xBar",
    ruleList = list(rule1 = list(enabled = TRUE), rule2 = list(enabled = TRUE, k = 7)),
    phase2 = TRUE, phase2Mu = 0, phase2Sd = 1
  )
  expect_true(all(abs(chart$pointData$plotStatistic) < 3 / sqrt(2)))
  expect_equal(which(chart$pointData$dotColor == "red"), 7:20)
  expect_equal(as.integer(chart$violationTable[[1]]$test2), 7:20)
  expect_true(all(is.na(chart$violationTable[[1]]$test1)))
})

test_that("Signal colours remain correct within each stage and around missing subgroups", {
  subgroupMean <- c(rep(10, 4), rep(0, 6), rep(10, 6), rep(0, 4))
  dataset <- data.frame(a = subgroupMean - 0.5, b = subgroupMean + 0.5, stage = rep(c("A", "B"), each = 10))
  chart <- controlChartSource$.controlChart_calculations(
    dataset, plotType = "xBar", stages = "stage", ruleList = list(rule1 = list(enabled = TRUE)),
    phase2 = TRUE, phase2Mu = 0, phase2Sd = 1
  )
  expect_equal(which(chart$pointData$dotColor == "red"), c(1:4, 11:16))
  expect_equal(as.integer(chart$violationTable[[1]]$test1), 1:4)
  expect_equal(as.integer(chart$violationTable[[2]]$test1), 1:6)

  dataset <- data.frame(a = c(rep(9.5, 8), -0.5, -0.5), b = c(rep(10.5, 8), 0.5, 0.5))
  dataset[3, ] <- NA_real_
  chart <- controlChartSource$.controlChart_calculations(
    dataset, plotType = "xBar", ruleList = list(rule1 = list(enabled = TRUE)),
    phase2 = TRUE, phase2Mu = 0, phase2Sd = 1
  )
  expect_true(is.na(chart$pointData$plotStatistic[3]))
  expect_equal(which(chart$pointData$dotColor == "red"), c(1L, 2L, 4:8))
  expect_equal(as.integer(chart$violationTable[[1]]$test1), c(1L, 2L, 4:8))
})

test_that("The rendered point layer retains all five beyond-limit signals", {
  chart <- controlChartSource$.controlChart_calculations(
    data.frame(a = rep(10, 5), b = rep(12, 5)), plotType = "xBar",
    ruleList = list(rule1 = list(enabled = TRUE)), phase2 = TRUE, phase2Mu = 0, phase2Sd = 1
  )
  plot <- controlChartSource$.controlChart_plotting(
    chart$pointData, chart$clData, chart$stageLabels, chart$clLabels, plotType = "xBar", phase2 = TRUE
  )
  pointLayer <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))
  expect_length(pointLayer, 1)
  expect_equal(ggplot2::ggplot_build(plot)$data[[pointLayer]]$fill, rep("red", 5))
  expect_equal(as.integer(chart$violationTable[[1]]$test1), 1:5)
})
