context("[Quality Control] Control charts with a stage without observations")

# jasp-stats/jasp-issues#4698: a stage whose rows are all missing has no mean or std. dev.; its limit labels read
# CL = NaN, LCL = NA, UCL = NA. It should get no limit labels or lines, and a note naming the stage.
# Stage A: subgroups (1, 2), (3, 4), (2, 3), (5, 6): mean 3.25, every range 1, every std. dev. sqrt(1/2).

.emptyStageEnd <- data.frame(First  = c(1, 3, 2, 5, NA, NA),
                             Second = c(2, 4, 3, 6, NA, NA),
                             Stage  = c("A", "A", "A", "A", "B", "B"))
.emptyStageMiddle <- data.frame(First  = c(1, 3, 2, 5, NA, NA, 4, 2, 6),
                                Second = c(2, 4, 3, 6, NA, NA, 5, 5, 3),
                                Stage  = c("A", "A", "A", "A", "B", "B", "C", "C", "C"))

# independent limits of stage A (subgroup size 2: d2 = 2 / sqrt(pi), c4 = sqrt(2 / pi), B4 = 3.267, D4 = 3.267)
.emptyStageLimitsA <- function(chart) {
  sBar <- sqrt(1 / 2)
  c4   <- sqrt(2 / pi)
  d2   <- 2 / sqrt(pi)
  switch(chart,
         xBarS = 3.25 + c(0, -3, 3) * sBar / c4 / sqrt(2),
         xBarR = 3.25 + c(0, -3, 3) * 1 / d2 / sqrt(2),
         s     = c(sBar, 0, sBar + 3 * sBar / c4 * sqrt(1 - c4^2)),
         R     = c(1, 0, 1 + 3 * 1 / d2 * 0.8525))
}

.emptyStageCharts <- function(results, plotKey) {
  plotObj <- results[["state"]][["figures"]][[plotKey]][["obj"]]
  lapply(plotObj$subplots, function(p) {
    built   <- ggplot2::ggplot_build(p)
    isLabel <- vapply(p$layers, function(l) inherits(l$geom, "GeomLabel"), logical(1))
    isStep  <- vapply(p$layers, function(l) inherits(l$geom, "GeomStep"), logical(1))
    labels  <- do.call(rbind, lapply(built$data[isLabel], function(d) d[c("x", "y", "label")]))
    steps   <- do.call(rbind, lapply(built$data[isStep], function(d) d[c("x", "y")]))
    list(labels = labels, steps = steps[!is.na(steps$y), ])
  })
}

.expectNoEmptyStageLimits <- function(chart, from, to) {
  expect_false(any(grepl("NaN|NA", chart$labels$label)))
  expect_false(any(is.na(chart$labels$y)))
  # no label and no drawn line between the boundaries of the empty stage
  expect_false(any(chart$labels$x > from & chart$labels$x < to))
  expect_false(any(chart$steps$x > from & chart$steps$x < to))
}

.emptyStageOptions <- function(chartType) {
  options <- analysisOptions("variablesChartsSubgroups")
  options$dataFormat             <- "wideFormat"
  options$measurementsWideFormat <- c("First", "Second")
  options$stagesWideFormat       <- "Stage"
  options$chartType              <- chartType
  options
}

test_that("Variables charts for subgroups: last stage without observations has no limits", {
  for (chartType in c("xBarAndS", "xBarAndR")) {
    results <- runAnalysis("variablesChartsSubgroups", .emptyStageEnd, .emptyStageOptions(chartType))
    charts  <- results[["results"]][["controlCharts"]][["collection"]]
    plots   <- .emptyStageCharts(results, charts[["controlCharts_plot"]][["data"]])
    # subplots: X-bar chart, then second chart (s or R)
    expected <- list(.emptyStageLimitsA(if (chartType == "xBarAndS") "xBarS" else "xBarR"),
                     .emptyStageLimitsA(if (chartType == "xBarAndS") "s" else "R"))
    for (k in 1:2) {
      .expectNoEmptyStageLimits(plots[[k]], from = 4.5, to = Inf)
      # stage A keeps its labels
      expect_equal(plots[[k]]$labels$x, rep(4.5, 3))
      expect_equal(plots[[k]]$labels$y, expected[[k]], tolerance = 1e-3)
    }
    expect_true(grepl("Stage B has no observations", charts[["controlCharts_plotNote"]][["text"]], fixed = TRUE))
  }
})

test_that("Variables charts for subgroups: stage without observations between two stages has no limits", {
  results <- runAnalysis("variablesChartsSubgroups", .emptyStageMiddle, .emptyStageOptions("xBarAndS"))
  charts  <- results[["results"]][["controlCharts"]][["collection"]]
  plots   <- .emptyStageCharts(results, charts[["controlCharts_plot"]][["data"]])
  for (chart in plots) {
    .expectNoEmptyStageLimits(chart, from = 4.5, to = 6.5)
    expect_equal(sum(chart$labels$x == 4.5), 3)                 # stage A
    expect_equal(sum(startsWith(chart$labels$label, "CL = ")), 1) # stage C (last stage)
  }
  # stage C: mean of (4, 5), (2, 5), (6, 3)
  expect_true(any(abs(plots[[1]]$labels$y - 25 / 6) < 1e-10))
  expect_true(grepl("Stage B has no observations", charts[["controlCharts_plotNote"]][["text"]], fixed = TRUE))
})

test_that("The empty-stage note identifies the occurrence of a repeated stage", {
  data <- transform(.emptyStageMiddle, Stage = c("A", "A", "B", "B", "A", "A", "C", "C", "C"))
  results <- runAnalysis("variablesChartsSubgroups", data, .emptyStageOptions("xBarAndS"))
  charts  <- results[["results"]][["controlCharts"]][["collection"]]
  expect_match(charts[["controlCharts_plotNote"]][["text"]],
               "Stage A (occurrence 2) has no observations", fixed = TRUE)
})

test_that("Variables charts for subgroups: known parameters draw no center line in a stage without observations", {
  options <- .emptyStageOptions("xBarAndS")
  options$knownParameters     <- TRUE
  options$knownParametersMean <- 3
  options$knownParametersSd   <- 1
  results <- runAnalysis("variablesChartsSubgroups", .emptyStageMiddle, options)
  charts  <- results[["results"]][["controlCharts"]][["collection"]]
  for (chart in .emptyStageCharts(results, charts[["controlCharts_plot"]][["data"]]))
    .expectNoEmptyStageLimits(chart, from = 4.5, to = 6.5)
})

test_that("Process capability study: control chart stage without observations has no limits", {
  options <- analysisOptions("processCapabilityStudies")
  options$dataFormat                   <- "wideFormat"
  options$measurementsWideFormat       <- c("First", "Second")
  options$stagesWideFormat             <- "Stage"
  options$capabilityStudyType          <- "normalCapabilityAnalysis"
  options$controlChartType             <- "xBarS"
  options$lowerSpecificationLimit      <- TRUE
  options$upperSpecificationLimit      <- TRUE
  options$lowerSpecificationLimitValue <- 0
  options$upperSpecificationLimitValue <- 8
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", .emptyStageEnd, options)
  xBar    <- results[["results"]][["xBar"]][["collection"]]
  plots   <- .emptyStageCharts(results, xBar[["xBar_plot"]][["data"]])
  expected <- list(.emptyStageLimitsA("xBarS"), .emptyStageLimitsA("s"))
  for (k in 1:2) {
    .expectNoEmptyStageLimits(plots[[k]], from = 4.5, to = Inf)
    expect_equal(plots[[k]]$labels$y, expected[[k]], tolerance = 1e-3)
  }
  expect_true(grepl("Stage B has no observations", xBar[["xBar_plotNote"]][["text"]], fixed = TRUE))
})
