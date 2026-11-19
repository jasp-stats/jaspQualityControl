context("[Quality Control] R and s charts with subgroups of one observation")

# jasp-stats/jasp-issues#4697: a subgroup with one observation has no range or standard deviation. In the R and s charts
# it is treated like an empty subgroup: no point, and the centre line and limits of the previous subgroup.
# Rows 2 and 5 have one observation; rows 1, 3 and 4 have a range of 1 and a standard deviation of 0.5.

.sizeOneData <- data.frame(First  = c(1, 3, 2,   4,   NA),
                           Second = c(2, NA, 3,  5,   6),
                           Third  = c(1.5, NA, 2.5, 4.5, NA))

# Independent references (subgroup size 3): sigma = R-bar / d2 or S-bar / c4, limits as in Montgomery (2009), ch. 6
.sizeOneD2 <- 1.693
.sizeOneD3 <- 0.8884
.sizeOneC4 <- sqrt(2 / 2) * gamma(3 / 2) / gamma(1)
.sizeOneC5 <- sqrt(1 - .sizeOneC4^2)
.sizeOneExpected <- list(
  R = list(lines  = c(center = 1,   UCL = 1 + 3 * .sizeOneD3 / .sizeOneD2,         LCL = 0),
           labels = c("CL = 1",   "LCL = 0", "UCL = 2.574")),
  s = list(lines  = c(center = 0.5, UCL = 0.5 + 3 * .sizeOneC5 * 0.5 / .sizeOneC4, LCL = 0),
           labels = c("CL = 0.5", "LCL = 0", "UCL = 1.284"))
)

# R or s chart (second subplot) of a "X-bar & R/s" plot: points, the step lines (centre, UCL, LCL) and all labels
.sizeOneChartData <- function(results, container, plotName) {
  plotKey <- results[["results"]][[container]][["collection"]][[plotName]][["data"]]
  plotObj <- results[["state"]][["figures"]][[plotKey]][["obj"]]$subplots[[2]]
  built   <- ggplot2::ggplot_build(plotObj)
  isPoint <- vapply(plotObj$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))
  isStep  <- vapply(plotObj$layers, function(l) inherits(l$geom, "GeomStep"), logical(1))
  steps   <- built$data[isStep]
  list(points = built$data[[which(isPoint)[1]]],
       x      = steps[[1]]$x,
       center = steps[[1]]$y, UCL = steps[[2]]$y, LCL = steps[[3]]$y,
       labels = as.character(unlist(lapply(built$data, function(d) d[["label"]]))))
}

.sizeOneExpectations <- function(chart, expected) {
  # rows 2 and 5 have no point; rows 1, 3 and 4 show their range / std. dev.
  points <- chart$points[order(chart$points$x), ]
  expect_true(all(is.na(points$y[points$x %in% c(2, 5)])))
  expect_equal(points$y[points$x %in% c(1, 3, 4)], rep(points$y[points$x == 1], 3))

  # the step lines start at x = 0.5 (row 1) and get one extra step at x = 5.5 (copy of row 5)
  expect_equal(chart$x, c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5))
  for (line in c("center", "UCL", "LCL")) {
    y <- chart[[line]]
    expect_false(anyNA(y), info = line)
    expect_equal(y[2], y[1], info = line)  # row 2 keeps the line of row 1
    expect_equal(y[5], y[4], info = line)  # row 5 keeps the line of row 4
    expect_equal(y, rep(expected$lines[[line]], 6), tolerance = 1e-3, info = line)
  }

  expect_false(any(grepl("NA|NaN", chart$labels)))
  expect_false("CL = 0" %in% chart$labels)
  expect_true(all(expected$labels %in% chart$labels))
}

.sizeOneOptions <- function(analysis) {
  options <- analysisOptions(analysis)
  options$dataFormat             <- "wideFormat"
  options$measurementsWideFormat <- c("First", "Second", "Third")
  options
}

for (chartType in c("xBarAndR", "xBarAndS")) {
  for (unbiased in if (chartType == "xBarAndS") c(TRUE, FALSE) else TRUE) {
    test_that(sprintf("Variables charts for subgroups, %s (unbiasing constant %s): subgroups of one observation keep the previous limits",
                      chartType, unbiased), {
      options <- .sizeOneOptions("variablesChartsSubgroups")
      options$chartType                 <- chartType
      options$xBarAndSUnbiasingConstant <- unbiased
      results <- runAnalysis("variablesChartsSubgroups", .sizeOneData, options)
      chart <- .sizeOneChartData(results, "controlCharts", "controlCharts_plot")
      .sizeOneExpectations(chart, .sizeOneExpected[[if (chartType == "xBarAndR") "R" else "s"]])
    })
  }
}

for (chartType in c("xBarR", "xBarS")) {
  test_that(sprintf("Process capability study, %s chart: subgroups of one observation keep the previous limits", chartType), {
    options <- .sizeOneOptions("processCapabilityStudies")
    options$controlChartType             <- chartType
    options$capabilityStudyType          <- "normalCapabilityAnalysis"
    options$lowerSpecificationLimit      <- TRUE
    options$upperSpecificationLimit      <- TRUE
    options$lowerSpecificationLimitValue <- 0
    options$upperSpecificationLimitValue <- 8
    set.seed(1)
    results <- runAnalysis("processCapabilityStudies", .sizeOneData, options)
    chart <- .sizeOneChartData(results, "xBar", "xBar_plot")
    .sizeOneExpectations(chart, .sizeOneExpected[[if (chartType == "xBarR") "R" else "s"]])
  })
}
