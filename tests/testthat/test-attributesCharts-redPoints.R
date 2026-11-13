context("[Quality Control] Attributes charts: red points match the test table")

# 26 samples of n = 100: a run up to sample 14 (beyond the UCL), then a sustained shift below the centre line
.redCounts <- c(5, 6, 4, 5, 2, 3, 6, 7, 8, 9, 10, 11, 12, 18, 4, 6, 8, 3, 4, 3, 4, 2, 4, 3, 4, 3)

.redRun <- function(family, chart = NULL, counts = .redCounts) {
  options <- jaspTools::analysisOptions("attributesCharts")
  options$defectiveOrDefect <- "count"
  options$total <- "size"
  options$attributesChart <- family
  if (family == "defectives")
    options$attributesChartDefectivesChartType <- chart
  if (family == "defects")
    options$attributesChartDefectsChartType <- chart
  jaspTools::runAnalysis("attributesCharts", data.frame(count = counts, size = 100), options, view = FALSE)
}

.redFlagged <- function(table) {
  sort(unique(unlist(lapply(table$data, function(row) unlist(row)))))
}

.redFill <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  point <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))
  expect_length(point, 1)
  data  <- built$data[[point]]
  as.integer(sort(floor(data$x[data$fill == "red"]))) # step charts plot subgroup i at i + 0.5
}

test_that("p, u, np, c and Laney charts colour every point listed in the test table", {
  charts <- list(
    pChart           = c("defectives", "PchartPlot"),
    npChart          = c("defectives", "NPchartPlot"),
    laneyPPrimeChart = c("defectives", "LaneyPPlot"),
    cChart           = c("defects",    "CchartPlot"),
    uChart           = c("defects",    "UchartPlot"),
    laneyUPrimeChart = c("defects",    "LaneyUPlot")
  )
  for (chart in names(charts)) {
    results <- .redRun(charts[[chart]][1], chart)
    expect_identical(results$status, "complete", info = chart)
    rows <- results$results$NelsonTable$data
    expect_equal(unlist(lapply(rows, `[[`, "test2")), c(13, 14, 24, 25, 26), info = chart)
    expect_equal(unlist(lapply(rows, `[[`, "test3")), c(12, 13, 14), info = chart)
    plot <- results$state$figures[[results$results[[charts[[chart]][2]]]$data]]$obj
    expect_equal(.redFill(plot), c(12L, 13L, 14L, 24L, 25L, 26L), info = chart)
    expect_equal(.redFill(plot), as.integer(.redFlagged(results$results$NelsonTable)), info = chart)
  }
})

test_that("X-mR chart colours every point listed in its test tables", {
  # alternating then stable proportions: only tests 5 (reducing variation) and 6 (bimodal) fire, not tests 1-3
  counts  <- c(8, 2, 9, 3, 8, 2, 9, 3, rep(c(6, 5), 9))
  results <- .redRun("xmr", counts = counts)
  expect_identical(results$status, "complete")
  plot   <- results$state$figures[[results$results$IPlotA$data]]$obj
  tables <- results$results$NelsonTableIMR$collection
  iTable <- tables$NelsonTableIMR_NelsonTableI
  expect_equal(unlist(lapply(iTable$data, `[[`, "test5")), 23:26)
  expect_equal(unlist(lapply(iTable$data, `[[`, "test6")), 8)
  expect_equal(.redFill(plot$subplots[[1]]), c(8L, 23L, 24L, 25L, 26L))
  expect_equal(.redFill(plot$subplots[[1]]), as.integer(.redFlagged(iTable)))
  expect_equal(.redFill(plot$subplots[[2]]), as.integer(.redFlagged(tables$NelsonTableIMR_NelsonTableMR)))
})

test_that("charts with a single subgroup still mark points beyond the limits", {
  for (chart in c("pChart", "laneyPPrimeChart")) {
    options <- jaspTools::analysisOptions("attributesCharts")
    options$defectiveOrDefect <- "count"
    options$total <- "size"
    options$attributesChart <- "defectives"
    options$attributesChartDefectivesChartType <- chart
    results <- jaspTools::runAnalysis("attributesCharts", data.frame(count = 5, size = 100), options, view = FALSE)
    expect_identical(results$status, "complete", info = chart)
  }
})
