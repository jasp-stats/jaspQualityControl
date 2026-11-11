context("[Quality Control] Attributes charts: out-of-control tests with unequal subgroup sizes")

.nelsonRun <- function(counts, sizes, chart) {
  options <- jaspTools::analysisOptions("attributesCharts")
  options$defectiveOrDefect <- "count"
  options$total <- "size"
  if (chart %in% c("pChart", "laneyPPrimeChart")) {
    options$attributesChart <- "defectives"
    options$attributesChartDefectivesChartType <- chart
  } else {
    options$attributesChart <- "defects"
    options$attributesChartDefectsChartType <- chart
  }
  jaspTools::runAnalysis("attributesCharts", data.frame(count = counts, size = sizes), options, view = FALSE)
}

.nelsonColumn <- function(results, column) {
  rows <- results$results$NelsonTable$data
  unlist(lapply(rows, `[[`, column))
}

.plotKey <- c(pChart = "PchartPlot", uChart = "UchartPlot", laneyPPrimeChart = "LaneyPPlot", laneyUPrimeChart = "LaneyUPlot")

.redPoints <- function(results, chart) {
  plot  <- results$state$figures[[results$results[[.plotKey[[chart]]]]$data]]$obj
  point <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))
  unname(which(plot$layers[[point]]$aes_params$fill == "red"))
}

test_that("test 1 uses each subgroup's own limits and agrees with the plot", {
  # the first subgroup is small (wide limits); subgroup 6 is beyond its own, much narrower limits
  sizes  <- rep(c(10, 1000), 6)
  counts <- c(2, 50, 1, 52, 2, 85, 1, 48, 0, 50, 2, 51)
  for (chart in c("pChart", "uChart", "laneyPPrimeChart", "laneyUPrimeChart")) {
    results <- .nelsonRun(counts, sizes, chart)
    expect_identical(results$status, "complete")
    expect_equal(as.integer(.nelsonColumn(results, "test1")), .redPoints(results, chart), info = chart)
  }
  expect_equal(.nelsonColumn(.nelsonRun(counts, sizes, "uChart"), "test1"), 6)
})

test_that("p chart reports shifts and trends with unequal subgroup sizes", {
  sizes  <- rep(c(100, 200), 8)
  # proportions 0.03 (x7), then rising 0.04 ... 0.11, then 0.06; centre line 0.054
  counts <- c(3, 6, 3, 6, 3, 6, 3, 8, 5, 12, 7, 16, 9, 20, 11, 12)
  results <- .nelsonRun(counts, sizes, "pChart")
  expect_identical(results$status, "complete")
  expect_null(.nelsonColumn(results, "test1"))
  # 7 points in a row on the same side of the centre line
  expect_equal(.nelsonColumn(results, "test2"), c(7, 8, 9, 16))
  # 7 increases in a row (8 points)
  expect_equal(.nelsonColumn(results, "test3"), c(14, 15))
})
