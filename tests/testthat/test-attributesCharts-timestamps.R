context("Attributes charts timestamp labels")

.actRun <- function(data, family, chart = "pChart") {
  options <- jaspTools::analysisOptions("attributesCharts")
  options$defectiveOrDefect <- "count"
  options$total <- "size"
  options$timeStamp <- "lot"
  options$attributesChart <- family
  options$attributesChartDefectivesChartType <- if (family == "defectives") chart else "npChart"
  options$attributesChartDefectsChartType <- if (family == "defects") chart else "cChart"
  jaspTools::runAnalysis("attributesCharts", data, options, view = FALSE)
}

test_that("charts with a timestamp and more than 60 samples label the thinned axis breaks", {
  set.seed(1)
  n <- 70
  lots <- sprintf("Lot %02d", seq_len(n))
  data <- data.frame(count = rpois(n, 3), size = 50, lot = lots)
  charts <- list(c("defectives", "pChart", "PchartPlot"), c("defectives", "npChart", "NPchartPlot"),
                 c("defectives", "laneyPPrimeChart", "LaneyPPlot"), c("defects", "cChart", "CchartPlot"),
                 c("defects", "uChart", "UchartPlot"), c("defects", "laneyUPrimeChart", "LaneyUPlot"))
  for (chart in charts) {
    result <- .actRun(data, chart[1], chart[2])
    expect_identical(result$status, "complete")
    plot <- result$state$figures[[result$results[[chart[3]]]$data]]$obj
    scale <- plot$scales$get_scales("x")
    breaks <- scale$breaks
    # each break is labelled with the timestamp of its sample; breaks past the last sample stay empty
    sampleAtBreak <- floor(breaks + 1e-8)
    expected <- ifelse(sampleAtBreak <= n, lots[pmin(sampleAtBreak, n)], "")
    expect_identical(as.character(scale$labels), expected)
    expect_lt(length(breaks), n)
    expect_no_error(ggplot2::ggplot_build(plot))
  }
})

test_that("X-mR moving ranges are labelled with the later of their two samples", {
  counts <- c(3, 3, 3, 3, 3, 3, 20, 3, 3, 3)
  data <- data.frame(count = counts, size = 50, lot = LETTERS[1:10])
  # The jump between F and G and the drop between G and H give the two out-of-limit moving ranges.
  result <- .actRun(data, "xmr")
  expect_identical(result$status, "complete")
  mrTable <- result$results$NelsonTableIMR$collection$NelsonTableIMR_NelsonTableMR$data
  expect_identical(vapply(mrTable, function(row) row$test1, character(1)), c("G", "H"))

  options <- jaspTools::analysisOptions("attributesCharts")
  options$defectiveOrDefect <- "count"
  options$total <- "size"
  options$attributesChart <- "xmr"
  result <- jaspTools::runAnalysis("attributesCharts", data[c("count", "size")], options, view = FALSE)
  mrTable <- result$results$NelsonTableIMR$collection$NelsonTableIMR_NelsonTableMR$data
  expect_equal(vapply(mrTable, function(row) as.numeric(row$test1), numeric(1)), c(7, 8))
})
