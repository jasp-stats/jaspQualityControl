context("Attributes charts timestamp labels")

.actRun <- function(data, family, chart = "pChart", timeStamp = "lot", report = FALSE) {
  options <- jaspTools::analysisOptions("attributesCharts")
  options$defectiveOrDefect <- "count"
  options$total <- "size"
  options$timeStamp <- timeStamp
  options$report <- report
  options$reportMetaData <- FALSE
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

test_that("X-mR skips moving ranges across missing samples and keeps source labels", {
  counts <- c(rep(3, 6), NA, 20, 3, 3, 3)
  data <- data.frame(count = counts, size = 50, lot = LETTERS[seq_along(counts)])
  for (timeStamp in c("", "lot")) {
    result <- .actRun(data, "xmr", timeStamp = timeStamp)
    expect_identical(result$status, "complete")
    mrTable <- result$results$NelsonTableIMR$collection$NelsonTableIMR_NelsonTableMR$data
    expectedSignal <- if (timeStamp == "") 9 else "I"
    expect_equal(lapply(mrTable, function(row) row$test1), list(expectedSignal))

    mrPlot <- result$state$figures[[result$results$IPlotA$data]]$obj$subplots[[2]]
    expect_equal(mrPlot$data$subgroups, c(2:6, 9:11))
    expect_equal(mrPlot$data$data, c(rep(0, 5), 0.34, 0, 0))
    expectedLabels <- if (timeStamp == "") as.character(c(2:6, 8:11)) else LETTERS[c(2:6, 8:11)]
    expect_identical(as.character(mrPlot$scales$get_scales("x")$get_labels()), expectedLabels)
    built <- ggplot2::ggplot_build(mrPlot)
    line <- which(vapply(mrPlot$layers, function(layer) inherits(layer$geom, "GeomLine"), logical(1)))[1]
    expect_equal(length(unique(built$data[[line]]$group)), 2L)
  }
})

test_that("a blank timestamp falls back to the source number without dropping the sample", {
  data <- data.frame(count = c(2, 2, 2, 30, 2, 2), size = 50, lot = c("A", "B", "C", NA, "E", "F"))
  results <- lapply(c("", "lot"), function(label) .actRun(data, "defectives", timeStamp = label))
  for (result in results) {
    expect_identical(result$status, "complete")
    expect_null(result$results$missingValuesNote)
    plot <- result$state$figures[[result$results$PchartPlot$data]]$obj
    expect_equal(ggplot2::ggplot_build(plot)$data[[1]]$yintercept, 40 / 300)
    expect_equal(as.integer(result$results$NelsonTable$data[[1]]$test1), 4L)
  }
  result <- results[[2L]]
  plot <- result$state$figures[[result$results$PchartPlot$data]]$obj
  expect_identical(plot$scales$get_scales("x")$labels, c("A", "B", "C", "4", "E", "F"))
})

test_that("interactive point coordinates use source numbers even with varying subgroup sizes", {
  data <- data.frame(count = c(2, 3, NA, 12, 5, 2), size = c(25, 50, 25, 50, 25, 50),
                     lot = c("A", "B", "C", "D", "E", "F"))
  result <- .actRun(data, "defectives")
  plot <- result$state$figures[[result$results$PchartPlot$data]]$obj
  converted <- jsonlite::fromJSON(jaspGraphs::convertGgplotToPlotly(plot), simplifyVector = FALSE)
  points <- Filter(function(trace) identical(trace$mode, "markers"), converted$data)
  expect_equal(sort(unlist(lapply(points, function(trace) trace$x))), c(1, 2, 4, 5, 6))
  hover <- unlist(lapply(points, function(trace) trace$text))
  expect_true(any(grepl("subgroups: 4", hover, fixed = TRUE) & grepl("Sample: D", hover, fixed = TRUE)))
})

test_that("the report figure includes the omission note with metadata disabled", {
  data <- data.frame(count = c(2, 3, NA, 12, 5, 2), size = 50, lot = LETTERS[1:6])
  for (family in c("defectives", "xmr")) {
    result <- .actRun(data, family, report = TRUE)
    expect_identical(result$status, "complete")
    report <- result$state$figures[[result$results$report$data]]$obj
    labels <- unlist(lapply(report$subplots, function(subplot) {
      lapply(subplot$layers, function(layer) layer$data$label)
    }))
    expect_true("Removed 1 observation(s) with missing values." %in% labels)
  }
})
