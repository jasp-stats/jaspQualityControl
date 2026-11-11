context("[Quality Control] Attributes Charts")

## Defectives
# NP
options <- analysisOptions("attributesCharts")
options$defectiveOrDefect <- "D"
options$total <- "Size"
options$attributesChart <- "defectives"
options$attributesChartDefectivesChartType <- "npChart"
set.seed(1)
results <- runAnalysis("attributesCharts", "datasets/attributeCharts/SPC_NP.csv", options)

test_that("np rejects the fixture's impossible defective-item counts", {
  # This fixture includes D = 88 with Size = 50; it remains valid for the c chart below.
  expect_identical(results$status, "validationError")
  expect_match(results$results$errorMessage, "cannot exceed the sample size")
  expect_null(results$results$NPchartPlot)
  expect_null(results$results$NelsonTable)
})

# P
options$attributesChartDefectivesChartType <- "pChart"
results <- runAnalysis("attributesCharts", "datasets/attributeCharts/SPC_P.csv", options)

test_that("p Chart plot matches", {
  plotName <- results[["results"]][["PchartPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "p-chart")
})

# Laney's P
options$attributesChartDefectivesChartType <- "laneyPPrimeChart"
results <- runAnalysis("attributesCharts", "datasets/attributeCharts/SPC_P.csv", options)

test_that("Laney p' Chart plot matches", {
  plotName <- results[["results"]][["LaneyPPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "laney-p-chart")
})

## Defects
# C
options$attributesChartDefectivesChartType <- "npChart"
options$attributesChart <- "defects"
options$attributesChartDefectsChartType <- "cChart"
results <- runAnalysis("attributesCharts", "datasets/attributeCharts/SPC_NP.csv", options)

test_that("c Chart plot matches", {
  plotName <- results[["results"]][["CchartPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "c-chart")
})

test_that("Test results for c chart table results match", {
  table <- results[["results"]][["NelsonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 3, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 18, 19, 20))
})

# U
options$attributesChartDefectsChartType <- "uChart"
results <- runAnalysis("attributesCharts", "datasets/attributeCharts/SPC_P.csv", options)

test_that("u Chart plot matches", {
  plotName <- results[["results"]][["UchartPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "u-chart")
})

# Laneys U
options$attributesChartDefectsChartType <- "laneyUPrimeChart"
results <- runAnalysis("attributesCharts", "datasets/attributeCharts/SPC_P.csv", options)

test_that("Laney u' Chart plot matches", {
  plotName <- results[["results"]][["LaneyUPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "laney-u-chart")
})

## I MR
options$attributesChart <- "xmr"
results <- runAnalysis("attributesCharts", "datasets/attributeCharts/SPC_P.csv", options)

test_that("Individuals and Moving Range Chart plot matches", {
  plotName <- results[["results"]][["IPlotA"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "individuals-and-moving-range-chart")
})

## p chart limits (jasp-stats/jasp-issues#3225); expectations from hand formulas
.acPchartRun <- function(counts, sizes, chart = "pChart", report = FALSE) {
  options <- analysisOptions("attributesCharts")
  options$defectiveOrDefect <- "D"
  options$total <- "N"
  options$attributesChart <- "defectives"
  options$attributesChartDefectivesChartType <- chart
  options$report <- report
  runAnalysis("attributesCharts", data.frame(D = counts, N = sizes), options, view = FALSE)
}

.acLayers <- function(plot, geom) {
  built <- ggplot2::ggplot_build(plot)$data
  built[vapply(plot$layers, function(layer) inherits(layer$geom, geom), logical(1))]
}

# Constant three-sigma p chart limits from the average subgroup size, bounded to [0, 1].
.acAverageSizeLimits <- function(counts, sizes) {
  center <- sum(counts) / sum(sizes)
  halfWidth <- 3 * sqrt(center * (1 - center) / mean(sizes))
  c(center = center, LCL = max(0, center - halfWidth), UCL = min(1, center + halfWidth))
}

test_that("p chart average-size limits are centred on the pooled CL and LCL is not negative", {
  counts <- c(1, 6, 0, 0, 2, 1, 0, 1, 3, 0, 1, 2)
  sizes  <- c(50, 60, 55, 45, 58, 50, 52, 47, 60, 49, 51, 56)
  expected <- .acAverageSizeLimits(counts, sizes)
  expect_equal(unname(expected["LCL"]), 0) # the untruncated LCL is negative
  for (report in c(FALSE, TRUE)) {
    result <- .acPchartRun(counts, sizes, report = report)
    expect_identical(result$status, "complete")
    plot <- if (report) {
      subplots <- result$state$figures[[result$results$report$data]]$obj$subplots
      # the chart is the report subplot with horizontal limit lines
      Filter(function(x) inherits(x, "ggplot") && length(.acLayers(x, "GeomHline")) > 0, subplots)[[1]]
    } else {
      result$state$figures[[result$results$PchartPlot$data]]$obj
    }
    hlines <- unlist(lapply(.acLayers(plot, "GeomHline"), `[[`, "yintercept"))
    expect_equal(sort(hlines), sort(unname(expected)))
    labels <- unlist(lapply(.acLayers(plot, "GeomLabel"), `[[`, "label"))
    expect_true(all(c("UCL = 0.09363", "LCL = 0") %in% labels))
  }
})

test_that("p chart average-size UCL is capped at 1", {
  counts <- c(9, 10, 8, 10, 7, 9)
  sizes  <- c(10, 10, 11, 12, 10, 10)
  expected <- .acAverageSizeLimits(counts, sizes)
  expect_equal(unname(expected["UCL"]), 1) # the uncapped UCL exceeds 1
  result <- .acPchartRun(counts, sizes)
  plot <- result$state$figures[[result$results$PchartPlot$data]]$obj
  hlines <- unlist(lapply(.acLayers(plot, "GeomHline"), `[[`, "yintercept"))
  expect_equal(sort(hlines), sort(unname(expected)))
})

test_that("p chart test table and point colours use the plotted average-size limits", {
  counts <- c(5, 8, 1, 2, 3, 2, 4, 3)
  sizes  <- c(46, 40, 47, 40, 51, 52, 43, 53)
  expected <- .acAverageSizeLimits(counts, sizes)
  proportions <- counts / sizes
  beyond <- which(proportions > expected["UCL"] | proportions < expected["LCL"])
  expect_identical(beyond, 2L)
  result <- .acPchartRun(counts, sizes)
  plot <- result$state$figures[[result$results$PchartPlot$data]]$obj
  points <- .acLayers(plot, "GeomPoint")[[1]]
  expect_identical(which(points$fill == "red"), beyond)
  jaspTools::expect_equal_tables(result$results$NelsonTable$data, list(2))
})

test_that("Laney p' UCL is capped at 1", {
  counts <- c(1, 9, 0, 10, 1, 8, 0, 9)
  sampleSize <- 10
  proportions <- counts / sampleSize
  center <- mean(proportions)
  sigmaP <- sqrt(center * (1 - center) / sampleSize)
  sigmaZ <- mean(abs(diff((proportions - center) / sigmaP))) / 1.128
  expectedUCL <- min(1, center + 3 * sigmaP * sigmaZ)
  expectedLCL <- max(0, center - 3 * sigmaP * sigmaZ)
  expect_equal(expectedUCL, 1) # the uncapped UCL exceeds 1
  result <- .acPchartRun(counts, rep(sampleSize, length(counts)), chart = "laneyPPrimeChart")
  plot <- result$state$figures[[result$results$LaneyPPlot$data]]$obj
  stepValues <- unique(unlist(lapply(.acLayers(plot, "GeomStep"), `[[`, "y")))
  expect_equal(sort(stepValues), sort(c(expectedLCL, expectedUCL)))

  # Unequal sizes: only the subgroups whose UCL exceeds 1 are capped. Uncapped limits stay
  # symmetric around the pooled CL, which holds whatever sigma Z is.
  counts <- c(9, 7, 80, 80)
  sizes  <- c(10, 10, 100, 100)
  center <- sum(counts) / sum(sizes)
  result <- .acPchartRun(counts, sizes, chart = "laneyPPrimeChart")
  plot <- result$state$figures[[result$results$LaneyPPlot$data]]$obj
  steps <- .acLayers(plot, "GeomStep")
  upper <- steps[[1]]$y
  lower <- steps[[2]]$y
  expect_equal(upper[1:2], c(1, 1))
  expect_true(all(upper[3:4] < 1 & lower[3:4] > 0))
  expect_equal(upper[3:4] - center, center - lower[3:4])
})

test_that("p chart keeps varying LCLs when every UCL is capped at 1", {
  counts <- c(19, 10, 28, 9)
  sizes  <- c(20, 10, 30, 10) # sizes differ by more than 25%: per-subgroup limits
  center <- sum(counts) / sum(sizes)
  halfWidths <- 3 * sqrt(center * (1 - center) / sizes)
  expect_true(all(center + halfWidths > 1))
  result <- .acPchartRun(counts, sizes)
  plot <- result$state$figures[[result$results$PchartPlot$data]]$obj
  steps <- .acLayers(plot, "GeomStep")
  expect_length(steps, 4)
  expect_equal(steps[[1]]$y, rep(1, 4))
  expect_equal(steps[[2]]$y, pmax(0, center - halfWidths))
})
