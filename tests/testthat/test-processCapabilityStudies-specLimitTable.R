context("[Quality Control] Process Capability Study - small specification limits in the summary tables and axis labels")

# Data in small units (jasp-issues #4700): the process summary tables rounded LSL, Target and USL to the set decimals
# (2 in the test suite, 3 in JASP), so that they read 0; the x-axis labels of the probability plot and of the
# capability histogram with a compressed x-axis read 0 as well. Values and labels are read from the results and the
# plot objects, without snapshots.
smallData <- data.frame(Measurement = c(0.00016058, 0.00020555, 0.00020832, 0.00016342, 0.00026613,
                                        0.00021752, 0.00014323, 0.00022483, 0.00018467, 0.00020527))
smallLimits <- c(lsl = 0.0001, target = 0.0002, usl = 0.0004)

.smallLimitOptions <- function(limits = smallLimits) {
  options <- analysisOptions("processCapabilityStudies")
  options$measurementLongFormat <- "Measurement"
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 1
  options$controlChartType <- "xmr"
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- limits[["lsl"]]
  options$target <- TRUE
  options$targetValue <- limits[["target"]]
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- limits[["usl"]]
  options
}

.xAxisLabels <- function(plot) {
  labels <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$x$get_labels()
  labels[!is.na(labels) & labels != ""]
}

test_that("ST1 Process summary table shows small specification limits as entered", {
  options <- .smallLimitOptions()
  results <- runAnalysis("processCapabilityStudies", smallData, options, view = FALSE)
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]]
  row <- table[["data"]][[1]]
  expect_equal(c(lsl = row[["lsl"]], target = row[["target"]], usl = row[["usl"]]), smallLimits)
  expect_equal(row[["mean"]], mean(smallData$Measurement))
  fieldTypes <- vapply(table[["schema"]][["fields"]], function(f) f[["type"]], character(1))
  names(fieldTypes) <- vapply(table[["schema"]][["fields"]], function(f) f[["name"]], character(1))
  expect_equal(unname(fieldTypes[c("lsl", "target", "usl")]), rep("number", 3))
})

test_that("ST2 Non-normal process summary table shows small specification limits as entered", {
  for (distribution in c("weibull", "lognormal")) {
    options <- .smallLimitOptions()
    options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
    options$nonNormalDistribution <- distribution
    results <- runAnalysis("processCapabilityStudies", smallData, options, view = FALSE)
    table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]]
    row <- table[["data"]][[1]]
    expect_equal(c(lsl = row[["lsl"]], target = row[["target"]], usl = row[["usl"]]), smallLimits, info = distribution)
    fieldTypes <- vapply(table[["schema"]][["fields"]], function(f) f[["type"]], character(1))
    names(fieldTypes) <- vapply(table[["schema"]][["fields"]], function(f) f[["name"]], character(1))
    expect_equal(unname(fieldTypes[c("lsl", "target", "usl")]), rep("number", 3), info = distribution)
  }
})

test_that("ST3 Probability plot x-axis labels of small values are distinct and not 0", {
  for (distribution in c("normal", "lognormal", "weibull")) {
    options <- .smallLimitOptions()
    options$nullDistribution <- distribution
    plot <- jaspQualityControl:::.qcProbabilityPlotObject(options, smallData, "Measurement", "")[[1]]
    labels <- .xAxisLabels(plot)
    values <- as.numeric(labels)
    expect_gte(length(labels), 3, label = distribution)
    expect_false(anyDuplicated(labels) > 0, info = distribution)
    expect_true(all(values > 0), info = distribution)
    xScale <- plot$scales$get_scales("x")
    ticks <- if (distribution == "normal") xScale$breaks else exp(xScale$breaks)
    expect_equal(values, signif(ticks, 3), tolerance = 1e-10, info = distribution)
  }
})

test_that("Probability plots distinguish nearby measurements at different scales", {
  for (scale in c(1, 0.01)) {
    data <- data.frame(Measurement = scale * seq(0.00020001, by = 0.00000001, length.out = 10))
    for (distribution in c("normal", "lognormal", "weibull")) {
      options <- .smallLimitOptions()
      options$nullDistribution <- distribution
      plot <- jaspQualityControl:::.qcProbabilityPlotObject(options, data, "Measurement", "")[[1]]
      labels <- .xAxisLabels(plot)
      expect_gte(length(labels), 3, label = distribution)
      expect_identical(anyDuplicated(labels), 0L, info = distribution)

      expect_true(any(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomRangeFrame"), logical(1))),
                  info = distribution)
      xScale <- plot$scales$get_scales("x")
      x <- if (distribution == "normal") data$Measurement else log(data$Measurement)
      expect_equal(xScale$breaks, jaspGraphs::getPrettyAxisBreaks(x, n = 3), info = distribution)
      expect_equal(xScale$limits, range(xScale$breaks), info = distribution)
      expect_true(all(nzchar(xScale$labels)), info = distribution)
      values <- if (distribution == "normal") xScale$breaks else exp(xScale$breaks)
      expect_lt(max(abs(as.numeric(xScale$labels) - values)), min(diff(values)) / 2, label = distribution)

      yScale <- plot$scales$get_scales("y")
      transform <- if (distribution == "weibull") function(p) log(-log1p(-p)) else qnorm
      percentTicks <- c(0.1, 1, 5, seq(10, 90, 10), 95, 99, 99.9)
      expect_identical(yScale$labels, percentTicks, info = distribution)
      expect_equal(yScale$breaks, transform(percentTicks / 100), info = distribution)
    }
  }
})

test_that("Probability tick labels distinguish adjacent doubles without inventing differences for ties", {
  labels <- jaspQualityControl:::.qcProbabilityTickLabels(c(1, 1, 1 + .Machine$double.eps), 3)
  expect_identical(labels[1:2], c("1", "1"))
  expect_identical(as.numeric(labels[3]), 1 + .Machine$double.eps)
  expect_false(labels[3] == labels[1])
})

test_that("ST4 Capability histogram with compressed x-axis labels small values", {
  # small limits: no gap is large enough to compress; wide limits: both sides are compressed; tiny units: 100 times smaller
  cases <- list(small = list(data = smallData, limits = smallLimits),
                wide  = list(data = smallData, limits = c(lsl = 0.00001, target = 0.0002, usl = 0.0009)),
                tiny  = list(data = smallData / 100, limits = smallLimits / 100))
  for (case in names(cases)) {
    options <- .smallLimitOptions(cases[[case]]$limits)
    options$processCapabilityPlotSpecificationLimits <- TRUE
    options$processCapabilityPlotXAxisModification <- "compress"
    plot <- jaspQualityControl:::.qcProcessCapabilityPlotObject(options, cases[[case]]$data, "Measurement", "", distribution = "normal")[[1]]
    labels <- .xAxisLabels(plot)
    values <- as.numeric(labels)
    expect_gte(length(labels), 3, label = case)
    expect_false(anyDuplicated(labels) > 0, info = case)
    expect_true(all(values > 0), info = case)
    expect_true(all(values >= cases[[case]]$limits[["lsl"]] & values <= cases[[case]]$limits[["usl"]]), info = case)
  }
})
