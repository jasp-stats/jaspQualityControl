context("[Quality Control] Process capability X-mR chart in the report")

.xmrData <- read.csv("datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv")

.xmrOptions <- function(report, movingRangeLength = 2) {
  options <- analysisOptions("processCapabilityStudies")
  options$testSet <- "jaspDefault"
  options$measurementLongFormat <- "Diameter"
  options$stagesLongFormat <- "Stage"
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 1
  options$capabilityStudyType <- "normalCapabilityAnalysis"
  options$controlChartType <- "xmr"
  options$xmrChartSpecificationLimits <- TRUE
  options$xmrChartMovingRangeLength <- movingRangeLength
  options$controlChartSdEstimationMethodGroupSizeEqualOne <- "meanMovingRange"
  options$lowerSpecificationLimit <- TRUE
  options$target <- TRUE
  options$upperSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- 0
  options$targetValue <- 6
  options$upperSpecificationLimitValue <- 12
  options$report <- report
  options
}

# the individuals panel and the moving range panel of the X-mR chart, from the plot of the control chart or of the report
.xmrPanels <- function(results, report) {
  plot <- if (report) results[["results"]][["report"]][["data"]] else results[["results"]][["xmr"]][["collection"]][["xmr_plot"]][["data"]]
  plot <- results[["state"]][["figures"]][[plot]][["obj"]]
  isControlChart <- function(panel) ggplot2::is_ggplot(panel) && any(vapply(panel$layers, function(l) inherits(l$geom, "GeomStep"), logical(1)))
  panels <- Filter(isControlChart, plot$subplots)
  stopifnot(length(panels) == 2)
  return(list(individuals = panels[[1]], movingRange = panels[[2]]))
}

.xmrLayerData <- function(panel, geom) {
  layers <- which(vapply(panel$layers, function(l) inherits(l$geom, geom), logical(1)))
  return(lapply(layers, function(i) ggplot2::layer_data(panel, i)))
}

# center line, upper and lower control limit (one value per stage), in the order of the step layers
.xmrLimits <- function(panel) {
  limits <- lapply(.xmrLayerData(panel, "GeomStep"), function(d) unique(d[["y"]][!is.na(d[["y"]])]))
  return(setNames(limits, c("center", "upper", "lower")))
}

.xmrLabels <- function(panel) unlist(lapply(.xmrLayerData(panel, "GeomLabel"), function(d) as.character(d[["label"]])))

.xmrFlaggedPoints <- function(panel) {
  points <- .xmrLayerData(panel, "GeomJaspPoint")[[1]]
  return(points[["x"]][points[["fill"]] == "red"])
}

# independent calculation: the sigma from the mean range of moving windows of the given length, divided by d2
# (the tolerance in the tests covers the rounding of the tabled d2 constants)
.xmrExpectedLimits <- function(movingRangeLength, nSigmas = 3) {
  d2 <- c("2" = 1.1283792, "3" = 1.6925688, "4" = 2.0587504)[[as.character(movingRangeLength)]]
  limits <- lapply(split(.xmrData[["Diameter"]], .xmrData[["Stage"]]), function(x) {
    windows <- embed(x, movingRangeLength)
    sigma <- mean(apply(windows, 1, function(w) max(w) - min(w))) / d2
    c(center = mean(x), upper = mean(x) + nSigmas * sigma, lower = mean(x) - nSigmas * sigma)
  })
  return(do.call(rbind, limits))
}

normalChart <- runAnalysis("processCapabilityStudies", .xmrData, .xmrOptions(report = FALSE), view = FALSE)
reportChart <- runAnalysis("processCapabilityStudies", .xmrData, .xmrOptions(report = TRUE), view = FALSE)
normalPanels <- .xmrPanels(normalChart, report = FALSE)
reportPanels <- .xmrPanels(reportChart, report = TRUE)

test_that("Report X-mR chart has the limits of each stage", {
  limits <- .xmrLimits(reportPanels$individuals)
  expect_equal(round(limits$upper, 3), c(13.118, 12.452))
  expect_equal(round(limits$lower, 3), c(1.047, 1.705))
  expect_equal(limits, .xmrLimits(normalPanels$individuals))
  expect_equal(.xmrLimits(reportPanels$movingRange), .xmrLimits(normalPanels$movingRange))
  expected <- .xmrExpectedLimits(2)
  expect_equal(unname(limits$center), unname(expected[, "center"]), tolerance = 5e-3)
  expect_equal(unname(limits$upper), unname(expected[, "upper"]), tolerance = 5e-3)
  expect_equal(unname(limits$lower), unname(expected[, "lower"]), tolerance = 5e-3)
})

test_that("Report X-mR chart shows the specification limits", {
  expect_true(all(c("LSL = 0", "Tar. = 6", "USL = 12") %in% .xmrLabels(reportPanels$individuals)))
  expect_equal(.xmrLabels(reportPanels$individuals), .xmrLabels(normalPanels$individuals))
})

test_that("Report X-mR chart flags the same points as the control chart", {
  expect_true(all(c(84, 85) %in% .xmrFlaggedPoints(reportPanels$movingRange)))
  expect_equal(.xmrFlaggedPoints(reportPanels$movingRange), .xmrFlaggedPoints(normalPanels$movingRange))
  expect_equal(.xmrFlaggedPoints(reportPanels$individuals), .xmrFlaggedPoints(normalPanels$individuals))
})

test_that("Moving range length applies to the individuals chart limits in the control chart and the report", {
  for (report in c(FALSE, TRUE)) {
    results <- runAnalysis("processCapabilityStudies", .xmrData, .xmrOptions(report = report, movingRangeLength = 3), view = FALSE)
    limits <- .xmrLimits(.xmrPanels(results, report)$individuals)
    expected <- .xmrExpectedLimits(3)
    expect_equal(unname(limits$center), unname(expected[, "center"]), tolerance = 5e-3)
    expect_equal(unname(limits$upper), unname(expected[, "upper"]), tolerance = 5e-3)
    expect_equal(unname(limits$lower), unname(expected[, "lower"]), tolerance = 5e-3)
    # not the limits of moving ranges of length 2
    expect_false(isTRUE(all.equal(unname(limits$upper), unname(.xmrExpectedLimits(2)[, "upper"]), tolerance = 5e-3)))
  }
})
