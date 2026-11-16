context("[Quality Control] Process Capability Study - specification limit labels of the capability histogram")

# Data in small units: rounding the limits to the set decimals (3, or 2 in the test suite) gave labels like "LSL = 0"
# (jasp-issues #4681). The labels are read from the layers of the histogram, without a snapshot.
smallData <- data.frame(Measurement = c(0.00016058, 0.00020555, 0.00020832, 0.00016342, 0.00026613, 0.00021752, 0.00014323, 0.00022483, 0.00018467, 0.00020527))

.histogramLabels <- function(axisModification, boundary = FALSE) {
  options <- analysisOptions("processCapabilityStudies")
  options$measurementLongFormat <- "Measurement"
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 1
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- 0.0001
  options$lowerSpecificationLimitBoundary <- boundary
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 0.0004
  options$upperSpecificationLimitBoundary <- boundary
  options$target <- TRUE
  options$targetValue <- 0.0002
  options$processCapabilityPlotSpecificationLimits <- TRUE
  options$processCapabilityPlotXAxisModification <- axisModification
  plot <- jaspQualityControl:::.qcProcessCapabilityPlotObject(options, smallData, "Measurement", "", distribution = "normal")[[1]]
  unlist(lapply(plot$layers, function(l) if (is.data.frame(l$data) && "label" %in% names(l$data)) as.character(l$data$label)))
}

test_that("SL1 Capability histogram labels small specification limits, default x axis", {
  labels <- .histogramLabels("none")
  expect_true(all(c("Target = 0.0002", "LSL = 0.0001", "USL = 0.0004") %in% labels))
})

test_that("SL2 Capability histogram labels small specification limits, compressed and thinned x axis", {
  for (axisModification in c("compress", "thin")) {
    labels <- .histogramLabels(axisModification)
    expect_true(all(c("Target = 0.0002", "LSL = 0.0001", "USL = 0.0004") %in% labels), info = axisModification)
  }
})

test_that("SL3 Capability histogram labels small boundaries", {
  for (axisModification in c("none", "compress")) {
    labels <- .histogramLabels(axisModification, boundary = TRUE)
    expect_true(all(c("LB = 0.0001", "UB = 0.0004") %in% labels), info = axisModification)
  }
})
