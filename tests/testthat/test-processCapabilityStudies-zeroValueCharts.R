context("[Quality Control] Process Capability Study - control charts of non-normal studies with zero values")

# jasp-stats/jasp-issues#4690: for the distributions that need positive data (Weibull, log-normal, gamma, log-logistic)
# zeros are replaced by half the smallest positive value before the fit. The control charts were drawn from the
# replaced data (the X-bar point of the subgroup with the zero was 7.44 instead of 7.08) and the note on the replaced
# zeros stayed after a transformation, stage or subgroup change that left no zeros.

.zeroChartData <- function() {
  diameter <- read.csv(testthat::test_path("datasets", "processCapabilityStudy", "processCapabilityAnalysisLongFormatDebug.csv"))[["Diameter"]]
  diameter[which.min(diameter)] <- 0 # row 8, in subgroup 2
  data.frame(Diameter = diameter)
}

.zeroChartOptions <- function(distribution, controlChartType = "xBarR", subgroupSize = 5, report = FALSE) {
  options <- analysisOptions("processCapabilityStudies")
  options$testSet <- "jaspDefault"
  options$measurementLongFormat <- "Diameter"
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- subgroupSize
  options$controlChartType <- controlChartType
  options$controlChartSdEstimationMethodGroupSizeEqualOne <- "meanMovingRange"
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- 0.4
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 12
  options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
  options$nonNormalDistribution <- distribution
  options$report <- report
  options
}

.zeroChartRun <- function(...) {
  set.seed(1)
  runAnalysis("processCapabilityStudies", .zeroChartData(), .zeroChartOptions(...), view = FALSE)
}

# the points of the upper and lower control chart (control charts are the panels with step lines for the limits)
.zeroChartPoints <- function(results, element) {
  plot <- if (element == "report") results[["results"]][["report"]][["data"]] else results[["results"]][[element]][["collection"]][[paste0(element, "_plot")]][["data"]]
  plot <- results[["state"]][["figures"]][[plot]][["obj"]]
  hasLayer <- function(panel, geom) any(vapply(panel$layers, function(l) inherits(l$geom, geom), logical(1)))
  panels <- Filter(function(panel) ggplot2::is_ggplot(panel) && hasLayer(panel, "GeomStep"), plot$subplots)
  stopifnot(length(panels) == 2)
  lapply(panels, function(panel) {
    pointLayer <- which(vapply(panel$layers, function(l) inherits(l$geom, "GeomJaspPoint"), logical(1)))[1]
    ggplot2::layer_data(panel, pointLayer)[["y"]]
  })
}

# independent calculation from the data as entered: means and ranges of consecutive subgroups of 5
.zeroChartSubgroups <- function() {
  subgroups <- split(.zeroChartData()[["Diameter"]], ceiling(seq_len(100) / 5))
  list(means = unname(vapply(subgroups, mean, numeric(1))), ranges = unname(vapply(subgroups, function(x) diff(range(x)), numeric(1))))
}

test_that("Weibull X-bar & R chart shows the data as entered, not the zero replaced for the fit", {
  results <- .zeroChartRun("weibull")
  expected <- .zeroChartSubgroups()
  expect_equal(expected$means[2], 7.08)
  points <- .zeroChartPoints(results, "xBar")
  expect_equal(points[[1]], expected$means)
  expect_equal(points[[2]], expected$ranges)
  # the fit still replaces the zero
  expect_false(is.null(results[["results"]][["zeroWarning"]]))
})

test_that("X-bar & R chart is the same for a distribution that keeps zeros (logistic) and one that replaces them (Weibull)", {
  weibull  <- .zeroChartPoints(.zeroChartRun("weibull"), "xBar")
  logistic <- .zeroChartRun("logistic")
  expect_null(logistic[["results"]][["zeroWarning"]])
  expect_equal(.zeroChartPoints(logistic, "xBar"), weibull)
  expect_equal(weibull[[1]][2], 7.08)
})

test_that("Weibull X-bar & R chart in the report shows the data as entered", {
  points <- .zeroChartPoints(.zeroChartRun("weibull", report = TRUE), "report")
  expected <- .zeroChartSubgroups()
  expect_equal(points[[1]], expected$means)
  expect_equal(points[[2]], expected$ranges)
})

test_that("Weibull X-mR chart (also in the report) shows the data as entered", {
  diameter <- .zeroChartData()[["Diameter"]]
  for (report in c(FALSE, TRUE)) {
    points <- .zeroChartPoints(.zeroChartRun("weibull", controlChartType = "xmr", subgroupSize = 1, report = report),
                               if (report) "report" else "xmr")
    expect_equal(points[[1]], diameter, label = paste("individuals, report =", report))
    expect_equal(points[[1]][8], 0)
    expect_equal(points[[2]][-1], abs(diff(diameter)), label = paste("moving ranges, report =", report))
  }
})

test_that("The zero values note depends on every option that changes the data, and goes when no zeros are left", {
  # jaspTools runs are stateless (no state is reused between runs), so the dependencies are checked directly
  dependencies <- jaspQualityControl:::.qcCapabilityZeroWarningDependencies()
  dataOptions <- c("capabilityStudyType", "nonNormalDistribution", "dataFormat", "measurementLongFormat", "measurementsWideFormat",
                   "stagesLongFormat", "stagesWideFormat", "subgroup", "subgroupSizeType", "groupingVariableMethod",
                   "manualSubgroupSizeValue", "dataTransformation", "dataTransformationShift", "dataTransformationLambda",
                   "dataTransformationMethod", "dataTransformationContinuityAdjustment")
  expect_true(all(dataOptions %in% dependencies), label = paste(setdiff(dataOptions, dependencies), collapse = ", "))
  qmlFile <- system.file("qml", "processCapabilityStudies.qml", package = "jaspQualityControl")
  qmlOptions <- sub('.*name:\\s*"([^"]+)".*', "\\1", grep('name:\\s*"[^"]+"', readLines(qmlFile), value = TRUE))
  expect_true(all(dataOptions %in% qmlOptions), label = paste(setdiff(dataOptions, qmlOptions), collapse = ", "))

  # fresh run: Box-Cox with shift 2 and lambda 0 leaves no zeros, so no note
  options <- .zeroChartOptions("weibull")
  options$dataTransformation <- "boxCox"
  options$dataTransformationShift <- 2
  options$dataTransformationLambda <- 0
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", .zeroChartData(), options, view = FALSE)
  expect_equal(results[["status"]], "complete")
  expect_null(results[["results"]][["zeroWarning"]])
})
