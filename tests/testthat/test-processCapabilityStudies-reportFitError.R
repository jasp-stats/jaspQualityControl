context("[Quality Control] Process Capability Study - report when the non-normal fit fails")

# jasp-stats/jasp-issues#4689: with "Show Report" on, a non-normal distribution that cannot be fitted ended the whole
# analysis. The report now shows the error in the panels that need the fit, and keeps its other elements.

.reportFitErrorData <- function() {
  data <- read.csv("datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv")
  data[["Diameter"]][which.min(data[["Diameter"]])] <- 0 # row 8
  data
}

# 3-parameter Weibull with a historical threshold equal to the smallest observation: the fit fails. The GUI sets the
# null distribution of the probability plot to the non-normal distribution; "normal" keeps the probability plot drawable.
.reportFitErrorResults <- function(report, stages = "", nullDistribution = "normal") {
  options <- analysisOptions("processCapabilityStudies")
  options$measurementLongFormat <- "Diameter"
  options$stagesLongFormat <- stages
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 5
  options$controlChartType <- "xBarR"
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- 2
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 12
  options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
  options$nonNormalDistribution <- "3ParameterWeibull"
  options$historicalThreshold <- TRUE
  options$historicalThresholdValue <- 0
  options$nullDistribution <- nullDistribution
  options$report <- report
  set.seed(1)
  runAnalysis("processCapabilityStudies", .reportFitErrorData(), options, view = FALSE)
}

# text of every text layer of the report panels, one character vector per panel
.reportFitErrorPanelTexts <- function(results) {
  report <- results[["state"]][["figures"]][[results[["results"]][["report"]][["data"]]]][["obj"]]
  lapply(report$subplots, function(panel) {
    if (!ggplot2::is_ggplot(panel))
      return(character(0))
    textLayers <- which(vapply(panel$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
    unlist(lapply(textLayers, function(i) as.character(ggplot2::layer_data(panel, i)[["label"]])))
  })
}

.reportFitErrorPanelIndex <- function(panelTexts, title) which(vapply(panelTexts, function(x) length(x) > 1 && x[1] == title, logical(1)))

.reportFitErrorIsControlChart <- function(panel) ggplot2::is_ggplot(panel) && any(vapply(panel$layers, function(l) inherits(l$geom, "GeomStep"), logical(1)))

test_that("Without the report, the failed fit shows as an error in the process summary table", {
  results <- .reportFitErrorResults(report = FALSE)
  expect_false(identical(results[["status"]], "fatalError"))
  summaryTable <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]]
  expect_identical(summaryTable[["status"]], "error")
  expect_true(nchar(summaryTable[["error"]][["errorMessage"]]) > 0)
})

for (stages in c("", "Stage")) {
  test_that(sprintf("Report keeps its elements and shows the fit error (stages: '%s')", stages), {
    results <- .reportFitErrorResults(report = TRUE, stages = stages)
    expect_false(identical(results[["status"]], "fatalError"))
    expect_false(identical(results[["results"]][["report"]][["status"]], "error"))

    report <- results[["state"]][["figures"]][[results[["results"]][["report"]][["data"]]]][["obj"]]
    expect_true(inherits(report, "jaspGraphsPlot"))
    # the X-bar and R chart of the report are still drawn
    expect_equal(sum(vapply(report$subplots, .reportFitErrorIsControlChart, logical(1))), 2)

    panelTexts <- .reportFitErrorPanelTexts(results)
    if (stages == "") {
      # one error panel for the capability plot and one in place of the capability tables
      capabilityPanel <- .reportFitErrorPanelIndex(panelTexts, "Capability of the process")
      tablesPanel <- .reportFitErrorPanelIndex(panelTexts, "Process capability tables")
    } else {
      # only stage 1 (with the zero) cannot be fitted: an error panel for its capability plot, stage 2 keeps its plot,
      # and the tables are shown with a panel that names the stage whose estimates are unavailable
      capabilityPanel <- which(vapply(panelTexts, function(x) length(x) > 1 && x[1] == "1" &&
                                        grepl("could not be estimated", paste(x, collapse = " ")), logical(1)))
      tablesPanel <- .reportFitErrorPanelIndex(panelTexts, "Some process capability estimates are unavailable")
      expect_false(any(vapply(panelTexts, function(x) x[1] %in% "2" && grepl("could not be estimated", paste(x, collapse = " ")), logical(1))))
      expect_true(any(vapply(panelTexts, function(x) "Process performance (total)" %in% x, logical(1))))
      expect_match(paste(panelTexts[[tablesPanel]], collapse = " "), "Stage 1:")
    }
    expect_length(capabilityPanel, 1)
    expect_length(tablesPanel, 1)
    expect_true(length(panelTexts[[capabilityPanel]]) > 1) # title plus the error message
    expect_true(length(panelTexts[[tablesPanel]]) > 1)
    # the probability plot (normal null distribution) does not need the failed fit and is still drawn
    expect_length(.reportFitErrorPanelIndex(panelTexts, "Probability plot"), 0)
  })
}

test_that("Report shows a failed probability plot fit as an error panel", {
  # the zero cannot be fitted by a Weibull null distribution either
  results <- .reportFitErrorResults(report = TRUE, nullDistribution = "weibull")
  expect_false(identical(results[["status"]], "fatalError"))
  panelTexts <- .reportFitErrorPanelTexts(results)
  probabilityPanel <- .reportFitErrorPanelIndex(panelTexts, "Probability plot")
  expect_length(probabilityPanel, 1)
  expect_true(length(panelTexts[[probabilityPanel]]) > 1)
})

test_that("Report explains why the three-parameter Weibull probability plot is unavailable", {
  results <- .reportFitErrorResults(report = TRUE, nullDistribution = "3ParameterWeibull")
  expect_false(identical(results[["status"]], "fatalError"))
  panelTexts <- .reportFitErrorPanelTexts(results)
  expect_length(.reportFitErrorPanelIndex(panelTexts, "Capability of the process"), 1)
  expect_length(.reportFitErrorPanelIndex(panelTexts, "Process capability tables"), 1)
  probabilityPanel <- .reportFitErrorPanelIndex(panelTexts, "Probability plot")
  expect_length(probabilityPanel, 1)
  expect_match(paste(panelTexts[[probabilityPanel]][-1], collapse = " "),
               "Probability plot and table for 3ParameterWeibull distribution are not implemented in Quality Control module.",
               fixed = TRUE)
})
