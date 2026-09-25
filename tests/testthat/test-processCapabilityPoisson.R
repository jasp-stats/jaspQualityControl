context("[Quality Control] Poisson Capability Analysis")
.numDecimals <- 2

# Shared option scaffolding ####

poissonOptions <- function(...) {
  options <- analysisOptions("processCapabilityStudies")
  options$capabilityDataType    <- "attributes"
  options$attributeDistribution <- "poisson"
  options$attributeCounts       <- "Defects"
  options$testSet               <- "jaspDefault"
  # GUI defaults of the Poisson controls, set explicitly rather than relying on analysisOptions()
  # deriving them from the QML, because the controls carry visibility bindings
  options$poissonHistoricalDpu      <- FALSE
  options$poissonHistoricalDpuValue <- 1
  options$poissonTarget             <- FALSE
  options$poissonTargetValue        <- 0
  options$poissonCiMethod           <- "exact"
  options$poissonYieldStatistics    <- FALSE
  overrides <- list(...)
  for (name in names(overrides))
    options[[name]] <- overrides[[name]]
  return(options)
}

poissonDataset     <- function() testthat::test_path("datasets/processCapabilityStudy/poissonCapability.csv")
poissonMissingData <- function() testthat::test_path("datasets/processCapabilityStudy/poissonCapabilityMissing.csv")
poissonEdgeData    <- function() testthat::test_path("datasets/processCapabilityStudy/poissonCapabilityEdgeCases.csv")

poissonCollection <- function(results) results[["results"]][["attributeCapability"]][["collection"]]

poissonTable <- function(results, key)
  poissonCollection(results)[[paste0("attributeCapability_", key)]][["data"]]

poissonFootnotes <- function(results, key)
  unlist(lapply(poissonCollection(results)[[paste0("attributeCapability_", key)]][["footnotes"]], `[[`, "text"))

poissonPlot <- function(results, key) {
  plotName <- poissonCollection(results)[[paste0("attributeCapability_", key)]][["data"]]
  return(results[["state"]][["figures"]][[plotName]][["obj"]])
}

poissonChartElement <- function(results, key)
  poissonCollection(results)[["attributeCapability_controlChart"]][["collection"]][[paste0("attributeCapability_controlChart_", key)]]

# 1. Summary table, constant sample size ####

options <- poissonOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 10)
set.seed(1)
resultsConstant <- runAnalysis("processCapabilityStudies", poissonDataset(), options)

test_that("P1 Summary statistics table with a constant sample size", {
  # 344 defects over 25 * 10 = 250 inspected units, exact (Garwood) interval
  table <- poissonTable(resultsConstant, "summaryTable")
  jaspTools::expect_equal_tables(table,
                                 list(1.2344172176, 1.5293696715, "Mean DPU", 1.376))
})

# 2. Summary table, variable sample size ####

options <- poissonOptions(attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize")
set.seed(1)
resultsVariable <- runAnalysis("processCapabilityStudies", poissonDataset(), options)

test_that("P2 Summary statistics table with a variable sample size", {
  # 344 defects over 282 inspected units
  table <- poissonTable(resultsVariable, "summaryTable")
  jaspTools::expect_equal_tables(table,
                                 list(1.0943415050, 1.3558241768, "Mean DPU", 1.2198581560))
})

# 3. Confidence interval methods ####

test_that("P3 Wald interval bounds", {
  options <- poissonOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 10,
                            poissonCiMethod = "wald")
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", poissonDataset(), options)
  jaspTools::expect_equal_tables(poissonTable(results, "summaryTable"),
                                 list(1.2305923339, 1.5214076661, "Mean DPU", 1.376))
})

test_that("P3 Score interval bounds", {
  options <- poissonOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 10,
                            poissonCiMethod = "score")
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", poissonDataset(), options)
  jaspTools::expect_equal_tables(poissonTable(results, "summaryTable"),
                                 list(1.2380724215, 1.5292934137, "Mean DPU", 1.376))
})

test_that("P3 The three interval methods are each named in a footnote", {
  method <- function(m) {
    options <- poissonOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 10,
                              poissonCiMethod = m)
    set.seed(1)
    poissonFootnotes(runAnalysis("processCapabilityStudies", poissonDataset(), options), "summaryTable")
  }
  expect_true(any(grepl("exact (Garwood)", method("exact"), fixed = TRUE)))
  expect_true(any(grepl("Wald",            method("wald"),  fixed = TRUE)))
  expect_true(any(grepl("score method",    method("score"), fixed = TRUE)))
})

# 4. The CI level is a proportion, not a percentage ####

test_that("P4 The CI level is read as a proportion", {
  # regression guard: a CIField delivers 0.95, so alpha must be 1 - 0.95 and the overtitle "95% CI"
  schema <- poissonCollection(resultsConstant)[["attributeCapability_summaryTable"]][["schema"]][["fields"]]
  overtitles <- unique(unlist(lapply(schema, function(field) field[["overTitle"]])))
  expect_true("95% CI" %in% overtitles)
})

# 5. Yield statistics are opt-in ####

test_that("P5 Without the check box only the mean DPU is reported", {
  table <- poissonTable(resultsConstant, "summaryTable")
  expect_equal(length(table), 1)
})

test_that("P5 The yield rows are added and the Z bounds are the swap of the DPU bounds", {
  options <- poissonOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 10,
                            poissonYieldStatistics = TRUE)
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", poissonDataset(), options)
  table <- poissonTable(results, "summaryTable")
  jaspTools::expect_equal_tables(table,
                                 list(1.2344172176,     1.5293696715,     "Mean DPU",            1.376,
                                      70.8995694867,    78.3327801053,    "Defective units (%)", 74.7413117413,
                                      708995.694867111, 783327.801052881, "PPM defective",       747413.117413390,
                                      -0.7834815262,    -0.5504531384,    "Process Z",           -0.6663713580))

  # Z decreases in the rate, so the upper DPU bound produces the lower Z bound
  zLower <- qnorm(-expm1(-1.5293696715), lower.tail = FALSE)
  zUpper <- qnorm(-expm1(-1.2344172176), lower.tail = FALSE)
  expect_equal(table[[4]][["ciLower"]], zLower, tolerance = 1e-8)
  expect_equal(table[[4]][["ciUpper"]], zUpper, tolerance = 1e-8)

  expect_true(any(grepl("a unit is conforming when it carries no defect",
                        poissonFootnotes(results, "summaryTable"), fixed = TRUE)))
})

# 6. Historical DPU affects the chart only, and is not divided by 100 ####

test_that("P6 A historical DPU moves the centre line but not the statistics", {
  options <- poissonOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 10,
                            poissonHistoricalDpu = TRUE, poissonHistoricalDpuValue = 2)
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", poissonDataset(), options)

  # the point estimate and its interval stay data based
  jaspTools::expect_equal_tables(poissonTable(results, "summaryTable"),
                                 list(1.2344172176, 1.5293696715, "Mean DPU", 1.376))

  expect_true(any(grepl("historical defect rate of 2 defects per unit",
                        poissonFootnotes(results, "summaryTable"), fixed = TRUE)))

  # regression guard for X3: the Poisson value is a rate, so it must not be divided by 100.
  # Layer 1 of the control chart is the centre line step.
  plotObject <- poissonChartElement(results, "plot")[["data"]]
  testPlot   <- results[["state"]][["figures"]][[plotObject]][["obj"]]
  centre     <- unique(na.omit(ggplot2::ggplot_build(testPlot)$data[[1]][["y"]]))
  expect_equal(as.numeric(centre), 2, tolerance = 1e-8)
})

# 7. u chart ####

test_that("P7 u chart plot", {
  plotObject <- poissonChartElement(resultsVariable, "plot")[["data"]]
  testPlot   <- resultsVariable[["state"]][["figures"]][[plotObject]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "poisson-u-chart")
})

test_that("P7 u chart test results table", {
  table <- poissonChartElement(resultsVariable, "table")[["data"]]
  jaspTools::expect_equal_tables(table, list("Point 14"))
})

test_that("P7 The test results table is titled for the u chart", {
  expect_match(poissonChartElement(resultsVariable, "table")[["title"]], "u chart", fixed = TRUE)
})

# 8. Supporting plots, all on the DPU scale ####

test_that("P8 Cumulative defects per unit plot", {
  jaspTools::expect_equal_plots(poissonPlot(resultsVariable, "cumulativePlot"), "poisson-cumulative")
})

test_that("P8 Poisson plot and rate plot", {
  options <- poissonOptions(attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize",
                            attributeDistributionPlot = TRUE, attributeRatePlot = TRUE)
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", poissonDataset(), options)
  jaspTools::expect_equal_plots(poissonPlot(results, "distributionPlot"), "poisson-distribution")
  jaspTools::expect_equal_plots(poissonPlot(results, "ratePlot"), "poisson-rate")

  # P7 scale convention: the rate plot is in DPU, not in percent
  ratePlotData <- ggplot2::layer_data(poissonPlot(results, "ratePlot"), 2)
  expect_equal(max(ratePlotData[["y"]]), 2.9, tolerance = 1e-8)
})

test_that("P8 Histogram of the defects per unit", {
  options <- poissonOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 10,
                            attributeHistogram = TRUE)
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", poissonDataset(), options)
  jaspTools::expect_equal_plots(poissonPlot(results, "histogram"), "poisson-histogram")
})

test_that("P8 Histogram with a target in DPU", {
  options <- poissonOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 10,
                            attributeHistogram = TRUE, poissonTarget = TRUE, poissonTargetValue = 0.5)
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", poissonDataset(), options)
  jaspTools::expect_equal_plots(poissonPlot(results, "histogram"), "poisson-histogram-target")

  # the target is tabulated in DPU and carries a value but no interval
  table <- poissonTable(results, "summaryTable")
  expect_equal(vapply(table, `[[`, character(1), "statistic"), c("Mean DPU", "Target DPU"))
  expect_equal(table[[2]][["value"]], 0.5, tolerance = 1e-8)
})

# 9. Defects may exceed the sample size ####

test_that("P9 More defects than inspected units analyses cleanly and gives a DPU above one", {
  # 18 of the 25 samples carry more defects than inspected units, which is legal for a defect rate
  # and must not raise the binomial "more defectives than inspected units" error
  expect_equal(resultsVariable[["status"]], "complete")
  expect_null(resultsVariable[["results"]][["errorMessage"]])
  expect_gt(poissonTable(resultsVariable, "summaryTable")[[1]][["value"]], 1)
})

# 10. Validation differs from the binomial mode ####

test_that("P10 A fractional sample size column is accepted", {
  options <- poissonOptions(attributeSampleSizeType = "variable",
                            attributeSampleSizeVariable = "SampleSizeFractional")
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", poissonEdgeData(), options)
  expect_equal(results[["status"]], "complete")
  expect_null(results[["results"]][["errorMessage"]])
})

test_that("P10 Non-integer defects are rejected", {
  options <- poissonOptions(attributeCounts = "DefectsFractional",
                            attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize")
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", poissonEdgeData(), options)
  expect_match(results[["results"]][["errorMessage"]],
               "The number of defects must contain whole numbers.", fixed = TRUE)
})

test_that("P10 A zero sample size is excluded as a missing sample", {
  # the shared reader blanks a sample with nothing inspected rather than rejecting it, as in the
  # binomial mode; the sample keeps its row so the point numbering does not shift
  options <- poissonOptions(attributeSampleSizeType = "variable",
                            attributeSampleSizeVariable = "SampleSizeZero")
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", poissonEdgeData(), options)
  expect_equal(results[["status"]], "complete")
  expect_true(any(grepl("1 sample with missing values", poissonFootnotes(results, "summaryTable"), fixed = TRUE)))
})

# 11. Degenerate zero-defect process ####

test_that("P11 Zero observed defects renders, reports no violations and is flagged", {
  options <- poissonOptions(attributeCounts = "DefectsZero",
                            attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize")
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", poissonEdgeData(), options)
  expect_equal(results[["status"]], "complete")

  # every statistic equals the collapsed centre line, and .nelsonLaws uses strict comparisons, so
  # nothing may be flagged
  jaspTools::expect_equal_tables(poissonChartElement(results, "table")[["data"]],
                                 list("No test violations occurred."))

  footnotes <- poissonFootnotes(results, "summaryTable")
  expect_true(any(grepl("No defects were observed", footnotes, fixed = TRUE)))
  expect_false(any(grepl("The process is not in control", footnotes, fixed = TRUE)))

  # only the upper bound is informative
  jaspTools::expect_equal_tables(poissonTable(results, "summaryTable"),
                                 list(0, 0.0414480837541, "Mean DPU", 0))
})

# 12. The u chart limits agree with qcc ####

test_that("P12 The u chart limits match qcc at three sigma", {
  data <- read.csv(poissonDataset())
  reference <- qcc::qcc(data$Defects, sizes = data$SampleSize, type = "u", plot = FALSE)

  plotObject <- poissonChartElement(resultsVariable, "plot")[["data"]]
  testPlot   <- resultsVariable[["state"]][["figures"]][[plotObject]][["obj"]]
  built      <- ggplot2::ggplot_build(testPlot)$data
  # layers 1 to 3 of .controlChart_plotting are the centre, UCL and LCL step lines
  expect_equal(sort(unique(round(built[[2]][["y"]], 8))),
               sort(unique(round(reference$limits[, "UCL"], 8))))
  expect_equal(sort(unique(round(built[[3]][["y"]], 8))),
               sort(unique(round(reference$limits[, "LCL"], 8))))
  # the lower limit is clamped at zero but the upper one is not
  expect_gte(min(built[[3]][["y"]]), 0)
})

# 13. Missing values keep the sample numbering ####

test_that("P13 A missing sample does not shift the point numbers", {
  options <- poissonOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 10)
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", poissonMissingData(), options)
  # row 3 is missing and row 7 is out of control; the violation must stay "Point 7"
  jaspTools::expect_equal_tables(poissonChartElement(results, "table")[["data"]], list("Point 7"))
  expect_true(any(grepl("1 sample with missing values", poissonFootnotes(results, "summaryTable"), fixed = TRUE)))
})

# 14. Zone based rules stay off on a clamped u chart ####

test_that("P14 Rules 4, 5, 6, 7 and 9 do not fire on the u chart", {
  options <- poissonOptions(attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize",
                            testSet = "nelsonLaws")
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", poissonDataset(), options)
  schema  <- poissonChartElement(results, "table")[["schema"]][["fields"]]
  columns <- unlist(lapply(schema, `[[`, "name"))
  expect_false(any(c("test4", "test5", "test6", "test7", "test9") %in% columns))
})

# 15. Element keys follow the sample-size type ####

test_that("P15 The two sample-size dependent panels are separate elements", {
  options <- poissonOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 10,
                            attributeHistogram = TRUE, attributeRatePlot = TRUE)
  set.seed(1)
  constant <- runAnalysis("processCapabilityStudies", poissonDataset(), options)
  expect_true("attributeCapability_histogram" %in% names(poissonCollection(constant)))
  expect_false("attributeCapability_ratePlot" %in% names(poissonCollection(constant)))

  options <- poissonOptions(attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize",
                            attributeHistogram = TRUE, attributeRatePlot = TRUE)
  set.seed(1)
  variable <- runAnalysis("processCapabilityStudies", poissonDataset(), options)
  expect_true("attributeCapability_ratePlot" %in% names(poissonCollection(variable)))
  expect_false("attributeCapability_histogram" %in% names(poissonCollection(variable)))
})

# 16. Empty state ####

test_that("P16 Without an assigned variable the analysis renders empty output", {
  options <- poissonOptions(attributeCounts = "")
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", poissonDataset(), options)
  expect_equal(results[["status"]], "complete")

  summaryTable <- poissonCollection(results)[["attributeCapability_summaryTable"]]
  expect_equal(length(summaryTable[["data"]]), 0)
  expect_equal(length(summaryTable[["schema"]][["fields"]]), 4)
  expect_null(summaryTable[["error"]])
})

# 17. Report ####

test_that("P17 Report", {
  options <- poissonOptions(attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize",
                            report = TRUE, attributeDistributionPlot = TRUE)
  options$reportTitleText      <- "Poisson capability"
  options$reportLocationText   <- "Amsterdam"
  options$reportLineText       <- "Line 1"
  options$reportMachineText    <- "Machine 1"
  options$reportVariableText   <- "Defects"
  options$reportProcessText    <- "Process 1"
  options$reportDateText       <- "2026-01-01"
  options$reportReportedByText <- "JASP"
  options$reportConclusionText <- "In control"
  set.seed(1)
  results  <- runAnalysis("processCapabilityStudies", poissonDataset(), options)
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "poisson-report")
})

test_that("P17 Report without components selected shows an error", {
  options <- poissonOptions(attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize",
                            report = TRUE)
  options$reportProcessStability        <- FALSE
  options$reportProcessCapabilityPlot   <- FALSE
  options$reportProcessCapabilityTables <- FALSE
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", poissonDataset(), options)
  expect_match(results[["results"]][["report"]][["error"]][["errorMessage"]],
               "No report components selected.", fixed = TRUE)
})

# 18. The shared branch is fully switched by attributeDistribution ####

test_that("P18 Switching to the binomial distribution produces the binomial output", {
  # same fixture and a sample size large enough for the counts to be valid defective counts; only
  # attributeDistribution differs, so any Poisson row surviving here would prove a missed branch
  options <- poissonOptions(attributeDistribution = "binomial",
                            attributeSampleSizeType = "constant", attributeSampleSizeValue = 50)
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", poissonDataset(), options)
  expect_equal(results[["status"]], "complete")

  statistics <- vapply(poissonTable(results, "summaryTable"), `[[`, character(1), "statistic")
  expect_equal(statistics, c("Defective (%)", "PPM defective", "Process Z"))
  expect_false("Mean DPU" %in% statistics)

  # the p chart, not the u chart, and the binomial interval footnote
  expect_match(poissonChartElement(results, "table")[["title"]], "p chart", fixed = TRUE)
  expect_true(any(grepl("Clopper-Pearson", poissonFootnotes(results, "summaryTable"), fixed = TRUE)))
})
