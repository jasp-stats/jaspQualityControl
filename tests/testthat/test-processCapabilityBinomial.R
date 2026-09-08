context("[Quality Control] Binomial Capability Analysis")
.numDecimals <- 2

# Shared option scaffolding ####

binomialOptions <- function(...) {
  options <- analysisOptions("processCapabilityStudies")
  options$capabilityDataType <- "attributes"
  options$attributeCounts  <- "Defectives"
  options$testSet             <- "jaspDefault"
  overrides <- list(...)
  for (name in names(overrides))
    options[[name]] <- overrides[[name]]
  return(options)
}

binomialDataset      <- function() testthat::test_path("datasets/processCapabilityStudy/binomialCapability.csv")
binomialMissingData  <- function() testthat::test_path("datasets/processCapabilityStudy/binomialCapabilityMissing.csv")
binomialEdgeData     <- function() testthat::test_path("datasets/processCapabilityStudy/binomialCapabilityEdgeCases.csv")

binomialCollection <- function(results) results[["results"]][["attributeCapability"]][["collection"]]

binomialTable <- function(results, key)
  binomialCollection(results)[[paste0("attributeCapability_", key)]][["data"]]

binomialFootnotes <- function(results, key)
  binomialCollection(results)[[paste0("attributeCapability_", key)]][["footnotes"]]

binomialPlot <- function(results, key) {
  plotName <- binomialCollection(results)[[paste0("attributeCapability_", key)]][["data"]]
  return(results[["state"]][["figures"]][[plotName]][["obj"]])
}

binomialChartElement <- function(results, key)
  binomialCollection(results)[["attributeCapability_controlChart"]][["collection"]][[paste0("attributeCapability_controlChart_", key)]]

# 1. Summary table, constant sample size ####

options <- binomialOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 50)
set.seed(1)
resultsConstant <- runAnalysis("processCapabilityStudies", binomialDataset(), options)

test_that("B1 Summary statistics table with a constant sample size", {
  # 92 defectives out of 25 * 50 = 1250 inspected units, exact (Clopper-Pearson) interval
  table <- binomialTable(resultsConstant, "summaryTable")
  jaspTools::expect_equal_tables(table,
                                 list(5.9741250430, 8.9502194335, "Defective (%)", 7.36,
                                      59741.2504301, 89502.1943348, "PPM defective", 73600,
                                      1.3438268382, 1.5569493940, "Process Z", 1.4494928344))
})

# 2. Summary table, variable sample size ####

options <- binomialOptions(attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize")
set.seed(1)
resultsVariable <- runAnalysis("processCapabilityStudies", binomialDataset(), options)

test_that("B2 Summary statistics table with a variable sample size", {
  # 92 defectives out of 1345 inspected units
  table <- binomialTable(resultsVariable, "summaryTable")
  jaspTools::expect_equal_tables(table,
                                 list(5.5494128152, 8.3231084788, "Defective (%)", 6.8401486989,
                                      55494.1281507, 83231.0847878, "PPM defective", 68401.4869888,
                                      1.3836613625, 1.5937668887, "Process Z", 1.4878025463))
})

# 3. Confidence interval methods ####

test_that("B3 Wald interval bounds", {
  options <- binomialOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 50,
                             binomialCiMethod = "wald")
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialDataset(), options)
  table <- binomialTable(results, "summaryTable")
  jaspTools::expect_equal_tables(table,
                                 list(5.9124576955, 8.8075423045, "Defective (%)", 7.36,
                                      59124.5769546, 88075.4230454, "PPM defective", 73600,
                                      1.3527020162, 1.5621648709, "Process Z", 1.4494928344))
})

test_that("B3 Wilson interval bounds", {
  options <- binomialOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 50,
                             binomialCiMethod = "wilson")
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialDataset(), options)
  table <- binomialTable(results, "summaryTable")
  jaspTools::expect_equal_tables(table,
                                 list(6.0394232282, 8.9418535113, "Defective (%)", 7.36,
                                      60394.2322820, 89418.5351132, "PPM defective", 73600,
                                      1.3443443194, 1.5514726173, "Process Z", 1.4494928344))
})

test_that("B3 The CI level is read as a proportion, not as a percentage", {
  # regression guard: a CIField delivers 0.95, so alpha must be 1 - 0.95 and the overtitle "95% CI"
  schema <- binomialCollection(resultsConstant)[["attributeCapability_summaryTable"]][["schema"]][["fields"]]
  overtitles <- unique(unlist(lapply(schema, function(field) field[["overTitle"]])))
  expect_true("95% CI" %in% overtitles)
})

# 4. Historical proportion affects the chart only ####

test_that("B4 A historical proportion moves the centre line but not the statistics", {
  options <- binomialOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 50,
                             binomialHistoricalProportion = TRUE, binomialHistoricalProportionValue = 2)
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialDataset(), options)

  # the point estimate and its interval stay data based
  table <- binomialTable(results, "summaryTable")
  jaspTools::expect_equal_tables(table,
                                 list(5.9741250430, 8.9502194335, "Defective (%)", 7.36,
                                      59741.2504301, 89502.1943348, "PPM defective", 73600,
                                      1.3438268382, 1.5569493940, "Process Z", 1.4494928344))

  footnotes <- unlist(lapply(binomialFootnotes(results, "summaryTable"), `[[`, "text"))
  expect_true(any(grepl("historical proportion defective of 2%", footnotes, fixed = TRUE)))
})

# 5. p chart ####

test_that("B5 p chart plot", {
  plotObject <- binomialChartElement(resultsVariable, "plot")[["data"]]
  testPlot <- resultsVariable[["state"]][["figures"]][[plotObject]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "binomial-p-chart")
})

test_that("B5 p chart test results table", {
  table <- binomialChartElement(resultsVariable, "table")[["data"]]
  jaspTools::expect_equal_tables(table, list("Point 14"))
})

# 6. Supporting plots ####

test_that("B6 Cumulative defective plot", {
  plotObject <- binomialPlot(resultsVariable, "cumulativePlot")
  jaspTools::expect_equal_plots(plotObject, "binomial-cumulative")
})

test_that("B6 Binomial plot and rate plot", {
  options <- binomialOptions(attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize",
                             attributeDistributionPlot = TRUE, attributeRatePlot = TRUE)
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialDataset(), options)
  jaspTools::expect_equal_plots(binomialPlot(results, "distributionPlot"), "binomial-distribution")
  jaspTools::expect_equal_plots(binomialPlot(results, "ratePlot"), "binomial-rate")
})

test_that("B6 Histogram of the percentage defective", {
  options <- binomialOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 50,
                             attributeHistogram = TRUE)
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialDataset(), options)
  jaspTools::expect_equal_plots(binomialPlot(results, "histogram"), "binomial-histogram")
})

test_that("B6 Histogram with a target", {
  # the target is drawn as a dashed line and, being outside the observed range, also widens the x axis
  options <- binomialOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 50,
                             attributeHistogram = TRUE, binomialTarget = TRUE, binomialTargetValue = 2)
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialDataset(), options)
  jaspTools::expect_equal_plots(binomialPlot(results, "histogram"), "binomial-histogram-target")
})

test_that("B6 The number of bins of the histogram follows the GUI", {
  binWidth <- function(nBins) {
    options <- binomialOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 50,
                               attributeHistogram = TRUE, attributeHistogramBinNumber = nBins)
    set.seed(1)
    results <- runAnalysis("processCapabilityStudies", binomialDataset(), options)
    layer <- ggplot2::layer_data(binomialPlot(results, "histogram"), 1)
    # the spacing of the bin centres, which survives the bars clipped by the axis limits
    return(min(diff(sort(layer[["x"]]))))
  }
  # hist() rounds the boundaries, so the option controls the bin width rather than an exact count
  expect_lt(binWidth(25), binWidth(10))
})

# 7. Element keys follow the QML gating ####

test_that("B7 The two sample-size dependent panels are separate elements", {
  # QML only offers the histogram for a constant sample size and the rate plot for a variable one,
  # so each has its own element key and neither can be silently substituted for the other
  options <- binomialOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 50,
                             attributeHistogram = TRUE)
  set.seed(1)
  constant <- runAnalysis("processCapabilityStudies", binomialDataset(), options)
  expect_true("attributeCapability_histogram" %in% names(binomialCollection(constant)))
  expect_false("attributeCapability_ratePlot" %in% names(binomialCollection(constant)))

  options <- binomialOptions(attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize",
                             attributeRatePlot = TRUE)
  set.seed(1)
  variable <- runAnalysis("processCapabilityStudies", binomialDataset(), options)
  expect_true("attributeCapability_ratePlot" %in% names(binomialCollection(variable)))
  expect_false("attributeCapability_histogram" %in% names(binomialCollection(variable)))
})

test_that("B7 A panel that does not match the sample size type is dropped", {
  # QML hides the check box that does not apply but keeps its value, so a histogram ticked under a
  # constant sample size must not survive the switch to a variable one, and vice versa
  options <- binomialOptions(attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize",
                             attributeHistogram = TRUE, attributeRatePlot = TRUE)
  set.seed(1)
  variable <- runAnalysis("processCapabilityStudies", binomialDataset(), options)
  expect_false("attributeCapability_histogram" %in% names(binomialCollection(variable)))
  expect_true("attributeCapability_ratePlot" %in% names(binomialCollection(variable)))

  options <- binomialOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 50,
                             attributeHistogram = TRUE, attributeRatePlot = TRUE)
  set.seed(1)
  constant <- runAnalysis("processCapabilityStudies", binomialDataset(), options)
  expect_false("attributeCapability_ratePlot" %in% names(binomialCollection(constant)))
  expect_true("attributeCapability_histogram" %in% names(binomialCollection(constant)))
})

# 8. Error paths ####

test_that("B8 More defectives than inspected units is reported for a single sample", {
  options <- binomialOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 5)
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialDataset(), options)
  # regression guard: %i throws on the doubles that come out of the data reader, %s must be used
  expect_match(results[["results"]][["errorMessage"]],
               "Sample 14 has more defectives (12) than inspected units (5).", fixed = TRUE)
})

test_that("B8 More defectives than inspected units reports the number of affected samples", {
  options <- binomialOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 2)
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialDataset(), options)
  expect_match(results[["results"]][["errorMessage"]], "samples are affected in total", fixed = TRUE)
})

test_that("B8 Non-integer defectives are rejected", {
  options <- binomialOptions(attributeCounts = "DefectivesFractional",
                             attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize")
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialEdgeData(), options)
  expect_match(results[["results"]][["errorMessage"]],
               "The number of defectives must contain whole numbers.", fixed = TRUE)
})

test_that("B8 A sample size below one is rejected", {
  options <- binomialOptions(attributeSampleSizeType = "variable",
                             attributeSampleSizeVariable = "SampleSizeInvalid")
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialEdgeData(), options)
  expect_match(results[["results"]][["errorMessage"]],
               "The sample size must be a positive whole number.", fixed = TRUE)
})

# 9. Degenerate process ####

test_that("B9 Zero observed defectives does not crash and is flagged", {
  options <- binomialOptions(attributeCounts = "DefectivesZero",
                             attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize")
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialEdgeData(), options)
  expect_equal(results[["status"]], "complete")
  footnotes <- unlist(lapply(binomialFootnotes(results, "summaryTable"), `[[`, "text"))
  expect_true(any(grepl("No defectives were observed", footnotes, fixed = TRUE)))
})

# 10. Missing values keep the sample numbering ####

test_that("B10 A missing sample does not shift the point numbers", {
  options <- binomialOptions(attributeSampleSizeType = "constant", attributeSampleSizeValue = 50)
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialMissingData(), options)
  # row 3 is missing and row 7 is out of control; the violation must stay "Point 7"
  table <- binomialChartElement(results, "table")[["data"]]
  jaspTools::expect_equal_tables(table, list("Point 7"))

  footnotes <- unlist(lapply(binomialFootnotes(results, "summaryTable"), `[[`, "text"))
  expect_true(any(grepl("1 sample with missing values", footnotes, fixed = TRUE)))
})

# 11. Zone based rules stay off on a clamped p chart ####

test_that("B11 Rules 4, 5, 6, 7 and 9 do not fire when the lower limit is clamped", {
  options <- binomialOptions(attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize",
                             testSet = "nelsonLaws")
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialDataset(), options)
  schema <- binomialChartElement(results, "table")[["schema"]][["fields"]]
  columns <- unlist(lapply(schema, `[[`, "name"))
  expect_false(any(c("test4", "test5", "test6", "test7", "test9") %in% columns))
})

# 12. Out-of-control footnote ####

test_that("B12 An out-of-control point is flagged on the summary table", {
  footnotes <- unlist(lapply(binomialFootnotes(resultsVariable, "summaryTable"), `[[`, "text"))
  expect_true(any(grepl("The process is not in control", footnotes, fixed = TRUE)))
})

test_that("B12 A stable process carries no out-of-control footnote", {
  options <- binomialOptions(attributeCounts = "Defectives",
                             attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize")
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialEdgeData(), options)
  footnotes <- unlist(lapply(binomialFootnotes(results, "summaryTable"), `[[`, "text"))
  expect_false(any(grepl("The process is not in control", footnotes, fixed = TRUE)))
})

# 13. Empty state ####

test_that("B13 Without an assigned variable the analysis renders empty output", {
  options <- binomialOptions(attributeCounts = "")
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialDataset(), options)
  expect_equal(results[["status"]], "complete")

  summaryTable <- binomialCollection(results)[["attributeCapability_summaryTable"]]
  expect_equal(length(summaryTable[["data"]]), 0)
  expect_equal(length(summaryTable[["schema"]][["fields"]]), 4)
  expect_null(summaryTable[["error"]])
})

# 14. Report ####

test_that("B14 Report", {
  options <- binomialOptions(attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize",
                             report = TRUE, attributeDistributionPlot = TRUE)
  options$reportTitleText     <- "Binomial capability"
  options$reportLocationText  <- "Amsterdam"
  options$reportLineText      <- "Line 1"
  options$reportMachineText   <- "Machine 1"
  options$reportVariableText  <- "Defectives"
  options$reportProcessText   <- "Process 1"
  options$reportDateText      <- "2026-01-01"
  options$reportReportedByText <- "JASP"
  options$reportConclusionText <- "In control"
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialDataset(), options)
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "binomial-report")
})

test_that("B14 Report without components selected shows an error", {
  options <- binomialOptions(attributeSampleSizeType = "variable", attributeSampleSizeVariable = "SampleSize",
                             report = TRUE)
  options$reportProcessStability        <- FALSE
  options$reportProcessCapabilityPlot   <- FALSE
  options$reportProcessCapabilityTables <- FALSE
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", binomialDataset(), options)
  expect_match(results[["results"]][["report"]][["error"]][["errorMessage"]],
               "No report components selected.", fixed = TRUE)
})
