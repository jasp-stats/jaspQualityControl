context("[Quality Control] Process Capability Study - editing the subgroup size keeps the control chart type")

# The form keeps its state between option updates, like the GUI (and RPC partial updates): each update
# below sends only the option that is edited. Editing the subgroup size must not reset X-bar & R / R-bar
# to X-bar & s / S-bar (the defaults), which changed the within std. dev. and Cp.

datasetPath <- testthat::test_path("datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv")
runInfo <- jaspTools:::analysisRunInfo("processCapabilityStudies")

formUpdate <- function(options) {
  json <- as.character(jsonlite::toJSON(options, auto_unbox = TRUE, digits = NA, null = "null"))
  status <- jaspSyntax::loadQmlAndParseOptionsStatus(runInfo$moduleName, runInfo$analysisName, runInfo$qmlFile, json,
                                                    runInfo$version, runInfo$preloadData)
  parsed <- jsonlite::fromJSON(status$options, simplifyVector = FALSE)
  optionValue <- function(name) { v <- parsed[[name]]; if (is.list(v)) v[["value"]] else v }
  c(chart   = optionValue("controlChartType"),
    sdGroup = optionValue("controlChartSdEstimationMethodGroupSize"),
    sd      = optionValue("controlChartSdEstimationMethodGroupSizeLargerThanOne"))
}

startForm <- function(options) {
  jaspSyntax::clearQmlForms()
  invisible(jaspSyntax::loadDataSet(jaspTools:::loadCorrectDataset(datasetPath)))
  formUpdate(options)
}

test_that("Editing the subgroup size 5 -> 4 -> 5 keeps X-bar & R and R-bar", {
  on.exit(jaspSyntax::clearQmlForms())
  expect_equal(startForm(list(manualSubgroupSizeValue = 5, controlChartType = "xBarR",
                              controlChartSdEstimationMethodGroupSizeLargerThanOne = "rBar")),
               c(chart = "xBarR", sdGroup = "largerThanOne", sd = "rBar"))
  expect_equal(formUpdate(list(manualSubgroupSizeValue = 4)), c(chart = "xBarR", sdGroup = "largerThanOne", sd = "rBar"))
  final <- formUpdate(list(manualSubgroupSizeValue = 5))
  expect_equal(final, c(chart = "xBarR", sdGroup = "largerThanOne", sd = "rBar"))

  jaspSyntax::clearQmlForms()
  # the options the form ends with give the X-bar & R / R-bar results (LF1.5, verified with Minitab)
  options <- analysisOptions("processCapabilityStudies")
  options$measurementLongFormat <- "Diameter"
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 5
  options$controlChartType <- final[["chart"]]
  options$controlChartSdEstimationMethodGroupSizeLargerThanOne <- final[["sd"]]
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- -15 # with USL 0 as in examples/ProcessCapabilityStudyLongFormat.jasp
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 0
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", datasetPath, options, view = FALSE)
  normal <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]]
  summaryRow <- normal[["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]][[1]]
  withinRow  <- normal[["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]][[1]]
  expect_equal(summaryRow[["sdw"]], 1.76268271711092, tolerance = 1e-6)
  expect_equal(withinRow[["cp"]], 1.41829, tolerance = 1e-2) # table value is rounded; the reset gives 1.35222
  expect_equal(results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["title"]], "X-bar & R control chart")
})

test_that("Editing the subgroup size or the data format keeps X-bar & mR and a pooled std. dev.", {
  on.exit(jaspSyntax::clearQmlForms())
  startForm(list(manualSubgroupSizeValue = 5, controlChartType = "xBarMR",
                 controlChartSdEstimationMethodGroupSizeLargerThanOne = "pooled"))
  expected <- c(chart = "xBarMR", sdGroup = "largerThanOne", sd = "pooled")
  expect_equal(formUpdate(list(manualSubgroupSizeValue = 3)), expected)
  expect_equal(formUpdate(list(controlLimitsNumberOfSigmas = 2)), expected)
  expect_equal(formUpdate(list(dataFormat = "wideFormat")), expected)
  expect_equal(formUpdate(list(dataFormat = "longFormat")), expected)
})

test_that("Subgroup size 1 switches to the X-mR chart and the moving range method", {
  on.exit(jaspSyntax::clearQmlForms())
  startForm(list(manualSubgroupSizeValue = 5, controlChartType = "xBarR"))
  expect_equal(formUpdate(list(manualSubgroupSizeValue = 1))[c("chart", "sdGroup")], c(chart = "xmr", sdGroup = "equalOne"))
  expect_equal(formUpdate(list(controlLimitsNumberOfSigmas = 2))[c("chart", "sdGroup")], c(chart = "xmr", sdGroup = "equalOne"))
  expect_equal(formUpdate(list(manualSubgroupSizeValue = 4))[c("chart", "sdGroup")], c(chart = "xBarS", sdGroup = "largerThanOne"))
})
