context("[Quality Control] Process Capability Studies")

options <- analysisOptions("processCapabilityStudies")
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartsType <- "xbarR"
options$lowerSpecificationField <- TRUE
options$rank <- "Bernard"
options$upperSpecificationField <- TRUE
options$variablesLong <- "Measurement1"
set.seed(1)
results <- runAnalysis("processCapabilityStudies", "partOperatorData.csv", options)


test_that("Capability of the process plot matches", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process")
})

test_that("Process performance (total) table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.17, -1.19, -1.47, -0.92, 1.54, 0.13, -1.19, 0.21))
})

test_that("Non-conformance statistics table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.01361451190873, 5.21049869917078e-06, 0, "ppm &lt; LSL", 999828.712435699,
                                      999999.936216869, 1e+06, "ppm &gt; USL", 999830.726050211, 999999.936222079,
                                      1e+06, "ppm total"))
})

test_that("Process capability (within) table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.25, -1.76, -2.2, -1.32, 2.27, 0.19, -1.76, 0.31))
})

test_that("Process summary table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, 7.95839450136667, 30, 1.94326433986529, 1.31730184135856,
                                      1))
})

test_that("X-bar & R Chart plot matches", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-chart")
})

test_that("Histogram plot matches", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram")
})

test_that("Probability Plot matches", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot")
})

test_that("Summary of test against the normal distribution table results match", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.727071326290741, 7.95839450136667, 30, 0.0517713802760936, 1.94326433986529
                                 ))
})
