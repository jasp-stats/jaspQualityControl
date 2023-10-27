context("[Quality Control] Process Capability Study")
.numDecimals <- 2

#Long format
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$subgroup <- "Time"
options$probabilityPlotRankMethod <- "bernard"
options$lowerSpecificationLimit <- TRUE
options$upperSpecificationLimit <- TRUE
options$target <- TRUE
options$lowerSpecificationLimitValue <- -16
options$targetValue <- -8
options$upperSpecificationLimitValue <- 0
options$controlChartType <- "xBarR"
set.seed(1)
results <- runAnalysis("processCapabilityStudies", "SPCSubgroups_Long.csv", options)

test_that("Capability of the process plot matches", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process")
})

test_that("Process performance (total) table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.29, 1.14, 1.44, 1.44, 1.27, 1.11, 1.43, 1.6, 1.27, 1.27, 1.6
                                 ))
})

test_that("Non-conformance statistics table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.773329467129713, 0.209114003668631, 0, "ppm &lt; LSL", 68.3893384010315,
                                      29.5212971422876, 0, "ppm &gt; USL", 69.1626678681612, 29.7304111459562,
                                      0, "ppm total"))
})

test_that("Process capability (within) table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.51, 1.34, 1.16, 1.52, 1.69, 1.32, 1.34, 1.7))
})

test_that("Process summary table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-16, -7.08, 100, 1.85635681000733, 1.76268271711092, -8, 0))
})

test_that("X-bar & R Control Chart plot matches", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart")
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
                                 list(0.430918770255289, -7.08, 100, 0.300991417646472, 1.85635681000733
                                 ))
})

# Wide format
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1", "dm2", "dm3", "dm4", "dm5")
options$controlChartType <- "xBarMR"
set.seed(1)
results <- runAnalysis("processCapabilityStudies", "SPCSubgroups_Wide.csv", options)


test_that("Capability of the process plot matches2", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process2")
})

test_that("Process performance (total) table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.29, 1.14, 1.44, 1.44, 1.27, 1.11, 1.43, 1.6, 1.27, 1.27, 1.6
                                 ))
})

test_that("Non-conformance statistics table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.773329467129713, 0.209114003668631, 0, "ppm &lt; LSL", 68.3893384010315,
                                      29.5212971422876, 0, "ppm &gt; USL", 69.1626678681612, 29.7304111459562,
                                      0, "ppm total"))
})

test_that("Process capability (within) table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.51, 1.34, 1.16, 1.52, 1.69, 1.32, 1.34, 1.7))
})

test_that("Process summary table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-16, -7.08, 100, 1.85635681000733, 1.76268271711092, -8, 0))
})

test_that("X-bar & mR Control Chart plot matches", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-mr-control-chart")
})

test_that("Histogram plot matches2", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram2")
})

test_that("Probability Plot matches2", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot2")
})

test_that("Summary of test against the normal distribution table results match", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.430918770255289, -7.08, 100, 0.300991417646472, 1.85635681000733
                                 ))
})

# Long format- Weibull
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "longFormat"
options$measurementLongFormat <- "Ovality"
options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
options$nonNormalDistribution <- "weibull"
options$nullDistribution <- "weibull"
options$probabilityPlotRankMethod <- "bernard"
options$lowerSpecificationLimit <- TRUE
options$upperSpecificationLimit <- TRUE
options$target <- FALSE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 0
options$upperSpecificationLimitValue <- 15
options$controlChartType <- "xmr"
set.seed(1)
results <- runAnalysis("processCapabilityStudies", "msaPCS_Weibull.csv", options)

test_that("X-mR Control Chart plot matches", {
  plotName <- results[["results"]][["xmr"]][["collection"]][["xmr_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart")
})

test_that("Test results for individuals chart table results match", {
  table <- results[["results"]][["xmr"]][["collection"]][["xmr_tableIndividual"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3, 11, 42, 12, 100, ""))
})

test_that("Test results for moving range chart table results match", {
  table <- results[["results"]][["xmr"]][["collection"]][["xmr_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3, 11, 42, 29, 43, 50, "", 51))
})

test_that("Non-conformance statistics table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_PerformanceNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, "ppm &lt; LSL", 0.0589189995681139, 0, "ppm &gt; USL", 0.0589189995681139,
                                      0, "Total ppm"))
})

test_that("Capability of the process plot matches3", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process3")
})

test_that("Process performance (total) table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.59, 1.05, 1.05, 1.85))
})

test_that("Process summary table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.07532784402308, 0, 3.42, 100, 1.74772579501061, 3.86889820358025,
                                      15))
})

test_that("Histogram plot matches3", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram3")
})

test_that("Probability Plot matches3", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot3")
})

test_that("Summary of test against the weibull distribution table results match", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.70609252641799, 2.07536754156247, 100, 0.063348428618532, 3.86876200879491
                                 ))
})

# Long format- Weibull
options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
options$nonNormalDistribution <- "lognormal"
options$nullDistribution <- "lognormal"
options$controlChartType <- "xBarS"
set.seed(1)
results <- runAnalysis("processCapabilityStudies", "msaPCS_Weibull.csv", options)

test_that("Non-conformance statistics table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_PerformanceNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, "ppm &lt; LSL", 1196.84133284115, 0, "ppm &gt; USL", 1196.84133284115,
                                      0, "Total ppm"))
})

test_that("Capability of the process plot matches4", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process4")
})

test_that("Process performance (total) table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.06, 1.02, 1.26, 1.02))
})

test_that("Process summary table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.10078741574583, 0, 3.42, 100, 1.74772579501061, 0.529320014694866,
                                      15))
})

test_that("X-bar & s Control Chart plot matches", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart")
})

test_that("Test results for s chart table results match", {
  table <- results[["results"]][["xBar"]][["collection"]][["xBar_tableSecondPlot"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2))
})

test_that("Histogram plot matches4", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram4")
})

test_that("Probability Plot matches4", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot4")
})

test_that("Summary of test against the lognormal distribution table results match", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.61105240529119, 1.10078741574583, 100, 0.109186903478858, 0.529320014694866
                                 ))
})
