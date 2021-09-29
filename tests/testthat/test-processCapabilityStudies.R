context("[Quality Control] Process Capability Study")

#Long format
options <- analysisOptions("processCapabilityStudies")
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartsType <- "xbarR"
options$lowerSpecificationField <- TRUE
options$rank <- "Bernard"
options$upperSpecificationField <- TRUE
options$variablesLong <- "Ovality"
set.seed(1)
results <- runAnalysis("processCapabilityStudies", "Ovality.csv", options)

test_that("Capability of the process plot matches", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process")
})

test_that("Process performance (total) table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.19, -0.46, -0.54, -0.38, 0.84, 0.17, -0.46, 0.21))
})

test_that("Non-conformance statistics table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(5719.38770585623, 3749.39583764955, 0, "ppm &lt; LSL", 916921.247171634,
                                      928397.340752607, 970000, "ppm &gt; USL", 922640.634877491,
                                      932146.736590257, 970000, "ppm total"))
})

test_that("Process capability (within) table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.2, -0.49, -0.57, -0.41, 0.89, 0.18, -0.49, 0.23))
})

test_that("Process summary table results match", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, 3.42, 100, 1.74772579501061, 1.6530524505589, 1))
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

## Probability plot
# Normal
test_that("Probability Plot matches", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot")
})
# Lognormal
options$nullDistribution <- "Lognormal"
results <- runAnalysis("processCapabilityStudies", "Ovality.csv", options)
test_that("Lognormal Probability Plot matches", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "lognormal-probability-plot")
})

# Weibull
options$nullDistribution <- "Weibull"
results <- runAnalysis("processCapabilityStudies", "Ovality.csv", options)
test_that("Weibull Probability Plot matches", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "weibull-probability-plot")
})

test_that("Summary of test against the normal distribution table results match", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.50923060404098, 3.42, 100, 0.000645146007375599, 1.74772579501061
                                 ))
})

# Wide format
options <- analysisOptions("processCapabilityStudies")
options$pcDataFormat <- "PCwideFormat"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartsType <- "xbarR"
options$lowerSpecificationField <- TRUE
options$rank <- "Bernard"
options$upperSpecificationField <- TRUE
options$variables <- c("Vd1","Vd2","Vd3", "Vd4", "Vd5")
set.seed(1)
results <- runAnalysis("processCapabilityStudies", "Ovality_wide.csv", options)

test_that("Capability of the process plot matches wide", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process-wide")
})

test_that("Process performance (total) table results match wide", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.19, -0.46, -0.54, -0.38, 0.84, 0.17, -0.46, 0.21))
})

test_that("Non-conformance statistics table results match wide", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(5719.38770585623, 3749.39583764955, 0, "ppm &lt; LSL", 916921.247171634,
                                      928397.340752607, 970000, "ppm &gt; USL", 922640.634877491,
                                      932146.736590257, 970000, "ppm total"))
})

test_that("Process capability (within) table results match wide", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.2, -0.49, -0.57, -0.41, 0.89, 0.18, -0.49, 0.23))
})

test_that("Process summary table results match wide", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, 3.42, 100, 1.74772579501061, 1.6530524505589, 1))
})

test_that("X-bar & R Chart plot matches wide", {
  plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-chart-wide")
})

test_that("Histogram plot matches wide", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-wide")
})

test_that("Probability Plot matches wide", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-wide")
})

test_that("Summary of test against the normal distribution table results match wide", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.50923060404098, 3.42, 100, 0.000645146007375599, 1.74772579501061
                                 ))
})
