context("[Quality Control] Process Capability Study")
.numDecimals <- 2

# Long / Column format ####

## Basic tests ####

### Normal capability analysis ####

#### Subgroup Variable ####

##### Without stages (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "groupingVariable"
options$subgroup <- "Time"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF1.1 (Normal) Basic tests of process capability plot with subgroup variable and no stages", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process1")
})

test_that("LF1.2 (Normal) Basic tests of Process performance (total) table with subgroup variable and no stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
		list(0.93, 0.83, 1.04, 1.08, 0.88, 0.77, 1, 1.27, 0.95, 0.88, 1.2
			))
})

test_that("LF1.3 (Normal) Basic tests of Non-conformance statistics table with subgroup variable and no stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(68.39, 29.52, 0, "ppm &lt; LSL", 4020.39, 2625.65, 10000, "ppm &gt; USL",
                                      4088.78, 2655.17, 10000, "ppm total"))
})

test_that("LF1.4 (Normal) Basic tests of Process capability (within) table with subgroup variable and no stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.13, 0.93, 0.8, 1.06, 1.34, 0.99, 0.93, 1.28))
})

test_that("LF1.5 (Normal) Basic tests of Process summary table with subgroup variable and no stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.08, 100, 1.85635681000733, 1.76268271711092, 6, 12))
})

test_that("LF1.6 (Normal) Basic tests of distribution histogram with subgroup variable and no stages", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram1")
})

test_that("LF1.7 (Normal) Basic tests of Probability plot against normal distribution with subgroup variable and no stages", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distribution1")
})

test_that("LF1.8 (Normal) Basic tests of Summary of test against the normal distribution table with subgroup variable and no stages", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.43, 7.08, 100, 0.3, 1.85635681000733))
})

test_that("LF1.9 (Normal) Basic tests of X-bar & R control chart with subgroup variable and no stages", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart1")
})

##### With stages (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "groupingVariable"
options$subgroup <- "Time"
options$stagesLongFormat <- "Stage"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF2.1 (Normal) Basic tests of process capability plot with subgroup variable and stages", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process2")
})

test_that("LF2.2 (Normal) Basic tests of Process performance (total) table with subgroup variable and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.89, 0.73, 1.05, 1, 0.82, 0.65, 1, 1.18, 0.81, 0.82, 1.19, "1 (BL)",
                                      0.97, "-", "-", 1.13, 0.92, 0.77, 1.08, 1.33, 0.95, 0.92, 1.29,
                                      2, 0.08, 0.1, 0.06, 0.13, 0.1, "-", "-", 0.15, "-", 0.1, "-",
                                      "Change (2 vs. BL)"))
})

test_that("LF2.3 (Normal) Basic tests of Non-conformance statistics table with subgroup variable and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(192.5, 33.43, -159.07, 76.21, 14.21, -62, 0, 0, 0, "ppm &lt; LSL",
                                      6851.85, 2782.57, -4069.28, 4276.16, 1804.5, -2471.66, 0, 16666.67,
                                      16666.67, "ppm &gt; USL", 7044.35, 2816, -4228.35, 4352.37,
                                      1818.71, -2533.66, 0, 16666.67, 16666.67, "ppm total"))
})

test_that("LF2.4 (Normal) Basic tests of Process capability (within) table with subgroup variable and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.07, 0.88, 0.68, 1.08, 1.26, 0.85, 0.88, 1.29, "1 (BL)", 1.18,
                                      0.97, 0.79, 1.15, 1.4, 0.99, 0.97, 1.38, 2, 0.11, 0.09, "-",
                                      "-", 0.14, "-", 0.09, "-", "Change (2 vs. BL)"))
})

test_that("LF2.5 (Normal) Basic tests of Process summary table with subgroup variable and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.0825, 40, 1.9949792750032, 1.87016337059329, "1 (BL)", 6,
                                      12, 0, 7.07833333333333, 60, 1.77526237417885, 1.69102894812267,
                                      2, 6, 12, "-", -0.00416666666666643, 20, -0.21971690082435,
                                      -0.179134422470622, "Change (2 vs. BL)", "-", "-"))
})

test_that("LF2.6 (Normal) Basic tests of distribution histogram with subgroup variable and stages", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram2")
})

test_that("LF2.7 (Normal) Basic tests of Probability plot against normal distribution with subgroup variable and stages", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distribution2")
})

test_that("LF2.8 (Normal) Basic tests of Summary of test against the normal distribution table with subgroup variable and stages", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.31, 7.0825, 40, 0.54, 1.9949792750032, "1 (BL)", 0.51, 7.07833333333333,
                                      60, 0.19, 1.77526237417885, 2, "-", -0.00416666666666643, 20,
                                      "-", -0.21971690082435, "Change (2 vs. BL)"))
})

test_that("LF2.9 (Normal) Basic tests of Summary of X-bar & R control chart with subgroup variable and stages", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart2")
})

#### Manual subgroups ####

##### Without stages (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$subgroup <- "Time"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF3.1 (Normal) Basic tests of process capability plot with manual subgroups and no stages", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process3")
})

test_that("LF3.2 (Normal) Basic tests of Process performance (total) table with manual subgroups and no stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.93, 0.83, 1.04, 1.08, 0.88, 0.77, 1, 1.27, 0.95, 0.88, 1.2
                                 ))
})

test_that("LF3.3 (Normal) Basic tests of Non-conformance statistics table with manual subgroups and no stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(68.39, 29.52, 0, "ppm &lt; LSL", 4020.39, 2625.65, 10000, "ppm &gt; USL",
                                      4088.78, 2655.17, 10000, "ppm total"))
})

test_that("LF3.4 (Normal) Basic tests of Process capability (within) table with manual subgroups and no stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.13, 0.93, 0.8, 1.06, 1.34, 0.99, 0.93, 1.28))
})

test_that("LF3.5 (Normal) Basic tests of Process summary table with manual subgroups and no stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.08, 100, 1.85635681000733, 1.76268271711092, 6, 12))
})

test_that("LF3.6 (Normal) Basic tests of distribution histogram with manual subgroups and no stages", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram3")
})

test_that("LF3.7 (Normal) Basic tests of Probability plot against normal distribution with manual subgroups and no stages", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distribution3")
})

test_that("LF3.8 (Normal) Basic tests of Summary of test against the normal distribution table with manual subgroups and no stages", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.43, 7.08, 100, 0.3, 1.85635681000733))
})

test_that("LF3.9 (Normal) Basic tests of X-bar & R control chart with manual subgroups and no stages", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart3")
})

##### With stages (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$stagesLongFormat <- "Stage"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF4.1 (Normal) Basic tests of process capability plot with manual subgroups and stages", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process4")
})

test_that("LF4.2 (Normal) Basic tests of Process performance (total) table with manual subgroups and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.89, 0.73, 1.05, 1, 0.82, 0.65, 1, 1.18, 0.81, 0.82, 1.19, "1 (BL)",
                                      0.97, "-", "-", 1.13, 0.92, 0.77, 1.08, 1.33, 0.95, 0.92, 1.29,
                                      2, 0.08, 0.1, 0.06, 0.13, 0.1, "-", "-", 0.15, "-", 0.1, "-",
                                      "Change (2 vs. BL)"))
})

test_that("LF4.3 (Normal) Basic tests of Non-conformance statistics table with manual subgroups and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(192.5, 33.43, -159.07, 76.21, 14.21, -62, 0, 0, 0, "ppm &lt; LSL",
                                      6851.85, 2782.57, -4069.28, 4276.16, 1804.5, -2471.66, 0, 16666.67,
                                      16666.67, "ppm &gt; USL", 7044.35, 2816, -4228.35, 4352.37,
                                      1818.71, -2533.66, 0, 16666.67, 16666.67, "ppm total"))
})

test_that("LF4.4 (Normal) Basic tests of Process capability (within) table with manual subgroups and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.07, 0.88, 0.68, 1.08, 1.26, 0.85, 0.88, 1.29, "1 (BL)", 1.18,
                                      0.97, 0.79, 1.15, 1.4, 0.99, 0.97, 1.38, 2, 0.11, 0.09, "-",
                                      "-", 0.14, "-", 0.09, "-", "Change (2 vs. BL)"))
})

test_that("LF4.5 (Normal) Basic tests of Process summary table with manual subgroups and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.0825, 40, 1.9949792750032, 1.87016337059329, "1 (BL)", 6,
                                      12, 0, 7.07833333333333, 60, 1.77526237417885, 1.69102894812267,
                                      2, 6, 12, "-", -0.00416666666666643, 20, -0.21971690082435,
                                      -0.179134422470622, "Change (2 vs. BL)", "-", "-"))
})

test_that("LF4.6 (Normal) Basic tests of distribution histogram with manual subgroups and stages", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram4")
})

test_that("LF4.7 (Normal) Basic tests of Probability plot against normal distribution with manual subgroups and stages", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distribution4")
})

test_that("LF4.8 (Normal) Basic tests of Summary of test against the normal distribution table with manual subgroups and stages", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.31, 7.0825, 40, 0.54, 1.9949792750032, "1 (BL)", 0.51, 7.07833333333333,
                                      60, 0.19, 1.77526237417885, 2, "-", -0.00416666666666643, 20,
                                      "-", -0.21971690082435, "Change (2 vs. BL)"))
})

test_that("LF4.9 (Normal) Basic tests of X-bar & R control chart with manual subgroups and stages", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart4")
})

### Non-normal capability analysis with all distribution (manual subgroups and stages) ####

#### Weibull (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$stagesLongFormat <- "Stage"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
options$nonNormalDistribution <- "weibull"
options$nullDistribution <- "weibull"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)


test_that("LF5.1 (Weibull) Basic tests of non-conformance statistics table with manual subgroups and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_PerformanceNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0, 0, 0, 0, "ppm &lt; LSL", 3562.31, 2044.04, -1518.28,
                                      0, 16666.67, 16666.67, "ppm &gt; USL", 3562.31, 2044.04, -1518.28,
                                      0, 16666.67, 16666.67, "Total ppm"))
})

test_that("LF5.2 (Weibull) Basic tests of process capability plot with manual subgroups and stages", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process5")
})

test_that("LF5.3 (Weibull) Basic tests of process performance (total) table with manual subgroups and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.1, 0.91, 1.27, 0.91, "1 (BL)", 1.13, 0.96, 1.29, 0.96, 2, 0.03,
                                      0.05, 0.02, 0.05, "Change (2 vs. BL)"))
})

test_that("LF5.4 (Weibull) Basic tests of process summary table with manual subgroups and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4.04046744554796, 0, 7.0825, 40, 1.9949792750032, "1 (BL)", 6,
                                      7.82155648030048, 12, 4.18662734025142, 0, 7.07833333333333,
                                      60, 1.77526237417885, 2, 6, 7.76307853935701, 12, 0.146159894703453,
                                      "-", -0.00416666666666643, 20, -0.21971690082435, "Change (2 vs. BL)",
                                      "-", -0.0584779409434724, "-"))
})

test_that("LF5.5 (Weibull) Basic tests of distribution histogram with manual subgroups and stages", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram5")
})

test_that("LF5.6 (Weibull) Basic tests of probability plot against Weibull distribution with manual subgroups and stages", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-weibull-distribution5")
})

test_that("LF5.7 (Weibull) Basic tests of summary of test against the Weibull distribution table with manual subgroups and stages", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.32, 4.04074076167675, 40, 0.52, 7.82187513782923, "1 (BL)",
                                      0.79, 4.18683491505342, 60, 0.04, 7.76289753922706, 2, "-",
                                      0.146094153376669, 20, "-", -0.0589775986021728, "Change (2 vs. BL)"
                                 ))
})

test_that("LF5.8 (Weibull) Basic tests of X-bar & R control chart with manual subgroups and stages", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart5")
})

#### Lognormal (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$stagesLongFormat <- "Stage"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
options$nonNormalDistribution <- "lognormal"
options$nullDistribution <- "lognormal"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF6.1 (Lognormal) Basic tests of non-conformance statistics table with manual subgroups and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_PerformanceNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0, 0, 0, 0, "ppm &lt; LSL", 29855.8, 13912, -15943.8, 0,
                                      16666.67, 16666.67, "ppm &gt; USL", 29855.8, 13912, -15943.8,
                                      0, 16666.67, 16666.67, "Total ppm"))
})

test_that("LF6.2 (Lognormal) Basic tests of Capability of the process plot with manual subgroups and stages", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process6")
})

test_that("LF6.3 (Lognormal) Basic tests of Process performance (total) table with manual subgroups and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.85, 0.52, 1.68, 0.52, "1 (BL)", 1.04, 0.66, 1.87, 0.66, 2, 0.19,
                                      0.14, 0.19, 0.14, "Change (2 vs. BL)"))
})

test_that("LF6.4 (Lognormal) Basic tests of Process summary table with manual subgroups and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.91547396579744, 0, 7.0825, 40, 1.9949792750032, "1 (BL)", 6,
                                      0.302420475915792, 12, 1.92589790251696, 0, 7.07833333333333,
                                      60, 1.77526237417885, 2, 6, 0.254122719576421, 12, 0.0104239367195216,
                                      "-", -0.00416666666666643, 20, -0.21971690082435, "Change (2 vs. BL)",
                                      "-", -0.0482977563393712, "-"))
})

test_that("LF6.5 (Lognormal) Basic tests of distribution histogram with manual subgroups and stages", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram6")
})

test_that("LF6.6 (Lognormal) Basic tests of Probability plot against lognormal distribution with manual subgroups and stages", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-lognormal-distribution6")
})

test_that("LF6.7 (Lognormal) Basic tests of Summary of test against the lognormal distribution table with manual subgroups and stages", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.4, 1.91547396579744, 40, 0.34, 0.302420475915792, "1 (BL)",
                                      0.47, 1.92589790251696, 60, 0.25, 0.254122719576421, 2, "-",
                                      0.0104239367195216, 20, "-", -0.0482977563393712, "Change (2 vs. BL)"
                                 ))
})

test_that("LF6.8 (Lognormal) Basic tests of X-bar & R control chart with manual subgroups and stages", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart6")
})

#### 3-parameter-Weibull (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$stagesLongFormat <- "Stage"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
options$nonNormalDistribution <- "3ParameterWeibull"
options$nullDistribution <- "weibull"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF7.1 (3-parameter-Weibull) Basic tests of non-conformance statistics table with manual subgroups and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_PerformanceNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0, 0, 0, 0, "ppm &lt; LSL", 6970.32, 5985.64, -984.68, 0,
                                      16666.67, 16666.67, "ppm &gt; USL", 6970.32, 5985.64, -984.68,
                                      0, 16666.67, 16666.67, "Total ppm"))
})

test_that("LF7.2 (3-parameter-Weibull) Basic tests of Capability of the process plot with manual subgroups and stages", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process7")
})

test_that("LF7.3 (3-parameter-Weibull) Basic tests of Process performance (total) table with manual subgroups and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.14, 0.83, 1.55, 0.83, "1 (BL)", 1.25, 0.83, 1.96, 0.83, 2, 0.11,
                                      0, 0.41, 0, "Change (2 vs. BL)"))
})

test_that("LF7.4 (3-parameter-Weibull) Basic tests of Process summary table with manual subgroups and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.91649603464548, 0, 7.0825, 40, 1.9949792750032, "1 (BL)", 6,
                                      5.84160971651428, 1.88000690348087, 12, 2.37007212099737, 0,
                                      7.07833333333333, 60, 1.77526237417885, 2, 6, 4.45754220653612,
                                      3.12233333994186, 12, -0.546423913648105, "-", -0.00416666666666643,
                                      20, -0.21971690082435, "Change (2 vs. BL)", "-", -1.38406750997815,
                                      1.24232643646099, "-"))
})

test_that("LF7.5 (3-parameter-Weibull) Basic tests of distribution histogram with manual subgroups and stages", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram7")
})

#TODO: Add test of probability plot once 3-parameter Weibull is available.

test_that("LF7.6 (3-parameter-Weibull) Basic tests of X-bar & R control chart with manual subgroups and stages", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart7")
})

#### 3-parameter-lognormal (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$stagesLongFormat <- "Stage"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
options$nonNormalDistribution <- "3ParameterLognormal"
options$nullDistribution <- "lognormal"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF8.1 (3-parameter-lognormal) Basic tests of non-conformance statistics table with manual subgroups and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_PerformanceNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(71.05, 0, -71.05, 0, 0, 0, "ppm &lt; LSL", 7827.89, 8631.81, 803.93,
                                      0, 16666.67, 16666.67, "ppm &gt; USL", 7898.93, 8631.81, 732.88,
                                      0, 16666.67, 16666.67, "Total ppm"))
})

test_that("LF8.2 (3-parameter-lognormal) Basic tests of Capability of the process plot with manual subgroups and stages", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process8")
})

test_that("LF8.3 (3-parameter-lognormal) Basic tests of Process performance (total) table with manual subgroups and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.01, 0.8, 1.25, 0.8, "1 (BL)", 1.11, 0.75, 1.72, 0.75, 2, 0.1,
                                      -0.05, 0.47, -0.05, "Change (2 vs. BL)"))
})

test_that("LF8.4 (3-parameter-lognormal) Basic tests of Process summary table with manual subgroups and stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4.11317129873061, 0, 7.0825, 40, 1.9949792750032, "1 (BL)", 6,
                                      0.0322068303337232, -54.0895173071168, 12, 2.29729723342943,
                                      0, 7.07833333333333, 60, 1.77526237417885, 2, 6, 0.173040153292655,
                                      -3.01896187603078, 12, -1.81587406530118, "-", -0.00416666666666643,
                                      20, -0.21971690082435, "Change (2 vs. BL)", "-", 0.140833322958932,
                                      51.070555431086, "-"))
})

test_that("LF8.5 (3-parameter-lognormal) Basic tests of distribution histogram with manual subgroups and stages", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram8")
})

test_that("LF8.6 (3-parameter-lognormal) Basic tests of X-bar & R control chart with manual subgroups and stages", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart8")
})

## Missing values

### Missing values in measurements ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "DiameterMissing1"
options$subgroupSizeType <- "groupingVariable"
options$subgroup <- "Time"
options$stagesLongFormat <- "Stage"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF9.1 (Normal) Missing value test of Capability of the process plot with single missing value in measurement", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process9")
})

test_that("LF9.2 (Normal) Missing value test of Process performance (total) table with single missing value in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.94, 0.76, 1.1, 1.04, 0.87, 0.68, 1.06, 1.21, 0.84, 0.87, 1.23,
                                      "1 (BL)", 0.97, "-", "-", 1.13, 0.92, 0.77, 1.08, 1.33, 0.95,
                                      0.92, 1.29, 2, 0.03, 0.07, 0.01, 0.09, 0.05, "-", "-", 0.12,
                                      "-", 0.05, "-", "Change (2 vs. BL)"))
})

test_that("LF9.3 (Normal) Missing value test of Non-conformance statistics table with single missing value in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(138.78, 33.43, -105.35, 81.93, 14.21, -67.72, 0, 0, 0, "ppm &lt; LSL",
                                      4521.39, 2782.57, -1738.82, 3400.68, 1804.5, -1596.18, 0, 16666.67,
                                      16666.67, "ppm &gt; USL", 4660.17, 2816, -1844.17, 3482.61,
                                      1818.71, -1663.9, 0, 16666.67, 16666.67, "ppm total"))
})

test_that("LF9.4 (Normal) Missing value test of Process capability (within) table with single missing value in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.08, 0.9, 0.69, 1.11, 1.26, 0.85, 0.9, 1.3, "1 (BL)", 1.18, 0.97,
                                      0.79, 1.15, 1.4, 0.99, 0.97, 1.38, 2, 0.1, 0.07, "-", "-", 0.14,
                                      "-", 0.07, "-", "Change (2 vs. BL)"))
})

test_that("LF9.5 (Normal) Missing value test of Process summary table with single missing value in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 6.98461538461538, 39, 1.92128499509799, 1.85314569868517, "1 (BL)",
                                      6, 12, 0, 7.07833333333333, 60, 1.77526237417885, 1.69102894812267,
                                      2, 6, 12, "-", 0.0937179487179485, 21, -0.146022620919131, -0.162116750562499,
                                      "Change (2 vs. BL)", "-", "-"))
})

test_that("LF9.6 (Normal) Missing value test of distribution histogram with single missing value in measurement", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram9")
})

test_that("LF9.7 (Normal) Missing value test of Probability plot against normal distribution with single missing value in measurement", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distribution9")
})

test_that("LF9.8 (Normal) Missing value test of Summary of test against the normal distribution table with single missing value in measurement", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.32, 6.98461538461538, 39, 0.52, 1.92128499509799, "1 (BL)",
                                      0.51, 7.07833333333333, 60, 0.19, 1.77526237417885, 2, "-",
                                      0.0937179487179485, 21, "-", -0.146022620919131, "Change (2 vs. BL)"
                                 ))
})

test_that("LF9.9 (Normal) Missing value test of X-bar & R control chart with single missing value in measurement", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart9")
})

#### All but one missing ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "DiameterMissing99"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$subgroup <- "Time"
options$stagesLongFormat <- "Stage"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF10.1 (Normal) Missing value test of Process performance (total) table with all but one missing value in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.41, "", "", "*", "<unicode>", "NaN", Inf, "*", "", "*", "",
                                      "1 (BL)", "NaN", "-", "-", "*", "<unicode>", "NaN", "NaN", "*",
                                      "", "*", "", 2, "NaN", "NaN", "NaN", "*", "NaN", "-", "-", "*",
                                      "-", "*", "-", "Change (2 vs. BL)"))
})

test_that("LF10.2 (Normal) Missing value test of Non-conformance statistics table with all but one missing value in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("*", "*", "*", 0, "*", "*", "*", "*", "*", "ppm &lt; LSL", "*",
                                      "*", "*", 0, "*", "*", "*", "*", "*", "ppm &gt; USL", "*", "*",
                                      "*", 0, "*", "*", "*", "*", "*", "ppm total"))
})

test_that("LF10.3 (Normal) Missing value test of Process capability (within) with all but one missing value in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("<unicode>", "<unicode>", "NaN", "NaN", "<unicode>", "NaN", "<unicode>",
                                      "NaN", "1 (BL)", "<unicode>", "<unicode>", "NaN", "NaN", "NaN",
                                      "NaN", "NaN", "NaN", 2, "NaN", "NaN", "-", "-", "NaN", "-",
                                      "NaN", "-", "Change (2 vs. BL)"))
})

test_that("LF10.4 (Normal) Missing value test of Process summary table with all but one missing value in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 10.9, 1, "", 0, "1 (BL)", 6, 12, 0, "NaN", 0, "", 0, 2, 6,
                                      12, "-", "NaN", -1, "", 0, "Change (2 vs. BL)", "-", "-"))
})

test_that("LF10.5 (Normal) Missing value test of X-bar & R control chart with all but one missing value in measurement", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart10")
})

#### All missing ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "DiameterMissingAll"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$subgroup <- "Time"
options$stagesLongFormat <- "Stage"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF11.1 (Normal) Missing value test of Process performance (total) table with all missing values in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("*", "NaN", "NaN", "*", "<unicode>", "NaN", "NaN", "*", "", "*",
                                      "", "1 (BL)", "*", "-", "-", "*", "<unicode>", "NaN", "NaN",
                                      "*", "", "*", "", 2, "*", "NaN", "NaN", "*", "NaN", "-", "-",
                                      "*", "-", "*", "-", "Change (2 vs. BL)"))
})

test_that("LF11.2 (Normal) Missing value test of Non-conformance statistics table with all missing values in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("*", "*", "*", "*", "*", "*", "*", "*", "*", "ppm &lt; LSL", "*",
                                      "*", "*", "*", "*", "*", "*", "*", "*", "ppm &gt; USL", "*",
                                      "*", "*", "*", "*", "*", "*", "*", "*", "ppm total"))
})

test_that("LF11.3 (Normal) Missing value test of Process capability (within) table with all missing values in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("<unicode>", "<unicode>", "NaN", "NaN", "*", "NaN", "*", "NaN",
                                      "1 (BL)", "<unicode>", "<unicode>", "NaN", "NaN", "*", "NaN",
                                      "*", "NaN", 2, "NaN", "NaN", "-", "-", "*", "-", "*", "-", "Change (2 vs. BL)"
                                 ))
})

test_that("LF11.4 (Normal) Missing value test of Process summary table with all missing values in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, "NaN", 0, "", 0, "1 (BL)", 6, 12, 0, "NaN", 0, "", 0, 2, 6,
                                      12, "-", "NaN", 0, "", 0, "Change (2 vs. BL)", "-", "-"))
})

test_that("LF11.5 (Normal) Missing value test of X-bar & R control chart with all missing values in measurement", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart11")
})

### Missing values in stages ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$subgroup <- "Time"
options$stagesLongFormat <- "StageMissing15"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF12.1 (Normal) Missing value test of Capability of the process plot with missing values in stages", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process12")
})

test_that("LF12.2 (Normal) Missing value test of Process performance (total) table with missing values in stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.73, 0.54, 0.92, 0.8, 0.64, 0.43, 0.85, 0.97, 0.59, 0.64, 1.01,
                                      "1 (BL)", 0.95, "-", "-", 1.09, 0.9, 0.68, 1.11, 1.28, 0.85,
                                      0.9, 1.32, 2, 0.22, 0.22, 0.23, 0.29, 0.26, "-", "-", 0.31,
                                      "-", 0.26, "-", "Change (2 vs. BL)", 1.01, "-", "-", 1.15, 0.96,
                                      0.75, 1.18, 1.34, 0.92, 0.96, 1.37, 3, 0.28, 0.28, 0.28, 0.35,
                                      0.32, "-", "-", 0.37, "-", 0.32, "-", "Change (3 vs. BL)"))
})

test_that("LF12.3 (Normal) Missing value test of Non-conformance statistics table with missing values in stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1768.49, 61.64, 30.29, -1706.85, -1738.21, 1749.67, 55.8, 29.82,
                                      -1693.87, -1719.85, 0, 0, 0, 0, 0, "ppm &lt; LSL", 28003.94,
                                      3609.52, 1943.35, -24394.42, -26060.59, 27863.78, 3429.3, 1927.26,
                                      -24434.47, -25936.52, 0, 0, 28571.43, 0, 28571.43, "ppm &gt; USL",
                                      29772.43, 3671.16, 1973.64, -26101.27, -27798.8, 29613.45, 3485.1,
                                      1957.08, -26128.35, -27656.36, 0, 0, 28571.43, 0, 28571.43,
                                      "ppm total"))
})

test_that("LF12.4 (Normal) Missing value test of Process capability (within) table with missing values in stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.81, 0.64, 0.4, 0.87, 0.97, 0.55, 0.64, 1.05, "1 (BL)", 1.09,
                                      0.9, 0.66, 1.14, 1.29, 0.82, 0.9, 1.36, 2, 0.28, 0.26, "-",
                                      "-", 0.32, "-", 0.26, "-", "Change (2 vs. BL)", 1.15, 0.96,
                                      0.73, 1.2, 1.34, 0.89, 0.96, 1.4, 3, 0.34, 0.32, "-", "-", 0.37,
                                      "-", 0.32, "-", "Change (3 vs. BL)"))
})

test_that("LF12.5 (Normal) Missing value test of Process summary table with missing values in stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.25, 20, 2.48564298575471, 2.48280309544282, "1 (BL)", 6,
                                      12, 0, 7.06, 30, 1.83877770496487, 1.82717110920034, 2, 6, 12,
                                      "-", -0.19, 10, -0.646865280789841, -0.655631986242477, "Change (2 vs. BL)",
                                      "-", "-", 0, 6.97714285714286, 35, 1.73968991731382, 1.73811571060066,
                                      3, 6, 12, "-", -0.272857142857143, 15, -0.745953068440889, -0.744687384842157,
                                      "Change (3 vs. BL)", "-", "-"))
})

test_that("LF12.6 (Normal) Missing value test of distribution histogram with missing values in stages", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram12")
})

test_that("LF12.7 (Normal) Missing value test of Probability plot against normal distribution with missing values in stages", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distribution12")
})

test_that("LF12.8 (Normal) Missing value test of Summary of test against the normal distribution table with missing values in stages", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.51, 7.25, 20, 0.17, 2.48564298575471, "1 (BL)", 0.32, 7.06,
                                      30, 0.52, 1.83877770496487, 2, "-", -0.19, 10, "-", -0.646865280789841,
                                      "Change (2 vs. BL)", 0.56, 6.97714285714286, 35, 0.14, 1.73968991731382,
                                      3, "-", -0.272857142857143, 15, "-", -0.745953068440889, "Change (3 vs. BL)"
                                 ))
})

test_that("LF12.9 (Normal) Missing value test of X-bar & R control chart with missing values in stages", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart12")
})

### Missing values in subgroup variable ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "groupingVariable"
options$subgroup <- "TimeMissing15"
options$stagesLongFormat <- "Stage"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF13.1 (Normal) Missing value test of Capability of the process plot with missing values in subgroups variable", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process13")
})

test_that("LF13.2 (Normal) Missing value test of Process performance (total) table with missing values in subgroups variable", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.87, 0.7, 1.03, 1, 0.8, 0.61, 0.99, 1.2, 0.79, 0.8, 1.2, "1 (BL)",
                                      0.93, "-", "-", 1.08, 0.87, 0.71, 1.04, 1.28, 0.9, 0.87, 1.25,
                                      2, 0.06, 0.08, 0.04, 0.08, 0.07, "-", "-", 0.08, "-", 0.07,
                                      "-", "Change (2 vs. BL)"))
})

test_that("LF13.3 (Normal) Missing value test of Non-conformance statistics table with missing values in subgroups variable", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(163.56, 62.17, -101.38, 72.46, 44.36, -28.09, 0, 0, 0, "ppm &lt; LSL",
                                      8085.91, 4376.11, -3709.81, 5488.2, 3706.59, -1781.61, 0, 19607.84,
                                      19607.84, "ppm &gt; USL", 8249.47, 4438.28, -3811.19, 5560.66,
                                      3750.95, -1809.71, 0, 19607.84, 19607.84, "ppm total"))
})

test_that("LF13.4 (Normal) Missing value test of Process capability (within) table with missing values in subgroups variable", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.06, 0.85, 0.63, 1.06, 1.27, 0.81, 0.85, 1.29, "1 (BL)", 1.1,
                                      0.89, 0.71, 1.07, 1.31, 0.9, 0.89, 1.29, 2, 0.04, 0.04, "-",
                                      "-", 0.04, "-", 0.04, "-", "Change (2 vs. BL)"))
})

test_that("LF13.5 (Normal) Missing value test of Process summary table with missing values in subgroups variable", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.18823529411765, 34, 2.00072179488673, 1.8918266982047, "1 (BL)",
                                      6, 12, 0, 7.12941176470588, 51, 1.85787987960977, 1.81895155162279,
                                      2, 6, 12, "-", -0.0588235294117645, 17, -0.142841915276952,
                                      -0.072875146581916, "Change (2 vs. BL)", "-", "-"))
})

test_that("LF13.6 (Normal) Missing value test of distribution histogram with missing values in subgroups variable", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram13")
})

test_that("LF13.7 (Normal) Missing value test of Probability plot against normal distribution with missing values in subgroups variable", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distribution13")
})

test_that("LF13.8 (Normal) Missing value test of Summary of test against the normal distribution table with missing values in subgroups variable", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.2, 7.18823529411765, 34, 0.88, 2.00072179488673, "1 (BL)", 0.54,
                                      7.12941176470588, 51, 0.16, 1.85787987960977, 2, "-", -0.0588235294117645,
                                      17, "-", -0.142841915276952, "Change (2 vs. BL)"))
})

test_that("LF13.9 (Normal) Missing value test of X-bar & R control chart with missing values in subgroups variable", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart13")
})

## Changed options ####

### Set different manual subgroup sizes (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 7
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF14.1 (Normal) Option test of Capability of the process plot with changed manual subgroup size", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process14")
})

test_that("LF14.2 (Normal) Option test of Process performance (total) table with changed manual subgroup size", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.93, 0.83, 1.04, 1.08, 0.88, 0.77, 1, 1.27, 0.95, 0.88, 1.2
                                 ))
})

test_that("LF14.3 (Normal) Option test of Non-conformance statistics table with changed manual subgroup size", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(68.39, 75.29, 0, "ppm &lt; LSL", 4020.39, 4221.73, 10000, "ppm &gt; USL",
                                      4088.78, 4297.02, 10000, "ppm total"))
})

test_that("LF14.4 (Normal) Option test of Process capability (within) table with changed manual subgroup size", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.07, 0.88, 0.75, 1, 1.26, 0.93, 0.88, 1.21))
})

test_that("LF14.5 (Normal) Option test of Process summary table with changed manual subgroup size", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.08, 100, 1.85635681000733, 1.86802166416107, 6, 12))
})

test_that("LF14.6 (Normal) Option test of distribution histogram with changed manual subgroup size", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram14")
})

test_that("LF14.7 (Normal) Option test of Probability plot against normal distribution with changed manual subgroup size", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distribution14")
})

test_that("LF14.8 (Normal) Option test of Summary of test against the normal distribution table with changed manual subgroup size", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.43, 7.08, 100, 0.3, 1.85635681000733))
})

test_that("LF14.9 (Normal) Option test of X-bar & R control chart with changed manual subgroup size", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart14")
})

### Fixed subgroup sizes ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 7
options$subgroupSizeUnequal <- "fixedSubgroupSize"
options$fixedSubgroupSizeValue <- 5
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options, makeTests = TRUE)

test_that("LF15.1 (Normal) Option test of X-bar & R control chart with fixed subgroup size", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart15")
})

### All checkboxes / - All control charts ####

### Advanced options ####

## Edge cases ####

### Multiple stages assigned ####

## Report ####




# Wide / Row format ####

## (Normal) Basic tests ####

### Normal capability analysis ####

#### Subgroup Variable ####

##### Without stages ####

##### With stages ####

#### Manual subgroups ####

##### Without stages ####

##### With stages ####

### Non-normal capability analysis with all distribution ####

#### Subgroup Variable ####

##### Without stages ####

##### With stages ####

#### Manual subgroups ####

##### Without stages ####

##### With stages ####

## Missing values in measurements ####

### All but one missing ####

### Whole subgroup missing ####

### All missing ####

## Missing values in stages ####

## Missing values in subgroup variable ####

## Changed options ####

### Set different manual subgroup sizes ####

### Fixed vs actual sizes with unequal subgroup sizes ####

### All checkboxes / - All control charts ####

### Advanced options ####

## Report ####





















#Long format
# options <- analysisOptions("processCapabilityStudies")
# options$measurementLongFormat <- "Diameter"
# options$capabilityStudyType <- "normalCapabilityAnalysis"
# options$subgroupSizeType <- "groupingVariable"
# options$subgroup <- "Time"
# options$probabilityPlotRankMethod <- "bernard"
# options$lowerSpecificationLimit <- TRUE
# options$upperSpecificationLimit <- TRUE
# options$target <- TRUE
# options$lowerSpecificationLimitValue <- -16
# options$targetValue <- -8
# options$upperSpecificationLimitValue <- 0
# options$controlChartType <- "xBarR"
# options$controlChartSdEstimationMethodGroupSizeLargerThanOne <- "rBar"
# set.seed(1)
# results <- runAnalysis("processCapabilityStudies", "SPCSubgroups_Long.csv", options)
#
# test_that("Capability of the process plot matches", {
#   plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "capability-of-the-process")
# })
#
# test_that("Process performance (total) table results match", {
#   table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(1.29, 1.14, 1.44, 1.44, 1.27, 1.11, 1.43, 1.6, 1.27, 1.27, 1.6
#                                  ))
# })
#
# test_that("Non-conformance statistics table results match", {
#   table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(0.77, 0.21, 0, "ppm &lt; LSL", 68.39, 29.52, 0, "ppm &gt; USL",
#                                       69.16, 29.73, 0, "ppm total"))
# })
#
# test_that("Process capability (within) table results match", {
#   table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(1.51, 1.34, 1.16, 1.52, 1.69, 1.32, 1.34, 1.7))
# })
#
# test_that("Process summary table results match", {
#   table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(-16, -7.08, 100, 1.85635681000733, 1.76268271711092, -8, 0))
# })
#
# test_that("X-bar & R Control Chart plot matches", {
#   plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart")
# })
#
# test_that("Histogram plot matches", {
#   plotName <- results[["results"]][["histogram"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "histogram")
# })
#
# test_that("Probability Plot matches", {
#   plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "probability-plot")
# })
#
# test_that("Summary of test against the normal distribution table results match", {
#   table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(0.43, -7.08, 100, 0.30, 1.85635681000733
#                                  ))
# })
#
# # Wide format
# options$dataFormat <- "wideFormat"
# options$measurementsWideFormat <- c("dm1", "dm2", "dm3", "dm4", "dm5")
# options$axisLabels <- "Time"
# options$controlChartType <- "xBarMR"
# options$controlChartSdEstimationMethodGroupSizeLargerThanOne <- "rBar"
# set.seed(1)
# results <- runAnalysis("processCapabilityStudies", "SPCSubgroups_Wide.csv", options)
#
#
# test_that("Capability of the process plot matches2", {
#   plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "capability-of-the-process2")
# })
#
# test_that("Process performance (total) table results match", {
#   table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(1.29, 1.14, 1.44, 1.44, 1.27, 1.11, 1.43, 1.6, 1.27, 1.27, 1.6
#                                  ))
# })
#
# test_that("Non-conformance statistics table results match", {
#   table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(0.77, 0.21, 0, "ppm &lt; LSL", 68.39, 29.52, 0, "ppm &gt; USL",
#                                       69.16, 29.73, 0, "ppm total"))
# })
#
# test_that("Process capability (within) table results match", {
#   table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(1.51, 1.34, 1.16, 1.52, 1.69, 1.32, 1.34, 1.7))
# })
#
# test_that("Process summary table results match", {
#   table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(-16, -7.08, 100, 1.85635681000733, 1.76268271711092, -8, 0))
# })
#
# test_that("X-bar & mR Control Chart plot matches", {
#   plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "x-bar-mr-control-chart")
# })
#
# test_that("Histogram plot matches2", {
#   plotName <- results[["results"]][["histogram"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "histogram2")
# })
#
# test_that("Probability Plot matches2", {
#   plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "probability-plot2")
# })
#
# test_that("Summary of test against the normal distribution table results match", {
#   table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(0.43, -7.08, 100, 0.30, 1.85635681000733
#                                  ))
# })
#
# # Long format- Weibull
# options <- analysisOptions("processCapabilityStudies")
# options$dataFormat <- "longFormat"
# options$measurementLongFormat <- "Ovality"
# options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
# options$nonNormalDistribution <- "weibull"
# options$nullDistribution <- "weibull"
# options$probabilityPlotRankMethod <- "bernard"
# options$lowerSpecificationLimit <- TRUE
# options$upperSpecificationLimit <- TRUE
# options$target <- FALSE
# options$lowerSpecificationLimitValue <- 0
# options$targetValue <- 0
# options$upperSpecificationLimitValue <- 15
# options$controlChartType <- "xmr"
# set.seed(1)
# results <- runAnalysis("processCapabilityStudies", "msaPCS_Weibull.csv", options)
#
# test_that("X-mR Control Chart plot matches", {
#   plotName <- results[["results"]][["xmr"]][["collection"]][["xmr_plot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart")
# })
#
# test_that("Test results for individuals chart table results match", {
#   table <- results[["results"]][["xmr"]][["collection"]][["xmr_tableIndividual"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(3, 11, 42, 12, 100, ""))
# })
#
# test_that("Test results for moving range chart table results match", {
#   table <- results[["results"]][["xmr"]][["collection"]][["xmr_tableMR"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(3, 11, 42, 29, 43, 50, "", 51))
# })
#
# test_that("Non-conformance statistics table results match", {
#   table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_PerformanceNonNormal"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(0, 0, "ppm &lt; LSL", 0.06, 0, "ppm &gt; USL", 0.06,
#                                       0, "Total ppm"))
# })
#
# test_that("Capability of the process plot matches3", {
#   plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_capabilityPlot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "capability-of-the-process3")
# })
#
# test_that("Process performance (total) table results match", {
#   table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(1.59, 1.05, 1.05, 1.85))
# })
#
# test_that("Process summary table results match", {
#   table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(2.07532784402308, 0, 3.42, 100, 1.74772579501061, 3.86889820358025,
#                                       15))
# })
#
# test_that("Histogram plot matches3", {
#   plotName <- results[["results"]][["histogram"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "histogram3")
# })
#
# test_that("Probability Plot matches3", {
#   plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "probability-plot3")
# })
#
# test_that("Summary of test against the weibull distribution table results match", {
#   table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(0.71, 2.07536754156247, 100, 0.06, 3.86876200879491
#                                  ))
# })
#
# # Long format- Weibull
# options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
# options$nonNormalDistribution <- "lognormal"
# options$nullDistribution <- "lognormal"
# options$controlChartType <- "xBarS"
# set.seed(1)
# results <- runAnalysis("processCapabilityStudies", "msaPCS_Weibull.csv", options)
#
# test_that("Non-conformance statistics table results match", {
#   table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_PerformanceNonNormal"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(0, 0, "ppm &lt; LSL", 1196.84133284115, 0, "ppm &gt; USL", 1196.84133284115,
#                                       0, "Total ppm"))
# })
#
# test_that("Capability of the process plot matches4", {
#   plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_capabilityPlot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "capability-of-the-process4")
# })
#
# test_that("Process performance (total) table results match", {
#   table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(1.06, 1.02, 1.26, 1.02))
# })
#
# test_that("Process summary table results match", {
#   table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(1.10078741574583, 0, 3.42, 100, 1.74772579501061, 0.529320014694866,
#                                       15))
# })
#
# test_that("X-bar & s Control Chart plot matches", {
#   plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart")
# })
#
# test_that("Test results for s chart table results match", {
#   table <- results[["results"]][["xBar"]][["collection"]][["xBar_tableSecondPlot"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(2))
# })
#
# test_that("Histogram plot matches4", {
#   plotName <- results[["results"]][["histogram"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "histogram4")
# })
#
# test_that("Probability Plot matches4", {
#   plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "probability-plot4")
# })
#
# test_that("Summary of test against the lognormal distribution table results match", {
#   table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(0.61, 1.10078741574583, 100, 0.11, 0.529320014694866
#                                  ))
# })
