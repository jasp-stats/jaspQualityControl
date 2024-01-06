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

## Missing values ####

### Missing values in measurements ####

#### Single missing value ####
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
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF15.1 (Normal) Option test of X-bar & R control chart with fixed subgroup size", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart15")
})

### All checkboxes / All control charts ####

#### S-bar (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$subgroup <- "Time"
options$probabilityPlotRankMethod <- "bernard"
options$probabilityPlotGridLines <- TRUE
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarS"
options$controlChartSdEstimationMethodGroupSizeLargerThanOne <- "sBar"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF16.1 (Normal) Option test of Capability of the process plot with s-bar calculation method", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process16")
})

test_that("LF16.2 (Normal) Option test of Process performance (total) table with s-bar calculation method", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.93, 0.83, 1.04, 1.08, 0.88, 0.77, 1, 1.27, 0.95, 0.88, 1.2
                                 ))
})

test_that("LF16.3 (Normal) Option test of Non-conformance statistics table with s-bar calculation method", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(68.39, 64.2, 0, "ppm &lt; LSL", 4020.39, 3893.42, 10000, "ppm &gt; USL",
                                      4088.78, 3957.62, 10000, "ppm total"))
})

test_that("LF16.4 (Normal) Option test of Process capability (within) table with s-bar calculation method", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.08, 0.89, 0.76, 1.01, 1.28, 0.94, 0.89, 1.22))
})

test_that("LF16.5 (Normal) Option test of Process summary table with s-bar calculation method", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.08, 100, 1.85635681000733, 1.84880707556386, 6, 12))
})

test_that("LF16.6 (Normal) Option test of distribution histogram with s-bar calculation method", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram16")
})

test_that("LF16.7 (Normal) Option test of Probability plot against normal distribution with s-bar calculation method", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distribution16")
})

test_that("LF16.8 (Normal) Option test of Summary of test against the normal distribution table with s-bar calculation method", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.43, 7.08, 100, 0.3, 1.85635681000733))
})

test_that("LF16.9 (Normal) Option test of X-bar & s control chart with s-bar calculation method", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart16")
})

#### x-mR (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 1
options$probabilityPlotRankMethod <- "bernard"
options$probabilityPlotGridLines <- TRUE
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xmr"
options$xmrChartSpecificationLimits <- TRUE
options$controlChartSdEstimationMethodGroupSizeEqualOne <- "meanMovingRange"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF17.1 (Normal) Option test of Capability of the process plot with x-mR calculation method", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process17")
})

test_that("LF17.2 (Normal) Option test of Process performance (total) table with x-mR calculation method", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.93, 0.83, 1.04, 1.08, 0.88, 0.77, 1, 1.27, 0.95, 0.88, 1.2
                                 ))
})

test_that("LF17.3 (Normal) Option test of Non-conformance statistics table with x-mR calculation method", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(68.39, 72.55, 0, "ppm &lt; LSL", 4020.39, 4142.82, 10000, "ppm &gt; USL",
                                      4088.78, 4215.37, 10000, "ppm total"))
})

test_that("LF17.4 (Normal) Option test of Process capability (within) table with x-mR calculation method", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.07, 0.88, 0.76, 1, 1.27, 0.94, 0.88, 1.2))
})

test_that("LF17.5 (Normal) Option test of Process summary table with x-mR calculation method", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.08, 100, 1.85635681000733, 1.86349308689734, 6, 12))
})

test_that("LF17.6 (Normal) Option test of distribution histogram with x-mR calculation method", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram17")
})

test_that("LF17.7 (Normal) Option test of Probability plot against normal distribution with x-mR calculation method", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distribution17")
})

test_that("LF17.8 (Normal) Option test of Summary of test against the normal distribution table with x-mR calculation method", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.43, 7.08, 100, 0.3, 1.85635681000733))
})

test_that("LF17.9 (Normal) Option test of X-mR control chart with x-mR calculation method", {
  plotName <- results[["results"]][["xmr"]][["collection"]][["xmr_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-mr-control-chart17")
})

test_that("LF17.10 (Normal) Option test of Test results for individuals chart table with x-mR calculation method", {
  table <- results[["results"]][["xmr"]][["collection"]][["xmr_tableIndividual"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 58"))
})

test_that("LF17.11 (Normal) Option test of Test results for moving range chart table with x-mR calculation method", {
  table <- results[["results"]][["xmr"]][["collection"]][["xmr_tableMR"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Point 23", "Point 52",
                                      "Point 53", "Point 54",
                                      "Point 55", "Point 74",
                                      "Point 75", "Point 76",
                                      "Point 77", "Point 78",
                                      "Point 79"))
})

#### Boundaries (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$subgroup <- "Time"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$lowerSpecificationLimitBoundary <- TRUE
options$upperSpecificationLimitBoundary <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF18.1 (Normal) Option test of Capability of the process plot with boundaries", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process18")
})

test_that("LF18.2 (Normal) Option test of Process performance (total) table with boundaries", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("*", "*", "*", "*", "*"))
})

test_that("LF18.3 (Normal) Option test of Non-conformance statistics table with boundaries", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("*", "*", 0, "ppm &lt; LB", "*", "*", 10000, "ppm &gt; UB", "*",
                                      "*", 10000, "ppm total"))
})

test_that("LF18.4 (Normal) Option test of Process capability (within) table with boundaries", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("*", "*", "*", "*"))
})

test_that("LF18.5 (Normal) Option test of Process summary table with boundaries", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.08, 100, 1.85635681000733, 1.76268271711092, 6, 12))
})

test_that("LF18.6 (Normal) Option test of distribution histogram with boundaries", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram18")
})

test_that("LF18.7 (Normal) Option test of Probability plot against normal distribution with boundaries", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distribution18")
})

test_that("LF18.8 (Normal) Option test of Summary of test against the normal distribution table with boundaries", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.43, 7.08, 100, 0.3, 1.85635681000733))
})

test_that("LF18.9 (Normal) Option test of X-bar & R control chart with boundaries", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart18")
})

### Advanced options ####

#### Unbiasing constant for s-bar (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$subgroup <- "Time"
options$probabilityPlotRankMethod <- "bernard"
options$probabilityPlotGridLines <- TRUE
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarS"
options$controlChartSdEstimationMethodGroupSizeLargerThanOne <- "sBar"
options$controlChartSdUnbiasingConstant <- FALSE
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF19.1 (Normal) Option test of Capability of the process plot with s-bar calculation method without unbiasing constant", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process19")
})

test_that("LF19.2 (Normal) Option test of Process performance (total) table with s-bar calculation method without unbiasing constant", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.93, 0.83, 1.04, 1.08, 0.88, 0.77, 1, 1.27, 0.95, 0.88, 1.2
                                 ))
})

test_that("LF19.3 (Normal) Option test of Non-conformance statistics table with s-bar calculation method without unbiasing constant", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(68.39, 23.11, 0, "ppm &lt; LSL", 4020.39, 2319.55, 10000, "ppm &gt; USL",
                                      4088.78, 2342.65, 10000, "ppm total"))
})

test_that("LF19.4 (Normal) Option test of Process capability (within) table with s-bar calculation method without unbiasing constant", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.15, 0.94, 0.81, 1.07, 1.36, 1, 0.94, 1.29))
})

test_that("LF19.5 (Normal) Option test of Process summary table with s-bar calculation method without unbiasing constant", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.08, 100, 1.85635681000733, 1.73785203372984, 6, 12))
})

test_that("LF19.6 (Normal) Option test of distribution histogram with s-bar calculation method without unbiasing constant", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram19")
})

test_that("LF19.7 (Normal) Option test of Probability plot against normal distribution with s-bar calculation method without unbiasing constant", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distribution19")
})

test_that("LF19.8 (Normal) Option test of Summary of test against the normal distribution table with s-bar calculation method without unbiasing constant", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.43, 7.08, 100, 0.3, 1.85635681000733))
})

test_that("LF19.9 (Normal) Option test of X-bar & s control chart with s-bar calculation method without unbiasing constant", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart19")
})

## Edge cases ####

### Multiple stages assigned ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$stagesLongFormat <- "StageMultiAssigned"
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

test_that("LF20.1 (Normal) Edge case test of Capability of the process plot with multiple stages assigned per subgroup", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-process20")
})

test_that("LF20.2 (Normal) Edge case test of Process performance (total) table with multiple stages assigned per subgroup", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.73, 0.54, 0.91, 0.82, 0.64, 0.43, 0.84, 1.01, 0.6, 0.64, 1.04,
                                      "1 (BL)", 1.16, "-", "-", 1.3, 1.12, 0.88, 1.36, 1.49, 1.04,
                                      1.12, 1.56, 2, 0.43, 0.39, 0.46, 0.48, 0.48, "-", "-", 0.48,
                                      "-", 0.48, "-", "Change (2 vs. BL)", 0.94, "-", "-", 1.1, 0.89,
                                      0.72, 1.07, 1.31, 0.9, 0.89, 1.29, 3, 0.21, 0.24, 0.18, 0.28,
                                      0.25, "-", "-", 0.3, "-", 0.25, "-", "Change (3 vs. BL)"))
})

test_that("LF20.3 (Normal) Edge case test of Non-conformance statistics table with multiple stages assigned per subgroup", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1168.95, 4.13, 43.51, -1164.82, -1125.43, 1356.97, 0.65, 17.53,
                                      -1356.33, -1339.44, 0, 0, 0, 0, 0, "ppm &lt; LSL", 28368.98,
                                      380.54, 3734.44, -27988.44, -24634.54, 30255.52, 128.66, 2394.91,
                                      -30126.85, -27860.6, 0, 0, 22222.22, 0, 22222.22, "ppm &gt; USL",
                                      29537.92, 384.67, 3777.95, -29153.26, -25759.97, 31612.49, 129.31,
                                      2412.44, -31483.18, -29200.05, 0, 0, 22222.22, 0, 22222.22,
                                      "ppm total"))
})

test_that("LF20.4 (Normal) Edge case test of Process capability (within) table with multiple stages assigned per subgroup", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.81, 0.63, 0.39, 0.86, 1, 0.55, 0.63, 1.06, "1 (BL)", 1.42, 1.22,
                                      0.93, 1.51, 1.61, 1.1, 1.22, 1.73, 2, 0.61, 0.59, "-", "-",
                                      0.61, "-", 0.59, "-", "Change (2 vs. BL)", 1.16, 0.94, 0.74,
                                      1.14, 1.38, 0.93, 0.94, 1.38, 3, 0.35, 0.31, "-", "-", 0.38,
                                      "-", 0.31, "-", "Change (3 vs. BL)"))
})

test_that("LF20.5 (Normal) Edge case test of Process summary table with multiple stages assigned per subgroup", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.38, 20, 2.42478430945279, 2.46130696474635, "1 (BL)", 6,
                                      12, 0, 6.83714285714286, 35, 1.53356288623924, 1.41260287433976,
                                      2, 6, 12, "-", -0.542857142857143, 15, -0.891221423213548, -1.04870409040658,
                                      "Change (2 vs. BL)", "-", "-", 0, 7.13555555555556, 45, 1.81836034477062,
                                      1.72446737365052, 3, 6, 12, "-", -0.244444444444444, 25, -0.606423964682161,
                                      -0.736839591095825, "Change (3 vs. BL)", "-", "-"))
})

test_that("LF20.6 (Normal) Edge case test of distribution histogram with multiple stages assigned per subgroup", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram20")
})

test_that("LF20.7 (Normal) Edge case test of Probability plot against normal distribution with multiple stages assigned per subgroup", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distribution20")
})

test_that("LF20.8 (Normal) Edge case test of Summary of test against the normal distribution table with multiple stages assigned per subgroup", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.41, 7.38, 20, 0.31, 2.42478430945279, "1 (BL)", 0.21, 6.83714285714286,
                                      35, 0.86, 1.53356288623924, 2, "-", -0.542857142857143, 15,
                                      "-", -0.891221423213548, "Change (2 vs. BL)", 0.61, 7.13555555555556,
                                      45, 0.11, 1.81836034477062, 3, "-", -0.244444444444444, 25,
                                      "-", -0.606423964682161, "Change (3 vs. BL)"))
})

test_that("LF20.9 (Normal) Edge case test of X-bar & R control chart with multiple stages assigned per subgroup", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart20")
})

## Report (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$measurementLongFormat <- "Diameter"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
options$report <- TRUE
options$reportDate <- "01.01.2020"
options$reportMiscellaneous <- "Comment"
options$reportProcessName <- "Process name"
options$reportReportedBy <- "Mrs. Doe"
options$reportTitle <- "Title"
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options)

test_that("LF21 (Normal) Basic test of report functionality", {
  plotName <- results[["results"]][["pcReport"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "title21")
})

# Wide / Row format ####

## (Normal) Basic tests ####

### Normal capability analysis ####

#### Without stages (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
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
                       "datasets/processCapabilityStudy/processCapabilityAnalysisWideFormatDebug.csv", options)

test_that("WF1.1 (Normal) Basic tests of process capability plot without stages", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-processW1")
})

test_that("WF1.2 (Normal) Basic tests of Process performance (total) table without stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.93, 0.83, 1.04, 1.08, 0.88, 0.77, 1, 1.27, 0.95, 0.88, 1.2
                                 ))
})

test_that("WF1.3 (Normal) Basic tests of Non-conformance statistics table without stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(68.39, 29.52, 0, "ppm &lt; LSL", 4020.39, 2625.65, 10000, "ppm &gt; USL",
                                      4088.78, 2655.17, 10000, "ppm total"))
})

test_that("WF1.4 (Normal) Basic tests of Process capability (within) table without stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.13, 0.93, 0.8, 1.06, 1.34, 0.99, 0.93, 1.28))
})

test_that("WF1.5 (Normal) Basic tests of Process summary table without stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.08, 100, 1.85635681000733, 1.76268271711092, 6, 12))
})

test_that("WF1.6 (Normal) Basic tests of distribution histogram without stages", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogramW1")
})

test_that("WF1.7 (Normal) Basic tests of Probability plot against normal distribution without stages", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distributionW1")
})

test_that("WF1.8 (Normal) Basic tests of Summary of test against the normal distribution table without stages", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.43, 7.08, 100, 0.3, 1.85635681000733))
})

test_that("WF1.9 (Normal) Basic tests of X-bar & R control chart without stages", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chartW1")
})

#### With stages (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
options$stagesWideFormat <- "Stage"
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
                       "datasets/processCapabilityStudy/processCapabilityAnalysisWideFormatDebug.csv", options)

test_that("WF2.1 (Normal) Basic tests of process capability plot with stages", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-processW2")
})

test_that("WF2.2 (Normal) Basic tests of Process performance (total) table with stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.9, 0.75, 1.04, 1.02, 0.83, 0.67, 0.99, 1.21, 0.85, 0.83, 1.19,
                                      "1 (BL)", 0.98, "-", "-", 1.13, 0.93, 0.76, 1.11, 1.33, 0.94,
                                      0.93, 1.31, 2, 0.08, 0.07, 0.09, 0.11, 0.1, "-", "-", 0.12,
                                      "-", 0.1, "-", "Change (2 vs. BL)"))
})

test_that("WF2.3 (Normal) Basic tests of Non-conformance statistics table with stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(137.75, 34.52, -103.23, 63.98, 12, -51.98, 0, 0, 0, "ppm &lt; LSL",
                                      6242, 2570.76, -3671.24, 4259.85, 1489.74, -2770.1, 0, 20000,
                                      20000, "ppm &gt; USL", 6379.75, 2605.28, -3774.48, 4323.82,
                                      1501.75, -2822.08, 0, 20000, 20000, "ppm total"))
})

test_that("WF2.4 (Normal) Basic tests of Process capability (within) table with stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.08, 0.88, 0.7, 1.05, 1.28, 0.88, 0.88, 1.27, "1 (BL)", 1.2,
                                      0.99, 0.79, 1.19, 1.41, 0.98, 0.99, 1.41, 2, 0.12, 0.11, "-",
                                      "-", 0.13, "-", 0.11, "-", "Change (2 vs. BL)"))
})

test_that("WF2.5 (Normal) Basic tests of Process summary table with stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.114, 50, 1.9558400251763, 1.85726569217541, "1 (BL)", 6,
                                      12, 0, 7.046, 50, 1.77053087853241, 1.66809974204643, 2, 6,
                                      12, "-", -0.0679999999999996, 0, -0.185309146643895, -0.189165950128977,
                                      "Change (2 vs. BL)", "-", "-"))
})

test_that("WF2.6 (Normal) Basic tests of distribution histogram with stages", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogramW2")
})

test_that("WF2.7 (Normal) Basic tests of Probability plot against normal distribution with stages", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distributionW2")
})

test_that("WF2.8 (Normal) Basic tests of Summary of test against the normal distribution table with stages", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.24, 7.114, 50, 0.77, 1.9558400251763, "1 (BL)", 0.64, 7.046,
                                      50, 0.09, 1.77053087853241, 2, "-", -0.0679999999999996, 0,
                                      "-", -0.185309146643895, "Change (2 vs. BL)"))
})

test_that("WF2.9 (Normal) Basic tests of X-bar & R control chart with stages", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chartW2")
})

### Non-normal capability analysis with all distribution ####

#### Weibull (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
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
                       "datasets/processCapabilityStudy/processCapabilityAnalysisWideFormatDebug.csv", options)

test_that("WF3.1 (Weibull) Basic tests of Non-conformance statistics table", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_PerformanceNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, "ppm &lt; LSL", 2555.7, 10000, "ppm &gt; USL", 2555.7, 10000,
                                      "Total ppm"))
})

test_that("WF3.2 (Weibull) Basic tests of Capability of the process plot", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-processW3")
})

test_that("WF3.3 (Weibull) Basic tests of Process performance (total) table", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.12, 0.94, 1.28, 0.94))
})

test_that("WF3.4 (Weibull) Basic tests of Process summary table", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4.13179160447975, 0, 7.08, 100, 1.85635681000733, 6, 7.78727470049078,
                                      12))
})

test_that("WF3.5 (Weibull) Basic tests of distribution histogram", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogramW3")
})

test_that("WF3.6 (Weibull) Basic tests of Probability plot against Weibull distribution", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-weibull-distributionW3")
})

test_that("WF3.7 (Weibull) Basic tests of Summary of test against the Weibull distribution table", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.6, 4.13213439552013, 100, 0.12, 7.78757547787008))
})

test_that("WF3.8 (Weibull) Basic tests of X-bar & R control chart", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chartW3")
})

#### Lognormal (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
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
                       "datasets/processCapabilityStudy/processCapabilityAnalysisWideFormatDebug.csv", options)


test_that("WF4.1 (Lognormal) Basic tests of Non-conformance statistics table", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_PerformanceNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, "ppm &lt; LSL", 19568.07, 10000, "ppm &gt; USL", 19568.07,
                                      10000, "Total ppm"))
})

test_that("WF4.2 (Lognormal) Basic tests of Capability of the process plot", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-processW4")
})

test_that("WF4.3 (Lognormal) Basic tests of Process performance (total) table", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.96, 0.6, 1.79, 0.6))
})

test_that("WF4.4 (Lognormal) Basic tests of Process summary table", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.92172832782915, 0, 7.08, 100, 1.85635681000733, 6, 0.273022707246043,
                                      12))
})

test_that("WF4.5 (Lognormal) Basic tests of distribution histogram", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogramW4")
})

test_that("WF4.6 (Lognormal) Basic tests of Probability plot against lognormal distribution", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-lognormal-distributionW4")
})

test_that("WF4.7 (Lognormal) Basic tests of Summary of test against the lognormal distribution table", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.56, 1.92172832782915, 100, 0.14, 0.273022707246043))
})

test_that("WF4.8 (Lognormal) Basic tests of X-bar & R control chart", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chartW4")
})

#### 3-parameter-Weibull (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
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
                       "datasets/processCapabilityStudy/processCapabilityAnalysisWideFormatDebug.csv", options)

test_that("WF5.1 (3-parameter-Weibull) Basic tests of Non-conformance statistics table", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_PerformanceNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, "ppm &lt; LSL", 6112.64, 10000, "ppm &gt; USL", 6112.64,
                                      10000, "Total ppm"))
})

test_that("WF5.2 (3-parameter-Weibull) Basic tests of Capability of the process plot", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-processW5")
})

test_that("WF5.3 (3-parameter-Weibull) Basic tests of Process performance (total) table", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.2, 0.84, 1.73, 0.84))
})

test_that("WF5.4 (3-parameter-Weibull) Basic tests of Process summary table", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.64846838111814, 0, 7.08, 100, 1.85635681000733, 6, 5.1219561122954,
                                      2.5264194646292, 12))
})

test_that("WF5.5 (3-parameter-Weibull) Basic tests of distribution histogram", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogramW5")
})

test_that("WF5.6 (3-parameter-Weibull) Basic tests of Probability plot against Weibull distribution", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-weibull-distributionW5")
})

test_that("WF5.7 (3-parameter-Weibull) Basic tests of Summary of test against the Weibull distribution table", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.6, 4.13213439552013, 100, 0.12, 7.78757547787008))
})

test_that("WF5.8 (3-parameter-Weibull) Basic tests of X-bar & R control chart", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chartW5")
})

#### 3-parameter-lognormal (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
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
                       "datasets/processCapabilityStudy/processCapabilityAnalysisWideFormatDebug.csv", options)

test_that("WF6.1 (3-parameter-lognormal) Basic tests of Non-conformance statistics table", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_PerformanceNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.16, 0, "ppm &lt; LSL", 8869.72, 10000, "ppm &gt; USL", 8869.88,
                                      10000, "Total ppm"))
})

test_that("WF6.2 (3-parameter-lognormal) Basic tests of Capability of the process plot", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-processW6")
})

test_that("WF6.3 (3-parameter-lognormal) Basic tests of Process performance (total) table", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.07, 0.76, 1.52, 0.76))
})

test_that("WF6.4 (3-parameter-lognormal) Basic tests of Process summary table", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.70930280255199, 0, 7.08, 100, 1.85635681000733, 6, 0.121871527001624,
                                      -8.0506521367885, 12))
})

test_that("WF6.5 (3-parameter-lognormal) Basic tests of distribution histogram", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogramW6")
})

test_that("WF6.6 (3-parameter-lognormal) Basic tests of Probability plot against lognormal distribution", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-lognormal-distributionW6")
})

test_that("WF6.7 (3-parameter-lognormal) Basic tests of Summary of test against the lognormal distribution table", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.56, 1.92172832782915, 100, 0.14, 0.273022707246043))
})

test_that("WF6.8 (3-parameter-lognormal) Basic tests of X-bar & R control chart", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chartW6")
})

## Missing values ####

### Missing values in measurements ####

#### Single missing value ####
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1Missing1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
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
                       "datasets/processCapabilityStudy/processCapabilityAnalysisWideFormatDebug.csv", options)

test_that("WF7.1 (Normal) Missing value test of Capability of the process plot with single missing value in measurement", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-processW7")
})

test_that("WF7.2 (Normal) Missing value test of Process performance (total) table with single missing value in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.96, 0.85, 1.06, 1.1, 0.91, 0.79, 1.03, 1.29, 0.97, 0.91, 1.22
                                 ))
})

test_that("WF7.3 (Normal) Missing value test of Non-conformance statistics table with single missing value in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(57.11, 29.97, 0, "ppm &lt; LSL", 3294.28, 2356.83, 10101.01, "ppm &gt; USL",
                                      3351.4, 2386.8, 10101.01, "ppm total"))
})

test_that("WF7.4 (Normal) Missing value test of Process capability (within) table with single missing value in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.14, 0.94, 0.81, 1.07, 1.34, 0.99, 0.94, 1.28))
})

test_that("WF7.5 (Normal) Missing value test of Process summary table with single missing value in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.04141414141414, 99, 1.82505298223401, 1.7546443818877, 6,
                                      12))
})

test_that("WF7.6 (Normal) Missing value test of distribution histogram with single missing value in measurement", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogramW7")
})

test_that("WF7.7 (Normal) Missing value test of Probability plot against normal distribution with single missing value in measurement", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distributionW7")
})

test_that("WF7.8 (Normal) Missing value test of Summary of test against the normal distribution table with single missing value in measurement", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.4, 7.04141414141414, 99, 0.36, 1.82505298223401))
})

test_that("WF7.9 (Normal) Missing value test of X-bar & R control chart with single missing value in measurement", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chartW7")
})

#### All but one missing ####
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1Missing19", "dm2MissingAll", "dm3MissingAll", "dm4MissingAll", "dm5MissingAll")
options$axisLabels <- "Time"
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
                       "datasets/processCapabilityStudy/processCapabilityAnalysisWideFormatDebug.csv", options)

test_that("WF8.1 (Normal) Missing value test of Process performance (total) table with all but one missing value in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "*", "*", "*", "<unicode>", "*", "<unicode>", "*", "*", "*",
                                      "*"))
})

test_that("WF8.2 (Normal) Missing value test of Non-conformance statistics table with all but one missing value in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("*", 0, 0, "ppm &lt; LSL", "*", 0, 0, "ppm &gt; USL", "*", 0,
                                      0, "ppm total"))
})

test_that("WF8.3 (Normal) Missing value test of Process capability (within) table with all but one missing value in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("<unicode>", "<unicode>", "*", "*", "<unicode>", "*", "<unicode>",
                                      "*"))
})

test_that("WF8.4 (Normal) Missing value test of Process summary table with all but one missing value in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 8, 1, "", 0, 6, 12))
})

test_that("WF8.5 (Normal) Missing value test of X-bar & R control chart with all but one missing value in measurement", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chartW8")
})

#### All missing ####
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1MissingAll", "dm2MissingAll", "dm3MissingAll", "dm4MissingAll", "dm5MissingAll")
options$axisLabels <- "Time"
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
                       "datasets/processCapabilityStudy/processCapabilityAnalysisWideFormatDebug.csv", options)

test_that("WF9.1 (Normal) Missing value test of Process performance (total) table with all missing values in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("*", "*", "*", "*", "<unicode>", "*", "*", "*", "*", "*", "*"
                                 ))
})

test_that("WF9.2 (Normal) Missing value test of Non-conformance statistics table with all missing values in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("*", "*", "*", "ppm &lt; LSL", "*", "*", "*", "ppm &gt; USL",
                                      "*", "*", "*", "ppm total"))
})

test_that("WF9.3 (Normal) Missing value test of Process capability (within) table with all missing values in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("<unicode>", "<unicode>", "*", "*", "*", "*", "*", "*"))
})

test_that("WF9.4 (Normal) Missing value test of Process summary table with all missing values in measurement", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, "NaN", 0, "", 0, 6, 12))
})

test_that("WF9.5 (Normal) Missing value test of X-bar & R control chart with all missing values in measurement", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chartW9")
})

### Missing values in stages ####
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
options$stagesWideFormat <- "StageMissing7"
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
                       "datasets/processCapabilityStudy/processCapabilityAnalysisWideFormatDebug.csv", options)

test_that("WF10.1 (Normal) Missing value test of Capability of the process plot with missing values in stages", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-processW10")
})

test_that("WF10.2 (Normal) Missing value test of Process performance (total) table with missing values in stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.77, 0.6, 0.93, 0.98, 0.71, 0.51, 0.91, 1.25, 0.74, 0.71, 1.21,
                                      "1 (BL)", 0.87, "-", "-", 0.97, 0.79, 0.54, 1.03, 1.16, 0.71,
                                      0.79, 1.23, 2, 0.1, 0.04, 0.15, -0.01, 0.08, "-", "-", -0.09,
                                      "-", 0.08, "-", "Change (2 vs. BL)", 1.18, "-", "-", 1.3, 1.13,
                                      0.81, 1.46, 1.46, 0.95, 1.13, 1.63, 3, 0.41, 0.28, 0.55, 0.32,
                                      0.42, "-", "-", 0.21, "-", 0.42, "-", "Change (3 vs. BL)"))
})

test_that("WF10.3 (Normal) Missing value test of Non-conformance statistics table with missing values in stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(89.06, 241.46, 5.61, 152.41, -83.44, 55.04, 170.79, 0.23, 115.75,
                                      -54.81, 0, 0, 0, 0, 0, "ppm &lt; LSL", 16467.96, 9157.29, 345.62,
                                      -7310.67, -16122.34, 13882.24, 7738.07, 49.55, -6144.17, -13832.69,
                                      0, 0, 0, 0, 0, "ppm &gt; USL", 16557.02, 9398.75, 351.23, -7158.26,
                                      -16205.79, 13937.28, 7908.86, 49.78, -6028.42, -13887.5, 0,
                                      0, 0, 0, 0, "ppm total"))
})

test_that("WF10.4 (Normal) Missing value test of Process capability (within) table with missing values in stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.01, 0.73, 0.5, 0.96, 1.29, 0.73, 0.73, 1.28, "1 (BL)", 1, 0.81,
                                      0.52, 1.09, 1.19, 0.68, 0.81, 1.31, 2, -0.01, 0.08, "-", "-",
                                      -0.1, "-", 0.08, "-", "Change (2 vs. BL)", 1.49, 1.3, 0.87,
                                      1.73, 1.68, 1.01, 1.3, 1.94, 3, 0.48, 0.57, "-", "-", 0.39,
                                      "-", 0.57, "-", "Change (3 vs. BL)"))
})

test_that("WF10.5 (Normal) Missing value test of Process summary table with missing values in stages", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.648, 25, 2.04044929692784, 1.97764402407567, "1 (BL)", 6,
                                      12, 0, 7.16, 20, 2.05154627178213, 1.99914015477214, 2, 6, 12,
                                      "-", -0.488, -5, 0.011096974854286, 0.0214961306964743, "Change (2 vs. BL)",
                                      "-", "-", 0, 6.77, 20, 1.5413937643711, 1.34350816852966, 3,
                                      6, 12, "-", -0.878, -5, -0.499055532556746, -0.634135855546002,
                                      "Change (3 vs. BL)", "-", "-"))
})

test_that("WF10.6 (Normal) Missing value test of distribution histogram with missing values in stages", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogramW10")
})

test_that("WF10.7 (Normal) Missing value test of Probability plot against normal distribution with missing values in stages", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distributionW10")
})

test_that("WF10.8 (Normal) Missing value test of Summary of test against the normal distribution table with missing values in stages", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.31, 7.648, 25, 0.54, 2.04044929692784, "1 (BL)", 0.67, 7.16,
                                      20, 0.07, 2.05154627178213, 2, "-", -0.488, -5, "-", 0.011096974854286,
                                      "Change (2 vs. BL)", 0.52, 6.77, 20, 0.16, 1.5413937643711,
                                      3, "-", -0.878, -5, "-", -0.499055532556746, "Change (3 vs. BL)"
                                 ))
})

test_that("WF10.9 (Normal) Missing value test of X-bar & R control chart with missing values in stages", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chartW10")
})

## Changed options ####

### Fixed subgroup sizes (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1Missing1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
options$subgroupSizeUnequal <- "fixedSubgroupSize"
options$fixedSubgroupSizeValue <- 4
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
                       "datasets/processCapabilityStudy/processCapabilityAnalysisWideFormatDebug.csv", options)

test_that("WF11 (Normal) Options test of X-bar & R control chart with fixed subgroup size", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chartW11")
})

### All checkboxes / All control charts ####

#### S-bar (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarS"
options$controlChartSdEstimationMethodGroupSizeLargerThanOne <- "sBar"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisWideFormatDebug.csv", options)

test_that("WF12.1 (Normal) Option test of Capability of the process plot with s-bar calculation method", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-processW12")
})

test_that("WF12.2 (Normal) Option test of Process performance (total) table with s-bar calculation method", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.93, 0.83, 1.04, 1.08, 0.88, 0.77, 1, 1.27, 0.95, 0.88, 1.2
                                 ))
})

test_that("WF12.3 (Normal) Option test of Non-conformance statistics table with s-bar calculation method", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(68.39, 64.2, 0, "ppm &lt; LSL", 4020.39, 3893.42, 10000, "ppm &gt; USL",
                                      4088.78, 3957.62, 10000, "ppm total"))
})

test_that("WF12.4 (Normal) Option test of Process capability (within) table with s-bar calculation method", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.08, 0.89, 0.76, 1.01, 1.28, 0.94, 0.89, 1.22))
})

test_that("WF12.5 (Normal) Option test of Process summary table with s-bar calculation method", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.08, 100, 1.85635681000733, 1.84880707556386, 6, 12))
})

test_that("WF12.6 (Normal) Option test of distribution histogram with s-bar calculation method", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogramW12")
})

test_that("WF12.7 (Normal) Option test of Probability plot against normal distribution with s-bar calculation method", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distributionW12")
})

test_that("WF12.8 (Normal) Option test of Summary of test against the normal distribution table with s-bar calculation method", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.43, 7.08, 100, 0.3, 1.85635681000733))
})

test_that("WF12.9 (Normal) Option test of X-bar & s control chart with s-bar calculation method", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chartW12")
})


#### xbar-mR ####
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarMR"
options$controlChartSdEstimationMethodGroupSizeLargerThanOne <- "rBar"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisWideFormatDebug.csv", options)

test_that("WF13 (Normal) Option test of X-bar & mR control chart", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-mr-control-chartW13")
})

#### Boundaries (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$lowerSpecificationLimitBoundary <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$upperSpecificationLimitBoundary <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisWideFormatDebug.csv", options)


test_that("WF14.1 (Normal) Option test of Capability of the process plot with boundaries", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-processW14")
})

test_that("WF14.2 (Normal) Option test of Process performance (total) table with boundaries", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("*", "*", "*", "*", "*"))
})

test_that("WF14.3 (Normal) Option test of Non-conformance statistics table with boundaries", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("*", "*", 0, "ppm &lt; LB", "*", "*", 10000, "ppm &gt; UB", "*",
                                      "*", 10000, "ppm total"))
})

test_that("WF14.4 (Normal) Option test of Process capability (within) table with boundaries", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("*", "*", "*", "*"))
})

test_that("WF14.5 (Normal) Option test of Process summary table with boundaries", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.08, 100, 1.85635681000733, 1.76268271711092, 6, 12))
})

test_that("WF14.6 (Normal) Option test of distribution histogram with boundaries", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogramW14")
})

test_that("WF14.7 (Normal) Option test of Probability plot against normal distribution with boundaries", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distributionW14")
})

test_that("WF14.8 (Normal) Option test of Summary of test against the normal distribution table with boundaries", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.43, 7.08, 100, 0.3, 1.85635681000733))
})

test_that("WF14.9 (Normal) Option test of X-bar & R control chart with boundaries", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chartW14")
})

### Advanced options ####

#### Unbiasing constant for s-bar (verified with Minitab) ####
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarS"
options$controlChartSdEstimationMethodGroupSizeLargerThanOne <- "sBar"
options$controlChartSdUnbiasingConstant <- FALSE
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisWideFormatDebug.csv", options)

test_that("WF15.1 (Normal) Option test of Capability of the process plot with s-bar method and no unbiasing constant", {
  plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "capability-of-the-processW15")
})

test_that("WF15.2 (Normal) Option test of Process performance (total) table with s-bar method and no unbiasing constant", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.93, 0.83, 1.04, 1.08, 0.88, 0.77, 1, 1.27, 0.95, 0.88, 1.2
                                 ))
})

test_that("WF15.3 (Normal) Option test of Non-conformance statistics table with s-bar method and no unbiasing constant", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(68.39, 23.11, 0, "ppm &lt; LSL", 4020.39, 2319.55, 10000, "ppm &gt; USL",
                                      4088.78, 2342.65, 10000, "ppm total"))
})

test_that("WF15.4 (Normal) Option test of Process capability (within) table with s-bar method and no unbiasing constant", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.15, 0.94, 0.81, 1.07, 1.36, 1, 0.94, 1.29))
})

test_that("WF15.5 (Normal) Option test of Process summary table with s-bar method and no unbiasing constant", {
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 7.08, 100, 1.85635681000733, 1.73785203372984, 6, 12))
})

test_that("WF15.6 (Normal) Option test of distribution histogram with s-bar method and no unbiasing constant", {
  plotName <- results[["results"]][["histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogramW15")
})

test_that("WF15.7 (Normal) Option test of Probability plot against normal distribution with s-bar method and no unbiasing constant", {
  plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-plot-against-normal-distributionW15")
})

test_that("WF15.8 (Normal) Option test of Summary of test against the normal distribution table with s-bar method and no unbiasing constant", {
  table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.43, 7.08, 100, 0.3, 1.85635681000733))
})

test_that("WF15.9 (Normal) Option test of X-bar & s control chart with s-bar method and no unbiasing constant", {
  plotName <- results[["results"]][["xBar"]][["collection"]][["xBar_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chartW15")
})

## Report ####
options <- analysisOptions("processCapabilityStudies")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- c("dm1", "dm2", "dm3", "dm4", "dm5")
options$axisLabels <- "Time"
options$probabilityPlotRankMethod <- "bernard"
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartType <- "xBarR"
options$lowerSpecificationLimit <- TRUE
options$target <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- 0
options$targetValue <- 6
options$upperSpecificationLimitValue <- 12
options$report <- TRUE
options$reportDate <- "01.01.2020"
options$reportMiscellaneous <- "Comment"
options$reportProcessName <- "Process name"
options$reportReportedBy <- "Mrs. Doe"
options$reportTitle <- "Title"
set.seed(1)
results <- runAnalysis("processCapabilityStudies",
                       "datasets/processCapabilityStudy/processCapabilityAnalysisWideFormatDebug.csv", options)

test_that("WF16 (Normal) Basic test of report functionality", {
  plotName <- results[["results"]][["pcReport"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "titleW16")
})
