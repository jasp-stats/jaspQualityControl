context("[Quality Control] Process Capability Study")
.numDecimals <- 2

# Long / Column format ####

## (Normal) Basic tests ####

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

#### Weibull ####
# options <- analysisOptions("processCapabilityStudies")
# options$measurementLongFormat <- "Diameter"
# options$subgroupSizeType <- "manual"
# options$manualSubgroupSizeValue <- 5
# options$stagesLongFormat <- "Stage"
# options$probabilityPlotRankMethod <- "bernard"
# options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
# options$nonNormalDistribution <- "weibull"
# options$nullDistribution <- "weibull"
# options$controlChartType <- "xBarR"
# options$lowerSpecificationLimit <- TRUE
# options$target <- TRUE
# options$upperSpecificationLimit <- TRUE
# options$lowerSpecificationLimitValue <- 0
# options$targetValue <- 6
# options$upperSpecificationLimitValue <- 12
# set.seed(1)
# results <- runAnalysis("processCapabilityStudies",
#                        "datasets/processCapabilityStudy/processCapabilityAnalysisLongFormatDebug.csv", options, makeTests = TRUE)

#### Lognormal ####

#### 3-parameter-Weibull ####

#### 3-parameter-lognormal ####

### Missing values in measurements ####

##### All but one missing ####

### Whole subgroup missing ####

### All missing ####

### Missing values in stages ####

### Missing values in subgroup variable ####

## Changed options ####

### Set different manual subgroup sizes ####

### Fixed vs actual sizes with unequal subgroup sizes ####

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
