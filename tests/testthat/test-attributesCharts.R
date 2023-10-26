context("[Quality Control] Attributes Charts")

## Defectives
# NP
options <- analysisOptions("attributesCharts")
options$defectiveOrDefect <- "D"
options$total <- "Size"
options$attributesChart <- "defectives"
options$attributesChartDefectivesChartType <- "npChart"
set.seed(1)
results <- runAnalysis("attributesCharts", "SPC_NP.csv", options)

test_that("np Chart plot matches", {
  plotName <- results[["results"]][["NPchartPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "np-chart")
})

test_that("Test results for np chart table results match", {
  table <- results[["results"]][["NelsonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 2, 3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
                                 ))
})

# P
options$attributesChartDefectivesChartType <- "pChart"
results <- runAnalysis("attributesCharts", "SPC_P.csv", options)

test_that("p Chart plot matches", {
  plotName <- results[["results"]][["PchartPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "p-chart")
})

# Laney's P
options$attributesChartDefectivesChartType <- "laneyPPrimeChart"
results <- runAnalysis("attributesCharts", "SPC_P.csv", options)

test_that("Laney p' Chart plot matches", {
  plotName <- results[["results"]][["LaneyPPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "laney-p-chart")
})

## Defects
# C
options$attributesChartDefectivesChartType <- "npChart"
options$attributesChart <- "defects"
options$attributesChartDefectsChartType <- "cChart"
results <- runAnalysis("attributesCharts", "SPC_NP.csv", options)

test_that("c Chart plot matches", {
  plotName <- results[["results"]][["CchartPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "c-chart")
})

test_that("Test results for c chart table results match", {
  table <- results[["results"]][["NelsonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 3, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 18, 19, 20))
})

# U
options$attributesChartDefectsChartType <- "uChart"
results <- runAnalysis("attributesCharts", "SPC_P.csv", options)

test_that("u Chart plot matches", {
  plotName <- results[["results"]][["UchartPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "u-chart")
})

# Laneys U
options$attributesChartDefectsChartType <- "laneyUPrimeChart"
results <- runAnalysis("attributesCharts", "SPC_P.csv", options)

test_that("Laney u' Chart plot matches", {
  plotName <- results[["results"]][["LaneyUPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "laney-u-chart")
})

## I MR
options$attributesChart <- "xmr"
results <- runAnalysis("attributesCharts", "SPC_P.csv", options)

test_that("Individuals and Moving Range Chart plot matches", {
  plotName <- results[["results"]][["IPlotA"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "individuals-and-moving-range-chart")
})
