context("[Quality Control] Attributes Charts")

## Defectives
# NP
options <- analysisOptions("attributesCharts")
options$D <- "D"
options$total <- "Size"
set.seed(1)
results <- runAnalysis("attributesCharts", "tests\\testthat\\P_dat.csv", options, makeTests = T)

test_that("NP Control Chart plot matches", {
  plotName <- results[["results"]][["NPchartPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "np-control-chart")
})

test_that("Test result for NP chart table results match", {
  table <- results[["results"]][["NelsonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 2, 3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
                                 ))
})

# P
options$TypeDefectives <- "pchart"
results <- runAnalysis("attributesCharts", "tests\\testthat\\P_dat.csv", options, makeTests = T)

test_that("Test result for P chart table results match", {
  table <- results[["results"]][["NelsonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 2, 3, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
                                 ))
})

test_that("P Control Chart plot matches", {
  plotName <- results[["results"]][["PchartPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "p-control-chart")
})

# Laney's P
options$TypeDefectives <- "Laneyprimechart"
results <- runAnalysis("attributesCharts", "tests\\testthat\\P_dat.csv", options, makeTests = T)

test_that("Laney P' Control Chart plot matches", {
  options <- analysisOptions("attributesCharts")
  options$D <- "D"
  options$total <- "Size"
  options$TypeDefectives <- "Laneyprimechart"
  set.seed(1)
  results <- runAnalysis("attributesCharts", "tests\testthat\P_dat.csv", options)
  plotName <- results[["results"]][["LaneyPPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "laney-p-control-chart")
})

## Defects
# C
options$Attributes <- "Defects"
results <- runAnalysis("attributesCharts", "tests\\testthat\\P_dat.csv", options, makeTests = T)

test_that("C Control Chart plot matches", {
  plotName <- results[["results"]][["CchartPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "c-control-chart")
})

test_that("Test result for C chart table results match", {
  table <- results[["results"]][["NelsonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 3, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 18, 19, 20))
})

# U
options$TypeDefects <- "uchart"
results <- runAnalysis("attributesCharts", "tests\\testthat\\P_dat.csv", options, makeTests = T)

test_that("Test result for U chart table results match", {
  table <- results[["results"]][["NelsonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 3, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 18, 19, 20))
})

test_that("U Control Chart plot matches", {
  plotName <- results[["results"]][["UchartPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "u-control-chart")
})

# Laneys U
options$TypeDefects <- "Laneychart"
results <- runAnalysis("attributesCharts", "tests\\testthat\\P_dat.csv", options, makeTests = T)

test_that("Laney U' Control Chart plot matches", {
  plotName <- results[["results"]][["LaneyUPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "laney-u-control-chart")
})

## I MR
options$Attributes <- "ImR"
results <- runAnalysis("attributesCharts", "tests\\testthat\\P_dat.csv", options, makeTests = T)

test_that("Individual and Moving Range Control Charts plot matches", {
  plotName <- results[["results"]][["IPlotA"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "individual-and-moving-range-control-charts")
})
