context("[Quality Control] Attributes Charts")

## Defectives
# NP
options <- analysisOptions("attributesCharts")
options$D <- "D"
options$total <- "Size"
options$Attributes <- "Defectives"
options$TypeDefectives <- "npchart"
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


## Defects
# C
options$TypeDefectives <- "npchart"
options$Attributes <- "Defects"
options$TypeDefects <- "cchart"
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
test_that("u Chart plot matches", {
  options <- analysisOptions("attributesCharts")
  options$D <- "D"
  options$total <- "Size"
  options$timeStamp <- "ï..Month"
  options$Attributes <- "Defects"
  options$TypeDefectives <- "npchart"
  options$TypeDefects <- "uchart"
  set.seed(1)
  results <- runAnalysis("attributesCharts", "SPC_P.csv", options)
  plotName <- results[["results"]][["UchartPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "u-chart")
})

# P
test_that("u Chart plot matches", {
  options <- analysisOptions("attributesCharts")
  options$D <- "D"
  options$total <- "Size"
  options$timeStamp <- "ï..Month"
  options$Attributes <- "Defects"
  options$TypeDefectives <- "pchart"
  options$TypeDefects <- "uchart"
  set.seed(1)
  results <- runAnalysis("attributesCharts", "SPC_P.csv", options)
  plotName <- results[["results"]][["UchartPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "u-chart")
})

# Laney's P
test_that("u Chart plot matches", {
  options <- analysisOptions("attributesCharts")
  options$D <- "D"
  options$total <- "Size"
  options$timeStamp <- "ï..Month"
  options$Attributes <- "Defects"
  options$TypeDefectives <- "Laneyprimechart"
  options$TypeDefects <- "uchart"
  set.seed(1)
  results <- runAnalysis("attributesCharts", "SPC_P.csv", options)
  plotName <- results[["results"]][["UchartPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "u-chart")
})

# Laneys U
test_that("Laney u' Chart plot matches", {
  options <- analysisOptions("attributesCharts")
  options$D <- "D"
  options$total <- "Size"
  options$timeStamp <- "ï..Month"
  options$Attributes <- "Defects"
  options$TypeDefectives <- "Laneyprimechart"
  options$TypeDefects <- "Laneychart"
  set.seed(1)
  results <- runAnalysis("attributesCharts", "SPC_P.csv", options)
  plotName <- results[["results"]][["LaneyUPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "laney-u-chart")
})

## I MR
test_that("Individuals and Moving Range Chart plot matches", {
  options <- analysisOptions("attributesCharts")
  options$D <- "D"
  options$total <- "Size"
  options$timeStamp <- "ï..Month"
  options$Attributes <- "ImR"
  options$TypeDefectives <- "Laneyprimechart"
  options$TypeDefects <- "Laneychart"
  set.seed(1)
  results <- runAnalysis("attributesCharts", "SPC_P.csv", options)
  plotName <- results[["results"]][["IPlotA"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "individuals-and-moving-range-chart")
})
