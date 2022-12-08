context("[Quality Control] Variables Charts for Individuals")

options <- analysisOptions("variablesChartsIndividuals")
options$variables <- "Diameter"
options$subgroups <- "Time"
options$CorPlot <- TRUE
set.seed(1)
results <- runAnalysis("variablesChartsIndividuals", "SPCSubgroups_Long.csv", options)


test_that("Diameter plot matches", {
  plotName <- results[["results"]][["CorPlot"]][["collection"]][["CorPlot_Diameter"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "diameter")
})

test_that("titleless-plot-1 matches", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Diameter"]][["collection"]][["Ichart_Diameter_Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "titleless-plot-1")
})

test_that("Test results for Diameter for Individuals chart table results match", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Diameter"]][["collection"]][["Ichart_Diameter_Table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("16:55"))
})

test_that("Test results for Diameter for Range chart table results match", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Diameter"]][["collection"]][["Ichart_Diameter_Table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("10:10", "16:10", "16:10", "16:10", "16:10", "20:10", "20:10",
                                      "20:10", "21:00", "21:00", "21:00"))
})

# test for different moving range lengths (verified with Minitab)
options$CorPlot <- FALSE
options$movingRangeLength <- 3
results <- runAnalysis("variablesChartsIndividuals", "SPCSubgroups_Long.csv", options)


test_that("titleless-plot-2 matches", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Diameter"]][["collection"]][["Ichart_Diameter_Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "titleless-plot-2")
})

test_that("Test results for Diameter for Individuals chart table results match", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Diameter"]][["collection"]][["Ichart_Diameter_Table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("16:55"))
})

test_that("Test results for Diameter for Range chart table results match", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Diameter"]][["collection"]][["Ichart_Diameter_Table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("08:25", "08:25", "16:10", "16:10", "20:10", "20:10", "20:10",
                                      "21:00", "21:00", "21:00", "21:00", "21:00", "22:25", "00:15",
                                      "01:05", "01:05"))
})


options$movingRangeLength <- 5
results <- runAnalysis("variablesChartsIndividuals", "SPCSubgroups_Long.csv", options)


test_that("titleless-plot-3 matches", {
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Diameter"]][["collection"]][["Ichart_Diameter_Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "titleless-plot-3")
})

test_that("Test results for Diameter for Individuals chart table results match", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Diameter"]][["collection"]][["Ichart_Diameter_Table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("16:55"))
})

test_that("Test results for Diameter for Range chart table results match", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Diameter"]][["collection"]][["Ichart_Diameter_Table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("06:55", "06:55", "06:55", "06:55", "08:25", "08:25", "08:25",
                                      "08:25", "08:25", "09:40", "09:40", "09:40", "09:40", "11:40",
                                      "13:05", "13:05", "13:05", "13:05", "13:05", "15:05", "16:10",
                                      "16:55", "20:10", "20:10", "20:10", "21:00", "21:00", "21:00",
                                      "21:00", "00:15", "00:15", "00:15", "00:15", "00:15"))
})


# test analysis of stages plot

test_that("titleless-plot-4 matches", {
  options <- analysisOptions("variablesChartsIndividuals")
  options$variables <- "Bags"
  options$subgroups <- "Month"
  options$split <- "Split"
  set.seed(1)
  results <- runAnalysis("variablesChartsIndividuals", "AnalysisOfStages.csv", options)
  plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Bags"]][["collection"]][["Ichart_Bags_Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "titleless-plot-4")
})


