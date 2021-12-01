context("[Quality Control] Variables Charts for Subgroups")

## Long
options <- analysisOptions("variablesChartsSubgroups")
options$variablesLong <- "Diameter"
options$subgroups <- "Time"
options$Wlimits <- TRUE
set.seed(1)
results <- runAnalysis("variablesChartsSubgroups", "SPCSubgroups_Long.csv", options)

# R cahrt
test_that("X-bar & R Control Chart plot matches", {
  plotName <- results[["results"]][["XbarPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart")
})


# S chart
options$TypeChart <- "Schart"
results <- runAnalysis("variablesChartsSubgroups", "SPCSubgroups_Long.csv", options)

test_that("X-bar & s Control Chart plot matches", {
  plotName <- results[["results"]][["SPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart")
})

## Wide
options$CCDataFormat <- "CCwideFormat"
options$TypeChart <- "Xbarchart"
options$variables <- c("dm1", "dm2", "dm3", "dm4", "dm5")
set.seed(1)
results <- runAnalysis("variablesChartsSubgroups", "SPCSubgroups_Wide.csv", options)

test_that("X-bar & R Control Chart2 plot matches", {
  plotName <- results[["results"]][["XbarPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart2")
})

# S chart
options$TypeChart <- "Schart"
results <- runAnalysis("variablesChartsSubgroups", "SPCSubgroups_Wide.csv", options)

test_that("X-bar & s Control Chart2 plot matches", {
  plotName <- results[["results"]][["SPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart2")
})
