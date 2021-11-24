context("[Quality Control] Process Capability Study")

#Long format
options <- analysisOptions("processCapabilityStudies")
options$variablesLong <- "Diameter"
options$subgroups <- "Time"
options$rank <- "Bernard"
options$lowerSpecificationField <- TRUE
options$upperSpecificationField <- TRUE
options$targetValueField <- TRUE
options$lowerSpecification <- -16
options$targetValue <- -8
options$upperSpecification <- 0
set.seed(1)
results <- runAnalysis("processCapabilityStudies", "SPCSubgroups_Long.csv", options)

test_that("ImR Charts matches", {
  plotName <- results[["results"]][["ImR Charts"]][["collection"]][["ImR Charts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "ImR Charts-0")
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
options$pcDataFormat <- "PCwideFormat"
options$variables <- c("dm1", "dm2", "dm3", "dm4", "dm5")
set.seed(1)
results <- runAnalysis("processCapabilityStudies", "SPCSubgroups_Wide.csv", options)

test_that("ImR Charts matches2", {
  plotName <- results[["results"]][["ImR Charts"]][["collection"]][["ImR Charts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "ImR Charts-2")
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
