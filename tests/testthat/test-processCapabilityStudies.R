context("[Quality Control] Process Capability Studies")

options <- analysisOptions("processCapabilityStudies")
options$capabilityStudy <- "initialCapabilityAnalysis"
options$lowerSpecification <- 4
options$lowerSpecificationField <- TRUE
options$nonNormalCapabilityStudy <- TRUE
options$nullDistribution <- "Lognormal"
options$rank <- "Bernard"
options$targetValue <- 8
options$targetValueField <- TRUE
options$upperSpecification <- 12
options$upperSpecificationField <- TRUE
options$variables <- c("Measurement1", "Measurement2", "Measurement3")
set.seed(1)
results <- runAnalysis("processCapabilityStudies", "partOperatorData.csv", options)


test_that("Process Capability based on the Weibull Distribution table results match", {
	table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_capabilityTableNonNormal"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(4.94218019580985, 4, 8.07925471611111, 1.87033619812937, 8.80523617115434,
			 12))
})

test_that("Capability of the Process plot matches", {
	plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "capability-of-the-process", dir="processCapabilityStudies")
})

test_that("Process Performance (Total) table results match", {
	table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.712884311743994, 0.698759450807161, 0.727009172680828, 1.36617298250936,
			 0.698759450807161))
})

test_that("Process Capability (Within) table results match", {
	table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1.37070002254134, 1.34354141225133, 1.39785863283134, 1.34354141225133,
			 4.030624236754))
})

test_that("Process Summary table results match", {
	table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(4, 8.07925471611111, 30, 1.87033619812937, 0.972738973813743,
			 8, 12))
})

test_that("R Chart plot matches", {
	plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_rplot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "r-chart", dir="processCapabilityStudies")
})

test_that("X-bar Chart plot matches", {
	plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_xplot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "x-bar-chart", dir="processCapabilityStudies")
})

test_that("Distribution Plot matches", {
	plotName <- results[["results"]][["histogram"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "distribution-plot", dir="processCapabilityStudies")
})

test_that("Measurement1 plot matches", {
	plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_plotContainer"]][["collection"]][["probabilityContainer_plotContainer_Measurement1"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "measurement1", dir="processCapabilityStudies")
})

test_that("Measurement2 plot matches", {
	plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_plotContainer"]][["collection"]][["probabilityContainer_plotContainer_Measurement2"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "measurement2", dir="processCapabilityStudies")
})

test_that("Measurement3 plot matches", {
	plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_plotContainer"]][["collection"]][["probabilityContainer_plotContainer_Measurement3"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "measurement3", dir="processCapabilityStudies")
})

test_that("Summary of Tests Against the Lognormal Distribution table results match", {
	table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.508725540645823, 2.04619141068134, 30, 0.73653710866348, "No",
			 0.235586590929892, "Measurement1", 0.3362181689652, 2.04342958808724,
			 30, 0.907985736560375, "No", 0.217214387143492, "Measurement2",
			 0.852722059928624, 2.0971825233772, 30, 0.443502084476377, "No",
			 0.245886916677861, "Measurement3"))
})
