context("[Quality Control] Variables Charts for Subgroups")

options <- analysisOptions("variablesChartsSubgroups")
options$variables <- c("Measurement1", "Measurement2", "Measurement3")
options$time <- "Part"
options$Xbarchart <- TRUE
options$Schart <- TRUE
set.seed(1)
results <- runAnalysis("variablesChartsSubgroups", "partOperatorData.csv", options)


test_that("Nelson tests' results for R chart table results match", {
	table <- results[["results"]][["NelsonTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(3, 4))
})

test_that("Nelson tests' results for X-bar chart table results match", {
	table <- results[["results"]][["NelsonTable2"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 6, 1, 6, 1, 6, 3, 5, 5, 5, 8, 8, 10, 10, 10))
})

test_that("Nelson tests' results for s chart table results match", {
	table <- results[["results"]][["NelsonTableS"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(3, 4))
})

test_that("Nelson tests' results for X-bar chart table results match", {
	table <- results[["results"]][["NelsonTableX"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 6, 1, 6, 1, 6, 3, 5, 5, 5, 8, 8, 10, 10, 10))
})

test_that("Xbar & s Control Chart plot matches", {
	plotName <- results[["results"]][["SPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "xbar-s-control-chart", dir="variablesChartsSubgroups")
})

test_that("X-bar & R Control Chart plot matches", {
	plotName <- results[["results"]][["XbarPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart", dir="variablesChartsSubgroups")
})