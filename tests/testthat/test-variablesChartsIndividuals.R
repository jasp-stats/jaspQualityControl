context("[Quality Control] Variables Charts for Individuals")

options <- analysisOptions("variablesChartsIndividuals")
options$variables <- "Measurement1"
set.seed(1)
results <- runAnalysis("variablesChartsIndividuals", "partOperatorData.csv", options)


test_that("Charts for: Measurement1 plot matches", {
	plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Measurement1"]][["collection"]][["Ichart_Measurement1_Plot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "charts-for-measurement1", dir="variablesChartsIndividuals")
})

test_that("Nelson tests' results for Individual chart table results match", {
	table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Measurement1"]][["collection"]][["Ichart_Measurement1_Table1"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(3, 16, 2, 17, 3, 18, 15, 25, 30))
})

test_that("Nelson tests' results for R chart table results match", {
	table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_Measurement1"]][["collection"]][["Ichart_Measurement1_Table2"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(3))
})
