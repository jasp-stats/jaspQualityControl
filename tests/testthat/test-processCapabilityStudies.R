context("[Quality Control] Process Capability Studies")

options <- analysisOptions("processCapabilityStudies")
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChartsType <- "xbarR"
options$lowerSpecificationField <- TRUE
options$rank <- "Bernard"
options$upperSpecificationField <- TRUE
options$variablesLong <- "Measurement1"
set.seed(1)
results <- runAnalysis("processCapabilityStudies", "partOperatorData.csv", options)


test_that("Capability of the Process plot matches", {
	plotName <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "capability-of-the-process", dir="processCapabilityStudies")
})

test_that("Overall Capability table results match", {
	table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableOverall"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.171532676484168, -1.19359203285214, -1.52311578649644, -0.864068279207846,
			 1.53665738582048, 0.127598556598227, -1.19359203285214, 0.215383203520269
			))
})

test_that("Performance table results match", {
	table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(2.01361451190873, 16.9211440957939, 0, "PPM &lt; LSL", 0.00247021703092543,
			 374.780947267839, 999828.712435699, 999359.808753505, 1e+06,
			 "PPM &gt; USL", 995231.851356578, 999997.553773326, 999830.726050211,
			 999376.7298976, 1e+06, "PPM total", "", ""))
})

test_that("Potential Capability (Within) table results match", {
	table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.154266470540219, -1.07344696035231, -1.41504931744125, -0.731844603263367,
			 1.38197990143274, 0.108585346037329, -1.07344696035231, 0.199877344433774,
			 -3.22034088105692))
})

test_that("Process Summary table results match", {
	table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-1, 7.95839450136667, 30, 1.94326433986529, 2.16076333480663,
			 1))
})

test_that("X-bar & R Chart plot matches", {
	plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "x-bar-r-chart", dir="processCapabilityStudies")
})

test_that("Histogram plot matches", {
	plotName <- results[["results"]][["histogram"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "histogram", dir="processCapabilityStudies")
})

test_that("Probability Plot matches", {
	plotName <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_ProbabilityPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "probability-plot", dir="processCapabilityStudies")
})

test_that("Summary of test against the Normal distribution table results match", {
	table <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.727071326290741, 7.95839450136667, 30, 0.535382300681328, 1.94326433986529
			))
})
