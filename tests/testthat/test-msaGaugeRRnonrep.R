context("[Quality Control] MSA Gauge non-replicabe")

options <- analysisOptions("msaGaugeRRnonrep")
options$parts <- "Batch"
options$operators <- "Operator"
options$measurements <- "Result"
options$NRrCharts <- TRUE
options$NRxbarCharts <- TRUE
options$NRpartOperatorGraph <- TRUE
options$NRpartOperatorGraphAll <- TRUE
options$NRoperatorGraph <- TRUE
options$anovaGaugeNestedReport <- T
set.seed(1)

results <- runAnalysis("msaGaugeRRnonrep", "msaGaugeNN.csv", options)

test_that("Measurements by Operator plot matches", {
  plotName <- results[["results"]][["NRoperatorGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "measurements-by-operator")
})

test_that("Operator A plot matches", {
  plotName <- results[["results"]][["NRpartOperatorGraph"]][["collection"]][["NRpartOperatorGraph_A"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "operator-a")
})

test_that("Operator B plot matches", {
  plotName <- results[["results"]][["NRpartOperatorGraph"]][["collection"]][["NRpartOperatorGraph_B"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "operator-b")
})

test_that("Operator C plot matches", {
  plotName <- results[["results"]][["NRpartOperatorGraph"]][["collection"]][["NRpartOperatorGraph_C"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "operator-c")
})

test_that("Range Chart by Operator plot matches", {
  plotName <- results[["results"]][["NRrCharts"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "range-chart-by-operator")
})

test_that("Xbar Chart by Operator plot matches", {
  plotName <- results[["results"]][["NRxbarCharts"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "xbar-chart-by-operator")
})

test_that("Report plot matches", {
  plotName <- results[["results"]][["anovaGaugeNestedReport"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "report")
})

test_that("Components of Variation plot matches", {
  plotName <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "components-of-variation")
})

test_that("Gauge r&R (Nested) table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2, 0.696059139499019, 2.18133333333334, 4.36266666666668, 0.51759708068839,
                                      "Operator", 12, 38.849173553719, 3.13383333333333, 37.606, 4.4622407258139e-09,
                                      "Batch(Operator)", 15, "", 0.0806666666666667, 1.21, "", "Repeatability",
                                      29, "", "", 43.1786666666667, "", "Total"))
})

test_that("Variance Components table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0806666666666667, 5.01892466428164, "Total Gauge r &amp; R",
                                      0.0806666666666667, 5.01892466428164, "Repeatability", 0, 0,
                                      "Reproducibility", 1.52658333333333, 94.9810753357184, "Part-To-Part",
                                      1.60725, 100, "Total Variation"))
})

test_that("Gauge Evaluation table results match", {
  table <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.284018778721877, 22.4029566447861, "Total Gauge r &amp; R",
                                      1.70411267233126, 0.284018778721877, 22.4029566447861, "Repeatability",
                                      1.70411267233126, 0, 0, "Reproducibility", 0, 1.23554981013852,
                                      97.458234816622, "Part-To-Part", 7.41329886083112, 1.26777363910124,
                                      100, "Total Variation", 7.60664183460744))
})
