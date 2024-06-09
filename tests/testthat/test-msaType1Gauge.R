# context("[Quality Control] MSA Type 1 Gauge")
#
# options <- analysisOptions("msaType1Gauge")
# options$measurement <- "dm"
# options$referenceValue <- -4
# options$toleranceRange <- 15
# options$histogram <- TRUE
# set.seed(1)
# results <- runAnalysis("msaType1Gauge", "msaType1.csv", options)
#
#
# test_that("Bias Histogram plot matches", {
#   plotName <- results[["results"]][["biasHistogram"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "bias-histogram")
# })
#
# test_that("Run Chart of dm plot matches", {
#   plotName <- results[["results"]][["biasRun"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "run-chart-of-dm")
# })
#
# test_that("Basic Statistics table results match", {
#   table <- results[["results"]][["biasTable"]][["collection"]][["biasTable_Basic"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(2.52008746203907, 0.0540000000000003, 0.360000000000002, -3.946,
#                                       -4, 0.420014577006512, 15))
# })
#
# test_that("Capability table results match", {
#   table <- results[["results"]][["biasTable"]][["collection"]][["biasTable_Capability"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(1.19, 1.15, 16.8, 17.43))
# })
#
# test_that("T-Test of Observed Bias Against 0 table results match", {
#   table <- results[["results"]][["biasTtest"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(0.0540000000000001, 49, -0.0653668220476197, 0.367744349288335,
#                                       0.909105737620188, 0.17336682204762))
# })
