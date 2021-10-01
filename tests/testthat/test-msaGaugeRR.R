context("[Quality Control] Gauge r&R")

# Long format
options <- analysisOptions("msaGaugeRR")
options$gaugeRRdataFormat <- "gaugeRRlongFormat"
options$measurementsLong <- "Dm"
options$operators <- "Operators"
options$parts <- "Parts"
options$tolerance <- TRUE
options$gaugeRchart <- TRUE
options$gaugeXbarChart<- TRUE
options$gaugeScatterPlotOperators <- TRUE
options$gaugeScatterPlotFitLine <- TRUE
options$gaugeScatterPlotOriginLine <- TRUE
options$gaugeByPart <- TRUE
options$gaugeByPartAll <- TRUE
options$gaugeByOperator <- TRUE
options$gaugeByInteraction <- TRUE
results <- runAnalysis("msaGaugeRR", "GageRandr_long.csv", options)

test_that("Gauge r & R Variance Components table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.43, "Total Gauge r &amp; R", 0.157929724596392, 1.22, "Repeatability",
                                      0.13454415954416, 0.21, "Reproducibility", 0.0233855650522319,
                                      0.21, "Operator", 0.0233855650522319, 98.57, "Part-to-Part",
                                      10.8561068903662, 100, "Total Variation", 11.0140366149625
                                 ))
})

test_that("Gauge Evaluation table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.397403729972923, 11.97, 238.44, "Total Gauge r &amp; R", 2.38442237983754,
                                      0.366802616599391, 11.05, 220.08, "Repeatability", 2.20081569959634,
                                      0.15292339602635, 4.61, 91.75, "Reproducibility", 0.917540376158101,
                                      0.15292339602635, 4.61, 91.75, "Operator", 0.917540376158101,
                                      3.29486067844547, 99.28, 1976.92, "Part-to-Part", 19.7691640706728,
                                      3.31874021504585, 100, 1991.24, "Total Variation", 19.9124412902751
                                 ))
})

test_that("Components of Variation plot matches", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "components-of-variation")
})

test_that("Two-Way ANOVA Table with Interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 622.789783889978, 97.8395061728396, 1.41581280737466e-20, 880.555555555556,
                                      "Parts", 2, 5.32220039292733, 0.836111111111118, 0.0152789978971192,
                                      1.67222222222223, "Operators", 18, 1.22946859903382, 0.157098765432099,
                                      0.268355463232597, 2.82777777777779, "Parts  *  Operators",
                                      60, 0.127777777777778, 7.66666666666667, "Repeatability", 89,
                                      892.722222222223, "Total"))
})

test_that("Two-Way ANOVA Table without Interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 727.192518087171, 97.8395061728396, 2.19585066080569e-71, 880.555555555556,
                                      "Parts", 2, 6.21439915299104, 0.836111111111118, 0.00313130866789579,
                                      1.67222222222223, "Operators", 78, "", 0.13454415954416, "",
                                      10.4944444444445, "Repeatability", 89, 892.722222222223, "Total"
                                 ))
})

test_that("Parts by Operator Interaction plot matches", {
  plotName <- results[["results"]][["gaugeByInteraction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "parts-by-operator-interaction")
})

test_that("Measurement by Operator plot matches", {
  plotName <- results[["results"]][["gaugeByOperator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "measurement-by-operator")
})

test_that("Measurements by Part plot matches", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "measurements-by-part")
})

test_that("Descriptives table results match", {
  table <- results[["results"]][["gaugeDescriptives"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.5, -4.68333333333333, -11, "Operator A", 3.24697029428266,
                                      -0.5, -4.61666666666667, -11, "Operator B", 3.23153463418761,
                                      -0.5, -4.36666666666667, -11, "Operator C", 3.12093413660998,
                                      -0.5, -4.55555555555556, -11, "Overall", 3.1671101601526))
})

test_that("Range Chart by Operator plot matches", {
  plotName <- results[["results"]][["gaugeRchart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "range-chart-by-operator")
})

test_that("Matrix Plot for Operators matches", {
  plotName <- results[["results"]][["gaugeScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "matrix-plot-for-operators")
})

test_that("Average Chart by Operator plot matches", {
  plotName <- results[["results"]][["gaugeXbarChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "average-chart-by-operator")
})


# Wide Format
options$gaugeRRdataFormat <- "gaugeRRwideFormat"
options$operators <- "Operator"
options$parts <- "Part"
options$measurements <- c("Measurement1","Measurement2","Measurement3")
results <- runAnalysis("msaGaugeRR", "partOperatorData.csv", options)

test_that("Gauge r & R Variance Components table results match wide", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(22.56, "Total Gauge r &amp; R", 0.848225424491385, 22.54, "Repeatability",
                                      0.847522164107094, 0.02, "Reproducibility", 0.000703260384290253,
                                      0.02, "Operator", 0.000703260384290253, 77.44, "Part-to-Part",
                                      2.91190554000432, 100, "Total Variation", 3.7601309644957))
})

test_that("Gauge Evaluation table results match wide", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.920991544201892, 47.5, 552.59, "Total Gauge r &amp; R", 5.52594926521135,
                                      0.920609669787959, 47.48, 552.37, "Repeatability", 5.52365801872775,
                                      0.0265190570022815, 1.37, 15.91, "Reproducibility", 0.159114342013689,
                                      0.0265190570022815, 1.37, 15.91, "Operator", 0.159114342013689,
                                      1.70643064318604, 88, 1023.86, "Part-to-Part", 10.2385838591162,
                                      1.93910571256332, 100, 1163.46, "Total Variation", 11.6346342753799
                                 ))
})

test_that("Components of Variation plot matches wide", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "components-of-variation-wide")
})

test_that("Two-Way ANOVA Table with Interaction results match wide", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 49.6115781426451, 27.0546720241459, 7.05242347812389e-11, 243.492048217314,
                                      "Part", 2, 1.59283423428891, 0.868619975635802, 0.230713495855352,
                                      1.7372399512716, "Operator", 18, 0.581263589632294, 0.545329800764598,
                                      0.899320321215792, 9.81593641376276, "Part  *  Operator", 60,
                                      0.938179873109843, 56.2907923865906, "Repeatability", 89, 311.336016968939,
                                      "Total"))
})

test_that("Two-Way ANOVA Table without Interaction results match wide", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 31.9220820055477, 27.0546720241459, 1.20668982102463e-22, 243.492048217314,
                                      "Part", 2, 1.02489352187141, 0.868619975635802, 0.363615551786536,
                                      1.7372399512716, "Operator", 78, "", 0.847522164107094, "",
                                      66.1067288003534, "Repeatability", 89, 311.336016968939, "Total"
                                 ))
})

test_that("Parts by Operator Interaction plot matches wide", {
  plotName <- results[["results"]][["gaugeByInteraction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "parts-by-operator-interaction-wide")
})

test_that("Measurement by Operator plot matches wide", {
  plotName <- results[["results"]][["gaugeByOperator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "measurement-by-operator-wide")
})

test_that("Measurements by Part plot matches wide", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "measurements-by-part-wide")
})

test_that("Descriptives table results match wide", {
  table <- results[["results"]][["gaugeDescriptives"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(12.81936487, 8.22048370693333, 5.113996382, "A", 2.12241545183144,
                                      11.22739331, 8.1269397486, 4.874269979, "B", 1.7255931113897,
                                      11.82882975, 7.8903406928, 4.818953594, "C", 1.78703692208988,
                                      12.81936487, 8.07925471611111, 4.818953594, "Overall", 1.87033619812937
                                 ))
})

test_that("Range Chart by Operator plot matches wide", {
  plotName <- results[["results"]][["gaugeRchart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "range-chart-by-operator-wide")
})

test_that("Matrix Plot for Operators matches wide", {
  plotName <- results[["results"]][["gaugeScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "matrix-plot-for-operators-wide")
})

test_that("Average Chart by Operator plot matches wide", {
  plotName <- results[["results"]][["gaugeXbarChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "average-chart-by-operator-wide")
})


