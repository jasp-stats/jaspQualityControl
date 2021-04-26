context("[Quality Control] Gauge r&R")

options <- analysisOptions("msaGaugeRR")
options$operators <- "Operator"
options$parts <- "Part"
options$measurements <- c("1", "2", "3")
options$gaugeRchart <- TRUE
options$gaugeXbarChart <- TRUE
options$gaugeScatterPlotOperators <- TRUE
options$gaugeScatterPlotFitLine <- TRUE
options$gaugeScatterPlotOriginLine <- TRUE
options$gaugeByPart <- TRUE
options$gaugeByPartAll <- TRUE
options$gaugeByOperator <- TRUE
options$gaugeByInteraction <- TRUE
set.seed(1)
results <- runAnalysis("msaGaugeRR", "gaugeRRwide.csv", options)


test_that("Gauge r & R Variance Components table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(22.5584010871524, "Total Gauge r &amp; R", 0.848225424385098,
                                      22.5396980046314, "Repeatability", 0.8475221639879, 0.0187030825210353,
                                      "Reproducibility", 0.000703260397198535, 0.0187030825210353,
                                      "Operator", 0.000703260397198535, 77.4415989128476, "Part-To-Part",
                                      2.91190554016356, 100, "Total Variation", 3.76013096454866
                                 ))
})

test_that("Gauge Evaluation table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.920991544144189, 47.4956851589198, 55.2594926486514, "Total Gauge r &amp; R",
                                      5.52594926486514, 0.920609669723222, 47.4759918323265, 55.2365801833933,
                                      "Repeatability", 5.52365801833933, 0.0265190572456589, 1.36759213660489,
                                      1.59114343473954, "Reproducibility", 0.159114343473954, 0.0265190572456589,
                                      1.36759213660489, 1.59114343473954, "Operator", 0.159114343473954,
                                      1.7064306432327, 88.0009084685196, 102.385838593962, "Part-To-Part",
                                      10.2385838593962, 1.93910571257698, 100, 116.346342754619, "Total Variation",
                                      11.6346342754619))
})

test_that("Components of Variation plot matches", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "components-of-variation", dir="msaGaugeRR")
})

test_that("Two-Way ANOVA Table with Interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 49.6115781866863, 27.05467202546, 7.05242342472244e-11, 243.49204822914,
                                      "Part", 2, 1.59283423611708, 0.868619975903856, 0.230713495496991,
                                      1.73723995180771, "Operator", 18, 0.581263589155473, 0.545329800306983,
                                      0.899320321555707, 9.8159364055257, "Part  x  Operator", 60,
                                      0.938179873092175, 56.2907923855305, "Repeatability", 89, 311.336016972003,
                                      "Total"))
})

test_that("Two-Way ANOVA Table without Interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 31.9220820115876, 27.05467202546, 1.20668981418678e-22, 243.49204822914,
                                      "Part", 2, 1.02489352233183, 0.868619975903856, 0.363615551623408,
                                      1.73723995180771, "Operator", 78, "", 0.8475221639879, "", 66.1067287910562,
                                      "Repeatability", 89, 311.336016972003, "Total"))
})

test_that("Parts by Operator Interaction plot matches", {
  plotName <- results[["results"]][["gaugeByInteraction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "parts-by-operator-interaction", dir="msaGaugeRR")
})

test_that("Measurement by Operator plot matches", {
  plotName <- results[["results"]][["gaugeByOperator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "measurement-by-operator", dir="msaGaugeRR")
})

test_that("Measurements by Part plot matches", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "measurements-by-part", dir="msaGaugeRR")
})

test_that("Descriptives table results match", {
  table <- results[["results"]][["gaugeDescriptives"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(12.8193648683409, 8.22048370714336, 5.11399638200386, "A", 2.12241545205159,
                                      11.2273933064237, 8.12693974847847, 4.87426997919696, "B", 1.72559311117233,
                                      11.8288297458125, 7.89034069287904, 4.81895359386578, "C", 1.78703692206271,
                                      12.8193648683409, 8.07925471616695, 4.81895359386578, "Overall",
                                      1.87033619813857))
})

test_that("Operator A plot matches", {
  plotName <- results[["results"]][["gaugeRchart"]][["collection"]][["gaugeRchart_A"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "operator-ar", dir="msaGaugeRR")
})

test_that("Operator B plot matches", {
  plotName <- results[["results"]][["gaugeRchart"]][["collection"]][["gaugeRchart_B"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "operator-br", dir="msaGaugeRR")
})

test_that("Operator C plot matches", {
  plotName <- results[["results"]][["gaugeRchart"]][["collection"]][["gaugeRchart_C"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "operator-cr", dir="msaGaugeRR")
})

test_that("Matrix Plot for Operators matches", {
  plotName <- results[["results"]][["gaugeScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "matrix-plot-for-operators", dir="msaGaugeRR")
})

test_that("Operator A plot matches", {
  plotName <- results[["results"]][["gaugeXbarChart"]][["collection"]][["gaugeXbarChart_A"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "operator-ax", dir="msaGaugeRR")
})

test_that("Operator B plot matches", {
  plotName <- results[["results"]][["gaugeXbarChart"]][["collection"]][["gaugeXbarChart_B"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "operator-bx", dir="msaGaugeRR")
})

test_that("Operator C plot matches", {
  plotName <- results[["results"]][["gaugeXbarChart"]][["collection"]][["gaugeXbarChart_C"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "operator-cx", dir="msaGaugeRR")
})
