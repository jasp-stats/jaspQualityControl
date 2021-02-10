context("Gauge r&R")
library("jaspTools")

options <- analysisOptions("msaGaugeRR")
options$operators <- "Operator"
options$parts <- "Part"
options$measurements <- c("1", "2", "3")
set.seed(1)
results <- runAnalysis("msaGaugeRR", "gaugeRRwide.csv", options)


test_that("r & R Table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(22.5584010871524, "Total r &amp; R", 0.848225424385098, 22.5396980046314,
                                      "Repeatability", 0.8475221639879, 0.0187030825210353, "Reproducibility",
                                      0.000703260397198535, 0.0187030825210353, "Operator", 0.000703260397198535,
                                      77.4415989128476, "Part-To-Part", 2.91190554016356, 100, "Total Variation",
                                      3.76013096454866))
})

test_that("r & R Table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.920991544144189, 47.4956851589198, 55.2594926486514, "Total r &amp; R",
                                      5.52594926486514, 0.920609669723222, 47.4759918323265, 55.2365801833933,
                                      "Repeatability", 5.52365801833933, 0.0265190572456589, 1.36759213660489,
                                      1.59114343473954, "Reproducibility", 0.159114343473954, 0.0265190572456589,
                                      1.36759213660489, 1.59114343473954, "Operator", 0.159114343473954,
                                      1.7064306432327, 88.0009084685196, 102.385838593962, "Part-To-Part",
                                      10.2385838593962, 1.93910571257698, 100, 116.346342754619, "Total Variation",
                                      11.6346342754619))
})

test_that("Variation Components Graph plot matches", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "variation-components-graph", dir="msaGaugeRR")
})

test_that("Two-Way ANOVA Table with Interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 28.8374040004607, 27.05467202546, 1.3113141108791e-18, 243.49204822914,
                                      "Part", 2, 0.925856545015132, 0.868619975903856, 0.401777425414269,
                                      1.73723995180771, "Operator", 18, 0.581263589155473, 0.545329800306983,
                                      0.899320321555707, 9.8159364055257, "Part  x  Operator", 60,
                                      "", 0.938179873092175, "", 56.2907923855305, "Repeatability",
                                      89, 311.336016972003, "Total"))
})

test_that("Two-Way ANOVA Table without Interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 31.9220820115876, 27.05467202546, 1.20668981418678e-22, 243.49204822914,
                                      "Part", 2, 1.02489352233183, 0.868619975903856, 0.363615551623408,
                                      1.73723995180771, "Operator", 78, "", 0.8475221639879, "", 66.1067287910562,
                                      "Repeatability", 89, 311.336016972003, "Total"))
})

test_that("Measurements by Part plot matches", {
  options <- analysisOptions("msaGaugeRR")
  options$operators <- "Operator"
  options$parts <- "Part"
  options$measurements <- c("1", "2", "3")
  options$gaugeANOVA <- FALSE
  options$gaugeByPart <- TRUE
  set.seed(1)
  results <- runAnalysis("msaGaugeRR", "gaugeRRwide.csv", options)
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "measurements-by-part", dir="msaGaugeRR")
})

test_that("Measurements by Part plot with all measurements matches", {
  options <- analysisOptions("msaGaugeRR")
  options$operators <- "Operator"
  options$parts <- "Part"
  options$measurements <- c("1", "2", "3")
  options$gaugeANOVA <- FALSE
  options$gaugeByPart <- TRUE
  options$gaugeByPartAll <- TRUE
  set.seed(1)
  results <- runAnalysis("msaGaugeRR", "gaugeRRwide.csv", options)
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "measurements-by-part-all", dir="msaGaugeRR")
})

test_that("Measurements by Operator plot matches", {
  options <- analysisOptions("msaGaugeRR")
  options$operators <- "Operator"
  options$parts <- "Part"
  options$measurements <- c("1", "2", "3")
  options$gaugeANOVA <- FALSE
  options$gaugeByOperator <- TRUE
  set.seed(1)
  results <- runAnalysis("msaGaugeRR", "gaugeRRwide.csv", options)
  plotName <- results[["results"]][["gaugeByOperator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "measurements-by-operator", dir="msaGaugeRR")
})

test_that("Parts by Operator Interaction plot matches", {
  options <- analysisOptions("msaGaugeRR")
  options$operators <- "Operator"
  options$parts <- "Part"
  options$measurements <- c("1", "2", "3")
  options$gaugeANOVA <- FALSE
  options$gaugeByInteraction <- TRUE
  set.seed(1)
  results <- runAnalysis("msaGaugeRR", "gaugeRRwide.csv", options)
  plotName <- results[["results"]][["gaugeByInteraction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "parts-by-operator-interaction", dir="msaGaugeRR")
})


#with Historical Standard Deviation

options <- analysisOptions("msaGaugeRR")
options$operators <- "Operator"
options$parts <- "Part"
options$measurements <- c("1", "2", "3")
options$standardDeviationReference <- "historicalStandardDeviation"
options$historicalStandardDeviationValue <- 5
set.seed(1)
results <- runAnalysis("msaGaugeRR", "gaugeRRwide.csv", options)


test_that("r & R Table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(22.5584010871524, "Total r &amp; R", 0.848225424385098, 22.5396980046314,
                                      "Repeatability", 0.8475221639879, 0.0187030825210353, "Reproducibility",
                                      0.000703260397198535, 0.0187030825210353, "Operator", 0.000703260397198535,
                                      77.4415989128476, "Part-To-Part", 2.91190554016356, 100, "Total Variation",
                                      3.76013096454866))
})

test_that("r & R Table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.920991544144189, 18.4198308828838, 55.2594926486514, "Total r &amp; R",
                                      5.52594926486514, 0.920609669723222, 18.4121933944644, 55.2365801833933,
                                      "Repeatability", 5.52365801833933, 0.0265190572456589, 0.530381144913178,
                                      1.59114343473954, "Reproducibility", 0.159114343473954, 0.0265190572456589,
                                      0.530381144913178, 1.59114343473954, "Operator", 0.159114343473954,
                                      4.91444550031994, 98.2889100063988, 294.866730019196, "Part-To-Part",
                                      29.4866730019196, 5, 100, 300, "Total Variation", 30))
})

test_that("Variation Components Graph plot matches", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "variation-components-graph-hsd", dir="msaGaugeRR")
})

test_that("Two-Way ANOVA Table with Interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 28.8374040004607, 27.05467202546, 1.3113141108791e-18, 243.49204822914,
                                      "Part", 2, 0.925856545015132, 0.868619975903856, 0.401777425414269,
                                      1.73723995180771, "Operator", 18, 0.581263589155473, 0.545329800306983,
                                      0.899320321555707, 9.8159364055257, "Part  x  Operator", 60,
                                      "", 0.938179873092175, "", 56.2907923855305, "Repeatability",
                                      89, 311.336016972003, "Total"))
})

test_that("Two-Way ANOVA Table without Interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 31.9220820115876, 27.05467202546, 1.20668981418678e-22, 243.49204822914,
                                      "Part", 2, 1.02489352233183, 0.868619975903856, 0.363615551623408,
                                      1.73723995180771, "Operator", 78, "", 0.8475221639879, "", 66.1067287910562,
                                      "Repeatability", 89, 311.336016972003, "Total"))
})

#change tolerance

options <- analysisOptions("msaGaugeRR")
options$operators <- "Operator"
options$parts <- "Part"
options$measurements <- c("1", "2", "3")
options$tolerance <- 15
set.seed(1)
results <- runAnalysis("msaGaugeRR", "gaugeRRwide.csv", options)


test_that("r & R Table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(22.5584010871524, "Total r &amp; R", 0.848225424385098, 22.5396980046314,
                                      "Repeatability", 0.8475221639879, 0.0187030825210353, "Reproducibility",
                                      0.000703260397198535, 0.0187030825210353, "Operator", 0.000703260397198535,
                                      77.4415989128476, "Part-To-Part", 2.91190554016356, 100, "Total Variation",
                                      3.76013096454866))
})

test_that("r & R Table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.920991544144189, 47.4956851589198, 36.8396617657676, "Total r &amp; R",
                                      5.52594926486514, 0.920609669723222, 47.4759918323265, 36.8243867889289,
                                      "Repeatability", 5.52365801833933, 0.0265190572456589, 1.36759213660489,
                                      1.06076228982636, "Reproducibility", 0.159114343473954, 0.0265190572456589,
                                      1.36759213660489, 1.06076228982636, "Operator", 0.159114343473954,
                                      1.7064306432327, 88.0009084685196, 68.2572257293079, "Part-To-Part",
                                      10.2385838593962, 1.93910571257698, 100, 77.564228503079, "Total Variation",
                                      11.6346342754619))
})

test_that("Variation Components Graph plot matches", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "variation-components-graph-tol", dir="msaGaugeRR")
})

test_that("Two-Way ANOVA Table with Interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 28.8374040004607, 27.05467202546, 1.3113141108791e-18, 243.49204822914,
                                      "Part", 2, 0.925856545015132, 0.868619975903856, 0.401777425414269,
                                      1.73723995180771, "Operator", 18, 0.581263589155473, 0.545329800306983,
                                      0.899320321555707, 9.8159364055257, "Part  x  Operator", 60,
                                      "", 0.938179873092175, "", 56.2907923855305, "Repeatability",
                                      89, 311.336016972003, "Total"))
})

test_that("Two-Way ANOVA Table without Interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 31.9220820115876, 27.05467202546, 1.20668981418678e-22, 243.49204822914,
                                      "Part", 2, 1.02489352233183, 0.868619975903856, 0.363615551623408,
                                      1.73723995180771, "Operator", 78, "", 0.8475221639879, "", 66.1067287910562,
                                      "Repeatability", 89, 311.336016972003, "Total"))
})



#without interaction removal

options <- analysisOptions("msaGaugeRR")
options$operators <- "Operator"
options$parts <- "Part"
options$measurements <- c("1", "2", "3")
options$alphaForANOVA <- 1
set.seed(1)
results <- runAnalysis("msaGaugeRR", "gaugeRRwide.csv", options)


test_that("r & R Table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(24.3669573417114, "Total r &amp; R", 0.948956212278737, 24.0902463682638,
                                      "Repeatability", 0.938179873092175, 0.276710973447582, "Reproducibility",
                                      0.0107763391865624, 0.276710973447582, "Operator", 0.0107763391865624,
                                      75.6330426582886, "Part-To-Part", 2.94548246946144, 0, "Operator x Part",
                                      0, 100, "Total Variation", 3.89443868174018))
})

test_that("r & R Table results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_RRtable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.974143835518522, 49.3628983566721, 58.4486301311113, "Total r &amp; R",
                                      5.84486301311113, 0.968596857878537, 49.0818157450026, 58.1158114727122,
                                      "Repeatability", 5.81158114727122, 0.103809147894405, 5.26033243671521,
                                      6.22854887366429, "Reproducibility", 0.622854887366429, 0.103809147894405,
                                      5.26033243671521, 6.22854887366429, "Operator", 0.622854887366429,
                                      1.71624079588543, 86.96725973508, 102.974447753126, "Part-To-Part",
                                      10.2974447753126, 0, 0, 0, "Operator x Part", 0, 1.97343322201188,
                                      100, 118.405993320713, "Total Variation", 11.8405993320713
                                 ))
})

test_that("Variation Components Graph plot matches", {
  plotName <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_VarCompGraph"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "variation-components-graph-with-interaction", dir="msaGaugeRR")
})

test_that("Two-Way ANOVA Table with Interaction results match", {
  table <- results[["results"]][["gaugeANOVA"]][["collection"]][["gaugeANOVA_anovaTable1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 28.8374040004607, 27.05467202546, 1.3113141108791e-18, 243.49204822914,
                                      "Part", 2, 0.925856545015132, 0.868619975903856, 0.401777425414269,
                                      1.73723995180771, "Operator", 18, 0.581263589155473, 0.545329800306983,
                                      0.899320321555707, 9.8159364055257, "Part  x  Operator", 60,
                                      "", 0.938179873092175, "", 56.2907923855305, "Repeatability",
                                      89, 311.336016972003, "Total"))
})
