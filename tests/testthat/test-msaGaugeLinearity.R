context("[Quality Control] MSA Gauge Linearity")

options <- analysisOptions("msaGaugeLinearity")
options$part <- "Part"
options$measurement <- "Measurement"
options$standard <- "Reference"
set.seed(1)
results <- runAnalysis("msaGaugeLinearity", "msaLinearity.csv", options)

test_that("Bias and Linearity plot matches", {
  plotName <- results[["results"]][["LB"]][["collection"]][["LB_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "bias-and-linearity")
})

test_that("Percentage Process Variation Graph plot matches", {
  plotName <- results[["results"]][["LB"]][["collection"]][["LB_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "percentage-process-variation-graph")
})

test_that("Gauge Bias table results match", {
  table <- results[["results"]][["LB"]][["collection"]][["LB_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.491666666666667, 2.49166666666667, 1, 2.87233310444284e-08,
                                      2, 0.125, 4.125, 2, 0.353991325267683, 4, 0.0250000000000004,
                                      6.025, 3, 0.667130710762814, 6, -0.291666666666667, 7.70833333333333,
                                      4, 6.41948050554358e-07, 8, -0.616666666666667, 9.38333333333333,
                                      5, 1.55444480038029e-08, 10, -0.0533333333333334, "Average",
                                      0.356307101472113))
})

test_that("Regression Model table results match", {
  table <- results[["results"]][["LB"]][["collection"]][["LB_table2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0725242725916125, 10.1575188601321, 0.736666666666666, "Intercept",
                                      1.73379959546223e-14, 0.0109334454718107, -12.0425594114992,
                                      -0.131666666666667, "Slope", 2.03771558228568e-17))
})

test_that("Gauge Linearity table results match", {
  table <- results[["results"]][["LB"]][["collection"]][["LB_table3"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.239539788646537, 13.1666666666667, 0.714318415932242))
})
