context("DoE Analysis")

# Syrup loss example 9-1 from SKF manual

options <- analysisOptions("doeAnalysis")
options$dependent <- "Syruploss"
options$fixedFactors <- c("Nozzle", "Speed_coded", "Pressure_coded")
options$modelTerms <- list(list(components = "Nozzle"), list(components = "Speed_coded"), 
    list(components = "Pressure_coded"), list(components = c("Nozzle", 
    "Speed_coded")), list(components = c("Nozzle", "Pressure_coded"
    )), list(components = c("Speed_coded", "Pressure_coded")), 
    list(components = c("Nozzle", "Speed_coded", "Pressure_coded"
    )))
set.seed(1)
results <- runAnalysis("doeAnalysis", "syruploss.csv", options)

test_that("Model Summary table results match", {
	table <- results[["results"]][["tableSummary"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.870165811967448, 0.735432220612914, 0.933858055153228, 20.6518764280634
			))
})

test_that("ANOVA table results match", {
	table <- results[["results"]][["tableAnova"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(6253.35897435897, 162587.333333333, 26, 14.6620374545345, 3.96322013708729e-10,
			 "Model", 496.888888888884, 993.777777777768, 2, 1.16503842646866,
			 0.327101550898847, "Nozzle", 30595.1666666666, 61190.3333333332,
			 2, 71.7354435326298, 1.57084318354504e-11, "Speed_coded", 34552.6666666667,
			 69105.3333333333, 2, 81.0144587729582, 3.89295583050645e-12,
			 "Pressure_coded", 1575.22222222222, 6300.88888888889, 4, 3.69336980591377,
			 0.015949847526571, "Nozzle<unicode><unicode><unicode>Speed_coded",
			 1878.47222222222, 7513.88888888888, 4, 4.40438973557379, 0.00718663002707217,
			 "Nozzle<unicode><unicode><unicode>Pressure_coded", 3213.58333333333,
			 12854.3333333333, 4, 7.53477921062914, 0.000326895391999146,
			 "Speed_coded<unicode><unicode><unicode>Pressure_coded", 578.597222222222,
			 4628.77777777778, 8, 1.35661716816465, 0.259495873905025, "Nozzle<unicode><unicode><unicode>Speed_coded<unicode><unicode><unicode>Pressure_coded",
			 426.5, 11515.5, 27, "", "", "Error", "", 174102.833333333, 53,
			 "", "", "Total"))
})

# Example from minitab

options <- analysisOptions("doeAnalysis")
options$dependent <- "Response"
options$fixedFactors <- c("A", "B", "C", "D")
options$modelTerms <- list(list(components = "A"), list(components = "B"), list(components = "C"), 
    list(components = "D"), list(components = c("A", "B")), list(
        components = c("A", "C")), list(components = c("A", "D"
    )), list(components = c("B", "C")), list(components = c("B", 
    "D")), list(components = c("C", "D")), list(components = c("A", 
    "B", "C")), list(components = c("A", "B", "D")), list(components = c("A", 
    "C", "D")), list(components = c("B", "C", "D")), list(components = c("A", 
    "B", "C", "D")))
set.seed(1)
results <- runAnalysis("doeAnalysis", "minitab.csv", options)


test_that("ANOVA table results match", {
	table <- results[["results"]][["tableAnova"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.0392916666666667, 0.589375, 4, "", "", "Model", 0.105625, 0.105625,
			 1, "", "", "A1", 0.030625, 0.030625, 1, "", "", "B1", 0.00562499999999997,
			 0.00562499999999997, 1, "", "", "C1", 0.225625, 0.225625, 1,
			 "", "", "D1", 0.050625, 0.050625, 1, "", "", "A1<unicode><unicode><unicode>B1",
			 0.00562500000000002, 0.00562500000000002, 1, "", "", "A1<unicode><unicode><unicode>C1",
			 0.005625, 0.005625, 1, "", "", "A1<unicode><unicode><unicode>D1",
			 0.015625, 0.015625, 1, "", "", "B1<unicode><unicode><unicode>C1",
			 0.00562500000000002, 0.00562500000000002, 1, "", "", "B1<unicode><unicode><unicode>D1",
			 0.030625, 0.030625, 1, "", "", "C1<unicode><unicode><unicode>D1",
			 0.000625000000000005, 0.000625000000000005, 1, "", "", "A1<unicode><unicode><unicode>B1<unicode><unicode><unicode>C1",
			 0.000625000000000002, 0.000625000000000002, 1, "", "", "A1<unicode><unicode><unicode>B1<unicode><unicode><unicode>D1",
			 0.075625, 0.075625, 1, "", "", "A1<unicode><unicode><unicode>C1<unicode><unicode><unicode>D1",
			 0.030625, 0.030625, 1, "", "", "B1<unicode><unicode><unicode>C1<unicode><unicode><unicode>D1",
			 0.000625000000000005, 0.000625000000000005, 1, "", "", "A1<unicode><unicode><unicode>B1<unicode><unicode><unicode>C1<unicode><unicode><unicode>D1",
			 "", "", 0, "", "", "Error", "", 0.589375, 15, "", "", "Total"
			))
})

test_that("Coefficients table results match", {
	table <- results[["results"]][["tableCoefficients"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.50625, "", "", "", "(Intercept)", "", "", 0.08125, 0.1625, "",
			 "", "A1", "", "", 0.04375, 0.0874999999999999, "", "", "B1",
			 "", "", -0.01875, -0.0374999999999999, "", "", "C1", "", "",
			 0.11875, 0.2375, "", "", "D1", "", "", -0.05625, -0.1125, "",
			 "", "A1<unicode><unicode><unicode>B1", "", "", 0.01875, 0.0375000000000001,
			 "", "", "A1<unicode><unicode><unicode>C1", "", "", 0.01875,
			 0.0375, "", "", "A1<unicode><unicode><unicode>D1", "", "", -0.03125,
			 -0.0625, "", "", "B1<unicode><unicode><unicode>C1", "", "",
			 -0.01875, -0.0375000000000001, "", "", "B1<unicode><unicode><unicode>D1",
			 "", "", -0.04375, -0.0875, "", "", "C1<unicode><unicode><unicode>D1",
			 "", "", 0.00625000000000002, 0.0125, "", "", "A1<unicode><unicode><unicode>B1<unicode><unicode><unicode>C1",
			 "", "", 0.00625000000000001, 0.0125, "", "", "A1<unicode><unicode><unicode>B1<unicode><unicode><unicode>D1",
			 "", "", -0.06875, -0.1375, "", "", "A1<unicode><unicode><unicode>C1<unicode><unicode><unicode>D1",
			 "", "", 0.04375, 0.0875, "", "", "B1<unicode><unicode><unicode>C1<unicode><unicode><unicode>D1",
			 "", "", -0.00625000000000002, -0.0125, "", "", "A1<unicode><unicode><unicode>B1<unicode><unicode><unicode>C1<unicode><unicode><unicode>D1",
			 "", ""))
})

test_that("Model Summary table results match", {
	table <- results[["results"]][["tableSummary"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", "", 1, ""))
})
