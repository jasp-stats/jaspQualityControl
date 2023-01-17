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
dataset <- structure(list(Run.order = c(19L, 15L, 11L, 6L, 21L, 20L, 10L, 
22L, 27L, 1L, 5L, 4L, 12L, 2L, 25L, 18L, 8L, 13L, 16L, 14L, 17L, 
23L, 24L, 3L, 9L, 7L, 26L, 45L, 35L, 50L, 52L, 38L, 29L, 37L, 
53L, 46L, 41L, 49L, 54L, 43L, 51L, 40L, 48L, 36L, 39L, 42L, 28L, 
47L, 33L, 32L, 31L, 44L, 34L, 30L), Standard.order = c(1L, 2L, 
3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 
17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 1L, 2L, 
3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 
17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L), Nozzle = c(1L, 
2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 
3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 
1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 
2L, 3L, 1L, 2L, 3L), Speed = c(100L, 100L, 100L, 120L, 120L, 
120L, 140L, 140L, 140L, 100L, 100L, 100L, 120L, 120L, 120L, 140L, 
140L, 140L, 100L, 100L, 100L, 120L, 120L, 120L, 140L, 140L, 140L, 
100L, 100L, 100L, 120L, 120L, 120L, 140L, 140L, 140L, 100L, 100L, 
100L, 120L, 120L, 120L, 140L, 140L, 140L, 100L, 100L, 100L, 120L, 
120L, 120L, 140L, 140L, 140L), Speed_coded = c(-1L, -1L, -1L, 
0L, 0L, 0L, 1L, 1L, 1L, -1L, -1L, -1L, 0L, 0L, 0L, 1L, 1L, 1L, 
-1L, -1L, -1L, 0L, 0L, 0L, 1L, 1L, 1L, -1L, -1L, -1L, 0L, 0L, 
0L, 1L, 1L, 1L, -1L, -1L, -1L, 0L, 0L, 0L, 1L, 1L, 1L, -1L, -1L, 
-1L, 0L, 0L, 0L, 1L, 1L, 1L), Pressure = c(10L, 10L, 10L, 10L, 
10L, 10L, 10L, 10L, 10L, 15L, 15L, 15L, 15L, 15L, 15L, 15L, 15L, 
15L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 10L, 10L, 10L, 
10L, 10L, 10L, 10L, 10L, 10L, 15L, 15L, 15L, 15L, 15L, 15L, 15L, 
15L, 15L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L), Pressure_coded = c(-1L, 
-1L, -1L, -1L, -1L, -1L, -1L, -1L, -1L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, -1L, -1L, -1L, 
-1L, -1L, -1L, -1L, -1L, -1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), Blocks = c(1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
2L, 2L, 2L), Syruploss = c(-35L, 17L, -39L, -45L, -65L, -55L, 
-40L, 20L, 15L, 110L, 55L, 90L, -10L, -55L, -28L, 80L, 110L, 
110L, 4L, -23L, -30L, -40L, -64L, -61L, 31L, -20L, 54L, -25L, 
24L, -35L, -60L, -58L, -67L, 15L, 4L, -30L, 75L, 120L, 113L, 
30L, -44L, -26L, 54L, 44L, 135L, 5L, -5L, -55L, -30L, -62L, -52L, 
36L, -31L, 4L)), class = "data.frame", row.names = c(NA, -54L
))
results <- runAnalysis("doeAnalysis", dataset, options)

test_that("Model Summary table results match", {
	table <- results[["results"]][["tableSummary"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.870165811967448, 26, 27, 14.6620374545345, 3.96322013708724e-10,
			 0.933858055153228, 20.6518764280634))
})

test_that("Analysis of Variance table results match", {
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
