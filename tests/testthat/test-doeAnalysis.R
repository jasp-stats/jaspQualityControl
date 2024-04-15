context("DoE Analysis")
.numDecimals <- 2
# Testing factorial analysis (coded, without blocks) and residual plots (verified with other software)

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Yield"
options$fixedFactorsFactorial <- c("Exposure_time", "Develop_time", "Mask_dimension")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$plotNorm <- TRUE
options$plotHist <- TRUE
options$plotFitted <- TRUE
options$plotRunOrder <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(list(components = "Exposure_time"),
                           list(components = "Develop_time"),
                           list(components = "Mask_dimension"), list(components = c("Exposure_time",
    "Develop_time")), list(components = c("Develop_time", "Mask_dimension"
    )), list(components = c("Exposure_time", "Mask_dimension")),
    list(components = c("Exposure_time", "Develop_time", "Mask_dimension"
    )))
set.seed(1)
results <- runAnalysis("doeAnalysis", "DoEFactorialAnalysis.csv", options)

test_that("Residuals versus Fitted Values plot matches", {
  plotName <- results[["results"]][["plotFitted"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "residuals-versus-fitted-values")
})

test_that("Histogram of Residuals plot matches", {
  plotName <- results[["results"]][["plotHist"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-residuals")
})

test_that("Normal Probability Plot of Residuals matches", {
  skip("Fails only on Linux, need to investigate why.")
  plotName <- results[["results"]][["plotNorm"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "normal-probability-plot-of-residuals")
})

test_that("Residuals versus Run Order plot matches", {
  plotName <- results[["results"]][["plotRunOrder"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "residuals-versus-run-order")
})

test_that("ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(725.133928571429, 5075.9375, 7, 8.29316858980905, 0.00391648487987164,
                                      "Model", "", 5066.1875, 3, "", "", "<unicode> Linear terms",
                                      4590.0625, 4590.0625, 1, 52.4953538241601, 8.84512701642711e-05,
                                      "<unicode> <unicode> Exposure_time", 473.0625, 473.0625, 1,
                                      5.41029306647606, 0.0484630885931658, "<unicode> <unicode> Develop_time",
                                      3.0625, 3.0625, 1, 0.0350250178699071, 0.856202495502322, "<unicode> <unicode> Mask_dimension",
                                      "", 9.75, 4, "", "", "<unicode> Interaction terms", 1.5625,
                                      1.5625, 1, 0.0178699070764832, 0.896958544582933, "<unicode> <unicode> Exposure_time<unicode><unicode><unicode>Develop_time",
                                      3.0625, 3.0625, 1, 0.0350250178699071, 0.856202495502322, "<unicode> <unicode> Develop_time<unicode><unicode><unicode>Mask_dimension",
                                      0.0625, 0.0625, 1, 0.000714796283059328, 0.979325452661291,
                                      "<unicode> <unicode> Exposure_time<unicode><unicode><unicode>Mask_dimension",
                                      5.0625, 5.0625, 1, 0.0578984989278056, 0.815900529536508, "<unicode> <unicode> Exposure_time<unicode><unicode><unicode>Develop_time<unicode><unicode><unicode>Mask_dimension",
                                      87.4375, 699.5, 8, "", "", "Error", "", 5775.4375, 15, "", "",
                                      "Total"))
})

test_that("Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 30.3125, "", 1.18536689121333e-06, 2.33770052615813,
                                      "(Intercept)", 12.9668020607485, "", "A", -16.9375, -33.875,
                                      8.84512701642713e-05, 2.33770052615813, "Exposure_time", -7.2453677494079,
                                      1, "B", 5.4375, 10.875, 0.0484630885931658, 2.33770052615813,
                                      "Develop_time", 2.3260036686291, 1, "C", 0.4375, 0.875, 0.856202495502322,
                                      2.33770052615813, "Mask_dimension", 0.187149720464411, 1, "AB",
                                      -0.312499999999999, -0.624999999999998, 0.896958544582933, 2.33770052615813,
                                      "Exposure_time<unicode>Develop_time", -0.133678371760293, 1,
                                      "BC", -0.4375, -0.875000000000001, 0.856202495502322, 2.33770052615813,
                                      "Develop_time<unicode>Mask_dimension", -0.187149720464411, 1,
                                      "AC", -0.0625, -0.125, 0.979325452661291, 2.33770052615813,
                                      "Exposure_time<unicode>Mask_dimension", -0.0267356743520587,
                                      1, "ABC", 0.5625, 1.125, 0.815900529536508, 2.33770052615813,
                                      "Exposure_time<unicode>Develop_time<unicode>Mask_dimension",
                                      0.240621069168528, 1))
})

test_that("Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Yield = 30.31 (Intercept) - 16.94 A + 5.44 B + 0.44 C - 0.31 AB - 0.44 BC - 0.06 AC + 0.56 ABC"
                                 ))
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.772906814418821, 0.515534537426818, 0.878883634356705, 9.35080210463252
                                 ))
})



# Testing RSM analysis (coded, without block) and contour plots (verified with other software)
options <- analysisOptions("doeAnalysis")
options$designType <- "responseSurfaceDesign"
options$dependentResponseSurface <- "Vdk"
options$continuousFactorsResponseSurface <- c("Inlet_feeding", "Time", "Oil_temperature")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$rsmPredefinedModel <- TRUE
options$rsmPredefinedTerms <- "fullQuadratic"
options$modelTerms <- NULL
options$contourSurfacePlot <- TRUE
options$contourSurfacePlotType <- "contourPlot"
options$contourSurfacePlotVariables <- c("Inlet_feeding", "Time", "Oil_temperature")
options$contourSurfacePlotResponseDivision <- 5
options$fourInOneResidualPlot <- TRUE
options$tableAlias <- FALSE
set.seed(1)
results <- runAnalysis("doeAnalysis", "QT 9 p17 - RSM (15+6) Ovality Vdk.csv", options)

test_that("ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(17.4286687766283, 156.858018989655, 9, 34.6406642274952, 8.30307965125175e-07,
                                      "Model", "", 38.4572155403843, 3, "", "", "<unicode> Linear terms",
                                      11.8899000543329, 11.8899000543329, 1, 23.6319847923751, 0.000501646267633432,
                                      "<unicode> <unicode> Inlet_feeding", 24.9981706073659, 24.9981706073659,
                                      1, 49.6855637920343, 2.12989264950597e-05, "<unicode> <unicode> Time",
                                      1.56914487868547, 1.56914487868547, 1, 3.11878213783765, 0.105096442839075,
                                      "<unicode> <unicode> Oil_temperature", "", 6.41456198135817,
                                      3, "", "", "<unicode> Squared terms", 0.971816680445325, 0.971816680445325,
                                      1, 1.93155172947742, 0.19207390468129, "<unicode> <unicode> Inlet_feeding^2",
                                      5.07806473672077, 5.07806473672077, 1, 10.0929989389734, 0.00880891663240329,
                                      "<unicode> <unicode> Time^2", 0.364680564192077, 0.364680564192077,
                                      1, 0.724827417192739, 0.412715467088767, "<unicode> <unicode> Oil_temperature^2",
                                      "", 111.986241467913, 3, "", "", "<unicode> Interaction terms",
                                      101.961745328922, 101.961745328922, 1, 202.655901564035, 1.97376239173672e-08,
                                      "<unicode> <unicode> Inlet_feeding<unicode><unicode><unicode>Time",
                                      0.0982123904506134, 0.0982123904506134, 1, 0.195203803812119,
                                      0.667180323291378, "<unicode> <unicode> Inlet_feeding<unicode><unicode><unicode>Oil_temperature",
                                      9.92628374854026, 9.92628374854026, 1, 19.7291638717197, 0.000992134508871047,
                                      "<unicode> <unicode> Time<unicode><unicode><unicode>Oil_temperature",
                                      0.50312744184607, 5.53440186030677, 11, "", "", "Error", "",
                                      162.392420849962, 20, "", "", "Total"))
})

test_that("Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(11.3372638071208, "", 6.47199104788295e-12, 0.37687921214787,
                                      "(Intercept)", 30.081955814195, "", -1.36214285714286, -2.72428571428572,
                                      2.25952596434626e-05, 0.194497123284304, "Inlet_feeding", -7.00340876071343,
                                      1.20300751879699, 1.40850855060275, 2.81701710120549, 1.64160761307207e-05,
                                      0.194244773092596, "Time", 7.25120438598012, 1.18560348165058,
                                      -0.707142857142858, -1.41428571428572, 0.00391707189094081,
                                      0.194497123284304, "Oil_temperature", -3.63574969748626, 1.20300751879699,
                                      -0.606579758901038, -1.21315951780208, 0.19207390468129, 0.436450245724234,
                                      "Inlet_feeding^2", -1.38980276639436, "", -1.38657975890104,
                                      -2.77315951780207, 0.00880891663240336, 0.436450245724234, "Time^2",
                                      -3.1769480541824, "", -0.371579758901038, -0.743159517802075,
                                      0.412715467088763, 0.436450245724234, "Oil_temperature^2", -0.851367968150524,
                                      "", -2.96517857142857, -5.93035714285714, 1.97376239173673e-08,
                                      0.20829134541906, "Inlet_feeding<unicode>Time", -14.2357262394314,
                                      1.20300751879699, -0.0918643117465637, -0.183728623493127, 0.667180323291377,
                                      0.207923074104616, "Inlet_feeding<unicode>Oil_temperature",
                                      -0.441818745428621, 1.20300751879699, -0.92517857142857, -1.85035714285714,
                                      0.000992134508871048, 0.20829134541906, "Time<unicode>Oil_temperature",
                                      -4.44175234245672, 1.18560348165058))
})

test_that("Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Vdk = 11.34 (Intercept) - 1.36 Inlet_feeding + 1.41 Time - 0.71 Oil_temperature - 0.61 Inlet_feeding^2 - 1.39 Time^2 - 0.37 Oil_temperature^2 - 2.97 Inlet_feeding<unicode>Time - 0.09 Inlet_feeding<unicode>Oil_temperature - 0.93 Time<unicode>Oil_temperature"
                                 ))
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.965793350741183, 0.937924676228648, 0.981186342907651, 0.709314769228775
                                 ))
})
