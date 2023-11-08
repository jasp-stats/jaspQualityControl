context("DoE Analysis")

# Testing factorial analysis (coded, without blocks) and residual plots (verified with other software)

options <- analysisOptions("doeAnalysis")
options$dependent <- "Yield"
options$fixedFactors <- c("Exposure_time", "Develop_time", "Mask_dimension")
options$codeFactors <- TRUE
options$tableEquation <- TRUE
options$plotNorm <- TRUE
options$plotHist <- TRUE
options$plotFitted <- TRUE
options$plotRunOrder <- TRUE
options$tableAlias <- TRUE
options$modelTerms <- list(list(components = "Exposure_time"), list(components = "Develop_time"),
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
                                      "Model", 4590.0625, 4590.0625, 1, 52.4953538241601, 8.8451270164271e-05,
                                      "Exposure_time", 473.0625, 473.0625, 1, 5.41029306647606, 0.0484630885931658,
                                      "Develop_time", 3.0625, 3.0625, 1, 0.0350250178699071, 0.856202495502322,
                                      "Mask_dimension", 1.5625, 1.5625, 1, 0.0178699070764832, 0.896958544582933,
                                      "Exposure_time<unicode><unicode><unicode>Develop_time", 3.0625,
                                      3.0625, 1, 0.0350250178699071, 0.856202495502322, "Develop_time<unicode><unicode><unicode>Mask_dimension",
                                      0.0625, 0.0625, 1, 0.000714796283059328, 0.979325452661291,
                                      "Exposure_time<unicode><unicode><unicode>Mask_dimension", 5.0625,
                                      5.0625, 1, 0.0578984989278056, 0.815900529536508, "Exposure_time<unicode><unicode><unicode>Develop_time<unicode><unicode><unicode>Mask_dimension",
                                      87.4375, 699.5, 8, "", "", "Error", "", 5775.4375, 15, "", "",
                                      "Total"))
})

test_that("Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", 30.3125, "", 1.18536689121333e-06, 2.33770052615813,
                                      "(Intercept)", 12.9668020607485, "", "A", -16.9375, -67.75,
                                      8.84512701642711e-05, 2.33770052615813, "Exposure_time", -7.2453677494079,
                                      "", "B", 5.4375, 21.75, 0.0484630885931658, 2.33770052615813,
                                      "Develop_time", 2.32600366862911, "", "C", 0.4375, 1.75, 0.856202495502322,
                                      2.33770052615813, "Mask_dimension", 0.187149720464411, "", "AB",
                                      -0.312499999999999, -1.25, 0.896958544582933, 2.33770052615813,
                                      "Exposure_time<unicode><unicode><unicode>Develop_time", -0.133678371760293,
                                      "", "BC", -0.4375, -1.75, 0.856202495502322, 2.33770052615813,
                                      "Develop_time<unicode><unicode><unicode>Mask_dimension", -0.187149720464411,
                                      "", "AC", -0.0625000000000001, -0.25, 0.979325452661291, 2.33770052615813,
                                      "Exposure_time<unicode><unicode><unicode>Mask_dimension", -0.0267356743520587,
                                      "", "ABC", 0.5625, 2.25, 0.815900529536507, 2.33770052615813,
                                      "Exposure_time<unicode><unicode><unicode>Develop_time<unicode><unicode><unicode>Mask_dimension",
                                      0.240621069168528, ""))
})

test_that("Regression Equation in coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Yield = 30.312  - 16.938 A + 5.4375 B + 0.4375 C - 0.3125 AB - 0.4375 BC - 0.0625 AC + 0.5625 ABC"
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
options$dependent <- "Vdk"
options$continuousFactors <- c("Inlet_feeding", "Time", "Oil_temperature")
options$codeFactors <- TRUE
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
                                 list(32.0705532324527, 288.634979092074, 9, 63.7424051345315, 3.31223932582688e-08,
                                      "Model", 52.2485352887742, 156.745605866322, 3, 103.847516440495,
                                      2.36940699581611e-08, "Linear terms(Inlet_feeding,Time,Oil_temperature)",
                                      39.0123216181991, 117.036964854597, 3, 77.5396417954375, 1.10371314511512e-07,
                                      "Two-way interaction terms(Inlet_feeding,Time,Oil_temperature)",
                                      4.95080279038474, 14.8524083711542, 3, 9.84005716766173, 0.00190217445835828,
                                      "Squared terms(Inlet_feeding,Time,Oil_temperature)", 0.503127441846071,
                                      5.53440186030678, 11, "", "", "Error", 0.382337038728024, 1.91168519364012,
                                      5, 0.633232583015919, 0.633232583015919, "Lack of fit", 0.603786111111109,
                                      3.62271666666665, 6, "", "", "Pure error", "", 294.169380952381,
                                      20, "", "", "Total"))
})

test_that("Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(11.3372638071208, "", 6.471991047883e-12, 0.37687921214787, "(Intercept)",
                                      30.081955814195, "", -1.36214285714286, -8.6725, 2.25952596434628e-05,
                                      0.194497123284305, "Inlet_feeding", -7.00340876071343, "", 1.40850855060275,
                                      5.18296717368201, 1.64160761307207e-05, 0.194244773092596, "Time",
                                      7.25120438598012, "", -0.707142857142858, -7.39393000324301,
                                      0.00391707189094082, 0.194497123284305, "Oil_temperature", -3.6357496974,
                                      "", -2.96517857142857, -10.3362390490947, 1.97376239173674e-08,
                                      0.20829134541906, "Inlet_feeding<unicode><unicode><unicode>Time",
                                      -14.2357262394314, "", -0.0918643117465642, -0.522344164345772,
                                      0.667180323291375, 0.207923074104616, "Inlet_feeding<unicode><unicode><unicode>Oil_temperature",
                                      -0.441818745428623, "", -0.92517857142857, -3.15060053776106,
                                      0.00099213450887105, 0.20829134541906, "Time<unicode><unicode><unicode>Oil_temperature",
                                      -4.44175234245672, "", -0.606579758901038, -2.79525327541786,
                                      0.19207390468129, 0.436450245724234, "Inlet_feeding^2", -1.38980276639436,
                                      "", -1.38657975890104, -2.5834641343026, 0.00880891663240334,
                                      0.436450245724234, "Time^2", -3.1769480541824, "", -0.371579758901037,
                                      -0.603887873857458, 0.412715467088765, 0.436450245724234, "Oil_temperature^2",
                                      -0.851367968150521, ""))
})

test_that("Regression Equation in coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Vdk = 11.337 (Intercept) - 1.3621 Inlet_feeding + 1.4085 Time - 0.70714 Oil_temperature - 2.9652 Inlet_feeding<unicode><unicode><unicode>Time - 0.091864 Inlet_feeding<unicode><unicode><unicode>Oil_temperature - 0.92518 Time<unicode><unicode><unicode>Oil_temperature - 0.60658 Inlet_feeding^2 - 1.3866 Time^2 - 0.37158 Oil_temperature^2"
                                 ))
})

test_that("Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.965793350741183, 0.937924676228648, 0.981186342907651, 0.709314769228775
                                 ))
})
