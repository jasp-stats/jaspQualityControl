context("[Quality Control] Bayesian Gauge r&R")
.numDecimals <- 2
## tests long-format
### automatic model selection & posterior on variances (generalized inverse Gaussian)
options <- analysisOptions("msaBayesianGaugeRR")
options$measurementLongFormat <- "Dm"
options$operatorLongFormat <- "Operators"
options$partLongFormat <- "Parts"
options$tolerance <- TRUE
options$priorPlot <- TRUE
options$posteriorPlot <- TRUE
options$posteriorPlotType <- "var"
options$posteriorHistogram <- TRUE
options$posteriorCi <- TRUE
options$contourPlot <- TRUE
options$contourLSL <- -10
options$contourUSL <- 0
options$rChart <- TRUE
options$xBarChart <- TRUE
options$scatterPlot <- TRUE
options$scatterPlotFitLine <- TRUE
options$scatterPlotOriginLine <- TRUE
options$partMeasurementPlot <- TRUE
options$partMeasurementPlotAllValues <- TRUE
options$operatorMeasurementPlot <- TRUE
options$partByOperatorMeasurementPlot <- TRUE
options$trafficLightChart <- TRUE
options$distType <- "gig"
options$mcmcChains <- 2
options$customCiType <- "customCiQuantiles"
set.seed(1)
results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options)


test_that("L1 Model Comparison table results match", {
  table <- results[["results"]][["BFtable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9.1287767597893e+66, 0.0077421542931849, "Null model", 1, "",
                                      "Parts + Operators + Parts<unicode><unicode><unicode>Operators",
                                      0.113212575159566, 0.00872259468666002, "Parts + Operators"
                                 ))
})

test_that("L1 Contour plot matches", {
  plotName <- results[["results"]][["contourPlot"]][["collection"]][["contourPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 Contour plot")
})

test_that("L1 Producer's (δ) and Consumer's (β) Risk table results match", {
  table <- results[["results"]][["contourPlot"]][["collection"]][["contourPlot_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.00738797550717418, 0.0217229988723209, "<unicode>", 0.0504418242104906,
                                      0.0466531148350437, 0.108462891253646, "<unicode>", 0.210628344001828
                                 ))
})

test_that("L1 % Contribution to Total Variation table results match", {
  table <- results[["results"]][["contribTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.672854564674615, 3.01603148541284, "Total gauge r&amp;R", 10.6957816353577,
                                      0.428066317944134, 1.36262810109564, "Repeatability", 2.88196943211859,
                                      0.128937314959488, 1.65340338431719, "Reproducibility", 8.86803963413681,
                                      0.128937314959488, 1.65340338431719, "Operator", 8.86803963413681,
                                      89.3042183646423, 96.9839685145869, "Part-to-part", 99.3271454353254,
                                      "", 100, "Total variation", ""))
})

test_that("L1 Part by operator interaction plot matches", {
  plotName <- results[["results"]][["gaugeByInteraction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 part-by-operator-interaction")
})

test_that("L1 Measurements by operator plot matches", {
  plotName <- results[["results"]][["gaugeByOperator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 measurements-by-operator")
})

test_that("L1 Measurements by part plot matches", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 measurements-by-part")
})

test_that("L1 % Study Variation & % Tolerance table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_percStudyVarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(8.20277120172739, 22.1408240975003, 16.0560064136487, 31.6484709916296,
                                      "Total gauge r&amp;R", 32.7044057966602, 62.4858618220583, 6.54267771618492,
                                      19.0232155665105, 11.3621810602882, 22.2068164968796, "Repeatability",
                                      16.9763642511899, 26.042334748647, 3.59078423443732, 8.35951370532176,
                                      10.6197907610815, 21.0549057299311, "Reproducibility", 29.7792514328602,
                                      58.2732584746615, 3.59078423443732, 8.35951370532176, 10.6197907610815,
                                      21.0549057299311, "Operator", 29.7792514328602, 58.2732584746615,
                                      94.5009091800832, 131.338182169877, 98.4586706459067, 203.15001505139,
                                      "Part-to-part", 99.6630048891023, 325.127983561911, "", 134.972221466956,
                                      100, 206.127409873692, "Total variation", "", 327.452182037894
                                 ))
})

test_that("L1 Standard Deviation & Study Variation table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_stdTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.369013734958338, 2.21408240975003, 0.527474516527165, 3.16484709916296,
                                      "Total gauge r&amp;R", 1.04143103036764, 6.24858618220583, 0.317053592775174,
                                      1.90232155665105, 0.370113608281325, 2.22068164968796, "Repeatability",
                                      0.434038912477449, 2.6042334748647, 0.139325228422029, 0.835951370532175,
                                      0.350915095498847, 2.10549057299309, "Reproducibility", 0.971220974577692,
                                      5.82732584746615, 0.139325228422029, 0.835951370532175, 0.350915095498847,
                                      2.10549057299309, "Operator", 0.971220974577692, 5.82732584746615,
                                      2.18896970283128, 13.1338182169877, 3.38583358418984, 20.3150015051389,
                                      "Part-to-part", 5.41879972603185, 32.5127983561911, 2.24953702444927,
                                      13.4972221466956, 3.4354568312282, 20.6127409873691, "Total variation",
                                      5.45753636729823, 32.7452182037894))
})

test_that("L1 Matrix plot for operators matches", {
  plotName <- results[["results"]][["gaugeScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 matrix-plot-for-operators")
})

test_that("L1 Trace plot operators matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_g_Operators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 Trace plot operators")
})

test_that("L1 Trace plot parts matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_g_Parts"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 Trace plot parts")
})

test_that("L1 Trace plot error matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_sig2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 Trace plot error")
})

test_that("L1 Diagnostics table results match", {
  table <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(13156.2958719656, 14548.1788817734, 0.0643396308484891, 0.026604748502423,
                                      0.416661068666242, "<unicode><sup>2</sup><sub>Part</sub>", 0.999980107827912,
                                      8826.27376375007, 8779.64520704258, 0.00739624941904107, 0.000226595508114019,
                                      0.0456256051161729, "<unicode><sup>2</sup><sub>Operator</sub>",
                                      0.999989789442352, 9519.12272634791, 13138.4226911852, 0.000228956655794118,
                                      0.000320520616274043, 0.000765639483270394, "<unicode><sup>2</sup><sub>Error</sub>",
                                      1.00096014378277))
})

test_that("L1 g-prior plot matches", {
  plotName <- results[["results"]][["priorPlot"]][["collection"]][["priorPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 g-prior")
})

test_that("L1 rChart matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 rChart")
})

test_that("L1 Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("No test violations occurred."))
})

test_that("L1 Traffic light chart matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_trafficPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 Traffic light chart")
})

test_that("L1 Components of variation plot matches", {
  plotName <- results[["results"]][["varCompPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 components-of-variation")
})

test_that("L1 Variance Components table results match", {
  table <- results[["results"]][["varCompTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.136171136634584, 1.08457861699851, 0.328512928494555, 0.64962130987885,
                                      "Total gauge r&amp;R", 0.100522980701193, 0.188389777677887,
                                      0.137871376665235, 0.0224366411670959, "Repeatability", 0.0194115194133937,
                                      0.943270181509932, 0.190641551829319, 0.648481519656201, "Reproducibility",
                                      0.0194115194133937, 0.943270181509932, 0.190641551829319, 0.648481519656201,
                                      "Operator", 4.79158836022261, 29.3633907449221, 12.2100438401309,
                                      7.26694993903746, "Part-to-part", 5.06041682996016, 29.7847032117442,
                                      12.5385567686254, 7.30119170960903, "Total variation"))
})

test_that("L1 Error plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Error"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 error")
})

test_that("L1 Operator plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 operator")
})

test_that("L1 Part plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 part")
})

test_that("L1 Posterior Summary table results match", {
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4.78249678043341, 29.8522838983694, "<unicode><sup>2</sup><sub>Part</sub>",
                                      12.2063561157816, 0.019717972167065, 0.963666980675533, "<unicode><sup>2</sup><sub>Operator</sub>",
                                      0.190584413578255, 0.100232646227405, 0.187736626967306, "<unicode><sup>2</sup><sub>Error</sub>",
                                      0.137874639867539))
})

test_that("L1 xBar chart matches", {
  plotName <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L1 xBar chart")
})

test_that("L1 Test results for x-bar chart table results match", {
  table <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Operator A", "Point 1", "", "Point 2", "", "Point 3", "", "Point 4",
                                      "", "Point 5", "", "Point 6", "", "Point 7", "", "Point 8",
                                      "", "Point 9", "", "Point 10", "Operator B", "Point 1", "",
                                      "Point 2", "", "Point 3", "", "Point 4", "", "Point 5", "",
                                      "Point 6", "", "Point 7", "", "Point 8", "", "Point 9", "",
                                      "Point 10", "Operator C", "Point 1", "", "Point 2", "", "Point 3",
                                      "", "Point 4", "", "Point 5", "", "Point 6", "", "Point 7",
                                      "", "Point 8", "", "Point 9", "", "Point 10"))
})


### full model, autocorrelation plot & posterior on %study var (metalog fit)
options <- analysisOptions("msaBayesianGaugeRR")
options$measurementLongFormat <- "Dm"
options$operatorLongFormat <- "Operators"
options$partLongFormat <- "Parts"
options$estimationType <- "manual"
options$tolerance <- TRUE
options$posteriorPlot <- TRUE
options$posteriorPlotType <- "percStudyVar"
options$posteriorHistogram <- TRUE
options$posteriorCi <- TRUE
options$trafficLightChart <- TRUE
options$diagnosticsPlotType <- "autocor"
options$distType <- "metalog"
options$modelType <- "fullModel"
options$mcmcChains <- 2
options$customCiType <- "customCiQuantiles"
set.seed(1)
results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options)


test_that("L2 Model Comparison table results match", {
  table <- results[["results"]][["BFtable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9.1287767597893e+66, 0.0077421542931849, "Null model", 1, "",
                                      "Parts + Operators + Parts<unicode><unicode><unicode>Operators",
                                      0.113212575159566, 0.00872259468666002, "Parts + Operators"
                                 ))
})

test_that("L2 % Contribution to Total Variation table results match", {
  table <- results[["results"]][["contribTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.69447557946222, 3.05404347725902, "Total gauge r&amp;R", 9.81157819333386,
                                      0.365617611408123, 1.18254623665808, "Repeatability", 2.50534144416177,
                                      0.248512477297506, 1.87149724060094, "Reproducibility", 8.28077895940993,
                                      0.112863258114114, 1.48254203391395, "Operator", 7.79715004565745,
                                      90.1884218066661, 96.9459565227407, "Part-to-part", 99.3055244205378,
                                      "", 100, "Total variation", ""))
})

test_that("L2 % Study Variation & % Tolerance table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_percStudyVarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(8.33352010084559, 22.9786386424899, 16.3322195040502, 32.2484497939107,
                                      "Total gauge r&amp;R", 31.3234388066817, 59.6412966340563, 6.04663221290274,
                                      17.6288943903814, 10.5801416260002, 20.7059537295768, "Repeatability",
                                      15.8282704124082, 24.373940737509, 4.98510256793306, 12.3575654327892,
                                      12.0283968363823, 23.8525213529975, "Reproducibility", 28.7763423711902,
                                      55.6207429515081, 3.35951273364442, 7.8490424913573, 10.0901334559607,
                                      20.0717059566318, "Operator", 27.9233752657033, 54.2107521223829,
                                      94.9675848939788, 130.639342259913, 98.4410373260722, 203.511825478069,
                                      "Part-to-part", 99.6521572373075, 333.768084944735, "", 134.396600839345,
                                      100, 206.549553474733, "Total variation", "", 335.858700611061
                                 ))
})

test_that("L2 Standard Deviation & Study Variation table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_stdTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.382977310708165, 2.29786386424899, 0.537474163231843, 3.22484497939106,
                                      "Total gauge r&amp;R", 0.994021610567605, 5.96412966340563,
                                      0.293814906506357, 1.76288943903814, 0.34509922882628, 2.07059537295769,
                                      "Repeatability", 0.406232345625149, 2.4373940737509, 0.20595942387982,
                                      1.23575654327892, 0.397542022549958, 2.38525213529974, "Reproducibility",
                                      0.927012382525134, 5.56207429515081, 0.130817374855955, 0.78490424913573,
                                      0.334528432610531, 2.00717059566317, "Operator", 0.903512535373049,
                                      5.42107521223829, 2.17732237099855, 13.0639342259913, 3.39186375796782,
                                      20.3511825478067, "Part-to-part", 5.56280141574558, 33.3768084944735,
                                      2.23994334732242, 13.4396600839345, 3.44249255791222, 20.6549553474733,
                                      "Total variation", 5.59764501018435, 33.5858700611061))
})

test_that("L2 Autocor plot operators matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_g_Operators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 Autocor plot operators")
})

test_that("L2 Autocor plot parts matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_g_Parts"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 Autocor plot parts")
})

test_that("L2 Autocor plot inter matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_g_Parts:Operators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 Autocor plot inter")
})

test_that("L2 Autocor plot error matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_sig2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 Autocor plot error")
})

test_that("L2 Diagnostics table results match", {
  table <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(12967.850228195, 14329.7160415497, 0.062732257546333, 0.0341846247500968,
                                      0.539677077803615, "<unicode><sup>2</sup><sub>Part</sub>", 1.00002456233026,
                                      8500.80410601691, 9050.8556088105, 0.0148892656693548, 0.00016236059250508,
                                      0.0329594078357549, "<unicode><sup>2</sup><sub>Operator</sub>",
                                      1.00012670868512, 2552.53155004218, 4983.42629160057, 0.00043254562416457,
                                      0.000245120601889498, 0.00221431079824162, "<unicode><sup>2</sup><sub>Part<unicode><unicode><unicode>Operator</sub>",
                                      1.00024837506665, 6397.65222878932, 10084.8788553676, 0.000251800876700841,
                                      0.000254706253452919, 0.000694930652478803, "<unicode><sup>2</sup><sub>Error</sub>",
                                      1.00000831671343))
})

test_that("L2 Traffic light chart matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_trafficPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 Traffic light chart")
})

test_that("L2 Components of variation plot matches", {
  plotName <- results[["results"]][["varCompPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 components-of-variation")
})

test_that("L2 Variance Components table results match", {
  table <- results[["results"]][["varCompTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.146671620526603, 0.988078962511109, 0.34370391186105, 1.58695054948859,
                                      "Total gauge r&amp;R", 0.0863271993104535, 0.165024718682726,
                                      0.11991388657612, 0.020108398519423, "Repeatability", 0.0424192843572241,
                                      0.859351985170204, 0.223790025284931, 1.58641961102281, "Reproducibility",
                                      0.0171131855642062, 0.816334902198017, 0.184248502487699, 1.58585651722007,
                                      "Operator", 4.74073270725694, 30.944759618133, 12.2577502514977,
                                      7.07891510019515, "Part-to-part", 5.01734620004458, 31.3336296606438,
                                      12.6014541633589, 7.26094148376794, "Total variation"))
})

test_that("L2 Operator plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 operator")
})

test_that("L2 Part-to-part plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Part-to-part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 part-to-part")
})

test_that("L2 Repeatability plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Repeatability"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 repeatability")
})

test_that("L2 Reproducibility plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Reproducibility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 reproducibility")
})

test_that("L2 Total gauge r&R plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Total gauge r&R"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L2 total-gauge-r-r")
})

test_that("L2 Posterior Summary table results match", {
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(8.40483271767977, 33.0057533152063, "Total gauge r&amp;R", 16.4140241975042,
                                      6.04889943061452, 15.7471976500399, "Repeatability", 10.5766752981399,
                                      5.07341929971864, 29.5462725588561, "Reproducibility", 12.0468877459586,
                                      3.38522425736962, 28.4919128501656, "Operator", 10.0912145081404,
                                      94.6627001896635, 99.6460991284399, "Part-to-part", 98.4312844657675
                                 ))
})

### type 3, density diagnostics plot & posterior on %Tolerance (metalog fit)
options <- analysisOptions("msaBayesianGaugeRR")
options$measurementLongFormat <- "Dm"
options$partLongFormat <- "Parts"
options$type3 <- TRUE
options$tolerance <- TRUE
options$posteriorPlot <- TRUE
options$posteriorPlotType <- "percTol"
options$posteriorHistogram <- TRUE
options$posteriorCi <- TRUE
options$trafficLightChart <- TRUE
options$diagnosticsPlotType <- "density"
options$distType <- "metalog"
options$mcmcChains <- 2
options$customCiType <- "customCiQuantiles"
set.seed(1)
results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options)


test_that("L3 Model Comparison table results match", {
  table <- results[["results"]][["BFtable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.21824945358133e+67, 4.52862083972528e-07, "Null model", 1, "",
                                      "Parts"))
})

test_that("L3 % Contribution to Total Variation table results match", {
  table <- results[["results"]][["contribTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.486660998980681, 1.57812882489572, "Total gauge r&amp;R", 3.31935853093398,
                                      0.486660998980681, 1.57812882489572, "Repeatability", 3.31935853093398,
                                      96.680641469066, 98.4218711751048, "Part-to-part", 99.5133390010193,
                                      "", 100, "Total variation", ""))
})

test_that("L3 % Study Variation & % Tolerance table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_percStudyVarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(6.97610906534103, 20.3690235963062, 12.2240799868402, 23.7480532160329,
                                      "Total gauge r&amp;R", 18.2191068137751, 27.8369834027837, 6.97610906534103,
                                      20.3690235963062, 12.2240799868402, 23.7480532160329, "Repeatability",
                                      18.2191068137751, 27.8369834027837, 98.3263146207898, 131.405161770283,
                                      99.2070911544104, 203.640368012146, "Part-to-part", 99.7563727292204,
                                      334.591586444325, "", 133.578417870203, 100, 205.107533242308,
                                      "Total variation", "", 335.288690363822))
})

test_that("L3 Standard Deviation & Study Variation table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_stdTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.339483726605103, 2.03690235963062, 0.395800886933879, 2.37480532160328,
                                      "Total gauge r&amp;R", 0.463949723379728, 2.78369834027837,
                                      0.339483726605103, 2.03690235963062, 0.395800886933879, 2.37480532160328,
                                      "Repeatability", 0.463949723379728, 2.78369834027837, 2.19008602950472,
                                      13.1405161770283, 3.39400613353577, 20.3640368012146, "Part-to-part",
                                      5.57652644073874, 33.4591586444325, 2.22630696450339, 13.3578417870203,
                                      3.41845888737181, 20.5107533242309, "Total variation", 5.58814483939704,
                                      33.5288690363822))
})

test_that("L3 Density plot type3 parts matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_g_Parts"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L3 Density plot type3 parts")
})

test_that("L3 Density plot type 3 error matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_sig2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L3 Density plot type 3 error")
})

test_that("L3 Diagnostics table results match", {
  table <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(13013.8840798061, 13434.1168244218, 0.0641205840760188, 0.0456636982145211,
                                      0.4668331247511, "<unicode><sup>2</sup><sub>Part</sub>", 0.999911614893083,
                                      10264.3177964902, 13098.7327637058, 0.000253246417063196, 0.000424909254537975,
                                      0.000947491447932325, "<unicode><sup>2</sup><sub>Error</sub>",
                                      0.999947182865367))
})

test_that("L3 Traffic light chart matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_trafficPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L3 Traffic light chart")
})

test_that("L3 Components of variation plot matches", {
  plotName <- results[["results"]][["varCompPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L3 components-of-variation")
})

test_that("L3 Variance Components table results match", {
  table <- results[["results"]][["varCompTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.115249200662058, 0.215249345973517, 0.157658926333129, 0.0254995397510453,
                                      "Total gauge r&amp;R", 0.115249200662058, 0.215249345973517,
                                      0.157658926333129, 0.0254995397510453, "Repeatability", 4.79647682219662,
                                      31.0976471553276, 12.2669633998433, 7.0481079233022, "Part-to-part",
                                      4.95644270972351, 31.2273628685899, 12.4246223261765, 7.04797820646694,
                                      "Total variation"))
})

test_that("L3 Part-to-part plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Part-to-part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L3 part-to-part")
})

test_that("L3 Repeatability plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Repeatability"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L3 repeatability")
})

test_that("L3 Total gauge r&R plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Total gauge r&R"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L3 total-gauge-r-r")
})

test_that("L3 Total variation plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Total variation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L3 total-variation")
})

test_that("L3 Posterior Summary table results match", {
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(20.4255240578281, 27.7936833384094, "Total gauge r&amp;R", 23.7462166121699,
                                      20.4255240578281, 27.7936833384094, "Repeatability", 23.7462166121699,
                                      132.494221789943, 328.60136274761, "Part-to-part", 203.470639651509,
                                      134.688309385618, 329.570001781427, "Total variation", 204.959377455086
                                 ))
})


### historical SD & posterior on %Contribution (metalog)
options <- analysisOptions("msaBayesianGaugeRR")
options$measurementLongFormat <- "Dm"
options$operatorLongFormat <- "Operators"
options$partLongFormat <- "Parts"
options$processVariationReference <- "historicalSd"
options$historicalSdValue <- 1.5
options$tolerance <- TRUE
options$posteriorPlot <- TRUE
options$posteriorPlotType <- "percContrib"
options$posteriorHistogram <- TRUE
options$posteriorCi <- TRUE
options$trafficLightChart <- TRUE
options$diagnosticsPlotType <- "density"
options$distType <- "metalog"
options$mcmcChains <- 2
options$customCiType <- "customCiQuantiles"
set.seed(1)
results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options)


test_that("L4 Model Comparison table results match", {
  table <- results[["results"]][["BFtable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9.1287767597893e+66, 0.0077421542931849, "Null model", 1, "",
                                      "Parts + Operators + Parts<unicode><unicode><unicode>Operators",
                                      0.113212575159566, 0.00872259468666002, "Parts + Operators"
                                 ))
})

test_that("L4 % Contribution to Total Variation table results match", {
  table <- results[["results"]][["contribTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(6.05205051709262, 14.6005745997579, "Total gauge r&amp;R", 48.2034940888228,
                                      4.46768803116411, 6.12761674067714, "Repeatability", 8.37287900790609,
                                      0.862734196150831, 8.47295785908085, "Reproducibility", 41.9231191782192,
                                      0.862734196150831, 8.47295785908085, "Operator", 41.9231191782192,
                                      "", 85.3994254002103, "Part-to-part", "", "", 100, "Total variation",
                                      ""))
})

test_that("L4 % Study Variation & % Tolerance table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_percStudyVarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(24.6009156638892, 22.1408240975003, 35.1649677684775, 31.6484709916296,
                                      "Total gauge r&amp;R", 69.4287353578425, 62.4858618220583, 21.1369061850116,
                                      19.0232155665105, 24.6742405520885, 22.2068164968796, "Repeatability",
                                      28.9359274984966, 26.042334748647, 9.28834856146862, 8.35951370532176,
                                      23.3943396999233, 21.0549057299311, "Reproducibility", 64.7480649718461,
                                      58.2732584746615, 9.28834856146862, 8.35951370532176, 23.3943396999233,
                                      21.0549057299311, "Operator", 64.7480649718461, 58.2732584746615,
                                      "", "", 92.4118095268483, 83.1706285741424, "Part-to-part",
                                      "", "", "", "", 100, 90, "Total variation", "", ""))
})

test_that("L4 Standard Deviation & Study Variation table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_stdTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.369013734958338, 2.21408240975003, 0.527474516527165, 3.16484709916296,
                                      "Total gauge r&amp;R", 1.04143103036764, 6.24858618220583, 0.317053592775174,
                                      1.90232155665105, 0.370113608281325, 2.22068164968796, "Repeatability",
                                      0.434038912477449, 2.6042334748647, 0.139325228422029, 0.835951370532175,
                                      0.350915095498847, 2.10549057299309, "Reproducibility", 0.971220974577692,
                                      5.82732584746615, 0.139325228422029, 0.835951370532175, 0.350915095498847,
                                      2.10549057299309, "Operator", 0.971220974577692, 5.82732584746615,
                                      "", "", 1.38617714290269, 8.31706285741529, "Part-to-part",
                                      "", "", "", "", 1.5, 9, "Total variation", "", ""))
})

test_that("L4 Density plot histSd operators matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_g_Operators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 Density plot histSd operators")
})

test_that("L4 Density plot histSd parts matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_g_Parts"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 Density plot histSd parts")
})

test_that("L4 Density plot histSd error matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_sig2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 Density plot histSd error")
})

test_that("L4 Diagnostics table results match", {
  table <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(13156.2958719656, 14548.1788817734, 0.0643396308484891, 0.026604748502423,
                                      0.416661068666242, "<unicode><sup>2</sup><sub>Part</sub>", 0.999980107827912,
                                      8826.27376375007, 8779.64520704258, 0.00739624941904107, 0.000226595508114019,
                                      0.0456256051161729, "<unicode><sup>2</sup><sub>Operator</sub>",
                                      0.999989789442352, 9519.12272634791, 13138.4226911852, 0.000228956655794118,
                                      0.000320520616274043, 0.000765639483270394, "<unicode><sup>2</sup><sub>Error</sub>",
                                      1.00096014378277))
})

test_that("L4 Traffic light chart matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_trafficPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 Traffic light chart")
})

test_that("L4 Components of variation plot matches", {
  plotName <- results[["results"]][["varCompPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 components-of-variation")
})

test_that("L4 Variance Components table results match", {
  table <- results[["results"]][["varCompTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.136171136634584, 1.08457861699851, 0.328512928494555, 0.64962130987885,
                                      "Total gauge r&amp;R", 0.100522980701193, 0.188389777677887,
                                      0.137871376665235, 0.0224366411670959, "Repeatability", 0.0194115194133937,
                                      0.943270181509932, 0.190641551829319, 0.648481519656201, "Reproducibility",
                                      0.0194115194133937, 0.943270181509932, 0.190641551829319, 0.648481519656201,
                                      "Operator", "", "", 1.92148707150488, "", "Part-to-part", "",
                                      "", 2.25, "", "Total variation"))
})

test_that("L4 Operator plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 operator")
})

test_that("L4 Repeatability plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Repeatability"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 repeatability")
})

test_that("L4 Reproducibility plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Reproducibility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 reproducibility")
})

test_that("L4 Total gauge r&R plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Total gauge r&R"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L4 total-gauge-r-r")
})

test_that("L4 Posterior Summary table results match", {
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(6.08426523238411, 49.8077618247364, "Total gauge r&amp;R", 15.0301360534277,
                                      4.50423780799053, 8.31507421356034, "Repeatability", 6.1266418829029,
                                      0.887272856100956, 41.8955809104603, "Reproducibility", 9.4272887180478,
                                      0.887272856100956, 41.8955809104603, "Operator", 9.4272887180478
                                 ))
})


### report
test_that("L Gauge r&R report plot matches", {
  options <- analysisOptions("msaBayesianGaugeRR")
  options$measurementLongFormat <- "Dm"
  options$operatorLongFormat <- "Operators"
  options$partLongFormat <- "Parts"
  options$tolerance <- TRUE
  options$posteriorPlotType <- "var"
  options$report <- TRUE
  options$reportRChartByOperator <- TRUE
  options$reportMeasurementsByOperatorPlot <- TRUE
  options$reportAverageChartByOperator <- TRUE
  options$reportPartByOperatorPlot <- TRUE
  options$mcmcChains <- 2
  options$customCiType <- "customCiQuantiles"
  set.seed(1)
  results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_long.csv", options)
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "L gauge-r-r-report")
})



## tests wide format
#local_snapshotter(snap_dir = "_snaps/msaBayesianGaugeRR/wide format/")
### automatic model selection & posterior on variances (generalized inverse Gaussian)
options <- analysisOptions("msaBayesianGaugeRR")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("Measurement1", "Measurement2", "Measurement3")
options$operatorWideFormat <- "Operator"
options$partWideFormat <- "Part"
options$tolerance <- TRUE
options$priorPlot <- TRUE
options$posteriorPlot <- TRUE
options$posteriorPlotType <- "var"
options$posteriorHistogram <- TRUE
options$posteriorCi <- TRUE
options$contourPlot <- TRUE
options$contourLSL <- 5
options$contourUSL <- 12
options$rChart <- TRUE
options$xBarChart <- TRUE
options$scatterPlot <- TRUE
options$scatterPlotFitLine <- TRUE
options$scatterPlotOriginLine <- TRUE
options$partMeasurementPlot <- TRUE
options$partMeasurementPlotAllValues <- TRUE
options$operatorMeasurementPlot <- TRUE
options$partByOperatorMeasurementPlot <- TRUE
options$trafficLightChart <- TRUE
options$distType <- "gig"
options$mcmcChains <- 2
options$customCiType <- "customCiQuantiles"
set.seed(1)
results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_wide.csv", options)


test_that("W1 Model Comparison table results match", {
  table <- results[["results"]][["BFtable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(92702680603773728, 0.00765529237919074, "Null model", 1, "", "Part + Operator + Part<unicode><unicode><unicode>Operator",
                                      0.0121495135306725, 0.00886204496168727, "Part + Operator"
                                 ))
})

test_that("W1 Contour plot wide matches", {
  plotName <- results[["results"]][["contourPlot"]][["collection"]][["contourPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 Contour plot wide")
})

test_that("W1 Producer's (δ) and Consumer's (β) Risk table results match", {
  table <- results[["results"]][["contourPlot"]][["collection"]][["contourPlot_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0264927749011355, 0.0787475031694541, "<unicode>", 0.213793581986107,
                                      0.190669690866134, 0.310594648025299, "<unicode>", 0.411549984734059
                                 ))
})

test_that("W1 % Contribution to Total Variation table results match", {
  table <- results[["results"]][["contribTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(12.3269052367313, 33.9284976466776, "Total gauge r&amp;R", 69.3587838257263,
                                      7.09756342908547, 19.8025194624623, "Repeatability", 35.5218633925746,
                                      1.91788890553919, 14.1259781842151, "Reproducibility", 56.2082001015145,
                                      1.91788890553919, 14.1259781842151, "Operator", 56.2082001015145,
                                      30.6412161742737, 66.0715023533227, "Part-to-part", 87.6730947632687,
                                      "", 100, "Total variation", ""))
})

test_that("W1 Part by operator interaction plot matches", {
  plotName <- results[["results"]][["gaugeByInteraction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 part-by-operator-interaction")
})

test_that("W1 Measurements by operator plot matches", {
  plotName <- results[["results"]][["gaugeByOperator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 measurements-by-operator")
})

test_that("W1 Measurements by part plot matches", {
  plotName <- results[["results"]][["gaugeByPart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 measurements-by-part")
})

test_that("W1 % Study Variation & % Tolerance table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_percStudyVarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(35.1096922476569, 54.0297071548185, 57.0017393460991, 74.7495086161511,
                                      "Total gauge r&amp;R", 83.2819210724576, 140.230313974081, 26.6412521701421,
                                      47.452522327498, 43.6853564019964, 55.425842187894, "Repeatability",
                                      59.6002209628415, 64.9073776359718, 13.8487865812482, 18.6433631335287,
                                      34.2767972476711, 46.5139370842548, "Reproducibility", 74.9721282190718,
                                      128.70188362376, 13.8487865812482, 18.6433631335287, 34.2767972476711,
                                      46.5139370842548, "Operator", 74.9721282190718, 128.70188362376,
                                      55.3545084521765, 67.2003644717557, 80.6965158375164, 106.466816727854,
                                      "Part-to-part", 93.6339119740239, 171.623834317457, "", 93.5117075742855,
                                      100, 132.14110254032, "Total variation", "", 203.977912189941
                                 ))
})

test_that("W1 Standard Deviation & Study Variation table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_stdTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.900495119246975, 5.40297071548185, 1.24582514360251, 7.47495086161507,
                                      "Total gauge r&amp;R", 2.33717189956802, 14.0230313974081, 0.790875372124967,
                                      4.7452522327498, 0.923764036464903, 5.5425842187894, "Repeatability",
                                      1.0817896272662, 6.49073776359718, 0.310722718892144, 1.86433631335287,
                                      0.775232284737573, 4.65139370842548, "Reproducibility", 2.14503139372934,
                                      12.870188362376, 0.310722718892144, 1.86433631335287, 0.775232284737573,
                                      4.65139370842548, "Operator", 2.14503139372934, 12.870188362376,
                                      1.12000607452926, 6.72003644717558, 1.77444694546422, 10.6466816727853,
                                      "Part-to-part", 2.86039723862429, 17.1623834317457, 1.55852845957142,
                                      9.35117075742855, 2.20235170900533, 13.214110254032, "Total variation",
                                      3.39963186983235, 20.3977912189941))
})

test_that("W1 Matrix plot for operators matches", {
  plotName <- results[["results"]][["gaugeScatterOperators"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 matrix-plot-for-operators")
})

test_that("W1 Trace plot operators wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_g_Operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 Trace plot operators wide")
})

test_that("W1 Trace plot parts wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_g_Part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 Trace plot parts wide")
})

test_that("W1 Trace plot error wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_sig2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 Trace plot error wide")
})

test_that("W1 Diagnostics table wide results match", {
  table <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(11489.1632603585, 13677.1607176617, 0.0188803933569998, 0.0110455472900119,
                                      0.159912102522656, "<unicode><sup>2</sup><sub>Part</sub>", 1.00004644044169,
                                      8946.79354696483, 9284.24667889662, 0.0525552167179807, 0.00142100662694245,
                                      0.197346171272066, "<unicode><sup>2</sup><sub>Operator</sub>",
                                      0.999921535112366, 9627.27034873237, 12550.7045825935, 0.00141570326076179,
                                      0.00158112334408733, 0.00365958909245312, "<unicode><sup>2</sup><sub>Error</sub>",
                                      1.00097355083603))
})

test_that("W1 g-prior plot matches", {
  plotName <- results[["results"]][["priorPlot"]][["collection"]][["priorPlot_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 g-prior")
})

test_that("W1 rChart wide matches", {
  plotName <- results[["results"]][["rChart"]][["collection"]][["rChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 rChart wide")
})

test_that("W1 Test results for range chart table results match", {
  table <- results[["results"]][["rChart"]][["collection"]][["rChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("C", "Point 10"))
})

test_that("W1 Traffic light chart wide matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_trafficPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 Traffic light chart wide")
})

test_that("W1 Components of variation plot matches", {
  plotName <- results[["results"]][["varCompPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 components-of-variation")
})

test_that("W1 Variance Components table results match", {
  table <- results[["results"]][["varCompTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.810891459819477, 5.46237261253729, 1.78655246954848, 4.25598172544705,
                                      "Total gauge r&amp;R", 0.625483854562397, 1.17026879778004,
                                      0.858831277132578, 0.139329767865209, "Repeatability", 0.0965486080600336,
                                      4.6011597614014, 0.927721192415901, 4.24873211986627, "Reproducibility",
                                      0.0965486080600336, 4.6011597614014, 0.927721192415901, 4.24873211986627,
                                      "Operator", 1.25441360896374, 8.18187264910991, 3.36535744263274,
                                      2.05016685602997, "Part-to-part", 2.42901096405622, 11.5574968504741,
                                      5.15190991218123, 4.71475336587418, "Total variation"))
})

test_that("W1 Error plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Error"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 error")
})

test_that("W1 Operator plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 operator")
})

test_that("W1 Part plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 part")
})

test_that("W1 Posterior Summary table results match", {
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.27284283054443, 8.51804137015566, "<unicode><sup>2</sup><sub>Part</sub>",
                                      3.3737108373, 0.098113661553187, 4.61971702790384, "<unicode><sup>2</sup><sub>Operator</sub>",
                                      0.927748105326958, 0.623677008626749, 1.16560947901581, "<unicode><sup>2</sup><sub>Error</sub>",
                                      0.858916283560981))
})

test_that("W1 xBar chart wide matches", {
  plotName <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W1 xBar chart wide")
})

test_that("W1 Test results for x-bar chart table results match", {
  table <- results[["results"]][["xBarChart"]][["collection"]][["xBarChart_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("A", "Point 1", "", "Point 5", "", "Point 8", "", "Point 10",
                                      "B", "Point 1", "", "Point 5", "", "Point 8", "", "Point 10",
                                      "C", "Point 1", "", "Point 3", "", "Point 5", "", "Point 10"
                                 ))
})

### full model, autocorrelation plot & posterior on %study var (metalog fit)
options <- analysisOptions("msaBayesianGaugeRR")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("Measurement1", "Measurement2", "Measurement3")
options$operatorWideFormat <- "Operator"
options$partWideFormat <- "Part"
options$estimationType <- "manual"
options$tolerance <- TRUE
options$posteriorPlot <- TRUE
options$posteriorPlotType <- "percStudyVar"
options$posteriorHistogram <- TRUE
options$posteriorCi <- TRUE
options$trafficLightChart <- TRUE
options$diagnosticsPlotType <- "autocor"
options$distType <- "metalog"
options$modelType <- "fullModel"
options$mcmcChains <- 2
options$customCiType <- "customCiQuantiles"
set.seed(1)
results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_wide.csv", options)


test_that("W2 Model Comparison table results match", {
  table <- results[["results"]][["BFtable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(92702680603773728, 0.00765529237919074, "Null model", 1, "", "Part + Operator + Part<unicode><unicode><unicode>Operator",
                                      0.0121495135306725, 0.00886204496168727, "Part + Operator"
                                 ))
})

test_that("W2 % Contribution to Total Variation table results match", {
  table <- results[["results"]][["contribTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(13.3778291589204, 36.1913295547785, "Total gauge r&amp;R", 69.466346367937,
                                      6.54447642381507, 18.3197432349895, "Repeatability", 32.73935334103,
                                      4.02429567876643, 17.871586319789, "Reproducibility", 56.444856724105,
                                      1.82503213233058, 13.6051251659484, "Operator", 53.6866163018582,
                                      30.533653632063, 63.8086704452217, "Part-to-part", 86.6221708410796,
                                      "", 100, "Total variation", ""))
})

test_that("W2 % Study Variation & % Tolerance table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_percStudyVarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(36.5757146253627, 56.7656934104138, 58.9923979409587, 77.8390144712959,
                                      "Total gauge r&amp;R", 83.3464734120505, 138.841013897683, 25.582174090092,
                                      45.9144985156591, 42.0347378543699, 53.6837141943982, "Repeatability",
                                      57.218312728733, 62.9766489691705, 20.0606472250702, 27.8931683934347,
                                      39.9676499900152, 53.9870114150065, "Reproducibility", 75.1297913909348,
                                      126.892261568645, 13.5093748143594, 18.1862826792913, 33.6092783763241,
                                      46.0434055352073, "Operator", 73.2711510905113, 124.613898462338,
                                      55.2572651173971, 65.549306545672, 79.2720451566425, 105.353853521724,
                                      "Part-to-part", 93.0710324521567, 175.700681171956, "", 94.5404823595808,
                                      100, 133.080812955375, "Total variation", "", 205.322828943223
                                 ))
})

test_that("W2 Standard Deviation & Study Variation table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_stdTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.946094890173563, 5.67656934104138, 1.29731690785493, 7.78390144712956,
                                      "Total gauge r&amp;R", 2.31401689829472, 13.8841013897683, 0.765241641927652,
                                      4.59144985156591, 0.89472856990664, 5.36837141943982, "Repeatability",
                                      1.04961081615284, 6.29766489691705, 0.464886139890577, 2.78931683934346,
                                      0.899783523583441, 5.39870114150067, "Reproducibility", 2.11487102614409,
                                      12.6892261568645, 0.303104711321522, 1.81862826792913, 0.767390092253462,
                                      4.60434055352075, "Operator", 2.07689830770563, 12.4613898462338,
                                      1.09248844242787, 6.5549306545672, 1.7558975586954, 10.5353853521724,
                                      "Part-to-part", 2.92834468619927, 17.5700681171956, 1.57567470599301,
                                      9.45404823595808, 2.21801354925625, 13.3080812955376, "Total variation",
                                      3.42204714905372, 20.5322828943223))
})

test_that("W2 Autocor plot operators wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_g_Operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 Autocor plot operators wide")
})

test_that("W2 Autocor plot parts wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_g_Part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 Autocor plot parts wide")
})

test_that("W2 Autocor plot inter wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_g_Part:Operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 Autocor plot inter wide")
})

test_that("W2 Autocor plot error wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_sig2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 Autocor plot error wide")
})

test_that("W2 Diagnostics table wide results match", {
  table <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(10713.5473264095, 13407.275608121, 0.0190269587098109, 0.0103036342722492,
                                      0.134655246165873, "<unicode><sup>2</sup><sub>Part</sub>", 1.00019626284334,
                                      8693.66878281009, 9627.98196118723, 0.117006744102641, 0.00119814090722604,
                                      0.172676716849297, "<unicode><sup>2</sup><sub>Operator</sub>",
                                      1.00027692390321, 2785.13714277183, 4772.44199873121, 0.00196487944719096,
                                      0.000824576247414721, 0.00886968674039701, "<unicode><sup>2</sup><sub>Part<unicode><unicode><unicode>Operator</sub>",
                                      1.00068074432022, 6957.81110299329, 10358.8045674784, 0.00158875487364865,
                                      0.00191146086502458, 0.00424397364874796, "<unicode><sup>2</sup><sub>Error</sub>",
                                      1.00005531082368))
})

test_that("W2 Traffic light chart wide matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_trafficPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 Traffic light chart wide")
})

test_that("W2 Components of variation plot matches", {
  plotName <- results[["results"]][["varCompPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 components-of-variation")
})

test_that("W2 Variance Components table results match", {
  table <- results[["results"]][["varCompTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.895095541218567, 5.35467421909935, 2.03563916421946, 13.36030351333,
                                      "Total gauge r&amp;R", 0.585594770562784, 1.1016828653987, 0.805784153180511,
                                      0.131825823292889, "Repeatability", 0.216119123075015, 4.47267980982418,
                                      1.22985501103896, 13.3556841019572, "Reproducibility", 0.0918724662796369,
                                      4.31350665542013, 1.04095448066503, 13.3531032948241, "Operator",
                                      1.1935309978082, 8.57520261358881, 3.30518987594095, 2.00879397541868,
                                      "Part-to-part", 2.48275078237167, 11.7104066907196, 5.34082904016041,
                                      13.5126413089862, "Total variation"))
})

test_that("W2 Operator plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 operator")
})

test_that("W2 Part-to-part plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Part-to-part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 part-to-part")
})

test_that("W2 Repeatability plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Repeatability"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 repeatability")
})

test_that("W2 Reproducibility plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Reproducibility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 reproducibility")
})

test_that("W2 Total gauge r&R plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Total gauge r&R"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W2 total-gauge-r-r")
})

test_that("W2 Posterior Summary table results match", {
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(36.6908448164942, 84.640380749268, "Total gauge r&amp;R", 59.0499639096235,
                                      25.5255275390376, 56.9200489576364, "Repeatability", 42.03575729992,
                                      20.0919962355863, 75.9155319105894, "Reproducibility", 39.996598338415,
                                      13.7729319889636, 73.7855331557218, "Operator", 33.6414451776228,
                                      53.9844930078528, 93.0486168370966, "Part-to-part", 79.2144723166527
                                 ))
})

### type 3, density diagnostics plot & posterior on %Tolerance (metalog fit)
options <- analysisOptions("msaBayesianGaugeRR")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("Measurement1", "Measurement2", "Measurement3")
options$partWideFormat <- "Part"
options$type3 <- TRUE
options$tolerance <- TRUE
options$posteriorPlot <- TRUE
options$posteriorPlotType <- "percTol"
options$posteriorHistogram <- TRUE
options$posteriorCi <- TRUE
options$trafficLightChart <- TRUE
options$diagnosticsPlotType <- "density"
options$distType <- "metalog"
options$mcmcChains <- 2
options$customCiType <- "customCiQuantiles"
set.seed(1)
results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_wide.csv", options)


test_that("W3 Model Comparison table results match", {
  table <- results[["results"]][["BFtable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(98096672972404490240, 3.49979418330506e-06, "Null model", 1, "",
                                      "Part"))
})

test_that("W3 % Contribution to Total Variation table results match", {
  table <- results[["results"]][["contribTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(8.83081567952286, 23.7598919702468, "Total gauge r&amp;R", 41.8274496817367,
                                      8.83081567952286, 23.7598919702468, "Repeatability", 41.8274496817367,
                                      58.1725503182633, 76.2401080297539, "Part-to-part", 91.1691843204771,
                                      "", 100, "Total variation", ""))
})

test_that("W3 % Study Variation & % Tolerance table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_percStudyVarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(29.7166882611562, 48.0241014671239, 47.9003801961148, 55.9991052269221,
                                      "Total gauge r&amp;R", 64.6741445105543, 65.6920462384278, 29.7166882611562,
                                      48.0241014671239, 47.9003801961148, 55.9991052269221, "Repeatability",
                                      64.6741445105543, 65.6920462384278, 76.2709317618789, 67.5499445080692,
                                      87.1711025379854, 106.583932335271, "Part-to-part", 95.4825556392114,
                                      175.730879489471, "", 87.0869358486883, 100, 121.077346756447,
                                      "Total variation", "", 184.656155967787))
})

test_that("W3 Standard Deviation & Study Variation table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_stdTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.800401691118732, 4.80241014671239, 0.933318420448701, 5.5999105226922,
                                      "Total gauge r&amp;R", 1.09486743730713, 6.56920462384278, 0.800401691118732,
                                      4.80241014671239, 0.933318420448701, 5.5999105226922, "Repeatability",
                                      1.09486743730713, 6.56920462384278, 1.12583240846782, 6.75499445080692,
                                      1.77639887225451, 10.6583932335271, "Part-to-part", 2.92884799149119,
                                      17.5730879489471, 1.45144893081147, 8.70869358486883, 2.01795577927412,
                                      12.1077346756448, "Total variation", 3.07760259946311, 18.4656155967787
                                 ))
})

test_that("W3 Density plot type3 parts wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_g_Part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W3 Density plot type3 parts wide")
})

test_that("W3 Density plot type 3 error wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_sig2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W3 Density plot type 3 error wide")
})

test_that("W3 Diagnostics table wide results match", {
  table <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(11707.3772753072, 12898.8791981331, 0.018538497634247, 0.0117758505186677,
                                      0.133935146076606, "<unicode><sup>2</sup><sub>Part</sub>", 0.999912218555296,
                                      10252.2089186134, 12928.747430743, 0.00140504193471408, 0.00203664967823852,
                                      0.00556308506257785, "<unicode><sup>2</sup><sub>Error</sub>",
                                      0.999971878611095))
})

test_that("W3 Traffic light chart wide matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_trafficPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W3 Traffic light chart wide")
})

test_that("W3 Components of variation plot matches", {
  plotName <- results[["results"]][["varCompPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W3 components-of-variation")
})

test_that("W3 Variance Components table results match", {
  table <- results[["results"]][["varCompTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.640642867591348, 1.19873470534066, 0.876622085936743, 0.141450061477036,
                                      "Total gauge r&amp;R", 0.640642867591348, 1.19873470534066,
                                      0.876622085936743, 0.141450061477036, "Repeatability", 1.26749861196421,
                                      8.57815073257154, 3.3707105788214, 1.97733245942888, "Part-to-part",
                                      2.106703999134, 9.47163777543298, 4.24733266475813, 1.98142724023979,
                                      "Total variation"))
})

test_that("W3 Part-to-part plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Part-to-part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W3 part-to-part")
})

test_that("W3 Repeatability plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Repeatability"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W3 repeatability")
})

test_that("W3 Total gauge r&R plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Total gauge r&R"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W3 total-gauge-r-r")
})

test_that("W3 Total variation plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Total variation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W3 total-variation")
})

test_that("W3 Posterior Summary table results match", {
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(48.1714882535499, 65.4777848056588, "Total gauge r&amp;R", 55.9859173475586,
                                      48.1714882535499, 65.4777848056588, "Repeatability", 55.9859173475586,
                                      68.0697507873148, 173.397787709273, "Part-to-part", 106.53503836118,
                                      87.6024563014082, 182.690390900527, "Total variation", 121.01621390635
                                 ))
})

### historical SD & posterior on %Contribution (metalog)
options <- analysisOptions("msaBayesianGaugeRR")
options$dataFormat <- "wideFormat"
options$measurementsWideFormat <- list("Measurement1", "Measurement2", "Measurement3")
options$operatorWideFormat <- "Operator"
options$partWideFormat <- "Part"
options$processVariationReference <- "historicalSd"
options$historicalSdValue <- 1.5
options$tolerance <- TRUE
options$posteriorPlot <- TRUE
options$posteriorPlotType <- "percContrib"
options$posteriorHistogram <- TRUE
options$posteriorCi <- TRUE
options$trafficLightChart <- TRUE
options$diagnosticsPlotType <- "density"
options$distType <- "metalog"
options$mcmcChains <- 2
options$customCiType <- "customCiQuantiles"
set.seed(1)
results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_wide.csv", options)


test_that("W4 Model Comparison table results match", {
  table <- results[["results"]][["BFtable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(92702680603773728, 0.00765529237919074, "Null model", 1, "", "Part + Operator + Part<unicode><unicode><unicode>Operator",
                                      0.0121495135306725, 0.00886204496168727, "Part + Operator"
                                 ))
})

test_that("W4 % Contribution to Total Variation table results match", {
  table <- results[["results"]][["contribTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(36.0396204364212, 79.4023319799323, "Total gauge r&amp;R", 242.772116112769,
                                      27.7992824249954, 38.1702789836703, "Repeatability", 52.0119465680019,
                                      4.29104924711261, 41.2320529962621, "Reproducibility", 204.495989395618,
                                      4.29104924711261, 41.2320529962621, "Operator", 204.495989395618,
                                      "", 20.5976680200604, "Part-to-part", "", "", 100, "Total variation",
                                      ""))
})

test_that("W4 % Study Variation & % Tolerance table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_percStudyVarTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(60.0330079497983, 54.0297071548185, 83.0550095735007, 74.7495086161511,
                                      "Total gauge r&amp;R", 155.811459971201, 140.230313974081, 52.7250248083311,
                                      47.452522327498, 61.5842690976603, 55.425842187894, "Repeatability",
                                      72.1193084844131, 64.9073776359718, 20.714847926143, 18.6433631335287,
                                      51.6821523158386, 46.5139370842548, "Reproducibility", 143.002092915289,
                                      128.70188362376, 20.714847926143, 18.6433631335287, 51.6821523158386,
                                      46.5139370842548, "Operator", 143.002092915289, 128.70188362376,
                                      "", "", 45.3846538161075, 40.846188434481, "Part-to-part", "",
                                      "", "", "", 100, 90, "Total variation", "", ""))
})

test_that("W4 Standard Deviation & Study Variation table results match", {
  table <- results[["results"]][["gaugeEvaluation"]][["collection"]][["gaugeEvaluation_stdTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.900495119246975, 5.40297071548185, 1.24582514360251, 7.47495086161507,
                                      "Total gauge r&amp;R", 2.33717189956802, 14.0230313974081, 0.790875372124967,
                                      4.7452522327498, 0.923764036464903, 5.5425842187894, "Repeatability",
                                      1.0817896272662, 6.49073776359718, 0.310722718892144, 1.86433631335287,
                                      0.775232284737573, 4.65139370842548, "Reproducibility", 2.14503139372934,
                                      12.870188362376, 0.310722718892144, 1.86433631335287, 0.775232284737573,
                                      4.65139370842548, "Operator", 2.14503139372934, 12.870188362376,
                                      "", "", 0.680769807241364, 4.08461884344987, "Part-to-part",
                                      "", "", "", "", 1.5, 9, "Total variation", "", ""))
})

test_that("W4 Density plot histSd operators wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_g_Operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 Density plot histSd operators wide")
})

test_that("W4 Density plot histSd parts wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_g_Part"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 Density plot histSd parts wide")
})

test_that("W4 Density plot histSd error wide matches", {
  plotName <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_sig2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 Density plot histSd error wide")
})

test_that("W4 Diagnostics table wide results match", {
  table <- results[["results"]][["mcmcDiagnostics"]][["collection"]][["mcmcDiagnostics_table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(11489.1632603585, 13677.1607176617, 0.0188803933569998, 0.0110455472900119,
                                      0.159912102522656, "<unicode><sup>2</sup><sub>Part</sub>", 1.00004644044169,
                                      8946.79354696483, 9284.24667889662, 0.0525552167179807, 0.00142100662694245,
                                      0.197346171272066, "<unicode><sup>2</sup><sub>Operator</sub>",
                                      0.999921535112366, 9627.27034873237, 12550.7045825935, 0.00141570326076179,
                                      0.00158112334408733, 0.00365958909245312, "<unicode><sup>2</sup><sub>Error</sub>",
                                      1.00097355083603))
})

test_that("W4 Traffic light chart wide matches", {
  plotName <- results[["results"]][["trafficPlot"]][["collection"]][["trafficPlot_trafficPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 Traffic light chart wide")
})

test_that("W4 Components of variation plot matches", {
  plotName <- results[["results"]][["varCompPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 components-of-variation")
})

test_that("W4 Variance Components table results match", {
  table <- results[["results"]][["varCompTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.810891459819477, 5.46237261253729, 1.78655246954848, 4.25598172544705,
                                      "Total gauge r&amp;R", 0.625483854562397, 1.17026879778004,
                                      0.858831277132578, 0.139329767865209, "Repeatability", 0.0965486080600336,
                                      4.6011597614014, 0.927721192415901, 4.24873211986627, "Reproducibility",
                                      0.0965486080600336, 4.6011597614014, 0.927721192415901, 4.24873211986627,
                                      "Operator", "", "", 0.463447530451708, "", "Part-to-part", "",
                                      "", 2.25, "", "Total variation"))
})

test_that("W4 Operator plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Operator"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 operator")
})

test_that("W4 Repeatability plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Repeatability"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 repeatability")
})

test_that("W4 Reproducibility plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Reproducibility"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 reproducibility")
})

test_that("W4 Total gauge r&R plot matches", {
  plotName <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_Total gauge r&R"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W4 total-gauge-r-r")
})

test_that("W4 Posterior Summary table results match", {
  table <- results[["results"]][["posteriorSummaries"]][["collection"]][["posteriorSummaries_postSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(36.2684396989805, 246.249732595721, "Total gauge r&amp;R", 79.4194770714244,
                                      28.0791556215122, 51.6892435406356, "Repeatability", 38.1493514877285,
                                      4.43965092830744, 196.978472951731, "Reproducibility", 42.7178342722391,
                                      4.43965092830744, 196.978472951731, "Operator", 42.7178342722391
                                 ))
})

### report
test_that("W Gauge r&R report plot matches", {
  options <- analysisOptions("msaBayesianGaugeRR")
  options$dataFormat <- "wideFormat"
  options$measurementsWideFormat <- list("Measurement1", "Measurement2", "Measurement3")
  options$operatorWideFormat <- "Operator"
  options$partWideFormat <- "Part"
  options$tolerance <- TRUE
  options$posteriorPlotType <- "var"
  options$report <- TRUE
  options$reportRChartByOperator <- TRUE
  options$reportMeasurementsByOperatorPlot <- TRUE
  options$reportAverageChartByOperator <- TRUE
  options$reportPartByOperatorPlot <- TRUE
  options$mcmcChains <- 2
  options$customCiType <- "customCiQuantiles"
  set.seed(1)
  results <- runAnalysis("msaBayesianGaugeRR", "datasets/msaGaugeRRCrossed/msaGaugeRRCrossed_wide.csv", options)
  plotName <- results[["results"]][["report"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "W gauge-r-r-report")
})
