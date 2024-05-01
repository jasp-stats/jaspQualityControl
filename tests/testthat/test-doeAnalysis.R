context("DoE Analysis")
.numDecimals <- 2

# Basic tests

## Factorial designs

### Two factors full factorial (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$fixedFactorsFactorial <- c("A", "B")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "A"),
  list(components = "B"),
  list(components = c("A", "B"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/2level2facFull.csv", options)

test_that("1.1 Two factors full factorial ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(53.6626325023352, 160.987897507006, 3, "", "", "Model", "", 148.691283261526,
                                      2, "", "", "<unicode> Linear terms", 62.0097134571341, 62.0097134571341,
                                      1, "", "", "<unicode> <unicode> A", 86.6815698043919, 86.6815698043919,
                                      1, "", "", "<unicode> <unicode> B", "", 12.2966142454797, 1,
                                      "", "", "<unicode> Interaction terms", 12.2966142454797, 12.2966142454797,
                                      1, "", "", "<unicode> <unicode> A<unicode><unicode><unicode>B",
                                      0, 0, 0, "", "", "Error", "", 160.987897507006, 3, "", "", "Total"
                                 ))
})

test_that("1.2 Two factors full factorial Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 99.3133959225, "", "", "", "(Intercept)", "", "",
                                      "A", 3.93731232750001, 7.87462465500001, "", "", "A", "", "NaN",
                                      "B", 4.6551468775, 9.31029375499999, "", "", "B", "", "NaN",
                                      "AB", -1.7533264275, -3.50665285500001, "", "", "A<unicode>B",
                                      "", "NaN"))
})

test_that("1.3 Two factors full factorial Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 99.31 (Intercept) + 3.94 A + 4.66 B - 1.75 AB"))
})

test_that("1.4 Two factors full factorial Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", "", 1, ""))
})

### Five factors full factorial (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$fixedFactorsFactorial <- c("A", "B", "C", "D", "E")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "A"),
  list(components = "B"),
  list(components = "C"),
  list(components = "D"),
  list(components = "E"),
  list(components = c("A", "B")),
  list(components = c("A", "C")),
  list(components = c("A", "D")),
  list(components = c("A", "E")),
  list(components = c("B", "C")),
  list(components = c("B", "D")),
  list(components = c("B", "E")),
  list(components = c("C", "D")),
  list(components = c("C", "E")),
  list(components = c("D", "E"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/2level5facFull.csv", options)


test_that("2.1 Five factors full factorial ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(118.542221893211, 1778.13332839816, 15, 2.63173059967985, 0.0318832484215919,
                                      "Model", "", 906.296316752094, 5, "", "", "<unicode> Linear terms",
                                      162.13896103187, 162.13896103187, 1, 3.59961251217536, 0.0759912026029757,
                                      "<unicode> <unicode> A", 243.097271810399, 243.097271810399,
                                      1, 5.39695071261994, 0.0336761387549081, "<unicode> <unicode> B",
                                      0.437571792073413, 0.437571792073413, 1, 0.00971443808260777,
                                      0.922710410988336, "<unicode> <unicode> C", 64.4409045780405,
                                      64.4409045780405, 1, 1.43063878625335, 0.249085400175541, "<unicode> <unicode> D",
                                      436.181607539711, 436.181607539711, 1, 9.68357489210809, 0.00671125493403696,
                                      "<unicode> <unicode> E", "", 871.837011646071, 10, "", "", "<unicode> Interaction terms",
                                      67.4016021614681, 67.4016021614681, 1, 1.49636860219795, 0.238942285824101,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>B", 47.6509408633181,
                                      47.6509408633181, 1, 1.05788838078723, 0.318996088964146, "<unicode> <unicode> A<unicode><unicode><unicode>C",
                                      18.5769765311749, 18.5769765311749, 1, 0.412423496082852, 0.529836840772662,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>D", 59.1507367552762,
                                      59.1507367552762, 1, 1.3131928980773, 0.268668089382453, "<unicode> <unicode> A<unicode><unicode><unicode>E",
                                      134.516684996498, 134.516684996498, 1, 2.98637624990437, 0.103214709350105,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>C", 84.5899429388892,
                                      84.5899429388892, 1, 1.87796329191461, 0.189487611022757, "<unicode> <unicode> B<unicode><unicode><unicode>D",
                                      1.79916468907368, 1.79916468907368, 1, 0.0399428717504902, 0.844110325478541,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>E", 51.6884900877184,
                                      51.6884900877184, 1, 1.14752515047035, 0.299961265674163, "<unicode> <unicode> C<unicode><unicode><unicode>D",
                                      210.572179125343, 210.572179125343, 1, 4.67486806299832, 0.0461007806065773,
                                      "<unicode> <unicode> C<unicode><unicode><unicode>E", 195.890293497311,
                                      195.890293497311, 1, 4.34891864977489, 0.0534069008645126, "<unicode> <unicode> D<unicode><unicode><unicode>E",
                                      45.0434485610464, 720.695176976742, 16, "", "", "Error", "",
                                      2498.82850537491, 31, "", "", "Total"))
})

test_that("2.2 Five factors full factorial Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 101.699596592187, "", 9.7648722242665e-23, 1.18642646950104,
                                      "(Intercept)", 85.71925796207, "", "A", 2.25096480031251, 4.50192960062502,
                                      0.0759912026029757, 1.18642646950104, "A", 1.89726448134554,
                                      1, "B", -2.75622744781249, -5.51245489562499, 0.0336761387549081,
                                      1.18642646950104, "B", -2.32313381289583, 1, "C", 0.116936386562506,
                                      0.233872773125013, 0.922710410988331, 1.18642646950104, "C",
                                      0.0985618490218655, 1, "D", 1.41907655468751, 2.83815310937502,
                                      0.249085400175541, 1.18642646950104, "D", 1.19609313443952,
                                      1, "E", -3.69197443593749, -7.38394887187498, 0.00671125493403696,
                                      1.18642646950104, "E", -3.11184429110907, 1, "AB", 1.45130977656249,
                                      2.90261955312499, 0.2389422858241, 1.18642646950104, "A<unicode>B",
                                      1.22326146109405, 1, "AC", 1.22028353343749, 2.44056706687498,
                                      0.318996088964146, 1.18642646950104, "A<unicode>C", 1.02853700992586,
                                      1, "AD", -0.761925532187506, -1.52385106437501, 0.529836840772662,
                                      1.18642646950104, "A<unicode>D", -0.6422020679528, 1, "AE",
                                      -1.35958101031251, -2.71916202062502, 0.268668089382452, 1.18642646950104,
                                      "A<unicode>E", -1.14594628935099, 1, "BC", -2.05027959218751,
                                      -4.10055918437501, 0.103214709350105, 1.18642646950104, "B<unicode>C",
                                      -1.72811349450908, 1, "BD", 1.6258646059375, 3.25172921187499,
                                      0.189487611022757, 1.18642646950104, "B<unicode>D", 1.37038800779729,
                                      1, "BE", -0.237115787187507, -0.474231574375015, 0.844110325478542,
                                      1.18642646950104, "B<unicode>E", -0.199857128345449, 1, "CD",
                                      1.27093088531249, 2.54186177062499, 0.299961265674163, 1.18642646950104,
                                      "C<unicode>D", 1.07122600345135, 1, "CE", -2.5652252528125,
                                      -5.13045050562501, 0.0461007806065773, 1.18642646950104, "C<unicode>E",
                                      -2.16214432057583, 1, "DE", 2.4741810103125, 4.94836202062499,
                                      0.0534069008645126, 1.18642646950104, "D<unicode>E", 2.08540611147443,
                                      1))
})

test_that("2.3 Five factors full factorial Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 101.7 (Intercept) + 2.25 A - 2.76 B + 0.12 C + 1.42 D - 3.69 E + 1.45 AB + 1.22 AC - 0.76 AD - 1.36 AE - 2.05 BC + 1.63 BD - 0.24 BE + 1.27 CD - 2.57 CE + 2.47 DE"
                                 ))
})

test_that("2.4 Five factors full factorial Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.441199385076272, 0, 0.711586779394205, 6.71144161570719))
})

### Five factors full factorial with covariates (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$fixedFactorsFactorial <- c("A", "B", "C", "D", "E")
options$covariates <- "Covariate"
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "A"),
  list(components = "B"),
  list(components = "C"),
  list(components = "D"),
  list(components = "E"),
  list(components = c("A", "B")),
  list(components = c("A", "C")),
  list(components = c("A", "D")),
  list(components = c("A", "E")),
  list(components = c("B", "C")),
  list(components = c("B", "D")),
  list(components = c("B", "E")),
  list(components = c("C", "D")),
  list(components = c("C", "E")),
  list(components = c("D", "E"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/2level5facFullCovariate.csv", options)

test_that("3.1 Five factors full factorial with covariates ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(108.781300354899, 1740.50080567839, 16, 2.29851364266209, 0.0573982083927571,
                                      "Model", "", 10.7931450559473, 1, "", "", "<unicode> Covariates",
                                      10.7931450559473, 10.7931450559473, 1, 0.228055659174776, 0.639853403417289,
                                      "<unicode> <unicode> Covariate", "", 881.110290198346, 5, "",
                                      "", "<unicode> Linear terms", 141.030657480367, 141.030657480367,
                                      1, 2.97993211328283, 0.104834657354436, "<unicode> <unicode> A",
                                      247.027120947493, 247.027120947493, 1, 5.21960305450403, 0.0373144413753065,
                                      "<unicode> <unicode> B", 0.0101186920035161, 0.0101186920035161,
                                      1, 0.000213804684629606, 0.988526411897268, "<unicode> <unicode> C",
                                      74.9247545321398, 74.9247545321398, 1, 1.58313579542971, 0.227543055508507,
                                      "<unicode> <unicode> D", 418.117638546342, 418.117638546342,
                                      1, 8.83469027581948, 0.00949269699438971, "<unicode> <unicode> E",
                                      "", 848.597370424093, 10, "", "", "<unicode> Interaction terms",
                                      78.059234858766, 78.059234858766, 1, 1.64936634948543, 0.218527053935101,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>B", 49.4067640697525,
                                      49.4067640697525, 1, 1.04394892213659, 0.323101766517528, "<unicode> <unicode> A<unicode><unicode><unicode>C",
                                      15.9960197378871, 15.9960197378871, 1, 0.337990716013441, 0.569625231928612,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>D", 68.4044545664915,
                                      68.4044545664915, 1, 1.44536396905517, 0.247909307290872, "<unicode> <unicode> A<unicode><unicode><unicode>E",
                                      110.976222237363, 110.976222237363, 1, 2.34489163111195, 0.146509749204623,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>C", 84.3268156217385,
                                      84.3268156217385, 1, 1.78179830096219, 0.201836398006105, "<unicode> <unicode> B<unicode><unicode><unicode>D",
                                      3.87968368382462, 3.87968368382462, 1, 0.0819764596248717, 0.778553166992426,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>E", 34.4857874083674,
                                      34.4857874083674, 1, 0.728673518127394, 0.406739278085951, "<unicode> <unicode> C<unicode><unicode><unicode>D",
                                      201.73385056334, 201.73385056334, 1, 4.26257092159968, 0.0566992364049144,
                                      "<unicode> <unicode> C<unicode><unicode><unicode>E", 201.328537676563,
                                      201.328537676563, 1, 4.25400679158132, 0.0569197989649348, "<unicode> <unicode> D<unicode><unicode><unicode>E",
                                      47.326802128053, 709.902031920794, 15, "", "", "Error", "",
                                      2450.40283759918, 31, "", "", "Total"))
})

test_that("3.2 Five factors full factorial with covariates Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 109.731825415679, "", 9.90816291567143e-06, 16.8635074468689,
                                      "(Intercept)", 6.50705825946392, "", "A", 2.13840267682821,
                                      4.27680535365642, 0.104834657354436, 1.23875752075877, "A",
                                      1.72624798719154, 1.03756527040855, "B", -2.98338174503944,
                                      -5.96676349007888, 0.0373144413753065, 1.30584036708283, "B",
                                      -2.2846450609458, 1.1529832484784, "C", -0.0182577225440942,
                                      -0.0365154450881883, 0.98852641189724, 1.24864215989919, "C",
                                      -0.014622061572523, 1.05418979411064, "D", 1.70149654086625,
                                      3.4029930817325, 0.227543055508507, 1.35229669371421, "D", 1.25822724315988,
                                      1.23647913020274, "E", -3.63318781224342, -7.26637562448684,
                                      0.0094926969943897, 1.22234045141578, "E", -2.97232068858989,
                                      1.01024610967775, "COV1", -0.170265654150517, "", 0.639853403417289,
                                      0.356538657135893, "Covariate", -0.477551734553207, 1.05418979411064,
                                      "AB", 1.71225939719856, 3.42451879439712, 0.218527053935101,
                                      1.33324816148842, "A<unicode>B", 1.28427658605358, 1.03756527040855,
                                      "AC", 1.24356000989087, 2.48712001978175, 0.323101766517528,
                                      1.21710241047352, "A<unicode>C", 1.02173818668805, 1.03756527040855,
                                      "AD", -0.7098549031372, -1.4197098062744, 0.569625231928613,
                                      1.22100432101365, "A<unicode>D", -0.581369689623943, 1.03756527040855,
                                      "AE", -1.510844791645, -3.02168958329001, 0.247909307290872,
                                      1.2566989137569, "A<unicode>E", -1.20223290965402, 1.03756527040855,
                                      "BC", -1.9133327168253, -3.82666543365061, 0.146509749204623,
                                      1.24947942604136, "B<unicode>C", -1.53130389900632, 1.1529832484784,
                                      "BD", 1.6233491385151, 3.2466982770302, 0.201836398006105, 1.21613745615216,
                                      "B<unicode>D", 1.3348401780596, 1.1529832484784, "BE", -0.355336376717944,
                                      -0.710672753435889, 0.778553166992427, 1.24106660753279, "B<unicode>E",
                                      -0.286315315037235, 1.1529832484784, "CD", 1.08825673873087,
                                      2.17651347746173, 0.406739278085951, 1.27486697751332, "C<unicode>D",
                                      0.853623756773084, 1.05418979411064, "CE", -2.51881683426932,
                                      -5.03763366853864, 0.0566992364049144, 1.22000266183344, "C<unicode>E",
                                      -2.06459945790937, 1.05418979411064, "DE", 2.51425344501745,
                                      5.02850689003489, 0.0569197989649348, 1.21901756761415, "D<unicode>E",
                                      2.06252437357267, 1.23647913020274))
})

test_that("3.3 Five factors full factorial with covariates Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 109.73 (Intercept) + 2.14 A - 2.98 B - 0.02 C + 1.7 D - 3.63 E - 0.17 COV1 + 1.71 AB + 1.24 AC - 0.71 AD - 1.51 AE - 1.91 BC + 1.62 BD - 0.36 BE + 1.09 CD - 2.52 CE + 2.51 DE"
                                 ))
})

test_that("3.4 Five factors full factorial with covariates Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.41287252694057, 0, 0.715906061422857, 6.87944780691394))
})

### Five factors fractional factorial (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$fixedFactorsFactorial <- c("A", "B", "C", "D", "E")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "A"),
  list(components = "B"),
  list(components = "C"),
  list(components = "D"),
  list(components = "E"),
  list(components = c("A", "B")),
  list(components = c("A", "C")),
  list(components = c("A", "D")),
  list(components = c("A", "E")),
  list(components = c("B", "C")),
  list(components = c("B", "D")),
  list(components = c("B", "E")),
  list(components = c("C", "D")),
  list(components = c("C", "E")),
  list(components = c("D", "E"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/2level5facPart.csv", options)

test_that("4.1 Five factors fractional factorial ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(80.1546981866672, 561.08288730667, 7, "", "", "Model", "", 424.686497752278,
                                      5, "", "", "<unicode> Linear terms", 50.7650166276942, 50.7650166276942,
                                      1, "", "", "<unicode> <unicode> A", 26.1413998561852, 26.1413998561852,
                                      1, "", "", "<unicode> <unicode> B", 73.6306217983596, 73.6306217983596,
                                      1, "", "", "<unicode> <unicode> C", 57.9093886693107, 57.9093886693107,
                                      1, "", "", "<unicode> <unicode> D", 216.240070800728, 216.240070800728,
                                      1, "", "", "<unicode> <unicode> E", "", 136.396389554392, 2,
                                      "", "", "<unicode> Interaction terms", 77.6251988684634, 77.6251988684634,
                                      1, "", "", "<unicode> <unicode> B<unicode><unicode><unicode>C",
                                      58.7711906859291, 58.7711906859291, 1, "", "", "<unicode> <unicode> B<unicode><unicode><unicode>E",
                                      0, 0, 0, "", "", "Error", "", 561.08288730667, 7, "", "", "Total"
                                 ))
})

test_that("4.2 Five factors fractional factorial Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 102.53699416875, "", "", "", "(Intercept)", "",
                                      "", "A", -2.51905281375, -5.03810562749999, "", "", "A", "",
                                      "NaN", "B", -1.80767114875, -3.6153422975, "", "", "B", "",
                                      "NaN", "C", -3.03378109375, -6.0675621875, "", "", "C", "",
                                      "NaN", "D", 2.69047831875, 5.3809566375, "", "", "D", "", "NaN",
                                      "E", 5.19903922375, 10.3980784475, "", "", "E", "", "NaN", "BC",
                                      3.11498793874999, 6.22997587749999, "", "", "B<unicode>C", "",
                                      "NaN", "BE", 2.71042410625, 5.42084821250001, "", "", "B<unicode>E",
                                      "", "NaN"))
})

test_that("4.3 Five factors fractional factorial Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 102.54 (Intercept) - 2.52 A - 1.81 B - 3.03 C + 2.69 D + 5.2 E + 3.11 BC + 2.71 BE"
                                 ))
})

test_that("4.4 Five factors fractional factorial Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", "", 1, ""))
})

### Nine factors highest factorial ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$fixedFactorsFactorial <- c("A", "B", "C", "D", "E", "F", "G", "H", "J")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "A"),
  list(components = "B"),
  list(components = "C"),
  list(components = "D"),
  list(components = "E"),
  list(components = "F"),
  list(components = "G"),
  list(components = "H"),
  list(components = "J"),
  list(components = c("A", "B")),
  list(components = c("A", "C")),
  list(components = c("A", "D")),
  list(components = c("A", "E")),
  list(components = c("A", "F")),
  list(components = c("A", "G")),
  list(components = c("A", "H")),
  list(components = c("A", "J")),
  list(components = c("B", "C")),
  list(components = c("B", "D")),
  list(components = c("B", "E")),
  list(components = c("B", "F")),
  list(components = c("B", "G")),
  list(components = c("B", "H")),
  list(components = c("B", "J")),
  list(components = c("C", "D")),
  list(components = c("C", "E")),
  list(components = c("C", "F")),
  list(components = c("C", "G")),
  list(components = c("C", "H")),
  list(components = c("C", "J")),
  list(components = c("D", "E")),
  list(components = c("D", "F")),
  list(components = c("D", "G")),
  list(components = c("D", "H")),
  list(components = c("D", "J")),
  list(components = c("E", "F")),
  list(components = c("E", "G")),
  list(components = c("E", "H")),
  list(components = c("E", "J")),
  list(components = c("F", "G")),
  list(components = c("F", "H")),
  list(components = c("F", "J")),
  list(components = c("G", "H")),
  list(components = c("G", "J")),
  list(components = c("H", "J"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/2level9facFull.csv", options)

test_that("5.1 Nine factors highest factorial ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(75.5535356760645, 3399.9091054229, 45, 0.602413101972684, 0.967387168558948,
                                      "Model", "", 1045.45142898722, 9, "", "", "<unicode> Linear terms",
                                      30.8204833710261, 30.8204833710261, 1, 0.245741815067952, 0.621415495032236,
                                      "<unicode> <unicode> A", 4.96595226817408, 4.96595226817408,
                                      1, 0.0395951649826861, 0.842766776754448, "<unicode> <unicode> B",
                                      55.5556881194261, 55.5556881194261, 1, 0.442963709279499, 0.507563306486719,
                                      "<unicode> <unicode> C", 354.524564444884, 354.524564444884,
                                      1, 2.82674054472366, 0.0965115801168659, "<unicode> <unicode> D",
                                      9.75443046735563, 9.75443046735563, 1, 0.0777752710476811, 0.781037368149799,
                                      "<unicode> <unicode> E", 101.788197823913, 101.788197823913,
                                      1, 0.811590661464417, 0.370288714827774, "<unicode> <unicode> F",
                                      16.9830667330298, 16.9830667330298, 1, 0.135411557117832, 0.713834030384068,
                                      "<unicode> <unicode> G", 57.4807438322387, 57.4807438322387,
                                      1, 0.458312809398357, 0.500318955258729, "<unicode> <unicode> H",
                                      413.578301927173, 413.578301927173, 1, 3.29759534802913, 0.0730363575287113,
                                      "<unicode> <unicode> J", "", 2354.45767643568, 36, "", "", "<unicode> Interaction terms",
                                      9.32952478999323, 9.32952478999323, 1, 0.0743873588228562, 0.785738397452057,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>B", 89.8767841531353,
                                      89.8767841531353, 1, 0.71661705640299, 0.399719587411985, "<unicode> <unicode> A<unicode><unicode><unicode>C",
                                      3.30384365240207, 3.30384365240207, 1, 0.0263426282472021, 0.871465653397687,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>D", 93.7580820583753,
                                      93.7580820583753, 1, 0.747563916663779, 0.389770922927739, "<unicode> <unicode> A<unicode><unicode><unicode>E",
                                      2.39883481785182, 2.39883481785182, 1, 0.0191266962004003, 0.890342941728586,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>F", 21.3054337592348,
                                      21.3054337592348, 1, 0.169875206036721, 0.681298243583707, "<unicode> <unicode> A<unicode><unicode><unicode>G",
                                      12.0575085574474, 12.0575085574474, 1, 0.0961384674741988, 0.757300428570357,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>H", 12.2344995710173,
                                      12.2344995710173, 1, 0.0975496748326882, 0.755583033929782,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>J", 10.4978399080392,
                                      10.4978399080392, 1, 0.0837027181643595, 0.773071152537467,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>C", 30.6076244702708,
                                      30.6076244702708, 1, 0.244044621289539, 0.622620763165018, "<unicode> <unicode> B<unicode><unicode><unicode>D",
                                      11.1894201996074, 11.1894201996074, 1, 0.0892169144885798, 0.765930691875554,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>E", 374.067255529071,
                                      374.067255529071, 1, 2.98256082568834, 0.0879312888335771, "<unicode> <unicode> B<unicode><unicode><unicode>F",
                                      409.675212987153, 409.675212987153, 1, 3.26647474070622, 0.0743768268842423,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>G", 2.62960706148078,
                                      2.62960706148078, 1, 0.0209667189324902, 0.885224968044367,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>H", 73.2168714282507,
                                      73.2168714282507, 1, 0.583782112103071, 0.447026664986005, "<unicode> <unicode> B<unicode><unicode><unicode>J",
                                      44.9261072677291, 44.9261072677291, 1, 0.358210577394384, 0.551151429297806,
                                      "<unicode> <unicode> C<unicode><unicode><unicode>D", 112.128221749561,
                                      112.128221749561, 1, 0.894035061078328, 0.347164081950862, "<unicode> <unicode> C<unicode><unicode><unicode>E",
                                      38.3039240158414, 38.3039240158414, 1, 0.305409739962959, 0.582014077789976,
                                      "<unicode> <unicode> C<unicode><unicode><unicode>F", 15.5338549516,
                                      15.5338549516, 1, 0.123856516617682, 0.725791670550459, "<unicode> <unicode> C<unicode><unicode><unicode>G",
                                      114.694808045684, 114.694808045684, 1, 0.914499294794103, 0.341734325116038,
                                      "<unicode> <unicode> C<unicode><unicode><unicode>H", 13.0063127731282,
                                      13.0063127731282, 1, 0.103703594448318, 0.748249183228579, "<unicode> <unicode> C<unicode><unicode><unicode>J",
                                      3.15071025244288, 3.15071025244288, 1, 0.0251216454611607, 0.874454019681298,
                                      "<unicode> <unicode> D<unicode><unicode><unicode>E", 52.3254569274613,
                                      52.3254569274613, 1, 0.41720801730522, 0.520137023544091, "<unicode> <unicode> D<unicode><unicode><unicode>F",
                                      10.5218911236825, 10.5218911236825, 1, 0.0838944863892648, 0.77281860960123,
                                      "<unicode> <unicode> D<unicode><unicode><unicode>G", 50.2470011968016,
                                      50.2470011968016, 1, 0.400635808568518, 0.528522353503045, "<unicode> <unicode> D<unicode><unicode><unicode>H",
                                      0.0801623713050503, 0.0801623713050503, 1, 0.000639160858949187,
                                      0.979891749560344, "<unicode> <unicode> D<unicode><unicode><unicode>J",
                                      60.9726212121022, 60.9726212121022, 1, 0.486154692181061, 0.487619799966989,
                                      "<unicode> <unicode> E<unicode><unicode><unicode>F", 66.8500537309446,
                                      66.8500537309446, 1, 0.53301738793221, 0.467421683226516, "<unicode> <unicode> E<unicode><unicode><unicode>G",
                                      32.3136561311949, 32.3136561311949, 1, 0.257647370859424, 0.613103632224309,
                                      "<unicode> <unicode> E<unicode><unicode><unicode>H", 7.63092299646814,
                                      7.63092299646814, 1, 0.060843850020819, 0.805783607203107, "<unicode> <unicode> E<unicode><unicode><unicode>J",
                                      138.431956921864, 138.431956921864, 1, 1.10376336243213, 0.296528087166271,
                                      "<unicode> <unicode> F<unicode><unicode><unicode>G", 28.2072330589053,
                                      28.2072330589053, 1, 0.224905513858894, 0.636587754513333, "<unicode> <unicode> F<unicode><unicode><unicode>H",
                                      135.647572134721, 135.647572134721, 1, 1.08156254996586, 0.301405028207972,
                                      "<unicode> <unicode> F<unicode><unicode><unicode>J", 242.50706773453,
                                      242.50706773453, 1, 1.93358833067212, 0.168128897239096, "<unicode> <unicode> G<unicode><unicode><unicode>H",
                                      30.75253213426, 30.75253213426, 1, 0.245200017586777, 0.621799688777868,
                                      "<unicode> <unicode> G<unicode><unicode><unicode>J", 0.0772667621222354,
                                      0.0772667621222354, 1, 0.000616073217923371, 0.980258186002336,
                                      "<unicode> <unicode> H<unicode><unicode><unicode>J", 125.418148158887,
                                      10284.2881490287, 82, "", "", "Error", "", 13684.1972544516,
                                      127, "", "", "Total"))
})

test_that("5.2 Nine factors highest factorial Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 101.837859991875, "", 1.83191023845293e-88, 0.989863264542786,
                                      "(Intercept)", 102.880734784024, "", "A", 0.490698508593758,
                                      0.981397017187515, 0.621415495032238, 0.989863264542786, "A",
                                      0.495723526845306, 1, "B", 0.196968276874991, 0.393936553749982,
                                      0.842766776754461, 0.989863264542786, "B", 0.198985338612369,
                                      1, "C", -0.658808631875008, -1.31761726375002, 0.507563306486717,
                                      0.989863264542786, "C", -0.6655551887556, 1, "D", -1.66424852703124,
                                      -3.32849705406248, 0.096511580116866, 0.989863264542786, "D",
                                      -1.68129133249525, 1, "E", -0.276055226406257, -0.552110452812514,
                                      0.781037368149807, 0.989863264542786, "E", -0.278882181301845,
                                      1, "F", 0.891751252031259, 1.78350250406252, 0.370288714827774,
                                      0.989863264542786, "F", 0.900883267390629, 1, "G", -0.364252946249994,
                                      -0.728505892499988, 0.713834030384062, 0.989863264542786, "G",
                                      -0.367983093521751, 1, "H", 0.670125593593742, 1.34025118718748,
                                      0.500318955258728, 0.989863264542786, "H", 0.676988042286094,
                                      1, "I", 1.79752064906249, 3.59504129812498, 0.0730363575287111,
                                      0.989863264542786, "J", 1.81592823317144, 1, "AB", -0.269975762656241,
                                      -0.539951525312482, 0.785738397452062, 0.989863264542786, "A<unicode>B",
                                      -0.272740460553349, 1, "AC", 0.837951297031258, 1.67590259406252,
                                      0.399719587411985, 0.989863264542786, "A<unicode>C", 0.846532371739551,
                                      1, "AD", -0.160658888750007, -0.321317777500014, 0.871465653397697,
                                      0.989863264542786, "A<unicode>D", -0.162304122705488, 1, "AE",
                                      0.855853384687508, 1.71170676937502, 0.389770922927737, 0.989863264542786,
                                      "A<unicode>E", 0.86461778646046, 1, "AF", 0.136897395937491,
                                      0.273794791874982, 0.89034294172859, 0.989863264542786, "A<unicode>F",
                                      0.138299299348909, 1, "AG", -0.407981251093758, -0.815962502187516,
                                      0.681298243583702, 0.989863264542786, "A<unicode>G", -0.412159199869088,
                                      1, "AH", 0.306919021250009, 0.613838042500018, 0.75730042857035,
                                      0.989863264542786, "A<unicode>H", 0.31006203810561, 1, "AI",
                                      0.309163432343758, 0.618326864687516, 0.75558303392978, 0.989863264542786,
                                      "A<unicode>J", 0.31232943318344, 1, "BC", -0.286381518750009,
                                      -0.572763037500017, 0.773071152537459, 0.989863264542786, "B<unicode>C",
                                      -0.289314220466892, 1, "BD", -0.489001090156241, -0.978002180312482,
                                      0.622620763165017, 0.989863264542786, "B<unicode>D", -0.494008725924493,
                                      1, "BE", 0.295664244218742, 0.591328488437484, 0.765930691875545,
                                      0.989863264542786, "B<unicode>E", 0.298692006067432, 1, "BF",
                                      -1.70950297859374, -3.41900595718748, 0.0879312888335769, 0.989863264542786,
                                      "B<unicode>F", -1.72700921412954, 1, "BG", 1.78901861406251,
                                      3.57803722812501, 0.0743768268842421, 0.989863264542786, "B<unicode>G",
                                      1.80733913273249, 1, "BH", 0.143331103281242, 0.286662206562485,
                                      0.885224968044369, 0.989863264542786, "B<unicode>H", 0.144798891337226,
                                      1, "BI", -0.756311316875008, -1.51262263375002, 0.447026664986005,
                                      0.989863264542786, "B<unicode>J", -0.764056354010011, 1, "CD",
                                      -0.592440050156239, -1.18488010031248, 0.551151429297809, 0.989863264542786,
                                      "C<unicode>D", -0.598506956847101, 1, "CE", -0.935949642031259,
                                      -1.87189928406252, 0.347164081950862, 0.989863264542786, "C<unicode>E",
                                      -0.94553427282057, 1, "CF", 0.54703693328126, 1.09407386656252,
                                      0.582014077789973, 0.989863264542786, "C<unicode>F", 0.552638887487086,
                                      1, "CG", 0.348365098437508, 0.696730196875016, 0.72579167055046,
                                      0.989863264542786, "C<unicode>G", 0.351932545550538, 1, "CH",
                                      0.946600859843742, 1.89320171968748, 0.341734325116037, 0.989863264542786,
                                      "C<unicode>H", 0.95629456486697, 1, "CI", -0.318766087500008,
                                      -0.637532175000015, 0.748249183228581, 0.989863264542786, "C<unicode>J",
                                      -0.322030424724616, 1, "DE", -0.156891439687492, -0.313782879374984,
                                      0.874454019681288, 0.989863264542785, "D<unicode>E", -0.158498092925956,
                                      1, "DF", 0.639368932812492, 1.27873786562498, 0.520137023544094,
                                      0.989863264542786, "D<unicode>F", 0.645916416655602, 1, "DG",
                                      -0.286709390156259, -0.573418780312517, 0.772818609601225, 0.989863264542786,
                                      "D<unicode>G", -0.289645449453757, 1, "DH", -0.626541855624991,
                                      -1.25308371124998, 0.528522353503047, 0.989863264542786, "D<unicode>H",
                                      -0.632957983256797, 1, "DI", -0.02502535765624, -0.05005071531248,
                                      0.979891749560312, 0.989863264542786, "D<unicode>J", -0.0252816308601968,
                                      1, "EF", -0.690180123749991, -1.38036024749998, 0.487619799966988,
                                      0.989863264542786, "E<unicode>F", -0.697247941682918, 1, "EG",
                                      0.722679766406257, 1.44535953281251, 0.467421683226517, 0.989863264542786,
                                      "E<unicode>G", 0.730080398265977, 1, "EH", 0.502444463124992,
                                      1.00488892624998, 0.613103632224308, 0.989863264542786, "E<unicode>H",
                                      0.507589766306834, 1, "EI", -0.244165079218758, -0.488330158437515,
                                      0.805783607203115, 0.989863264542786, "E<unicode>J", -0.246665461750959,
                                      1, "FG", 1.03995176015624, 2.07990352031248, 0.29652808716627,
                                      0.989863264542786, "F<unicode>G", 1.0506014289121, 1, "FH",
                                      -0.469434775312494, -0.938869550624988, 0.636587754513338, 0.989863264542786,
                                      "F<unicode>H", -0.47424204142915, 1, "FI", 1.02943997265626,
                                      2.05887994531252, 0.301405028207971, 0.989863264542786, "F<unicode>J",
                                      1.03998199502004, 1, "GH", -1.37643977953124, -2.75287955906248,
                                      0.168128897239097, 0.989863264542786, "G<unicode>H", -1.39053526768368,
                                      1, "GI", -0.490157278124991, -0.980314556249982, 0.621799688777872,
                                      0.989863264542786, "G<unicode>J", -0.495176753883672, 1, "HI",
                                      -0.0245692201562575, -0.0491384403125149, 0.980258186002227,
                                      0.989863264542786, "H<unicode>J", -0.0248208222653923, 1))
})

test_that("5.3 Nine factors highest factorial Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 101.84 (Intercept) + 0.49 A + 0.2 B - 0.66 C - 1.66 D - 0.28 E + 0.89 F - 0.36 G + 0.67 H + 1.8 I - 0.27 AB + 0.84 AC - 0.16 AD + 0.86 AE + 0.14 AF - 0.41 AG + 0.31 AH + 0.31 AI - 0.29 BC - 0.49 BD + 0.3 BE - 1.71 BF + 1.79 BG + 0.14 BH - 0.76 BI - 0.59 CD - 0.94 CE + 0.55 CF + 0.35 CG + 0.95 CH - 0.32 CI - 0.16 DE + 0.64 DF - 0.29 DG - 0.63 DH - 0.03 DI - 0.69 EF + 0.72 EG + 0.5 EH - 0.24 EI + 1.04 FG - 0.47 FH + 1.03 FI - 1.38 GH - 0.49 GI - 0.02 HI"
                                 ))
})

test_that("5.4 Nine factors highest factorial Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0.248455137133958, 11.1990244288905))
})

### Nine factors smallest fractional factorial ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$fixedFactorsFactorial <- c("A", "B", "C", "D", "E", "F", "G", "H", "J")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "A"),
  list(components = "B"),
  list(components = "C"),
  list(components = "D"),
  list(components = "E"),
  list(components = "F"),
  list(components = "G"),
  list(components = "H"),
  list(components = "J"),
  list(components = c("A", "B")),
  list(components = c("A", "C")),
  list(components = c("A", "D")),
  list(components = c("A", "E")),
  list(components = c("A", "F")),
  list(components = c("A", "G")),
  list(components = c("A", "H")),
  list(components = c("A", "J")),
  list(components = c("B", "C")),
  list(components = c("B", "D")),
  list(components = c("B", "E")),
  list(components = c("B", "F")),
  list(components = c("B", "G")),
  list(components = c("B", "H")),
  list(components = c("B", "J")),
  list(components = c("C", "D")),
  list(components = c("C", "E")),
  list(components = c("C", "F")),
  list(components = c("C", "G")),
  list(components = c("C", "H")),
  list(components = c("C", "J")),
  list(components = c("D", "E")),
  list(components = c("D", "F")),
  list(components = c("D", "G")),
  list(components = c("D", "H")),
  list(components = c("D", "J")),
  list(components = c("E", "F")),
  list(components = c("E", "G")),
  list(components = c("E", "H")),
  list(components = c("E", "J")),
  list(components = c("F", "G")),
  list(components = c("F", "H")),
  list(components = c("F", "J")),
  list(components = c("G", "H")),
  list(components = c("G", "J")),
  list(components = c("H", "J"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/2level9facPart.csv", options)

test_that("6.1 Nine factors smallest fractional ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(90.1796309137531, 1352.6944637063, 15, "", "", "Model", "", 1169.65318986718,
                                      9, "", "", "<unicode> Linear terms", 0.969245325104176, 0.969245325104176,
                                      1, "", "", "<unicode> <unicode> A", 168.831270610839, 168.831270610839,
                                      1, "", "", "<unicode> <unicode> B", 285.153858355603, 285.153858355603,
                                      1, "", "", "<unicode> <unicode> C", 82.9727915216316, 82.9727915216316,
                                      1, "", "", "<unicode> <unicode> D", 5.7176647368841, 5.7176647368841,
                                      1, "", "", "<unicode> <unicode> E", 0.00593949473499266, 0.00593949473499266,
                                      1, "", "", "<unicode> <unicode> F", 208.446004546923, 208.446004546923,
                                      1, "", "", "<unicode> <unicode> G", 318.373969472585, 318.373969472585,
                                      1, "", "", "<unicode> <unicode> H", 99.1824458028796, 99.1824458028796,
                                      1, "", "", "<unicode> <unicode> J", "", 183.041273839112, 6,
                                      "", "", "<unicode> Interaction terms", 3.46722538910245, 3.46722538910245,
                                      1, "", "", "<unicode> <unicode> A<unicode><unicode><unicode>B",
                                      4.0740894872578, 4.0740894872578, 1, "", "", "<unicode> <unicode> A<unicode><unicode><unicode>C",
                                      12.4472474600343, 12.4472474600343, 1, "", "", "<unicode> <unicode> A<unicode><unicode><unicode>D",
                                      5.0281447678916, 5.0281447678916, 1, "", "", "<unicode> <unicode> A<unicode><unicode><unicode>E",
                                      150.051089770656, 150.051089770656, 1, "", "", "<unicode> <unicode> A<unicode><unicode><unicode>G",
                                      7.97347696416911, 7.97347696416911, 1, "", "", "<unicode> <unicode> A<unicode><unicode><unicode>H",
                                      0, 0, 0, "", "", "Error", "", 1352.6944637063, 15, "", "", "Total"
                                 ))
})

test_that("6.2 Nine factors smallest fractional Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 99.366952810625, "", "", "", "(Intercept)", "",
                                      "", "A", -0.246125644375004, -0.492251288750007, "", "", "A",
                                      "", "NaN", "B", -3.248377196875, -6.49675439374999, "", "",
                                      "B", "", "NaN", "C", -4.221624823125, -8.44324964625, "", "",
                                      "C", "", "NaN", "D", 2.277235049375, 4.55447009875001, "", "",
                                      "D", "", "NaN", "E", 0.597790971874999, 1.19558194375, "", "",
                                      "E", "", "NaN", "F", -0.0192670293749972, -0.0385340587499944,
                                      "", "", "F", "", "NaN", "G", -3.609414811875, -7.21882962375001,
                                      "", "", "G", "", "NaN", "H", -4.460759250625, -8.92151850125001,
                                      "", "", "H", "", "NaN", "I", -2.489759599375, -4.97951919875,
                                      "", "", "J", "", "NaN", "AB", -0.465512176875002, -0.931024353750003,
                                      "", "", "A<unicode>B", "", "NaN", "AC", 0.504609346874998, 1.00921869375,
                                      "", "", "A<unicode>C", "", "NaN", "AD", -0.882016420625006,
                                      -1.76403284125001, "", "", "A<unicode>D", "", "NaN", "AE", 0.560588126875003,
                                      1.12117625375001, "", "", "A<unicode>E", "", "NaN", "AG", -3.062383566875,
                                      -6.12476713374999, "", "", "A<unicode>G", "", "NaN", "AH", 0.705933644375001,
                                      1.41186728875, "", "", "A<unicode>H", "", "NaN"))
})

test_that("6.3 Nine factors smallest fractional Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 99.37 (Intercept) - 0.25 A - 3.25 B - 4.22 C + 2.28 D + 0.6 E - 0.02 F - 3.61 G - 4.46 H - 2.49 I - 0.47 AB + 0.5 AC - 0.88 AD + 0.56 AE - 3.06 AG + 0.71 AH"
                                 ))
})

test_that("6.4 Nine factors smallest fractional Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", "", 1, ""))
})

### One center point (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$continuousFactorsFactorial <- c("A", "B", "C")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "A"),
  list(components = "B"),
  list(components = "C"),
  list(components = c("A", "B")),
  list(components = c("B", "C")),
  list(components = c("A", "C")),
  list(components = c("A", "B", "C"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/oneCenterPointFactorial.csv", options)

test_that("7.1 One center point ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(122.7259174713, 859.081422299102, 7, 1.11103241899578, 0.625640913233291,
                                      "Model", "", 329.303756397979, 3, "", "", "<unicode> Linear terms",
                                      20.4988060432464, 20.4988060432464, 1, 0.185574803872046, 0.741048872740814,
                                      "<unicode> <unicode> A", 153.781480665113, 153.781480665113,
                                      1, 1.39217708843015, 0.447579170584851, "<unicode> <unicode> B",
                                      155.02346968962, 155.02346968962, 1, 1.40342076131274, 0.446316687483796,
                                      "<unicode> <unicode> C", "", 529.777665901122, 4, "", "", "<unicode> Interaction terms",
                                      2.17033944907811, 2.17033944907811, 1, 0.0196479890950102, 0.911341828758052,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>B", 518.237753393667,
                                      518.237753393667, 1, 4.69158395090061, 0.275353167962136, "<unicode> <unicode> B<unicode><unicode><unicode>C",
                                      3.3559860453033, 3.3559860453033, 1, 0.0303815964130099, 0.890138865476036,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>C", 6.01358701307385,
                                      6.01358701307385, 1, 0.0544407429468962, 0.854071207712093,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>B<unicode><unicode><unicode>C",
                                      110.461148903492, 110.461148903492, 1, "", "", "Error", "",
                                      969.542571202594, 8, "", "", "Total"))
})

test_that("7.2 One center point Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 100.148696226667, "", 0.022260826815119, 3.50334996671428,
                                      "(Intercept)", 28.5865520653634, "", "A", 1.60073444250001,
                                      3.20146888500001, 0.741048872740814, 3.7158637775, "A", 0.430783941056356,
                                      1, "B", 4.38436826500001, 8.76873653000001, 0.447579170584851,
                                      3.7158637775, "B", 1.17990554216435, 1, "C", 4.40203744999999,
                                      8.80407489999998, 0.446316687483796, 3.7158637775, "C", 1.18466061018029,
                                      1, "AB", 0.520857400000005, 1.04171480000001, 0.911341828758052,
                                      3.7158637775, "A<unicode>B", 0.140171284844687, 1, "BC", 8.04858491749999,
                                      16.097169835, 0.275353167962136, 3.7158637775, "B<unicode>C",
                                      2.16600645218351, 1, "AC", 0.647686849999992, 1.29537369999998,
                                      0.890138865476036, 3.7158637775, "A<unicode>C", 0.174303173846634,
                                      1, "ABC", 0.867005407499994, 1.73401081499999, 0.854071207712093,
                                      3.7158637775, "A<unicode>B<unicode>C", 0.233325401418054, 1
                                 ))
})

test_that("7.3 One center point Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 100.15 (Intercept) + 1.6 A + 4.38 B + 4.4 C + 0.52 AB + 8.05 BC + 0.65 AC + 0.87 ABC"
                                 ))
})

test_that("7.4 One center point Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0885503973984001, 0, 0.8860687996748, 10.5100499001428))
})


### Two center points (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$continuousFactorsFactorial <- c("A", "B", "C")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "A"),
  list(components = "B"),
  list(components = "C"),
  list(components = c("A", "B")),
  list(components = c("B", "C")),
  list(components = c("A", "C")),
  list(components = c("A", "B", "C"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/twoCenterPointFactorial.csv", options)

test_that("8.1 Two center points ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(131.789445221657, 922.5261165516, 7, 0.609062731722388, 0.739795250787739,
                                      "Model", "", 245.174809387768, 3, "", "", "<unicode> Linear terms",
                                      5.61077871514618, 5.61077871514618, 1, 0.0259301206222484, 0.886866808752451,
                                      "<unicode> <unicode> A", 67.1096301575722, 67.1096301575722,
                                      1, 0.310146040905659, 0.633593302894651, "<unicode> <unicode> B",
                                      172.45440051505, 172.45440051505, 1, 0.796995147058882, 0.466195805480084,
                                      "<unicode> <unicode> C", "", 677.351307163832, 4, "", "", "<unicode> Interaction terms",
                                      5.56079929788496, 5.56079929788496, 1, 0.0256991415756653, 0.887365396018913,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>B", 581.89138353693,
                                      581.89138353693, 1, 2.68920136226875, 0.242710003420074, "<unicode> <unicode> B<unicode><unicode><unicode>C",
                                      79.8250414862999, 79.8250414862999, 1, 0.36891010312493, 0.605373737327509,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>C", 10.0740828427165,
                                      10.0740828427165, 1, 0.0465572065005873, 0.849172159620474,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>B<unicode><unicode><unicode>C",
                                      216.380741026405, 432.761482052811, 2, "", "", "Error", "",
                                      1355.28759860441, 9, "", "", "Total"))
})

test_that("8.2 Two center points Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 102.596932367, "", 0.00204933629173055, 4.65167433325255,
                                      "(Intercept)", 22.0559147130281, "", "A", 0.837464828750004,
                                      1.67492965750001, 0.886866808752451, 5.20073000917185, "A",
                                      0.161028322422636, 1, "B", -2.89632590875, -5.7926518175, 0.633593302894651,
                                      5.20073000917185, "B", -0.556907569445468, 1, "C", -4.64293011625,
                                      -9.2858602325, 0.466195805480084, 5.20073000917185, "C", -0.892745846844936,
                                      1, "AB", 0.833726521249994, 1.66745304249999, 0.887365396018913,
                                      5.20073000917185, "A<unicode>B", 0.160309518044517, 1, "BC",
                                      8.52856511625, 17.0571302325, 0.242710003420074, 5.20073000917185,
                                      "B<unicode>C", 1.63987845960265, 1, "AC", -3.15881784625, -6.3176356925,
                                      0.605373737327508, 5.20073000917185, "A<unicode>C", -0.607379702595444,
                                      1, "ABC", -1.12216770375, -2.24433540750001, 0.849172159620473,
                                      5.20073000917185, "A<unicode>B<unicode>C", -0.215771190154263,
                                      1))
})

test_that("8.3 Two center points Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 102.6 (Intercept) + 0.84 A - 2.9 B - 4.64 C + 0.83 AB + 8.53 BC - 3.16 AC - 1.12 ABC"
                                 ))
})

test_that("8.4 Two center points Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0.680686606666776, 14.7098858264232))
})

### Four center points (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$continuousFactorsFactorial <- c("A", "B", "C")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "A"),
  list(components = "B"),
  list(components = "C"),
  list(components = c("A", "B")),
  list(components = c("B", "C")),
  list(components = c("A", "C")),
  list(components = c("A", "B", "C"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/fourCenterPointFactorial.csv", options)

test_that("9.1 Four center points ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(290.51551762095, 2033.60862334665, 7, 3.57535197416145, 0.117806649515407,
                                      "Model", "", 730.813669799013, 3, "", "", "<unicode> Linear terms",
                                      345.519281926251, 345.519281926251, 1, 4.25227904127894, 0.108193899336995,
                                      "<unicode> <unicode> A", 143.670027399833, 143.670027399833,
                                      1, 1.76813589958397, 0.254373371146824, "<unicode> <unicode> B",
                                      241.624360472929, 241.624360472929, 1, 2.97365228989092, 0.159720253244519,
                                      "<unicode> <unicode> C", "", 1302.79495354764, 4, "", "", "<unicode> Interaction terms",
                                      22.9131620981902, 22.9131620981902, 1, 0.281990511257076, 0.623516478002212,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>B", 844.76462822812,
                                      844.76462822812, 1, 10.396452850336, 0.0321473359349366, "<unicode> <unicode> B<unicode><unicode><unicode>C",
                                      80.029040114527, 80.029040114527, 1, 0.984911198227457, 0.377164601363556,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>C", 355.088123106801,
                                      355.088123106801, 1, 4.37004202855576, 0.104775693491955, "<unicode> <unicode> A<unicode><unicode><unicode>B<unicode><unicode><unicode>C",
                                      81.2550819389151, 325.02032775566, 4, "", "", "Error", "", 2358.62895110231,
                                      11, "", "", "Total"))
})

test_that("9.2 Four center points Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 103.146956850833, "", 2.42004650296181e-06, 2.60216387420987,
                                      "(Intercept)", 39.6389166236324, "", "A", 6.57190309125001,
                                      13.1438061825, 0.108193899336995, 3.18698685945901, "A", 2.06210548742758,
                                      1, "B", -4.23777694375, -8.47555388750001, 0.254373371146824,
                                      3.18698685945901, "B", -1.32971271317679, 1, "C", 5.49572971125001,
                                      10.9914594225, 0.159720253244519, 3.18698685945901, "C", 1.72442810516731,
                                      1, "AB", -1.69237858125, -3.38475716250001, 0.623516478002212,
                                      3.18698685945901, "A<unicode>B", -0.531027787650586, 1, "BC",
                                      10.27597092875, 20.5519418575, 0.0321473359349366, 3.18698685945901,
                                      "B<unicode>C", 3.22435309020833, 1, "AC", 3.16285156375001,
                                      6.32570312750002, 0.377164601363556, 3.18698685945901, "A<unicode>C",
                                      0.992426923368898, 1, "ABC", 6.66228304624999, 13.3245660925,
                                      0.104775693491955, 3.18698685945901, "A<unicode>B<unicode>C",
                                      2.09046454850489, 1))
})

test_that("9.3 Four center points Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 103.15 (Intercept) + 6.57 A - 4.24 B + 5.5 C - 1.69 AB + 10.28 BC + 3.16 AC + 6.66 ABC"
                                 ))
})

test_that("9.4 Four center points Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.621048532915556, 0.206890168987148, 0.862199466514747, 9.01416007950353
                                 ))
})

### Two blocks (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$continuousFactorsFactorial <- c("A", "B", "C")
options$blocksFactorial <- "Blocks"
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "A"),
  list(components = "B"),
  list(components = "C"),
  list(components = c("A", "B")),
  list(components = c("B", "C")),
  list(components = c("A", "C")),
  list(components = c("A", "B", "C"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/twoBlocksFactorial.csv", options)

test_that("10.1 Two blocks ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(38.3143421461033, 268.200395022723, 7, "", "", "Model", 57.2971559169046,
                                      57.2971559169046, 1, "", "", "<unicode> Block", "", 46.035882096092,
                                      3, "", "", "<unicode> Linear terms", 7.05073541235602, 7.05073541235602,
                                      1, "", "", "<unicode> <unicode> A", 3.58135310939011, 3.58135310939011,
                                      1, "", "", "<unicode> <unicode> B", 35.4037935743459, 35.4037935743459,
                                      1, "", "", "<unicode> <unicode> C", "", 164.867357009726, 3,
                                      "", "", "<unicode> Interaction terms", 0.313793656145354, 0.313793656145354,
                                      1, "", "", "<unicode> <unicode> A<unicode><unicode><unicode>B",
                                      0.732622024251352, 0.732622024251352, 1, "", "", "<unicode> <unicode> B<unicode><unicode><unicode>C",
                                      163.82094132933, 163.82094132933, 1, "", "", "<unicode> <unicode> A<unicode><unicode><unicode>C",
                                      0, 0, 0, "", "", "Error", "", 268.200395022723, 7, "", "", "Total"
                                 ))
})

test_that("10.2 Two blocks Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 99.75068923625, "", "", "", "(Intercept)", "",
                                      "", "A", 0.93879812875, 1.8775962575, "", "", "A", "", "NaN",
                                      "B", 0.669080816249997, 1.33816163249999, "", "", "B", "", "NaN",
                                      "C", -2.10368110625, -4.2073622125, "", "", "C", "", "NaN",
                                      "BLK", -2.67621831875, "", "", "", "Block", "", "NaN", "AB",
                                      -0.198051021250002, -0.396102042500004, "", "", "A<unicode>B",
                                      "", "NaN", "BC", 0.302618163749995, 0.605236327499991, "", "",
                                      "B<unicode>C", "", "NaN", "AC", -4.52522017875, -9.0504403575,
                                      "", "", "A<unicode>C", "", "NaN"))
})

test_that("10.3 Two blocks Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 99.75 (Intercept) + 0.94 A + 0.67 B - 2.1 C - 2.68 BLK - 0.2 AB + 0.3 BC - 4.53 AC"
                                 ))
})

test_that("10.4 Two blocks Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", "", 1, ""))
})

### Four blocks (verified with Minitab) ####
#
# options <- analysisOptions("doeAnalysis")
# options$dependentFactorial <- "Result"
# options$continuousFactorsFactorial <- c("A", "B", "C")
# options$blocksFactorial <- "Blocks"
# options$codeFactors <- TRUE
# options$codeFactorsMethod <- "automatic"
# options$tableEquation <- TRUE
# options$tableAlias <- TRUE
# options$highestOrder <- FALSE
# options$histogramBinWidthType <- "doane"
# options$modelTerms <- list(
#   list(components = "A"),
#   list(components = "B"),
#   list(components = "C"),
#   list(components = c("A", "B")),
#   list(components = c("B", "C")),
#   list(components = c("A", "C")),
#   list(components = c("A", "B", "C"))
# )
# set.seed(123)
# results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/fourBlocksFactorial.csv", options, makeTests = T)

### Eight blocks (verified with Minitab) ####
#
# options <- analysisOptions("doeAnalysis")
# options$dependentFactorial <- "Result"
# options$continuousFactorsFactorial <- c("A", "B", "C", "D")
# options$blocksFactorial <- "Blocks"
# options$codeFactors <- TRUE
# options$codeFactorsMethod <- "automatic"
# options$tableEquation <- TRUE
# options$tableAlias <- TRUE
# options$highestOrder <- FALSE
# options$histogramBinWidthType <- "doane"
# options$modelTerms <- list(
#   list(components = "A"),
#   list(components = "B"),
#   list(components = "C"),
#   list(components = "D"),
#   list(components = c("A", "B")),
#   list(components = c("A", "C")),
#   list(components = c("A", "D")),
#   list(components = c("B", "C")),
#   list(components = c("B", "D")),
#   list(components = c("D", "C"))
# )
# set.seed(123)
# results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/eightBlocksFactorial.csv", options)


### Two factor one HTC (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$fixedFactorsFactorial <- c("AHTC", "B")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "AHTC"),
  list(components = "B"),
  list(components = c("AHTC", "B"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/2fac1htc.csv", options)

test_that("13.1 Two factor one HTC ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(107.302682689122, 321.908048067365, 3, "", "", "Model", "", 276.632034149505,
                                      2, "", "", "<unicode> Linear terms", 2.95020693918108, 2.95020693918108,
                                      1, "", "", "<unicode> <unicode> AHTC", 273.681827210324, 273.681827210324,
                                      1, "", "", "<unicode> <unicode> B", "", 45.2760139178593, 1,
                                      "", "", "<unicode> Interaction terms", 45.2760139178593, 45.2760139178593,
                                      1, "", "", "<unicode> <unicode> AHTC<unicode><unicode><unicode>B",
                                      0, 0, 0, "", "", "Error", "", 321.908048067365, 3, "", "", "Total"
                                 ))
})

test_that("13.2 Two factor one HTC Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 101.7083684475, "", "", "", "(Intercept)", "",
                                      "", "A", -0.858808322500003, -1.71761664500001, "", "", "AHTC",
                                      "", "NaN", "B", 8.2716659025, 16.543331805, "", "", "B", "",
                                      "NaN", "AB", 3.3643726725, 6.72874534500001, "", "", "AHTC<unicode>B",
                                      "", "NaN"))
})

test_that("13.3 Two factor one HTC Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 101.71 (Intercept) - 0.86 A + 8.27 B + 3.36 AB"))
})

test_that("13.4 Two factor one HTC Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", "", 1, ""))
})

### Five factor two HTC (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$fixedFactorsFactorial <- c("AHTC", "BHTC", "C", "D", "E")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "AHTC"),
  list(components = "BHTC"),
  list(components = "C"),
  list(components = "D"),
  list(components = "E"),
  list(components = c("AHTC", "BHTC")),
  list(components = c("AHTC", "C")),
  list(components = c("AHTC", "D")),
  list(components = c("AHTC", "E")),
  list(components = c("BHTC", "C")),
  list(components = c("BHTC", "D")),
  list(components = c("BHTC", "E")),
  list(components = c("C", "D")),
  list(components = c("C", "E")),
  list(components = c("D", "E"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/5fac2htc.csv", options)


test_that("14.1 Five factor two HTC ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(174.318753634873, 2614.78130452309, 15, 2.61163525622296, 0.0329124356311262,
                                      "Model", "", 400.259395435181, 5, "", "", "<unicode> Linear terms",
                                      140.575248415804, 140.575248415804, 1, 2.10609166976954, 0.166037988636009,
                                      "<unicode> <unicode> AHTC", 59.7059800632574, 59.7059800632574,
                                      1, 0.894512146794939, 0.358322866809719, "<unicode> <unicode> BHTC",
                                      9.20223619766671, 9.20223619766671, 1, 0.13786746399218, 0.715281146799883,
                                      "<unicode> <unicode> C", 142.938638721329, 142.938638721329,
                                      1, 2.14149987065109, 0.162729783343212, "<unicode> <unicode> D",
                                      47.8372920371246, 47.8372920371246, 1, 0.71669602863312, 0.409713838637603,
                                      "<unicode> <unicode> E", "", 2214.52190908791, 10, "", "", "<unicode> Interaction terms",
                                      226.337356590491, 226.337356590491, 1, 3.39097548569085, 0.084170364100319,
                                      "<unicode> <unicode> AHTC<unicode><unicode><unicode>BHTC", 400.163215336672,
                                      400.163215336672, 1, 5.99522621419044, 0.0262506427209202, "<unicode> <unicode> AHTC<unicode><unicode><unicode>C",
                                      47.8156888202629, 47.8156888202629, 1, 0.716372370268043, 0.409817322777284,
                                      "<unicode> <unicode> AHTC<unicode><unicode><unicode>D", 1205.68940827039,
                                      1205.68940827039, 1, 18.0635812328549, 0.000611029450159505,
                                      "<unicode> <unicode> AHTC<unicode><unicode><unicode>E", 40.6674201825126,
                                      40.6674201825126, 1, 0.609277350334588, 0.446455218618594, "<unicode> <unicode> BHTC<unicode><unicode><unicode>C",
                                      162.054036140275, 162.054036140275, 1, 2.4278858434455, 0.138751650475217,
                                      "<unicode> <unicode> BHTC<unicode><unicode><unicode>D", 7.32398807648315,
                                      7.32398807648315, 1, 0.109727640187037, 0.744753292512754, "<unicode> <unicode> BHTC<unicode><unicode><unicode>E",
                                      78.8170157159959, 78.8170157159959, 1, 1.18083277181598, 0.293286875775025,
                                      "<unicode> <unicode> C<unicode><unicode><unicode>D", 9.34031650760153,
                                      9.34031650760153, 1, 0.139936176612575, 0.713255474631006, "<unicode> <unicode> C<unicode><unicode><unicode>E",
                                      36.3134634472267, 36.3134634472267, 1, 0.544046578103623, 0.471440264802726,
                                      "<unicode> <unicode> D<unicode><unicode><unicode>E", 66.7469751832722,
                                      1067.95160293236, 16, "", "", "Error", "", 3682.73290745545,
                                      31, "", "", "Total"))
})

test_that("14.2 Five factor two HTC Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 100.472419629687, "", 2.73357601650784e-21, 1.44424477651029,
                                      "(Intercept)", 69.5674453969345, "", "A", -2.0959428696875,
                                      -4.19188573937499, 0.16603798863601, 1.44424477651029, "AHTC",
                                      -1.45123797833765, 1, "B", -1.3659472453125, -2.73189449062499,
                                      0.358322866809719, 1.44424477651029, "BHTC", -0.945786522844844,
                                      1, "C", -0.536255425312495, -1.07251085062499, 0.715281146799883,
                                      1.44424477651029, "C", -0.371305082098509, 1, "D", -2.11348822093749,
                                      -4.22697644187499, 0.162729783343212, 1.44424477651029, "D",
                                      -1.46338643927402, 1, "E", -1.2226673203125, -2.445334640625,
                                      0.409713838637603, 1.44424477651029, "E", -0.846579014996899,
                                      1, "AB", -2.6595192034375, -5.31903840687501, 0.084170364100319,
                                      1.44424477651029, "AHTC<unicode>BHTC", -1.84146015044878, 1,
                                      "AC", 3.53625514906249, 7.07251029812499, 0.0262506427209202,
                                      1.44424477651029, "AHTC<unicode>C", 2.44851510393349, 1, "AD",
                                      1.2223912121875, 2.44478242437499, 0.409817322777284, 1.44424477651029,
                                      "AHTC<unicode>D", 0.846387836791174, 1, "AE", -6.1382240109375,
                                      -12.276448021875, 0.000611029450159505, 1.44424477651029, "AHTC<unicode>E",
                                      -4.25012720196172, 1, "BC", 1.12732288218749, 2.25464576437499,
                                      0.446455218618594, 1.44424477651029, "BHTC<unicode>C", 0.780562201451357,
                                      1, "BD", -2.25037521968751, -4.50075043937501, 0.138751650475217,
                                      1.44424477651029, "BHTC<unicode>D", -1.55816746322258, 1, "BE",
                                      -0.478408431562504, -0.956816863125009, 0.744753292512756, 1.44424477651029,
                                      "BHTC<unicode>E", -0.331251626693417, 1, "CD", 1.5694049003125,
                                      3.13880980062499, 0.293286875775025, 1.44424477651029, "C<unicode>D",
                                      1.08666129581208, 1, "CE", 0.540263723437495, 1.08052744687499,
                                      0.713255474631007, 1.44424477651029, "C<unicode>E", 0.37408044136599,
                                      1, "DE", -1.0652679159375, -2.13053583187501, 0.471440264802726,
                                      1.44424477651029, "D<unicode>E", -0.737595131561769, 1))
})

test_that("14.3 Five factor two HTC Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 100.47 (Intercept) - 2.1 A - 1.37 B - 0.54 C - 2.11 D - 1.22 E - 2.66 AB + 3.54 AC + 1.22 AD - 6.14 AE + 1.13 BC - 2.25 BD - 0.48 BE + 1.57 CD + 0.54 CE - 1.07 DE"
                                 ))
})

test_that("14.4 Five factor two HTC Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.43814653881291, 0, 0.710011116806663, 8.1698822013094))
})

### Seven factor three HTC (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$fixedFactorsFactorial <- c("AHTC", "BHTC", "CHTC", "D", "E", "F", "G")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "AHTC"),
  list(components = "BHTC"),
  list(components = "CHTC"),
  list(components = "D"),
  list(components = "E"),
  list(components = "F"),
  list(components = "G"),
  list(components = c("AHTC", "BHTC")),
  list(components = c("AHTC", "CHTC")),
  list(components = c("AHTC", "D")),
  list(components = c("AHTC", "E")),
  list(components = c("AHTC", "F")),
  list(components = c("AHTC", "G")),
  list(components = c("BHTC", "CHTC")),
  list(components = c("BHTC", "D")),
  list(components = c("BHTC", "E")),
  list(components = c("BHTC", "F")),
  list(components = c("BHTC", "G")),
  list(components = c("CHTC", "D")),
  list(components = c("CHTC", "E")),
  list(components = c("CHTC", "F")),
  list(components = c("CHTC", "G")),
  list(components = c("D", "E")),
  list(components = c("D", "F")),
  list(components = c("D", "G")),
  list(components = c("E", "F")),
  list(components = c("E", "G")),
  list(components = c("G", "F"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/7fac3htc.csv", options)

test_that("15.1 Seven factor three HTC ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(119.557000580297, 3347.59601624831, 28, 1.37785805113187, 0.126907369221792,
                                      "Model", "", 1133.66575048578, 7, "", "", "<unicode> Linear terms",
                                      43.2280629758079, 43.2280629758079, 1, 0.498190271727746, 0.481954010338246,
                                      "<unicode> <unicode> AHTC", 645.182487456781, 645.182487456781,
                                      1, 7.435531842358, 0.00756508861305575, "<unicode> <unicode> BHTC",
                                      13.9958546745165, 13.9958546745165, 1, 0.161297966260056, 0.688829967480027,
                                      "<unicode> <unicode> CHTC", 156.603169263384, 156.603169263384,
                                      1, 1.80480387225339, 0.182204168026775, "<unicode> <unicode> D",
                                      78.6259842597319, 78.6259842597319, 1, 0.906140543126782, 0.343458820294831,
                                      "<unicode> <unicode> E", 99.5231643487423, 99.5231643487423,
                                      1, 1.14697418475245, 0.286787456196657, "<unicode> <unicode> F",
                                      96.5070275068138, 96.5070275068138, 1, 1.11221412544354, 0.294168158465975,
                                      "<unicode> <unicode> G", "", 2213.93026576254, 21, "", "", "<unicode> Interaction terms",
                                      60.9543768511048, 60.9543768511048, 1, 0.702480645118006, 0.403970431247995,
                                      "<unicode> <unicode> AHTC<unicode><unicode><unicode>BHTC", 19.5366623670943,
                                      19.5366623670943, 1, 0.225154088878858, 0.636185509275199, "<unicode> <unicode> AHTC<unicode><unicode><unicode>CHTC",
                                      54.6018887973059, 54.6018887973059, 1, 0.629270153326124, 0.429520256557514,
                                      "<unicode> <unicode> AHTC<unicode><unicode><unicode>D", 237.930922582267,
                                      237.930922582267, 1, 2.74208148165301, 0.100903870838271, "<unicode> <unicode> AHTC<unicode><unicode><unicode>E",
                                      76.8698345525518, 76.8698345525518, 1, 0.885901452138497, 0.348881431660969,
                                      "<unicode> <unicode> AHTC<unicode><unicode><unicode>F", 72.2742046291169,
                                      72.2742046291169, 1, 0.832938215696524, 0.363641458096223, "<unicode> <unicode> AHTC<unicode><unicode><unicode>G",
                                      143.384124940379, 143.384124940379, 1, 1.65245840891527, 0.201623601342148,
                                      "<unicode> <unicode> BHTC<unicode><unicode><unicode>CHTC", 164.07745002545,
                                      164.07745002545, 1, 1.89094268365253, 0.172199610620702, "<unicode> <unicode> BHTC<unicode><unicode><unicode>D",
                                      117.764742574936, 117.764742574936, 1, 1.35720282299462, 0.246822287959285,
                                      "<unicode> <unicode> BHTC<unicode><unicode><unicode>E", 77.3576531789622,
                                      77.3576531789622, 1, 0.891523413367277, 0.347363470758474, "<unicode> <unicode> BHTC<unicode><unicode><unicode>F",
                                      67.6280158091849, 67.6280158091849, 1, 0.779392303357226, 0.379465752347437,
                                      "<unicode> <unicode> BHTC<unicode><unicode><unicode>G", 63.8321524757666,
                                      63.8321524757666, 1, 0.735646133500497, 0.393130806567667, "<unicode> <unicode> CHTC<unicode><unicode><unicode>D",
                                      64.9335315793323, 64.9335315793323, 1, 0.748339192525319, 0.389093941112954,
                                      "<unicode> <unicode> CHTC<unicode><unicode><unicode>E", 128.810834736743,
                                      128.810834736743, 1, 1.48450567389266, 0.2259664858836, "<unicode> <unicode> CHTC<unicode><unicode><unicode>F",
                                      37.9690246270166, 37.9690246270166, 1, 0.437581455055177, 0.509828780952428,
                                      "<unicode> <unicode> CHTC<unicode><unicode><unicode>G", 38.6528283427324,
                                      38.6528283427324, 1, 0.445462084800987, 0.506050660681576, "<unicode> <unicode> D<unicode><unicode><unicode>E",
                                      141.901609729595, 141.901609729595, 1, 1.63537287223244, 0.203951103609661,
                                      "<unicode> <unicode> D<unicode><unicode><unicode>F", 20.2719126550819,
                                      20.2719126550819, 1, 0.233627624715184, 0.629914243961655, "<unicode> <unicode> D<unicode><unicode><unicode>G",
                                      27.6806780963416, 27.6806780963416, 1, 0.31901139197799, 0.573479972639345,
                                      "<unicode> <unicode> E<unicode><unicode><unicode>F", 369.15956704774,
                                      369.15956704774, 1, 4.2544516769427, 0.0417659917866407, "<unicode> <unicode> E<unicode><unicode><unicode>G",
                                      228.338250163832, 228.338250163832, 1, 2.63152885102947, 0.107941266462288,
                                      "<unicode> <unicode> F<unicode><unicode><unicode>G", 86.7701868723592,
                                      8590.24850036356, 99, "", "", "Error", "", 11937.8445166119,
                                      127, "", "", "Total"))
})

test_that("15.2 Seven factor three HTC Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 98.6847437647656, "", 5.65573938672141e-109, 0.823342021847729,
                                      "(Intercept)", 119.85874781819, "", "A", 0.581136164765628,
                                      1.16227232953126, 0.481954010338243, 0.823342021847729, "AHTC",
                                      0.705825950024333, 1, "B", 2.24510315648437, 4.49020631296875,
                                      0.00756508861305579, 0.823342021847729, "BHTC", 2.72681716335327,
                                      1, "C", 0.330669948203123, 0.661339896406246, 0.688829967480033,
                                      0.823342021847729, "CHTC", 0.40161918064262, 1, "D", 1.10610228273437,
                                      2.21220456546875, 0.182204168026775, 0.823342021847729, "D",
                                      1.3434298910823, 1, "E", 0.783750918359373, 1.56750183671875,
                                      0.343458820294829, 0.823342021847729, "E", 0.951914146930693,
                                      1, "F", -0.881773622578122, -1.76354724515624, 0.286787456196658,
                                      0.823342021847729, "F", -1.07096880661971, 1, "G", -0.868309364453123,
                                      -1.73661872890625, 0.294168158465974, 0.823342021847729, "G",
                                      -1.05461562924297, 1, "AB", 0.690076857421878, 1.38015371484376,
                                      0.403970431247996, 0.823342021847729, "AHTC<unicode>BHTC", 0.838141184477891,
                                      1, "AC", -0.390679119921874, -0.781358239843747, 0.636185509275193,
                                      0.823342021847729, "AHTC<unicode>CHTC", -0.474504045165967,
                                      1, "AD", 0.653128820546878, 1.30625764109376, 0.429520256557512,
                                      0.823342021847729, "AHTC<unicode>D", 0.793265499896554, 1, "AE",
                                      -1.36339111507812, -2.72678223015625, 0.10090387083827, 0.823342021847729,
                                      "AHTC<unicode>E", -1.65592315088986, 1, "AF", 0.774948761171871,
                                      1.54989752234374, 0.348881431660967, 0.823342021847729, "AHTC<unicode>F",
                                      0.941223380573659, 1, "AG", -0.751426791953129, -1.50285358390626,
                                      0.363641458096224, 0.823342021847729, "AHTC<unicode>G", -0.912654488673845,
                                      1, "BC", -1.05838956726563, -2.11677913453125, 0.201623601342147,
                                      0.823342021847729, "BHTC<unicode>CHTC", -1.28547983605939, 1,
                                      "BD", -1.13219038960938, -2.26438077921875, 0.172199610620701,
                                      0.823342021847729, "BHTC<unicode>D", -1.37511551647581, 1, "BE",
                                      0.959185618828122, 1.91837123765624, 0.246822287959286, 0.823342021847729,
                                      "BHTC<unicode>E", 1.16499048193306, 1, "BF", 0.777403798203127,
                                      1.55480759640625, 0.347363470758471, 0.823342021847729, "BHTC<unicode>F",
                                      0.944205175460973, 1, "BG", 0.726872666640628, 1.45374533328126,
                                      0.379465752347437, 0.823342021847729, "BHTC<unicode>G", 0.882831979120164,
                                      1, "CD", 0.706178937109373, 1.41235787421875, 0.393130806567668,
                                      0.823342021847729, "CHTC<unicode>D", 0.857698159902709, 1, "CE",
                                      0.712245193359374, 1.42449038671875, 0.389093941112955, 0.823342021847729,
                                      "CHTC<unicode>E", 0.865066004721787, 1, "CF", 1.00316232304688,
                                      2.00632464609375, 0.2259664858836, 0.823342021847729, "CHTC<unicode>F",
                                      1.21840291935495, 1, "CG", 0.544640252734377, 1.08928050546875,
                                      0.509828780952428, 0.823342021847729, "CHTC<unicode>G", 0.661499399134403,
                                      1, "DE", -0.549522721484377, -1.09904544296875, 0.506050660681577,
                                      0.823342021847729, "D<unicode>E", -0.6674294605432, 1, "DF",
                                      1.05290375914063, 2.10580751828125, 0.203951103609661, 0.823342021847729,
                                      "D<unicode>F", 1.27881698152333, 1, "DG", 0.397962708828127,
                                      0.795925417656253, 0.629914243961662, 0.823342021847729, "D<unicode>G",
                                      0.483350416070136, 1, "EF", 0.465032576953128, 0.930065153906256,
                                      0.573479972639346, 0.823342021847729, "E<unicode>F", 0.564810934718855,
                                      1, "EG", -1.69825178273437, -3.39650356546874, 0.0417659917866405,
                                      0.823342021847729, "E<unicode>G", -2.06263222047526, 1, "FG",
                                      -1.33562441554688, -2.67124883109375, 0.107941266462288, 0.823342021847729,
                                      "F<unicode>G", -1.62219877050547, 1))
})

test_that("15.3 Seven factor three HTC Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 98.68 (Intercept) + 0.58 A + 2.25 B + 0.33 C + 1.11 D + 0.78 E - 0.88 F - 0.87 G + 0.69 AB - 0.39 AC + 0.65 AD - 1.36 AE + 0.77 AF - 0.75 AG - 1.06 BC - 1.13 BD + 0.96 BE + 0.78 BF + 0.73 BG + 0.71 CD + 0.71 CE + 1 CF + 0.54 CG - 0.55 DE + 1.05 DF + 0.4 DG + 0.47 EF - 1.7 EG - 1.34 FG"
                                 ))
})

test_that("15.4 Seven factor three HTC Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.076900882947905, 0, 0.280418798518446, 9.31505163014995))
})

### Three factors three levels (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$fixedFactorsFactorial <- c("A", "B", "C")
options$codeFactors <- FALSE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "A"),
  list(components = "B"),
  list(components = "C"),
  list(components = c("A", "B")),
  list(components = c("B", "C")),
  list(components = c("A", "C")),
  list(components = c("A", "B", "C"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/3level3facFull.csv", options)

test_that("16.1 Three factors three levels ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(103.872530627319, 2700.68579631029, 26, "", "", "Model", "", 350.578267591584,
                                      6, "", "", "<unicode> Linear terms", 132.136684132621, 264.273368265242,
                                      2, "", "", "<unicode> <unicode> A", 5.81490099225339, 11.6298019845068,
                                      2, "", "", "<unicode> <unicode> B", 37.3375486709174, 74.6750973418348,
                                      2, "", "", "<unicode> <unicode> C", "", 2350.1075287187, 20,
                                      "", "", "<unicode> Interaction terms", 61.064862061605, 244.25944824642,
                                      4, "", "", "<unicode> <unicode> A<unicode><unicode><unicode>B",
                                      89.7949908715026, 359.17996348601, 4, "", "", "<unicode> <unicode> B<unicode><unicode><unicode>C",
                                      151.309870553013, 605.239482212051, 4, "", "", "<unicode> <unicode> A<unicode><unicode><unicode>C",
                                      142.678579346778, 1141.42863477422, 8, "", "", "<unicode> <unicode> A<unicode><unicode><unicode>B<unicode><unicode><unicode>C",
                                      0, 0, 0, "", "", "Error", "", 2700.68579631029, 26, "", "",
                                      "Total"))
})

test_that("16.2 Three factors three levels Uncoded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", 99.4256253577778, "", "", "", "(Intercept)", "", "", "A1",
                                      -2.08298983111111, -6.24896949333332, "", "", "A1", "", "NaN",
                                      "A2", -2.33899599777778, -7.01698799333335, "", "", "A2", "",
                                      "NaN", "B1", 0.896028588888884, 2.68808576666665, "", "", "B1",
                                      "", "NaN", "B2", -0.657656098888886, -1.97296829666666, "",
                                      "", "B2", "", "NaN", "C1", -1.62829194888889, -4.88487584666667,
                                      "", "", "C1", "", "NaN", "C2", 2.28387911888889, 6.85163735666666,
                                      "", "", "C2", "", "NaN", "A1B1", 5.98538610111111, 17.9561583033333,
                                      "", "", "A1<unicode>B1", "", "NaN", "A2B1", -3.02668897888889,
                                      -9.08006693666668, "", "", "A2<unicode>B1", "", "NaN", "A1B2",
                                      -3.18827553444444, -9.56482660333332, "", "", "A1<unicode>B2",
                                      "", "NaN", "A2B2", 2.02817473888889, 6.08452421666666, "", "",
                                      "A2<unicode>B2", "", "NaN", "B1C1", -3.47706119777778, -10.4311835933333,
                                      "", "", "B1<unicode>C1", "", "NaN", "B2C1", -1.85954645333333,
                                      -5.57863935999999, "", "", "B2<unicode>C1", "", "NaN", "B1C2",
                                      -2.47341247888889, -7.42023743666667, "", "", "B1<unicode>C2",
                                      "", "NaN", "B2C2", 2.69501473555556, 8.08504420666667, "", "",
                                      "B2<unicode>C2", "", "NaN", "A1C1", -4.79232923777778, -14.3769877133333,
                                      "", "", "A1<unicode>C1", "", "NaN", "A2C1", -2.59365589111111,
                                      -7.78096767333333, "", "", "A2<unicode>C1", "", "NaN", "A1C2",
                                      6.32266759111111, 18.9680027733333, "", "", "A1<unicode>C2",
                                      "", "NaN", "A2C2", -3.36479560555556, -10.0943868166667, "",
                                      "", "A2<unicode>C2", "", "NaN", "A1B1C1", -2.43673828222222,
                                      -7.31021484666666, "", "", "A1<unicode>B1<unicode>C1", "", "NaN",
                                      "A2B1C1", 4.19618861777778, 12.5885658533333, "", "", "A2<unicode>B1<unicode>C1",
                                      "", "NaN", "A1B2C1", -3.80741124333334, -11.42223373, "", "",
                                      "A1<unicode>B2<unicode>C1", "", "NaN", "A2B2C1", -2.03393254666666,
                                      -6.10179763999999, "", "", "A2<unicode>B2<unicode>C1", "", "NaN",
                                      "A1B1C2", -3.62274334777778, -10.8682300433334, "", "", "A1<unicode>B1<unicode>C2",
                                      "", "NaN", "A2B1C2", 7.78068559555556, 23.3420567866667, "",
                                      "", "A2<unicode>B1<unicode>C2", "", "NaN", "A1B2C2", 13.3645392611111,
                                      40.0936177833334, "", "", "A1<unicode>B2<unicode>C2", "", "NaN",
                                      "A2B2C2", -7.90977640888889, -23.7293292266667, "", "", "A2<unicode>B2<unicode>C2",
                                      "", "NaN"))
})

test_that("16.3 Three factors three levels Discrete Predictor Levels table results match", {
  table <- results[["results"]][["tableCoefficientsLegend"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2, "A1", 3, "A2", 2, "B1", 3, "B2", 2, "C1", 3, "C2"))
})

test_that("16.4 Three factors three levels Regression Equation in Uncoded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 99.43  - 2.08 A1 - 2.34 A2 + 0.9 B1 - 0.66 B2 - 1.63 C1 + 2.28 C2 + 5.99 A1B1 - 3.03 A2B1 - 3.19 A1B2 + 2.03 A2B2 - 3.48 B1C1 - 1.86 B2C1 - 2.47 B1C2 + 2.7 B2C2 - 4.79 A1C1 - 2.59 A2C1 + 6.32 A1C2 - 3.36 A2C2 - 2.44 A1B1C1 + 4.2 A2B1C1 - 3.81 A1B2C1 - 2.03 A2B2C1 - 3.62 A1B1C2 + 7.78 A2B1C2 + 13.36 A1B2C2 - 7.91 A2B2C2"
                                 ))
})

test_that("16.5 Three factors three levelsModel Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", "", 1, ""))
})

### Three factors 2*three and 1*four levels (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$fixedFactorsFactorial <- c("A", "B", "C")
options$codeFactors <- FALSE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "A"),
  list(components = "B"),
  list(components = "C"),
  list(components = c("A", "B")),
  list(components = c("B", "C")),
  list(components = c("A", "C")),
  list(components = c("A", "B", "C"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/4level3facFull.csv", options)

test_that("17.1 Three factors 2*three and 1*four levels ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(131.504590854318, 4602.66067990112, 35, "", "", "Model", "", 277.059734986186,
                                      7, "", "", "<unicode> Linear terms", 61.7886687517631, 185.366006255289,
                                      3, "", "", "<unicode> <unicode> A", 18.1794771188365, 36.3589542376731,
                                      2, "", "", "<unicode> <unicode> B", 27.667387246612, 55.3347744932241,
                                      2, "", "", "<unicode> <unicode> C", "", 4325.60094491493, 28,
                                      "", "", "<unicode> Interaction terms", 43.3401743664158, 260.041046198495,
                                      6, "", "", "<unicode> <unicode> A<unicode><unicode><unicode>B",
                                      115.90088619687, 463.603544787479, 4, "", "", "<unicode> <unicode> B<unicode><unicode><unicode>C",
                                      112.319199183442, 673.915195100651, 6, "", "", "<unicode> <unicode> A<unicode><unicode><unicode>C",
                                      244.003429902359, 2928.04115882831, 12, "", "", "<unicode> <unicode> A<unicode><unicode><unicode>B<unicode><unicode><unicode>C",
                                      0, 0, 0, "", "", "Error", "", 4602.66067990112, 35, "", "",
                                      "Total"))
})

test_that("17.2 Three factors 2*three and 1*four levels Uncoded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", 102.659950219167, "", "", "", "(Intercept)", "", "", "A1",
                                      0.608225378611117, 2.43290151444447, "", "", "A1", "", "NaN",
                                      "A2", -2.10790445361111, -8.43161781444446, "", "", "A2", "",
                                      "NaN", "A3", 3.45709875638889, 13.8283950255555, "", "", "A3",
                                      "", "NaN", "B1", 1.05405196000001, 3.16215588000002, "", "",
                                      "B1", "", "NaN", "B2", 0.298617572499999, 0.895852717499996,
                                      "", "", "B2", "", "NaN", "C1", -1.74051705666666, -5.22155116999999,
                                      "", "", "C1", "", "NaN", "C2", 1.0534684775, 3.1604054325, "",
                                      "", "C2", "", "NaN", "A1B1", -2.44136849444444, -9.76547397777776,
                                      "", "", "A1<unicode>B1", "", "NaN", "A2B1", -3.17944767222222,
                                      -12.7177906888889, "", "", "A2<unicode>B1", "", "NaN", "A3B1",
                                      0.814238331111104, 3.25695332444442, "", "", "A3<unicode>B1",
                                      "", "NaN", "A1B2", 0.0214516297222197, 0.085806518888879, "",
                                      "", "A1<unicode>B2", "", "NaN", "A2B2", 4.01435786194444, 16.0574314477778,
                                      "", "", "A2<unicode>B2", "", "NaN", "A3B2", -2.66235668805555,
                                      -10.6494267522222, "", "", "A3<unicode>B2", "", "NaN", "B1C1",
                                      1.2290394675, 3.68711840250001, "", "", "B1<unicode>C1", "",
                                      "NaN", "B2C1", 0.807115629999998, 2.42134688999999, "", "",
                                      "B2<unicode>C1", "", "NaN", "B1C2", -5.46931025916667, -16.4079307775,
                                      "", "", "B1<unicode>C2", "", "NaN", "B2C2", 5.01265248083334,
                                      15.0379574425, "", "", "B2<unicode>C2", "", "NaN", "A1C1", -0.263536797777762,
                                      -1.05414719111105, "", "", "A1<unicode>C1", "", "NaN", "A2C1",
                                      -3.91930759888889, -15.6772303955556, "", "", "A2<unicode>C1",
                                      "", "NaN", "A3C1", 2.95523971111111, 11.8209588444444, "", "",
                                      "A3<unicode>C1", "", "NaN", "A1C2", 7.54829102138888, 30.1931640855555,
                                      "", "", "A1<unicode>C2", "", "NaN", "A2C2", -2.35092072305555,
                                      -9.4036828922222, "", "", "A2<unicode>C2", "", "NaN", "A3C2",
                                      -5.73768854972222, -22.9507541988889, "", "", "A3<unicode>C2",
                                      "", "NaN", "A1B1C1", 12.7148384236111, 50.8593536944446, "",
                                      "", "A1<unicode>B1<unicode>C1", "", "NaN", "A2B1C1", -13.8793367052778,
                                      -55.5173468211112, "", "", "A2<unicode>B1<unicode>C1", "", "NaN",
                                      "A3B1C1", -2.95340518861112, -11.8136207544445, "", "", "A3<unicode>B1<unicode>C1",
                                      "", "NaN", "A1B2C1", -17.5550878455556, -70.2203513822223, "",
                                      "", "A1<unicode>B2<unicode>C1", "", "NaN", "A2B2C1", 6.87543332555556,
                                      27.5017333022222, "", "", "A2<unicode>B2<unicode>C1", "", "NaN",
                                      "A3B2C1", 14.5556210555556, 58.2224842222222, "", "", "A3<unicode>B2<unicode>C1",
                                      "", "NaN", "A1B1C2", -20.5366983130556, -82.1467932522223, "",
                                      "", "A1<unicode>B1<unicode>C2", "", "NaN", "A2B1C2", 8.9325174513889,
                                      35.7300698055556, "", "", "A2<unicode>B1<unicode>C2", "", "NaN",
                                      "A3B1C2", 11.5863058647222, 46.3452234588889, "", "", "A3<unicode>B1<unicode>C2",
                                      "", "NaN", "A1B2C2", 13.1013852202778, 52.4055408811111, "",
                                      "", "A1<unicode>B2<unicode>C2", "", "NaN", "A2B2C2", -8.36482153527778,
                                      -33.4592861411111, "", "", "A2<unicode>B2<unicode>C2", "", "NaN",
                                      "A3B2C2", -6.31234586861112, -25.2493834744445, "", "", "A3<unicode>B2<unicode>C2",
                                      "", "NaN"))
})

test_that("17.3 Three factors 2*three and 1*four levels Discrete Predictor Levels table results match", {
  table <- results[["results"]][["tableCoefficientsLegend"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2, "A1", 3, "A2", 4, "A3", 2, "B1", 3, "B2", 2, "C1", 3, "C2"
                                 ))
})

test_that("17.4 Three factors 2*three and 1*four levels Regression Equation in Uncoded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 102.66  + 0.61 A1 - 2.11 A2 + 3.46 A3 + 1.05 B1 + 0.3 B2 - 1.74 C1 + 1.05 C2 - 2.44 A1B1 - 3.18 A2B1 + 0.81 A3B1 + 0.02 A1B2 + 4.01 A2B2 - 2.66 A3B2 + 1.23 B1C1 + 0.81 B2C1 - 5.47 B1C2 + 5.01 B2C2 - 0.26 A1C1 - 3.92 A2C1 + 2.96 A3C1 + 7.55 A1C2 - 2.35 A2C2 - 5.74 A3C2 + 12.71 A1B1C1 - 13.88 A2B1C1 - 2.95 A3B1C1 - 17.56 A1B2C1 + 6.88 A2B2C1 + 14.56 A3B2C1 - 20.54 A1B1C2 + 8.93 A2B1C2 + 11.59 A3B1C2 + 13.1 A1B2C2 - 8.36 A2B2C2 - 6.31 A3B2C2"
                                 ))
})

test_that("17.5 Three factors 2*three and 1*four levels Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", "", 1, ""))
})

### Four factors 1x three, 1*two, 1*five and 1*six levels  (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$fixedFactorsFactorial <- c("A", "B", "C", "D")
options$codeFactors <- FALSE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "A"),
  list(components = "B"),
  list(components = "C"),
  list(components = "D"),
  list(components = c("A", "B")),
  list(components = c("A", "C")),
  list(components = c("A", "D")),
  list(components = c("B", "C")),
  list(components = c("B", "D")),
  list(components = c("D", "C"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/6level4facFull.csv", options)

test_that("18.1 Four factors 1x three, 1*two, 1*five and 1*six levels ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(95.0212460548813, 6841.52971595146, 72, 1.09008572906614, 0.322574585190677,
                                      "Model", "", 1218.43208672634, 13, "", "", "<unicode> Linear terms",
                                      186.160268039525, 930.801340197624, 5, 2.13563450211764, 0.0636169442854498,
                                      "<unicode> <unicode> A", 11.9937577468595, 47.975030987438,
                                      4, 0.137592640599311, 0.968167231270774, "<unicode> <unicode> B",
                                      71.0125895072409, 213.037768521723, 3, 0.81465791725322, 0.487428987223623,
                                      "<unicode> <unicode> C", 26.6179470195548, 26.6179470195548,
                                      1, 0.305361646871023, 0.581279381167575, "<unicode> <unicode> D",
                                      "", 5623.09762922512, 59, "", "", "<unicode> Interaction terms",
                                      99.8983176712079, 1997.96635342416, 20, 1.14603559700958, 0.308276136943842,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>B", 47.1534853116117,
                                      707.302279674175, 15, 0.540945773161404, 0.914322220822236,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>C", 156.718073736802,
                                      783.590368684008, 5, 1.79787303113825, 0.115879874736061, "<unicode> <unicode> A<unicode><unicode><unicode>D",
                                      109.921618241654, 1319.05941889984, 12, 1.26102311152473, 0.246223149804816,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>C", 158.459000089108,
                                      633.836000356434, 4, 1.81784497479082, 0.127677213621419, "<unicode> <unicode> B<unicode><unicode><unicode>D",
                                      60.4477360621653, 181.343208186496, 3, 0.693457696794096, 0.557253731576649,
                                      "<unicode> <unicode> C<unicode><unicode><unicode>D", 87.1685992406161,
                                      14557.1560731829, 167, "", "", "Error", "", 21398.6857891343,
                                      239, "", "", "Total"))
})

test_that("18.2 Four factors 1x three, 1*two, 1*five and 1*six levels Uncoded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", 100.622776305583, "", 9.85349697235174e-188, 0.602662838439455,
                                      "(Intercept)", 166.963631881033, "", "A1", 1.83923588891664,
                                      11.0354153334998, 0.174143056318661, 1.34759507426359, "A1",
                                      1.36482829600851, 0.999999999999998, "A2", -0.835238451083331,
                                      -5.01143070649998, 0.53623469143814, 1.34759507426359, "A2",
                                      -0.619799275787465, 0.999999999999998, "A3", -3.86264354008332,
                                      -23.1758612404999, 0.00468748678333847, 1.34759507426359, "A3",
                                      -2.86632358180302, 0.999999999999998, "A4", 0.0555922019166662,
                                      0.333553211499997, 0.967143561833919, 1.3475950742636, "A4",
                                      0.0412528978313794, 0.999999999999998, "A5", 1.81259895416668,
                                      10.8755937250001, 0.180428941403236, 1.3475950742636, "A5",
                                      1.34506202106533, 0.999999999999998, "B1", 0.0523020939999818,
                                      0.261510469999909, 0.965440495697373, 1.20532567687891, "B1",
                                      0.0433924996399428, 1, "B2", 0.382013101083338, 1.91006550541669,
                                      0.751686764211576, 1.20532567687891, "B2", 0.31693766125727,
                                      1, "B3", -0.579240674333329, -2.89620337166664, 0.631452667303152,
                                      1.20532567687891, "B3", -0.480567771387086, 1, "B4", -0.429925973291664,
                                      -2.14962986645832, 0.721775184931635, 1.20532567687891, "B4",
                                      -0.356688637385475, 1, "C1", -1.16728803675001, -4.66915214700003,
                                      0.265061764145456, 1.04384265601081, "C1", -1.11826052521264,
                                      1, "C2", -0.623949337416665, -2.49579734966666, 0.550821319164731,
                                      1.04384265601081, "C2", -0.597742709424402, 1, "C3", 0.583777642916669,
                                      2.33511057166668, 0.576734705262861, 1.04384265601081, "C3",
                                      0.55925827475537, 1, "D1", -0.333028696333336, -0.666057392666672,
                                      0.58127938116757, 0.602662838439455, "D1", -0.552595373551962,
                                      1, "A1B1", 2.16192807399993, 12.9715684439996, 0.423610190595182,
                                      2.69519014852719, "A1<unicode>B1", 0.802143060363045, 0.999999999999998,
                                      "A2B1", 2.48019585275001, 14.8811751165001, 0.358779736176767,
                                      2.69519014852719, "A2<unicode>B1", 0.920230379331617, 0.999999999999998,
                                      "A3B1", -0.619076316999986, -3.71445790199991, 0.818608543080568,
                                      2.69519014852719, "A3<unicode>B1", -0.229696712619065, 0.999999999999998,
                                      "A4B1", -0.0905169177499928, -0.543101506499957, 0.973248499548591,
                                      2.69519014852719, "A4<unicode>B1", -0.0335846128702484, 0.999999999999998,
                                      "A5B1", 1.74193691500002, 10.4516214900001, 0.518963756911622,
                                      2.69519014852719, "A5<unicode>B1", 0.646313179777656, 0.999999999999998,
                                      "A1B2", 0.29149603316668, 1.74897619900008, 0.914003238284294,
                                      2.69519014852719, "A1<unicode>B2", 0.108154162453425, 0.999999999999998,
                                      "A2B2", -5.95875060058333, -35.7525036035, 0.0284025191983166,
                                      2.69519014852719, "A2<unicode>B2", -2.21088319272744, 0.999999999999998,
                                      "A3B2", 3.29672514216666, 19.780350853, 0.222981792767448, 2.69519014852719,
                                      "A3<unicode>B2", 1.2231883319877, 0.999999999999998, "A4B2",
                                      1.48394661391667, 8.90367968349999, 0.582650047089169, 2.69519014852719,
                                      "A4<unicode>B2", 0.550590693843097, 0.999999999999998, "A5B2",
                                      -1.88382994333334, -11.30297966, 0.485549360509785, 2.69519014852719,
                                      "A5<unicode>B2", -0.698959939565961, 0.999999999999998, "A1B3",
                                      0.754560273583341, 4.52736164150005, 0.779850444881491, 2.69519014852719,
                                      "A1<unicode>B3", 0.279965505957224, 0.999999999999998, "A2B3",
                                      5.56682459233334, 33.400947554, 0.0404238914274163, 2.69519014852719,
                                      "A2<unicode>B3", 2.06546636250336, 0.999999999999998, "A3B3",
                                      -2.97992420741667, -17.8795452445, 0.270470365476904, 2.69519014852719,
                                      "A3<unicode>B3", -1.10564525810733, 0.999999999999998, "A4B3",
                                      -2.99322356066667, -17.959341364, 0.268345762199821, 2.69519014852719,
                                      "A4<unicode>B3", -1.1105797349038, 0.999999999999998, "A5B3",
                                      -0.334861699166667, -2.009170195, 0.901271360152021, 2.69519014852719,
                                      "A5<unicode>B3", -0.124244183420474, 0.999999999999998, "A1B4",
                                      1.23777494254168, 7.42664965525011, 0.646649513420164, 2.69519014852719,
                                      "A1<unicode>B4", 0.459253289871988, 0.999999999999998, "A2B4",
                                      1.78441243504166, 10.7064746102499, 0.508837065305229, 2.69519014852719,
                                      "A2<unicode>B4", 0.662072928701066, 0.999999999999998, "A3B4",
                                      -4.62143769970833, -27.72862619825, 0.0882556056894754, 2.69519014852719,
                                      "A3<unicode>B4", -1.71469820125076, 0.999999999999998, "A4B4",
                                      1.22935048954167, 7.37610293725003, 0.648890866417959, 2.69519014852719,
                                      "A4<unicode>B4", 0.45612755382527, 0.999999999999998, "A5B4",
                                      -0.93506725895834, -5.61040355375004, 0.729073753100948, 2.69519014852719,
                                      "A5<unicode>B4", -0.346939253792285, 0.999999999999998, "A1C1",
                                      -0.99604273775004, -5.97625642650024, 0.67012220159439, 2.3341031366541,
                                      "A1<unicode>C1", -0.42673467256372, 0.999999999999998, "A2C1",
                                      -0.823248210749993, -4.93948926449996, 0.724754891069772, 2.3341031366541,
                                      "A2<unicode>C1", -0.35270429906114, 0.999999999999998, "A3C1",
                                      -1.14437378974999, -6.86624273849993, 0.624576363679346, 2.3341031366541,
                                      "A3<unicode>C1", -0.490284157447485, 0.999999999999998, "A4C1",
                                      1.14819040025, 6.8891424015, 0.623422378263534, 2.3341031366541,
                                      "A4<unicode>C1", 0.491919308199857, 0.999999999999998, "A5C1",
                                      1.38763077800001, 8.32578466800005, 0.552980461517495, 2.3341031366541,
                                      "A5<unicode>C1", 0.594502769054651, 0.999999999999998, "A1C2",
                                      3.48527190391667, 20.9116314235, 0.137272948327096, 2.3341031366541,
                                      "A1<unicode>C2", 1.49319533022553, 0.999999999999998, "A2C2",
                                      0.572154478916669, 3.43292687350001, 0.806658135402696, 2.3341031366541,
                                      "A2<unicode>C2", 0.245128190752035, 0.999999999999998, "A3C2",
                                      -1.82219269308334, -10.9331561585, 0.43609464383128, 2.3341031366541,
                                      "A3<unicode>C2", -0.780682166296826, 0.999999999999998, "A4C2",
                                      -4.03342838808333, -24.2005703285, 0.0858291754359795, 2.3341031366541,
                                      "A4<unicode>C2", -1.72804205810082, 0.999999999999998, "A5C2",
                                      1.54020670166667, 9.24124021, 0.510245719182309, 2.3341031366541,
                                      "A5<unicode>C2", 0.659870884657877, 0.999999999999998, "A1C3",
                                      -2.36475511441665, -14.1885306864999, 0.312462614462435, 2.3341031366541,
                                      "A1<unicode>C3", -1.01313222936948, 0.999999999999998, "A2C3",
                                      -0.354738748416669, -2.12843249050002, 0.879385568836887, 2.3341031366541,
                                      "A2<unicode>C3", -0.151980751341255, 0.999999999999998, "A3C3",
                                      1.86947458958332, 11.2168475374999, 0.424304905531446, 2.3341031366541,
                                      "A3<unicode>C3", 0.800939153127222, 0.999999999999998, "A4C3",
                                      0.386364076583335, 2.31818445950001, 0.868727058993398, 2.3341031366541,
                                      "A4<unicode>C3", 0.165529993304915, 0.999999999999998, "A5C3",
                                      0.530487328333333, 3.18292397, 0.820486560190045, 2.3341031366541,
                                      "A5<unicode>C3", 0.227276729979369, 0.999999999999998, "A1D1",
                                      -0.456940892666681, -2.74164535600008, 0.734976379107704, 1.34759507426359,
                                      "A1<unicode>D1", -0.339078779221852, 0.999999999999998, "A2D1",
                                      -3.80250040316667, -22.815002419, 0.00535694096718883, 1.34759507426359,
                                      "A2<unicode>D1", -2.82169360499079, 0.999999999999998, "A3D1",
                                      0.623309711333337, 3.73985826800002, 0.644299887079111, 1.34759507426359,
                                      "A3<unicode>D1", 0.462534869143797, 0.999999999999998, "A4D1",
                                      1.34357639133334, 8.06145834800003, 0.320198267338514, 1.34759507426359,
                                      "A4<unicode>D1", 0.997017885411571, 0.999999999999998, "A5D1",
                                      0.922293502083337, 5.53376101250003, 0.494671561078099, 1.34759507426359,
                                      "A5<unicode>D1", 0.684399579441423, 0.999999999999998, "B1C1",
                                      0.611882882999964, 3.05941441499982, 0.769816067486868, 2.08768531202162,
                                      "B1<unicode>C1", 0.293091530354948, 1, "B2C1", -0.958982909916653,
                                      -4.79491454958327, 0.646578617499653, 2.08768531202162, "B2<unicode>C1",
                                      -0.459352233018308, 1, "B3C1", 0.234758471333341, 1.1737923566667,
                                      0.910602325919279, 2.08768531202162, "B3<unicode>C1", 0.112449165581383,
                                      1, "B4C1", 0.960032864458338, 4.80016432229169, 0.646218303899364,
                                      2.08768531202162, "B4<unicode>C1", 0.459855160607844, 1, "B1C2",
                                      0.258035162833336, 1.29017581416668, 0.901781664781973, 2.08768531202162,
                                      "B1<unicode>C2", 0.123598686711776, 1, "B2C2", -2.56563291925,
                                      -12.82816459625, 0.220824442094974, 2.08768531202162, "B2<unicode>C2",
                                      -1.22893661438158, 1, "B3C2", 5.10555194116667, 25.5277597058333,
                                      0.0155008840162766, 2.08768531202162, "B3<unicode>C2", 2.44555628751475,
                                      1, "B4C2", -3.13673013654166, -15.6836506827083, 0.134858712443026,
                                      2.08768531202162, "B4<unicode>C2", -1.5024918355651, 1, "B1C3",
                                      -1.65977939249999, -8.29889696249993, 0.42772258769392, 2.08768531202162,
                                      "B1<unicode>C3", -0.795033323720964, 1, "B2C3", -0.781744359583339,
                                      -3.9087217979167, 0.708540821625709, 2.08768531202162, "B2<unicode>C3",
                                      -0.374455074757571, 1, "B3C3", 0.273118703333332, 1.36559351666666,
                                      0.896072240065414, 2.08768531202162, "B3<unicode>C3", 0.130823693475553,
                                      1, "B4C3", 2.82049885312499, 14.102494265625, 0.178517471806308,
                                      2.08768531202162, "B4<unicode>C3", 1.35101724234183, 1, "B1D1",
                                      2.18756558174999, 10.9378279087499, 0.0713310151267161, 1.20532567687891,
                                      "B1<unicode>D1", 1.81491660197143, 1, "B2D1", 1.76306185508334,
                                      8.81530927541668, 0.145421659921488, 1.20532567687891, "B2<unicode>D1",
                                      1.46272653848098, 1, "B3D1", -1.5651846845, -7.8259234225, 0.195886117667914,
                                      1.20532567687891, "B3<unicode>D1", -1.29855748908703, 1, "B4D1",
                                      -1.28248421887499, -6.41242109437497, 0.288858114606009, 1.20532567687891,
                                      "B4<unicode>D1", -1.0640146837292, 1, "C1D1", 1.23298579583333,
                                      4.9319431833333, 0.23920315428995, 1.04384265601081, "C1<unicode>D1",
                                      1.18119889883151, 1, "C2D1", 0.373515440166671, 1.49406176066668,
                                      0.720924381169575, 1.04384265601081, "C2<unicode>D1", 0.357827339221902,
                                      1, "C3D1", -0.613255965833333, -2.45302386333333, 0.557662487616341,
                                      1.04384265601081, "C3<unicode>D1", -0.587498472400981, 1))
})

test_that("18.3 Four factors 1x three, 1*two, 1*five and 1*six levels Discrete Predictor Levels table results match", {
  table <- results[["results"]][["tableCoefficientsLegend"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2, "A1", 3, "A2", 4, "A3", 5, "A4", 6, "A5", 2, "B1", 3, "B2",
                                      4, "B3", 5, "B4", 2, "C1", 3, "C2", 4, "C3", 2, "D1"))
})

test_that("18.4 Four factors 1x three, 1*two, 1*five and 1*six levels Regression Equation in Uncoded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 100.62  + 1.84 A1 - 0.84 A2 - 3.86 A3 + 0.06 A4 + 1.81 A5 + 0.05 B1 + 0.38 B2 - 0.58 B3 - 0.43 B4 - 1.17 C1 - 0.62 C2 + 0.58 C3 - 0.33 D1 + 2.16 A1B1 + 2.48 A2B1 - 0.62 A3B1 - 0.09 A4B1 + 1.74 A5B1 + 0.29 A1B2 - 5.96 A2B2 + 3.3 A3B2 + 1.48 A4B2 - 1.88 A5B2 + 0.75 A1B3 + 5.57 A2B3 - 2.98 A3B3 - 2.99 A4B3 - 0.33 A5B3 + 1.24 A1B4 + 1.78 A2B4 - 4.62 A3B4 + 1.23 A4B4 - 0.94 A5B4 - 1 A1C1 - 0.82 A2C1 - 1.14 A3C1 + 1.15 A4C1 + 1.39 A5C1 + 3.49 A1C2 + 0.57 A2C2 - 1.82 A3C2 - 4.03 A4C2 + 1.54 A5C2 - 2.36 A1C3 - 0.35 A2C3 + 1.87 A3C3 + 0.39 A4C3 + 0.53 A5C3 - 0.46 A1D1 - 3.8 A2D1 + 0.62 A3D1 + 1.34 A4D1 + 0.92 A5D1 + 0.61 B1C1 - 0.96 B2C1 + 0.23 B3C1 + 0.96 B4C1 + 0.26 B1C2 - 2.57 B2C2 + 5.11 B3C2 - 3.14 B4C2 - 1.66 B1C3 - 0.78 B2C3 + 0.27 B3C3 + 2.82 B4C3 + 2.19 B1D1 + 1.76 B2D1 - 1.57 B3D1 - 1.28 B4D1 + 1.23 C1D1 + 0.37 C2D1 - 0.61 C3D1"
                                 ))
})

test_that("18.5 Four factors 1x three, 1*two, 1*five and 1*six levels Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0264217427274993, 0, 0.319717284667332, 9.3364125466164))
})

## RSM designs ####

### Three continuous predictors (verified with Minitab) CCD ####

### Five continuous predictors (verified with Minitab) CCD ####

### Three continuous and two categorical predictors (verified with Minitab) CCD ####

### Four continuous and one categorical predictor (verified with Minitab) CCD ####

### Three continuous predictors BBD (verified with Minitab) ####

### Four continuous predictors BBD (verified with Minitab) ####

### Five continuous predictors BBD (verified with Minitab) ####

### Six continuous predictors one discrete predictor BBD (verified with Minitab) ####

# Specific tests ####

## Plots ####

## Sums of squares types ####

## Coding squared terms ####

## Alias names ####
