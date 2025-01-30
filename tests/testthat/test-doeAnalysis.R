context("[Quality Control] DoE Analysis")
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
                                      "BLK1", 2.67621831875, "", "", "", "Block1", "", "NaN", "AB",
                                      -0.198051021250002, -0.396102042500004, "", "", "A<unicode>B",
                                      "", "NaN", "BC", 0.302618163749995, 0.605236327499991, "", "",
                                      "B<unicode>C", "", "NaN", "AC", -4.52522017875, -9.0504403575,
                                      "", "", "A<unicode>C", "", "NaN"))
})

test_that("10.3 Two blocks Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 99.75 (Intercept) + 0.94 A + 0.67 B - 2.1 C + 2.68 BLK1 - 0.2 AB + 0.3 BC - 4.53 AC"
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
                                 list(1, "A1", 2, "A2", 1, "B1", 2, "B2", 1, "C1", 2, "C2"))
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
                                 list(1, "A1", 2, "A2", 3, "A3", 1, "B1", 2, "B2", 1, "C1", 2, "C2"
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
                                 list(1, "A1", 2, "A2", 3, "A3", 4, "A4", 5, "A5", 1, "B1", 2, "B2",
                                      3, "B3", 4, "B4", 1, "C1", 2, "C2", 3, "C3", 1, "D1"))
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

### Three continuous predictors CCD (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$designType <- "responseSurfaceDesign"
options$dependentResponseSurface <- "Result"
options$continuousFactorsResponseSurface <- c("A", "B", "C")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "manual"
options$codeFactorsManualTable <- list(
  list(predictors = "A", lowValue = "-1", highValue = "1"),
  list(predictors = "B", lowValue = "-1", highValue = "1"),
  list(predictors = "C", lowValue = "-1", highValue = "1")
)
options$squaredTermsCoded <- TRUE
options$tableEquation <- TRUE
options$rsmPredefinedModel <- TRUE
options$rsmPredefinedTerms <- "fullQuadratic"
options$modelTerms <- NULL
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/RSM3contCCD.csv", options)

test_that("19.1 Three continuous predictors CCD ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(33.4159369942268, 300.743432948041, 9, 0.603726470392989, 0.769693680161224,
                                      "Model", "", 136.241022081355, 3, "", "", "<unicode> Linear terms",
                                      0.647624788809821, 0.647624788809821, 1, 0.0117006513375552,
                                      0.91600080679181, "<unicode> <unicode> A", 110.228628924466,
                                      110.228628924466, 1, 1.99150306897945, 0.188530129971547, "<unicode> <unicode> B",
                                      25.3647683680789, 25.3647683680789, 1, 0.458265829320952, 0.513789823406216,
                                      "<unicode> <unicode> C", "", 94.2155813129141, 3, "", "", "<unicode> Squared terms",
                                      23.0209343677798, 23.0209343677798, 1, 0.41591972876324, 0.53349530232022,
                                      "<unicode> <unicode> A^2", 71.1884410693395, 71.1884410693395,
                                      1, 1.28616313428521, 0.283211015769789, "<unicode> <unicode> B^2",
                                      0.00620587579476251, 0.00620587579476251, 1, 0.000112121694804385,
                                      0.991759824072754, "<unicode> <unicode> C^2", "", 70.2868295537721,
                                      3, "", "", "<unicode> Interaction terms", 24.8333207140014,
                                      24.8333207140014, 1, 0.448664152837953, 0.518137155944881, "<unicode> <unicode> A<unicode><unicode><unicode>B",
                                      0.010619694879324, 0.010619694879324, 1, 0.000191866261516248,
                                      0.989220847538756, "<unicode> <unicode> A<unicode><unicode><unicode>C",
                                      45.4428891448914, 45.4428891448914, 1, 0.821017680056228, 0.386194705961586,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>C", 55.3494647542538,
                                      553.494647542538, 10, "", "", "Error", "", 854.238080490579,
                                      19, "", "", "Total"))
})

test_that("19.2 Three continuous predictors CCD Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 101.126534122288, "", 1.35754827517153e-11, 3.02555115875388,
                                      "(Intercept)", 33.4241692888572, "", "A", -0.220389873736422,
                                      -0.719793327623154, 0.916000806791808, 2.03744839199981, "A",
                                      -0.108169549030935, 1, "B", 2.87525989685885, 9.39059882314099,
                                      0.188530129971547, 2.03744839199981, "B", 1.4112062460815, 1,
                                      "C", -1.37925750124087, -4.50465499905269, 0.513789823406216,
                                      2.03744839199981, "C", -0.676953343533327, 1, "A^2", 1.32028414200427,
                                      4.31204800778595, 0.53349530232022, 2.04721118835923, "A^2",
                                      0.644918389227071, "", "B^2", -2.32172444992377, -7.58275205345104,
                                      0.283211015769789, 2.04721118835923, "B^2", -1.13409132537252,
                                      "", "C^2", -0.0216774140676994, -0.0707984343451061, 0.991759824072725,
                                      2.04721118835923, "C^2", -0.0105887532224133, "", "AB", -1.761864095,
                                      -5.75424813427, 0.518137155944881, 2.63033896946415, "A<unicode>B",
                                      -0.669823971531291, 1, "AC", 0.0364343500000011, 0.118994587100004,
                                      0.989220847538727, 2.63033896946415, "A<unicode>C", 0.0138515797480746,
                                      1, "BC", -2.3833508225, -7.784023786285, 0.386194705961586,
                                      2.63033896946415, "B<unicode>C", -0.906100259384263, 1))
})

test_that("19.3 Three continuous predictors CCD Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 101.13 (Intercept) - 0.22 A + 2.88 B - 1.38 C + 1.32 A^2 - 2.32 B^2 - 0.02 C^2 - 1.76 AB + 0.04 AC - 2.38 BC"
                                 ))
})

test_that("19.4 Three continuous predictors CCD Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0.356816916827087, 7.43972208850934))
})


### Five continuous predictors CCD (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$designType <- "responseSurfaceDesign"
options$dependentResponseSurface <- "Result"
options$continuousFactorsResponseSurface <- c("A", "B", "C", "D", "E")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "manual"
options$codeFactorsManualTable <- list(
  list(predictors = "A", lowValue = "-1", highValue = "1"),
  list(predictors = "B", lowValue = "-1", highValue = "1"),
  list(predictors = "C", lowValue = "-1", highValue = "1"),
  list(predictors = "D", lowValue = "-1", highValue = "1"),
  list(predictors = "E", lowValue = "-1", highValue = "1")
)
options$squaredTermsCoded <- TRUE
options$tableEquation <- TRUE
options$rsmPredefinedModel <- TRUE
options$rsmPredefinedTerms <- "fullQuadratic"
options$modelTerms <- NULL
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/RSM5contCCD.csv", options)

test_that("20.1 Five continuous predictors CCD ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(51.3587113078249, 1027.1742261565, 20, 0.419782874778523, 0.977915041906087,
                                      "Model", "", 341.185797996187, 5, "", "", "<unicode> Linear terms",
                                      43.5159978609836, 43.5159978609836, 1, 0.355680082614468, 0.554984654836323,
                                      "<unicode> <unicode> A", 167.725674152279, 167.725674152279,
                                      1, 1.37091379197208, 0.250040619119986, "<unicode> <unicode> B",
                                      17.7431526190489, 17.7431526190489, 1, 0.145024503621524, 0.705776826305417,
                                      "<unicode> <unicode> C", 54.1622602726811, 54.1622602726811,
                                      1, 0.442697815867989, 0.510447146810385, "<unicode> <unicode> D",
                                      58.0387130911945, 58.0387130911945, 1, 0.474382187743008, 0.495792171556951,
                                      "<unicode> <unicode> E", "", 112.410259826369, 5, "", "", "<unicode> Squared terms",
                                      27.2974186686115, 27.2974186686115, 1, 0.223116752561445, 0.639784215001193,
                                      "<unicode> <unicode> A^2", 8.80938068797559, 8.80938068797559,
                                      1, 0.0720038929336097, 0.790111496726485, "<unicode> <unicode> B^2",
                                      13.8759183253928, 13.8759183253928, 1, 0.113415479798807, 0.738421088882711,
                                      "<unicode> <unicode> C^2", 19.7879088296081, 19.7879088296081,
                                      1, 0.161737415967496, 0.690155047932803, "<unicode> <unicode> D^2",
                                      42.6396333147809, 42.6396333147809, 1, 0.348517075226023, 0.558975217166694,
                                      "<unicode> <unicode> E^2", "", 573.578168333942, 10, "", "",
                                      "<unicode> Interaction terms", 1.3469787771719, 1.3469787771719,
                                      1, 0.0110095952361003, 0.917069213512083, "<unicode> <unicode> A<unicode><unicode><unicode>B",
                                      6.16225756654512, 6.16225756654512, 1, 0.0503675059310901, 0.823808823720829,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>C", 23.1841269930401,
                                      23.1841269930401, 1, 0.189496566999841, 0.666169937690087, "<unicode> <unicode> A<unicode><unicode><unicode>D",
                                      9.31816109574629, 9.31816109574629, 1, 0.0761624338464623, 0.784287905909558,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>E", 57.1418482201193,
                                      57.1418482201193, 1, 0.467051619972113, 0.499117346598992, "<unicode> <unicode> B<unicode><unicode><unicode>C",
                                      1.710927412204, 1.710927412204, 1, 0.0139843467513747, 0.906582167574386,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>D", 156.341909645397,
                                      156.341909645397, 1, 1.27786805019209, 0.266445152501293, "<unicode> <unicode> B<unicode><unicode><unicode>E",
                                      284.888898875528, 284.888898875528, 1, 2.32855299358409, 0.136548146962589,
                                      "<unicode> <unicode> C<unicode><unicode><unicode>D", 19.0204310412573,
                                      19.0204310412573, 1, 0.15546439968421, 0.695903744576438, "<unicode> <unicode> C<unicode><unicode><unicode>E",
                                      14.4626287069341, 14.4626287069341, 1, 0.11821098506664, 0.733164116033324,
                                      "<unicode> <unicode> D<unicode><unicode><unicode>E", 122.345894493485,
                                      4037.414518285, 33, "", "", "Error", "", 5064.58874444149, 53,
                                      "", "", "Total"))
})

test_that("20.2 Five continuous predictors CCD Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 95.7137721498586, "", 1.41697458968158e-25, 3.17945282904517,
                                      "(Intercept)", 30.1038503466657, "", "A", 1.00369820969494,
                                      4.74949992827645, 0.554984654836324, 1.68295838189235, "A",
                                      0.596389203972093, 1, "B", 1.97050911987227, 9.32444915523557,
                                      0.250040619119986, 1.68295838189235, "B", 1.17086027858668,
                                      1, "C", 0.640905605769826, 3.03276532650282, 0.705776826305417,
                                      1.68295838189235, "C", 0.380820828765344, 1, "D", -1.11976545692704,
                                      -5.29873014217874, 0.510447146810384, 1.68295838189235, "D",
                                      -0.665355405680295, 1, "E", 1.15914445813947, 5.48507157591596,
                                      0.49579217155695, 1.68295838189235, "E", 0.68875408364888, 1,
                                      "A^2", 0.676630609752464, 3.20181604534866, 0.639784215001194,
                                      1.4324700503189, "A^2", 0.472352360596879, "", "B^2", 0.384382440063385,
                                      1.81889770637994, 0.790111496726482, 1.4324700503189, "B^2",
                                      0.268335411255413, "", "C^2", 0.482416016068628, 2.28279258803675,
                                      0.73842108888271, 1.4324700503189, "C^2", 0.336772148193415,
                                      "", "D^2", -0.57609061563766, -2.72606079319741, 0.690155047932804,
                                      1.43247005031891, "D^2", -0.402165906023241, "", "E^2", 0.845663489789393,
                                      4.00167963368341, 0.558975217166694, 1.4324700503189, "E^2",
                                      0.590353347772351, "", "AB", 0.205165998124996, 0.970845503127479,
                                      0.917069213512079, 1.95532841306042, "A<unicode>B", 0.104926618339206,
                                      1, "AC", -0.438828609999998, -2.07653698251999, 0.823808823720831,
                                      1.95532841306042, "A<unicode>C", -0.224427061494573, 1, "AD",
                                      -0.851177988750004, -4.02777424276502, 0.666169937690088, 1.95532841306042,
                                      "A<unicode>D", -0.43531203406274, 1, "AE", -0.539622584999997,
                                      -2.55349407221998, 0.784287905909556, 1.95532841306042, "A<unicode>E",
                                      -0.275975422540603, 1, "BC", -1.3362944125, -6.32334515995001,
                                      0.499117346598993, 1.95532841306042, "B<unicode>C", -0.683411749951749,
                                      1, "BD", -0.231228202499996, -1.09417185422998, 0.906582167574383,
                                      1.95532841306042, "B<unicode>D", -0.118255430113699, 1, "BE",
                                      2.210358495, 10.45941639834, 0.266445152501293, 1.95532841306042,
                                      "B<unicode>E", 1.13042825963972, 1, "CD", -2.983752350625, -14.1191161231575,
                                      0.136548146962589, 1.95532841306042, "C<unicode>D", -1.52595969592388,
                                      1, "CE", -0.770965933124993, -3.64821079554747, 0.695903744576438,
                                      1.95532841306042, "C<unicode>E", -0.394289740779811, 1, "DE",
                                      0.672277581874996, 3.18121751743248, 0.733164116033323, 1.95532841306042,
                                      "D<unicode>E", 0.343818244231805, 1))
})

test_that("20.3 Five continuous predictors CCD Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 95.71 (Intercept) + 1 A + 1.97 B + 0.64 C - 1.12 D + 1.16 E + 0.68 A^2 + 0.38 B^2 + 0.48 C^2 - 0.58 D^2 + 0.85 E^2 + 0.21 AB - 0.44 AC - 0.85 AD - 0.54 AE - 1.34 BC - 0.23 BD + 2.21 BE - 2.98 CD - 0.77 CE + 0.67 DE"
                                 ))
})

test_that("20.4 Five continuous predictors CCD Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0.202398894110419, 11.0610078425741))
})


### Three continuous and two categorical predictors CCD (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$designType <- "responseSurfaceDesign"
options$dependentResponseSurface <- "Result"
options$continuousFactorsResponseSurface <- c("A", "B", "C")
options$fixedFactorsResponseSurface <- c("D", "E")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "manual"
options$codeFactorsManualTable <- list(
  list(predictors = "A", lowValue = "-1", highValue = "1"),
  list(predictors = "B", lowValue = "-1", highValue = "1"),
  list(predictors = "C", lowValue = "-1", highValue = "1"),
  list(predictors = "D", lowValue = "1", highValue = "2"),
  list(predictors = "E", lowValue = "1", highValue = "2")
)
options$squaredTermsCoded <- TRUE
options$tableEquation <- TRUE
options$rsmPredefinedModel <- TRUE
options$rsmPredefinedTerms <- "fullQuadratic"
options$modelTerms <- NULL
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/RSM3cont2discCCD.csv", options)

test_that("21.1 Three continuous and two categorical predictors CCD ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(72.7397226090048, 1309.31500696209, 18, 0.850063612714722, 0.636675405849037,
                                      "Model", "", 129.039377068184, 5, "", "", "<unicode> Linear terms",
                                      1.03353865663394, 1.03353865663394, 1, 0.0120783194219908, 0.912848334563602,
                                      "<unicode> <unicode> A", 28.7652355681139, 28.7652355681139,
                                      1, 0.336161304863071, 0.564188827319796, "<unicode> <unicode> B",
                                      2.50518730405929, 2.50518730405929, 1, 0.0292765561076262, 0.86470835745907,
                                      "<unicode> <unicode> C", 25.8844577816353, 25.8844577816353,
                                      1, 0.302495457857227, 0.58433051029605, "<unicode> <unicode> D",
                                      70.8509577577415, 70.8509577577415, 1, 0.827990800014242, 0.366435695900971,
                                      "<unicode> <unicode> E", "", 285.525611162234, 3, "", "", "<unicode> Squared terms",
                                      120.841719383848, 120.841719383848, 1, 1.41220154355353, 0.239297377175185,
                                      "<unicode> <unicode> A^2", 10.2525983748001, 10.2525983748001,
                                      1, 0.119815700439813, 0.730425539896361, "<unicode> <unicode> B^2",
                                      154.431293403586, 154.431293403586, 1, 1.80474187250486, 0.184116818332041,
                                      "<unicode> <unicode> C^2", "", 894.750018731668, 10, "", "",
                                      "<unicode> Interaction terms", 164.019124783417, 164.019124783417,
                                      1, 1.91678885713043, 0.171254881209314, "<unicode> <unicode> A<unicode><unicode><unicode>B",
                                      2.14773960622188, 2.14773960622188, 1, 0.0250992885778402, 0.874643231973497,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>C", 68.6740245803558,
                                      68.6740245803558, 1, 0.802550344441512, 0.373853640532771, "<unicode> <unicode> A<unicode><unicode><unicode>D",
                                      39.903289914766, 39.903289914766, 1, 0.466324775067945, 0.497268202585986,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>E", 81.4255399388621,
                                      81.4255399388621, 1, 0.951569323679375, 0.333172860185823, "<unicode> <unicode> B<unicode><unicode><unicode>C",
                                      30.9515833689429, 30.9515833689429, 1, 0.361711783247677, 0.54978573411972,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>D", 2.77314757882505,
                                      2.77314757882505, 1, 0.0324080401312291, 0.857731743254824,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>E", 415.582646863043,
                                      415.582646863043, 1, 4.85665429428255, 0.0313242695622218, "<unicode> <unicode> C<unicode><unicode><unicode>D",
                                      0.000585072983085411, 0.000585072983085411, 1, 6.83738177524744e-06,
                                      0.997922195618352, "<unicode> <unicode> C<unicode><unicode><unicode>E",
                                      89.2723370242511, 89.2723370242511, 1, 1.04326993016229, 0.311096028475124,
                                      "<unicode> <unicode> D<unicode><unicode><unicode>E", 85.5697403359106,
                                      5219.75416049054, 61, "", "", "Error", "", 6529.06916745263,
                                      79, "", "", "Total"))
})

test_that("21.2 Three continuous and two categorical predictors CCD Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 99.3627387297987, "", 1.21502530331299e-52, 1.88095280770782,
                                      "(Intercept)", 52.8257478457873, "", "A", -0.139207704143692,
                                      -0.454652361733299, 0.9128483345636, 1.26665988192056, "A",
                                      -0.109901407734348, 1, "B", 0.734402042543345, 2.39855707094656,
                                      0.564188827319793, 1.26665988192056, "B", 0.579794191815575,
                                      1, "C", 0.216730488382614, 0.707841775057619, 0.864708357459063,
                                      1.26665988192056, "C", 0.171103933641601, 1, "D", 0.568819586750001,
                                      1.1376391735, 0.58433051029605, 1.03422519510931, "D", 0.549995870763797,
                                      1, "E", -0.941082872000001, -1.882165744, 0.366435695900971,
                                      1.03422519510931, "E", -0.909939998029674, 1, "A^2", -1.51246170038837,
                                      -4.93969991346842, 0.239297377175186, 1.27272930803825, "A^2",
                                      -1.18836086419636, "", "B^2", -0.440547671887108, -1.4388286963833,
                                      0.730425539896366, 1.27272930803825, "B^2", -0.34614404579569,
                                      "", "C^2", 1.70979322697661, 5.58418467930562, 0.184116818332041,
                                      1.27272930803825, "C^2", 1.34340681571327, "", "AB", 2.263978279375,
                                      7.39415306043875, 0.171254881209314, 1.63525361503872, "A<unicode>B",
                                      1.38448143979268, 1, "AC", 0.259069223750001, 0.846120084767504,
                                      0.874643231973502, 1.63525361503872, "A<unicode>C", 0.158427549933205,
                                      1, "AD", -1.13473946230355, -3.7060590838834, 0.373853640532771,
                                      1.26665988192056, "A<unicode>D", -0.895851742444872, 1, "AE",
                                      0.864976404004672, 2.82501293547926, 0.497268202585988, 1.26665988192056,
                                      "A<unicode>E", 0.682879766187241, 1, "BC", 1.59516398, 5.20980555867999,
                                      0.333172860185824, 1.63525361503872, "B<unicode>C", 0.975484148348589,
                                      1, "BD", 0.761800658983606, 2.48804095224046, 0.549785734119722,
                                      1.26665988192056, "B<unicode>D", 0.601424794340634, 1, "BE",
                                      -0.228027066190016, -0.744736398176592, 0.857731743254818, 1.26665988192056,
                                      "B<unicode>E", -0.1800223323125, 1, "CD", 2.79144201533625,
                                      9.11684962208819, 0.0313242695622218, 1.26665988192056, "C<unicode>D",
                                      2.20378181639711, 1, "CE", 0.00331211136367841, 0.0108173557137737,
                                      0.997922195618131, 1.26665988192056, "C<unicode>E", 0.00261483876686491,
                                      1, "DE", 1.0563636745, 2.112727349, 0.311096028475123, 1.03422519510931,
                                      "D<unicode>E", 1.02140585966711, 1))
})

test_that("21.3 Three continuous and two categorical predictors CCD Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 99.36 (Intercept) - 0.14 A + 0.73 B + 0.22 C + 0.57 D - 0.94 E - 1.51 A^2 - 0.44 B^2 + 1.71 C^2 + 2.26 AB + 0.26 AC - 1.13 AD + 0.86 AE + 1.6 BC + 0.76 BD - 0.23 BE + 2.79 CD - 0 CE + 1.06 DE"
                                 ))
})

test_that("21.4 Three continuous and two categorical predictors CCD Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0.203191170479226, 9.25039136122957))
})

### Four continuous and one categorical predictor CCD (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$designType <- "responseSurfaceDesign"
options$dependentResponseSurface <- "Result"
options$continuousFactorsResponseSurface <- c("A", "B", "C", "D")
options$fixedFactorsResponseSurface <- c("E")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "manual"
options$codeFactorsManualTable <- list(
  list(predictors = "A", lowValue = "-1", highValue = "1"),
  list(predictors = "B", lowValue = "-1", highValue = "1"),
  list(predictors = "C", lowValue = "-1", highValue = "1"),
  list(predictors = "D", lowValue = "-1", highValue = "1"),
  list(predictors = "E", lowValue = "1", highValue = "2")
)
options$squaredTermsCoded <- TRUE
options$tableEquation <- TRUE
options$rsmPredefinedModel <- TRUE
options$rsmPredefinedTerms <- "fullQuadratic"
options$modelTerms <- NULL
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/RSM4cont1discCCD.csv", options)

test_that("22.1 Four continuous and one categorical predictor CCD ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(112.80717375643, 2143.33630137217, 19, 1.73488065260125, 0.0706676426284591,
                                      "Model", "", 806.988962707037, 5, "", "", "<unicode> Linear terms",
                                      376.767145242181, 376.767145242181, 1, 5.79436581070454, 0.020784004318429,
                                      "<unicode> <unicode> A", 69.4774594300852, 69.4774594300852,
                                      1, 1.06850562906043, 0.307492657895273, "<unicode> <unicode> B",
                                      168.13355100747, 168.13355100747, 1, 2.58575438939563, 0.115694828056362,
                                      "<unicode> <unicode> C", 57.115010048984, 57.115010048984, 1,
                                      0.878381423871639, 0.354270007041141, "<unicode> <unicode> D",
                                      135.495796978318, 135.495796978318, 1, 2.08381283617676, 0.156654702711686,
                                      "<unicode> <unicode> E", "", 120.612651609061, 4, "", "", "<unicode> Squared terms",
                                      6.93860397448543, 6.93860397448543, 1, 0.106709967022029, 0.745624547672554,
                                      "<unicode> <unicode> A^2", 96.8797300099845, 96.8797300099845,
                                      1, 1.48992979459318, 0.229374281402984, "<unicode> <unicode> B^2",
                                      15.1202574310955, 15.1202574310955, 1, 0.232537002799104, 0.632277359946838,
                                      "<unicode> <unicode> C^2", 1.67406019349528, 1.67406019349528,
                                      1, 0.0257456555666906, 0.873330637626117, "<unicode> <unicode> D^2",
                                      "", 1215.73468705607, 10, "", "", "<unicode> Interaction terms",
                                      304.703219754009, 304.703219754009, 1, 4.68608248157982, 0.0364318452393383,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>B", 111.675477411698,
                                      111.675477411698, 1, 1.71747610262702, 0.197493536979093, "<unicode> <unicode> A<unicode><unicode><unicode>C",
                                      38.1410888614778, 38.1410888614778, 1, 0.58657827274173, 0.448241439165348,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>D", 229.653138285242,
                                      229.653138285242, 1, 3.53187455330177, 0.0674989393565679, "<unicode> <unicode> A<unicode><unicode><unicode>E",
                                      11.5900152530921, 11.5900152530921, 1, 0.178244809761461, 0.675146700686413,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>C", 60.9783696093709,
                                      60.9783696093709, 1, 0.937796685615801, 0.33866437428897, "<unicode> <unicode> B<unicode><unicode><unicode>D",
                                      145.83351421438, 145.83351421438, 1, 2.2427983423967, 0.142088256074661,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>E", 0.305054234760973,
                                      0.305054234760973, 1, 0.00469148080088946, 0.945733347959429,
                                      "<unicode> <unicode> C<unicode><unicode><unicode>D", 25.2672062937827,
                                      25.2672062937827, 1, 0.388588649858534, 0.53658076120345, "<unicode> <unicode> C<unicode><unicode><unicode>E",
                                      287.587603138256, 287.587603138256, 1, 4.42285851155002, 0.0417985945131365,
                                      "<unicode> <unicode> D<unicode><unicode><unicode>E", 65.023016763308,
                                      2600.92067053232, 40, "", "", "Error", "", 4744.25697190449,
                                      59, "", "", "Total"))
})

test_that("22.2 Four continuous and one categorical predictor CCD Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 97.9076142108333, "", 9.88257161322062e-35, 2.32778537031997,
                                      "(Intercept)", 42.0604130686565, "", "A", -2.80166299291666,
                                      -11.2066519716667, 0.020784004318429, 1.16389268515998, "A",
                                      -2.40714889666272, 1, "B", 1.203098945, 4.81239577999999, 0.307492657895274,
                                      1.16389268515998, "B", 1.03368545944133, 1, "C", 1.87157215,
                                      7.48628859999999, 0.115694828056362, 1.16389268515998, "C",
                                      1.60802810590973, 1, "D", 1.09082356166667, 4.36329424666669,
                                      0.354270007041142, 1.16389268515998, "D", 0.937220050933417,
                                      1, "E", 1.50275190333333, 3.00550380666667, 0.156654702711686,
                                      1.04101726501299, "E", 1.44354176807488, 1, "A^2", -0.355647252604167,
                                      -1.42258901041667, 0.745624547672552, 1.08872191571025, "A^2",
                                      -0.326664915505216, "", "B^2", 1.32892315239583, 5.31569260958334,
                                      0.229374281402984, 1.08872191571025, "B^2", 1.22062680397949,
                                      "", "C^2", 0.525004469270834, 2.10001787708333, 0.632277359946836,
                                      1.08872191571025, "C^2", 0.482220906638345, "", "D^2", 0.174690361145833,
                                      0.698761444583333, 0.873330637626108, 1.08872191571025, "D^2",
                                      0.16045452803425, "", "AB", -3.085769858125, -12.3430794325,
                                      0.0364318452393384, 1.42547159699988, "A<unicode>B", -2.16473612285189,
                                      1, "AC", 1.8681163425, 7.47246536999999, 0.197493536979093,
                                      1.42547159699988, "A<unicode>C", 1.31052512475993, 1, "AD",
                                      1.091745861875, 4.3669834475, 0.448241439165348, 1.42547159699988,
                                      "A<unicode>D", 0.765883981254166, 1, "AE", -2.18733636666667,
                                      -8.74934546666668, 0.0674989393565679, 1.16389268515998, "A<unicode>E",
                                      -1.87932821862009, 1, "BC", -0.601820551874999, -2.4072822075,
                                      0.675146700686413, 1.42547159699988, "B<unicode>C", -0.422190489899359,
                                      1, "BD", -1.380425315, -5.52170126, 0.33866437428897, 1.42547159699988,
                                      "B<unicode>D", -0.968399032225768, 1, "BE", -1.74304280291666,
                                      -6.97217121166666, 0.142088256074661, 1.16389268515998, "B<unicode>E",
                                      -1.49759752350112, 1, "CD", 0.097636800624997, 0.390547202499988,
                                      0.945733347959414, 1.42547159699988, "C<unicode>D", 0.0684943851778517,
                                      1, "CE", 0.72553437625, 2.902137505, 0.536580761203449, 1.16389268515998,
                                      "C<unicode>E", 0.623368791213144, 1, "DE", -2.44773536125, -9.790941445,
                                      0.0417985945131366, 1.16389268515998, "D<unicode>E", -2.10305932192842,
                                      1))
})

test_that("22.3 Four continuous and one categorical predictor CCD Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 97.91 (Intercept) - 2.8 A + 1.2 B + 1.87 C + 1.09 D + 1.5 E - 0.36 A^2 + 1.33 B^2 + 0.53 C^2 + 0.17 D^2 - 3.09 AB + 1.87 AC + 1.09 AD - 2.19 AE - 0.6 BC - 1.38 BD - 1.74 BE + 0.1 CD + 0.73 CE - 2.45 DE"
                                 ))
})

test_that("22.4 Four continuous and one categorical predictor CCD Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.191741034230725, 0, 0.452027819817441, 8.06368506101944))
})

### Three continuous predictors BBD (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$designType <- "responseSurfaceDesign"
options$dependentResponseSurface <- "Result"
options$continuousFactorsResponseSurface <- c("A", "B", "C")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "manual"
options$codeFactorsManualTable <- list(
  list(predictors = "A", lowValue = "-1", highValue = "1"),
  list(predictors = "B", lowValue = "-1", highValue = "1"),
  list(predictors = "C", lowValue = "-1", highValue = "1")
)
options$squaredTermsCoded <- TRUE
options$tableEquation <- TRUE
options$rsmPredefinedModel <- TRUE
options$rsmPredefinedTerms <- "fullQuadratic"
options$modelTerms <- NULL
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/RSM3contBBD.csv", options)

test_that("23.1 Three continuous predictors BBD ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(117.387204366981, 1056.48483930283, 9, 2.10615458167809, 0.213246448597519,
                                      "Model", "", 278.946705729, 3, "", "", "<unicode> Linear terms",
                                      161.292756283381, 161.292756283381, 1, 2.89390551099354, 0.14965508320902,
                                      "<unicode> <unicode> A", 114.132490343792, 114.132490343792,
                                      1, 2.04775868675108, 0.211839968723496, "<unicode> <unicode> B",
                                      3.52145910182577, 3.52145910182577, 1, 0.0631818200416107, 0.811539615969981,
                                      "<unicode> <unicode> C", "", 584.723306443657, 3, "", "", "<unicode> Squared terms",
                                      407.920784372129, 407.920784372129, 1, 7.31889164240753, 0.0425117691577854,
                                      "<unicode> <unicode> A^2", 161.545782081756, 161.545782081756,
                                      1, 2.89844528555759, 0.149397975249734, "<unicode> <unicode> B^2",
                                      15.256739989772, 15.256739989772, 1, 0.273735566020244, 0.623186909931675,
                                      "<unicode> <unicode> C^2", "", 192.814827130172, 3, "", "",
                                      "<unicode> Interaction terms", 138.590143539917, 138.590143539917,
                                      1, 2.48657651714316, 0.175646568137584, "<unicode> <unicode> A<unicode><unicode><unicode>B",
                                      1.37244973825602, 1.37244973825602, 1, 0.0246244155820777, 0.8814462450247,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>C", 52.852233851999,
                                      52.852233851999, 1, 0.948271790606003, 0.374883301192008, "<unicode> <unicode> B<unicode><unicode><unicode>C",
                                      55.7353222731884, 278.676611365942, 5, "", "", "Error", "",
                                      1335.16145066877, 14, "", "", "Total"))
})

test_that("23.2 Three continuous predictors BBD Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 90.170651, "", 4.6230339293677e-06, 4.31027154106669,
                                      "(Intercept)", 20.9199467228194, "", "A", -4.49016642624999,
                                      -8.98033285249999, 0.14965508320902, 2.63949148211328, "A",
                                      -1.70114829188802, 1, "B", 3.7771101775, 7.55422035499999, 0.211839968723496,
                                      2.63949148211328, "B", 1.4309991917367, 1, "C", 0.663462423749997,
                                      1.32692484749999, 0.811539615969981, 2.63949148211328, "C",
                                      0.25135994120307, 1, "A^2", 10.5108774975, 21.021754995, 0.0425117691577854,
                                      3.88522626312229, "A^2", 2.70534501356251, "", "B^2", 6.61452814999999,
                                      13.2290563, 0.149397975249734, 3.88522626312229, "B^2", 1.70248209551748,
                                      "", "C^2", 2.0327404525, 4.065480905, 0.623186909931675, 3.88522626312229,
                                      "C^2", 0.523197444584971, "", "AB", -5.8862157525, -11.772431505,
                                      0.175646568137584, 3.73280465177286, "A<unicode>B", -1.57688823863429,
                                      1, "AC", -0.585758000000002, -1.171516, 0.881446245024701, 3.73280465177286,
                                      "A<unicode>C", -0.156921686143367, 1, "BC", 3.6349770925, 7.269954185,
                                      0.374883301192008, 3.73280465177286, "B<unicode>C", 0.973792478203648,
                                      1))
})

test_that("23.3 Three continuous predictors BBD Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 90.17 (Intercept) - 4.49 A + 3.78 B + 0.66 C + 10.51 A^2 + 6.61 B^2 + 2.03 C^2 - 5.89 AB - 0.59 AC + 3.63 BC"
                                 ))
})

test_that("23.4 Three continuous predictors BBD Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.393245006596696, 0, 0.783301788070249, 7.46560930354572))
})

### Four continuous predictors BBD (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$designType <- "responseSurfaceDesign"
options$dependentResponseSurface <- "Result"
options$continuousFactorsResponseSurface <- c("A", "B", "C", "D")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "manual"
options$codeFactorsManualTable <- list(
  list(predictors = "A", lowValue = "-1", highValue = "1"),
  list(predictors = "B", lowValue = "-1", highValue = "1"),
  list(predictors = "C", lowValue = "-1", highValue = "1"),
  list(predictors = "D", lowValue = "-1", highValue = "1")
)
options$squaredTermsCoded <- TRUE
options$tableEquation <- TRUE
options$rsmPredefinedModel <- TRUE
options$rsmPredefinedTerms <- "fullQuadratic"
options$modelTerms <- NULL
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/RSM4contBBD.csv", options)

test_that("24.1 Four continuous predictors BBD ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(77.9009838048483, 1090.61377326788, 14, 1.16863848196573, 0.397556720497501,
                                      "Model", "", 386.491064149768, 4, "", "", "<unicode> Linear terms",
                                      158.15740784502, 158.15740784502, 1, 2.3726123084486, 0.149425279207315,
                                      "<unicode> <unicode> A", 141.429572917864, 141.429572917864,
                                      1, 2.12166821684615, 0.170891581643576, "<unicode> <unicode> B",
                                      0.310224212911066, 0.310224212911066, 1, 0.00465385590191786,
                                      0.946734703534669, "<unicode> <unicode> C", 86.5938591739732,
                                      86.5938591739732, 1, 1.29904541881187, 0.276627249732825, "<unicode> <unicode> D",
                                      "", 231.187284626765, 4, "", "", "<unicode> Squared terms",
                                      127.346622042302, 127.346622042302, 1, 1.91040158670906, 0.192104702264582,
                                      "<unicode> <unicode> A^2", 36.6285498239363, 36.6285498239363,
                                      1, 0.549486422021114, 0.47278544521242, "<unicode> <unicode> B^2",
                                      58.4510180703602, 58.4510180703602, 1, 0.876858104876022, 0.36752708377893,
                                      "<unicode> <unicode> C^2", 8.76109469016671, 8.76109469016671,
                                      1, 0.131430335009931, 0.723258299307619, "<unicode> <unicode> D^2",
                                      "", 472.935424491344, 6, "", "", "<unicode> Interaction terms",
                                      19.3381716403284, 19.3381716403284, 1, 0.290103288122267, 0.600003542981556,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>B", 191.360390164568,
                                      191.360390164568, 1, 2.87070977730543, 0.115976802494062, "<unicode> <unicode> A<unicode><unicode><unicode>C",
                                      66.8260371052421, 66.8260371052421, 1, 1.00249669187868, 0.336469973222273,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>D", 4.41153859417568,
                                      4.41153859417568, 1, 0.0661800854626668, 0.80134017944043, "<unicode> <unicode> B<unicode><unicode><unicode>C",
                                      190.244330544465, 190.244330544465, 1, 2.85396711044145, 0.116939413446734,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>D", 0.754956442564435,
                                      0.754956442564435, 1, 0.0113255456850063, 0.91700630379072,
                                      "<unicode> <unicode> C<unicode><unicode><unicode>D", 66.6596086018097,
                                      799.915303221716, 12, "", "", "Error", "", 1890.52907648959,
                                      26, "", "", "Total"))
})

test_that("24.2 Four continuous predictors BBD Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 104.898052433333, "", 3.99668370301669e-11, 4.71379566102908,
                                      "(Intercept)", 22.2534152892051, "", "A", -3.63039722166666,
                                      -7.26079444333332, 0.149425279207315, 2.35689783051454, "A",
                                      -1.54032863650865, 1, "B", -3.43304496666666, -6.86608993333332,
                                      0.170891581643576, 2.35689783051454, "B", -1.45659473322065,
                                      1, "C", -0.160785626666668, -0.321571253333336, 0.946734703534665,
                                      2.35689783051454, "C", -0.0682191754708207, 1, "D", 2.68629018,
                                      5.37258036, 0.276627249732826, 2.35689783051454, "D", 1.13975673668194,
                                      1, "A^2", 4.88646003083333, 9.77292006166666, 0.192104702264582,
                                      3.53534674577181, "A^2", 1.38217277744465, "", "B^2", -2.62065890416667,
                                      -5.24131780833334, 0.47278544521242, 3.53534674577181, "B^2",
                                      -0.741273513637925, "", "C^2", -3.31052350666666, -6.62104701333332,
                                      0.36752708377893, 3.53534674577181, "C^2", -0.936407018809674,
                                      "", "D^2", -1.28168063666666, -2.56336127333332, 0.723258299307619,
                                      3.53534674577181, "D^2", -0.362533219181265, "", "AB", 2.19875940249998,
                                      4.39751880499997, 0.600003542981556, 4.08226679070004, "A<unicode>B",
                                      0.538612372789808, 1, "AC", -6.9166536375, -13.833307275, 0.115976802494062,
                                      4.08226679070004, "A<unicode>C", -1.6943169058076, 1, "AD",
                                      -4.087359695, -8.17471939000001, 0.336469973222274, 4.08226679070004,
                                      "A<unicode>D", -1.00124756772673, 1, "BC", 1.05018315, 2.10036629999999,
                                      0.80134017944043, 4.08226679070004, "B<unicode>C", 0.257254903670788,
                                      1, "BD", 6.8964543525, 13.792908705, 0.116939413446734, 4.08226679070004,
                                      "B<unicode>D", 1.689368849731, 1, "CD", -0.434441147499996,
                                      -0.868882294999991, 0.917006303790719, 4.08226679070004, "C<unicode>D",
                                      -0.106421547089894, 1))
})

test_that("24.3 Four continuous predictors BBD Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 104.9 (Intercept) - 3.63 A - 3.43 B - 0.16 C + 2.69 D + 4.89 A^2 - 2.62 B^2 - 3.31 C^2 - 1.28 D^2 + 2.2 AB - 6.92 AC - 4.09 AD + 1.05 BC + 6.9 BD - 0.43 CD"
                                 ))
})

test_that("24.4 Four continuous predictors BBD Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.132951036731663, 0, 0.599823555414614, 8.16453358140008))
})

### Five continuous predictors BBD (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$designType <- "responseSurfaceDesign"
options$dependentResponseSurface <- "Result"
options$continuousFactorsResponseSurface <- c("A", "B", "C", "D", "E")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "manual"
options$codeFactorsManualTable <- list(
  list(predictors = "A", lowValue = "-1", highValue = "1"),
  list(predictors = "B", lowValue = "-1", highValue = "1"),
  list(predictors = "C", lowValue = "-1", highValue = "1"),
  list(predictors = "D", lowValue = "-1", highValue = "1"),
  list(predictors = "E", lowValue = "-1", highValue = "1")
)
options$squaredTermsCoded <- TRUE
options$tableEquation <- TRUE
options$rsmPredefinedModel <- TRUE
options$rsmPredefinedTerms <- "fullQuadratic"
options$modelTerms <- NULL
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/RSM5contBBD.csv", options)

test_that("25.1 Five continuous predictors BBD ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(68.7454290560407, 1374.90858112081, 20, 1.04584144910548, 0.452095870787799,
                                      "Model", "", 558.680485897376, 5, "", "", "<unicode> Linear terms",
                                      129.046322149616, 129.046322149616, 1, 1.96321405527439, 0.173462017177071,
                                      "<unicode> <unicode> A", 9.53159669575598, 9.53159669575598,
                                      1, 0.145006570436155, 0.706570367047832, "<unicode> <unicode> B",
                                      0.333181436287987, 0.333181436287987, 1, 0.00506877273045189,
                                      0.943808804277179, "<unicode> <unicode> C", 238.325047314577,
                                      238.325047314577, 1, 3.62569870119544, 0.0684640958200898, "<unicode> <unicode> D",
                                      181.444338301139, 181.444338301139, 1, 2.76035821299705, 0.109117645393389,
                                      "<unicode> <unicode> E", "", 248.132517729345, 5, "", "", "<unicode> Squared terms",
                                      0.0157157943820039, 0.0157157943820039, 1, 0.000239088320431022,
                                      0.987785956694379, "<unicode> <unicode> A^2", 9.32796249475518,
                                      9.32796249475518, 1, 0.141908632278136, 0.709569400869392, "<unicode> <unicode> B^2",
                                      37.7049176873545, 37.7049176873545, 1, 0.573614366715204, 0.455905961439862,
                                      "<unicode> <unicode> C^2", 176.000016795467, 176.000016795467,
                                      1, 2.67753238485005, 0.114302304857156, "<unicode> <unicode> D^2",
                                      25.0839049573867, 25.0839049573867, 1, 0.38160773552627, 0.542330859706051,
                                      "<unicode> <unicode> E^2", "", 568.095577494093, 10, "", "",
                                      "<unicode> Interaction terms", 27.2991735142916, 27.2991735142916,
                                      1, 0.415309171527529, 0.525156562174539, "<unicode> <unicode> A<unicode><unicode><unicode>B",
                                      0.757667134078247, 0.757667134078247, 1, 0.0115265800842996,
                                      0.915358836060124, "<unicode> <unicode> A<unicode><unicode><unicode>C",
                                      104.632102931823, 104.632102931823, 1, 1.59179441681812, 0.218715160783781,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>D", 66.6342362765354,
                                      66.6342362765354, 1, 1.01372334400123, 0.323658767224049, "<unicode> <unicode> A<unicode><unicode><unicode>E",
                                      14.9529347898301, 14.9529347898301, 1, 0.227482746179785, 0.637539532807536,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>C", 93.2070462332465,
                                      93.2070462332465, 1, 1.41798216460262, 0.244919918111232, "<unicode> <unicode> B<unicode><unicode><unicode>D",
                                      29.4735542921594, 29.4735542921594, 1, 0.448388571494304, 0.509239032472825,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>E", 9.70816249800328,
                                      9.70816249800328, 1, 0.147692710256946, 0.703999664080917, "<unicode> <unicode> C<unicode><unicode><unicode>D",
                                      211.413343349594, 211.413343349594, 1, 3.21628420107367, 0.0850160563448271,
                                      "<unicode> <unicode> C<unicode><unicode><unicode>E", 10.0173564745314,
                                      10.0173564745314, 1, 0.152396555747579, 0.699562125344016, "<unicode> <unicode> D<unicode><unicode><unicode>E",
                                      65.7321710808453, 1643.30427702113, 25, "", "", "Error", "",
                                      3018.21285814195, 45, "", "", "Total"))
})

test_that("25.2 Five continuous predictors BBD Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 99.5499281533334, "", 3.75552239017416e-21, 3.30988849461844,
                                      "(Intercept)", 30.0765201955268, "", "A", -2.83996393187502,
                                      -5.67992786375004, 0.173462017177071, 2.02688447933098, "A",
                                      -1.4011474066901, 1, "B", 0.771832101874981, 1.54366420374996,
                                      0.706570367047831, 2.02688447933098, "B", 0.380797282600803,
                                      1, "C", 0.144304676874998, 0.288609353749995, 0.943808804277181,
                                      2.02688447933098, "C", 0.071195313964133, 1, "D", -3.8594449675,
                                      -7.718889935, 0.0684640958200898, 2.02688447933098, "D", -1.90412675554844,
                                      1, "E", -3.367531906875, -6.73506381375, 0.109117645393389,
                                      2.02688447933098, "E", -1.66143257852886, 1, "A^2", 0.0424354581250189,
                                      0.0848709162500377, 0.987785956694349, 2.74441455864091, "A^2",
                                      0.0154624810568101, "", "B^2", 1.03384188145836, 2.06768376291671,
                                      0.709569400869393, 2.74441455864091, "B^2", 0.376707621741496,
                                      "", "C^2", -2.07854640354168, -4.15709280708335, 0.455905961439862,
                                      2.74441455864091, "C^2", -0.757373333776153, "", "D^2", -4.49073140937501,
                                      -8.98146281875002, 0.114302304857156, 2.74441455864091, "D^2",
                                      -1.63631671287989, "", "E^2", 1.69534581812499, 3.39069163624999,
                                      0.54233085970605, 2.74441455864091, "E^2", 0.617744069600244,
                                      "", "AB", -2.61243054999993, -5.22486109999985, 0.525156562174539,
                                      4.05376895866197, "A<unicode>B", -0.644444855303795, 1, "AC",
                                      0.435220385, 0.87044077, 0.915358836060127, 4.05376895866197,
                                      "A<unicode>C", 0.107361911701957, 1, "AD", -5.1144917375, -10.228983475,
                                      0.218715160783781, 4.05376895866197, "A<unicode>D", -1.26166335320406,
                                      1, "AE", -4.08148981, -8.16297962, 0.323658767224049, 4.05376895866197,
                                      "A<unicode>E", -1.00683829088947, 1, "BC", -1.93345124, -3.86690248,
                                      0.637539532807536, 4.05376895866197, "B<unicode>C", -0.476951513447421,
                                      1, "BD", 4.82718982, 9.65437964, 0.244919918111232, 4.05376895866197,
                                      "B<unicode>D", 1.1907905628626, 1, "BE", -2.7144775875, -5.42895517500001,
                                      0.509239032472826, 4.05376895866197, "B<unicode>E", -0.66961822816759,
                                      1, "CD", 1.5578962175, 3.115792435, 0.703999664080917, 4.05376895866197,
                                      "C<unicode>D", 0.384308092885052, 1, "CE", -7.270029975, -14.54005995,
                                      0.0850160563448271, 4.05376895866197, "C<unicode>E", -1.79340017873136,
                                      1, "DE", -1.582510385, -3.16502077, 0.699562125344017, 4.05376895866197,
                                      "D<unicode>E", -0.390380014534015, 1))
})

test_that("25.3 Five continuous predictors BBD Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 99.55 (Intercept) - 2.84 A + 0.77 B + 0.14 C - 3.86 D - 3.37 E + 0.04 A^2 + 1.03 B^2 - 2.08 C^2 - 4.49 D^2 + 1.7 E^2 - 2.61 AB + 0.44 AC - 5.11 AD - 4.08 AE - 1.93 BC + 4.83 BD - 2.71 BE + 1.56 CD - 7.27 CE - 1.58 DE"
                                 ))
})

test_that("25.4 Five continuous predictors BBD Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0423289680020797, 0, 0.467960537778933, 8.10753791732393))
})

### Six continuous predictors one discrete predictor BBD (verified with Minitab) ####

options <- analysisOptions("doeAnalysis")
options$designType <- "responseSurfaceDesign"
options$dependentResponseSurface <- "Result"
options$continuousFactorsResponseSurface <- c("A", "B", "C", "D", "E", "F")
options$fixedFactorsResponseSurface <- c("G")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "manual"
options$codeFactorsManualTable <- list(
  list(predictors = "A", lowValue = "-1", highValue = "1"),
  list(predictors = "B", lowValue = "-1", highValue = "1"),
  list(predictors = "C", lowValue = "-1", highValue = "1"),
  list(predictors = "D", lowValue = "-1", highValue = "1"),
  list(predictors = "E", lowValue = "-1", highValue = "1"),
  list(predictors = "F", lowValue = "-1", highValue = "1"),
  list(predictors = "G", lowValue = "1", highValue = "2")
)
options$squaredTermsCoded <- TRUE
options$tableEquation <- TRUE
options$rsmPredefinedModel <- TRUE
options$rsmPredefinedTerms <- "fullQuadratic"
options$modelTerms <- NULL
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/RSM6cont1discreteBBD.csv", options)

test_that("26.1 Six continuous predictors one discrete predictor BBD ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(58.4708923393394, 1988.01033953754, 34, 0.551827558106997, 0.971137841340843,
                                      "Model", "", 549.423409303307, 7, "", "", "<unicode> Linear terms",
                                      148.444118039262, 148.444118039262, 1, 1.40096297312432, 0.240402446156652,
                                      "<unicode> <unicode> A", 0.0915199562987254, 0.0915199562987254,
                                      1, 0.000863732910202329, 0.976634280417924, "<unicode> <unicode> B",
                                      14.0213308644215, 14.0213308644215, 1, 0.132328351129307, 0.71708146970929,
                                      "<unicode> <unicode> C", 110.238294645755, 110.238294645755,
                                      1, 1.04038995319587, 0.311099310674949, "<unicode> <unicode> D",
                                      126.514525709377, 126.514525709377, 1, 1.19399925320276, 0.278117305361186,
                                      "<unicode> <unicode> E", 137.360815129875, 137.360815129875,
                                      1, 1.29636268851172, 0.258601463002206, "<unicode> <unicode> F",
                                      12.7528049583179, 12.7528049583179, 1, 0.120356453230125, 0.729646327949738,
                                      "<unicode> <unicode> G", "", 410.21612922751, 6, "", "", "<unicode> Squared terms",
                                      0.520376967674565, 0.520376967674565, 1, 0.00491113338411936,
                                      0.94432202387229, "<unicode> <unicode> A^2", 19.0540329109544,
                                      19.0540329109544, 1, 0.179825209307916, 0.672770525307467, "<unicode> <unicode> B^2",
                                      18.2810826961831, 18.2810826961831, 1, 0.172530379137032, 0.679091074708391,
                                      "<unicode> <unicode> C^2", 217.91444787517, 217.91444787517,
                                      1, 2.05659932380207, 0.155817900500779, "<unicode> <unicode> D^2",
                                      70.8052141022808, 70.8052141022808, 1, 0.668234515261823, 0.416327875078363,
                                      "<unicode> <unicode> E^2", 83.640974675247, 83.640974675247,
                                      1, 0.789373874181105, 0.377207752036254, "<unicode> <unicode> F^2",
                                      "", 1028.37080100672, 21, "", "", "<unicode> Interaction terms",
                                      6.01209365702016, 6.01209365702016, 1, 0.0567400090734014, 0.812392086142887,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>B", 440.051747614463,
                                      440.051747614463, 1, 4.15305242679573, 0.0451836575337196, "<unicode> <unicode> A<unicode><unicode><unicode>C",
                                      55.7553964264334, 55.7553964264334, 1, 0.526199670132038, 0.470528062002619,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>D", 29.1987342835018,
                                      29.1987342835018, 1, 0.27556730528361, 0.601212071044823, "<unicode> <unicode> A<unicode><unicode><unicode>E",
                                      42.1082465570162, 42.1082465570162, 1, 0.39740270661291, 0.530402193847767,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>F", 0.900677572632048,
                                      0.900677572632048, 1, 0.0085002757040683, 0.926794116680112,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>G", 28.0203823202928,
                                      28.0203823202928, 1, 0.264446437097189, 0.608635030985745, "<unicode> <unicode> B<unicode><unicode><unicode>C",
                                      2.9595292963495, 2.9595292963495, 1, 0.027930988555341, 0.867733261887847,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>D", 1.5524259949816,
                                      1.5524259949816, 1, 0.0146512463155304, 0.903990061485116, "<unicode> <unicode> B<unicode><unicode><unicode>E",
                                      10.7700161050561, 10.7700161050561, 1, 0.10164359479131, 0.750776536375731,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>F", 23.2998448052022,
                                      23.2998448052022, 1, 0.219895677126106, 0.6405168538671, "<unicode> <unicode> B<unicode><unicode><unicode>G",
                                      43.5127263739269, 43.5127263739269, 1, 0.410657689336253, 0.523642550351348,
                                      "<unicode> <unicode> C<unicode><unicode><unicode>D", 28.9209380034335,
                                      28.9209380034335, 1, 0.27294556245144, 0.602944595064481, "<unicode> <unicode> C<unicode><unicode><unicode>E",
                                      85.5651474239567, 85.5651474239567, 1, 0.807533534600416, 0.371804999348072,
                                      "<unicode> <unicode> C<unicode><unicode><unicode>F", 13.6344901965695,
                                      13.6344901965695, 1, 0.128677486013748, 0.720842607120989, "<unicode> <unicode> C<unicode><unicode><unicode>G",
                                      5.37856088762692, 5.37856088762692, 1, 0.0507609513383817, 0.822373946082957,
                                      "<unicode> <unicode> D<unicode><unicode><unicode>E", 20.4271598082805,
                                      20.4271598082805, 1, 0.19278429428861, 0.66190620879747, "<unicode> <unicode> D<unicode><unicode><unicode>F",
                                      66.2263773865734, 66.2263773865734, 1, 0.625021077212417, 0.431748425997108,
                                      "<unicode> <unicode> D<unicode><unicode><unicode>G", 70.8830942457607,
                                      70.8830942457607, 1, 0.668969520452989, 0.416072825057049, "<unicode> <unicode> E<unicode><unicode><unicode>F",
                                      2.68492531643733, 2.68492531643733, 1, 0.0253393735205992, 0.873963928278907,
                                      "<unicode> <unicode> E<unicode><unicode><unicode>G", 50.5082867312094,
                                      50.5082867312094, 1, 0.476679308557414, 0.492119696740909, "<unicode> <unicode> F<unicode><unicode><unicode>G",
                                      105.958630518417, 7734.98002784443, 73, "", "", "Error", "",
                                      9722.99036738197, 107, "", "", "Total"))
})

test_that("26.2 Six continuous predictors one discrete predictor BBD Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 102.887750861667, "", 4.79064641984392e-47, 2.97151238875673,
                                      "(Intercept)", 34.6247087008493, "", "A", 1.75857493229167,
                                      3.51714986458334, 0.240402446156652, 1.48575619437837, "A",
                                      1.18362281708504, 1, "B", 0.0436653839583305, 0.0873307679166609,
                                      0.976634280417894, 1.48575619437836, "B", 0.0293893332725427,
                                      1, "C", -0.540472996249999, -1.0809459925, 0.717081469709289,
                                      1.48575619437837, "C", -0.363769640197348, 1, "D", -1.51546400104167,
                                      -3.03092800208333, 0.31109931067495, 1.48575619437837, "D",
                                      -1.01999507508413, 1, "E", 1.62348984770833, 3.24697969541667,
                                      0.278117305361185, 1.48575619437837, "E", 1.09270272865165,
                                      1, "F", -1.69165116041667, -3.38330232083333, 0.258601463002206,
                                      1.48575619437837, "F", -1.13857924120885, 1, "G", -0.343629927962963,
                                      -0.687259855925927, 0.729646327949739, 0.990504129585577, "G",
                                      -0.34692427593082, 1, "A^2", 0.159047485347219, 0.318094970694438,
                                      0.94432202387231, 2.26953007499613, "A^2", 0.0700794790514025,
                                      "", "B^2", 0.962412443263887, 1.92482488652777, 0.672770525307469,
                                      2.26953007499613, "B^2", 0.424058025873717, "", "C^2", -0.942689631944446,
                                      -1.88537926388889, 0.679091074708393, 2.26953007499613, "C^2",
                                      -0.415367763719128, "", "D^2", -3.25469867090278, -6.50939734180555,
                                      0.155817900500778, 2.26953007499613, "D^2", -1.43408483842556,
                                      "", "E^2", -1.85524125923611, -3.71048251847223, 0.416327875078362,
                                      2.26953007499613, "E^2", -0.817456124364987, "", "F^2", -2.01640291444444,
                                      -4.03280582888889, 0.377207752036255, 2.26953007499613, "F^2",
                                      -0.888467148622335, "", "AB", -0.612989276874998, -1.22597855375,
                                      0.812392086142893, 2.57340521632351, "A<unicode>B", -0.238201614338353,
                                      1, "AC", -5.244352603125, -10.48870520625, 0.0451836575337195,
                                      2.57340521632351, "A<unicode>C", -2.03790392972675, 1, "AD",
                                      -1.319983385625, -2.63996677125, 0.47052806200262, 1.81967227920319,
                                      "A<unicode>D", -0.725396215962033, 1, "AE", -1.350896329375,
                                      -2.70179265875, 0.601212071044825, 2.57340521632351, "A<unicode>E",
                                      -0.524945049775314, 1, "AF", 1.622271681875, 3.24454336375,
                                      0.530402193847765, 2.57340521632351, "A<unicode>F", 0.630398847249036,
                                      1, "AG", -0.136982174374999, -0.273964348749998, 0.926794116680109,
                                      1.48575619437837, "A<unicode>G", -0.0921969397760523, 1, "BC",
                                      1.323357055, 2.64671411, 0.608635030985748, 2.57340521632351,
                                      "B<unicode>C", 0.514243558148457, 1, "BD", -0.430082063124995,
                                      -0.860164126249991, 0.867733261887844, 2.57340521632351, "B<unicode>D",
                                      -0.167125666955564, 1, "BE", -0.220257377499999, -0.440514754999998,
                                      0.903990061485114, 1.81967227920319, "B<unicode>E", -0.121042332741612,
                                      1, "BF", -0.8204425675, -1.640885135, 0.750776536375733, 2.57340521632351,
                                      "B<unicode>F", -0.318815926188307, 1, "BG", 0.696716178541672,
                                      1.39343235708334, 0.6405168538671, 1.48575619437837, "B<unicode>G",
                                      0.468930354238352, 1, "CD", 1.649104423125, 3.29820884625001,
                                      0.523642550351346, 2.57340521632351, "C<unicode>D", 0.640825787040641,
                                      1, "CE", -1.34445476875, -2.6889095375, 0.602944595064479, 2.57340521632351,
                                      "C<unicode>E", -0.522441922563114, 1, "CF", -1.63520972875,
                                      -3.2704194575, 0.371804999348071, 1.81967227920319, "C<unicode>F",
                                      -0.898628696737656, 1, "CG", -0.532965176249998, -1.0659303525,
                                      0.720842607120992, 1.48575619437837, "C<unicode>G", -0.358716442352097,
                                      1, "DE", 0.579793114375005, 1.15958622875001, 0.82237394608296,
                                      2.57340521632351, "D<unicode>E", 0.225301911528463, 1, "DF",
                                      -1.129910389375, -2.25982077875, 0.661906208797466, 2.57340521632351,
                                      "D<unicode>F", -0.439072083248996, 1, "DG", -1.17461321104166,
                                      -2.34922642208333, 0.431748425997108, 1.48575619437837, "D<unicode>G",
                                      -0.790582745329302, 1, "EF", -2.10480245875, -4.20960491750001,
                                      0.416072825057048, 2.57340521632351, "E<unicode>F", -0.817905569398443,
                                      1, "EG", 0.236507809791665, 0.473015619583331, 0.8739639282789,
                                      1.48575619437837, "E<unicode>G", 0.159183458690286, 1, "FG",
                                      1.02579528833333, 2.05159057666666, 0.492119696740909, 1.48575619437837,
                                      "F<unicode>G", 0.690419661189782, 1))
})

test_that("26.3 Six continuous predictors one discrete predictor BBD Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 102.89 (Intercept) + 1.76 A + 0.04 B - 0.54 C - 1.52 D + 1.62 E - 1.69 F - 0.34 G + 0.16 A^2 + 0.96 B^2 - 0.94 C^2 - 3.25 D^2 - 1.86 E^2 - 2.02 F^2 - 0.61 AB - 5.24 AC - 1.32 AD - 1.35 AE + 1.62 AF - 0.14 AG + 1.32 BC - 0.43 BD - 0.22 BE - 0.82 BF + 0.7 BG + 1.65 CD - 1.34 CE - 1.64 CF - 0.53 CG + 0.58 DE - 1.13 DF - 1.17 DG - 2.1 EF + 0.24 EG + 1.03 FG"
                                 ))
})

test_that("26.4 Six continuous predictors one discrete predictor BBD Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0.200979745789412, 10.293620865294))
})

# Specific tests ####

## Plots ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$fixedFactorsFactorial <- c("A", "B", "C", "D", "E")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- TRUE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$plotNorm <- TRUE
options$plotHist <- TRUE
options$plotFitted <- TRUE
options$plotRunOrder <- TRUE
options$fourInOneResidualPlot <- TRUE
options$plotPareto <- TRUE
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

test_that("27.1 Factorial design plots Matrix residual plot matches", {
  plotName <- results[["results"]][["fourInOneResidualPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "matrix-residual-plot27")
})

test_that("27.2 Factorial design plots Residuals versus Fitted Values plot matches", {
  plotName <- results[["results"]][["plotFitted"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "residuals-versus-fitted-values27")
})

test_that("27.3 Factorial design plots Histogram of Residuals plot matches", {
  plotName <- results[["results"]][["plotHist"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-of-residuals27")
})

test_that("27.4 Factorial design plotsNormal Probability Plot of Residuals matches", {
  plotName <- results[["results"]][["plotNorm"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "normal-probability-plot-of-residuals27")
})

test_that("27.5 Factorial design plotsPareto Chart of Standardized Effects plot matches", {
  plotName <- results[["results"]][["plotPareto"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "pareto-chart-of-standardized-effects27")
})

test_that("27.6 Factorial design plotsResiduals versus Run Order plot matches", {
  plotName <- results[["results"]][["plotRunOrder"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "residuals-versus-run-order27")
})

test_that("27.7 Factorial design plotsANOVA table results match", {
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

test_that("27.8 Factorial design plotsCoded Coefficients table results match", {
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

test_that("27.9 Factorial design plotsRegression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 101.7 (Intercept) + 2.25 A - 2.76 B + 0.12 C + 1.42 D - 3.69 E + 1.45 AB + 1.22 AC - 0.76 AD - 1.36 AE - 2.05 BC + 1.63 BD - 0.24 BE + 1.27 CD - 2.57 CE + 2.47 DE"
                                 ))
})

test_that("27.10 Factorial design plotsModel Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.441199385076272, 0, 0.711586779394205, 6.71144161570719))
})

## Sums of squares types ####

### Type 1 ####

options <- analysisOptions("doeAnalysis")
options$designType <- "responseSurfaceDesign"
options$dependentResponseSurface <- "Result"
options$continuousFactorsResponseSurface <- c("A", "B", "C", "D", "E")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "manual"
options$codeFactorsManualTable <- list(
  list(predictors = "A", lowValue = "-1", highValue = "1"),
  list(predictors = "B", lowValue = "-1", highValue = "1"),
  list(predictors = "C", lowValue = "-1", highValue = "1"),
  list(predictors = "D", lowValue = "-1", highValue = "1"),
  list(predictors = "E", lowValue = "-1", highValue = "1")
)
options$squaredTermsCoded <- TRUE
options$sumOfSquaresType <- "type1"
options$tableEquation <- TRUE
options$rsmPredefinedModel <- TRUE
options$rsmPredefinedTerms <- "linearAndSquared"
options$modelTerms <- NULL
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/RSM5contCCD.csv", options)

test_that("28.1 Type 1 SS ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(45.0954304768421, 450.954304768421, 10, 0.420539272623761, 0.92886919055636,
                                      "Model", "", 341.185797996188, 5, "", "", "<unicode> Linear terms",
                                      43.5159978609834, 43.5159978609834, 1, 0.405810209470177, 0.527480997882405,
                                      "<unicode> <unicode> A", 167.72567415228, 167.72567415228, 1,
                                      1.56413260196178, 0.217823903668735, "<unicode> <unicode> B",
                                      17.7431526190489, 17.7431526190489, 1, 0.165464492024287, 0.686192201689509,
                                      "<unicode> <unicode> C", 54.1622602726813, 54.1622602726813,
                                      1, 0.505092362970769, 0.481108317086755, "<unicode> <unicode> D",
                                      58.0387130911947, 58.0387130911947, 1, 0.541242381529636, 0.465912547720501,
                                      "<unicode> <unicode> E", "", 109.768506772233, 5, "", "", "<unicode> Squared terms",
                                      23.7383463474594, 23.7383463474594, 1, 0.221372915186563, 0.640373727286971,
                                      "<unicode> <unicode> A^2", 7.39472140267591, 7.39472140267591,
                                      1, 0.0689597754595923, 0.794111504395272, "<unicode> <unicode> B^2",
                                      13.2252238626253, 13.2252238626253, 1, 0.123332363493701, 0.727162944861034,
                                      "<unicode> <unicode> C^2", 22.7705818446913, 22.7705818446913,
                                      1, 0.212347987920946, 0.647255519334776, "<unicode> <unicode> D^2",
                                      42.6396333147808, 42.6396333147808, 1, 0.397637636220154, 0.531649118010972,
                                      "<unicode> <unicode> E^2", 107.232388060906, 4610.99268661894,
                                      43, "", "", "Error", "", 5061.94699138736, 53, "", "", "Total"
                                 ))
})

test_that("28.2 Type 1 SS Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 95.7137721498586, "", 1.04311322261897e-31, 2.97660135246737,
                                      "(Intercept)", 32.1553882485874, "", "A", 1.00369820969494,
                                      4.74949992827646, 0.527480997882405, 1.57558437411744, "A",
                                      0.637032345701674, 1, "B", 1.97050911987227, 9.32444915523557,
                                      0.217823903668736, 1.57558437411744, "B", 1.25065287028887,
                                      1, "C", 0.640905605769827, 3.03276532650282, 0.686192201689509,
                                      1.57558437411744, "C", 0.406773268571432, 1, "D", -1.11976545692704,
                                      -5.29873014217874, 0.481108317086755, 1.57558437411744, "D",
                                      -0.710698503565872, 1, "E", 1.15914445813947, 5.48507157591596,
                                      0.465912547720501, 1.57558437411744, "E", 0.735691770736656,
                                      1, "A^2", 0.676630609752464, 3.20181604534866, 0.61645603146906,
                                      1.34107738608242, "A^2", 0.504542554198941, "", "B^2", 0.384382440063385,
                                      1.81889770637994, 0.775778221507103, 1.34107738608242, "B^2",
                                      0.286622117450097, "", "C^2", 0.482416016068627, 2.28279258803674,
                                      0.72081522517578, 1.34107738608242, "C^2", 0.359722728214715,
                                      "", "D^2", -0.57609061563766, -2.72606079319741, 0.669651253139691,
                                      1.34107738608242, "D^2", -0.42957298483762, "", "E^2", 0.845663489789393,
                                      4.00167963368341, 0.531649118010972, 1.34107738608242, "E^2",
                                      0.630585153821555, ""))
})

test_that("28.3 Type 1 SS Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 95.71 (Intercept) + 1 A + 1.97 B + 0.64 C - 1.12 D + 1.16 E + 0.68 A^2 + 0.38 B^2 + 0.48 C^2 - 0.58 D^2 + 0.85 E^2"
                                 ))
})

test_that("28.4 Type 1 SS Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0.0890871250796769, 10.3553072412607))
})

### Type 2 ####

options <- analysisOptions("doeAnalysis")
options$designType <- "responseSurfaceDesign"
options$dependentResponseSurface <- "Result"
options$continuousFactorsResponseSurface <- c("A", "B", "C", "D", "E")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "manual"
options$codeFactorsManualTable <- list(
  list(predictors = "A", lowValue = "-1", highValue = "1"),
  list(predictors = "B", lowValue = "-1", highValue = "1"),
  list(predictors = "C", lowValue = "-1", highValue = "1"),
  list(predictors = "D", lowValue = "-1", highValue = "1"),
  list(predictors = "E", lowValue = "-1", highValue = "1")
)
options$squaredTermsCoded <- TRUE
options$sumOfSquaresType <- "type2"
options$tableEquation <- TRUE
options$rsmPredefinedModel <- TRUE
options$rsmPredefinedTerms <- "linearAndSquared"
options$modelTerms <- NULL
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/RSM5contCCD.csv", options)

test_that("29.1 Type 2 SS ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(45.3596057822557, 453.596057822557, 10, 0.423002850188251, 0.92755442663839,
                                      "Model", "", 341.185797996188, 5, "", "", "<unicode> Linear terms",
                                      43.5159978609836, 43.5159978609836, 1, 0.405810209470179, 0.527480997882405,
                                      "<unicode> <unicode> A", 167.72567415228, 167.72567415228, 1,
                                      1.56413260196178, 0.217823903668735, "<unicode> <unicode> B",
                                      17.7431526190485, 17.7431526190485, 1, 0.165464492024283, 0.686192201689513,
                                      "<unicode> <unicode> C", 54.1622602726811, 54.1622602726811,
                                      1, 0.505092362970767, 0.481108317086755, "<unicode> <unicode> D",
                                      58.0387130911949, 58.0387130911949, 1, 0.541242381529639, 0.4659125477205,
                                      "<unicode> <unicode> E", "", 112.410259826369, 5, "", "", "<unicode> Squared terms",
                                      27.2974186686115, 27.2974186686115, 1, 0.254563188997593, 0.616456031469059,
                                      "<unicode> <unicode> A^2", 8.80938068797605, 8.80938068797605,
                                      1, 0.0821522382115795, 0.7757782215071, "<unicode> <unicode> B^2",
                                      13.8759183253933, 13.8759183253933, 1, 0.129400441194241, 0.720815225175776,
                                      "<unicode> <unicode> C^2", 19.7879088296077, 19.7879088296077,
                                      1, 0.1845329493023, 0.669651253139693, "<unicode> <unicode> D^2",
                                      42.6396333147804, 42.6396333147804, 1, 0.397637636220151, 0.531649118010973,
                                      "<unicode> <unicode> E^2", 107.232388060906, 4610.99268661894,
                                      43, "", "", "Error", "", 5064.5887444415, 53, "", "", "Total"
                                 ))
})

test_that("29.2 Type 2 SS Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 95.7137721498586, "", 1.04311322261897e-31, 2.97660135246737,
                                      "(Intercept)", 32.1553882485874, "", "A", 1.00369820969494,
                                      4.74949992827646, 0.527480997882405, 1.57558437411744, "A",
                                      0.637032345701674, 1, "B", 1.97050911987227, 9.32444915523557,
                                      0.217823903668736, 1.57558437411744, "B", 1.25065287028887,
                                      1, "C", 0.640905605769827, 3.03276532650282, 0.686192201689509,
                                      1.57558437411744, "C", 0.406773268571432, 1, "D", -1.11976545692704,
                                      -5.29873014217874, 0.481108317086755, 1.57558437411744, "D",
                                      -0.710698503565872, 1, "E", 1.15914445813947, 5.48507157591596,
                                      0.465912547720501, 1.57558437411744, "E", 0.735691770736656,
                                      1, "A^2", 0.676630609752464, 3.20181604534866, 0.61645603146906,
                                      1.34107738608242, "A^2", 0.504542554198941, "", "B^2", 0.384382440063385,
                                      1.81889770637994, 0.775778221507103, 1.34107738608242, "B^2",
                                      0.286622117450097, "", "C^2", 0.482416016068627, 2.28279258803674,
                                      0.72081522517578, 1.34107738608242, "C^2", 0.359722728214715,
                                      "", "D^2", -0.57609061563766, -2.72606079319741, 0.669651253139691,
                                      1.34107738608242, "D^2", -0.42957298483762, "", "E^2", 0.845663489789393,
                                      4.00167963368341, 0.531649118010972, 1.34107738608242, "E^2",
                                      0.630585153821555, ""))
})

test_that("29.3 Type 2 SS Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 95.71 (Intercept) + 1 A + 1.97 B + 0.64 C - 1.12 D + 1.16 E + 0.68 A^2 + 0.38 B^2 + 0.48 C^2 - 0.58 D^2 + 0.85 E^2"
                                 ))
})

test_that("29.4 Type 2 SS Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0.0890871250796769, 10.3553072412607))
})

## Coding squared terms ####

### Coded squared terms ####

options <- analysisOptions("doeAnalysis")
options$designType <- "responseSurfaceDesign"
options$dependentResponseSurface <- "Result"
options$continuousFactorsResponseSurface <- c("A", "B", "C", "D")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "manual"
options$codeFactorsManualTable <- list(
  list(predictors = "A", lowValue = "-1", highValue = "1"),
  list(predictors = "B", lowValue = "0", highValue = "30"),
  list(predictors = "C", lowValue = "-1", highValue = "1"),
  list(predictors = "D", lowValue = "-1", highValue = "1")
)
options$squaredTermsCoded <- TRUE
options$tableEquation <- TRUE
options$rsmPredefinedModel <- TRUE
options$rsmPredefinedTerms <- "linearAndSquared"
options$modelTerms <- NULL
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/RSM4contCCD_uncoded.csv", options)

test_that("30.1 Coded Squared Terms ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(53.6009309381646, 428.807447505317, 8, 0.734815921863529, 0.660416614783697,
                                      "Model", "", 140.181000812604, 4, "", "", "<unicode> Linear terms",
                                      63.1077346389266, 63.1077346389266, 1, 0.865144828527668, 0.362872709933773,
                                      "<unicode> <unicode> A", 37.4976813358419, 37.4976813358419,
                                      1, 0.514056244850083, 0.481287340427568, "<unicode> <unicode> B",
                                      34.037306875782, 34.037306875782, 1, 0.466617922336705, 0.502014598205835,
                                      "<unicode> <unicode> C", 5.53827796205383, 5.53827796205383,
                                      1, 0.0759243310702544, 0.785591679264418, "<unicode> <unicode> D",
                                      "", 288.626446692712, 4, "", "", "<unicode> Squared terms",
                                      65.4319337921556, 65.4319337921556, 1, 0.897007307657829, 0.35435731472199,
                                      "<unicode> <unicode> A^2", 2.0294343542887, 2.0294343542887,
                                      1, 0.0278215443240813, 0.869124697680983, "<unicode> <unicode> B^2",
                                      19.6252348513958, 19.6252348513958, 1, 0.269042622706554, 0.609397684905525,
                                      "<unicode> <unicode> C^2", 201.539843694872, 201.539843694872,
                                      1, 2.76291257343506, 0.111328352752227, "<unicode> <unicode> D^2",
                                      72.9447053926513, 1531.83881324568, 21, "", "", "Error", "",
                                      1960.64626075099, 29, "", "", "Total"))
})

test_that("30.2 Coded Squared Terms Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 103.801045518333, "", 1.16794507283658e-18, 3.48675363322034,
                                      "(Intercept)", 29.7701118109866, "", "A", -1.62156990083333,
                                      -6.4862796033333, 0.362872709933774, 1.74337681661017, "A",
                                      -0.930131618926949, 1, "B", -1.24996135499999, -4.99984541999997,
                                      0.481287340427568, 1.74337681661017, "B", -0.716977157830069,
                                      1, "C", 1.19089089333333, 4.7635635733333, 0.502014598205835,
                                      1.74337681661017, "C", 0.68309437293591, 1, "D", -0.480376499999993,
                                      -1.92150599999997, 0.785591679264418, 1.74337681661017, "D",
                                      -0.275543700835738, 1, "A^2", 1.54451910083333, 6.17807640333333,
                                      0.35435731472199, 1.63077968594997, "A^2", 0.947104697305334,
                                      "", "B^2", 0.272010650833332, 1.08804260333333, 0.869124697680985,
                                      1.63077968594997, "B^2", 0.166797914627493, "", "C^2", 0.845874313333332,
                                      3.38349725333333, 0.609397684905526, 1.63077968594997, "C^2",
                                      0.51869318744953, "", "D^2", -2.71068382541667, -10.8427353016667,
                                      0.111328352752227, 1.63077968594997, "D^2", -1.66220112303989,
                                      ""))
})

test_that("30.3 Coded Squared Terms Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 103.8 (Intercept) - 1.62 A - 1.25 B + 1.19 C - 0.48 D + 1.54 A^2 + 0.27 B^2 + 0.85 C^2 - 2.71 D^2"
                                 ))
})

test_that("30.4 Coded Squared Terms Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0.237414316588398, 8.54076726018519))
})


### Uncoded squared terms ####

options <- analysisOptions("doeAnalysis")
options$designType <- "responseSurfaceDesign"
options$dependentResponseSurface <- "Result"
options$continuousFactorsResponseSurface <- c("A", "B", "C", "D")
options$codeFactors <- TRUE
options$codeFactorsMethod <- "manual"
options$codeFactorsManualTable <- list(
  list(predictors = "A", lowValue = "-1", highValue = "1"),
  list(predictors = "B", lowValue = "0", highValue = "30"),
  list(predictors = "C", lowValue = "-1", highValue = "1"),
  list(predictors = "D", lowValue = "-1", highValue = "1")
)
options$squaredTermsCoded <- FALSE
options$tableEquation <- TRUE
options$rsmPredefinedModel <- TRUE
options$rsmPredefinedTerms <- "linearAndSquared"
options$modelTerms <- NULL
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/RSM4contCCD_uncoded.csv", options)

test_that("31.1 Uncoded Squared Terms ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(51.0593032861315, 408.474426289052, 8, 0.699972712361868, 0.688104762520746,
                                      "Model", "", 119.84797959634, 4, "", "", "<unicode> Linear terms",
                                      63.1077346389266, 63.1077346389266, 1, 0.865144828527668, 0.362872709933773,
                                      "<unicode> <unicode> A", 17.1646601195773, 17.1646601195773,
                                      1, 0.235310568836797, 0.632635670262947, "<unicode> <unicode> B",
                                      34.037306875782, 34.037306875782, 1, 0.466617922336705, 0.502014598205835,
                                      "<unicode> <unicode> C", 5.53827796205383, 5.53827796205383,
                                      1, 0.0759243310702544, 0.785591679264418, "<unicode> <unicode> D",
                                      "", 288.626446692712, 4, "", "", "<unicode> Squared terms",
                                      65.4319337921556, 65.4319337921556, 1, 0.897007307657829, 0.35435731472199,
                                      "<unicode> <unicode> A^2", 2.0294343542887, 2.0294343542887,
                                      1, 0.0278215443240813, 0.869124697680983, "<unicode> <unicode> B^2",
                                      19.6252348513958, 19.6252348513958, 1, 0.269042622706554, 0.609397684905525,
                                      "<unicode> <unicode> C^2", 201.539843694872, 201.539843694872,
                                      1, 2.76291257343506, 0.111328352752227, "<unicode> <unicode> D^2",
                                      72.9447053926513, 1531.83881324568, 21, "", "", "Error", "",
                                      1940.31323953473, 29, "", "", "Total"))
})

test_that("31.2 Uncoded Squared Terms Coded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 103.801045518333, "", 1.16794507283658e-18, 3.48675363322034,
                                      "(Intercept)", 29.7701118109866, "", "A", -1.62156990083333,
                                      -6.4862796033333, 0.362872709933774, 1.74337681661017, "A",
                                      -0.930131618926949, 1, "B", -1.24996135499999, -4.99984541999997,
                                      0.481287340427568, 1.74337681661017, "B", -0.716977157830069,
                                      1, "C", 1.19089089333333, 4.7635635733333, 0.502014598205835,
                                      1.74337681661017, "C", 0.68309437293591, 1, "D", -0.480376499999993,
                                      -1.92150599999997, 0.785591679264418, 1.74337681661017, "D",
                                      -0.275543700835738, 1, "A^2", 1.54451910083333, 6.17807640333333,
                                      0.35435731472199, 1.63077968594997, "A^2", 0.947104697305334,
                                      "", "B^2", 0.272010650833332, 1.08804260333333, 0.869124697680985,
                                      1.63077968594997, "B^2", 0.166797914627493, "", "C^2", 0.845874313333332,
                                      3.38349725333333, 0.609397684905526, 1.63077968594997, "C^2",
                                      0.51869318744953, "", "D^2", -2.71068382541667, -10.8427353016667,
                                      0.111328352752227, 1.63077968594997, "D^2", -1.66220112303989,
                                      ""))
})

test_that("31.3 Uncoded Squared Terms Regression Equation in Coded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 103.8 (Intercept) - 1.62 A - 1.25 B + 1.19 C - 0.48 D + 1.54 A^2 + 0.27 B^2 + 0.85 C^2 - 2.71 D^2"
                                 ))
})

test_that("31.4 Uncoded Squared Terms Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0.237414316588398, 8.54076726018519))
})

## Factorial analysis with nominal levels ####

options <- analysisOptions("doeAnalysis")
options$dependentFactorial <- "Result"
options$fixedFactorsFactorial <- c("A", "B", "C")
options$codeFactors <- FALSE
options$codeFactorsMethod <- "automatic"
options$tableEquation <- TRUE
options$tableAlias <- FALSE
options$highestOrder <- FALSE
options$histogramBinWidthType <- "doane"
options$modelTerms <- list(
  list(components = "A"),
  list(components = "B"),
  list(components = "C"),
  list(components = c("A", "B")),
  list(components = c("A", "C")),
  list(components = c("B", "C"))
)
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/2level3facFullText.csv", options)

test_that("32.1 Factorial analysis with nominal levels ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(30.3398635060475, 182.039181036285, 6, 0.316556719142333, 0.874163523074942,
                                      "Model", "", 125.387802560961, 3, "", "", "<unicode> Linear terms",
                                      53.6986344608876, 53.6986344608876, 1, 0.560274885349228, 0.590939896340302,
                                      "<unicode> <unicode> A", 68.8129870092555, 68.8129870092555,
                                      1, 0.717973348749309, 0.552492115731678, "<unicode> <unicode> B",
                                      2.87618109081815, 2.87618109081815, 1, 0.0300091808121394, 0.890801067082115,
                                      "<unicode> <unicode> C", "", 56.6513784753237, 3, "", "", "<unicode> Interaction terms",
                                      1.71696022965141, 1.71696022965141, 1, 0.0179142301377849, 0.915295640945133,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>B", 1.33812631708615,
                                      1.33812631708615, 1, 0.0139615946739632, 0.925124693647521,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>C", 53.5962919285861,
                                      53.5962919285861, 1, 0.559207075131571, 0.59123116801622, "<unicode> <unicode> B<unicode><unicode><unicode>C",
                                      95.8433723607233, 95.8433723607233, 1, "", "", "Error", "",
                                      277.882553397008, 7, "", "", "Total"))
})

test_that("32.2 Factorial analysis with nominal levels Uncoded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(106.66044882, "", 0.0206519176719196, 3.46127455499999, "(Intercept)",
                                      30.815367901377, "", 2.59081633999999, 5.18163267999998, 0.590939896340302,
                                      3.46127455499999, "A1", 0.748515120321045, 1, 2.93285243, 5.86570486000001,
                                      0.552492115731678, 3.46127455499999, "B1", 0.847333080169368,
                                      1, 0.599602065000003, 1.19920413000001, 0.890801067082115, 3.46127455499999,
                                      "C1", 0.173231581451361, 1, 0.463271010000007, 0.926542020000014,
                                      0.915295640945133, 3.46127455499999, "A1<unicode>B1", 0.133844051559212,
                                      1, -0.408981404999994, -0.817962809999989, 0.925124693647521,
                                      3.46127455499999, "A1<unicode>C1", -0.118159192084083, 1, 2.58834628499999,
                                      5.17669256999999, 0.59123116801622, 3.46127455499999, "B1<unicode>C1",
                                      0.747801494470004, 1))
})

test_that("32.3 Factorial analysis with nominal levels Discrete Predictor Levels table results match", {
  table <- results[["results"]][["tableCoefficientsLegend"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("High", "A1", "High", "B1", "High", "C1"))
})

test_that("32.4 Factorial analysis with nominal levels Regression Equation in Uncoded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 106.66 (Intercept) + 2.59 A1 + 2.93 B1 + 0.6 C1 + 0.46 A1<unicode>B1 - 0.41 A1<unicode>C1 + 2.59 B1<unicode>C1"
                                 ))
})

test_that("32.5 Factorial analysis with nominal levels Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0.655093955381241, 9.78996283755578))
})

## RSM analysis with nominal levels ####

options <- analysisOptions("doeAnalysis")
options$designType <- "responseSurfaceDesign"
options$dependentResponseSurface <- "Result"
options$continuousFactorsResponseSurface <- c("A", "B")
options$fixedFactorsResponseSurface <- c("C", "D")
options$codeFactors <- FALSE
options$codeFactorsMethod <- "manual"
options$codeFactorsManualTable <- list(
  list(predictors = "A", lowValue = "-1", highValue = "1"),
  list(predictors = "B", lowValue = "-1", highValue = "1"),
  list(predictors = "C", lowValue = "LevelA", highValue = "LevelB"),
  list(predictors = "D", lowValue = "WhateverLow", highValue = "WhateverHigh")
)
options$squaredTermsCoded <- TRUE
options$tableEquation <- TRUE
options$rsmPredefinedModel <- TRUE
options$rsmPredefinedTerms <- "fullQuadratic"
options$modelTerms <- NULL
set.seed(123)
results <- runAnalysis("doeAnalysis", "datasets/doeAnalysis/RSM2cont2discreteCCDText.csv", options)

test_that("33.1 RSM analysis with nominal levels ANOVA table results match", {
  table <- results[["results"]][["tableAnova"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(78.8155854449644, 945.787025339573, 12, 1.02124001802806, 0.447322259419835,
                                      "Model", "", 80.7371025092129, 4, "", "", "<unicode> Linear terms",
                                      2.33193684261232, 2.33193684261232, 1, 0.0302156890638417, 0.862817503307452,
                                      "<unicode> <unicode> A", 16.6942264995341, 16.6942264995341,
                                      1, 0.216312701036187, 0.644210743273365, "<unicode> <unicode> B",
                                      51.643067337493, 51.643067337493, 1, 0.669156572535934, 0.417855858206551,
                                      "<unicode> <unicode> C", 10.0678718295735, 10.0678718295735,
                                      1, 0.130452797510682, 0.719731612087039, "<unicode> <unicode> D",
                                      "", 145.119336540444, 2, "", "", "<unicode> Squared terms",
                                      2.65209420122892, 2.65209420122892, 1, 0.0343640755135464, 0.853806178423993,
                                      "<unicode> <unicode> A^2", 142.467242339215, 142.467242339215,
                                      1, 1.84599591963321, 0.181334819594722, "<unicode> <unicode> B^2",
                                      "", 719.930586289916, 6, "", "", "<unicode> Interaction terms",
                                      65.9619029932596, 65.9619029932596, 1, 0.854690536417314, 0.360388060663142,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>B", 186.679984933186,
                                      186.679984933186, 1, 2.41887527831368, 0.127211174949052, "<unicode> <unicode> A<unicode><unicode><unicode>C",
                                      34.7327647294642, 34.7327647294642, 1, 0.450044100773071, 0.50590166788665,
                                      "<unicode> <unicode> A<unicode><unicode><unicode>D", 112.72519454446,
                                      112.72519454446, 1, 1.46061821477157, 0.233436020916554, "<unicode> <unicode> B<unicode><unicode><unicode>C",
                                      299.346939991402, 299.346939991402, 1, 3.87873886449694, 0.0553656265712981,
                                      "<unicode> <unicode> B<unicode><unicode><unicode>D", 20.4837990981441,
                                      20.4837990981441, 1, 0.265415466270679, 0.609062330372558, "<unicode> <unicode> C<unicode><unicode><unicode>D",
                                      77.1763582053433, 3318.58340282976, 43, "", "", "Error", "",
                                      4264.37042816933, 55, "", "", "Total"))
})

test_that("33.2 RSM analysis with nominal levels Uncoded Coefficients table results match", {
  table <- results[["results"]][["tableCoefficients"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("", 102.924261345452, "", 2.83112726863751e-42, 1.79323216341033,
                                      "(Intercept)", 57.3959487486065, "", "A", 0.269950044176439,
                                      0.763534027073639, 0.86281750330745, 1.55298460860153, "A",
                                      0.173826606317451, 1, "B", 0.722284278001449, 2.04292844313806,
                                      0.644210743273365, 1.55298460860153, "B", 0.465094292629127,
                                      1, "C1", 0.960311215714292, 1.92062243142858, 0.41785585820655,
                                      1.17394601820818, "C1", 0.818019909620747, 1, "D1", -0.424008756428565,
                                      -0.84801751285713, 0.719731612087039, 1.17394601820818, "D1",
                                      -0.361182498898662, 1, "A^2", 0.29964051657501, 0.847511364530129,
                                      0.853806178423995, 1.61639762912004, "A^2", 0.185375498687246,
                                      "", "B^2", -2.19615795661686, -6.21167273308355, 0.181334819594722,
                                      1.61639762912004, "B^2", -1.35867432434458, "", "AB", -2.030423339375,
                                      -5.74290444629091, 0.360388060663142, 2.19625189535125, "A<unicode>B",
                                      -0.92449474656015, 1, "AC1", 2.41531561720225, 8.24640333676234,
                                      0.127211174949052, 1.55298460860153, "A<unicode>C1", 1.55527337735643,
                                      1, "AD1", -1.04182479241094, -3.55701233547728, 0.505901667886649,
                                      1.55298460860153, "A<unicode>D1", -0.670853263220113, 1, "BC1",
                                      -1.87687568326827, -6.40805441200253, 0.233436020916554, 1.55298460860153,
                                      "B<unicode>C1", -1.2085603893772, 1, "BD1", 3.05852773033027,
                                      10.4424668566467, 0.0553656265712982, 1.55298460860153, "B<unicode>D1",
                                      1.96945141206808, 1, "C1D1", 0.604799245000005, 2.41919698000002,
                                      0.609062330372557, 1.17394601820818, "C1<unicode>D1", 0.515184885522354,
                                      1))
})

test_that("33.3 RSM analysis with nominal levels Discrete Predictor Levels table results match", {
  table <- results[["results"]][["tableCoefficientsLegend"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("LevelA", "C1", "WhateverHigh", "D1"))
})

test_that("33.4 RSM analysis with nominal levels Regression Equation in Uncoded Units table results match", {
  table <- results[["results"]][["tableEquation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Result = 102.92  + 0.27 A + 0.72 B + 0.96 C1 - 0.42 D1 + 0.3 A^2 - 2.2 B^2 - 2.03 AB + 2.42 AC1 - 1.04 AD1 - 1.88 BC1 + 3.06 BD1 + 0.6 C1D1"
                                 ))
})

test_that("33.5 RSM analysis with nominal levels Model Summary table results match", {
  table <- results[["results"]][["tableSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.00551580589595602, 0, 0.222494175518657, 8.785007581405))
})
