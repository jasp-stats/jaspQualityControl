context("DoE factorialAnalysis")

options <- analysisOptions("doeFactorial")
options$FAresponse <- "Yield"
options$FAassignedFactors <- c("Aperture.setting" ,"Exposure.time", "Develop.time" ,   "Mask.dimension",  "Etch.time")
options$FArunOrder <- "RunOrder"
options$modelTerms <- list(
  list(components="Aperture.setting"),
  list(components="Exposure.time"),
  list(components="Develop.time"),
  list(components="Mask.dimension"),
  list(components="Etch.time"))

results <- runAnalysis("doeFactorial", read.csv("doeFactorialAnalysis.csv"), options)






