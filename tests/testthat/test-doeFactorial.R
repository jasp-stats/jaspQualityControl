context("DoE factorialAnalysis")

data <- read.csv("DoEFactorialAnalysis.csv")
options <- analysisOptions("factorialAnalysis")
options$FAresponse <- "Cycle.time"
options$FAassignedFactors <- c("Rough.feed", "Air.feed", "Grinding.length", "Fine.feed", "Stock.allowance")
options$FArunOrder <- "RunOrder"
options$intOrder <- 2

results <- runAnalysis("factorialAnalysis", data, options)




