library("jaspTools")

options <- analysisOptions("msaGaugeRR")
options$operators <- "Operator"
options$parts <- "Part"
options$measurements <- c("1", "2", "3")
options$alphaForANOVA <- 1
jaspTools::runAnalysis("msaGaugeRR", "gaugeRRwide.csv", options, makeTests = TRUE)
