context("Attribute Agreement Analysis")


options <- analysisOptions("msaAttribute")
options$AAAdataFormat <- "AAAwideFormat"
options$operators <- "Operator"
options$parts <- "Part"
options$measurements <- c("Repeat.1", "Repeat.2", "Repeat.3")
options$standard <- "Reference"
options$AAAcohensKappa <- FALSE
options$AAAfleissKappa <- FALSE
options$PositiveRef <- "Yes"
options$AAAkendallTau <- FALSE

results <- runAnalysis("msaAttribute", "AAARow.csv", options)





