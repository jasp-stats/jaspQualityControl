context("[Quality Control] MSA - Gauge Linearity")
.numDecimals <- 2
set.seed(1)

options <- analysisOptions("msaGaugeLinearity")
options$part <- "Part"
options$measurement <- "Measurement"
options$standard <- "Reference"
options$manualProcessVariation <- TRUE
options$manualProcessVariationValue <- 1
set.seed(1)
results <- runAnalysis("msaGaugeLinearity", "datasets/msaLinearityStudy/msaLinearity.csv", options, makeTests = T)

