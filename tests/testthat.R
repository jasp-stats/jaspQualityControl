library(jaspTools)
library(testthat)
.numDecimals <- 2

jaspTools::runTestsTravis(module = getwd())


### For manual testing of individual analyses ###
### ----------------------------------------- ###
# jaspTools::testAnalysis("attributesCharts")
# jaspTools::testAnalysis("doeAnalysis")
# jaspTools::testAnalysis("doeFactorial")
# jaspTools::testAnalysis("doeResponseSurfaceMethodology")
# jaspTools::testAnalysis("msaAttribute")
# jaspTools::testAnalysis("msaGaugeLinearity")
# jaspTools::testAnalysis("msaGaugeRR")
# jaspTools::testAnalysis("msaGaugeRRnonrep")
# jaspTools::testAnalysis("msaTestRetest")
# jaspTools::testAnalysis("msaType1Gauge")
# jaspTools::testAnalysis("probabilityOfDetection")
# jaspTools::testAnalysis("processCapabilityStudies")
# jaspTools::testAnalysis("rareEventCharts")
# jaspTools::testAnalysis("timeWeightedCharts")
# jaspTools::testAnalysis("variablesChartsIndividuals")
# jaspTools::testAnalysis("variablesChartsSubgroups")
