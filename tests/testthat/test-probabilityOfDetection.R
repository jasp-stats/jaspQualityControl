options <- analysisOptions("probabilityOfDetection")
options$covariate <- "target.size"
options$horizontalAsymptotes <- list(
  list(horizontalAsymptoteName = "Asymptote 1", horizontalAsymptoteValue = 0.5, value = "#"),
  list(horizontalAsymptoteName = "Asymptote 2", horizontalAsymptoteValue = 0.25, value = "#2"),
  list(horizontalAsymptoteName = "Asymptote 3", horizontalAsymptoteValue = 0.75, value = "#3")
)
options$outcome <- c("test1", "test2")
options$detectionPlotDataDisplayType <- "points"
options$verticalAsymptotes <- list(
  list(value = "#", verticalAsymptoteValue = 0.1, verticallAsymptoteName = "Asymptote 1"),
  list(value = "#2", verticalAsymptoteValue = 0.2, verticallAsymptoteName = "Asymptote 2"),
  list(value = "#3", verticalAsymptoteValue = 0.3, verticallAsymptoteName = "Asymptote 3")
)
options$detectionPlotCi <- TRUE
options$modelFitTable <- TRUE
options$xAxisTicksType <- "dataAndModelBased"
set.seed(1)
results <- runAnalysis("probabilityOfDetection", "EXAMPLE 8 hit miss repeated measures 24.csv", options)

test_that("Vertical asymptotes table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_asymptoteContainer"]][["collection"]][["mainContainer_asymptoteContainer_xAsymptotesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Asymptote 1", 0.1, 0.663466358381688, "Asymptote 2", 0.2, 0.908621215337589,
                                      "Asymptote 3", 0.3, 0.980450259512257))
})

test_that("Horizontal asymptotes table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_asymptoteContainer"]][["collection"]][["mainContainer_asymptoteContainer_yAsymptotesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Asymptote 1", 0.0580637232550454, 0.5, "Asymptote 2", -0.00981973596020396,
                                      0.25, "Asymptote 3", 0.125953355588338, 0.75))
})

test_that("Detection plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_detectionPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "detection-plot")
})

test_that("Fit table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_fitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(290.470583966239))
})

test_that("Parameter estimates table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.939354906227213, "Intercept", 16.1813493626685, "target.size"
                                 ))
})

options <- analysisOptions("probabilityOfDetection")
options$covariate <- "depth.inch"
options$horizontalAsymptotes <- list()
options$logTransformedCovariate <- TRUE
options$outcome <- "hit.miss"
options$detectionPlotDataDisplayType <- "points"
options$verticalAsymptotes <- list()
options$detectionPlotCi <- TRUE
options$xAxisTicksType <- "dataAndModelBased"
set.seed(1)
results <- runAnalysis("probabilityOfDetection", "EXAMPLE 3 hm.csv", options)


test_that("Detection plot matches for example 3", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_detectionPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "detection-plot-ex3")
})

test_that("Parameter estimates table results matches for example 3", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(17.6573206618451, "Intercept", 4.39007841700679, "depth.inch"))
})
