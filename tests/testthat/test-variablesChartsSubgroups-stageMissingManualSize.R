context("[Quality Control] Manual subgroup size with missing stages")

# jasp-stats/INTERNAL-jasp#3418: rows without a stage must not shift the subgroups formed by a manual subgroup size.
# Expected values: Diameter rows 1-100 cut into 20 subgroups of 5, observations with empty StageMissing15 left out.

.stageMissingCsv <- "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormatDebug.csv"

.stageMissingPlotData <- function(results, plotKey) {
  plotObj <- results[["state"]][["figures"]][[plotKey]][["obj"]]
  if (!inherits(plotObj, "ggplot"))
    plotObj <- plotObj$subplots[[1]]
  built   <- ggplot2::ggplot_build(plotObj)
  isPoint <- vapply(plotObj$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))
  labels  <- unlist(lapply(built$data, function(d) d[["label"]]))
  list(points = built$data[[which(isPoint)[1]]], labels = as.character(labels))
}

test_that("Variables charts for subgroups: manual size with missing stages keeps 20 subgroups", {
  options <- analysisOptions("variablesChartsSubgroups")
  options$testSet                 <- "jaspDefault"
  options$measurementLongFormat   <- "Diameter"
  options$stagesLongFormat        <- "StageMissing15"
  options$subgroupSizeType        <- "manual"
  options$manualSubgroupSizeValue <- 5
  options$chartType               <- "xBarAndR"
  results <- runAnalysis("variablesChartsSubgroups", .stageMissingCsv, options)

  charts   <- results[["results"]][["controlCharts"]][["collection"]]
  plotData <- .stageMissingPlotData(results, charts[["controlCharts_plot"]][["data"]])
  expect_equal(nrow(plotData$points), 20)
  expect_equal(plotData$points$y[1], -7.975)
  expect_true(all(c("-7.356", "-6.796", "CL = -7.136") %in% plotData$labels))

  note <- charts[["controlCharts_plotNote"]][["text"]]
  expect_false(grepl("more than one stage", note))
  expect_true(grepl("Removed 15 observation(s) that were not assigned to any stage", note, fixed = TRUE))
})

test_that("Process capability: manual size with missing stages keeps 20 subgroups", {
  options <- analysisOptions("processCapabilityStudies")
  options$testSet                      <- "jaspDefault"
  options$measurementLongFormat        <- "Diameter"
  options$stagesLongFormat             <- "StageMissing15"
  options$subgroupSizeType             <- "manual"
  options$manualSubgroupSizeValue      <- 5
  options$capabilityStudyType          <- "normalCapabilityAnalysis"
  options$controlChartType             <- "xBarR"
  options$lowerSpecificationLimit      <- TRUE
  options$upperSpecificationLimit      <- TRUE
  options$lowerSpecificationLimitValue <- -15
  options$upperSpecificationLimitValue <- 0
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", .stageMissingCsv, options)

  xBar     <- results[["results"]][["xBar"]][["collection"]]
  plotData <- .stageMissingPlotData(results, xBar[["xBar_plot"]][["data"]])
  expect_equal(nrow(plotData$points), 20)
  expect_equal(plotData$points$y[1], -7.975)
  expect_true(all(c("-7.356", "-6.796", "CL = -7.136") %in% plotData$labels))
  expect_false(grepl("more than one stage", xBar[["xBar_plotNote"]][["text"]]))
})

test_that("Time weighted charts: manual size with missing stages keeps 20 subgroups", {
  options <- analysisOptions("timeWeightedCharts")
  options$measurementLongFormat   <- "Diameter"
  options$stagesLongFormat        <- "StageMissing15"
  options$subgroupSizeType        <- "manual"
  options$manualSubgroupSizeValue <- 5
  options$cumulativeSumChart      <- TRUE
  results <- runAnalysis("timeWeightedCharts", .stageMissingCsv, options)

  cusum   <- results[["results"]][["CusumChart"]][["collection"]]
  plotObj <- results[["state"]][["figures"]][[cusum[["CusumChart_plot"]][["data"]]]][["obj"]]
  if (!inherits(plotObj, "ggplot"))
    plotObj <- plotObj$subplots[[1]]
  built   <- ggplot2::ggplot_build(plotObj)
  isPoint <- vapply(plotObj$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))
  # one upper and one lower CUSUM point per subgroup
  nPoints <- sum(vapply(built$data[isPoint], nrow, integer(1)))
  expect_equal(nPoints, 2 * 20)
  expect_false(grepl("more than one stage", cusum[["CusumChart_plotNote"]][["text"]]))
})
