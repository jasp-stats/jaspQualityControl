context("[Quality Control] Stages: a recurring stage value forms a new stage")

# Stage A in rows 1-25, B in rows 26-45 and A again in rows 46-70. A new stage starts at each change of the
# stage value (Minitab: "with each new value"), so there are three stages, in row order, labelled A, B and A.
set.seed(3425)
y <- c(rnorm(25, 10, 0.9), rnorm(20, 10.3, 0.9), rnorm(25, 9.8, 0.9))
y[30] <- 14 # beyond the limits of stage B (data row 30)
stageValues <- rep(c("A", "B", "A"), c(25, 20, 25))
runLengths  <- c(25, 20, 25)
runs        <- rep(1:3, runLengths)
recurringStageData <- data.frame(y = y, stage = stageValues, time = paste0("t", seq_along(y)))

# independent per-stage reference: individuals chart with sigma = mean moving range / d2 within each stage
reference <- t(sapply(split(y, runs), function(v) {
  sigma <- mean(abs(diff(v))) / 1.128
  c(center = mean(v), LCL = mean(v) - 3 * sigma, UCL = mean(v) + 3 * sigma)
}))

.recurringStagesIndividuals <- function(axisLabels = "") {
  options <- jaspTools::analysisOptions("variablesChartsIndividuals")
  options[["measurement"]]               <- "y"
  options[["stage"]]                     <- "stage"
  options[["axisLabels"]]                <- axisLabels
  options[["xmrChart"]]                  <- TRUE
  options[["xmrChartMovingRangeLength"]] <- 2
  options[["controlLimitsNumberOfSigmas"]] <- 3
  set.seed(1)
  jaspTools::runAnalysis("variablesChartsIndividuals", recurringStageData, options, view = FALSE)
}

.layerData <- function(plot, geom) {
  for (layer in plot$layers)
    if (inherits(layer$geom, geom))
      return(layer$data)
  NULL
}

results <- .recurringStagesIndividuals()
plotName <- results[["results"]][["Ichart"]][["collection"]][["Ichart_plot"]][["data"]]
subplots <- results[["state"]][["figures"]][[plotName]][["obj"]]$subplots

test_that("Individuals chart computes limits per stage period, in row order", {
  expect_identical(results[["status"]], "complete")
  clData <- subplots[[1]]$data
  # the step of data row r starts at r - 0.5; the closing step of the previous stage sits at the same x, so take the last
  stepOfRow <- vapply(seq_along(y) - 0.5, function(x) max(which(clData$subgroup == x)), numeric(1))
  for (statistic in c("center", "LCL", "UCL")) {
    perRow <- clData[[statistic]][stepOfRow]
    expect_equal(unname(perRow), unname(rep(reference[, statistic], runLengths)), tolerance = 1e-8, info = statistic)
  }

  points <- .layerData(subplots[[1]], "GeomJaspPoint")
  expect_equal(points$plotStatistic[order(points$subgroup)], y) # plotted in row order
  expect_length(unique(points$stage), 3) # the two A periods are not joined by one line

  stageLabels <- .layerData(subplots[[1]], "GeomText")
  expect_equal(as.character(stageLabels$label), c("A", "B", "A"))
  expect_equal(stageLabels$separationLine, c(NA, 25.5, 45.5))
})

test_that("Moving range chart has no moving range across a stage change", {
  points <- .layerData(subplots[[2]], "GeomJaspPoint")
  mr <- points$plotStatistic[order(points$subgroup)]
  expected <- c(NA, abs(diff(y)))
  expected[c(26, 46)] <- NA # first observation of a stage
  expect_equal(mr, expected, tolerance = 1e-8)
})

test_that("Test table numbers points by data row", {
  table <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(table, list("B", "Point 30"))

  resultsWithTime <- .recurringStagesIndividuals(axisLabels = "time")
  tableWithTime <- resultsWithTime[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["data"]]
  jaspTools::expect_equal_tables(tableWithTime, list("B", "Point 30 (t30)"))
})

test_that("Process capability reports each stage period separately", {
  options <- jaspTools::analysisOptions("processCapabilityStudies")
  options[["testSet"]]                       <- "jaspDefault"
  options[["dataFormat"]]                    <- "wideFormat"
  options[["measurementsWideFormat"]]        <- "y"
  options[["stagesWideFormat"]]              <- "stage"
  options[["capabilityStudyType"]]           <- "normalCapabilityAnalysis"
  options[["lowerSpecificationLimit"]]       <- TRUE
  options[["upperSpecificationLimit"]]       <- TRUE
  options[["lowerSpecificationLimitValue"]]  <- 7
  options[["upperSpecificationLimitValue"]]  <- 13
  options[["processCapabilityTable"]]        <- TRUE
  options[["processCapabilityTableCi"]]      <- FALSE
  options[["processCapabilityPlot"]]         <- FALSE
  options[["controlChart"]]                  <- FALSE
  options[["histogram"]]                     <- FALSE
  options[["probabilityPlot"]]               <- FALSE
  set.seed(1)
  capResults <- jaspTools::runAnalysis("processCapabilityStudies", recurringStageData[c("y", "stage")], options, view = FALSE)
  expect_identical(capResults[["status"]], "complete")
  capability <- capResults[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]]

  summaryRows <- capability[["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]]
  stageRows <- summaryRows[vapply(summaryRows, function(row) !grepl("Change", row[["stage"]]), logical(1))]
  expect_equal(vapply(stageRows, function(row) row[["stage"]], character(1)), c("A (BL)", "B", "A"))
  expect_equal(vapply(stageRows, function(row) as.numeric(row[["n"]]), numeric(1)), runLengths)
  expect_equal(vapply(stageRows, function(row) as.numeric(row[["mean"]]), numeric(1)), unname(reference[, "center"]),
               tolerance = 1e-3)

  # one set of columns per stage period (columns named by stage number, so the two A periods do not overwrite each other)
  performance <- capability[["capabilityAnalysis_normalCapabilityAnalysis_capabilityTablePerformance"]][["data"]]
  for (column in c("observed1", "observed2", "observed3", "observedComparison2", "observedComparison3"))
    expect_false(is.null(performance[[3]][[column]]), info = column)
  observedTotal <- vapply(split(y < 7 | y > 13, runs), mean, numeric(1)) * 1e6
  expect_equal(unname(vapply(paste0("observed", 1:3), function(column) as.numeric(performance[[3]][[column]]), numeric(1))),
               unname(observedTotal))
})
