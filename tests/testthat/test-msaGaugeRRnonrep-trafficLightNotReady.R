context("[Quality Control] MSA Gauge non-replicable - traffic light chart before all variables are assigned")

# Ticking "Traffic light chart" before the measurement, operator and part variables were all assigned
# stopped the analysis with "object 'studyVar' not found". The chart is shown once all are assigned.

longData <- read.csv(testthat::test_path("datasets", "msaGaugeRRNested", "msaGaugeRRNested_long.csv"))
wideData <- read.csv(testthat::test_path("datasets", "msaGaugeRRNested", "msaGaugeRRNested_wide.csv"))

longVariables <- list(measurementLongFormat = "Result", operatorLongFormat = "Operator", partLongFormat = "Batch")
wideVariables <- list(dataFormat = "wideFormat", measurementsWideFormat = c("Result1", "Result2"),
                      operatorWideFormat = "Operator", partWideFormat = "Batch")

# GUI-equivalent options: QML defaults, traffic light chart ticked, only the given variables assigned
.trafficLightNotReadyOptions <- function(assigned, ...) {
  options <- analysisOptions("msaGaugeRRnonrep")
  options$testSet <- "jaspDefault"
  options$trafficLightChart <- TRUE
  for (name in names(assigned))
    options[[name]] <- assigned[[name]]
  extra <- list(...)
  for (name in names(extra))
    options[[name]] <- extra[[name]]
  options
}

.trafficLightNotReadyRun <- function(data, options) {
  set.seed(1)
  runAnalysis("msaGaugeRRnonrep", data, options, view = FALSE, quiet = TRUE)
}

.expectNoCrashNoTrafficLight <- function(results) {
  expect_identical(results[["status"]], "complete")
  expect_null(results[["results"]][["errorMessage"]])
  expect_null(results[["results"]][["trafficLightChart"]])
}

.trafficLightLabels <- function(results) {
  plotName <- results[["results"]][["trafficLightChart"]][["data"]]
  plotObj  <- results[["state"]][["figures"]][[plotName]][["obj"]]
  plots    <- if (inherits(plotObj, "ggplot")) list(plotObj) else plotObj$subplots
  unlist(lapply(plots, function(p) unlist(lapply(ggplot2::ggplot_build(p)$data, function(d) d$label))))
}

# Independent reference from a nested ANOVA (Operator / Batch, 2 replicates):
# %study var = sd(gauge) / sd(total) * 100, %tolerance = 6 * sd(gauge) / tolerance * 100
.trafficLightReference <- function(data, toleranceValue = 15) {
  data$Operator <- factor(data$Operator)
  data$Batch    <- factor(data$Batch)
  ms            <- summary(stats::aov(Result ~ Operator / Batch, data = data))[[1]][["Mean Sq"]]
  nReplicates   <- 2
  nPartsPerOp   <- 5
  varRepeat     <- ms[3]
  varReprod     <- max(0, (ms[1] - ms[2]) / (nPartsPerOp * nReplicates))
  varPart       <- max(0, (ms[2] - ms[3]) / nReplicates)
  varGauge      <- varRepeat + varReprod
  c(studyVar  = sprintf("%.2f%%", sqrt(varGauge / (varGauge + varPart)) * 100),
    tolerance = sprintf("%.2f%%", 6 * sqrt(varGauge) / toleranceValue * 100))
}

# Long format ####

for (variable in names(longVariables)) {
  test_that(sprintf("Long format, %s unassigned: traffic light chart does not crash the analysis", variable), {
    assigned <- longVariables[setdiff(names(longVariables), variable)]
    results  <- .trafficLightNotReadyRun(longData, .trafficLightNotReadyOptions(assigned))
    .expectNoCrashNoTrafficLight(results)
  })
}

test_that("Long format, no variables assigned: traffic light chart does not crash the analysis", {
  results <- .trafficLightNotReadyRun(longData, .trafficLightNotReadyOptions(list()))
  .expectNoCrashNoTrafficLight(results)
})

test_that("Long format, other plots ticked and part unassigned: no crash, other plots still shown empty", {
  options <- .trafficLightNotReadyOptions(longVariables[c("measurementLongFormat", "operatorLongFormat")],
                                          rChart = TRUE, xBarChart = TRUE)
  results <- .trafficLightNotReadyRun(longData, options)
  .expectNoCrashNoTrafficLight(results)
  expect_false(is.null(results[["results"]][["rChart"]]))
  expect_false(is.null(results[["results"]][["xBarChart"]]))
})

test_that("Long format, report with traffic light chart and part unassigned: no crash", {
  options <- .trafficLightNotReadyOptions(longVariables[c("measurementLongFormat", "operatorLongFormat")],
                                          report = TRUE, reportTrafficLightChart = TRUE)
  results <- .trafficLightNotReadyRun(longData, options)
  expect_identical(results[["status"]], "complete")
  expect_null(results[["results"]][["errorMessage"]])
})

test_that("Long format, all variables assigned: traffic light chart is shown with %study var and %tolerance", {
  options <- .trafficLightNotReadyOptions(longVariables, tolerance = TRUE, toleranceValue = 15)
  results <- .trafficLightNotReadyRun(longData, options)
  expect_identical(results[["status"]], "complete")
  expect_false(is.null(results[["results"]][["trafficLightChart"]]))
  reference <- .trafficLightReference(longData, toleranceValue = 15)
  expect_identical(reference, c(studyVar = "22.40%", tolerance = "11.36%"))
  expect_setequal(.trafficLightLabels(results), unname(reference))
})

# Wide format ####

for (variable in names(wideVariables)[-1]) {
  test_that(sprintf("Wide format, %s unassigned: traffic light chart does not crash the analysis", variable), {
    assigned <- wideVariables[setdiff(names(wideVariables), variable)]
    results  <- .trafficLightNotReadyRun(wideData, .trafficLightNotReadyOptions(assigned))
    .expectNoCrashNoTrafficLight(results)
  })
}

test_that("Wide format, only one measurement assigned: traffic light chart does not crash the analysis", {
  assigned <- modifyList(wideVariables, list(measurementsWideFormat = "Result1"))
  results  <- .trafficLightNotReadyRun(wideData, .trafficLightNotReadyOptions(assigned))
  .expectNoCrashNoTrafficLight(results)
})

test_that("Wide format, all variables assigned: traffic light chart is shown with %study var", {
  results <- .trafficLightNotReadyRun(wideData, .trafficLightNotReadyOptions(wideVariables))
  expect_identical(results[["status"]], "complete")
  expect_false(is.null(results[["results"]][["trafficLightChart"]]))
  # same data as the long format file, two measurement columns
  expect_identical(.trafficLightLabels(results), .trafficLightReference(longData)[["studyVar"]])
})
