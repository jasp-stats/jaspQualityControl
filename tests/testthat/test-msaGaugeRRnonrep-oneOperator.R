context("[Quality Control] MSA - Gauge r&R non-replicable - one operator")

# With data from a single operator the nested ANOVA (Operator / Part) cannot be fitted. The analysis must show a
# clear error instead of stopping with "contrasts can be applied only to factors with 2 or more levels".

.gaugeRRnonrepOneOperatorError <- "At least 2 operators are required. The operator variable has only one level."

.gaugeRRnonrepOneOperatorOptions <- function(wideFormat, trafficLightChart = FALSE, report = FALSE) {
  options <- analysisOptions("msaGaugeRRnonrep")
  options$testSet <- "jaspDefault"
  if (wideFormat) {
    options$dataFormat             <- "wideFormat"
    options$partWideFormat         <- "Batch"
    options$operatorWideFormat     <- "Operator"
    options$measurementsWideFormat <- c("Result1", "Result2")
  } else {
    options$dataFormat            <- "longFormat"
    options$partLongFormat        <- "Batch"
    options$operatorLongFormat    <- "Operator"
    options$measurementLongFormat <- "Result"
  }
  options$trafficLightChart <- trafficLightChart
  options$report            <- report
  options
}

.gaugeRRnonrepExpectOneOperatorError <- function(results) {
  expect_identical(results[["status"]], "complete")
  expect_null(results[["results"]][["errorMessage"]])
  expect_identical(results[["results"]][["gaugeRRNonRep"]][["error"]][["errorMessage"]], .gaugeRRnonrepOneOperatorError)
  # nothing else is computed (no tables, charts or report)
  expect_identical(setdiff(names(results[["results"]]), c(".meta", "name")), "gaugeRRNonRep")
}

longData <- read.csv(test_path("datasets", "msaGaugeRRNested", "msaGaugeRRNested_long.csv"))
wideData <- read.csv(test_path("datasets", "msaGaugeRRNested", "msaGaugeRRNested_wide.csv"))

test_that("Long format with one operator shows an error instead of crashing (default options)", {
  oneOperator <- longData[longData[["Operator"]] == "A", ]
  expect_length(unique(oneOperator[["Operator"]]), 1)
  results <- runAnalysis("msaGaugeRRnonrep", oneOperator, .gaugeRRnonrepOneOperatorOptions(wideFormat = FALSE))
  .gaugeRRnonrepExpectOneOperatorError(results)
})

test_that("Long format with one operator shows an error with the traffic light chart and the report ticked", {
  oneOperator <- longData[longData[["Operator"]] == "A", ]
  options <- .gaugeRRnonrepOneOperatorOptions(wideFormat = FALSE, trafficLightChart = TRUE, report = TRUE)
  results <- runAnalysis("msaGaugeRRnonrep", oneOperator, options)
  .gaugeRRnonrepExpectOneOperatorError(results)
})

test_that("Wide format with one operator shows an error instead of crashing", {
  oneOperator <- wideData[wideData[["Operator"]] == "A", c("Operator", "Batch", "Result1", "Result2")]
  expect_length(unique(oneOperator[["Operator"]]), 1)
  results <- runAnalysis("msaGaugeRRnonrep", oneOperator, .gaugeRRnonrepOneOperatorOptions(wideFormat = TRUE))
  .gaugeRRnonrepExpectOneOperatorError(results)
})

test_that("Two operators still give the nested ANOVA without the one-operator error", {
  twoOperators <- longData[longData[["Operator"]] %in% c("A", "B"), ]
  results <- runAnalysis("msaGaugeRRnonrep", twoOperators, .gaugeRRnonrepOneOperatorOptions(wideFormat = FALSE))
  expect_identical(results[["status"]], "complete")
  expect_null(results[["results"]][["gaugeRRNonRep"]][["error"]])

  # independent nested ANOVA: Operator, Part within Operator, Repeatability
  twoOperators[["Operator"]] <- factor(twoOperators[["Operator"]])
  twoOperators[["Batch"]]    <- factor(twoOperators[["Batch"]])
  reference  <- summary(aov(Result ~ Operator / Batch, data = twoOperators))[[1]]
  anovaTable <- results[["results"]][["gaugeRRNonRep"]][["collection"]][["gaugeRRNonRep_Table1"]][["data"]]
  expect_equal(vapply(anovaTable[1:3], function(row) row[["DF"]], numeric(1)), unname(reference[["Df"]]))
  expect_equal(vapply(anovaTable[1:3], function(row) row[["SS"]], numeric(1)), unname(reference[["Sum Sq"]]), tolerance = 1e-3)
})
