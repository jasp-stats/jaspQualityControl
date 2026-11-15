context("[Quality Control] DoE Analysis - missing values of a response")

# Each response is analysed on its own runs: a missing value of one response must not drop that run from the
# analysis of the other responses (jasp-stats/INTERNAL-jasp#3428).
# Montgomery, Design and Analysis of Experiments, Table 6.10 (filtration rate, 2^4 design), model A, C, D, AC, AD
# (Table 6.13: Error SS 195.125 on 10 df, R^2 0.96595, predicted R^2 0.91284).

filtration <- expand.grid(A = c(-1, 1), B = c(-1, 1), C = c(-1, 1), D = c(-1, 1))
filtration$Rate  <- c(45, 71, 48, 65, 68, 60, 80, 65, 43, 100, 45, 104, 75, 86, 70, 96)
filtration$Rate2 <- filtration$Rate / 2 + c(1, -1, 2, 0, -2, 1, 0, 3, -1, 2, 1, -3, 0, 2, -1, 1)
filtration$Rate2[c(3, 12)] <- NA

filtrationOptions <- function(dependent) {
  options <- analysisOptions("doeAnalysis")
  options$designType <- "factorialDesign"
  options$dependentFactorial <- dependent
  options$continuousFactorsFactorial <- c("A", "B", "C", "D")
  options$fixedFactorsFactorial <- list()
  options$codeFactors <- TRUE
  options$codeFactorsMethod <- "automatic"
  options$highestOrder <- FALSE
  options$histogramBinWidthType <- "doane"
  options$plotRunOrder <- TRUE
  options$modelTerms <- list(list(components = "A"), list(components = "C"), list(components = "D"),
                             list(components = c("A", "C")), list(components = c("A", "D")))
  options
}

anovaRow <- function(results, dep, term) {
  rows <- results[["results"]][[dep]][["collection"]][[paste0(dep, "_tableAnova")]][["data"]]
  Filter(function(r) r[["terms"]] == term, rows)[[1]]
}
summaryTable <- function(results, dep) results[["results"]][[dep]][["collection"]][[paste0(dep, "_tableSummary")]]
footnoteTexts <- function(table) vapply(table[["footnotes"]], function(f) f[["text"]], character(1))

set.seed(1)
resultsAlone <- runAnalysis("doeAnalysis", filtration[c("A", "B", "C", "D", "Rate")], filtrationOptions("Rate"))
set.seed(1)
resultsBoth  <- runAnalysis("doeAnalysis", filtration, filtrationOptions(c("Rate", "Rate2")))

test_that("Response without missing values is analysed on all runs when another response has missing values", {
  errorRow <- anovaRow(resultsBoth, "Rate", "Error")
  expect_equal(errorRow[["adjss"]], 195.125, tolerance = 1e-6)
  expect_equal(errorRow[["df"]], 10)
  expect_equal(anovaRow(resultsBoth, "Rate", "Total")[["df"]], 15)
  summaryRow <- summaryTable(resultsBoth, "Rate")[["data"]][[1]]
  expect_equal(summaryRow[["rsq"]], 0.96595, tolerance = 1e-4)
  expect_equal(summaryRow[["predrsq"]], 0.91284, tolerance = 1e-4)
  expect_length(summaryTable(resultsBoth, "Rate")[["footnotes"]], 0)

  for (table in c("Rate_tableAnova", "Rate_tableCoefficients", "Rate_tableSummary"))
    expect_equal(resultsBoth[["results"]][["Rate"]][["collection"]][[table]][["data"]],
                 resultsAlone[["results"]][["Rate"]][["collection"]][[table]][["data"]])
})

test_that("Response with missing values is analysed on its own runs, with a footnote", {
  runs <- filtration[!is.na(filtration$Rate2), ]
  fit  <- lm(Rate2 ~ A + C + D + A:C + A:D, data = runs)
  errorRow <- anovaRow(resultsBoth, "Rate2", "Error")
  expect_equal(errorRow[["adjss"]], sum(resid(fit)^2), tolerance = 1e-6)
  expect_equal(errorRow[["df"]], 8)
  expect_equal(anovaRow(resultsBoth, "Rate2", "Total")[["df"]], 13)
  expect_equal(summaryTable(resultsBoth, "Rate2")[["data"]][[1]][["rsq"]], summary(fit)$r.squared, tolerance = 1e-6)
  expect_equal(footnoteTexts(summaryTable(resultsBoth, "Rate2")),
               "2 runs with a missing value of Rate2 were left out of the analysis of this response.")

  # residuals versus run order: the runs of Rate2 keep their run numbers
  plotName <- resultsBoth[["results"]][["Rate2"]][["collection"]][["Rate2_plotRunOrder"]][["data"]]
  plotData <- ggplot2::ggplot_build(resultsBoth[["state"]][["figures"]][[plotName]][["obj"]])[["data"]][[2]]
  expect_equal(plotData[["x"]], c(1, 2, 4:11, 13:16))
  expect_equal(plotData[["y"]], unname(resid(fit)), tolerance = 1e-6)
})

test_that("Runs with a missing predictor are left out of all responses, with a footnote", {
  data <- filtration
  data$A[5] <- NA
  set.seed(1)
  results <- runAnalysis("doeAnalysis", data, filtrationOptions(c("Rate", "Rate2")))
  expect_equal(anovaRow(results, "Rate", "Total")[["df"]], 14)
  expect_equal(anovaRow(results, "Rate2", "Total")[["df"]], 12)
  predictorNote <- "1 run with a missing value of a predictor, block or covariate was left out of the analysis of all responses."
  expect_equal(footnoteTexts(summaryTable(results, "Rate")), predictorNote)
  expect_equal(footnoteTexts(summaryTable(results, "Rate2")),
               c(predictorNote, "2 runs with a missing value of Rate2 were left out of the analysis of this response."))
})

test_that("Response optimizer uses the observed range of a response without its missing values", {
  options <- filtrationOptions(c("Rate", "Rate2"))
  options$responsesResponseOptimizer <- list(
    list(responseOptimizerGoal = "maximize", responseOptimizerImportance = 1, responseOptimizerLowerBound = 0,
         responseOptimizerTarget = 0, responseOptimizerUpperBound = 0, responseOptimizerWeight = 1, variable = "Rate2")
  )
  options$optimizationSolutionTable <- TRUE
  set.seed(1)
  results  <- runAnalysis("doeAnalysis", filtration, options)
  settings <- results[["results"]][["tableRoSettings"]][["data"]][[1]]
  expect_equal(settings[["lb"]], min(filtration$Rate2, na.rm = TRUE))
  expect_equal(settings[["target"]], max(filtration$Rate2, na.rm = TRUE))
})

# A response that misses every run of one level of a discrete factor cannot be fitted (lm() needs 2 levels). Only that
# response shows an error, the other responses are still analysed, also in the response optimizer.
filtrationFactor <- filtration
filtrationFactor$A <- factor(filtrationFactor$A)
filtrationFactor$RateNoHighA <- ifelse(filtrationFactor$A == 1, NA, filtrationFactor$Rate)

factorOptions <- function(dependent) {
  options <- filtrationOptions(dependent)
  options$continuousFactorsFactorial <- c("B", "C", "D")
  options$fixedFactorsFactorial <- list("A")
  options$modelTerms <- list(list(components = "A"), list(components = "C"), list(components = "D"))
  options
}
errorOf <- function(element) element[["error"]][["errorMessage"]]
roSetting <- function(variable)
  list(responseOptimizerGoal = "maximize", responseOptimizerImportance = 1, responseOptimizerLowerBound = 0,
       responseOptimizerTarget = 0.5, responseOptimizerUpperBound = 1, responseOptimizerWeight = 1, variable = variable)

set.seed(1)
resultsFactorAlone <- runAnalysis("doeAnalysis", filtrationFactor[c("A", "B", "C", "D", "Rate")], factorOptions("Rate"), view = FALSE)
set.seed(1)
resultsFactorBoth  <- runAnalysis("doeAnalysis", filtrationFactor, factorOptions(c("Rate", "RateNoHighA")), view = FALSE)
set.seed(1)
resultsFactorFirst <- runAnalysis("doeAnalysis", filtrationFactor, factorOptions(c("RateNoHighA", "Rate")), view = FALSE)

test_that("Response that loses all runs of a factor level shows its own error", {
  expect_identical(resultsFactorBoth[["status"]], "complete")
  expect_null(resultsFactorBoth[["results"]][["errorPlot"]])
  collection <- resultsFactorBoth[["results"]][["RateNoHighA"]][["collection"]]
  message    <- errorOf(collection[["RateNoHighA_tableSummary"]]) # the container shows its error once, in its first table
  expect_match(message, "RateNoHighA cannot be analysed", fixed = TRUE)
  expect_match(message, "the factor A has fewer than 2 levels left", fixed = TRUE)
  for (table in c("tableSummary", "tableAnova", "tableCoefficients"))
    expect_length(collection[[paste0("RateNoHighA_", table)]][["data"]], 0)
})

test_that("Response that loses all runs of a factor level does not affect the other responses", {
  # also when the response that could not be analysed comes first
  for (results in list(resultsFactorBoth, resultsFactorFirst)) {
    for (table in c("Rate_tableAnova", "Rate_tableCoefficients", "Rate_tableSummary"))
      expect_equal(results[["results"]][["Rate"]][["collection"]][[table]][["data"]],
                   resultsFactorAlone[["results"]][["Rate"]][["collection"]][[table]][["data"]])
    expect_length(results[["results"]][["Rate"]][["collection"]][["Rate_tableSummary"]][["data"]], 1)
  }
})

test_that("Response optimizer names the response that could not be analysed", {
  options <- factorOptions(c("Rate", "RateNoHighA"))
  options$responsesResponseOptimizer <- list(roSetting("Rate"), roSetting("RateNoHighA"))
  options$optimizationSolutionTable <- TRUE
  set.seed(1)
  results <- runAnalysis("doeAnalysis", filtrationFactor, options, view = FALSE)
  expect_identical(results[["status"]], "complete")
  expect_match(errorOf(results[["results"]][["tableRoSettings"]]),
               "The response optimizer cannot be used because the response RateNoHighA could not be analysed", fixed = TRUE)
  expect_null(results[["results"]][["tableRoSolution"]])
  expect_length(results[["results"]][["Rate"]][["collection"]][["Rate_tableSummary"]][["data"]], 1)

  options$responsesResponseOptimizer <- list(roSetting("Rate")) # the response that could be analysed still has its optimizer
  set.seed(1)
  results <- runAnalysis("doeAnalysis", filtrationFactor, options, view = FALSE)
  expect_identical(results[["status"]], "complete")
  expect_false(is.null(results[["results"]][["tableRoSolution"]]))
})

# The contour and surface plots of a response must not depend on the other responses: rsm gets the data of the fit from
# the frame where the fits were made, which holds the runs of the last response unless the fit carries its data.
contourOptions <- function(dependent) {
  options <- filtrationOptions(dependent)
  options$contourSurfacePlot          <- TRUE
  options$contourSurfacePlotType      <- "contourPlot"
  options$contourSurfacePlotVariables <- c("A", "D")
  options
}
contourValues <- function(results, dep) {
  plots <- results[["results"]][[dep]][["collection"]][[paste0(dep, "_contourSurfacePlot")]][["collection"]]
  results[["state"]][["figures"]][[plots[[1]][["data"]]]][["obj"]][["data"]][["z"]]
}

test_that("Contour plot of a response does not depend on the other responses or their order", {
  set.seed(1)
  rateAlone  <- runAnalysis("doeAnalysis", filtration[c("A", "B", "C", "D", "Rate")], contourOptions("Rate"), view = FALSE)
  set.seed(1)
  rate2Alone <- runAnalysis("doeAnalysis", filtration[c("A", "B", "C", "D", "Rate2")], contourOptions("Rate2"), view = FALSE)
  for (responses in list(c("Rate", "Rate2"), c("Rate2", "Rate"))) {
    set.seed(1)
    results <- runAnalysis("doeAnalysis", filtration, contourOptions(responses), view = FALSE)
    expect_equal(contourValues(results, "Rate"),  contourValues(rateAlone, "Rate"))
    expect_equal(contourValues(results, "Rate2"), contourValues(rate2Alone, "Rate2"))
  }
})

# A factor with 3 levels of which the response misses all runs of one level: the response is fitted on the other two
# levels, so it cannot be predicted at the third (not estimable) and the response optimizer must not stop the analysis.
threeLevels <- expand.grid(F = factor(c("a", "b", "c")), B = c(-1, 1), C = c(-1, 1))
noise       <- c(0.2, -0.1, 0.3, -0.3, 0.1, 0, -0.2, 0.25, -0.15, 0.1, -0.05, 0.2)
threeLevels$Y1 <- 10 + as.integer(threeLevels$F) + 2 * threeLevels$B + threeLevels$C + noise
threeLevels$Y2 <- ifelse(threeLevels$F == "c", NA, 5 + as.integer(threeLevels$F) - threeLevels$B + 2 * threeLevels$C + rev(noise))

test_that("Response optimizer skips the settings that a response cannot predict because a factor level has no runs", {
  options <- filtrationOptions(c("Y1", "Y2"))
  options$continuousFactorsFactorial <- c("B", "C")
  options$fixedFactorsFactorial      <- list("F")
  options$modelTerms                 <- list(list(components = "F"), list(components = "B"), list(components = "C"))
  options$responsesResponseOptimizer <- list(roSetting("Y1"), roSetting("Y2"))
  options$optimizationSolutionTable  <- TRUE
  set.seed(1)
  results <- runAnalysis("doeAnalysis", threeLevels, options, view = FALSE)
  expect_identical(results[["status"]], "complete")
  expect_length(results[["results"]][["Y2"]][["collection"]][["Y2_tableSummary"]][["data"]], 1)
  solution <- results[["results"]][["tableRoSolution"]][["data"]]
  expect_length(solution, 1)
  expect_true(solution[[1]][["F"]] %in% c("a", "b")) # Y2 cannot be predicted at F = c
})
