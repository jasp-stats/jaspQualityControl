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
