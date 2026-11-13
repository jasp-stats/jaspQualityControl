context("[Quality Control] DoE Analysis - alias names")

# Montgomery (2017), Design and Analysis of Experiments, Example 6.2 (Table 6.10): filtration rate, 2^4 design in
# standard order. Montgomery's final model: y = 70.06 + 10.81 x1 + 4.94 x3 + 7.31 x4 - 9.06 x1x3 + 8.31 x1x4.
# The plot captions are wrapped over lines; the tests join them. The coded coefficients table shows the alias "(Intercept)" for the intercept, as before.
.aliasTestData <- function(factorNames) {
  d <- expand.grid(A = c(-1, 1), B = c(-1, 1), C = c(-1, 1), D = c(-1, 1))
  d$Rate <- c(45, 71, 48, 65, 68, 60, 80, 65, 43, 100, 45, 104, 75, 86, 70, 96)
  d$A <- factor(d$A)
  d$B <- factor(d$B)
  names(d)[1:4] <- factorNames
  d
}

.aliasTestRun <- function(dataset, discrete, continuous, terms, highestOrder = FALSE) {
  options <- analysisOptions("doeAnalysis")
  options$dependentFactorial         <- "Rate"
  options$fixedFactorsFactorial      <- discrete
  options$continuousFactorsFactorial <- continuous
  options$codeFactors                <- TRUE
  options$codeFactorsMethod          <- "automatic"
  options$tableEquation              <- TRUE
  options$tableAlias                 <- TRUE
  options$plotPareto                 <- TRUE
  options$normalEffectsPlot          <- TRUE
  options$highestOrder               <- highestOrder
  options$order                      <- 2
  options$histogramBinWidthType      <- "doane"
  options$modelTerms                 <- lapply(terms, function(x) list(components = x))
  set.seed(1)
  runAnalysis("doeAnalysis", dataset, options, makeTests = FALSE)
}

.aliasTestOutput <- function(results) {
  collection <- results[["results"]][["Rate"]][["collection"]]
  coefRows   <- collection[["Rate_tableCoefficients"]][["data"]]
  pareto     <- results[["state"]][["figures"]][[collection[["Rate_plotPareto"]][["data"]]]][["obj"]]
  normal     <- results[["state"]][["figures"]][[collection[["Rate_normalEffectsPlot"]][["data"]]]][["obj"]]
  footnotes  <- function(table) vapply(table[["footnotes"]], `[[`, character(1), "text")
  list(alias           = vapply(coefRows, `[[`, character(1), "alias"),
       terms           = vapply(coefRows, `[[`, character(1), "terms"),
       coef            = vapply(coefRows, function(row) as.numeric(row[["coef"]]), numeric(1)),
       equation        = collection[["Rate_tableEquation"]][["data"]][[1]][["formula"]],
       coefFootnotes   = footnotes(collection[["Rate_tableCoefficients"]]),
       equationNotes   = footnotes(collection[["Rate_tableEquation"]]),
       paretoLabels    = ggplot2::ggplot_build(pareto)$layout$panel_params[[1]]$y$get_labels(),
       paretoCaption   = if (!is.null(pareto$labels$caption)) gsub("\n", " ", pareto$labels$caption, fixed = TRUE),
       normalLabels    = ggplot2::ggplot_build(normal)$data[[3]][["label"]],
       normalValues    = ggplot2::ggplot_build(normal)$data[[3]][["x"]],
       normalCaption   = if (!is.null(normal$labels$caption)) gsub("\n", " ", normal$labels$caption, fixed = TRUE))
}

test_that("Aliases follow the factors in the form order, not the model terms (factors named A to D)", {
  # A, B discrete and C, D continuous: before, the continuous factors were lettered first (C -> A, ...)
  out <- .aliasTestOutput(.aliasTestRun(.aliasTestData(c("A", "B", "C", "D")), c("A", "B"), c("C", "D"),
                                        list("A", "B", "C", "D"), highestOrder = TRUE))
  coefs <- setNames(out$coef, out$alias)
  expect_equal(unname(coefs[c("A", "B", "C", "D")]), c(10.8125, 1.5625, 4.9375, 7.3125))
  expect_equal(out$alias, c("(Intercept)", gsub("✻", "",out$terms[-1])))
  expect_match(out$equation, "\\+ 10\\.81\\d* A ")
  expect_match(out$equation, "\\+ 4\\.93\\d* C ")
  # largest Pareto bar is factor A (t = 8.55), not C
  expect_equal(tail(out$paretoLabels, 1), "A")
  # factors named by their own letter need no legend
  expect_null(out$paretoCaption)
  expect_false(any(grepl("Aliases", out$coefFootnotes)))
})

test_that("Dropping factors from the model does not re-letter the others (Montgomery's final model)", {
  out <- .aliasTestOutput(.aliasTestRun(.aliasTestData(c("A", "B", "C", "D")), c("A", "B"), c("C", "D"),
                                        list("A", "C", "D", c("A", "C"), c("A", "D"))))
  expect_equal(out$alias, c("(Intercept)", "A", "C", "D", "AC", "AD"))
  expect_equal(out$coef, c(70.0625, 10.8125, 4.9375, 7.3125, -9.0625, 8.3125))
  expect_false(grepl("B", out$equation))
  expect_equal(sort(out$paretoLabels), sort(c("A", "C", "D", "AC", "AD")))
})

test_that("Factors with long names get letters in form order and a legend in the tables and effect plots", {
  factorNames <- c("Temperature", "Pressure", "Concentration", "Stirring")
  out <- .aliasTestOutput(.aliasTestRun(.aliasTestData(factorNames), factorNames[1:2], factorNames[3:4],
                                        list("Temperature", "Concentration", "Stirring", c("Temperature", "Concentration"),
                                             c("Temperature", "Stirring"))))
  expect_equal(out$alias, c("(Intercept)", "A", "C", "D", "AC", "AD"))
  expect_equal(out$coef, c(70.0625, 10.8125, 4.9375, 7.3125, -9.0625, 8.3125))
  legend <- "Aliases: A = Temperature, B = Pressure, C = Concentration, D = Stirring."
  expect_true(legend %in% out$coefFootnotes)
  expect_true(legend %in% out$equationNotes)
  expect_equal(out$paretoCaption, legend)
  expect_equal(out$normalCaption, legend)
  # each label of the normal plot sits at the t-value of its own term: A (Temperature) has the largest
  expect_equal(out$normalLabels[which.max(out$normalValues)], "A")
})

test_that("No factor gets a letter that is the name of another factor", {
  # discrete factors named C and D: the continuous factors get A and B, not C and D
  factorNames <- c("C", "D", "Temp", "Press")
  out <- .aliasTestOutput(.aliasTestRun(.aliasTestData(factorNames), c("C", "D"), c("Temp", "Press"),
                                        list("C", "D", "Temp", "Press", c("C", "Temp"))))
  expect_equal(out$alias, c("(Intercept)", "C", "D", "A", "B", "CA"))
  expect_equal(out$coef, c(70.0625, 10.8125, 1.5625, 4.9375, 7.3125, -9.0625))
  expect_equal(out$paretoCaption, "Aliases: A = Temp, B = Press.")
})
