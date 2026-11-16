context("DoE inference and display consistency")

.doeConsistencyOptions <- function(coded = TRUE) {
  options <- jaspTools::analysisOptions("doeAnalysis")
  options$designType <- "responseSurfaceDesign"
  options$continuousFactorsResponseSurface <- c("A", "B")
  options$dependentResponseSurface <- "Y"
  options$rsmPredefinedModel <- TRUE
  options$rsmPredefinedTerms <- "fullQuadratic"
  options$codeFactors <- coded
  options
}
.doeConsistencyTable <- function(result, name) result$results$Y$collection[[paste0("Y_", name)]]
.doeConsistencyNotes <- function(table) vapply(table$footnotes, function(x) x$text, character(1))
.doeConsistencyData <- expand.grid(A = c(150, 160, 170), B = c(200, 215, 230), rep = 1:2)
.doeConsistencyData$Y <- with(.doeConsistencyData, 10 + (A - 160) / 10 + ((B - 215) / 15)^2 +
                              (A - 160) * (B - 215) / 150 + ifelse(rep == 1, -.1, .1))

test_that("RSM ANOVA and plotted residuals agree across display units and the retired option", {
  for (ssType in c("type1", "type2", "type3")) {
    reference <- NULL
    for (coded in c(TRUE, FALSE)) for (legacy in c(FALSE, TRUE)) {
      options <- .doeConsistencyOptions(coded)
      options$sumOfSquaresType <- ssType
      options$squaredTermsCoded <- legacy
      options$plotFitted <- TRUE
      result <- jaspTools::runAnalysis("doeAnalysis", .doeConsistencyData, options, view = FALSE)
      anova <- .doeConsistencyTable(result, "tableAnova")$data
      if (is.null(reference)) reference <- anova else expect_equal(anova, reference)
      row <- function(label) anova[[which(vapply(anova, function(x) x$terms == label, logical(1)))]]
      expect_equal(.doeConsistencyTable(result, "tableSummary")$data[[1]]$rsq,
                   row("Model")$adjss / row("Total")$adjss, tolerance = 1e-10)
      plot <- .doeConsistencyTable(result, "plotFitted")
      plotData <- result$state$figures[[plot$data]]$obj$data
      expect_equal(sum(plotData$y^2), row("Error")$adjss, tolerance = 1e-10)
    }
  }
})

test_that("forward selection and its final AIC do not depend on display units", {
  data <- expand.grid(A = 1:5, rep = 1:2)
  data$Y <- 10 + (data$A - 3)^2 + ifelse(data$rep == 1, -.1, .1)
  results <- lapply(c(TRUE, FALSE), function(coded) {
    options <- .doeConsistencyOptions(coded)
    options$continuousFactorsResponseSurface <- "A"
    options$stepwiseMethodResponseSurface <- "forward"
    jaspTools::runAnalysis("doeAnalysis", data, options, view = FALSE)
  })
  expect_equal(.doeConsistencyTable(results[[1]], "stepwiseTable")$data,
               .doeConsistencyTable(results[[2]], "stepwiseTable")$data)
  reference <- stats::lm(Y ~ A + I(A^2), data)
  for (result in results) {
    expect_equal(.doeConsistencyTable(result, "tableSummary")$data[[1]]$rsq, summary(reference)$r.squared)
    steps <- .doeConsistencyTable(result, "stepwiseTable")$data
    expect_equal(tail(steps, 1)[[1]]$AIC, stats::extractAIC(reference)[2], tolerance = 1e-9)
    expect_match(tail(steps, 1)[[1]]$formula, "A^2 + A", fixed = TRUE)
  }
})

test_that("effect significance plots use the same coded tests in either display", {
  plotData <- lapply(c(TRUE, FALSE), function(coded) {
    options <- .doeConsistencyOptions(coded)
    options$plotPareto <- TRUE
    options$normalEffectsPlot <- TRUE
    options$tableCoefficientsCi <- TRUE
    options$tableCoefficientsCiLevel <- .90
    result <- jaspTools::runAnalysis("doeAnalysis", .doeConsistencyData, options, view = FALSE)
    lapply(c("plotPareto", "normalEffectsPlot"), function(name) {
      plot <- .doeConsistencyTable(result, name)
      expect_identical(plot$status, "complete")
      expect_match(plot$title, "\u03b1 = .05", fixed = TRUE)
      result$state$figures[[plot$data]]$obj$data
    })
  })
  expect_equal(plotData[[1]], plotData[[2]])
})

test_that("actual factor coding is shown only for non-identity coding", {
  for (coded in c(TRUE, FALSE)) {
    result <- jaspTools::runAnalysis("doeAnalysis", .doeConsistencyData, .doeConsistencyOptions(coded), view = FALSE)
    coding <- .doeConsistencyTable(result, "tableCoding")$data
    expect_identical(vapply(coding, function(x) x$factor, character(1)), c("A", "B"))
    expect_identical(vapply(coding, function(x) x$low, character(1)), c("150", "200"))
    expect_identical(vapply(coding, function(x) x$high, character(1)), c("170", "230"))
  }
  data <- expand.grid(A = c(-sqrt(2), -1, 0, 1, sqrt(2)), B = c(-1, 1), rep = 1:2)
  data$Y <- 10 + data$A + data$B + data$A^2 + ifelse(data$rep == 1, -.1, .1)
  options <- .doeConsistencyOptions()
  result <- jaspTools::runAnalysis("doeAnalysis", data, options, view = FALSE)
  coding <- .doeConsistencyTable(result, "tableCoding")$data
  expect_length(coding, 1)
  expect_equal(as.numeric(c(coding[[1]]$low, coding[[1]]$high)), c(-sqrt(2), sqrt(2)))
  options$codeFactorsMethod <- "manual"
  options$codeFactorsManualTable <- list(list(predictors = "A", lowValue = -1, highValue = 1),
                                       list(predictors = "B", lowValue = -1, highValue = 1))
  result <- jaspTools::runAnalysis("doeAnalysis", data, options, view = FALSE)
  expect_null(.doeConsistencyTable(result, "tableCoding"))
})

test_that("Lenth screening rejects saturated continuous three-level designs", {
  data <- data.frame(A = c(0, 1, 3), B = c(0, 2, 1), Y = c(1, 4, 8))
  options <- jaspTools::analysisOptions("doeAnalysis")
  options$continuousFactorsFactorial <- c("A", "B")
  options$dependentFactorial <- "Y"
  options$highestOrder <- FALSE
  options$modelTerms <- list(list(components = "A"), list(components = "B"))
  options$plotPareto <- TRUE
  options$normalEffectsPlot <- TRUE
  result <- jaspTools::runAnalysis("doeAnalysis", data, options, view = FALSE)
  for (name in c("plotPareto", "normalEffectsPlot"))
    expect_match(.doeConsistencyTable(result, name)$error$errorMessage, "orthogonal two-level effects with equal precision", fixed = TRUE)
  expect_identical(.doeConsistencyTable(result, "tableAnova")$status, "complete")
})

.doeSearchData <- data.frame(A = rep(1:5, 2), Y = 3 * rep(1:5, 2) + rep(c(-.1, .1), each = 5))
.doeSearchOptions <- function() {
  options <- jaspTools::analysisOptions("doeAnalysis")
  options$continuousFactorsFactorial <- "A"
  options$dependentFactorial <- "Y"
  options$highestOrder <- FALSE
  options$modelTerms <- list(list(components = "A"))
  options$responsesResponseOptimizer <- list(list(variable = "Y", responseOptimizerGoal = "maximize",
    responseOptimizerLowerBound = 0, responseOptimizerTarget = 20, responseOptimizerUpperBound = 25,
    responseOptimizerWeight = 1, responseOptimizerImportance = 1))
  options$responseOptimizerManualBounds <- TRUE
  options$responseOptimizerManualTarget <- TRUE
  options$optimizationIntervals <- TRUE
  options$optimizationPlot <- FALSE
  options
}

test_that("non-converged optimizer results are not reported as solutions", {
  testthat::local_mocked_bindings(optim = function(par, ...) list(par = par, value = -1, convergence = 1L), .package = "stats")
  result <- jaspTools::runAnalysis("doeAnalysis", .doeSearchData, .doeSearchOptions(), view = FALSE)
  for (name in c("tableRoSolution", "tableRoPrediction")) {
    expect_match(result$results[[name]]$error$errorMessage, "Optimization did not converge", fixed = TRUE)
    expect_length(result$results[[name]]$data, 0)
  }
  expect_identical(.doeConsistencyTable(result, "tableSummary")$status, "complete")
})

test_that("a failed optimizer start is excluded and reported", {
  calls <- 0L
  original <- stats::optim
  testthat::local_mocked_bindings(optim = function(...) {
    calls <<- calls + 1L
    if (calls == 1L) stop("synthetic optimizer failure")
    original(...)
  }, .package = "stats")
  result <- jaspTools::runAnalysis("doeAnalysis", .doeSearchData, .doeSearchOptions(), view = FALSE)
  solution <- result$results$tableRoSolution
  expect_identical(solution$status, "complete")
  expect_match(solution$title, "Best Solution Found", fixed = TRUE)
  expect_true(any(grepl("Some optimization runs failed", .doeConsistencyNotes(solution), fixed = TRUE)))
  expect_equal(as.numeric(solution$data[[1]]$A), 5)
  expect_equal(as.numeric(solution$data[[1]]$Y_fit), 15, tolerance = 1e-3)
})

test_that("optimizer warnings remain visible when the solution table is hidden", {
  calls <- 0L
  original <- stats::optim
  testthat::local_mocked_bindings(optim = function(...) {
    calls <<- calls + 1L
    if (calls == 1L) stop("synthetic optimizer failure")
    original(...)
  }, .package = "stats")
  for (intervals in c(TRUE, FALSE)) {
    calls <- 0L
    options <- .doeSearchOptions()
    options$optimizationSolutionTable <- FALSE
    options$optimizationIntervals <- intervals
    options$optimizationPlot <- !intervals
    result <- jaspTools::runAnalysis("doeAnalysis", .doeSearchData, options, view = FALSE)
    table <- if (intervals) result$results$tableRoPrediction else result$results$plotRo$collection$plotRo_table
    expect_identical(table$status, "complete")
    expect_true(any(grepl("Some optimization runs failed", .doeConsistencyNotes(table), fixed = TRUE)))
  }
})

test_that("unsupported squared-only models give an error instead of mixed outputs", {
  for (coded in c(TRUE, FALSE)) for (legacy in c(TRUE, FALSE)) {
    options <- .doeConsistencyOptions(coded)
    options$rsmPredefinedModel <- FALSE
    options$continuousFactorsResponseSurface <- "A"
    options$modelTerms <- list()
    options$squaredTerms <- "A"
    options$squaredTermsCoded <- legacy
    options$plotFitted <- TRUE
    result <- jaspTools::runAnalysis("doeAnalysis", .doeConsistencyData, options, view = FALSE)
    expect_match(.doeConsistencyTable(result, "tableSummary")$error$errorMessage, "omits lower-order terms")
    expect_length(.doeConsistencyTable(result, "tableAnova")$data, 0)
    expect_identical(.doeConsistencyTable(result, "plotFitted")$status, "error")
  }
})

test_that("hierarchy includes all lower-order interactions, independently of term order", {
  missing <- jaspQualityControl:::.doeMissingLowerTerms
  expect_setequal(missing(Y ~ A + B + C + A:B:C), c("A:B", "A:C", "B:C"))
  expect_length(missing(Y ~ B:A + B + A + I(A^2)), 0)
  expect_identical(missing(Y ~ I(A^2)), "A")
  expect_setequal(missing(Y ~ (A + B + C + D)^4 - A:B), "A:B")
})

test_that("factorial curvature and projected replication assumptions are visible", {
  data <- expand.grid(A = c(-1, 0, 1), B = c(-1, 1), rep = 1:2)
  data$Y <- 10 + 2 * data$A + 5 * data$B + ifelse(data$rep == 1, -.1, .1)
  options <- jaspTools::analysisOptions("doeAnalysis")
  options$continuousFactorsFactorial <- c("A", "B")
  options$dependentFactorial <- "Y"
  options$highestOrder <- FALSE
  options$modelTerms <- list(list(components = "A"))
  result <- jaspTools::runAnalysis("doeAnalysis", data, options, view = FALSE)
  notes <- .doeConsistencyNotes(.doeConsistencyTable(result, "tableAnova"))
  expect_true(any(grepl("Center-point curvature is not modeled automatically", notes, fixed = TRUE)))
  expect_true(any(grepl("omitted predictors (B)", notes, fixed = TRUE)))
  expect_true(any(grepl("have no effect", notes, fixed = TRUE)))
})

test_that("removed alias assumptions are stated beside the optimizer and predictions", {
  data <- expand.grid(A = c(-1, 1), B = c(-1, 1), rep = 1:2)
  data$C <- data$A * data$B
  data$Y <- 10 + 2 * data$A + 3 * data$B + 4 * data$C + ifelse(data$rep == 1, -.1, .1)
  options <- jaspTools::analysisOptions("doeAnalysis")
  options$continuousFactorsFactorial <- c("A", "B", "C")
  options$dependentFactorial <- "Y"
  options$highestOrder <- FALSE
  options$modelTerms <- lapply(list("A", "B", "C", c("A", "B")), function(x) list(components = x))
  options$responsesResponseOptimizer <- list(list(variable = "Y", responseOptimizerGoal = "maximize",
    responseOptimizerLowerBound = 0, responseOptimizerTarget = 30, responseOptimizerUpperBound = 40,
    responseOptimizerWeight = 1, responseOptimizerImportance = 1))
  options$responseOptimizerManualBounds <- TRUE
  options$responseOptimizerManualTarget <- TRUE
  options$optimizationIntervals <- TRUE
  result <- jaspTools::runAnalysis("doeAnalysis", data, options, view = FALSE)
  expect_true(any(grepl("effects are assumed to be zero", .doeConsistencyNotes(.doeConsistencyTable(result, "tableSummary")), fixed = TRUE)))
  for (name in c("tableRoSolution", "tableRoPrediction"))
    expect_true(any(grepl("removed aliased terms", .doeConsistencyNotes(result$results[[name]]), fixed = TRUE)))
})

test_that("contour output without a slice to explain keeps the plot available", {
  options <- .doeConsistencyOptions()
  options$contourSurfacePlot <- TRUE
  options$contourSurfacePlotVariables <- c("A", "B")
  result <- jaspTools::runAnalysis("doeAnalysis", .doeConsistencyData, options, view = FALSE)
  expect_identical(result$status, "complete")
  expect_identical(.doeConsistencyTable(result, "tableAnova")$status, "complete")
  plots <- .doeConsistencyTable(result, "contourSurfacePlot")$collection
  expect_identical(plots[[1]]$status, "complete")
  expect_null(plots[["Y_contourSurfacePlot_sliceNote"]]) # nothing held at a mean or averaged over
})

test_that("the slice note follows all contour and surface plots in display order", {
  data <- expand.grid(A = -1:1, B = -1:1, C = -1:1, rep = 1:2)
  data$Y <- with(data, 5 + A + B^2 + C + A * B + ifelse(rep == 1, -.1, .1))
  for (type in c("contourPlot", "surfacePlot")) {
    options <- .doeConsistencyOptions()
    options$continuousFactorsResponseSurface <- c("A", "B", "C")
    options$contourSurfacePlot <- TRUE
    options$contourSurfacePlotType <- type
    options$contourSurfacePlotVariables <- c("C", "A", "B")
    result <- jaspTools::runAnalysis("doeAnalysis", data, options, view = FALSE)
    responseMeta <- Filter(function(x) x$name == "Y", result$results[[".meta"]])[[1]]
    plotMeta <- Filter(function(x) x$name == "Y_contourSurfacePlot", responseMeta$meta)[[1]]$meta
    expect_identical(vapply(plotMeta, function(x) x$type, character(1)), c("image", "image", "image", "htmlNode"))
    expect_identical(tail(plotMeta, 1)[[1]]$name, "Y_contourSurfacePlot_sliceNote")
    title <- if (type == "contourPlot") "Contour plot" else "Surface plot"
    expect_identical(vapply(head(plotMeta, 3), function(x) x$title, character(1)),
                     paste(title, "of Y vs", c("C and A", "C and B", "A and B")))
    plots <- .doeConsistencyTable(result, "contourSurfacePlot")$collection
    for (item in head(plotMeta, 3))
      expect_identical(plots[[item$name]]$status, "complete")
  }
})

test_that("alias removal names the lower-order term required by a retained interaction", {
  # Non-regular design: A:B is aliased with C, while A:B:C remains estimable.
  data <- expand.grid(A = -1:1, B = -1:1, rep = 1:2)
  data$C <- data$A * data$B
  data$Y <- with(data, 10 + A + 2 * B + 3 * C + .5 * A * B * C + ifelse(rep == 1, -.1, .1))
  reference <- stats::lm(Y ~ A * B * C, data)
  expect_true(is.na(stats::coef(reference)[["A:B"]]))
  expect_false(is.na(stats::coef(reference)[["A:B:C"]]))
  for (coded in c(TRUE, FALSE)) {
    options <- jaspTools::analysisOptions("doeAnalysis")
    options$continuousFactorsFactorial <- c("A", "B", "C")
    options$dependentFactorial <- "Y"
    options$highestOrder <- TRUE
    options$order <- 3
    options$codeFactors <- coded
    result <- jaspTools::runAnalysis("doeAnalysis", data, options, view = FALSE)
    message <- .doeConsistencyTable(result, "tableSummary")$error$errorMessage
    expect_match(message, "Removing aliased terms leaves missing lower-order terms", fixed = TRUE)
    expect_match(gsub("\u2009", "", message, fixed = TRUE), "A\u273bB", fixed = TRUE)
    expect_match(message, "Remove the higher-order terms", fixed = TRUE)
    expect_length(.doeConsistencyTable(result, "tableAnova")$data, 0)
  }
})
