context("[Quality Control] Distribution fits do not depend on the measurement unit")

qcFunction <- function(name) getFromNamespace(name, "jaspQualityControl")
# optim probes invalid parameter values on its way ("NaNs produced"); only the estimates matter here
fitDistribution <- function(...) suppressWarnings(qcFunction(".distributionParameters")(...))

# Independent reference: 2-parameter Weibull MLE from the 1-D profile score of the shape.
weibullProfileMle <- function(x) {
  s <- median(x)
  z <- x / s
  score <- function(k) sum(z^k * log(z)) / sum(z^k) - 1 / k - mean(log(z))
  shape <- uniroot(score, c(0.01, 100), tol = 1e-12)$root
  c(shape = shape, scale = s * mean(z^shape)^(1 / shape))
}

# Parameters proportional to the unit of the data; for log-logistic beta = log(scale) shifts by log(unit).
unitParameters <- list(weibull = "theta", gamma = "theta", loglogistic = character(0), "3ParameterWeibull" = c("theta", "threshold"))

test_that("Weibull, gamma, log-logistic and 3-parameter Weibull fits are unit-equivariant for small samples", {
  set.seed(1)
  x <- rweibull(20, shape = 3, scale = 2)
  for (distribution in names(unitParameters)) {
    # the 3-parameter Weibull fit itself does not converge for n = 8 (in any unit)
    for (n in if (distribution == "3ParameterWeibull") 20 else c(8, 20)) {
      reference <- fitDistribution(x[seq_len(n)], distribution = distribution)
      for (unit in c(1e-4, 1e-3, 1e3, 1e5)) {
        fit <- fitDistribution(x[seq_len(n)] * unit, distribution = distribution)
        expected <- reference
        for (par in unitParameters[[distribution]])
          expected[[par]] <- reference[[par]] * unit
        if (distribution == "loglogistic")
          expected[["beta"]] <- reference[["beta"]] + log(unit)
        expect_equal(fit, expected, tolerance = 1e-8, label = sprintf("%s fit, n = %d, data x %g", distribution, n, unit))
      }
    }
  }
})

test_that("Weibull fit matches the exact maximum likelihood estimate for data in small units", {
  set.seed(1)
  x <- rweibull(20, shape = 3, scale = 2) * 0.001
  fit <- fitDistribution(x, distribution = "weibull")
  expect_equal(c(shape = fit[["beta"]], scale = fit[["theta"]]), weibullProfileMle(x), tolerance = 1e-3)

  set.seed(2)
  x <- rweibull(5, shape = 1.5, scale = 0.002)
  fit <- fitDistribution(x, distribution = "weibull")
  expect_equal(c(shape = fit[["beta"]], scale = fit[["theta"]]), weibullProfileMle(x), tolerance = 1e-3)
})

test_that("Weibull probability plot keeps its confidence bands for data in small units", {
  set.seed(1)
  x <- rweibull(20, shape = 3, scale = 2)
  options <- list(nullDistribution = "weibull", probabilityPlotRankMethod = "bernard", probabilityPlotGridLines = FALSE)
  bands <- function(unit) {
    plot <- qcFunction(".qcProbabilityPlotObject")(options, data.frame(x = x * unit), "x", "")[[1]]
    sum(vapply(plot$layers, function(layer) identical(layer$aes_params$linetype, "dashed"), logical(1)))
  }
  expect_equal(bands(1), 2)
  expect_equal(bands(1e-3), 2)
})

test_that("Zero-heavy data fall back to the largest absolute value as unit", {
  expect_equal(qcFunction(".qcUnitScale")(c(0, 0, 0, 1e-4, 2e-4)), 2e-4)
  expect_equal(qcFunction(".qcUnitScale")(c(0, 0)), 1)
})

test_that("Historical (fixed) parameters are rescaled with the data", {
  set.seed(1)
  x <- rweibull(20, shape = 3, scale = 2)
  unit <- 1e-3

  fixedScale <- fitDistribution(x * unit, distribution = "weibull", fix.arg = list(scale = 2 * unit))
  reference  <- fitDistribution(x, distribution = "weibull", fix.arg = list(scale = 2))
  expect_equal(fixedScale[["theta"]], 2 * unit)
  expect_equal(fixedScale[["beta"]], reference[["beta"]], tolerance = 1e-8)

  fixedRate <- fitDistribution(x * unit, distribution = "gamma", fix.arg = list(rate = 1 / (0.3 * unit)))
  reference <- fitDistribution(x, distribution = "gamma", fix.arg = list(rate = 1 / 0.3))
  expect_equal(fixedRate[["theta"]], 0.3 * unit)
  expect_equal(fixedRate[["beta"]], reference[["beta"]], tolerance = 1e-8)

  fixedScale <- fitDistribution(x * unit, distribution = "loglogistic", fix.arg = list(scale = 1.6 * unit))
  reference  <- fitDistribution(x, distribution = "loglogistic", fix.arg = list(scale = 1.6))
  expect_equal(fixedScale[["beta"]], log(1.6 * unit))
  expect_equal(fixedScale[["theta"]], reference[["theta"]], tolerance = 1e-8)

  fixedThreshold <- fitDistribution(x * unit, distribution = "3ParameterWeibull", fix.arg = list(thres = -0.2 * unit))
  reference      <- fitDistribution(x, distribution = "3ParameterWeibull", fix.arg = list(thres = -0.2))
  expect_equal(fixedThreshold[["threshold"]], -0.2 * unit)
  expect_equal(fixedThreshold[["beta"]], reference[["beta"]], tolerance = 1e-8)
  expect_equal(fixedThreshold[["theta"]], reference[["theta"]] * unit, tolerance = 1e-8)
})

test_that("Non-normal capability study with Weibull data in small units runs with default plots", {
  set.seed(1)
  dataset <- data.frame(Diameter = rweibull(20, shape = 3, scale = 2) * 0.001)
  options <- analysisOptions("processCapabilityStudies")
  options$measurementLongFormat <- "Diameter"
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 5
  options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
  options$nonNormalDistribution <- "weibull"
  options$nullDistribution <- "weibull"
  options$controlChartType <- "xBarR"
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 0.004
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", dataset, options, view = FALSE)

  expect_identical(results[["status"]], "complete")
  expect_null(results[["results"]][["histogram"]][["error"]])
  probabilityTable <- results[["results"]][["probabilityContainer"]][["collection"]][["probabilityContainer_probabilityTable"]]
  expect_null(probabilityTable[["error"]])

  exact <- unname(weibullProfileMle(dataset[["Diameter"]]))
  summaryTable <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]]
  expect_null(summaryTable[["error"]])
  expect_equal(c(summaryTable[["data"]][[1]][["beta"]], summaryTable[["data"]][[1]][["theta"]]), exact, tolerance = 1e-3)
  expect_equal(c(probabilityTable[["data"]][[1]][["mean"]], probabilityTable[["data"]][[1]][["sd"]]), exact, tolerance = 1e-3)
})
