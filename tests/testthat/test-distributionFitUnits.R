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
  # relative accuracy of each parameter (the scale is ~0.002, the shape ~3)
  expect_equal(c(shape = fit[["beta"]], scale = fit[["theta"]]) / weibullProfileMle(x), c(shape = 1, scale = 1), tolerance = 1e-3)

  set.seed(2)
  x <- rweibull(5, shape = 1.5, scale = 0.002)
  fit <- fitDistribution(x, distribution = "weibull")
  expect_equal(c(shape = fit[["beta"]], scale = fit[["theta"]]) / weibullProfileMle(x), c(shape = 1, scale = 1), tolerance = 1e-3)
})

test_that("Weibull probability plot confidence bands are correct in any unit", {
  set.seed(1)
  x <- rweibull(20, shape = 3, scale = 2)
  options <- list(nullDistribution = "weibull", probabilityPlotRankMethod = "bernard", probabilityPlotGridLines = FALSE)
  for (unit in c(1e-6, 1e-3, 1, 1e3, 1e6)) {
    plot <- qcFunction(".qcProbabilityPlotObject")(options, data.frame(x = x * unit), "x", "")[[1]]
    dashed <- Filter(function(layer) identical(layer$aes_params$linetype, "dashed"), plot$layers)
    expect_length(dashed, 2)
    band <- dashed[[1]]$data # zp, log lower and log upper percentile (before axis clipping)

    # independent reference: delta method with the analytic inverse observed information at the fitted parameters
    fit <- fitDistribution(x * unit, distribution = "weibull")
    shape <- fit[["beta"]]; scale <- fit[["theta"]]
    z <- x * unit / scale; n <- length(x)
    information <- -matrix(c(-n / shape^2 - sum(z^shape * log(z)^2),
                             -n / scale + sum(z^shape) / scale + shape * sum(z^shape * log(z)) / scale,
                             -n / scale + sum(z^shape) / scale + shape * sum(z^shape * log(z)) / scale,
                             n * shape / scale^2 - shape * (shape + 1) * sum(z^shape) / scale^2), 2)
    varcov <- solve(information)
    # Var(log percentile) = Var(log scale + zp / shape)
    gradient <- rbind(-band$zp / shape^2, 1 / scale)
    sdLogPercentile <- sqrt(colSums(gradient * (varcov %*% gradient)))
    logPercentile <- log(scale) + band$zp / shape
    expect_equal(band$percentileLower, logPercentile - qnorm(0.975) * sdLogPercentile, tolerance = 1e-5, label = sprintf("lower band, data x %g", unit))
    expect_equal(band$percentileUpper, logPercentile + qnorm(0.975) * sdLogPercentile, tolerance = 1e-5, label = sprintf("upper band, data x %g", unit))
  }
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
  expect_equal(c(summaryTable[["data"]][[1]][["beta"]], summaryTable[["data"]][[1]][["theta"]]) / exact, c(1, 1), tolerance = 1e-3)
  expect_equal(c(probabilityTable[["data"]][[1]][["mean"]], probabilityTable[["data"]][[1]][["sd"]]) / exact, c(1, 1), tolerance = 1e-3)
})

test_that("3-parameter Weibull and log-logistic fits work for data with a large offset", {
  # on the rescaled data the 3-parameter Weibull scale and threshold gap are ~1e-3 (optim's Hessian step would fail)
  set.seed(1)
  x <- 100 + rweibull(50, shape = 2, scale = 0.1)
  fit <- fitDistribution(x, distribution = "3ParameterWeibull")
  # independent reference: maximise the 2-parameter Weibull profile log-likelihood over the threshold
  profileLoglik <- function(logGap) {
    p <- weibullProfileMle(x - (min(x) - exp(logGap)))
    sum(dweibull(x - (min(x) - exp(logGap)), p[["shape"]], p[["scale"]], log = TRUE))
  }
  threshold <- min(x) - exp(optimize(profileLoglik, log(sd(x)) + c(-10, 5), maximum = TRUE, tol = 1e-10)$maximum)
  reference <- weibullProfileMle(x - threshold)
  expect_equal(c(fit[["beta"]], fit[["theta"]]) / reference, c(shape = 1, scale = 1), tolerance = 1e-3)
  expect_equal((fit[["threshold"]] - threshold) / sd(x), 0, tolerance = 1e-3)

  set.seed(1)
  x <- 1000 + rweibull(50, shape = 2, scale = 0.1)
  fit <- fitDistribution(x, distribution = "loglogistic")
  # independent reference: log(x) is logistic with location beta and scale theta
  y <- log(x); m <- median(y); s <- mad(y)
  p <- optim(c(0, log(0.5)), function(p) -sum(dlogis((y - m) / s, p[1], exp(p[2]), log = TRUE)), control = list(reltol = 1e-14))$par
  expect_equal(fit[["theta"]] / (s * exp(p[2])), 1, tolerance = 1e-3)
  expect_equal((fit[["beta"]] - (m + s * p[1])) / (s * exp(p[2])), 0, tolerance = 1e-3)
})

test_that("3-parameter Weibull fit with a free threshold and shape <= 1 gives an error", {
  # the likelihood then increases towards threshold = min(x), so there is no maximum likelihood estimate
  set.seed(4)
  x <- rweibull(15, shape = 0.6, scale = 1) # without the check, the fit returns shape 0.45 in every unit
  for (unit in c(1e-3, 1, 1e3))
    expect_error(fitDistribution(x * unit, distribution = "3ParameterWeibull"), "could not be estimated", info = sprintf("data x %g", unit))
  # with a fixed threshold it is a 2-parameter Weibull fit of x - threshold, which has an MLE for any shape
  fit <- fitDistribution(x, distribution = "3ParameterWeibull", fix.arg = list(thres = -0.1))
  reference <- weibullProfileMle(x + 0.1)
  expect_equal(c(fit[["beta"]], fit[["theta"]]) / reference, c(shape = 1, scale = 1), tolerance = 1e-3)
  expect_equal(fit[["threshold"]], -0.1)
})
