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

expectWeibullBandsMatchDeltaMethod <- function(x, label) {
  options <- list(nullDistribution = "weibull", probabilityPlotRankMethod = "bernard", probabilityPlotGridLines = FALSE)
  plot <- qcFunction(".qcProbabilityPlotObject")(options, data.frame(x = x), "x", "")[[1]]
  dashed <- Filter(function(layer) identical(layer$aes_params$linetype, "dashed"), plot$layers)
  expect_length(dashed, 2)
  band <- dashed[[1]]$data # zp, log lower and log upper percentile (before axis clipping)

  # independent reference: delta method with the analytic inverse observed information at the fitted parameters
  fit <- fitDistribution(x, distribution = "weibull")
  shape <- fit[["beta"]]; scale <- fit[["theta"]]
  z <- x / scale; n <- length(x)
  information <- -matrix(c(-n / shape^2 - sum(z^shape * log(z)^2),
                           -n / scale + sum(z^shape) / scale + shape * sum(z^shape * log(z)) / scale,
                           -n / scale + sum(z^shape) / scale + shape * sum(z^shape * log(z)) / scale,
                           n * shape / scale^2 - shape * (shape + 1) * sum(z^shape) / scale^2), 2)
  # adjugate inverse: solve() reports a singular matrix for a high shape (entries differ by ~shape^4 / scale^2)
  varcov <- matrix(c(information[2, 2], -information[1, 2], -information[2, 1], information[1, 1]), 2) /
    (information[1, 1] * information[2, 2] - information[1, 2]^2)
  # Var(log percentile) = Var(log scale + zp / shape)
  gradient <- rbind(-band$zp / shape^2, 1 / scale)
  sdLogPercentile <- sqrt(colSums(gradient * (varcov %*% gradient)))
  logPercentile <- log(scale) + band$zp / shape
  # compare the half-width itself: a tolerance on the log percentile would be relative to its magnitude (e.g. 7 for
  # scale 1000) and hide percent-level errors of the much smaller half-width
  expect_equal((band$percentileUpper - band$percentileLower) / 2, qnorm(0.975) * sdLogPercentile, tolerance = 1e-6, label = paste("band half-width,", label))
  expect_equal((band$percentileUpper + band$percentileLower) / 2, logPercentile, tolerance = 1e-8, label = paste("band centre,", label))
}

test_that("Weibull probability plot confidence bands are correct in any unit", {
  set.seed(1)
  x <- rweibull(20, shape = 3, scale = 2)
  for (unit in c(1e-6, 1e-3, 1, 1e3, 1e6))
    expectWeibullBandsMatchDeltaMethod(x * unit, sprintf("data x %g", unit))
})

test_that("Weibull probability plot confidence bands are correct for data with a high shape", {
  # data that vary little relative to their location have a high shape and a narrow likelihood, much narrower
  # than the step of a finite-difference Hessian
  for (shape in c(100, 300, 900)) {
    for (scale in c(0.5, 10, 1000)) {
      set.seed(shape + scale)
      expectWeibullBandsMatchDeltaMethod(rweibull(30, shape = shape, scale = scale), sprintf("shape %g, scale %g", shape, scale))
    }
  }
})

test_that("Weibull observed information matches a finite-difference Hessian with steps relative to the likelihood width", {
  numericHessian <- function(f, par, h) {
    p <- length(par)
    hessian <- matrix(0, p, p)
    for (i in seq_len(p)) {
      for (j in seq_len(p)) {
        ei <- replace(numeric(p), i, h[i])
        ej <- replace(numeric(p), j, h[j])
        hessian[i, j] <- (f(par + ei + ej) - f(par + ei - ej) - f(par - ei + ej) + f(par - ei - ej)) / (4 * h[i] * h[j])
      }
    }
    hessian
  }
  for (shape in c(0.8, 3, 100, 600)) {
    for (scale in c(0.002, 10)) {
      set.seed(shape + 1000 * scale)
      x <- rweibull(25, shape = shape, scale = scale)
      varcov <- qcFunction(".qcWeibullObservedVarcov")(x, shape, scale)$varcov
      negLogLik <- function(theta) -sum(dweibull(x, theta[1], theta[2], log = TRUE))
      reference <- numericHessian(negLogLik, c(shape, scale), h = 0.01 * sqrt(diag(varcov)))
      # shape and scale entries differ by orders of magnitude: compare in correlation form (entries of order 1)
      information <- unname(solve(varcov, tol = 0))
      scaling <- outer(1 / sqrt(diag(information)), 1 / sqrt(diag(information)))
      expect_equal(information * scaling, reference * scaling, tolerance = 1e-4, label = sprintf("information, shape %g, scale %g", shape, scale))
    }
  }
})

test_that("Weibull probability plot confidence bands are correct for data with a coefficient of variation of 1e-5", {
  # shape ~1e5 (e.g. 100 +- 0.001): the shape and scale information differ by a factor of about shape^4, so a plain
  # solve() reports a singular matrix even in unit 1
  set.seed(1)
  expectWeibullBandsMatchDeltaMethod(rweibull(30, shape = 1e5, scale = 100), "shape 1e5, scale 100")
})

test_that("Weibull observed information gives no variances where it is not positive definite", {
  # positive definite at the maximum likelihood estimate; away from it (a fit that did not converge) it need not be,
  # and the variances would be invalid
  set.seed(1)
  x <- rweibull(20, shape = 3, scale = 2)
  mle <- weibullProfileMle(x)
  observedVarcov <- qcFunction(".qcWeibullObservedVarcov")
  varcov <- observedVarcov(x, mle[["shape"]], mle[["scale"]])$varcov
  expect_true(all(eigen(varcov, symmetric = TRUE, only.values = TRUE)$values > 0))

  invalid <- "Hessian contains invalid values"
  expect_error(observedVarcov(x, mle[["shape"]], 0.5 * mle[["scale"]]), invalid) # positive diagonal, correlation below -1
  expect_error(observedVarcov(x, mle[["shape"]], 2 * mle[["scale"]]), invalid)   # negative scale information
  expect_error(observedVarcov(c(0, x), mle[["shape"]], mle[["scale"]]), invalid) # log(0): not finite
})

test_that("Weibull probability plot omits the bands when the observed information cannot be inverted", {
  # cannot happen at the maximum likelihood estimate, so the failure is simulated
  local_mocked_bindings(.qcWeibullObservedVarcov = function(...) stop("not positive definite", call. = FALSE),
                        .package = "jaspQualityControl")
  set.seed(1)
  options <- list(nullDistribution = "weibull", probabilityPlotRankMethod = "bernard", probabilityPlotGridLines = FALSE)
  plot <- qcFunction(".qcProbabilityPlotObject")(options, data.frame(x = rweibull(30, shape = 3, scale = 2)), "x", "")[[1]]
  lines <- Filter(function(layer) inherits(layer$geom, "GeomLine"), plot$layers)
  expect_length(lines, 1) # the fitted line, no bands
  expect_true(all(is.finite(lines[[1]]$data$percentileEstimate)))
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
