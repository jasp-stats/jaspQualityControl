context("[Quality Control] Location-scale fits and probability-plot bands for any spread of the data")

qcFunction <- function(name) getFromNamespace(name, "jaspQualityControl")
# optim probes invalid parameter values on its way ("NaNs produced"); only the estimates matter here
fitDistribution <- function(...) suppressWarnings(qcFunction(".distributionParameters")(...))

test_that("Logistic fit is location-scale equivariant, also for a small spread", {
  set.seed(1)
  x <- rlogis(25, location = 0, scale = 1)
  reference <- fitDistribution(x, distribution = "logistic")
  for (spread in c(1e-4, 1e-3, 0.5, 1e3)) {
    fit <- fitDistribution(10 + x * spread, distribution = "logistic")
    expect_equal(fit[["beta"]], 10 + reference[["beta"]] * spread, tolerance = 1e-8, label = sprintf("location, spread %g", spread))
    expect_equal(fit[["theta"]], reference[["theta"]] * spread, tolerance = 1e-8, label = sprintf("scale, spread %g", spread))
  }
})

test_that("Logistic fit with a historical (fixed) parameter and a small spread", {
  set.seed(1)
  x <- rlogis(25, location = 0, scale = 1)
  spread <- 1e-3
  fixedScale <- fitDistribution(10 + x * spread, distribution = "logistic", fix.arg = list(scale = 0.8 * spread))
  reference  <- fitDistribution(x, distribution = "logistic", fix.arg = list(scale = 0.8))
  expect_equal(fixedScale[["theta"]], 0.8 * spread)
  expect_equal(fixedScale[["beta"]], 10 + reference[["beta"]] * spread, tolerance = 1e-8)

  fixedLocation <- fitDistribution(10 + x * spread, distribution = "logistic", fix.arg = list(location = 10 + 0.1 * spread))
  reference     <- fitDistribution(x, distribution = "logistic", fix.arg = list(location = 0.1))
  expect_equal(fixedLocation[["beta"]], 10 + 0.1 * spread)
  expect_equal(fixedLocation[["theta"]], reference[["theta"]] * spread, tolerance = 1e-8)
})

# Raw data of the lower confidence band of the probability plot (before axis clipping): zp and both band limits on the
# plot's axis scale (x for the normal plot, log(x) for the log-normal plot)
probabilityPlotBand <- function(x, nullDistribution) {
  options <- list(nullDistribution = nullDistribution, probabilityPlotRankMethod = "bernard", probabilityPlotGridLines = FALSE)
  plot <- qcFunction(".qcProbabilityPlotObject")(options, data.frame(x = x), "x", "")[[1]]
  dashed <- Filter(function(layer) identical(layer$aes_params$linetype, "dashed"), plot$layers)
  expect_length(dashed, 2)
  dashed[[1]]$data
}

# Independent reference: at (mean, s) of the (log) data, the inverse observed information of (mu, sigma) is
# diag(s^2 / n, s^2 / (2n - 3)), so the band is mean + zp * s -/+ 1.96 * s * sqrt(1 / n + zp^2 / (2n - 3))
expectAnalyticBand <- function(band, values, label) {
  n <- length(values); m <- mean(values); s <- sd(values)
  halfWidth <- qnorm(0.975) * s * sqrt(1 / n + band$zp^2 / (2 * n - 3))
  expect_equal(band$percentileLower, m + band$zp * s - halfWidth, tolerance = 1e-5, label = paste("lower band,", label))
  expect_equal(band$percentileUpper, m + band$zp * s + halfWidth, tolerance = 1e-5, label = paste("upper band,", label))
}

test_that("Normal probability plot confidence bands are correct for any std. dev.", {
  set.seed(1)
  z <- rnorm(20)
  for (spread in c(1e-3, 0.02, 1, 1e5)) {
    x <- 10 + z * spread
    expectAnalyticBand(probabilityPlotBand(x, "normal"), x, sprintf("std. dev. %g", spread))
  }
})

test_that("Log-normal probability plot confidence bands are correct for any sdlog", {
  set.seed(1)
  z <- rnorm(20)
  for (spread in c(1e-3, 0.02, 1)) {
    x <- 10 * exp(z * spread)
    expectAnalyticBand(probabilityPlotBand(x, "lognormal"), log(x), sprintf("sdlog %g", spread))
  }
})

test_that("Log-normal fit with a historical (fixed) parameter and a small sdlog", {
  set.seed(1)
  z <- rnorm(25)
  spread <- 1e-3
  fixedMeanlog <- fitDistribution(10 * exp(z * spread), distribution = "lognormal", fix.arg = list(meanlog = log(10) + 0.1 * spread))
  reference    <- fitDistribution(10 * exp(z), distribution = "lognormal", fix.arg = list(meanlog = log(10) + 0.1))
  expect_equal(fixedMeanlog[["beta"]], log(10) + 0.1 * spread)
  expect_equal(fixedMeanlog[["theta"]], reference[["theta"]] * spread, tolerance = 1e-6)

  fixedSdlog <- fitDistribution(10 * exp(z * spread), distribution = "lognormal", fix.arg = list(sdlog = 0.9 * spread))
  reference  <- fitDistribution(10 * exp(z), distribution = "lognormal", fix.arg = list(sdlog = 0.9))
  expect_equal(fixedSdlog[["theta"]], 0.9 * spread)
  expect_equal(fixedSdlog[["beta"]] - log(10), (reference[["beta"]] - log(10)) * spread, tolerance = 1e-6)
})

test_that("Non-normal capability study with logistic data with a small spread", {
  set.seed(1)
  dataset <- data.frame(Diameter = 10 + rlogis(20, scale = 5e-4))
  options <- analysisOptions("processCapabilityStudies")
  options$measurementLongFormat <- "Diameter"
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 5
  options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
  options$nonNormalDistribution <- "logistic"
  options$controlChartType <- "xBarR"
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 10.004
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", dataset, options, view = FALSE)

  summaryTable <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]]
  expect_null(summaryTable[["error"]])
  # independent reference: logistic MLE on the standardized data with a tight tolerance
  x <- dataset[["Diameter"]]
  z <- (x - mean(x)) / sd(x)
  opt <- optim(c(median(z), 0), function(p) -sum(dlogis(z, p[1], exp(p[2]), log = TRUE)), method = "BFGS", control = list(reltol = 1e-14))
  reference <- c(mean(x) + opt$par[1] * sd(x), exp(opt$par[2]) * sd(x))
  expect_equal(summaryTable[["data"]][[1]][["beta"]], reference[1], tolerance = 1e-6)
  expect_equal(summaryTable[["data"]][[1]][["theta"]] / reference[2], 1, tolerance = 1e-3)
})
