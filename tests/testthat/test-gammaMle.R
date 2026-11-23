context("Gamma maximum likelihood fit")

# The gamma MLE satisfies its likelihood equations:
# shape / rate = mean(x) and digamma(shape) - log(rate) = mean(log(x)).
.gammaScore <- function(x, shape, rate)
  c(mean = shape / rate / mean(x) - 1, log = digamma(shape) - log(rate) - mean(log(x)))

.gammaFit <- function(x, fix.arg = NULL) {
  fit <- jaspQualityControl:::.distributionParameters(x, distribution = "gamma", fix.arg = fix.arg)
  c(shape = fit[["beta"]], rate = 1 / fit[["theta"]])
}

test_that("gamma fit is the maximum likelihood estimate for data with little variation", {
  # 10 +- 0.02: the shape is ~3e5 and a general optimiser stopped at about half of it
  set.seed(1)
  x   <- rnorm(100, 10, 0.02)
  fit <- .gammaFit(x)
  expect_equal(unname(.gammaScore(x, fit[["shape"]], fit[["rate"]])), c(0, 0), tolerance = 1e-12)
  expect_equal(fit[["shape"]], 313118.9, tolerance = 1e-6)
})

test_that("gamma fit solves its likelihood equations from large to tiny variation and in any unit", {
  for (cv in c(0.3, 0.02, 0.001)) {
    set.seed(7)
    x   <- rnorm(60, 50, 50 * cv)
    fit <- .gammaFit(x)
    score <- .gammaScore(x, fit[["shape"]], fit[["rate"]])
    expect_lt(abs(score[["mean"]]), 1e-12)
    expect_lt(abs(score[["log"]]) / (cv^2 / 2), 1e-8) # relative to log(mean(x)) - mean(log(x)) ~ CV^2 / 2
    for (unit in c(1e-3, 1e3))
      expect_equal(.gammaFit(x * unit) * c(1, unit), fit, tolerance = 1e-10)
  }
})

test_that("gamma upper-tail ppm retain precision for tightly clustered measurements", {
  # Shapes from the likelihood equation evaluated with 70-digit decimal arithmetic.
  offsets <- c(1e8, 1e9)
  shapes <- c(690978956409681.2, 6.909788937215822e16)
  for (i in seq_along(offsets)) {
    x <- offsets[i] + c(0, 1, 2, 4, 7, 11)
    fit <- .gammaFit(x)
    expect_equal(fit[["shape"]], shapes[i], tolerance = 1e-10)
    for (unit in c(1e-3, 1e3))
      expect_equal(.gammaFit(x * unit) * c(1, unit), fit, tolerance = 1e-7)
    expected <- pgamma(offsets[i] + 10, shapes[i], scale = mean(x) / shapes[i], lower.tail = FALSE)
    actual <- pgamma(offsets[i] + 10, fit[["shape"]], rate = fit[["rate"]], lower.tail = FALSE)
    expect_lt(abs(1e6 * (actual - expected)), 0.05)
  }
})

test_that("gamma fitting handles a positive value much smaller than the mean", {
  x <- c(1e-100, 1, 2, 4, 7, 11)
  fit <- .gammaFit(x)
  expect_lt(abs(log(fit[["shape"]]) - digamma(fit[["shape"]]) -
                  (log(mean(x)) - mean(log(x)))), 1e-11)
})

test_that("gamma fit with a historical shape or scale estimates the other parameter", {
  set.seed(3)
  x <- rgamma(40, shape = 4, rate = 2)
  fixedShape <- .gammaFit(x, list(shape = 3))
  expect_equal(fixedShape[["rate"]], 3 / mean(x))
  fixedRate <- .gammaFit(x, list(rate = 1.5))
  expect_equal(digamma(fixedRate[["shape"]]), mean(log(1.5 * x)), tolerance = 1e-12)
})

test_that("gamma fit of zeros, negative or constant data gives the estimation error", {
  for (x in list(c(1, 2, 0, 3), c(-1, 2, 3), rep(2, 5)))
    expect_error(.gammaFit(x), "could not be estimated")
})

test_that("gamma capability study gives the exact expected ppm for data with little variation", {
  set.seed(1)
  x <- rnorm(100, 10, 0.02)
  options <- analysisOptions("processCapabilityStudies")
  options$dataFormat                   <- "longFormat"
  options$measurementLongFormat        <- "x"
  options$subgroupSizeType             <- "manual"
  options$manualSubgroupSizeValue      <- 5
  options$lowerSpecificationLimit      <- TRUE
  options$lowerSpecificationLimitValue <- 9.94
  options$upperSpecificationLimit      <- TRUE
  options$upperSpecificationLimitValue <- 10.06
  options$capabilityStudyType          <- "nonNormalCapabilityAnalysis"
  options$nonNormalDistribution        <- "gamma"
  options$nullDistribution             <- "gamma"
  results <- runAnalysis("processCapabilityStudies", data.frame(x = x), options, view = FALSE)
  table <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis_PerformanceNonNormal"]][["data"]]
  # exact MLE (shape 313118.9): 245.9 ppm below the LSL; the early-stopping fit gave ~6800
  expect_equal(as.numeric(table[[1]][["expOverall1"]]), 245.92, tolerance = 1e-4) # the table holds formatted text
})
