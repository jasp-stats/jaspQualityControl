context("[Quality Control] Three-parameter Weibull threshold boundaries")

test_that("A fixed shape of one allows a fixed threshold at the smallest observation", {
  for (threshold in c(0, 1)) {
    for (unit in c(1, 1e-3)) {
      x <- (0:4 + threshold) * unit
      fit <- suppressWarnings(jaspQualityControl:::.distributionParameters(
        x, "3ParameterWeibull", fix.arg = list(shape = 1, thres = threshold * unit)))

      # With shape fixed at one this is a shifted exponential: its scale MLE is the mean excess.
      expect_equal(fit$beta, 1)
      expect_equal(fit$threshold, min(x))
      expect_equal(fit$theta / mean(x - min(x)), 1, tolerance = 1e-4)
    }
  }
})

test_that("Invalid Weibull thresholds still give an estimation error", {
  x <- 1:5
  for (fixed in list(list(shape = 2, thres = 1), list(shape = 1, thres = 2), list(thres = 1))) {
    expect_error(suppressWarnings(jaspQualityControl:::.distributionParameters(
      x, "3ParameterWeibull", fix.arg = fixed)), "could not be estimated")
  }
})
