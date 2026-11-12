context("[Quality Control] Probability plot bands and Anderson-Darling p-value")

lineLayerData <- function(plot) {
  isLine <- vapply(plot$layers, function(layer) inherits(layer$geom, "GeomLine"), logical(1))
  lapply(plot$layers[isLine], function(layer) layer$data)
}

test_that("Weibull probability plot bands use the standard normal critical value", {
  # Previously the critical value was log(-log(1 - 0.975)) = 1.305 instead of qnorm(0.975) = 1.960
  set.seed(1)
  dataset <- data.frame(x = rweibull(30, shape = 2, scale = 10))
  options <- jaspTools::analysisOptions("processCapabilityStudies")
  options[["nullDistribution"]] <- "weibull"
  plot   <- jaspQualityControl:::.qcProbabilityPlotObject(options, dataset, "x", "")[[1]]
  layers <- lineLayerData(plot)
  expect_length(layers, 3) # fit line, lower and upper confidence band

  fit   <- fitdistrplus::fitdist(dataset[["x"]], "weibull")
  shape <- fit$estimate[["shape"]]
  scale <- fit$estimate[["scale"]]
  V     <- fit$vcov
  zp    <- layers[[1]][["zp"]]
  # delta method for log percentile log(t_p) = log(scale) + zp / shape
  seLog <- sqrt(V["scale", "scale"] / scale^2 + zp^2 * V["shape", "shape"] / shape^4 -
                  2 * zp * V["shape", "scale"] / (scale * shape^2))
  expect_equal(layers[[1]][["percentileEstimate"]], log(scale) + zp / shape, tolerance = 1e-4)
  expect_equal(layers[[3]][["percentileUpper"]] - layers[[1]][["percentileEstimate"]], qnorm(0.975) * seLog, tolerance = 1e-3)
  expect_equal(layers[[1]][["percentileEstimate"]] - layers[[2]][["percentileLower"]], qnorm(0.975) * seLog, tolerance = 1e-3)
})

test_that("Normal and lognormal probability plot bands use the standard normal critical value", {
  set.seed(1)
  dataset <- data.frame(x = rlnorm(30, meanlog = 1, sdlog = 0.4))
  options <- jaspTools::analysisOptions("processCapabilityStudies")
  for (distribution in c("normal", "lognormal")) {
    options[["nullDistribution"]] <- distribution
    layers <- lineLayerData(jaspQualityControl:::.qcProbabilityPlotObject(options, dataset, "x", "")[[1]])
    upper  <- layers[[3]][["percentileUpper"]] - layers[[1]][["percentileEstimate"]]
    lower  <- layers[[1]][["percentileEstimate"]] - layers[[2]][["percentileLower"]]
    expect_equal(upper, lower, info = distribution) # symmetric on the plotted scale
  }
})

test_that("Probability plot bands follow the units of the data", {
  # Fixed finite-difference steps of 0.001 crossed sigma = 0 for data varying on the order of 0.001, dropping the bands
  set.seed(1)
  x <- rweibull(20, shape = 3, scale = 2)
  options <- jaspTools::analysisOptions("processCapabilityStudies")
  for (distribution in c("normal", "lognormal")) { # the Weibull fit itself fails for data this small
    options[["nullDistribution"]] <- distribution
    original <- lineLayerData(jaspQualityControl:::.qcProbabilityPlotObject(options, data.frame(x = x), "x", "")[[1]])
    rescaled <- lineLayerData(jaspQualityControl:::.qcProbabilityPlotObject(options, data.frame(x = x / 1000), "x", "")[[1]])
    expect_length(rescaled, 3)
    # the normal plot uses the measurement scale, the lognormal plot the log scale
    toOriginal <- if (distribution == "normal") function(v) v * 1000 else function(v) v + log(1000)
    expect_equal(toOriginal(rescaled[[2]][["percentileLower"]]), original[[2]][["percentileLower"]], tolerance = 1e-4, info = distribution)
    expect_equal(toOriginal(rescaled[[3]][["percentileUpper"]]), original[[3]][["percentileUpper"]], tolerance = 1e-4, info = distribution)
  }
})

adStar <- function(ad, n) ad * (1 + 0.75 / n + 2.25 / n^2)
adPValue <- function(ad, n) jaspQualityControl:::.qcAndersonDarlingPValueNormal(ad, n)

test_that("Anderson-Darling p-value tests the modified statistic in every branch", {
  # D'Agostino & Stephens (1986): branch by adStar, not by the unmodified ad
  n  <- 20
  ad <- 0.59 # ad < 0.6 <= adStar previously fell through to the p-value for known parameters
  expect_gte(adStar(ad, n), 0.6)
  a <- adStar(ad, n)
  expect_equal(adPValue(ad, n), exp(1.2937 - 5.709 * a + 0.0186 * a^2))

  # one value inside every branch
  ns  <- c(10, 30, 100)
  ads <- c(0.1, 0.25, 0.45, 1.5)
  for (n in ns) for (ad in ads) {
    a <- adStar(ad, n)
    expected <- if (a >= 0.6) exp(1.2937 - 5.709 * a + 0.0186 * a^2)
      else if (a > 0.34) exp(0.9177 - 4.279 * a - 1.38 * a^2)
      else if (a > 0.2) 1 - exp(-8.318 + 42.796 * a - 59.938 * a^2)
      else 1 - exp(-13.436 + 101.14 * a - 223.73 * a^2)
    expect_equal(adPValue(ad, n), expected, info = paste("n =", n, "ad =", ad))
  }
})

test_that("Anderson-Darling p-value stays a small probability for very large statistics", {
  # the quadratic in the exponent rises again for large adStar (p = Inf at adStar ~ 850); capped as in nortest::ad.test
  expect_equal(adPValue(9.9, 1e6), exp(1.2937 - 5.709 * adStar(9.9, 1e6) + 0.0186 * adStar(9.9, 1e6)^2))
  expect_equal(adPValue(10, 1e6), 3.7e-24)
  expect_equal(adPValue(850, 5000), 3.7e-24)
  expect_equal(adPValue(Inf, 2000), 3.7e-24)
})
