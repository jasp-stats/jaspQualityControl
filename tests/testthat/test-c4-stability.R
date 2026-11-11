# Independent gamma recurrence, with exact half-integer starting values.
c4Reference <- function(n) {
  start <- if (n %% 2 == 0) 2 else 3
  value <- if (start == 2) sqrt(2 / pi) else sqrt(pi) / 2
  if (n > start)
    for (k in seq(start, n - 2, by = 2))
      value <- value * k / sqrt((k - 1) * (k + 1))
  value
}
qcFunction <- function(name) getFromNamespace(name, 'jaspQualityControl')

test_that('c4 stays finite across the direct-gamma overflow boundary', {
  n <- c(2, 10, 100, 340, 344, 345, 401, 1000)
  observed <- vapply(n, function(n) qcFunction('KnownControlStats.RS')(n)$constants[3], numeric(1))
  expect_equal(observed, vapply(n, c4Reference, numeric(1)), tolerance = 2e-12)
  expect_true(all(is.finite(observed)))
  for (n in 0:1)
    expect_identical(qcFunction('KnownControlStats.RS')(n)$constants[3:4], c(0, 0))
})

test_that('pooled, S-bar and uncorrected estimators preserve subgroup contracts', {
  for (m in c(85, 86, 100)) {
    set.seed(32)
    data <- as.data.frame(matrix(rnorm(m * 5, 10, 2), m, 5))
    pooled <- sqrt(mean(apply(data, 1, var)))
    expect_equal(qcFunction('.sdXbar')(data, 'pooled'), pooled / c4Reference(4 * m + 1), tolerance = 2e-12)
    expect_equal(qcFunction('.sdXbar')(data, 'pooled', FALSE), pooled)
    expect_equal(qcFunction('.sdXbar')(data, 's'), mean(apply(data, 1, sd)) / c4Reference(5), tolerance = 2e-12)
    expect_equal(qcFunction('.sdXbar')(data, 'r'), mean(apply(data, 1, function(x) diff(range(x)))) / 2.326)
  }
  data[1, 1] <- NA_real_
  data[2, 2:5] <- NA_real_
  data[3, ] <- NA_real_
  retained <- data[rowSums(!is.na(data)) > 1, ]
  sizes <- rowSums(!is.na(retained))
  nu <- sum(sizes - 1)
  pooled <- sqrt(sum((sizes - 1) * apply(retained, 1, var, na.rm = TRUE)) / nu)
  expect_equal(qcFunction('.sdXbar')(data, 'pooled'), pooled / c4Reference(nu + 1), tolerance = 2e-12)
})

# The EWMA form only offers S-bar and R-bar; the capability study is where the GUI reaches the pooled estimator.
test_that('pooled within-subgroup sd reaches actual analysis output', {
  for (m in c(85, 86, 100)) {
    set.seed(32)
    data <- as.data.frame(matrix(rnorm(m * 5, 10, 2), m, 5))
    names(data) <- paste0('x', 1:5)
    options <- jaspTools::analysisOptions('processCapabilityStudies')
    options$dataFormat <- 'wideFormat'
    options$measurementsWideFormat <- names(data)
    options$controlChartSdEstimationMethodGroupSize <- 'largerThanOne'
    options$controlChartSdEstimationMethodGroupSizeLargerThanOne <- 'pooled'
    options$lowerSpecificationLimit <- TRUE
    options$lowerSpecificationLimitValue <- 0
    options$upperSpecificationLimit <- TRUE
    options$upperSpecificationLimitValue <- 20
    results <- jaspTools::runAnalysis('processCapabilityStudies', data, options, view = FALSE)
    expect_identical(results$status, 'complete')
    summaryTable <- results$results$capabilityAnalysis$collection$capabilityAnalysis_normalCapabilityAnalysis$collection$capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable$data
    sigma <- sqrt(mean(apply(data, 1, var))) / c4Reference(4 * m + 1)
    expect_equal(as.numeric(summaryTable[[1]]$sdw), sigma, tolerance = 2e-12)
  }
})


test_that('large S-chart subgroups produce finite actual limits', {
  for (n in c(344, 345, 401)) {
    set.seed(19)
    data <- data.frame(value = rnorm(6 * n, 10, 2))
    options <- jaspTools::analysisOptions('variablesChartsSubgroups')
    options$testSet <- 'jaspDefault'
    options$measurementLongFormat <- 'value'
    options$manualSubgroupSizeValue <- n
    options$chartType <- 'xBarAndS'
    options$xBarAndSUnbiasingConstant <- TRUE
    results <- jaspTools::runAnalysis('variablesChartsSubgroups', data, options, view = FALSE)
    expect_identical(results$status, 'complete')
    key <- results$results$controlCharts$collection$controlCharts_plot$data
    plots <- results$state$figures[[key]]$obj$subplots
    expect_length(plots, 2)
    groups <- matrix(data$value, ncol = n, byrow = TRUE)
    sigma <- mean(apply(groups, 1, sd)) / c4Reference(n)
    for (plot in plots) {
      expect_true(all(is.finite(plot$data$UCL)))
      expect_true(all(is.finite(plot$data$LCL)))
    }
    sLimits <- plots[[2]]$data
    # sqrt(1 - c4^2) amplifies rounding in c4 near one.
    expect_equal(unique(sLimits$UCL), sigma * (c4Reference(n) + 3 * sqrt(1 - c4Reference(n)^2)),
                 tolerance = 2e-10)
  }
})
