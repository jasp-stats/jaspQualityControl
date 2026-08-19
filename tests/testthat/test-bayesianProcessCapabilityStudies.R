test_that("process overview sample sizes are bounded and include the final observation", {
  expect_equal(jaspQualityControl:::.bpcsOverviewSampleSizes(3), 3)
  expect_equal(jaspQualityControl:::.bpcsOverviewSampleSizes(10), 3:10)

  sampleSizes <- jaspQualityControl:::.bpcsOverviewSampleSizes(100)
  expect_lte(length(sampleSizes), 50)
  expect_equal(tail(sampleSizes, 1), 100)
})

test_that("process overview uses the selected process criterion", {
  options <- list(
    processCriteria = list(
      list(lower = -Inf, label = "Incapable", upper = 1),
      list(lower = 1, label = "Capable", upper = 1.33),
      list(lower = 1.33, label = "Satisfactory", upper = 1.5),
      list(lower = 1.5, label = "Excellent", upper = 2),
      list(lower = 2, label = "Super", upper = Inf)
    ),
    processOverviewThreshold = "1.33"
  )

  expect_equal(
    jaspQualityControl:::.bpcsOverviewCriteria(options)$values,
    c(1, 1.33, 1.5, 2)
  )
  expect_equal(jaspQualityControl:::.bpcsOverviewThreshold(options), 1.33)
  expect_equal(
    jaspQualityControl:::.bpcsOverviewThreshold(
      modifyList(options, list(processOverviewThreshold = "interval2"))
    ),
    1.33
  )
})

test_that("process criteria require connected, unbounded regions", {
  options <- list(
    processCriteria = list(
      list(lower = -Inf, label = "Incapable", upper = 1),
      list(lower = 1.1, label = "Capable", upper = Inf)
    )
  )

  expect_error(
    jaspQualityControl:::.bpcsProcessCriteria(options),
    "Adjacent process criteria"
  )
})

test_that("process overview uses the selected likelihood", {
  expect_equal(
    jaspQualityControl:::.bpcsDistributionFromOptions(list(capabilityStudyType = "normalCapabilityAnalysis")),
    "normal"
  )
  expect_equal(
    jaspQualityControl:::.bpcsDistributionFromOptions(list(capabilityStudyType = "tCapabilityAnalysis")),
    "t"
  )
})

test_that("Cpk exceedance probabilities are accumulated above each criterion", {
  summary.bpcsOverviewTest <- function(object, interval_probability, ...) {
    list(interval_summary = data.frame(
      metric = c("Cp", "Cpk"),
      below = c(0, 0.1),
      interval1 = c(0, 0.2),
      interval2 = c(0, 0.3),
      interval3 = c(0, 0.15),
      above = c(0, 0.25)
    ))
  }

  fit <- structure(list(), class = "bpcsOverviewTest")
  expect_equal(
    jaspQualityControl:::.bpcsCpkExceedanceProbabilities(fit, c(1, 4 / 3, 1.5, 2)),
    c(0.9, 0.7, 0.4, 0.25)
  )
})

test_that("process overview probability lengths match the configured thresholds", {
  summary.bpcsOverviewShortTest <- function(object, interval_probability, ...) {
    list(interval_summary = data.frame(
      metric = "Cpk",
      below = 0.1,
      above = 0.9
    ))
  }

  fit <- structure(list(), class = "bpcsOverviewShortTest")
  expect_error(
    jaspQualityControl:::.bpcsCpkExceedanceProbabilities(fit, c(1, 1.33)),
    "Expected 3 interval probabilities"
  )
})
