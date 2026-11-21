nelsonBoundaryViolations <- function(x, ruleList) {
  jaspQualityControl:::.nelsonLaws(plotStatistics = x, center = 0, UCL = 3, LCL = -3, nSigmasControlLimits = 3,
                                   ruleList = ruleList)$violationList
}

test_that("NL1.1a Trend and oscillation count points and reset on ties and missing values", {
  trend <- list(rule3 = list(enabled = TRUE, k = 6))
  oscillation <- list(rule4 = list(enabled = TRUE, k = 14))
  expect_equal(nelsonBoundaryViolations(1:6, trend), list(test3 = 6))
  expect_equal(nelsonBoundaryViolations(7:1, trend), list(test3 = c(6, 7)))
  expect_equal(nelsonBoundaryViolations(c(1:5, 5, 6:9), trend), list(test3 = numeric()))
  expect_equal(nelsonBoundaryViolations(c(1:5, NA, 6:10), trend), list(test3 = numeric()))
  expect_equal(nelsonBoundaryViolations(rep(c(0, 1), 7), oscillation), list(test4 = 14))
  expect_equal(nelsonBoundaryViolations(c(rep(c(0, 1), 7), 0), oscillation), list(test4 = c(14, 15)))
  expect_equal(nelsonBoundaryViolations(c(rep(c(0, 1), 6), 1, 0), oscillation), list(test4 = numeric()))
  expect_equal(nelsonBoundaryViolations(c(rep(c(0, 1), 6), NA, 0), oscillation), list(test4 = numeric()))
  expect_equal(nelsonBoundaryViolations(c(rep(c(0, 1), 6), 2, 1), oscillation), list(test4 = numeric()))
})
