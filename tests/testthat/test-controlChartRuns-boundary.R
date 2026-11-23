nelsonBoundaryViolations <- function(x, ruleList) {
  jaspQualityControl:::.nelsonLaws(plotStatistics = x, center = 0, UCL = 3, LCL = -3, nSigmasControlLimits = 3,
                                   ruleList = ruleList)$violationList
}

test_that("NL1.1b Within-band runs signal at the threshold and reset", {
  within <- list(rule7 = list(enabled = TRUE, k = 15))
  expect_equal(nelsonBoundaryViolations(rep(0.1, 15), within), list(test7 = 15))
  expect_equal(nelsonBoundaryViolations(c(rep(0.1, 14), 2, rep(0.1, 15)), within), list(test7 = 30))
  expect_equal(nelsonBoundaryViolations(c(rep(0.1, 14), NA, rep(0.1, 15)), within), list(test7 = 30))
})
