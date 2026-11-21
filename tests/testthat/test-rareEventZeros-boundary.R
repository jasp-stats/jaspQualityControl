test_that("Zero runs signal at the threshold and reset", {
  violations <- function(x) jaspQualityControl:::.nelsonLaws(x, 0, 3, -3, 3,
    list(rule9 = list(enabled = TRUE, k = 3)))$violationList
  expect_equal(violations(rep(0, 3)), list(test9 = 3))
  expect_equal(violations(c(0, 0, 1, 0, 0, 0)), list(test9 = 6))
  expect_equal(violations(c(0, 0, NA, 0, 0, 0)), list(test9 = 6))
})
