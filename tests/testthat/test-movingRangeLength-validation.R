test_that("Moving-range length larger than the data is reported before allocating windows", {
  expect_error(jaspQualityControl:::.movingRangeSigma(1:3, 100000, stage = "A"),
               "stage 'A' has fewer than", class = "qcDataError")
})
