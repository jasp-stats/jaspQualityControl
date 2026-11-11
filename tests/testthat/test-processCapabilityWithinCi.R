context("[Quality Control] Process Capability Within CI Degrees of Freedom")

# Degrees of freedom of the within std. dev. ----

test_that("Within std. dev. degrees of freedom follow the estimation method", {
  withinDf <- jaspQualityControl:::.qcCapabilityWithinSdDf

  # 4 subgroups of size 2: sum(n_i - 1) = 4
  equalSizes <- data.frame(a = c(1, 2, 3, 4), b = c(2, 4, 5, 7))
  expect_equal(withinDf(equalSizes, "pooled"), 4)
  expect_equal(withinDf(equalSizes, "r"),      0.9 * 4)
  expect_equal(withinDf(equalSizes, "s"),      0.88 * 4)

  # sizes 3, 1, 3, 2: the subgroup of size 1 does not contribute, mean size of the others is 8/3
  unequalSizes <- data.frame(a = c(1, 2, 3, 4), b = c(2, NA, 4, 5), c = c(3, NA, 6, NA))
  expect_equal(withinDf(unequalSizes, "pooled"), 5)
  expect_equal(withinDf(unequalSizes, "r"),      0.9 * 5)
  expect_equal(withinDf(unequalSizes, "s"),      0.88 * 5)

  # moving ranges: number of complete moving ranges
  individuals <- data.frame(x = c(1, 2, NA, 4, 5, 6))
  expect_equal(withinDf(individuals, "movingRange", 2), 3)
  expect_equal(withinDf(individuals, "movingRange", 3), 1)
  expect_equal(withinDf(data.frame(x = 1:10), "movingRange", 2), 9)
  expect_equal(withinDf(data.frame(x = 1:10), "movingRange", 4), 7)

  # no degrees of freedom
  expect_true(is.na(withinDf(data.frame(a = c(1, NA), b = c(NA, 2)), "pooled")))
  expect_true(is.na(withinDf(data.frame(x = c(1, NA, 2)), "movingRange", 2)))
})

test_that("S-bar degrees of freedom factor matches the reference table", {
  sBarFactor <- jaspQualityControl:::.qcSBarDfFactor
  expect_equal(sapply(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 17, 18, 64, 65, 100), sBarFactor),
               c(0.88, 0.92, 0.94, 0.95, 0.96, 0.96, 0.97, 0.97, 0.98, 0.98, 0.99, 0.99, 1, 1))
  # non-integer mean subgroup size uses the next smaller size
  expect_equal(sBarFactor(4.9), 0.94)
})

# Confidence intervals in the process capability (within) table ----

.pcWithinCiData <- data.frame(
  X1 = c(9.348, 10.235, 10.078, 9.038, 9.929, 11.445, 10.452, 10.041, 9.578, 7.947, 11.131, 8.539, 10.74, 11.909, 8.556, 10.702, 9.738, 8.428, 8.485, 8.398),
  X2 = c(9.469, 8.538, 10.688, 12.1, 8.713, 10.788, 10.769, 10.332, 8.992, 9.881, 9.72, 10.563, 9.628, 10.977, 9.625, 11.053, 8.951, 8.74, 13.241, 9.583)
)

.pcWithinCiOptions <- function(sdMethod) {
  options <- analysisOptions("processCapabilityStudies")
  options$dataFormat <- "wideFormat"
  options$measurementsWideFormat <- c("X1", "X2")
  options$controlChartSdEstimationMethodGroupSizeLargerThanOne <- sdMethod
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- 6.5
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- 13
  options$processCapabilityTableCi <- TRUE
  options$processCapabilityTableCiLevel <- 0.9
  options
}

.pcWithinTable <- function(results) {
  results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]]
}

# 20 subgroups of size 2, N = 40; the interval follows from Cp, Cpk and the degrees of freedom of the method
.pcExpectedWithinCi <- function(row, df, level = 0.9, N = 40) {
  alpha <- 1 - level
  halfWidth <- qnorm(1 - alpha / 2) * sqrt(1 / (9 * N) + row[["cpk"]]^2 / (2 * df))
  c(cplci = row[["cp"]] * sqrt(qchisq(alpha / 2, df) / df), cpuci = row[["cp"]] * sqrt(qchisq(1 - alpha / 2, df) / df),
    cpklci = row[["cpk"]] - halfWidth, cpkuci = row[["cpk"]] + halfWidth)
}

for (sdMethod in c("rBar", "sBar", "pooled")) {
  results <- runAnalysis("processCapabilityStudies", .pcWithinCiData, .pcWithinCiOptions(sdMethod))
  test_that(paste0("Within Cp/Cpk confidence intervals use the degrees of freedom of the ", sdMethod, " method"), {
    row <- .pcWithinTable(results)[["data"]][[1]]
    df  <- switch(sdMethod, rBar = 0.9 * 20 * (2 - 1), sBar = 0.88 * 20 * (2 - 1), pooled = 20)
    expected <- .pcExpectedWithinCi(row, df)
    # Cp and Cpk in the table are rounded to 3 decimals
    expect_equal(unlist(row[names(expected)]), expected, tolerance = 2e-3)
  })
}

results <- runAnalysis("processCapabilityStudies", .pcWithinCiData, .pcWithinCiOptions("rBar"))

test_that("Process capability (within) table with R-bar confidence intervals matches", {
  table <- .pcWithinTable(results)[["data"]]
  jaspTools::expect_equal_tables(table, list(1, 0.946, 0.673, 1.219, 1.055, 0.723, 0.946, 1.267))
})

options <- .pcWithinCiOptions("rBar")
options$historicalStdDev <- TRUE
options$historicalStdDevValue <- 1
results <- runAnalysis("processCapabilityStudies", .pcWithinCiData, options)

test_that("Within Cp/Cpk confidence intervals are not computed with a historical std. dev.", {
  table <- .pcWithinTable(results)
  row <- table[["data"]][[1]]
  expect_identical(unlist(row[c("cplci", "cpuci", "cpklci", "cpkuci")]), c(cplci = "*", cpuci = "*", cpklci = "*", cpkuci = "*"))
  expect_true(any(grepl("Confidence intervals for Cp and Cpk are not computed when a historical standard deviation is used.",
                        unlist(table[["footnotes"]]), fixed = TRUE)))
})

options$stagesWideFormat <- "stage"
results <- runAnalysis("processCapabilityStudies", cbind(.pcWithinCiData, stage = rep(c("A", "B"), each = 10)), options)

test_that("Within Cp/Cpk confidence intervals are not computed with a historical std. dev. and stages", {
  rows <- .pcWithinTable(results)[["data"]]
  ciCells <- lapply(rows, function(row) unname(unlist(row[c("cplci", "cpuci", "cpklci", "cpkuci")])))
  expect_identical(ciCells, list(rep("*", 4), rep("*", 4), rep("-", 4)))
})
