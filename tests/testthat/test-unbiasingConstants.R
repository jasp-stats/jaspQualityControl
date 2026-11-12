context("[Quality Control] Unbiasing constants d2 and d3")

# Published d2 (n = 2..50) and d3 (n = 2..25): Wheeler and Chambers (1992); Montgomery, Introduction to SQC, App. VI
d2Table <- c(1.128, 1.693, 2.059, 2.326, 2.534, 2.704, 2.847, 2.970, 3.078, 3.173, 3.258, 3.336, 3.407, 3.472, 3.532,
             3.588, 3.640, 3.689, 3.735, 3.778, 3.819, 3.858, 3.895, 3.931, 3.964, 3.997, 4.027, 4.057, 4.086, 4.113,
             4.139, 4.165, 4.189, 4.213, 4.236, 4.259, 4.280, 4.301, 4.322, 4.341, 4.361, 4.379, 4.398, 4.415, 4.433,
             4.450, 4.466, 4.482, 4.498)
d3Table <- c(0.8525, 0.8884, 0.8798, 0.8641, 0.8480, 0.8332, 0.8198, 0.8078, 0.7971, 0.7873, 0.7785, 0.7704, 0.7630,
             0.7562, 0.7499, 0.7441, 0.7386, 0.7335, 0.7287, 0.7242, 0.7199, 0.7159, 0.7121, 0.7084)
rangeConstants <- function(n) jaspQualityControl:::KnownControlStats.RS(n, 0)$constants[1:2]

test_that("Numerical integration reproduces the published d2 and d3 tables", {
  exact <- sapply(2:50, jaspQualityControl:::.normalRangeConstants)
  testthat::expect_equal(round(exact["d2", ], 3), d2Table)
  testthat::expect_equal(round(exact["d3", 1:24], 4), d3Table)
})

test_that("Constants for n <= 50 are unchanged", {
  constants <- t(sapply(2:50, rangeConstants))
  testthat::expect_equal(constants[, 1], d2Table)
  testthat::expect_equal(constants[1:24, 2], d3Table)
  n <- 26:50
  testthat::expect_equal(constants[25:49, 2], 0.80818 - 0.0051871 * n + 0.00005098 * n^2 - 0.00000019 * n^3)
})

test_that("d2 and d3 beyond n = 50 match published values (Harter, 1960)", {
  testthat::expect_equal(round(rangeConstants(100), 4), c(5.0152, 0.6052))
  testthat::expect_equal(round(rangeConstants(500)[1], 4), 6.0734)
  testthat::expect_equal(round(rangeConstants(1000)[1], 4), 6.4829)
})

test_that("d2 increases, d3 decreases and stays positive for large n", {
  ns <- c(25:120, 137, 231, 250, 500, 1000, 5000, 1e5, 1e6, 2000001, 1e8)
  constants <- t(sapply(ns, rangeConstants))
  testthat::expect_true(all(is.finite(constants)))
  testthat::expect_true(all(diff(constants[, 1]) > 0))
  testthat::expect_true(all(diff(constants[, 2]) < 0))
  testthat::expect_true(all(constants[, 2] > 0))
  # step from the tabulated d2(50) = 4.498 to the exact d2(51) = 4.51356
  testthat::expect_equal(round(constants[ns == 51, 1] - constants[ns == 50, 1], 5), 0.01556)
})

test_that("R and S chart limits work with named subgroup sizes (as from rowSums)", {
  n <- c(a = 5, b = 60)
  limitsR <- jaspQualityControl:::.controlLimits(sigma = 2, n = n, type = "r")
  limitsS <- jaspQualityControl:::.controlLimits(sigma = 2, n = n, type = "s")
  d2d3 <- sapply(unname(n), rangeConstants)
  c4 <- sapply(unname(n), function(x) jaspQualityControl:::KnownControlStats.RS(x, 0)$constants[3])
  testthat::expect_equal(unname(limitsR$UCL), 2 * (d2d3[1, ] + 3 * d2d3[2, ]))
  testthat::expect_equal(unname(limitsS$UCL), 2 * (c4 + 3 * sqrt(1 - c4^2)))
  testthat::expect_equal(names(jaspQualityControl:::KnownControlStats.RS(n["b"], 0)$constants), rep("b", 4))
})

test_that("d2 and d3 for large n agree with simulation", {
  set.seed(1)
  ranges <- replicate(20000, diff(range(rnorm(250))))
  constants <- rangeConstants(250)
  testthat::expect_equal(unname(constants[1]), mean(ranges), tolerance = 0.002)
  testthat::expect_equal(unname(constants[2]), sd(ranges), tolerance = 0.02)
})

# Subgroups of 250 from N(0, 1): R-bar must estimate the same sigma as S-bar
set.seed(1)
largeSubgroups <- data.frame(Measurement = rnorm(20 * 250))
largeSubgroupsWide <- as.data.frame(matrix(largeSubgroups$Measurement, ncol = 250, byrow = TRUE))
sigmaRbar <- mean(apply(largeSubgroupsWide, 1, function(x) diff(range(x)))) / 5.638368

test_that("Within std. dev. from R-bar agrees with S-bar for subgroups of 250", {
  testthat::expect_equal(jaspQualityControl:::.sdXbar(largeSubgroupsWide, type = "r"), sigmaRbar, tolerance = 1e-6)
  testthat::expect_equal(jaspQualityControl:::.sdXbar(largeSubgroupsWide, type = "r"),
                         jaspQualityControl:::.sdXbar(largeSubgroupsWide, type = "s"), tolerance = 0.03)
})

test_that("S-bar, pooled and S chart calculations do not use d2 and d3", {
  cache <- jaspQualityControl:::.normalRangeConstantsCache
  rm(list = ls(cache), envir = cache)
  jaspQualityControl:::.sdXbar(largeSubgroupsWide, type = "s")
  jaspQualityControl:::.sdXbar(largeSubgroupsWide, type = "pooled")
  jaspQualityControl:::.controlLimits(sigma = 1, n = c(60, 250), type = "s")
  jaspQualityControl:::.controlChart_calculations(largeSubgroupsWide, plotType = "s")
  testthat::expect_length(ls(cache), 0)
})

options <- analysisOptions("timeWeightedCharts")
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 250
options$cumulativeSumChart <- FALSE
options$exponentiallyWeightedMovingAverageChart <- TRUE
options$exponentiallyWeightedMovingAverageChartSdSource <- "data"
options$exponentiallyWeightedMovingAverageChartSdMethod <- "r"
options$exponentiallyWeightedMovingAverageChartLambda <- 0.3
results <- runAnalysis("timeWeightedCharts", largeSubgroups, options, view = FALSE)

test_that("EWMA chart with R-bar and subgroups of 250 has correct limits and no false alarms", {
  plotName <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_plot"]][["data"]]
  limits <- results[["state"]][["figures"]][[plotName]][["obj"]][["data"]]
  lambda <- 0.3
  i <- seq_len(20)
  expectedHalfWidth <- 3 * sigmaRbar * sqrt(lambda / (2 - lambda) * (1 - (1 - lambda)^(2 * i)) / 250)
  testthat::expect_equal(head(limits$UCL - limits$center, 20), expectedHalfWidth, tolerance = 1e-6) # last row repeats the last step
  table <- results[["results"]][["EWMAPlot"]][["collection"]][["EWMAPlot_table"]][["data"]]
  jaspTools::expect_equal_tables(table, list("No test violations occurred."))
})

options <- analysisOptions("processCapabilityStudies")
options$testSet <- "jaspDefault"
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 250
options$capabilityStudyType <- "normalCapabilityAnalysis"
options$controlChart <- FALSE
options$lowerSpecificationLimit <- TRUE
options$upperSpecificationLimit <- TRUE
options$lowerSpecificationLimitValue <- -3
options$upperSpecificationLimitValue <- 3
withinCapability <- list()
for (sdMethod in c("rBar", "sBar")) {
  options$controlChartSdEstimationMethodGroupSizeLargerThanOne <- sdMethod
  results <- runAnalysis("processCapabilityStudies", largeSubgroups, options, view = FALSE)
  tables <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_normalCapabilityAnalysis"]][["collection"]]
  withinCapability[[sdMethod]] <- c(
    sdw = tables[["capabilityAnalysis_normalCapabilityAnalysis_processSummaryTable"]][["data"]][[1]][["sdw"]],
    cp  = tables[["capabilityAnalysis_normalCapabilityAnalysis_capabilityTableWithin"]][["data"]][[1]][["cp"]])
}

test_that("Process capability with R-bar and subgroups of 250 gives the correct within std. dev. and Cp", {
  testthat::expect_equal(unname(withinCapability[["rBar"]]["sdw"]), sigmaRbar, tolerance = 1e-6)
  testthat::expect_equal(withinCapability[["rBar"]], withinCapability[["sBar"]], tolerance = 0.03)
})
