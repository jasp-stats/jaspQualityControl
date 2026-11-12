context("[Quality Control] Laney p' and u' charts with unequal subgroup sizes")

.laneyRun <- function(counts, sizes, chart) {
  options <- jaspTools::analysisOptions("attributesCharts")
  options$defectiveOrDefect <- "count"
  options$total <- "size"
  if (chart == "laneyPPrimeChart") {
    options$attributesChart <- "defectives"
    options$attributesChartDefectivesChartType <- chart
  } else {
    options$attributesChart <- "defects"
    options$attributesChartDefectsChartType <- chart
  }
  jaspTools::runAnalysis("attributesCharts", data.frame(count = counts, size = sizes), options, view = FALSE)
}

.laneyStepLimits <- function(results, key) {
  plot  <- results$state$figures[[results$results[[key]]$data]]$obj
  built <- ggplot2::ggplot_build(plot)$data
  steps <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomStep"), logical(1)))
  # the first two step layers are the per-subgroup UCL and LCL
  list(UCL = built[[steps[1]]]$y, LCL = built[[steps[2]]]$y)
}

# Laney (2002): z_i = (x_i - xbar) / s_i with each subgroup's own s_i,
# sigma_z = mean |z_i - z_{i-1}| / 1.128, limits xbar +- 3 * s_i * sigma_z (LCL truncated at 0).
.laneyReference <- function(counts, sizes, chart) {
  center <- sum(counts) / sum(sizes)
  s      <- if (chart == "laneyPPrimeChart") sqrt(center * (1 - center) / sizes) else sqrt(center / sizes)
  z      <- (counts / sizes - center) / s
  sigmaZ <- mean(abs(diff(z))) / 1.128
  list(UCL = center + 3 * s * sigmaZ, LCL = pmax(0, center - 3 * s * sigmaZ))
}

counts <- c(1, 40, 2, 50, 1, 60)
sizes  <- c(10, 1000, 10, 1000, 10, 1000)

for (chart in c("laneyPPrimeChart", "laneyUPrimeChart")) {
  key <- if (chart == "laneyPPrimeChart") "LaneyPPlot" else "LaneyUPlot"

  test_that(paste(chart, "z-scores use each subgroup's own size"), {
    results <- .laneyRun(counts, sizes, chart)
    expect_identical(results$status, "complete")
    limits   <- .laneyStepLimits(results, key)
    expected <- .laneyReference(counts, sizes, chart)
    expect_equal(limits$UCL, expected$UCL, tolerance = 1e-10)
    expect_equal(limits$LCL, expected$LCL, tolerance = 1e-10)
    # hand-checked values for this example
    expect_equal(limits$UCL[1:2], c(0.4084, 0.0866), tolerance = 1e-3)
    expect_equal(limits$LCL[2], 0.0151, tolerance = 1e-2)
  })

  test_that(paste(chart, "reversing the subgroup order preserves the limits"), {
    limits         <- .laneyStepLimits(.laneyRun(counts, sizes, chart), key)
    reversedLimits <- .laneyStepLimits(.laneyRun(rev(counts), rev(sizes), chart), key)
    expect_equal(rev(reversedLimits$UCL), limits$UCL, tolerance = 1e-10)
    expect_equal(rev(reversedLimits$LCL), limits$LCL, tolerance = 1e-10)
  })

  test_that(paste(chart, "equal subgroup sizes give the constant-size Laney limits"), {
    equalCounts <- c(3, 5, 2, 8, 4, 6, 20)
    equalSizes  <- rep(50, 7)
    limits   <- .laneyStepLimits(.laneyRun(equalCounts, equalSizes, chart), key)
    expected <- .laneyReference(equalCounts, equalSizes, chart)
    expect_equal(limits$UCL, expected$UCL, tolerance = 1e-10)
    expect_equal(limits$LCL, expected$LCL, tolerance = 1e-10)
  })
}
