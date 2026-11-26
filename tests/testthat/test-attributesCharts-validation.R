context("Attributes charts active-family validation")

.acvRun <- function(counts = c(12, 18, 9), sizes = c(10, 10, 10),
                    family = "defects", chart = "uChart", inactive = "npChart") {
  options <- jaspTools::analysisOptions("attributesCharts")
  options$defectiveOrDefect <- "count"
  options$total <- "size"
  options$attributesChart <- family
  options$attributesChartDefectivesChartType <- if (family == "defectives") chart else inactive
  options$attributesChartDefectsChartType <- if (family == "defects") chart else "uChart"
  jaspTools::runAnalysis("attributesCharts", data.frame(count = counts, size = sizes), options, view = FALSE)
}

test_that("Poisson charts allow rates above one and fractional exposures", {
  for (chart in c("uChart", "laneyUPrimeChart")) {
    key <- if (chart == "uChart") "UchartPlot" else "LaneyUPlot"
    for (sizes in list(c(10, 10, 10), c(0.5, 1.5, 2.5))) {
      result <- .acvRun(sizes = sizes, chart = chart)
      expect_identical(result$status, "complete")
      plot <- result$state$figures[[result$results[[key]]$data]]$obj
      # The plotted observations must be the count/exposure ratios, independently of qcc limits.
      pointIndex <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))[1]
      expect_equal(ggplot2::ggplot_build(plot)$data[[pointIndex]]$y, c(12, 18, 9) / sizes)
    }
  }
})

test_that("every active binomial chart enforces binomial support", {
  for (chart in c("pChart", "npChart", "laneyPPrimeChart")) {
    for (counts in list(c(12, 2, 1), c(-1, 2, 1), c(0.5, 2, 1)))
      expect_identical(.acvRun(counts, family = "defectives", chart = chart)$status, "validationError")
    expect_identical(.acvRun(c(0, 10, 4), family = "defectives", chart = chart)$status, "complete")
    for (sizes in list(c(10, 10.5, 10), c(0, 10, 10), c(-1, 10, 10),
                        c(Inf, 10, 10)))
      expect_identical(.acvRun(c(0, 1, 2), sizes, "defectives", chart)$status, "validationError")
  }
})

test_that("active charts reject invalid counts and exposures", {
  for (chart in c("uChart", "laneyUPrimeChart", "cChart")) {
    for (counts in list(c(-1, 2, 1), c(0.5, 2, 1), c(Inf, 2, 1)))
      expect_identical(.acvRun(counts, chart = chart)$status, "validationError")
  }
  for (chart in c("uChart", "laneyUPrimeChart")) {
    for (sizes in list(c(0, 10, 10), c(-1, 10, 10), c(Inf, 10, 10)))
      expect_identical(.acvRun(sizes = sizes, chart = chart)$status, "validationError")
  }
})

test_that("c charts ignore inactive binomial choices", {
  for (inactive in c("npChart", "pChart", "laneyPPrimeChart"))
    expect_identical(.acvRun(chart = "cChart", inactive = inactive)$status, "complete")
  # These fresh runs exercise option combinations, not native persistent transitions.
})

test_that("incomplete assignments remain ready for further input", {
  options <- jaspTools::analysisOptions("attributesCharts")
  for (family in c("defectives", "defects", "xmr")) {
    options$attributesChart <- family
    for (selection in c("none", "count", "size")) {
      options$defectiveOrDefect <- if (selection == "count") "count" else ""
      options$total <- if (selection == "size") "size" else ""
      result <- jaspTools::runAnalysis("attributesCharts", data.frame(count = 1:3, size = 10),
                                       options, view = FALSE)
      expect_identical(result$status, "complete")
    }
  }
})


test_that("c charts retain validation of their assigned sample-size column", {
  counts <- c(2, 20, 4, 6, 8)
  for (sizes in list(c(10, Inf, 10, 10, 10), c(10, -1, 10, 10, 10))) {
    result <- .acvRun(counts, sizes, chart = "cChart")
    expect_identical(result$status, "validationError")
    expect_null(result$results$CchartPlot)
    expect_null(result$results$NelsonTable)
  }
  # a sample-size column without any numeric value is rejected by the form, as in JASP
  expect_error(.acvRun(counts, rep(NA_real_, 5), chart = "cChart"),
               "Minimum number of numeric values is 1")
  result <- .acvRun(counts, rep(10, 5), chart = "cChart")
  expect_identical(result$status, "complete")
  plot <- result$state$figures[[result$results$CchartPlot$data]]$obj
  pointIndex <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))[1]
  expect_equal(ggplot2::ggplot_build(plot)$data[[pointIndex]]$y, counts)
})

.acvPoints <- function(result, key) {
  plot <- result$state$figures[[result$results[[key]]$data]]$obj
  pointIndex <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))[1]
  ggplot2::ggplot_build(plot)$data[[pointIndex]]$y
}

test_that("rows with a missing count or sample size are removed with a note", {
  # Blank rows inside the data and below shorter columns are both dropped, not rejected.
  counts <- c(2, NA, 4, 6, 3, 5, NA)
  sizes  <- c(10, 10, NA, 10, 10, 10, NA)
  kept   <- c(1, 4, 5, 6)
  charts <- list(pChart = c("defectives", "PchartPlot", "p"),  npChart = c("defectives", "NPchartPlot", "np"),
                 cChart = c("defects", "CchartPlot", "c"),     uChart  = c("defects", "UchartPlot", "u"))
  for (chart in names(charts)) {
    family <- charts[[chart]][1]
    result <- .acvRun(counts, sizes, family = family, chart = chart)
    expect_identical(result$status, "complete")
    expect_match(result$results$missingValuesNote$rawtext, "Removed 3 observation\\(s\\) with missing values")
    plotted <- switch(charts[[chart]][3], p = counts[kept] / sizes[kept], u = counts[kept] / sizes[kept], counts[kept])
    expect_equal(.acvPoints(result, charts[[chart]][2]), plotted)
  }

  # Complete data have no note, and no complete rows stop the analysis with an explanation.
  expect_null(.acvRun(c(2, 4, 6), c(10, 10, 10), "defectives", "pChart")$results$missingValuesNote)
  result <- .acvRun(c(2, NA, 6), c(NA, 10, NA), "defectives", "pChart")
  expect_identical(result$status, "validationError")
  expect_match(result$results$errorMessage, "No observations remain")
})

test_that("valid np data retain observations and binomial control limits", {
  counts <- c(0, 2, 4, 7, 3)
  sampleSize <- 10
  result <- .acvRun(counts, rep(sampleSize, length(counts)), "defectives", "npChart")
  expect_identical(result$status, "complete")
  plot <- result$state$figures[[result$results$NPchartPlot$data]]$obj
  built <- ggplot2::ggplot_build(plot)$data
  pointIndex <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))[1]
  limitIndices <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomHline"), logical(1)))
  expect_equal(built[[pointIndex]]$y, counts)
  # Independently calculate the fitted np center and three-sigma binomial limits.
  center <- mean(counts)
  standardDeviation <- sqrt(center * (1 - center / sampleSize))
  expectedLimits <- c(center, max(0, center - 3 * standardDeviation), center + 3 * standardDeviation)
  actualLimits <- unlist(lapply(built[limitIndices], function(layer) layer$yintercept), use.names = FALSE)
  expect_equal(sort(actualLimits), sort(expectedLimits))
})
