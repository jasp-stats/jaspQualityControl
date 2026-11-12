context("[Quality Control] Log-logistic density in the process capability histogram")

test_that("Histogram density line for the log-logistic distribution uses the log-logistic fit", {
  set.seed(1)
  x <- exp(rlogis(50, location = log(10), scale = 0.1)) # log-logistic with scale 10, shape 10
  options <- list(nullDistribution = "loglogistic", histogramDensityLine = TRUE, histogramBinNumber = 10,
                  histogramBinBoundaryDirection = "left")
  plot <- getFromNamespace(".qcDistributionPlotObject", "jaspQualityControl")(options, data.frame(x = x), "x", "")[[1]]
  densityLayer <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomLine"), logical(1)))
  expect_length(densityLayer, 1)
  curve <- ggplot2::layer_data(plot, densityLayer)
  expect_true(all(is.finite(curve$y)))

  # independent reference: log(X) is logistic(log(scale), 1 / shape), and the log-logistic MLE equals the logistic MLE of log(x)
  z <- log(x)
  opt <- optim(c(median(z), log(sd(z))), function(p) -sum(dlogis(z, p[1], exp(p[2]), log = TRUE)), method = "BFGS", control = list(reltol = 1e-14))
  expect_identical(opt$convergence, 0L)
  expected <- flexsurv::dllogis(curve$x, shape = exp(-opt$par[2]), scale = exp(opt$par[1]))
  expect_equal(curve$y, expected, tolerance = 1e-3)
  expect_gt(max(curve$y), 0.1) # a visible curve over the data, not a flat line
})

test_that("A failed log-logistic fit is shown on the histogram instead of stopping the analysis", {
  set.seed(1)
  dataset <- data.frame(Diameter = c(0, exp(rlogis(19, location = log(10), scale = 0.1)))) # a zero: the fit fails
  options <- analysisOptions("processCapabilityStudies")
  options$measurementLongFormat <- "Diameter"
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 5
  options$controlChartType <- "xBarR"
  options$histogram <- TRUE
  options$histogramDensityLine <- TRUE
  options$probabilityPlot <- FALSE
  options$nullDistribution <- "loglogistic"
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", dataset, options, view = FALSE)

  expect_identical(results[["status"]], "complete")
  expect_match(results[["results"]][["histogram"]][["error"]][["errorMessage"]], "could not be estimated")
})
