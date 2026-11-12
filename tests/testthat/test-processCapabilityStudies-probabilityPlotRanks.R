context("[Quality Control] Probability plot rank methods")

# y as given and as drawn: points outside the axis limits become NA when the plot is built
pointLayerY <- function(plot) {
  k <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))
  expect_length(k, 1)
  list(y = plot$layers[[k]]$data$y, drawnY = ggplot2::layer_data(plot, k)$y)
}

probabilityPlot <- function(x, distribution, rankMethod) {
  options <- jaspTools::analysisOptions("processCapabilityStudies")
  options[["nullDistribution"]]          <- distribution
  options[["probabilityPlotRankMethod"]] <- rankMethod
  jaspQualityControl:::.qcProbabilityPlotObject(options, data.frame(x = x), "x", "")[[1]]
}

test_that("Kaplan-Meier rank puts the largest observation at 90% of the distance between the prior p and 1", {
  expect_equal(jaspQualityControl:::.qcPpKm(c(5, 1, 3, 2, 4)), c(0.2, 0.4, 0.6, 0.8, 0.98)) # (n - 0.1) / n
  expect_equal(jaspQualityControl:::.qcPpKm(c(1, 2, 2, 3)), c(0.25, 0.625, 0.625, 0.9625))  # prior p is an average rank
  expect_equal(jaspQualityControl:::.qcPpKm(c(1, 2, 3, 3)), c(0.25, 0.5, 0.875, 0.875))     # tied maximum stays below 1
  expect_equal(jaspQualityControl:::.qcPpKm(c(4, 4, 4)), c(2, 2, 2) / 3)                    # all equal: unchanged
  expect_equal(jaspQualityControl:::.qcPpKm(c(2, 1)), c(0.5, 0.95))
  expect_equal(jaspQualityControl:::.qcPpKm(7), 0.9)                                        # no prior p: 0
})

for (distribution in c("normal", "lognormal", "weibull")) {
  test_that(paste("Kaplan-Meier probability plot draws every observation, the largest at a finite position:", distribution), {
    points <- pointLayerY(probabilityPlot(qnorm(ppoints(125), mean = 100, sd = 5), distribution, "kaplanMeier"))
    pMax   <- (125 - 0.1) / 125 # beyond the 99.9% tick
    expect_equal(max(points$y), if (distribution == "weibull") log(-log(1 - pMax)) else qnorm(pMax))
    expect_equal(points$drawnY, points$y)
  })
}

test_that("Probability plot of a large sample draws the points beyond the 0.1% and 99.9% ticks", {
  points <- pointLayerY(probabilityPlot(qnorm(ppoints(1200), mean = 100, sd = 5), "normal", "bernard"))
  expect_lt(min(points$y), qnorm(0.001))
  expect_gt(max(points$y), qnorm(0.999))
  expect_equal(points$drawnY, points$y)
})
