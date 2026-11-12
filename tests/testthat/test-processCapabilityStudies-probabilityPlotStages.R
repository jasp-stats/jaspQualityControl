context("[Quality Control] Probability plot per stage")

# Each stage's panel must equal the panel drawn from that stage's data alone.
# Previously, every stage showed the fit line and confidence bands of the last stage.
set.seed(1)
stagedData <- data.frame(
  x     = c(rnorm(20, 10, 1), rnorm(25, 14, 3), rnorm(15, 12, 0.5)),
  stage = rep(c("A", "B", "C"), c(20, 25, 15))
)
baseOptions <- jaspTools::analysisOptions("processCapabilityStudies")

lineLayerData <- function(plot) {
  isLine <- vapply(plot$layers, function(layer) inherits(layer$geom, "GeomLine"), logical(1))
  lapply(which(isLine), function(k) ggplot2::layer_data(plot, k)[c("x", "y")])
}

for (distribution in c("normal", "lognormal", "weibull")) {
  test_that(paste("Probability plot fit line and confidence bands are estimated per stage:", distribution), {
    options <- baseOptions
    options[["nullDistribution"]] <- distribution
    stagedPlots <- jaspQualityControl:::.qcProbabilityPlotObject(options, stagedData, "x", "stage")
    expect_length(stagedPlots, 3)
    for (i in seq_along(stagedPlots)) {
      stage       <- unique(stagedData[["stage"]])[i]
      stageData   <- stagedData[stagedData[["stage"]] == stage, "x", drop = FALSE]
      singlePlot  <- jaspQualityControl:::.qcProbabilityPlotObject(options, stageData, "x", "")[[1]]
      stagedLines <- lineLayerData(stagedPlots[[i]])
      expect_length(stagedLines, 3) # fit line, lower and upper confidence band
      expect_equal(stagedLines, lineLayerData(singlePlot), info = paste("stage", stage))
    }
  })
}
