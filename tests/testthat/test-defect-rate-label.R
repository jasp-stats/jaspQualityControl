test_that('u charts label defect rates while p charts retain proportions', {
  for (variant in c('uChart', 'laneyUPrimeChart', 'pChart', 'laneyPPrimeChart')) {
    for (variable in c(FALSE, TRUE)) {
      data <- data.frame(defects = c(1, 3, 2, 6, 1, 8, 0, 2),
                         exposure = if (variable) c(10, 12, 18, 14, 20, 16, 11, 15) else rep(10, 8))
      options <- jaspTools::analysisOptions('attributesCharts')
      options$defectiveOrDefect <- 'defects'
      options$total <- 'exposure'
      rate <- variant %in% c('uChart', 'laneyUPrimeChart')
      options$attributesChart <- if (rate) 'defects' else 'defectives'
      if (rate)
        options$attributesChartDefectsChartType <- variant
      else
        options$attributesChartDefectivesChartType <- variant
      results <- jaspTools::runAnalysis('attributesCharts', data, options, view = FALSE)
      key <- switch(variant, uChart = 'UchartPlot', laneyUPrimeChart = 'LaneyUPlot',
                    pChart = 'PchartPlot', laneyPPrimeChart = 'LaneyPPlot')
      expect_identical(results$status, 'complete')
      plot <- results$state$figures[[results$results[[key]]$data]]$obj
      expect_identical(plot$scales$get_scales('y')$name, if (rate) 'Defects per unit' else 'Proportion')
      points <- which(vapply(plot$layers, function(layer) inherits(layer$geom, 'GeomPoint'), logical(1)))
      expect_length(points, 1)
      expect_equal(ggplot2::ggplot_build(plot)$data[[points]]$y, data$defects / data$exposure)
    }
  }
})
