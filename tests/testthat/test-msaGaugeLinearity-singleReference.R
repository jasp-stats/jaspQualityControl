context("[Quality Control] MSA - Gauge Linearity - Single reference value")

# With one reference value (one part) the slope of the bias line cannot be estimated: lm() returns an aliased slope and
# summary() drops its row, so the Regression model table showed the intercept's standard error as the slope, with
# shifted columns, a wrong equation and %Linearity (jasp-stats/INTERNAL-jasp#3439).

options <- analysisOptions("msaGaugeLinearity")
options$part <- "Part"
options$measurement <- "Measurement"
options$standard <- "Reference"
options$linearityTable <- TRUE
options$biasTable <- TRUE
options$linearityAndBiasPlot <- TRUE
options$manualProcessVariation <- TRUE
options$manualProcessVariationValue <- 1
options$percentProcessVariationPlot <- TRUE

linearityData <- read.csv(testthat::test_path("datasets", "msaLinearityStudy", "msaLinearity.csv"))
linearityData <- linearityData[c("Part", "Reference", "Measurement")]
linearityData[["Part"]] <- factor(linearityData[["Part"]])

.linPlotObject <- function(results, name) {
  plotName <- results[["results"]][["LB"]][["collection"]][[paste0("LB_", name)]][["data"]]
  results[["state"]][["figures"]][[plotName]][["obj"]]
}

onePart <- droplevels(linearityData[linearityData[["Part"]] == "1", ])
set.seed(1)
results <- runAnalysis("msaGaugeLinearity", onePart, options, view = FALSE)
output  <- results[["results"]][["LB"]][["collection"]]

test_that("Single reference value: regression table shows an error instead of a slope", {
  expect_identical(results[["status"]], "complete")
  regression <- output[["LB_tableRegression"]]
  expect_identical(regression[["status"]], "error")
  expect_identical(regression[["error"]][["errorMessage"]], "Linearity requires at least 2 different reference values.")
  expect_length(regression[["data"]], 0)
  # no regression equation and no S, R-squared, linearity or %Linearity
  expect_null(output[["LB_tableEquation"]])
  expect_null(output[["LB_tableGaugeLinearity"]])
})

test_that("Single reference value: gauge bias table is still shown", {
  bias <- onePart[["Measurement"]] - onePart[["Reference"]]
  rows <- output[["LB_tableGaugeBias"]][["data"]]
  expect_length(rows, 2)
  expect_identical(rows[[1]][["part"]], "1")
  expect_equal(rows[[1]][["referenceValue"]], 2)
  expect_equal(rows[[1]][["observedMean"]], mean(onePart[["Measurement"]]))
  expect_equal(rows[[1]][["bias"]], mean(bias))
  expect_equal(rows[[1]][["pvalue"]], t.test(bias)$p.value, tolerance = 1e-6)
  expect_equal(rows[[1]][["percentBias"]], abs(mean(bias)) * 100)
  expect_identical(rows[[2]][["part"]], "Total")
  expect_equal(rows[[2]][["bias"]], mean(bias))
  expect_equal(rows[[2]][["percentBias"]], abs(mean(bias)) * 100)
  expect_equal(mean(bias), 0.491666666666667)
})

test_that("Single reference value: graphs show the bias but no regression line or linearity", {
  bias <- onePart[["Measurement"]] - onePart[["Reference"]]

  biasPlot <- .linPlotObject(results, "plotBias")
  geoms    <- vapply(biasPlot$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomSmooth" %in% geoms)
  pointData <- ggplot2::ggplot_build(biasPlot)$data[geoms == "GeomPoint"]
  expect_equal(sort(pointData[[1]][["y"]]), sort(bias)) # measurements
  expect_equal(pointData[[2]][["y"]], mean(bias))       # mean bias

  processVarPlot <- .linPlotObject(results, "plotProcessVar")
  expect_identical(as.character(processVarPlot$layers[[1]]$data[["Source"]]), "Bias")
  expect_equal(ggplot2::ggplot_build(processVarPlot)$data[[1]][["y"]], abs(mean(bias)) * 100)
})

test_that("Two reference values: regression table matches lm()", {
  twoParts <- droplevels(linearityData[linearityData[["Part"]] %in% c("1", "2"), ])
  set.seed(1)
  results <- runAnalysis("msaGaugeLinearity", twoParts, options, view = FALSE)
  output  <- results[["results"]][["LB"]][["collection"]]

  bias     <- twoParts[["Measurement"]] - twoParts[["Reference"]]
  expected <- coef(summary(lm(bias ~ twoParts[["Reference"]])))
  rows     <- output[["LB_tableRegression"]][["data"]]
  expect_length(rows, 2)
  for (i in 1:2) {
    expect_equal(rows[[i]][["coefficient"]],   expected[i, 1], tolerance = 1e-8)
    expect_equal(rows[[i]][["SEcoefficient"]], expected[i, 2], tolerance = 1e-8)
    expect_equal(rows[[i]][["Tvalues"]],       expected[i, 3], tolerance = 1e-8)
    expect_equal(rows[[i]][["pvalue"]],        expected[i, 4], tolerance = 1e-6)
  }
  expect_equal(output[["LB_tableGaugeLinearity"]][["data"]][[1]][["percentLin"]], abs(expected[2, 1]) * 100, tolerance = 1e-8)
})
