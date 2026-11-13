context("[Quality Control] MSA - Gauge Linearity - Total bias p-value")

# The Total row of the Gauge bias table tests the average bias against the repeatability
# (pooled within-part SD, df = N - number of parts), not the residual SD of the bias regression,
# which also contains lack of fit such as curvature (jasp-stats/INTERNAL-jasp#3429).

.linTotalRow <- function(results) {
  rows <- results[["results"]][["LB"]][["collection"]][["LB_tableGaugeBias"]][["data"]]
  rows[[length(rows)]]
}

.linPooledP <- function(dataset) {
  bias <- dataset[["Measurement"]] - dataset[["Reference"]]
  fit  <- lm(bias ~ Part, dataset, contrasts = list(Part = "contr.sum"))
  # balanced design: sum-contrast intercept = mean bias, SE = sigma / sqrt(N), df = N - k
  summary(fit)$coefficients["(Intercept)", "Pr(>|t|)"]
}

options <- analysisOptions("msaGaugeLinearity")
options$part <- "Part"
options$measurement <- "Measurement"
options$standard <- "Reference"
options$linearityTable <- TRUE
options$biasTable <- TRUE
options$linearityAndBiasPlot <- FALSE
options$manualProcessVariation <- FALSE
options$percentProcessVariationPlot <- FALSE

test_that("Curved bias: Total p-value uses pooled within-part SD", {
  # 5 references x 12 measurements, within-part SD exactly 0.05, average bias -0.04, slope 0
  refs <- c(2, 4, 6, 8, 10)
  bias <- 0.05 * (refs - 6)^2 - 0.44
  eps  <- as.numeric(scale(seq(-1, 1, length.out = 12))) * 0.05
  dataset <- data.frame(Part        = factor(rep(1:5, each = 12)),
                        Reference   = rep(refs, each = 12),
                        Measurement = rep(refs + bias, each = 12) + rep(eps, 5))
  set.seed(1)
  results <- runAnalysis("msaGaugeLinearity", dataset, options)
  total <- .linTotalRow(results)

  expectedP <- 2 * pt(-abs(-0.04 / (0.05 / sqrt(60))), df = 55)
  expect_equal(total[["bias"]], -0.04, tolerance = 1e-10)
  expect_equal(total[["pvalue"]], expectedP, tolerance = 1e-6)
  expect_equal(total[["pvalue"]], .linPooledP(dataset), tolerance = 1e-6)
  expect_lt(total[["pvalue"]], 1e-6) # residual SD of the bias line gave p = 0.37
})

test_that("Minitab example: Total p-value uses pooled within-part SD", {
  dataset <- read.csv(testthat::test_path("datasets", "msaLinearityStudy", "msaLinearity.csv"))
  dataset[["Part"]] <- factor(dataset[["Part"]])
  set.seed(1)
  results <- runAnalysis("msaGaugeLinearity", dataset[c("Part", "Reference", "Measurement")], options)
  total <- .linTotalRow(results)

  expect_equal(total[["pvalue"]], .linPooledP(dataset), tolerance = 1e-6)
  expect_equal(total[["pvalue"]], 0.0894220227213575, tolerance = 1e-6)
})
