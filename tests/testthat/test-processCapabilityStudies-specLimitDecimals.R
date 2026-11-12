context("[Quality Control] Process Capability Study - non-normal indices use the exact specification limits")

# Specification limits with more decimals than shown (default 3): the summary table shows them rounded,
# but Pp/PpL/PpU/Ppk and the observed/expected ppm must use the limits as entered.
lsl <- 0.0144
usl <- 0.0274

runNonNormal <- function(method) {
  set.seed(1)
  # two observations between the rounded (0.014, 0.027) and the exact limits
  dataset <- data.frame(Diameter = c(exp(rnorm(30, log(0.02), 0.2)), 0.0142, 0.0272))
  options <- analysisOptions("processCapabilityStudies")
  options$measurementLongFormat <- "Diameter"
  options$subgroupSizeType <- "manual"
  options$manualSubgroupSizeValue <- 4
  options$capabilityStudyType <- "nonNormalCapabilityAnalysis"
  options$nonNormalDistribution <- "lognormal"
  options$nonNormalMethod <- method
  options$controlChartType <- "xBarR"
  options$lowerSpecificationLimit <- TRUE
  options$lowerSpecificationLimitValue <- lsl
  options$upperSpecificationLimit <- TRUE
  options$upperSpecificationLimitValue <- usl
  set.seed(1)
  results <- runAnalysis("processCapabilityStudies", dataset, options, view = FALSE)
  tables <- results[["results"]][["capabilityAnalysis"]][["collection"]][["capabilityAnalysis_nonNormalCapabilityAnalysis"]][["collection"]]
  list(data = dataset[["Diameter"]],
       summary = tables[["capabilityAnalysis_nonNormalCapabilityAnalysis_summaryTableNonNormal"]][["data"]][[1]],
       capability = tables[["capabilityAnalysis_nonNormalCapabilityAnalysis_overallCapabilityNonNormal"]][["data"]][[1]],
       ppm = tables[["capabilityAnalysis_nonNormalCapabilityAnalysis_PerformanceNonNormal"]][["data"]])
}

test_that("Percentile method: indices and ppm use the exact specification limits", {
  res <- runNonNormal("percentile")
  meanlog <- res$summary[["beta"]]
  sdlog <- res$summary[["theta"]]
  # display of the limits unchanged
  expect_equal(c(res$summary[["lsl"]], res$summary[["usl"]]), c(0.014, 0.027))

  q <- function(p) qlnorm(p, meanlog, sdlog)
  ppl <- (q(0.5) - lsl) / (q(0.5) - q(0.00135))
  ppu <- (usl - q(0.5)) / (q(0.99865) - q(0.5))
  expect_equal(c(res$capability[["pp"]], res$capability[["ppl"]], res$capability[["ppu"]], res$capability[["ppk"]]),
               round(c((usl - lsl) / (q(0.99865) - q(0.00135)), ppl, ppu, min(ppl, ppu)), 3))

  expect_equal(as.numeric(res$ppm[[1]][["observed1"]]), round(1e6 * mean(res$data < lsl), 3))
  expect_equal(as.numeric(res$ppm[[2]][["observed1"]]), round(1e6 * mean(res$data > usl), 3))
  expect_equal(as.numeric(res$ppm[[1]][["expOverall1"]]), round(1e6 * plnorm(lsl, meanlog, sdlog), 3))
  expect_equal(as.numeric(res$ppm[[2]][["expOverall1"]]), round(1e6 * plnorm(usl, meanlog, sdlog, lower.tail = FALSE), 3))
})

test_that("Non-conformance method: indices use the exact specification limits", {
  res <- runNonNormal("nonConformance")
  meanlog <- res$summary[["beta"]]
  sdlog <- res$summary[["theta"]]
  zLSL <- qnorm(plnorm(lsl, meanlog, sdlog))
  zUSL <- qnorm(plnorm(usl, meanlog, sdlog))
  expect_equal(c(res$capability[["pp"]], res$capability[["ppl"]], res$capability[["ppu"]], res$capability[["ppk"]]),
               round(c((zUSL - zLSL) / 6, -zLSL / 3, zUSL / 3, min(-zLSL / 3, zUSL / 3)), 3))
})
