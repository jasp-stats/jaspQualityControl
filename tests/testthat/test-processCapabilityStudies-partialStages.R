context("[Quality Control] Process capability with partially populated stages")

for (emptyStage in c(FALSE, TRUE)) {
  test_that(paste("Stage validity uses individual measurements; empty stage:", emptyStage), {
    dataset <- data.frame(
      measurement = 10 + sin(seq_len(40)) + seq_len(40) / 50,
      stage = rep(c("A", "B"), each = 20)
    )
    # Every subgroup retains four measurements, but none is a complete row after reshaping.
    dataset$stage[seq(5, 40, 5)] <- NA
    if (emptyStage)
      dataset$measurement[21:40] <- NA_real_

    options <- analysisOptions("processCapabilityStudies")
    options$measurementLongFormat <- "measurement"
    options$stagesLongFormat <- "stage"
    options$subgroupSizeType <- "manual"
    options$manualSubgroupSizeValue <- 5
    options$lowerSpecificationLimit <- TRUE
    options$lowerSpecificationLimitValue <- 7
    options$upperSpecificationLimit <- TRUE
    options$upperSpecificationLimitValue <- 14
    options$controlChart <- FALSE
    options$processCapabilityTable <- FALSE
    options$processCapabilityPlot <- TRUE
    options$histogram <- TRUE
    options$probabilityPlot <- TRUE
    results <- runAnalysis("processCapabilityStudies", dataset, options)

    capability <- results$results$capabilityAnalysis$collection$capabilityAnalysis_normalCapabilityAnalysis$collection
    probability <- results$results$probabilityContainer$collection
    plots <- list(
      capability$capabilityAnalysis_normalCapabilityAnalysis_capabilityPlot,
      results$results$histogram,
      probability[[if (emptyStage) "probabilityContainer_plot" else "probabilityContainer_ProbabilityPlot"]]
    )
    for (plot in plots) {
      expect_false(is.null(plot))
      if (emptyStage) {
        expect_match(plot[["error"]][["errorMessage"]], "At least one of the stages contains no valid measurements", fixed = TRUE)
      } else {
        expect_null(plot[["error"]])
        expect_true(nzchar(plot$data))
        expect_false(is.null(results$state$figures[[plot$data]]$obj))
      }
    }
  })
}
