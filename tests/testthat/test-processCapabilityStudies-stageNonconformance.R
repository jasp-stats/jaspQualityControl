context("[Quality Control] Stage-specific observed nonconformance")

# Both stages have one value below 1 and one above 3; values on the limits conform.
smallStage <- c(0.5, 1, 3, seq(1.4, 2.4, length.out = 6), 3.5)
largeStage <- c(0.5, 1, 3, seq(1.3, 2.6, length.out = 16), 3.5)
stageData <- data.frame(measurement = c(smallStage, largeStage), stage = rep(c("A", "B"), c(10, 20)))
stageCases <- list(
  forward = stageData,
  reversed = stageData[c(11:30, 1:10), ],
  # stages are periods of equal consecutive values, so the missing measurements stay inside their stage
  missing = rbind(stageData[1:10, ], data.frame(measurement = c(NA_real_, NA_real_), stage = "A"),
                  stageData[11:30, ], data.frame(measurement = NA_real_, stage = "B")),
  single = stageData[stageData$stage == "A", ],
  equal = data.frame(measurement = rep(smallStage, 2), stage = rep(c("A", "B"), each = 10))
)

for (capabilityType in c("normalCapabilityAnalysis", "nonNormalCapabilityAnalysis")) {
  for (caseName in names(stageCases)) {
    test_that(paste(capabilityType, "uses each stage's valid measurement count:", caseName), {
      options <- jaspTools::analysisOptions("processCapabilityStudies")
      options[["testSet"]] <- "jaspDefault"
      options[["dataFormat"]] <- "wideFormat"
      options[["measurementsWideFormat"]] <- "measurement"
      options[["stagesWideFormat"]] <- if (caseName == "single") "" else "stage"
      options[["capabilityStudyType"]] <- capabilityType
      options[["nonNormalDistribution"]] <- "weibull"
      options[["lowerSpecificationLimit"]] <- TRUE
      options[["upperSpecificationLimit"]] <- TRUE
      options[["lowerSpecificationLimitValue"]] <- 1
      options[["upperSpecificationLimitValue"]] <- 3
      options[["historicalStdDev"]] <- TRUE
      options[["historicalStdDevValue"]] <- 1
      options[["processCapabilityTableCi"]] <- FALSE
      options[["processCapabilityTable"]] <- TRUE
      options[["processCapabilityPlot"]] <- FALSE
      options[["controlChart"]] <- FALSE
      options[["histogram"]] <- FALSE
      options[["probabilityPlot"]] <- FALSE

      set.seed(1)
      results <- jaspTools::runAnalysis("processCapabilityStudies", stageCases[[caseName]], options, view = FALSE)
      expect_identical(results[["status"]], "complete")
      containerName <- paste0("capabilityAnalysis_", capabilityType)
      tableName <- if (capabilityType == "normalCapabilityAnalysis") {
        "capabilityTablePerformance"
      } else {
        "PerformanceNonNormal"
      }
      capability <- results[["results"]][["capabilityAnalysis"]][["collection"]][[containerName]][["collection"]]
      table <- capability[[paste0(containerName, "_", tableName)]][["data"]]
      expect_length(table, 3)

      if (caseName == "single") {
        expected <- list(observed1 = c(100000, 100000, 200000))
      } else {
        # columns are named by stage number (1 = baseline)
        expected <- list(observed1 = c(100000, 100000, 200000), observed2 = c(50000, 50000, 100000),
                         observedComparison2 = c(-50000, -50000, -100000))
        if (caseName == "equal")
          expected <- list(observed1 = expected[["observed1"]], observed2 = expected[["observed1"]], observedComparison2 = c(0, 0, 0))
        if (caseName == "reversed")
          expected <- list(observed1 = c(50000, 50000, 100000), observed2 = c(100000, 100000, 200000),
                           observedComparison2 = c(50000, 50000, 100000))
      }
      for (column in names(expected))
        expect_equal(vapply(table, function(row) as.numeric(row[[column]]), numeric(1)), expected[[column]])
    })
  }
}
