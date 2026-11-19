context("[Quality Control] DoE Factorial Design - unused blocks and centre points")

# Split-plot designs have no blocks and no centre points, and designs with a specified generator have no blocks. The form
# disables these controls, but a disabled control keeps its value, so leftover values must not change the summary or the
# design (jasp-stats/jasp-issues#4695)

unusedSettingsOptions <- function(factorialType, blocks, centerpoints) {
  options <- analysisOptions("doeFactorial")
  options$actualExporter <- FALSE
  options$numberOfCategorical <- 4
  rowNames <- paste("Row", 0:3)
  options$categoricalVariables <- list(
    list(levels = rowNames, name = "data 1", values = c("A", "B", "C", "D")),
    list(levels = rowNames, name = "data 2", values = rep("a", 4)),
    list(levels = rowNames, name = "data 3", values = rep("b", 4)))
  options$codedOutput <- TRUE
  options$displayDesign <- TRUE
  options$exportDesignFile <- ""
  options$factorialType <- factorialType
  options$runOrder <- "runOrderStandard"
  options$selectedDesign2 <- list(list(levels = c("Row 0", "Row 1"), name = "data 1", values = c(8, 16)),
                                  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(0, 0)))
  options$selectedRow <- 0 # 1/2 fraction, 8 runs
  options$blocks <- blocks
  options$centerpoints <- centerpoints
  options$setSeed <- TRUE
  options
}

unusedSettingsDesign <- function(results) {
  table <- results[["results"]][["displayDesign"]]
  list(columns = vapply(table[["schema"]][["fields"]], function(field) field[["name"]], character(1)),
       data = do.call(rbind, lapply(table[["data"]], as.data.frame)))
}

unusedSettingsSummary <- function(results) {
  summary <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]][[1]]
  unlist(summary[c("baseRuns", "baseBlocks", "centerpoints", "totalRuns", "totalBlocks")])
}

options <- unusedSettingsOptions("factorialTypeSplit", blocks = "2", centerpoints = 2)
options$factorialDesignTypeSplitPlotNumberHardToChangeFactors <- 1
set.seed(1)
results <- runAnalysis("doeFactorial", "debug.csv", options)

test_that("Split-plot design ignores leftover Blocks and Centre points", {
  expect_equal(unusedSettingsSummary(results),
               c(baseRuns = 8, baseBlocks = 1, centerpoints = 0, totalRuns = 8, totalBlocks = 1))
  design <- unusedSettingsDesign(results)
  expect_false("Block" %in% design$columns)
  expect_equal(nrow(design$data), 8)
  factors <- design$data[c("A", "B", "C", "D")]
  expect_equal(sort(unique(unlist(factors))), c(-1, 1))
  expect_true(all(colSums(factors) == 0)) # balanced: 4 runs at -1 and 4 at 1 per factor
})

options <- unusedSettingsOptions("factorialTypeSpecify", blocks = "2", centerpoints = 0)
options$factorialTypeSpecifyGenerators <- "D=ABC"
set.seed(1)
results <- runAnalysis("doeFactorial", "debug.csv", options)

test_that("Design with a specified generator ignores Blocks", {
  expect_equal(unusedSettingsSummary(results),
               c(baseRuns = 8, baseBlocks = 1, centerpoints = 0, totalRuns = 8, totalBlocks = 1))
  design <- unusedSettingsDesign(results)
  expect_false("Block" %in% design$columns)
  expect_equal(nrow(design$data), 8)
  factors <- design$data[c("A", "B", "C", "D")]
  expect_equal(sort(unique(unlist(factors))), c(-1, 1))
  expect_equal(factors[["D"]], factors[["A"]] * factors[["B"]] * factors[["C"]]) # D = ABC
})

options <- unusedSettingsOptions("factorialTypeSpecify", blocks = "2", centerpoints = 2)
options$factorialTypeSpecifyGenerators <- "D=ABC"
set.seed(1)
results <- runAnalysis("doeFactorial", "debug.csv", options)

test_that("Design with a specified generator: centre points counted once, as in the design table", {
  expect_equal(unusedSettingsSummary(results),
               c(baseRuns = 8, baseBlocks = 1, centerpoints = 2, totalRuns = 10, totalBlocks = 1))
  design <- unusedSettingsDesign(results)
  expect_equal(nrow(design$data), 10)
  centre <- rowSums(design$data[c("A", "B", "C", "D")] == 0) == 4
  expect_equal(sum(centre), 2)
  expect_equal(sort(unique(unlist(design$data[!centre, c("A", "B", "C", "D")]))), c(-1, 1))
})
