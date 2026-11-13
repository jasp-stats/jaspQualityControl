context("[Quality Control] DoE Factorial Design - run order seed persistence")

# Without "Set seed", the run order is randomized with the seed in the hidden option `randomSeed`, which the
# form draws once when the analysis is created and which is saved in the .jasp file. jaspTools rebuilds the
# form on every runAnalysis(), so a fresh analysis draws a new randomSeed per run; a reopened file is mimicked
# by passing the stored randomSeed explicitly. Each runAnalysis() also starts with an empty jaspResults, as
# after reopening a file saved without the analysis state.

.doeSeedOptions <- function() {
  options <- analysisOptions("doeFactorial")
  options$actualExporter <- FALSE
  options$numberOfCategorical <- 5
  options$categoricalVariables <- list(
    list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 1", values = c("A", "B", "C", "D", "E")),
    list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 2", values = c("a", "a", "a", "a", "a")),
    list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 3", values = c("b", "b", "b", "b", "b")))
  options$codedOutput <- TRUE
  options$displayDesign <- TRUE
  options$exportDesignFile <- ""
  options$factorialType <- "factorialTypeDefault"
  options$runOrder <- "runOrderRandom"
  options$selectedDesign2 <- list(list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 1", values = c(8, 16, 32)),
                                  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 2", values = c(0, 0, 0)))
  options$selectedRow <- 1 # 1/2 fraction, 16 runs, resolution V
  options$setSeed <- FALSE
  options
}

.doeSeedStdOrder <- function(results) {
  vapply(results[["results"]][["displayDesign"]][["data"]], function(row) row[["sro"]], numeric(1))
}

.doeSeedFootnotes <- function(results) {
  vapply(results[["results"]][["displayDesign"]][["footnotes"]], function(x) x[["text"]], character(1))
}

test_that("Without Set seed, the stored seed gives the same run order in every run", {
  options <- .doeSeedOptions()
  options$randomSeed <- 123456
  set.seed(1)
  results1 <- runAnalysis("doeFactorial", "debug.csv", options)
  set.seed(99)
  results2 <- runAnalysis("doeFactorial", "debug.csv", options)
  options$codedOutput <- FALSE
  set.seed(7)
  results3 <- runAnalysis("doeFactorial", "debug.csv", options)

  expect_equal(.doeSeedStdOrder(results1), c(12, 10, 15, 1, 7, 6, 4, 11, 2, 3, 16, 5, 8, 9, 13, 14))
  expect_equal(.doeSeedStdOrder(results2), .doeSeedStdOrder(results1))
  expect_equal(.doeSeedStdOrder(results3), .doeSeedStdOrder(results1))
  expect_true("Run order randomized with seed 123456." %in% .doeSeedFootnotes(results1))
})

test_that("Without Set seed, a new analysis shows its drawn seed, which reproduces the run order", {
  options <- .doeSeedOptions()
  set.seed(1)
  results1 <- runAnalysis("doeFactorial", "debug.csv", options)
  footnote <- grep("^Run order randomized with seed ", .doeSeedFootnotes(results1), value = TRUE)
  expect_length(footnote, 1)
  seed <- as.numeric(sub("^Run order randomized with seed (\\d+)\\.$", "\\1", footnote))
  expect_true(seed >= 1 && seed <= 999999)

  options$randomSeed <- seed # as stored in the saved file
  set.seed(99)
  results2 <- runAnalysis("doeFactorial", "debug.csv", options)
  expect_equal(.doeSeedStdOrder(results2), .doeSeedStdOrder(results1))
})

test_that("With Set seed, the set seed is used and shown", {
  options <- .doeSeedOptions()
  options$setSeed <- TRUE
  options$seed <- 123456
  options$randomSeed <- 1
  results1 <- runAnalysis("doeFactorial", "debug.csv", options)
  options$randomSeed <- 2
  results2 <- runAnalysis("doeFactorial", "debug.csv", options)

  expect_equal(.doeSeedStdOrder(results1), c(12, 10, 15, 1, 7, 6, 4, 11, 2, 3, 16, 5, 8, 9, 13, 14))
  expect_equal(.doeSeedStdOrder(results2), .doeSeedStdOrder(results1))
  expect_true("Run order randomized with seed 123456." %in% .doeSeedFootnotes(results1))
})
