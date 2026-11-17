context("DoE Factorial Design: standard order of blocked designs with centre points")

# Blocked designs label their runs "<run>.<block>.<run in block>" and their centre points "0.<block>.0" (FrF2),
# the Standard Order numbers every run once: the factorial runs keep their number, the centre points follow them, block by block
# (jasp-stats/jasp-issues#4696)

blockedStandardOrderOptions <- function(blocks = "2", centerpoints = 2, replications = 1, runOrder = "runOrderStandard") {
  options <- analysisOptions("doeFactorial")
  options$actualExporter <- FALSE
  options$numberOfCategorical <- 3
  options$categoricalVariables <- list(
    list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 1", values = c("A", "B", "C")),
    list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 2", values = c("a", "a", "a")),
    list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 3", values = c("b", "b", "b")))
  options$codedOutput <- TRUE
  options$displayDesign <- TRUE
  options$exportDesignFile <- ""
  options$factorialType <- "factorialTypeDefault"
  options$runOrder <- runOrder
  options$selectedDesign2 <- list(list(levels = c("Row 0", "Row 1"), name = "data 1", values = c(4, 8)),
                                  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(0, 0)))
  options$selectedRow <- 1 # full factorial, 8 runs
  options$blocks <- blocks
  options$centerpoints <- centerpoints
  options$replications <- replications
  options$setSeed <- TRUE
  return(options)
}

blockedStandardOrderDesign <- function(options) {
  set.seed(1)
  results <- runAnalysis("doeFactorial", "debug.csv", options)
  design <- do.call(rbind, lapply(results[["results"]][["displayDesign"]][["data"]], as.data.frame))
  design[c("ro", "sro", "A", "B", "C", "Block")]
}

standardDesign   <- blockedStandardOrderDesign(blockedStandardOrderOptions())
randomDesign     <- blockedStandardOrderDesign(blockedStandardOrderOptions(runOrder = "runOrderRandom"))
noCenterDesign   <- blockedStandardOrderDesign(blockedStandardOrderOptions(centerpoints = 0))

test_that("Blocked design with centre points: Standard Order numbers every run once", {
  # base: 1, 1, 2, 2 for the four centre points and 3 to 10 for the factorial runs
  expect_equal(standardDesign[["sro"]], 1:12)
  expect_equal(sort(standardDesign[["ro"]]), 1:12)
  centerPoints <- standardDesign[standardDesign[["A"]] == 0, ]
  expect_equal(centerPoints[["sro"]], 9:12)
  expect_equal(centerPoints[["Block"]], c(1, 1, 2, 2))
  expect_true(all(centerPoints[c("B", "C")] == 0))
})

test_that("Blocked design with centre points: the factorial runs keep the Standard Order of the design without centre points", {
  expect_equal(noCenterDesign[["sro"]], 1:8)
  factorialRuns <- standardDesign[standardDesign[["A"]] != 0, ]
  rownames(factorialRuns) <- NULL
  expect_equal(factorialRuns[c("sro", "A", "B", "C", "Block")], noCenterDesign[c("sro", "A", "B", "C", "Block")])
})

test_that("Blocked design with centre points in run order: run order and standard order identify the same runs", {
  expect_equal(randomDesign[["ro"]], 1:12)
  expect_equal(sort(randomDesign[["sro"]]), 1:12)
  inStandardOrder <- randomDesign[order(randomDesign[["sro"]]), ]
  rownames(inStandardOrder) <- NULL
  expect_equal(inStandardOrder, standardDesign)
  # the factorial runs are numbered as in the FrF2 labels "<run>.<block>.<run in block>"
  reference <- FrF2::FrF2(nfactors = 3, nruns = 8, ncenter = 2, blocks = 2, alias.block.2fis = TRUE,
                          seed = blockedStandardOrderOptions()$seed)
  labels <- as.character(DoE.base::run.order(reference)[["run.no.in.std.order"]])
  run <- as.numeric(sub("\\..*$", "", labels))
  expect_equal(randomDesign[["sro"]][run > 0], run[run > 0])
  expect_equal(randomDesign[["Block"]], as.numeric(sub("^[0-9]+\\.([0-9]+)\\..*$", "\\1", labels)))
})

test_that("Replicated blocked design with centre points: Standard Order numbers every run of a replicate once", {
  options <- blockedStandardOrderOptions(replications = 2)
  set.seed(1)
  results <- runAnalysis("doeFactorial", "debug.csv", options)
  design <- do.call(rbind, lapply(results[["results"]][["displayDesign"]][["data"]], as.data.frame))
  expect_equal(nrow(design), 24)
  for (r in 1:2)
    expect_equal(sort(design[["sro"]][design[["Replication"]] == r]), 1:12)
  expect_true(all(design[["sro"]][design[["A"]] == 0] > 8))
})
