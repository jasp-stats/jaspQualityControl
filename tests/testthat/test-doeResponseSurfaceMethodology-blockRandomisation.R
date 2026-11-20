context("[Quality Control] DoE Response surface methodology - run order within blocks")

# With a random run order, blocked designs (Box-Behnken with 4 or 5 continuous factors) are randomised within each
# block and the blocks follow each other, as a block is a batch or time period. Unblocked designs keep their run order.

rsmBlockRun <- function(k, designType = "boxBehnkenDesign", runOrder = "runOrderRandom", seed = 1, replicates = 1,
                        categorical = FALSE, selectedRow = 0) {
  opts <- analysisOptions("doeResponseSurfaceMethodology")
  opts$actualExporter <- FALSE
  opts$alphaType <- "default"
  opts$centerPointType <- "default"
  opts$codedOutput <- TRUE
  opts$setSeed <- TRUE
  opts$seed <- seed
  opts$designType <- designType
  opts$displayDesign <- TRUE
  opts$exportDesignFile <- ""
  opts$numberOfContinuous <- k
  opts$runOrder <- runOrder
  rows <- paste("Row", seq_len(k) - 1)
  opts$continuousVariables <- list(
    list(levels = rows, name = "data 1", values = LETTERS[seq_len(k)]),
    list(levels = rows, name = "data 2", values = rep(-1, k)),
    list(levels = rows, name = "data 3", values = rep(1, k))
  )
  opts$numberOfCategorical <- as.integer(categorical)
  if (categorical) {
    opts$categoricalVariables <- list(
      list(levels = "Row 0", name = "data 1", values = "G"),
      list(levels = "Row 0", name = "data 2", values = "a"),
      list(levels = "Row 0", name = "data 3", values = "b")
    )
  }
  # the design itself comes from selectedRow (default designs); these are only the displayed design list values
  opts$selectedDesign2 <- list(
    list(levels = "Row 0", name = "data 1", values = 0),
    list(levels = "Row 0", name = "data 2", values = 0),
    list(levels = "Row 0", name = "data 3", values = 0)
  )
  opts$selectedRow <- selectedRow
  opts$replicates <- replicates
  res <- runAnalysis("doeResponseSurfaceMethodology", "debug.csv", opts)
  do.call(rbind, lapply(res[["results"]][["designTable"]][["data"]], as.data.frame))
}

blockAlongRunOrder <- function(design) as.integer(as.character(design[["Block"]][order(design[["run.order"]])]))

expectRandomisedWithinBlocks <- function(designRandom, designStandard, nBlocks) {
  n <- nrow(designRandom)
  # a permutation of all runs, displayed in run order
  expect_identical(sort(as.integer(designRandom[["run.order"]])), seq_len(n))
  expect_identical(as.integer(designRandom[["run.order"]]), seq_len(n))
  # the blocks follow each other: block 1 first, ..., exactly nBlocks - 1 switches along the run order
  blocks <- blockAlongRunOrder(designRandom)
  expect_false(is.unsorted(blocks))
  expect_equal(sum(diff(blocks) != 0), nBlocks - 1)
  expect_equal(length(unique(blocks)), nBlocks)
  # the same runs (incl. their blocks and run order) as in standard order, only displayed differently
  byStd <- designRandom[order(designRandom[["std.order"]]), ]
  rownames(byStd) <- NULL
  expect_identical(as.integer(designStandard[["std.order"]]), seq_len(n))
  expect_equal(byStd, designStandard)
  # each block holds its own standard-order runs, in random order and on consecutive run numbers
  offset <- 0L
  for (b in sort(unique(blocks))) {
    inBlock <- as.integer(as.character(designRandom[["Block"]])) == b
    stdInBlock <- as.integer(designStandard[["std.order"]][as.integer(as.character(designStandard[["Block"]])) == b])
    expect_setequal(as.integer(designRandom[["std.order"]][inBlock]), stdInBlock)
    expect_identical(sort(as.integer(designRandom[["run.order"]][inBlock])), offset + seq_len(sum(inBlock)))
    expect_true(is.unsorted(designRandom[["std.order"]][inBlock])) # randomised within the block
    offset <- offset + sum(inBlock)
  }
}

# 1 Blocked Box-Behnken designs ####
for (seed in c(1, 123)) {
  for (k in c(4, 5)) {
    nBlocks        <- if (k == 4) 3 else 2
    designRandom   <- rsmBlockRun(k, seed = seed)
    designStandard <- rsmBlockRun(k, seed = seed, runOrder = "runOrderStandard")

    test_that(sprintf("1.1 Box-Behnken design with %d factors is randomised within its %d blocks (seed %d)", k, nBlocks, seed), {
      expect_equal(nrow(designRandom), if (k == 4) 33 else 46)
      # before the fix the random order was one permutation over all runs: 24 block switches for 4 factors, seed 1
      expectRandomisedWithinBlocks(designRandom, designStandard, nBlocks)
    })
  }
}

test_that("1.2 Box-Behnken design with 4 factors, seed 1: blocks of 11 runs follow each other", {
  designRandom <- rsmBlockRun(4, seed = 1)
  expect_equal(blockAlongRunOrder(designRandom), rep(1:3, each = 11))
})

# 2 Blocked Box-Behnken design with a discrete factor ####
# the discrete factor is a treatment factor: its levels share the blocks and are randomised within each block
test_that("2.1 Blocked Box-Behnken design with a discrete factor is randomised within blocks over both levels", {
  designRandom   <- rsmBlockRun(4, categorical = TRUE)
  designStandard <- rsmBlockRun(4, categorical = TRUE, runOrder = "runOrderStandard")
  expect_equal(nrow(designRandom), 66)
  expectRandomisedWithinBlocks(designRandom, designStandard, 3)
  # every block has the 11 runs of each discrete level, and the levels are mixed within each block
  expect_equal(as.vector(table(designRandom[["Block"]], designRandom[["x_cat1"]])), rep(11, 6))
  levels <- as.integer(factor(designRandom[["x_cat1"]][order(designRandom[["run.order"]])], levels = c("a", "b")))
  for (b in 1:3) {
    levelsInBlock <- levels[blockAlongRunOrder(designRandom) == b]
    expect_gt(sum(diff(levelsInBlock) != 0), 1)
  }
})

# 3 Blocked Box-Behnken design with replicates ####
# every replicate has its own blocks, which are randomised one after another
test_that("3.1 Blocked Box-Behnken design with 2 replicates is randomised within its 6 blocks", {
  designRandom   <- rsmBlockRun(4, replicates = 2)
  designStandard <- rsmBlockRun(4, replicates = 2, runOrder = "runOrderStandard")
  expect_equal(nrow(designRandom), 66)
  expectRandomisedWithinBlocks(designRandom, designStandard, 6)
  expect_false(is.unsorted(designRandom[["Replication"]][order(designRandom[["run.order"]])]))
})

test_that("3.2 Blocked Box-Behnken design with 2 replicates and a discrete factor is randomised within its 4 blocks", {
  designRandom   <- rsmBlockRun(5, replicates = 2, categorical = TRUE)
  designStandard <- rsmBlockRun(5, replicates = 2, categorical = TRUE, runOrder = "runOrderStandard")
  expect_equal(nrow(designRandom), 184)
  expectRandomisedWithinBlocks(designRandom, designStandard, 4)
})

# 4 Unblocked designs keep their run order ####
# run.order (shown in standard order) as before the fix, i.e., set.seed(seed); sample(n)
test_that("4.1 Unblocked designs keep their random run order", {
  bbd3 <- rsmBlockRun(3, runOrder = "runOrderStandard")
  expect_false("Block" %in% names(bbd3))
  expect_equal(as.integer(bbd3[["run.order"]]), c(9L, 4L, 7L, 1L, 2L, 13L, 11L, 3L, 8L, 12L, 5L, 6L, 15L, 10L, 14L))

  bbd3cat <- rsmBlockRun(3, runOrder = "runOrderStandard", categorical = TRUE)
  expect_equal(as.integer(bbd3cat[["run.order"]]),
               c(25L, 4L, 7L, 1L, 2L, 23L, 11L, 14L, 18L, 19L, 27L, 10L, 30L, 21L, 28L, 9L, 5L, 22L, 15L, 12L, 13L,
                 17L, 26L, 8L, 6L, 20L, 29L, 3L, 24L, 16L))

  ccd3 <- rsmBlockRun(3, designType = "centralCompositeDesign", runOrder = "runOrderStandard")
  expect_false("Block" %in% names(ccd3))
  expect_equal(as.integer(ccd3[["run.order"]]),
               c(4L, 7L, 1L, 2L, 13L, 19L, 11L, 17L, 14L, 3L, 18L, 5L, 9L, 16L, 6L, 15L, 12L, 10L, 20L, 8L))

  ccd4cat <- rsmBlockRun(4, designType = "centralCompositeDesign", runOrder = "runOrderStandard", categorical = TRUE)
  expect_equal(as.integer(ccd4cat[["run.order"]]),
               c(57L, 4L, 39L, 1L, 34L, 23L, 43L, 14L, 18L, 51L, 33L, 21L, 53L, 42L, 46L, 10L, 7L, 9L, 15L, 50L, 37L,
                 41L, 25L, 49L, 60L, 58L, 40L, 44L, 52L, 20L, 3L, 6L, 47L, 30L, 31L, 35L, 38L, 26L, 12L, 62L, 28L,
                 8L, 24L, 22L, 48L, 29L, 19L, 56L, 55L, 2L, 13L, 16L, 11L, 59L, 32L, 27L, 17L, 61L, 36L, 54L, 45L, 5L))
})
