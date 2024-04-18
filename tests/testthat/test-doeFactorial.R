context("DoE Factorial Design")

# Two-level factorial designs ####

## Basic tests #####

### Two factors full factorial (verified with Minitab) ####

options <- analysisOptions("doeFactorial")
options$actualExporter <- FALSE
options$numberOfCategorical <- 2
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0", "Row 1"), name = "data 1", values = c("A", "B")),
  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c("a", "a")),
  list(levels = c("Row 0","Row 1"), name = "data 3", values = c("b", "b")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "factorialTypeDefault"
options$runOrder <- "runOrderStandard"
options$selectedDesign2 <- list(list(levels = "Row 0", name = "data 1", values = 4), # give design list, always two lists, number of values gives number of designs
                                list(levels = "Row 0", name = "data 2", values = 0))
options$selectedRow <- 0 # select design
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)


test_that("1.1 Two Factor Two Level Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, -1, 4, 1, 1, -1, 1, 2, -1, 1, 3, 3, 1, 1, 2, 4))
})

test_that("1.2 Two Factor Two Level Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 4, 2, 0, 0, 1, "Value", 1, 4))
})

### Five factors full factorial (verified with Minitab) ####

options <- analysisOptions("doeFactorial")
options$actualExporter <- FALSE
options$numberOfCategorical <- 5
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 1", values = c("A", "B", "C", "D", "E")),
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 2", values = c("a", "a", "a", "a", "a")),
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 3", values = c("b", "b", "b", "b", "b")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "factorialTypeDefault"
options$runOrder <- "runOrderStandard"
options$selectedDesign2 <- list(list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 1", values = c(8, 16, 32)), # give design list, always two lists, number of values gives number of designs
                                list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 2", values = c(0, 0, 0))
                                )
options$selectedRow <- 2 # select design
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)

test_that("2.1 Five Factor Two Level Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, -1, -1, -1, -1, 27, 1, 1, -1, -1, -1, -1, 10, 2, -1, 1, -1,
                                      -1, -1, 22, 3, 1, 1, -1, -1, -1, 12, 4, -1, -1, 1, -1, -1, 11,
                                      5, 1, -1, 1, -1, -1, 5, 6, -1, 1, 1, -1, -1, 23, 7, 1, 1, 1,
                                      -1, -1, 14, 8, -1, -1, -1, 1, -1, 1, 9, 1, -1, -1, 1, -1, 32,
                                      10, -1, 1, -1, 1, -1, 20, 11, 1, 1, -1, 1, -1, 2, 12, -1, -1,
                                      1, 1, -1, 25, 13, 1, -1, 1, 1, -1, 13, 14, -1, 1, 1, 1, -1,
                                      18, 15, 1, 1, 1, 1, -1, 9, 16, -1, -1, -1, -1, 1, 8, 17, 1,
                                      -1, -1, -1, 1, 3, 18, -1, 1, -1, -1, 1, 26, 19, 1, 1, -1, -1,
                                      1, 15, 20, -1, -1, 1, -1, 1, 29, 21, 1, -1, 1, -1, 1, 30, 22,
                                      -1, 1, 1, -1, 1, 24, 23, 1, 1, 1, -1, 1, 21, 24, -1, -1, -1,
                                      1, 1, 6, 25, 1, -1, -1, 1, 1, 28, 26, -1, 1, -1, 1, 1, 4, 27,
                                      1, 1, -1, 1, 1, 19, 28, -1, -1, 1, 1, 1, 7, 29, 1, -1, 1, 1,
                                      1, 31, 30, -1, 1, 1, 1, 1, 17, 31, 1, 1, 1, 1, 1, 16, 32))
})

test_that("2.1 Five Factor Two Level Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 32, 5, 0, 0, 1, "Value", 1, 32))
})
### Five factors fractional factorial (verified with Minitab) ####

options <- analysisOptions("doeFactorial")
options$actualExporter <- FALSE
options$numberOfCategorical <- 5
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 1", values = c("A", "B", "C", "D", "E")),
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 2", values = c("a", "a", "a", "a", "a")),
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 3", values = c("b", "b", "b", "b", "b")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "factorialTypeDefault"
options$runOrder <- "runOrderStandard"
options$selectedDesign2 <- list(list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 1", values = c(8, 16, 32)), # give design list, always two lists, number of values gives number of designs
                                list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 2", values = c(0, 0, 0))
)
options$selectedRow <- 0 # select design
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)

test_that("3.1 Five Factor Two Level Frac. Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, -1, -1, 1, 1, 5, 1, 1, -1, -1, -1, -1, 7, 2, -1, 1, -1, -1,
                                      1, 1, 3, 1, 1, -1, 1, -1, 3, 4, -1, -1, 1, 1, -1, 4, 5, 1, -1,
                                      1, -1, 1, 8, 6, -1, 1, 1, -1, -1, 6, 7, 1, 1, 1, 1, 1, 2, 8
                                 ))
})

test_that("3.2 Five Factor Two Level Frac. Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 8, 5, 0, 0, 1, "Value", 1, 8))
})


### Nine factors highest factorial ####

options <- analysisOptions("doeFactorial")
options$actualExporter <- FALSE
options$numberOfCategorical <- 9
options$categoricalVariables <- list(
  list(
    levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6", "Row 7", "Row 8"),
    name = "data 1",
    values = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
  ),
  list(
    levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6", "Row 7", "Row 8"),
    name = "data 2",
    values = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
  ),
  list(
    levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6", "Row 7", "Row 8"),
    name = "data 3",
    values = c("b", "b", "b", "b", "b", "b", "b", "b", "b")
  )
)
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "factorialTypeDefault"
options$runOrder <- "runOrderStandard"
options$selectedDesign2 <- list(list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 1", values = c(16, 32, 64, 128)), # give design list, always two lists, number of values gives number of designs
                                list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 2", values = c(0, 0, 0, 0))
)
options$selectedRow <- 3 # select design
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)

test_that("4.1 Nine Factor Two Level Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, -1, -1, -1, -1, -1, -1, -1, -1, 116, 1, 1, -1, -1, -1, -1,
                                      -1, -1, 1, 1, 27, 2, -1, 1, -1, -1, -1, -1, -1, 1, 1, 92, 3,
                                      1, 1, -1, -1, -1, -1, -1, -1, -1, 122, 4, -1, -1, 1, -1, -1,
                                      -1, -1, 1, 1, 88, 5, 1, -1, 1, -1, -1, -1, -1, -1, -1, 55, 6,
                                      -1, 1, 1, -1, -1, -1, -1, -1, -1, 114, 7, 1, 1, 1, -1, -1, -1,
                                      -1, 1, 1, 10, 8, -1, -1, -1, 1, -1, -1, -1, 1, -1, 86, 9, 1,
                                      -1, -1, 1, -1, -1, -1, -1, 1, 38, 10, -1, 1, -1, 1, -1, -1,
                                      -1, -1, 1, 108, 11, 1, 1, -1, 1, -1, -1, -1, 1, -1, 110, 12,
                                      -1, -1, 1, 1, -1, -1, -1, -1, 1, 98, 13, 1, -1, 1, 1, -1, -1,
                                      -1, 1, -1, 24, 14, -1, 1, 1, 1, -1, -1, -1, 1, -1, 84, 15, 1,
                                      1, 1, 1, -1, -1, -1, -1, 1, 105, 16, -1, -1, -1, -1, 1, -1,
                                      -1, 1, -1, 66, 17, 1, -1, -1, -1, 1, -1, -1, -1, 1, 34, 18,
                                      -1, 1, -1, -1, 1, -1, -1, -1, 1, 54, 19, 1, 1, -1, -1, 1, -1,
                                      -1, 1, -1, 62, 20, -1, -1, 1, -1, 1, -1, -1, -1, 1, 12, 21,
                                      1, -1, 1, -1, 1, -1, -1, 1, -1, 64, 22, -1, 1, 1, -1, 1, -1,
                                      -1, 1, -1, 22, 23, 1, 1, 1, -1, 1, -1, -1, -1, 1, 93, 24, -1,
                                      -1, -1, 1, 1, -1, -1, -1, -1, 11, 25, 1, -1, -1, 1, 1, -1, -1,
                                      1, 1, 5, 26, -1, 1, -1, 1, 1, -1, -1, 1, 1, 95, 27, 1, 1, -1,
                                      1, 1, -1, -1, -1, -1, 25, 28, -1, -1, 1, 1, 1, -1, -1, 1, 1,
                                      60, 29, 1, -1, 1, 1, 1, -1, -1, -1, -1, 67, 30, -1, 1, 1, 1,
                                      1, -1, -1, -1, -1, 63, 31, 1, 1, 1, 1, 1, -1, -1, 1, 1, 125,
                                      32, -1, -1, -1, -1, -1, 1, -1, -1, 1, 128, 33, 1, -1, -1, -1,
                                      -1, 1, -1, 1, -1, 1, 34, -1, 1, -1, -1, -1, 1, -1, 1, -1, 112,
                                      35, 1, 1, -1, -1, -1, 1, -1, -1, 1, 120, 36, -1, -1, 1, -1,
                                      -1, 1, -1, 1, -1, 40, 37, 1, -1, 1, -1, -1, 1, -1, -1, 1, 51,
                                      38, -1, 1, 1, -1, -1, 1, -1, -1, 1, 28, 39, 1, 1, 1, -1, -1,
                                      1, -1, 1, -1, 26, 40, -1, -1, -1, 1, -1, 1, -1, 1, 1, 106, 41,
                                      1, -1, -1, 1, -1, 1, -1, -1, -1, 19, 42, -1, 1, -1, 1, -1, 1,
                                      -1, -1, -1, 117, 43, 1, 1, -1, 1, -1, 1, -1, 1, 1, 96, 44, -1,
                                      -1, 1, 1, -1, 1, -1, -1, -1, 14, 45, 1, -1, 1, 1, -1, 1, -1,
                                      1, 1, 101, 46, -1, 1, 1, 1, -1, 1, -1, 1, 1, 59, 47, 1, 1, 1,
                                      1, -1, 1, -1, -1, -1, 2, 48, -1, -1, -1, -1, 1, 1, -1, 1, 1,
                                      126, 49, 1, -1, -1, -1, 1, 1, -1, -1, -1, 113, 50, -1, 1, -1,
                                      -1, 1, 1, -1, -1, -1, 127, 51, 1, 1, -1, -1, 1, 1, -1, 1, 1,
                                      70, 52, -1, -1, 1, -1, 1, 1, -1, -1, -1, 80, 53, 1, -1, 1, -1,
                                      1, 1, -1, 1, 1, 121, 54, -1, 1, 1, -1, 1, 1, -1, 1, 1, 50, 55,
                                      1, 1, 1, -1, 1, 1, -1, -1, -1, 78, 56, -1, -1, -1, 1, 1, 1,
                                      -1, -1, 1, 16, 57, 1, -1, -1, 1, 1, 1, -1, 1, -1, 73, 58, -1,
                                      1, -1, 1, 1, 1, -1, 1, -1, 32, 59, 1, 1, -1, 1, 1, 1, -1, -1,
                                      1, 90, 60, -1, -1, 1, 1, 1, 1, -1, 1, -1, 85, 61, 1, -1, 1,
                                      1, 1, 1, -1, -1, 1, 87, 62, -1, 1, 1, 1, 1, 1, -1, -1, 1, 36,
                                      63, 1, 1, 1, 1, 1, 1, -1, 1, -1, 104, 64, -1, -1, -1, -1, -1,
                                      -1, 1, -1, 1, 115, 65, 1, -1, -1, -1, -1, -1, 1, 1, -1, 39,
                                      66, -1, 1, -1, -1, -1, -1, 1, 1, -1, 52, 67, 1, 1, -1, -1, -1,
                                      -1, 1, -1, 1, 43, 68, -1, -1, 1, -1, -1, -1, 1, 1, -1, 111,
                                      69, 1, -1, 1, -1, -1, -1, 1, -1, 1, 23, 70, -1, 1, 1, -1, -1,
                                      -1, 1, -1, 1, 107, 71, 1, 1, 1, -1, -1, -1, 1, 1, -1, 119, 72,
                                      -1, -1, -1, 1, -1, -1, 1, 1, 1, 3, 73, 1, -1, -1, 1, -1, -1,
                                      1, -1, -1, 37, 74, -1, 1, -1, 1, -1, -1, 1, -1, -1, 74, 75,
                                      1, 1, -1, 1, -1, -1, 1, 1, 1, 9, 76, -1, -1, 1, 1, -1, -1, 1,
                                      -1, -1, 71, 77, 1, -1, 1, 1, -1, -1, 1, 1, 1, 35, 78, -1, 1,
                                      1, 1, -1, -1, 1, 1, 1, 83, 79, 1, 1, 1, 1, -1, -1, 1, -1, -1,
                                      8, 80, -1, -1, -1, -1, 1, -1, 1, 1, 1, 17, 81, 1, -1, -1, -1,
                                      1, -1, 1, -1, -1, 118, 82, -1, 1, -1, -1, 1, -1, 1, -1, -1,
                                      94, 83, 1, 1, -1, -1, 1, -1, 1, 1, 1, 77, 84, -1, -1, 1, -1,
                                      1, -1, 1, -1, -1, 20, 85, 1, -1, 1, -1, 1, -1, 1, 1, 1, 124,
                                      86, -1, 1, 1, -1, 1, -1, 1, 1, 1, 29, 87, 1, 1, 1, -1, 1, -1,
                                      1, -1, -1, 15, 88, -1, -1, -1, 1, 1, -1, 1, -1, 1, 58, 89, 1,
                                      -1, -1, 1, 1, -1, 1, 1, -1, 46, 90, -1, 1, -1, 1, 1, -1, 1,
                                      1, -1, 89, 91, 1, 1, -1, 1, 1, -1, 1, -1, 1, 69, 92, -1, -1,
                                      1, 1, 1, -1, 1, 1, -1, 61, 93, 1, -1, 1, 1, 1, -1, 1, -1, 1,
                                      102, 94, -1, 1, 1, 1, 1, -1, 1, -1, 1, 100, 95, 1, 1, 1, 1,
                                      1, -1, 1, 1, -1, 44, 96, -1, -1, -1, -1, -1, 1, 1, -1, -1, 49,
                                      97, 1, -1, -1, -1, -1, 1, 1, 1, 1, 33, 98, -1, 1, -1, -1, -1,
                                      1, 1, 1, 1, 53, 99, 1, 1, -1, -1, -1, 1, 1, -1, -1, 76, 100,
                                      -1, -1, 1, -1, -1, 1, 1, 1, 1, 21, 101, 1, -1, 1, -1, -1, 1,
                                      1, -1, -1, 47, 102, -1, 1, 1, -1, -1, 1, 1, -1, -1, 91, 103,
                                      1, 1, 1, -1, -1, 1, 1, 1, 1, 109, 104, -1, -1, -1, 1, -1, 1,
                                      1, 1, -1, 123, 105, 1, -1, -1, 1, -1, 1, 1, -1, 1, 68, 106,
                                      -1, 1, -1, 1, -1, 1, 1, -1, 1, 57, 107, 1, 1, -1, 1, -1, 1,
                                      1, 1, -1, 48, 108, -1, -1, 1, 1, -1, 1, 1, -1, 1, 72, 109, 1,
                                      -1, 1, 1, -1, 1, 1, 1, -1, 65, 110, -1, 1, 1, 1, -1, 1, 1, 1,
                                      -1, 6, 111, 1, 1, 1, 1, -1, 1, 1, -1, 1, 79, 112, -1, -1, -1,
                                      -1, 1, 1, 1, 1, -1, 42, 113, 1, -1, -1, -1, 1, 1, 1, -1, 1,
                                      4, 114, -1, 1, -1, -1, 1, 1, 1, -1, 1, 45, 115, 1, 1, -1, -1,
                                      1, 1, 1, 1, -1, 7, 116, -1, -1, 1, -1, 1, 1, 1, -1, 1, 81, 117,
                                      1, -1, 1, -1, 1, 1, 1, 1, -1, 99, 118, -1, 1, 1, -1, 1, 1, 1,
                                      1, -1, 56, 119, 1, 1, 1, -1, 1, 1, 1, -1, 1, 82, 120, -1, -1,
                                      -1, 1, 1, 1, 1, -1, -1, 13, 121, 1, -1, -1, 1, 1, 1, 1, 1, 1,
                                      97, 122, -1, 1, -1, 1, 1, 1, 1, 1, 1, 18, 123, 1, 1, -1, 1,
                                      1, 1, 1, -1, -1, 75, 124, -1, -1, 1, 1, 1, 1, 1, 1, 1, 103,
                                      125, 1, -1, 1, 1, 1, 1, 1, -1, -1, 41, 126, -1, 1, 1, 1, 1,
                                      1, 1, -1, -1, 31, 127, 1, 1, 1, 1, 1, 1, 1, 1, 1, 30, 128))
})

test_that("4.2 Nine Factor Two Level Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 128, 9, 0, 0, 1, "Value", 1, 128))
})

### Nine factors smallest fractional factorial ####

options <- analysisOptions("doeFactorial")
options$actualExporter <- FALSE
options$numberOfCategorical <- 9
options$categoricalVariables <- list(
  list(
    levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6", "Row 7", "Row 8"),
    name = "data 1",
    values = c("A", "B", "C", "D", "E", "F", "G", "H", "I")
  ),
  list(
    levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6", "Row 7", "Row 8"),
    name = "data 2",
    values = c("a", "a", "a", "a", "a", "a", "a", "a", "a")
  ),
  list(
    levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6", "Row 7", "Row 8"),
    name = "data 3",
    values = c("b", "b", "b", "b", "b", "b", "b", "b", "b")
  )
)
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "factorialTypeDefault"
options$runOrder <- "runOrderStandard"
options$selectedDesign2 <- list(list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 1", values = c(16, 32, 64, 128)), # give design list, always two lists, number of values gives number of designs
                                list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 2", values = c(0, 0, 0, 0))
)
options$selectedRow <- 0 # select design
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)


test_that("5.1 Nine Factor Two Level Frac. Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, -1, -1, -1, 1, 1, 1, -1, 1, 10, 1, 1, -1, -1, -1, -1, -1,
                                      -1, -1, -1, 11, 2, -1, 1, -1, -1, -1, 1, 1, 1, -1, 5, 3, 1,
                                      1, -1, -1, 1, -1, -1, 1, 1, 15, 4, -1, -1, 1, -1, 1, -1, 1,
                                      1, -1, 1, 5, 1, -1, 1, -1, -1, 1, -1, 1, 1, 2, 6, -1, 1, 1,
                                      -1, -1, -1, 1, -1, 1, 12, 7, 1, 1, 1, -1, 1, 1, -1, -1, -1,
                                      14, 8, -1, -1, -1, 1, 1, 1, -1, 1, -1, 3, 9, 1, -1, -1, 1, -1,
                                      -1, 1, 1, 1, 6, 10, -1, 1, -1, 1, -1, 1, -1, -1, 1, 7, 11, 1,
                                      1, -1, 1, 1, -1, 1, -1, -1, 4, 12, -1, -1, 1, 1, 1, -1, -1,
                                      -1, 1, 13, 13, 1, -1, 1, 1, -1, 1, 1, -1, -1, 9, 14, -1, 1,
                                      1, 1, -1, -1, -1, 1, -1, 8, 15, 1, 1, 1, 1, 1, 1, 1, 1, 1, 16,
                                      16))
})

test_that("5.2 Nine Factor Two Level Frac. Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 16, 9, 0, 0, 1, "Value", 1, 16))
})

## Center points ####


### One center point (verified with Minitab) ####

options <- analysisOptions("doeFactorial")
options$actualExporter <- FALSE
options$numberOfCategorical <- 3
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 1", values = c("A", "B", "C")),
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 2", values = c("a", "a", "a")),
  list(levels = c("Row 0","Row 1", "Row 2"), name = "data 3", values = c("b", "b", "b")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "factorialTypeDefault"
options$runOrder <- "runOrderStandard"
options$selectedDesign2 <- list(list(levels = c("Row 0", "Row 1") , name = "data 1", values = c(4, 8) ), # give design list, always two lists, number of values gives number of designs
                                list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(0, 0)))
options$selectedRow <- 1 # select design
options$centerpoints <- 1
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)

test_that("6.1 One Center Point Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0, 9, 0, -1, -1, -1, 5, 1, 1, -1, -1, 7, 2, -1, 1, -1, 1,
                                      3, 1, 1, -1, 3, 4, -1, -1, 1, 4, 5, 1, -1, 1, 8, 6, -1, 1, 1,
                                      6, 7, 1, 1, 1, 2, 8))
})

test_that("6.2 One Center Point Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 8, 3, 1, 0, 1, "Value", 1, 9))
})

### Two center points (verified with Minitab) ####

options <- analysisOptions("doeFactorial")
options$actualExporter <- FALSE
options$numberOfCategorical <- 3
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 1", values = c("A", "B", "C")),
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 2", values = c("a", "a", "a")),
  list(levels = c("Row 0","Row 1", "Row 2"), name = "data 3", values = c("b", "b", "b")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "factorialTypeDefault"
options$runOrder <- "runOrderStandard"
options$selectedDesign2 <- list(list(levels = c("Row 0", "Row 1") , name = "data 1", values = c(4, 8) ), # give design list, always two lists, number of values gives number of designs
                                list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(0, 0)))
options$selectedRow <- 1 # select design
options$centerpoints <- 2
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)


test_that("7.1 Two Center Point Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0, 1, 1, 0, 0, 0, 10, 1, -1, -1, -1, 6, 2, 1, -1, -1, 8,
                                      3, -1, 1, -1, 2, 4, 1, 1, -1, 4, 5, -1, -1, 1, 5, 6, 1, -1,
                                      1, 9, 7, -1, 1, 1, 7, 8, 1, 1, 1, 3, 9))
})

test_that("7.2 Two Center Point Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 8, 3, 2, 0, 1, "Value", 1, 10))
})

### Four center points (verified with Minitab) ####

options <- analysisOptions("doeFactorial")
options$actualExporter <- FALSE
options$numberOfCategorical <- 3
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 1", values = c("A", "B", "C")),
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 2", values = c("a", "a", "a")),
  list(levels = c("Row 0","Row 1", "Row 2"), name = "data 3", values = c("b", "b", "b")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "factorialTypeDefault"
options$runOrder <- "runOrderStandard"
options$selectedDesign2 <- list(list(levels = c("Row 0", "Row 1") , name = "data 1", values = c(4, 8) ), # give design list, always two lists, number of values gives number of designs
                                list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(0, 0)))
options$selectedRow <- 1 # select design
options$centerpoints <- 4
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)


test_that("8.1 Four Center Point Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0, 1, 1, 0, 0, 0, 2, 1, 0, 0, 0, 7, 1, 0, 0, 0, 12, 1, -1,
                                      -1, -1, 8, 2, 1, -1, -1, 10, 3, -1, 1, -1, 3, 4, 1, 1, -1, 5,
                                      5, -1, -1, 1, 6, 6, 1, -1, 1, 11, 7, -1, 1, 1, 9, 8, 1, 1, 1,
                                      4, 9))
})

test_that("8.2 Four Center Point Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 8, 3, 4, 0, 1, "Value", 1, 12))
})

## Corner points ####

## Blocks ####

# HTC factor designs ####

# General full factorial designs ####


