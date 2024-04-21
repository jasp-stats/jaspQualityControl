context("[Quality Control] DoE Factorial Design")

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

## Blocks ####

### Two blocks (verified with Minitab) ####

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
options$blocks <- "2"
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)

test_that("9.1 Two Blocks Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, -1, 1, -1, 4, 1, -1, -1, 2, 1, 5, 2, -1, 1, 2, -1, 7, 3, -1,
                                      1, 1, 1, 1, 4, 1, -1, 2, -1, 6, 5, 1, -1, 1, 1, 3, 6, 1, 1,
                                      1, -1, 2, 7, 1, 1, 2, 1, 8, 8))
})

test_that("9.2 Two Blocks Factorial Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2, 8, 3, 0, 0, 1, "Value", 2, 8))
})

### Four blocks (verified with Minitab) ####

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
options$blocks <- "4"
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)

test_that("10.1 Four Blocks Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, -1, 1, -1, 8, 1, -1, -1, 2, 1, 5, 2, -1, 1, 3, -1, 4, 3, -1,
                                      1, 4, 1, 1, 4, 1, -1, 4, -1, 2, 5, 1, -1, 3, 1, 3, 6, 1, 1,
                                      2, -1, 6, 7, 1, 1, 1, 1, 7, 8))
})

test_that("10.2 Four Blocks Factorial Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4, 8, 3, 0, 0, 1, "Value", 4, 8))
})

### Eight blocks (verified with Minitab) ####

options <- analysisOptions("doeFactorial")
options$actualExporter <- FALSE
options$numberOfCategorical <- 4
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 1", values = c("A", "B", "C", "D")),
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 2", values = c("a", "a", "a", "a")),
  list(levels = c("Row 0","Row 1", "Row 2"), name = "data 3", values = c("b", "b", "b", "b")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "factorialTypeDefault"
options$runOrder <- "runOrderStandard"
options$selectedDesign2 <- list(list(levels = c("Row 0", "Row 1") , name = "data 1", values = c(8, 16) ), # give design list, always two lists, number of values gives number of designs
                                list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(0, 0)))
options$selectedRow <- 1 # select design
options$blocks <- "8"
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)

test_that("11.1 Eight Blocks Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, -1, 1, -1, -1, 16, 1, -1, -1, 2, -1, 1, 14, 2, -1, -1, 3,
                                      1, -1, 11, 3, -1, -1, 4, 1, 1, 10, 4, -1, 1, 5, -1, -1, 8, 5,
                                      -1, 1, 6, -1, 1, 5, 6, -1, 1, 7, 1, -1, 4, 7, -1, 1, 8, 1, 1,
                                      1, 8, 1, -1, 8, -1, -1, 2, 9, 1, -1, 7, -1, 1, 3, 10, 1, -1,
                                      6, 1, -1, 6, 11, 1, -1, 5, 1, 1, 7, 12, 1, 1, 4, -1, -1, 9,
                                      13, 1, 1, 3, -1, 1, 12, 14, 1, 1, 2, 1, -1, 13, 15, 1, 1, 1,
                                      1, 1, 15, 16))
})

test_that("11.2 Eight Blocks Factorial Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(8, 16, 4, 0, 0, 1, "Value", 8, 16))
})

# HTC factor designs ####

### Two factor one HTC (verified with Minitab) ####

options <- analysisOptions("doeFactorial")
options$actualExporter <- FALSE
options$numberOfCategorical <- 2
options$factorialDesignTypeSplitPlotNumberHardToChangeFactors <- 1
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0", "Row 1"), name = "data 1", values = c("A", "B")),
  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c("a", "a")),
  list(levels = c("Row 0","Row 1"), name = "data 3", values = c("b", "b")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "factorialTypeSplit"
options$runOrder <- "runOrderStandard"
options$selectedDesign2 <- list(list(levels = "Row 0", name = "data 1", values = 4), # give design list, always two lists, number of values gives number of designs
                                list(levels = "Row 0", name = "data 2", values = 0))
options$selectedRow <- 0 # select design
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)

test_that("12.1 Two Factor One HTC Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, 1, 1, 1, -1, -1, 2, 2, 1, -1, 4, 3, 1, 1, 3, 4))
})

test_that("12.2 Two Factor One HTC Factorial Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 4, 2, 0, 0, 1, "Value", 1, 4))
})

### Five factor two HTC (verified with Minitab) ####

options <- analysisOptions("doeFactorial")
options$actualExporter <- FALSE
options$numberOfCategorical <- 5
options$factorialDesignTypeSplitPlotNumberHardToChangeFactors <- 2
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0", "Row 1"), name = "data 1", values = c("A", "B", "C", "D", "E")),
  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c("a", "a", "a", "a", "a")),
  list(levels = c("Row 0","Row 1"), name = "data 3", values = c("b", "b", "b", "b", "b")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "factorialTypeSplit"
options$runOrder <- "runOrderStandard"
options$selectedDesign2 <- list(list(levels = "Row 0", name = "data 1", values = c(16,32)), # give design list, always two lists, number of values gives number of designs
                                list(levels = "Row 0", name = "data 2", values = c(0, 0)))
options$selectedRow <- 1 # select design
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)

test_that("13.1 Five Factor Two HTC Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, 1, 1, 1, 1, 5, 1, -1, 1, 1, 1, -1, 7, 2, -1, 1, 1, -1, -1,
                                      1, 3, -1, 1, 1, -1, 1, 3, 4, -1, 1, -1, -1, 1, 4, 5, -1, 1,
                                      -1, -1, -1, 8, 6, -1, 1, -1, 1, -1, 6, 7, -1, 1, -1, 1, 1, 2,
                                      8, -1, -1, -1, 1, 1, 10, 9, -1, -1, -1, 1, -1, 11, 10, -1, -1,
                                      -1, -1, -1, 13, 11, -1, -1, -1, -1, 1, 15, 12, -1, -1, 1, -1,
                                      1, 16, 13, -1, -1, 1, -1, -1, 9, 14, -1, -1, 1, 1, -1, 12, 15,
                                      -1, -1, 1, 1, 1, 14, 16, 1, -1, 1, 1, 1, 22, 17, 1, -1, 1, 1,
                                      -1, 23, 18, 1, -1, 1, -1, -1, 19, 19, 1, -1, 1, -1, 1, 20, 20,
                                      1, -1, -1, -1, 1, 21, 21, 1, -1, -1, -1, -1, 17, 22, 1, -1,
                                      -1, 1, -1, 18, 23, 1, -1, -1, 1, 1, 24, 24, 1, 1, -1, 1, 1,
                                      27, 25, 1, 1, -1, 1, -1, 28, 26, 1, 1, -1, -1, -1, 25, 27, 1,
                                      1, -1, -1, 1, 29, 28, 1, 1, 1, -1, 1, 30, 29, 1, 1, 1, -1, -1,
                                      31, 30, 1, 1, 1, 1, -1, 32, 31, 1, 1, 1, 1, 1, 26, 32))
})

test_that("13.1 Five Factor Two HTC Factorial Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 32, 5, 0, 0, 1, "Value", 1, 32))
})


### Seven factor three HTC (verified with Minitab) ####

options <- analysisOptions("doeFactorial")
options$actualExporter <- FALSE
options$numberOfCategorical <- 7
options$factorialDesignTypeSplitPlotNumberHardToChangeFactors <- 3
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0", "Row 1"), name = "data 1", values = c("A", "B", "C", "D", "E", "F", "G")),
  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c("a", "a", "a", "a", "a", "a", "a")),
  list(levels = c("Row 0","Row 1"), name = "data 3", values = c("b", "b", "b", "b", "b", "b", "b")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "factorialTypeSplit"
options$runOrder <- "runOrderStandard"
options$selectedDesign2 <- list(list(levels = "Row 0", name = "data 1", values = c(64, 128)), # give design list, always two lists, number of values gives number of designs
                                list(levels = "Row 0", name = "data 2", values = c(0, 0)))
options$selectedRow <- 1 # select design
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)

test_that("14.1 Seven Factor Three HTC Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, 1, 1, 1, 1, 1, 1, 10, 1, -1, 1, 1, 1, 1, 1, -1, 11, 2, -1,
                                      1, 1, 1, 1, -1, -1, 5, 3, -1, 1, 1, 1, 1, -1, 1, 15, 4, -1,
                                      1, 1, 1, -1, -1, 1, 1, 5, -1, 1, 1, 1, -1, -1, -1, 2, 6, -1,
                                      1, 1, 1, -1, 1, -1, 12, 7, -1, 1, 1, 1, -1, 1, 1, 14, 8, -1,
                                      1, 1, -1, -1, 1, 1, 3, 9, -1, 1, 1, -1, -1, 1, -1, 6, 10, -1,
                                      1, 1, -1, -1, -1, -1, 7, 11, -1, 1, 1, -1, -1, -1, 1, 4, 12,
                                      -1, 1, 1, -1, 1, -1, 1, 13, 13, -1, 1, 1, -1, 1, -1, -1, 9,
                                      14, -1, 1, 1, -1, 1, 1, -1, 8, 15, -1, 1, 1, -1, 1, 1, 1, 16,
                                      16, -1, 1, -1, -1, 1, 1, 1, 27, 17, -1, 1, -1, -1, 1, 1, -1,
                                      24, 18, -1, 1, -1, -1, 1, -1, -1, 22, 19, -1, 1, -1, -1, 1,
                                      -1, 1, 29, 20, -1, 1, -1, -1, -1, -1, 1, 30, 21, -1, 1, -1,
                                      -1, -1, -1, -1, 19, 22, -1, 1, -1, -1, -1, 1, -1, 23, 23, -1,
                                      1, -1, -1, -1, 1, 1, 26, 24, -1, 1, -1, 1, -1, 1, 1, 28, 25,
                                      -1, 1, -1, 1, -1, 1, -1, 32, 26, -1, 1, -1, 1, -1, -1, -1, 20,
                                      27, -1, 1, -1, 1, -1, -1, 1, 17, 28, -1, 1, -1, 1, 1, -1, 1,
                                      25, 29, -1, 1, -1, 1, 1, -1, -1, 31, 30, -1, 1, -1, 1, 1, 1,
                                      -1, 18, 31, -1, 1, -1, 1, 1, 1, 1, 21, 32, -1, -1, -1, 1, 1,
                                      1, 1, 47, 33, -1, -1, -1, 1, 1, 1, -1, 38, 34, -1, -1, -1, 1,
                                      1, -1, -1, 34, 35, -1, -1, -1, 1, 1, -1, 1, 40, 36, -1, -1,
                                      -1, 1, -1, -1, 1, 42, 37, -1, -1, -1, 1, -1, -1, -1, 45, 38,
                                      -1, -1, -1, 1, -1, 1, -1, 41, 39, -1, -1, -1, 1, -1, 1, 1, 33,
                                      40, -1, -1, -1, -1, -1, 1, 1, 36, 41, -1, -1, -1, -1, -1, 1,
                                      -1, 37, 42, -1, -1, -1, -1, -1, -1, -1, 48, 43, -1, -1, -1,
                                      -1, -1, -1, 1, 35, 44, -1, -1, -1, -1, 1, -1, 1, 46, 45, -1,
                                      -1, -1, -1, 1, -1, -1, 43, 46, -1, -1, -1, -1, 1, 1, -1, 44,
                                      47, -1, -1, -1, -1, 1, 1, 1, 39, 48, -1, -1, 1, -1, 1, 1, 1,
                                      55, 49, -1, -1, 1, -1, 1, 1, -1, 64, 50, -1, -1, 1, -1, 1, -1,
                                      -1, 54, 51, -1, -1, 1, -1, 1, -1, 1, 58, 52, -1, -1, 1, -1,
                                      -1, -1, 1, 63, 53, -1, -1, 1, -1, -1, -1, -1, 53, 54, -1, -1,
                                      1, -1, -1, 1, -1, 51, 55, -1, -1, 1, -1, -1, 1, 1, 60, 56, -1,
                                      -1, 1, 1, -1, 1, 1, 62, 57, -1, -1, 1, 1, -1, 1, -1, 56, 58,
                                      -1, -1, 1, 1, -1, -1, -1, 50, 59, -1, -1, 1, 1, -1, -1, 1, 49,
                                      60, -1, -1, 1, 1, 1, -1, 1, 61, 61, -1, -1, 1, 1, 1, -1, -1,
                                      59, 62, -1, -1, 1, 1, 1, 1, -1, 57, 63, -1, -1, 1, 1, 1, 1,
                                      1, 52, 64, 1, -1, 1, 1, 1, 1, 1, 80, 65, 1, -1, 1, 1, 1, 1,
                                      -1, 69, 66, 1, -1, 1, 1, 1, -1, -1, 73, 67, 1, -1, 1, 1, 1,
                                      -1, 1, 66, 68, 1, -1, 1, 1, -1, -1, 1, 76, 69, 1, -1, 1, 1,
                                      -1, -1, -1, 79, 70, 1, -1, 1, 1, -1, 1, -1, 67, 71, 1, -1, 1,
                                      1, -1, 1, 1, 72, 72, 1, -1, 1, -1, -1, 1, 1, 74, 73, 1, -1,
                                      1, -1, -1, 1, -1, 68, 74, 1, -1, 1, -1, -1, -1, -1, 65, 75,
                                      1, -1, 1, -1, -1, -1, 1, 78, 76, 1, -1, 1, -1, 1, -1, 1, 70,
                                      77, 1, -1, 1, -1, 1, -1, -1, 75, 78, 1, -1, 1, -1, 1, 1, -1,
                                      71, 79, 1, -1, 1, -1, 1, 1, 1, 77, 80, 1, -1, -1, -1, 1, 1,
                                      1, 92, 81, 1, -1, -1, -1, 1, 1, -1, 88, 82, 1, -1, -1, -1, 1,
                                      -1, -1, 86, 83, 1, -1, -1, -1, 1, -1, 1, 94, 84, 1, -1, -1,
                                      -1, -1, -1, 1, 84, 85, 1, -1, -1, -1, -1, -1, -1, 83, 86, 1,
                                      -1, -1, -1, -1, 1, -1, 81, 87, 1, -1, -1, -1, -1, 1, 1, 87,
                                      88, 1, -1, -1, 1, -1, 1, 1, 89, 89, 1, -1, -1, 1, -1, 1, -1,
                                      85, 90, 1, -1, -1, 1, -1, -1, -1, 82, 91, 1, -1, -1, 1, -1,
                                      -1, 1, 90, 92, 1, -1, -1, 1, 1, -1, 1, 96, 93, 1, -1, -1, 1,
                                      1, -1, -1, 95, 94, 1, -1, -1, 1, 1, 1, -1, 93, 95, 1, -1, -1,
                                      1, 1, 1, 1, 91, 96, 1, 1, -1, 1, 1, 1, 1, 107, 97, 1, 1, -1,
                                      1, 1, 1, -1, 106, 98, 1, 1, -1, 1, 1, -1, -1, 103, 99, 1, 1,
                                      -1, 1, 1, -1, 1, 102, 100, 1, 1, -1, 1, -1, -1, 1, 111, 101,
                                      1, 1, -1, 1, -1, -1, -1, 105, 102, 1, 1, -1, 1, -1, 1, -1, 98,
                                      103, 1, 1, -1, 1, -1, 1, 1, 97, 104, 1, 1, -1, -1, -1, 1, 1,
                                      104, 105, 1, 1, -1, -1, -1, 1, -1, 108, 106, 1, 1, -1, -1, -1,
                                      -1, -1, 109, 107, 1, 1, -1, -1, -1, -1, 1, 99, 108, 1, 1, -1,
                                      -1, 1, -1, 1, 101, 109, 1, 1, -1, -1, 1, -1, -1, 112, 110, 1,
                                      1, -1, -1, 1, 1, -1, 110, 111, 1, 1, -1, -1, 1, 1, 1, 100, 112,
                                      1, 1, 1, -1, 1, 1, 1, 116, 113, 1, 1, 1, -1, 1, 1, -1, 118,
                                      114, 1, 1, 1, -1, 1, -1, -1, 115, 115, 1, 1, 1, -1, 1, -1, 1,
                                      122, 116, 1, 1, 1, -1, -1, -1, 1, 119, 117, 1, 1, 1, -1, -1,
                                      -1, -1, 113, 118, 1, 1, 1, -1, -1, 1, -1, 114, 119, 1, 1, 1,
                                      -1, -1, 1, 1, 121, 120, 1, 1, 1, 1, -1, 1, 1, 117, 121, 1, 1,
                                      1, 1, -1, 1, -1, 128, 122, 1, 1, 1, 1, -1, -1, -1, 126, 123,
                                      1, 1, 1, 1, -1, -1, 1, 127, 124, 1, 1, 1, 1, 1, -1, 1, 124,
                                      125, 1, 1, 1, 1, 1, -1, -1, 123, 126, 1, 1, 1, 1, 1, 1, -1,
                                      125, 127, 1, 1, 1, 1, 1, 1, 1, 120, 128))
})

test_that("14.2 Seven Factor Three HTC FactorialDesign Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 128, 7, 0, 0, 1, "Value", 1, 128))
})

# General full factorial designs ####

### Three factors three levels  (verified with Minitab) ####

options <- analysisOptions("doeFactorial")
options$actualExporter <- FALSE
options$numberOfCategorical <- 3
options$categoricalNoLevels <- 3
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0", "Row 1"), name = "data 1", values = c("A", "B", "C")),
  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c("a", "a", "a")),
  list(levels = c("Row 0","Row 1"), name = "data 3", values = c("b", "b", "b")),
  list(levels = c("Row 0","Row 1"), name = "data 4", values = c("c", "c", "c")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "generalFullFactorial"
options$runOrder <- "runOrderStandard"
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)

test_that("15.1 Three Factor Three Level General Full Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1, 1, 24, 1, 2, 1, 1, 10, 2, 3, 1, 1, 12, 3, 1, 2, 1, 11, 4,
                                      2, 2, 1, 5, 5, 3, 2, 1, 14, 6, 1, 3, 1, 20, 7, 2, 3, 1, 1, 8,
                                      3, 3, 1, 23, 9, 1, 1, 2, 2, 10, 2, 1, 2, 13, 11, 3, 1, 2, 9,
                                      12, 1, 2, 2, 21, 13, 2, 2, 2, 8, 14, 3, 2, 2, 3, 15, 1, 3, 2,
                                      26, 16, 2, 3, 2, 19, 17, 3, 3, 2, 22, 18, 1, 1, 3, 27, 19, 2,
                                      1, 3, 6, 20, 3, 1, 3, 16, 21, 1, 2, 3, 4, 22, 2, 2, 3, 25, 23,
                                      3, 2, 3, 7, 24, 1, 3, 3, 15, 25, 2, 3, 3, 18, 26, 3, 3, 3, 17,
                                      27))
})

test_that("15.2 Three Factor Three Level General Full Factorial Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(27, 3, 0, 1, "Value", 1, 27))
})

### Three factors 2*three and 1*four levels  (verified with Minitab) ####

options <- analysisOptions("doeFactorial")
options$actualExporter <- FALSE
options$numberOfCategorical <- 3
options$categoricalNoLevels <- 4
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0", "Row 1"), name = "data 1", values = c("A", "B", "C")),
  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c("a", "a", "a")),
  list(levels = c("Row 0","Row 1"), name = "data 3", values = c("b", "b", "b")),
  list(levels = c("Row 0","Row 1"), name = "data 4", values = c("c", "c", "c")),
  list(levels = c("Row 0","Row 1"), name = "data 5", values = c("d", "", "")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "generalFullFactorial"
options$runOrder <- "runOrderStandard"
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)

test_that("16.1 Three Factor Four Level General Full Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1, 1, 27, 1, 2, 1, 1, 10, 2, 3, 1, 1, 30, 3, 4, 1, 1, 22, 4,
                                      1, 2, 1, 12, 5, 2, 2, 1, 11, 6, 3, 2, 1, 5, 7, 4, 2, 1, 31,
                                      8, 1, 3, 1, 14, 9, 2, 3, 1, 1, 10, 3, 3, 1, 16, 11, 4, 3, 1,
                                      28, 12, 1, 1, 2, 33, 13, 2, 1, 2, 2, 14, 3, 1, 2, 17, 15, 4,
                                      1, 2, 25, 16, 1, 2, 2, 13, 17, 2, 2, 2, 9, 18, 3, 2, 2, 18,
                                      19, 4, 2, 2, 3, 20, 1, 3, 2, 36, 21, 2, 3, 2, 34, 22, 3, 3,
                                      2, 35, 23, 4, 3, 2, 15, 24, 1, 1, 3, 26, 25, 2, 1, 3, 32, 26,
                                      3, 1, 3, 24, 27, 4, 1, 3, 6, 28, 1, 2, 3, 7, 29, 2, 2, 3, 4,
                                      30, 3, 2, 3, 29, 31, 4, 2, 3, 19, 32, 1, 3, 3, 21, 33, 2, 3,
                                      3, 8, 34, 3, 3, 3, 20, 35, 4, 3, 3, 23, 36))
})

test_that("16.2 Three Factor Four Level General Full Factorial Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(36, 3, 0, 1, "Value", 1, 36))
})

### Four factors 1x three, 1*two, 1*five and 1*six levels  (verified with Minitab) ####

options <- analysisOptions("doeFactorial")
options$actualExporter <- FALSE
options$numberOfCategorical <- 4
options$categoricalNoLevels <- 6
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0", "Row 1"), name = "data 1", values = c("A", "B", "C", "D")),
  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c("a", "a", "a", "a")),
  list(levels = c("Row 0","Row 1"), name = "data 3", values = c("b", "b", "b", "b")),
  list(levels = c("Row 0","Row 1"), name = "data 4", values = c("c", "c", "c", "")),
  list(levels = c("Row 0","Row 1"), name = "data 5", values = c("d", "d", "d", "")),
  list(levels = c("Row 0","Row 1"), name = "data 6", values = c("e", "e", "", "")),
  list(levels = c("Row 0","Row 1"), name = "data 7", values = c("f", "", "", "")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "generalFullFactorial"
options$runOrder <- "runOrderStandard"
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)

test_that("17.1 Four Factor Six Level General Full Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1, 1, 1, 228, 1, 2, 1, 1, 1, 116, 2, 3, 1, 1, 1, 27, 3, 4,
                                      1, 1, 1, 133, 4, 5, 1, 1, 1, 47, 5, 6, 1, 1, 1, 221, 6, 1, 2,
                                      1, 1, 158, 7, 2, 2, 1, 1, 216, 8, 3, 2, 1, 1, 92, 9, 4, 2, 1,
                                      1, 149, 10, 5, 2, 1, 1, 186, 11, 6, 2, 1, 1, 182, 12, 1, 3,
                                      1, 1, 118, 13, 2, 3, 1, 1, 55, 14, 3, 3, 1, 1, 10, 15, 4, 3,
                                      1, 1, 156, 16, 5, 3, 1, 1, 147, 17, 6, 3, 1, 1, 107, 18, 1,
                                      4, 1, 1, 56, 19, 2, 4, 1, 1, 192, 20, 3, 4, 1, 1, 124, 21, 4,
                                      4, 1, 1, 38, 22, 5, 4, 1, 1, 159, 23, 6, 4, 1, 1, 128, 24, 1,
                                      5, 1, 1, 168, 25, 2, 5, 1, 1, 129, 26, 3, 5, 1, 1, 174, 27,
                                      4, 5, 1, 1, 24, 28, 5, 5, 1, 1, 106, 29, 6, 5, 1, 1, 233, 30,
                                      1, 1, 2, 1, 166, 31, 2, 1, 2, 1, 86, 32, 3, 1, 2, 1, 200, 33,
                                      4, 1, 2, 1, 223, 34, 5, 1, 2, 1, 163, 35, 6, 1, 2, 1, 91, 36,
                                      1, 2, 2, 1, 198, 37, 2, 2, 2, 1, 89, 38, 3, 2, 2, 1, 34, 39,
                                      4, 2, 2, 1, 154, 40, 5, 2, 2, 1, 12, 41, 6, 2, 2, 1, 177, 42,
                                      1, 3, 2, 1, 146, 43, 2, 3, 2, 1, 155, 44, 3, 3, 2, 1, 157, 45,
                                      4, 3, 2, 1, 54, 46, 5, 3, 2, 1, 22, 47, 6, 3, 2, 1, 5, 48, 1,
                                      4, 2, 1, 203, 49, 2, 4, 2, 1, 102, 50, 3, 4, 2, 1, 215, 51,
                                      4, 4, 2, 1, 84, 52, 5, 4, 2, 1, 62, 53, 6, 4, 2, 1, 234, 54,
                                      1, 5, 2, 1, 114, 55, 2, 5, 2, 1, 74, 56, 3, 5, 2, 1, 211, 57,
                                      4, 5, 2, 1, 25, 58, 5, 5, 2, 1, 57, 59, 6, 5, 2, 1, 142, 60,
                                      1, 1, 3, 1, 164, 61, 2, 1, 3, 1, 207, 62, 3, 1, 3, 1, 136, 63,
                                      4, 1, 3, 1, 1, 64, 5, 1, 3, 1, 237, 65, 6, 1, 3, 1, 97, 66,
                                      1, 2, 3, 1, 130, 67, 2, 2, 3, 1, 176, 68, 3, 2, 3, 1, 134, 69,
                                      4, 2, 3, 1, 81, 70, 5, 2, 3, 1, 145, 71, 6, 2, 3, 1, 30, 72,
                                      1, 3, 3, 1, 220, 73, 2, 3, 3, 1, 60, 74, 3, 3, 3, 1, 226, 75,
                                      4, 3, 3, 1, 196, 76, 5, 3, 3, 1, 189, 77, 6, 3, 3, 1, 120, 78,
                                      1, 4, 3, 1, 110, 79, 2, 4, 3, 1, 75, 80, 3, 4, 3, 1, 181, 81,
                                      4, 4, 3, 1, 28, 82, 5, 4, 3, 1, 40, 83, 6, 4, 3, 1, 26, 84,
                                      1, 5, 3, 1, 19, 85, 2, 5, 3, 1, 100, 86, 3, 5, 3, 1, 105, 87,
                                      4, 5, 3, 1, 14, 88, 5, 5, 3, 1, 2, 89, 6, 5, 3, 1, 180, 90,
                                      1, 1, 4, 1, 51, 91, 2, 1, 4, 1, 101, 92, 3, 1, 4, 1, 48, 93,
                                      4, 1, 4, 1, 238, 94, 5, 1, 4, 1, 58, 95, 6, 1, 4, 1, 93, 96,
                                      1, 2, 4, 1, 175, 97, 2, 2, 4, 1, 170, 98, 3, 2, 4, 1, 135, 99,
                                      4, 2, 4, 1, 173, 100, 5, 2, 4, 1, 139, 101, 6, 2, 4, 1, 31,
                                      102, 1, 3, 4, 1, 33, 103, 2, 3, 4, 1, 45, 104, 3, 3, 4, 1, 208,
                                      105, 4, 3, 4, 1, 229, 106, 5, 3, 4, 1, 141, 107, 6, 3, 4, 1,
                                      222, 108, 1, 4, 4, 1, 44, 109, 2, 4, 4, 1, 87, 110, 3, 4, 4,
                                      1, 165, 111, 4, 4, 4, 1, 16, 112, 5, 4, 4, 1, 143, 113, 6, 4,
                                      4, 1, 82, 114, 1, 5, 4, 1, 65, 115, 2, 5, 4, 1, 96, 116, 3,
                                      5, 4, 1, 219, 117, 4, 5, 4, 1, 240, 118, 5, 5, 4, 1, 85, 119,
                                      6, 5, 4, 1, 121, 120, 1, 1, 1, 2, 59, 121, 2, 1, 1, 2, 109,
                                      122, 3, 1, 1, 2, 239, 123, 4, 1, 1, 2, 185, 124, 5, 1, 1, 2,
                                      232, 125, 6, 1, 1, 2, 32, 126, 1, 2, 1, 2, 111, 127, 2, 2, 1,
                                      2, 199, 128, 3, 2, 1, 2, 42, 129, 4, 2, 1, 2, 119, 130, 5, 2,
                                      1, 2, 150, 131, 6, 2, 1, 2, 138, 132, 1, 3, 1, 2, 50, 133, 2,
                                      3, 1, 2, 160, 134, 3, 3, 1, 2, 153, 135, 4, 3, 1, 2, 104, 136,
                                      5, 3, 1, 2, 3, 137, 6, 3, 1, 2, 36, 138, 1, 4, 1, 2, 152, 139,
                                      2, 4, 1, 2, 204, 140, 3, 4, 1, 2, 49, 141, 4, 4, 1, 2, 72, 142,
                                      5, 4, 1, 2, 23, 143, 6, 4, 1, 2, 214, 144, 1, 5, 1, 2, 99, 145,
                                      2, 5, 1, 2, 9, 146, 3, 5, 1, 2, 39, 147, 4, 5, 1, 2, 76, 148,
                                      5, 5, 1, 2, 132, 149, 6, 5, 1, 2, 70, 150, 1, 1, 2, 2, 213,
                                      151, 2, 1, 2, 2, 103, 152, 3, 1, 2, 2, 115, 153, 4, 1, 2, 2,
                                      8, 154, 5, 1, 2, 2, 80, 155, 6, 1, 2, 2, 43, 156, 1, 2, 2, 2,
                                      13, 157, 2, 2, 2, 2, 108, 158, 3, 2, 2, 2, 95, 159, 4, 2, 2,
                                      2, 148, 160, 5, 2, 2, 2, 17, 161, 6, 2, 2, 2, 206, 162, 1, 3,
                                      2, 2, 37, 163, 2, 3, 2, 2, 184, 164, 3, 3, 2, 2, 41, 165, 4,
                                      3, 2, 2, 210, 166, 5, 3, 2, 2, 151, 167, 6, 3, 2, 2, 98, 168,
                                      1, 4, 2, 2, 77, 169, 2, 4, 2, 2, 123, 170, 3, 4, 2, 2, 35, 171,
                                      4, 4, 2, 2, 20, 172, 5, 4, 2, 2, 131, 173, 6, 4, 2, 2, 15, 174,
                                      1, 5, 2, 2, 113, 175, 2, 5, 2, 2, 169, 176, 3, 5, 2, 2, 73,
                                      177, 4, 5, 2, 2, 161, 178, 5, 5, 2, 2, 126, 179, 6, 5, 2, 2,
                                      162, 180, 1, 1, 3, 2, 193, 181, 2, 1, 3, 2, 227, 182, 3, 1,
                                      3, 2, 112, 183, 4, 1, 3, 2, 64, 184, 5, 1, 3, 2, 29, 185, 6,
                                      1, 3, 2, 195, 186, 1, 2, 3, 2, 66, 187, 2, 2, 3, 2, 205, 188,
                                      3, 2, 3, 2, 83, 189, 4, 2, 3, 2, 235, 190, 5, 2, 3, 2, 68, 191,
                                      6, 2, 3, 2, 194, 192, 1, 3, 3, 2, 218, 193, 2, 3, 3, 2, 167,
                                      194, 3, 3, 3, 2, 231, 195, 4, 3, 3, 2, 172, 196, 5, 3, 3, 2,
                                      202, 197, 6, 3, 3, 2, 127, 198, 1, 4, 3, 2, 94, 199, 2, 4, 3,
                                      2, 61, 200, 3, 4, 3, 2, 53, 201, 4, 4, 3, 2, 230, 202, 5, 4,
                                      3, 2, 90, 203, 6, 4, 3, 2, 52, 204, 1, 5, 3, 2, 179, 205, 2,
                                      5, 3, 2, 21, 206, 3, 5, 3, 2, 188, 207, 4, 5, 3, 2, 201, 208,
                                      5, 5, 3, 2, 79, 209, 6, 5, 3, 2, 183, 210, 1, 1, 4, 2, 209,
                                      211, 2, 1, 4, 2, 6, 212, 3, 1, 4, 2, 63, 213, 4, 1, 4, 2, 212,
                                      214, 5, 1, 4, 2, 67, 215, 6, 1, 4, 2, 4, 216, 1, 2, 4, 2, 190,
                                      217, 2, 2, 4, 2, 122, 218, 3, 2, 4, 2, 178, 219, 4, 2, 4, 2,
                                      191, 220, 5, 2, 4, 2, 225, 221, 6, 2, 4, 2, 7, 222, 1, 3, 4,
                                      2, 217, 223, 2, 3, 4, 2, 224, 224, 3, 3, 4, 2, 144, 225, 4,
                                      3, 4, 2, 171, 226, 5, 3, 4, 2, 125, 227, 6, 3, 4, 2, 140, 228,
                                      1, 4, 4, 2, 187, 229, 2, 4, 4, 2, 197, 230, 3, 4, 4, 2, 69,
                                      231, 4, 4, 4, 2, 137, 232, 5, 4, 4, 2, 46, 233, 6, 4, 4, 2,
                                      18, 234, 1, 5, 4, 2, 88, 235, 2, 5, 4, 2, 11, 236, 3, 5, 4,
                                      2, 71, 237, 4, 5, 4, 2, 236, 238, 5, 5, 4, 2, 117, 239, 6, 5,
                                      4, 2, 78, 240))
})

test_that("17.1 Four Factor Six Level General Full Factorial Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(240, 4, 0, 1, "Value", 1, 240))
})
