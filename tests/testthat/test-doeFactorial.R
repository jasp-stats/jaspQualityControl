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
results <- runAnalysis("doeFactorial", "debug.csv", options)


test_that("1.1 Two Factor Two Level Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, -1, 1, 1, 1, -1, 4, 2, -1, 1, 2, 3, 1, 1,
                                  3, 4))
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
results <- runAnalysis("doeFactorial", "debug.csv", options)

test_that("2.1 Five Factor Two Level Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, -1, -1, -1, -1, 4, 1, 1, -1, -1, -1, -1,
                                  5, 2, -1, 1, -1, -1, -1, 30, 3, 1, 1, -1, -1,
                                  -1, 2, 4, -1, -1, 1, -1, -1, 19, 5, 1, -1, 1,
                                  -1, -1, 27, 6, -1, 1, 1, -1, -1, 3, 7, 1, 1,
                                  1, -1, -1, 26, 8, -1, -1, -1, 1, -1, 17, 9,
                                  1, -1, -1, 1, -1, 13, 10, -1, 1, -1, 1, -1,
                                  7, 11, 1, 1, -1, 1, -1, 22, 12, -1, -1, 1, 1,
                                  -1, 23, 13, 1, -1, 1, 1, -1, 8, 14, -1, 1, 1,
                                  1, -1, 18, 15, 1, 1, 1, 1, -1, 21, 16, -1,
                                  -1, -1, -1, 1, 32, 17, 1, -1, -1, -1, 1, 9,
                                  18, -1, 1, -1, -1, 1, 10, 19, 1, 1, -1, -1,
                                  1, 15, 20, -1, -1, 1, -1, 1, 12, 21, 1, -1,
                                  1, -1, 1, 28, 22, -1, 1, 1, -1, 1, 6, 23, 1,
                                  1, 1, -1, 1, 24, 24, -1, -1, -1, 1, 1, 1, 25,
                                  1, -1, -1, 1, 1, 31, 26, -1, 1, -1, 1, 1, 20,
                                  27, 1, 1, -1, 1, 1, 25, 28, -1, -1, 1, 1, 1,
                                  11, 29, 1, -1, 1, 1, 1, 16, 30, -1, 1, 1, 1,
                                  1, 29, 31, 1, 1, 1, 1, 1, 14, 32))
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
results <- runAnalysis("doeFactorial", "debug.csv", options)

test_that("3.1 Five Factor Two Level Frac. Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, -1, -1, 1, 1, 1, 1, 1, -1, -1, -1, -1, 4,
                                  2, -1, 1, -1, -1, 1, 6, 3, 1, 1, -1, 1, -1,
                                  2, 4, -1, -1, 1, 1, -1, 8, 5, 1, -1, 1, -1,
                                  1, 5, 6, -1, 1, 1, -1, -1, 7, 7, 1, 1, 1, 1,
                                  1, 3, 8))
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
    values = c("A", "B", "C", "D", "E", "F", "G", "H", "J")
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
results <- runAnalysis("doeFactorial", "debug.csv", options)

test_that("4.1 Nine Factor Two Level Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, -1, -1, -1, -1, -1, -1, -1, -1, 4, 1, 1,
                                  -1, -1, -1, -1, -1, -1, 1, 1, 55, 2, -1, 1,
                                  -1, -1, -1, -1, -1, 1, 1, 106, 3, 1, 1, -1,
                                  -1, -1, -1, -1, -1, -1, 122, 4, -1, -1, 1,
                                  -1, -1, -1, -1, 1, 1, 112, 5, 1, -1, 1, -1,
                                  -1, -1, -1, -1, -1, 51, 6, -1, 1, 1, -1, -1,
                                  -1, -1, -1, -1, 19, 7, 1, 1, 1, -1, -1, -1,
                                  -1, 1, 1, 103, 8, -1, -1, -1, 1, -1, -1, -1,
                                  1, -1, 78, 9, 1, -1, -1, 1, -1, -1, -1, -1,
                                  1, 90, 10, -1, 1, -1, 1, -1, -1, -1, -1, 1,
                                  107, 11, 1, 1, -1, 1, -1, -1, -1, 1, -1, 96,
                                  12, -1, -1, 1, 1, -1, -1, -1, -1, 1, 60, 13,
                                  1, -1, 1, 1, -1, -1, -1, 1, -1, 8, 14, -1, 1,
                                  1, 1, -1, -1, -1, 1, -1, 93, 15, 1, 1, 1, 1,
                                  -1, -1, -1, -1, 1, 118, 16, -1, -1, -1, -1,
                                  1, -1, -1, 1, -1, 77, 17, 1, -1, -1, -1, 1,
                                  -1, -1, -1, 1, 57, 18, -1, 1, -1, -1, 1, -1,
                                  -1, -1, 1, 81, 19, 1, 1, -1, -1, 1, -1, -1,
                                  1, -1, 39, 20, -1, -1, 1, -1, 1, -1, -1, -1,
                                  1, 14, 21, 1, -1, 1, -1, 1, -1, -1, 1, -1,
                                  58, 22, -1, 1, 1, -1, 1, -1, -1, 1, -1, 63,
                                  23, 1, 1, 1, -1, 1, -1, -1, -1, 1, 52, 24,
                                  -1, -1, -1, 1, 1, -1, -1, -1, -1, 46, 25, 1,
                                  -1, -1, 1, 1, -1, -1, 1, 1, 82, 26, -1, 1,
                                  -1, 1, 1, -1, -1, 1, 1, 120, 27, 1, 1, -1, 1,
                                  1, -1, -1, -1, -1, 40, 28, -1, -1, 1, 1, 1,
                                  -1, -1, 1, 1, 64, 29, 1, -1, 1, 1, 1, -1, -1,
                                  -1, -1, 84, 30, -1, 1, 1, 1, 1, -1, -1, -1,
                                  -1, 75, 31, 1, 1, 1, 1, 1, -1, -1, 1, 1, 53,
                                  32, -1, -1, -1, -1, -1, 1, -1, -1, 1, 32, 33,
                                  1, -1, -1, -1, -1, 1, -1, 1, -1, 5, 34, -1,
                                  1, -1, -1, -1, 1, -1, 1, -1, 34, 35, 1, 1,
                                  -1, -1, -1, 1, -1, -1, 1, 123, 36, -1, -1, 1,
                                  -1, -1, 1, -1, 1, -1, 23, 37, 1, -1, 1, -1,
                                  -1, 1, -1, -1, 1, 38, 38, -1, 1, 1, -1, -1,
                                  1, -1, -1, 1, 3, 39, 1, 1, 1, -1, -1, 1, -1,
                                  1, -1, 44, 40, -1, -1, -1, 1, -1, 1, -1, 1,
                                  1, 94, 41, 1, -1, -1, 1, -1, 1, -1, -1, -1,
                                  37, 42, -1, 1, -1, 1, -1, 1, -1, -1, -1, 7,
                                  43, 1, 1, -1, 1, -1, 1, -1, 1, 1, 30, 44, -1,
                                  -1, 1, 1, -1, 1, -1, -1, -1, 56, 45, 1, -1,
                                  1, 1, -1, 1, -1, 1, 1, 119, 46, -1, 1, 1, 1,
                                  -1, 1, -1, 1, 1, 92, 47, 1, 1, 1, 1, -1, 1,
                                  -1, -1, -1, 62, 48, -1, -1, -1, -1, 1, 1, -1,
                                  1, 1, 116, 49, 1, -1, -1, -1, 1, 1, -1, -1,
                                  -1, 87, 50, -1, 1, -1, -1, 1, 1, -1, -1, -1,
                                  11, 51, 1, 1, -1, -1, 1, 1, -1, 1, 1, 128,
                                  52, -1, -1, 1, -1, 1, 1, -1, -1, -1, 104, 53,
                                  1, -1, 1, -1, 1, 1, -1, 1, 1, 16, 54, -1, 1,
                                  1, -1, 1, 1, -1, 1, 1, 100, 55, 1, 1, 1, -1,
                                  1, 1, -1, -1, -1, 117, 56, -1, -1, -1, 1, 1,
                                  1, -1, -1, 1, 124, 57, 1, -1, -1, 1, 1, 1,
                                  -1, 1, -1, 105, 58, -1, 1, -1, 1, 1, 1, -1,
                                  1, -1, 10, 59, 1, 1, -1, 1, 1, 1, -1, -1, 1,
                                  66, 60, -1, -1, 1, 1, 1, 1, -1, 1, -1, 88,
                                  61, 1, -1, 1, 1, 1, 1, -1, -1, 1, 102, 62,
                                  -1, 1, 1, 1, 1, 1, -1, -1, 1, 101, 63, 1, 1,
                                  1, 1, 1, 1, -1, 1, -1, 113, 64, -1, -1, -1,
                                  -1, -1, -1, 1, -1, 1, 59, 65, 1, -1, -1, -1,
                                  -1, -1, 1, 1, -1, 80, 66, -1, 1, -1, -1, -1,
                                  -1, 1, 1, -1, 71, 67, 1, 1, -1, -1, -1, -1,
                                  1, -1, 1, 2, 68, -1, -1, 1, -1, -1, -1, 1, 1,
                                  -1, 65, 69, 1, -1, 1, -1, -1, -1, 1, -1, 1,
                                  35, 70, -1, 1, 1, -1, -1, -1, 1, -1, 1, 67,
                                  71, 1, 1, 1, -1, -1, -1, 1, 1, -1, 121, 72,
                                  -1, -1, -1, 1, -1, -1, 1, 1, 1, 20, 73, 1,
                                  -1, -1, 1, -1, -1, 1, -1, -1, 18, 74, -1, 1,
                                  -1, 1, -1, -1, 1, -1, -1, 109, 75, 1, 1, -1,
                                  1, -1, -1, 1, 1, 1, 86, 76, -1, -1, 1, 1, -1,
                                  -1, 1, -1, -1, 95, 77, 1, -1, 1, 1, -1, -1,
                                  1, 1, 1, 110, 78, -1, 1, 1, 1, -1, -1, 1, 1,
                                  1, 21, 79, 1, 1, 1, 1, -1, -1, 1, -1, -1,
                                  111, 80, -1, -1, -1, -1, 1, -1, 1, 1, 1, 79,
                                  81, 1, -1, -1, -1, 1, -1, 1, -1, -1, 9, 82,
                                  -1, 1, -1, -1, 1, -1, 1, -1, -1, 108, 83, 1,
                                  1, -1, -1, 1, -1, 1, 1, 1, 33, 84, -1, -1, 1,
                                  -1, 1, -1, 1, -1, -1, 13, 85, 1, -1, 1, -1,
                                  1, -1, 1, 1, 1, 47, 86, -1, 1, 1, -1, 1, -1,
                                  1, 1, 1, 6, 87, 1, 1, 1, -1, 1, -1, 1, -1,
                                  -1, 114, 88, -1, -1, -1, 1, 1, -1, 1, -1, 1,
                                  25, 89, 1, -1, -1, 1, 1, -1, 1, 1, -1, 41,
                                  90, -1, 1, -1, 1, 1, -1, 1, 1, -1, 76, 91, 1,
                                  1, -1, 1, 1, -1, 1, -1, 1, 50, 92, -1, -1, 1,
                                  1, 1, -1, 1, 1, -1, 126, 93, 1, -1, 1, 1, 1,
                                  -1, 1, -1, 1, 43, 94, -1, 1, 1, 1, 1, -1, 1,
                                  -1, 1, 115, 95, 1, 1, 1, 1, 1, -1, 1, 1, -1,
                                  70, 96, -1, -1, -1, -1, -1, 1, 1, -1, -1, 12,
                                  97, 1, -1, -1, -1, -1, 1, 1, 1, 1, 85, 98,
                                  -1, 1, -1, -1, -1, 1, 1, 1, 1, 42, 99, 1, 1,
                                  -1, -1, -1, 1, 1, -1, -1, 69, 100, -1, -1, 1,
                                  -1, -1, 1, 1, 1, 1, 26, 101, 1, -1, 1, -1,
                                  -1, 1, 1, -1, -1, 127, 102, -1, 1, 1, -1, -1,
                                  1, 1, -1, -1, 89, 103, 1, 1, 1, -1, -1, 1, 1,
                                  1, 1, 29, 104, -1, -1, -1, 1, -1, 1, 1, 1,
                                  -1, 24, 105, 1, -1, -1, 1, -1, 1, 1, -1, 1,
                                  15, 106, -1, 1, -1, 1, -1, 1, 1, -1, 1, 61,
                                  107, 1, 1, -1, 1, -1, 1, 1, 1, -1, 31, 108,
                                  -1, -1, 1, 1, -1, 1, 1, -1, 1, 73, 109, 1,
                                  -1, 1, 1, -1, 1, 1, 1, -1, 17, 110, -1, 1, 1,
                                  1, -1, 1, 1, 1, -1, 36, 111, 1, 1, 1, 1, -1,
                                  1, 1, -1, 1, 98, 112, -1, -1, -1, -1, 1, 1,
                                  1, 1, -1, 97, 113, 1, -1, -1, -1, 1, 1, 1,
                                  -1, 1, 27, 114, -1, 1, -1, -1, 1, 1, 1, -1,
                                  1, 74, 115, 1, 1, -1, -1, 1, 1, 1, 1, -1, 22,
                                  116, -1, -1, 1, -1, 1, 1, 1, -1, 1, 72, 117,
                                  1, -1, 1, -1, 1, 1, 1, 1, -1, 49, 118, -1, 1,
                                  1, -1, 1, 1, 1, 1, -1, 125, 119, 1, 1, 1, -1,
                                  1, 1, 1, -1, 1, 68, 120, -1, -1, -1, 1, 1, 1,
                                  1, -1, -1, 1, 121, 1, -1, -1, 1, 1, 1, 1, 1,
                                  1, 83, 122, -1, 1, -1, 1, 1, 1, 1, 1, 1, 45,
                                  123, 1, 1, -1, 1, 1, 1, 1, -1, -1, 28, 124,
                                  -1, -1, 1, 1, 1, 1, 1, 1, 1, 91, 125, 1, -1,
                                  1, 1, 1, 1, 1, -1, -1, 48, 126, -1, 1, 1, 1,
                                  1, 1, 1, -1, -1, 99, 127, 1, 1, 1, 1, 1, 1,
                                  1, 1, 1, 54, 128))
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
    values = c("A", "B", "C", "D", "E", "F", "G", "H", "J")
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
results <- runAnalysis("doeFactorial", "debug.csv", options)


test_that("5.1 Nine Factor Two Level Frac. Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, -1, -1, -1, 1, 1, 1, -1, 1, 4, 1, 1, -1,
                                  -1, -1, -1, -1, -1, -1, -1, 5, 2, -1, 1, -1,
                                  -1, -1, 1, 1, 1, -1, 8, 3, 1, 1, -1, -1, 1,
                                  -1, -1, 1, 1, 2, 4, -1, -1, 1, -1, 1, -1, 1,
                                  1, -1, 10, 5, 1, -1, 1, -1, -1, 1, -1, 1, 1,
                                  13, 6, -1, 1, 1, -1, -1, -1, 1, -1, 1, 3, 7,
                                  1, 1, 1, -1, 1, 1, -1, -1, -1, 16, 8, -1, -1,
                                  -1, 1, 1, 1, -1, 1, -1, 1, 9, 1, -1, -1, 1,
                                  -1, -1, 1, 1, 1, 12, 10, -1, 1, -1, 1, -1, 1,
                                  -1, -1, 1, 11, 11, 1, 1, -1, 1, 1, -1, 1, -1,
                                  -1, 7, 12, -1, -1, 1, 1, 1, -1, -1, -1, 1, 9,
                                  13, 1, -1, 1, 1, -1, 1, 1, -1, -1, 6, 14, -1,
                                  1, 1, 1, -1, -1, -1, 1, -1, 14, 15, 1, 1, 1,
                                  1, 1, 1, 1, 1, 1, 15, 16))
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
results <- runAnalysis("doeFactorial", "debug.csv", options)

test_that("6.1 One Center Point Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0, 9, 0, -1, -1, -1, 1, 1, 1, -1, -1,
                                  4, 2, -1, 1, -1, 6, 3, 1, 1, -1, 2, 4, -1,
                                  -1, 1, 8, 5, 1, -1, 1, 5, 6, -1, 1, 1, 7, 7,
                                  1, 1, 1, 3, 8))
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
results <- runAnalysis("doeFactorial", "debug.csv", options)


test_that("7.1 Two Center Point Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0, 1, 1, 0, 0, 0, 10, 1, -1, -1, -1, 2,
                                  2, 1, -1, -1, 5, 3, -1, 1, -1, 7, 4, 1, 1,
                                  -1, 3, 5, -1, -1, 1, 9, 6, 1, -1, 1, 6, 7,
                                  -1, 1, 1, 8, 8, 1, 1, 1, 4, 9))
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
results <- runAnalysis("doeFactorial", "debug.csv", options)


test_that("8.1 Four Center Point Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0, 1, 1, 0, 0, 0, 2, 1, 0, 0, 0, 7, 1,
                                  0, 0, 0, 12, 1, -1, -1, -1, 3, 2, 1, -1, -1,
                                  6, 3, -1, 1, -1, 9, 4, 1, 1, -1, 4, 5, -1,
                                  -1, 1, 11, 6, 1, -1, 1, 8, 7, -1, 1, 1, 10,
                                  8, 1, 1, 1, 5, 9))
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
results <- runAnalysis("doeFactorial", "debug.csv", options)

test_that("9.1 Two Blocks Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, -1, 1, -1, 1, 1, -1, -1, 2, 1, 5, 2, -1,
                                  1, 2, -1, 8, 3, -1, 1, 1, 1, 4, 4, 1, -1, 2,
                                  -1, 6, 5, 1, -1, 1, 1, 2, 6, 1, 1, 1, -1, 3,
                                  7, 1, 1, 2, 1, 7, 8))
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
results <- runAnalysis("doeFactorial", "debug.csv", options)

test_that("10.1 Four Blocks Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, -1, 1, -1, 7, 1, -1, -1, 2, 1, 6, 2, -1,
                                  1, 3, -1, 3, 3, -1, 1, 4, 1, 1, 4, 1, -1, 4,
                                  -1, 2, 5, 1, -1, 3, 1, 4, 6, 1, 1, 2, -1, 5,
                                  7, 1, 1, 1, 1, 8, 8))
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
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 1", values = c("A", "B", "C", "D")),
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 2", values = c("a", "a", "a", "a")),
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 3", values = c("b", "b", "b", "b")))
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
results <- runAnalysis("doeFactorial", "debug.csv", options)

test_that("11.1 Eight Blocks Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, -1, 1, -1, -1, 15, 1, -1, -1, 2, -1, 1,
                                  13, 2, -1, -1, 3, 1, -1, 11, 3, -1, -1, 4, 1,
                                  1, 10, 4, -1, 1, 5, -1, -1, 7, 5, -1, 1, 6,
                                  -1, 1, 6, 6, -1, 1, 7, 1, -1, 3, 7, -1, 1, 8,
                                  1, 1, 1, 8, 1, -1, 8, -1, -1, 2, 9, 1, -1, 7,
                                  -1, 1, 4, 10, 1, -1, 6, 1, -1, 5, 11, 1, -1,
                                  5, 1, 1, 8, 12, 1, 1, 4, -1, -1, 9, 13, 1, 1,
                                  3, -1, 1, 12, 14, 1, 1, 2, 1, -1, 14, 15, 1,
                                  1, 1, 1, 1, 16, 16))
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
results <- runAnalysis("doeFactorial", "debug.csv", options)

test_that("12.1 Two Factor One HTC Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, 1, 1, 1, -1, -1, 2, 2, 1, -1, 3, 3, 1, 1,
                                  4, 4))
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
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 1", values = c("A", "B", "C", "D", "E")),
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 2", values = c("a", "a", "a", "a", "a")),
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 3", values = c("b", "b", "b", "b", "b")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "factorialTypeSplit"
options$runOrder <- "runOrderStandard"
options$selectedDesign2 <- list(list(levels = c("Row 0", "Row 1"), name = "data 1", values = c(16,32)), # give design list, always two lists, number of values gives number of designs
                                list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(0, 0)))
options$selectedRow <- 1 # select design
options$setSeed <- TRUE
set.seed(1)
results <- runAnalysis("doeFactorial", "debug.csv", options)

test_that("13.1 Five Factor Two HTC Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, 1, 1, 1, 1, 1, 1, -1, 1, 1, 1, -1, 4, 2,
                                  -1, 1, 1, -1, -1, 6, 3, -1, 1, 1, -1, 1, 2,
                                  4, -1, 1, -1, -1, 1, 8, 5, -1, 1, -1, -1, -1,
                                  5, 6, -1, 1, -1, 1, -1, 7, 7, -1, 1, -1, 1,
                                  1, 3, 8, -1, -1, -1, 1, 1, 12, 9, -1, -1, -1,
                                  1, -1, 9, 10, -1, -1, -1, -1, -1, 10, 11, -1,
                                  -1, -1, -1, 1, 14, 12, -1, -1, 1, -1, 1, 13,
                                  13, -1, -1, 1, -1, -1, 16, 14, -1, -1, 1, 1,
                                  -1, 11, 15, -1, -1, 1, 1, 1, 15, 16, 1, -1,
                                  1, 1, 1, 19, 17, 1, -1, 1, 1, -1, 18, 18, 1,
                                  -1, 1, -1, -1, 23, 19, 1, -1, 1, -1, 1, 22,
                                  20, 1, -1, -1, -1, 1, 20, 21, 1, -1, -1, -1,
                                  -1, 17, 22, 1, -1, -1, 1, -1, 24, 23, 1, -1,
                                  -1, 1, 1, 21, 24, 1, 1, -1, 1, 1, 30, 25, 1,
                                  1, -1, 1, -1, 27, 26, 1, 1, -1, -1, -1, 32,
                                  27, 1, 1, -1, -1, 1, 31, 28, 1, 1, 1, -1, 1,
                                  25, 29, 1, 1, 1, -1, -1, 28, 30, 1, 1, 1, 1,
                                  -1, 29, 31, 1, 1, 1, 1, 1, 26, 32))
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
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6"), name = "data 1", values = c("A", "B", "C", "D", "E", "F", "G")),
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6"), name = "data 2", values = c("a", "a", "a", "a", "a", "a", "a")),
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6"), name = "data 3", values = c("b", "b", "b", "b", "b", "b", "b")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "factorialTypeSplit"
options$runOrder <- "runOrderStandard"
options$selectedDesign2 <- list(list(levels = c("Row 0", "Row 1"), name = "data 1", values = c(64, 128)), # give design list, always two lists, number of values gives number of designs
                                list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(0, 0)))
options$selectedRow <- 1 # select design
options$setSeed <- TRUE
set.seed(1)
results <- runAnalysis("doeFactorial", "debug.csv", options)

test_that("14.1 Seven Factor Three HTC Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, 1, 1, 1, 1, 1, 1, 4, 1, -1, 1, 1, 1, 1,
                                  1, -1, 5, 2, -1, 1, 1, 1, 1, -1, -1, 8, 3,
                                  -1, 1, 1, 1, 1, -1, 1, 2, 4, -1, 1, 1, 1, -1,
                                  -1, 1, 10, 5, -1, 1, 1, 1, -1, -1, -1, 13, 6,
                                  -1, 1, 1, 1, -1, 1, -1, 3, 7, -1, 1, 1, 1,
                                  -1, 1, 1, 16, 8, -1, 1, 1, -1, -1, 1, 1, 1,
                                  9, -1, 1, 1, -1, -1, 1, -1, 12, 10, -1, 1, 1,
                                  -1, -1, -1, -1, 11, 11, -1, 1, 1, -1, -1, -1,
                                  1, 7, 12, -1, 1, 1, -1, 1, -1, 1, 9, 13, -1,
                                  1, 1, -1, 1, -1, -1, 6, 14, -1, 1, 1, -1, 1,
                                  1, -1, 14, 15, -1, 1, 1, -1, 1, 1, 1, 15, 16,
                                  -1, 1, -1, -1, 1, 1, 1, 28, 17, -1, 1, -1,
                                  -1, 1, 1, -1, 25, 18, -1, 1, -1, -1, 1, -1,
                                  -1, 30, 19, -1, 1, -1, -1, 1, -1, 1, 29, 20,
                                  -1, 1, -1, -1, -1, -1, 1, 19, 21, -1, 1, -1,
                                  -1, -1, -1, -1, 27, 22, -1, 1, -1, -1, -1, 1,
                                  -1, 32, 23, -1, 1, -1, -1, -1, 1, 1, 26, 24,
                                  -1, 1, -1, 1, -1, 1, 1, 17, 25, -1, 1, -1, 1,
                                  -1, 1, -1, 24, 26, -1, 1, -1, 1, -1, -1, -1,
                                  31, 27, -1, 1, -1, 1, -1, -1, 1, 22, 28, -1,
                                  1, -1, 1, 1, -1, 1, 23, 29, -1, 1, -1, 1, 1,
                                  -1, -1, 20, 30, -1, 1, -1, 1, 1, 1, -1, 18,
                                  31, -1, 1, -1, 1, 1, 1, 1, 21, 32, -1, -1,
                                  -1, 1, 1, 1, 1, 43, 33, -1, -1, -1, 1, 1, 1,
                                  -1, 47, 34, -1, -1, -1, 1, 1, -1, -1, 33, 35,
                                  -1, -1, -1, 1, 1, -1, 1, 38, 36, -1, -1, -1,
                                  1, -1, -1, 1, 48, 37, -1, -1, -1, 1, -1, -1,
                                  -1, 34, 38, -1, -1, -1, 1, -1, 1, -1, 41, 39,
                                  -1, -1, -1, 1, -1, 1, 1, 45, 40, -1, -1, -1,
                                  -1, -1, 1, 1, 40, 41, -1, -1, -1, -1, -1, 1,
                                  -1, 35, 42, -1, -1, -1, -1, -1, -1, -1, 39,
                                  43, -1, -1, -1, -1, -1, -1, 1, 42, 44, -1,
                                  -1, -1, -1, 1, -1, 1, 44, 45, -1, -1, -1, -1,
                                  1, -1, -1, 36, 46, -1, -1, -1, -1, 1, 1, -1,
                                  37, 47, -1, -1, -1, -1, 1, 1, 1, 46, 48, -1,
                                  -1, 1, -1, 1, 1, 1, 60, 49, -1, -1, 1, -1, 1,
                                  1, -1, 55, 50, -1, -1, 1, -1, 1, -1, -1, 51,
                                  51, -1, -1, 1, -1, 1, -1, 1, 62, 52, -1, -1,
                                  1, -1, -1, -1, 1, 64, 53, -1, -1, 1, -1, -1,
                                  -1, -1, 53, 54, -1, -1, 1, -1, -1, 1, -1, 50,
                                  55, -1, -1, 1, -1, -1, 1, 1, 54, 56, -1, -1,
                                  1, 1, -1, 1, 1, 63, 57, -1, -1, 1, 1, -1, 1,
                                  -1, 49, 58, -1, -1, 1, 1, -1, -1, -1, 58, 59,
                                  -1, -1, 1, 1, -1, -1, 1, 57, 60, -1, -1, 1,
                                  1, 1, -1, 1, 56, 61, -1, -1, 1, 1, 1, -1, -1,
                                  61, 62, -1, -1, 1, 1, 1, 1, -1, 59, 63, -1,
                                  -1, 1, 1, 1, 1, 1, 52, 64, 1, -1, 1, 1, 1, 1,
                                  1, 71, 65, 1, -1, 1, 1, 1, 1, -1, 80, 66, 1,
                                  -1, 1, 1, 1, -1, -1, 77, 67, 1, -1, 1, 1, 1,
                                  -1, 1, 72, 68, 1, -1, 1, 1, -1, -1, 1, 73,
                                  69, 1, -1, 1, 1, -1, -1, -1, 68, 70, 1, -1,
                                  1, 1, -1, 1, -1, 65, 71, 1, -1, 1, 1, -1, 1,
                                  1, 69, 72, 1, -1, 1, -1, -1, 1, 1, 76, 73, 1,
                                  -1, 1, -1, -1, 1, -1, 74, 74, 1, -1, 1, -1,
                                  -1, -1, -1, 75, 75, 1, -1, 1, -1, -1, -1, 1,
                                  67, 76, 1, -1, 1, -1, 1, -1, 1, 78, 77, 1,
                                  -1, 1, -1, 1, -1, -1, 79, 78, 1, -1, 1, -1,
                                  1, 1, -1, 66, 79, 1, -1, 1, -1, 1, 1, 1, 70,
                                  80, 1, -1, -1, -1, 1, 1, 1, 85, 81, 1, -1,
                                  -1, -1, 1, 1, -1, 92, 82, 1, -1, -1, -1, 1,
                                  -1, -1, 91, 83, 1, -1, -1, -1, 1, -1, 1, 95,
                                  84, 1, -1, -1, -1, -1, -1, 1, 86, 85, 1, -1,
                                  -1, -1, -1, -1, -1, 81, 86, 1, -1, -1, -1,
                                  -1, 1, -1, 90, 87, 1, -1, -1, -1, -1, 1, 1,
                                  96, 88, 1, -1, -1, 1, -1, 1, 1, 89, 89, 1,
                                  -1, -1, 1, -1, 1, -1, 94, 90, 1, -1, -1, 1,
                                  -1, -1, -1, 93, 91, 1, -1, -1, 1, -1, -1, 1,
                                  83, 92, 1, -1, -1, 1, 1, -1, 1, 82, 93, 1,
                                  -1, -1, 1, 1, -1, -1, 84, 94, 1, -1, -1, 1,
                                  1, 1, -1, 88, 95, 1, -1, -1, 1, 1, 1, 1, 87,
                                  96, 1, 1, -1, 1, 1, 1, 1, 104, 97, 1, 1, -1,
                                  1, 1, 1, -1, 102, 98, 1, 1, -1, 1, 1, -1, -1,
                                  101, 99, 1, 1, -1, 1, 1, -1, 1, 112, 100, 1,
                                  1, -1, 1, -1, -1, 1, 110, 101, 1, 1, -1, 1,
                                  -1, -1, -1, 108, 102, 1, 1, -1, 1, -1, 1, -1,
                                  99, 103, 1, 1, -1, 1, -1, 1, 1, 106, 104, 1,
                                  1, -1, -1, -1, 1, 1, 109, 105, 1, 1, -1, -1,
                                  -1, 1, -1, 97, 106, 1, 1, -1, -1, -1, -1, -1,
                                  107, 107, 1, 1, -1, -1, -1, -1, 1, 100, 108,
                                  1, 1, -1, -1, 1, -1, 1, 105, 109, 1, 1, -1,
                                  -1, 1, -1, -1, 111, 110, 1, 1, -1, -1, 1, 1,
                                  -1, 98, 111, 1, 1, -1, -1, 1, 1, 1, 103, 112,
                                  1, 1, 1, -1, 1, 1, 1, 120, 113, 1, 1, 1, -1,
                                  1, 1, -1, 122, 114, 1, 1, 1, -1, 1, -1, -1,
                                  121, 115, 1, 1, 1, -1, 1, -1, 1, 126, 116, 1,
                                  1, 1, -1, -1, -1, 1, 117, 117, 1, 1, 1, -1,
                                  -1, -1, -1, 118, 118, 1, 1, 1, -1, -1, 1, -1,
                                  115, 119, 1, 1, 1, -1, -1, 1, 1, 116, 120, 1,
                                  1, 1, 1, -1, 1, 1, 125, 121, 1, 1, 1, 1, -1,
                                  1, -1, 123, 122, 1, 1, 1, 1, -1, -1, -1, 124,
                                  123, 1, 1, 1, 1, -1, -1, 1, 113, 124, 1, 1,
                                  1, 1, 1, -1, 1, 119, 125, 1, 1, 1, 1, 1, -1,
                                  -1, 127, 126, 1, 1, 1, 1, 1, 1, -1, 114, 127,
                                  1, 1, 1, 1, 1, 1, 1, 128, 128))
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
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 1", values = c("A", "B", "C")),
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 2", values = c("a", "a", "a")),
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 3", values = c("b", "b", "b")),
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 4", values = c("c", "c", "c")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "generalFullFactorial"
options$runOrder <- "runOrderStandard"
options$setSeed <- TRUE
set.seed(1)
results <- runAnalysis("doeFactorial", "debug.csv", options)

test_that("15.1 Three Factor Three Level General Full Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1, 1, 4, 1, 2, 1, 1, 5, 2, 3, 1, 1, 26, 3,
                                  1, 2, 1, 2, 4, 2, 2, 1, 17, 5, 3, 2, 1, 12,
                                  6, 1, 3, 1, 3, 7, 2, 3, 1, 27, 8, 3, 3, 1,
                                  16, 9, 1, 1, 2, 11, 10, 2, 1, 2, 6, 11, 3, 1,
                                  2, 19, 12, 1, 2, 2, 23, 13, 2, 2, 2, 7, 14,
                                  3, 2, 2, 22, 15, 1, 3, 2, 21, 16, 2, 3, 2,
                                  14, 17, 3, 3, 2, 8, 18, 1, 1, 3, 9, 19, 2, 1,
                                  3, 20, 20, 3, 1, 3, 13, 21, 1, 2, 3, 18, 22,
                                  2, 2, 3, 24, 23, 3, 2, 3, 10, 24, 1, 3, 3, 1,
                                  25, 2, 3, 3, 25, 26, 3, 3, 3, 15, 27))
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
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 1", values = c("A", "B", "C")),
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 2", values = c("a", "a", "a")),
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 3", values = c("b", "b", "b")),
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 4", values = c("c", "c", "c")),
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 5", values = c("d", "", "")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "generalFullFactorial"
options$runOrder <- "runOrderStandard"
options$setSeed <- TRUE
set.seed(1)
results <- runAnalysis("doeFactorial", "debug.csv", options)

test_that("16.1 Three Factor Four Level General Full Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1, 1, 2, 1, 2, 1, 1, 26, 2, 3, 1, 1, 33, 3, 4, 1, 1, 1, 4, 1, 2,
                                      1, 20, 5, 2, 2, 1, 36, 6, 3, 2, 1, 17, 7, 4, 2, 1, 32, 8, 1, 3, 1,
                                      18, 9, 2, 3, 1, 13, 10, 3, 3, 1, 5, 11, 4, 3, 1, 35, 12, 1, 1, 2,
                                      25, 13, 2, 1, 2, 6, 14, 3, 1, 2, 19, 15, 4, 1, 2, 22, 16, 1, 2, 2,
                                      24, 17, 2, 2, 2, 7, 18, 3, 2, 2, 9, 19, 4, 2, 2, 30, 20, 1, 3, 2,
                                      11, 21, 2, 3, 2, 14, 22, 3, 3, 2, 4, 23, 4, 3, 2, 16, 24, 1, 1, 3,
                                      27, 25, 2, 1, 3, 12, 26, 3, 1, 3, 8, 27, 4, 1, 3, 21, 28, 1, 2, 3,
                                      31, 29, 2, 2, 3, 28, 30, 3, 2, 3, 15, 31, 4, 2, 3, 34, 32, 1, 3, 3,
                                      23, 33, 2, 3, 3, 3, 34, 3, 3, 3, 10, 35, 4, 3, 3, 29, 36))
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
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 1", values = c("A", "B", "C", "D")),
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 2", values = c("a", "a", "a", "a")),
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 3", values = c("b", "b", "b", "b")),
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 4", values = c("c", "c", "c", "")),
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 5", values = c("d", "d", "d", "")),
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 6", values = c("e", "e", "", "")),
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 7", values = c("f", "", "", "")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "generalFullFactorial"
options$runOrder <- "runOrderStandard"
options$setSeed <- TRUE
set.seed(1)
results <- runAnalysis("doeFactorial", "debug.csv", options)

test_that("17.1 Four Factor Six Level General Full Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1, 1, 1, 115, 1, 2, 1, 1, 1, 149, 2, 3, 1, 1, 1, 179, 3, 4, 1,
                                      1, 1, 209, 4, 5, 1, 1, 1, 185, 5, 6, 1, 1, 1, 226, 6, 1, 2, 1, 1,
                                      17, 7, 2, 2, 1, 1, 190, 8, 3, 2, 1, 1, 166, 9, 4, 2, 1, 1, 169, 10,
                                      5, 2, 1, 1, 231, 11, 6, 2, 1, 1, 173, 12, 1, 3, 1, 1, 71, 13, 2, 3,
                                      1, 1, 7, 14, 3, 3, 1, 1, 176, 15, 4, 3, 1, 1, 147, 16, 5, 3, 1, 1,
                                      156, 17, 6, 3, 1, 1, 225, 18, 1, 4, 1, 1, 155, 19, 2, 4, 1, 1, 42,
                                      20, 3, 4, 1, 1, 13, 21, 4, 4, 1, 1, 63, 22, 5, 4, 1, 1, 74, 23, 6,
                                      4, 1, 1, 57, 24, 1, 5, 1, 1, 50, 25, 2, 5, 1, 1, 105, 26, 3, 5, 1,
                                      1, 187, 27, 4, 5, 1, 1, 120, 28, 5, 5, 1, 1, 78, 29, 6, 5, 1, 1,
                                      204, 30, 1, 1, 2, 1, 87, 31, 2, 1, 2, 1, 170, 32, 3, 1, 2, 1, 32,
                                      33, 4, 1, 2, 1, 26, 34, 5, 1, 2, 1, 113, 35, 6, 1, 2, 1, 191, 36,
                                      1, 2, 2, 1, 21, 37, 2, 2, 2, 1, 182, 38, 3, 2, 2, 1, 53, 39, 4, 2,
                                      2, 1, 48, 40, 5, 2, 2, 1, 186, 41, 6, 2, 2, 1, 37, 42, 1, 3, 2, 1,
                                      6, 43, 2, 3, 2, 1, 43, 44, 3, 3, 2, 1, 61, 45, 4, 3, 2, 1, 125, 46,
                                      5, 3, 2, 1, 205, 47, 6, 3, 2, 1, 83, 48, 1, 4, 2, 1, 135, 49, 2, 4,
                                      2, 1, 140, 50, 3, 4, 2, 1, 10, 51, 4, 4, 2, 1, 229, 52, 5, 4, 2, 1,
                                      146, 53, 6, 4, 2, 1, 235, 54, 1, 5, 2, 1, 193, 55, 2, 5, 2, 1, 138,
                                      56, 3, 5, 2, 1, 197, 57, 4, 5, 2, 1, 219, 58, 5, 5, 2, 1, 202, 59,
                                      6, 5, 2, 1, 99, 60, 1, 1, 3, 1, 124, 61, 2, 1, 3, 1, 230, 62, 3, 1,
                                      3, 1, 196, 63, 4, 1, 3, 1, 97, 64, 5, 1, 3, 1, 151, 65, 6, 1, 3, 1,
                                      154, 66, 1, 2, 3, 1, 127, 67, 2, 2, 3, 1, 1, 68, 3, 2, 3, 1, 214,
                                      69, 4, 2, 3, 1, 35, 70, 5, 2, 3, 1, 130, 71, 6, 2, 3, 1, 220, 72,
                                      1, 3, 3, 1, 18, 73, 2, 3, 3, 1, 16, 74, 3, 3, 3, 1, 70, 75, 4, 3,
                                      3, 1, 228, 76, 5, 3, 3, 1, 153, 77, 6, 3, 3, 1, 117, 78, 1, 4, 3,
                                      1, 19, 79, 2, 4, 3, 1, 212, 80, 3, 4, 3, 1, 240, 81, 4, 4, 3, 1,
                                      189, 82, 5, 4, 3, 1, 92, 83, 6, 4, 3, 1, 33, 84, 1, 5, 3, 1, 12,
                                      85, 2, 5, 3, 1, 128, 86, 3, 5, 3, 1, 45, 87, 4, 5, 3, 1, 175, 88,
                                      5, 5, 3, 1, 29, 89, 6, 5, 3, 1, 94, 90, 1, 1, 4, 1, 141, 91, 2, 1,
                                      4, 1, 177, 92, 3, 1, 4, 1, 163, 93, 4, 1, 4, 1, 164, 94, 5, 1, 4,
                                      1, 217, 95, 6, 1, 4, 1, 207, 96, 1, 2, 4, 1, 168, 97, 2, 2, 4, 1,
                                      184, 98, 3, 2, 4, 1, 129, 99, 4, 2, 4, 1, 215, 100, 5, 2, 4, 1,
                                      167, 101, 6, 2, 4, 1, 88, 102, 1, 3, 4, 1, 69, 103, 2, 3, 4, 1, 65,
                                      104, 3, 3, 4, 1, 22, 105, 4, 3, 4, 1, 14, 106, 5, 3, 4, 1, 96, 107,
                                      6, 3, 4, 1, 82, 108, 1, 4, 4, 1, 216, 109, 2, 4, 4, 1, 24, 110, 3,
                                      4, 4, 1, 39, 111, 4, 4, 4, 1, 132, 112, 5, 4, 4, 1, 126, 113, 6, 4,
                                      4, 1, 208, 114, 1, 5, 4, 1, 64, 115, 2, 5, 4, 1, 121, 116, 3, 5, 4,
                                      1, 213, 117, 4, 5, 4, 1, 93, 118, 5, 5, 4, 1, 51, 119, 6, 5, 4, 1,
                                      161, 120, 1, 1, 1, 2, 44, 121, 2, 1, 1, 2, 52, 122, 3, 1, 1, 2,
                                      201, 123, 4, 1, 1, 2, 181, 124, 5, 1, 1, 2, 194, 125, 6, 1, 1, 2,
                                      28, 126, 1, 2, 1, 2, 199, 127, 2, 2, 1, 2, 160, 128, 3, 2, 1, 2, 3,
                                      129, 4, 2, 1, 2, 60, 130, 5, 2, 1, 2, 165, 131, 6, 2, 1, 2, 144,
                                      132, 1, 3, 1, 2, 171, 133, 2, 3, 1, 2, 56, 134, 3, 3, 1, 2, 116,
                                      135, 4, 3, 1, 2, 210, 136, 5, 3, 1, 2, 114, 137, 6, 3, 1, 2, 102,
                                      138, 1, 4, 1, 2, 224, 139, 2, 4, 1, 2, 157, 140, 3, 4, 1, 2, 79,
                                      141, 4, 4, 1, 2, 137, 142, 5, 4, 1, 2, 180, 143, 6, 4, 1, 2, 206,
                                      144, 1, 5, 1, 2, 89, 145, 2, 5, 1, 2, 62, 146, 3, 5, 1, 2, 112,
                                      147, 4, 5, 1, 2, 40, 148, 5, 5, 1, 2, 86, 149, 6, 5, 1, 2, 80, 150,
                                      1, 1, 2, 2, 200, 151, 2, 1, 2, 2, 152, 152, 3, 1, 2, 2, 192, 153,
                                      4, 1, 2, 2, 159, 154, 5, 1, 2, 2, 111, 155, 6, 1, 2, 2, 41, 156, 1,
                                      2, 2, 2, 227, 157, 2, 2, 2, 2, 95, 158, 3, 2, 2, 2, 145, 159, 4, 2,
                                      2, 2, 58, 160, 5, 2, 2, 2, 203, 161, 6, 2, 2, 2, 4, 162, 1, 3, 2,
                                      2, 34, 163, 2, 3, 2, 2, 98, 164, 3, 3, 2, 2, 25, 165, 4, 3, 2, 2,
                                      38, 166, 5, 3, 2, 2, 2, 167, 6, 3, 2, 2, 188, 168, 1, 4, 2, 2, 110,
                                      169, 2, 4, 2, 2, 239, 170, 3, 4, 2, 2, 158, 171, 4, 4, 2, 2, 30,
                                      172, 5, 4, 2, 2, 222, 173, 6, 4, 2, 2, 119, 174, 1, 5, 2, 2, 67,
                                      175, 2, 5, 2, 2, 68, 176, 3, 5, 2, 2, 162, 177, 4, 5, 2, 2, 118,
                                      178, 5, 5, 2, 2, 54, 179, 6, 5, 2, 2, 85, 180, 1, 1, 3, 2, 148,
                                      181, 2, 1, 3, 2, 15, 182, 3, 1, 3, 2, 233, 183, 4, 1, 3, 2, 109,
                                      184, 5, 1, 3, 2, 150, 185, 6, 1, 3, 2, 198, 186, 1, 2, 3, 2, 9,
                                      187, 2, 2, 3, 2, 108, 188, 3, 2, 3, 2, 232, 189, 4, 2, 3, 2, 136,
                                      190, 5, 2, 3, 2, 234, 191, 6, 2, 3, 2, 77, 192, 1, 3, 3, 2, 72,
                                      193, 2, 3, 3, 2, 236, 194, 3, 3, 3, 2, 66, 195, 4, 3, 3, 2, 91,
                                      196, 5, 3, 3, 2, 47, 197, 6, 3, 3, 2, 134, 198, 1, 4, 3, 2, 143,
                                      199, 2, 4, 3, 2, 81, 200, 3, 4, 3, 2, 172, 201, 4, 4, 3, 2, 218,
                                      202, 5, 4, 3, 2, 131, 203, 6, 4, 3, 2, 55, 204, 1, 5, 3, 2, 238,
                                      205, 2, 5, 3, 2, 46, 206, 3, 5, 3, 2, 31, 207, 4, 5, 3, 2, 76, 208,
                                      5, 5, 3, 2, 84, 209, 6, 5, 3, 2, 8, 210, 1, 1, 4, 2, 49, 211, 2, 1,
                                      4, 2, 73, 212, 3, 1, 4, 2, 20, 213, 4, 1, 4, 2, 139, 214, 5, 1, 4,
                                      2, 5, 215, 6, 1, 4, 2, 211, 216, 1, 2, 4, 2, 23, 217, 2, 2, 4, 2,
                                      75, 218, 3, 2, 4, 2, 122, 219, 4, 2, 4, 2, 123, 220, 5, 2, 4, 2,
                                      178, 221, 6, 2, 4, 2, 223, 222, 1, 3, 4, 2, 90, 223, 2, 3, 4, 2,
                                      142, 224, 3, 3, 4, 2, 11, 225, 4, 3, 4, 2, 174, 226, 5, 3, 4, 2,
                                      27, 227, 6, 3, 4, 2, 195, 228, 1, 4, 4, 2, 183, 229, 2, 4, 4, 2,
                                      36, 230, 3, 4, 4, 2, 100, 231, 4, 4, 4, 2, 107, 232, 5, 4, 4, 2,
                                      106, 233, 6, 4, 4, 2, 59, 234, 1, 5, 4, 2, 104, 235, 2, 5, 4, 2,
                                      101, 236, 3, 5, 4, 2, 133, 237, 4, 5, 4, 2, 103, 238, 5, 5, 4, 2,
                                      221, 239, 6, 5, 4, 2, 237, 240))
})

test_that("17.1 Four Factor Six Level General Full Factorial Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(240, 4, 0, 1, "Value", 1, 240))
})
