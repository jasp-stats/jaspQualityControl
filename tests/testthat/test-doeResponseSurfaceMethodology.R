context("[Quality Control] DoE Response surface methodology")

# Basic tests ####

## Three continuous predictors (verified with Minitab) ####

options <- analysisOptions("doeResponseSurfaceMethodology")
options$actualExporter <- FALSE
options$alphaType <- "default"
options$centerPointType <- "default"
options$codedOutput <- TRUE
options$setSeed <- TRUE
options$seed <- 123
options$designType <- "centralCompositeDesign"
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$numberOfCategorical <- 0
options$numberOfContinuous <- 3
options$runOrder <- "runOrderStandard"
options$continuousVariables <- list(   # values of each list give number of predictors, always three lists
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 1", values = c("A", "B", "C")), # names
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 2", values = c(-1, -1, -1)), # low level
  list(levels = c("Row 0","Row 1", "Row 2"), name = "data 3", values = c(1, 1, 1)) # high level
)
options$selectedDesign2 <- list( # give design list, always five lists, number of values gives number of designs
  list(levels = c("Row 0", "Row 1"), name = "data 1", values = c(20, 20)), # runs
  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(6, 6)), # total
  list(levels = c("Row 0", "Row 1"), name = "data 3", values = c(0, 4)), # cube
  list(levels = c("Row 0", "Row 1"), name = "data 4", values = c(0, 2)), # axial
  list(levels = c("Row 0", "Row 1"), name = "data 5", values = c(1.682, 1.633)) # alpha
)
options$selectedRow <- 1 # select design
dataset <- NULL
results <- runAnalysis("doeResponseSurfaceMethodology", dataset, options, makeTests = T)

test_that("1.1 Three Cont. Pred. Central Composite Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(6, 1, -1, -1, -1, 15, 2, 1, -1, -1, 8, 3, -1, 1, -1, 16, 4, 1,
                                      1, -1, 17, 5, -1, -1, 1, 1, 6, 1, -1, 1, 18, 7, -1, 1, 1, 12,
                                      8, 1, 1, 1, 7, 9, 0, 0, 0, 20, 10, 0, 0, 0, 10, 11, 0, 0, 0,
                                      5, 12, 0, 0, 0, 11, 13, -1.63299316185545, 0, 0, 9, 14, 1.63299316185545,
                                      0, 0, 19, 15, 0, -1.63299316185545, 0, 13, 16, 0, 1.63299316185545,
                                      0, 14, 17, 0, 0, -1.63299316185545, 4, 18, 0, 0, 1.63299316185545,
                                      3, 19, 0, 0, 0, 2, 20, 0, 0, 0))
})

test_that("1.2 Three Cont. Pred. Central Composite Design Summary table results match", {
  table <- results[["results"]][["doeRsmDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.63299316185545, 2, 20, 0, 3, 4, 1, "Value", 20))
})

## Five continuous predictors (verified with Minitab) ####

options <- analysisOptions("doeResponseSurfaceMethodology")
options$actualExporter <- FALSE
options$alphaType <- "default"
options$centerPointType <- "default"
options$codedOutput <- TRUE
options$setSeed <- TRUE
options$seed <- 123
options$designType <- "centralCompositeDesign"
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$numberOfCategorical <- 0
options$numberOfContinuous <- 5
options$runOrder <- "runOrderStandard"
options$continuousVariables <- list(   # values of each list give number of predictors, always three lists
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 1", values = c("A", "B", "C", "D", "E")), # names
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 2", values = c(-1, -1, -1, -1, -1)), # low level
  list(levels = c("Row 0","Row 1", "Row 2", "Row 3", "Row 4"), name = "data 3", values = c(1, 1, 1, 1, 1)) # high level
)
options$selectedDesign2 <- list( # give design list, always five lists, number of values gives number of designs
  list(levels = c("Row 0", "Row 1"), name = "data 1", values = c(52, 54)), # runs
  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(10, 12)), # total
  list(levels = c("Row 0", "Row 1"), name = "data 3", values = c(0, 8)), # cube
  list(levels = c("Row 0", "Row 1"), name = "data 4", values = c(0, 4)), # axial
  list(levels = c("Row 0", "Row 1"), name = "data 5", values = c(2.378, 2.366)) # alpha
)
options$selectedRow <- 1 # select design
dataset <- NULL
results <- runAnalysis("doeResponseSurfaceMethodology", dataset, options)

test_that("2.1 Five Cont. Pred. Central Composite Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(16, 1, -1, -1, -1, -1, -1, 42, 2, 1, -1, -1, -1, -1, 22, 3, -1,
                                      1, -1, -1, -1, 46, 4, 1, 1, -1, -1, -1, 48, 5, -1, -1, 1, -1,
                                      -1, 3, 6, 1, -1, 1, -1, -1, 26, 7, -1, 1, 1, -1, -1, 53, 8,
                                      1, 1, 1, -1, -1, 50, 9, -1, -1, -1, 1, -1, 21, 10, 1, -1, -1,
                                      1, -1, 43, 11, -1, 1, -1, 1, -1, 20, 12, 1, 1, -1, 1, -1, 29,
                                      13, -1, -1, 1, 1, -1, 24, 14, 1, -1, 1, 1, -1, 5, 15, -1, 1,
                                      1, 1, -1, 36, 16, 1, 1, 1, 1, -1, 10, 17, -1, -1, -1, -1, 1,
                                      2, 18, 1, -1, -1, -1, 1, 12, 19, -1, 1, -1, -1, 1, 34, 20, 1,
                                      1, -1, -1, 1, 31, 21, -1, -1, 1, -1, 1, 23, 22, 1, -1, 1, -1,
                                      1, 45, 23, -1, 1, 1, -1, 1, 35, 24, 1, 1, 1, -1, 1, 44, 25,
                                      -1, -1, -1, 1, 1, 32, 26, 1, -1, -1, 1, 1, 54, 27, -1, 1, -1,
                                      1, 1, 17, 28, 1, 1, -1, 1, 1, 8, 29, -1, -1, 1, 1, 1, 4, 30,
                                      1, -1, 1, 1, 1, 41, 31, -1, 1, 1, 1, 1, 47, 32, 1, 1, 1, 1,
                                      1, 28, 33, 0, 0, 0, 0, 0, 27, 34, 0, 0, 0, 0, 0, 1, 35, 0, 0,
                                      0, 0, 0, 38, 36, 0, 0, 0, 0, 0, 14, 37, 0, 0, 0, 0, 0, 25, 38,
                                      0, 0, 0, 0, 0, 6, 39, 0, 0, 0, 0, 0, 33, 40, 0, 0, 0, 0, 0,
                                      37, 41, -2.36643191323985, 0, 0, 0, 0, 52, 42, 2.36643191323985,
                                      0, 0, 0, 0, 40, 43, 0, -2.36643191323985, 0, 0, 0, 39, 44, 0,
                                      2.36643191323985, 0, 0, 0, 18, 45, 0, 0, -2.36643191323985,
                                      0, 0, 19, 46, 0, 0, 2.36643191323985, 0, 0, 9, 47, 0, 0, 0,
                                      -2.36643191323985, 0, 15, 48, 0, 0, 0, 2.36643191323985, 0,
                                      51, 49, 0, 0, 0, 0, -2.36643191323985, 11, 50, 0, 0, 0, 0, 2.36643191323985,
                                      30, 51, 0, 0, 0, 0, 0, 13, 52, 0, 0, 0, 0, 0, 49, 53, 0, 0,
                                      0, 0, 0, 7, 54, 0, 0, 0, 0, 0))
})

test_that("2.2 Five Cont. Pred. Central Composite Design Summary table results match", {
  table <- results[["results"]][["doeRsmDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.36643191323985, 4, 54, 0, 5, 8, 1, "Value", 54))
})


## Three continuous and two categorical predictors (verified with Minitab) ####

options <- analysisOptions("doeResponseSurfaceMethodology")
options$actualExporter <- FALSE
options$alphaType <- "default"
options$centerPointType <- "default"
options$codedOutput <- TRUE
options$setSeed <- TRUE
options$seed <- 123
options$designType <- "centralCompositeDesign"
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$numberOfCategorical <- 0
options$numberOfContinuous <- 3
options$runOrder <- "runOrderStandard"
options$continuousVariables <- list(   # values of each list give number of predictors, always three lists
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 1", values = c("A", "B", "C")), # names
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 2", values = c(-1, -1, -1)), # low level
  list(levels = c("Row 0","Row 1", "Row 2", "Row 3", "Row 4"), name = "data 3", values = c(1, 1, 1)) # high level
)
options$numberOfCategorical <- 2
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0", "Row 1"), name = "data 1", values = c("D", "E")),
  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c("a", "a")),
  list(levels = c("Row 0","Row 1"), name = "data 3", values = c("b", "b")))
options$selectedDesign2 <- list( # give design list, always five lists, number of values gives number of designs
  list(levels = c("Row 0", "Row 1"), name = "data 1", values = c(20, 20)), # runs
  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(6, 6)), # total
  list(levels = c("Row 0", "Row 1"), name = "data 3", values = c(0, 4)), # cube
  list(levels = c("Row 0", "Row 1"), name = "data 4", values = c(0, 2)), # axial
  list(levels = c("Row 0", "Row 1"), name = "data 5", values = c(1.682, 1.633)) # alpha
)
options$selectedRow <- 1 # select design
dataset <- NULL
results <- runAnalysis("doeResponseSurfaceMethodology", dataset, options)


test_that("3.1 Three Cont. Pred. Two. Disc. Pred. Central Composite Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(6, 1, -1, -1, -1, "a", "a", 15, 2, 1, -1, -1, "a", "a", 8, 3,
                                      -1, 1, -1, "a", "a", 16, 4, 1, 1, -1, "a", "a", 17, 5, -1, -1,
                                      1, "a", "a", 1, 6, 1, -1, 1, "a", "a", 18, 7, -1, 1, 1, "a",
                                      "a", 12, 8, 1, 1, 1, "a", "a", 7, 9, 0, 0, 0, "a", "a", 20,
                                      10, 0, 0, 0, "a", "a", 10, 11, 0, 0, 0, "a", "a", 5, 12, 0,
                                      0, 0, "a", "a", 11, 13, -1.63299316185545, 0, 0, "a", "a", 9,
                                      14, 1.63299316185545, 0, 0, "a", "a", 19, 15, 0, -1.63299316185545,
                                      0, "a", "a", 13, 16, 0, 1.63299316185545, 0, "a", "a", 14, 17,
                                      0, 0, -1.63299316185545, "a", "a", 4, 18, 0, 0, 1.63299316185545,
                                      "a", "a", 3, 19, 0, 0, 0, "a", "a", 2, 20, 0, 0, 0, "a", "a",
                                      26, 21, -1, -1, -1, "b", "a", 35, 22, 1, -1, -1, "b", "a", 28,
                                      23, -1, 1, -1, "b", "a", 36, 24, 1, 1, -1, "b", "a", 37, 25,
                                      -1, -1, 1, "b", "a", 21, 26, 1, -1, 1, "b", "a", 38, 27, -1,
                                      1, 1, "b", "a", 32, 28, 1, 1, 1, "b", "a", 27, 29, 0, 0, 0,
                                      "b", "a", 40, 30, 0, 0, 0, "b", "a", 30, 31, 0, 0, 0, "b", "a",
                                      25, 32, 0, 0, 0, "b", "a", 31, 33, -1.63299316185545, 0, 0,
                                      "b", "a", 29, 34, 1.63299316185545, 0, 0, "b", "a", 39, 35,
                                      0, -1.63299316185545, 0, "b", "a", 33, 36, 0, 1.63299316185545,
                                      0, "b", "a", 34, 37, 0, 0, -1.63299316185545, "b", "a", 24,
                                      38, 0, 0, 1.63299316185545, "b", "a", 23, 39, 0, 0, 0, "b",
                                      "a", 22, 40, 0, 0, 0, "b", "a", 46, 41, -1, -1, -1, "a", "b",
                                      55, 42, 1, -1, -1, "a", "b", 48, 43, -1, 1, -1, "a", "b", 56,
                                      44, 1, 1, -1, "a", "b", 57, 45, -1, -1, 1, "a", "b", 41, 46,
                                      1, -1, 1, "a", "b", 58, 47, -1, 1, 1, "a", "b", 52, 48, 1, 1,
                                      1, "a", "b", 47, 49, 0, 0, 0, "a", "b", 60, 50, 0, 0, 0, "a",
                                      "b", 50, 51, 0, 0, 0, "a", "b", 45, 52, 0, 0, 0, "a", "b", 51,
                                      53, -1.63299316185545, 0, 0, "a", "b", 49, 54, 1.63299316185545,
                                      0, 0, "a", "b", 59, 55, 0, -1.63299316185545, 0, "a", "b", 53,
                                      56, 0, 1.63299316185545, 0, "a", "b", 54, 57, 0, 0, -1.63299316185545,
                                      "a", "b", 44, 58, 0, 0, 1.63299316185545, "a", "b", 43, 59,
                                      0, 0, 0, "a", "b", 42, 60, 0, 0, 0, "a", "b", 66, 61, -1, -1,
                                      -1, "b", "b", 75, 62, 1, -1, -1, "b", "b", 68, 63, -1, 1, -1,
                                      "b", "b", 76, 64, 1, 1, -1, "b", "b", 77, 65, -1, -1, 1, "b",
                                      "b", 61, 66, 1, -1, 1, "b", "b", 78, 67, -1, 1, 1, "b", "b",
                                      72, 68, 1, 1, 1, "b", "b", 67, 69, 0, 0, 0, "b", "b", 80, 70,
                                      0, 0, 0, "b", "b", 70, 71, 0, 0, 0, "b", "b", 65, 72, 0, 0,
                                      0, "b", "b", 71, 73, -1.63299316185545, 0, 0, "b", "b", 69,
                                      74, 1.63299316185545, 0, 0, "b", "b", 79, 75, 0, -1.63299316185545,
                                      0, "b", "b", 73, 76, 0, 1.63299316185545, 0, "b", "b", 74, 77,
                                      0, 0, -1.63299316185545, "b", "b", 64, 78, 0, 0, 1.63299316185545,
                                      "b", "b", 63, 79, 0, 0, 0, "b", "b", 62, 80, 0, 0, 0, "b", "b"
                                 ))
})

test_that("3.2 Three Cont. Pred. Two. Disc. Pred. Central Composite Design Summary table results match", {
  table <- results[["results"]][["doeRsmDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.63299316185545, 2, 20, 2, 3, 4, 1, "Value", 20))
})

## Four continuous and one categorical predictor (verified with Minitab) ####

options <- analysisOptions("doeResponseSurfaceMethodology")
options$actualExporter <- FALSE
options$alphaType <- "default"
options$centerPointType <- "default"
options$codedOutput <- TRUE
options$setSeed <- TRUE
options$seed <- 123
options$designType <- "centralCompositeDesign"
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$numberOfCategorical <- 0
options$numberOfContinuous <- 4
options$runOrder <- "runOrderStandard"
options$continuousVariables <- list(   # values of each list give number of predictors, always three lists
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 1", values = c("A", "B", "C", "D")), # names
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4"), name = "data 2", values = c(-1, -1, -1, -1)), # low level
  list(levels = c("Row 0","Row 1", "Row 2", "Row 3", "Row 4"), name = "data 3", values = c(1, 1, 1, 1)) # high level
)
options$numberOfCategorical <- 1
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0", "Row 1"), name = "data 1", values = c("E")),
  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c("a")),
  list(levels = c("Row 0","Row 1"), name = "data 3", values = c("b")))
options$selectedDesign2 <- list( # give design list, always five lists, number of values gives number of designs
  list(levels = c("Row 0", "Row 1"), name = "data 1", values = c(31, 30)), # runs
  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(7, 6)), # total
  list(levels = c("Row 0", "Row 1"), name = "data 3", values = c(0, 4)), # cube
  list(levels = c("Row 0", "Row 1"), name = "data 4", values = c(0, 2)), # axial
  list(levels = c("Row 0", "Row 1"), name = "data 5", values = c(2, 2)) # alpha
)
options$selectedRow <- 1 # select design
dataset <- NULL
results <- runAnalysis("doeResponseSurfaceMethodology", dataset, options)


test_that("4.1 Four Cont. Pred. One. Disc. Pred. Central Composite Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 1, -1, -1, -1, -1, "a", 23, 2, 1, -1, -1, -1, "a", 12, 3, -1,
                                      1, -1, -1, "a", 24, 4, 1, 1, -1, -1, "a", 25, 5, -1, -1, 1,
                                      -1, "a", 2, 6, 1, -1, 1, -1, "a", 13, 7, -1, 1, 1, -1, "a",
                                      21, 8, 1, 1, 1, -1, "a", 27, 9, -1, -1, -1, 1, "a", 10, 10,
                                      1, -1, -1, 1, "a", 20, 11, -1, 1, -1, 1, "a", 30, 12, 1, 1,
                                      -1, 1, "a", 22, 13, -1, -1, 1, 1, "a", 29, 14, 1, -1, 1, 1,
                                      "a", 26, 15, -1, 1, 1, 1, "a", 14, 16, 1, 1, 1, 1, "a", 4, 17,
                                      0, 0, 0, 0, "a", 1, 18, 0, 0, 0, 0, "a", 15, 19, 0, 0, 0, 0,
                                      "a", 11, 20, 0, 0, 0, 0, "a", 19, 21, -2, 0, 0, 0, "a", 7, 22,
                                      2, 0, 0, 0, "a", 6, 23, 0, -2, 0, 0, "a", 17, 24, 0, 2, 0, 0,
                                      "a", 28, 25, 0, 0, -2, 0, "a", 8, 26, 0, 0, 2, 0, "a", 3, 27,
                                      0, 0, 0, -2, "a", 16, 28, 0, 0, 0, 2, "a", 18, 29, 0, 0, 0,
                                      0, "a", 5, 30, 0, 0, 0, 0, "a", 39, 31, -1, -1, -1, -1, "b",
                                      53, 32, 1, -1, -1, -1, "b", 42, 33, -1, 1, -1, -1, "b", 54,
                                      34, 1, 1, -1, -1, "b", 55, 35, -1, -1, 1, -1, "b", 32, 36, 1,
                                      -1, 1, -1, "b", 43, 37, -1, 1, 1, -1, "b", 51, 38, 1, 1, 1,
                                      -1, "b", 57, 39, -1, -1, -1, 1, "b", 40, 40, 1, -1, -1, 1, "b",
                                      50, 41, -1, 1, -1, 1, "b", 60, 42, 1, 1, -1, 1, "b", 52, 43,
                                      -1, -1, 1, 1, "b", 59, 44, 1, -1, 1, 1, "b", 56, 45, -1, 1,
                                      1, 1, "b", 44, 46, 1, 1, 1, 1, "b", 34, 47, 0, 0, 0, 0, "b",
                                      31, 48, 0, 0, 0, 0, "b", 45, 49, 0, 0, 0, 0, "b", 41, 50, 0,
                                      0, 0, 0, "b", 49, 51, -2, 0, 0, 0, "b", 37, 52, 2, 0, 0, 0,
                                      "b", 36, 53, 0, -2, 0, 0, "b", 47, 54, 0, 2, 0, 0, "b", 58,
                                      55, 0, 0, -2, 0, "b", 38, 56, 0, 0, 2, 0, "b", 33, 57, 0, 0,
                                      0, -2, "b", 46, 58, 0, 0, 0, 2, "b", 48, 59, 0, 0, 0, 0, "b",
                                      35, 60, 0, 0, 0, 0, "b"))
})

test_that("4.2 Four Cont. Pred. One. Disc. Pred. Central Composite Design Summary table results match", {
  table <- results[["results"]][["doeRsmDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2, 2, 30, 1, 4, 4, 1, "Value", 30))
})

# Face centered vs default alpha ####

## Three continuous predictors (verified with Minitab) ####

## Four continuous predictors (verified with Minitab) ####

# Box Behnken Designs ####

## Three continuous predictors (verified with Minitab) ####

## Four continuous predictors (verified with Minitab) ####

## Five continuous predictors (verified with Minitab) ####
