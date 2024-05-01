context("[Quality Control] DoE Response surface methodology")

# Basic tests ####

## Three continuous predictors CCD (verified with Minitab) ####

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
results <- runAnalysis("doeResponseSurfaceMethodology", dataset, options)

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

## Five continuous predictors CCD (verified with Minitab) ####

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


## Three continuous and two categorical predictors CCD (verified with Minitab) ####

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

## Four continuous and one categorical predictor (verified with Minitab) CCD ####

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

## Three continuous predictors face centered CCD (verified with Minitab) ####

options <- analysisOptions("doeResponseSurfaceMethodology")
options$actualExporter <- FALSE
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
options$alphaType <- "faceCentered"
dataset <- NULL
results <- runAnalysis("doeResponseSurfaceMethodology", dataset, options)

test_that("5.1 Three Cont. Pred. Face Centered Central Composite Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(6, 1, -1, -1, -1, 15, 2, 1, -1, -1, 8, 3, -1, 1, -1, 16, 4, 1,
                                      1, -1, 17, 5, -1, -1, 1, 1, 6, 1, -1, 1, 18, 7, -1, 1, 1, 12,
                                      8, 1, 1, 1, 7, 9, 0, 0, 0, 20, 10, 0, 0, 0, 10, 11, 0, 0, 0,
                                      5, 12, 0, 0, 0, 11, 13, -1, 0, 0, 9, 14, 1, 0, 0, 19, 15, 0,
                                      -1, 0, 13, 16, 0, 1, 0, 14, 17, 0, 0, -1, 4, 18, 0, 0, 1, 3,
                                      19, 0, 0, 0, 2, 20, 0, 0, 0))
})

test_that("5.2 Three Cont. Pred. Face Centered Central Composite Design Summary table results match", {
  table <- results[["results"]][["doeRsmDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 2, 20, 0, 3, 4, 1, "Value", 20))
})

## Four continuous predictors  face centered CCD (verified with Minitab) ####

options <- analysisOptions("doeResponseSurfaceMethodology")
options$actualExporter <- FALSE
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
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 1", values = c("A", "B", "C", "D")), # names
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 2", values = c(-1, -1, -1, -1)), # low level
  list(levels = c("Row 0","Row 1", "Row 2", "Row 3"), name = "data 3", values = c(1, 1, 1, 1)) # high level
)
options$selectedDesign2 <- list( # give design list, always five lists, number of values gives number of designs
  list(levels = c("Row 0", "Row 1"), name = "data 1", values = c(31, 30)), # runs
  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(7, 6)), # total
  list(levels = c("Row 0", "Row 1"), name = "data 3", values = c(0, 4)), # cube
  list(levels = c("Row 0", "Row 1"), name = "data 4", values = c(0, 2)), # axial
  list(levels = c("Row 0", "Row 1"), name = "data 5", values = c(2, 2)) # alpha
)
options$selectedRow <- 1 # select design
options$alphaType <- "faceCentered"
dataset <- NULL
results <- runAnalysis("doeResponseSurfaceMethodology", dataset, options)



test_that("6.1 Four Cont. Pred. Face Centered Central Composite Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 1, -1, -1, -1, -1, 23, 2, 1, -1, -1, -1, 12, 3, -1, 1, -1,
                                      -1, 24, 4, 1, 1, -1, -1, 25, 5, -1, -1, 1, -1, 2, 6, 1, -1,
                                      1, -1, 13, 7, -1, 1, 1, -1, 21, 8, 1, 1, 1, -1, 27, 9, -1, -1,
                                      -1, 1, 10, 10, 1, -1, -1, 1, 20, 11, -1, 1, -1, 1, 30, 12, 1,
                                      1, -1, 1, 22, 13, -1, -1, 1, 1, 29, 14, 1, -1, 1, 1, 26, 15,
                                      -1, 1, 1, 1, 14, 16, 1, 1, 1, 1, 4, 17, 0, 0, 0, 0, 1, 18, 0,
                                      0, 0, 0, 15, 19, 0, 0, 0, 0, 11, 20, 0, 0, 0, 0, 19, 21, -1,
                                      0, 0, 0, 7, 22, 1, 0, 0, 0, 6, 23, 0, -1, 0, 0, 17, 24, 0, 1,
                                      0, 0, 28, 25, 0, 0, -1, 0, 8, 26, 0, 0, 1, 0, 3, 27, 0, 0, 0,
                                      -1, 16, 28, 0, 0, 0, 1, 18, 29, 0, 0, 0, 0, 5, 30, 0, 0, 0,
                                      0))
})

test_that("6.2 Four Cont. Pred. Face Centered Central Composite Design Summary table results match", {
  table <- results[["results"]][["doeRsmDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 2, 30, 0, 4, 4, 1, "Value", 30))
})

# Box Behnken Designs ####

## Three continuous predictors BBD (verified with Minitab) ####

options <- analysisOptions("doeResponseSurfaceMethodology")
options$actualExporter <- FALSE
options$alphaType <- "default"
options$centerPointType <- "default"
options$codedOutput <- TRUE
options$setSeed <- TRUE
options$seed <- 123
options$designType <- "boxBehnkenDesign"
options$centerPointType <- "default"
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
options$selectedDesign2 <- list( # give design list, always three lists, number of values gives number of designs
  list(levels = c("Row 0"), name = "data 1", values = c(15)), # runs
  list(levels = c("Row 0"), name = "data 2", values = c(1)), # blocks
  list(levels = c("Row 0"), name = "data 3", values = c(3)) # centre points
)
options$selectedRow <- 0 # select design
dataset <- NULL
results <- runAnalysis("doeResponseSurfaceMethodology", dataset, options)

test_that("7.1 Three Cont. Pred. Box-Behnken Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(5, 1, -1, -1, 0, 12, 2, 1, -1, 0, 6, 3, -1, 1, 0, 11, 4, 1, 1,
                                      0, 14, 5, -1, 0, -1, 1, 6, 1, 0, -1, 15, 7, -1, 0, 1, 8, 8,
                                      1, 0, 1, 4, 9, 0, -1, -1, 3, 10, 0, 1, -1, 9, 11, 0, -1, 1,
                                      2, 12, 0, 1, 1, 13, 13, 0, 0, 0, 7, 14, 0, 0, 0, 10, 15, 0,
                                      0, 0))
})

test_that("7.2 Three Cont. Pred. Box-Behnken Design table results match Summary table results match", {
  table <- results[["results"]][["doeRsmDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 15, 0, 3, 3, 1, "Value", 1, 15))
})


## Four continuous predictors BBD (verified with Minitab) ####

options <- analysisOptions("doeResponseSurfaceMethodology")
options$actualExporter <- FALSE
options$alphaType <- "default"
options$centerPointType <- "default"
options$codedOutput <- TRUE
options$setSeed <- TRUE
options$seed <- 123
options$designType <- "boxBehnkenDesign"
options$centerPointType <- "default"
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$numberOfCategorical <- 0
options$numberOfContinuous <- 4
options$runOrder <- "runOrderStandard"
options$continuousVariables <- list(   # values of each list give number of predictors, always three lists
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 1", values = c("A", "B", "C", "D")), # names
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 2", values = c(-1, -1, -1, -1)), # low level
  list(levels = c("Row 0","Row 1", "Row 2", "Row 3"), name = "data 3", values = c(1, 1, 1, 1)) # high level
)
options$selectedDesign2 <- list( # give design list, always three lists, number of values gives number of designs
  list(levels = c("Row 0"), name = "data 1", values = c(33)), # runs
  list(levels = c("Row 0"), name = "data 2", values = c(3)), # blocks
  list(levels = c("Row 0"), name = "data 3", values = c(3)) # centre points
)
options$selectedRow <- 0 # select design
dataset <- NULL
results <- runAnalysis("doeResponseSurfaceMethodology", dataset, options)


test_that("8.1 Four Cont. Pred. Box-Behnken Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 10, 1, -1, -1, 0, 0, 1, 26, 2, 1, -1, 0, 0, 1, 13, 3, -1, 1,
                                      0, 0, 1, 27, 4, 1, 1, 0, 0, 1, 28, 5, 0, 0, -1, -1, 1, 2, 6,
                                      0, 0, 1, -1, 1, 15, 7, 0, 0, -1, 1, 1, 24, 8, 0, 0, 1, 1, 1,
                                      14, 9, 0, 0, 0, 0, 1, 11, 10, 0, 0, 0, 0, 1, 23, 11, 0, 0, 0,
                                      0, 2, 33, 12, -1, 0, 0, -1, 2, 30, 13, 1, 0, 0, -1, 2, 12, 14,
                                      -1, 0, 0, 1, 2, 29, 15, 1, 0, 0, 1, 2, 17, 16, 0, -1, -1, 0,
                                      2, 5, 17, 0, 1, -1, 0, 2, 1, 18, 0, -1, 1, 0, 2, 18, 19, 0,
                                      1, 1, 0, 2, 25, 20, 0, 0, 0, 0, 2, 20, 21, 0, 0, 0, 0, 2, 9,
                                      22, 0, 0, 0, 0, 3, 8, 23, -1, 0, -1, 0, 3, 22, 24, 1, 0, -1,
                                      0, 3, 6, 25, -1, 0, 1, 0, 3, 31, 26, 1, 0, 1, 0, 3, 4, 27, 0,
                                      -1, 0, -1, 3, 7, 28, 0, 1, 0, -1, 3, 19, 29, 0, -1, 0, 1, 3,
                                      16, 30, 0, 1, 0, 1, 3, 3, 31, 0, 0, 0, 0, 3, 21, 32, 0, 0, 0,
                                      0, 3, 32, 33, 0, 0, 0, 0))
})

test_that("8.2 Four Cont. Pred. Box-Behnken Design Summary table results match", {
  table <- results[["results"]][["doeRsmDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3, 33, 0, 3, 4, 1, "Value", 3, 33))
})

## Five continuous predictors BBD (verified with Minitab) ####

options <- analysisOptions("doeResponseSurfaceMethodology")
options$actualExporter <- FALSE
options$alphaType <- "default"
options$centerPointType <- "default"
options$codedOutput <- TRUE
options$setSeed <- TRUE
options$seed <- 123
options$designType <- "boxBehnkenDesign"
options$centerPointType <- "default"
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
options$selectedDesign2 <- list( # give design list, always three lists, number of values gives number of designs
  list(levels = c("Row 0"), name = "data 1", values = c(46)), # runs
  list(levels = c("Row 0"), name = "data 2", values = c(2)), # blocks
  list(levels = c("Row 0"), name = "data 3", values = c(3)) # centre points
)
options$selectedRow <- 0 # select design
dataset <- NULL
results <- runAnalysis("doeResponseSurfaceMethodology", dataset, options)

test_that("9.1 Five Cont. Pred. Box-Behnken Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 14, 1, -1, -1, 0, 0, 0, 1, 36, 2, 1, -1, 0, 0, 0, 1, 18, 3,
                                      -1, 1, 0, 0, 0, 1, 38, 4, 1, 1, 0, 0, 0, 1, 40, 5, -1, 0, -1,
                                      0, 0, 1, 2, 6, 1, 0, -1, 0, 0, 1, 22, 7, -1, 0, 1, 0, 0, 1,
                                      35, 8, 1, 0, 1, 0, 0, 1, 21, 9, 0, 0, -1, -1, 0, 1, 17, 10,
                                      0, 0, 1, -1, 0, 1, 39, 11, 0, 0, -1, 1, 0, 1, 16, 12, 0, 0,
                                      1, 1, 0, 1, 24, 13, 0, 0, 0, -1, -1, 1, 19, 14, 0, 0, 0, 1,
                                      -1, 1, 4, 15, 0, 0, 0, -1, 1, 1, 28, 16, 0, 0, 0, 1, 1, 1, 8,
                                      17, 0, -1, 0, 0, -1, 1, 41, 18, 0, 1, 0, 0, -1, 1, 10, 19, 0,
                                      -1, 0, 0, 1, 1, 26, 20, 0, 1, 0, 0, 1, 1, 34, 21, 0, 0, 0, 0,
                                      0, 1, 44, 22, 0, 0, 0, 0, 0, 1, 45, 23, 0, 0, 0, 0, 0, 2, 23,
                                      24, -1, 0, 0, -1, 0, 2, 15, 25, 1, 0, 0, -1, 0, 2, 42, 26, -1,
                                      0, 0, 1, 0, 2, 11, 27, 1, 0, 0, 1, 0, 2, 12, 28, -1, 0, 0, 0,
                                      -1, 2, 6, 29, 1, 0, 0, 0, -1, 2, 3, 30, -1, 0, 0, 0, 1, 2, 27,
                                      31, 1, 0, 0, 0, 1, 2, 46, 32, 0, -1, -1, 0, 0, 2, 31, 33, 0,
                                      1, -1, 0, 0, 2, 20, 34, 0, -1, 1, 0, 0, 2, 1, 35, 0, 1, 1, 0,
                                      0, 2, 25, 36, 0, -1, 0, -1, 0, 2, 30, 37, 0, 1, 0, -1, 0, 2,
                                      29, 38, 0, -1, 0, 1, 0, 2, 37, 39, 0, 1, 0, 1, 0, 2, 9, 40,
                                      0, 0, -1, 0, -1, 2, 33, 41, 0, 0, 1, 0, -1, 2, 43, 42, 0, 0,
                                      -1, 0, 1, 2, 7, 43, 0, 0, 1, 0, 1, 2, 32, 44, 0, 0, 0, 0, 0,
                                      2, 13, 45, 0, 0, 0, 0, 0, 2, 5, 46, 0, 0, 0, 0, 0))
})

test_that("9.2 Five Cont. Pred. Box-Behnken Design Summary table results match", {
  table <- results[["results"]][["doeRsmDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2, 46, 0, 3, 5, 1, "Value", 2, 46))
})


## Six continuous predictors one discrete predictor BBD (verified with Minitab) ####

options <- analysisOptions("doeResponseSurfaceMethodology")
options$actualExporter <- FALSE
options$alphaType <- "default"
options$centerPointType <- "default"
options$codedOutput <- TRUE
options$setSeed <- TRUE
options$seed <- 123
options$designType <- "boxBehnkenDesign"
options$centerPointType <- "default"
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$numberOfContinuous <- 6
options$runOrder <- "runOrderStandard"
options$continuousVariables <- list(   # values of each list give number of predictors, always three lists
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4", "Row 5"), name = "data 1", values = c("A", "B", "C", "D", "E", "F")), # names
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3", "Row 4", "Row 5"), name = "data 2", values = c(-1, -1, -1, -1, -1, -1)), # low level
  list(levels = c("Row 0","Row 1", "Row 2", "Row 3", "Row 4", "Row 5"), name = "data 3", values = c(1, 1, 1, 1, 1, 1)) # high level
)
options$numberOfCategorical <- 1
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0", "Row 1"), name = "data 1", values = c("G")),
  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c("a")),
  list(levels = c("Row 0","Row 1"), name = "data 3", values = c("b")))
options$selectedDesign2 <- list( # give design list, always three lists, number of values gives number of designs
  list(levels = c("Row 0"), name = "data 1", values = c(54)), # runs
  list(levels = c("Row 0"), name = "data 2", values = c(1)), # blocks
  list(levels = c("Row 0"), name = "data 3", values = c(6)) # centre points
)
options$selectedRow <- 0 # select design
dataset <- NULL
results <- runAnalysis("doeResponseSurfaceMethodology", dataset, options)


test_that("10.1 Six Cont. Pred. One Disc. Pred. Box-Behnken Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(16, 1, -1, -1, 0, -1, 0, 0, "a", 42, 2, 1, -1, 0, -1, 0, 0, "a",
                                      22, 3, -1, 1, 0, -1, 0, 0, "a", 46, 4, 1, 1, 0, -1, 0, 0, "a",
                                      48, 5, -1, -1, 0, 1, 0, 0, "a", 3, 6, 1, -1, 0, 1, 0, 0, "a",
                                      26, 7, -1, 1, 0, 1, 0, 0, "a", 53, 8, 1, 1, 0, 1, 0, 0, "a",
                                      50, 9, 0, -1, -1, 0, -1, 0, "a", 21, 10, 0, 1, -1, 0, -1, 0,
                                      "a", 43, 11, 0, -1, 1, 0, -1, 0, "a", 20, 12, 0, 1, 1, 0, -1,
                                      0, "a", 29, 13, 0, -1, -1, 0, 1, 0, "a", 24, 14, 0, 1, -1, 0,
                                      1, 0, "a", 5, 15, 0, -1, 1, 0, 1, 0, "a", 36, 16, 0, 1, 1, 0,
                                      1, 0, "a", 10, 17, 0, 0, -1, -1, 0, -1, "a", 2, 18, 0, 0, 1,
                                      -1, 0, -1, "a", 12, 19, 0, 0, -1, 1, 0, -1, "a", 34, 20, 0,
                                      0, 1, 1, 0, -1, "a", 31, 21, 0, 0, -1, -1, 0, 1, "a", 23, 22,
                                      0, 0, 1, -1, 0, 1, "a", 45, 23, 0, 0, -1, 1, 0, 1, "a", 35,
                                      24, 0, 0, 1, 1, 0, 1, "a", 44, 25, -1, 0, 0, -1, -1, 0, "a",
                                      32, 26, 1, 0, 0, -1, -1, 0, "a", 54, 27, -1, 0, 0, 1, -1, 0,
                                      "a", 17, 28, 1, 0, 0, 1, -1, 0, "a", 8, 29, -1, 0, 0, -1, 1,
                                      0, "a", 4, 30, 1, 0, 0, -1, 1, 0, "a", 41, 31, -1, 0, 0, 1,
                                      1, 0, "a", 47, 32, 1, 0, 0, 1, 1, 0, "a", 28, 33, 0, -1, 0,
                                      0, -1, -1, "a", 27, 34, 0, 1, 0, 0, -1, -1, "a", 1, 35, 0, -1,
                                      0, 0, 1, -1, "a", 38, 36, 0, 1, 0, 0, 1, -1, "a", 14, 37, 0,
                                      -1, 0, 0, -1, 1, "a", 25, 38, 0, 1, 0, 0, -1, 1, "a", 6, 39,
                                      0, -1, 0, 0, 1, 1, "a", 33, 40, 0, 1, 0, 0, 1, 1, "a", 37, 41,
                                      -1, 0, -1, 0, 0, -1, "a", 52, 42, 1, 0, -1, 0, 0, -1, "a", 40,
                                      43, -1, 0, 1, 0, 0, -1, "a", 39, 44, 1, 0, 1, 0, 0, -1, "a",
                                      18, 45, -1, 0, -1, 0, 0, 1, "a", 19, 46, 1, 0, -1, 0, 0, 1,
                                      "a", 9, 47, -1, 0, 1, 0, 0, 1, "a", 15, 48, 1, 0, 1, 0, 0, 1,
                                      "a", 51, 49, 0, 0, 0, 0, 0, 0, "a", 11, 50, 0, 0, 0, 0, 0, 0,
                                      "a", 30, 51, 0, 0, 0, 0, 0, 0, "a", 13, 52, 0, 0, 0, 0, 0, 0,
                                      "a", 49, 53, 0, 0, 0, 0, 0, 0, "a", 7, 54, 0, 0, 0, 0, 0, 0,
                                      "a", 70, 55, -1, -1, 0, -1, 0, 0, "b", 96, 56, 1, -1, 0, -1,
                                      0, 0, "b", 76, 57, -1, 1, 0, -1, 0, 0, "b", 100, 58, 1, 1, 0,
                                      -1, 0, 0, "b", 102, 59, -1, -1, 0, 1, 0, 0, "b", 57, 60, 1,
                                      -1, 0, 1, 0, 0, "b", 80, 61, -1, 1, 0, 1, 0, 0, "b", 107, 62,
                                      1, 1, 0, 1, 0, 0, "b", 104, 63, 0, -1, -1, 0, -1, 0, "b", 75,
                                      64, 0, 1, -1, 0, -1, 0, "b", 97, 65, 0, -1, 1, 0, -1, 0, "b",
                                      74, 66, 0, 1, 1, 0, -1, 0, "b", 83, 67, 0, -1, -1, 0, 1, 0,
                                      "b", 78, 68, 0, 1, -1, 0, 1, 0, "b", 59, 69, 0, -1, 1, 0, 1,
                                      0, "b", 90, 70, 0, 1, 1, 0, 1, 0, "b", 64, 71, 0, 0, -1, -1,
                                      0, -1, "b", 56, 72, 0, 0, 1, -1, 0, -1, "b", 66, 73, 0, 0, -1,
                                      1, 0, -1, "b", 88, 74, 0, 0, 1, 1, 0, -1, "b", 85, 75, 0, 0,
                                      -1, -1, 0, 1, "b", 77, 76, 0, 0, 1, -1, 0, 1, "b", 99, 77, 0,
                                      0, -1, 1, 0, 1, "b", 89, 78, 0, 0, 1, 1, 0, 1, "b", 98, 79,
                                      -1, 0, 0, -1, -1, 0, "b", 86, 80, 1, 0, 0, -1, -1, 0, "b", 108,
                                      81, -1, 0, 0, 1, -1, 0, "b", 71, 82, 1, 0, 0, 1, -1, 0, "b",
                                      62, 83, -1, 0, 0, -1, 1, 0, "b", 58, 84, 1, 0, 0, -1, 1, 0,
                                      "b", 95, 85, -1, 0, 0, 1, 1, 0, "b", 101, 86, 1, 0, 0, 1, 1,
                                      0, "b", 82, 87, 0, -1, 0, 0, -1, -1, "b", 81, 88, 0, 1, 0, 0,
                                      -1, -1, "b", 55, 89, 0, -1, 0, 0, 1, -1, "b", 92, 90, 0, 1,
                                      0, 0, 1, -1, "b", 68, 91, 0, -1, 0, 0, -1, 1, "b", 79, 92, 0,
                                      1, 0, 0, -1, 1, "b", 60, 93, 0, -1, 0, 0, 1, 1, "b", 87, 94,
                                      0, 1, 0, 0, 1, 1, "b", 91, 95, -1, 0, -1, 0, 0, -1, "b", 106,
                                      96, 1, 0, -1, 0, 0, -1, "b", 94, 97, -1, 0, 1, 0, 0, -1, "b",
                                      93, 98, 1, 0, 1, 0, 0, -1, "b", 72, 99, -1, 0, -1, 0, 0, 1,
                                      "b", 73, 100, 1, 0, -1, 0, 0, 1, "b", 63, 101, -1, 0, 1, 0,
                                      0, 1, "b", 69, 102, 1, 0, 1, 0, 0, 1, "b", 105, 103, 0, 0, 0,
                                      0, 0, 0, "b", 65, 104, 0, 0, 0, 0, 0, 0, "b", 84, 105, 0, 0,
                                      0, 0, 0, 0, "b", 67, 106, 0, 0, 0, 0, 0, 0, "b", 103, 107, 0,
                                      0, 0, 0, 0, 0, "b", 61, 108, 0, 0, 0, 0, 0, 0, "b"))
})

test_that("10.2 Six Cont. Pred. One Disc. Pred. Box-Behnken Design Summary table results match", {
  table <- results[["results"]][["doeRsmDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 54, 1, 6, 6, 1, "Value", 1, 54))
})
