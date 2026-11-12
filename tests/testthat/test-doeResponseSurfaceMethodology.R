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
results <- runAnalysis("doeResponseSurfaceMethodology", "debug.csv", options)

test_that("1.1 Three Cont. Pred. Central Composite Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(15, 1, -1, -1, -1, 19, 2, 1, -1, -1, 14, 3,
                                  -1, 1, -1, 3, 4, 1, 1, -1, 10, 5, -1, -1, 1,
                                  2, 6, 1, -1, 1, 6, 7, -1, 1, 1, 11, 8, 1, 1,
                                  1, 5, 9, 0, 0, 0, 4, 10, 0, 0, 0, 18, 11, 0,
                                  0, 0, 9, 12, 0, 0, 0, 20, 13,
                                  -1.63299316185545, 0, 0, 17, 14,
                                  1.63299316185545, 0, 0, 12, 15, 0,
                                  -1.63299316185545, 0, 7, 16, 0,
                                  1.63299316185545, 0, 16, 17, 0, 0,
                                  -1.63299316185545, 1, 18, 0, 0,
                                  1.63299316185545, 8, 19, 0, 0, 0, 13, 20, 0,
                                  0, 0))
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
results <- runAnalysis("doeResponseSurfaceMethodology", "debug.csv", options)

test_that("2.1 Five Cont. Pred. Central Composite Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(31, 1, -1, -1, -1, -1, -1, 15, 2, 1, -1, -1,
                                  -1, -1, 51, 3, -1, 1, -1, -1, -1, 14, 4, 1,
                                  1, -1, -1, -1, 3, 5, -1, -1, 1, -1, -1, 42,
                                  6, 1, -1, 1, -1, -1, 43, 7, -1, 1, 1, -1, -1,
                                  37, 8, 1, 1, 1, -1, -1, 52, 9, -1, -1, -1, 1,
                                  -1, 25, 10, 1, -1, -1, 1, -1, 26, 11, -1, 1,
                                  -1, 1, -1, 27, 12, 1, 1, -1, 1, -1, 5, 13,
                                  -1, -1, 1, 1, -1, 48, 14, 1, -1, 1, 1, -1,
                                  28, 15, -1, 1, 1, 1, -1, 9, 16, 1, 1, 1, 1,
                                  -1, 29, 17, -1, -1, -1, -1, 1, 35, 18, 1, -1,
                                  -1, -1, 1, 8, 19, -1, 1, -1, -1, 1, 44, 20,
                                  1, 1, -1, -1, 1, 7, 21, -1, -1, 1, -1, 1, 39,
                                  22, 1, -1, 1, -1, 1, 30, 23, -1, 1, 1, -1, 1,
                                  19, 24, 1, 1, 1, -1, 1, 4, 25, -1, -1, -1, 1,
                                  1, 46, 26, 1, -1, -1, 1, 1, 17, 27, -1, 1,
                                  -1, 1, 1, 11, 28, 1, 1, -1, 1, 1, 34, 29, -1,
                                  -1, 1, 1, 1, 21, 30, 1, -1, 1, 1, 1, 12, 31,
                                  -1, 1, 1, 1, 1, 53, 32, 1, 1, 1, 1, 1, 10,
                                  33, 0, 0, 0, 0, 0, 13, 34, 0, 0, 0, 0, 0, 47,
                                  35, 0, 0, 0, 0, 0, 33, 36, 0, 0, 0, 0, 0, 54,
                                  37, 0, 0, 0, 0, 0, 22, 38, 0, 0, 0, 0, 0, 20,
                                  39, 0, 0, 0, 0, 0, 41, 40, 0, 0, 0, 0, 0, 24,
                                  41, -2.36643191323985, 0, 0, 0, 0, 49, 42,
                                  2.36643191323985, 0, 0, 0, 0, 16, 43, 0,
                                  -2.36643191323985, 0, 0, 0, 45, 44, 0,
                                  2.36643191323985, 0, 0, 0, 6, 45, 0, 0,
                                  -2.36643191323985, 0, 0, 18, 46, 0, 0,
                                  2.36643191323985, 0, 0, 2, 47, 0, 0, 0,
                                  -2.36643191323985, 0, 23, 48, 0, 0, 0,
                                  2.36643191323985, 0, 38, 49, 0, 0, 0, 0,
                                  -2.36643191323985, 32, 50, 0, 0, 0, 0,
                                  2.36643191323985, 50, 51, 0, 0, 0, 0, 0, 1,
                                  52, 0, 0, 0, 0, 0, 36, 53, 0, 0, 0, 0, 0, 40,
                                  54, 0, 0, 0, 0, 0))
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
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 1", values = c("A", "B", "C")), # names
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 2", values = c(-1, -1, -1)), # low level
  list(levels = c("Row 0", "Row 1", "Row 2"), name = "data 3", values = c(1, 1, 1)) # high level
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
results <- runAnalysis("doeResponseSurfaceMethodology", "debug.csv", options)


test_that("3.1 Three Cont. Pred. Two. Disc. Pred. Central Composite Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(15, 1, -1, -1, -1, "a", "a", 19, 2, 1, -1,
                                  -1, "a", "a", 14, 3, -1, 1, -1, "a", "a", 3,
                                  4, 1, 1, -1, "a", "a", 10, 5, -1, -1, 1, "a",
                                  "a", 2, 6, 1, -1, 1, "a", "a", 6, 7, -1, 1,
                                  1, "a", "a", 11, 8, 1, 1, 1, "a", "a", 5, 9,
                                  0, 0, 0, "a", "a", 4, 10, 0, 0, 0, "a", "a",
                                  18, 11, 0, 0, 0, "a", "a", 9, 12, 0, 0, 0,
                                  "a", "a", 20, 13, -1.63299316185545, 0, 0,
                                  "a", "a", 17, 14, 1.63299316185545, 0, 0,
                                  "a", "a", 12, 15, 0, -1.63299316185545, 0,
                                  "a", "a", 7, 16, 0, 1.63299316185545, 0, "a",
                                  "a", 16, 17, 0, 0, -1.63299316185545, "a",
                                  "a", 1, 18, 0, 0, 1.63299316185545, "a", "a",
                                  8, 19, 0, 0, 0, "a", "a", 13, 20, 0, 0, 0,
                                  "a", "a", 35, 21, -1, -1, -1, "b", "a", 39,
                                  22, 1, -1, -1, "b", "a", 34, 23, -1, 1, -1,
                                  "b", "a", 23, 24, 1, 1, -1, "b", "a", 30, 25,
                                  -1, -1, 1, "b", "a", 22, 26, 1, -1, 1, "b",
                                  "a", 26, 27, -1, 1, 1, "b", "a", 31, 28, 1,
                                  1, 1, "b", "a", 25, 29, 0, 0, 0, "b", "a",
                                  24, 30, 0, 0, 0, "b", "a", 38, 31, 0, 0, 0,
                                  "b", "a", 29, 32, 0, 0, 0, "b", "a", 40, 33,
                                  -1.63299316185545, 0, 0, "b", "a", 37, 34,
                                  1.63299316185545, 0, 0, "b", "a", 32, 35, 0,
                                  -1.63299316185545, 0, "b", "a", 27, 36, 0,
                                  1.63299316185545, 0, "b", "a", 36, 37, 0, 0,
                                  -1.63299316185545, "b", "a", 21, 38, 0, 0,
                                  1.63299316185545, "b", "a", 28, 39, 0, 0, 0,
                                  "b", "a", 33, 40, 0, 0, 0, "b", "a", 55, 41,
                                  -1, -1, -1, "a", "b", 59, 42, 1, -1, -1, "a",
                                  "b", 54, 43, -1, 1, -1, "a", "b", 43, 44, 1,
                                  1, -1, "a", "b", 50, 45, -1, -1, 1, "a", "b",
                                  42, 46, 1, -1, 1, "a", "b", 46, 47, -1, 1, 1,
                                  "a", "b", 51, 48, 1, 1, 1, "a", "b", 45, 49,
                                  0, 0, 0, "a", "b", 44, 50, 0, 0, 0, "a", "b",
                                  58, 51, 0, 0, 0, "a", "b", 49, 52, 0, 0, 0,
                                  "a", "b", 60, 53, -1.63299316185545, 0, 0,
                                  "a", "b", 57, 54, 1.63299316185545, 0, 0,
                                  "a", "b", 52, 55, 0, -1.63299316185545, 0,
                                  "a", "b", 47, 56, 0, 1.63299316185545, 0,
                                  "a", "b", 56, 57, 0, 0, -1.63299316185545,
                                  "a", "b", 41, 58, 0, 0, 1.63299316185545,
                                  "a", "b", 48, 59, 0, 0, 0, "a", "b", 53, 60,
                                  0, 0, 0, "a", "b", 75, 61, -1, -1, -1, "b",
                                  "b", 79, 62, 1, -1, -1, "b", "b", 74, 63, -1,
                                  1, -1, "b", "b", 63, 64, 1, 1, -1, "b", "b",
                                  70, 65, -1, -1, 1, "b", "b", 62, 66, 1, -1,
                                  1, "b", "b", 66, 67, -1, 1, 1, "b", "b", 71,
                                  68, 1, 1, 1, "b", "b", 65, 69, 0, 0, 0, "b",
                                  "b", 64, 70, 0, 0, 0, "b", "b", 78, 71, 0, 0,
                                  0, "b", "b", 69, 72, 0, 0, 0, "b", "b", 80,
                                  73, -1.63299316185545, 0, 0, "b", "b", 77,
                                  74, 1.63299316185545, 0, 0, "b", "b", 72, 75,
                                  0, -1.63299316185545, 0, "b", "b", 67, 76, 0,
                                  1.63299316185545, 0, "b", "b", 76, 77, 0, 0,
                                  -1.63299316185545, "b", "b", 61, 78, 0, 0,
                                  1.63299316185545, "b", "b", 68, 79, 0, 0, 0,
                                  "b", "b", 73, 80, 0, 0, 0, "b", "b"))
})

test_that("3.2 Three Cont. Pred. Two. Disc. Pred. Central Composite Design Summary table results match", {
  table <- results[["results"]][["doeRsmDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.63299316185545, 2, 20, 2, 3, 4, 1, "Value", 80))
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
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 1", values = c("A", "B", "C", "D")), # names
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 2", values = c(-1, -1, -1, -1)), # low level
  list(levels = c("Row 0", "Row 1", "Row 2", "Row 3"), name = "data 3", values = c(1, 1, 1, 1)) # high level
)
options$numberOfCategorical <- 1
options$categoricalVariables <- list(   # number of lists gives number of levels (n-1), values of each list give number of factors
  list(levels = c("Row 0"), name = "data 1", values = c("E")),
  list(levels = c("Row 0"), name = "data 2", values = c("a")),
  list(levels = c("Row 0"), name = "data 3", values = c("b")))
options$selectedDesign2 <- list( # give design list, always five lists, number of values gives number of designs
  list(levels = c("Row 0", "Row 1"), name = "data 1", values = c(31, 30)), # runs
  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(7, 6)), # total
  list(levels = c("Row 0", "Row 1"), name = "data 3", values = c(0, 4)), # cube
  list(levels = c("Row 0", "Row 1"), name = "data 4", values = c(0, 2)), # axial
  list(levels = c("Row 0", "Row 1"), name = "data 5", values = c(2, 2)) # alpha
)
options$selectedRow <- 1 # select design
results <- runAnalysis("doeResponseSurfaceMethodology", "debug.csv", options)


test_that("4.1 Four Cont. Pred. One. Disc. Pred. Central Composite Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(15, 1, -1, -1, -1, -1, "a", 19, 2, 1, -1, -1,
                                  -1, "a", 14, 3, -1, 1, -1, -1, "a", 3, 4, 1,
                                  1, -1, -1, "a", 10, 5, -1, -1, 1, -1, "a",
                                  18, 6, 1, -1, 1, -1, "a", 22, 7, -1, 1, 1,
                                  -1, "a", 11, 8, 1, 1, 1, -1, "a", 5, 9, -1,
                                  -1, -1, 1, "a", 20, 10, 1, -1, -1, 1, "a",
                                  28, 11, -1, 1, -1, 1, "a", 24, 12, 1, 1, -1,
                                  1, "a", 9, 13, -1, -1, 1, 1, "a", 27, 14, 1,
                                  -1, 1, 1, "a", 8, 15, -1, 1, 1, 1, "a", 26,
                                  16, 1, 1, 1, 1, "a", 7, 17, 0, 0, 0, 0, "a",
                                  30, 18, 0, 0, 0, 0, "a", 25, 19, 0, 0, 0, 0,
                                  "a", 17, 20, 0, 0, 0, 0, "a", 4, 21, -2, 0,
                                  0, 0, "a", 1, 22, 2, 0, 0, 0, "a", 23, 23, 0,
                                  -2, 0, 0, "a", 21, 24, 0, 2, 0, 0, "a", 29,
                                  25, 0, 0, -2, 0, "a", 13, 26, 0, 0, 2, 0,
                                  "a", 16, 27, 0, 0, 0, -2, "a", 2, 28, 0, 0,
                                  0, 2, "a", 12, 29, 0, 0, 0, 0, "a", 6, 30, 0,
                                  0, 0, 0, "a", 45, 31, -1, -1, -1, -1, "b",
                                  49, 32, 1, -1, -1, -1, "b", 44, 33, -1, 1,
                                  -1, -1, "b", 33, 34, 1, 1, -1, -1, "b", 40,
                                  35, -1, -1, 1, -1, "b", 48, 36, 1, -1, 1, -1,
                                  "b", 52, 37, -1, 1, 1, -1, "b", 41, 38, 1, 1,
                                  1, -1, "b", 35, 39, -1, -1, -1, 1, "b", 50,
                                  40, 1, -1, -1, 1, "b", 58, 41, -1, 1, -1, 1,
                                  "b", 54, 42, 1, 1, -1, 1, "b", 39, 43, -1,
                                  -1, 1, 1, "b", 57, 44, 1, -1, 1, 1, "b", 38,
                                  45, -1, 1, 1, 1, "b", 56, 46, 1, 1, 1, 1,
                                  "b", 37, 47, 0, 0, 0, 0, "b", 60, 48, 0, 0,
                                  0, 0, "b", 55, 49, 0, 0, 0, 0, "b", 47, 50,
                                  0, 0, 0, 0, "b", 34, 51, -2, 0, 0, 0, "b",
                                  31, 52, 2, 0, 0, 0, "b", 53, 53, 0, -2, 0, 0,
                                  "b", 51, 54, 0, 2, 0, 0, "b", 59, 55, 0, 0,
                                  -2, 0, "b", 43, 56, 0, 0, 2, 0, "b", 46, 57,
                                  0, 0, 0, -2, "b", 32, 58, 0, 0, 0, 2, "b",
                                  42, 59, 0, 0, 0, 0, "b", 36, 60, 0, 0, 0, 0,
                                  "b"))
})

test_that("4.2 Four Cont. Pred. One. Disc. Pred. Central Composite Design Summary table results match", {
  table <- results[["results"]][["doeRsmDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2, 2, 30, 1, 4, 4, 1, "Value", 60))
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
results <- runAnalysis("doeResponseSurfaceMethodology", "debug.csv", options)

test_that("5.1 Three Cont. Pred. Face Centered Central Composite Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(15, 1, -1, -1, -1, 19, 2, 1, -1, -1, 14, 3,
                                  -1, 1, -1, 3, 4, 1, 1, -1, 10, 5, -1, -1, 1,
                                  2, 6, 1, -1, 1, 6, 7, -1, 1, 1, 11, 8, 1, 1,
                                  1, 5, 9, 0, 0, 0, 4, 10, 0, 0, 0, 18, 11, 0,
                                  0, 0, 9, 12, 0, 0, 0, 20, 13, -1, 0, 0, 17,
                                  14, 1, 0, 0, 12, 15, 0, -1, 0, 7, 16, 0, 1,
                                  0, 16, 17, 0, 0, -1, 1, 18, 0, 0, 1, 8, 19,
                                  0, 0, 0, 13, 20, 0, 0, 0))
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
results <- runAnalysis("doeResponseSurfaceMethodology", "debug.csv", options)



test_that("6.1 Four Cont. Pred. Face Centered Central Composite Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(15, 1, -1, -1, -1, -1, 19, 2, 1, -1, -1, -1,
                                  14, 3, -1, 1, -1, -1, 3, 4, 1, 1, -1, -1, 10,
                                  5, -1, -1, 1, -1, 18, 6, 1, -1, 1, -1, 22, 7,
                                  -1, 1, 1, -1, 11, 8, 1, 1, 1, -1, 5, 9, -1,
                                  -1, -1, 1, 20, 10, 1, -1, -1, 1, 28, 11, -1,
                                  1, -1, 1, 24, 12, 1, 1, -1, 1, 9, 13, -1, -1,
                                  1, 1, 27, 14, 1, -1, 1, 1, 8, 15, -1, 1, 1,
                                  1, 26, 16, 1, 1, 1, 1, 7, 17, 0, 0, 0, 0, 30,
                                  18, 0, 0, 0, 0, 25, 19, 0, 0, 0, 0, 17, 20,
                                  0, 0, 0, 0, 4, 21, -1, 0, 0, 0, 1, 22, 1, 0,
                                  0, 0, 23, 23, 0, -1, 0, 0, 21, 24, 0, 1, 0,
                                  0, 29, 25, 0, 0, -1, 0, 13, 26, 0, 0, 1, 0,
                                  16, 27, 0, 0, 0, -1, 2, 28, 0, 0, 0, 1, 12,
                                  29, 0, 0, 0, 0, 6, 30, 0, 0, 0, 0))
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
results <- runAnalysis("doeResponseSurfaceMethodology", "debug.csv", options)

test_that("7.1 Three Cont. Pred. Box-Behnken Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(15, 1, -1, -1, 0, 3, 2, 1, -1, 0, 14, 3, -1,
                                  1, 0, 10, 4, 1, 1, 0, 2, 5, -1, 0, -1, 6, 6,
                                  1, 0, -1, 5, 7, -1, 0, 1, 4, 8, 1, 0, 1, 12,
                                  9, 0, -1, -1, 7, 10, 0, 1, -1, 1, 11, 0, -1,
                                  1, 11, 12, 0, 1, 1, 13, 13, 0, 0, 0, 8, 14,
                                  0, 0, 0, 9, 15, 0, 0, 0))
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
results <- runAnalysis("doeResponseSurfaceMethodology", "debug.csv", options)


test_that("8.1 Four Cont. Pred. Box-Behnken Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 31, 1, -1, -1, 0, 0, 1, 15, 2, 1, -1, 0,
                                  0, 1, 19, 3, -1, 1, 0, 0, 1, 14, 4, 1, 1, 0,
                                  0, 1, 3, 5, 0, 0, -1, -1, 1, 10, 6, 0, 0, 1,
                                  -1, 1, 18, 7, 0, 0, -1, 1, 1, 22, 8, 0, 0, 1,
                                  1, 1, 11, 9, 0, 0, 0, 0, 1, 5, 10, 0, 0, 0,
                                  0, 1, 20, 11, 0, 0, 0, 0, 2, 30, 12, -1, 0,
                                  0, -1, 2, 24, 13, 1, 0, 0, -1, 2, 33, 14, -1,
                                  0, 0, 1, 2, 9, 15, 1, 0, 0, 1, 2, 29, 16, 0,
                                  -1, -1, 0, 2, 8, 17, 0, 1, -1, 0, 2, 28, 18,
                                  0, -1, 1, 0, 2, 7, 19, 0, 1, 1, 0, 2, 16, 20,
                                  0, 0, 0, 0, 2, 23, 21, 0, 0, 0, 0, 2, 27, 22,
                                  0, 0, 0, 0, 3, 4, 23, -1, 0, -1, 0, 3, 1, 24,
                                  1, 0, -1, 0, 3, 32, 25, -1, 0, 1, 0, 3, 21,
                                  26, 1, 0, 1, 0, 3, 25, 27, 0, -1, 0, -1, 3,
                                  2, 28, 0, 1, 0, -1, 3, 17, 29, 0, -1, 0, 1,
                                  3, 12, 30, 0, 1, 0, 1, 3, 26, 31, 0, 0, 0, 0,
                                  3, 13, 32, 0, 0, 0, 0, 3, 6, 33, 0, 0, 0, 0))
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
results <- runAnalysis("doeResponseSurfaceMethodology", "debug.csv", options)

test_that("9.1 Five Cont. Pred. Box-Behnken Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 31, 1, -1, -1, 0, 0, 0, 1, 15, 2, 1, -1,
                                  0, 0, 0, 1, 14, 3, -1, 1, 0, 0, 0, 1, 3, 4,
                                  1, 1, 0, 0, 0, 1, 42, 5, -1, 0, -1, 0, 0, 1,
                                  37, 6, 1, 0, -1, 0, 0, 1, 44, 7, -1, 0, 1, 0,
                                  0, 1, 25, 8, 1, 0, 1, 0, 0, 1, 26, 9, 0, 0,
                                  -1, -1, 0, 1, 27, 10, 0, 0, 1, -1, 0, 1, 5,
                                  11, 0, 0, -1, 1, 0, 1, 41, 12, 0, 0, 1, 1, 0,
                                  1, 28, 13, 0, 0, 0, -1, -1, 1, 9, 14, 0, 0,
                                  0, 1, -1, 1, 29, 15, 0, 0, 0, -1, 1, 1, 43,
                                  16, 0, 0, 0, 1, 1, 1, 8, 17, 0, -1, 0, 0, -1,
                                  1, 38, 18, 0, 1, 0, 0, -1, 1, 7, 19, 0, -1,
                                  0, 0, 1, 1, 10, 20, 0, 1, 0, 0, 1, 1, 33, 21,
                                  0, 0, 0, 0, 0, 1, 19, 22, 0, 0, 0, 0, 0, 1,
                                  4, 23, 0, 0, 0, 0, 0, 2, 40, 24, -1, 0, 0,
                                  -1, 0, 2, 17, 25, 1, 0, 0, -1, 0, 2, 11, 26,
                                  -1, 0, 0, 1, 0, 2, 34, 27, 1, 0, 0, 1, 0, 2,
                                  12, 28, -1, 0, 0, 0, -1, 2, 45, 29, 1, 0, 0,
                                  0, -1, 2, 35, 30, -1, 0, 0, 0, 1, 2, 13, 31,
                                  1, 0, 0, 0, 1, 2, 20, 32, 0, -1, -1, 0, 0, 2,
                                  32, 33, 0, 1, -1, 0, 0, 2, 23, 34, 0, -1, 1,
                                  0, 0, 2, 22, 35, 0, 1, 1, 0, 0, 2, 18, 36, 0,
                                  -1, 0, -1, 0, 2, 36, 37, 0, 1, 0, -1, 0, 2,
                                  21, 38, 0, -1, 0, 1, 0, 2, 39, 39, 0, 1, 0,
                                  1, 0, 2, 46, 40, 0, 0, -1, 0, -1, 2, 6, 41,
                                  0, 0, 1, 0, -1, 2, 1, 42, 0, 0, -1, 0, 1, 2,
                                  2, 43, 0, 0, 1, 0, 1, 2, 30, 44, 0, 0, 0, 0,
                                  0, 2, 16, 45, 0, 0, 0, 0, 0, 2, 24, 46, 0, 0,
                                  0, 0, 0))
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
  list(levels = c("Row 0"), name = "data 1", values = c("G")),
  list(levels = c("Row 0"), name = "data 2", values = c("a")),
  list(levels = c("Row 0"), name = "data 3", values = c("b")))
options$selectedDesign2 <- list( # give design list, always three lists, number of values gives number of designs
  list(levels = c("Row 0"), name = "data 1", values = c(54)), # runs
  list(levels = c("Row 0"), name = "data 2", values = c(1)), # blocks
  list(levels = c("Row 0"), name = "data 3", values = c(6)) # centre points
)
options$selectedRow <- 0 # select design
results <- runAnalysis("doeResponseSurfaceMethodology", "debug.csv", options)


test_that("10.1 Six Cont. Pred. One Disc. Pred. Box-Behnken Design table results match", {
  table <- results[["results"]][["designTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(31, 1, -1, -1, 0, -1, 0, 0, "a", 15, 2, 1,
                                  -1, 0, -1, 0, 0, "a", 51, 3, -1, 1, 0, -1, 0,
                                  0, "a", 14, 4, 1, 1, 0, -1, 0, 0, "a", 3, 5,
                                  -1, -1, 0, 1, 0, 0, "a", 42, 6, 1, -1, 0, 1,
                                  0, 0, "a", 43, 7, -1, 1, 0, 1, 0, 0, "a", 37,
                                  8, 1, 1, 0, 1, 0, 0, "a", 52, 9, 0, -1, -1,
                                  0, -1, 0, "a", 25, 10, 0, 1, -1, 0, -1, 0,
                                  "a", 26, 11, 0, -1, 1, 0, -1, 0, "a", 27, 12,
                                  0, 1, 1, 0, -1, 0, "a", 5, 13, 0, -1, -1, 0,
                                  1, 0, "a", 48, 14, 0, 1, -1, 0, 1, 0, "a",
                                  28, 15, 0, -1, 1, 0, 1, 0, "a", 9, 16, 0, 1,
                                  1, 0, 1, 0, "a", 29, 17, 0, 0, -1, -1, 0, -1,
                                  "a", 35, 18, 0, 0, 1, -1, 0, -1, "a", 8, 19,
                                  0, 0, -1, 1, 0, -1, "a", 44, 20, 0, 0, 1, 1,
                                  0, -1, "a", 7, 21, 0, 0, -1, -1, 0, 1, "a",
                                  39, 22, 0, 0, 1, -1, 0, 1, "a", 30, 23, 0, 0,
                                  -1, 1, 0, 1, "a", 19, 24, 0, 0, 1, 1, 0, 1,
                                  "a", 4, 25, -1, 0, 0, -1, -1, 0, "a", 46, 26,
                                  1, 0, 0, -1, -1, 0, "a", 17, 27, -1, 0, 0, 1,
                                  -1, 0, "a", 11, 28, 1, 0, 0, 1, -1, 0, "a",
                                  34, 29, -1, 0, 0, -1, 1, 0, "a", 21, 30, 1,
                                  0, 0, -1, 1, 0, "a", 12, 31, -1, 0, 0, 1, 1,
                                  0, "a", 53, 32, 1, 0, 0, 1, 1, 0, "a", 10,
                                  33, 0, -1, 0, 0, -1, -1, "a", 13, 34, 0, 1,
                                  0, 0, -1, -1, "a", 47, 35, 0, -1, 0, 0, 1,
                                  -1, "a", 33, 36, 0, 1, 0, 0, 1, -1, "a", 54,
                                  37, 0, -1, 0, 0, -1, 1, "a", 22, 38, 0, 1, 0,
                                  0, -1, 1, "a", 20, 39, 0, -1, 0, 0, 1, 1,
                                  "a", 41, 40, 0, 1, 0, 0, 1, 1, "a", 24, 41,
                                  -1, 0, -1, 0, 0, -1, "a", 49, 42, 1, 0, -1,
                                  0, 0, -1, "a", 16, 43, -1, 0, 1, 0, 0, -1,
                                  "a", 45, 44, 1, 0, 1, 0, 0, -1, "a", 6, 45,
                                  -1, 0, -1, 0, 0, 1, "a", 18, 46, 1, 0, -1, 0,
                                  0, 1, "a", 2, 47, -1, 0, 1, 0, 0, 1, "a", 23,
                                  48, 1, 0, 1, 0, 0, 1, "a", 38, 49, 0, 0, 0,
                                  0, 0, 0, "a", 32, 50, 0, 0, 0, 0, 0, 0, "a",
                                  50, 51, 0, 0, 0, 0, 0, 0, "a", 1, 52, 0, 0,
                                  0, 0, 0, 0, "a", 36, 53, 0, 0, 0, 0, 0, 0,
                                  "a", 40, 54, 0, 0, 0, 0, 0, 0, "a", 85, 55,
                                  -1, -1, 0, -1, 0, 0, "b", 69, 56, 1, -1, 0,
                                  -1, 0, 0, "b", 105, 57, -1, 1, 0, -1, 0, 0,
                                  "b", 68, 58, 1, 1, 0, -1, 0, 0, "b", 57, 59,
                                  -1, -1, 0, 1, 0, 0, "b", 96, 60, 1, -1, 0, 1,
                                  0, 0, "b", 97, 61, -1, 1, 0, 1, 0, 0, "b",
                                  91, 62, 1, 1, 0, 1, 0, 0, "b", 106, 63, 0,
                                  -1, -1, 0, -1, 0, "b", 79, 64, 0, 1, -1, 0,
                                  -1, 0, "b", 80, 65, 0, -1, 1, 0, -1, 0, "b",
                                  81, 66, 0, 1, 1, 0, -1, 0, "b", 59, 67, 0,
                                  -1, -1, 0, 1, 0, "b", 102, 68, 0, 1, -1, 0,
                                  1, 0, "b", 82, 69, 0, -1, 1, 0, 1, 0, "b",
                                  63, 70, 0, 1, 1, 0, 1, 0, "b", 83, 71, 0, 0,
                                  -1, -1, 0, -1, "b", 89, 72, 0, 0, 1, -1, 0,
                                  -1, "b", 62, 73, 0, 0, -1, 1, 0, -1, "b", 98,
                                  74, 0, 0, 1, 1, 0, -1, "b", 61, 75, 0, 0, -1,
                                  -1, 0, 1, "b", 93, 76, 0, 0, 1, -1, 0, 1,
                                  "b", 84, 77, 0, 0, -1, 1, 0, 1, "b", 73, 78,
                                  0, 0, 1, 1, 0, 1, "b", 58, 79, -1, 0, 0, -1,
                                  -1, 0, "b", 100, 80, 1, 0, 0, -1, -1, 0, "b",
                                  71, 81, -1, 0, 0, 1, -1, 0, "b", 65, 82, 1,
                                  0, 0, 1, -1, 0, "b", 88, 83, -1, 0, 0, -1, 1,
                                  0, "b", 75, 84, 1, 0, 0, -1, 1, 0, "b", 66,
                                  85, -1, 0, 0, 1, 1, 0, "b", 107, 86, 1, 0, 0,
                                  1, 1, 0, "b", 64, 87, 0, -1, 0, 0, -1, -1,
                                  "b", 67, 88, 0, 1, 0, 0, -1, -1, "b", 101,
                                  89, 0, -1, 0, 0, 1, -1, "b", 87, 90, 0, 1, 0,
                                  0, 1, -1, "b", 108, 91, 0, -1, 0, 0, -1, 1,
                                  "b", 76, 92, 0, 1, 0, 0, -1, 1, "b", 74, 93,
                                  0, -1, 0, 0, 1, 1, "b", 95, 94, 0, 1, 0, 0,
                                  1, 1, "b", 78, 95, -1, 0, -1, 0, 0, -1, "b",
                                  103, 96, 1, 0, -1, 0, 0, -1, "b", 70, 97, -1,
                                  0, 1, 0, 0, -1, "b", 99, 98, 1, 0, 1, 0, 0,
                                  -1, "b", 60, 99, -1, 0, -1, 0, 0, 1, "b", 72,
                                  100, 1, 0, -1, 0, 0, 1, "b", 56, 101, -1, 0,
                                  1, 0, 0, 1, "b", 77, 102, 1, 0, 1, 0, 0, 1,
                                  "b", 92, 103, 0, 0, 0, 0, 0, 0, "b", 86, 104,
                                  0, 0, 0, 0, 0, 0, "b", 104, 105, 0, 0, 0, 0,
                                  0, 0, "b", 55, 106, 0, 0, 0, 0, 0, 0, "b",
                                  90, 107, 0, 0, 0, 0, 0, 0, "b", 94, 108, 0,
                                  0, 0, 0, 0, 0, "b"))
})

test_that("10.2 Six Cont. Pred. One Disc. Pred. Box-Behnken Design Summary table results match", {
  table <- results[["results"]][["doeRsmDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 54, 1, 6, 6, 1, "Value", 1, 108))
})

# Axial distance helper ####
test_that("Orthogonal axial distance uses the fractional core size and all k axial pairs", {
  # expected values equal rsm::ccd(..., alpha = "orthogonal"); half fractions use generator x5 ~ x1*x2*x3*x4
  computeAlpha <- jaspQualityControl:::.doeRsmComputeAlpha
  # full factorial cores (unchanged)
  expect_equal(computeAlpha(k = 3, designtype = 0, centerPointsInCube = 4, centerPointsInStar = 2, alphaType = "orthogonal"), 1.632993, tolerance = 1e-6)
  expect_equal(computeAlpha(k = 5, designtype = 0, centerPointsInCube = 8, centerPointsInStar = 4, alphaType = "orthogonal"), 2.366432, tolerance = 1e-6)
  # half fraction 2^(5-1) core
  expect_equal(computeAlpha(k = 5, designtype = 1, centerPointsInCube = 8, centerPointsInStar = 4, alphaType = "orthogonal"), 2.160247, tolerance = 1e-6)
  expect_equal(computeAlpha(k = 5, designtype = 1, centerPointsInCube = 6, centerPointsInStar = 1, alphaType = "orthogonal"), 2)
  # rotatable: 2^((k - p) / 4)
  expect_equal(computeAlpha(k = 5, designtype = 1, centerPointsInCube = 8, centerPointsInStar = 4, alphaType = "rotatable"), 2)
})
