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
                                 list(31, 1, -1, -1, -1, "a", "a", 79, 2, 1, -1, -1, "a", "a", 51, 3, -1, 
                                  1, -1, "a", "a", 14, 4, 1, 1, -1, "a", "a", 67, 5, -1, -1, 1, "a", 
                                  "a", 42, 6, 1, -1, 1, "a", "a", 50, 7, -1, 1, 1, "a", "a", 43, 8, 1, 
                                  1, 1, "a", "a", 77, 9, 0, 0, 0, "a", "a", 25, 10, 0, 0, 0, "a", "a", 
                                  69, 11, 0, 0, 0, "a", "a", 57, 12, 0, 0, 0, "a", "a", 9, 13, -1.63299316185545, 
                                  0, 0, "a", "a", 26, 14, 1.63299316185545, 0, 0, "a", "a", 7, 15, 0, 
                                  -1.63299316185545, 0, "a", "a", 75, 16, 0, 1.63299316185545, 0, "a", 
                                  "a", 68, 17, 0, 0, -1.63299316185545, "a", "a", 62, 18, 0, 0, 1.63299316185545, 
                                  "a", "a", 19, 19, 0, 0, 0, "a", "a", 36, 20, 0, 0, 0, "a", "a", 72, 
                                  21, -1, -1, -1, "b", "a", 17, 22, 1, -1, -1, "b", "a", 73, 23, -1, 
                                  1, -1, "b", "a", 39, 24, 1, 1, -1, "b", "a", 53, 25, -1, -1, 1, "b", 
                                  "a", 12, 26, 1, -1, 1, "b", "a", 15, 27, -1, 1, 1, "b", "a", 32, 28, 
                                  1, 1, 1, "b", "a", 65, 29, 0, 0, 0, "b", "a", 45, 30, 0, 0, 0, "b", 
                                  "a", 66, 31, 0, 0, 0, "b", "a", 64, 32, 0, 0, 0, "b", "a", 41, 33, 
                                  -1.63299316185545, 0, 0, "b", "a", 10, 34, 1.63299316185545, 0, 0, 
                                  "b", "a", 23, 35, 0, -1.63299316185545, 0, "b", "a", 27, 36, 0, 1.63299316185545, 
                                  0, "b", "a", 74, 37, 0, 0, -1.63299316185545, "b", "a", 78, 38, 0, 
                                  0, 1.63299316185545, "b", "a", 56, 39, 0, 0, 0, "b", "a", 38, 40, 0, 
                                  0, 0, "b", "a", 71, 41, -1, -1, -1, "a", "b", 34, 42, 1, -1, -1, "a", 
                                  "b", 29, 43, -1, 1, -1, "a", "b", 5, 44, 1, 1, -1, "a", "b", 8, 45, 
                                  -1, -1, 1, "a", "b", 55, 46, 1, -1, 1, "a", "b", 13, 47, -1, 1, 1, 
                                  "a", "b", 18, 48, 1, 1, 1, "a", "b", 1, 49, 0, 0, 0, "a", "b", 40, 
                                  50, 0, 0, 0, "a", "b", 58, 51, 0, 0, 0, "a", "b", 80, 52, 0, 0, 0, 
                                  "a", "b", 6, 53, -1.63299316185545, 0, 0, "a", "b", 21, 54, 1.63299316185545, 
                                  0, 0, "a", "b", 54, 55, 0, -1.63299316185545, 0, "a", "b", 49, 56, 
                                  0, 1.63299316185545, 0, "a", "b", 76, 57, 0, 0, -1.63299316185545, 
                                  "a", "b", 16, 58, 0, 0, 1.63299316185545, "a", "b", 20, 59, 0, 0, 0, 
                                  "a", "b", 28, 60, 0, 0, 0, "a", "b", 11, 61, -1, -1, -1, "b", "b", 
                                  61, 62, 1, -1, -1, "b", "b", 44, 63, -1, 1, -1, "b", "b", 46, 64, 1, 
                                  1, -1, "b", "b", 52, 65, -1, -1, 1, "b", "b", 30, 66, 1, -1, 1, "b", 
                                  "b", 2, 67, -1, 1, 1, "b", "b", 59, 68, 1, 1, 1, "b", "b", 60, 69, 
                                  0, 0, 0, "b", "b", 4, 70, 0, 0, 0, "b", "b", 37, 71, 0, 0, 0, "b", 
                                  "b", 24, 72, 0, 0, 0, "b", "b", 33, 73, -1.63299316185545, 0, 0, "b", 
                                  "b", 3, 74, 1.63299316185545, 0, 0, "b", "b", 70, 75, 0, -1.63299316185545, 
                                  0, "b", "b", 22, 76, 0, 1.63299316185545, 0, "b", "b", 35, 77, 0, 0, 
                                  -1.63299316185545, "b", "b", 48, 78, 0, 0, 1.63299316185545, "b", "b", 
                                  47, 79, 0, 0, 0, "b", "b", 63, 80, 0, 0, 0, "b", "b"))
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
                                 list(31, 1, -1, -1, -1, -1, "a", 15, 2, 1, -1, -1, -1, "a", 51, 3, -1, 
                                  1, -1, -1, "a", 14, 4, 1, 1, -1, -1, "a", 3, 5, -1, -1, 1, -1, "a", 
                                  42, 6, 1, -1, 1, -1, "a", 50, 7, -1, 1, 1, -1, "a", 43, 8, 1, 1, 1, 
                                  -1, "a", 37, 9, -1, -1, -1, 1, "a", 57, 10, 1, -1, -1, 1, "a", 25, 
                                  11, -1, 1, -1, 1, "a", 26, 12, 1, 1, -1, 1, "a", 27, 13, -1, -1, 1, 
                                  1, "a", 5, 14, 1, -1, 1, 1, "a", 48, 15, -1, 1, 1, 1, "a", 28, 16, 
                                  1, 1, 1, 1, "a", 9, 17, 0, 0, 0, 0, "a", 29, 18, 0, 0, 0, 0, "a", 35, 
                                  19, 0, 0, 0, 0, "a", 8, 20, 0, 0, 0, 0, "a", 49, 21, -2, 0, 0, 0, "a", 
                                  7, 22, 2, 0, 0, 0, "a", 44, 23, 0, -2, 0, 0, "a", 19, 24, 0, 2, 0, 
                                  0, "a", 36, 25, 0, 0, -2, 0, "a", 58, 26, 0, 0, 2, 0, "a", 17, 27, 
                                  0, 0, 0, -2, "a", 12, 28, 0, 0, 0, 2, "a", 59, 29, 0, 0, 0, 0, "a", 
                                  10, 30, 0, 0, 0, 0, "a", 13, 31, -1, -1, -1, -1, "b", 39, 32, 1, -1, 
                                  -1, -1, "b", 38, 33, -1, 1, -1, -1, "b", 45, 34, 1, 1, -1, -1, "b", 
                                  60, 35, -1, -1, 1, -1, "b", 23, 36, 1, -1, 1, -1, "b", 21, 37, -1, 
                                  1, 1, -1, "b", 53, 38, 1, 1, 1, -1, "b", 24, 39, -1, -1, -1, 1, "b", 
                                  6, 40, 1, -1, -1, 1, "b", 2, 41, -1, 1, -1, 1, "b", 47, 42, 1, 1, -1, 
                                  1, "b", 41, 43, -1, -1, 1, 1, "b", 33, 44, 1, -1, 1, 1, "b", 32, 45, 
                                  -1, 1, 1, 1, "b", 30, 46, 1, 1, 1, 1, "b", 20, 47, 0, 0, 0, 0, "b", 
                                  1, 48, 0, 0, 0, 0, "b", 46, 49, 0, 0, 0, 0, "b", 11, 50, 0, 0, 0, 0, 
                                  "b", 34, 51, -2, 0, 0, 0, "b", 22, 52, 2, 0, 0, 0, "b", 52, 53, 0, 
                                  -2, 0, 0, "b", 54, 54, 0, 2, 0, 0, "b", 16, 55, 0, 0, -2, 0, "b", 55, 
                                  56, 0, 0, 2, 0, "b", 4, 57, 0, 0, 0, -2, "b", 56, 58, 0, 0, 0, 2, "b", 
                                  18, 59, 0, 0, 0, 0, "b", 40, 60, 0, 0, 0, 0, "b"))
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
                                 list(31, 1, -1, -1, 0, -1, 0, 0, "a", 79, 2, 1, -1, 0, -1, 0, 0, "a", 51, 
                                  3, -1, 1, 0, -1, 0, 0, "a", 14, 4, 1, 1, 0, -1, 0, 0, "a", 67, 5, -1, 
                                  -1, 0, 1, 0, 0, "a", 42, 6, 1, -1, 0, 1, 0, 0, "a", 50, 7, -1, 1, 0, 
                                  1, 0, 0, "a", 43, 8, 1, 1, 0, 1, 0, 0, "a", 105, 9, 0, -1, -1, 0, -1, 
                                  0, "a", 25, 10, 0, 1, -1, 0, -1, 0, "a", 90, 11, 0, -1, 1, 0, -1, 0, 
                                  "a", 91, 12, 0, 1, 1, 0, -1, 0, "a", 69, 13, 0, -1, -1, 0, 1, 0, "a", 
                                  97, 14, 0, 1, -1, 0, 1, 0, "a", 57, 15, 0, -1, 1, 0, 1, 0, "a", 92, 
                                  16, 0, 1, 1, 0, 1, 0, "a", 9, 17, 0, 0, -1, -1, 0, -1, "a", 72, 18, 
                                  0, 0, 1, -1, 0, -1, "a", 26, 19, 0, 0, -1, 1, 0, -1, "a", 7, 20, 0, 
                                  0, 1, 1, 0, -1, "a", 103, 21, 0, 0, -1, -1, 0, 1, "a", 93, 22, 0, 0, 
                                  1, -1, 0, 1, "a", 83, 23, 0, 0, -1, 1, 0, 1, "a", 36, 24, 0, 0, 1, 
                                  1, 0, 1, "a", 78, 25, -1, 0, 0, -1, -1, 0, "a", 81, 26, 1, 0, 0, -1, 
                                  -1, 0, "a", 101, 27, -1, 0, 0, 1, -1, 0, "a", 76, 28, 1, 0, 0, 1, -1, 
                                  0, "a", 15, 29, -1, 0, 0, -1, 1, 0, "a", 32, 30, 1, 0, 0, -1, 1, 0, 
                                  "a", 89, 31, -1, 0, 0, 1, 1, 0, "a", 87, 32, 1, 0, 0, 1, 1, 0, "a", 
                                  41, 33, 0, -1, 0, 0, -1, -1, "a", 74, 34, 0, 1, 0, 0, -1, -1, "a", 
                                  23, 35, 0, -1, 0, 0, 1, -1, "a", 27, 36, 0, 1, 0, 0, 1, -1, "a", 60, 
                                  37, 0, -1, 0, 0, -1, 1, "a", 53, 38, 0, 1, 0, 0, -1, 1, "a", 84, 39, 
                                  0, -1, 0, 0, 1, 1, "a", 71, 40, 0, 1, 0, 0, 1, 1, "a", 73, 41, -1, 
                                  0, -1, 0, 0, -1, "a", 38, 42, 1, 0, -1, 0, 0, -1, "a", 34, 43, -1, 
                                  0, 1, 0, 0, -1, "a", 63, 44, 1, 0, 1, 0, 0, -1, "a", 13, 45, -1, 0, 
                                  -1, 0, 0, 1, "a", 18, 46, 1, 0, -1, 0, 0, 1, "a", 33, 47, -1, 0, 1, 
                                  0, 0, 1, "a", 94, 48, 1, 0, 1, 0, 0, 1, "a", 68, 49, 0, 0, 0, 0, 0, 
                                  0, "a", 99, 50, 0, 0, 0, 0, 0, 0, "a", 104, 51, 0, 0, 0, 0, 0, 0, "a", 
                                  21, 52, 0, 0, 0, 0, 0, 0, "a", 80, 53, 0, 0, 0, 0, 0, 0, "a", 86, 54, 
                                  0, 0, 0, 0, 0, 0, "a", 47, 55, -1, -1, 0, -1, 0, 0, "b", 98, 56, 1, 
                                  -1, 0, -1, 0, 0, "b", 108, 57, -1, 1, 0, -1, 0, 0, "b", 16, 58, 1, 
                                  1, 0, -1, 0, 0, "b", 30, 59, -1, -1, 0, 1, 0, 0, "b", 6, 60, 1, -1, 
                                  0, 1, 0, 0, "b", 82, 61, -1, 1, 0, 1, 0, 0, "b", 8, 62, 1, 1, 0, 1, 
                                  0, 0, "b", 22, 63, 0, -1, -1, 0, -1, 0, "b", 46, 64, 0, 1, -1, 0, -1, 
                                  0, "b", 39, 65, 0, -1, 1, 0, -1, 0, "b", 52, 66, 0, 1, 1, 0, -1, 0, 
                                  "b", 17, 67, 0, -1, -1, 0, 1, 0, "b", 66, 68, 0, 1, -1, 0, 1, 0, "b", 
                                  4, 69, 0, -1, 1, 0, 1, 0, "b", 64, 70, 0, 1, 1, 0, 1, 0, "b", 5, 71, 
                                  0, 0, -1, -1, 0, -1, "b", 59, 72, 0, 0, 1, -1, 0, -1, "b", 45, 73, 
                                  0, 0, -1, 1, 0, -1, "b", 37, 74, 0, 0, 1, 1, 0, -1, "b", 107, 75, 0, 
                                  0, -1, -1, 0, 1, "b", 35, 76, 0, 0, 1, -1, 0, 1, "b", 75, 77, 0, 0, 
                                  -1, 1, 0, 1, "b", 3, 78, 0, 0, 1, 1, 0, 1, "b", 54, 79, -1, 0, 0, -1, 
                                  -1, 0, "b", 106, 80, 1, 0, 0, -1, -1, 0, "b", 12, 81, -1, 0, 0, 1, 
                                  -1, 0, "b", 62, 82, 1, 0, 0, 1, -1, 0, "b", 100, 83, -1, 0, 0, -1, 
                                  1, 0, "b", 48, 84, 1, 0, 0, -1, 1, 0, "b", 96, 85, -1, 0, 0, 1, 1, 
                                  0, "b", 70, 86, 1, 0, 0, 1, 1, 0, "b", 95, 87, 0, -1, 0, 0, -1, -1, 
                                  "b", 56, 88, 0, 1, 0, 0, -1, -1, "b", 58, 89, 0, -1, 0, 0, 1, -1, "b", 
                                  102, 90, 0, 1, 0, 0, 1, -1, "b", 10, 91, 0, -1, 0, 0, -1, 1, "b", 65, 
                                  92, 0, 1, 0, 0, -1, 1, "b", 28, 93, 0, -1, 0, 0, 1, 1, "b", 2, 94, 
                                  0, 1, 0, 0, 1, 1, "b", 88, 95, -1, 0, -1, 0, 0, -1, "b", 49, 96, 1, 
                                  0, -1, 0, 0, -1, "b", 29, 97, -1, 0, 1, 0, 0, -1, "b", 40, 98, 1, 0, 
                                  1, 0, 0, -1, "b", 1, 99, -1, 0, -1, 0, 0, 1, "b", 44, 100, 1, 0, -1, 
                                  0, 0, 1, "b", 85, 101, -1, 0, 1, 0, 0, 1, "b", 77, 102, 1, 0, 1, 0, 
                                  0, 1, "b", 19, 103, 0, 0, 0, 0, 0, 0, "b", 24, 104, 0, 0, 0, 0, 0, 
                                  0, "b", 55, 105, 0, 0, 0, 0, 0, 0, "b", 11, 106, 0, 0, 0, 0, 0, 0, 
                                  "b", 20, 107, 0, 0, 0, 0, 0, 0, "b", 61, 108, 0, 0, 0, 0, 0, 0, "b"))
})

test_that("10.2 Six Cont. Pred. One Disc. Pred. Box-Behnken Design Summary table results match", {
  table <- results[["results"]][["doeRsmDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 54, 1, 6, 6, 1, "Value", 1, 108))
})

## Box-Behnken replicates ####
# the design has to contain every replicate, i.e., as many rows as the Design Summary reports as Total runs
bbdReplicatesRun <- function(k, replicates, runs, blocks, centerpoints, categorical = FALSE, runOrder = "runOrderStandard") {
  opts <- analysisOptions("doeResponseSurfaceMethodology")
  opts$actualExporter <- FALSE
  opts$alphaType <- "default"
  opts$centerPointType <- "default"
  opts$codedOutput <- TRUE
  opts$setSeed <- TRUE
  opts$seed <- 123
  opts$designType <- "boxBehnkenDesign"
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
  opts$selectedDesign2 <- list(
    list(levels = "Row 0", name = "data 1", values = runs),
    list(levels = "Row 0", name = "data 2", values = blocks),
    list(levels = "Row 0", name = "data 3", values = centerpoints)
  )
  opts$selectedRow <- 0
  opts$replicates <- replicates
  res <- runAnalysis("doeResponseSurfaceMethodology", "debug.csv", opts)
  design <- do.call(rbind, lapply(res[["results"]][["designTable"]][["data"]], as.data.frame))
  list(design = design, summary = res[["results"]][["doeRsmDesignSummaryTable"]][["data"]][[1]])
}
bbdRunKey <- function(design, k) do.call(paste, design[paste0("x", seq_len(k))])

base3 <- bbdReplicatesRun(3, 1, 15, 1, 3)
for (reps in c(2, 3)) {
  res3 <- bbdReplicatesRun(3, reps, 15, 1, 3)
  test_that(sprintf("11.%d Three Cont. Pred. Box-Behnken Design with %d replicates has Total runs rows", reps - 1, reps), {
    expect_equal(res3$summary$totalRuns, 15 * reps)
    expect_equal(nrow(res3$design), res3$summary$totalRuns)
    expect_equal(sort(res3$design$std.order), seq_len(15 * reps))
    expect_equal(sort(res3$design$run.order), seq_len(15 * reps))
    # every base run (incl. each centre point) is repeated once per replicate
    expect_equal(table(bbdRunKey(res3$design, 3)), table(bbdRunKey(base3$design, 3)) * reps, ignore_attr = TRUE)
    # replicates are not blocks: no Block column, one block, replicates are identified in their own column
    expect_false("Block" %in% names(res3$design))
    expect_equal(res3$summary$totalBlocks, 1)
    expect_equal(as.integer(table(res3$design$Replication)), rep(15L, reps))
    expect_equal(sort(unique(res3$design$Replication)), seq_len(reps))
    expect_equal(res3$design$Replication, (res3$design$std.order - 1L) %/% 15L + 1L)
    # the first replicate is the unreplicated design
    expect_equal(res3$design[res3$design$std.order <= 15, c("std.order", "x1", "x2", "x3")],
                 base3$design[c("std.order", "x1", "x2", "x3")], ignore_attr = TRUE)
  })
}

test_that("11.3 Box-Behnken Design with one replicate has no Block and Replication column", {
  expect_equal(nrow(base3$design), 15)
  expect_false(any(c("Block", "Replication") %in% names(base3$design)))
})

base4 <- bbdReplicatesRun(4, 1, 33, 3, 3)
for (reps in c(2, 3)) {
  res4 <- bbdReplicatesRun(4, reps, 33, 3, 3)
  test_that(sprintf("11.%d Four Cont. Pred. Box-Behnken Design with %d replicates has Total runs rows", reps + 2, reps), {
    expect_equal(res4$summary$totalRuns, 33 * reps)
    expect_equal(nrow(res4$design), res4$summary$totalRuns)
    expect_equal(sort(res4$design$std.order), seq_len(33 * reps))
    expect_equal(table(bbdRunKey(res4$design, 4)), table(bbdRunKey(base4$design, 4)) * reps, ignore_attr = TRUE)
    # every replicate has its own three blocks: 3 * replicates blocks of 11 runs, each inside one replicate
    expect_equal(length(unique(res4$design$Block)), res4$summary$totalBlocks)
    expect_equal(res4$summary$totalBlocks, 3 * reps)
    expect_equal(as.integer(table(res4$design$Block)), rep(11L, 3 * reps))
    expect_equal(as.integer(table(res4$design$Replication)), rep(33L, reps))
    expect_equal(as.integer(rowSums(table(res4$design$Block, res4$design$Replication) > 0)), rep(1L, 3 * reps))
    # the first replicate has the blocks of the unreplicated design
    expect_equal(as.integer(res4$design$Block[res4$design$Replication == 1]), as.integer(base4$design$Block))
  })
}

test_that("11.6 Box-Behnken Design with replicates and a discrete factor has Total runs rows", {
  res <- bbdReplicatesRun(3, 2, 15, 1, 3, categorical = TRUE)
  expect_equal(res$summary$totalRuns, 60)
  expect_equal(nrow(res$design), 60)
  expect_equal(sort(res$design$std.order), seq_len(60))
  expect_equal(as.integer(table(res$design$x_cat1)), c(30L, 30L))
})

test_that("11.7 Box-Behnken Design with replicates in random run order has Total runs rows", {
  res <- bbdReplicatesRun(4, 2, 33, 3, 3, runOrder = "runOrderRandom")
  expect_equal(nrow(res$design), res$summary$totalRuns)
  expect_equal(sort(res$design$run.order), seq_len(66))
  expect_equal(sort(res$design$std.order), seq_len(66))
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

# Random run order with discrete factors ####
# the run order is randomised over the whole design, i.e., the runs of the discrete factor levels are not grouped
rsmDiscreteRun <- function(designType = "centralCompositeDesign", runOrder = "runOrderRandom", seed = 1, levels = c("a", "b")) {
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
  opts$runOrder <- runOrder
  if (designType == "centralCompositeDesign") {
    k <- 2
    opts$selectedDesign2 <- list(
      list(levels = "Row 0", name = "data 1", values = 13), # runs
      list(levels = "Row 0", name = "data 2", values = 5),  # total
      list(levels = "Row 0", name = "data 3", values = 5),  # cube
      list(levels = "Row 0", name = "data 4", values = 0),  # axial
      list(levels = "Row 0", name = "data 5", values = 1.414) # alpha
    )
    opts$selectedRow <- 1
  } else {
    k <- 4 # blocked design
    opts$selectedDesign2 <- list(
      list(levels = "Row 0", name = "data 1", values = 27), # runs
      list(levels = "Row 0", name = "data 2", values = 3),  # blocks
      list(levels = "Row 0", name = "data 3", values = 3)   # centre points
    )
    opts$selectedRow <- 0
  }
  opts$numberOfContinuous <- k
  rows <- paste("Row", seq_len(k) - 1)
  opts$continuousVariables <- list(
    list(levels = rows, name = "data 1", values = LETTERS[seq_len(k)]),
    list(levels = rows, name = "data 2", values = rep(-1, k)),
    list(levels = rows, name = "data 3", values = rep(1, k))
  )
  opts$numberOfCategorical <- 1
  opts$categoricalVariables <- list(
    list(levels = "Row 0", name = "data 1", values = "Material"),
    list(levels = "Row 0", name = "data 2", values = levels[1]),
    list(levels = "Row 0", name = "data 3", values = levels[2])
  )
  res <- runAnalysis("doeResponseSurfaceMethodology", "debug.csv", opts)
  design <- do.call(rbind, lapply(res[["results"]][["designTable"]][["data"]], as.data.frame))
  design[["x_cat1"]] <- factor(design[["x_cat1"]], levels = levels)
  design
}
levelSwitches <- function(design) sum(diff(as.integer(design[["x_cat1"]][order(design[["run.order"]])])) != 0)

for (seed in c(1, 7, 123)) {
  designRandom <- rsmDiscreteRun(seed = seed)
  designStandard <- rsmDiscreteRun(runOrder = "runOrderStandard", seed = seed)
  n <- nrow(designRandom)

  test_that(sprintf("12.1 Random run order is a permutation over the whole design with a discrete factor (seed %d)", seed), {
    expect_equal(n, 28) # 2 x 14 runs
    expect_identical(sort(designRandom[["run.order"]]), seq_len(n))
    expect_identical(sort(designRandom[["std.order"]]), seq_len(n))
    expect_identical(designRandom[["run.order"]], seq_len(n)) # displayed in run order
  })

  test_that(sprintf("12.2 Random run order does not group the runs by discrete level (seed %d)", seed), {
    # before the fix all runs of the first level came first (one level switch, correlation 0.87)
    expect_gt(levelSwitches(designRandom), 1)
    expect_lt(abs(cor(designRandom[["run.order"]], as.integer(designRandom[["x_cat1"]]))), 0.5)
    expect_true(all(table(designRandom[["x_cat1"]][seq_len(n / 2)]) > 0))
  })

  test_that(sprintf("12.3 Standard order enumerates the expanded design with a discrete factor and shows the same runs as the random order (seed %d)", seed), {
    byStd <- designRandom[order(designRandom[["std.order"]]), ]
    rownames(byStd) <- NULL
    expect_identical(designStandard[["std.order"]], seq_len(n))
    expect_equal(byStd, designStandard) # same runs and the same run.order column, only displayed differently
    # the second level repeats the first level with the standard order offset by the base runs
    expect_equal(designStandard[["x1"]][15:28], designStandard[["x1"]][1:14])
    expect_equal(designStandard[["x2"]][15:28], designStandard[["x2"]][1:14])
    expect_equal(as.character(designStandard[["x_cat1"]]), rep(c("a", "b"), each = 14))
    # run.order is not a repeated base permutation + offset
    expect_false(identical(designStandard[["run.order"]][15:28] - 14L, designStandard[["run.order"]][1:14]))
  })
}

test_that("12.4 Blocked Box-Behnken design with a discrete factor keeps its blocks and mixes the discrete levels", {
  designRandom   <- rsmDiscreteRun("boxBehnkenDesign", seed = 1)
  designStandard <- rsmDiscreteRun("boxBehnkenDesign", runOrder = "runOrderStandard", seed = 1)
  n <- nrow(designRandom)
  byStd <- designRandom[order(designRandom[["std.order"]]), ]
  rownames(byStd) <- NULL
  expect_identical(sort(designRandom[["run.order"]]), seq_len(n))
  expect_identical(designStandard[["std.order"]], seq_len(n))
  expect_equal(byStd, designStandard)                                  # same runs, same blocks
  expect_equal(as.character(designStandard[["Block"]][1:(n / 2)]), as.character(designStandard[["Block"]][(n / 2 + 1):n]))
  expect_equal(as.vector(table(designRandom[["Block"]], designRandom[["x_cat1"]])), rep(c(11, 11, 11), 2)) # runs per block and level
  expect_gt(levelSwitches(designRandom), 1)
})
