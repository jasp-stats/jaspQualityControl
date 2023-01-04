context("DoE Factorial Design")

options <- analysisOptions("doeFactorial")
options$actualExporter <- FALSE
options$categoricalVariables <- list(list(
  levels = c("Row 0", "Row 1", "Row 2"), name = "data 1",
  values = c("A", "B", "C")
), list(levels = c(
  "Row 0", "Row 1",
  "Row 2"
), name = "data 2", values = c("a", "a", "a")), list(levels = c(
  "Row 0",
  "Row 1", "Row 2"
), name = "data 3", values = c("b", "b", "b")))
options$codedOutput <- TRUE
options$displayDesign <- TRUE
options$exportDesignFile <- ""
options$factorialType <- "factorialTypeDefault"
options$runOrder <- "runOrderStandard"
options$selectedDesign2 <- list(list(levels = c("Row 0", "Row 1"), name = "data 1", values = c(
  4,
  8
)), list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(
  0,
  0
)))
options$selectedRow <- 1
options$setSeed <- TRUE
set.seed(1)
dataset <- NULL
results <- runAnalysis("doeFactorial", dataset, options)


test_that("Factorial Design table results match", {
  table <- results[["results"]][["displayDesign"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(
      -1, -1, -1, 5, 1, 1, -1, -1, 7, 2, -1, 1, -1, 1, 3, 1, 1, -1,
      3, 4, -1, -1, 1, 4, 5, 1, -1, 1, 8, 6, -1, 1, 1, 6, 7, 1, 1,
      1, 2, 8
    )
  )
})

test_that("Design Summary table results match", {
  table <- results[["results"]][["doeFactorialDesignSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(
    table,
    list(1, 8, 3, 0, 0, 1, "Value", 1, 8)
  )
})
