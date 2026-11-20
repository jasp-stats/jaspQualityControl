replicatedDesignOptions <- function(blocks = "1", replications = 3, centerpoints = 0) {
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
  options$runOrder <- "runOrderStandard"
  options$selectedDesign2 <- list(list(levels = c("Row 0", "Row 1"), name = "data 1", values = c(4, 8)),
                                  list(levels = c("Row 0", "Row 1"), name = "data 2", values = c(0, 0)))
  options$selectedRow <- 1 # full factorial, 8 runs
  options$blocks <- blocks
  options$replications <- replications
  options$centerpoints <- centerpoints
  options$setSeed <- TRUE
  return(options)
}

replicatedDesignTable <- function(results) {
  table <- results[["results"]][["displayDesign"]]
  list(columns = vapply(table[["schema"]][["fields"]], function(field) field[["name"]], character(1)),
       data = do.call(rbind, lapply(table[["data"]], as.data.frame)))
}


test_that("Block identifiers stay with runs when changing display order", {
  for (centers in c(0, 2)) {
    options <- replicatedDesignOptions(blocks = "2", replications = 2, centerpoints = centers)
    standard <- replicatedDesignTable(runAnalysis("doeFactorial", "debug.csv", options))$data
    options$runOrder <- "runOrderRandom"
    random <- replicatedDesignTable(runAnalysis("doeFactorial", "debug.csv", options))$data
    standard <- standard[order(standard$ro), ]
    random <- random[order(random$ro), ]
    rownames(standard) <- rownames(random) <- NULL
    expect_equal(standard, random)
  }
})
