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


test_that("Factor names that match metadata retain their values in the table and export", {
  options <- replicatedDesignOptions(blocks = "2", replications = 2, centerpoints = 2)
  options$categoricalVariables[[1]][["values"]] <- c("Block", "Replication", "Response")
  options$categoricalVariables[[2]][["values"]][2] <- "a, low"
  options$codedOutput <- FALSE
  options$actualExporter <- TRUE
  options$exportDesignFile <- tempfile(fileext = ".csv")
  on.exit(unlink(options$exportDesignFile))
  result <- runAnalysis("doeFactorial", "debug.csv", options)
  table <- replicatedDesignTable(result)
  exported <- read.csv(options$exportDesignFile, check.names = FALSE)
  expect_equal(names(exported), c("RunOrder", "StandardOrder", "Block", "Replication", "Response", "Block.1", "Replication.1", "Response.1"))
  expect_equal(nrow(exported), 24)
  expect_true(all(exported$Block %in% c("a", "b", "center")))
  expect_true(all(exported$Replication %in% c("a, low", "b", "center")))
  expect_true("a, low" %in% exported$Replication)
  expect_true(all(exported$Response %in% c("a", "b", "center")))
  expect_equal(sort(unique(exported$Block.1)), 1:4)
  expect_equal(sort(unique(exported$Replication.1)), 1:2)
  expect_equal(ncol(table$data), 7)
  titles <- vapply(result[["results"]][["displayDesign"]][["schema"]][["fields"]], function(field) field[["title"]], character(1))
  expect_equal(titles, c("Run Order", "Standard Order", "Block", "Replication", "Response", "Block (design)", "Replication (design)"))
})
