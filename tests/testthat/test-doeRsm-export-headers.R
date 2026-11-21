test_that("response-surface worksheet uses factor names without losing metadata or responses", {
  export <- jaspQualityControl:::.doeRsmExportDesign
  opts <- list(actualExporter = TRUE, exportDesignFile = tempfile(fileext = ".csv"))
  on.exit(unlink(opts$exportDesignFile))
  design <- data.frame(run.order = 1:2, std.order = 1:2, x1 = c(-1, 1), x2 = c(1, -1), Block = 1:2, x_cat1 = c("a", "b"))
  # the entered names have priority, as in the factorial export: the design's own Block column becomes Block.1
  export(opts, design, factorNames = c("Temp, C", "Pressure", "Block"), factorIndices = c(3L, 4L, 6L))
  result <- read.csv(opts$exportDesignFile, check.names = FALSE)
  expect_identical(names(result), c("RunOrder", "StandardOrder", "Temp, C", "Pressure", "Block.1", "Block", "Response"))
  expect_equal(result[["Temp, C"]], c(-1L, 1L))
  expect_equal(result[["Block.1"]], 1:2)
  expect_identical(result[["Block"]], c("a", "b"))
  expect_true(all(is.na(result[["Response"]])))
})

test_that("CCD and Box-Behnken worksheets use displayed factor names", {
  for (type in c("centralCompositeDesign", "boxBehnkenDesign")) {
    for (nCategorical in 0:1) {
      opts <- analysisOptions("doeResponseSurfaceMethodology")
      opts$numberOfContinuous <- 3L
      opts$continuousVariables <- list(
        list(levels = paste("Row", 0:2), name = "data 1", values = c("Temperature", "Pressure", "Time")),
        list(levels = paste("Row", 0:2), name = "data 2", values = rep(-1, 3)),
        list(levels = paste("Row", 0:2), name = "data 3", values = rep(1, 3)))
      opts$numberOfCategorical <- nCategorical
      opts$categoricalVariables <- if (nCategorical == 0) {
        list(list(levels = list(), name = "data 1", values = list()), # an empty table, as sent by the GUI
             list(levels = list(), name = "data 2", values = list()),
             list(levels = list(), name = "data 3", values = list()))
      } else {
        list(list(levels = "Row 0", name = "data 1", values = "Material"),
             list(levels = "Row 0", name = "data 2", values = "a"),
             list(levels = "Row 0", name = "data 3", values = "b"))
      }
      opts$designType <- type
      opts$selectedRow <- 0L
      opts$displayDesign <- TRUE
      opts$runOrder <- "runOrderStandard"
      opts$actualExporter <- TRUE
      opts$exportDesignFile <- tempfile(fileext = ".csv")
      result <- runAnalysis("doeResponseSurfaceMethodology", "debug.csv", opts, view = FALSE)
      info <- paste(type, nCategorical, "discrete factors")
      expect_identical(result$status, "complete", info = info)
      exported <- read.csv(opts$exportDesignFile, check.names = FALSE)
      unlink(opts$exportDesignFile)
      # every named column holds the values of its factor in the design table
      tableData <- result$results$designTable$data
      tableColumn <- function(name) vapply(tableData, function(row) as.character(row[[name]]), character(1))
      for (i in 1:3)
        expect_equal(as.character(exported[[c("Temperature", "Pressure", "Time")[i]]]), tableColumn(paste0("x", i)), info = info)
      if (nCategorical == 1) { # the form sets the discrete factor name, so take it from the table
        fields <- result$results$designTable$schema$fields
        discreteName <- fields[[which(vapply(fields, function(field) identical(field$name, "x_cat1"), logical(1)))]]$title
        expect_equal(as.character(exported[[discreteName]]), tableColumn("x_cat1"), info = info)
      }
      expect_identical(names(exported)[c(1:2, ncol(exported))], c("RunOrder", "StandardOrder", "Response"))
      expect_equal(nrow(exported), length(tableData), info = info)
    }
  }
})
