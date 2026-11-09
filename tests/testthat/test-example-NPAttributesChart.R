context("Example: NPAttributesChart")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("saved np example rejects impossible defective-item counts", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "NPAttributesChart.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("attributesCharts", encoded$dataset, encoded$options, encodedDataset = TRUE)

  # The saved data include 88 defective items in a sample of 50.
  expect_identical(results$status, "validationError")
  expect_match(results$results$errorMessage, "cannot exceed the sample size")
  expect_null(results$results$NPchartPlot)
  expect_null(results$results$NelsonTable)

})

