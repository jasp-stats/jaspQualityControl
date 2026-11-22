context("[Quality Control] Variables charts with only singleton subgroups")

# Each row is a subgroup with one observation, despite selecting two measurement columns.
.allSingletonsData <- data.frame(First  = c(1, NA, 3, NA, 5),
                                 Second = c(NA, 2, NA, 4, NA))
.allSingletonsError <- paste("All subgroups are of size 1.",
                             "Variables charts for subgroups cannot be calculated.",
                             "Use variables charts for individuals.")

.allSingletonsOptions <- function(chartType) {
  options <- analysisOptions("variablesChartsSubgroups")
  options$dataFormat             <- "wideFormat"
  options$measurementsWideFormat <- c("First", "Second")
  options$chartType              <- chartType
  options$subgroupSizeUnequal    <- "actualSizes"
  options$knownParameters        <- FALSE
  options$report                 <- FALSE
  options
}

test_that("Wide-format X-bar & R charts reject subgroups with only one observed value", {
  results <- runAnalysis("variablesChartsSubgroups", .allSingletonsData, .allSingletonsOptions("xBarAndR"))
  plot <- results[["results"]][["plot"]]

  expect_identical(plot[["status"]], "error")
  expect_identical(plot[["error"]][["errorMessage"]], .allSingletonsError)
  expect_null(results[["results"]][["controlCharts"]])
})

test_that("Wide-format X-bar & s charts reject subgroups with only one observed value", {
  results <- runAnalysis("variablesChartsSubgroups", .allSingletonsData, .allSingletonsOptions("xBarAndS"))
  plot <- results[["results"]][["plot"]]

  expect_identical(plot[["status"]], "error")
  expect_identical(plot[["error"]][["errorMessage"]], .allSingletonsError)
  expect_null(results[["results"]][["controlCharts"]])
})
