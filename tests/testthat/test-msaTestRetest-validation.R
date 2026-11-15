context("Test-retest paired input validation")

.trvWide <- data.frame(part = letters[1:5], first = 10:14, second = 11:15, third = 1001:1005)
.trvRun <- function(data = .trvWide, wide = TRUE, readings = c("first", "second"), plots = FALSE) {
  options <- jaspTools::analysisOptions("msaTestRetest")
  options$dataFormat <- if (wide) "wideFormat" else "longFormat"
  options$partWideFormat <- "part"
  options$measurementsWideFormat <- readings
  options$measurementLongFormat <- "reading"
  options$partLongFormat <- "part"
  options$operator <- "set"
  options$repeatabilityAndReproducibilityTable <- TRUE
  options$runChartPart <- plots
  options$scatterPlotMeasurement <- plots
  options$rChart <- plots
  options$trafficLightChart <- plots
  options$scatterPlotMeasurementFitLine <- FALSE
  jaspTools::runAnalysis("msaTestRetest", data, options, view = FALSE)
}

.trvLong <- tidyr::pivot_longer(.trvWide[c("part", "first", "second")], -part,
                               names_to = "set", values_to = "reading")

test_that("third measurement sets are explicitly rejected before output", {
  # Wide format is capped at two columns in the QML, so only long format can supply a third set.
  for (extra in c(0, 10000)) {
    data <- .trvWide
    data$third <- data$third + extra
    input <- tidyr::pivot_longer(data, -part, names_to = "set", values_to = "reading")
    result <- .trvRun(input, FALSE, plots = TRUE)
    expect_identical(result$status, "validationError")
    expect_match(result$results$errorMessage, "exactly two")
    expect_null(result$results$rAndR2)
    expect_length(result$state$figures, 0L)
  }
})

test_that("the supported pair retains numerical results across formats and orderings", {
  reference <- .trvRun(plots = TRUE)
  expect_identical(reference$status, "complete")
  row <- reference$results$rAndR2$data[[1]]
  # Five absolute paired differences are all one. Preserve the declared finite-study d2 = 1.19105.
  expect_equal(row$Rbar, 1)
  expect_equal(row$d2, 1.19105)
  expect_equal(row$GRR, 1 / 1.19105)
  expect_equal(row$n, 5)
  for (result in list(.trvRun(.trvWide[5:1, ], readings = c("second", "first"), plots = TRUE),
                      .trvRun(.trvLong, FALSE, plots = TRUE),
                      .trvRun(.trvLong[nrow(.trvLong):1, ], FALSE, plots = TRUE))) {
    expect_identical(result$status, "complete")
    expect_equal(result$results$rAndR2$data, reference$results$rAndR2$data)
    expect_equal(length(result$state$figures), length(reference$state$figures))
  }
})

test_that("duplicate and incomplete pairs are rejected", {
  badLong <- list(rbind(.trvLong, .trvLong[1, ]), .trvLong[-1, ],
                  transform(.trvLong, reading = replace(reading, 1, NA)),
                  transform(.trvLong, part = replace(part, 1, NA)),
                  transform(.trvLong, set = replace(set, 1, NA)),
                  subset(.trvLong, set == "first"))
  for (data in badLong)
    expect_identical(.trvRun(data, FALSE)$status, "validationError")
  expect_identical(.trvRun(rbind(.trvWide, .trvWide[1, ]))$status, "validationError")
})

test_that("wide rows with missing values are excluded listwise", {
  for (data in list(transform(.trvWide, first = replace(first, 1, NA)),
                    transform(.trvWide, part = replace(part, 1, NA)))) {
    result <- .trvRun(data)
    expect_identical(result$status, "complete")
    expect_equal(result$results$rAndR2$data[[1]]$n, 4)
    expect_equal(result$results$rAndR2$data[[1]]$Rbar, 1)
  }
})

test_that("blank padding rows do not affect either format", {
  # Another, longer column in the data file pads the assigned columns with blanks.
  wide <- rbind(.trvWide, data.frame(part = NA, first = NA, second = NA, third = 1006))
  long <- rbind(as.data.frame(.trvLong), data.frame(part = NA, set = NA, reading = NA))
  for (result in list(.trvRun(wide), .trvRun(long, FALSE))) {
    expect_identical(result$status, "complete")
    expect_equal(result$results$rAndR2$data[[1]]$n, 5)
    expect_equal(result$results$rAndR2$data[[1]]$Rbar, 1)
  }
})

test_that("long-format scatter axes follow set level order, not row order", {
  long <- as.data.frame(.trvLong)
  long$set <- factor(long$set, levels = c("first", "second"))
  for (data in list(long, long[nrow(long):1, ])) {
    result <- .trvRun(data, FALSE, plots = TRUE)
    plot <- result$state$figures[[result$results$ScatterOperators$data]]$obj
    built <- ggplot2::ggplot_build(plot)$data[[1]]
    expect_equal(sort(built$x), .trvWide$first)
    expect_equal(sort(built$y), .trvWide$second)
  }
})

test_that("unassigned inputs and too few wide selections retain readiness", {
  for (readings in list(character(), "first"))
    expect_identical(.trvRun(readings = readings)$status, "complete")
  options <- jaspTools::analysisOptions("msaTestRetest")
  expect_identical(jaspTools::runAnalysis("msaTestRetest", .trvWide, options, view = FALSE)$status, "complete")
})


test_that("nominal part and set labels are not interpreted as infinite measurements", {
  for (specialLabels in c(FALSE, TRUE)) {
    data <- .trvWide[c("part", "first", "second")]
    data$part <- factor(if (specialLabels) c("Inf", "-Inf", "c", "d", "e") else letters[1:5])
    for (wide in c(TRUE, FALSE)) {
      input <- if (wide) data else tidyr::pivot_longer(data, -part, names_to = "set", values_to = "reading")
      if (!wide)
        input$set <- factor(input$set, levels = c("first", "second"),
                            labels = if (specialLabels) c("Inf", "-Inf") else c("first", "second"))
      coercionWarnings <- character()
      result <- withCallingHandlers(.trvRun(input, wide), warning = function(warning) {
        if (grepl("NAs introduced by coercion", conditionMessage(warning), fixed = TRUE)) {
          coercionWarnings <<- c(coercionWarnings, conditionMessage(warning))
          invokeRestart("muffleWarning")
        }
      })
      expect_identical(result$status, "complete")
      expect_length(coercionWarnings, 0L)
      expect_equal(result$results$rAndR2$data[[1]]$Rbar, 1)
    }
  }
})

test_that("infinite measurement values are still rejected in both formats", {
  for (value in c(Inf, -Inf)) {
    wide <- .trvWide
    wide$first[1] <- value
    long <- .trvLong
    long$reading[1] <- value
    expect_identical(.trvRun(wide)$status, "validationError")
    expect_identical(.trvRun(long, FALSE)$status, "validationError")
  }
})

test_that("nonuniform paired differences remain aligned after long-row reordering", {
  data <- data.frame(part = letters[1:5], first = c(2, 10, -5, 4, 8), second = c(3, 14, 0, 6, 13))
  long <- tidyr::pivot_longer(data, -part, names_to = "set", values_to = "reading")
  long <- long[c(10, 3, 8, 1, 5, 6, 2, 9, 4, 7), ]
  for (result in list(.trvRun(data, plots = TRUE), .trvRun(long, FALSE, plots = TRUE))) {
    expect_identical(result$status, "complete")
    # Absolute paired differences are 1, 4, 5, 2, 5, independently of row order.
    expect_equal(result$results$rAndR2$data[[1]]$Rbar, 3.4)
    expect_equal(result$results$rAndR2$data[[1]]$GRR, 3.4 / 1.19105)
    # Traffic light needs a process SD or tolerance (none ticked here), so 3 figures.
    expect_length(result$state$figures, 3L)
  }
})
