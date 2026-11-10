context("Agreement exact confidence interval boundaries")

.aacInversion <- function(m, n) {
  # Invert binomial tail probabilities directly, without beta/F quantiles.
  lower <- if (m == 0) 0 else stats::uniroot(function(p)
    stats::pbinom(m - 1, n, p, lower.tail = FALSE) - 0.025, c(0, 1), tol = 1e-12)$root
  upper <- if (m == n) 1 else stats::uniroot(function(p)
    stats::pbinom(m, n, p) - 0.025, c(0, 1), tol = 1e-12)$root
  round(c(lower, upper) * 100, 3)
}

test_that("mixed boundaries and interior cases match independent binomial inversion", {
  m <- c(0, 1, 9, 10)
  result <- jaspQualityControl:::.AAACI(m, 10)
  expected <- vapply(m, .aacInversion, numeric(2), n = 10)
  expect_equal(result$lower, expected[1, ])
  expect_equal(result$upper, expected[2, ])
  expect_equal(result$lower, c(0, 0.253, 55.498, 69.150))
  expect_equal(result$upper, c(30.850, 44.502, 99.747, 100))
  expect_equal(jaspQualityControl:::.AAACI(0, c(1, 10))$lower, c(0, 0))
})

test_that("all interior values retain the existing rounded F formula", {
  for (n in c(2, 10, 100)) {
    m <- seq_len(n - 1)
    lowerF <- stats::qf(0.025, 2 * m, 2 * (n - m + 1))
    upperF <- stats::qf(0.975, 2 * (m + 1), 2 * (n - m))
    expected <- list(lower = round(100 * m * lowerF / (n - m + 1 + m * lowerF), 3),
                     upper = round(100 * (m + 1) * upperF / (n - m + (m + 1) * upperF), 3))
    expect_equal(jaspQualityControl:::.AAACI(m, n), expected)
  }
})

test_that("zero observations and invalid counts have no interval", {
  result <- jaspQualityControl:::.AAACI(c(0, -1, 11, 0.5, NA, Inf, 1, 1, 1),
                                       c(0, 10, 10, 10, 10, 10, -1, 1.5, NA))
  expect_true(all(is.na(result$lower)))
  expect_true(all(is.na(result$upper)))
  expect_equal(jaspQualityControl:::.AAACI(numeric(), numeric()), list(lower = numeric(), upper = numeric()))
})

.aacData <- data.frame(part = rep(1:10, 2), appraiser = rep(c("A", "B"), each = 10),
                       first = rep(rep(c("yes", "no"), 5), 2),
                       second = c(rep(c("yes", "no"), 5), rep(c("no", "yes"), 5)),
                       standard = rep(rep(c("yes", "no"), 5), 2))

test_that("agreement tables and error bars include finite boundary endpoints", {
  for (wide in c(TRUE, FALSE)) {
    data <- .aacData
    if (!wide) {
      data <- tidyr::pivot_longer(data, c("first", "second"), names_to = "set", values_to = "rating")
      data <- data[c("part", "appraiser", "standard", "rating")]
    }
    for (standard in c("", "standard")) {
      options <- jaspTools::analysisOptions("msaAttribute")
      options$dataFormat <- if (wide) "wideFormat" else "longFormat"
      options$measurementsWideFormat <- c("first", "second")
      options$partWideFormat <- "part"
      options$operatorWideFormat <- "appraiser"
      options$standardWideFormat <- standard
      options$measurementLongFormat <- "rating"
      options$partLongFormat <- "part"
      options$operatorLongFormat <- "appraiser"
      options$standardLongFormat <- standard
      options$cohensKappa <- FALSE
      options$fleissKappa <- FALSE
      options$kendallsTau <- FALSE
      options$positiveReference <- ""
      result <- jaspTools::runAnalysis("msaAttribute", data, options, view = FALSE)
      expect_identical(result$status, "complete")
      output <- result$results$AAAtableGraphs$collection
      for (key in c("Within", "Between", if (standard != "") c("EachVsStandard", "AllVsStandard"))) {
        table <- output[[paste0("AAAtableGraphs_", key)]]
        expect_gt(length(table$data), 0)
        for (row in table$data) {
          expected <- .aacInversion(row$Matched, row$Inspected)
          expect_equal(c(row$CIL, row$CIU), expected)
        }
      }
      for (key in c("PlotWithin", if (standard != "") "PlotVs")) {
        plot <- result$state$figures[[output[[paste0("AAAtableGraphs_", key)]]$data]]$obj
        layerIndex <- which(vapply(plot$layers, function(layer)
          inherits(layer$geom, "GeomErrorbar"), logical(1)))[1]
        errorBars <- ggplot2::ggplot_build(plot)$data[[layerIndex]]
        expect_equal(errorBars$ymin, c(69.150, 0))
        expect_equal(errorBars$ymax, c(100, 30.850))
      }
    }
  }
})
