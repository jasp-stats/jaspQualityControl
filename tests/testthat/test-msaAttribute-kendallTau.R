context("Attribute Agreement Analysis - Kendall's tau with several trials")

# Kendall's tau-b by pair counting, as in Minitab's methods and formulas for Attribute Agreement Analysis:
# (C - D) / sqrt((n0 - T_X) * (n0 - T_Y)), n0 = n(n - 1) / 2, T_X and T_Y the pairs tied on x and y.
.tauB <- function(x, y) {
  n <- length(x)
  s <- tiedX <- tiedY <- 0
  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    s     <- s + sign(x[i] - x[j]) * sign(y[i] - y[j])
    tiedX <- tiedX + (x[i] == x[j])
    tiedY <- tiedY + (y[i] == y[j])
  }
  n0 <- n * (n - 1) / 2
  s / sqrt((n0 - tiedX) * (n0 - tiedY))
}

# Appraiser vs standard (Minitab): tau-b of each trial with the standard, averaged over trials.
.tauVsStandard <- function(ratings, reference)
  mean(vapply(seq_len(ncol(ratings)), function(t) .tauB(ratings[, t], reference), numeric(1)))

# Between two appraisers: tau-b over every combination of their ratings of the same part.
.tauBetween <- function(x, y) {
  xs <- ys <- numeric(0)
  for (part in seq_len(nrow(x))) for (i in seq_len(ncol(x))) for (j in seq_len(ncol(y))) {
    xs <- c(xs, x[part, i])
    ys <- c(ys, y[part, j])
  }
  .tauB(xs, ys)
}

.kendallOptions <- function(format, measurements, standard) {
  options <- analysisOptions("msaAttribute")
  options$dataFormat <- format
  if (format == "wideFormat") {
    options$operatorWideFormat     <- "Operator"
    options$partWideFormat         <- "Part"
    options$measurementsWideFormat <- measurements
    options$standardWideFormat     <- standard
  } else {
    options$operatorLongFormat    <- "Operator"
    options$partLongFormat        <- "Part"
    options$measurementLongFormat <- measurements
    options$standardLongFormat    <- standard
  }
  options$positiveReference <- ""
  options$kendallsTau <- TRUE
  options
}

# Three appraisers rate eight parts three times on a 1-5 scale.
reference <- c(1, 2, 3, 4, 5, 3, 2, 4)
ratings <- list(
  P = cbind(c(1, 2, 3, 4, 5, 3, 2, 4), c(1, 2, 3, 5, 5, 3, 1, 4), c(2, 2, 3, 4, 4, 3, 2, 4)),
  Q = cbind(c(1, 3, 3, 4, 5, 2, 2, 4), c(1, 2, 4, 4, 5, 3, 2, 5), c(1, 2, 3, 4, 5, 3, 3, 4)),
  R = cbind(c(2, 1, 3, 5, 4, 3, 2, 3), c(1, 2, 2, 4, 5, 4, 2, 4), c(1, 3, 3, 4, 5, 3, 1, 5))
)
wide <- do.call(rbind, lapply(names(ratings), function(appraiser)
  data.frame(Operator = appraiser, Part = 1:8, T1 = ratings[[appraiser]][, 1], T2 = ratings[[appraiser]][, 2],
             T3 = ratings[[appraiser]][, 3], Ref = reference)))
long <- do.call(rbind, lapply(1:3, function(trial)
  data.frame(Operator = wide$Operator, Part = wide$Part, Rating = wide[[paste0("T", trial)]], Ref = wide$Ref)))

expectedBetween <- outer(names(ratings), names(ratings), Vectorize(function(a, b)
  if (a == b) 1 else .tauBetween(ratings[[a]], ratings[[b]])))
expectedVsStandard <- vapply(ratings, .tauVsStandard, numeric(1), reference = reference)

kendallTable <- function(results) {
  rows <- results[["results"]][["KendallTau"]][["data"]]
  do.call(rbind, lapply(rows, as.data.frame))
}

test_that("KT1 Wide format, three trials, with standard: tau-b matches pair counting", {
  results <- runAnalysis("msaAttribute", wide, .kendallOptions("wideFormat", c("T1", "T2", "T3"), "Ref"))
  expect_identical(results[["status"]], "complete")
  table <- kendallTable(results)
  expect_identical(table[["Operator"]], c("P", "Q", "R"))
  expect_equal(unname(as.matrix(table[c("P", "Q", "R")])), unname(expectedBetween))
  expect_equal(table[["Ref"]], unname(expectedVsStandard))
  footnotes <- vapply(results[["results"]][["KendallTau"]][["footnotes"]], `[[`, character(1), "text")
  expect_setequal(footnotes, c("Between appraisers: Kendall's tau-b over all combinations of the two appraisers' ratings of each part.",
                               "Appraiser vs standard: Kendall's tau-b of each trial with the standard, averaged over trials."))
})

test_that("KT2 Long format, three trials, with standard: same table as wide format", {
  results <- runAnalysis("msaAttribute", long, .kendallOptions("longFormat", "Rating", "Ref"))
  table <- kendallTable(results)
  expect_identical(table[["Operator"]], c("P", "Q", "R"))
  expect_equal(unname(as.matrix(table[c("P", "Q", "R")])), unname(expectedBetween))
  expect_equal(table[["Ref"]], unname(expectedVsStandard))
})

test_that("KT3 Without standard: one row per appraiser, no empty standard column", {
  results <- runAnalysis("msaAttribute", wide, .kendallOptions("wideFormat", c("T1", "T2", "T3"), ""))
  fields <- vapply(results[["results"]][["KendallTau"]][["schema"]][["fields"]], `[[`, character(1), "name")
  expect_identical(fields, c("Operator", "P", "Q", "R"))
  table <- kendallTable(results)
  expect_equal(unname(as.matrix(table[c("P", "Q", "R")])), unname(expectedBetween))

  oneTrial <- runAnalysis("msaAttribute", wide, .kendallOptions("wideFormat", "T1", ""))
  fields <- vapply(oneTrial[["results"]][["KendallTau"]][["schema"]][["fields"]], `[[`, character(1), "name")
  expect_identical(fields, c("Operator", "P", "Q", "R"))
  expect_length(oneTrial[["results"]][["KendallTau"]][["footnotes"]], 0)
})

test_that("KT4 Rotating one appraiser's trial columns does not change the table", {
  rotated <- wide
  rowsQ <- rotated$Operator == "Q"
  rotated[rowsQ, c("T1", "T2", "T3")] <- rotated[rowsQ, c("T2", "T3", "T1")]
  table <- kendallTable(runAnalysis("msaAttribute", rotated, .kendallOptions("wideFormat", c("T1", "T2", "T3"), "Ref")))
  expect_equal(unname(as.matrix(table[c("P", "Q", "R")])), unname(expectedBetween))
  expect_equal(table[["Ref"]], unname(expectedVsStandard))
})

test_that("KT5 Long format in shuffled row order: tau between appraisers does not change", {
  # Shuffling changes which rating is trial k of an appraiser and part, so only the statistics that do not
  # depend on trials stay the same; appraiser vs standard is averaged over trials, as in Minitab.
  set.seed(1)
  shuffled <- long[sample(nrow(long)), ]
  table <- kendallTable(runAnalysis("msaAttribute", shuffled, .kendallOptions("longFormat", "Rating", "Ref")))
  expect_equal(unname(as.matrix(table[c("P", "Q", "R")])), unname(expectedBetween))
})

test_that("KT6 Long format in run order (trial order kept), wide format in shuffled rows: same table", {
  trial <- ave(seq_len(nrow(long)), long$Operator, long$Part, FUN = seq_along)
  set.seed(2)
  runOrder <- long[order(trial, sample(nrow(long))), ]
  table <- kendallTable(runAnalysis("msaAttribute", runOrder, .kendallOptions("longFormat", "Rating", "Ref")))
  expect_equal(unname(as.matrix(table[c("P", "Q", "R")])), unname(expectedBetween))
  expect_equal(table[["Ref"]], unname(expectedVsStandard))

  set.seed(3)
  shuffledWide <- wide[sample(nrow(wide)), ]
  table <- kendallTable(runAnalysis("msaAttribute", shuffledWide, .kendallOptions("wideFormat", c("T1", "T2", "T3"), "Ref")))
  expect_equal(unname(as.matrix(table[c("P", "Q", "R")])), unname(expectedBetween))
  expect_equal(table[["Ref"]], unname(expectedVsStandard))
})

test_that("KT7 Standard as factor with reversed levels uses its numbers", {
  dataset <- wide
  dataset$Ref <- factor(dataset$Ref, levels = as.character(5:1))
  table <- kendallTable(runAnalysis("msaAttribute", dataset, .kendallOptions("wideFormat", c("T1", "T2", "T3"), "Ref")))
  expect_equal(table[["Ref"]], unname(expectedVsStandard))
})

test_that("KT8 A constant trial leaves that appraiser's tau with the standard undefined", {
  dataset <- wide
  dataset$T2[dataset$Operator == "P"] <- 3
  constantP <- ratings$P
  constantP[, 2] <- 3
  results <- runAnalysis("msaAttribute", dataset, .kendallOptions("wideFormat", c("T1", "T2", "T3"), "Ref"))
  rows <- results[["results"]][["KendallTau"]][["data"]]
  expect_identical(rows[[1]][["Ref"]], "")
  expect_equal(rows[[2]][["Ref"]], .tauVsStandard(ratings$Q, reference))
  expect_equal(rows[[1]][["Q"]], .tauBetween(constantP, ratings$Q))
})

test_that("KT9 An appraiser with one rating for all parts has undefined tau with the others", {
  dataset <- wide
  dataset[dataset$Operator == "P", c("T1", "T2", "T3")] <- 3
  results <- runAnalysis("msaAttribute", dataset, .kendallOptions("wideFormat", c("T1", "T2", "T3"), "Ref"))
  rows <- results[["results"]][["KendallTau"]][["data"]]
  expect_identical(rows[[1]][["Q"]], "")
  expect_identical(rows[[1]][["Ref"]], "")
  expect_equal(rows[[2]][["R"]], .tauBetween(ratings$Q, ratings$R))
})

test_that("KT10 Ratings that differ only in the last digits are not ties", {
  # 0.1 + 0.2 and 0.3 are different numbers; identical appraisers agree perfectly.
  values  <- c(0.3, 0.1 + 0.2, 0.4, 0.5)
  dataset <- data.frame(Operator = rep(c("A", "B"), each = 4), Part = rep(1:4, 2), T1 = rep(values, 2), T2 = rep(values, 2))
  table <- kendallTable(runAnalysis("msaAttribute", dataset, .kendallOptions("wideFormat", c("T1", "T2"), "")))
  expect_equal(table[["A"]], c(1, 1))
  expect_equal(.tauB(values, values), 1)
})

test_that("KT11 Issue example (two trials, wide format) matches pair counting", {
  dataset <- data.frame(Operator = rep(c("A", "B"), each = 6), Part = rep(1:6, 2),
                        T1 = c(1, 2, 3, 4, 5, 1, 1, 2, 3, 5, 4, 1),
                        T2 = c(1, 2, 3, 4, 5, 2, 2, 2, 3, 4, 5, 1),
                        Ref = rep(c(1, 2, 3, 4, 5, 1), 2))
  results <- runAnalysis("msaAttribute", dataset, .kendallOptions("wideFormat", c("T1", "T2"), "Ref"))
  expect_identical(results[["status"]], "complete")
  a <- as.matrix(dataset[1:6, c("T1", "T2")])
  b <- as.matrix(dataset[7:12, c("T1", "T2")])
  table <- kendallTable(results)
  expect_equal(table[["A"]], c(1, .tauBetween(a, b)))
  expect_equal(table[["Ref"]], c(.tauVsStandard(a, dataset$Ref[1:6]), .tauVsStandard(b, dataset$Ref[1:6])))
  # Trial 1 of A equals the standard; trial 2 has 13 of 14 untied pairs concordant.
  expect_equal(table[["Ref"]][1], (1 + 13 / 14) / 2)
})

test_that("KT12 A trial column that is not numeric gives a table error", {
  dataset <- wide
  dataset$T2 <- as.character(dataset$T2)
  dataset$T2[1] <- "x"
  results <- runAnalysis("msaAttribute", dataset, .kendallOptions("wideFormat", c("T1", "T2", "T3"), ""))
  expect_identical(results[["results"]][["KendallTau"]][["error"]][["errorMessage"]],
                   "Kendall's tau is only available for numeric measurements.")
})
