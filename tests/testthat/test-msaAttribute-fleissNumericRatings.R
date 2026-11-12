context("Attribute Agreement Analysis - Fleiss' kappa vs standard with numeric ratings")

# Fleiss' kappa of one trial and the standard (two ratings per part), from its definition:
# P_i = (sum_j n_ij^2 - n) / (n (n - 1)), P_e = sum_j p_j^2, kappa = (mean(P_i) - P_e) / (1 - P_e), n = 2.
.fleissTwoRatings <- function(x, y) {
  categories <- sort(unique(c(x, y)))
  counts <- t(vapply(seq_along(x), function(i) vapply(categories, function(k) (x[i] == k) + (y[i] == k), numeric(1)),
                     numeric(length(categories))))
  agreement <- (rowSums(counts^2) - 2) / 2
  shares    <- colSums(counts) / (2 * length(x))
  expected  <- sum(shares^2)
  (mean(agreement) - expected) / (1 - expected)
}

# Appraiser vs standard (Minitab): the kappa of each trial with the standard, averaged over trials.
.fleissVsStandard <- function(ratings, reference)
  mean(vapply(seq_len(ncol(ratings)), function(t) .fleissTwoRatings(ratings[, t], reference), numeric(1)))

.fleissOptions <- function(format, measurements) {
  options <- analysisOptions("msaAttribute")
  options$dataFormat <- format
  if (format == "wideFormat") {
    options$operatorWideFormat     <- "Operator"
    options$partWideFormat         <- "Part"
    options$measurementsWideFormat <- measurements
    options$standardWideFormat     <- "Ref"
  } else {
    options$operatorLongFormat    <- "Operator"
    options$partLongFormat        <- "Part"
    options$measurementLongFormat <- measurements
    options$standardLongFormat    <- "Ref"
  }
  options$positiveReference <- ""
  options$fleissKappa <- TRUE
  options
}

.fleissVsStandardColumn <- function(results) {
  rows <- results[["results"]][["fleissKappa"]][["data"]]
  setNames(vapply(rows, function(row) row[["vsStandard"]], numeric(1)),
           vapply(rows, function(row) row[["appraiser"]], character(1)))
}

.fleissWide <- function(reference, ratings)
  do.call(rbind, lapply(names(ratings), function(appraiser)
    data.frame(Operator = appraiser, Part = seq_along(reference), T1 = ratings[[appraiser]][, 1],
               T2 = ratings[[appraiser]][, 2], Ref = reference)))

.fleissLong <- function(wide)
  do.call(rbind, lapply(c("T1", "T2"), function(trial)
    data.frame(Operator = wide$Operator, Part = wide$Part, Rating = wide[[trial]], Ref = wide$Ref)))

# Ratings that as.matrix() formats to a common width (" 1", "-1", " 9", "10"): negative ratings and ratings >= 10.
scales <- list(
  "-1/0/1" = list(
    reference = c(-1, 0, 1, 1, 0, -1, 1, 0),
    ratings   = list(A = cbind(c(-1, 0, 1, 1, 0, -1, 0, 0), c(-1, 0, 1, 0, 0, -1, 1, 0)),
                     B = cbind(c(-1, 1, 1, 1, 0, 0, 1, 0), c(0, 0, 1, 1, 0, -1, 1, 1)))),
  "1-10" = list(
    reference = c(1, 3, 5, 7, 9, 10, 2, 10),
    ratings   = list(A = cbind(c(1, 3, 5, 7, 9, 10, 2, 9), c(1, 3, 4, 7, 9, 10, 2, 10)),
                     B = cbind(c(2, 3, 5, 7, 10, 10, 2, 10), c(1, 3, 5, 8, 9, 9, 2, 10))))
)

for (scale in names(scales)) {
  reference <- scales[[scale]][["reference"]]
  ratings   <- scales[[scale]][["ratings"]]
  wide      <- .fleissWide(reference, ratings)
  expected  <- vapply(ratings, .fleissVsStandard, numeric(1), reference = reference)
  expected  <- c(expected, All = mean(expected))

  test_that(paste("Fleiss' kappa vs standard follows its definition for numeric ratings on a", scale, "scale"), {
    resultsWide <- runAnalysis("msaAttribute", wide, .fleissOptions("wideFormat", c("T1", "T2")), view = FALSE)
    resultsLong <- runAnalysis("msaAttribute", .fleissLong(wide), .fleissOptions("longFormat", "Rating"), view = FALSE)
    expect_identical(resultsWide[["status"]], "complete")
    expect_identical(resultsLong[["status"]], "complete")
    expect_equal(.fleissVsStandardColumn(resultsWide), expected, tolerance = 1e-10)
    expect_equal(.fleissVsStandardColumn(resultsLong), expected, tolerance = 1e-10)
  })

  test_that(paste("Fleiss' kappa vs standard does not depend on the labels of a", scale, "scale"), {
    # kappa treats the ratings as categories, so naming them with letters must give the same values
    values   <- sort(unique(unlist(wide[c("T1", "T2", "Ref")])))
    lettered <- wide
    for (column in c("T1", "T2", "Ref"))
      lettered[[column]] <- LETTERS[match(wide[[column]], values)]
    numeric <- runAnalysis("msaAttribute", wide, .fleissOptions("wideFormat", c("T1", "T2")), view = FALSE)
    letters <- runAnalysis("msaAttribute", lettered, .fleissOptions("wideFormat", c("T1", "T2")), view = FALSE)
    expect_equal(.fleissVsStandardColumn(numeric), .fleissVsStandardColumn(letters), tolerance = 1e-10)
  })
}
