context("Attribute Agreement Analysis - miss and false alarm rates")

# AIAG-like Go/No-Go study: 24 No-Go and 26 Go parts, 3 appraisers x 3 trials. The error counts give the
# AIAG rates with good = "Go": miss 4/72, 7/72, 2/72 (5.56, 9.72, 2.78 %), false alarm 13/78, 6/78, 11/78
# (16.67, 7.69, 14.1 %). They must not depend on whether the positive reference is the good or the bad category.
.msaAttributeGoNoGoData <- function() {
  ref    <- c(rep("No-Go", 24), rep("Go", 26))
  misses <- c(A = 4, B = 7, C = 2)
  falses <- c(A = 13, B = 6, C = 11)
  rows <- list()
  for (op in names(misses)) {
    for (trial in 1:3) {
      res <- ref
      if (trial == 1) {
        res[seq_len(misses[[op]])]      <- "Go"
        res[24 + seq_len(falses[[op]])] <- "No-Go"
      }
      rows[[length(rows) + 1]] <- data.frame(Part = 1:50, Operator = op, Results = res, Reference = ref)
    }
  }
  return(do.call(rbind, rows))
}

.msaAttributeGoNoGoTable <- function(positiveReference, positiveReferenceCategory) {
  options <- analysisOptions("msaAttribute")
  options$dataFormat <- "longFormat"
  options$operatorLongFormat <- "Operator"
  options$partLongFormat <- "Part"
  options$measurementLongFormat <- "Results"
  options$standardLongFormat <- "Reference"
  options$cohensKappa <- TRUE
  options$fleissKappa <- TRUE
  options$kendallsTau <- FALSE
  options$positiveReference <- positiveReference
  options$positiveReferenceCategory <- positiveReferenceCategory
  results <- runAnalysis("msaAttribute", .msaAttributeGoNoGoData(), options, makeTests = FALSE)
  return(results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_StudyEffectiveness"]])
}

expectedRows <- list("A", "66 (unacceptable)", "16.67 (unacceptable)", "5.56 (unacceptable)",
                     "B", "74 (unacceptable)", "7.69 (marginally acceptable)", "9.72 (unacceptable)",
                     "C", "74 (unacceptable)", "14.1 (unacceptable)", "2.78 (marginally acceptable)")

test_that("Miss and false alarm rates with the accept category as positive reference", {
  table <- .msaAttributeGoNoGoTable("Go", "accept")
  jaspTools::expect_equal_tables(table[["data"]], expectedRows)
  footnotes <- vapply(table[["footnotes"]], function(f) f[["text"]], character(1))
  expect_true(any(startsWith(footnotes, "Accept (good) category: Go.")))
})

test_that("Miss and false alarm rates with the reject category as positive reference", {
  table <- .msaAttributeGoNoGoTable("No-Go", "reject")
  jaspTools::expect_equal_tables(table[["data"]], expectedRows)
  footnotes <- vapply(table[["footnotes"]], function(f) f[["text"]], character(1))
  expect_true(any(startsWith(footnotes, "Accept (good) category: Go.")))
})
