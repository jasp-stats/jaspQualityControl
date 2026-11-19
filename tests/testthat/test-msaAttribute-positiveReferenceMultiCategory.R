context("Attribute Agreement Analysis - positive reference with more than two categories")

# jasp-stats/jasp-issues#4699: ratings and standard with three categories (low/mid/high). A positive reference
# only matters for the miss and false alarm rates, which need two categories; it must not replace the
# agreement tables and plots, and the study effectiveness summary must say why it has no rates.
.msaAttributeThreeCategoryData <- function() {
  data.frame(Operator = rep(c("A", "B"), each = 4),
             Part     = rep(1:4, 2),
             T1       = c("low", "mid", "high", "low", "low", "mid", "high", "mid"),
             T2       = c("low", "mid", "mid",  "low", "mid", "mid", "high", "low"),
             Ref      = c("low", "mid", "high", "mid", "low", "mid", "high", "mid"),
             stringsAsFactors = FALSE)
}

.msaAttributeWideOptions <- function(positiveReference, cohensKappa) {
  options <- analysisOptions("msaAttribute")
  options$dataFormat <- "wideFormat"
  options$operatorWideFormat <- "Operator"
  options$partWideFormat <- "Part"
  options$measurementsWideFormat <- c("T1", "T2")
  options$measurementsWideFormat.types <- c("nominal", "nominal")
  options$standardWideFormat <- "Ref"
  options$standardWideFormat.types <- "nominal"
  options$kendallsTau <- FALSE
  options$fleissKappa <- TRUE
  options$cohensKappa <- cohensKappa
  options$positiveReference <- positiveReference
  options$positiveReferenceCategory <- "accept"
  return(options)
}

agreementTables <- c("AAAtableGraphs_Within", "AAAtableGraphs_Between", "AAAtableGraphs_EachVsStandard",
                     "AAAtableGraphs_AllVsStandard")

test_that("A positive reference with three categories keeps the agreement tables and plots", {
  withoutReference <- runAnalysis("msaAttribute", .msaAttributeThreeCategoryData(),
                                  .msaAttributeWideOptions("", FALSE), makeTests = FALSE)
  withReference    <- runAnalysis("msaAttribute", .msaAttributeThreeCategoryData(),
                                  .msaAttributeWideOptions("high", TRUE), makeTests = FALSE)

  expect_equal(withReference[["status"]], "complete")
  aaaWithout <- withoutReference[["results"]][["AAAtableGraphs"]][["collection"]]
  aaa        <- withReference[["results"]][["AAAtableGraphs"]][["collection"]]

  # Hand counts: within A 3/4 (part 3 differs), B 2/4 (parts 1, 4); each vs standard 2/4 for both;
  # between and all vs standard 1/4 (only part 2 is "mid" everywhere).
  jaspTools::expect_equal_tables(aaaWithout[["AAAtableGraphs_Within"]][["data"]],
                                 list("A", 19.412, 99.369, 4, 3, 75, "B", 6.759, 93.241, 4, 2, 50))
  jaspTools::expect_equal_tables(aaaWithout[["AAAtableGraphs_EachVsStandard"]][["data"]],
                                 list("A", 6.759, 93.241, 4, 2, 50, "B", 6.759, 93.241, 4, 2, 50))
  jaspTools::expect_equal_tables(aaaWithout[["AAAtableGraphs_Between"]][["data"]], list(0.631, 80.588, 4, 1, 25))
  jaspTools::expect_equal_tables(aaaWithout[["AAAtableGraphs_AllVsStandard"]][["data"]], list(0.631, 80.588, 4, 1, 25))

  for (name in agreementTables) {
    expect_false(is.null(aaa[[name]]), info = name)
    expect_null(aaa[[name]][["error"]], info = name)
    expect_equal(aaa[[name]][["data"]], aaaWithout[[name]][["data"]], info = name)
  }
  expect_false(is.null(aaa[["AAAtableGraphs_PlotWithin"]]))
  expect_false(is.null(aaa[["AAAtableGraphs_PlotVs"]]))

  effectiveness <- aaa[["AAAtableGraphs_StudyEffectiveness"]]
  expect_equal(effectiveness[["error"]][["errorMessage"]],
               "The miss rate and false alarm rate need exactly two categories in the results and the standard.")

  # Cohen's kappa (unweighted) does not need two categories: A 0.4286, B 0.6, all 0.5122.
  cohen <- withReference[["results"]][["cohensKappa"]]
  expect_null(cohen[["error"]])
  jaspTools::expect_equal_tables(cohen[["data"]], list("A", 0.428571428571429, "B", 0.6, "All", 0.51219512195122))
})

test_that("Two categories still give the miss and false alarm rates", {
  data <- data.frame(Operator = rep(c("A", "B"), each = 5),
                     Part     = rep(1:5, 2),
                     T1       = c("Go", "NoGo", "Go", "NoGo", "Go", "Go", "NoGo", "Go", "Go", "NoGo"),
                     T2       = c("Go", "NoGo", "NoGo", "NoGo", "Go", "NoGo", "NoGo", "Go", "NoGo", "NoGo"),
                     Ref      = rep(c("Go", "NoGo", "Go", "Go", "NoGo"), 2),
                     stringsAsFactors = FALSE)
  options <- .msaAttributeWideOptions("Go", TRUE)
  results <- runAnalysis("msaAttribute", data, options, makeTests = FALSE)
  effectiveness <- results[["results"]][["AAAtableGraphs"]][["collection"]][["AAAtableGraphs_StudyEffectiveness"]]
  expect_null(effectiveness[["error"]])
  # A: miss 2/4 Go ratings of NoGo parts, false alarm 3/6 NoGo ratings of Go parts, effectiveness 2/5.
  # B: miss 0/4, false alarm 2/6, effectiveness 3/5.
  jaspTools::expect_equal_tables(effectiveness[["data"]],
                                 list("A", "40 (unacceptable)", "50 (unacceptable)", "50 (unacceptable)",
                                      "B", "60 (unacceptable)", "33.33 (unacceptable)", "0 (acceptable)"))
})
