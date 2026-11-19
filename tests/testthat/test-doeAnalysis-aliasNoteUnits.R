context("[Quality Control] DoE Analysis - aliased terms note and covariate units")

# jasp-stats/jasp-issues#4692: the partners named in the note on removed aliased terms depended on the units of the
# covariate (stats::alias() drops weights below 1e-6 in absolute terms; covariates are not coded).
aliasNoteData <- data.frame(A = rep(c(-1, 1, -1, 1), 2), B = rep(c(-1, -1, 1, 1), 2), y = c(5, 7, 6, 9, 5.5, 7.4, 6.2, 8.7))

aliasNoteOptions <- function() {
  options <- analysisOptions("doeAnalysis")
  options$dependentFactorial         <- "y"
  options$continuousFactorsFactorial <- c("A", "B")
  options$fixedFactorsFactorial      <- character(0)
  options$covariates                 <- "X"
  options$codeFactors                <- FALSE
  options$codeFactorsMethod          <- "automatic"
  options$highestOrder               <- FALSE
  options$modelTerms                 <- lapply(list("A", "B", c("A", "B")), function(x) list(components = x))
  options
}

aliasNoteRemoved <- function(results) {
  table <- results[["results"]][["y"]][["collection"]][["y_tableSummary"]]
  notes <- vapply(table[["footnotes"]], function(footnote) footnote[["text"]], character(1))
  grep("aliased terms were removed", notes, value = TRUE)
}

test_that("The note names the covariate an aliased term is aliased with, whatever the covariate units", {
  # X = scale * (5 + A x B), so A x B = X / scale - 5: aliased with X and the intercept
  for (scale in c(1e7, 1, 1e-7)) {
    data <- transform(aliasNoteData, X = scale * (5 + A * B))
    set.seed(1)
    results <- runAnalysis("doeAnalysis", data, aliasNoteOptions(), makeTests = FALSE)
    note    <- aliasNoteRemoved(results)
    expect_length(note, 1)
    expect_match(note, "zero: A[^,]+B \\(aliased with X, the intercept\\)[.]$", label = paste("note for X scale", scale))
    expect_identical(jaspQualityControl:::.doeAliasedWith(lm(y ~ A + B + X + A:B, data = data), "A:B"), "X, the intercept",
                     label = paste("partners for X scale", scale))
  }
})

test_that("An alias partner is found when squaring a covariate would overflow or underflow", {
  for (scale in c(1e-170, 1e155)) {
    data <- transform(aliasNoteData, X = scale * (5 + A * B))
    fit <- lm(y ~ A + B + X + A:B, data = data)
    expect_identical(jaspQualityControl:::.doeAliasedWith(fit, "A:B"), "X, the intercept")
  }
})

test_that("A covariate in tiny or large units that is not aliased is not named as a partner", {
  for (scale in c(1e-7, 1e7)) {
    data <- transform(aliasNoteData, C = A * B, X = scale * c(1, 3, 2, 5, 4, 7, 6, 9))
    expect_identical(jaspQualityControl:::.doeAliasedWith(lm(y ~ A + B + C + X + A:B, data = data), "A:B"), "C",
                     label = paste("partners for X scale", scale))
  }
})
