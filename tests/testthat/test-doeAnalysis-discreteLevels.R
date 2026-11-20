context("[Quality Control] DoE Analysis - discrete factor levels")

# JASP Data Library, FactorialDesignAnalysis: three two-level discrete factors, full factorial model.
factorialExample <- data.frame(
  Exposure_time = factor(rep(c("below", "above"), each = 2, times = 4)),
  Develop_time = factor(rep(rep(c("30", "45"), each = 4), times = 2)),
  Mask_dimension = factor(rep(c("small", "large"), each = 8)),
  Yield = c(8, 9, 34, 52, 16, 22, 45, 60, 6, 10, 30, 50, 15, 21, 44, 63)
)

test_that("Factor term lookup handles single-level factors and spaces in names", {
  data <- data.frame(single = factor(c("a", "a")), check.names = FALSE)
  data[["factor with spaces"]] <- factor(c("low", "high"), levels = c("low", "high"))
  map <- jaspQualityControl:::.doeFactorTermMap(c("single", "factor with spaces"), data)
  expect_identical(map, c("factor with spaces1" = "factor with spaces"))
  expect_identical(jaspQualityControl:::.doeCoefficientLevels(names(map), unname(map), data), "low")
  expect_length(jaspQualityControl:::.doeFactorTermMap("single", data), 0)
})

test_that("Uncoded discrete effects identify their contrast levels", {
  options <- analysisOptions("doeAnalysis")
  options$contourSurfacePlotResponseDivision <- 5
  options$dependentFactorial <- "Yield"
  options$fixedFactorsFactorial <- c("Exposure_time", "Develop_time", "Mask_dimension")
  options$highestOrder <- FALSE
  options$modelTerms <- lapply(list("Exposure_time", "Develop_time", "Mask_dimension",
                                    c("Exposure_time", "Develop_time"), c("Exposure_time", "Mask_dimension"),
                                    c("Develop_time", "Mask_dimension"),
                                    c("Exposure_time", "Develop_time", "Mask_dimension")),
                               function(term) list(components = term))
  options$codeFactors <- FALSE
  options$tableAlias <- FALSE
  results <- runAnalysis("doeAnalysis", factorialExample, options, view = FALSE)
  collection <- results[["results"]][["Yield"]][["collection"]]
  levels <- collection[["Yield_tableCoefficientsLegend"]][["data"]]
  expect_equal(vapply(levels, `[[`, character(1), "factorLevel"), c("above", "30", "large"))
  coefficients <- collection[["Yield_tableCoefficients"]]
  exposure <- Filter(function(row) row[["terms"]] == "Exposure_time1", coefficients[["data"]])[[1]]
  expect_equal(exposure[["coef"]], 16.9375, tolerance = 1e-8)
  expect_equal(exposure[["effects"]], 33.875, tolerance = 1e-8)
  expect_match(paste(vapply(coefficients[["footnotes"]], `[[`, character(1), "text"), collapse = " "),
               "contrast +1", fixed = TRUE)

  options$codeFactors <- TRUE
  coded <- runAnalysis("doeAnalysis", factorialExample, options, view = FALSE)[["results"]][["Yield"]][["collection"]]
  exposureCoded <- Filter(function(row) row[["terms"]] == "Exposure_time", coded[["Yield_tableCoefficients"]][["data"]])[[1]]
  expect_equal(exposureCoded[["coef"]], -16.9375, tolerance = 1e-8)
  expect_equal(exposureCoded[["effects"]], -33.875, tolerance = 1e-8)
  expect_equal(coded[["Yield_tableSummary"]][["data"]][[1]][["rsq"]],
               collection[["Yield_tableSummary"]][["data"]][[1]][["rsq"]])
})
