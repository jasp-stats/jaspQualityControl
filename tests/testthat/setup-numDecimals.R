# JASP Desktop defines the global `.numDecimals` (decimals preference) that the analyses read.
# jaspTools does not, and testthat evaluates test files in their own environment, so a
# `.numDecimals <- 2` at the top of a test file is not visible to the package code.
# Define it in the global environment for the duration of the test run, and restore it afterwards.
local({
  hadValue <- exists(".numDecimals", envir = globalenv(), inherits = FALSE)
  oldValue <- if (hadValue) get(".numDecimals", envir = globalenv(), inherits = FALSE)

  assign(".numDecimals", 2, envir = globalenv())

  withr::defer({
    if (hadValue)
      assign(".numDecimals", oldValue, envir = globalenv())
    else
      rm(".numDecimals", envir = globalenv())
  }, envir = testthat::teardown_env())
})
