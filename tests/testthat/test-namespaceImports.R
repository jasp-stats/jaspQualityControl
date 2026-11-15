context("[Quality Control] Namespace imports")

# jaspTools attaches jaspBase, so a jaspBase function called without `jaspBase::` and without importFrom(jaspBase, ...) in
# NAMESPACE works in the tests but fails when the engine runs the module without attaching jaspBase
# (jasp-stats/INTERNAL-jasp#3394). This test checks the sources statically.

test_that("every jaspBase function called in R/ is qualified or imported", {
  rDir   <- testthat::test_path("..", "..", "R")
  nsFile <- testthat::test_path("..", "..", "NAMESPACE")
  skip_if_not(dir.exists(rDir) && file.exists(nsFile))

  nsLines  <- readLines(nsFile)
  imported <- sub("^importFrom\\(jaspBase,(.*)\\)$", "\\1", grep("^importFrom\\(jaspBase,", nsLines, value = TRUE))
  imported <- gsub('"', "", imported, fixed = TRUE) # infix operators are written quoted, e.g. "%setOrRetrieve%"

  called <- unique(unlist(lapply(list.files(rDir, pattern = "\\.[Rr]$", full.names = TRUE), function(file) {
    tokens   <- utils::getParseData(parse(file, keep.source = TRUE))
    tokens   <- tokens[tokens$terminal, ]
    previous <- c(NA, head(tokens$text, -1))
    # infix operators such as %setOrRetrieve% are SPECIAL tokens
    tokens$text[tokens$token %in% c("SYMBOL_FUNCTION_CALL", "SPECIAL") & !previous %in% c("::", ":::", "$", "@")]
  })))

  unimported <- setdiff(intersect(called, getNamespaceExports("jaspBase")), imported)
  expect_equal(sort(unimported), character(0), info = paste("Not imported from jaspBase:", toString(unimported)))
})
