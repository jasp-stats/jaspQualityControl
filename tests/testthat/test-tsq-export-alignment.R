# Capture the real column API argument; standalone R has no Desktop writer.
# All loading, filtering, computation, output and setScale forwarding still run.
runTsqExport <- function(data, options) {
  sink <- new.env(parent = emptyenv())
  columnClass <- getFromNamespace('jaspColumnR', 'jaspBase')
  originalSetter <- columnClass$public_methods$setScale
  captureSetter <- originalSetter
  body(captureSetter) <- substitute({
    assign('values', scalarData, envir = SINK)
    private$jaspObject$setScale(scalarData, computed)
  }, list(SINK = sink))
  columnClass$set('public', 'setScale', captureSetter, overwrite = TRUE)
  on.exit(columnClass$set('public', 'setScale', originalSetter, overwrite = TRUE))
  result <- jaspTools::runAnalysis('multivariateControlCharts', data, options, view = FALSE)
  list(result = result, values = sink$values)
}
exportFixture <- function() {
  data.frame(x = c(1, 4, 2, 7, 6, 9, 3, 11, 8, 13, 5, 12),
             y = c(2, 1, 6, 3, 8, 7, 12, 5, 10, 9, 4, 14),
             phase = factor(ifelse(seq_len(12) %in% c(2, 5, 8, 11), 'test', 'train')),
             label = paste('row', seq_len(12)))
}
exportOptions <- function(staged = FALSE) {
  options <- jaspTools::analysisOptions('multivariateControlCharts')
  options$variables <- c('x', 'y')
  options$addTsqToData <- TRUE
  options$tsqColumn <- 'T2'
  options$tSquaredValuesTable <- TRUE
  options$axisLabels <- 'label'
  if (staged) {
    options$stage <- 'phase'
    options$trainingLevel <- 'train'
  }
  options
}

test_that('T-squared export retains input positions and missing entries', {
  for (staged in c(FALSE, TRUE)) {
    for (missing in list(integer(), 1L, 6L, 12L, c(1L, 6L, 12L))) {
      data <- exportFixture()
      data$x[missing] <- NA_real_
      data$label[3] <- NA_character_
      # Native filters can leave nonconsecutive physical rownames in input.
      rownames(data) <- as.character(seq(11, by = 2, length.out = nrow(data)))
      options <- exportOptions(staged)
      output <- runTsqExport(data, options)
      expect_identical(output$result$status, 'complete')
      expect_null(output$result$state$other[[1]]$error)
      keep <- complete.cases(data[c('x', 'y')])
      training <- if (staged) keep & data$phase == 'train' else keep
      expected <- rep(NA_real_, nrow(data))
      expected[keep] <- mahalanobis(data[keep, c('x', 'y')], colMeans(data[training, c('x', 'y')]),
                                    cov(data[training, c('x', 'y')]))
      expect_length(output$values, nrow(data))
      expect_identical(which(is.na(output$values)), missing)
      expect_equal(output$values, expected, tolerance = 1e-12)
      expect_equal(length(unique(expected[keep])), sum(keep))
      expect_false(is.na(output$values[3]))
      rows <- output$result$results$tsqValuesTable$data
      order <- if (staged) c(which(training), which(keep & !training)) else which(keep)
      expect_equal(vapply(rows, function(row) row$tsq, numeric(1)), expected[order], tolerance = 1e-12)
    }
  }
})

test_that('missing stage values are restored without dropping optional labels', {
  data <- exportFixture()
  data$phase[7] <- NA
  data$label[3] <- NA_character_
  output <- runTsqExport(data, exportOptions(TRUE))
  keep <- complete.cases(data[c('x', 'y', 'phase')])
  train <- which(keep & data$phase == 'train')
  expected <- rep(NA_real_, nrow(data))
  expected[keep] <- mahalanobis(data[keep, c('x', 'y')], colMeans(data[train, c('x', 'y')]),
                                cov(data[train, c('x', 'y')]))
  expect_null(output$result$state$other[[1]]$error)
  expect_equal(output$values, expected, tolerance = 1e-12)
  expect_identical(which(is.na(output$values)), 7L)
  expect_false(is.na(output$values[3]))
})
