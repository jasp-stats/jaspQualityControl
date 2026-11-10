phaseFixture <- function(n, missing = FALSE) {
  train <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(3, 1, 5, 2, 6, 4), phase = 'train')
  test <- data.frame(x = seq_len(n) + 6, y = seq_len(n) * 0.7 + 1, phase = 'test')
  data <- rbind(train, test)
  data <- data[order(c(seq(1, by = 2, length.out = 6), seq(2, by = 2, length.out = n))), ]
  if (missing)
    data <- rbind(data, data.frame(x = NA, y = 1, phase = 'test'), data.frame(x = 1, y = 2, phase = NA))
  rownames(data) <- NULL
  data$phase <- factor(data$phase, levels = c('train', 'test'))
  data$label <- paste('Observation', seq_len(nrow(data)))
  if (missing) data$label[2] <- NA_character_
  data
}
phaseOptions <- function(labels = FALSE) {
  options <- jaspTools::analysisOptions('multivariateControlCharts')
  options$variables <- c('x', 'y')
  options$stage <- 'phase'
  options$trainingLevel <- 'train'
  options$tSquaredValuesTable <- TRUE
  options$confidenceLevelAutomatic <- FALSE
  options$confidenceLevel <- 0.99
  if (labels) options$axisLabels <- 'label'
  options
}

test_that('Phase II supports short and long test samples with aligned labels', {
  for (n in c(1, 2, 4, 20)) {
    for (missing in c(FALSE, TRUE)) {
      data <- phaseFixture(n, missing)
      options <- phaseOptions(labels = missing)
      results <- jaspTools::runAnalysis('multivariateControlCharts', data, options, view = FALSE)
      model <- results$state$other[[1]]
      expect_null(model$error)
      if (!is.null(model$error)) next
      keep <- complete.cases(data[c('x', 'y', 'phase')])
      train <- which(keep & data$phase == 'train')
      test <- which(keep & data$phase == 'test')
      expected <- mahalanobis(data[test, c('x', 'y')], colMeans(data[train, c('x', 'y')]),
                              cov(data[train, c('x', 'y')]))
      expect_equal(unname(model$mqccResult$newstats), unname(expected), tolerance = 1e-12)
      expect_identical(names(model$mqccResult$newstats), as.character(test))
      expect_equal(model$nTraining, length(train))
      expect_equal(model$nTest, n)
      expect_equal(model$nDropped, if (missing) 2 else 0)
      rows <- results$results$tsqValuesTable$data
      expect_length(rows, length(train) + n)
      expect_equal(vapply(tail(rows, n), function(row) row$tsq, numeric(1)), unname(expected), tolerance = 1e-12)
      plotKey <- results$results$tsqChart$data
      plot <- results$state$figures[[plotKey]]$obj
      expect_equal(tail(plot$data$tsq, n), unname(expected), tolerance = 1e-12)
      if (missing) {
        rowOrder <- c(train, test)
        present <- which(!is.na(data$label[rowOrder]))
        expect_identical(vapply(rows[present], function(row) row$timestamp, character(1)),
                         data$label[rowOrder[present]])
      }
    }
  }
})

test_that('Phase I alone retains the sample covariance statistic', {
  data <- phaseFixture(4)[1:6, ]
  options <- phaseOptions()
  options$stage <- ''
  results <- jaspTools::runAnalysis('multivariateControlCharts', data, options, view = FALSE)
  model <- results$state$other[[1]]
  expected <- mahalanobis(data[c('x', 'y')], colMeans(data[c('x', 'y')]), cov(data[c('x', 'y')]))
  expect_null(model$error)
  expect_equal(unname(model$mqccResult$statistics), unname(expected), tolerance = 1e-12)
  expect_null(model$mqccResult$newstats)
})

test_that('a staged analysis with no complete test rows retains Phase I', {
  data <- phaseFixture(2)
  data$x[data$phase == 'test'] <- NA_real_
  results <- jaspTools::runAnalysis('multivariateControlCharts', data, phaseOptions(), view = FALSE)
  model <- results$state$other[[1]]
  train <- data[data$phase == 'train', c('x', 'y')]
  expect_null(model$error)
  expect_false(model$hasTestData)
  expect_equal(unname(model$mqccResult$statistics),
               unname(mahalanobis(train, colMeans(train), cov(train))), tolerance = 1e-12)
  expect_null(model$mqccResult$newstats)
})
