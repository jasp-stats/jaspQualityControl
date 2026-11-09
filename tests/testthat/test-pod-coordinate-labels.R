podCoordinateFixture <- function() {
  hits <- c(2, 3, 7, 12, 16, 18)
  data.frame(size = rep(exp(1:6), each = 20),
             detected = factor(unlist(lapply(hits, function(n) c(rep('no', 20 - n), rep('yes', n)))),
                               levels = c('no', 'yes')))
}
podCoordinateOptions <- function(transform = FALSE, displayLog = FALSE, link = 'logit') {
  options <- jaspTools::analysisOptions('probabilityOfDetection')
  options$covariate <- 'size'
  options$outcome <- 'detected'
  options$logTransformedCovariate <- transform
  options$logarithmicXAxis <- displayLog
  options$linkFunction <- link
  options$xAxisTicksType <- 'dataBased'
  options$detectionPlotDataDisplayType <- 'points'
  options$detectionPlotDataDisplayTypePointsJitter <- FALSE
  options$detectionPlotDensityDisplay <- FALSE
  options$horizontalAsymptotes <- list(
    list(horizontalAsymptoteName = 'p=0.9', horizontalAsymptoteValue = 0.9, value = '#'))
  options$verticalAsymptotes <- list(
    list(verticallAsymptoteName = 'x=3', verticalAsymptoteValue = 3, value = '#'))
  options
}
runPodCoordinates <- function(data, options, encoded = FALSE) {
  loaderCalls <- 0L
  originalReader <- getFromNamespace('.readDataSetToEnd', 'jaspBase')
  testthat::local_mocked_bindings(.readDataSetToEnd = function(...) {
    loaderCalls <<- loaderCalls + 1L
    originalReader(...)
  }, .package = 'jaspQualityControl')
  result <- jaspTools::runAnalysis('probabilityOfDetection', data, options,
                                  encodedDataset = encoded, view = FALSE)
  list(result = result, loaderCalls = loaderCalls)
}
podCoordinateTables <- function(result) {
  result$results$mainContainer$collection$mainContainer_asymptoteContainer$collection
}
podCoordinateTitle <- function(table) {
  fields <- table$schema$fields
  fields[[which(vapply(fields, function(field) field$name == 'x', logical(1)))]]$title
}

test_that('POD table and plot coordinates reflect transformation, not display scale', {
  data <- podCoordinateFixture()
  for (link in c('logit', 'probit')) {
    for (transform in c(FALSE, TRUE, FALSE)) {
      for (displayLog in c(FALSE, TRUE)) {
        options <- podCoordinateOptions(transform, displayLog, link)
        output <- runPodCoordinates(data, options)
        result <- output$result
        expect_identical(output$loaderCalls, 1L)
        expect_identical(result$status, 'complete')
        coords <- if (transform) log(data$size) else data$size
        fit <- glm(data$detected ~ coords, family = binomial(link))
        expectedX <- (family(fit)$linkfun(.9) - coef(fit)[1]) / coef(fit)[2]
        expectedY <- family(fit)$linkinv(coef(fit)[1] + 3 * coef(fit)[2])
        label <- if (transform) 'log(size)' else 'size'
        tables <- podCoordinateTables(result)
        expect_length(tables, 2)
        for (table in tables) {
          expect_identical(podCoordinateTitle(table), label)
          expect_identical(length(table$footnotes) > 0, transform)
        }
        vertical <- tables$mainContainer_asymptoteContainer_xAsymptotesTable$data[[1]]
        horizontal <- tables$mainContainer_asymptoteContainer_yAsymptotesTable$data[[1]]
        expect_equal(vertical$x, 3)
        expect_equal(vertical$y, unname(expectedY), tolerance = 1e-12)
        # The existing inverse uses uniroot's default tolerance (~1.2e-4).
        expect_equal(horizontal$x, unname(expectedX), tolerance = 1e-4)
        expect_equal(horizontal$y, .9)
        container <- result$results$mainContainer$collection
        parameters <- container$mainContainer_mainTable$data
        expect_equal(vapply(parameters, function(row) row$estimates, numeric(1)), unname(coef(fit)),
                     tolerance = 1e-12)
        expect_identical(parameters[[2]]$parameters, label)
        plot <- result$state$figures[[container$mainContainer_detectionPlot$data]]$obj
        expect_identical(plot$scales$get_scales('x')$name, label)
        points <- which(vapply(plot$layers, function(layer) inherits(layer$geom, 'GeomPoint'), logical(1)))
        expect_equal(plot$layers[[points]]$data$x, coords)
      }
    }
  }
})

test_that('POD empty reference tables show the pending coordinate scale', {
  for (transform in c(FALSE, TRUE)) {
    options <- podCoordinateOptions(transform)
    options$covariate <- ''
    output <- runPodCoordinates(podCoordinateFixture(), options)
    expect_identical(output$loaderCalls, 0L)
    expect_identical(output$result$status, 'complete')
    for (table in podCoordinateTables(output$result)) {
      expect_length(table$data, 0)
      expect_identical(podCoordinateTitle(table), if (transform) 'log(covariate)' else 'covariate')
    }
  }
})

test_that('POD preserves missing-row and log-finiteness filtering', {
  data <- podCoordinateFixture()
  data$size[c(1, 2)] <- c(NA, 0)
  data$detected[3] <- NA
  options <- podCoordinateOptions(TRUE)
  output <- runPodCoordinates(data, options)
  expect_identical(output$loaderCalls, 1L)
  keep <- complete.cases(data) & data$size > 0
  fit <- glm(data$detected[keep] ~ log(data$size[keep]), family = binomial('logit'))
  rows <- output$result$results$mainContainer$collection$mainContainer_mainTable$data
  expect_equal(vapply(rows, function(row) row$estimates, numeric(1)), unname(coef(fit)), tolerance = 1e-12)
})

test_that('POD decodes names before marking transformed coordinates', {
  data <- podCoordinateFixture()
  names(data)[1] <- 'Crack size (mm)'
  options <- podCoordinateOptions(TRUE)
  options$covariate <- names(data)[1]
  options$covariate.types <- 'scale'
  options$outcome.types <- 'nominal'
  encoded <- jaspTools:::encodeOptionsAndDataset(options, data)
  # Standalone R has no native decoder: substitute only that display contract.
  originalDecoder <- jaspBase::decodeColNames
  testthat::local_mocked_bindings(decodeColNames = function(x, ...) {
    decoded <- originalDecoder(x, ...)
    if (is.character(decoded)) {
      index <- match(decoded, encoded$encodingMap$encoded)
      decoded[!is.na(index)] <- encoded$encodingMap$original[index[!is.na(index)]]
    }
    decoded
  }, .package = 'jaspBase')
  output <- runPodCoordinates(encoded$dataset, encoded$options, encoded = TRUE)
  expect_identical(output$loaderCalls, 1L)
  expect_identical(output$result$status, 'complete')
  for (table in podCoordinateTables(output$result))
    expect_identical(podCoordinateTitle(table), 'log(Crack size (mm))')
  container <- output$result$results$mainContainer$collection
  plot <- output$result$state$figures[[container$mainContainer_detectionPlot$data]]$obj
  expect_identical(plot$scales$get_scales('x')$name, 'log(Crack size (mm))')
})
