context("[Quality Control] MSA - Type 1 Instrument Capability - run chart limits and labels")

# Label boxes of the run chart as drawn at the size JASP renders it (700 x 400 px at 72 ppi), in npc of the
# panel (0 to 1 is inside the panel). ggrepel places the labels at draw time, so ggplot_build() does not show them.
.type1RunChartLabelBoxes <- function(plot, width = 700, height = 400) {
  grDevices::pdf(NULL, width = width / 72, height = height / 72)
  on.exit(grDevices::dev.off())
  gt <- ggplot2::ggplotGrob(plot)
  grid::grid.newpage()
  grid::grid.draw(gt)
  vpTree <- paste(utils::capture.output(print(grid::current.vpTree())), collapse = "")
  grid::seekViewport(regmatches(vpTree, regexpr("panel\\.[0-9]+-[0-9]+-[0-9]+-[0-9]+", vpTree)))

  findLabelTrees <- function(g) {
    if (inherits(g, "labelrepeltree")) return(list(g))
    if (inherits(g, "gTree")) return(unlist(lapply(g$children, findLabelTrees), recursive = FALSE))
    list()
  }
  labelTrees <- findLabelTrees(gt$grobs[[which(gt$layout$name == "panel")]])

  boxes <- lapply(labelTrees, function(tree) {
    set.seed(1)
    drawn <- grid::makeContent(tree)$children
    rects <- Filter(function(g) inherits(g, "roundrect"), drawn)
    texts <- Filter(function(g) inherits(g, "text"), drawn)
    do.call(rbind, Map(function(r, txt) {
      x <- grid::convertX(r$x, "npc", TRUE)
      y <- grid::convertY(r$y, "npc", TRUE)
      w <- grid::convertWidth(r$width, "npc", TRUE)
      h <- grid::convertHeight(r$height, "npc", TRUE)
      data.frame(label = txt$label, left = x - r$just[1] * w, right = x + (1 - r$just[1]) * w,
                 bottom = y - r$just[2] * h, top = y + (1 - r$just[2]) * h)
    }, rects, texts))
  })
  do.call(rbind, boxes)
}

.type1LabelsInsideAndApart <- function(boxes) {
  outside <- boxes$label[boxes$left < 0 | boxes$right > 1 | boxes$bottom < 0 | boxes$top > 1]
  expect_true(length(outside) == 0, info = sprintf("label boxes cut off at the panel edge: %s", toString(outside)))

  # the Mean and Ref. boxes do not cover each other
  meanBox <- boxes[boxes$label == "Mean", ]
  refBox  <- boxes[boxes$label == "Ref.", ]
  overlap <- meanBox$left < refBox$right && refBox$left < meanBox$right && meanBox$bottom < refBox$top && refBox$bottom < meanBox$top
  expect_false(overlap, info = "Mean and Ref. label boxes overlap")
}

.type1RunChartChecks <- function(plot, measurements, nSigma, reference, toleranceLines, expectedLabels) {
  m <- mean(measurements)
  s <- sd(measurements)
  referenceLines <- c(m + nSigma * s, m - nSigma * s, m, reference, toleranceLines)

  built <- ggplot2::ggplot_build(plot)
  xRange <- built$layout$panel_params[[1]]$x.range
  yRange <- built$layout$panel_params[[1]]$y.range

  # every reference line (incl. mean +/- k*s) lies inside the y limits, so none is dropped
  expect_true(all(referenceLines >= yRange[1] & referenceLines <= yRange[2]),
              info = sprintf("lines %s outside y range [%.3f, %.3f]", toString(round(referenceLines, 3)), yRange[1], yRange[2]))
  hlines <- unlist(lapply(built$data, function(d) d$yintercept))
  expect_equal(sort(hlines), sort(referenceLines), tolerance = 1e-8)

  # every label is kept (not dropped by the limits) and anchored inside the panel
  labelLayers <- Filter(function(d) !is.null(d$label), built$data)
  labels <- do.call(rbind, lapply(labelLayers, function(d) d[, c("label", "x", "y")]))
  expect_setequal(labels$label, expectedLabels)
  expect_false(anyNA(labels$x) || anyNA(labels$y))
  expect_true(all(labels$x >= xRange[1] & labels$x <= xRange[2]))
  expect_true(all(labels$y >= yRange[1] & labels$y <= yRange[2]))

  # the drawn label boxes are fully inside the panel (not cut off at its edges)
  boxes <- .type1RunChartLabelBoxes(plot)
  expect_setequal(boxes$label, expectedLabels)
  .type1LabelsInsideAndApart(boxes)

  # x axis title
  expect_identical(plot$scales$get_scales("x")$name, "Observation")
}

.type1RunChartPlot <- function(results) {
  plotName <- results[["results"]][["biasRun"]][["data"]]
  results[["state"]][["figures"]][[plotName]][["obj"]]
}

test_that("Run chart of the Type 1 instrument capability example keeps all lines and label boxes inside the plot", {
  jaspFile <- testthat::test_path("..", "..", "examples", "Type1InstrumentCapability.jasp")
  opts     <- jaspTools::analysisOptions(jaspFile)
  dataset  <- jaspTools::extractDatasetFromJASPFile(jaspFile)
  set.seed(1)
  results  <- jaspTools::runAnalysis("msaType1Gauge", dataset, opts)

  # jaspTools (from its PR #101) gives a .jasp variables option as list(types = , value = ); before, the column name
  measurement  <- opts[["measurement"]]
  measurements <- dataset[[unlist(if (is.list(measurement) && "value" %in% names(measurement)) measurement[["value"]] else measurement)]]
  tolerance    <- opts[["referenceValue"]] + c(1, -1) * opts[["percentToleranceForCg"]] / 200 * opts[["toleranceRange"]]
  .type1RunChartChecks(.type1RunChartPlot(results), measurements[!is.na(measurements)],
                       nSigma = as.numeric(opts[["studyVarianceMultiplier"]]) / 2, reference = opts[["referenceValue"]],
                       toleranceLines = if (opts[["runChartToleranceLimitLines"]]) tolerance,
                       expectedLabels = c("Mean", "Ref.", "Mean + 3s", "Mean - 3s", "Ref. + 0.10 * tol.", "Ref. - 0.10 * tol."))
})

test_that("Run chart without tolerance lines keeps the Mean +/- 3s lines and labels inside the plot", {
  options <- analysisOptions("msaType1Gauge")
  options$measurement                       <- "dm"
  options$referenceValue                    <- -4
  options$toleranceRange                    <- 15
  options$percentToleranceForCg             <- 20
  options$studyVarianceMultiplier           <- 6
  options$runChart                          <- TRUE
  options$runChartIndividualMeasurementDots <- TRUE
  options$runChartToleranceLimitLines       <- FALSE
  set.seed(1)
  results <- runAnalysis("msaType1Gauge", "datasets/msaType1InstrumentCapability/msaType1.csv", options)

  data <- read.csv(testthat::test_path("datasets", "msaType1InstrumentCapability", "msaType1.csv"))
  # mean = -3.946, s = 0.420: Mean + 3s = -2.686 lies above the data (max -3.4) and the reference (-4)
  .type1RunChartChecks(.type1RunChartPlot(results), data[["dm"]], nSigma = 3, reference = -4, toleranceLines = NULL,
                       expectedLabels = c("Mean", "Ref.", "Mean + 3s", "Mean - 3s"))
})

test_that("Run chart with mean equal to the reference shows both the Mean and Ref. labels", {
  options <- analysisOptions("msaType1Gauge")
  options$measurement                       <- "dmMissing25"
  options$referenceValue                    <- -4
  options$toleranceRange                    <- 15
  options$percentToleranceForCg             <- 20
  options$studyVarianceMultiplier           <- 6
  options$runChart                          <- TRUE
  options$runChartIndividualMeasurementDots <- TRUE
  options$runChartToleranceLimitLines       <- TRUE
  set.seed(1)
  results <- runAnalysis("msaType1Gauge", "datasets/msaType1InstrumentCapability/msaType1.csv", options)

  data <- read.csv(testthat::test_path("datasets", "msaType1InstrumentCapability", "msaType1.csv"))
  measurements <- data[["dmMissing25"]][!is.na(data[["dmMissing25"]])]
  expect_equal(mean(measurements), -4)
  .type1RunChartChecks(.type1RunChartPlot(results), measurements, nSigma = 3, reference = -4, toleranceLines = c(-2.5, -5.5),
                       expectedLabels = c("Mean", "Ref.", "Mean + 3s", "Mean - 3s", "Ref. + 0.10 * tol.", "Ref. - 0.10 * tol."))

  # plot resized narrower in JASP: the labels still stay inside and Mean and Ref. do not cover each other
  .type1LabelsInsideAndApart(.type1RunChartLabelBoxes(.type1RunChartPlot(results), width = 450))
})
