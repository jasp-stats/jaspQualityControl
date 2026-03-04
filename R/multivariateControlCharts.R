#
# Copyright (C) 2013-2026 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#' @export
multivariateControlCharts <- function(jaspResults, dataset, options) {

  variables <- unlist(options[["variables"]])
  variables <- variables[variables != ""]
  stage     <- unlist(options[["stage"]])
  stage     <- if (length(stage) == 0 || identical(stage, "")) "" else stage

  ready <- length(variables) >= 2

  if (is.null(dataset)) {
    numericCols <- if (ready) variables else NULL
    factorCols  <- if (stage != "") stage else NULL
    dataset <- .readDataSetToEnd(columns.as.numeric = numericCols,
                                 columns.as.factor  = factorCols)
  }

  if (ready) {
    .hasErrors(dataset,
               type = c("infinity", "observations", "variance"),
               infinity.target  = variables,
               observations.amount = "< 3",
               observations.target = variables,
               variance.target = variables,
               exitAnalysisIfErrors = TRUE)
  }

  # Validate stage column
  if (ready && stage != "") {
    stageLevels <- levels(dataset[[stage]])
    if (length(stageLevels) < 2)
      .quitAnalysis(gettext("The stage variable must have at least two levels to define training and test phases."))
  }

  .multivariateComputeModel(jaspResults, dataset, options, variables, stage, ready)
  .multivariateTsqChart(jaspResults, dataset, options, variables, stage, ready)
  .multivariateSummaryTable(jaspResults, dataset, options, variables, stage, ready)
  .multivariateCenterTable(jaspResults, dataset, options, variables, stage, ready)
  .multivariateCovarianceTable(jaspResults, dataset, options, variables, stage, ready)
  .multivariateTsqTable(jaspResults, dataset, options, variables, stage, ready)
  .multivariateExportTsqColumn(jaspResults, dataset, options, variables, stage, ready)
}

.multivariateDependencies <- function() {
  c("variables", "confidenceLevel", "confidenceLevelAutomatic", "stage", "trainingLevel")
}

.multivariateComputeModel <- function(jaspResults, dataset, options, variables, stage, ready) {
  if (!is.null(jaspResults[["modelState"]]))
    return()

  modelState <- createJaspState()
  modelState$dependOn(.multivariateDependencies())
  jaspResults[["modelState"]] <- modelState

  if (!ready)
    return()

  hasStage <- stage != ""

  dataMatrix <- as.data.frame(dataset[, variables, drop = FALSE])
  if (hasStage)
    stageVector <- dataset[[stage]]

  # remove rows with any NA across variables (and stage if present)
  if (hasStage) {
    completeRows <- stats::complete.cases(dataMatrix) & !is.na(stageVector)
  } else {
    completeRows <- stats::complete.cases(dataMatrix)
  }
  nDropped   <- sum(!completeRows)
  dataMatrix <- dataMatrix[completeRows, , drop = FALSE]
  if (hasStage)
    stageVector <- stageVector[completeRows]

  p <- length(variables)
  if (options[["confidenceLevelAutomatic"]]) {
    confidenceLevel <- (1 - 0.0027)^p
  } else {
    confidenceLevel <- options[["confidenceLevel"]]
  }

  if (hasStage) {
    # Phase I/II split
    trainingLevel <- options[["trainingLevel"]]
    if (trainingLevel == "")
      trainingLevel <- levels(stageVector)[1]

    isTraining     <- stageVector == trainingLevel
    trainingData   <- dataMatrix[isTraining, , drop = FALSE]
    testData       <- dataMatrix[!isTraining, , drop = FALSE]

    if (nrow(trainingData) < 3) {
      modelState$object <- list(error = gettext("The training phase must contain at least 3 observations."))
      return()
    }

    # Check singularity on training data covariance
    covMatrix <- stats::cov(trainingData)
    rcond     <- tryCatch(rcond(covMatrix), error = function(e) 0)
    if (rcond < .Machine$double.eps) {
      modelState$object <- list(error = gettext("The covariance matrix of the training phase is computationally singular. This typically occurs when variables are linearly dependent or nearly perfectly correlated. Please remove redundant variables."))
      return()
    }

    hasTestData <- nrow(testData) > 0
    if (hasTestData) {
      mqccResult <- try(qcc::mqcc(trainingData, type = "T2.single",
                                   newdata = testData,
                                   pred.limits = TRUE,
                                   confidence.level = confidenceLevel,
                                   plot = FALSE))
    } else {
      mqccResult <- try(qcc::mqcc(trainingData, type = "T2.single",
                                   confidence.level = confidenceLevel,
                                   plot = FALSE))
    }

    if (jaspBase::isTryError(mqccResult)) {
      modelState$object <- list(error = .extractErrorMessage(mqccResult))
      return()
    }

    phaseLabels <- as.character(stageVector)

    modelState$object <- list(
      mqccResult      = mqccResult,
      nDropped        = nDropped,
      confidenceLevel = confidenceLevel,
      hasStage        = TRUE,
      hasTestData     = hasTestData,
      trainingLevel   = trainingLevel,
      phaseLabels     = phaseLabels,
      nTraining       = nrow(trainingData),
      nTest           = nrow(testData)
    )

  } else {
    # Single-phase (original behavior)
    covMatrix <- stats::cov(dataMatrix)
    rcond     <- tryCatch(rcond(covMatrix), error = function(e) 0)
    if (rcond < .Machine$double.eps) {
      modelState$object <- list(error = gettext("The covariance matrix of the selected variables is computationally singular. This typically occurs when variables are linearly dependent or nearly perfectly correlated. Please remove redundant variables."))
      return()
    }

    mqccResult <- try(qcc::mqcc(dataMatrix, type = "T2.single",
                                 confidence.level = confidenceLevel,
                                 plot = FALSE))

    if (jaspBase::isTryError(mqccResult)) {
      modelState$object <- list(error = .extractErrorMessage(mqccResult))
      return()
    }

    modelState$object <- list(
      mqccResult      = mqccResult,
      nDropped        = nDropped,
      confidenceLevel = confidenceLevel,
      hasStage        = FALSE
    )
  }
}

.multivariateTsqChart <- function(jaspResults, dataset, options, variables, stage, ready) {
  if (!is.null(jaspResults[["tsqChart"]]))
    return()

  chartTitle <- gettext("Hotelling T\u00B2 Control Chart")
  jaspPlot <- createJaspPlot(title = chartTitle, width = 700, height = 400)
  jaspPlot$position <- 1
  jaspPlot$info <- gettext("Displays the Hotelling T\u00B2 statistic for each sample with upper and lower control limits. Points exceeding the UCL are flagged as out of control.")
  jaspPlot$dependOn(.multivariateDependencies())
  jaspResults[["tsqChart"]] <- jaspPlot

  if (!ready)
    return()

  stateObj <- jaspResults[["modelState"]]$object
  if (!is.null(stateObj$error)) {
    jaspPlot$setError(stateObj$error)
    return()
  }

  mqccResult <- stateObj$mqccResult

  if (isTRUE(stateObj$hasStage) && isTRUE(stateObj$hasTestData)) {
    jaspPlot$plotObject <- .multivariateTsqChartPhased(stateObj)
  } else {
    jaspPlot$plotObject <- .multivariateTsqChartSingle(stateObj)
  }
}

.multivariateTsqChartSingle <- function(stateObj) {
  mqccResult <- stateObj$mqccResult
  tsqValues  <- as.numeric(mqccResult$statistics)
  ucl        <- as.numeric(mqccResult$limits[, "UCL"])
  lcl        <- as.numeric(mqccResult$limits[, "LCL"])
  n          <- length(tsqValues)
  sample     <- seq_len(n)

  violation  <- tsqValues > ucl
  dotColor   <- ifelse(violation, "red", "blue")

  pointData <- data.frame(
    sample   = sample,
    tsq      = tsqValues,
    dotColor = dotColor,
    stringsAsFactors = FALSE
  )

  yBreakDeterminants <- c(tsqValues, ucl, lcl, 0)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yBreakDeterminants)
  yLimits <- range(yBreaks)

  xBreaks <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(sample)))
  if (xBreaks[1] == 0)
    xBreaks[1] <- 1
  xLimits <- c(0.5, max(xBreaks) * 1.2 + 0.5)

  labelX <- max(xLimits) * 0.95
  limitLabels <- data.frame(
    x     = c(labelX, labelX),
    y     = c(ucl, lcl),
    label = c(gettextf("UCL = %s", round(ucl, 3)),
              gettextf("LCL = %s", round(lcl, 3)))
  )

  plotObject <- ggplot2::ggplot(pointData, ggplot2::aes(x = sample, y = tsq)) +
    ggplot2::geom_hline(yintercept = ucl, col = "red", linewidth = 1.5, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = lcl, col = "red", linewidth = 1.5, linetype = "dashed") +
    jaspGraphs::geom_line(mapping = ggplot2::aes(x = sample, y = tsq), col = "blue", na.rm = TRUE) +
    jaspGraphs::geom_point(mapping = ggplot2::aes(x = sample, y = tsq),
                           size = 4, fill = dotColor, inherit.aes = TRUE, na.rm = TRUE) +
    ggplot2::geom_label(data = limitLabels, mapping = ggplot2::aes(x = x, y = y, label = label),
                        inherit.aes = FALSE, size = 4.5, na.rm = TRUE) +
    ggplot2::scale_y_continuous(name = gettext("Hotelling T\u00B2"), breaks = yBreaks, limits = yLimits) +
    ggplot2::scale_x_continuous(name = gettext("Sample"), breaks = xBreaks, limits = xLimits) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plotObject)
}

.multivariateTsqChartPhased <- function(stateObj) {
  mqccResult  <- stateObj$mqccResult
  nTraining   <- stateObj$nTraining
  nTest       <- stateObj$nTest
  nTotal      <- nTraining + nTest

  # T\u00B2 values
  phase1Tsq   <- as.numeric(mqccResult$statistics)
  phase2Tsq   <- as.numeric(mqccResult$newstats)
  allTsq      <- c(phase1Tsq, phase2Tsq)

  # Limits
  phase1Ucl   <- as.numeric(mqccResult$limits[, "UCL"])
  phase1Lcl   <- as.numeric(mqccResult$limits[, "LCL"])
  phase2Ucl   <- as.numeric(mqccResult$pred.limits[, "UPL"])
  phase2Lcl   <- as.numeric(mqccResult$pred.limits[, "LPL"])

  sample <- seq_len(nTotal)

  # Violations per phase
  violation1 <- phase1Tsq > phase1Ucl
  violation2 <- phase2Tsq > phase2Ucl
  dotColor   <- c(ifelse(violation1, "red", "blue"),
                  ifelse(violation2, "red", "blue"))

  # Phase label for grouping
  phase <- c(rep("Phase I", nTraining), rep("Phase II", nTest))

  pointData <- data.frame(
    sample   = sample,
    tsq      = allTsq,
    phase    = phase,
    dotColor = dotColor,
    stringsAsFactors = FALSE
  )

  # Control limit segments (per phase)
  clData <- data.frame(
    xmin  = c(0.5, nTraining + 0.5),
    xmax  = c(nTraining + 0.5, nTotal + 0.5),
    ucl   = c(phase1Ucl, phase2Ucl),
    lcl   = c(phase1Lcl, phase2Lcl),
    phase = c("Phase I", "Phase II"),
    stringsAsFactors = FALSE
  )

  # Axis
  yBreakDeterminants <- c(allTsq, phase1Ucl, phase1Lcl, phase2Ucl, phase2Lcl, 0)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yBreakDeterminants)
  yLimits <- range(yBreaks)

  xBreaks <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(sample)))
  if (xBreaks[1] == 0)
    xBreaks[1] <- 1
  xLimits <- c(0.5, max(xBreaks) * 1.2 + 0.5)

  # Limit labels at right edge of each phase
  labelData <- data.frame(
    x     = c(clData$xmax[1] - 0.5, max(xLimits) * 0.95,
              clData$xmax[1] - 0.5, max(xLimits) * 0.95),
    y     = c(phase1Ucl, phase2Ucl, phase1Lcl, phase2Lcl),
    label = c(gettextf("UCL = %s", round(phase1Ucl, 3)),
              gettextf("UCL = %s", round(phase2Ucl, 3)),
              gettextf("LCL = %s", round(phase1Lcl, 3)),
              gettextf("LCL = %s", round(phase2Lcl, 3))),
    stringsAsFactors = FALSE
  )

  # Phase label positions
  phaseLabelData <- data.frame(
    x     = c((0.5 + nTraining + 0.5) / 2,
              (nTraining + 0.5 + nTotal + 0.5) / 2),
    y     = rep(max(yLimits), 2),
    label = c(gettextf("Training (%s)", stateObj$trainingLevel),
              gettext("Test")),
    stringsAsFactors = FALSE
  )

  plotObject <- ggplot2::ggplot(pointData, ggplot2::aes(x = sample, y = tsq)) +
    # Limit lines per phase as segments
    ggplot2::geom_segment(data = clData,
                          mapping = ggplot2::aes(x = xmin, xend = xmax, y = ucl, yend = ucl),
                          col = "red", linewidth = 1.5, linetype = "dashed", inherit.aes = FALSE) +
    ggplot2::geom_segment(data = clData,
                          mapping = ggplot2::aes(x = xmin, xend = xmax, y = lcl, yend = lcl),
                          col = "red", linewidth = 1.5, linetype = "dashed", inherit.aes = FALSE) +
    # Phase separator
    ggplot2::geom_vline(xintercept = nTraining + 0.5, linetype = "solid", col = "darkgray", linewidth = 1) +
    # Data lines per phase (break at separator)
    jaspGraphs::geom_line(data = pointData[pointData$phase == "Phase I", ],
                          mapping = ggplot2::aes(x = sample, y = tsq), col = "blue", na.rm = TRUE) +
    jaspGraphs::geom_line(data = pointData[pointData$phase == "Phase II", ],
                          mapping = ggplot2::aes(x = sample, y = tsq), col = "blue", na.rm = TRUE) +
    jaspGraphs::geom_point(mapping = ggplot2::aes(x = sample, y = tsq),
                           size = 4, fill = dotColor, inherit.aes = TRUE, na.rm = TRUE) +
    # Limit labels
    ggplot2::geom_label(data = labelData, mapping = ggplot2::aes(x = x, y = y, label = label),
                        inherit.aes = FALSE, size = 3.5, na.rm = TRUE) +
    # Phase labels at top
    ggplot2::geom_text(data = phaseLabelData, mapping = ggplot2::aes(x = x, y = y, label = label),
                       inherit.aes = FALSE, size = 4, fontface = "bold", vjust = 1.5) +
    ggplot2::scale_y_continuous(name = gettext("Hotelling T\u00B2"), breaks = yBreaks, limits = yLimits) +
    ggplot2::scale_x_continuous(name = gettext("Sample"), breaks = xBreaks, limits = xLimits) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plotObject)
}

.multivariateSummaryTable <- function(jaspResults, dataset, options, variables, stage, ready) {
  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  table <- createJaspTable(title = gettext("Hotelling T\u00B2 Control Chart Summary"))
  table$position <- 2
  table$info <- gettext("Summary of the Hotelling T\u00B2 control chart, including the number of variables, observations, confidence level, control limits, and the determinant of the covariance matrix.")
  table$dependOn(.multivariateDependencies())
  table$showSpecifiedColumnsOnly <- TRUE

  jaspResults[["summaryTable"]] <- table

  if (!ready)
    return()

  stateObj <- jaspResults[["modelState"]]$object
  if (!is.null(stateObj$error)) {
    table$setError(stateObj$error)
    return()
  }

  mqccResult <- stateObj$mqccResult

  if (stateObj$nDropped > 0)
    table$addFootnote(gettextf("Removed %i observation(s) with missing values.", stateObj$nDropped))

  if (isTRUE(stateObj$hasStage) && isTRUE(stateObj$hasTestData)) {
    # Two-row table: one per phase
    table$addColumnInfo(name = "phase",           title = gettext("Phase"),                  type = "string")
    table$addColumnInfo(name = "numVariables",    title = gettext("Number of Variables"),    type = "integer")
    table$addColumnInfo(name = "numObservations", title = gettext("Number of Observations"), type = "integer")
    table$addColumnInfo(name = "confidenceLevel", title = gettext("Confidence Level"),       type = "number")
    table$addColumnInfo(name = "lcl",             title = gettext("LCL"),                    type = "number")
    table$addColumnInfo(name = "ucl",             title = gettext("UCL"),                    type = "number")
    table$addColumnInfo(name = "detS",            title = gettext("|S|"),                    type = "number")

    phase1Ucl <- as.numeric(mqccResult$limits[, "UCL"])
    phase1Lcl <- as.numeric(mqccResult$limits[, "LCL"])
    phase2Ucl <- as.numeric(mqccResult$pred.limits[, "UPL"])
    phase2Lcl <- as.numeric(mqccResult$pred.limits[, "LPL"])

    table$addRows(list(
      phase           = gettextf("Training (%s)", stateObj$trainingLevel),
      numVariables    = length(variables),
      numObservations = stateObj$nTraining,
      confidenceLevel = stateObj$confidenceLevel,
      lcl             = phase1Lcl,
      ucl             = phase1Ucl,
      detS            = det(mqccResult$cov)
    ))

    table$addRows(list(
      phase           = gettext("Test"),
      numVariables    = length(variables),
      numObservations = stateObj$nTest,
      confidenceLevel = stateObj$confidenceLevel,
      lcl             = phase2Lcl,
      ucl             = phase2Ucl,
      detS            = det(mqccResult$cov)
    ))

    table$addFootnote(gettext("Control limits for the training phase use the Beta distribution; test phase limits use the F distribution (prediction limits)."))

  } else {
    table$addColumnInfo(name = "numVariables",    title = gettext("Number of Variables"),    type = "integer")
    table$addColumnInfo(name = "numObservations", title = gettext("Number of Observations"), type = "integer")
    table$addColumnInfo(name = "confidenceLevel", title = gettext("Confidence Level"),       type = "number")
    table$addColumnInfo(name = "lcl",             title = gettext("LCL"),                    type = "number")
    table$addColumnInfo(name = "ucl",             title = gettext("UCL"),                    type = "number")
    table$addColumnInfo(name = "detS",            title = gettext("|S|"),                    type = "number")

    table$addRows(list(
      numVariables    = length(variables),
      numObservations = length(mqccResult$statistics),
      confidenceLevel = stateObj$confidenceLevel,
      lcl             = as.numeric(mqccResult$limits[, "LCL"]),
      ucl             = as.numeric(mqccResult$limits[, "UCL"]),
      detS            = det(mqccResult$cov)
    ))
  }
}

.multivariateCenterTable <- function(jaspResults, dataset, options, variables, stage, ready) {
  if (!options[["centerTable"]])
    return()

  if (!is.null(jaspResults[["centerTable"]]))
    return()

  table <- createJaspTable(title = gettext("Variable Centers"))
  table$position <- 3
  table$info <- gettext("Displays the sample mean of each variable used in the multivariate control chart.")
  table$dependOn(c(.multivariateDependencies(), "centerTable"))
  table$showSpecifiedColumnsOnly <- TRUE

  table$addColumnInfo(name = "variable", title = gettext("Variable"), type = "string")
  table$addColumnInfo(name = "center",   title = gettext("Center"),   type = "number")

  jaspResults[["centerTable"]] <- table

  if (!ready)
    return()

  stateObj <- jaspResults[["modelState"]]$object
  if (!is.null(stateObj$error)) {
    table$setError(stateObj$error)
    return()
  }

  mqccResult <- stateObj$mqccResult
  centers    <- mqccResult$center

  rows <- data.frame(
    variable = names(centers),
    center   = as.numeric(centers),
    stringsAsFactors = FALSE
  )

  table$setData(rows)

  if (isTRUE(stateObj$hasStage))
    table$addFootnote(gettextf("Centers estimated from training phase (%s) only.", stateObj$trainingLevel))
}

.multivariateCovarianceTable <- function(jaspResults, dataset, options, variables, stage, ready) {
  if (!options[["covarianceMatrixTable"]])
    return()

  if (!is.null(jaspResults[["covarianceTable"]]))
    return()

  table <- createJaspTable(title = gettext("Covariance Matrix"))
  table$position <- 4
  table$info <- gettext("Displays the sample covariance matrix of the selected variables, used to compute the Hotelling T\u00B2 statistic.")
  table$dependOn(c(.multivariateDependencies(), "covarianceMatrixTable"))
  table$showSpecifiedColumnsOnly <- TRUE

  jaspResults[["covarianceTable"]] <- table

  if (!ready)
    return()

  stateObj <- jaspResults[["modelState"]]$object
  if (!is.null(stateObj$error)) {
    table$setError(stateObj$error)
    return()
  }

  mqccResult <- stateObj$mqccResult
  covMatrix  <- mqccResult$cov
  varNames   <- colnames(covMatrix)

  # Row label column
  table$addColumnInfo(name = "variable", title = "", type = "string")

  for (v in varNames)
    table$addColumnInfo(name = v, title = v, type = "number")

  for (i in seq_along(varNames)) {
    row <- list(variable = varNames[i])
    for (j in seq_along(varNames))
      row[[varNames[j]]] <- covMatrix[i, j]
    table$addRows(row)
  }

  if (isTRUE(stateObj$hasStage))
    table$addFootnote(gettextf("Covariance matrix estimated from training phase (%s) only.", stateObj$trainingLevel))
}

.multivariateExportTsqColumn <- function(jaspResults, dataset, options, variables, stage, ready) {
  if (!ready || !options[["addTsqToData"]] || options[["tsqColumn"]] == "")
    return()

  if (!is.null(jaspResults[["tsqColumn"]]))
    return()

  stateObj <- jaspResults[["modelState"]]$object
  if (is.null(stateObj) || !is.null(stateObj$error))
    return()

  mqccResult <- stateObj$mqccResult

  if (isTRUE(stateObj$hasStage) && isTRUE(stateObj$hasTestData)) {
    # Concatenate Phase I and Phase II T² values in original row order
    phase1Tsq <- as.numeric(mqccResult$statistics)
    phase2Tsq <- as.numeric(mqccResult$newstats)
    phaseLabels <- stateObj$phaseLabels
    tsqValues <- numeric(length(phaseLabels))
    tsqValues[phaseLabels == stateObj$trainingLevel] <- phase1Tsq
    tsqValues[phaseLabels != stateObj$trainingLevel] <- phase2Tsq
  } else {
    tsqValues <- as.numeric(mqccResult$statistics)
  }

  jaspResults[["tsqColumn"]] <- createJaspColumn(columnName = options[["tsqColumn"]])
  jaspResults[["tsqColumn"]]$dependOn(c(.multivariateDependencies(), "addTsqToData", "tsqColumn"))
  jaspResults[["tsqColumn"]]$setScale(tsqValues)
}

.multivariateTsqTable <- function(jaspResults, dataset, options, variables, stage, ready) {
  if (!options[["tSquaredValuesTable"]])
    return()

  if (!is.null(jaspResults[["tsqValuesTable"]]))
    return()

  table <- createJaspTable(title = gettext("Hotelling T\u00B2 Values"))
  table$position <- 5
  table$info <- gettext("Lists the Hotelling T\u00B2 statistic for each sample and indicates whether the sample is in or out of control.")
  table$dependOn(c(.multivariateDependencies(), "tSquaredValuesTable"))
  table$showSpecifiedColumnsOnly <- TRUE

  table$addColumnInfo(name = "sample", title = gettext("Sample"), type = "integer")
  table$addColumnInfo(name = "tsq",    title = gettext("T\u00B2"),   type = "number")
  table$addColumnInfo(name = "status", title = gettext("Status"), type = "string")

  jaspResults[["tsqValuesTable"]] <- table

  if (!ready)
    return()

  stateObj <- jaspResults[["modelState"]]$object
  if (!is.null(stateObj$error)) {
    table$setError(stateObj$error)
    return()
  }

  mqccResult <- stateObj$mqccResult

  if (isTRUE(stateObj$hasStage) && isTRUE(stateObj$hasTestData)) {
    # Add phase column
    table$addColumnInfo(name = "phase", title = gettext("Phase"), type = "string", overtitle = "")

    phase1Tsq <- as.numeric(mqccResult$statistics)
    phase2Tsq <- as.numeric(mqccResult$newstats)
    allTsq    <- c(phase1Tsq, phase2Tsq)

    phase1Ucl <- as.numeric(mqccResult$limits[, "UCL"])
    phase2Ucl <- as.numeric(mqccResult$pred.limits[, "UPL"])

    nTraining <- stateObj$nTraining
    nTest     <- stateObj$nTest

    phase <- c(rep(gettextf("Training (%s)", stateObj$trainingLevel), nTraining),
               rep(gettext("Test"), nTest))
    ucl   <- c(rep(phase1Ucl, nTraining), rep(phase2Ucl, nTest))

    table$addFootnote(gettextf("Training UCL = %s, Test UCL = %s",
                               round(phase1Ucl, 4), round(phase2Ucl, 4)))

    rows <- data.frame(
      sample = seq_along(allTsq),
      tsq    = allTsq,
      status = ifelse(allTsq > ucl,
                      gettext("Out of control"),
                      gettext("In control")),
      phase  = phase,
      stringsAsFactors = FALSE
    )

    table$setData(rows)

  } else {
    tsqValues <- as.numeric(mqccResult$statistics)
    ucl       <- as.numeric(mqccResult$limits[, "UCL"])
    lcl       <- as.numeric(mqccResult$limits[, "LCL"])

    table$addFootnote(gettextf("UCL = %s, LCL = %s", round(ucl, 4), round(lcl, 4)))

    rows <- data.frame(
      sample = seq_along(tsqValues),
      tsq    = tsqValues,
      status = ifelse(tsqValues > ucl,
                      gettext("Out of control"),
                      gettext("In control")),
      stringsAsFactors = FALSE
    )

    table$setData(rows)
  }
}
