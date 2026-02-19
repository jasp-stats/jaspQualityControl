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

  ready <- length(variables) >= 2

  if (is.null(dataset)) {
    if (ready) {
      dataset <- .readDataSetToEnd(columns.as.numeric = variables)
    } else {
      dataset <- .readDataSetToEnd()
    }
  }

  if (ready) {
    .hasErrors(dataset,
               type = c("infinity", "observations"),
               infinity.target  = variables,
               observations.amount = "< 3",
               observations.target = variables,
               exitAnalysisIfErrors = TRUE)
  }

  .multivariateComputeModel(jaspResults, dataset, options, variables, ready)
  .multivariateTsqChart(jaspResults, dataset, options, variables, ready)
  .multivariateSummaryTable(jaspResults, dataset, options, variables, ready)
  .multivariateCenterTable(jaspResults, dataset, options, variables, ready)
  .multivariateCovarianceTable(jaspResults, dataset, options, variables, ready)
  .multivariateTsqTable(jaspResults, dataset, options, variables, ready)
  .multivariateExportTsqColumn(jaspResults, dataset, options, variables, ready)
}

.multivariateDependencies <- function() {
  c("variables", "confidenceLevel", "confidenceLevelAutomatic")
}

.multivariateComputeModel <- function(jaspResults, dataset, options, variables, ready) {
  if (!is.null(jaspResults[["modelState"]]))
    return()

  modelState <- createJaspState()
  modelState$dependOn(.multivariateDependencies())
  jaspResults[["modelState"]] <- modelState

  if (!ready)
    return()

  dataMatrix <- as.data.frame(dataset[, variables, drop = FALSE])
  # remove rows with any NA
  completeRows <- stats::complete.cases(dataMatrix)
  nDropped <- sum(!completeRows)
  dataMatrix <- dataMatrix[completeRows, , drop = FALSE]

  p <- length(variables)
  if (options[["confidenceLevelAutomatic"]]) {
    confidenceLevel <- (1 - 0.0027)^p
  } else {
    confidenceLevel <- options[["confidenceLevel"]]
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
    confidenceLevel = confidenceLevel
  )
}

.multivariateTsqChart <- function(jaspResults, dataset, options, variables, ready) {
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
  tsqValues  <- as.numeric(mqccResult$statistics)
  ucl        <- as.numeric(mqccResult$limits[, "UCL"])
  n          <- length(tsqValues)
  sample     <- seq_len(n)

  # Determine which points exceed UCL
  violation  <- tsqValues > ucl
  dotColor   <- ifelse(violation, "red", "blue")

  pointData <- data.frame(
    sample      = sample,
    tsq         = tsqValues,
    dotColor    = dotColor,
    stringsAsFactors = FALSE
  )

  lcl <- as.numeric(mqccResult$limits[, "LCL"])

  # Axis breaks
  yBreakDeterminants <- c(tsqValues, ucl, lcl, 0)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yBreakDeterminants)
  yLimits <- range(yBreaks)

  xBreaks <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(sample)))
  if (xBreaks[1] == 0)
    xBreaks[1] <- 1
  xLimits <- c(0.5, max(xBreaks) * 1.2 + 0.5)

  # Control limit labels
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

  jaspPlot$plotObject <- plotObject
}

.multivariateSummaryTable <- function(jaspResults, dataset, options, variables, ready) {
  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  table <- createJaspTable(title = gettext("Hotelling T\u00B2 Control Chart Summary"))
  table$position <- 2
  table$info <- gettext("Summary of the Hotelling T\u00B2 control chart, including the number of variables, observations, confidence level, control limits, and the determinant of the covariance matrix.")
  table$dependOn(.multivariateDependencies())
  table$showSpecifiedColumnsOnly <- TRUE

  table$addColumnInfo(name = "numVariables",    title = gettext("Number of Variables"),    type = "integer")
  table$addColumnInfo(name = "numObservations", title = gettext("Number of Observations"), type = "integer")
  table$addColumnInfo(name = "confidenceLevel", title = gettext("Confidence Level"),       type = "number")
  table$addColumnInfo(name = "lcl",             title = gettext("LCL"),                    type = "number")
  table$addColumnInfo(name = "ucl",             title = gettext("UCL"),                    type = "number")
  table$addColumnInfo(name = "detS",            title = gettext("|S|"),                    type = "number")

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

  row <- list(
    numVariables    = length(variables),
    numObservations = length(mqccResult$statistics),
    confidenceLevel = stateObj$confidenceLevel,
    lcl             = as.numeric(mqccResult$limits[, "LCL"]),
    ucl             = as.numeric(mqccResult$limits[, "UCL"]),
    detS            = det(mqccResult$cov)
  )

  table$addRows(row)
}

.multivariateCenterTable <- function(jaspResults, dataset, options, variables, ready) {
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
}

.multivariateCovarianceTable <- function(jaspResults, dataset, options, variables, ready) {
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
}

.multivariateExportTsqColumn <- function(jaspResults, dataset, options, variables, ready) {
  if (!ready || !options[["addTsqToData"]] || options[["tsqColumn"]] == "")
    return()

  if (!is.null(jaspResults[["tsqColumn"]]))
    return()

  stateObj <- jaspResults[["modelState"]]$object
  if (is.null(stateObj) || !is.null(stateObj$error))
    return()

  mqccResult <- stateObj$mqccResult
  tsqValues  <- as.numeric(mqccResult$statistics)

  jaspResults[["tsqColumn"]] <- createJaspColumn(columnName = options[["tsqColumn"]])
  jaspResults[["tsqColumn"]]$dependOn(c(.multivariateDependencies(), "addTsqToData", "tsqColumn"))
  jaspResults[["tsqColumn"]]$setScale(tsqValues)
}

.multivariateTsqTable <- function(jaspResults, dataset, options, variables, ready) {
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
  tsqValues  <- as.numeric(mqccResult$statistics)
  ucl        <- as.numeric(mqccResult$limits[, "UCL"])
  lcl        <- as.numeric(mqccResult$limits[, "LCL"])

  # print both limits in a single footnote for reference
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
