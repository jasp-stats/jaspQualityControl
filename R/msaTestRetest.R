#
# Copyright (C) 2013-2018 University of Amsterdam
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
msaTestRetest <- function(jaspResults, dataset, options, ...) {

  wideFormat <- options[["dataFormat"]] == "wideFormat"
  if (wideFormat) {
    measurements <- unlist(options[["measurementsWideFormat"]])
    parts <- unlist(options[["partWideFormat"]])
    factor.vars <- parts
  } else {
    measurements <- unlist(options[["measurementLongFormat"]])
    parts <- unlist(options[["partLongFormat"]])
    operators <- unlist(options[["operator"]])
    factor.vars <- c(parts, operators)
  }

  numeric.vars <- measurements
  numeric.vars <- numeric.vars[numeric.vars != ""]
  factor.vars <- factor.vars[factor.vars != ""]

  if (wideFormat){
    ready <- (length(measurements) > 1 && !identical(parts, ""))
  }else{
    ready <- (!identical(measurements, "") && !identical(operators, "") && !identical(parts, ""))
  }

  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = numeric.vars, columns.as.factor = factor.vars,
                                         exclude.na.listwise = c(numeric.vars, factor.vars))
  }

  .hasErrors(dataset, type = c('infinity', 'missingValues'),
             all.target = c(measurements, options[["operator"]], options[["part"]]),
             exitAnalysisIfErrors = TRUE)

  if (!wideFormat && ready) {
    dataset <- as.data.frame(tidyr::pivot_wider(dataset, values_from = measurements, names_from = operators))
    measurements <- names(dataset[-1])
  }

  # Range Method
  # Range Method r and R table
  if (options[["repeatabilityAndReproducibilityTable"]]) {
    .rAndRtableRange(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, jaspResults, ready = ready,
                     EnableSD = options[["manualProcessSd"]], EnableTol = options[["tolerance"]])
  }

  # Scatter Plot Operators vs Parts
  if (options[["runChartPart"]]) {
    if (is.null(jaspResults[["ScatterOperatorParts"]])) {
      jaspResults[["ScatterOperatorParts"]] <- createJaspContainer(gettext("Scatterplot operators vs parts"))
      jaspResults[["ScatterOperatorParts"]]$position <- 2
    }
    jaspResults[["ScatterOperatorParts"]] <- .ScatterPlotOperatorParts(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready)
  }

  # Rchart Range method
  if (options[["rChart"]] && is.null(jaspResults[["rChart"]])) {
    jaspResults[["rChart"]] <- createJaspContainer(gettext("Range method range chart"))
    jaspResults[["rChart"]]$position <- 3
    jaspResults[["rChart"]]$dependOn(c("rChart", "measurements", "measurementsLong", "parts"))
    jaspResults[["rChart"]][["plot"]] <- createJaspPlot(title = gettext("Range chart by part"), width = 800, height = 400)
    if (ready) {
      rChart <- .controlChart(dataset = dataset[measurements], plotType = "R", xAxisLabels = dataset[[parts]])
      jaspResults[["rChart"]][["plot"]]$plotObject <- rChart$plotObject
      jaspResults[["rChart"]][["table"]] <- rChart$table
    }
  }

  # Scatter Plot Operators
  if (options[["scatterPlotMeasurement"]]) {
    if (is.null(jaspResults[["ScatterOperators"]])) {
      jaspResults[["ScatterOperators"]] <- createJaspContainer(gettext("Scatterplot operators"))
      jaspResults[["ScatterOperators"]]$position <- 2
    }
    jaspResults[["ScatterOperators"]] <- .ScatterPlotOperators(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready)
  }

  # Traffic light graph
  if(options[["trafficLightChart"]] && is.null(jaspResults[["trafficPlot"]] )) {
    jaspResults[["trafficPlot"]] <- createJaspContainer(gettext("Traffic light chart"))
    jaspResults[["trafficPlot"]]$position <- 4
    jaspResults[["trafficPlot"]]$dependOn(c("trafficLightChart", "manualProcessSdValue", "manualProcessSd", "toleranceValue", "tolerance"))
    TrafficContainer <- jaspResults[["trafficPlot"]]

    valuesVec <- .rAndRtableRange(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, jaspResults, ready = ready, GRRpercent = TRUE)
    TrafficContainer[["plot"]] <- .trafficplot(StudyVar = valuesVec[1], ToleranceUsed = options[["tolerance"]],
                                               ToleranceVar = valuesVec[2],options = options, ready = ready,
                                               Xlab.StudySD = "Percent study variation of GRR", Xlab.Tol = "Percent tolerance of GRR")

  }
  return()
}


.ScatterPlotOperatorParts <- function(dataset, measurements, parts, operators, options, ready) {


  plot <- createJaspPlot(title = gettext("Run chart of parts"), width = 500, height = 320)
  plot$dependOn(c("runChartPart"))

  if (ready) {
    partIndex <- 1:length(dataset[[measurements[1]]])
    dataset <- cbind(dataset, Parts = factor(partIndex, partIndex))

    allMeasurements <- as.vector(unlist(dataset[measurements]))
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(allMeasurements)
    yLimits <- range(yBreaks)

    p <- ggplot2::ggplot() +
      jaspGraphs::geom_point(data = dataset, ggplot2::aes_string(x = "Parts", y = measurements[1], group = 1), fill = "red",  size = 4) +
      jaspGraphs::geom_point(data = dataset, ggplot2::aes_string(x = "Parts", y = measurements[2], group = 2),fill = "green", shape = 22, size = 4) +
      ggplot2::scale_y_continuous(name = "Measurements", limits = yLimits, breaks = yBreaks) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(legend.position = "right")

    plot$plotObject <- p
  }

  return(plot)
}

.rAndRtableRange <- function(dataset, measurements, parts, operators, options, jaspResults, ready, GRRpercent = FALSE, ProcessSD = "", tolerance = "", EnableSD = FALSE, EnableTol = FALSE) {

  if (!ready)
    return()

  n <- length(dataset[[measurements[1]]])
  Rbar <- sum(abs(dataset[measurements[1]] - dataset[measurements[2]]) / n)
  d2 <- .d2Value(n)
  GRR <- Rbar/d2
  GRRpercent.PSD <- GRR/options[["manualProcessSdValue"]] * 100
  GRRpercent.Tol <- GRR/(options[["toleranceValue"]] / 6)

  if (GRRpercent)
    return(c(GRRpercent.PSD, GRRpercent.Tol))
  else {
    table <- createJaspTable(title = gettext("Short gauge study"))
    table$position <- 1
    table$dependOn(c("repeatabilityAndReproducibilityTable", "toleranceValue", "manualProcessSdValue", "tolerance", "manualProcessSd"))

    table$addColumnInfo(name = "n", title = gettext("Sample size (n)"), type = "integer")
    table$addColumnInfo(name = "Rbar", title = gettext("R-bar"), type = "number")
    table$addColumnInfo(name = "d2", title = gettext("d2"), type = "number")
    table$addColumnInfo(name = "PSD", title = gettext("Process std. dev."), type = "number")
    table$addColumnInfo(name = "tolerance", title = gettext("Tolerance"), type = "number")
    table$addColumnInfo(name = "GRR", title = gettext("GRR"), type = "number")
    table$addColumnInfo(name = "GRRpercent.PSD", title = gettextf("%%GRR of process std. dev."), type = "number")
    table$addColumnInfo(name = "GRRpercent.Tol", title = gettextf("%%GRR of tolerance"), type = "number")

    rows <- list()
    rows[["n"]] = n
    rows[["Rbar"]] = Rbar
    rows[["d2"]] = d2
    rows[["GRR"]] = GRR

    if (EnableSD) {
      rows[["PSD"]] = options[["manualProcessSdValue"]]
      rows[["GRRpercent.PSD"]] = GRRpercent.PSD
    }

    if (EnableTol){
      rows[["tolerance"]] = options[["toleranceValue"]]
      rows[["GRRpercent.Tol"]] = GRRpercent.Tol
    }

    table$addRows(rows)
    table$showSpecifiedColumnsOnly <- TRUE
    jaspResults[["rAndR2"]] <- table
  }
}

.ScatterPlotOperators <- function(dataset, measurements, parts, operators, options, ready) {

  plot <- createJaspPlot(title = gettext("Scatterplot of 1st measurement vs 2nd measurement"))
  plot$dependOn(c("scatterPlotMeasurement", "scatterPlotMeasurementFitLine", "rangeScatterPlotOriginLine", "gaugeRRmethod"))

  if (ready) {

    p <- ggplot2::ggplot(data = dataset, ggplot2::aes_string(x = measurements[1], y = measurements[2])) +
      jaspGraphs::geom_point() + ggplot2::scale_x_continuous(limits = c(min(dataset[measurements])*0.9,max(dataset[measurements])*1.1)) +
      ggplot2::scale_y_continuous(limits = c(min(dataset[measurements])*0.9,max(dataset[measurements])*1.1)) +
      ggplot2::geom_abline(col = "gray", linetype = "dashed") +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()

    if (options[["scatterPlotMeasurementFitLine"]])
      p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE)

    if (options[["scatterPlotMeasurementAllValues"]])
      p <- p + ggplot2::geom_jitter(size = 2)

    plot$plotObject <- p

  }

  return(plot)
}

.d2Value <- function(n) {
  d2table <- data.frame(n = 1:20, d2 = c(1.41421, 1.27931, 1.23105, 1.20621, 1.19105, 1.18083, 1.17348, 1.16794, 1.16361, 1.16014,
                                         1.15729, 1.15490, 1.15289, 1.15115, 1.14965, 1.14833, 1.14717, 1.14613, 1.14520, 1.14437))
  if(n <= 20){
    return(d2table$d2[d2table$n == n])
  }else{
    return(1.12838)
  }
}
