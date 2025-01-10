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
variablesChartsIndividuals <- function(jaspResults, dataset, options) {
  # reading variables in from the GUI
  variables <- unlist(options[["measurement"]])
  stages <- unlist(options[["stage"]])
  axisLabelVariable <- unlist(options[["axisLabels"]])

  numeric_variables  <- variables
  numeric_variables  <- numeric_variables[numeric_variables != ""]
  factorVariables <- c(stages, axisLabelVariable)
  factorVariables  <- factorVariables[factorVariables != ""]

  ready <- length(numeric_variables) == 1

  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.numeric = numeric_variables, columns.as.factor = factorVariables)
  }

  if (axisLabelVariable != ""){
    axisLabels <- dataset[[axisLabelVariable]]
    xAxisTitle <- axisLabelVariable
    if (stages != "") {
      stageDataForOrdering <- factor(dataset[[stages]], levels = unique(dataset[[stages]]))
      axisLabels <- axisLabels[order(stageDataForOrdering)]
    }
  } else {
    axisLabels <- ""
    xAxisTitle <- gettext("Sample")
  }

  # Checking for errors in the dataset
  .hasErrors(dataset, type = c('infinity', "observations"),
             infinity.target = c(options$measurement, options$axisLabels),
             observations.amount = c("< 2"),
             observations.target = c(options[["measurement"]]),
             exitAnalysisIfErrors = TRUE)

  if (!identical(stages, "") && anyNA(dataset[[stages]])) {
    nDroppedRows <- sum(is.na(dataset[[stages]]))
    dataset <- dataset[!is.na(dataset[[stages]]),]
    droppedStagesNote <- gettextf("<i>Note.</i> Removed %i observation(s) that were not assigned to any Stage.", nDroppedRows)
  } else if (!identical(stages, "") && !anyNA(dataset[[stages]])) {
    nDroppedRows <- 0
  }

  # default plot
  if (!ready) {
    plot <- createJaspPlot(title = gettext("Variables Charts for Individuals"), width = 700, height = 400)
    jaspResults[["plot"]] <- plot
    plot$dependOn(c("xmrChart", "autocorrelationPlot", "report", "measurement"))
    return()
  }

  # Create the rule list for the out-of-control signals
  if (ready) {
    ruleList1 <- .getRuleListIndividualCharts(options, type = "I")
    ruleList2 <- .getRuleListIndividualCharts(options, type = "MR")
  }
  # ImR chart
  if (options$xmrChart && is.null(jaspResults[["Ichart"]])) {
    jaspResults[["Ichart"]] <- createJaspContainer(position = 1)
    jaspResults[["Ichart"]]$dependOn(c("xmrChart", "xmrChartMovingRangeLength", "axisLabels", "reportTitle",
                                       "reportMeasurementName", "reportMiscellaneous","reportReportedByBy","reportDate", "report",
                                       "stage", "controlLimitsNumberOfSigmas", .getDependenciesControlChartRules()))
    jaspResults[["Ichart"]][["plot"]] <- createJaspPlot(title =  gettext("X-mR control chart"), width = 1200, height = 500)
    if (ready) {
      # Error conditions for stages
      if(!identical(stages, "") && any(table(dataset[[stages]]) < options[["xmrChartMovingRangeLength"]])) {
        jaspResults[["Ichart"]][["plot"]]$setError(gettext("Moving range length is larger than the number of observations
                                                           in one of the stages."))
        return()
      }
      columnsToPass <- c(variables, stages)
      columnsToPass <- columnsToPass[columnsToPass != ""]
      individualChart <- .controlChart(dataset = dataset[columnsToPass], plotType = "I", stages = stages, nSigmasControlLimits = options[["controlLimitsNumberOfSigmas"]],
                                       xAxisLabels = axisLabels, tableLabels = axisLabels, xAxisTitle = xAxisTitle, ruleList = ruleList1,
                                       movingRangeLength = options[["xmrChartMovingRangeLength"]])
      mrChart <- .controlChart(dataset = dataset[columnsToPass], plotType = "MR", stages = stages, nSigmasControlLimits = options[["controlLimitsNumberOfSigmas"]],
                               xAxisLabels = axisLabels, tableLabels = axisLabels, xAxisTitle = xAxisTitle, ruleList = ruleList2,
                               movingRangeLength = options[["xmrChartMovingRangeLength"]])
    }
    jaspResults[["Ichart"]][["plot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(mrChart$plotObject, individualChart$plotObject), layout = matrix(2:1, 2), removeXYlabels= "x")
    if (!identical(stages, "") && nDroppedRows > 0)
      jaspResults[["Ichart"]][["plotNote"]] <- createJaspHtml(droppedStagesNote)
    jaspResults[["Ichart"]][["tableI"]] <- individualChart$table
    jaspResults[["Ichart"]][["tableMR"]] <- mrChart$table
  }

  # Autocorrelation Plot
  if(options[["autocorrelationPlot"]] && ready){
    jaspResults[["autocorrelationPlot"]] <- createJaspContainer(position = 2, title = "Autocorrelation function")
    jaspResults[["autocorrelationPlot"]]$dependOn(c("autocorrelationPlot", "measurement", "autocorrelationPlotLagsNumber"))
    Corplot <- jaspResults[["autocorrelationPlot"]]
    Corplot[[variables]] <- .autocorrelationPlot(dataset = dataset, options = options, variable = variables,
                                       CI = options[["autocorrelationPlotCiLevel"]], lags = options[["autocorrelationPlotLagsNumber"]])
  }

  # Report
  if (options[["report"]] && is.null(jaspResults[["report"]])) {
    jaspResults[["autocorrelationPlot"]] <- NULL
    jaspResults[["Ichart"]] <- NULL
    nElements <- sum(options[["reportIMRChart"]] * 2, options[["reportAutocorrelationChart"]], options[["reportMetaData"]])
    plotHeight <- if ((options[["reportIMRChart"]] && nElements == 2) || nElements == 0) 1000 else ceiling(nElements/2) * 500
    reportPlot <- createJaspPlot(title = gettext("Variables Chart for Individuals Report"), width = 1250, height = plotHeight)
    jaspResults[["report"]] <- reportPlot
    jaspResults[["report"]]$dependOn(c("xmrChart", "xmrChartMovingRangeLength", "axisLabels",
                                      "stage", "controlLimitsNumberOfSigmas", "autocorrelationPlotLagsNumber",
                                      "reportMetaData", "reportTitle", "reportTitleText", "reportIMRChart", "reportAutocorrelationChart",
                                      "reportChartName", "reportChartNameText", "reportSubtitle", "reportSubtitleText",
                                      "reportMeasurementName", "reportMeasurementNameText", "reportFootnote",
                                      "reportFootnoteText", "reportLocation", "reportLocationText", "reportDate",
                                      "reportDateText", "reportPerformedBy", "reportPerformedByText", "reportPrintDate",
                                      "reportPrintDateText", .getDependenciesControlChartRules()))

    if (nElements == 0) {
      reportPlot$setError(gettext("No report components selected."))
      return()
    }

    if(!ready)
      return()

    # Plot meta data
    if (options[["reportTitle"]] ) {
      title <- if (options[["reportTitleText"]] == "") gettext("Variables Chart for Individuals Report") else options[["reportTitleText"]]
    } else {
      title <- ""
    }

    if (options[["reportMetaData"]]) {
      text <- c()
      text <- if (options[["reportChartName"]]) c(text, gettextf("Chart name: %s", options[["reportChartNameText"]])) else text
      text <- if (options[["reportSubtitle"]]) c(text, gettextf("Sub-title: %s", options[["reportSubtitleText"]])) else text
      text <- if (options[["reportMeasurementName"]]) c(text, gettextf("Measurement name: %s", options[["reportMeasurementNameText"]])) else text
      text <- if (options[["reportFootnote"]]) c(text, gettextf("Footnote: %s", options[["reportFootnoteText"]])) else text
      text <- if (options[["reportLocation"]]) c(text, gettextf("Location: %s", options[["reportLocationText"]])) else text
      text <- if (options[["reportDate"]]) c(text, gettextf("Date: %s", options[["reportDateText"]])) else text
      text <- if (options[["reportPerformedBy"]]) c(text, gettextf("Performed by: %s", options[["reportPerformedByText"]])) else text
      text <- if (options[["reportPrintDate"]]) c(text, gettextf("Print date: %s", options[["reportPrintDateText"]])) else text
    } else {
      text <- NULL
    }

    plots <- list()
    plotIndexCounter <- 1
    if (options[["reportIMRChart"]]) {
      plots[[plotIndexCounter]] <- list(individualChart$plotObject, mrChart$plotObject)
      plotIndexCounter <- plotIndexCounter + 1
    }
    if (options[["reportAutocorrelationChart"]]) {
      if (anyNA(dataset[[variables]])) {
        reportPlot$setError(gettextf("Autocorrelation plot requires uninterrupted series of values. Missing values detected in %s.", variables))
        return()
      } else {
        plots[[plotIndexCounter]] <- .autocorrelationPlotObject(dataset = dataset, options = options, variable = variables, CI = options$autocorrelationPlotCiLevel, lags = options$autocorrelationPlotLagsNumber)
      }
    }
    reportPlotObject <- .qcReport(text = text, plots = plots, textMaxRows = 8,
                                  reportTitle = title)
    reportPlot$plotObject <- reportPlotObject
  }
}

.autocorrelationPlot <- function(dataset = dataset, options = options, variable, lags = NULL, CI = 0.95) {
  ppPlot <- createJaspPlot(width = 1200, height = 500, title = gettext("Autocorrelation plot"))
  ppPlot$dependOn(optionContainsValue = list(variables = variable))

  if (anyNA(dataset[[variable]])) {
    ppPlot$setError(gettextf("Autocorrelation plot requires uninterrupted series of values. Missing values detected in %s.", variable))
  } else {
    p <- .autocorrelationPlotObject(dataset, options, variable, lags, CI)
    ppPlot$plotObject <- p
  }
  return(ppPlot)
}

.autocorrelationPlotObject <- function(dataset = dataset, options = options, variable = var, lags = NULL, CI = 0.95) {
  list.acf <- stats::acf(dataset[[variable]], lag.max = lags, type = "correlation", ci.type = "ma", plot = FALSE, ci = CI)
  N <- as.numeric(list.acf$n.used)
  df1 <- data.frame(lag = list.acf$lag, acf = list.acf$acf)
  df1$lag.acf <- dplyr::lag(df1$acf, default = 0)
  df1$lag.acf[2] <- 0
  df1$lag.acf.cumsum <- cumsum((df1$lag.acf)^2)
  df1$acfstd <- sqrt(1/N * (1 + 2 * df1$lag.acf.cumsum))
  df1$acfstd[1] <- 0
  df1 <- dplyr::select(df1, lag, acf, acfstd)

  p <- ggplot2::ggplot(data = df1, ggplot2::aes(x = lag, y = acf)) +
    ggplot2::geom_col(fill = "#4373B6", width = 0.2) +
    jaspGraphs::geom_line(ggplot2::aes(x = lag, y = qnorm((1+CI)/2)*acfstd), color = "red") +
    jaspGraphs::geom_line(ggplot2::aes(x = lag, y = -qnorm((1+CI)/2)*acfstd), color = "red") +
    ggplot2::geom_hline(yintercept = 0, color = 'green') +
    ggplot2::scale_y_continuous(name = gettext("Autocorrelation"), limits = c(-1,1), breaks = seq(-1,1,0.2)) +
    ggplot2::scale_x_continuous(name = gettext('Lag'), breaks = seq(1,max(df1$lag),2)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(p)
}
