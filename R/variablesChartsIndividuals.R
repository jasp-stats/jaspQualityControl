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
#
 # dataset <- read.csv("C:/Users/Jonee/Desktop/Temporary Files/IndividualChartStagesWithNA.csv")
 # dataset$Stage[2] <- NA
 # variables <- "Yield"
 # stages <- "Stage"
 # axisLabels <- "Month"

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
      axisLabels <- axisLabels[order(dataset[[stages]])]
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
  # ImR chart
  if (options$xmrChart && is.null(jaspResults[["Ichart"]])) {
    jaspResults[["Ichart"]] <- createJaspContainer(position = 1)
    jaspResults[["Ichart"]]$dependOn(c("xmrChart", "variables", "xmrChartMovingRangeLength", "axisLabels", "reportTitle",
                                       "reportMeasurementName", "reportMiscellaneous","reportReportedByBy","reportDate", "report",
                                       "stage"))
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
      individualChart <- .controlChart(dataset = dataset[columnsToPass], plotType = "I", stages = stages,
                                                   xAxisLabels = axisLabels, xAxisTitle = xAxisTitle)
      mrChart <- .controlChart(dataset = dataset[columnsToPass], plotType = "MR", stages = stages,
                                           xAxisLabels = axisLabels, xAxisTitle = xAxisTitle,
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


    jaspResults[["report"]] <- createJaspContainer(gettext("Report"))
    jaspResults[["report"]]$dependOn(c("report", "xmrChart", "variables","ncol", "axisLabels",
                                         "reportTitle", "reportMeasurementName", "reportMiscellaneous","reportReportedByBy","reportDate",
                                         "stage", "reportAutocorrelationChart", "reportIMRChart", "reportMetaData"))
    jaspResults[["report"]]$position <- 9
    Iplot <- jaspResults[["report"]]

    Iplot[["report"]] <- .individualChartReport(dataset, variables, axisLabels, xAxisTitle, stages, options)
  }

  # Error handling
  if (options[["report"]] && (!options[["xmrChart"]] || length(variables) < 1)){
    plot <- createJaspPlot(title = gettext("Report"), width = 700, height = 400)
    jaspResults[["plot"]] <- plot
    jaspResults[["plot"]]$setError(gettext("Please insert more measurements and check the X-mR chart."))
    plot$dependOn(c("report", "xmrChart", "measurement"))
    return()
  }
}

.autocorrelationPlot <- function(dataset = dataset, options = options, variable = var, lags = NULL, CI = 0.95) {
  ppPlot <- createJaspPlot(width = 1200, height = 500, title = gettextf("%s",variable))
  ppPlot$dependOn(optionContainsValue = list(variables = variable))

  if (anyNA(dataset[[variable]])) {
    plot <- createJaspPlot(title = title, width = 400, height = 400)
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

.individualChartReport <- function(dataset, variables, axisLabels, xAxisTitle, stages, options){

  if (options[["reportTitle"]] == "") {
    title <- gettextf("Individual charts report")
  } else {
    title <- options[["reportTitle"]]
  }
  name <- gettextf("Name: %s", options[["reportMeasurementName"]])
  date <- gettextf("Date of study: %s", options[["reportDate"]])
  text1 <- c(name, date)

  reportedBy <- gettextf("Performed by: %s", options[["reportReportedBy"]])
  misc <- gettextf("Misc: %s", options[["reportMiscellaneous"]])
  text2 <- c(reportedBy, misc)

  plotList <- list()
  indexCounter <- 0
  if (options[["reportMetaData"]]) {
    indexCounter <- indexCounter + 1
    plotList[[indexCounter]] <- .ggplotWithText(text1)
    indexCounter <- indexCounter + 1
    plotList[[indexCounter]] <- .ggplotWithText(text2)
  }
  if (options[["reportAutocorrelationChart"]]) {
    if (anyNA(dataset[[variables]])) {
      plot <- createJaspPlot(title = title, width = 400, height = 400)
      plot$setError(gettextf("Autocorrelation plot requires uninterrupted series of values. Missing values detected in %s.", variables))
      return(plot)
    } else {
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- .autocorrelationPlotObject(dataset = dataset, options = options, variable = variables, CI = options$autocorrelationPlotCiLevel, lags = options$autocorrelationPlotLagsNumber)
      # add an empty plot after the autocorrelation chart, so all report elements appear in blocks of 2 and don't get split up
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- ggplot2::ggplot() + ggplot2::theme_void()
    }
  }
  if (options[["reportIMRChart"]]) {
    indexCounter <- indexCounter + 1

    columnsToPass <- c(variables, stages)
    columnsToPass <- columnsToPass[columnsToPass != ""]
    plotList[[indexCounter]] <- .controlChart(dataset = dataset[columnsToPass], plotType = "I", stages = stages,
                                                          xAxisLabels = axisLabels, xAxisTitle = xAxisTitle,
                                                          clLabelSize = 3.5)$plotObject
    indexCounter <- indexCounter + 1
    plotList[[indexCounter]] <- .controlChart(dataset = dataset[columnsToPass], plotType = "MR", stages = stages,
                                                          xAxisLabels = axisLabels, xAxisTitle = xAxisTitle,
                                                          movingRangeLength = options[["xmrChartMovingRangeLength"]], clLabelSize = 3.5)$plotObject
  }

  if (indexCounter == 0) {
    plot <- createJaspPlot(title = title, width = 400, height = 400)
    plot$setError(gettext("No report components selected."))
    return(plot)
  }

  if (indexCounter == 2) {
    matrixNCols <- 1
    matrixNRows <- 2
  } else {
    matrixNCols <- 2
    matrixNRows <- indexCounter / matrixNCols
  }
  matrixPlot <- createJaspPlot(title = title, width = 1200, height = 400 * matrixNRows)
  plotMat <- matrix(plotList, matrixNRows, matrixNCols, byrow = FALSE)
  p <- jaspGraphs::ggMatrixPlot(plotMat)
  matrixPlot$plotObject <- p

  return(matrixPlot)
}
