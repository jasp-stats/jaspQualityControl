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
variablesChartsSubgroups <- function(jaspResults, dataset, options) {
  wideFormat <- (options[["dataFormat"]] == "wideFormat")

  # In wide format we have one subgroup per row, else we need a either a grouping variable or later specify subgroup size manually
  if (wideFormat) {
    measurements <- unlist(options[["measurementsWideFormat"]])
    axisLabels <- options[["axisLabels"]]
    factorVariables <- axisLabels
  } else {
    measurements <- options[["measurementLongFormat"]]
    subgroupVariable <- options[["subgroup"]]
    factorVariables <- subgroupVariable
  }

  stages <- options[["stages"]]
  factorVariables <- c(factorVariables, stages)
  measurements <- measurements[measurements != ""]
  factorVariables <- factorVariables[factorVariables != ""]

  # Check if analysis is ready
  if (wideFormat) {
    ready <- length(measurements) > 1
  } else if (!wideFormat && options[["subgroupSizeType"]] == "manual"){
    ready <- length(measurements) == 1
  } else if (!wideFormat && options[["subgroupSizeType"]] == "groupingVariable") {
    ready <- length(measurements) == 1 && subgroupVariable != ""
  }

  # Return an empty plot as default
  if (!ready) {
    plot <- createJaspPlot(title = gettext("Control charts"), width = 700, height = 400)
    jaspResults[["plot"]] <- plot
    plot$dependOn(c("report", "chartType", "measurementLongFormat", "variables", "stages", "subgroup", "subgroupSizeType"))
    return()
  }

  # Data reading
  if (is.null(dataset) && ready) {
    if (length(factorVariables) >= 1) {
      dataset <- .readDataSetToEnd(columns.as.numeric = measurements, columns.as.factor = factorVariables)
    } else {
      dataset <- .readDataSetToEnd(columns.as.numeric = measurements)
    }
  }
  if (!wideFormat && !identical(subgroupVariable, "")) # empty strings should also be treated as NA
    dataset[,subgroupVariable][dataset[[subgroupVariable]] == ""] <- NA

  # error handling
  .hasErrors(dataset, type = c('infinity'),
             infinity.target = c(measurements, options$subgroup),
             exitAnalysisIfErrors = TRUE)

  plotNotes <- ""
  if (!identical(stages, "")) {
    if ((!wideFormat && options[["subgroupSizeType"]] == "manual" &&
         any(lapply(split(dataset[[stages]], ceiling(seq_along(dataset[[stages]])/options[["manualSubgroupSizeValue"]])), FUN = function(x)length(unique(x))) > 1)) ||
        (!wideFormat && options[["subgroupSizeType"]] == "groupingVariable" &&
         any(table(dplyr::count_(dataset, vars = c(stages, subgroupVariable))[subgroupVariable]) > 1))) {
      plotNotes <- paste0(plotNotes, gettext("One or more subgroups are assigned to more than one stage, only first stage is considered.<br>"))
    }
    if (anyNA(dataset[[stages]])) {
      nDroppedStageRows <- sum(is.na(dataset[[stages]]))
      dataset <- dataset[!is.na(dataset[[stages]]),]
      removalType <- if (wideFormat) "subgroup(s)" else "observation(s)"
      plotNotes <- paste0(plotNotes, gettextf("Removed %1$i %2$s that were not assigned to any stage.<br>", nDroppedStageRows, removalType))
    }
  }

  if (!wideFormat && options[["subgroupSizeType"]] == "groupingVariable" && anyNA(dataset[[subgroupVariable]])) {
    nDroppedSubgroupRows <- sum(is.na(dataset[[subgroupVariable]]))
    dataset <- dataset[!is.na(dataset[[subgroupVariable]]),]
    plotNotes <- paste0(plotNotes, gettextf("Removed %i observation(s) that were not assigned to any subgroups.<br>", nDroppedSubgroupRows))
  }

  # Rearrange data if not already wide format (one group per row)
  if (!wideFormat && ready) {
    reshapeOutputList <- .reshapeSubgroupDataLongToWide(dataset, measurements, stages = stages, subgroupVariable = subgroupVariable,
                                                        subgroupSizeType = options[["subgroupSizeType"]],
                                                        manualSubgroupSizeValue = options[["manualSubgroupSizeValue"]])
    dataset <- reshapeOutputList$dataset
    measurements <- reshapeOutputList$measurements
    axisLabels <- reshapeOutputList$axisLabels
    xAxisTitle <- reshapeOutputList$xAxisTitle
  }  else if (wideFormat && ready) {
    if (axisLabels != "") {
      xAxisTitle <- options[["axisLabels"]]
      axisLabels <- dataset[[axisLabels]]
    } else {
      xAxisTitle <- gettext("Sample")
    }
  }

  # Check if all subgroups are of size 1 and return error if yes
  if (all(apply(dataset[measurements], 1, function(x) length(x) <= 1))) {
    plot <- createJaspPlot(title = gettext("Control charts"), width = 700, height = 400)
    plot$setError(gettext("All subgroups are of size 1. Variables charts for subgroups cannot be calculated. Use variables charts for individuals."))
    jaspResults[["plot"]] <- plot
    plot$dependOn(c("report", "chartType", "measurementLongFormat", "variables", "stages", "subgroup", "subgroupSizeType"))
    return()
  }

  # Plot note about R/S chart recommendation
  if (length(measurements) > 5 && options[["chartType"]] == "xBarAndR") # if the subgroup size is above 5, R chart is not recommended
    plotNotes <- paste0(plotNotes, gettext("Subgroup size is >5, results may be biased. S-chart is recommended."))


  #X bar & R/s chart
  if (ready) {
    if (is.null(jaspResults[["controlCharts"]])) {
      jaspResults[["controlCharts"]] <- createJaspContainer(position = 1)
      jaspResults[["controlCharts"]]$dependOn(c("chartType", "variables", "warningLimits", "knownParameters", "knownParametersMean", "manualTicks", 'nTicks',
                                                "knownParametersSd", "manualSubgroupSizeValue", "dataFormat", "subgroup", "measurementLongFormat",
                                                "report", "reportTitle", "reportMeasurementName", "reportMiscellaneous","reportReportedBy","reportDate", "reportSubtitle",
                                                "reportChartName", "subgroupSizeUnequal", "axisLabels", "stages", "subgroupSizeType",
                                                "fixedSubgroupSizeValue", "xBarAndSUnbiasingConstant"))
      secondPlotType <- ifelse(options[["chartType"]] == "xBarAndR", "R", "s")
      jaspResults[["controlCharts"]][["plot"]] <- createJaspPlot(title =  gettextf("X-bar & %1$s control chart", secondPlotType), width = 1200, height = 500)
      if (length(measurements) > 50 && secondPlotType == "R") { # if the subgroup size is above 50, the R package cannot calculate R charts.
        jaspResults[["controlCharts"]][["plot"]]$setError(gettextf("Subgroup size is >50, R chart calculation is not possible. Use S-chart instead."))
        return()
      }

      columnsToPass <- c(measurements, stages)
      columnsToPass <- columnsToPass[columnsToPass != ""]
      xBarSdType <- tolower(secondPlotType)
      clLabelSize <- if (options[["report"]]) 3.5 else 4.5
      fixedSubgroupSize <- if (options[["subgroupSizeUnequal"]] == "fixedSubgroupSize") options[["fixedSubgroupSizeValue"]] else ""

      # first chart is always xBar-chart, second is either R- or s-chart
      xBarChart <- .controlChart(dataset = dataset[columnsToPass], plotType = "xBar", stages = stages, xBarSdType = xBarSdType,
                                 phase2 = options[["knownParameters"]], phase2Mu = options[["knownParametersMean"]], phase2Sd = options[["knownParametersSd"]],
                                 fixedSubgroupSize = fixedSubgroupSize, warningLimits = options[["warningLimits"]],
                                 xAxisLabels = axisLabels, xAxisTitle = xAxisTitle, clLabelSize = clLabelSize,
                                 unbiasingConstantUsed = options[["xBarAndSUnbiasingConstant"]])
      secondChart <- .controlChart(dataset = dataset[columnsToPass], plotType = secondPlotType, stages = stages, phase2 = options[["knownParameters"]],
                                   phase2Sd = options[["knownParametersSd"]], fixedSubgroupSize = fixedSubgroupSize,
                                   xAxisLabels = axisLabels, xAxisTitle = xAxisTitle, clLabelSize = clLabelSize,
                                   unbiasingConstantUsed = options[["xBarAndSUnbiasingConstant"]])
      jaspResults[["controlCharts"]][["plot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(secondChart$plotObject, xBarChart$plotObject), layout = matrix(2:1, 2), removeXYlabels= "x")
      if (!identical(plotNotes, ""))
        jaspResults[["controlCharts"]][["plotNote"]] <- createJaspHtml(paste0("<i>Note.</i> ", plotNotes))

      # Nelson tests tables
        jaspResults[["controlCharts"]][["xBarTable"]]  <- xBarChart$table
        jaspResults[["controlCharts"]][["secondTable"]] <- secondChart$table

      # Report
      if (options[["report"]] && is.null(jaspResults[["report"]])) {

        jaspResults[["controlCharts"]] <- NULL
        jaspResults[["NelsonTables"]] <- NULL

        jaspResults[["report"]] <- createJaspContainer(gettext("Report"))
        jaspResults[["report"]]$dependOn(c("report", "manualSubgroupSizeValue","chartType", "variables", "measurementLongFormat",
                                           "manualTicks", 'nTicks',"dataFormat", "subgroup", "reportTitle", "reportMeasurementName",
                                           "reportMiscellaneous","reportReportedBy","reportDate", "reportSubtitle", "reportChartName",
                                           "stages", "xBarAndSUnbiasingConstant"))
        jaspResults[["report"]]$position <- 9
        Iplot <- jaspResults[["report"]]
        Iplot[["report"]] <- .CCReport(p1 = xBarChart$plotObject, p2 = secondChart$plotObject, reportTitle = options$reportTitle,
                                         reportMeasurementName = options$reportMeasurementName, reportDate = options$reportDate, reportReportedBy = options$reportReportedBy,
                                         reportMiscellaneous = options$reportMiscellaneous, reportSubtitle = options$reportSubtitle, reportChartName = options$reportChartName,
                                         options = options)
      }
    }
  }
}

.CCReport <- function(p1 = "", p2 = "", reportTitle = "", reportMeasurementName = "", reportDate = "",
                      reportReportedBy = "", reportMiscellaneous = "" , reportSubtitle = "", reportChartName = "", options) {

  if (identical(reportTitle, "")) {
    title <- if (options[["chartType"]] == "xBarAndR") gettext("Report for X-bar & R control chart") else gettext("Report for X-bar & s control chart")
  } else {
    title <- reportTitle
  }
  name <- gettextf("Name: %s", reportMeasurementName)
  date <- gettextf("Date: %s", reportDate)
  subtitle <- gettextf("Sub-title: %s", reportSubtitle)
  text1 <- c(name, date, subtitle)

  reportedBy <- gettextf("Reported by: %s", reportReportedBy)
  misc <- gettextf("Misc: %s", reportMiscellaneous)
  chartName <- gettextf("Name of chart: %s", reportChartName)
  text2 <- c(reportedBy, misc, chartName)

  matrixPlot <- createJaspPlot(width = 1200, aspectRatio = 1)
  plotMat <- matrix(list(), 3, 2)
  plotMat[[1, 1]] <- .ggplotWithText(text1)
  plotMat[[1, 2]] <- p1
  plotMat[[2, 1]] <- .ggplotWithText(text2)
  plotMat[[2, 2]] <- p2

  p <- jaspGraphs::ggMatrixPlot(plotMat, topLabels = c(gettext(title), ""))
  matrixPlot$plotObject <- p

  return(matrixPlot)
}
