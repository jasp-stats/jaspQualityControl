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
    stages <- options[["stagesWideFormat"]]
    axisLabels <- options[["axisLabels"]]
    factorVariables <- c(axisLabels, stages)
  } else {
    measurements <- options[["measurementLongFormat"]]
    stages <- options[["stagesLongFormat"]]
    subgroupVariable <- options[["subgroup"]]
    factorVariables <- c(subgroupVariable, stages)
  }

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
    plot$dependOn(c("report", "chartType", "measurementLongFormat", "variables", "stagesWideFormat", "stagesLongFormat",
                    "subgroup", "subgroupSizeType", "measurementsWideFormat", "dataFormat", "groupingVariableMethod"))
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
                                                        manualSubgroupSizeValue = options[["manualSubgroupSizeValue"]],
                                                        subgroupVariableMethod = options[["groupingVariableMethod"]])
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
    plot$dependOn(c("report", "chartType", "measurementLongFormat", "variables", "subgroup", "stagesWideFormat", "stagesLongFormat",
                    "subgroupSizeType", "groupingVariableMethod"))
    return()
  }

  # Plot note about R/S chart recommendation
  if (length(measurements) > 5 && options[["chartType"]] == "xBarAndR") # if the subgroup size is above 5, R chart is not recommended
    plotNotes <- paste0(plotNotes, gettext("Subgroup size is >5, results may be biased. An s-chart is recommended."))

  #X bar & R/s chart
  if (ready) {


    if (is.null(jaspResults[["controlCharts"]])) {
      jaspResults[["controlCharts"]] <- createJaspContainer(position = 1)
      jaspResults[["controlCharts"]]$dependOn(c("chartType", "variables", "warningLimits", "knownParameters", "knownParametersMean", "manualTicks", 'nTicks',
                                                "knownParametersSd", "manualSubgroupSizeValue", "dataFormat", "subgroup", "measurementLongFormat",
                                                "report", "reportTitle", "reportMeasurementName", "reportMiscellaneous","reportReportedBy","reportDate", "reportSubtitle",
                                                "reportChartName", "subgroupSizeUnequal", "axisLabels", "stagesWideFormat", "stagesLongFormat",
                                                "subgroupSizeType", "fixedSubgroupSizeValue", "xBarAndSUnbiasingConstant", "controlLimitsNumberOfSigmas",
                                                "groupingVariableMethod", .getDependenciesControlChartRules()))
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

      # Create the rule list for the out-of-control signals
      ruleList1 <- .getRuleListSubgroupCharts(options, type = "xBar")
      ruleList2 <- .getRuleListSubgroupCharts(options, type = secondPlotType)

      # first chart is always xBar-chart, second is either R- or s-chart
      xBarChart <- .controlChart(dataset = dataset[columnsToPass], ruleList = ruleList1, plotType = "xBar", stages = stages, xBarSdType = xBarSdType,
                                 nSigmasControlLimits = options[["controlLimitsNumberOfSigmas"]], phase2 = options[["knownParameters"]],
                                 phase2Mu = options[["knownParametersMean"]], phase2Sd = options[["knownParametersSd"]],
                                 fixedSubgroupSize = fixedSubgroupSize, warningLimits = options[["warningLimits"]],
                                 xAxisLabels = axisLabels, tableLabels = axisLabels, xAxisTitle = xAxisTitle, clLabelSize = clLabelSize,
                                 unbiasingConstantUsed = options[["xBarAndSUnbiasingConstant"]])
      secondChart <- .controlChart(dataset = dataset[columnsToPass], ruleList = ruleList2, plotType = secondPlotType,  stages = stages,
                                   phase2 = options[["knownParameters"]], nSigmasControlLimits = options[["controlLimitsNumberOfSigmas"]],
                                   phase2Sd = options[["knownParametersSd"]], fixedSubgroupSize = fixedSubgroupSize,
                                   xAxisLabels = axisLabels, tableLabels = axisLabels, xAxisTitle = xAxisTitle, clLabelSize = clLabelSize,
                                   unbiasingConstantUsed = options[["xBarAndSUnbiasingConstant"]])
      jaspResults[["controlCharts"]][["plot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(secondChart$plotObject, xBarChart$plotObject), layout = matrix(2:1, 2), removeXYlabels= "x")
      if (!identical(plotNotes, ""))
        jaspResults[["controlCharts"]][["plotNote"]] <- createJaspHtml(paste0("<i>Note.</i> ", plotNotes))

      # Nelson tests tables
        jaspResults[["controlCharts"]][["xBarTable"]]  <- xBarChart$table
        jaspResults[["controlCharts"]][["secondTable"]] <- secondChart$table

      # Report
      if (options[["report"]]) {
        jaspResults[["controlCharts"]] <- NULL
        jaspResults[["NelsonTables"]] <- NULL
        reportPlot <- createJaspPlot(title = gettext("Variables Chart for Subgroups Report"), width = 1250, height = 1000)
        jaspResults[["report"]] <- reportPlot
        jaspResults[["report"]]$dependOn(c("chartType", "variables", "warningLimits", "knownParameters", "knownParametersMean", "manualTicks", 'nTicks',
                                           "knownParametersSd", "manualSubgroupSizeValue", "dataFormat", "subgroup", "measurementLongFormat",
                                           "subgroupSizeUnequal", "axisLabels", "stagesWideFormat", "stagesLongFormat",
                                           "subgroupSizeType", "fixedSubgroupSizeValue", "xBarAndSUnbiasingConstant", "controlLimitsNumberOfSigmas",
                                           "groupingVariableMethod", "report", "reportMetaData", "reportTitle", "reportTitleText",
                                           "reportChartName", "reportChartNameText", "reportSubtitle", "reportSubtitleText",
                                           "reportMeasurementName", "reportMeasurementNameText", "reportFootnote",
                                           "reportFootnoteText", "reportLocation", "reportLocationText", "reportDate",
                                           "reportDateText", "reportPerformedBy", "reportPerformedByText", "reportPrintDate",
                                           "reportPrintDateText", .getDependenciesControlChartRules()))

        # Plot meta data
        if (options[["reportTitle"]] ) {
          title <- if (options[["reportTitleText"]] == "") gettext("Variables Chart for Subgroups Report") else options[["reportTitleText"]]
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

        plots <- list(list(xBarChart$plotObject, secondChart$plotObject))
        reportPlotObject <- .qcReport(text = text, plots = plots, textMaxRows = 8,
                                      reportTitle = title)
        reportPlot$plotObject <- reportPlotObject
      }
    }
  }
}
