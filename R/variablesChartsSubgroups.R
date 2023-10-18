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

  #dataset <- read.csv("C:/Users/Jonee/Google Drive/SKF Six Sigma/JASP Data Library/2_1_VariablesChartsForSubgroups/SubgroupChartLongFormatStagesMultiDefined.csv")
  #dataset <- read.csv("C:/Users/Jonee/Google Drive/SKF Six Sigma/Datasets/ControlChartError.csv")

  wideFormat <- (options[["CCDataFormat"]] == "CCwideFormat")

  # In wide format we have one subgroup per row, else we need a either a grouping variable or later specify subgroup size manually
  if (wideFormat) {
    measurements <- unlist(options[["variables"]])
    axisLabels <- options[["axisLabels"]]
    factorVariables <- axisLabels
  } else {
    measurements <- options[["variablesLong"]]
    subgroupVariable <- options[["subgroups"]]
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
    plot$dependOn(c("CCReport", "TypeChart", "variablesLong", "variables", "stages", "subgroups", "subgroupSizeType"))
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

  # error handling
  .hasErrors(dataset, type = c('infinity'),
             infinity.target = c(measurements, options$subgroups),
             exitAnalysisIfErrors = TRUE)

  plotNotes <- ""
  if (!identical(stages, "")) {
    if ((!wideFormat && options[["subgroupSizeType"]] == "manual" &&
         any(lapply(split(dataset[[stages]], ceiling(seq_along(dataset[[stages]])/options[["CCSubgroupSize"]])), FUN = function(x)length(unique(x))) > 1)) ||
        (!wideFormat && options[["subgroupSizeType"]] == "groupingVariable" &&
         any(table(dplyr::count_(dataset, vars = c(stages, subgroupVariable))[subgroupVariable]) > 1))) {
      plotNotes <- paste0(plotNotes, gettext("One or more subgroups are assigned to more than one stage, only first stage is considered.<br>"))
    }
    if (anyNA(dataset[[stages]])) {
      nDroppedStageRows <- sum(is.na(dataset[[stages]]))
      dataset <- dataset[!is.na(dataset[[stages]]),]
      removalType <- if (wideFormat) "subgroup(s)" else "observation(s)"
      plotNotes <- paste0(plotNotes, gettextf("Removed %i %s that were not assigned to any stage.<br>", nDroppedStageRows, removalType))
    }
  }

  if (!wideFormat && options[["subgroupSizeType"]] == "groupingVariable" && anyNA(dataset[[subgroupVariable]])) {
    nDroppedSubgroupRows <- sum(is.na(dataset[[subgroupVariable]]))
    dataset <- dataset[!is.na(dataset[[subgroupVariable]]),]
    removalType <- if (wideFormat) "subgroup(s)" else "observation(s)"
    plotNotes <- paste0(plotNotes, gettextf("Removed %i observation(s) that were not assigned to any subgroups.<br>", nDroppedSubgroupRows))
  }

  # Rearrange data if not already wide format (one group per row)
  if (!wideFormat && ready) {
    # if subgroup size is set manual, use that. Else determine subgroup size from largest level in subgroups variable
    if (options[["subgroupSizeType"]] == "manual") {
      k <- options[["CCSubgroupSize"]]
      if (stages != "") {
        # Only take the first stage of each subgroup, to avoid multiple stages being defined
        stagesPerSubgroup <- dataset[[stages]][seq(1, length(dataset[[stages]]), k)]
      }
      # fill up with NA to allow all subgroup sizes
      if(length(dataset[[measurements]]) %% k != 0) {
        rest <- length(dataset[[measurements]]) %% k
        dataset_expanded <- c(dataset[[measurements]], rep(NA, k - rest))
        dataset <- as.data.frame(matrix(dataset_expanded, ncol = k, byrow = TRUE))
      } else {
        dataset <- as.data.frame(matrix(dataset[[measurements]], ncol = k, byrow = TRUE))
      }
      measurements <- colnames(dataset)
      axisLabels <- as.character(seq_len(nrow(dataset)))
      xAxisTitle <- gettext("Sample")
      if (stages != "") {
        dataset[[stages]] <- stagesPerSubgroup
        axisLabels <- axisLabels[order(dataset[[stages]])]
      }
    } else {
      subgroups <- dataset[[subgroupVariable]]
      subgroups <- na.omit(subgroups)
      # add sequence of occurence to allow pivot_wider
      if (stages != "") {
        # Only take the first defined stage of each subgroup, to avoid multiple stages being defined
        stagesPerSubgroup <- dataset[[stages]][match(unique(subgroups), subgroups)]
      }
      occurenceVector <- with(dataset, ave(seq_along(subgroups), subgroups, FUN = seq_along))
      dataset$occurence <- occurenceVector
      # transform into one group per row
      dataset <- tidyr::pivot_wider(data = dataset[c(measurements, subgroupVariable, "occurence")],
                                    values_from = tidyr::all_of(measurements), names_from = occurence)
      # arrange into dataframe
      dataset <- as.data.frame(dataset)
      measurements <- as.character(unique(occurenceVector))
      axisLabels <- dataset[[subgroupVariable]]
      xAxisTitle <- subgroupVariable
      if (stages != ""){
        dataset[[stages]] <- stagesPerSubgroup
        axisLabels <- axisLabels[order(dataset[[stages]])]
      }
    }
  }  else if (wideFormat && ready) {
    multipleStagesPerSubgroupDefined <- FALSE # not possible in this format
    if (axisLabels != "") {
      xAxisTitle <- options[["axisLabels"]]
      axisLabels <- dataset[[axisLabels]]
    } else {
      xAxisTitle <- gettext("Sample")
    }
  }

  # Plot note about R/S chart recommendation
  if (length(measurements) > 5 && options[["TypeChart"]] == "xBarRchart") # if the subgroup size is above 5, R chart is not recommended
    plotNotes <- paste0(plotNotes, gettext("Subgroup size is >5, results may be biased. S-chart is recommended."))


  #X bar & R/s chart
  if (ready) {
    if (is.null(jaspResults[["controlCharts"]])) {
      jaspResults[["controlCharts"]] <- createJaspContainer(position = 1)
      jaspResults[["controlCharts"]]$dependOn(c("TypeChart", "variables", "Wlimits", "Phase2", "mean", "manualTicks", 'nTicks',
                                                "SD", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong",
                                                "CCReport", "ccTitle", "ccName", "ccMisc","ccReportedBy","ccDate", "ccSubTitle",
                                                "ccChartName", "subgroupSizeUnequal", "axisLabels", "stages", "subgroupSizeType",
                                                "fixedSubgroupSizeValue"))
      secondPlotType <- ifelse(options[["TypeChart"]] == "xBarRchart", "R", "s")
      jaspResults[["controlCharts"]][["plot"]] <- createJaspPlot(title =  gettextf("X-bar & %1$s control chart", secondPlotType), width = 1200, height = 500)
      if (length(measurements) > 50 && secondPlotType == "R") { # if the subgroup size is above 50, the R package cannot calculate R charts.
        jaspResults[["controlCharts"]][["plot"]]$setError(gettextf("Subgroup size is >50, R chart calculation is not possible. Use S-chart instead."))
        return()
      }

      columnsToPass <- c(measurements, stages)
      columnsToPass <- columnsToPass[columnsToPass != ""]
      xBarSdType <- tolower(secondPlotType)
      clLabelSize <- if (options[["CCReport"]]) 3.5 else 4.5
      fixedSubgroupSize <- if (options[["subgroupSizeUnequal"]] == "fixedSubgroupSize") options[["fixedSubgroupSizeValue"]] else ""

      # first chart is always xBar-chart, second is either R- or s-chart
      xBarChart <- .controlChartPlotFunction(dataset = dataset[columnsToPass], plotType = "xBar", stages = stages, xBarSdType = xBarSdType,
                                             phase2 = options[["Phase2"]], phase2Mu = options[["mean"]], phase2Sd = options[["SD"]],
                                             fixedSubgroupSize = fixedSubgroupSize, warningLimits = options[["Wlimits"]],
                                             xAxisLabels = axisLabels, xAxisTitle = xAxisTitle, clLabelSize = clLabelSize)
      secondChart <- .controlChartPlotFunction(dataset = dataset[columnsToPass], plotType = secondPlotType, stages = stages, phase2 = options[["Phase2"]],
                                               phase2Sd = options[["SD"]], fixedSubgroupSize = fixedSubgroupSize,
                                               xAxisLabels = axisLabels, xAxisTitle = xAxisTitle, clLabelSize = clLabelSize)
      jaspResults[["controlCharts"]][["plot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(secondChart$plotObject, xBarChart$plotObject), layout = matrix(2:1, 2), removeXYlabels= "x")
      if (!identical(plotNotes, ""))
        jaspResults[["controlCharts"]][["plotNote"]] <- createJaspHtml(paste0("<i>Note.</i> ", plotNotes))

      # Nelson tests tables
        jaspResults[["controlCharts"]][["xBarTable"]]  <- xBarChart$table
        jaspResults[["controlCharts"]][["secondTable"]] <- secondChart$table

      # Report
      if (options[["CCReport"]] && is.null(jaspResults[["CCReport"]])) {

        jaspResults[["controlCharts"]] <- NULL
        jaspResults[["NelsonTables"]] <- NULL

        jaspResults[["CCReport"]] <- createJaspContainer(gettext("Report"))
        jaspResults[["CCReport"]]$dependOn(c("CCReport", "CCSubgroupSize","TypeChart", "variables", "variablesLong",
                                             "manualTicks", 'nTicks',"CCDataFormat", "subgroups", "ccTitle", "ccName",
                                             "ccMisc","ccReportedBy","ccDate", "ccSubTitle", "ccChartName", "stages"))
        jaspResults[["CCReport"]]$position <- 9
        Iplot <- jaspResults[["CCReport"]]
        Iplot[["ccReport"]] <- .CCReport(p1 = xBarChart$plotObject, p2 = secondChart$plotObject, ccTitle = options$ccTitle,
                                         ccName = options$ccName, ccDate = options$ccDate, ccReportedBy = options$ccReportedBy,
                                         ccMisc = options$ccMisc, ccSubTitle = options$ccSubTitle, ccChartName = options$ccChartName,
                                         options = options)
      }
    }
  }
}

.CCReport <- function(p1 = "", p2 = "", ccTitle = "", ccName = "", ccDate = "",
                      ccReportedBy = "", ccMisc = "" , ccSubTitle = "", ccChartName = "", options) {

  if (identical(ccTitle, "")) {
    title <- if (options[["TypeChart"]] == "xBarRchart") gettext("Report for X-bar & R control chart") else gettext("Report for X-bar & s control chart")
  } else {
    title <- ccTitle
  }
  name <- gettextf("Name: %s", ccName)
  date <- gettextf("Date: %s", ccDate)
  subtitle <- gettextf("Sub-title: %s", ccSubTitle)
  text1 <- c(name, date, subtitle)

  reportedBy <- gettextf("Reported by: %s", ccReportedBy)
  misc <- gettextf("Misc: %s", ccMisc)
  chartName <- gettextf("Name of chart: %s", ccChartName)
  text2 <- c(reportedBy, misc, chartName)

  matrixPlot <- createJaspPlot(width = 1200, aspectRatio = 1)
  plotMat <- matrix(list(), 3, 2)
  plotMat[[1, 1]] <- .ggplotWithText(text1)
  plotMat[[1, 2]] <- p1
  plotMat[[2, 1]] <- .ggplotWithText(text2)
  plotMat[[2, 2]] <- p2
  # plotMat[[3, 1]] <-
  # plotMat[[3, 2]] <- p2

  p <- jaspGraphs::ggMatrixPlot(plotMat, topLabels = c(gettext(title), ""))
  matrixPlot$plotObject <- p

  return(matrixPlot)
}
