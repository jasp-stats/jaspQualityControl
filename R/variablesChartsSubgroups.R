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
    plot <- createJaspPlot(title = gettext("Control Charts"), width = 700, height = 400)
    jaspResults[["plot"]] <- plot
    plot$dependOn(c("CCReport", "TypeChart", "variablesLong", "variables", "stages"))
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

  # Rearrange data if not already wide format (one group per row)
  if (!wideFormat && ready) {
    # if subgroup size is set manual, use that. Else determine subgroup size from largest level in subgroups variable
    if (options[["subgroupSizeType"]] == "manual") {
      k <- options[["CCSubgroupSize"]]
      if (stages != "") {
        # Only take the first stage of each subgroup, to avoid multiple stages being defined
        stagesPerSubgroup <- dataset[[stages]][seq(1, length(dataset[[stages]]), k)]
        # this line checks whether there are any subgroups with more than one stage assigned, so we can display a message later that only the first stage is used
        multipleStagesPerSubgroupDefined <- any(lapply(split(dataset[[stages]], ceiling(seq_along(dataset[[stages]])/k)), FUN = function(x)length(unique(x))) > 1)
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
      if (stages != "") {
        dataset[[stages]] <- stagesPerSubgroup
        axisLabels <- axisLabels[order(dataset[[stages]])]
      }
    } else {
      subgroups <- dataset[[subgroupVariable]]
      subgroups <- na.omit(subgroups)
      # add sequence of occurence to allow pivot_wider
      if (stages != "") {
        # this line checks whether there are any subgroups with more than one stage assigned, so we can display a message later that only the first stage is used
        multipleStagesPerSubgroupDefined <- any(table(dplyr::count_(dataset, vars = c(stages, subgroupVariable))[subgroupVariable]) > 1)
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
      if (stages != ""){
        dataset[[stages]] <- stagesPerSubgroup
        axisLabels <- axisLabels[order(dataset[[stages]])]
      }
    }
  }  else if (wideFormat && ready) {
    multipleStagesPerSubgroupDefined <- FALSE # not possible in this format
    if (axisLabels != "")
      axisLabels <- dataset[[axisLabels]]
  }

  #Checking for errors in the dataset
  .hasErrors(dataset, type = c('infinity', 'missingValues'),
             all.target = c(options[["measurementsWideFormat"]], options[["subgroup"]]),
             exitAnalysisIfErrors = TRUE)

  .hasErrors(dataset, type = c('infinity', 'missingValues'),
             infinity.target = c(measurements, options[["subgroup"]]),
             missingValues.target = c(options[["subgroup"]]),
             exitAnalysisIfErrors = TRUE)

  #X bar & R/s chart
  if (ready) {
    if (is.null(jaspResults[["controlCharts"]])) {
      secondPlotType <- ifelse(options[["TypeChart"]] == "xBarRchart", "R", "s")
      jaspResults[["controlCharts"]] <- createJaspPlot(title =  gettextf("X-bar & %1$s Control Chart", secondPlotType), width = 1200, height = 500)
      jaspResults[["controlCharts"]]$dependOn(c("TypeChart", "variables", "Wlimits", "Phase2", "mean", "manualTicks", 'nTicks',
                                                "SD", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong",
                                                "CCReport", "ccTitle", "ccName", "ccMisc","ccReportedBy","ccDate", "ccSubTitle",
                                                "ccChartName", "subgroupSizeUnequal", "axisLabels", "stages"))
      jaspResults[["controlCharts"]]$position <- 1

      if (length(measurements) > 50 && secondPlotType == "R") { # if the subgroup size is above 50, the R package cannot calculate R charts.
        jaspResults[["controlCharts"]]$setError(gettextf("Subgroup size is >50, R chart calculation is not possible. Use S-chart instead."))
        return()
      }

      columnsToPass <- c(measurements, stages)
      columnsToPass <- columnsToPass[columnsToPass != ""]
      xBarSdType <- tolower(secondPlotType)
      clLabelSize <- if (options[["CCReport"]]) 3.5 else 4.5

      # first chart is always xBar-chart, second is either R- or s-chart
      xBarChart <- .controlChartPlotFunction(dataset = dataset[columnsToPass], plotType = "xBar", stages = stages, xBarSdType = xBarSdType,
                                             phase2 = options[["Phase2"]], phase2Mu = options[["mean"]], phase2Sd = options[["SD"]],
                                             limitsPerSubgroup = (options[["subgroupSizeUnequal"]] == "actualSizes"),
                                             warningLimits = options[["Wlimits"]], xAxisLabels = axisLabels, clLabelSize = clLabelSize)
      secondChart <- .controlChartPlotFunction(dataset = dataset[columnsToPass], plotType = secondPlotType, , stages = stages, phase2 = options[["Phase2"]],
                                               phase2Sd = options[["SD"]], limitsPerSubgroup = (options[["subgroupSizeUnequal"]] == "actualSizes"),
                                               xAxisLabels = axisLabels, clLabelSize = clLabelSize)
      jaspResults[["controlCharts"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(secondChart$plotObject, xBarChart$plotObject), layout = matrix(2:1, 2), removeXYlabels= "x")


      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTables"]])) {
        jaspResults[["NelsonTables"]] <- createJaspContainer(title = gettext("Out-of-control Signals"))
        jaspResults[["NelsonTables"]]$dependOn(c("TypeChart", "variables", "Phase2", "mean", "SD", "CCSubgroupSize",
                                                 "CCDataFormat", "subgroups", "variablesLong", "stages"))
        jaspResults[["NelsonTables"]]$position <- 2
        allTables <- jaspResults[["NelsonTables"]]

        allTables[["xBarTable"]]  <- xBarChart$table
        allTables[["secondTable"]] <- secondChart$table

        if (stages != "" ) {
          if (multipleStagesPerSubgroupDefined)
            allTables[["xBarTable"]]$addFootnote(gettext("One or more subgroups are assigned to more than one stage. Only first stage is considered."))
        }
        if (length(measurements) > 5 && secondPlotType == "R") # if the subgroup size is above 5, R chart is not recommended
          allTables[["secondTable"]]$addFootnote(gettext("Subgroup size is >5, results may be biased. S-chart is recommended."))
      }
    }
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
                                       ccName = options$ccName, ccDate = options$ccDate, ccReportedBy = options$ccReportedBy, ccSubTitle = options$ccSubTitle,
                                       ccChartName = options$ccChartName)
    }
  }
}

.CCReport <- function(p1 = "", p2 = "", ccTitle = "", ccName = "", ccDate = "",
                      ccReportedBy = "", ccMisc = "" , ccSubTitle = "", ccChartName = "") {

  if (ccTitle == "")
    title <- "Report for Control Charts"
  else
    title <- ccTitle
  name <- gettextf("Name: %s", ccName)
  date <- gettextf("Date: %s", ccDate)
  text1 <- c(name, date)

  reportedBy <- gettextf("Reported by: %s", ccReportedBy)
  misc <- gettextf("Misc: %s", ccMisc)
  text2 <- c(reportedBy, misc)

  matrixPlot <- createJaspPlot(width = 1200, aspectRatio = 1)
  plotMat <- matrix(list(), 3, 2)
  plotMat[[1, 1]] <- .ggplotWithText(text1)
  plotMat[[1, 2]] <- .ggplotWithText(text2)
  plotMat[[2, 1]] <- .ggplotWithText(gettextf("Sub-title: %s", ccSubTitle))
  plotMat[[2, 2]] <- .ggplotWithText(gettextf("Name of chart: %s", ccChartName))
  plotMat[[3, 1]] <- p1
  plotMat[[3, 2]] <- p2

  p <- jaspGraphs::ggMatrixPlot(plotMat, topLabels = c(gettext(title), ""))
  matrixPlot$plotObject <- p

  return(matrixPlot)
}
