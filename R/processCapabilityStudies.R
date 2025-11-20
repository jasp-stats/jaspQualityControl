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
processCapabilityStudies <- function(jaspResults, dataset, options) {
  wideFormat <- options[["dataFormat"]] == "wideFormat"
  # In wide format we have one subgroup per row, else we need a either a grouping variable or later specify subgroup size manually
  if (wideFormat) {
    measurements <- unlist(options[["measurementsWideFormat"]])
    axisLabels <- options[["axisLabels"]]
    stages <- options[["stagesWideFormat"]]
    factorVariables <- c(axisLabels, stages)
  } else {
    measurements <- options[["measurementLongFormat"]]
    subgroupVariable <- options[["subgroup"]]
    stages <- options[["stagesLongFormat"]]
    factorVariables <- c(subgroupVariable, stages)
  }

  measurements <- measurements[measurements != ""]
  factorVariables <- factorVariables[factorVariables != ""]

  # Check if analysis is ready
  if (wideFormat) {
    ready <- length(measurements) > 0
  } else if (!wideFormat && options[["subgroupSizeType"]] == "manual") {
    ready <- length(measurements) == 1
  } else if (!wideFormat && options[["subgroupSizeType"]] == "groupingVariable") {
    ready <- length(measurements) == 1 && !identical(subgroupVariable, "")
  }

  # Data reading
  if (is.null(dataset) && ready) {
    if (length(factorVariables) >= 1) {
      dataset <- .readDataSetToEnd(columns.as.numeric = measurements, columns.as.factor = factorVariables)
    } else {
      dataset <- .readDataSetToEnd(columns.as.numeric = measurements)
    }
  }

  # Some error handling and messages
  if (ready) {
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
    if (!wideFormat && options[["subgroupSizeType"]] == "groupingVariable" && (anyNA(dataset[[subgroupVariable]]) ||
                                                                               any(dataset[[subgroupVariable]] == ""))) {
      nDroppedSubgroupRows <- sum(c(is.na(dataset[[subgroupVariable]]), dataset[[subgroupVariable]] == ""))
      dataset <- dataset[!is.na(dataset[[subgroupVariable]]),]
      dataset <- dataset[dataset[[subgroupVariable]] != "",]
      plotNotes <- paste0(plotNotes, gettextf("Removed %i observation(s) that were not assigned to any subgroups.<br>", nDroppedSubgroupRows))
    }
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
  } else if (wideFormat && ready) {
    if (axisLabels != "") {
      xAxisTitle <- options[["axisLabels"]]
      axisLabels <- dataset[[axisLabels]]
    } else {
      xAxisTitle <- gettext("Sample")
    }
  }


  # Error Handling
  if (ready) {
    .hasErrors(dataset, type = c('infinity'),
               all.target = measurements, exitAnalysisIfErrors = TRUE)
  }

  # Transform data as needed
  if (ready) {
    results <- .qcDataTransformations(jaspResults, dataset, measurements, options)
    dataset <- results[["dataset"]]
    # change specifications (limits + target value)
    transformedSpecs <- results[["transformedSpecs"]]
    options <- modifyList(options, transformedSpecs)
  }

  # Plot note about R/S chart recommendation
  if (length(measurements) > 5 && options[["controlChartType"]] == "xBarR") # if the subgroup size is above 5, R chart is not recommended
    plotNotes <- paste0(plotNotes, gettext("Subgroup size is >5, results may be biased. An s-chart is recommended."))

  # correction for zero values for non-normal capability
  if (options$capabilityStudyType == "nonNormalCapabilityAnalysis" && ready) {
    x <- na.omit(unlist(dataset[measurements]))
    zeroCorrect <- any(x == 0)
    if (zeroCorrect) {
      zeroCorrectionIndices <- which(dataset[measurements] == 0, arr.ind = TRUE)
      dataset[measurements][zeroCorrectionIndices] <- min(x[x > 0])/2
      jaspResults[["zeroWarning"]] <- createJaspHtml(text = gettext("All zero values have been replaced with a value equal to one-half of the smallest data point."), elementType = "p",
                                                     title = "Zero values found in non-normal capability study:",
                                                     position = 1)
      jaspResults[["zeroWarning"]]$dependOn(c("measurementLongFormat", "measurementsWideFormat", "capabilityStudyType",
                                              "nullDistribution"))
    }
  }



  # Report
  if (options[["report"]]) {
    nElements <- sum(options[["reportProcessStability"]]*2, options[["reportProcessCapabilityPlot"]], options[["reportProbabilityPlot"]],
      options[["reportProcessCapabilityTables"]], options[["reportMetaData"]])
    plotHeight <- ceiling(nElements/2) * 500
    reportPlot <- createJaspPlot(title = gettext("Process Capability Report"), width = 1250, height = plotHeight)
    jaspResults[["report"]] <- reportPlot
    jaspResults[["report"]]$dependOn(c(
      "subgroups", "controlChartType", "controlLimitsNumberOfSigmas",
      "capabilityStudyType", "nonNormalDistribution", "nonNormalMethod",
      "lowerSpecificationLimitBoundary", "upperSpecificationLimitBoundary",
      "processCapabilityPlotBinNumber", "processCapabilityPlotDistributions",
      "processCapabilityPlotSpecificationLimits", "xBarMovingRangeLength",
      "xmrChartMovingRangeLength", "xmrChartSpecificationLimits", "probabilityPlotRankMethod",
      "histogramBinBoundaryDirection", "nullDistribution", "controlChartSdEstimationMethodGroupSizeLargerThanOne",
      "controlChartSdEstimationMethodGroupSizeEqualOne", "controlChartSdUnbiasingConstant",
      .qcDataOptionNames(), .qcReportOptionNames(), .getDependenciesControlChartRules()
    ))

    if((!options[["upperSpecificationLimit"]] && !options[["lowerSpecificationLimit"]]) && options[["reportProcessCapabilityTables"]]) {
      reportPlot$setError(gettext("No specification limits set."))
      return()
    }

    if (!options[["reportProcessStability"]] && !options[["reportProcessCapabilityPlot"]] && !options[["reportProbabilityPlot"]] &&
        !options[["reportProcessCapabilityTables"]]) {
      reportPlot$setError(gettext("No report components selected."))
      return()
    }

    if(!ready)
      return()

    # Plot meta data
    if (options[["reportTitle"]] ) {
      title <- if (options[["reportTitleText"]] == "") gettext("Process Capability Report") else options[["reportTitleText"]]
    } else {
      title <- ""
    }

    if (options[["reportMetaData"]]) {
      text <- c()
      text <- if (options[["reportLocation"]]) c(text, gettextf("Location: %s", options[["reportLocationText"]])) else text
      text <- if (options[["reportLine"]]) c(text, gettextf("Line: %s", options[["reportLineText"]])) else text
      text <- if (options[["reportMachine"]]) c(text, gettextf("Machine: %s", options[["reportMachineText"]])) else text
      text <- if (options[["reportVariable"]]) c(text, gettextf("Variable: %s", options[["reportVariableText"]])) else text
      text <- if (options[["reportProcess"]]) c(text, gettextf("Process: %s", options[["reportProcessText"]])) else text
      text <- if (options[["reportDate"]]) c(text, gettextf("Date: %s", options[["reportDateText"]])) else text
      text <- if (options[["reportReportedBy"]]) c(text, gettextf("Reported by: %s", options[["reportReportedByText"]])) else text
      text <- if (options[["reportConclusion"]]) c(text, gettextf("Conclusion: %s", options[["reportConclusionText"]])) else text
    } else {
      text <- NULL
    }

    plots <- list()
    tables <- list()
    plotIndexCounter <- 1
    if (options[["reportProcessStability"]]) {
      controlCharts <- list()
      # X-bar and second chart OR ImR Chart
      if (options[["controlChartType"]] == "xmr") {
        ruleList1 <- .getRuleListIndividualCharts(options, type = "I")
        ruleList2 <- .getRuleListIndividualCharts(options, type = "MR")
        controlCharts[[1]] <- .controlChart(dataset = dataset[measurements], plotType = "I",
                                            nSigmasControlLimits = options[["controlLimitsNumberOfSigmas"]],
                                            xAxisLabels = seq_along(unlist(dataset[measurements])), ruleList = ruleList1)$plotObject
        controlCharts[[2]] <- .controlChart(dataset = dataset[measurements], plotType = "MR",
                                            xAxisLabels = seq_along(unlist(dataset[measurements])),
                                            nSigmasControlLimits = options[["controlLimitsNumberOfSigmas"]],
                                            movingRangeLength = options[["xmrChartMovingRangeLength"]], ruleList = ruleList2)$plotObject
      } else {
        secondPlotType <- switch(options[["controlChartType"]],
                                 "xBarR" = "R",
                                 "xBarS" = "s",
                                 "xBarMR" = "MMR")
        sdType <- switch(options[["controlChartType"]],
                         "xBarR" = "r",
                         "xBarS" = "s",
                         "xBarMR" = "r")
        columnsToPass <- c(measurements, stages)
        columnsToPass <- columnsToPass[columnsToPass != ""]
        fixedSubgroupSize <- if (options[["subgroupSizeUnequal"]] == "fixedSubgroupSize") options[["fixedSubgroupSizeValue"]] else ""
        unbiasingConstantUsed <- options[["controlChartSdUnbiasingConstant"]]
        # Create the rule list for the out-of-control signals
        ruleList1 <- .getRuleListSubgroupCharts(options, type = "xBar")
        ruleList2 <- .getRuleListSubgroupCharts(options, type = secondPlotType)
        controlCharts[[1]] <- .controlChart(dataset = dataset[columnsToPass], plotType = "xBar", xBarSdType = sdType,
                                            nSigmasControlLimits = options[["controlLimitsNumberOfSigmas"]], stages = stages,
                                            xAxisLabels = axisLabels, tableLabels = axisLabels, fixedSubgroupSize = fixedSubgroupSize,
                                            unbiasingConstantUsed = unbiasingConstantUsed, ruleList = ruleList1)$plotObject
        controlCharts[[2]] <- .controlChart(dataset = dataset[columnsToPass], plotType = secondPlotType,
                                            nSigmasControlLimits = options[["controlLimitsNumberOfSigmas"]], xAxisLabels = axisLabels,
                                            tableLabels = axisLabels, stages = stages, movingRangeLength = options[["xBarMovingRangeLength"]],
                                            fixedSubgroupSize = fixedSubgroupSize, unbiasingConstantUsed = unbiasingConstantUsed, ruleList = ruleList2)$plotObject
      }
      plots[[plotIndexCounter]] <- controlCharts
      plotIndexCounter <- plotIndexCounter + 1
    }
    if (options[["reportProcessCapabilityPlot"]]) {
      distribution <- if (options[["capabilityStudyType"]] == "normalCapabilityAnalysis") "normal" else options[["nonNormalDistribution"]]
      if (identical(stages, "")) {
        plots[[plotIndexCounter]] <- .qcProcessCapabilityPlotObject(options, dataset, measurements, stages, distribution = distribution)[[1]]
        plotIndexCounter <- plotIndexCounter + 1
      } else {
        processCapabilityPlots <- .qcProcessCapabilityPlotObject(options, dataset, measurements, stages, distribution = distribution)
        for (i in seq_along(processCapabilityPlots)) {
          plots[[plotIndexCounter]] <- processCapabilityPlots[[i]]
          plotIndexCounter <- plotIndexCounter + 1
        }
      }
    }
    if (options[["reportProbabilityPlot"]]) {
      if (identical(stages, "")) {
        plots[[plotIndexCounter]] <- .qcProbabilityPlotObject(options, dataset, measurements, stages)[[1]]
      } else {
        probabilityPlots <- .qcProbabilityPlotObject(options, dataset, measurements, stages)
        for (j in seq_along(probabilityPlots)) {
          plots[[plotIndexCounter]] <- probabilityPlots[[j]]
          plotIndexCounter <- plotIndexCounter + 1
        }
      }
      plotIndexCounter <- plotIndexCounter + 1
    }
    if (options[["reportProcessCapabilityTables"]]) {
      if (options[["capabilityStudyType"]] == "normalCapabilityAnalysis") {
        processSummaryDF <- .qcProcessSummaryTable(options, dataset, ready, container, measurements, stages, returnDataframe = TRUE)
        overallCapDF <- .qcProcessCapabilityTableOverall(options, dataset, ready, container, measurements, stages, returnOverallCapDataframe = TRUE)
        performanceDF <- .qcProcessCapabilityTableOverall(options, dataset, ready, container, measurements, stages, returnPerformanceDataframe = TRUE)

        if (.qcWithinProcessValid(options)) {
          potentialWithinDF <- .qcProcessCapabilityTableWithin(options, dataset, ready, container, measurements, stages, returnDataframe = TRUE)
          if (identical(stages, "")) {
            tables[[1]] <- list(potentialWithinDF, overallCapDF, performanceDF, processSummaryDF)
            tableTitles <- list(list("Process capability (within)", "Process performance (total)", "Non-conformance statistics", "Process summary"))
            tableSize <- 6
          } else {
            tables[[1]] <- list(potentialWithinDF, overallCapDF)
            tables[[2]] <- processSummaryDF
            tables[[3]] <- performanceDF
            tableTitles <- list(list("Process capability (within)", "Process performance (total)"), "Process summary", "Non-conformance statistics")
            tableSize <- 5
          }
        } else { # leave out within tables
          if (identical(stages, "")) {
            tables[[1]] <- list(overallCapDF, performanceDF, processSummaryDF)
            tableTitles <- list(list("Process performance (total)", "Non-conformance statistics", "Process summary"))
            tableSize <- 6
          } else {
            tables[[1]] <- list(overallCapDF)
            tables[[2]] <- processSummaryDF
            tables[[3]] <- performanceDF
            tableTitles <- list(list("Process performance (total)"), "Process summary", "Non-conformance statistics")
            tableSize <- 5
          }
        }

      } else {
        processSummaryDF <- .qcProcessCapabilityTableNonNormal(options, dataset, ready, container, measurements, stages, returnSummaryDF = TRUE)
        overallCapDF <- .qcProcessCapabilityTableNonNormal(options, dataset, ready, container, measurements, stages, returnCapabilityDF = TRUE)
        performanceDF <- .qcProcessCapabilityTableNonNormal(options, dataset, ready, container, measurements, stages, returnPerformanceDF = TRUE)
        tableSize <- 6
        if (identical(stages, "")) {
          tables[[1]] <- list(overallCapDF, performanceDF, processSummaryDF)
          tableTitles <- list(list("Process performance (total)", "Non-conformance statistics", "Process summary"))
        } else {
          tables[[1]] <- list(overallCapDF, processSummaryDF)
          tables[[2]] <- performanceDF
          tableTitles <- list(list("Process performance (total)", "Process summary"), "Non-conformance statistics")
        }
      }
    }
    reportPlotObject <- .qcReport(text = text, plots = plots, tables = tables, textMaxRows = 8, tableTitles = tableTitles,
                                  reportTitle = title, tableSize = tableSize)
    reportPlot$plotObject <- reportPlotObject
  } else {
    # X-bar and R Chart OR ImR OR X-bar and mR Chart
    if((options[["controlChartType"]] == "xBarR" | options[["controlChartType"]] == "xBarMR"  | options[["controlChartType"]] == "xBarS") &&
       options[["controlChart"]]) {
      secondPlotType <- switch(options[["controlChartType"]],
                               "xBarR" = "R",
                               "xBarS" = "s",
                               "xBarMR" = "MMR")
      sdType <- switch(options[["controlChartType"]],
                       "xBarR" = "r",
                       "xBarS" = "s",
                       "xBarMR" = "r")
      secondPlotTitle <- switch(options[["controlChartType"]],
                                "xBarR" = "R",
                                "xBarS" = "s",
                                "xBarMR" = "mR")
      # first chart is always xBar-chart, second is either R-, mR-, or s-chart
      jaspResults[["xBar"]] <- createJaspContainer(gettextf("X-bar & %s control chart", secondPlotTitle))
      jaspResults[["xBar"]]$dependOn(c(
        "subgroups", "controlChartType", "report", "controlChartSdUnbiasingConstant",
        "controlChart", "controlLimitsNumberOfSigmas",
        .qcDataOptionNames(), .getDependenciesControlChartRules()
      ))
      jaspResults[["xBar"]]$position <- 1


      if (ready && is.null(jaspResults[["xBar"]][["plot"]])) {
        jaspResults[["xBar"]][["plot"]] <- createJaspPlot(title = gettextf("X-bar & %s control chart", secondPlotTitle),
                                                          width = 1200, height = 500)
        # Error conditions
        if (secondPlotType == "R" && length(measurements) > 50) { # if the subgroup size is above 50, the R package cannot calculate R charts.
          jaspResults[["xBar"]][["plot"]]$setError(gettext("Subgroup size is >50, R chart calculation is not possible. Use S-chart instead."))
          return()
        } else if(length(measurements) < 2) {
          jaspResults[["xBar"]][["plot"]]$setError(gettext("Subgroup size is 1, calculation of selected control chart not possible. Use x-mR chart."))
          return()
        }
        columnsToPass <- c(measurements, stages)
        columnsToPass <- columnsToPass[columnsToPass != ""]
        fixedSubgroupSize <- if (options[["subgroupSizeUnequal"]] == "fixedSubgroupSize") options[["fixedSubgroupSizeValue"]] else ""
        unbiasingConstantUsed <- options[["controlChartSdUnbiasingConstant"]]
        # Create the rule list for the out-of-control signals
        ruleList1 <- .getRuleListSubgroupCharts(options, type = "xBar")
        ruleList2 <- .getRuleListSubgroupCharts(options, type = secondPlotType)
        xBarChart <- .controlChart(dataset = dataset[columnsToPass], plotType = "xBar", xBarSdType = sdType,
                                   nSigmasControlLimits = options[["controlLimitsNumberOfSigmas"]], stages = stages,
                                   xAxisLabels = axisLabels, tableLabels = axisLabels, fixedSubgroupSize = fixedSubgroupSize,
                                   unbiasingConstantUsed = unbiasingConstantUsed, ruleList = ruleList1)
        secondPlot <- .controlChart(dataset = dataset[columnsToPass], plotType = secondPlotType,
                                    nSigmasControlLimits = options[["controlLimitsNumberOfSigmas"]], xAxisLabels = axisLabels,
                                    tableLabels = axisLabels, stages = stages, movingRangeLength = options[["xBarMovingRangeLength"]],
                                    fixedSubgroupSize = fixedSubgroupSize, unbiasingConstantUsed = unbiasingConstantUsed, ruleList = ruleList2)
        jaspResults[["xBar"]][["plot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(secondPlot$plotObject, xBarChart$plotObject),
                                                                               layout = matrix(2:1, 2), removeXYlabels= "x")
        if (!identical(plotNotes, ""))
          jaspResults[["xBar"]][["plotNote"]] <- createJaspHtml(paste0("<i>Note.</i> ", plotNotes))
        jaspResults[["xBar"]][["tableXBar"]] <- xBarChart$table
        jaspResults[["xBar"]][["tableSecondPlot"]] <- secondPlot$table
      }
    } else if (options[["controlChartType"]] == "xmr" && options[["controlChart"]]) {
      jaspResults[["xmr"]] <- createJaspContainer(gettext("X-mR control chart"))
      jaspResults[["xmr"]]$dependOn(c(
        "subgroups", "controlChartType", "report", "controlChart",
        "controlLimitsNumberOfSigmas",
        .qcDataOptionNames(), .getDependenciesControlChartRules()
      ))

      jaspResults[["xmr"]]$position <- 1
      if (ready && is.null(jaspResults[["xmr"]][["plot"]])) {
        jaspResults[["xmr"]][["plot"]] <- createJaspPlot(title =  gettext("X-mR control chart"), width = 1200, height = 500)
        columnsToPass <- c(measurements, stages)
        columnsToPass <- columnsToPass[columnsToPass != ""]
        if (options[["xmrChartSpecificationLimits"]]) {
          specificationLimitsControlChart <- c()
          specificationLimitsControlChart <- if (options[["lowerSpecificationLimit"]]) c(specificationLimitsControlChart,
                                                                                         options[["lowerSpecificationLimitValue"]]) else c(specificationLimitsControlChart, NA)
          specificationLimitsControlChart <- if (options[["target"]]) c(specificationLimitsControlChart,
                                                                        options[["targetValue"]]) else c(specificationLimitsControlChart, NA)
          specificationLimitsControlChart <- if (options[["upperSpecificationLimit"]]) c(specificationLimitsControlChart,
                                                                                         options[["upperSpecificationLimitValue"]]) else c(specificationLimitsControlChart, NA)
        } else {
          specificationLimitsControlChart <- NA
        }
        ruleList1 <- .getRuleListIndividualCharts(options, type = "I")
        ruleList2 <- .getRuleListIndividualCharts(options, type = "MR")
        individualChart <- .controlChart(dataset = dataset[columnsToPass], plotType = "I",
                                         nSigmasControlLimits = options[["controlLimitsNumberOfSigmas"]], stages = stages,
                                         xAxisLabels = seq_along(unlist(dataset[measurements])),
                                         specificationLimits = specificationLimitsControlChart, ruleList = ruleList1)
        mrChart <- .controlChart(dataset = dataset[columnsToPass], plotType = "MR",
                                 nSigmasControlLimits = options[["controlLimitsNumberOfSigmas"]], stages = stages,
                                 xAxisLabels = seq_along(unlist(dataset[measurements])), movingRangeLength = options[["xmrChartMovingRangeLength"]], ruleList = ruleList2)
        jaspResults[["xmr"]][["plot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(mrChart$plotObject, individualChart$plotObject),
                                                                              layout = matrix(2:1, 2), removeXYlabels= "x")
        if (!identical(plotNotes, ""))
          jaspResults[["xmr"]][["plotNote"]] <- createJaspHtml(paste0("<i>Note.</i> ", plotNotes))
        jaspResults[["xmr"]][["tableIndividual"]] <- individualChart$table
        jaspResults[["xmr"]][["tableMR"]] <- mrChart$table
      }
    }

    # Distribution plot - moved jaspResults ref here to avoid big files
    .qcDistributionPlot(options, dataset, ready, jaspResults, measurements, stages)

    # Probability plots section
    .qcProbabilityPlotContainer(options, dataset, ready, jaspResults, measurements, stages)

    # Perform capability analysis
    .qcCapabilityAnalysis(options, dataset, ready, jaspResults, measurements, stages)
  }
}

# Functions for capability analysis section ####

## Containers ####


.qcCapabilityAnalysis <- function(options, dataset, ready, jaspResults, measurements, stages) {
  container <- createJaspContainer(gettext("Capability study"))
  container$dependOn(c(
    "capabilityStudyType", "processCapabilityPlot", "processCapabilityTable",
    "manualSubgroupSize", "report", "controlChartSdUnbiasingConstant",
    "lowerSpecificationLimitBoundary", "upperSpecificationLimitBoundary",
    "controlChartSdEstimationMethodGroupSizeLargerThanOne", "controlChartSdEstimationMethodGroupSizeEqualOne",
    "controlChartSdEstimationMethodMeanMovingRangeLength",
    .qcDataOptionNames()
  ))

  container$position <- 4
  jaspResults[["capabilityAnalysis"]] <- container

  if (!ready)
    return()

  if (sum(options[["upperSpecificationLimit"]], options[["lowerSpecificationLimit"]], options[["target"]]) >= 2) {
    if (options[["lowerSpecificationLimit"]]) {
      if (options[["upperSpecificationLimit"]] && options[["upperSpecificationLimitValue"]] < options[["lowerSpecificationLimitValue"]]){
        p <- createJaspPlot(title = gettext("Process capability study"), width = 400, height = 400)
        p$setError(gettext("Lower specification limit must be smaller than upper specification limit."))
        container[["p"]] <- p
        return()
      }
    }
    if (options[["target"]]) {
      if ((options[["upperSpecificationLimit"]] && options[["upperSpecificationLimitValue"]] < options[["targetValue"]]) ||
          (options[["lowerSpecificationLimit"]] && options[["lowerSpecificationLimitValue"]] > options[["targetValue"]])) {
        p <- createJaspPlot(title = gettext("Process capability study"), width = 400, height = 400)
        p$setError(gettext("Target outside of specification limits."))
        container[["p"]] <- p
        return()
      }
    }
  }

  ready <- (length(measurements) > 0 && (options[["lowerSpecificationLimit"]] | options[["upperSpecificationLimit"]]))

  if (options[["capabilityStudyType"]] == "nonNormalCapabilityAnalysis" && ready && (any(na.omit(unlist(dataset[measurements])) < 0))) {
    p <- createJaspPlot(title = gettext("Process capability study"), width = 400, height = 400)
    p$setError(gettext("Dataset contains negative numbers. Not compatible with the selected distribution."))
    container[["p"]] <- p
    return()
  }

  if (options[["capabilityStudyType"]] == "normalCapabilityAnalysis") {
    normalContainer <- createJaspContainer(gettext("Process capability"))
    normalContainer$position <- 1
    container[["normalCapabilityAnalysis"]] <- normalContainer
    .qcProcessSummaryTable(options, dataset, ready, normalContainer, measurements, stages)
    if (options[["processCapabilityPlot"]])
      .qcProcessCapabilityPlot(options, dataset, ready, normalContainer, measurements, stages, "normal")
    if (options[["processCapabilityTable"]]) {
      if (.qcWithinProcessValid(options))
        .qcProcessCapabilityTableWithin(options, dataset, ready, normalContainer, measurements, stages)

      .qcProcessCapabilityTableOverall(options, dataset, ready, normalContainer, measurements, stages)
    }
  }

  if (options[["capabilityStudyType"]] == "nonNormalCapabilityAnalysis") {
    nonNormalContainer <- createJaspContainer(gettext("Process capability (non-normal capability analysis)"))
    nonNormalContainer$position <- 2
    container[["nonNormalCapabilityAnalysis"]] <- nonNormalContainer
    if (options[["processCapabilityPlot"]])
      .qcProcessCapabilityPlot(options, dataset, ready, nonNormalContainer, measurements, stages, options[["nonNormalDistribution"]])
    if (options[["processCapabilityTable"]])
      .qcProcessCapabilityTableNonNormal(options, dataset, ready, nonNormalContainer, measurements, stages)
  }
}

## Output ####

.qcDataTransformations <- function(jaspResults, dataset, measurements, options) {
  # this function transforms `dataset`, lower and upper specification limits, and target (in the options list)
  # transformed `dataset` and `options` are returned in a list
  # as a side product, this function generates output that shows to the user how was the data transformed

  # if no transform, don't show anything and return unchanged inputs
  if (options[["dataTransformation"]] == "none") return(list(dataset=dataset, transformedSpecs=list()))

  # create the main output (fill later)
  dataTransformationContainer <- jaspResults[["dataTransformationContainer"]] %setOrRetrieve%
    createJaspContainer(
      title = gettext("Data transformation"),
      dependencies = .qcDataOptionNames(),
      position=0
    )

  # return state if available
  if(!is.null(jaspResults[["dataTransformationState"]])) return(jaspResults[["dataTransformationState"]]$object)

  # transform data and return parameters of the transform
  result <- try(.qcTransformData(dataset = dataset, measurements = measurements, options = options))

  if(isTryError(result)) {
    message <- gettextf("Data could not be transformed: %1$s", .extractErrorMessage(result))
    .quitAnalysis(message)
  }

  # retrieve data and parameters
  dataset <- result[["dataset"]]
  parameters <- result[["parameters"]]


  # transform lower and upper spec limits, target
  transformedSpecs <- try(.qcTransformSpecs(options = options, parameters = parameters), silent=TRUE)

  if(isTryError(transformedSpecs)) {
    message <- gettextf("Specification limits could not be transformed: %1$s", .extractErrorMessage(transformedSpecs))
    .quitAnalysis(message)
  }

  .qcFillTransformOutput(dataTransformationContainer, options=options, parameters=parameters)

  output <- list(dataset=dataset, transformedSpecs=transformedSpecs)

  jaspResults[["dataTransformationState"]] <- createJaspState(object = output, dependencies = .qcDataOptionNames())

  return(output)
}

.qcProcessSummaryTable <- function(options, dataset, ready, container, measurements, stages, returnDataframe = FALSE) {
  if (identical(stages, "")) {
    nStages <- 1
    dataset[["stage"]] <- 1
    stages <- "stage"
  } else if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
  }
  table <- createJaspTable(title = gettext("Process summary"))
  table$position <- 1
  if (nStages > 1) {
    table$addColumnInfo(name = "stage", title = gettext("Stage"), type = "string")
    table$transpose <- TRUE
  }
  if (options[["lowerSpecificationLimit"]]) {
    lslTitle <- if (options[["lowerSpecificationLimitBoundary"]]) gettext("LB") else gettext("LSL")
    table$addColumnInfo(name = "lsl", type = "integer", title = lslTitle)
  }
  if (options[["target"]])
    table$addColumnInfo(name = "target", type = "integer", title = gettext("Target"))
  if (options[["upperSpecificationLimit"]]) {
    uslTitle <- if (options[["upperSpecificationLimitBoundary"]]) gettext("UB") else gettext("USL")
    table$addColumnInfo(name = "usl", type = "integer", title = uslTitle)
  }
  table$addColumnInfo(name = "n", type = "integer", title = gettext("Sample size"))
  table$addColumnInfo(name = "mean", type = "number", title = gettext("Mean"))
  table$addColumnInfo(name = "sd", type = "number", title = gettext("Std. dev. (total)"))
  if (.qcWithinProcessValid(options))
    table$addColumnInfo(name = "sdw", type = "number", title = gettext("Std. dev. (within)"))
  table$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  tableColNames <- c("lsl", "target", "usl", "mean", "n", "sd", "sdw")
  if (nStages > 1) {
    tableColNames <- c("stage", tableColNames)
    table$addFootnote(gettext("Columns titled 'Change' concern changes of the respective stage in comparison to baseline (BL)."))
  }
  tableDf <- data.frame(matrix(ncol = length(tableColNames), nrow = 0))
  colnames(tableDf) <- tableColNames
  for (i in seq_len(nStages)) {
    stage <- unique(dataset[[stages]])[i]
    dataCurrentStage <- dataset[which(dataset[[stages]] == stage), ][!names(dataset) %in% stages]
    if (length(measurements) < 2) {
      k <- options[["controlChartSdEstimationMethodMeanMovingRangeLength"]]
      sdw <- if (options[["historicalStdDev"]]) options[["historicalStdDevValue"]] else .controlChart_calculations(dataCurrentStage[measurements], plotType = "MR", movingRangeLength = k)$sd
    } else {
      sdType <- if (options[["controlChartSdEstimationMethodGroupSizeLargerThanOne"]] == "rBar") "r" else "s"
      unbiasingConstantUsed <- options[["controlChartSdUnbiasingConstant"]]
      sdw <- if (options[["historicalStdDev"]]) options[["historicalStdDevValue"]] else .sdXbar(dataCurrentStage[measurements], type = sdType, unbiasingConstantUsed = unbiasingConstantUsed)
    }
    allData <- na.omit(unlist(dataCurrentStage[, measurements]))

    if (is.na(sdw))
      table$addFootnote(gettext("The within standard deviation could not be calculated."))

    processMean <- if (options[["historicalMean"]]) options[["historicalMeanValue"]] else mean(allData, na.rm = TRUE)

    tableDfCurrentStage <- data.frame(lsl = round(options[["lowerSpecificationLimitValue"]], .numDecimals),
                                      target = round(options[["targetValue"]], .numDecimals),
                                      usl    = round(options[["upperSpecificationLimitValue"]], .numDecimals),
                                      mean   = processMean,
                                      n      = length(allData),
                                      sd     = sd(allData, na.rm = TRUE),
                                      sdw    = sdw)
    if (i == 1 && nStages > 1)
      baseLineDf <- tableDfCurrentStage
    if (i > 1) {
      changeDf <- tableDfCurrentStage - baseLineDf
      changeDf$stage <- gettextf("Change (%s vs. BL)", stage)
      # Differences in specification limits are not relevant
      changeDf$lsl <- "-"
      changeDf$target <- "-"
      changeDf$usl <- "-"
    }
    if(nStages > 1)
      tableDfCurrentStage$stage <- if (i == 1) gettextf("%s (BL)", stage) else as.character(stage)
    tableDf <- rbind(tableDf, tableDfCurrentStage)
    if (i > 1)
      tableDf <- rbind(tableDf, changeDf)
  }
  tableList <- as.list(tableDf)
  table$setData(tableList)

  nDecimals <- .numDecimals

  if (returnDataframe) {
    lslTitle <- if (options[["lowerSpecificationLimitBoundary"]]) gettext("LB") else gettext("LSL")
    uslTitle <- if (options[["upperSpecificationLimitBoundary"]]) gettext("UB") else gettext("USL")
    sourceVector <- c(lslTitle, "Target", uslTitle, "N", "Mean", "SD (total)")
    if (nStages > 1)
      sourceVector <- c("Stage", sourceVector)
    if (.qcWithinProcessValid(options)) {
      sourceVector <- c(sourceVector, "SD (within)")
    }
    tableNRows <- if (nStages > 1) nStages * 2 - 1 else 1
    formattedTableDf <- data.frame(matrix(nrow = tableNRows, ncol = 0))
    if (nStages > 1) {
      formattedStages <- tableList[["stage"]]
      changeNames <- formattedStages[grep("Change", formattedStages)]
      formattedChangeNames <- unlist(sapply(changeNames, function(x) strsplit(unlist(strsplit(x, "\\("))[2], "\\)"))) # removing the word "Change" and the brackets because it makes the table too big in the report
      formattedStages[grep("Change", formattedStages)] <- formattedChangeNames
      formattedTableDf[["stage"]] <- formattedStages
    }
    lsl <- options[["lowerSpecificationLimitValue"]]
    target <- options[["targetValue"]]
    usl <- options[["upperSpecificationLimitValue"]]
    if (!options[["lowerSpecificationLimit"]])
      lsl <- '*'
    if (!options[["target"]])
      target <- '*'
    if (!options[["upperSpecificationLimit"]])
      usl <- '*'
    formattedTableDf[["lsl"]] <- tableList[["lsl"]]
    formattedTableDf[["target"]] <- tableList[["target"]]
    formattedTableDf[["usl"]] <- tableList[["usl"]]
    formattedTableDf[["n"]] <- tableList[["n"]]
    formattedTableDf[["mean"]] <- round(tableList[["mean"]], nDecimals)
    formattedTableDf[["sd"]] <- round(tableList[["sd"]], nDecimals)
    if (.qcWithinProcessValid(options)) formattedTableDf[["sdw"]] <- round(tableList[["sdw"]], nDecimals)
    colnames(formattedTableDf) <- sourceVector
    return(formattedTableDf)
  }
  container[["processSummaryTable"]] <- table
}

.qcProcessCapabilityPlot <- function(options, dataset, ready, container, measurements, stages,
                                     distribution = c('normal', "weibull", "lognormal", "3ParameterLognormal", "3ParameterWeibull")) {
  # calculating plot dimensions based on number of stages
  if (identical(stages, "")) {
    nStages <- 1
  } else if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
  }
  # making this explicit in case we want to change it at some point
  nCol <- 1
  nRow <- nStages
  plotWidth <- nCol * 800
  plotHeight <- nRow * 500
  plot <- createJaspPlot(title = gettext("Capability of the process"), width = plotWidth, height = plotHeight)
  plot$dependOn(c("processCapabilityPlotBinNumber", "histogramBinBoundaryDirection"))
  plot$position <- 2
  container[["capabilityPlot"]] <- plot

  if (!options[["upperSpecificationLimit"]] && !options[["lowerSpecificationLimit"]]) {
    plot$setError(gettext("No specification limits set."))
    return()
  }
  if (!ready)
    return()
  if (sum(!is.na(dataset[measurements])) < 1) {
    plot$setError(gettext("No valid measurements detected."))
    return()
  }
  if (sum(!is.na(dataset[measurements])) < 2 && options[["processCapabilityPlotDistributions"]]) {
    plot$setError(gettextf("Need at least 2 measurements to fit distribution. %1$i measurement(s) detected.",
                           sum(!is.na(dataset[measurements]))))
    return()
  }
  if (nStages > 1 && (length(unique(na.omit(dataset)[[stages]])) < nStages)) {
    plot$setError(gettext("At least one of the stages contains no valid measurements."))
    return()
  }
  plotList <- try(.qcProcessCapabilityPlotObject(options, dataset, measurements, stages, distribution))
  if (jaspBase::isTryError(plotList)) {
    plot$setError(plotList[1])
    return()
  }
  plotMat <- matrix(plotList, ncol = nCol, nrow = nRow, byrow = TRUE)
  plot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)
}

.qcProcessCapabilityPlotObject <- function(options, dataset, measurements, stages, distribution = c('normal', "weibull", "lognormal", "3ParameterLognormal", "3ParameterWeibull")) {
  if (identical(stages, "")) {
    nStages <- 1
    dataset[["stage"]] <- 1
    stages <- "stage"
  } else if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
  }
  plotList <- list()
  for (i in seq_len(nStages)) {
    stage <- unique(dataset[[stages]])[i]
    dataCurrentStage <- dataset[which(dataset[[stages]] == stage), ][!names(dataset) %in% stages]
    if (length(measurements) < 2) {
      k <- options[["controlChartSdEstimationMethodMeanMovingRangeLength"]]
      sdw <- if (options[["historicalStdDev"]]) options[["historicalStdDevValue"]] else .controlChart_calculations(dataCurrentStage[measurements], plotType = "MR", movingRangeLength = k)$sd
    } else {
      sdType <- if (options[["controlChartSdEstimationMethodGroupSizeLargerThanOne"]] == "rBar") "r" else "s"
      unbiasingConstantUsed <- options[["controlChartSdUnbiasingConstant"]]
      sdw <- if (options[["historicalStdDev"]]) options[["historicalStdDevValue"]] else .sdXbar(dataCurrentStage[measurements], type = sdType, unbiasingConstantUsed = unbiasingConstantUsed)
    }
    allData <- as.vector(na.omit(unlist(dataCurrentStage[, measurements])))
    plotData <- data.frame(x = allData)
    sdo <- sd(allData, na.rm = TRUE)

    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(plotData[["x"]], min(plotData[["x"]]) - 1 * sdo, max(plotData[["x"]]) + 1 * sdo), min.n = 4)
    xLimits <- range(xBreaks)
    if (options[["lowerSpecificationLimit"]] && options[["processCapabilityPlotSpecificationLimits"]])
      xLimits <- range(xLimits, options[["lowerSpecificationLimitValue"]])
    if (options[["upperSpecificationLimit"]] && options[["processCapabilityPlotSpecificationLimits"]])
      xLimits <- range(xLimits, options[["upperSpecificationLimitValue"]])
    if (options[["target"]] && options[["processCapabilityPlotSpecificationLimits"]])
      xLimits <- range(xLimits, options[["target"]])

    nBins <- options[["processCapabilityPlotBinNumber"]]
    h <- hist(allData, plot = FALSE, breaks = nBins)
    binWidth <- (h$breaks[2] - h$breaks[1])

    p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x)) +
      ggplot2::geom_histogram(ggplot2::aes(y =..density..), closed = options[["histogramBinBoundaryDirection"]],
                              fill = "grey", col = "black", linewidth = .7, binwidth = binWidth, center = binWidth/2, na.rm = TRUE) +
      ggplot2::scale_y_continuous(name = gettext("Density")) +
      ggplot2::scale_x_continuous(name = gettext("Measurement"), breaks = xBreaks, limits = xLimits)

    legendColors <- c()
    legendLty <- c()
    legendLabels <- c()

    # Overlay distribution
    if (options[["processCapabilityPlotDistributions"]]) {
      if (distribution == "normal") {
        processMean <- if (options[["historicalMean"]]) options[["historicalMeanValue"]] else mean(allData, na.rm = TRUE)
        if (.qcWithinProcessValid(options)) {
        p <- p + ggplot2::stat_function(fun = dnorm, args = list(mean = processMean, sd = sd(allData)),
                                        mapping = ggplot2::aes(color = "sdoDist", linetype = "sdoDist")) +
          ggplot2::stat_function(fun = dnorm, args = list(mean = processMean, sd = sdw),
                                 mapping = ggplot2::aes(color = "sdwDist", linetype = "sdwDist"))
        legendColors <- c(legendColors, "dodgerblue", "red")
        legendLty <- c(legendLty, "solid", "solid")
        legendLabels <- c(legendLabels, gettext("Normal dist.\n(std. dev. total)"),
                          gettext("Normal dist.\n(std. dev. within)"))
        }
      } else if (distribution == "weibull") {
        distParameters <- .distributionParameters(data = allData, distribution = distribution)
        if (jaspBase::isTryError(distParameters))
          stop(distParameters[1], call. = FALSE)
        shape <- distParameters$beta
        scale <- distParameters$theta
        p <- p + ggplot2::stat_function(fun = dweibull, args = list(shape = shape, scale = scale),
                                        mapping = ggplot2::aes(color = "weibull", linetype = "weibull"))
        legendColors <- c(legendColors, "red")
        legendLty <- c(legendLty, "solid")
        legendLabels <- c(legendLabels, gettext("Weibull dist."))
      } else if (distribution == "lognormal") {
        distParameters <- .distributionParameters(data = allData, distribution = distribution)
        if (jaspBase::isTryError(distParameters))
          stop(distParameters[1], call. = FALSE)
        shape <- distParameters$beta
        scale <- distParameters$theta
        p <- p + ggplot2::stat_function(fun = dlnorm, args = list(meanlog = shape, sdlog = scale),
                                        mapping = ggplot2::aes(color = "lognormal", linetype = "lognormal"))
        legendColors <- c(legendColors, "red")
        legendLty <- c(legendLty, "solid")
        legendLabels <- c(legendLabels, "Lognormal dist.")
      } else if (distribution == "3ParameterLognormal") {
        distParameters <- .distributionParameters(data = allData, distribution = distribution)
        if (jaspBase::isTryError(distParameters))
          stop(distParameters[1], call. = FALSE)
        shape <- distParameters$theta
        scale <- distParameters$beta
        threshold <- distParameters$threshold
        p <- p + ggplot2::stat_function(fun = FAdist::dlnorm3 , args = list(shape = shape, scale = scale, thres = threshold),
                                        mapping = ggplot2::aes(color = "lognormal3", linetype = "lognormal3"))
        legendColors <- c(legendColors, "red")
        legendLty <- c(legendLty, "solid")
        legendLabels <- c(legendLabels, gettext("3-parameter\nlognormal dist."))
      } else if (distribution == "3ParameterWeibull") {
        distParameters <- .distributionParameters(data = allData, distribution = distribution)
        if (jaspBase::isTryError(distParameters))
          stop(distParameters[1], call. = FALSE)
        shape <- distParameters$beta
        scale <- distParameters$theta
        threshold <- distParameters$threshold
        p <- p + ggplot2::stat_function(fun = FAdist::dweibull3 , args = list(shape = shape, scale = scale, thres = threshold),
                                        mapping = ggplot2::aes(color = "weibull3", linetype = "weibull3"))
        legendColors <- c(legendColors, "red")
        legendLty <- c(legendLty, "solid")
        legendLabels <- c(legendLabels, gettext("3-parameter Weibull dist."))
      }
    }

    # Display specification limits
    if (options[["processCapabilityPlotSpecificationLimits"]]) {
      specLimitsDf <- data.frame(matrix(ncol = 5, nrow = 0))
      colnames(specLimitsDf) <- c("label", "xIntercept", "lty", "yPosLabel", "color")
      yPosLabel <- max(ggplot2::layer_scales(p)$y$range$range)
      if (options[["target"]]) {
        specLimitsDf <- rbind(specLimitsDf, data.frame(label = gettextf("Target = %g", round(options[["targetValue"]], .numDecimals)),
                                                       xIntercept = options[["targetValue"]], lty = "solid", yPosLabel = yPosLabel,
                                                       color = "darkgreen"))
      }
      if (options[["lowerSpecificationLimit"]]) {
        lslLty <- if (options[["lowerSpecificationLimitBoundary"]]) "solid" else "dotted"
        lslLabel <- if (options[["lowerSpecificationLimitBoundary"]]) gettextf("LB = %g", round(options[["lowerSpecificationLimitValue"]], .numDecimals)) else gettextf("LSL = %g", round(options[["lowerSpecificationLimitValue"]], .numDecimals))
        specLimitsDf <- rbind(specLimitsDf, data.frame(label = lslLabel, xIntercept = options[["lowerSpecificationLimitValue"]],
                                                       lty = lslLty, yPosLabel = yPosLabel, color = "darkred"))
      }
      if (options[["upperSpecificationLimit"]]) {
        uslLty <- if (options[["upperSpecificationLimitBoundary"]]) "solid" else "dotted"
        uslLabel <- if (options[["upperSpecificationLimitBoundary"]]) gettextf("UB = %g", round(options[["upperSpecificationLimitValue"]], .numDecimals)) else gettextf("USL = %g", round(options[["upperSpecificationLimitValue"]], .numDecimals))
        specLimitsDf <- rbind(specLimitsDf, data.frame(label = uslLabel, xIntercept = options[["upperSpecificationLimitValue"]],
                                                       lty = uslLty, yPosLabel = yPosLabel, color = "darkred"))
      }
      p <- p + ggplot2::geom_vline(data = specLimitsDf,
                                   mapping = ggplot2::aes(xintercept = xIntercept), color = specLimitsDf$color,
                                   linetype = specLimitsDf$lty, linewidth = 1, na.rm = TRUE) +
        ggplot2::geom_label(specLimitsDf, mapping = ggplot2::aes(x = xIntercept, y = yPosLabel, label = label), inherit.aes = FALSE,
                            size = 4.5, na.rm = TRUE)
    }

    # Add a legend if needed
    if (options[["processCapabilityPlotDistributions"]] || options[["processCapabilityPlotSpecificationLimits"]]) {
      p <- p + ggplot2::scale_color_manual("", values = legendColors, labels = legendLabels) +
        ggplot2::scale_linetype_manual("", values = legendLty, labels = legendLabels)
    }
    if (nStages > 1)
      p <- p + ggplot2::ggtitle(stage) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    p <- p + jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), legend.position = "right")
    plotList[[i]] <- p
  }
  return(plotList)
}

.qcProcessCapabilityTableWithin <- function(options, dataset, ready, container, measurements, stages, returnDataframe = FALSE) {
  if (!options[["lowerSpecificationLimit"]] && !options[["upperSpecificationLimit"]])
    return()
  if (!ready)
    return()

  if (identical(stages, "")) {
    nStages <- 1
    dataset[["stage"]] <- 1
    stages <- "stage"
  } else if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
  }
  table <- createJaspTable(title = gettext("Process capability (within)"))
  sourceVector <- c()
  if (nStages > 1) {
    table$addColumnInfo(name = "stage",      	title = gettext("Stage"),  		type = "string")
    table$transpose <- TRUE
    table$addFootnote(gettext("Columns titled 'Change' concern changes of the respective stage in comparison to baseline (BL)."))
    sourceVector <- c(sourceVector, "Stage")
  }
  tableColNames <- c()
  ciLevel <- options[["processCapabilityTableCiLevel"]]
  ciLevelPercent <- ciLevel * 100

  if (options[["lowerSpecificationLimit"]] && options[["upperSpecificationLimit"]]) {
    table$addColumnInfo(name = "cp", type = "integer", title = gettext("Cp"))
    sourceVector <- c(sourceVector, 'Cp')
    tableColNames <- c(tableColNames, "cp")
    if (options[["processCapabilityTableCi"]] &&
        !(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])) {
      table$addColumnInfo(name = "cplci", title = gettext("Lower"), type = "integer", overtitle = gettextf("%s CI for Cp", paste(ciLevelPercent, "%")))
      table$addColumnInfo(name = "cpuci", title = gettext("Upper"), type = "integer", overtitle = gettextf("%s CI for Cp", paste(ciLevelPercent, "%")))
      tableColNames <- c(tableColNames, "cplci", "cpuci")
      sourceVector <- c(sourceVector, paste0(ciLevelPercent, "% CI Cp"))
    }
  }
  if (options[["lowerSpecificationLimit"]]) {
    table$addColumnInfo(name = "cpl",   type = "integer", title = gettext("CpL"))
    sourceVector <- c(sourceVector, 'CpL')
    tableColNames <- c(tableColNames, "cpl")
  }
  if (options[["upperSpecificationLimit"]]) {
    table$addColumnInfo(name = "cpu",   type = "integer", title = gettext("CpU"))
    sourceVector <- c(sourceVector, 'CpU')
    tableColNames <- c(tableColNames, "cpu")
  }
  table$addColumnInfo(name = "cpk",   type = "integer", title = gettext("Cpk"))
  sourceVector <- c(sourceVector, 'Cpk')
  tableColNames <- c(tableColNames, "cpk")
  if (options[["processCapabilityTableCi"]] &&
      !(options[["upperSpecificationLimitBoundary"]] && options[["lowerSpecificationLimitBoundary"]])) {
    table$addColumnInfo(name = "cpklci", title = gettext("Lower"), type = "integer", overtitle = gettextf("%s CI for Cpk", paste(ciLevelPercent, "%")))
    table$addColumnInfo(name = "cpkuci", title = gettext("Upper"), type = "integer", overtitle = gettextf("%s CI for Cpk", paste(ciLevelPercent, "%")))
    tableColNames <- c(tableColNames, "cpklci", "cpkuci")
    sourceVector <- c(sourceVector, paste0(ciLevelPercent, "% CI Cpk"))
  }

  if (options[["processCapabilityTableZbench"]]) {
    table$addColumnInfo(name="zbench", title=gettext("Z bench (ST)"), type="number")
  }

  table$showSpecifiedColumnsOnly <- TRUE
  if (options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])
    table$addFootnote(gettext("Statistics displayed as * were not calculated because the relevant specification limit is not set or set as boundary."))

  if (nStages > 1)
    tableColNames <- c("stage", tableColNames)
  tableDf <- data.frame(matrix(ncol = length(tableColNames), nrow = 0))
  colnames(tableDf) <- tableColNames
  for (i in seq_len(nStages)) {
    stage <- unique(dataset[[stages]])[i]
    dataCurrentStage <- dataset[which(dataset[[stages]] == stage), ][!names(dataset) %in% stages]

    if (length(measurements) < 2) {
      k <- options[["controlChartSdEstimationMethodMeanMovingRangeLength"]]
      sdw <- if (options[["historicalStdDev"]]) options[["historicalStdDevValue"]] else .controlChart_calculations(dataCurrentStage[measurements], plotType = "MR", movingRangeLength = k)$sd
    } else {
      sdType <- if (options[["controlChartSdEstimationMethodGroupSizeLargerThanOne"]] == "rBar") "r" else "s"
      unbiasingConstantUsed <- options[["controlChartSdUnbiasingConstant"]]
      sdw <- if (options[["historicalStdDev"]]) options[["historicalStdDevValue"]] else .sdXbar(dataCurrentStage[measurements], type = sdType, unbiasingConstantUsed = unbiasingConstantUsed)
    }
    allData <- na.omit(unlist(dataCurrentStage[, measurements]))

    # Calculate capability indices
    usl <- options[["upperSpecificationLimitValue"]]
    lsl <- options[["lowerSpecificationLimitValue"]]
    n <- length(allData)
    k <- length(measurements)
    processMean <- if (options[["historicalMean"]]) options[["historicalMeanValue"]] else mean(allData, na.rm = TRUE)
    tolMultiplier <- 6
    cp <- if (options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]]) NA else (usl - lsl) / (tolMultiplier * sdw)
    cpl <-if (options[["lowerSpecificationLimitBoundary"]]) NA else (processMean - lsl) / ((tolMultiplier/2) * sdw)
    cpu <- if (options[["upperSpecificationLimitBoundary"]]) NA else (usl - processMean) / ((tolMultiplier/2) * sdw)
    if (options[["lowerSpecificationLimit"]] && options[["upperSpecificationLimit"]]) {
      cpk <-if (options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]]) NA else min(cpu, cpl, na.rm = TRUE)
    } else if (options[["lowerSpecificationLimit"]] && !options[["upperSpecificationLimit"]]) {
      cpk <- cpl
    } else {
      cpk <- cpu
    }
    zbench <- 3*cpk


    if (options[["processCapabilityTableCi"]]) {
      ciAlpha <- 1 - ciLevel
      if (!(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])) {
        #CI for Cp
        dfCp <- 0.9 * k * ((n/k) - 1)
        ciLbCp <- cp * sqrt( qchisq(p = ciAlpha/2, df = dfCp) /dfCp)
        ciUbCp <- cp * sqrt( qchisq(p = 1 - (ciAlpha/2), df = dfCp) /dfCp)
      }
      if (!(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])) {
        #CI for Cpk
        dfCpk <- 0.9 * k * ((n/k) - 1)
        normCIrange <- qnorm(1 - (ciAlpha / 2))
        intervalCpk <- sqrt(1 / (((tolMultiplier / 2)^2) * n)  +  ((cpk^2)/ (2 * dfCpk)))
        ciLbCpk <- cpk - (normCIrange * intervalCpk)
        ciUbCpk <- cpk + (normCIrange * intervalCpk)
      }
    }
    tableDfCurrentStage <- data.frame(cp = round(cp, .numDecimals),
                                      cpl = round(cpl, .numDecimals),
                                      cpu = round(cpu, .numDecimals),
                                      cpk = round(cpk, .numDecimals),
                                      zbench = round(zbench, .numDecimals))
    if (options[["processCapabilityTableCi"]]) {
      if (!(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])) {
        tableDfCurrentStage[["cplci"]] <- round(ciLbCp, .numDecimals)
        tableDfCurrentStage[["cpuci"]] <- round(ciUbCp, .numDecimals)
      }
      if (!(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])) {
        tableDfCurrentStage[["cpklci"]] <- round(ciLbCpk, .numDecimals)
        tableDfCurrentStage[["cpkuci"]] <- round(ciUbCpk, .numDecimals)
      }
    }
    if (i == 1 && nStages > 1)
      baseLineDf <- tableDfCurrentStage
    if (i > 1) {
      changeDf <- tableDfCurrentStage - baseLineDf
      changeDf <- round(changeDf, .numDecimals)
      changeDf$stage <- gettextf("Change (%s vs. BL)", stage)
      if (options[["processCapabilityTableCi"]]) {
        if (!(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])) {
          changeDf[["cplci"]] <- "-"
          changeDf[["cpuci"]] <- "-"
        }
        if (!(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])) {
          changeDf[["cpklci"]] <- "-"
          changeDf[["cpkuci"]] <- "-"
        }
      }
    }
    if (nStages > 1)
      tableDfCurrentStage$stage <- if (i == 1) gettextf("%s (BL)", stage) else as.character(stage)
    tableDf <- rbind(tableDf, tableDfCurrentStage)
    if (i > 1)
      tableDf <- rbind(tableDf, changeDf)
  }
  tableList <- as.list(tableDf)
  tableListAllNAIndices <- sapply(tableList, function(x)all(is.na(x)))
  tableListAllNANames <- names(tableListAllNAIndices)[tableListAllNAIndices]
  nStars <- if (nStages > 1) length(tableList[["stage"]]) else 1
  for (name in tableListAllNANames)
    tableList[[name]] <- rep("*", nStars) # This looks better in the table and makes clearer that there is not an error
  table$setData(tableList)

  if (returnDataframe) {
    tableNRows <- if (nStages > 1) nStages * 2 - 1 else 1
    formattedTableDf <- data.frame(matrix(nrow = tableNRows, ncol = 0))
    if (nStages > 1) {
      formattedStages <- tableList[["stage"]]
      changeNames <- formattedStages[grep("Change", formattedStages)]
      formattedChangeNames <- unlist(sapply(changeNames, function(x) strsplit(unlist(strsplit(x, "\\("))[2], "\\)"))) # removing the word "Change" and the brackets because it makes the table too big in the report
      formattedStages[grep("Change", formattedStages)] <- formattedChangeNames
      formattedTableDf[["stage"]] <- formattedStages
    }
    if (options[["lowerSpecificationLimit"]] && options[["upperSpecificationLimit"]]) {
      formattedTableDf[["cp"]] <- tableList[["cp"]]
      if (options[["processCapabilityTableCi"]] &&
          !(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]]))
        formattedTableDf[["cpci"]] <- paste0("[", tableList[["cplci"]], ", ", tableList[["cpuci"]], "]")
    }
    if (options[["lowerSpecificationLimit"]])
      formattedTableDf[["cpl"]] <- tableList[["cpl"]]
    if (options[["upperSpecificationLimit"]])
      formattedTableDf[["cpu"]] <- tableList[["cpu"]]
    formattedTableDf[["cpk"]] <- tableList[["cpk"]]
    if (options[["processCapabilityTableCi"]] &&
                !(options[["upperSpecificationLimitBoundary"]] && options[["lowerSpecificationLimitBoundary"]]))
        formattedTableDf[["cpkci"]] <- paste0("[", tableList[["cpklci"]], ", ", tableList[["cpkuci"]], "]")
    colnames(formattedTableDf) <- sourceVector
    return(formattedTableDf)
  }
  container[["capabilityTableWithin"]] <- table
}

.qcProcessCapabilityTableOverall <- function(options, dataset, ready, container, measurements, stages, returnOverallCapDataframe = FALSE,
                                             returnPerformanceDataframe = FALSE) {
  if (!ready)
    return()

  if (identical(stages, "")) {
    nStages <- 1
    dataset[["stage"]] <- 1
    stages <- "stage"
  } else if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
  }
  table <- createJaspTable(title = gettext("Process performance (total)"))
  sourceVector1 <- c()
  if (nStages > 1) {
    table$addColumnInfo(name = "stage",      	title = gettext("Stage"),  		type = "string")
    table$transpose <- TRUE
    table$addFootnote(gettext("Columns titled 'Change' concern changes of the respective stage in comparison to baseline (BL)."))
    sourceVector1 <- c(sourceVector1, "Stage")
  }
  tableColNames <- c()
  ciLevel <- options[["processCapabilityTableCiLevel"]]
  ciLevelPercent <- ciLevel * 100
  ciAlpha <- 1 - ciLevel

  if (options[["lowerSpecificationLimit"]] && options[["upperSpecificationLimit"]]) {
    table$addColumnInfo(name = "pp",  type = "integer", title = gettext("Pp"))
    sourceVector1 <- c(sourceVector1, 'Pp')
    tableColNames <- c(tableColNames, "pp")
    if (options[["processCapabilityTableCi"]] &&
        !(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])) {
      table$addColumnInfo(name = "pplci", title = gettext("Lower"), type = "integer", overtitle = gettextf("%s CI for Pp", paste(ciLevelPercent, "%")))
      table$addColumnInfo(name = "ppuci", title = gettext("Upper"), type = "integer", overtitle = gettextf("%s CI for Pp", paste(ciLevelPercent, "%")))
      tableColNames <- c(tableColNames, "pplci", "ppuci")
      sourceVector1 <- c(sourceVector1, paste0(ciLevelPercent, "% CI Pp"))
    }
  }
  if (options[["lowerSpecificationLimit"]]) {
    table$addColumnInfo(name = "ppl", type = "integer", title = gettext("PpL"))
    sourceVector1 <- c(sourceVector1, 'PpL')
    tableColNames <- c(tableColNames, "ppl")
  }
  if (options[["upperSpecificationLimit"]]) {
    table$addColumnInfo(name = "ppu", type = "integer", title = gettext("PpU"))
    sourceVector1 <- c(sourceVector1, 'PpU')
    tableColNames <- c(tableColNames, "ppu")
  }
  table$addColumnInfo(name = "ppk",   type = "integer", title = gettext("Ppk"))
  sourceVector1 <- c(sourceVector1, 'Ppk')
  tableColNames <- c(tableColNames, "ppk")
  if (options[["processCapabilityTableCi"]] &&
      !(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])) {
    table$addColumnInfo(name = "ppklci", title = gettext("Lower"), type = "integer", overtitle = gettextf("%s CI for Ppk", paste(ciLevelPercent, "%")))
    table$addColumnInfo(name = "ppkuci", title = gettext("Upper"), type = "integer", overtitle = gettextf("%s CI for Ppk", paste(ciLevelPercent, "%")))
    tableColNames <- c(tableColNames, "ppklci", "ppkuci")
    sourceVector1 <- c(sourceVector1,  paste0(ciLevelPercent, "% CI Ppk"))
  }
  if (options[["processCapabilityTableZbench"]]) {
    table$addColumnInfo(name="zbench", title=gettext("Z bench (LT)"), type="number")
  }
  if (options[["target"]]) {
    table$addColumnInfo(name = "cpm", type = "integer", title = gettext("Cpm"))
    sourceVector1 <- c(sourceVector1, 'Cpm')
    tableColNames <- c(tableColNames, "cpm")
    if (options[["processCapabilityTableCi"]] &&
        !(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])) {
      table$addColumnInfo(name = "cpmlci", title = gettext("Lower"), type = "integer", overtitle = gettextf("%s CI for Cpm", paste(ciLevelPercent, "%")))
      table$addColumnInfo(name = "cpmuci", title = gettext("Upper"), type = "integer", overtitle = gettextf("%s CI for Cpm", paste(ciLevelPercent, "%")))
      tableColNames <- c(tableColNames, "cpmlci", "cpmuci")
      sourceVector1 <- c(sourceVector1,  paste0(ciLevelPercent, "% CI Cpm"))
    }
  }
  table$showSpecifiedColumnsOnly <- TRUE
  if (options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])
    table$addFootnote(gettext("Statistics displayed as * were not calculated because the relevant specification limit is not set or set as boundary."))

  if (nStages > 1)
    tableColNames <- c("stage", tableColNames)
  tableDf <- data.frame(matrix(ncol = length(tableColNames), nrow = 0))
  colnames(tableDf) <- tableColNames
  for (i in seq_len(nStages)) {
    stage <- unique(dataset[[stages]])[i]
    dataCurrentStage <- dataset[which(dataset[[stages]] == stage), ][!names(dataset) %in% stages]

    if (length(measurements) < 2) {
      k <- options[["controlChartSdEstimationMethodMeanMovingRangeLength"]]
      sdw <- if (options[["historicalStdDev"]]) options[["historicalStdDevValue"]] else .controlChart_calculations(dataCurrentStage[measurements], plotType = "MR", movingRangeLength = k)$sd
    } else {
      sdType <- if (options[["controlChartSdEstimationMethodGroupSizeLargerThanOne"]] == "rBar") "r" else "s"
      unbiasingConstantUsed <- options[["controlChartSdUnbiasingConstant"]]
      sdw <- if (options[["historicalStdDev"]]) options[["historicalStdDevValue"]] else .sdXbar(dataCurrentStage[measurements], type = sdType, unbiasingConstantUsed = unbiasingConstantUsed)
    }
    allData <- na.omit(unlist(dataCurrentStage[, measurements]))
    sdo <- sd(allData)
    processMean <- if (options[["historicalMean"]]) options[["historicalMeanValue"]] else mean(allData, na.rm = TRUE)
    usl <- options[["upperSpecificationLimitValue"]]
    lsl <- options[["lowerSpecificationLimitValue"]]
    m <- (options[["upperSpecificationLimitValue"]] + options[["lowerSpecificationLimitValue"]])/2
    n <- length(allData)
    t <- options[["targetValue"]]
    tolMultiplier <- 6
    pp <- if (options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]]) NA else (usl - lsl) / (tolMultiplier * sdo)
    ppl <- if (options[["lowerSpecificationLimitBoundary"]]) NA else (processMean - lsl) / ((tolMultiplier/2) * sdo)
    ppu <- if (options[["upperSpecificationLimitBoundary"]]) NA else (usl - processMean) / ((tolMultiplier/2) * sdo)

    if (options[["lowerSpecificationLimit"]] && options[["upperSpecificationLimit"]]) {
      ppk <- if (options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]]) NA else min(ppu, ppl, na.rm = TRUE)
    } else if (options[["lowerSpecificationLimit"]] && !options[["upperSpecificationLimit"]]) {
      ppk <- ppl
    } else {
      ppk <- ppu
    }
    zbench <- 3*ppk
    cp <- (usl - lsl) / (tolMultiplier * sdw)

    if (options[["lowerSpecificationLimit"]] && options[["upperSpecificationLimit"]] && options[["target"]]) {
      if (t == m) {
        cpm <- (usl - lsl) / (tolMultiplier * sqrt((sum((allData - t)^2)) / n))
      } else {
        cpm <- min(c(t - lsl, usl - t)) / ((tolMultiplier / 2) * sqrt((sum((allData - t)^2)) / n))
      }
    } else if (options[["upperSpecificationLimit"]] && options[["target"]]) {
      cpm <- (usl - t) / ((tolMultiplier / 2) * sqrt((sum((allData - t)^2)) / n))
    } else if (options[["lowerSpecificationLimit"]] && options[["target"]]) {
      cpm <- (t - lsl) / ((tolMultiplier / 2) * sqrt((sum((allData - t)^2)) / n))
    }
    if (options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])
      cpm <- NA

    if (options[["processCapabilityTableCi"]]) {
      if (!(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])) {
        #CI for Pp
        dfPp <- n - 1
        ciLbPp <- pp * sqrt(qchisq(p = ciAlpha/2, df = dfPp) /dfPp)
        ciUbPp <- pp * sqrt(qchisq(p = 1 - (ciAlpha/2), df = dfPp) / dfPp)
      }
      if (!(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])) {
        #CI for Ppk
        dfPpk <- n - 1
        normCIrange <- qnorm(1 - (ciAlpha / 2))
        intervalPpk <- sqrt(1 / (((tolMultiplier / 2)^2) * n)  +  ((ppk^2)/ (2 * dfPpk)))
        ciLbPpk <- ppk - (normCIrange * intervalPpk)
        ciUbPpk <- ppk + (normCIrange * intervalPpk)
      }
      if (options[["target"]] &&
          !(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])){
        #CI for Cpm
        a <- (processMean - t) / sdo
        dfCpm <- (n * ((1 + (a^2))^2)) / (1 + (2 * (a^2)))
        ciLbCpm <- cpm * sqrt( qchisq(p = ciAlpha/2, df = dfCpm) /dfCpm)
        ciUbCpm <- cpm * sqrt(qchisq(p = 1 - (ciAlpha/2), df = dfCpm) / dfCpm)
      }
    }

    tableDfCurrentStage <- data.frame(pp = round(pp, .numDecimals),
                                      ppl = round(ppl, .numDecimals),
                                      ppu = round(ppu, .numDecimals),
                                      ppk = round(ppk, .numDecimals),
                                      zbench = round(zbench, .numDecimals))
    if (options[["target"]])
      tableDfCurrentStage[["cpm"]] <- round(cpm, .numDecimals)
    if (options[["processCapabilityTableCi"]]) {
      if (!(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])) {
        tableDfCurrentStage[["pplci"]] <- round(ciLbPp, .numDecimals)
        tableDfCurrentStage[["ppuci"]] <- round(ciUbPp, .numDecimals)
      }
      if (!(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])) {
        tableDfCurrentStage[["ppklci"]] <- round(ciLbPpk, .numDecimals)
        tableDfCurrentStage[["ppkuci"]] <- round(ciUbPpk, .numDecimals)
        if (options[["target"]]) {
          tableDfCurrentStage[["cpmlci"]] <- round(ciLbCpm, .numDecimals)
          tableDfCurrentStage[["cpmuci"]] <- round(ciUbCpm, .numDecimals)
        }
      }
    }

    if (i == 1 && nStages > 1)
      baseLineDf <- tableDfCurrentStage
    if (i > 1) {
      changeDf <- tableDfCurrentStage - baseLineDf
      changeDf <- round(changeDf, .numDecimals)
      changeDf$stage <- gettextf("Change (%s vs. BL)", stage)
      if (options[["processCapabilityTableCi"]]) {
        if (!(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])) {
          changeDf[["pplci"]] <- "-"
          changeDf[["ppuci"]] <- "-"
        }
        if (!(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])) {
          changeDf[["ppklci"]] <- "-"
          changeDf[["ppkuci"]] <- "-"
          if (options[["target"]]) {
            tableDfCurrentStage[["cpmlci"]] <- "-"
            tableDfCurrentStage[["cpmuci"]] <- "-"
          }
        }
      }
    }
    if (nStages > 1)
      tableDfCurrentStage$stage <- if (i == 1) gettextf("%s (BL)", stage) else as.character(stage)
    tableDf <- rbind(tableDf, tableDfCurrentStage)
    if (i > 1)
      tableDf <- rbind(tableDf, changeDf)
  }
  tableList <- as.list(tableDf)
  tableListAllNAIndices <- sapply(tableList, function(x)all(is.na(x)))
  tableListAllNANames <- names(tableListAllNAIndices)[tableListAllNAIndices]
  nStars <- if (nStages > 1) length(tableList[["stage"]]) else 1
  for (name in tableListAllNANames)
    tableList[[name]] <- rep("*", nStars) # This looks better in the table and makes clearer that there is not an error
  table$setData(tableList)

  if (returnOverallCapDataframe) {
    tableNRows <- if (nStages > 1) nStages * 2 - 1 else 1
    formattedTableDf <- data.frame(matrix(nrow = tableNRows, ncol = 0))
    if (nStages > 1) {
      formattedStages <- tableList[["stage"]]
      changeNames <- formattedStages[grep("Change", formattedStages)]
      formattedChangeNames <- unlist(sapply(changeNames, function(x) strsplit(unlist(strsplit(x, "\\("))[2], "\\)"))) # removing the word "Change" and the brackets because it makes the table too big in the report
      formattedStages[grep("Change", formattedStages)] <- formattedChangeNames
      formattedTableDf[["stage"]] <- formattedStages
    }
    if (options[["lowerSpecificationLimit"]] && options[["upperSpecificationLimit"]]) {
      formattedTableDf[["pp"]] <- tableList[["pp"]]
      if (options[["processCapabilityTableCi"]] &&
          !(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]]))
        formattedTableDf[["ppci"]] <- paste0("[", tableList[["pplci"]], ", ",tableList[["ppuci"]], "]")
    }
    if (options[["lowerSpecificationLimit"]])
      formattedTableDf[["ppl"]] <- tableList[["ppl"]]
    if (options[["upperSpecificationLimit"]])
      formattedTableDf[["ppu"]] <- tableList[["ppu"]]
    formattedTableDf[["ppk"]] <- tableList[["ppk"]]
    if (options[["processCapabilityTableCi"]] &&
        !(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]]))
      formattedTableDf[["ppkci"]] <- paste0("[", tableList[["ppklci"]], ", ",tableList[["ppkuci"]], "]")
    if (options[["target"]]) {
      formattedTableDf[["cpm"]] <- tableList[["cpm"]]
      if (options[["processCapabilityTableCi"]] &&
          !(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]]))
        formattedTableDf[["cpmci"]] <- paste0("[", tableList[["cpmlci"]], ", ",tableList[["cpmuci"]], "]")
    }
    colnames(formattedTableDf) <- sourceVector1
    return(formattedTableDf)
  }

  table2 <- createJaspTable(title = gettext("Non-conformance statistics"))
  table2$addColumnInfo(name = "rowNames", type = "string", title = "")
  table2$showSpecifiedColumnsOnly <- TRUE
  if (nStages > 1) {
    table2$transpose <- TRUE
    table2$addFootnote(gettext("Rows titled 'Change' concern changes of the respective stage in comparison to baseline (BL)."))
  }
  if (options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])
    table2$addFootnote(gettext("Statistics displayed as * were not calculated because the relevant specification limit is not set or set as boundary."))

  nDecimals <- .numDecimals
  tableList2 <- list()
  tableNCols <- if (nStages > 1) 5 else 4
  tableDf2 <- data.frame(matrix(nrow = 0, ncol = tableNCols))
  for (i in seq_len(nStages)) {
    stage <- unique(dataset[[stages]])[i]
    stageTitle <- if (i == 1) gettextf("%s (BL)", stage) else as.character(stage)
    colOvertitle <- if (nStages == 1) "" else stageTitle
    observedColName <- paste0("observed", stage)
    expOverallColName <- paste0("expOverall", stage)
    expWithinColName <- paste0("expWithin", stage)
    table2$addColumnInfo(name = observedColName, type = "integer", title = "Observed", overtitle = colOvertitle)
    table2$addColumnInfo(name = expOverallColName, type = "integer", title = "Expected total", overtitle = colOvertitle)
    if (.qcWithinProcessValid(options))
      table2$addColumnInfo(name = expWithinColName, type = "integer", title = "Expected within", overtitle = colOvertitle)

    # Table columns for comparison
    if (i > 1) {
      colOvertitle2 <- gettextf("Change (%s vs. BL)", stage)
      observedComparisonColName <- paste0("observedComparison", stage)
      expOverallComparisonColName <- paste0("expOverallComparison", stage)
      expWithinComparisonColName <- paste0("expWithinComparison", stage)
      table2$addColumnInfo(name = observedComparisonColName, type = "integer", title = "Observed", overtitle = colOvertitle2)
      table2$addColumnInfo(name = expOverallComparisonColName, type = "integer", title = "Expected total", overtitle = colOvertitle2)
      table2$addColumnInfo(name = expWithinComparisonColName, type = "integer", title = "Expected within", overtitle = colOvertitle2)
    }

    # Calculate performance
    stage <- unique(dataset[[stages]])[i]
    dataCurrentStage <- dataset[which(dataset[[stages]] == stage), ][!names(dataset) %in% stages]
    allData <- na.omit(unlist(dataCurrentStage[, measurements]))
    allDataVector <- as.vector(allData)
    lslTitle <- if (options[["lowerSpecificationLimitBoundary"]]) gettext("LB") else gettext("LSL")
    uslTitle <- if (options[["upperSpecificationLimitBoundary"]]) gettext("UB") else gettext("USL")
    rowNames <- c(sprintf("ppm < %s", lslTitle), sprintf("ppm > %s", uslTitle), "ppm total")
    sdo <- sd(allData)
    processMean <- if (options[["historicalMean"]]) options[["historicalMeanValue"]] else mean(allData, na.rm = TRUE)
    if (length(measurements) < 2) {
      k <- options[["controlChartSdEstimationMethodMeanMovingRangeLength"]]
      sdw <- if (options[["historicalStdDev"]]) options[["historicalStdDevValue"]] else .controlChart_calculations(dataCurrentStage[measurements], plotType = "MR", movingRangeLength = k)$sd
    } else {
      sdType <- if (options[["controlChartSdEstimationMethodGroupSizeLargerThanOne"]] == "rBar") "r" else "s"
      unbiasingConstantUsed <- options[["controlChartSdUnbiasingConstant"]]
      sdw <- if (options[["historicalStdDev"]]) options[["historicalStdDevValue"]] else .sdXbar(dataCurrentStage[measurements], type = sdType, unbiasingConstantUsed = unbiasingConstantUsed)
    }

    # observed
    if (options[["lowerSpecificationLimit"]]) {
      oLSL <- (1e6*length(allDataVector[allDataVector < lsl])) / n
    } else {
      oLSL <- NA
    }
    if (options[["upperSpecificationLimit"]]) {
      oUSL <- (1e6*length(allDataVector[allDataVector > usl])) / n
    } else {
      oUSL <- NA
    }
    oTOT <- if (all(is.na(c(oLSL, oUSL)))) NA else sum(c(oLSL, oUSL), na.rm = TRUE)
    observed <- c(oLSL, oUSL, oTOT)

    # expected total
    if (options[["lowerSpecificationLimit"]] && !options[["lowerSpecificationLimitBoundary"]]) {
      eoLSL <- 1e6 * (1 - pnorm((processMean - lsl)/sdo))
    } else {
      eoLSL <- NA
    }
    if (options[["upperSpecificationLimit"]] && !options[["upperSpecificationLimitBoundary"]]) {
      eoUSL <- 1e6 * (1 - pnorm((usl - processMean)/sdo))
    } else {
      eoUSL <- NA
    }
    eoTOT <- if (all(is.na(c(eoLSL, eoUSL)))) NA else sum(c(eoLSL, eoUSL), na.rm = TRUE)
    expOverall <- c(eoLSL, eoUSL, eoTOT)

    # expected within
    if (options[["lowerSpecificationLimit"]] && !options[["lowerSpecificationLimitBoundary"]]) {
      ewLSL <- 1e6 * (1 - pnorm((processMean - lsl)/sdw))
    } else {
      ewLSL <- NA
    }
    if (options[["upperSpecificationLimit"]] && !options[["upperSpecificationLimitBoundary"]]) {
      ewUSL <- 1e6 * (1 - pnorm((usl - processMean)/sdw))
    } else {
      ewUSL <- NA
    }
    ewTOT <- if (all(is.na(c(ewLSL, ewUSL)))) NA else sum(c(ewLSL, ewUSL), na.rm = TRUE)
    expWithin <- c(ewLSL, ewUSL, ewTOT)

    if (i == 1 && nStages > 1) {
      observedBaseline <- observed
      expOverallBaseline <- expOverall
      expWithinBaseline <- expWithin
    }

    if (nStages > 1) {
      currentTableDf1 <- data.frame("Stage" = c(stageTitle, "", ""),
                                    "Source" = c("ppm < LSL", "ppm > USL", "ppm total"),
                                    "Observed" = .pcTableFormatNumbers(observed),
                                    "Expected Overall" = .pcTableFormatNumbers(expOverall),
                                    "Expected Within" = .pcTableFormatNumbers(expWithin))

    } else {
      currentTableDf1 <- data.frame("Source" = c("ppm < LSL", "ppm > USL", "ppm total"),
                                    "Observed" = .pcTableFormatNumbers(observed),
                                    "Expected Overall" =  .pcTableFormatNumbers(expOverall),
                                    "Expected Within" =  .pcTableFormatNumbers(expWithin))
    }

    tableDf2 <- rbind(tableDf2, currentTableDf1)

    if (i > 1) {
      observedComparison <- observed - observedBaseline
      expOverallComparison <- expOverall - expOverallBaseline
      expWithinComparison <- expWithin - expWithinBaseline
      tableList2[[observedComparisonColName]] <- round(observedComparison, nDecimals)
      tableList2[[expOverallComparisonColName]] <- round(expOverallComparison, nDecimals)
      tableList2[[expWithinComparisonColName]] <- round(expWithinComparison, nDecimals)
      tableList2[[expOverallComparisonColName]][is.na(tableList2[[expOverallComparisonColName]])] <- "*" # This looks better in the table and makes clearer that there is not an error
      tableList2[[expWithinComparisonColName]][is.na(tableList2[[expWithinComparisonColName]])] <- "*"
      tableList2[[observedComparisonColName]][is.na(tableList2[[observedComparisonColName]])] <- "*"

      currentTableDf2 <- data.frame("Stage" = c(gettextf("%s vs. BL", stage),"", "") ,
                                    "Source" = c("ppm < LSL", "ppm > USL", "ppm total"),
                                    "Observed" = .pcTableFormatNumbers(observedComparison),
                                    "Expected Overall" = .pcTableFormatNumbers(expOverallComparison),
                                    "Expected Within" = .pcTableFormatNumbers(expWithinComparison))
      tableDf2 <- rbind(tableDf2, currentTableDf2)


    }
    tableList2[[observedColName]] <- round(observed, nDecimals)
    tableList2[[expOverallColName]] <- round(expOverall, nDecimals)
    tableList2[[expWithinColName]] <- round(expWithin, nDecimals)
    tableList2[[expOverallColName]][is.na(tableList2[[expOverallColName]])] <- "*" # This looks better in the table and makes clearer that there is not an error
    tableList2[[expWithinColName]][is.na(tableList2[[expWithinColName]])] <- "*"
    tableList2[[observedColName]][is.na(tableList2[[observedColName]])] <- "*"
  }


  if (returnPerformanceDataframe) {
    tableDf2ColNames <- if (nStages > 1) c("Stage", "Source", "Observed", "Expected Overall", "Expected Within") else c("Source", "Observed", "Expected Overall", "Expected Within")
    colnames(tableDf2) <- tableDf2ColNames
    tableDf2$`Expected Overall`[is.na(tableDf2$`Expected Overall`)] <- "*" # This looks better in the table and makes clearer that there is not an error
    tableDf2$`Expected Within`[is.na(tableDf2$`Expected Within`)] <- "*"
    tableDf2$Observed[is.na(tableDf2$Observed)] <- "*"

    if (!.qcWithinProcessValid(options)) tableDf2$`Expected Within` <- "*"
    return(tableDf2)
  }

  tableList2[["rowNames"]] <- rowNames
  table2$setData(tableList2)

  container[["capabilityTableOverall"]] <- table
  container[["capabilityTablePerformance"]] <- table2
}

.qcProcessCapabilityTableNonNormal <- function(options, dataset, ready, container, measurements, stages,
                                               returnSummaryDF = FALSE, returnCapabilityDF = FALSE, returnPerformanceDF = FALSE) {
  if (!ready)
    return()

  nDecimals <- .numDecimals

  if (identical(stages, "")) {
    nStages <- 1
    dataset[["stage"]] <- 1
    stages <- "stage"
  } else if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
  }
  table <- createJaspTable(title = gettextf("Process summary"))
  if (nStages > 1) {
    table$addColumnInfo(name = "stage",      	title = gettext("Stage"),  		type = "string")
    table$transpose <- TRUE
    table$addFootnote(gettext("Columns titled 'Change' concern changes of the respective stage in comparison to baseline (BL)."))
  }

  if (((options[["nullDistribution"]] == "lognormal") || options[["nullDistribution"]] == "weibull") &&
      any(na.omit(unlist(dataset[measurements])) < 0)) {
    table$setError(gettext("Dataset contains negative numbers. Not compatible with the selected distribution."))
    container[["summaryTableNonNormal"]] <- table
    return()
  }
  if(!options[["upperSpecificationLimit"]] && !options[["lowerSpecificationLimit"]]){
    table$setError(gettext("No specification limits set."))
    container[["summaryTableNonNormal"]] <- table
    return()
  }

  sourceVector1 <- c()

  lslTitle <- if (options[["lowerSpecificationLimitBoundary"]]) gettext("LB") else gettext("LSL")
  if (options[["lowerSpecificationLimit"]])
    table$addColumnInfo(name = "lsl", type = "integer", title = lslTitle)
  if (options[["target"]])
    table$addColumnInfo(name = "target", type = "integer", title = gettext("Target"))
  uslTitle <- if (options[["upperSpecificationLimitBoundary"]]) gettext("UB") else gettext("USL")
  if (options[["upperSpecificationLimit"]])
    table$addColumnInfo(name = "usl", type = "integer", title = uslTitle)
  table$addColumnInfo(name = "n", type = "integer", title = gettext("Sample size"))
  table$addColumnInfo(name = "mean", type = "number", title = gettext("Mean"))
  table$addColumnInfo(name = "sd", type = "number", title = gettext("Std. dev."))
  if (options[["nonNormalDistribution"]] == "3ParameterLognormal" | options[["nonNormalDistribution"]] == "lognormal") {
    table$addColumnInfo(name = "beta", type = "number", title = gettextf("Log mean (%1$s)", "\u03BC"))
    table$addColumnInfo(name = "theta", type = "number", title = gettextf("Log std.dev (%1$s)", "\u03C3"))
  } else {
    table$addColumnInfo(name = "beta", type = "number", title = gettextf("Shape (%1$s)", "\u03B2"))
    table$addColumnInfo(name = "theta", type = "number", title = gettextf("Scale (%1$s)", "\u03B8"))
  }
  sourceVector1 <- c(sourceVector1, lslTitle, "Target", uslTitle, "N", "Mean", "Std. dev.", "Beta", "Theta")

  if (options[["nonNormalDistribution"]] == "3ParameterLognormal" | options[["nonNormalDistribution"]] == "3ParameterWeibull") {
    table$addColumnInfo(name = "threshold", type = "number", title = gettext('Threshold'))
    sourceVector1 <- c(sourceVector1, 'Threshold')
  }
  table$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  tableColNames <- tolower(sourceVector1)
  if (nStages > 1)
    tableColNames <- c("stage", tableColNames)
  tableDf <- data.frame(matrix(ncol = length(tableColNames), nrow = 0))
  colnames(tableDf) <- tableColNames



  ## estimate parameters
  distParameters <- list()
  for (i in seq_len(nStages)) {
    stage <- unique(dataset[[stages]])[i]
    dataCurrentStage <- dataset[which(dataset[[stages]] == stage), ][!names(dataset) %in% stages]
    allData <- as.vector(na.omit(unlist(dataCurrentStage[, measurements])))
    distParameters[[i]] <- try(.distributionParameters(data = allData, distribution = options[["nonNormalDistribution"]]))
    if (jaspBase::isTryError(distParameters[[i]])) {
      table$setError(distParameters[[i]][1])
      container[["summaryTableNonNormal"]] <- table
      return()
    }
  }

  for (i in seq_len(nStages)) {
    stage <- unique(dataset[[stages]])[i]
    dataCurrentStage <- dataset[which(dataset[[stages]] == stage), ][!names(dataset) %in% stages]
    allData <- as.vector(na.omit(unlist(dataCurrentStage[, measurements])))
    n <- length(allData)
    lsl <- round(options[["lowerSpecificationLimitValue"]], .numDecimals)
    usl <- round(options[["upperSpecificationLimitValue"]], .numDecimals)
    target <- round(options[["targetValue"]], .numDecimals)
    sd <- sd(allData)
    processMean <- if (options[["historicalMean"]]) options[["historicalMeanValue"]] else mean(allData, na.rm = TRUE)
    beta <- distParameters[[i]]$beta
    theta <- distParameters[[i]]$theta

    tableDfCurrentStage <- data.frame("n" = n,
                                      "mean" = processMean,
                                      "sd" = sd,
                                      "lsl" = lsl,
                                      "usl" = usl,
                                      "target" = target,
                                      "beta" = beta,
                                      "theta" = theta)

    if (options[["nonNormalDistribution"]] == "3ParameterLognormal" | options[["nonNormalDistribution"]] == "3ParameterWeibull") {
      threshold <- distParameters[[i]]$threshold
      tableDfCurrentStage[['threshold']] <- threshold
    }
    if (i == 1 && nStages > 1)
      baseLineDf <- tableDfCurrentStage
    if (i > 1) {
      changeDf <- tableDfCurrentStage - baseLineDf
      changeDf$stage <- gettextf("Change (%s vs. BL)", stage)
      # Differences in specification limits are not relevant
      changeDf$lsl <- "-"
      changeDf$target <- "-"
      changeDf$usl <- "-"
    }
    if (nStages > 1)
      tableDfCurrentStage$stage <- if (i == 1) gettextf("%s (BL)", stage) else as.character(stage)
    tableDf <- rbind(tableDf, tableDfCurrentStage)
    if (i > 1)
      tableDf <- rbind(tableDf, changeDf)
  }
  tableList <- as.list(tableDf)
  table$setData(tableList)

  if (returnSummaryDF) {
    if (nStages > 1)
      sourceVector1 <- c("Stage", sourceVector1)
    tableNRows <- if (nStages > 1) nStages * 2 - 1 else 1
    formattedTableDf1 <- data.frame(matrix(nrow = tableNRows, ncol = 0))
    if (nStages > 1) {
      formattedStages <- tableList[["stage"]]
      changeNames <- formattedStages[grep("Change", formattedStages)]
      formattedChangeNames <- unlist(sapply(changeNames, function(x) strsplit(unlist(strsplit(x, "\\("))[2], "\\)"))) # removing the word "Change" and the brackets because it makes the table too big in the report
      formattedStages[grep("Change", formattedStages)] <- formattedChangeNames
      formattedTableDf1[["stage"]] <- formattedStages
    }
    if (!options[["lowerSpecificationLimit"]])
      lsl <- '*'
    if (!options[["target"]])
      target <- '*'
    if (!options[["upperSpecificationLimit"]])
      usl <- '*'
    formattedTableDf1[["lsl"]] <- tableList[["lsl"]]
    formattedTableDf1[["target"]] <- tableList[["target"]]
    formattedTableDf1[["usl"]] <- tableList[["usl"]]
    formattedTableDf1[["n"]] <- tableList[["n"]]
    formattedTableDf1[["mean"]] <- round(tableList[["mean"]], nDecimals)
    formattedTableDf1[["sd"]] <- round(tableList[["sd"]], nDecimals)
    formattedTableDf1[["beta"]] <- round(tableList[["beta"]], nDecimals)
    formattedTableDf1[["theta"]] <- round(tableList[["theta"]], nDecimals)
    if(options[["nonNormalDistribution"]] == "3ParameterLognormal" | options[["nonNormalDistribution"]] == "3ParameterWeibull")
      formattedTableDf1[["threshold"]] <- round(tableList[["threshold"]], nDecimals)
    colnames(formattedTableDf1) <- sourceVector1
    return(formattedTableDf1)
  }

  table2 <- createJaspTable(title = gettextf("Process performance (total)"))
  table2$showSpecifiedColumnsOnly <- TRUE
  sourceVector2 <- c()
  if (nStages > 1) {
    table2$addColumnInfo(name = "stage",      	title = gettext("Stage"),  		type = "string")
    table2$transpose <- TRUE
    table2$addFootnote(gettext("Columns titled 'Change' concern changes of the respective stage in comparison to baseline (BL)."))
    sourceVector2 <- c("Stage")
  }

  if (options[["upperSpecificationLimit"]] && options[["lowerSpecificationLimit"]]) {
    table2$addColumnInfo(name = "pp", type = "integer", title = gettext("Pp"))
    sourceVector2 <- c(sourceVector2, 'Pp')
  }
  if (options[["lowerSpecificationLimit"]]) {
    table2$addColumnInfo(name = "ppl", type = "integer", title = gettext("PpL"))
    sourceVector2 <- c(sourceVector2, 'PpL')
  }
  if (options[["upperSpecificationLimit"]]) {
    table2$addColumnInfo(name = "ppu", type = "integer", title = gettext("PpU"))
    sourceVector2 <- c(sourceVector2, 'PpU')
  }
  table2$addColumnInfo(name = "ppk", type = "integer", title = gettext("Ppk"))
  sourceVector2 <- c(sourceVector2, 'Ppk')
  tableColNames2 <- tolower(sourceVector2)

  if (nStages > 1)
    tableColNames2 <- c("stage", tableColNames2)
  tableDf2 <- data.frame(matrix(ncol = length(tableColNames2), nrow = 0))
  colnames(tableDf2) <- tableColNames2

  for (i in seq_len(nStages)) {
    tableDfCurrentStage2 <- data.frame(matrix(ncol = 0, nrow = 1))
    stage <- unique(dataset[[stages]])[i]
    dataCurrentStage <- dataset[which(dataset[[stages]] == stage), ][!names(dataset) %in% stages]
    allData <- as.vector(na.omit(unlist(dataCurrentStage[, measurements])))
    beta <- distParameters[[i]]$beta
    theta <- distParameters[[i]]$theta
    if (options[["nonNormalDistribution"]] == "3ParameterLognormal" | options[["nonNormalDistribution"]] == "3ParameterWeibull")
      threshold <- distParameters[[i]]$threshold

    if (options[["nonNormalDistribution"]] == "lognormal") {
      if (options[["lowerSpecificationLimit"]] && !options[["lowerSpecificationLimitBoundary"]]) {
        if (options[['nonNormalMethod' ]] == "nonConformance") {
          p1 <- plnorm(q = lsl, meanlog = beta, sdlog = theta, lower.tail = TRUE)
          zLSL <- qnorm(p1)
          ppl <- -zLSL/3
        } else {
          x135 <- qlnorm(p = 0.00135, meanlog = beta, sdlog = theta)
          x05 <- qlnorm(p = 0.5, meanlog = beta, sdlog = theta)
          ppl <- (x05 - lsl) / (x05 - x135)
        }
      } else {
        ppl <- NA
      }
      tableDfCurrentStage2[["ppl"]] <- round(ppl, .numDecimals)
      if (options[["upperSpecificationLimit"]]  && !options[["upperSpecificationLimitBoundary"]]) {
        if (options[['nonNormalMethod' ]] == "nonConformance") {
          p2 <- plnorm(q = usl,  meanlog = beta, sdlog = theta)
          zUSL <- qnorm(p2)
          ppu <- zUSL/3
        } else {
          x99 <- qlnorm(p = 0.99865, meanlog = beta, sdlog = theta)
          x05 <- qlnorm(p = 0.5, meanlog = beta, sdlog = theta)
          ppu <- (usl - x05) / (x99 - x05)
        }
      } else {
        ppu <- NA
      }
      tableDfCurrentStage2[["ppu"]] <- round(ppu, .numDecimals)
    } else if (options[["nonNormalDistribution"]] == "weibull") {
      if (options[["lowerSpecificationLimit"]]  && !options[["lowerSpecificationLimitBoundary"]]) {
        if (options[['nonNormalMethod' ]] == "nonConformance") {
          p1 <- pweibull(q = lsl, shape = beta, scale = theta, lower.tail = TRUE)
          zLSL <- qnorm(p1)
          ppl <- -zLSL/3
        } else {
          x135 <- qweibull(p = 0.00135, shape = beta, scale = theta)
          x05 <- qweibull(p = 0.5, shape = beta, scale = theta)
          ppl <- (x05 - lsl) / (x05 - x135)
        }

      } else {
        ppl <- NA
      }
      tableDfCurrentStage2[["ppl"]] <- round(ppl, .numDecimals)
      if (options[["upperSpecificationLimit"]] && !options[["upperSpecificationLimitBoundary"]]) {
        if (options[['nonNormalMethod' ]] == "nonConformance") {
          p2 <- pweibull(q = usl, shape = beta, scale = theta)
          zUSL <- qnorm(p2)
          ppu <- zUSL/3
        } else {
          x99 <- qweibull(p = 0.99865, shape = beta, scale = theta)
          x05 <- qweibull(p = 0.5, shape = beta, scale = theta)
          ppu <- (usl - x05) / (x99 - x05)
        }
      } else {
        ppu <- NA
      }
      tableDfCurrentStage2[["ppu"]] <- round(ppu, .numDecimals)
    } else if (options[["nonNormalDistribution"]] == "3ParameterLognormal") {
      if (options[["lowerSpecificationLimit"]]  && !options[["lowerSpecificationLimitBoundary"]]) {
        if(options[['nonNormalMethod' ]] == "nonConformance") {
          p1 <- FAdist::plnorm3(q = lsl, shape = theta, scale = beta, thres = threshold, lower.tail = TRUE)
          zLSL <- qnorm(p1)
          ppl <- -zLSL/3
        } else {
          x135 <- FAdist::qlnorm3(p = 0.00135, shape = theta, scale = beta, thres = threshold)
          x05 <- FAdist::qlnorm3(p = 0.5, shape = theta, scale = beta, thres = threshold)
          ppl <- (x05 - lsl) / (x05 - x135)
        }
      } else {
        ppl <- NA
      }
      tableDfCurrentStage2[["ppl"]] <- round(ppl, .numDecimals)
      if (options[["upperSpecificationLimit"]] && !options[["upperSpecificationLimitBoundary"]]) {
        if(options[['nonNormalMethod' ]] == "nonConformance"){
          p2 <- FAdist::plnorm3(q = usl, shape = theta, scale = beta, thres = threshold)
          zUSL <- qnorm(p2)
          ppu <- zUSL/3
        } else {
          x99 <- FAdist::qlnorm3(p = 0.99865, shape = theta, scale = beta, thres = threshold)
          x05 <- FAdist::qlnorm3(p = 0.5, shape = theta, scale = beta, thres = threshold)
          ppu <- (usl - x05) / (x99 - x05)
        }
      } else {
        ppu <- NA
      }
      tableDfCurrentStage2[["ppu"]] <- round(ppu, .numDecimals)
    } else if (options[["nonNormalDistribution"]] == "3ParameterWeibull") {
      if (options[["lowerSpecificationLimit"]]  && !options[["lowerSpecificationLimitBoundary"]]){
        if (options[['nonNormalMethod' ]] == "nonConformance") {
          p1 <- FAdist::pweibull3(q = lsl, shape = beta, scale = theta, thres = threshold, lower.tail = TRUE)
          zLSL <- qnorm(p1)
          ppl <- -zLSL/3
        } else {
          x135 <- FAdist::qweibull3(p = 0.00135, shape = beta, scale = theta, thres = threshold)
          x05 <- FAdist::qweibull3(p = 0.5, shape = beta, scale = theta, thres = threshold)
          ppl <- (x05 - lsl) / (x05 - x135)
        }
      } else {
        ppl <- NA
      }
      tableDfCurrentStage2[["ppl"]] <- round(ppl, .numDecimals)
      if (options[["upperSpecificationLimit"]]  && !options[["upperSpecificationLimitBoundary"]]) {
        if (options[['nonNormalMethod' ]] == "nonConformance") {
          p2 <- FAdist::pweibull3(q = usl, shape = beta, scale = theta, thres = threshold)
          zUSL <- qnorm(p2)
          ppu <- zUSL/3
        } else {
          x99 <- FAdist::qweibull3(p = 0.99865, shape = beta, scale = theta, thres = threshold)
          x05 <- FAdist::qweibull3(p = 0.5, shape = beta, scale = theta, thres = threshold)
          ppu <- (usl - x05) / (x99 - x05)
        }
      } else {
        ppu <- NA
      }
    }
    tableDfCurrentStage2[["ppu"]] <- round(ppu, .numDecimals)
    if (options[["upperSpecificationLimit"]] && options[["lowerSpecificationLimit"]] &&
        !(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])) {
      if (options[['nonNormalMethod' ]] == "nonConformance") {
        pp <- (zUSL - zLSL)/6
      } else {
        pp <- (usl - lsl) / (x99 - x135)
      }
    } else {
      pp <- NA
    }
    tableDfCurrentStage2[["pp"]] <- round(pp, .numDecimals)
    ppk <- if (all(is.na(c(ppl, ppu)))) NA else min(c(ppl, ppu), na.rm = T)
    tableDfCurrentStage2[["ppk"]] <- round(ppk, .numDecimals)

    if (i == 1 && nStages > 1)
      baseLineDf2 <- tableDfCurrentStage2
    if (i > 1) {
      changeDf2 <- tableDfCurrentStage2 - baseLineDf2
      changeDf2 <- round(changeDf2, .numDecimals)
      changeDf2$stage <- gettextf("Change (%s vs. BL)", stage)
    }
    if (nStages > 1)
      tableDfCurrentStage2$stage <- if (i == 1) gettextf("%s (BL)", stage) else as.character(stage)
    tableDf2 <- rbind(tableDf2, tableDfCurrentStage2)
    if (i > 1)
      tableDf2 <- rbind(tableDf2, changeDf2)
  }
  tableList2 <- as.list(tableDf2)
  tableListAllNAIndices <- sapply(tableList2, function(x)all(is.na(x)))
  tableListAllNANames <- names(tableListAllNAIndices)[tableListAllNAIndices]
  nStars <- if (nStages > 1) length(tableList2[["stage"]]) else 1
  for (name in tableListAllNANames)
    tableList2[[name]] <- rep("*", nStars) # This looks better in the table and makes clearer that there is not an error
  table2$setData(tableList2)

  if (returnCapabilityDF) {
    tableNRows <- if (nStages > 1) nStages * 2 - 1 else 1
    formattedTableDf2 <- data.frame(matrix(nrow = tableNRows, ncol = 0))
    if (nStages > 1) {
      formattedStages <- tableList2[["stage"]]
      changeNames <- formattedStages[grep("Change", formattedStages)]
      formattedChangeNames <- unlist(sapply(changeNames, function(x) strsplit(unlist(strsplit(x, "\\("))[2], "\\)"))) # removing the word "Change" and the brackets because it makes the table too big in the report
      formattedStages[grep("Change", formattedStages)] <- formattedChangeNames
      formattedTableDf2[["stage"]] <- formattedStages
    }
    if (options[["lowerSpecificationLimit"]] && options[["upperSpecificationLimit"]])
      formattedTableDf2[["pp"]] <- tableList2[["pp"]]
    if (options[["lowerSpecificationLimit"]])
      formattedTableDf2[["ppl"]] <- tableList2[["ppl"]]
    if (options[["upperSpecificationLimit"]])
      formattedTableDf2[["ppu"]] <- tableList2[["ppu"]]
    formattedTableDf2[["ppk"]] <- tableList2[["ppk"]]
    colnames(formattedTableDf2) <- sourceVector2
    return(formattedTableDf2)
  }

  table3 <- createJaspTable(title = gettextf("Non-conformance statistics"))
  table3$addColumnInfo(name = "rowNames", type = "string", title = "")
  if (nStages > 1) {
    table3$transpose <- TRUE
    table3$addFootnote(gettext("Rows titled 'Change' concern changes of the respective stage in comparison to baseline (BL)."))
  }
  tableList3 <- list()
  rowNames <- c(paste0("ppm < ", lslTitle), paste0("ppm > ", uslTitle), "Total ppm")
  tableNCols <- if (nStages > 1) 4 else 3
  tableDf3 <- data.frame(matrix(nrow = 0, ncol = tableNCols))
  for (i in seq_len(nStages)) {
    stage <- unique(dataset[[stages]])[i]
    stageTitle <- if (i == 1) gettextf("%s (BL)", stage) else as.character(stage)
    colOvertitle <- if (nStages == 1) "" else stageTitle
    observedColName <- paste0("observed", stage)
    expOverallColName <- paste0("expOverall", stage)
    table3$addColumnInfo(name = observedColName, type = "integer", title = "Observed", overtitle = colOvertitle)
    table3$addColumnInfo(name = expOverallColName, type = "integer", title = "Expected total", overtitle = colOvertitle)

    # Table columns for comparison
    if (i > 1) {
      colOvertitle <- gettextf("Change (%s vs. BL)", stage)
      observedComparisonColName <- paste0("observedComparison", stage)
      expOverallComparisonColName <- paste0("expOverallComparison", stage)
      table3$addColumnInfo(name = observedComparisonColName, type = "integer", title = "Observed", overtitle = colOvertitle)
      table3$addColumnInfo(name = expOverallComparisonColName, type = "integer", title = "Expected total", overtitle = colOvertitle)
    }

    #Calculate performance
    stage <- unique(dataset[[stages]])[i]
    dataCurrentStage <- dataset[which(dataset[[stages]] == stage), ][!names(dataset) %in% stages]
    allData <- na.omit(unlist(dataCurrentStage[, measurements]))
    allDataVector <- as.vector(allData)
    beta <- distParameters[[i]]$beta
    theta <- distParameters[[i]]$theta
    if (options[["nonNormalDistribution"]] == "3ParameterLognormal" | options[["nonNormalDistribution"]] == "3ParameterWeibull")
      threshold <- distParameters[[i]]$threshold

    #observed
    if (options[["lowerSpecificationLimit"]]){
      oLSL <- (1e6*length(allDataVector[allDataVector < lsl])) / n
    } else {
      oLSL <- NA
    }
    if (options[["upperSpecificationLimit"]]) {
      oUSL <- (1e6*length(allDataVector[allDataVector > usl])) / n
    } else {
      oUSL <- NA
    }
    oTOT <- sum(c(oLSL, oUSL), na.rm = TRUE)
    observed <- c(oLSL, oUSL, oTOT)

    # expected total
    if (options[["nonNormalDistribution"]] == "lognormal") {
      distname <- "lognormal"
      if (options[["lowerSpecificationLimit"]] && !options[["lowerSpecificationLimitBoundary"]]) {
        eoLSL <- 1e6 * plnorm(q = lsl, meanlog = beta, sdlog = theta, lower.tail = TRUE)
      } else {
        eoLSL <- NA
      }
      if (options[["upperSpecificationLimit"]] && !options[["upperSpecificationLimitBoundary"]]) {
        eoUSL <- 1e6 * (1 - plnorm(q = usl, meanlog = beta, sdlog = theta))
      } else {
        eoUSL <- NA
      }
    } else if (options[["nonNormalDistribution"]] == "weibull") {
      distname <- "Weibull"
      if (options[["lowerSpecificationLimit"]] && !options[["lowerSpecificationLimitBoundary"]]) {
        eoLSL <- 1e6 * pweibull(q = lsl, shape = beta, scale = theta, lower.tail = TRUE)
      } else {
        eoLSL <- NA
      }
      if (options[["upperSpecificationLimit"]] && !options[["upperSpecificationLimitBoundary"]]) {
        eoUSL <- 1e6 * (1 - pweibull(q = usl, shape = beta, scale = theta))
      } else {
        eoUSL <- NA
      }
    } else if (options[["nonNormalDistribution"]] == "3ParameterLognormal") {
      distname <- "3-parameter-lognormal"
      if (options[["lowerSpecificationLimit"]] && !options[["lowerSpecificationLimitBoundary"]]) {
        eoLSL <- 1e6 * FAdist::plnorm3(q = lsl, shape = theta, scale = beta, thres = threshold, lower.tail = TRUE)
      } else {
        eoLSL <- NA
      }
      if (options[["upperSpecificationLimit"]] && !options[["upperSpecificationLimitBoundary"]]) {
        eoUSL <- 1e6 * (1 - FAdist::plnorm3(q = usl, shape = theta, scale = beta, thres = threshold))
      } else {
        eoUSL <- NA
      }
    } else if (options[["nonNormalDistribution"]] == "3ParameterWeibull") {
      distname <- "3-parameter-Weibull"
      if (options[["lowerSpecificationLimit"]] && !options[["lowerSpecificationLimitBoundary"]]) {
        eoLSL <- 1e6 * FAdist::pweibull3(q = lsl, shape = beta, scale = theta, thres = threshold, lower.tail = TRUE)
      } else {
        eoLSL <- NA
      }
      if (options[["upperSpecificationLimit"]] && !options[["upperSpecificationLimitBoundary"]]) {
        eoUSL <- 1e6 * (1 - FAdist::pweibull3(q = usl, shape = beta, scale = theta, thres = threshold))
      } else {
        eoUSL <- NA
      }
    }

    eoTOT <- if (!(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])) sum(c(eoLSL, eoUSL), na.rm = TRUE) else NA
    expOverall <- c(eoLSL, eoUSL, eoTOT)

    if (i == 1 && nStages > 1) {
      observedBaseline <- observed
      expOverallBaseline <- expOverall
    }
    if (nStages > 1) {
      currentTableDf1 <- data.frame("Stage" = c(stageTitle, "", ""),
                                    "Source" = c("ppm < LSL", "ppm > USL", "ppm total"),
                                    "Observed" = .pcTableFormatNumbers(observed),
                                    "Expected Overall" = .pcTableFormatNumbers(expOverall))

    } else {
      currentTableDf1 <- data.frame("Source" = c("ppm < LSL", "ppm > USL", "ppm total"),
                                    "Observed" = .pcTableFormatNumbers(observed),
                                    "Expected Overall" = .pcTableFormatNumbers(expOverall))
    }

    tableDf3 <- rbind(tableDf3, currentTableDf1)


    if (i > 1) {
      observedComparison <- observed - observedBaseline
      expOverallComparison <- expOverall - expOverallBaseline
      tableList3[[observedComparisonColName]] <- round(observedComparison, nDecimals)
      tableList3[[expOverallComparisonColName]] <- round(expOverallComparison, nDecimals)
      tableList3[[expOverallComparisonColName]][is.na(tableList3[[expOverallComparisonColName]])] <- "*" # This looks better in the table and makes clearer that there is not an error
      tableList3[[observedComparisonColName]][is.na(tableList3[[observedComparisonColName]])] <- "*"

      currentTableDf2 <- data.frame("Stage" = c(gettextf("%s vs. BL", stage),"", "") ,
                                    "Source" = c("ppm < LSL", "ppm > USL", "ppm total"),
                                    "Observed" = round(observedComparison, nDecimals),
                                    "Expected Overall" = round(expOverallComparison, nDecimals))
      tableDf3 <- rbind(tableDf3, currentTableDf2)
    }
    tableList3[[observedColName]] <- round(observed, nDecimals)
    tableList3[[expOverallColName]] <- round(expOverall, nDecimals)
    tableList3[[expOverallColName]][is.na(tableList3[[expOverallColName]])] <- "*" # This looks better in the table and makes clearer that there is not an error
    tableList3[[observedColName]][is.na(tableList3[[observedColName]])] <- "*"
  }

  tableList3[["rowNames"]] <- rowNames

  if(returnPerformanceDF){
    tableDf3ColNames <- if (nStages > 1) c("Stage", "Source", "Observed", "Expected Overall") else c("Source", "Observed", "Expected Overall")
    colnames(tableDf3) <- tableDf3ColNames
    tableDf3$`Expected Overall`[is.na(tableDf3$`Expected Overall`)] <- "*" # This looks better in the table and makes clearer that there is not an error
    tableDf3$Observed[is.na(tableDf3$Observed)] <- "*"
    return(tableDf3)
  }

  table$addFootnote(gettextf("Calculations based on %s distribution.", distname))
  container[["summaryTableNonNormal"]] <- table
  if (options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])
    table2$addFootnote(gettext("Statistics displayed as * were not calculated because the relevant specification limit is not set or set as boundary."))
  container[["overallCapabilityNonNormal"]] <- table2

  table3$setData(tableList3)
  if (options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])
    table3$addFootnote(gettext("Statistics displayed as * were not calculated because the relevant specification limit is not set or set as boundary."))
  container[["PerformanceNonNormal"]] <- table3
}

# Functions for probability plot section ####

## Containers ####

.qcProbabilityPlotContainer <- function(options, dataset, ready, jaspResults, measurements, stages) {
  if (!options[["probabilityPlot"]] || !is.null(jaspResults[["probabilityContainer"]]))
    return()
  container <- createJaspContainer(gettext("Probability table and plot"))
  container$dependOn(c(
    "probabilityPlot", "probabilityPlotRankMethod", "nullDistribution",
    "probabilityPlotGridLines", "report",
    .qcDataOptionNames()
  ))
  container$position <- 3
  jaspResults[["probabilityContainer"]] <- container

  if (!ready)
    return()
  if (identical(stages, "")) {
    nStages <- 1
  } else if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
  }
  if (sum(!is.na(dataset[measurements])) < 2) {
    plot <- createJaspPlot(width = 400, height = 400,
                           title = gettext("Probability plot"))
    container[["plot"]] <- plot
    plot$setError(gettextf("Need at least 2 measurements to calculate probability table/plot. %1$i measurement(s) detected.",
                           sum(!is.na(dataset[measurements]))))
    return()
  }
  if (nStages > 1 && (length(unique(na.omit(dataset)[[stages]])) < nStages)) {
    plot <- createJaspPlot(width = 400, height = 400,
                           title = gettext("Probability plot"))
    container[["plot"]] <- plot
    plot$setError(gettext("At least one of the stages contains no valid measurements."))
    return()
  }
  .qcProbabilityTable(dataset, options, container, measurements, stages)

  if (is.null(container[["ProbabilityPlot"]]))
    container[["ProbabilityPlot"]]  <- .qcProbabilityPlot(dataset, options, measurements, stages)
}

## Output ####

.qcProbabilityTable <- function(dataset, options, container, measurements, stages) {
  distributionTitle <- switch (options[["nullDistribution"]],
                               "weibull" = "Weibull",
                               "lognormal" = "lognormal",
                               "normal" = "normal")
  if (identical(stages, "")) {
    nStages <- 1
    dataset[["stage"]] <- 1
    stages <- "stage"
  } else if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
  }
  table <- createJaspTable(title = gettextf("Summary of test against the %1$s distribution", distributionTitle))
  table$position <- 1
  if (nStages > 1) {
    table$addColumnInfo(name = "stage",      	title = gettext("Stage"),  		type = "string")
    table$transpose <- TRUE
  }
  table$addColumnInfo(name = "n",      	title = gettext("N"),  		type = "integer")
  if (options[["nullDistribution"]] == "normal") {
    table$addColumnInfo(name = "mean",  title = gettextf("Mean (%1$s)", "\u03BC"), 				type = "number")
    table$addColumnInfo(name = "sd",    title = gettextf("Std. dev. (%1$s)", "\u03C3"), 	type = "number")
  } else if (options[["nullDistribution"]] == "lognormal") {
    table$addColumnInfo(name = "mean",  title = gettextf("Log mean (%1$s)", "\u03BC"),  		type = "number")
    table$addColumnInfo(name = "sd",    title = gettextf("Log std.dev (%1$s)", "\u03C3"), 	type = "number")
  } else if (options[["nullDistribution"]] == "weibull") {
    table$addColumnInfo(name = "mean",  title = gettextf("Shape (%1$s)", "\u03B2"), 			type = "number")
    table$addColumnInfo(name = "sd",    title = gettextf("Scale (%1$s)", "\u03B8"),        type = "number")
  }
  table$addColumnInfo(name = "ad",     	title = gettext("AD"), type = "integer")
  table$addColumnInfo(name = "p",		title = gettext("<i>p</i>-value"), type = "integer")
  table$addFootnote(gettextf("The Anderson-Darling statistic A<i>D</i> is calculated against the %1$s distribution.", distributionTitle))
  table$addFootnote(gettextf("Red dotted lines in the probability plot below represent a 95%% confidence interval."))
  if (nStages > 1)
    table$addFootnote(gettext("Columns titled 'Change' concern changes of the respective stage in comparison to baseline (BL)."))
  if (((options[["nullDistribution"]] == "lognormal") || options[["nullDistribution"]] == "weibull") &&
      any(na.omit(unlist(dataset[measurements])) < 0)) {
    table$setError(gettext("Dataset contains negative numbers. Not compatible with the selected distribution."))
    container[["probabilityTable"]] <- table
    return()
  }
  tableColNames <- c("mean", "sd", "n", "ad", "p")
  if (nStages > 1)
    tableColNames <- c("stage", tableColNames)
  tableDf <- data.frame(matrix(ncol = length(tableColNames), nrow = 0))
  colnames(tableDf) <- tableColNames
  for (i in seq_len(nStages)) {
    stage <- unique(dataset[[stages]])[i]
    dataCurrentStage <- dataset[which(dataset[[stages]] == stage), ][!names(dataset) %in% stages]
    values <- as.vector(na.omit(unlist(dataCurrentStage[measurements]))) # distribution fitting function complains if this is not explicitly a vector

    if (options[["nullDistribution"]] == "normal") {
      meanx   <- mean(values)
      sdx     <- sd(values)
      test    <- goftest::ad.test(x = values, "norm", mean = meanx, sd = sdx)
    } else if (options[["nullDistribution"]] == "lognormal") {
      fit    <- EnvStats::elnorm(values)
      meanx  <- fit$parameters[1]
      sdx    <- fit$parameters[2]
      test   <- goftest::ad.test(x = values, "plnorm", meanlog = meanx, sdlog = sdx)
    } else if (options[["nullDistribution"]] == "weibull") {
      fit    <- fitdistrplus::fitdist(values, 'weibull',
                                      control = list(
                                        maxit = 10000,
                                        abstol = .Machine$double.eps^0.75,
                                        reltol = .Machine$double.eps^0.75,
                                        ndeps = rep(1e-8, 2)))
      meanx  <- fit$estimate[1]
      sdx    <- fit$estimate[2]
      test   <- goftest::ad.test(x = values, "pweibull", shape = meanx, scale = sdx)
    }
    n      <- length(values)
    ad     <- test$statistic
    adStar <- ad*(1 + (0.75/n) + (2.25/(n^2)))
    if(ad >= 0.6) {
      p <- exp(1.2937 - (5.709 * adStar) + 0.0186 * (adStar^2))
    } else if(adStar < 0.6 && adStar > 0.34) {
      p <- exp(0.9177 - (4.279 * adStar) - 1.38 * (adStar^2))
    } else if(adStar < 0.34 && adStar > 0.2) {
      p <- 1 - exp(-8.318 + (42.796 * adStar) - 59.938 * (adStar^2))
    } else if(adStar <= 0.2) {
      p <- 1 - exp(-13.436 + (101.14 * adStar) - 223.73 * (adStar^2))      #Jaentschi & Bolboaca (2018)
    } else {
      p <- test$p.value
    }
    tableDfCurrentStage <- data.frame(mean = meanx, sd = sdx, n = n, ad = round(ad, .numDecimals), p = round(p, .numDecimals))
    if (i == 1 && nStages > 1)
      baseLineDf <- tableDfCurrentStage
    if (i > 1) {
      changeDf <- tableDfCurrentStage - baseLineDf
      changeDf$stage <- gettextf("Change (%s vs. BL)", stage)
      # Differences in p-value or AD are not interesting
      changeDf$p <- "-"
      changeDf$ad <- "-"
    }
    if(nStages > 1)
      tableDfCurrentStage$stage <- if (i == 1) gettextf("%s (BL)", stage) else as.character(stage)
    tableDf <- rbind(tableDf, tableDfCurrentStage)
    if (i > 1)
      tableDf <- rbind(tableDf, changeDf)
  }
  tableList <- as.list(tableDf)
  table$setData(tableList)
  container[["probabilityTable"]] <- table
}

.qcProbabilityPlot <- function(dataset, options, measurements = NULL, stages) {
  distributionTitle <- switch (options[["nullDistribution"]],
                               "weibull" = "Weibull",
                               "lognormal" = "lognormal",
                               "normal" = "normal")
  # calculating plot dimensions based on number of stages
  if (identical(stages, "")) {
    nStages <- 1
  } else if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
  }
  nCol <- if (nStages > 1) 2 else 1
  nRow <- ceiling(nStages/2)
  plotWidth <- nCol * 600
  plotHeight <- nRow * 600
  plot <- createJaspPlot(width = plotWidth, height = plotHeight,
                         title = gettextf("Probability plot against %1$s distribution", distributionTitle))
  if (((options[["nullDistribution"]] == "lognormal") || options[["nullDistribution"]] == "weibull") &&
      any(na.omit(unlist(dataset[measurements])) < 0)) {
    plot$setError(gettext("Dataset contains negative numbers. Not compatible with the selected distribution."))
    return(plot)
  }

  plotList <- try(.qcProbabilityPlotObject(options, dataset, measurements, stages))
  if (jaspBase::isTryError(plotList)) {
    plot$setError(plotList[1])
    return()
  }
  # if number of plots is odd, add an empty plot at the end
  if (nStages > 1 && nStages %% 2 != 0)
    plotList[[nStages + 1]] <- ggplot2::ggplot() + ggplot2::theme_void()
  plotMat <- matrix(plotList, ncol = nCol, nrow = nRow, byrow = TRUE)
  plot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)
  return(plot)
}

.qcProbabilityPlotObject <- function(options, dataset, measurements, stages) {
  if (identical(stages, "")) {
    nStages <- 1
    dataset[["stage"]] <- 1
    stages <- "stage"
  } else if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
  }
  plotList <- list()
  for (i in seq_len(nStages)) {
    stage <- unique(dataset[[stages]])[i]
    dataCurrentStage <- dataset[which(dataset[[stages]] == stage), ][!names(dataset) %in% stages]
    dataCurrentStage <- as.vector(na.omit(unlist(dataCurrentStage[measurements])))
    dataCurrentStage <- dataCurrentStage[order(dataCurrentStage)]
    label_x <- dataCurrentStage
    n <- length(dataCurrentStage)

    # Method for rank
    Rank_funs <- matrix(list(.qcPpMedian, .qcPpMean, .qcPpKmModif, .qcPpKm), ncol = 1,
                        dimnames = list(c("bernard", "herdJohnson", "hazen", "kaplanMeier"), c("p")), byrow = TRUE)

    rankByUser <- options[["probabilityPlotRankMethod"]]
    p <- Rank_funs[[rankByUser, 'p']](dataCurrentStage)

    # Functions for computing y
    y_funs <- matrix(list(qnorm,qnorm,.qcWeibull), ncol = 1,
                     dimnames = list(c("normal", "lognormal", "weibull"), c('y')), byrow = TRUE)

    DisByUser <- options[["nullDistribution"]]

    y <- y_funs[[DisByUser, 'y']](p)

    # Quantities
    pSeq <- seq(0.001, 0.999, 0.001)
    ticks <- c(0.1, 1, 5, seq(10, 90, 10), 95, 99, 99.9)

    # Computing according to the distribution
    if (options[["nullDistribution"]] == "normal") {
      lpdf <- quote(-log(sigma) - 0.5 / sigma ^ 2 * (x - mu) ^ 2)
      matrix <- try(mle.tools::observed.varcov(logdensity = lpdf, X = dataCurrentStage, parms = c("mu", "sigma"),
                                               mle = c(mean(dataCurrentStage), sd(dataCurrentStage))))
      if (jaspBase::isTryError(matrix)) {
        stop(gettext("Fitting distribution failed. Values might be too extreme. Try a different distribution."), call. = FALSE)
        return()
      }
      varMu <- matrix$varcov[1, 1]
      varSigma <- matrix$varcov[2,2]
      covarMuSigma <- matrix$varcov[1, 2]
      zp <- qnorm(p = pSeq)
      zalpha <- qnorm(0.975)
      percentileEstimate <- mean(dataCurrentStage) + zp * sd(dataCurrentStage)
      varPercentile <- varMu + zp^2 * varSigma + 2*zp * covarMuSigma
      percentileLower <- percentileEstimate - zalpha * sqrt(varPercentile)
      percentileUpper <- percentileEstimate + zalpha * sqrt(varPercentile)
      yBreaks <- qnorm(ticks / 100)
      xBreaks <- label_x <- jaspGraphs::getPrettyAxisBreaks(dataCurrentStage)
      xLimits <- range(xBreaks)
    } else if (options[["nullDistribution"]] == "lognormal") {
      fit    <- EnvStats::elnorm(dataCurrentStage)
      meanlog <- as.numeric(fit$parameters[1])
      sdlog <- as.numeric(fit$parameters[2])
      lpdf <- quote(log(1/(sqrt(2*pi)*x*sdlog) * exp(-(log(x)- meanlog)^2/(2*sdlog^2))))
      matrix <- try(mle.tools::observed.varcov(logdensity = lpdf, X = dataCurrentStage, parms = c("meanlog", "sdlog"), mle = fit$parameters))
      if (jaspBase::isTryError(matrix)) {
        stop(gettext("Fitting distribution failed. Values might be too extreme. Try a different distribution."), call. = FALSE)
        return()
      }
      varmeanlog <- matrix$varcov[1, 1]
      varsdlog <- matrix$varcov[2,2]
      covarSS <- matrix$varcov[1, 2]
      zp <- qnorm(p = pSeq)
      zalpha <- qnorm(0.975)
      percentileEstimate <- exp(meanlog + zp*sdlog)
      varPercentile <- percentileEstimate^2*( varmeanlog+zp^2*varsdlog + 2*zp * covarSS)
      percentileLower <- exp( log(percentileEstimate) - zalpha * (sqrt(varPercentile)/percentileEstimate))
      percentileUpper <- exp(log(percentileEstimate) + zalpha * (sqrt(varPercentile)/percentileEstimate))
      yBreaks <- qnorm(ticks / 100)
      dataCurrentStage <- log(label_x)
      percentileEstimate <- log(percentileEstimate)
      percentileLower <- log(percentileLower)
      percentileUpper <- log(percentileUpper)
      labelFrame <- data.frame(labs = label_x, value = dataCurrentStage)
      index <- c(1,jaspGraphs::getPrettyAxisBreaks(1:nrow(labelFrame), 4)[-1])
      xBreaks <- labelFrame[index,2]
      label_x <- labelFrame[index,1]
      xLimits <- c(min(xBreaks) * 0.8, max(xBreaks) * 1.2)
    } else if (options[["nullDistribution"]] == "weibull") {
      fit <- fitdistrplus::fitdist(dataCurrentStage, 'weibull')
      shape <- as.numeric(fit$estimate[1])
      scale <- as.numeric(fit$estimate[2])
      lpdf <- quote(log(shape) - shape * log(scale) + shape * log(x) - (x/scale)^shape)
      matrix <- try(mle.tools::observed.varcov(logdensity = lpdf, X = dataCurrentStage, parms = c("shape", "scale"), mle = fit$estimate))
      if (jaspBase::isTryError(matrix)) {
        stop(gettext("Fitting distribution failed. Values might be too extreme. Try a different distribution."), call. = FALSE)
        return()
      }
      varShape <- matrix$varcov[1,1]
      varScale <- matrix$varcov[2,2]
      covarSS <- matrix$varcov[1,2]
      zp <- log(-1*log(1-pSeq))
      zalpha <- log(-1*log(1-0.975))
      percentileEstimate <- scale * (- log(1 - pSeq))^(1/shape)
      varPercentile <- (percentileEstimate^2 / scale^2) * varScale + (percentileEstimate^2/shape^4)*zp^2*varShape - 2*((zp*percentileEstimate^2) / (scale * shape^2))*covarSS
      percentileLower <- exp(log(percentileEstimate) - zalpha * (sqrt(varPercentile)/percentileEstimate))
      percentileUpper <- exp(log(percentileEstimate) + zalpha * (sqrt(varPercentile)/percentileEstimate))
      yBreaks <- log(-1*log(1-(ticks / 100)))
      dataCurrentStage <- log(label_x)
      percentileEstimate <- log(percentileEstimate)
      percentileLower <- log(percentileLower)
      percentileUpper <- log(percentileUpper)
      labelFrame <- data.frame(labs = label_x, value = dataCurrentStage)
      index <- c(1,jaspGraphs::getPrettyAxisBreaks(1:nrow(labelFrame), 4)[-1])
      xBreaks <- labelFrame[index,2]
      label_x <- labelFrame[index,1]
      xLimits <- c(min(xBreaks) * 0.8, max(xBreaks) * 1.2)
    }
    data1 <- data.frame(x = dataCurrentStage, y = y)
    yLimits <- range(yBreaks)
    label_x <- round(label_x, .numDecimals)
    p <- ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileEstimate), na.rm = TRUE) +
      jaspGraphs::geom_point(data = data1, ggplot2::aes(x = x, y = y), na.rm = TRUE)

    if (options[["probabilityPlotGridLines"]])
      p <- p + ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "lightgray"))
    if (nStages > 1)
      p <- p + ggplot2::ggtitle(stage) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    p <- p + ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileLower), col = "darkred", linetype = "dashed", na.rm = TRUE) +
      ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileUpper), col = "darkred", linetype = "dashed", na.rm = TRUE) +
      ggplot2::scale_x_continuous(gettext("Measurement"), breaks = xBreaks, limits = xLimits, labels = label_x) +
      ggplot2::scale_y_continuous(gettext('Percent'), labels = ticks, breaks = yBreaks, limits = yLimits) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
    plotList[[i]] <- p
  }
  return(plotList)
}

.qcPpMedian <- function(x) {
  x <- x[order(x)]
  n <- length(x)
  i <- rank(x)
  p <- (i - 0.3) / (n + 0.4)
  return(p)
}

.qcPpMean <- function(x) {
  x <- x[order(x)]
  n <- length(x)
  i <- rank(x)
  p <- (i) / (n + 1)
  return(p)
}

.qcPpKmModif <- function(x,i,n) {
  x <- x[order(x)]
  n <- length(x)
  i <- rank(x)
  p <- (i - 0.5) / (n)
  return(p)
}

.qcPpKm <- function(x) {
  x <- x[order(x)]
  n <- length(x)
  i <- rank(x)
  p <- (i) / (n)
  return(p)
}

.qcWeibull <- function(p) {
  return(log(-log(1 - p)))
}

# Functions for distribution plot section ####

.qcDistributionPlot <- function(options, dataset, ready, jaspResults, measurements, stages) {

  if(!options[["histogram"]] || !is.null(jaspResults[["histogram"]]))
    return()

  if (!ready) {
    plot <- createJaspPlot(title = gettext("Histogram"), width = 600, height = 400)
    plot$dependOn(c(
      "histogram", "histogramDensityLine", "histogramBinNumber",
      "report", "nullDistribution", "histogramBinBoundaryDirection",
      .qcDataOptionNames()
    ))
    jaspResults[["histogram"]] <- plot
    return()
  }

  # calculating plot dimensions based on number of stages
  if (identical(stages, "")) {
    nStages <- 1
  } else if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
  }
  nCol <- if (nStages > 1) 2 else 1
  nRow <- ceiling(nStages/2)
  plotWidth <- nCol * 600
  plotHeight <- nRow * 400
  plot <- createJaspPlot(title = gettext("Histogram"), width = plotWidth, height = plotHeight)
  plot$dependOn(c(
    "histogram", "histogramDensityLine", "histogramBinNumber",
    "report", "nullDistribution", "histogramBinBoundaryDirection",
    .qcDataOptionNames()
  ))
  plot$position <- 2
  jaspResults[["histogram"]] <- plot

  if (sum(!is.na(dataset[measurements])) < 1) {
    plot$setError(gettext("No valid measurements detected."))
    return()
  }
  if (sum(!is.na(dataset[measurements])) < 2 && options[["histogramDensityLine"]]) {
    plot$setError(gettextf("Need at least 2 measurements to fit distribution. %1$i measurement(s) detected.",
                           sum(!is.na(dataset[measurements]))))
    return()
  }
  if (options[["histogramDensityLine"]] && (options[['nullDistribution']]  == "weibull" || options[['nullDistribution']]  == "lognormal") &&
      (any(na.omit(unlist(dataset[measurements])) < 0))) {
    plot$setError(gettext("Dataset contains negative numbers. Could not overlay the selected distribution."))
    return()
  }
  if (nStages > 1 && (length(unique(na.omit(dataset)[[stages]])) < nStages)) {
    plot$setError(gettext("At least one of the stages contains no valid measurements."))
    return()
  }
  plotList <- .qcDistributionPlotObject(options, dataset, measurements, stages)
  # if number of plots is odd, add an empty plot at the end
  if (nStages > 1 && nStages %% 2 != 0)
    plotList[[nStages + 1]] <- ggplot2::ggplot() + ggplot2::theme_void()
  plotMat <- matrix(plotList, ncol = nCol, nrow = nRow, byrow = TRUE)
  plot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)
}

.qcDistributionPlotObject <- function(options, dataset, measurements, stages) {
  if (identical(stages, "")) {
    nStages <- 1
    dataset[["stage"]] <- 1
    stages <- "stage"
  } else if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
  }
  plotList <- list()
  for (i in seq_len(nStages)) {
    stage <- unique(dataset[[stages]])[i]
    dataCurrentStage <- dataset[which(dataset[[stages]] == stage), ][!names(dataset) %in% stages]
    dataCurrentStage <- as.vector(na.omit(unlist(dataCurrentStage[, measurements]))) # the distribution fitting functions complain if this is not explicitly a vector
    nBins <- options[["histogramBinNumber"]]
    n <- length(dataCurrentStage)
    df <- data.frame(measurements = dataCurrentStage)
    h <- hist(dataCurrentStage, plot = FALSE, breaks = nBins)
    binWidth <- (h$breaks[2] - h$breaks[1])
    freqs <- h$counts
    yLabels <- jaspGraphs::getPrettyAxisBreaks(c(0, freqs, max(freqs) + 5))
    yBreaks <- yLabels / (n * binWidth)
    yLimits <- range(yBreaks)
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(h$breaks, dataCurrentStage), min.n = 4)
    xLimits <- range(xBreaks)

    p <- ggplot2::ggplot() +
      ggplot2::geom_histogram(data = df, mapping = ggplot2::aes(y =..density.., x = measurements), closed = options[["histogramBinBoundaryDirection"]],
                              fill = "grey", col = "black", linewidth = .7, binwidth = binWidth, center = binWidth/2, na.rm = TRUE) +
      ggplot2::scale_x_continuous(name = gettext("Measurement"), breaks = xBreaks, limits = xLimits) +
      ggplot2::scale_y_continuous(name =  gettext("Counts"), labels = yLabels, breaks = yBreaks, limits = yLimits) +
      jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()
    if (nStages > 1)
      p <- p + ggplot2::ggtitle(stage) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

    if (options[["histogramDensityLine"]]) {
      if (options[['nullDistribution']]  == "normal") {
        p <- p + ggplot2::stat_function(fun = dnorm, args = list(mean = mean(dataCurrentStage), sd = sd(dataCurrentStage)),
                                        mapping = ggplot2::aes(color = "normalDist"))
        legendLabel <- gettext("Normal dist.")
      } else if (options[['nullDistribution']]  == "weibull") {
        fit_Weibull <- fitdistrplus::fitdist(dataCurrentStage, "weibull", method = "mle",
                                             control = list(maxit = 500, abstol = .Machine$double.eps, reltol = .Machine$double.eps))
        shape <- fit_Weibull$estimate[[1]]
        scale <- fit_Weibull$estimate[[2]]
        p <- p + ggplot2::stat_function(fun = dweibull, args = list(shape = shape, scale = scale),
                                        mapping = ggplot2::aes(color = "weibullDist"))
        legendLabel <- gettext("Weibull dist.")
      } else if(options[['nullDistribution']]  == "lognormal") {
        fit_Lnorm    <- EnvStats::elnorm(dataCurrentStage)
        shape  <- fit_Lnorm$parameters[1]
        scale    <- fit_Lnorm$parameters[2]
        p <- p + ggplot2::stat_function(fun = dlnorm, args = list(meanlog = shape, sdlog = scale),
                                        mapping = ggplot2::aes(color = "lognormallDist"))
        legendLabel <- gettext("Lognormal dist.")
      }
      p <- p + ggplot2::scale_color_manual("", values = "dodgerblue", labels = legendLabel) +
        ggplot2::theme(legend.position = "right")
    }
    plotList[[i]] <- p
  }
  return(plotList)
}

.pcTableFormatNumbers <- function(number) {

  if (is.null(number))
    return(number)

  if (all(is.na(number)))
    return(rep(NA, length(number)))

  output <- formatC(number, format = "f", digits = .numDecimals)
  output <- sub("\\.?0+$", "", output)
  if (anyNA(number))
    output[is.na(number)] <- NA

  return(output)
}


# helper functions for transforms ----

.qcTransformData <- function(dataset, measurements, options) {
  # we need to append a unique id for each row in a wide format
  # so that we can do pivot_longer -> pivot_wider

  # save any possible columns that might be next to grouping and measurements
  extra <- dplyr::select(dataset, -tidyr::all_of(measurements))

  # then add a column which IDs the measurements
  dataset[["id"]] <- seq_len(nrow(dataset))

  # and finally convert to long
  dataset <- tidyr::pivot_longer(dataset, tidyr::all_of(measurements))

  x <- dataset[["value"]]
  group <- dataset[["name"]]
  lambda <- options[["dataTransformationLambda"]]
  shift <- options[["dataTransformationShift"]]
  method <- options[["dataTransformationMethod"]]
  ca <- options[["dataTransformationContinuityAdjustment"]]

  dataset[["value"]] <- switch(
    options[["dataTransformation"]],
    boxCox = jaspBase::BoxCox(x, lambda=lambda, shift=shift, continuityAdjustment=ca),
    boxCoxAuto = jaspBase::BoxCoxAuto(x, predictor=as.factor(group), shift=shift, method=method, continuityAdjustment=ca),
    yeoJohnson = jaspBase::YeoJohnson(x, lambda=lambda),
    yeoJohnsonAuto = jaspBase::YeoJohnsonAuto(x),
    johnson = jaspBase::Johnson(x)
  )

  # some auto-transformations return meta-info about the applied transform
  attr <- attributes(dataset[["value"]])
  attributes(dataset[["value"]]) <- NULL

  # reshape back to wider format and remove the id col
  dataset <- tidyr::pivot_wider(dataset, values_from="value", names_from="name", id_cols="id")
  dataset[["id"]] <- NULL
  dataset <- as.data.frame(dataset)
  # return extra columns
  dataset <- cbind(dataset, extra)

  return(list(dataset=dataset, parameters=attr))
}


.qcTransformSpecs <- function(options, parameters) {
  # lower and upper specification specs + target value transformed
  specs <- list()

  if(options[["lowerSpecificationLimit"]]) specs <- c(specs, options["lowerSpecificationLimitValue"])
  if(options[["upperSpecificationLimit"]]) specs <- c(specs, options["upperSpecificationLimitValue"])
  if(options[["target"]]) specs <- c(specs, options["targetValue"])

  specs <- unlist(specs)

  if (length(specs) == 0L) return(list())

  specNames <- names(specs)

  if (options[["dataTransformation"]] %in% c("boxCox", "boxCoxAuto")) {
    shift <- options[["dataTransformationShift"]]
    lambda <- if(options[["dataTransformation"]] == "boxCox") options[["dataTransformationLambda"]] else parameters[["lambda"]]
    if (any(specs + shift <= 0))
      stop(gettextf("Some specification limits or target value are outside of the support of the Box-Cox transform. The lower bound of the Box-Cox transform is -shift (%1$f).", -shift))

    specs <- BoxCox(specs, lambda=lambda, shift=shift, continuityAdjustment=options[["dataTransformationContinuityAdjustment"]])
  } else if(options[["dataTransformation"]] %in% c("yeoJohnson", "yeoJohnsonAuto")) {
    lambda <- if(options[["dataTransformation"]] == "yeoJohnson") options[["dataTransformationLambda"]] else parameters[["lambda"]]
    specs <- YeoJohnson(specs, lambda=lambda)
  } else if (options[["dataTransformation"]] == "johnson") {
    args <- parameters[["params"]]
    args[["x"]] <- specs

    # check for errors (invalid bounds)
    # there might be some corrections for these cases but I could not find proper references except for documentation of other software.
    # so for now we will just throw an error...
    if (parameters[["type"]] == "sb") { # bounded between epsilon and epsilon+lambda
      min <- args[["epsilon"]]
      max <- args[["epsilon"]] + args[["lambda"]]

      if (any(specs <= min) || any(specs >= max))
        stop(gettextf("Some specification limits or target value are outside of the support of the Johnson (SB) transform. The bounds of the transform were identified to between %1$f and %2$f.", min, max))
    } else if(parameters[["type"]] == "sl") { # bounded from below by epsilon
      min <- args[["epsilon"]]

      if (any(specs <= min))
        stop(gettextf("Some specification limits or target value are outside of the support of the Johnson (SL) transform. The lower bound of the transform was identified as %1$f.", min))
    }

    #TODO: export these functions from jaspBase
    specs <- switch(
      parameters[["type"]],
      sb = with(data=args, gamma + eta * log((x - epsilon) / (lambda + epsilon - x))),
      sl = with(data=args, gamma + eta * log(x - epsilon)),
      su = with(data=args, gamma + eta * asinh((x - epsilon) / lambda))
    )
  }

  names(specs) <- specNames

  return(as.list(specs))
}

.qcFillTransformOutput <- function(container, options, parameters) {
  container[["formula"]] <- .qcTransformFormula(options, parameters)
  container[["table"]] <- .qcTransformTable(options, parameters)
}

.qcTransformFormula <- function(options, parameters) {
  if (options[["dataTransformation"]] %in% c("boxCox", "boxCoxAuto")) {
    name <- "Box-Cox"
    shift <- options[["dataTransformationShift"]]
    lambda <- if(options[["dataTransformation"]] == "boxCox") options[["dataTransformationLambda"]] else parameters[["lambda"]]

    if (shift == 0) {
      if (lambda == 0) {
        formula <- r"(y = \ln(x))"
      } else if (!options[["dataTransformationContinuityAdjustment"]]) {
        formula <- r"(y = x^\lambda)"
      } else {
        formula <- r"(y = \frac{x^\lambda - 1}{\lambda})"
      }
    } else {
      if (lambda == 0) {
        formula <- r"(y = \ln(x + \text{shift}))"
      } else if (!options[["dataTransformationContinuityAdjustment"]]) {
        formula <- r"(y = (x+\text{shift})^\lambda)"
      } else {
        formula <- r"(y = \frac{(x+\text{shift})^\lambda - 1}{\lambda})"
      }
    }

  } else if (options[["dataTransformation"]] %in% c("yeoJohnson", "yeoJohnsonAuto")) {
    name <- "Yeo-Johnson"

    formula <-
      r"(
      y =
      \begin{cases}
        ((x+1)^\lambda-1)/\lambda                      &  \text{if }\lambda \neq 0, x \geq 0 \\
        \ln(x + 1)                                     &  \text{if }\lambda =    0, x \geq 0 \\
        -((-x + 1)^{(2-\lambda)} - 1) / (2 - \lambda)  &  \text{if }\lambda \neq 2, x <    0 \\
        -\ln(-x + 1)                                   &  \text{if }\lambda =    2, x <    0
      \end{cases}
      )"

  } else if (options[["dataTransformation"]] == "johnson") {
    formula <- switch(
      parameters[["type"]],
      "sb" = r"(y = \gamma + \eta \ln \frac{x-\epsilon}{\lambda + \epsilon - x})",
      "sl" = r"(y = \gamma + \eta \ln (x-\epsilon))",
      "su" = r"(y = \gamma + \eta \sinh^{-1} \frac{x-\epsilon}{\lambda})"
    )
    name <- switch(
      parameters[["type"]],
      "sb" = "Johnson (SB)",
      "sl" = "Johnson (SL)",
      "su" = "Johnson (SU)"
    )
  }

  formula <- mathExpression(formula, inline=FALSE)

  intro <- gettextf("The measurements, specification limits and target value were transformed using the %s transformation, with the following formula,", name)

  html <- createJaspHtml(title="", text = paste(intro, formula, sep="</br>"))

  return(html)
}


.qcTransformTable <- function(options, parameters) {
  table <- createJaspTable(title = gettext("Parameters of the transform"))
  table$addColumnInfo(name = "par",   title = gettext("Parameter"), type = "string")
  table$addColumnInfo(name = "value", title = gettext("Value"),     type = "number"  )

  if (options[["dataTransformation"]] %in% c("boxCox", "boxCoxAuto") && options[["dataTransformationShift"]]!=0)
    table$addRows(list(par="shift", value=options[["dataTransformationShift"]]))


  if (options[["dataTransformation"]] %in% c("boxCox", "yeoJohnson")) {
    table$addRows(list(par=mathExpression("\\lambda"), value=options[["dataTransformationLambda"]]))

  } else if (options[["dataTransformation"]] %in% c("boxCoxAuto", "yeoJohnsonAuto")) {
    table$addRows(list(par=mathExpression("\\lambda"), value=parameters[["lambda"]]), rowNames = "lambda")
    table$addFootnote(gettext("Estimated from data"), rowNames = "lambda", colNames = "value")

  } else if (options[["dataTransformation"]] == "johnson") {
    if (parameters[["type"]] %in% c("sb", "su")) {
      pars <- mathExpression(c("\\eta", "\\gamma", "\\lambda", "\\epsilon"))
      vals <- c(parameters[["params"]][c("eta", "gamma", "lambda", "epsilon")])
    } else if (parameters[["type"]] == "sl") {
      pars <- mathExpression(c("\\eta", "\\gamma", "\\epsilon"))
      vals <- c(parameters[["params"]][c("eta", "gamma", "epsilon")])
    }

    table$setData(list(par=pars, value=vals))

    table$addFootnote(gettext("All values estimated from data"), colNames = "value")
  }
  return(table)
}

.qcDataOptionNames <- function() {
  dependencies <- c("dataFormat",
                    "measurementLongFormat", "subgroup","stagesLongFormat",
                    "measurementsWideFormat", "stagesWideFormat",
                    "subgroupSizeType", "groupingVariable", "groupingVariableMethod",
                    "subgroupSizeUnequal", "fixedSubgroupSizeValue", "manualSubgroupSizeValue",
                    "dataTransformation", "dataTransformationShift", "dataTransformationLambda",
                    "dataTransformationMethod",
                    "dataTransformationLambdaLower", "dataTransformationLambdaUpper",
                    "dataTransformationContinuityAdjustment",
                    "lowerSpecificationLimit", "lowerSpecificationLimitValue",
                    "target", "targetValue",
                    "upperSpecificationLimit", "upperSpecificationLimitValue")
  return(dependencies)
}

.qcReportOptionNames <- function() {
  dependencies <- c("report", "reportMetaData", "reportTitle", "reportTitleText",
                    "reportLocation", "reportLocationText", "reportLine", "reportLineText",
                    "reportMachine", "reportMachineText", "reportVariable", "reportVariableText",
                    "reportProcess", "reportProcessText", "reportDate", "reportDateText",
                    "reportReportedBy", "reportReportedByText", "reportConclusion",
                    "reportConclusionText", "reportProcessStability", "reportProcessCapabilityPlot",
                    "reportProbabilityPlot", "reportProcessCapabilityTables")

  return(dependencies)
}

.qcWithinProcessValid <- function(options) {
  # within process results make sense only for selected transforms:
  # - none,
  # - those that have only fixed parameters (Box-Cox and Yeo-Johnson with manually fixed params)
  # - Box-Cox auto (normalizes within groups, not across groups)
  return(options[["dataTransformation"]] %in% c("none", "boxCox", "boxCoxAuto", "yeoJohnson"))
}
