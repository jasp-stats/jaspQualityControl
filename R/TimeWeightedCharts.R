#' @export
timeWeightedCharts <- function(jaspResults, dataset, options) {
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
    # Workaround to create subgroups of size 1 in long format while still using axis labels, but not creating a whole separate variable form
    if (options[["subgroupSizeType"]] == "individual") {
      axisLabels <- options[["subgroup"]]
      options[["subgroupSizeType"]] <- "manual"
      options[["manualSubgroupSizeValue"]] <- 1
      subgroupVariable <- ""
    } else {
      subgroupVariable <- options[["subgroup"]]
      axisLabels <- ""
    }
    factorVariables <- c(axisLabels, subgroupVariable, stages)
  }

  measurements <- measurements[measurements != ""]
  factorVariables <- factorVariables[factorVariables != ""]

  # Check if analysis is ready
  if (wideFormat) {
    ready <- length(measurements) >= 1
  } else if (!wideFormat && options[["subgroupSizeType"]] == "manual"){
    ready <- length(measurements) == 1
  } else if (!wideFormat && options[["subgroupSizeType"]] == "groupingVariable") {
    ready <- length(measurements) == 1 && subgroupVariable != ""
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

  # warning handling
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

    if (!wideFormat && options[["subgroupSizeType"]] == "groupingVariable" && anyNA(dataset[[subgroupVariable]])) {
      nDroppedSubgroupRows <- sum(is.na(dataset[[subgroupVariable]]))
      dataset <- dataset[!is.na(dataset[[subgroupVariable]]),]
      plotNotes <- paste0(plotNotes, gettextf("Removed %i observation(s) that were not assigned to any subgroups.<br>", nDroppedSubgroupRows))
    }
  }

  # Rearrange data if not already wide format (one group per row)
  if (!wideFormat && ready) {
    reshapeOutputList <- .reshapeSubgroupDataLongToWide(dataset, measurements, stages = stages, subgroupVariable = subgroupVariable,
                                                        subgroupSizeType = options[["subgroupSizeType"]],
                                                        manualSubgroupSizeValue = options[["manualSubgroupSizeValue"]],
                                                        subgroupVariableMethod = options[["groupingVariableMethod"]])
    axisLabels <- if (axisLabels == "") reshapeOutputList$axisLabels else dataset[[axisLabels]]
    dataset <- reshapeOutputList$dataset
    measurements <- reshapeOutputList$measurements
    xAxisTitle <- reshapeOutputList$xAxisTitle
  }  else if (wideFormat && ready) {
    if (axisLabels != "") {
      xAxisTitle <- options[["axisLabels"]]
      axisLabels <- dataset[[axisLabels]]
    } else {
      xAxisTitle <- gettext("Sample")
    }
  }

  #Cusum chart
  if (options[["cumulativeSumChart"]] && is.null(jaspResults[["CusumChart"]])) {
    cusumChart <- .Cusumchart(dataset = dataset, measurements = measurements, stages = stages,
                              axisLabels = axisLabels, options = options, ready = ready)
  }
  #EWMA chart
  if (options[["exponentiallyWeightedMovingAverageChart"]] && is.null(jaspResults[["EWMAPlot"]])) {
    ewmaPlot <- .EWMA(dataset = dataset, measurements = measurements, stages = stages,
                      axisLabels = axisLabels, options = options, ready = ready)
  }


  # Report
  if (options[["report"]]) {
    reportPlot <- createJaspPlot(title = gettext("Time weighted charts report"), width = 1250, height = 1000)
    jaspResults[["report"]] <- reportPlot
    jaspResults[["report"]]$dependOn(c(""))

    # Plot meta data
    if (options[["reportTitle"]] ) {
      title <- if (options[["reportTitleText"]] == "") gettext("Time weighted charts report") else options[["reportTitleText"]]
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
    if (options[["cumulativeSumChart"]])
      plots[["cusum"]] <- cusumChart$plotObject
    if (options[["exponentiallyWeightedMovingAverageChart"]])
      plots[["ewma"]] <- ewmaPlot$plotObject
    reportPlotObject <- .qcReport(text = text, plots = plots, textMaxRows = 8,
                                  reportTitle = title)
    reportPlot$plotObject <- reportPlotObject

    ###
    ### If not report mode
    ###
  } else {
    if (options[["cumulativeSumChart"]])
      jaspResults[["CusumChart"]] <- cusumChart
    if (options[["exponentiallyWeightedMovingAverageChart"]])
      jaspResults[["EWMAPlot"]] <- ewmaPlot
  }
}

.Cusumchart <- function(dataset, measurements, stages, axisLabels, options, ready) {

  plot <- createJaspPlot(title = gettext("Cumulative sum chart"), width = 1200, height = 500)
  plot$dependOn(c("cumulativeSumChart", "measurements"))

  if (!ready)
    return(plot)


  columnsToPass <- c(measurements, stages)
  columnsToPass <- columnsToPass[columnsToPass != ""]
  phase2 <- (options[["cumulativeSumChartSdSource"]] == "historical")
  plotObject <- .controlChart(dataset[columnsToPass], plotType = "cusum", stages = stages, xBarSdType = options[["cumulativeSumChartSdMethod"]],
                              nSigmasControlLimits = options[["cumulativeSumChartNumberSd"]], xAxisLabels = axisLabels,
                              cusumShiftSize = options[["cumulativeSumChartShiftSize"]], cusumTarget = options[["cumulativeSumChartTarget"]],
                              movingRangeLength = options[["cumulativeSumChartAverageMovingRangeLength"]], phase2 = phase2,
                              phase2Sd = options[["cumulativeSumChartSdValue"]])$plotObject

  plot$plotObject <- plotObject
  return(plot)
}

.EWMA <- function(dataset, measurements, stages, axisLabels, options, ready) {

  plot <-  createJaspPlot(title = gettext("Exponentially weighted moving average chart"), width = 1200, height = 500)
  plot$dependOn(c(""))

  if (!ready)
    return(plot)

  columnsToPass <- c(measurements, stages)
  columnsToPass <- columnsToPass[columnsToPass != ""]
  phase2 <- (options[["exponentiallyWeightedMovingAverageChartSdSource"]] == "historical")
  plotObject <- .controlChart(dataset[columnsToPass], plotType = "ewma", stages = stages, xBarSdType = options[["exponentiallyWeightedMovingAverageChartSdMethod"]],
                              nSigmasControlLimits = options[["exponentiallyWeightedMovingAverageChartSigmaControlLimits"]],
                              xAxisLabels = axisLabels, movingRangeLength = options[["exponentiallyWeightedMovingAverageChartMovingRangeLength"]],
                              ewmaLambda = options[["exponentiallyWeightedMovingAverageChartLambda"]], phase2 = phase2,
                              phase2Sd = options[["exponentiallyWeightedMovingAverageChartSdValue"]])$plotObject

  plot$plotObject <- plotObject
  return(plot)
}

