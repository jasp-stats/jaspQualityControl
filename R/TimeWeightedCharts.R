#' @export
timeWeightedChartsInternal <- function(jaspResults, dataset, options) {
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
           any(table(dplyr::count(dataset, dplyr::across(dplyr::all_of(c(stages, subgroupVariable))))[[subgroupVariable]]) > 1))) {
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

  # Create the rule list for the out-of-control signals
  if (ready)
    ruleList <- .getRuleListTimeWeightedCharts(options)


  #Cusum chart
  if (options[["cumulativeSumChart"]]) {
    cusumChart <- .Cusumchart(dataset = dataset, measurements = measurements, stages = stages,
                              axisLabels = axisLabels, options = options, ready = ready, ruleList = ruleList)
  }
  #EWMA chart
  if (options[["exponentiallyWeightedMovingAverageChart"]]) {
    ewmaPlot <- .EWMA(dataset = dataset, measurements = measurements, stages = stages,
                      axisLabels = axisLabels, options = options, ready = ready, ruleList = ruleList)
  }


  # Report
  if (options[["report"]]) {
    reportPlot <- createJaspPlot(title = gettext("Time weighted charts report"), width = 1250, height = 1000)
    jaspResults[["report"]] <- reportPlot
    jaspResults[["report"]]$dependOn(c("dataFormat", "measurementLongFormat", "subgroup", "stagesLongFormat", "measurementsWideFormat",
                                       "axisLabels", "stagesWideFormat", "subgroupSizeType", "manualSubgroupSizeValue",
                                       "groupingVariableMethod", "cumulativeSumChart", "cumulativeSumChartNumberSd",
                                       "cumulativeSumChartShiftSize", "cumulativeSumChartTarget", "cumulativeSumChartSdSource",
                                       "cumulativeSumChartSdMethod", "cumulativeSumChartSdValue", "cumulativeSumChartAverageMovingRangeLength",
                                       "exponentiallyWeightedMovingAverageChart", "exponentiallyWeightedMovingAverageChartSigmaControlLimits",
                                       "exponentiallyWeightedMovingAverageChartLambda", "exponentiallyWeightedMovingAverageChartSdSource",
                                       "exponentiallyWeightedMovingAverageChartSdMethod", "exponentiallyWeightedMovingAverageChartSdValue",
                                       "exponentiallyWeightedMovingAverageChartMovingRangeLength", "report", "reportMetaData",
                                       "reportTitle", "reportTitleText", "reportChartName", "reportChartNameText", "reportSubtitle",
                                       "reportSubtitleText", "reportMeasurementName", "reportMeasurementNameText", "reportFootnote",
                                       "reportFootnoteText", "reportLocation", "reportLocationText", "reportDate", "reportDateText",
                                       "reportPerformedBy", "reportPerformedByText", "reportPrintDate", "reportPrintDateText", .getDependenciesControlChartRules()))

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
      plots[["cusum"]] <- .timeWeightedReportChart(cusumChart, gettext("Cumulative sum chart"))
    if (options[["exponentiallyWeightedMovingAverageChart"]])
      plots[["ewma"]] <- .timeWeightedReportChart(ewmaPlot, gettext("Exponentially weighted moving average chart"))
    reportPlotObject <- .qcReport(text = text, plots = plots, textMaxRows = 8,
                                  reportTitle = title)
    reportPlot$plotObject <- reportPlotObject

    ###
    ### If not report mode
    ###
  } else {
    plotNote <- if (ready && !identical(plotNotes, "")) paste0("<i>Note.</i> ", plotNotes) else NULL
    if (options[["cumulativeSumChart"]]) {
      jaspResults[["CusumChart"]] <- createJaspContainer(position = 1)
      jaspResults[["CusumChart"]][["plot"]] <- cusumChart[["plot"]]
      if (!is.null(plotNote))
        jaspResults[["CusumChart"]][["plotNote"]] <- createJaspHtml(plotNote)
      jaspResults[["CusumChart"]][["table"]] <- cusumChart[["table"]]
    }
    if (options[["exponentiallyWeightedMovingAverageChart"]]) {
      jaspResults[["EWMAPlot"]] <- createJaspContainer(position = 2)
      jaspResults[["EWMAPlot"]][["plot"]] <- ewmaPlot[["plot"]]
      if (!is.null(plotNote))
        jaspResults[["EWMAPlot"]][["plotNote"]] <- createJaspHtml(plotNote)
      jaspResults[["EWMAPlot"]][["table"]] <- ewmaPlot[["table"]]
    }
  }
}

.Cusumchart <- function(dataset, measurements, stages, axisLabels, options, ready, ruleList) {

  plot <- createJaspPlot(title = gettext("Cumulative sum chart"), width = 1200, height = 500)
  plot$dependOn(c("dataFormat", "measurementLongFormat", "subgroup", "stagesLongFormat", "measurementsWideFormat",
                  "axisLabels", "stagesWideFormat", "subgroupSizeType", "manualSubgroupSizeValue",
                  "groupingVariableMethod", "cumulativeSumChart", "cumulativeSumChartNumberSd",
                  "cumulativeSumChartShiftSize", "cumulativeSumChartTarget", "cumulativeSumChartSdSource",
                  "cumulativeSumChartSdMethod", "cumulativeSumChartSdValue", "cumulativeSumChartAverageMovingRangeLength",
                  "report", .getDependenciesControlChartRules()))

  if (!ready)
    return(plot)


  columnsToPass <- c(measurements, stages)
  columnsToPass <- columnsToPass[columnsToPass != ""]
  phase2 <- (options[["cumulativeSumChartSdSource"]] == "historical")
  cusumChart <- tryCatch(
    .controlChart(dataset[columnsToPass], plotType = "cusum", stages = stages, xBarSdType = options[["cumulativeSumChartSdMethod"]],
                  nSigmasControlLimits = options[["cumulativeSumChartNumberSd"]], xAxisLabels = axisLabels,
                  cusumShiftSize = options[["cumulativeSumChartShiftSize"]], cusumTarget = options[["cumulativeSumChartTarget"]],
                  movingRangeLength = options[["cumulativeSumChartAverageMovingRangeLength"]], phase2 = phase2,
                  phase2Sd = options[["cumulativeSumChartSdValue"]], tableLabels = axisLabels, ruleList = ruleList),
    qcDataError = identity
  )
  return(.timeWeightedChartResult(plot, cusumChart))
}

.EWMA <- function(dataset, measurements, stages, axisLabels, options, ready, ruleList) {

  plot <-  createJaspPlot(title = gettext("Exponentially weighted moving average chart"), width = 1200, height = 500)
  plot$dependOn(c("dataFormat", "measurementLongFormat", "subgroup", "stagesLongFormat", "measurementsWideFormat",
                  "axisLabels", "stagesWideFormat", "subgroupSizeType", "manualSubgroupSizeValue",
                  "groupingVariableMethod", "exponentiallyWeightedMovingAverageChart",
                  "exponentiallyWeightedMovingAverageChartSigmaControlLimits", "exponentiallyWeightedMovingAverageChartLambda",
                  "exponentiallyWeightedMovingAverageChartSdSource", "exponentiallyWeightedMovingAverageChartSdMethod",
                  "exponentiallyWeightedMovingAverageChartSdValue", "exponentiallyWeightedMovingAverageChartMovingRangeLength",
                  "report", .getDependenciesControlChartRules()))

  if (!ready)
    return(plot)

  columnsToPass <- c(measurements, stages)
  columnsToPass <- columnsToPass[columnsToPass != ""]
  phase2 <- (options[["exponentiallyWeightedMovingAverageChartSdSource"]] == "historical")
  ewmaChart <- tryCatch(
    .controlChart(dataset[columnsToPass], plotType = "ewma", stages = stages, xBarSdType = options[["exponentiallyWeightedMovingAverageChartSdMethod"]],
                  nSigmasControlLimits = options[["exponentiallyWeightedMovingAverageChartSigmaControlLimits"]],
                  xAxisLabels = axisLabels, movingRangeLength = options[["exponentiallyWeightedMovingAverageChartMovingRangeLength"]],
                  ewmaLambda = options[["exponentiallyWeightedMovingAverageChartLambda"]], phase2 = phase2,
                  phase2Sd = options[["exponentiallyWeightedMovingAverageChartSdValue"]], tableLabels = axisLabels,
                  ruleList = ruleList),
    qcDataError = identity
  )
  return(.timeWeightedChartResult(plot, ewmaChart))
}

.getRuleListTimeWeightedCharts <- function(options) {
    ruleList <- list("rule1" = list("enabled" = options[["rule1"]]),
                     "rule2" = NULL,
                     "rule3" = NULL,
                     "rule4" = NULL,
                     "rule5" = NULL,
                     "rule6" = NULL,
                     "rule7" = NULL,
                     "rule8" = NULL,
                     "rule9" = NULL
    )
  return(ruleList)
}

# chart: .controlChart() output, or a "qcDataError" condition (e.g. no complete moving range), shown as the plot's error
.timeWeightedChartResult <- function(plot, chart) {
  if (inherits(chart, "qcDataError")) {
    errorMessage <- conditionMessage(chart)
    plot$setError(errorMessage)
    return(list("plot" = plot, "errorMessage" = errorMessage))
  }

  plot$plotObject <- chart$plotObject
  return(list("plot" = plot, "table" = chart$table))
}

.timeWeightedReportChart <- function(chartResult, chartTitle) {
  if (is.null(chartResult[["errorMessage"]]))
    return(chartResult[["plot"]]$plotObject)

  return(.qcReportErrorPanel(chartTitle, chartResult[["errorMessage"]]))
}
