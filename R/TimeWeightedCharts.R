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
  if (options[["cumulativeSumChart"]] && is.null(jaspResults[["CusumPlot"]])) {
    jaspResults[["CusumPlot"]] <- .Cusumchart(dataset = dataset, measurements = measurements, stages = stages,
                                              axisLabels = axisLabels, options = options, ready = ready)
  }
  #EWMA chart
  if (options[["exponentiallyWeightedMovingAverageChart"]] && is.null(jaspResults[["EWMAPlot"]])) {
    jaspResults[["EWMAPlot"]] <- createJaspPlot(title = gettext("Exponentially weighted moving average chart"), width = 1200, height = 500)
    jaspResults[["EWMAPlot"]]$dependOn(c("ExponentiallyWeightedMovingAverageChart", "measurements"))
    jaspResults[["EWMAPlot"]]$plotObject <- .EWMA(dataset = dataset, options = options, ready = ready)
  }
}

.Cusumchart <- function(dataset, measurements, stages, axisLabels, options, ready) {

  plot <- createJaspPlot(title = gettext("Cumulative sum chart"), width = 1200, height = 500)
  plot$dependOn(c("cumulativeSumChart", "measurements"))

  if (!ready)
    return(plot)


  columnsToPass <- c(measurements, stages)
  columnsToPass <- columnsToPass[columnsToPass != ""]
  plotObject <- .controlChart(dataset[columnsToPass], plotType = "cusum", stages = stages, xBarSdType = options[["cumulativeSumChartSdMethod"]],
                              nSigmasControlLimits = options[["cumulativeSumChartNumberSd"]], xAxisLabels = axisLabels,
                              cusumShiftSize = options[["cumulativeSumChartShiftSize"]], cusumTarget = options[["cumulativeSumChartTarget"]],
                              movingRangeLength = options[["averageMovingRangeLength"]])$plotObject

  plot$plotObject <- plotObject
  #
  # data1 <- dataset[, options[["measurements"]]]
  # sixsigma <- qcc::cusum(data1, decision.interval = options[["cumulativeSumChartNumberSd"]], se.shift = options[["cumulativeSumChartShiftSize"]], plot = FALSE)
  # subgroups <- c(1:length(sixsigma$pos))
  # data_plot <- data.frame(y_neg = sixsigma$neg , y_pos = sixsigma$pos, x = subgroups)
  # center <- 0
  # UCL <- sixsigma$decision.interval
  # LCL <- -UCL
  # yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL, data_plot$y_neg,  data_plot$y_pos))
  # yLimits <- range(yBreaks)
  # if (length(subgroups) > 60)
  #   xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  # else
  #   xBreaks <- c(subgroups)
  # xLimits <- c(1,max(xBreaks) + 2.5)
  # dfLabel <- data.frame(
  #   x = max(xLimits - 1),
  #   y = c(center, UCL, LCL),
  #   l = c(
  #     gettextf("CL = %g", round(center, 4)),
  #     gettextf("UCL = %g",   round(UCL, 5)),
  #     gettextf("LCL = %g",   round(LCL, 5))
  #   )
  # )
  #
  # p <- ggplot2::ggplot(data_plot, ggplot2::aes(x))  +
  #   ggplot2::geom_hline(yintercept =  center, color = 'green') +
  #   ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
  #   ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
  #   ggplot2::scale_y_continuous(name = gettext("Cumulative sum") ,limits = yLimits, breaks = yBreaks) +
  #   ggplot2::scale_x_continuous(name = gettext('Subgroups'), breaks = xBreaks, limits = range(xLimits)) +
  #   jaspGraphs::geom_line(ggplot2::aes(y = y_neg), col = "blue") +
  #   jaspGraphs::geom_line(ggplot2::aes(y = y_pos), col = "blue")+
  #   jaspGraphs::geom_point(ggplot2::aes(y = y_neg), size = 4, fill = ifelse(data_plot$y_neg < LCL, 'red', 'blue')) +
  #   jaspGraphs::geom_point(ggplot2::aes(y = y_pos), size = 4, fill = ifelse(data_plot$y_pos > UCL, 'red', 'blue')) +
  #   jaspGraphs::geom_rangeframe() +
  #   jaspGraphs::themeJaspRaw()

  return(plot)
}
.EWMA <- function(dataset, options, ready) {
  if (!ready)
    return()
  decimals <- .numDecimals
  data1 <- dataset[, options[["measurements"]]]
  sixsigma <- qcc::ewma(data1, center = options[["exponentiallyWeightedMovingAverageChartCenter"]] , lambda = options[["exponentiallyWeightedMovingAverageChartLambda"]],
                        std.dev = options[["exponentiallyWeightedMovingAverageChartSd"]], nsigmas = options[["exponentiallyWeightedMovingAverageChartSigmaControlLimits"]], plot = FALSE)
  subgroups <- 1:length(sixsigma$sizes)
  center <- sixsigma$center
  UCL <-  sixsigma$limits[,2]
  LCL <- sixsigma$limits[,1]
  data_plot <- data.frame(y = sixsigma$y, x = subgroups, UCL = UCL, LCL = LCL)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL, data_plot$y))
  yLimits <- range(yBreaks)
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks-0.5) * 1.15)
  UCL.label <- center + options[["exponentiallyWeightedMovingAverageChartSigmaControlLimits"]] * sqrt(options[["exponentiallyWeightedMovingAverageChartLambda"]] / (2-options[["exponentiallyWeightedMovingAverageChartLambda"]])) * options[["exponentiallyWeightedMovingAverageChartSd"]]
  LCL.label <- center - options[["exponentiallyWeightedMovingAverageChartSigmaControlLimits"]] * sqrt(options[["exponentiallyWeightedMovingAverageChartLambda"]] / (2-options[["exponentiallyWeightedMovingAverageChartLambda"]])) * options[["exponentiallyWeightedMovingAverageChartSd"]]
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center, UCL.label, LCL.label),
    l = c(
      gettextf("CL = %g", round(center, decimals + 1)),
      gettextf("UCL = %g",   round(UCL.label, decimals + 2)),
      gettextf("LCL = %g",   round(LCL.label, decimals + 2))
    )
  )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_step(ggplot2::aes(x = x, y = UCL, color = "red"),linetype = "dashed", size = 1.5) +
    ggplot2::geom_step(ggplot2::aes(x = x, y = LCL, color = "red"), linetype = "dashed", size = 1.5) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("EWMA") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Subgroup'), breaks = xBreaks, limits = range(xLimits)) +
    ggplot2::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$y > UCL | data_plot$y < LCL, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(p)
}
.Gchart <- function(dataset, options, ready){
  if (!ready)
    return()

  data1 <- dataset[, options[["measurements"]]]
  subgroups <- c(1:length(data1))
  data_plot <- data.frame(x =  subgroups, y = data1)
  center = mean(data1)
  UCL = center+3*sqrt(center*(center + 1))
  LCL = center-3*sqrt(center*(center + 1))
  LCL <- ifelse(LCL < 0 , 0, LCL)
  sixsigma <- list(statistics = data1, limits = data.frame(LCL, UCL), center = center)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL, data_plot$y))
  yLimits <- range(yBreaks)
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks) + 2.5)
  dfLabel <- data.frame(
    x = max(xLimits - 1),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 4)),
      gettextf("UCL = %g",   round(UCL, 5)),
      gettextf("LCL = %g",   round(LCL, 5))
    )
  )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = UCL, color = "red"),linetype = "dashed", size = 1.5) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = LCL, color = "red"), linetype = "dashed", size = 1.5) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Counts") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Subgroup'), breaks = xBreaks, limits = range(xLimits)) +
    ggplot2::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$y > UCL | data_plot$y < LCL, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(list(p = p, sixsigma = sixsigma))
}
.Tchart <- function(dataset, options){
  if (!ready)
    return()

  data1 <- dataset[, options[["measurements"]]]
  subgroups <- c(1:length(data1))
  data_plot <- data.frame(x = subgroups , y = data1^0.2777)
  data2 <- data.frame(process = data1)
  MR_T <- qcc::qcc(matrix(cbind(data2$process[1:length(data2$process)-1], data2$process[2:length(data2$process)]), ncol=2)
                   , type="R", plot = FALSE)$statistics
  center = mean(data_plot$y)^3.6
  UCL = (mean(data_plot$y) + 2.66 * mean(MR_T))^3.6
  LCL = (mean(data_plot$y, na.rm = TRUE) - 2.66 * mean(MR_T, na.rm =))^3.6
  LCL <- ifelse(LCL < 0 , 0, LCL)
  sixsigma <- list(statistics = data1, limits = data.frame(LCL, UCL), center = center)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL, data_plot$y))
  yLimits <- range(yBreaks)
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks) + 2.5)
  dfLabel <- data.frame(
    x = max(xLimits - 1),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 4)),
      gettextf("UCL = %g",   round(UCL, 5)),
      gettextf("LCL = %g",   round(LCL, 5))
    )
  )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = UCL, color = "red"),linetype = "dashed", linewidth = 1.5) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = LCL, color = "red"), linetype = "dashed", linewidth = 1.5) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l), inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Counts") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Subgroup'), breaks = xBreaks, limits = range(xLimits)) +
    ggplot2::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$y > UCL | data_plot$y < LCL, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(list(p = p, sixsigma = sixsigma))
}
