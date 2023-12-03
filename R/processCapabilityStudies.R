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
  } else if (!wideFormat && options[["subgroupSizeType"]] == "manual"){
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
  plotNotes <- ""
  if (!wideFormat && options[["subgroupSizeType"]] == "groupingVariable" && anyNA(dataset[[subgroupVariable]])) {
    nDroppedSubgroupRows <- sum(is.na(dataset[[subgroupVariable]]))
    dataset <- dataset[!is.na(dataset[[subgroupVariable]]),]
    plotNotes <- paste0(plotNotes, gettextf("Removed %i observation(s) that were not assigned to any subgroups.<br>", nDroppedSubgroupRows))
  }

  # Rearrange data if not already wide format (one group per row)
  if (!wideFormat && ready) {
    # if subgroup size is set manual, use that. Else determine subgroup size from largest level in subgroups variable
    if (options[["subgroupSizeType"]] == "manual") {
      k <- options[["manualSubgroupSizeValue"]]
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

  # Error Handling
  if (ready) {
  .hasErrors(dataset, type = c('infinity'),
             all.target = measurements, exitAnalysisIfErrors = TRUE)
  }
  if (options[["capabilityStudyType"]] == "nonNormalCapabilityAnalysis" && ready) {
    .hasErrors(dataset,
               all.target = measurements,
               custom = function () {
                 if (any(unlist(dataset[measurements]) < 0))
                   return(gettext("Values must be positive to fit a Weibull/lognormal distribution."))},
               exitAnalysisIfErrors = TRUE)
  }

  # Plot note about R/S chart recommendation
  if (length(measurements) > 5 && options[["controlChartType"]] == "xBarR") # if the subgroup size is above 5, R chart is not recommended
    plotNotes <- paste0(plotNotes, gettext("Subgroup size is >5, results may be biased. S-chart is recommended."))


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
      jaspResults[["zeroWarning"]]$dependOn(c("measurementLongFormat", "measurementsWideFormat", 'capabilityStudyType', 'nullDistribution'))
    }
  }

  # Report
  if (options[["report"]]) {
    if (is.null(jaspResults[["pcReport"]])) {
      jaspResults[["pcReport"]] <- createJaspContainer(gettext("Report"))
      jaspResults[["pcReport"]]$position <- 6
    }
    jaspResults[["pcReport"]] <- .pcReport(dataset, measurements, parts, operators, options, ready, jaspResults, wideFormat, subgroups, axisLabels)
    jaspResults[["pcReport"]]$dependOn(c("report", "variables", "variablesLong", "subgroups", "controlChartType"))
  } else {
    # X-bar and R Chart OR ImR OR X-bar and mR Chart
    if(options[["controlChartType"]] == "xBarR" | options[["controlChartType"]] == "xBarMR"  | options[["controlChartType"]] == "xBarS") {
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
      jaspResults[["xBar"]]$dependOn(c("variables", "variablesLong", "subgroups", "controlChartType", "report"))
      jaspResults[["xBar"]]$position <- 1


      if (ready && is.null(jaspResults[["xBar"]][["plot"]])) {
        jaspResults[["xBar"]][["plot"]] <- createJaspPlot(title = gettextf("X-bar & %s control chart", secondPlotTitle),
                                                          width = 1200, height = 500)
        # Error conditions
        if (secondPlotType == "R" && length(measurements) > 50) { # if the subgroup size is above 50, the R package cannot calculate R charts.
          jaspResults[["xBar"]][["plot"]]$setError(gettext("Subgroup size is >50, R chart calculation is not possible. Use S-chart instead."))
          return()
        } else if(wideFormat && length(measurements) < 2) {
          jaspResults[["xBar"]][["plot"]]$setError(gettext("Subgroup size is 1, calculation of selected control charts not possible."))
          return()
        }
        columnsToPass <- c(measurements, stages)
        columnsToPass <- columnsToPass[columnsToPass != ""]
        fixedSubgroupSize <- if (options[["subgroupSizeUnequal"]] == "fixedSubgroupSize") options[["fixedSubgroupSizeValue"]] else ""
        unbiasingConstantUsed <- options[["controlChartSdUnbiasingConstant"]]
        xBarChart <- .controlChart(dataset = dataset[columnsToPass], plotType = "xBar", xBarSdType = sdType, stages = stages,
                                   xAxisLabels = axisLabels, fixedSubgroupSize = fixedSubgroupSize, unbiasingConstantUsed = unbiasingConstantUsed)
        secondPlot <- .controlChart(dataset = dataset[columnsToPass], plotType = secondPlotType, xAxisLabels = axisLabels, stages = stages,
                                    movingRangeLength = options[["xBarMovingRangeLength"]], fixedSubgroupSize = fixedSubgroupSize,
                                    unbiasingConstantUsed = unbiasingConstantUsed)
        jaspResults[["xBar"]][["plot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(secondPlot$plotObject, xBarChart$plotObject),
                                                                               layout = matrix(2:1, 2), removeXYlabels= "x")
        if (!identical(plotNotes, ""))
          jaspResults[["xBar"]][["plotNote"]] <- createJaspHtml(paste0("<i>Note.</i> ", plotNotes))
        jaspResults[["xBar"]][["tableXBar"]] <- xBarChart$table
        jaspResults[["xBar"]][["tableSecondPlot"]] <- secondPlot$table
      }
    } else if (options[["controlChartType"]] == "xmr") {
      jaspResults[["xmr"]] <- createJaspContainer(gettext("X-mR control chart"))
      jaspResults[["xmr"]]$dependOn(c("variables", "variablesLong", "subgroups", "controlChartType", "report"))
      jaspResults[["xmr"]]$position <- 1
      if (ready && is.null(jaspResults[["xmr"]][["plot"]])) {
        jaspResults[["xmr"]][["plot"]] <- createJaspPlot(title =  gettext("X-mR control chart"), width = 1200, height = 500)
        columnsToPass <- c(measurements, stages)
        columnsToPass <- columnsToPass[columnsToPass != ""]
        individualChart <- .controlChart(dataset = dataset[columnsToPass], plotType = "I", stages = stages,
                                                     xAxisLabels = seq_along(unlist(dataset[measurements])))
        mrChart <- .controlChart(dataset = dataset[columnsToPass], plotType = "MR", stages = stages,
                                 xAxisLabels = seq_along(unlist(dataset[measurements])), movingRangeLength = options[["xmrChartMovingRangeLength"]])
        jaspResults[["xmr"]][["plot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(mrChart$plotObject, individualChart$plotObject),
                                                                              layout = matrix(2:1, 2), removeXYlabels= "x")
        jaspResults[["xmr"]][["tableIndividual"]] <- individualChart$table
        jaspResults[["xmr"]][["tableMR"]] <- mrChart$table
      }
    }

    # Distribution plot - moved jaspResults ref here to avoid big files
    .qcDistributionPlot(options, dataset, ready, jaspResults, measurements, stages)

    # Probability plots section
    .qcProbabilityPlotContainer(options, dataset, ready, jaspResults, measurements, stages)

    # Perform capability analysis
    .qcCapabilityAnalysis(options, dataset, ready, jaspResults, measurements = measurements)
  }
}

#############################################################
## Functions for capability analysis section ################
#############################################################

################
## Containers ##
################

.qcCapabilityAnalysis <- function(options, dataset, ready, jaspResults, measurements) {

  container <- createJaspContainer(gettext("Capability study"))
  container$dependOn(options = c("CapabilityStudyType", "measurementsWideFormat", "subgroup", "lowerSpecificationLimitValue", "upperSpecificationLimitValue", "targetValue", "measurementLongFormat", "manualSubgroupSizeValue", "dataFormat",
                                 "processCapabilityPlot", "processCapabilityTable", "manualSubgroupSize", "report"))
  container$position <- 4

  if (!ready)
    return(container)


  if( sum(options[["upperSpecificationLimit"]], options[["lowerSpecificationLimit"]], options[["target"]]) >= 2){
    if (options[["lowerSpecificationLimit"]]){
      lowSpec <- options[["lowerSpecificationLimitValue"]]
      if (options[["upperSpecificationLimit"]] && options[["upperSpecificationLimitValue"]] < lowSpec){
        container$setError(gettext("Error: LSL > USL."))
      }
    }
    if (options[["target"]]){
      target <- options[["targetValue"]]
      if ((options[["upperSpecificationLimit"]] && options[["upperSpecificationLimitValue"]] < target) || (options[["lowerSpecificationLimit"]] && options[["lowerSpecificationLimitValue"]] > target)){
        container$setError(gettext("Error: Target outside of specification limits."))
      }
    }
  }

  ready <- (length(measurements) > 0 && (options[["lowerSpecificationLimit"]] | options[["upperSpecificationLimit"]]))

  jaspResults[["capabilityAnalysis"]] <- container

  if (options[["capabilityStudyType"]] == "normalCapabilityAnalysis") {

    normalContainer <- createJaspContainer(gettext("Process capability"))
    normalContainer$position <- 1
    container[["normalCapabilityAnalysis"]] <- normalContainer

    .qcProcessSummaryTable(options, dataset, ready, normalContainer, measurements)

    if (options[["processCapabilityPlot"]])
      .qcProcessCapabilityPlot(options, dataset, ready, normalContainer, measurements, distribution = 'normal')

    if (options[["processCapabilityTable"]]){
      .qcProcessCapabilityTableWithin(options, dataset, ready, normalContainer, measurements)
      .qcProcessCapabilityTableOverall(options, dataset, ready, normalContainer, measurements)
    }

  }

  if (options[["capabilityStudyType"]] == "nonNormalCapabilityAnalysis") {

    nonNormalContainer <- createJaspContainer(gettext("Process capability (non-normal capability analysis)"))
    nonNormalContainer$position <- 2

    container[["nonNormalCapabilityAnalysis"]] <- nonNormalContainer

    if (options[["processCapabilityPlot"]])
      .qcProcessCapabilityPlot(options, dataset, ready, nonNormalContainer, measurements, distribution = options[["nonNormalDistribution"]])

    if (options[["processCapabilityTable"]])
      .qcProcessCapabilityTableNonNormal(options, dataset, ready, nonNormalContainer, measurements)
  }
}

################
## Output ######
################

.qcProcessSummaryTable <- function(options, dataset, ready, container, measurements, returnDataframe = FALSE) {

  table <- createJaspTable(title = gettext("Process summary"))
  table$position <- 1

  if (options[["lowerSpecificationLimit"]]) {
    lslTitle <- if (options[["lowerSpecificationLimitBoundary"]]) gettext("LB") else gettext("LSL")
    table$addColumnInfo(name = "lsl", type = "number", title = lslTitle)
  }
  if (options[["target"]])
    table$addColumnInfo(name = "target", type = "number", title = gettext("Target"))
  if (options[["upperSpecificationLimit"]]) {
    uslTitle <- if (options[["upperSpecificationLimitBoundary"]]) gettext("UB") else gettext("USL")
    table$addColumnInfo(name = "usl", type = "number", title = uslTitle)
  }

  table$addColumnInfo(name = "n", type = "integer", title = gettext("Sample size"))
  table$addColumnInfo(name = "mean", type = "number", title = gettext("Average"))
  table$addColumnInfo(name = "sd", type = "number", title = gettext("Std. dev. (total)"))
  table$addColumnInfo(name = "sdw", type = "number", title = gettext("Std. dev. (within)"))

  table$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  # Take a look at this input! Is is supposed to be like this or must it be transposed?
  # Transposed gives NA often as std.dev
  if (length(measurements) < 2) {
    k <- options[["controlChartSdEstimationMethodMeanMovingRangeLength"]]
    sdw <- .controlChart_calculations(dataset[measurements], plotType = "MR", movingRangeLength = k)$sd
  } else {
    sdType <- if (options[["controlChartSdEstimationMethodGroupSizeLargerThanOne"]] == "rBar") "r" else "s"
    unbiasingConstantUsed <- options[["controlChartSdUnbiasingConstant"]]
    sdw <- .sdXbar(dataset[measurements], type = sdType, unbiasingConstantUsed = unbiasingConstantUsed)
  }
  allData <- na.omit(unlist(dataset[, measurements]))

  if (is.na(sdw))
    table$addFootnote(gettext("The within standard deviation could not be calculated."))

  rows <- list(
    "lsl"    = options[["lowerSpecificationLimitValue"]],
    "target" = options[["targetValue"]],
    "usl"    = options[["upperSpecificationLimitValue"]],
    "mean"   = mean(allData, na.rm = TRUE),
    "n"      = length(allData),
    "sd"     = sd(allData, na.rm = TRUE),
    "sdw"    = sdw
  )
  table$addRows(rows)

  nDecimals <- .numDecimals

  if (returnDataframe) {
    lslTitle <- if (options[["lowerSpecificationLimitBoundary"]]) gettext("LB") else gettext("LSL")
    uslTitle <- if (options[["upperSpecificationLimitBoundary"]]) gettext("UB") else gettext("USL")
    sourceVector <- c(lslTitle, 'Target', uslTitle, 'Sample size', 'Mean', "Std. dev. (total)", "Std. dev. (within)")
    lsl <- options[["lowerSpecificationLimitValue"]]
    target <- options[["targetValue"]]
    usl <- options[["upperSpecificationLimitValue"]]
    if (!options[["lowerSpecificationLimit"]])
      lsl <- '*'
    if (!options[["target"]])
      target <- '*'
    if (!options[["upperSpecificationLimit"]])
      usl <- '*'
    mean <- mean(allData, na.rm = TRUE)
    n <- as.integer(length(allData))
    sd <- sd(allData, na.rm = TRUE)
    valueVector <- c(lsl, target, usl, n, mean, sd, sdw)
    df <- data.frame(sources = sourceVector,
                     values = round(as.numeric(valueVector), nDecimals))
    return(df)
  }
  container[["processSummaryTable"]] <- table
}

.qcProcessCapabilityPlotObject <- function(options, dataset, measurements, distribution = c('normal', "weibull", "lognormal", "3ParameterLognormal", "3ParameterWeibull")) {

  # Take a look at this input! Is is supposed to be like this or must it be transposed?
  # Transposed gives NA often as std.dev
  if (length(measurements) < 2) {
    k <- options[["controlChartSdEstimationMethodMeanMovingRangeLength"]]
    sdw <- .controlChart_calculations(dataset[measurements], plotType = "MR", movingRangeLength = k)$sd
  } else {
    sdType <- if (options[["controlChartSdEstimationMethodGroupSizeLargerThanOne"]] == "rBar") "r" else "s"
    unbiasingConstantUsed <- options[["controlChartSdUnbiasingConstant"]]
    sdw <- .sdXbar(dataset[measurements], type = sdType, unbiasingConstantUsed = unbiasingConstantUsed)
  }
  allData <- as.vector(na.omit(unlist(dataset[, measurements])))
  plotData <- data.frame(x = allData)
  sdo <- sd(allData, na.rm = TRUE)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(plotData[["x"]], min(plotData[["x"]]) - 1 * sdo, max(plotData[["x"]]) + 1 * sdo), min.n = 4)
  xLimits <- range(xBreaks)
  nBins <- options[["processCapabilityPlotBinNumber"]]


  h <- hist(allData, plot = FALSE, breaks = nBins)
  binWidth <- (h$breaks[2] - h$breaks[1])

  p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(ggplot2::aes(y =..density..), closed = "left", fill = "grey", col = "black", linewidth = .7,
                            binwidth = binWidth, center = binWidth/2) +
    ggplot2::scale_y_continuous(name = gettext("Density")) +
    ggplot2::scale_x_continuous(name = gettext("Measurement"), breaks = xBreaks, limits = xLimits)

  legendColors <- c()
  legendLty <- c()
  legendLabels <- c()
  if (options[["processCapabilityPlotDistributions"]]) {
    if(distribution == "normal") {
      p <- p + ggplot2::stat_function(fun = dnorm, args = list(mean = mean(allData), sd = sd(allData)),
                                      mapping = ggplot2::aes(color = "sdoDist", linetype = "sdoDist")) +
        ggplot2::stat_function(fun = dnorm, args = list(mean = mean(allData), sd = sdw),
                               mapping = ggplot2::aes(color = "sdwDist", linetype = "sdwDist"))
      legendColors <- c(legendColors, "dodgerblue", "red")
      legendLty <- c(legendLty, "solid", "solid")
      legendLabels <- c(legendLabels, gettext("Normal dist.\n(std. dev. total)"),
                        gettext("Normal dist.\n(std. dev. within)"))

    } else if (distribution == "weibull") {
      shape <- .distributionParameters(data = allData, distribution = distribution)$beta
      scale <- .distributionParameters(data = allData, distribution = distribution)$theta
      p <- p + ggplot2::stat_function(fun = dweibull, args = list(shape = shape, scale = scale),
                                      mapping = ggplot2::aes(color = "weibull", linetype = "weibull"))
      legendColors <- c(legendColors, "red")
      legendLty <- c(legendLty, "solid")
      legendLabels <- c(legendLabels, gettext("Weibull dist."))
    } else if (distribution == "lognormal") {
      shape <- .distributionParameters(data = allData, distribution = distribution)$beta
      scale <- .distributionParameters(data = allData, distribution = distribution)$theta
      p <- p + ggplot2::stat_function(fun = dlnorm, args = list(meanlog = shape, sdlog = scale),
                                      mapping = ggplot2::aes(color = "lognormal", linetype = "lognormal"))
      legendColors <- c(legendColors, "red")
      legendLty <- c(legendLty, "solid")
      legendLabels <- c(legendLabels, "Lognormal dist.")
    } else if (distribution == "3ParameterLognormal") {
      shape <- .distributionParameters(data = allData, distribution = distribution)$theta
      scale <- .distributionParameters(data = allData, distribution = distribution)$beta
      threshold <- .distributionParameters(data = allData, distribution = distribution)$threshold
      p <- p + ggplot2::stat_function(fun = FAdist::dlnorm3 , args = list(shape = shape, scale = scale, thres = threshold),
                                      mapping = ggplot2::aes(color = "lognormal3", linetype = "lognormal3"))
      legendColors <- c(legendColors, "red")
      legendLty <- c(legendLty, "solid")
      legendLabels <- c(legendLabels, gettext("3-parameter\nlognormal dist."))
    } else if (distribution == "3ParameterWeibull") {
      shape <- .distributionParameters(data = allData, distribution = distribution)$beta
      scale <- .distributionParameters(data = allData, distribution = distribution)$theta
      threshold <- .distributionParameters(data = allData, distribution = distribution)$threshold
      p <- p + ggplot2::stat_function(fun = FAdist::dweibull3 , args = list(shape = shape, scale = scale, thres = threshold),
                                      mapping = ggplot2::aes(color = "weibull3", linetype = "weibull3"))
      legendColors <- c(legendColors, "red")
      legendLty <- c(legendLty, "solid")
      legendLabels <- c(legendLabels, gettext("3-parameter Weibull dist."))
    }
  }

  if (options[["processCapabilityPlotSpecificationLimits"]]) {
    specLimitsDf <- data.frame(matrix(ncol = 5, nrow = 0))
    colnames(specLimitsDf) <- c("label", "xIntercept", "lty", "yPosLabel", "color")
    yPosLabel <- max(ggplot2::layer_scales(p)$y$range$range)
    if (options[["target"]]) {
      specLimitsDf <- rbind(specLimitsDf, data.frame(label = gettext("Target"), xIntercept = options[["targetValue"]],
                                                     lty = "dotted", yPosLabel = yPosLabel, color = "darkgreen"))
    }
    if (options[["lowerSpecificationLimit"]]) {
      lslLty <- if (options[["lowerSpecificationLimitBoundary"]]) "solid" else "dotted"
      lslLabel <- if (options[["lowerSpecificationLimitBoundary"]]) "LB" else "LSL"
      specLimitsDf <- rbind(specLimitsDf, data.frame(label = lslLabel, xIntercept = options[["lowerSpecificationLimitValue"]],
                                                     lty = lslLty, yPosLabel = yPosLabel, color = "darkred"))
    }
    if (options[["upperSpecificationLimit"]]) {
      uslLty <- if (options[["upperSpecificationLimitBoundary"]]) "solid" else "dotted"
      uslLabel <- if (options[["upperSpecificationLimitBoundary"]]) "UB" else "USL"
      specLimitsDf <- rbind(specLimitsDf, data.frame(label = uslLabel, xIntercept = options[["upperSpecificationLimitValue"]],
                                                     lty = uslLty, yPosLabel = yPosLabel, color = "darkred"))
    }
    p <- p + ggplot2::geom_vline(data = specLimitsDf,
                                 mapping = ggplot2::aes(xintercept = xIntercept), color = specLimitsDf$color,
                                 linetype = specLimitsDf$lty, linewidth = 1) +
      ggplot2::geom_label(specLimitsDf, mapping = ggplot2::aes(x = xIntercept, y = yPosLabel, label = label), inherit.aes = FALSE,
                          size = 4.5)
  }
  if (options[["processCapabilityPlotDistributions"]] || options[["processCapabilityPlotSpecificationLimits"]]) {
    p <- p + ggplot2::scale_color_manual("", values = legendColors, labels = legendLabels) +
      ggplot2::scale_linetype_manual("", values = legendLty, labels = legendLabels)

  }
  p <- p + jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), legend.position = "right")

  return(p)
}

.qcProcessCapabilityPlot <- function(options, dataset, ready, container, measurements, distribution = c('normal', "weibull", "lognormal", "3ParameterLognormal", "3ParameterWeibull")) {

  plot <- createJaspPlot(title = gettext("Capability of the process"), width = 800, height = 500)
  plot$dependOn(c("csBinWidthType", "processCapabilityPlotBinNumber"))
  plot$position <- 2


  if(!options[["upperSpecificationLimit"]] && !options[["lowerSpecificationLimit"]]){
    plot$setError(gettext("No specification limits set."))
    return()
  }

  if (ready) plot$plotObject <- .qcProcessCapabilityPlotObject(options, dataset, measurements, distribution)

  container[["capabilityPlot"]] <- plot;

  return(plot)
}

.qcProcessCapabilityTableWithin <- function(options, dataset, ready, container, measurements, returnDataframe = FALSE) {

  if (!options[["lowerSpecificationLimit"]] && !options[["upperSpecificationLimit"]])
    return()

  if (!ready)
    return()

  table <- createJaspTable(title = gettext("Process capability (within)"))
  sourceVector <- vector()

  ciLevel <- options[["processCapabilityTableCiLevel"]]
  ciLevelPercent <- ciLevel * 100

  if (options[["lowerSpecificationLimit"]] && options[["upperSpecificationLimit"]]) {
    table$addColumnInfo(name = "cp", type = "integer", title = gettext("Cp"))
    sourceVector <- c(sourceVector, 'Cp')
    if (options[["processCapabilityTableCi"]] &&
        !(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])){
      table$addColumnInfo(name = "cplci", title = gettext("Lower"), type = "integer", overtitle = gettextf("%s CI for Cp", paste(ciLevelPercent, "%")))
      table$addColumnInfo(name = "cpuci", title = gettext("Upper"), type = "integer", overtitle = gettextf("%s CI for Cp", paste(ciLevelPercent, "%")))
    }
  }
  if (options[["lowerSpecificationLimit"]]){
    table$addColumnInfo(name = "cpl",   type = "integer", title = gettext("CpL"))
    sourceVector <- c(sourceVector, 'CpL')
  }
  if (options[["upperSpecificationLimit"]]){
    table$addColumnInfo(name = "cpu",   type = "integer", title = gettext("CpU"))
    sourceVector <- c(sourceVector, 'CpU')
  }
  table$addColumnInfo(name = "cpk",   type = "integer", title = gettext("Cpk"))
  sourceVector <- c(sourceVector, 'Cpk')
  if (options[["processCapabilityTableCi"]] &&
      !(options[["upperSpecificationLimitBoundary"]] && options[["lowerSpecificationLimitBoundary"]])) {
    table$addColumnInfo(name = "cpklci", title = gettext("Lower"), type = "integer", overtitle = gettextf("%s CI for Cpk", paste(ciLevelPercent, "%")))
    table$addColumnInfo(name = "cpkuci", title = gettext("Upper"), type = "integer", overtitle = gettextf("%s CI for Cpk", paste(ciLevelPercent, "%")))
  }
  table$showSpecifiedColumnsOnly <- TRUE
  if (options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])
    table$addFootnote(gettext("Statistics displayed as * were not calculated because the relevant specification limit is not set or set as boundary."))


  # Take a look at this input! Is is supposed to be like this or must it be transposed?
  # Transposed gives NA often as std.dev
  if (length(measurements) < 2) {
    k <- options[["controlChartSdEstimationMethodMeanMovingRangeLength"]]
    sdw <- .controlChart_calculations(dataset[measurements], plotType = "MR", movingRangeLength = k)$sd
  } else {
    sdType <- if (options[["controlChartSdEstimationMethodGroupSizeLargerThanOne"]] == "rBar") "r" else "s"
    unbiasingConstantUsed <- options[["controlChartSdUnbiasingConstant"]]
    sdw <- .sdXbar(dataset[measurements], type = sdType, unbiasingConstantUsed = unbiasingConstantUsed)
  }
  allData <- na.omit(unlist(dataset[, measurements]))

  # Calculate capability indices
  usl <- options[["upperSpecificationLimitValue"]]
  lsl <- options[["lowerSpecificationLimitValue"]]
  n <- length(allData)
  k <- length(measurements)
  tolMultiplier <- 6
  cp <- if (options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]]) NA else (usl - lsl) / (tolMultiplier * sdw)
  cpl <-if (options[["lowerSpecificationLimitBoundary"]]) NA else (mean(allData) - lsl) / ((tolMultiplier/2) * sdw)
  cpu <- if (options[["upperSpecificationLimitBoundary"]]) NA else (usl - mean(allData)) / ((tolMultiplier/2) * sdw)
  if (options[["lowerSpecificationLimit"]] && options[["upperSpecificationLimit"]]){
    cpk <-if (options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]]) NA else min(cpu, cpl, na.rm = TRUE)
  }else if(options[["lowerSpecificationLimit"]] && !options[["upperSpecificationLimit"]]){
    cpk <- cpl
  }else{
    cpk <- cpu
  }

  if (options[["processCapabilityTableCi"]]){
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

  rows <- list("cp" = round(cp, 2), "cpl" = round(cpl, 2), "cpu" = round(cpu, 2), "cpk" = round(cpk, 2))

  if (options[["processCapabilityTableCi"]]) {
    if (!(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])) {
      rows[["cplci"]] <- round(ciLbCp, 2)
      rows[["cpuci"]] <- round(ciUbCp, 2)
    }
    if (!(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])) {
      rows[["cpklci"]] <- round(ciLbCpk, 2)
      rows[["cpkuci"]] <- round(ciUbCpk, 2)
    }
  }

  rows[is.na(rows)] <- "*" # This looks better in the table and makes clearer that there is not an error
  table$addRows(rows)

  if(returnDataframe) {
    valueVector <- c()
    if(options[["lowerSpecificationLimit"]] && options[["upperSpecificationLimit"]])
      valueVector <- c(valueVector, rows[["cp"]])
    if(options[["lowerSpecificationLimit"]])
      valueVector <- c(valueVector, rows[["cpl"]])
    if(options[["upperSpecificationLimit"]])
      valueVector <- c(valueVector, rows[["cpu"]])
    valueVector <- c(valueVector, rows[["cpk"]])
    df <- data.frame(sources = sourceVector,
                     values = valueVector)
    return(df)
  }

  container[["capabilityTableWithin"]] <- table
}

.qcProcessCapabilityTableOverall <- function(options, dataset, ready, container, measurements, returnOverallCapDataframe = FALSE,
                                             returnPerformanceDataframe = FALSE) {

  if (!ready)
    return()

  table <- createJaspTable(title = gettext("Process performance (total)"))
  sourceVector1 <- vector()

  ciLevel <- options[["processCapabilityTableCiLevel"]]
  ciLevelPercent <- ciLevel * 100
  ciAlpha <- 1 - ciLevel

  if (options[["lowerSpecificationLimit"]] && options[["upperSpecificationLimit"]]){
    table$addColumnInfo(name = "pp",  type = "integer", title = gettext("Pp"))
    sourceVector1 <- c(sourceVector1, 'Pp')
    if (options[["processCapabilityTableCi"]] &&
        !(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])) {
      table$addColumnInfo(name = "pplci", title = gettext("Lower"), type = "integer", overtitle = gettextf("%s CI for Pp", paste(ciLevelPercent, "%")))
      table$addColumnInfo(name = "ppuci", title = gettext("Upper"), type = "integer", overtitle = gettextf("%s CI for Pp", paste(ciLevelPercent, "%")))
    }
  }
  if (options[["lowerSpecificationLimit"]]){
    table$addColumnInfo(name = "ppl", type = "integer", title = gettext("PpL"))
    sourceVector1 <- c(sourceVector1, 'PpL')
  }
  if (options[["upperSpecificationLimit"]]){
    table$addColumnInfo(name = "ppu", type = "integer", title = gettext("PpU"))
    sourceVector1 <- c(sourceVector1, 'PpU')
  }
  table$addColumnInfo(name = "ppk",   type = "integer", title = gettext("Ppk"))
  sourceVector1 <- c(sourceVector1, 'Ppk')
  if (options[["processCapabilityTableCi"]] &&
      !(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])) {
    table$addColumnInfo(name = "ppklci", title = gettext("Lower"), type = "integer", overtitle = gettextf("%s CI for Ppk", paste(ciLevelPercent, "%")))
    table$addColumnInfo(name = "ppkuci", title = gettext("Upper"), type = "integer", overtitle = gettextf("%s CI for Ppk", paste(ciLevelPercent, "%")))
  }
  if (options[["target"]]){
    table$addColumnInfo(name = "cpm", type = "integer", title = gettext("Cpm"))
    sourceVector1 <- c(sourceVector1, 'Cpm')
    if (options[["processCapabilityTableCi"]] &&
        !(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])) {
      table$addColumnInfo(name = "cpmlci", title = gettext("Lower"), type = "integer", overtitle = gettextf("%s CI for Cpm", paste(ciLevelPercent, "%")))
      table$addColumnInfo(name = "cpmuci", title = gettext("Upper"), type = "integer", overtitle = gettextf("%s CI for Cpm", paste(ciLevelPercent, "%")))
    }
  }
  table$showSpecifiedColumnsOnly <- TRUE
  if (options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])
    table$addFootnote(gettext("Statistics displayed as * were not calculated because the relevant specification limit is not set or set as boundary."))


  # Take a look at this input! Is is supposed to be like this or must it be transposed?
  # Transposed gives NA often as std.dev
  if (length(measurements) < 2) {
    k <- options[["controlChartSdEstimationMethodMeanMovingRangeLength"]]
    sdw <- .controlChart_calculations(dataset[measurements], plotType = "MR", movingRangeLength = k)$sd
  } else {
    sdType <- if (options[["controlChartSdEstimationMethodGroupSizeLargerThanOne"]] == "rBar") "r" else "s"
    unbiasingConstantUsed <- options[["controlChartSdUnbiasingConstant"]]
    sdw <- .sdXbar(dataset[measurements], type = sdType, unbiasingConstantUsed = unbiasingConstantUsed)
  }
  allData <- na.omit(unlist(dataset[, measurements]))
  sdo <- sd(allData)
  meanOverall <- mean(allData)
  usl <- options[["upperSpecificationLimitValue"]]
  lsl <- options[["lowerSpecificationLimitValue"]]
  m <- (options[["upperSpecificationLimitValue"]] + options[["lowerSpecificationLimitValue"]])/2
  n <- length(allData)
  t <- options[["targetValue"]]
  tolMultiplier <- 6

  pp <- if (options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]]) NA else (usl - lsl) / (tolMultiplier * sdo)
  ppl <- if (options[["lowerSpecificationLimitBoundary"]]) NA else (meanOverall - lsl) / ((tolMultiplier/2) * sdo)
  ppu <- if (options[["upperSpecificationLimitBoundary"]]) NA else (usl - mean(allData)) / ((tolMultiplier/2) * sdo)

  if (options[["lowerSpecificationLimit"]] && options[["upperSpecificationLimit"]]){
    ppk <- if (options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]]) NA else min(ppu, ppl, na.rm = TRUE)
  }else if(options[["lowerSpecificationLimit"]] && !options[["upperSpecificationLimit"]]){
    ppk <- ppl
  }else{
    ppk <- ppu
  }
  cp <- (usl - lsl) / (tolMultiplier * sdw)

  if (options[["lowerSpecificationLimit"]] && options[["upperSpecificationLimit"]] && options[["target"]]){
    if (t == m){
      cpm <- (usl - lsl) / (tolMultiplier * sqrt((sum((allData - t)^2)) / n))
    }else{
      cpm <- min(c(t - lsl, usl - t)) / ((tolMultiplier / 2) * sqrt((sum((allData - t)^2)) / n))
    }
  }else if (options[["upperSpecificationLimit"]] && options[["target"]]){
    cpm <- (usl - t) / ((tolMultiplier / 2) * sqrt((sum((allData - t)^2)) / n))
  }else if (options[["lowerSpecificationLimit"]] && options[["target"]]){
    cpm <- (t - lsl) / ((tolMultiplier / 2) * sqrt((sum((allData - t)^2)) / n))
  }
  if (options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])
    cpm <- NA

  if (options[["processCapabilityTableCi"]]) {
    if (!(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])) {
      #CI for Pp
      dfPp <- n - 1
      ciLbPp <- pp * sqrt( qchisq(p = ciAlpha/2, df = dfPp) /dfPp)
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
      a <- (meanOverall - t) / sdo
      dfCpm <- (n * ((1 + (a^2))^2)) / (1 + (2 * (a^2)))
      ciLbCpm <- cpm * sqrt( qchisq(p = ciAlpha/2, df = dfCpm) /dfCpm)
      ciUbCpm <- cpm * sqrt(qchisq(p = 1 - (ciAlpha/2), df = dfCpm) / dfCpm)
    }
  }

  rows <- list("pp" = round(pp,2), "ppl" = round(ppl,2), "ppu" = round(ppu,2), "ppk" = round(ppk,2))
  if (options[["target"]])
    rows[["cpm"]] <- round(cpm,2)

  if (options[["processCapabilityTableCi"]]) {
    if (!(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])) {
      rows[["pplci"]] <- round(ciLbPp,2)
      rows[["ppuci"]] <- round(ciUbPp,2)
    }
    if (!(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])) {
      rows[["ppklci"]] <- round(ciLbPpk,2)
      rows[["ppkuci"]] <- round(ciUbPpk,2)
      if (options[["target"]]) {
        rows[["cpmlci"]] <- round(ciLbCpm,2)
        rows[["cpmuci"]] <- round(ciUbCpm,2)
      }
    }
  }

  rows[is.na(rows)] <- "*" # This looks better in the table and makes clearer that there is not an error
  table$addRows(rows)

  if (returnOverallCapDataframe) {
    valueVector1 <- c()
    if (options[["lowerSpecificationLimit"]] && options[["upperSpecificationLimit"]])
      valueVector1 <- c(valueVector1, rows[["pp"]])
    if (options[["lowerSpecificationLimit"]])
      valueVector1 <- c(valueVector1, rows[["ppl"]])
    if (options[["upperSpecificationLimit"]])
      valueVector1 <- c(valueVector1, rows[["ppu"]])
    valueVector1 <- c(valueVector1, rows[["ppk"]])
    if (options[["target"]])
      valueVector1 <- c(valueVector1, rows[["cpm"]])
    df <- data.frame(sources = sourceVector1,
                     values = valueVector1)
    return(df)
  }

  table2 <- createJaspTable(title = gettext("Non-conformance statistics"))
  table2$addColumnInfo(name = "rowNames", type = "string", title = "")
  table2$addColumnInfo(name = "observed", type = "integer", title = "Observed")
  table2$addColumnInfo(name = "expOverall", type = "integer", title = "Expected total")
  table2$addColumnInfo(name = "expWithin", type = "integer", title = "Expected within")
  table2$showSpecifiedColumnsOnly <- TRUE
  if (options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])
    table2$addFootnote(gettext("Statistics displayed as * were not calculated because the relevant specification limit is not set or set as boundary."))


  #Calculate performance
  allDataVector <- as.vector(allData)
  lslTitle <- if (options[["lowerSpecificationLimitBoundary"]]) gettext("LB") else gettext("LSL")
  uslTitle <- if (options[["upperSpecificationLimitBoundary"]]) gettext("UB") else gettext("USL")
  rowNames <- c(sprintf("ppm < %s", lslTitle), sprintf("ppm > %s", uslTitle), "ppm total")

  #observed
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
    eoLSL <- 1e6 * (1 - pnorm((meanOverall - lsl)/sdo))
  }else{
    eoLSL <- NA
  }
  if (options[["upperSpecificationLimit"]] && !options[["upperSpecificationLimitBoundary"]]) {
    eoUSL <- 1e6 * (1 - pnorm((usl - meanOverall)/sdo))
  }else{
    eoUSL <- NA
  }
  eoTOT <- if (all(is.na(c(eoLSL, eoUSL)))) NA else sum(c(eoLSL, eoUSL), na.rm = TRUE)
  expOverall <- c(eoLSL, eoUSL, eoTOT)

  # expected within
  if (options[["lowerSpecificationLimit"]] && !options[["lowerSpecificationLimitBoundary"]]) {
    ewLSL <- 1e6 * (1 - pnorm((meanOverall - lsl)/sdw))
  }else{
    ewLSL <- NA
  }
  if (options[["upperSpecificationLimit"]] && !options[["upperSpecificationLimitBoundary"]]) {
    ewUSL <- 1e6 * (1 - pnorm((usl - meanOverall)/sdw))
  }else{
    ewUSL <- NA
  }
  ewTOT <- if (all(is.na(c(ewLSL, ewUSL)))) NA else sum(c(ewLSL, ewUSL), na.rm = TRUE)
  expWithin <- c(ewLSL, ewUSL, ewTOT)

  nDecimals <- .numDecimals
  if (returnPerformanceDataframe) {
    df <- data.frame("Source" = rowNames,
                     "Observed" = observed,
                     "Expected Overall" = round(expOverall, nDecimals),
                     "Expected Within"  = round(expWithin, nDecimals))
    df$Expected.Overall[is.na(df$Expected.Overall)] <- "*" # This looks better in the table and makes clearer that there is not an error
    df$Expected.Within[is.na(df$Expected.Within)] <- "*"
    df$Observed[is.na(df$Observed)] <- "*"
    return(df)
  }

  table2List <- list("rowNames" = rowNames,
                     "observed" = observed,
                     "expOverall" = round(expOverall, nDecimals),
                     "expWithin" = round(expWithin, nDecimals))
  table2List$expOverall[is.na(table2List$expOverall)] <- "*" # This looks better in the table and makes clearer that there is not an error
  table2List$expWithin[is.na(table2List$expWithin)] <- "*"
  table2List$observed[is.na(table2List$observed)] <- "*"
  table2$setData(table2List)

  container[["capabilityTableOverall"]] <- table
  container[["capabilityTablePerformance"]] <- table2

}

.qcProcessCapabilityTableNonNormal <- function(options, dataset, ready, container, measurements, returnSummaryDF = FALSE, returnCapabilityDF = FALSE,
                                               returnPerformanceDF = FALSE) {

  table <- createJaspTable(title = gettextf("Process summary"))

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

  sourceVector1 <- vector()

  lslTitle <- if (options[["lowerSpecificationLimitBoundary"]]) gettext("LB") else gettext("LSL")
  if (options[["lowerSpecificationLimit"]])
    table$addColumnInfo(name = "lsl", type = "number", title = lslTitle)

  if (options[["target"]])
    table$addColumnInfo(name = "target", type = "number", title = gettext("Target"))

  uslTitle <- if (options[["upperSpecificationLimitBoundary"]]) gettext("UB") else gettext("USL")
  if (options[["upperSpecificationLimit"]])
    table$addColumnInfo(name = "usl", type = "number", title = uslTitle)

  table$addColumnInfo(name = "n", type = "integer", title = gettext("Sample size"))
  table$addColumnInfo(name = "mean", type = "number", title = gettext("Average"))
  table$addColumnInfo(name = "sd", type = "number", title = gettext("Std. dev."))
  if(options[["nonNormalDistribution"]] == "3ParameterLognormal" | options[["nonNormalDistribution"]] == "lognormal"){
    table$addColumnInfo(name = "beta", type = "number", title = gettextf("Log mean (%1$s)", "\u03BC"))
    table$addColumnInfo(name = "theta", type = "number", title = gettextf("Log std.dev (%1$s)", "\u03C3"))
  }
  else{
    table$addColumnInfo(name = "beta", type = "number", title = gettextf("Shape (%1$s)", "\u03B2"))
    table$addColumnInfo(name = "theta", type = "number", title = gettextf("Scale (%1$s)", "\u03B8"))
  }
  sourceVector1 <- c(sourceVector1, lslTitle, 'Target', uslTitle, 'Sample size', 'Mean', 'Std. dev.', "Beta", "Theta")

  if(options[["nonNormalDistribution"]] == "3ParameterLognormal" | options[["nonNormalDistribution"]] == "3ParameterWeibull"){
    table$addColumnInfo(name = "threshold", type = "number", title = gettext('Threshold'))
    sourceVector1 <- c(sourceVector1, 'Threshold')
  }
  table$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  allData <- as.vector(na.omit(unlist(dataset[, measurements])))
  n <- length(allData)
  lsl <- options[["lowerSpecificationLimitValue"]]
  usl <- options[["upperSpecificationLimitValue"]]
  target <- options[["targetValue"]]
  sd <- sd(allData)
  mean <- mean(allData, na.rm = TRUE)

  distParameters <- .distributionParameters(data = allData, distribution = options[["nonNormalDistribution"]])
  beta <- distParameters$beta
  theta <- distParameters$theta

  rows <- list("n" = n,"mean" = mean, "sd" = sd, "lsl" = lsl,
               "usl" = usl, "target" = target, "beta" = beta, "theta" = theta)
  if(options[["nonNormalDistribution"]] == "3ParameterLognormal" | options[["nonNormalDistribution"]] == "3ParameterWeibull"){
    threshold <- distParameters$threshold
    rows[['threshold']] <- threshold
  }

  if(returnSummaryDF){
    if (!options[["lowerSpecificationLimit"]])
      lsl <- '*'
    if (!options[["target"]])
      target <- '*'
    if (!options[["upperSpecificationLimit"]])
      usl <- '*'
    valueVector <- c(lsl, target, usl, n, mean, sd, beta, theta)
    if(options[["nonNormalDistribution"]] == "3ParameterLognormal" | options[["nonNormalDistribution"]] == "3ParameterWeibull")
      valueVector <- c(valueVector, threshold)
    df <- data.frame(sources = sourceVector1,
                     values = valueVector)
    return(df)
  }

  table2 <- createJaspTable(title = gettextf("Process performance (total)"))

  sourceVector2 <- vector()

  if (options[["upperSpecificationLimit"]] && options[["lowerSpecificationLimit"]]){
    table2$addColumnInfo(name = "pp", type = "integer", title = gettext("Pp"))
    sourceVector2 <- c(sourceVector2, 'Pp')
  }
  if (options[["lowerSpecificationLimit"]]){
    table2$addColumnInfo(name = "ppl", type = "integer", title = gettext("PpL"))
    sourceVector2 <- c(sourceVector2, 'PpL')
  }
  if (options[["upperSpecificationLimit"]]){
    table2$addColumnInfo(name = "ppu", type = "integer", title = gettext("PpU"))
    sourceVector2 <- c(sourceVector2, 'PpU')
  }
  table2$addColumnInfo(name = "ppk", type = "integer", title = gettext("Ppk"))
  sourceVector2 <- c(sourceVector2, 'Ppk')

  table2data <- list()

  if (options[["nonNormalDistribution"]] == "lognormal") {
    if (options[["lowerSpecificationLimit"]] && !options[["lowerSpecificationLimitBoundary"]]){
      if(options[['nonNormalMethod' ]] == "nonConformance"){
        p1 <- plnorm(q = lsl, meanlog = beta, sdlog = theta, lower.tail = T)
        zLSL <- qnorm(p1)
        ppl <- -zLSL/3
      }else{
        x135 <- qlnorm(p = 0.00135, meanlog = beta, sdlog = theta)
        x05 <- qlnorm(p = 0.5, meanlog = beta, sdlog = theta)
        ppl <- (x05 - lsl) / (x05 - x135)
      }
    }else{
      ppl <- NA
    }
    table2data[["ppl"]] <- round(ppl,2)
    if (options[["upperSpecificationLimit"]]  && !options[["upperSpecificationLimitBoundary"]]){
      if(options[['nonNormalMethod' ]] == "nonConformance"){
        p2 <- plnorm(q = usl,  meanlog = beta, sdlog = theta)
        zUSL <- qnorm(p2)
        ppu <- zUSL/3
      }else{
        x99 <- qlnorm(p = 0.99865, meanlog = beta, sdlog = theta)
        x05 <- qlnorm(p = 0.5, meanlog = beta, sdlog = theta)
        ppu <- (usl - x05) / (x99 - x05)
      }
    }else{
      ppu <- NA
    }
    table2data[["ppu"]] <- round(ppu,2)
  }else if (options[["nonNormalDistribution"]] == "weibull") {
    if (options[["lowerSpecificationLimit"]]  && !options[["lowerSpecificationLimitBoundary"]]){
      if(options[['nonNormalMethod' ]] == "nonConformance"){
        p1 <- pweibull(q = lsl, shape = beta, scale = theta, lower.tail = T)
        zLSL <- qnorm(p1)
        ppl <- -zLSL/3
      }else{
        x135 <- qweibull(p = 0.00135, shape = beta, scale = theta)
        x05 <- qweibull(p = 0.5, shape = beta, scale = theta)
        ppl <- (x05 - lsl) / (x05 - x135)
      }

    }else{
      ppl <- NA
    }
    table2data[["ppl"]] <- round(ppl,2)
    if (options[["upperSpecificationLimit"]] && !options[["upperSpecificationLimitBoundary"]]){
      if(options[['nonNormalMethod' ]] == "nonConformance"){
        p2 <- pweibull(q = usl, shape = beta, scale = theta)
        zUSL <- qnorm(p2)
        ppu <- zUSL/3
      }else{
        x99 <- qweibull(p = 0.99865, shape = beta, scale = theta)
        x05 <- qweibull(p = 0.5, shape = beta, scale = theta)
        ppu <- (usl - x05) / (x99 - x05)
      }
    }else{
      ppu <- NA
    }
    table2data[["ppu"]] <- round(ppu,2)
  }else if (options[["nonNormalDistribution"]] == "3ParameterLognormal") {
    if (options[["lowerSpecificationLimit"]]  && !options[["lowerSpecificationLimitBoundary"]]) {
      if(options[['nonNormalMethod' ]] == "nonConformance"){
        p1 <- FAdist::plnorm3(q = lsl, shape = theta, scale = beta, thres = threshold, lower.tail = T)
        zLSL <- qnorm(p1)
        ppl <- -zLSL/3
      }else{
        x135 <- FAdist::qlnorm3(p = 0.00135, shape = theta, scale = beta, thres = threshold)
        x05 <- FAdist::qlnorm3(p = 0.5, shape = theta, scale = beta, thres = threshold)
        ppl <- (x05 - lsl) / (x05 - x135)
      }
    }else{
      ppl <- NA
    }
    table2data[["ppl"]] <- round(ppl,2)
    if (options[["upperSpecificationLimit"]]  && !options[["upperSpecificationLimitBoundary"]]){
      if(options[['nonNormalMethod' ]] == "nonConformance"){
        p2 <- FAdist::plnorm3(q = usl, shape = theta, scale = beta, thres = threshold)
        zUSL <- qnorm(p2)
        ppu <- zUSL/3
      }else{
        x99 <- FAdist::qlnorm3(p = 0.99865, shape = theta, scale = beta, thres = threshold)
        x05 <- FAdist::qlnorm3(p = 0.5, shape = theta, scale = beta, thres = threshold)
        ppu <- (usl - x05) / (x99 - x05)
      }
    }else{
      ppu <- NA
    }
    table2data[["ppu"]] <- round(ppu,2)
  }else if (options[["nonNormalDistribution"]] == "3ParameterWeibull") {
    if (options[["lowerSpecificationLimit"]]  && !options[["lowerSpecificationLimitBoundary"]]){
      if(options[['nonNormalMethod' ]] == "nonConformance"){
        p1 <- FAdist::pweibull3(q = lsl, shape = beta, scale = theta, thres = threshold, lower.tail = T)
        zLSL <- qnorm(p1)
        ppl <- -zLSL/3
      }else{
        x135 <- FAdist::qweibull3(p = 0.00135, shape = beta, scale = theta, thres = threshold)
        x05 <- FAdist::qweibull3(p = 0.5, shape = beta, scale = theta, thres = threshold)
        ppl <- (x05 - lsl) / (x05 - x135)
      }
    }else{
      ppl <- NA
    }
    table2data[["ppl"]] <- round(ppl,2)
    if (options[["upperSpecificationLimit"]]  && !options[["upperSpecificationLimitBoundary"]]){
      if(options[['nonNormalMethod' ]] == "nonConformance"){
        p2 <- FAdist::pweibull3(q = usl, shape = beta, scale = theta, thres = threshold)
        zUSL <- qnorm(p2)
        ppu <- zUSL/3
      }else{
        x99 <- FAdist::qweibull3(p = 0.99865, shape = beta, scale = theta, thres = threshold)
        x05 <- FAdist::qweibull3(p = 0.5, shape = beta, scale = theta, thres = threshold)
        ppu <- (usl - x05) / (x99 - x05)
      }
    }else{
      ppu <- NA
    }
  }
  table2data[["ppu"]] <- round(ppu,2)
  if (options[["upperSpecificationLimit"]] && options[["lowerSpecificationLimit"]] &&
      !(options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])) {
    if(options[['nonNormalMethod' ]] == "nonConformance"){
      pp <- (zUSL - zLSL)/6
    }else{
      pp <- (usl - lsl) / (x99 - x135)
    }
  }else{
    pp <- NA
  }
  table2data[["pp"]] <- round(pp,2)
  ppk <- if (all(is.na(c(ppl, ppu)))) NA else min(c(ppl, ppu), na.rm = T)
  table2data[["ppk"]] <- round(ppk,2)
  table2data[is.na(table2data)] <- "*" # This looks better in the table and makes clearer that there is not an error

  if(returnCapabilityDF) {
    valueVector <- c()
    if(options[["lowerSpecificationLimit"]] && options[["upperSpecificationLimit"]])
      valueVector <- c(table2data[["pp"]])
    if(options[["lowerSpecificationLimit"]])
      valueVector <- c(valueVector, table2data[["ppl"]])
    if(options[["upperSpecificationLimit"]])
      valueVector <- c(valueVector, table2data[["ppu"]])
    valueVector <- c(valueVector, table2data[["ppk"]])
    df <- data.frame(source = sourceVector2,
                     values = valueVector)
    return(df)
  }

  nDecimals <- .numDecimals
  table3 <- createJaspTable(title = gettextf("Non-conformance statistics"))

  table3$addColumnInfo(name = "rowNames", type = "string", title = "")
  table3$addColumnInfo(name = "observed", type = "integer", title = gettext("Observed"))
  table3$addColumnInfo(name = "expOverall", type = "integer", title = gettext("Expected total"))

  lslTitle <- if (options[["lowerSpecificationLimitBoundary"]]) gettext("LB") else gettext("LSL")
  uslTitle <- if (options[["upperSpecificationLimitBoundary"]]) gettext("UB") else gettext("USL")
  allDataVector <- as.vector(allData)
  rowNames <- c(paste0("ppm < ", lslTitle), paste0("ppm > ", uslTitle), "Total ppm")

  #observed
  if (options[["lowerSpecificationLimit"]]){
    oLSL <- (1e6*length(allDataVector[allDataVector < lsl])) / n
  }else{
    oLSL <- NA
  }
  if (options[["upperSpecificationLimit"]]){
    oUSL <- (1e6*length(allDataVector[allDataVector > usl])) / n
  }else{
    oUSL <- NA
  }
  oTOT <- sum(c(oLSL, oUSL), na.rm = TRUE)
  observed <- round(c(oLSL, oUSL, oTOT), nDecimals)
  observed[is.na(observed)] <- "*" # This looks better in the table and makes clearer that there is not an error

  # expected total
  if (options[["nonNormalDistribution"]] == "lognormal") {
    distname <- "lognormal"
    if (options[["lowerSpecificationLimit"]] && !options[["lowerSpecificationLimitBoundary"]]) {
      eoLSL <- 1e6 * plnorm(q = lsl, meanlog = beta, sdlog = theta, lower.tail = TRUE)
    }else{
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
      eoLSL <- 1e6 * pweibull(q = lsl, shape = beta, scale = theta, lower.tail = T)
    }else{
      eoLSL <- NA
    }
    if (options[["upperSpecificationLimit"]] && !options[["upperSpecificationLimitBoundary"]]) {
      eoUSL <- 1e6 * (1 - pweibull(q = usl, shape = beta, scale = theta))
    }else{
      eoUSL <- NA
    }
  }else if (options[["nonNormalDistribution"]] == "3ParameterLognormal") {
    distname <- "3-parameter-lognormal"
    if (options[["lowerSpecificationLimit"]] && !options[["lowerSpecificationLimitBoundary"]]){
      eoLSL <- 1e6 * FAdist::plnorm3(q = usl, shape = theta, scale = beta, thres = threshold, lower.tail = T)
    }else{
      eoLSL <- NA
    }
    if (options[["upperSpecificationLimit"]] && !options[["upperSpecificationLimitBoundary"]]){
      eoUSL <- 1e6 * (1 - FAdist::plnorm3(q = usl, shape = theta, scale = beta, thres = threshold))
    }else{
      eoUSL <- NA
    }
  }else if (options[["nonNormalDistribution"]] == "3ParameterWeibull") {
    distname <- "3-parameter-Weibull"
    if (options[["lowerSpecificationLimit"]] && !options[["lowerSpecificationLimitBoundary"]]) {
      eoLSL <- 1e6 * FAdist::pweibull3(q = usl, shape = beta, scale = theta, thres = threshold, lower.tail = T)
    }else{
      eoLSL <- NA
    }
    if (options[["upperSpecificationLimit"]] && !options[["upperSpecificationLimitBoundary"]]) {
      eoUSL <- 1e6 * (1 - FAdist::pweibull3(q = usl, shape = beta, scale = theta, thres = threshold))
    }else{
      eoUSL <- NA
    }
  }

  eoTOT <- if (!(options[["lowerSpecificationLimitBoundary"]] && options[["upperSpecificationLimitBoundary"]])) sum(c(eoLSL, eoUSL), na.rm = TRUE) else NA
  expOverall <- round(c(eoLSL, eoUSL, eoTOT), nDecimals)
  expOverall[is.na(expOverall)] <- "*" # This looks better in the table and makes clearer that there is not an error

  table3data <- list("rowNames" = rowNames, "observed" = observed, "expOverall" = expOverall)

  if(returnPerformanceDF){
    df <- data.frame("Source" = rowNames,
                     "Observed" = observed,
                     "Expected Overall" = expOverall)
    return(df)
  }

  table$addRows(rows)
  table$addFootnote(gettextf("Calculations based on %s distribution.", distname))
  container[["summaryTableNonNormal"]] <- table

  table2$setData(table2data)
  if (options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])
    table2$addFootnote(gettext("Statistics displayed as * were not calculated because the relevant specification limit is not set or set as boundary."))
  container[["overallCapabilityNonNormal"]] <- table2


  table3$setData(table3data)
  if (options[["lowerSpecificationLimitBoundary"]] || options[["upperSpecificationLimitBoundary"]])
    table3$addFootnote(gettext("Statistics displayed as * were not calculated because the relevant specification limit is not set or set as boundary."))
  container[["PerformanceNonNormal"]] <- table3


}

#############################################################
## Functions for probability plot section ###################
#############################################################

################
## Containers ##
################

.qcProbabilityPlotContainer <- function(options, dataset, ready, jaspResults, measurements, stages) {

  if (!options[["probabilityPlot"]] || !is.null(jaspResults[["probabilityContainer"]]))
    return()

  container <- createJaspContainer(gettext("Probability table and plot"))
  container$dependOn(options = c("measurementsWideFormat", "probabilityPlot", "probabilityPlotRankMethod", "nullDistribution", "probabilityPlotGridLines", "measurementLongFormat", "manualSubgroupSizeValue",
                                 "manualSubgroupSize", "subgroup", "report"))
  container$position <- 3

  jaspResults[["probabilityContainer"]] <- container

  if (!ready)
    return()

  .qcProbabilityTable(dataset, options, container, measurements)

  if (is.null(container[["ProbabilityPlot"]]))
    container[["ProbabilityPlot"]]  <- .qcProbabilityPlot(dataset, options, measurements, stages)
}

################
## Output ######
################

.qcProbabilityTable <- function(dataset, options, container, measurements) {

  table <- createJaspTable(title = gettextf("Summary of test against the %1$s distribution", options[["nullDistribution"]]))
  table$position <- 1

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

  table$addColumnInfo(name = "ad",     	title = gettext("AD"), type = "number")
  table$addColumnInfo(name = "p",		title = gettext("<i>p</i>-value"), type = "pvalue")

  table$addFootnote(gettextf("The Anderson-Darling statistic A<i>D</i> is calculated against the %2$s distribution.", "\u00B2", options[["nullDistribution"]]))
  table$addFootnote(gettextf("Red dotted lines in the probability plot below represent a 95%% confidence interval."))

  if (((options[["nullDistribution"]] == "lognormal") || options[["nullDistribution"]] == "weibull") &&
      any(na.omit(unlist(dataset[measurements])) < 0)){
    table$setError(gettext("Dataset contains negative numbers. Not compatible with the selected distribution."))
    container[["probabilityTable"]] <- table
    return()
  }

  values <- as.vector(na.omit(unlist(dataset[measurements]))) # distribution fitting function complains if this is not explicitly a vector

  if (options[["nullDistribution"]] == "normal") {
    meanx   <- mean(values)
    sdx     <- sd(values)
    test    <- goftest::ad.test(x = values, "norm", mean = meanx, sd = sdx)
  } else if (options[["nullDistribution"]] == "lognormal") {
    fit    <- fitdistrplus::fitdist(values, 'lnorm')
    meanx  <- fit$estimate[1]
    sdx    <- fit$estimate[2]
    test   <- goftest::ad.test(x = values, "plnorm", meanlog = meanx, sdlog = sdx)
  } else if (options[["nullDistribution"]] == "weibull") {
    fit    <- fitdistrplus::fitdist(values, 'weibull')
    meanx  <- fit$estimate[1]
    sdx    <- fit$estimate[2]
    test   <- goftest::ad.test(x = values, "pweibull", shape = meanx, scale = sdx)
  }

  n      <- length(values)
  ad     <- test$statistic
  adStar <- ad*(1 + (0.75/n) + (2.25/(n^2)))
  if(ad >= 0.6){
    p <- exp(1.2937 - (5.709 * adStar) + 0.0186 * (adStar^2))
  }else if(adStar < 0.6 && adStar > 0.34){
    p <- exp(0.9177 - (4.279 * adStar) - 1.38 * (adStar^2))
  }else if(adStar < 0.34 && adStar > 0.2){
    p <- 1 - exp(-8.318 + (42.796 * adStar) - 59.938 * (adStar^2))
  }else if(adStar <= 0.2){
    p <- 1 - exp(-13.436 + (101.14 * adStar) - 223.73 * (adStar^2))      #Jaentschi & Bolboaca (2018)
  } else {
    p <- test$p.value
  }

  row <- list(mean = meanx, sd = sdx, n = n, ad = ad, p = p)
  table$addRows(row)


  container[["probabilityTable"]] <- table
}

.qcProbabilityPlot <- function(dataset, options, measurements = NULL, stages, ggPlot = FALSE) {
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
  plot$dependOn(c("measurementLongFormat", "manualSubgroupSizeValue"))

  if (((options[["nullDistribution"]] == "lognormal") || options[["nullDistribution"]] == "weibull") &&
      any(na.omit(unlist(dataset[measurements])) < 0)) {
    plot$setError(gettext("Dataset contains negative numbers. Not compatible with the selected distribution."))
    return(plot)
  }

  plotList <- .qcProbabilityPlotObject(options, dataset, measurements, stages)
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
      lpdf <- quote(-log(sigma) - 0.5 / sigma ^ 2 * (X - mu) ^ 2)
      matrix <- mle.tools::observed.varcov(logdensity = lpdf, X = dataCurrentStage, parms = c("mu", "sigma"),
                                           mle = c(mean(dataCurrentStage), sd(dataCurrentStage)))
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
      fit <- fitdistrplus::fitdist(dataCurrentStage, 'lnorm')
      meanlog <- as.numeric(fit$estimate[1])
      sdlog <- as.numeric(fit$estimate[2])
      lpdf <- quote(log(1/(sqrt(2*pi)*X*sdlog) * exp(-(log(X)- meanlog)^2/(2*sdlog^2))))
      matrix <- mle.tools::observed.varcov(logdensity = lpdf, X = dataCurrentStage, parms = c("meanlog", "sdlog"), mle = fit$estimate)
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
      logLabels <- log(label_x)
      percentileEstimate <- log(percentileEstimate)
      percentileLower <- log(percentileLower)
      percentileUpper <- log(percentileUpper)

      labelFrame <- data.frame(labs = label_x, value = logLabels)
      index <- c(1,jaspGraphs::getPrettyAxisBreaks(1:nrow(labelFrame), 4)[-1])
      xBreaks <- labelFrame[index,2]
      label_x <- labelFrame[index,1]
      xLimits <- range(xBreaks)
    } else if (options[["nullDistribution"]] == "weibull") {
      fit <- fitdistrplus::fitdist(dataCurrentStage, 'weibull')
      shape <- as.numeric(fit$estimate[1])
      scale <- as.numeric(fit$estimate[2])
      lpdf <- quote(log(shape) - shape * log(scale) + shape * log(X) - (X / scale)^ shape )
      matrix <- mle.tools::observed.varcov(logdensity = lpdf, X = dataCurrentStage, parms = c("shape", "scale"), mle = fit$estimate)
      varShape <- matrix$varcov[1,1]
      varScale <- matrix$varcov[2,2]
      covarSS <- matrix$varcov[1,2]
      zp <- log(-1*log(1-pSeq))
      zalpha <- log(-1*log(1-0.975))
      percentileEstimate <- scale * (- log(1 - pSeq))^(1/shape)
      varPercentile <- (percentileEstimate^2 / scale^2) * varScale + (percentileEstimate^2/shape^4)*zp^2*varShape - 2*((zp*percentileEstimate^2) / (scale * shape^2))*covarSS
      percentileLower <- exp( log(percentileEstimate) - zalpha * (sqrt(varPercentile)/percentileEstimate))
      percentileUpper <- exp(log(percentileEstimate) + zalpha * (sqrt(varPercentile)/percentileEstimate))

      yBreaks <- log(-1*log(1-(ticks / 100)))
      logLabels <- log(label_x)
      percentileEstimate <- log(percentileEstimate)
      percentileLower <- log(percentileLower)
      percentileUpper <- log(percentileUpper)

      labelFrame <- data.frame(labs = label_x, value = logLabels)
      index <- c(1,jaspGraphs::getPrettyAxisBreaks(1:nrow(labelFrame), 4)[-1])
      xBreaks <- labelFrame[index,2]
      label_x <- labelFrame[index,1]
      xLimits <- range(xBreaks) * 1.2
    }
    data1 <- data.frame(x = dataCurrentStage, y = y)
    yLimits <- range(yBreaks)
    p <- ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileEstimate)) +
      jaspGraphs::geom_point(ggplot2::aes(x = dataCurrentStage, y = y))

    if (options[["probabilityPlotGridLines"]])
      p <- p + ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "lightgray"))
    if (nStages > 1)
      p <- p + ggplot2::ggtitle(stage) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

    p <- p + ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileLower), col = "darkred", linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileUpper), col = "darkred", linetype = "dashed") +
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

#############################################################
## Functions for distribution plot section ##################
#############################################################

.qcDistributionPlot <- function(options, dataset, ready, jaspResults, measurements, stages) {

  if(!options[["histogram"]] || !is.null(jaspResults[["histogram"]]))
    return()

  # calculating plot dimensions based on number of stages
  if (identical(stages, "")) {
    nStages <- 1
  } else if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
  }
  nCol <- if (nStages > 1) 2 else 1
  nRow <- ceiling(nStages/2)
  plotWidth <- nCol * 400
  plotHeight <- nRow * 400
  plot <- createJaspPlot(title = gettext("Histogram"), width = plotWidth, height = plotHeight)
  plot$dependOn(options = c("histogram", "histogramDensityLine", "measurementsWideFormat", "histogramBinNumber", "pcBinWidthType", "report", "measurementLongFormat", "manualSubgroupSizeValue", "manualSubgroupSize", "subgroup", 'nullDistribution'))
  plot$position <- 2
  jaspResults[["histogram"]] <- plot

  if (!ready)
    return()

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
    dataCurrentStage <- as.vector(na.omit(unlist(dataCurrentStage))) # the distribution fitting functions complain if this is not explicitly a vector
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
      ggplot2::geom_histogram(data = df, mapping = ggplot2::aes(y =..density.., x = measurements), closed = "left",
                              fill = "grey", col = "black", linewidth = .7, binwidth = binWidth, center = binWidth/2) +
      ggplot2::scale_x_continuous(name = gettext("Measurement"), breaks = xBreaks, limits = xLimits) +
      ggplot2::scale_y_continuous(name =  gettext("Counts"), labels = yLabels, breaks = yBreaks, limits = yLimits) +
      jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()
    if (nStages > 1)
      p <- p + ggplot2::ggtitle(stage) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

    if (options[["histogramDensityLine"]]) {
      if (options[['nullDistribution']]  == "normal") {
        p <- p + ggplot2::stat_function(fun = dnorm, args = list(mean = mean(dataCurrentStage), sd = sd(dataCurrentStage)),
                                        color = "dodgerblue")
      } else if (options[['nullDistribution']]  == "weibull") {
        fit_Weibull <- fitdistrplus::fitdist(dataCurrentStage, "weibull", method = "mle",
                                             control = list(maxit = 500, abstol = .Machine$double.eps, reltol = .Machine$double.eps))
        shape <- fit_Weibull$estimate[[1]]
        scale <- fit_Weibull$estimate[[2]]
        p <- p + ggplot2::stat_function(fun = dweibull, args = list(shape = shape, scale = scale), color = "dodgerblue")
      } else if(options[['nullDistribution']]  == "lognormal") {
        fit_Lnorm <- fitdistrplus::fitdist(dataCurrentStage, "lnorm", method = "mle",
                                           control = list(maxit = 500, abstol = .Machine$double.eps, reltol = .Machine$double.eps))
        shape <- fit_Lnorm$estimate[[1]]
        scale <- fit_Lnorm$estimate[[2]]
        p <- p + ggplot2::stat_function(fun = dlnorm, args = list(meanlog = shape, sdlog = scale), color = "dodgerblue")
      }
    }
    plotList[[i]] <- p
  }
  return(plotList)
}

.PClongTowide<- function(dataset, k, measurements, mode = c("manual", "subgroups")){
  if(identical(mode, "manual")){
    dataset <- dataset[measurements]
    n <- nrow(dataset)
    nGroups <- n/k

    if (nGroups != as.integer(nGroups)){
      return("error")
    }
    groupVector <- paste("V", 1:k, sep = "")
    group <- rep(groupVector, nGroups)
    wideDataset <- data.frame(row = 1:nGroups)
    dataset <- cbind(dataset, group)
    for(g in groupVector){
      groupData <- data.frame(x = dataset[[measurements]][dataset$group == g])
      colnames(groupData) <- g
      wideDataset <- cbind(wideDataset, groupData)
    }
    wideDataset <- wideDataset[groupVector]
    return(wideDataset)
  }else{
    nrep <- table(dataset[k])[[1]]
    dataset <- dataset[order(dataset[k]),]
    dataset <- cbind(dataset, data.frame(rep = rep(paste("V", 1:nrep, sep = ""))))
    wideDataset <- tidyr::spread(dataset, rep, measurements)
    return(wideDataset)
  }
}


.pcReport <- function(dataset, measurements, parts, operators, options, ready, container, wideFormat, subgroups, axisLabels) {

  if (options[["reportTitle"]] == ""){
    title <- "Process Capability Report"
  }else{
    title <- options[["reportTitle"]]
  }
  name <- gettextf("Process name: %s", options[["reportProcessName"]])
  date <- gettextf("Date of study: %s", options[["reportDate"]])
  text1 <- c(name, date)

  reportedBy <- gettextf("Reported by: %s", options[["reportReportedBy"]])
  misc <- gettextf("Misc: %s", options[["reportMiscellaneous"]])
  text2 <- c(reportedBy, misc)

  if(!options[["upperSpecificationLimit"]] && !options[["lowerSpecificationLimit"]]){
    plot <- createJaspPlot(title = gettext("Report"), width = 1200, height = 1000)
    plot$setError(gettext("No specification limits set."))
    return(plot)
  }

  if(!ready) {
    plot <- createJaspPlot(title = gettext("Report"), width = 1200, height = 1000)
    return(plot)
  }

  plotList <- list()
  indexCounter <- 0

  if (options[["reportMetaData"]]) {
    indexCounter <- indexCounter + 1
    plotList[[indexCounter]] <- .ggplotWithText(text1)
    indexCounter <- indexCounter + 1
    plotList[[indexCounter]] <- .ggplotWithText(text2)
  }
  if (options[["reportProcessStability"]]) {
    # X-bar and second chart OR ImR Chart
    if (options[["controlChartType"]] == "xmr"){
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- .controlChart(dataset = dataset[measurements], plotType = "I",
                                                xAxisLabels = seq_along(unlist(dataset[measurements])))$plotObject
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- .controlChart(dataset = dataset[measurements], plotType = "MR", xAxisLabels = seq_along(unlist(dataset[measurements])),
                                                movingRangeLength = options[["xmrChartMovingRangeLength"]])$plotObject
    } else {
      secondPlotType <- switch(options[["controlChartType"]],
                               "xBarR" = "R",
                               "xBarS" = "s",
                               "xBarMR" = "MMR")
      sdType <- switch(options[["controlChartType"]],
                       "xBarR" = "r",
                       "xBarS" = "s",
                       "xBarMR" = "r")
      fixedSubgroupSize <- if (options[["subgroupSizeUnequal"]] == "fixedSubgroupSize") options[["fixedSubgroupSizeValue"]] else ""
      indexCounter <- indexCounter + 1
      unbiasingConstantUsed <- options[["controlChartSdUnbiasingConstant"]]
      plotList[[indexCounter]] <- .controlChart(dataset = dataset[measurements], plotType = "xBar", xBarSdType = sdType,
                                                xAxisLabels = axisLabels, fixedSubgroupSize = fixedSubgroupSize,
                                                unbiasingConstantUsed = unbiasingConstantUsed)$plotObject
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- .controlChart(dataset = dataset[measurements], plotType = secondPlotType, xAxisLabels = axisLabels,
                                                movingRangeLength = options[["xBarMovingRangeLength"]], fixedSubgroupSize = fixedSubgroupSize,
                                                unbiasingConstantUsed = unbiasingConstantUsed)$plotObject
    }
  }
  if (options[["reportProcessCapabilityPlot"]]) {
    indexCounter <- indexCounter + 1
    plotList[[indexCounter]] <- .qcProcessCapabilityPlotObject(options, dataset, measurements, distribution = "normal")
  }
  if (options[["reportProbabilityPlot"]]) {
    indexCounter <- indexCounter + 1
    plotList[[indexCounter]] <- .qcProbabilityPlot(dataset, options, measurements, ggPlot = TRUE)
  }
  if (options[["reportProcessCapabilityTables"]]) {
    if (options[["capabilityStudyType"]] == "normalCapabilityAnalysis"){
      processSummaryDF <- .qcProcessSummaryTable(options, dataset, ready, container, measurements, returnDataframe = TRUE)
      potentialWithinDF <- .qcProcessCapabilityTableWithin(options, dataset, ready, container, measurements, returnDataframe = TRUE)
      overallCapDF <- .qcProcessCapabilityTableOverall(options, dataset, ready, container, measurements, returnOverallCapDataframe = TRUE)
      performanceDF <- .qcProcessCapabilityTableOverall(options, dataset, ready, container, measurements, returnPerformanceDataframe = TRUE)

      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- ggplotTable(processSummaryDF) #process summary
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- ggplotTable(performanceDF, displayColNames = TRUE)   # performance
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- ggplotTable(potentialWithinDF)  #Potential within
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- ggplotTable(overallCapDF) #overall capability
    } else {
      processSummaryDF <- .qcProcessCapabilityTableNonNormal(options, dataset, ready, container, measurements, returnSummaryDF = TRUE)
      overallCapDF <- .qcProcessCapabilityTableNonNormal(options, dataset, ready, container, measurements, returnCapabilityDF = TRUE)
      performanceDF <- .qcProcessCapabilityTableNonNormal(options, dataset, ready, container, measurements, returnPerformanceDF = TRUE)

      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- ggplotTable(processSummaryDF) #process summary
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- ggplotTable(performanceDF, displayColNames = TRUE)   # performance
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- ggplotTable(overallCapDF)  #overall capability
    }
  }

  if (indexCounter == 0) {
    plot <- createJaspPlot(title = title, width = 400, height = 400)
    plot$setError(gettext("No report components selected."))
    return(plot)
  } else if (indexCounter %% 2 != 0){
    indexCounter <- indexCounter + 1
    plotList[[indexCounter]] <- ggplot2::ggplot() + ggplot2::theme_void()
  }


  matrixNCols <- 2
  matrixNRows <- indexCounter / matrixNCols
  matrixPlot <- createJaspPlot(title = title, width = 1200, height = 400 * matrixNRows)
  plotMat <- matrix(plotList, matrixNRows, matrixNCols, byrow = TRUE)
  p <- jaspGraphs::ggMatrixPlot(plotMat)
  matrixPlot$plotObject <- p

  return(matrixPlot)
}

ggplotTable <- function(dataframe, displayColNames = FALSE){
  df <- tibble::tibble(dataframe)
  p <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggpp::geom_table(data = data.frame(x = 1, y = 1), ggplot2::aes(x = x, y = y), label = list(df),
                                                                    table.colnames = displayColNames, size = 7)

  return(p)
}

.distributionParameters <- function(data, distribution = c("lognormal", "weibull", "3ParameterLognormal", "3ParameterWeibull")){
  if (distribution == "lognormal") {
    fit_Lnorm <- fitdistrplus::fitdist(data, "lnorm", method = "mle",
                                       control = list(maxit = 500, abstol = .Machine$double.eps, reltol = .Machine$double.eps))
    beta <- fit_Lnorm$estimate[[1]]
    theta <- fit_Lnorm$estimate[[2]]
  } else if (distribution == "weibull") {
    fit_Weibull <- fitdistrplus::fitdist(data, "weibull", method = "mle",
                                         control = list(maxit = 500, abstol = .Machine$double.eps, reltol = .Machine$double.eps))
    beta <- fit_Weibull$estimate[[1]]
    theta <- fit_Weibull$estimate[[2]]
  }else if(distribution == "3ParameterLognormal"){
    temp <- EnvStats::elnorm3(data)
    beta <- temp$parameters[[1]]
    theta <- temp$parameters[[2]]
    threshold <- temp$parameters[[3]]
  }else if(distribution == "3ParameterWeibull"){
    temp <- weibullness::weibull.mle(data)
    beta <- temp[[1]]
    theta <- temp[[2]]
    threshold <- as.vector(temp[[3]])
  }
  list <- list(beta = beta,
               theta = theta)
  if(distribution == '3ParameterWeibull' | distribution == "3ParameterLognormal")
    list['threshold'] <- threshold
  return(list)
}

