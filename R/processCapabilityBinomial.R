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

# Binomial (attribute) capability analysis.
#
# Reached from processCapabilityStudies() when capabilityDataType == "attributes". Each inspected
# unit is pass/fail, the data are counts of defectives per sample, and capability is expressed as
# %Defective, PPM defective and Process Z instead of Cp/Cpk/Pp/Ppk. Stages are not supported here.

.qcBinomialCapability <- function(jaspResults, dataset, options) {

  numericColumns <- c(options[["binomialDefectives"]],
                      if (options[["binomialSampleSizeType"]] == "variable") options[["binomialSampleSizeVariable"]])
  numericColumns <- numericColumns[numericColumns != ""]
  labelColumn    <- options[["binomialLabels"]]
  labelColumn    <- labelColumn[labelColumn != ""]

  ready <- options[["binomialDefectives"]] != "" &&
    (options[["binomialSampleSizeType"]] == "constant" || options[["binomialSampleSizeVariable"]] != "")

  if (is.null(dataset) && ready) {
    if (length(labelColumn) >= 1) {
      dataset <- .readDataSetToEnd(columns.as.numeric = numericColumns, columns.as.factor = labelColumn)
    } else {
      dataset <- .readDataSetToEnd(columns.as.numeric = numericColumns)
    }
  }

  data <- NULL
  if (ready) {
    .hasErrors(dataset, type = c("infinity", "negativeValues", "observations"),
               all.target = numericColumns, observations.amount = "< 2", exitAnalysisIfErrors = TRUE)
    data <- .qcBinomialReadData(dataset, options)
    .qcBinomialCheckErrors(dataset, data)
  }

  if (options[["report"]])
    return(.qcBinomialReport(jaspResults, data, options, ready))

  container <- .qcBinomialContainer(jaspResults)
  .qcBinomialComputeState(container, data, options, ready)

  if (options[["binomialControlChart"]])
    .qcBinomialPChart(container, options, ready)
  if (options[["binomialCumulativePlot"]])
    .qcBinomialCumulativePlot(container, options, ready)
  if (options[["binomialDistributionPlot"]])
    .qcBinomialDistributionPlot(container, options, ready)
  if (.qcBinomialShowPanel(options, "rate"))
    .qcBinomialRatePlot(container, options, ready, type = "rate")
  if (.qcBinomialShowPanel(options, "histogram"))
    .qcBinomialRatePlot(container, options, ready, type = "histogram")
  if (options[["binomialSummaryTable"]])
    .qcBinomialSummaryTable(container, options, ready)
}

# Data assembly and validation ----

.qcBinomialReadData <- function(dataset, options) {
  defectives <- as.numeric(dataset[[options[["binomialDefectives"]]]])
  if (options[["binomialSampleSizeType"]] == "variable") {
    sampleSize <- as.numeric(dataset[[options[["binomialSampleSizeVariable"]]]])
  } else {
    sampleSize <- rep(as.numeric(options[["binomialSampleSizeValue"]]), length(defectives))
  }
  labels <- if (options[["binomialLabels"]] != "") as.character(dataset[[options[["binomialLabels"]]]]) else character(0)

  # Rows with missing values are kept rather than dropped: dropping them would renumber every
  # following sample, so "Point 7" in the test results table would no longer be row 7 of the
  # spreadsheet. They are blanked out instead and excluded from the totals.
  incomplete <- is.na(defectives) | is.na(sampleSize) | sampleSize == 0
  defectives[incomplete] <- NA_real_
  sampleSize[incomplete] <- NA_real_

  return(list(defectives = defectives,
              sampleSize = sampleSize,
              proportion = defectives / sampleSize,
              labels     = labels,
              nMissing   = sum(incomplete),
              index      = seq_along(defectives),
              equalSizes = length(unique(sampleSize[!is.na(sampleSize)])) == 1))
}

.qcBinomialCheckErrors <- function(dataset, data) {
  defectives <- data[["defectives"]]
  sampleSize <- data[["sampleSize"]]
  labels     <- data[["labels"]]
  complete   <- !is.na(defectives) & !is.na(sampleSize)
  tolerance  <- .Machine$double.eps^0.5

  .hasErrors(dataset, exitAnalysisIfErrors = TRUE, custom = function() {
    if (any(complete & abs(defectives - round(defectives)) > tolerance))
      return(gettext("The number of defectives must contain whole numbers."))

    if (any(complete & (abs(sampleSize - round(sampleSize)) > tolerance | sampleSize < 1)))
      return(gettext("The sample size must be a positive whole number."))

    affected <- which(complete & defectives > sampleSize)
    if (length(affected) > 0) {
      # the columns arrive as doubles, so %i would throw here; identify the sample by its label
      # when one is assigned, otherwise by its row number
      identifier <- if (length(labels) > 0) as.character(labels[affected[1]]) else as.character(affected[1])
      message <- gettextf("Sample %1$s has more defectives (%2$s) than inspected units (%3$s).",
                          identifier, as.integer(defectives[affected[1]]), as.integer(sampleSize[affected[1]]))
      if (length(affected) > 1)
        message <- paste(message, sprintf(ngettext(length(affected),
                                                   "%i sample is affected in total.",
                                                   "%i samples are affected in total."),
                                          length(affected)))
      return(message)
    }
  })
}

# Statistics ----

# Confidence interval for a binomial proportion. Returns c(lower, upper) on the proportion scale.
.qcBinomialProportionCi <- function(nDefective, nInspected, ciLevel, method) {
  if (is.na(nDefective) || is.na(nInspected) || nInspected <= 0)
    return(c(NA_real_, NA_real_))

  alpha      <- 1 - ciLevel
  proportion <- nDefective / nInspected

  if (method == "exact") {
    # Clopper-Pearson
    lower <- if (nDefective == 0)          0 else qbeta(alpha / 2,     nDefective,     nInspected - nDefective + 1)
    upper <- if (nDefective == nInspected) 1 else qbeta(1 - alpha / 2, nDefective + 1, nInspected - nDefective)
  } else if (method == "wald") {
    z         <- qnorm(1 - alpha / 2)
    halfWidth <- z * sqrt(proportion * (1 - proportion) / nInspected)
    lower     <- max(0, proportion - halfWidth)
    upper     <- min(1, proportion + halfWidth)
  } else {
    # Wilson score
    z           <- qnorm(1 - alpha / 2)
    denominator <- 1 + z^2 / nInspected
    centre      <- (proportion + z^2 / (2 * nInspected)) / denominator
    halfWidth   <- z * sqrt(proportion * (1 - proportion) / nInspected + z^2 / (4 * nInspected^2)) / denominator
    lower       <- max(0, centre - halfWidth)
    upper       <- min(1, centre + halfWidth)
  }
  return(c(lower, upper))
}

.qcBinomialCountViolations <- function(violationTable) {
  points <- c()
  for (stageViolations in violationTable) {
    tests  <- stageViolations[names(stageViolations) != "stage"]
    points <- c(points, unlist(tests, use.names = FALSE))
  }
  points <- suppressWarnings(as.numeric(points))
  return(length(unique(points[!is.na(points)])))
}

.qcBinomialStatistics <- function(data, options) {
  defectives <- data[["defectives"]]
  sampleSize <- data[["sampleSize"]]
  ciLevel    <- options[["binomialCiLevel"]]   # a CIField delivers a proportion in this module
  ciMethod   <- options[["binomialCiMethod"]]

  totalDefectives <- sum(defectives, na.rm = TRUE)
  totalInspected  <- sum(sampleSize, na.rm = TRUE)
  # the point estimate is always data based, also when a historical proportion is set: that value
  # only moves the centre line of the chart, so keeping it out here keeps the estimate inside its
  # own confidence interval
  pBar    <- if (totalInspected > 0) totalDefectives / totalInspected else NA_real_
  pCentre <- if (options[["binomialHistoricalProportion"]]) options[["binomialHistoricalProportionValue"]] / 100 else pBar

  ci <- .qcBinomialProportionCi(totalDefectives, totalInspected, ciLevel, ciMethod)

  # running estimate with a band; blanked out samples contribute nothing but keep their row
  cumulativeDefectives <- cumsum(ifelse(is.na(defectives), 0, defectives))
  cumulativeInspected  <- cumsum(ifelse(is.na(sampleSize), 0, sampleSize))
  cumulativeP          <- ifelse(cumulativeInspected > 0, cumulativeDefectives / cumulativeInspected, NA_real_)
  cumulativeCi         <- vapply(data[["index"]],
                                 function(i) .qcBinomialProportionCi(cumulativeDefectives[i], cumulativeInspected[i],
                                                                     ciLevel, ciMethod),
                                 numeric(2))

  controlChartData <- .controlChart_calculations(
    dataset              = data.frame(defectives = defectives, sampleSize = sampleSize),
    plotType             = "p",
    ruleList             = .getRuleListSubgroupCharts(options, type = "p"),
    nSigmasControlLimits = options[["controlLimitsNumberOfSigmas"]],
    phase2               = options[["binomialHistoricalProportion"]],
    phase2Mu             = options[["binomialHistoricalProportionValue"]] / 100)

  return(list(
    data               = data,
    ciLevel            = ciLevel,
    ciLevelPercent     = ciLevel * 100,
    ciMethod           = ciMethod,
    totalDefectives    = totalDefectives,
    totalInspected     = totalInspected,
    pBar               = pBar,
    pCentre            = pCentre,
    percentDefective   = 100 * pBar,
    percentDefectiveCi = 100 * ci,
    ppm                = 1e6 * pBar,
    ppmCi              = 1e6 * ci,
    # upper-tail form avoids underflow for very capable processes; Z decreases in p, so the bounds swap
    processZ           = qnorm(pBar, lower.tail = FALSE),
    processZCi         = c(qnorm(ci[2], lower.tail = FALSE), qnorm(ci[1], lower.tail = FALSE)),
    cumulativeP        = cumulativeP,
    cumulativeLower    = cumulativeCi[1, ],
    cumulativeUpper    = cumulativeCi[2, ],
    expectedDefectives = sampleSize * pCentre,
    controlChartData   = controlChartData,
    nViolations        = .qcBinomialCountViolations(controlChartData[["violationTable"]])
  ))
}

# Container and state ----

.qcBinomialContainer <- function(jaspResults) {
  if (!is.null(jaspResults[["binomialCapability"]]))
    return(jaspResults[["binomialCapability"]])

  container <- createJaspContainer(gettext("Binomial capability analysis"))
  container$dependOn(c(.qcBinomialOptionNames(), "report"))
  container$position <- 1
  jaspResults[["binomialCapability"]] <- container

  return(container)
}

.qcBinomialComputeState <- function(container, data, options, ready) {
  if (!is.null(container[["binomialState"]]))
    return()

  state <- createJaspState()
  # the confidence interval feeds the summary table and the band of the cumulative plot, the rule
  # settings feed the out-of-control footnote, so both belong to the cached statistics
  state$dependOn(c("binomialCiLevel", "binomialCiMethod", "controlLimitsNumberOfSigmas",
                   .getDependenciesControlChartRules()))
  container[["binomialState"]] <- state

  if (!ready)
    return()

  state$object <- .qcBinomialStatistics(data, options)
}

.qcBinomialGetState <- function(container) {
  if (is.null(container[["binomialState"]]))
    return(NULL)
  return(container[["binomialState"]]$object)
}

.qcBinomialXAxisTitle <- function(options) {
  if (options[["binomialLabels"]] != "") options[["binomialLabels"]] else gettext("Sample")
}

.qcBinomialAxisLabels <- function(state) {
  labels <- state[["data"]][["labels"]]
  if (length(labels) > 0) labels else ""
}

# p chart ----

.qcBinomialPChart <- function(container, options, ready) {
  if (!is.null(container[["pChart"]]))
    return()

  chartContainer <- createJaspContainer(gettext("p chart"))
  chartContainer$dependOn(c("binomialControlChart", "controlLimitsNumberOfSigmas",
                            .getDependenciesControlChartRules()))
  chartContainer$position <- 1
  container[["pChart"]] <- chartContainer

  plot <- createJaspPlot(title = gettext("p chart"), width = 1200, height = 500)
  plot$position <- 1
  chartContainer[["plot"]] <- plot

  state <- .qcBinomialGetState(container)
  if (!ready || is.null(state)) {
    emptyTable <- createJaspTable(title = gettextf("Test results for %1$s chart", "p"))
    emptyTable$showSpecifiedColumnsOnly <- TRUE
    emptyTable$addColumnInfo(name = "noViolations", title = gettext("Tests"), type = "string")
    emptyTable$position <- 2
    chartContainer[["table"]] <- emptyTable
    return()
  }

  plot$plotObject <- .qcBinomialPChartPlotObject(state, options)

  table <- .controlChart_table(state[["controlChartData"]][["violationTable"]], plotType = "p",
                               tableLabels = .qcBinomialAxisLabels(state),
                               nPoints = length(state[["controlChartData"]][["pointData"]][["plotStatistic"]]))
  table$position <- 2
  chartContainer[["table"]] <- table
}

.qcBinomialPChartPlotObject <- function(state, options) {
  controlChartData <- state[["controlChartData"]]
  return(.controlChart_plotting(pointData   = controlChartData[["pointData"]],
                                clData      = controlChartData[["clData"]],
                                stageLabels = controlChartData[["stageLabels"]],
                                clLabels    = controlChartData[["clLabels"]],
                                plotType    = "p",
                                phase2      = options[["binomialHistoricalProportion"]],
                                xAxisLabels = .qcBinomialAxisLabels(state),
                                xAxisTitle  = .qcBinomialXAxisTitle(options)))
}

# Supporting plots ----

.qcBinomialCumulativePlot <- function(container, options, ready) {
  if (!is.null(container[["cumulativePlot"]]))
    return()

  plot <- createJaspPlot(title = gettext("Cumulative defective (%)"), width = 600, height = 400)
  plot$position <- 2
  plot$dependOn(c("binomialCumulativePlot", "binomialCiLevel", "binomialCiMethod"))
  container[["cumulativePlot"]] <- plot

  state <- .qcBinomialGetState(container)
  if (!ready || is.null(state))
    return()

  plot$plotObject <- .qcBinomialCumulativePlotObject(state, options)
}

.qcBinomialCumulativePlotObject <- function(state, options) {
  plotData <- data.frame(index    = state[["data"]][["index"]],
                         estimate = 100 * state[["cumulativeP"]],
                         lower    = 100 * state[["cumulativeLower"]],
                         upper    = 100 * state[["cumulativeUpper"]])
  target  <- if (options[["binomialTarget"]]) options[["binomialTargetValue"]] else NULL
  overall <- state[["percentDefective"]]

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(na.omit(c(plotData$lower, plotData$upper, plotData$estimate,
                                                       overall, target)))
  yLimits <- range(yBreaks)

  # colours follow the continuous capability plots: grey/black for the data, red for the estimated
  # process level, darkgreen for the target
  plotObject <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = plotData, mapping = ggplot2::aes(x = index, ymin = lower, ymax = upper),
                         fill = "grey80", na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = overall, col = "red", linewidth = 1, na.rm = TRUE)
  if (!is.null(target))
    plotObject <- plotObject +
      ggplot2::geom_hline(yintercept = target, col = "darkgreen", linewidth = 1)
  plotObject <- plotObject +
    jaspGraphs::geom_line(plotData, mapping = ggplot2::aes(x = index, y = estimate), col = "black", na.rm = TRUE) +
    jaspGraphs::geom_point(plotData, mapping = ggplot2::aes(x = index, y = estimate), size = 3, na.rm = TRUE) +
    ggplot2::scale_y_continuous(name = gettext("Cumulative defective (%)"), breaks = yBreaks, limits = yLimits) +
    .qcBinomialSampleAxis(state, options) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plotObject)
}

.qcBinomialDistributionPlot <- function(container, options, ready) {
  if (!is.null(container[["distributionPlot"]]))
    return()

  plot <- createJaspPlot(title = gettext("Binomial plot"), width = 600, height = 400)
  plot$position <- 3
  plot$dependOn("binomialDistributionPlot")
  container[["distributionPlot"]] <- plot

  state <- .qcBinomialGetState(container)
  if (!ready || is.null(state))
    return()

  plot$plotObject <- .qcBinomialDistributionPlotObject(state)
}

.qcBinomialDistributionPlotObject <- function(state) {
  plotData <- data.frame(expected = state[["expectedDefectives"]],
                         observed = state[["data"]][["defectives"]])

  breaks <- jaspGraphs::getPrettyAxisBreaks(na.omit(c(plotData$expected, plotData$observed, 0)))
  limits <- range(breaks)

  plotObject <- ggplot2::ggplot() +
    # identity reference line, drawn like the other agreement diagonals in the module
    ggplot2::geom_abline(intercept = 0, slope = 1, col = "gray", linetype = "dashed", linewidth = 1) +
    jaspGraphs::geom_point(plotData, mapping = ggplot2::aes(x = expected, y = observed), size = 3, na.rm = TRUE) +
    ggplot2::scale_x_continuous(name = gettext("Expected defectives"), breaks = breaks, limits = limits) +
    ggplot2::scale_y_continuous(name = gettext("Observed defectives"), breaks = breaks, limits = limits) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plotObject)
}

# The two sample-size dependent panels are alternatives: the rate plot belongs to a variable sample
# size, the histogram to a constant one. QML hides the check box that does not apply, but a hidden
# check box keeps its value, so the rule is repeated here. Without it a histogram ticked under a
# constant sample size would come back when the user switches to a variable one.
.qcBinomialShowPanel <- function(options, type = c("rate", "histogram")) {
  type <- match.arg(type)
  if (type == "rate")
    return(options[["binomialRatePlot"]] && options[["binomialSampleSizeType"]] == "variable")
  return(options[["binomialHistogram"]] && options[["binomialSampleSizeType"]] == "constant")
}

# Covers both sample-size dependent panels.
.qcBinomialRatePlot <- function(container, options, ready, type = c("rate", "histogram")) {
  type <- match.arg(type)
  elementKey <- if (type == "rate") "ratePlot" else "histogram"
  if (!is.null(container[[elementKey]]))
    return()

  title <- if (type == "rate") gettext("Rate of defectives") else gettext("Distribution of defective (%)")
  plot  <- createJaspPlot(title = title, width = 600, height = 400)
  plot$position <- 4
  plot$dependOn(if (type == "rate") "binomialRatePlot" else c("binomialHistogram", "binomialHistogramBinNumber"))
  container[[elementKey]] <- plot

  state <- .qcBinomialGetState(container)
  if (!ready || is.null(state))
    return()

  if (type == "rate") {
    plot$plotObject <- .qcBinomialRatePlotObject(state)
    # the panel is chosen in QML from binomialSampleSizeType, so an assigned sample size column that
    # happens to be constant is reported rather than silently swapped for the histogram
    if (isTRUE(state[["data"]][["equalSizes"]])) {
      plot$title <- gettext("Rate of defectives (constant sample size)")
      note <- createJaspHtml(paste0("<i>", gettext("Note."), "</i> ",
                                    gettext("The assigned sample size column is constant, so all samples share a single x value.")),
                             elementType = "p")
      note$position <- 5
      note$dependOn("binomialRatePlot")
      container[["ratePlotNote"]] <- note
    }
  } else {
    plot$plotObject <- .qcBinomialHistogramPlotObject(state, options)
  }
}

.qcBinomialRatePlotObject <- function(state) {
  plotData <- data.frame(sampleSize = state[["data"]][["sampleSize"]],
                         percent    = 100 * state[["data"]][["proportion"]])

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(na.omit(plotData$sampleSize))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(na.omit(c(plotData$percent, state[["percentDefective"]])))

  plotObject <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = state[["percentDefective"]], col = "red", linewidth = 1, na.rm = TRUE) +
    jaspGraphs::geom_point(plotData, mapping = ggplot2::aes(x = sampleSize, y = percent), size = 3, na.rm = TRUE) +
    ggplot2::scale_x_continuous(name = gettext("Sample size"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext("Defective (%)"), breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plotObject)
}

.qcBinomialHistogramPlotObject <- function(state, options) {
  percent  <- na.omit(100 * state[["data"]][["proportion"]])
  plotData <- data.frame(percent = as.numeric(percent))
  target   <- if (options[["binomialTarget"]]) options[["binomialTargetValue"]] else NULL

  # breaks is a suggestion: hist() rounds the boundaries to readable values, as in the histograms of
  # the continuous path
  histogram <- hist(plotData$percent, plot = FALSE, breaks = options[["binomialHistogramBinNumber"]])
  binWidth  <- histogram$breaks[2] - histogram$breaks[1]
  # the target is included in the breaks so its line stays inside the panel when it falls outside the data
  xBreaks   <- jaspGraphs::getPrettyAxisBreaks(c(histogram$breaks, plotData$percent, target), min.n = 4)
  yBreaks   <- jaspGraphs::getPrettyAxisBreaks(c(0, histogram$counts))

  plotObject <- ggplot2::ggplot() +
    ggplot2::geom_histogram(data = plotData, mapping = ggplot2::aes(x = percent), fill = "grey", col = "black",
                            linewidth = .7, binwidth = binWidth, center = binWidth / 2, na.rm = TRUE)
  # darkgreen for the target as in the other capability plots, dashed and drawn over the bars
  if (!is.null(target))
    plotObject <- plotObject +
      ggplot2::geom_vline(xintercept = target, col = "darkgreen", linetype = "dashed", linewidth = 1)
  plotObject <- plotObject +
    ggplot2::scale_x_continuous(name = gettext("Defective (%)"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext("Count"), breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plotObject)
}

.qcBinomialSampleAxis <- function(state, options) {
  index   <- state[["data"]][["index"]]
  xBreaks <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(index)))
  xBreaks <- xBreaks[xBreaks >= 1 & xBreaks <= max(index)]
  labels  <- state[["data"]][["labels"]]
  xLabels <- if (length(labels) > 0) labels[xBreaks] else xBreaks

  return(ggplot2::scale_x_continuous(name = .qcBinomialXAxisTitle(options), breaks = xBreaks,
                                     limits = c(min(index) - .5, max(index) + .5), labels = xLabels))
}

# Summary table ----

.qcBinomialSummaryTable <- function(container, options, ready) {
  if (!is.null(container[["summaryTable"]]))
    return()

  table <- createJaspTable(title = gettext("Summary statistics"))
  table$position <- 6
  table$dependOn(c("binomialSummaryTable", "binomialCiLevel", "binomialCiMethod",
                   "controlLimitsNumberOfSigmas", .getDependenciesControlChartRules()))
  table$showSpecifiedColumnsOnly <- TRUE

  ciTitle <- gettextf("%s%% CI", options[["binomialCiLevel"]] * 100)
  table$addColumnInfo(name = "statistic", title = "",               type = "string")
  table$addColumnInfo(name = "value",     title = gettext("Value"), type = "number")
  table$addColumnInfo(name = "ciLower",   title = gettext("Lower"), type = "number", overtitle = ciTitle)
  table$addColumnInfo(name = "ciUpper",   title = gettext("Upper"), type = "number", overtitle = ciTitle)

  container[["summaryTable"]] <- table

  state <- .qcBinomialGetState(container)
  if (!ready || is.null(state))
    return()

  table$setData(.qcBinomialSummaryDataframe(state, options))
  for (footnote in .qcBinomialSummaryFootnotes(state, options))
    table$addFootnote(footnote)
}

.qcBinomialSummaryDataframe <- function(state, options, formatNumbers = FALSE) {
  tableDf <- data.frame(statistic = c(gettext("Defective (%)"), gettext("PPM defective"), gettext("Process Z")),
                        value     = c(state[["percentDefective"]], state[["ppm"]], state[["processZ"]]),
                        ciLower   = c(state[["percentDefectiveCi"]][1], state[["ppmCi"]][1], state[["processZCi"]][1]),
                        ciUpper   = c(state[["percentDefectiveCi"]][2], state[["ppmCi"]][2], state[["processZCi"]][2]),
                        stringsAsFactors = FALSE)

  if (options[["binomialTarget"]])
    tableDf <- rbind(tableDf, data.frame(statistic = gettext("Target defective (%)"),
                                         value     = options[["binomialTargetValue"]],
                                         ciLower   = NA_real_,
                                         ciUpper   = NA_real_,
                                         stringsAsFactors = FALSE))

  if (formatNumbers) {
    for (column in c("value", "ciLower", "ciUpper"))
      tableDf[[column]] <- .pcTableFormatNumbers(tableDf[[column]])
    colnames(tableDf) <- c(gettext("Statistic"), gettext("Value"),
                           gettextf("Lower %s%% CI", state[["ciLevelPercent"]]),
                           gettextf("Upper %s%% CI", state[["ciLevelPercent"]]))
  }

  return(tableDf)
}

.qcBinomialSummaryFootnotes <- function(state, options) {
  footnotes <- c()

  # the violation count is computed with the statistics, so this warning also shows when the p chart
  # itself is switched off
  if (state[["nViolations"]] > 0)
    footnotes <- c(footnotes, sprintf(ngettext(state[["nViolations"]],
                                               "The process is not in control (%i point fails the selected tests); the capability estimate may not be representative.",
                                               "The process is not in control (%i points fail the selected tests); the capability estimate may not be representative."),
                                      state[["nViolations"]]))

  footnotes <- c(footnotes, switch(options[["binomialCiMethod"]],
                                   "exact"  = gettext("Confidence intervals are exact (Clopper-Pearson)."),
                                   "wald"   = gettext("Confidence intervals use the Wald (normal approximation) method."),
                                   "wilson" = gettext("Confidence intervals use the Wilson score method.")))

  if (options[["binomialHistoricalProportion"]])
    footnotes <- c(footnotes, gettextf("The control chart centre line uses a historical proportion defective of %s%%; the statistics in this table are estimated from the observed data.",
                                       options[["binomialHistoricalProportionValue"]]))

  nMissing <- state[["data"]][["nMissing"]]
  if (nMissing > 0)
    footnotes <- c(footnotes, sprintf(ngettext(nMissing,
                                               "%i sample with missing values was excluded from the statistics.",
                                               "%i samples with missing values were excluded from the statistics."),
                                      nMissing))

  if (isTRUE(state[["totalDefectives"]] == 0))
    footnotes <- c(footnotes, gettext("No defectives were observed. The Process Z is therefore unbounded and only its confidence bound is informative."))
  else if (isTRUE(state[["totalDefectives"]] == state[["totalInspected"]]))
    footnotes <- c(footnotes, gettext("All inspected units were defective. The Process Z is therefore unbounded and only its confidence bound is informative."))

  return(footnotes)
}

# Report ----

.qcBinomialReport <- function(jaspResults, data, options, ready) {
  # the binomial report draws its own panels, so it counts them itself instead of reusing the
  # element count of the continuous path
  nSupportingPlots <- sum(options[["binomialCumulativePlot"]], options[["binomialDistributionPlot"]],
                          .qcBinomialShowPanel(options, "rate"), .qcBinomialShowPanel(options, "histogram"))
  nElements <- sum(options[["reportProcessStability"]],
                   options[["reportProcessCapabilityPlot"]] * nSupportingPlots,
                   options[["reportProcessCapabilityTables"]],
                   options[["reportMetaData"]])
  plotHeight <- max(1, ceiling(nElements / 2)) * 500

  reportPlot <- createJaspPlot(title = gettext("Process Capability Report"), width = 1250, height = plotHeight)
  jaspResults[["report"]] <- reportPlot
  # the report element key is shared with the continuous path, so capabilityDataType must be part of
  # the dependencies (it is, through .qcBinomialOptionNames)
  jaspResults[["report"]]$dependOn(c(
    .qcBinomialOptionNames(), .qcReportOptionNames(),
    "binomialControlChart", "binomialCumulativePlot", "binomialDistributionPlot",
    "binomialRatePlot", "binomialHistogram", "binomialHistogramBinNumber", "binomialSummaryTable",
    "binomialCiLevel", "binomialCiMethod", "controlLimitsNumberOfSigmas",
    .getDependenciesControlChartRules()
  ))

  if (!options[["reportProcessStability"]] && !options[["reportProcessCapabilityPlot"]] &&
      !options[["reportProcessCapabilityTables"]]) {
    reportPlot$setError(gettext("No report components selected."))
    return()
  }

  if (!ready)
    return()

  state <- .qcBinomialStatistics(data, options)

  title <- ""
  if (options[["reportTitle"]])
    title <- if (options[["reportTitleText"]] == "") gettext("Process Capability Report") else options[["reportTitleText"]]

  text <- NULL
  if (options[["reportMetaData"]]) {
    text <- c()
    text <- if (options[["reportLocation"]])   c(text, gettextf("Location: %s",    options[["reportLocationText"]]))   else text
    text <- if (options[["reportLine"]])       c(text, gettextf("Line: %s",        options[["reportLineText"]]))       else text
    text <- if (options[["reportMachine"]])    c(text, gettextf("Machine: %s",     options[["reportMachineText"]]))    else text
    text <- if (options[["reportVariable"]])   c(text, gettextf("Variable: %s",    options[["reportVariableText"]]))   else text
    text <- if (options[["reportProcess"]])    c(text, gettextf("Process: %s",     options[["reportProcessText"]]))    else text
    text <- if (options[["reportDate"]])       c(text, gettextf("Date: %s",        options[["reportDateText"]]))       else text
    text <- if (options[["reportReportedBy"]]) c(text, gettextf("Reported by: %s", options[["reportReportedByText"]])) else text
    text <- if (options[["reportConclusion"]]) c(text, gettextf("Conclusion: %s",  options[["reportConclusionText"]])) else text
  }

  plots <- list()
  if (options[["reportProcessStability"]])
    plots[[length(plots) + 1]] <- .qcBinomialPChartPlotObject(state, options)
  if (options[["reportProcessCapabilityPlot"]]) {
    if (options[["binomialCumulativePlot"]])
      plots[[length(plots) + 1]] <- .qcBinomialCumulativePlotObject(state, options)
    if (options[["binomialDistributionPlot"]])
      plots[[length(plots) + 1]] <- .qcBinomialDistributionPlotObject(state)
    if (.qcBinomialShowPanel(options, "rate"))
      plots[[length(plots) + 1]] <- .qcBinomialRatePlotObject(state)
    if (.qcBinomialShowPanel(options, "histogram"))
      plots[[length(plots) + 1]] <- .qcBinomialHistogramPlotObject(state, options)
  }
  # .qcReport cannot lay out an empty plot list
  if (length(plots) == 0)
    plots <- list(ggplot2::ggplot() + ggplot2::theme_void())

  tables      <- list()
  tableTitles <- ""
  if (options[["reportProcessCapabilityTables"]]) {
    tables[[1]] <- .qcBinomialSummaryDataframe(state, options, formatNumbers = TRUE)
    tableTitles <- list(gettext("Summary statistics"))
  }

  reportPlot$plotObject <- .qcReport(text = text, plots = plots, tables = tables, textMaxRows = 8,
                                     tableTitles = tableTitles, reportTitle = title, tableSize = 6)
}

# Dependencies ----

.qcBinomialOptionNames <- function() {
  dependencies <- c("capabilityDataType", "binomialDefectives", "binomialSampleSizeType",
                    "binomialSampleSizeValue", "binomialSampleSizeVariable", "binomialLabels",
                    "binomialHistoricalProportion", "binomialHistoricalProportionValue",
                    "binomialTarget", "binomialTargetValue")
  return(dependencies)
}
