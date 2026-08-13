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

# Attribute (count) capability analysis, in two distributions.
#
# Reached from processCapabilityStudies() when capabilityDataType == "attributes". Which distribution
# applies is decided by attributeDistribution:
#   binomial - every inspected unit is pass/fail, the data are counts of defective units per sample,
#              and capability is expressed as %Defective, PPM defective and Process Z.
#   poisson  - a unit can carry several defects, the data are counts of defects per sample, and
#              capability is expressed as the mean number of defects per unit (DPU).
# The two share everything except the chart type, the centre-line option, the interval family, the
# summary rows and two validation rules. Stages are not supported here.

# TRUE when the analysis runs in the Poisson (defects per unit) mode.
.qcAttributeIsPoisson <- function(options) options[["attributeDistribution"]] == "poisson"

.qcAttributeCapability <- function(jaspResults, dataset, options) {

  numericColumns <- c(options[["attributeCounts"]],
                      if (options[["attributeSampleSizeType"]] == "variable") options[["attributeSampleSizeVariable"]])
  numericColumns <- numericColumns[numericColumns != ""]
  labelColumn    <- options[["attributeLabels"]]
  labelColumn    <- labelColumn[labelColumn != ""]

  ready <- options[["attributeCounts"]] != "" &&
    (options[["attributeSampleSizeType"]] == "constant" || options[["attributeSampleSizeVariable"]] != "")

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
    data <- .qcAttributeReadData(dataset, options)
    .qcAttributeCheckErrors(dataset, data, options)
  }

  if (options[["report"]])
    return(.qcAttributeReport(jaspResults, data, options, ready))

  container <- .qcAttributeContainer(jaspResults, options)
  .qcAttributeComputeState(container, data, options, ready)

  if (options[["attributeControlChart"]])
    .qcAttributeControlChart(container, options, ready)
  if (options[["attributeCumulativePlot"]])
    .qcAttributeCumulativePlot(container, options, ready)
  if (options[["attributeDistributionPlot"]])
    .qcAttributeDistributionPlot(container, options, ready)
  if (.qcAttributeShowPanel(options, "rate"))
    .qcAttributeRatePlot(container, options, ready, type = "rate")
  if (.qcAttributeShowPanel(options, "histogram"))
    .qcAttributeRatePlot(container, options, ready, type = "histogram")
  if (options[["attributeSummaryTable"]])
    .qcAttributeSummaryTable(container, options, ready)
}

# Data assembly and validation ----

.qcAttributeReadData <- function(dataset, options) {
  counts <- as.numeric(dataset[[options[["attributeCounts"]]]])
  if (options[["attributeSampleSizeType"]] == "variable") {
    sampleSize <- as.numeric(dataset[[options[["attributeSampleSizeVariable"]]]])
  } else {
    sampleSize <- rep(as.numeric(options[["attributeSampleSizeValue"]]), length(counts))
  }
  labels <- if (options[["attributeLabels"]] != "") as.character(dataset[[options[["attributeLabels"]]]]) else character(0)

  # Rows with missing values are kept rather than dropped: dropping them would renumber every
  # following sample, so "Point 7" in the test results table would no longer be row 7 of the
  # spreadsheet. They are blanked out instead and excluded from the totals.
  incomplete <- is.na(counts) | is.na(sampleSize) | sampleSize == 0
  counts[incomplete]     <- NA_real_
  sampleSize[incomplete] <- NA_real_

  return(list(counts     = counts,
              sampleSize = sampleSize,
              # a proportion defective in binomial mode, a defect rate in Poisson mode
              rate       = counts / sampleSize,
              labels     = labels,
              nMissing   = sum(incomplete),
              index      = seq_along(counts),
              equalSizes = length(unique(sampleSize[!is.na(sampleSize)])) == 1))
}

.qcAttributeCheckErrors <- function(dataset, data, options) {
  counts     <- data[["counts"]]
  sampleSize <- data[["sampleSize"]]
  labels     <- data[["labels"]]
  complete   <- !is.na(counts) & !is.na(sampleSize)
  tolerance  <- .Machine$double.eps^0.5
  poisson    <- .qcAttributeIsPoisson(options)

  .hasErrors(dataset, exitAnalysisIfErrors = TRUE, custom = function() {
    if (any(complete & abs(counts - round(counts)) > tolerance))
      return(if (poisson) gettext("The number of defects must contain whole numbers.")
             else         gettext("The number of defectives must contain whole numbers."))

    if (poisson) {
      # No sample-size rule applies here. A Poisson exposure may be fractional (2.5 square metres,
      # 1.5 hours), so the binomial "positive whole number" rule is dropped; positivity is already
      # guaranteed elsewhere, because a negative sample size is rejected by the negativeValues check
      # in the orchestrator and a zero sample size is blanked out as a missing sample by
      # .qcAttributeReadData. Nor is there an upper check on the counts: C > n is legal here, ten
      # defects on five units being a defect rate of 2.
      return()
    }

    if (any(complete & (abs(sampleSize - round(sampleSize)) > tolerance | sampleSize < 1)))
      return(gettext("The sample size must be a positive whole number."))

    affected <- which(complete & counts > sampleSize)
    if (length(affected) > 0) {
      # the columns arrive as doubles, so %i would throw here; identify the sample by its label
      # when one is assigned, otherwise by its row number
      identifier <- if (length(labels) > 0) as.character(labels[affected[1]]) else as.character(affected[1])
      message <- gettextf("Sample %1$s has more defectives (%2$s) than inspected units (%3$s).",
                          identifier, as.integer(counts[affected[1]]), as.integer(sampleSize[affected[1]]))
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
.qcAttributeProportionCi <- function(nDefective, nInspected, ciLevel, method) {
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

# Confidence interval for a Poisson rate. Returns c(lower, upper) on the defects per unit scale.
# Unlike a proportion the rate is unbounded above, so only the lower bound is floored.
.qcAttributePoissonRateCi <- function(count, exposure, ciLevel, method) {
  alpha <- 1 - ciLevel
  rate  <- count / exposure

  if (method == "exact") {
    # Garwood, the Poisson analogue of Clopper-Pearson
    lower <- if (count == 0) 0 else qgamma(alpha / 2, shape = count) / exposure
    upper <- qgamma(1 - alpha / 2, shape = count + 1) / exposure
  } else if (method == "wald") {
    z         <- qnorm(1 - alpha / 2)
    halfWidth <- z * sqrt(rate / exposure)
    lower     <- max(0, rate - halfWidth)
    upper     <- rate + halfWidth
  } else {
    # score (Rao), the Poisson analogue of Wilson
    z      <- qnorm(1 - alpha / 2)
    centre <- count + z^2 / 2
    spread <- z * sqrt(count + z^2 / 4)
    lower  <- max(0, (centre - spread) / exposure)
    upper  <- (centre + spread) / exposure
  }
  return(c(lower, upper))
}

# Dispatches to the interval family that matches the distribution.
.qcAttributeRateCi <- function(count, exposure, ciLevel, method, poisson) {
  if (is.na(count) || is.na(exposure) || exposure <= 0)
    return(c(NA_real_, NA_real_))
  if (poisson)
    return(.qcAttributePoissonRateCi(count, exposure, ciLevel, method))
  return(.qcAttributeProportionCi(count, exposure, ciLevel, method))
}

.qcAttributeCountViolations <- function(violationTable) {
  points <- c()
  for (stageViolations in violationTable) {
    tests  <- stageViolations[names(stageViolations) != "stage"]
    points <- c(points, unlist(tests, use.names = FALSE))
  }
  points <- suppressWarnings(as.numeric(points))
  return(length(unique(points[!is.na(points)])))
}

.qcAttributeStatistics <- function(data, options) {
  counts     <- data[["counts"]]
  sampleSize <- data[["sampleSize"]]
  poisson    <- .qcAttributeIsPoisson(options)
  ciLevel    <- options[["attributeCiLevel"]]   # a CIField delivers a proportion in this module
  ciMethod   <- if (poisson) options[["poissonCiMethod"]] else options[["binomialCiMethod"]]

  totalCounts   <- sum(counts, na.rm = TRUE)
  totalExposure <- sum(sampleSize, na.rm = TRUE)
  # the point estimate is always data based, also when a historical centre line is set: that value
  # only moves the centre line of the chart, so keeping it out here keeps the estimate inside its
  # own confidence interval
  rateBar <- if (totalExposure > 0) totalCounts / totalExposure else NA_real_
  # the historical Poisson value is already a rate, the historical binomial value is a percentage
  phase2  <- if (poisson) options[["poissonHistoricalDpu"]] else options[["binomialHistoricalProportion"]]
  centre  <- if (!phase2) rateBar
             else if (poisson) options[["poissonHistoricalDpuValue"]]
             else              options[["binomialHistoricalProportionValue"]] / 100
  chartType <- if (poisson) "u" else "p"

  ci <- .qcAttributeRateCi(totalCounts, totalExposure, ciLevel, ciMethod, poisson)

  # running estimate with a band; blanked out samples contribute nothing but keep their row
  cumulativeCounts   <- cumsum(ifelse(is.na(counts), 0, counts))
  cumulativeExposure <- cumsum(ifelse(is.na(sampleSize), 0, sampleSize))
  cumulativeRate     <- ifelse(cumulativeExposure > 0, cumulativeCounts / cumulativeExposure, NA_real_)
  cumulativeCi       <- vapply(data[["index"]],
                               function(i) .qcAttributeRateCi(cumulativeCounts[i], cumulativeExposure[i],
                                                              ciLevel, ciMethod, poisson),
                               numeric(2))

  controlChartData <- .controlChart_calculations(
    dataset              = data.frame(counts = counts, sampleSize = sampleSize),
    plotType             = chartType,
    ruleList             = .getRuleListSubgroupCharts(options, type = chartType),
    nSigmasControlLimits = options[["controlLimitsNumberOfSigmas"]],
    phase2               = phase2,
    phase2Mu             = centre)

  statistics <- list(
    data             = data,
    ciLevel          = ciLevel,
    ciLevelPercent   = ciLevel * 100,
    ciMethod         = ciMethod,
    chartType        = chartType,
    totalCounts      = totalCounts,
    totalExposure    = totalExposure,
    rateBar          = rateBar,
    rateCi           = ci,
    centre           = centre,
    cumulativeRate   = cumulativeRate,
    cumulativeLower  = cumulativeCi[1, ],
    cumulativeUpper  = cumulativeCi[2, ],
    expectedCounts   = sampleSize * centre,
    controlChartData = controlChartData,
    nViolations      = .qcAttributeCountViolations(controlChartData[["violationTable"]])
  )

  if (poisson) {
    # yield statistics, all monotone in the rate. P(no defect on a unit) = exp(-rate), so the share of
    # conforming units follows from the rate alone. expm1 keeps the precision for the small rates of a
    # capable process, which is exactly where Z is largest.
    statistics[["percentUnits"]]   <- 100 * -expm1(-rateBar)
    statistics[["percentUnitsCi"]] <- 100 * -expm1(-ci)
    statistics[["ppmUnits"]]       <- 1e6 * -expm1(-rateBar)
    statistics[["ppmUnitsCi"]]     <- 1e6 * -expm1(-ci)
    statistics[["processZ"]]       <- qnorm(-expm1(-rateBar), lower.tail = FALSE)
    # Z decreases in the rate, so the bounds swap
    statistics[["processZCi"]]     <- qnorm(-expm1(-rev(ci)), lower.tail = FALSE)
  } else {
    statistics[["percentDefective"]]   <- 100 * rateBar
    statistics[["percentDefectiveCi"]] <- 100 * ci
    statistics[["ppm"]]                <- 1e6 * rateBar
    statistics[["ppmCi"]]              <- 1e6 * ci
    # upper-tail form avoids underflow for very capable processes; Z decreases in p, so bounds swap
    statistics[["processZ"]]           <- qnorm(rateBar, lower.tail = FALSE)
    statistics[["processZCi"]]         <- c(qnorm(ci[2], lower.tail = FALSE), qnorm(ci[1], lower.tail = FALSE))
  }

  return(statistics)
}

# Scale and label helpers ----

# Poisson reports a rate throughout, binomial a percentage throughout. Only the optional yield rows
# of the Poisson summary table break that rule.
.qcAttributeRateScale <- function(options) if (.qcAttributeIsPoisson(options)) 1 else 100

.qcAttributeRateLabel <- function(options) {
  if (.qcAttributeIsPoisson(options)) gettext("Defects per unit") else gettext("Defective (%)")
}

.qcAttributeTargetValue <- function(options) {
  if (.qcAttributeIsPoisson(options)) {
    if (options[["poissonTarget"]]) options[["poissonTargetValue"]] else NULL
  } else {
    if (options[["binomialTarget"]]) options[["binomialTargetValue"]] else NULL
  }
}

# The overall level on the scale the supporting plots use.
.qcAttributeOverallLevel <- function(state, options) .qcAttributeRateScale(options) * state[["rateBar"]]

# Container and state ----

.qcAttributeContainer <- function(jaspResults, options) {
  if (!is.null(jaspResults[["attributeCapability"]]))
    return(jaspResults[["attributeCapability"]])

  title <- if (.qcAttributeIsPoisson(options)) gettext("Poisson capability analysis") else gettext("Binomial capability analysis")
  container <- createJaspContainer(title)
  container$dependOn(c(.qcAttributeOptionNames(), "report"))
  container$position <- 1
  jaspResults[["attributeCapability"]] <- container

  return(container)
}

.qcAttributeComputeState <- function(container, data, options, ready) {
  if (!is.null(container[["attributeState"]]))
    return()

  state <- createJaspState()
  # the confidence interval feeds the summary table and the band of the cumulative plot, the rule
  # settings feed the out-of-control footnote, so both belong to the cached statistics
  state$dependOn(c("attributeCiLevel", "binomialCiMethod", "poissonCiMethod", "poissonYieldStatistics",
                   "controlLimitsNumberOfSigmas", .getDependenciesControlChartRules()))
  container[["attributeState"]] <- state

  if (!ready)
    return()

  state$object <- .qcAttributeStatistics(data, options)
}

.qcAttributeGetState <- function(container) {
  if (is.null(container[["attributeState"]]))
    return(NULL)
  return(container[["attributeState"]]$object)
}

.qcAttributeXAxisTitle <- function(options) {
  if (options[["attributeLabels"]] != "") options[["attributeLabels"]] else gettext("Sample")
}

.qcAttributeAxisLabels <- function(state) {
  labels <- state[["data"]][["labels"]]
  if (length(labels) > 0) labels else ""
}

# Control chart (p or u) ----

.qcAttributeControlChart <- function(container, options, ready) {
  if (!is.null(container[["controlChart"]]))
    return()

  poisson   <- .qcAttributeIsPoisson(options)
  chartName <- if (poisson) "u" else "p"
  title     <- if (poisson) gettext("u chart") else gettext("p chart")

  chartContainer <- createJaspContainer(title)
  chartContainer$dependOn(c("attributeControlChart", "controlLimitsNumberOfSigmas",
                            .getDependenciesControlChartRules()))
  chartContainer$position <- 1
  container[["controlChart"]] <- chartContainer

  plot <- createJaspPlot(title = title, width = 1200, height = 500)
  plot$position <- 1
  chartContainer[["plot"]] <- plot

  state <- .qcAttributeGetState(container)
  if (!ready || is.null(state)) {
    emptyTable <- createJaspTable(title = gettextf("Test results for %1$s chart", chartName))
    emptyTable$showSpecifiedColumnsOnly <- TRUE
    emptyTable$addColumnInfo(name = "noViolations", title = gettext("Tests"), type = "string")
    emptyTable$position <- 2
    chartContainer[["table"]] <- emptyTable
    return()
  }

  plot$plotObject <- .qcAttributeControlChartPlotObject(state, options)

  table <- .controlChart_table(state[["controlChartData"]][["violationTable"]], plotType = chartName,
                               tableLabels = .qcAttributeAxisLabels(state),
                               nPoints = length(state[["controlChartData"]][["pointData"]][["plotStatistic"]]))
  table$position <- 2
  chartContainer[["table"]] <- table
}

.qcAttributeControlChartPlotObject <- function(state, options) {
  controlChartData <- state[["controlChartData"]]
  phase2 <- if (.qcAttributeIsPoisson(options)) options[["poissonHistoricalDpu"]] else options[["binomialHistoricalProportion"]]
  return(.controlChart_plotting(pointData   = controlChartData[["pointData"]],
                                clData      = controlChartData[["clData"]],
                                stageLabels = controlChartData[["stageLabels"]],
                                clLabels    = controlChartData[["clLabels"]],
                                plotType    = state[["chartType"]],
                                phase2      = phase2,
                                xAxisLabels = .qcAttributeAxisLabels(state),
                                xAxisTitle  = .qcAttributeXAxisTitle(options)))
}

# Supporting plots ----

.qcAttributeCumulativePlot <- function(container, options, ready) {
  if (!is.null(container[["cumulativePlot"]]))
    return()

  title <- if (.qcAttributeIsPoisson(options)) gettext("Cumulative defects per unit") else gettext("Cumulative defective (%)")
  plot  <- createJaspPlot(title = title, width = 600, height = 400)
  plot$position <- 2
  plot$dependOn(c("attributeCumulativePlot", "attributeCiLevel", "binomialCiMethod", "poissonCiMethod"))
  container[["cumulativePlot"]] <- plot

  state <- .qcAttributeGetState(container)
  if (!ready || is.null(state))
    return()

  plot$plotObject <- .qcAttributeCumulativePlotObject(state, options)
}

.qcAttributeCumulativePlotObject <- function(state, options) {
  scale    <- .qcAttributeRateScale(options)
  plotData <- data.frame(index    = state[["data"]][["index"]],
                         estimate = scale * state[["cumulativeRate"]],
                         lower    = scale * state[["cumulativeLower"]],
                         upper    = scale * state[["cumulativeUpper"]])
  target  <- .qcAttributeTargetValue(options)
  overall <- .qcAttributeOverallLevel(state, options)

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(na.omit(c(plotData$lower, plotData$upper, plotData$estimate,
                                                       overall, target)))
  yLimits <- range(yBreaks)
  yTitle  <- if (.qcAttributeIsPoisson(options)) gettext("Cumulative defects per unit") else gettext("Cumulative defective (%)")

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
    ggplot2::scale_y_continuous(name = yTitle, breaks = yBreaks, limits = yLimits) +
    .qcAttributeSampleAxis(state, options) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plotObject)
}

.qcAttributeDistributionPlot <- function(container, options, ready) {
  if (!is.null(container[["distributionPlot"]]))
    return()

  title <- if (.qcAttributeIsPoisson(options)) gettext("Poisson plot") else gettext("Binomial plot")
  plot  <- createJaspPlot(title = title, width = 600, height = 400)
  plot$position <- 3
  plot$dependOn("attributeDistributionPlot")
  container[["distributionPlot"]] <- plot

  state <- .qcAttributeGetState(container)
  if (!ready || is.null(state))
    return()

  plot$plotObject <- .qcAttributeDistributionPlotObject(state, options)
}

.qcAttributeDistributionPlotObject <- function(state, options) {
  plotData <- data.frame(expected = state[["expectedCounts"]],
                         observed = state[["data"]][["counts"]])
  poisson  <- .qcAttributeIsPoisson(options)
  xTitle   <- if (poisson) gettext("Expected defects") else gettext("Expected defectives")
  yTitle   <- if (poisson) gettext("Observed defects") else gettext("Observed defectives")

  breaks <- jaspGraphs::getPrettyAxisBreaks(na.omit(c(plotData$expected, plotData$observed, 0)))
  limits <- range(breaks)

  plotObject <- ggplot2::ggplot() +
    # identity reference line, drawn like the other agreement diagonals in the module
    ggplot2::geom_abline(intercept = 0, slope = 1, col = "gray", linetype = "dashed", linewidth = 1) +
    jaspGraphs::geom_point(plotData, mapping = ggplot2::aes(x = expected, y = observed), size = 3, na.rm = TRUE) +
    ggplot2::scale_x_continuous(name = xTitle, breaks = breaks, limits = limits) +
    ggplot2::scale_y_continuous(name = yTitle, breaks = breaks, limits = limits) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plotObject)
}

# The two sample-size dependent panels are alternatives: the rate plot belongs to a variable sample
# size, the histogram to a constant one. QML hides the check box that does not apply, but a hidden
# check box keeps its value, so the rule is repeated here. Without it a histogram ticked under a
# constant sample size would come back when the user switches to a variable one.
.qcAttributeShowPanel <- function(options, type = c("rate", "histogram")) {
  type <- match.arg(type)
  if (type == "rate")
    return(options[["attributeRatePlot"]] && options[["attributeSampleSizeType"]] == "variable")
  return(options[["attributeHistogram"]] && options[["attributeSampleSizeType"]] == "constant")
}

# Covers both sample-size dependent panels.
.qcAttributeRatePlot <- function(container, options, ready, type = c("rate", "histogram")) {
  type <- match.arg(type)
  poisson    <- .qcAttributeIsPoisson(options)
  elementKey <- if (type == "rate") "ratePlot" else "histogram"
  if (!is.null(container[[elementKey]]))
    return()

  title <- if (type == "rate") {
    if (poisson) gettext("Rate of defects") else gettext("Rate of defectives")
  } else {
    if (poisson) gettext("Distribution of defects per unit") else gettext("Distribution of defective (%)")
  }
  plot <- createJaspPlot(title = title, width = 600, height = 400)
  plot$position <- 4
  plot$dependOn(if (type == "rate") "attributeRatePlot" else c("attributeHistogram", "attributeHistogramBinNumber"))
  container[[elementKey]] <- plot

  state <- .qcAttributeGetState(container)
  if (!ready || is.null(state))
    return()

  if (type == "rate") {
    plot$plotObject <- .qcAttributeRatePlotObject(state, options)
    # the panel is chosen in QML from attributeSampleSizeType, so an assigned sample size column that
    # happens to be constant is reported rather than silently swapped for the histogram
    if (isTRUE(state[["data"]][["equalSizes"]])) {
      plot$title <- if (poisson) gettext("Rate of defects (constant sample size)") else gettext("Rate of defectives (constant sample size)")
      note <- createJaspHtml(paste0("<i>", gettext("Note."), "</i> ",
                                    gettext("The assigned sample size column is constant, so all samples share a single x value.")),
                             elementType = "p")
      note$position <- 5
      note$dependOn("attributeRatePlot")
      container[["ratePlotNote"]] <- note
    }
  } else {
    plot$plotObject <- .qcAttributeHistogramPlotObject(state, options)
  }
}

.qcAttributeRatePlotObject <- function(state, options) {
  scale    <- .qcAttributeRateScale(options)
  plotData <- data.frame(sampleSize = state[["data"]][["sampleSize"]],
                         level      = scale * state[["data"]][["rate"]])
  overall  <- .qcAttributeOverallLevel(state, options)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(na.omit(plotData$sampleSize))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(na.omit(c(plotData$level, overall)))

  plotObject <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = overall, col = "red", linewidth = 1, na.rm = TRUE) +
    jaspGraphs::geom_point(plotData, mapping = ggplot2::aes(x = sampleSize, y = level), size = 3, na.rm = TRUE) +
    ggplot2::scale_x_continuous(name = gettext("Sample size"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = .qcAttributeRateLabel(options), breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plotObject)
}

.qcAttributeHistogramPlotObject <- function(state, options) {
  scale    <- .qcAttributeRateScale(options)
  level    <- na.omit(scale * state[["data"]][["rate"]])
  plotData <- data.frame(level = as.numeric(level))
  target   <- .qcAttributeTargetValue(options)

  # breaks is a suggestion: hist() rounds the boundaries to readable values, as in the histograms of
  # the continuous path
  histogram <- hist(plotData$level, plot = FALSE, breaks = options[["attributeHistogramBinNumber"]])
  binWidth  <- histogram$breaks[2] - histogram$breaks[1]
  # the target is included in the breaks so its line stays inside the panel when it falls outside the data
  xBreaks   <- jaspGraphs::getPrettyAxisBreaks(c(histogram$breaks, plotData$level, target), min.n = 4)
  yBreaks   <- jaspGraphs::getPrettyAxisBreaks(c(0, histogram$counts))

  plotObject <- ggplot2::ggplot() +
    ggplot2::geom_histogram(data = plotData, mapping = ggplot2::aes(x = level), fill = "grey", col = "black",
                            linewidth = .7, binwidth = binWidth, center = binWidth / 2, na.rm = TRUE)
  # darkgreen for the target as in the other capability plots, dashed and drawn over the bars
  if (!is.null(target))
    plotObject <- plotObject +
      ggplot2::geom_vline(xintercept = target, col = "darkgreen", linetype = "dashed", linewidth = 1)
  plotObject <- plotObject +
    ggplot2::scale_x_continuous(name = .qcAttributeRateLabel(options), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext("Count"), breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plotObject)
}

.qcAttributeSampleAxis <- function(state, options) {
  index   <- state[["data"]][["index"]]
  xBreaks <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(index)))
  xBreaks <- xBreaks[xBreaks >= 1 & xBreaks <= max(index)]
  labels  <- state[["data"]][["labels"]]
  xLabels <- if (length(labels) > 0) labels[xBreaks] else xBreaks

  return(ggplot2::scale_x_continuous(name = .qcAttributeXAxisTitle(options), breaks = xBreaks,
                                     limits = c(min(index) - .5, max(index) + .5), labels = xLabels))
}

# Summary table ----

.qcAttributeSummaryTable <- function(container, options, ready) {
  if (!is.null(container[["summaryTable"]]))
    return()

  table <- createJaspTable(title = gettext("Summary statistics"))
  table$position <- 6
  table$dependOn(c("attributeSummaryTable", "attributeCiLevel", "binomialCiMethod", "poissonCiMethod",
                   "poissonYieldStatistics", "controlLimitsNumberOfSigmas",
                   .getDependenciesControlChartRules()))
  table$showSpecifiedColumnsOnly <- TRUE

  ciTitle <- gettextf("%s%% CI", options[["attributeCiLevel"]] * 100)
  table$addColumnInfo(name = "statistic", title = "",               type = "string")
  table$addColumnInfo(name = "value",     title = gettext("Value"), type = "number")
  table$addColumnInfo(name = "ciLower",   title = gettext("Lower"), type = "number", overtitle = ciTitle)
  table$addColumnInfo(name = "ciUpper",   title = gettext("Upper"), type = "number", overtitle = ciTitle)

  container[["summaryTable"]] <- table

  state <- .qcAttributeGetState(container)
  if (!ready || is.null(state))
    return()

  table$setData(.qcAttributeSummaryDataframe(state, options))
  for (footnote in .qcAttributeSummaryFootnotes(state, options))
    table$addFootnote(footnote)
}

.qcAttributeSummaryDataframe <- function(state, options, formatNumbers = FALSE) {
  if (.qcAttributeIsPoisson(options)) {
    # Minitab's Poisson capability summary is the mean DPU and its interval, and nothing else
    tableDf <- data.frame(statistic = gettext("Mean DPU"),
                          value     = state[["rateBar"]],
                          ciLower   = state[["rateCi"]][1],
                          ciUpper   = state[["rateCi"]][2],
                          stringsAsFactors = FALSE)

    if (options[["poissonYieldStatistics"]])
      tableDf <- rbind(tableDf,
                       data.frame(statistic = c(gettext("Defective units (%)"), gettext("PPM defective"),
                                                gettext("Process Z")),
                                  value     = c(state[["percentUnits"]], state[["ppmUnits"]], state[["processZ"]]),
                                  ciLower   = c(state[["percentUnitsCi"]][1], state[["ppmUnitsCi"]][1],
                                                state[["processZCi"]][1]),
                                  ciUpper   = c(state[["percentUnitsCi"]][2], state[["ppmUnitsCi"]][2],
                                                state[["processZCi"]][2]),
                                  stringsAsFactors = FALSE))

    if (options[["poissonTarget"]])
      tableDf <- rbind(tableDf, data.frame(statistic = gettext("Target DPU"),
                                           value     = options[["poissonTargetValue"]],
                                           ciLower   = NA_real_,
                                           ciUpper   = NA_real_,
                                           stringsAsFactors = FALSE))
  } else {
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
  }

  if (formatNumbers) {
    for (column in c("value", "ciLower", "ciUpper"))
      tableDf[[column]] <- .pcTableFormatNumbers(tableDf[[column]])
    colnames(tableDf) <- c(gettext("Statistic"), gettext("Value"),
                           gettextf("Lower %s%% CI", state[["ciLevelPercent"]]),
                           gettextf("Upper %s%% CI", state[["ciLevelPercent"]]))
  }

  return(tableDf)
}

.qcAttributeSummaryFootnotes <- function(state, options) {
  footnotes <- c()
  poisson   <- .qcAttributeIsPoisson(options)

  # the violation count is computed with the statistics, so this warning also shows when the control
  # chart itself is switched off
  if (state[["nViolations"]] > 0)
    footnotes <- c(footnotes, sprintf(ngettext(state[["nViolations"]],
                                               "The process is not in control (%i point fails the selected tests); the capability estimate may not be representative.",
                                               "The process is not in control (%i points fail the selected tests); the capability estimate may not be representative."),
                                      state[["nViolations"]]))

  footnotes <- c(footnotes, if (poisson)
    switch(options[["poissonCiMethod"]],
           "exact" = gettext("Confidence intervals are exact (Garwood)."),
           "wald"  = gettext("Confidence intervals use the Wald (normal approximation) method."),
           "score" = gettext("Confidence intervals use the score method."))
  else
    switch(options[["binomialCiMethod"]],
           "exact"  = gettext("Confidence intervals are exact (Clopper-Pearson)."),
           "wald"   = gettext("Confidence intervals use the Wald (normal approximation) method."),
           "wilson" = gettext("Confidence intervals use the Wilson score method.")))

  if (poisson) {
    if (options[["poissonHistoricalDpu"]])
      footnotes <- c(footnotes, gettextf("The control chart centre line uses a historical defect rate of %s defects per unit; the statistics in this table are estimated from the observed data.",
                                         options[["poissonHistoricalDpuValue"]]))
    if (options[["poissonYieldStatistics"]])
      footnotes <- c(footnotes, gettext("Yield statistics assume that a unit is conforming when it carries no defect."))
  } else if (options[["binomialHistoricalProportion"]]) {
    footnotes <- c(footnotes, gettextf("The control chart centre line uses a historical proportion defective of %s%%; the statistics in this table are estimated from the observed data.",
                                       options[["binomialHistoricalProportionValue"]]))
  }

  nMissing <- state[["data"]][["nMissing"]]
  if (nMissing > 0)
    footnotes <- c(footnotes, sprintf(ngettext(nMissing,
                                               "%i sample with missing values was excluded from the statistics.",
                                               "%i samples with missing values were excluded from the statistics."),
                                      nMissing))

  if (poisson) {
    # a defect rate is unbounded above, so there is no "everything defective" degenerate case
    if (isTRUE(state[["totalCounts"]] == 0))
      footnotes <- c(footnotes, gettext("No defects were observed. The mean DPU is zero and only its upper confidence bound is informative."))
  } else if (isTRUE(state[["totalCounts"]] == 0)) {
    footnotes <- c(footnotes, gettext("No defectives were observed. The Process Z is therefore unbounded and only its confidence bound is informative."))
  } else if (isTRUE(state[["totalCounts"]] == state[["totalExposure"]])) {
    footnotes <- c(footnotes, gettext("All inspected units were defective. The Process Z is therefore unbounded and only its confidence bound is informative."))
  }

  return(footnotes)
}

# Report ----

.qcAttributeReport <- function(jaspResults, data, options, ready) {
  # the attribute report draws its own panels, so it counts them itself instead of reusing the
  # element count of the continuous path
  nSupportingPlots <- sum(options[["attributeCumulativePlot"]], options[["attributeDistributionPlot"]],
                          .qcAttributeShowPanel(options, "rate"), .qcAttributeShowPanel(options, "histogram"))
  nElements <- sum(options[["reportProcessStability"]],
                   options[["reportProcessCapabilityPlot"]] * nSupportingPlots,
                   options[["reportProcessCapabilityTables"]],
                   options[["reportMetaData"]])
  plotHeight <- max(1, ceiling(nElements / 2)) * 500

  reportPlot <- createJaspPlot(title = gettext("Process Capability Report"), width = 1250, height = plotHeight)
  jaspResults[["report"]] <- reportPlot
  # the report element key is shared with the continuous path, so capabilityDataType must be part of
  # the dependencies (it is, through .qcAttributeOptionNames)
  jaspResults[["report"]]$dependOn(c(
    .qcAttributeOptionNames(), .qcReportOptionNames(),
    "attributeControlChart", "attributeCumulativePlot", "attributeDistributionPlot",
    "attributeRatePlot", "attributeHistogram", "attributeHistogramBinNumber", "attributeSummaryTable",
    "attributeCiLevel", "binomialCiMethod", "poissonCiMethod", "poissonYieldStatistics",
    "controlLimitsNumberOfSigmas", .getDependenciesControlChartRules()
  ))

  if (!options[["reportProcessStability"]] && !options[["reportProcessCapabilityPlot"]] &&
      !options[["reportProcessCapabilityTables"]]) {
    reportPlot$setError(gettext("No report components selected."))
    return()
  }

  if (!ready)
    return()

  state <- .qcAttributeStatistics(data, options)

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
    plots[[length(plots) + 1]] <- .qcAttributeControlChartPlotObject(state, options)
  if (options[["reportProcessCapabilityPlot"]]) {
    if (options[["attributeCumulativePlot"]])
      plots[[length(plots) + 1]] <- .qcAttributeCumulativePlotObject(state, options)
    if (options[["attributeDistributionPlot"]])
      plots[[length(plots) + 1]] <- .qcAttributeDistributionPlotObject(state, options)
    if (.qcAttributeShowPanel(options, "rate"))
      plots[[length(plots) + 1]] <- .qcAttributeRatePlotObject(state, options)
    if (.qcAttributeShowPanel(options, "histogram"))
      plots[[length(plots) + 1]] <- .qcAttributeHistogramPlotObject(state, options)
  }
  # .qcReport cannot lay out an empty plot list
  if (length(plots) == 0)
    plots <- list(ggplot2::ggplot() + ggplot2::theme_void())

  tables      <- list()
  tableTitles <- ""
  if (options[["reportProcessCapabilityTables"]]) {
    tables[[1]] <- .qcAttributeSummaryDataframe(state, options, formatNumbers = TRUE)
    tableTitles <- list(gettext("Summary statistics"))
  }

  reportPlot$plotObject <- .qcReport(text = text, plots = plots, tables = tables, textMaxRows = 8,
                                     tableTitles = tableTitles, reportTitle = title, tableSize = 6)
}

# Dependencies ----

# Both distributions' options are listed unconditionally: the inactive set never changes while the
# user works in the other mode, so the over-inclusion costs nothing and a conditional vector would be
# one more thing to get wrong.
.qcAttributeOptionNames <- function() {
  dependencies <- c("capabilityDataType", "attributeDistribution",
                    "attributeCounts", "attributeSampleSizeType", "attributeSampleSizeValue",
                    "attributeSampleSizeVariable", "attributeLabels",
                    "binomialHistoricalProportion", "binomialHistoricalProportionValue",
                    "binomialTarget", "binomialTargetValue",
                    "poissonHistoricalDpu", "poissonHistoricalDpuValue",
                    "poissonTarget", "poissonTargetValue")
  return(dependencies)
}
