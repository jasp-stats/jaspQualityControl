#
# Copyright (C) 2013-2022 University of Amsterdam
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

variablesChartsIndividuals <- function(jaspResults, dataset, options) {

  # Read data
  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.numeric = options[["variable"]][options[["variable"]] != ""], columns.as.factor = options[["subgroups"]][options[["subgroups"]] != ""])
    dataset <- na.omit(dataset)
  }

  # Check for errors
  .hasErrors(dataset,
    type = c("infinity", "missingValues", "observations"),
    infinity.target = c(options$variable, options$subgroups),
    missingValues.target = c(options$variable, options$subgroups),
    observations.amount = c("< 2"),
    observations.target = c(options$variable),
    exitAnalysisIfErrors = TRUE
  )

  # Check if analysis is ready
  ready <- options[["variable"]] != ""

  # Summary table
  .qcIndividualsChartsSummaryTable(jaspResults, dataset, options, ready, position = 1)

  # Control charts
  .qcIndividualsXmRChartsContainer(jaspResults, dataset, options, ready, position = 2)
  .qcIndividualsAutocorrelationChart(jaspResults, dataset, options, ready, position = 3)

  #   # Report
  #   if (options[["CCReport"]] && is.null(jaspResults[["CCReport"]]) && options[["xmr"]]) {

  #     jaspResults[["CorPlot"]] <- NULL
  #     jaspResults[["Ichart"]] <- NULL


  #     jaspResults[["CCReport"]] <- createJaspContainer(gettext("Report"))
  #     jaspResults[["CCReport"]]$dependOn(c("CCReport", "ImRchart", "variables","ncol", "manualTicks", "nTicks", "subgroups", "ccTitle", "ccName", "ccMisc","ccReportedBy","ccDate", "ccSubTitle", "ccChartName"))
  #     jaspResults[["CCReport"]]$position <- 9
  #     Iplot <- jaspResults[["CCReport"]]

  #     IMR <- .IMRchart(dataset = dataset, options = options, variable = variables, manualXaxis = subgroups)
  #     Iplot[["ccReport"]] <- .CCReport(p1 = IMR$p1, p2 = IMR$p2, ccTitle = options$ccTitle,
  #                                        ccName = options$ccName, ccDate = options$ccDate, ccReportedBy = options$ccReportedBy, ccSubTitle = options$ccSubTitle,
  #                                        ccChartName = options$ccChartName)
  #   }
}

.qcIndividualsChartsSummaryTable <- function(jaspResults, dataset, options, ready, position) {
  if (!is.null(jaspResults[["summaryTable"]])) {
    return()
  }

  table <- createJaspTable(gettext("Data Summary"))
  table$position <- position
  table$dependOn(options = c("variable", "subgroups"))

  table$addColumnInfo(name = "p", title = gettext("No. Measurements"), type = "integer")

  jaspResults[["summaryTable"]] <- table

  if (!ready) {
    return()
  }

  row <- list(p = nrow(dataset))
  table$addRows(row)
}

.qcIndividualsXmRChartsContainer <- function(jaspResults, dataset, options, ready, position) {
  if (!options[["xmrchart"]]) {
    return()
  }

  if (is.null(jaspResults[["xmrchart"]])) {
    container <- createJaspContainer(title = gettext("X-mR Chart: Average and Moving Range"))
    container$dependOn(c("variable", "subgroups", "xmr", "ncol"))
    container$position <- position
    jaspResults[["xmrchart"]] <- container
  } else {
    container <- jaspResults[["xmrchart"]]
  }

  .qcIndividualsXmrChart(container, dataset, options, ready, type = "xbar", positionInContainer = 1)
  .qcIndividualsXmrChart(container, dataset, options, ready, type = "R", positionInContainer = 2)
  # .qcAttributesChartsNelsonTable(container, dataset, options, ready, type = "xbar", positionInContainer = 3)
  # .qcAttributesChartsNelsonTable(container, dataset, options, ready, type = "R", positionInContainer = 4)
}

.qcIndividualsXmrChart <- function(container, dataset, options, ready, type, positionInContainer) {
  jaspTitle <- paste0(type, "plot")
  if (!is.null(container[[jaspTitle]])) {
    return()
  }

  plot <- createJaspPlot(title = gettextf("%s Chart", toupper(type)), width = options$plotWidth * 1.75, height = options$plotHeight)
  plot$dependOn(c("palette", "ncol", "xmrchart"))
  plot$position <- positionInContainer
  container[[jaspTitle]] <- plot

  if (!ready || container$getError()) {
    return()
  }

  palette <- .qcColorPalette(options)

  p_try <- try({
    values <- dataset[[options[["variable"]]]]
    if (type == "xbar") {
	  plotTitle <- gettext("Value")
      qccOutput <- qcc::qcc(data = values, type = "xbar.one", plot = FALSE)
      center <- qccOutput[["center"]]
      ucl <- qccOutput[["limits"]][, 2]
      lcl <- qccOutput[["limits"]][, 1]
      statistics <- qccOutput$statistics
      fill <- ifelse(qccOutput$statistics > ucl | qccOutput$statistics < lcl, palette[3], palette[2])
    } else {
	  plotTitle <- gettext("Moving range")
      qccOutput <- qcc::qcc(data = matrix(c(values[1:length(values) - 1], values[2:length(values)]), ncol = options[["ncol"]]), type = "R", plot = FALSE)
      center <- qccOutput[["center"]]
      ucl <- max(qccOutput[["limits"]])
      lcl <- min(qccOutput[["limits"]])
      statistics <- qccOutput[["statistics"]]
      fill <- ifelse(statistics > ucl | statistics < lcl, palette[3], palette[2])
    }

    plotData <- data.frame(group = 1:length(statistics), stat = statistics, fill = fill)
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, lcl, plotData[["stat"]], ucl))

    # TODO: Find a way to nicely display x-breaks
    xBreaks <- floor(seq(1, nrow(plotData), length.out = min(20, nrow(plotData))))

    p <- ggplot2::ggplot(data = plotData, ggplot2::aes(x = group, y = stat)) +
      ggplot2::geom_segment(y = center, yend = center, x = 1, xend = nrow(plotData), color = palette[1])

    if (length(ucl) == 1) {
      labelData <- data.frame(
        x = rep(nrow(plotData) + 1, 3), y = c(center, ucl, lcl),
        l = c(gettextf("CL = %g", round(center, 4)), gettextf("UCL = %g", round(ucl, 4)), gettextf("LCL = %g", round(lcl, 4)))
      )
      p <- p + ggplot2::geom_segment(y = ucl, yend = ucl, x = 1, xend = nrow(plotData), color = palette[3], linetype = "dashed") +
        ggplot2::geom_segment(y = lcl, yend = lcl, x = 1, xend = nrow(plotData), color = palette[3], linetype = "dashed")
    } else {
      labelData <- data.frame(
        x = nrow(plotData) + 1, y = center,
        l = gettextf("CL = %g", round(center, 4))
      )
      ucl <- c(ucl, ucl[length(ucl)])
      lcl <- c(lcl, lcl[length(lcl)])
      p <- p + ggplot2::geom_step(data = data.frame(x = 1:length(ucl), y = ucl), mapping = ggplot2::aes(x = x, y = y), color = palette[3], size = 1.5, linetype = "F1", inherit.aes = FALSE) +
        ggplot2::geom_step(data = data.frame(x = 1:length(lcl), y = lcl), mapping = ggplot2::aes(x = x, y = y), color = palette[3], size = 1.5, linetype = "F1", inherit.aes = FALSE)
    }

    p <- p + jaspGraphs::geom_line(color = palette[2]) +
      jaspGraphs::geom_point(fill = plotData$fill, color = "black", size = 4) +
      ggplot2::geom_label(data = labelData, mapping = ggplot2::aes(x = x, y = y, label = l), inherit.aes = FALSE, size = 4.5, hjust = 0) +
      ggplot2::scale_y_continuous(name = plotTitle, breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::scale_x_continuous(name = gettext("Sample"), breaks = xBreaks, limits = c(1, max(xBreaks) * 1.15)) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(legend.position = "none")
  })

  if (isTryError(p_try)) {
    plot$setError(gettextf("Plotting not possible: %s", jaspBase:::.extractErrorMessage(p_try)))
  } else {
    plot$plotObject <- p
  }
}

.qcIndividualsAutocorrelationChart <- function(jaspResults, dataset, options, ready, position) {
  if (!is.null(jaspResults[["autocorPlot"]]) || !options[["autocorrelation"]]) {
    return()
  }

  plot <- createJaspPlot(title = gettext("Autocorrelation Function"), width = options$plotWidth * 1.75, height = options$plotHeight)
  plot$dependOn(c("variable", "subgroups", "palette", "nLag", "CI", "autocorrelation"))
  plot$position <- position
  jaspResults[["autocorPlot"]] <- plot

  if (!ready) {
    return()
  }

  palette <- .qcColorPalette(options)

  p_try <- try({
    list.acf <- stats::acf(dataset[[options[["variable"]]]], lag.max = options[["nLag"]], type = "correlation", ci.type = "ma", plot = FALSE, ci = options[["CI"]])
    N <- as.numeric(list.acf$n.used)
    plotData <- data.frame(lag = list.acf$lag, acf = list.acf$acf)
    plotData$lag.acf <- dplyr::lag(plotData$acf, default = 0)
    plotData$lag.acf[2] <- 0
    plotData$lag.acf.cumsum <- cumsum((plotData$lag.acf)^2)
    plotData$acfstd <- sqrt(1 / N * (1 + 2 * plotData$lag.acf.cumsum))
    plotData$acfstd[1] <- 0
    plotData <- dplyr::select(plotData, lag, acf, acfstd)

    p <- ggplot2::ggplot(data = plotData, ggplot2::aes(x = lag, y = acf)) +
      ggplot2::geom_col(fill = palette[2], width = 0.2) +
      jaspGraphs::geom_line(mapping = ggplot2::aes(x = lag, y = qnorm((1 + options[["CI"]]) / 2) * acfstd), color = palette[3]) +
      jaspGraphs::geom_line(mapping = ggplot2::aes(x = lag, y = -qnorm((1 + options[["CI"]]) / 2) * acfstd), color = palette[3]) +
      ggplot2::geom_segment(x = 0, xend = nrow(plotData), y = 0, yend = 0, color = palette[1]) +
      ggplot2::scale_y_continuous(name = gettext("Autocorrelation"), limits = c(-1, 1), breaks = seq(-1, 1, 0.2)) +
      ggplot2::scale_x_continuous(name = gettext("Lag"), breaks = seq(1, max(plotData$lag), 2)) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
  })

  if (isTryError(p_try)) {
    plot$setError(gettextf("Plotting not possible: %s", jaspBase:::.extractErrorMessage(p_try)))
  } else {
    plot$plotObject <- p
  }
}
