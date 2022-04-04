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

timeWeightedCharts <- function(jaspResults, dataset, options) {

  # Read data
  if (is.null(datset)) {
    dataset <- .readDataSetToEnd(columns.as.numeric = options[["variables"]])
    dataset <- na.omit(dataset)
  }

  # Check for errors
  .hasErrors(dataset,
    type = c("infinity", "missingValues"),
    all.target = options$variables, exitAnalysisIfErrors = TRUE
  )

  # Check if analysis is ready
  ready <- length(unlist(options[["variables"]])) > 0 && unlist(options[["variables"]]) != ""

  # Summary table
  .qcTimeWeightedChartsSummaryTable(jaspResults, dataset, options, ready, position = 1)

  # Control Charts
  .qcTimeWeightedChart(jaspResults, dataset, options, ready, type = "csc", position = 2)
  .qcTimeWeightedChart(jaspResults, dataset, options, ready, type = "ewma", position = 3)
  .qcTimeWeightedChart(jaspResults, dataset, options, ready, type = "g", position = 4)
  .qcTimeWeightedChart(jaspResults, dataset, options, ready, type = "t", position = 5)
}

.qcTimeWeightedChartsSummaryTable <- function(jaspResults, dataset, options, ready, position) {
  if (!is.null(jaspResults[["summaryTable"]])) {
    return()
  }

  table <- createJaspTable(gettext("Data Summary"))
  table$position <- position
  table$dependOn(options = "variables")

  table$addColumnInfo(name = "p", title = gettext("No. Measurements"), type = "integer")
  table$addColumnInfo(name = "n", title = gettext("No. Rows"), type = "integer")

  jaspResults[["summaryTable"]] <- table

  if (!ready) {
    return()
  }

  row <- list(p = length(options[["variables"]]), n = nrow(dataset))
  table$addRows(row)
}

.qcTimeWeightedChart <- function(jaspResults, dataset, options, ready, type, position) {
  jaspTitle <- paste0(type, "plot")

  if (!is.null(jaspResults[[jaspTitle]]) || !options[[type]]) {
    return()
  }

  plotTitle <- switch(type,
    "csc" = gettextf("CSC Chart"),
    "ewma" = gettextf("EWMA Chart"),
    "g" = gettextf("G Chart"),
    "t" = gettextf("T Chart")
  )

  depends <- switch(type,
    "csc" = c("numsigma", "shift"),
    "ewma" = c("lambda", "center", "sigma", "nsigma"),
    "g" = NULL,
    "t" = NULL
  )

  plot <- createJaspPlot(title = plotTitle, width = options$plotWidth * 1.75, height = options$plotHeight)
  plot$dependOn(c("variables", "palette", type, depends))
  plot$position <- position
  jaspResults[[jaspTitle]] <- plot

  if (!ready) {
    return()
  }

  palette <- .qcColorPalette(options)

  p_try <- try({
    if (type == "csc") {
      yTitle <- gettext("Cumulative sum")
      qccOutput <- qcc::cusum(dataset, decision.interval = options[["numsigma"]], se.shift = options[["shift"]], plot = FALSE)
      center <- 0
      ucl <- qccOutput$decision.interval
      lcl <- -ucl
      plotData <- data.frame(x = 1:length(qccOutput$pos), lower = qccOutput$neg, upper = qccOutput$pos)
      plotData$fill_upper <- ifelse(plotData$upper > ucl, palette[3], palette[2])
      plotData$fill_lower <- ifelse(plotData$lower < lcl, palette[3], palette[2])
      labelData <- data.frame(
        x = rep(nrow(plotData) + 1, 3), y = c(center, ucl, lcl),
        l = c(gettextf("CL = %g", round(center, 4)), gettextf("UCL = %g", round(ucl, 4)), gettextf("LCL = %g", round(lcl, 4)))
      )
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(plotData$lower, plotData$upper, ucl, lcl))
    } else if (type == "ewma") {
      yTitle <- gettext("Weighted average")
      qccOutput <- qcc::ewma(dataset,
        center = options[["center"]], lambda = options[["lambda"]],
        std.dev = options[["sigma"]], nsigmas = options[["nsigma"]], plot = FALSE
      )
      center <- qccOutput$center
      lcl <- qccOutput$limits[, 1]
      ucl <- qccOutput$limits[, 2]
      plotData <- data.frame(x = 1:length(qccOutput$y), y = qccOutput$y, ucl = ucl, lcl = lcl)
      plotData$fill <- ifelse(plotData$y > ucl | plotData$y < lcl, palette[3], palette[2])
      uclLabel <- center + options$nsigma * sqrt(options$lambda / (2 - options$lambda)) * options$sigma
      lclLabel <- center - options$nsigma * sqrt(options$lambda / (2 - options$lambda)) * options$sigma
      labelData <- data.frame(
        x = rep(nrow(plotData) + 1, 3), y = c(center, uclLabel, lclLabel),
        l = c(
          gettextf("CL = %g", round(center, decimalplaces(dataset[1]) + 1)),
          gettextf("UCL = %g", round(uclLabel, decimalplaces(dataset[1]) + 2)),
          gettextf("LCL = %g", round(lclLabel, decimalplaces(dataset[1]) + 2))
        )
      )
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(plotData$y, uclLabel, lclLabel))
    } else if (type == "g") {
      yTitle <- gettext("Time between events")
      center <- mean(dataset[, options$variables])
      ucl <- center + 3 * sqrt(center * (center + 1))
      lcl <- center - 3 * sqrt(center * (center + 1))
      lcl <- ifelse(lcl < 0, yes = 0, no = lcl)
      qccOutput <- list(statistics = dataset[, options$variables], limits = data.frame(lcl, ucl), center = center)
      plotData <- data.frame(x = 1:length(dataset[, options$variables]), y = dataset[, options$variables])
      plotData$fill <- ifelse(plotData$y > ucl | plotData$y < lcl, palette[3], palette[2])
      labelData <- data.frame(
        x = rep(nrow(plotData) + 1, 3), y = c(center, ucl, lcl),
        l = c(gettextf("CL = %g", round(center, 4)), gettextf("UCL = %g", round(ucl, 4)), gettextf("LCL = %g", round(lcl, 4)))
      )
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(plotData$y, lcl, ucl))
    } else if (type == "t") {
      yTitle <- gettext("Number between events")
      plotData <- data.frame(x = 1:length(dataset[, options$variables]), y = dataset[, options$variables]^0.2777)
      running <- dataset[, options$variables]
      qccOutput <- qcc::qcc(matrix(c(running[1:length(running) - 1], running[2:length(running)]), ncol = 2), type = "R", plot = FALSE)$statistics
      center <- mean(plotData$y)^3.6
      ucl <- (mean(plotData$y) + 2.66 * mean(qccOutput))^3.6
      lcl <- (mean(plotData$y) - 2.66 * mean(qccOutput))^3.6
      lcl <- ifelse(lcl < 0, yes = 0, no = lcl)
      plotData$fill <- ifelse(plotData$y > ucl | plotData$y < lcl, palette[3], palette[2])
      labelData <- data.frame(
        x = rep(nrow(plotData) + 1, 3), y = c(center, ucl, lcl),
        l = c(gettextf("CL = %g", round(center, 4)), gettextf("UCL = %g", round(ucl, 4)), gettextf("LCL = %g", round(lcl, 4)))
      )
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(plotData$y, lcl, ucl))
    }

    yLimits <- range(yBreaks)
    # TODO: Find a way to nicely display x-breaks
    xBreaks <- seq(1, nrow(plotData), length.out = min(20, nrow(plotData)))

    p <- ggplot2::ggplot(data = plotData, ggplot2::aes(x = x)) +
      ggplot2::geom_segment(y = center, yend = center, x = 1, xend = nrow(plotData), color = palette[1])

    if (length(ucl) == 1) {
      p <- p + ggplot2::geom_segment(y = ucl, yend = ucl, x = 1, xend = nrow(plotData), color = palette[3], linetype = "dashed") +
        ggplot2::geom_segment(y = lcl, yend = lcl, x = 1, xend = nrow(plotData), color = palette[3], linetype = "dashed")
      if (type == "csc") {
        p <- p + jaspGraphs::geom_line(mapping = ggplot2::aes(y = lower), color = palette[2]) +
          jaspGraphs::geom_line(mapping = ggplot2::aes(y = upper), color = palette[2]) +
          jaspGraphs::geom_point(mapping = ggplot2::aes(y = lower), fill = plotData$fill_lower, color = "black", size = 4) +
          jaspGraphs::geom_point(mapping = ggplot2::aes(y = upper), fill = plotData$fill_upper, color = "black", size = 4)
      } else {
        p <- p + jaspGraphs::geom_line(mapping = ggplot2::aes(y = y), color = palette[2]) +
          jaspGraphs::geom_point(mapping = ggplot2::aes(y = y), fill = plotData$fill, color = "black", size = 4)
      }
    } else {
      p <- p + ggplot2::geom_step(mapping = ggplot2::aes(y = ucl), color = palette[3], linetype = "dashed", size = 1.5) +
        ggplot2::geom_step(mapping = ggplot2::aes(y = lcl), color = palette[3], linetype = "dashed", size = 1.5) +
        jaspGraphs::geom_line(mapping = ggplot2::aes(y = y), color = palette[2]) +
        jaspGraphs::geom_point(mapping = ggplot2::aes(y = y), fill = plotData$fill, size = 4)
    }

    p <- p + ggplot2::geom_label(data = labelData, mapping = ggplot2::aes(x = x, y = y, label = l), inherit.aes = FALSE, size = 4.5, hjust = 0) +
      ggplot2::scale_y_continuous(name = yTitle, breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::scale_x_continuous(name = gettext("Subgroups"), breaks = xBreaks, limits = c(1, max(xBreaks) * 1.15)) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(legend.position = "none")
  })

  if (isTryError(p_try)) {
    plot$setError(gettextf("Plotting not possible: %s", jaspBase:::.extractErrorMessage(p_try)))
  } else {
    plot$plotObject <- p
  }
}
