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

attributesCharts <- function(jaspResults, dataset, options) {

  # Read data
  dataset <- .qcAttributesChartsReadDataset(options)

  # Check for errors
  .qcAttributesChartsCheckErrors(dataset, options)

  # Check if analysis is ready
  ready <- options[["defects"]] != "" && options[["sampleSize"]] != ""

  # Make top table
  .qcAttributesChartsSummaryTable(jaspResults, dataset, options, ready, position = 1)

  # Control charts
  .qcAttributesChartsContainer(jaspResults, dataset, options, ready, position = 2, type = "xmr")
  .qcAttributesChartsContainer(jaspResults, dataset, options, ready, position = 3, type = "np")
  .qcAttributesChartsContainer(jaspResults, dataset, options, ready, position = 4, type = "p")
  .qcAttributesChartsContainer(jaspResults, dataset, options, ready, position = 5, type = "Laney p'")
  .qcAttributesChartsContainer(jaspResults, dataset, options, ready, position = 6, type = "c")
  .qcAttributesChartsContainer(jaspResults, dataset, options, ready, position = 7, type = "u")
  .qcAttributesChartsContainer(jaspResults, dataset, options, ready, position = 8, type = "Laney u'")

  # Report
  #   if (options[["AReport"]] && is.null(jaspResults[["AReport"]]) && ready) {
  #     jaspResults[["PchartPlot"]] <- NULL
  #     jaspResults[["NelsonTable"]] <- NULL
  #     jaspResults[["NPchartPlot"]] <- NULL
  #     jaspResults[["LaneyPPlot"]] <- NULL
  #     jaspResults[["CchartPlot"]] <- NULL
  #     jaspResults[["UchartPlot"]] <- NULL
  #     jaspResults[["LaneyUPlot"]] <- NULL
  #     jaspResults[["IPlotA"]] <- NULL
  #     jaspResults[["NelsonTableIMR"]] <- NULL

  #     jaspResults[["AReport"]] <- createJaspContainer(title = gettextf("Report for Attribute Control Charts"))
  #     jaspResults[["AReport"]]$dependOn(c(
  #       "AReport", "ATitle", "AName", "AOperator", "AID", "AMisc", "AAppraiser", "AMeasurement", "ASize", "ATime", "AFrequency",
  #       "defects", "samplesize", "Attributes", "TypeDefects", "TypeDefectives", "timeStamp"
  #     ))
  #     Report <- jaspResults[["AReport"]]
  #     Report[["Report"]] <- .AReport(
  #       ccTitle = options$ATitle, ccName = options$AName,
  #       ccOperator = options$AOperator, ccID = options$AID, ccMisc = options$AMisc, ccAppraiser = options$AAppraiser,
  #       ccMeasurement = options$AMeasurement, ccSize = options$ASize, ccTime = options$ATime, ccFrequency = options$AFrequency
  #     )

  #     Report[["Plot"]] <- createJaspPlot(width = 1000, height = 800, position = 2)
  #     if (options$Attributes == "Defectives" & options$TypeDefectives == "pchart") {
  #       PlotReport <- .Pchart(dataset = dataset, options = options, timeStamp = timeStamp)$p
  #     } else if (options$Attributes == "Defectives" & options$TypeDefectives == "npchart") {
  #       PlotReport <- .NPchart(dataset = dataset, options = options, timeStamp = timeStamp)$p
  #     } else if (options$Attributes == "Defectives" & options$TypeDefectives == "Laneyprimechart") {
  #       PlotReport <- .LanyP(dataset = dataset, options = options, timeStamp = timeStamp)$p
  #     } else if (options$Attributes == "Defects" & options$TypeDefects == "cchart") {
  #       PlotReport <- .Cchart(dataset = dataset, options = options, timeStamp = timeStamp)$p
  #     } else if (options$Attributes == "Defects" & options$TypeDefects == "uchart") {
  #       PlotReport <- .Uchart(dataset = dataset, options = options, timeStamp = timeStamp)$p
  #     } else if (options$Attributes == "Defects" & options$TypeDefects == "Laneychart") {
  #       PlotReport <- .LanyU(dataset = dataset, options = options, timeStamp = timeStamp)$p
  #     } else if (options$Attributes == "ImR") {
  #       PlotReport <- .Ichart_attributes(dataset = dataset, options = options, timeStamp = timeStamp)$p
  #     }

  #     Report[["Plot"]]$plotObject <- PlotReport
  #   }
}

.qcColorPalette <- function(options) {
  # 1st color is for average data, 2nd is for data points and lines, 3rd is for confidence limits
  cols <- switch(options[["palette"]],
    "iso" = c("#339d4c", "#0053a6", "#931313"),
    "jasp" = c("black", "darkgray", "darkred"),
    "colorblind" = jaspGraphs::JASPcolors(palette = "colorblind")[1:3]
  )
  return(cols)
}

.qcAttributesChartsReadDataset <- function(options) {
  variables <- c(options[["sampleSize"]], options[["defects"]])
  variables <- variables[variables != ""]
  if (options[["timeStamp"]] == "") {
    dataset <- .readDataSetToEnd(columns.as.numeric = variables)
  } else {
    dataset <- .readDataSetToEnd(columns.as.numeric = variables, columns.as.factor = options[["timeStamp"]])
  }
  dataset <- na.omit(dataset)
}

.qcAttributesChartsCheckErrors <- function(dataset, options) {
  .hasErrors(dataset,
    type = c("infinity", "missingValues", "negativeValues"),
    all.target = c(options[["defects"]], options[["sampleSize"]]),
    exitAnalysisIfErrors = TRUE
  )
}

.qcAttributesChartsSummaryTable <- function(jaspResults, dataset, options, ready, position) {
  if (!is.null(jaspResults[["summaryTable"]])) {
    return()
  }

  table <- createJaspTable(gettext("Data Summary"))
  table$position <- position
  table$dependOn(options = c("defects", "sampleSize"))

  table$addColumnInfo(name = "nrow", title = gettext("Samples"), type = "integer")
  table$addColumnInfo(name = "n", title = gettext("Units"), type = "integer")
  table$addColumnInfo(name = "k", title = gettext("Nonconforming units"), type = "integer")

  jaspResults[["summaryTable"]] <- table

  if (!ready) {
    return()
  }

  row <- list(nrow = nrow(dataset), n = sum(dataset[[options[["sampleSize"]]]]), k = sum(dataset[[options[["defects"]]]]))
  table$addRows(row)
}

.qcAttributesChartsContainer <- function(jaspResults, dataset, options, ready, position, type) {
  checked <- switch(type,
    "np" = options[["npchart"]],
    "p" = options[["pchart"]],
    "Laney p'" = options[["lpchart"]],
    "c" = options[["cchart"]],
    "u" = options[["uchart"]],
    "Laney u'" = options[["luchart"]],
    "xmr" = options[["xmrchart"]]
  )

  if (!checked) {
    return()
  }

  optionTitle <- switch(type,
    "np" = "npchart",
    "p" = "pchart",
    "Laney p'" = "lpchart",
    "c" = "cchart",
    "u" = "uchart",
    "Laney u'" = "luchart",
    "xmr" = "xmrchart"
  )

  containerTitle <- switch(type,
    "np" = gettext("NP Chart: Number of Nonconforming Units"),
    "p" = gettext("P Chart: Proportion of Nonconforming Units"),
    "Laney p'" = gettext("Laney p' Chart: Proportion of Nonconforming Units"),
    "c" = gettext("C Chart: Number of Nonconformities per Unit"),
    "u" = gettext("U Chart: Average Nonconformities per Unit"),
    "Laney u'" = gettext("Laney u' Chart: Average Nonconformities per Unit"),
    "xmr" = gettext("X-mR Chart: Proportion and Moving Range")
  )

  if (is.null(jaspResults[[optionTitle]])) {
    container <- createJaspContainer(title = containerTitle)
    container$dependOn(c("defects", "sampleSize", optionTitle))
    container$position <- position
    jaspResults[[optionTitle]] <- container
  } else {
    container <- jaspResults[[optionTitle]]
  }

  # These charts require equal sample sizes
  if (type == "np" || type == "c") {
    if (length(unique(dataset[[options[["sampleSize"]]]])) > 1) {
      container$setError(gettext("Failed to create chart: Sample sizes must all be equal."))
    }
  }

  if (type != "xmr") {
    .qcAttributesChartsCreateChart(container, dataset, options, ready, type, positionInContainer = 1)
    .qcAttributesChartsNelsonTable(container, dataset, options, ready, type, positionInContainer = 2)
  } else {
    .qcAttributesChartsCreateChart(container, dataset, options, ready, type = "xbar", positionInContainer = 1)
    .qcAttributesChartsCreateChart(container, dataset, options, ready, type = "R", positionInContainer = 2)
    .qcAttributesChartsNelsonTable(container, dataset, options, ready, type = "xbar", positionInContainer = 3)
    .qcAttributesChartsNelsonTable(container, dataset, options, ready, type = "R", positionInContainer = 4)
  }
}

.qcAttributesChartsCreateChart <- function(container, dataset, options, ready, type, positionInContainer) {
  jaspTitle <- paste0(type, "plot")
  if (!is.null(container[[jaspTitle]])) {
    return()
  }

  plot <- createJaspPlot(title = gettextf("%s Chart", toupper(type)), width = options$plotWidth * 1.75, height = options$plotHeight)
  plot$dependOn(c("timeStamp", "palette"))
  plot$position <- positionInContainer
  container[[jaspTitle]] <- plot

  if (!ready || container$getError()) {
    return()
  }

  if (type == "p" || type == "Laney p'" || type == "u" || type == "Laney u'") {
    if (options[["defects"]] != "" && options[["sampleSize"]] != "") {
      higher <- dataset[[options[["defects"]]]] > dataset[[options[["sampleSize"]]]]
      if (any(higher)) {
        plot$setError(gettextf("%1$s row(s) in the data contain more nonconformities than units.", length(which(higher))))
        return()
      }
    }
  }

  palette <- .qcColorPalette(options)
  defects <- dataset[[options[["defects"]]]]
  totals <- dataset[[options[["sampleSize"]]]]
  proportions <- defects / totals

  plotTitle <- switch(type,
    "np" = gettext("Frequency"),
    "c" = gettext("Frequency"),
    "u" = gettext("Average"),
    "p" = gettext("Proportion"),
    "Laney p'" = gettext("Proportion"),
    "Laney u'" = gettext("Proportion"),
    "xbar" = gettext("Proportion"),
    "R" = gettext("Moving range")
  )

  p_try <- try({
    if (type == "np" || type == "c" || type == "u") {
      qccOutput <- qcc::qcc(data = defects, sizes = totals, type = type, plot = FALSE)
      center <- qccOutput[["center"]]
      ucl <- qccOutput[["limits"]][, 2]
      lcl <- qccOutput[["limits"]][, 1]
      statistics <- qccOutput$statistics
      fill <- ifelse(NelsonLaws(qccOutput, chart = "c")$red_points, palette[3], palette[2])
      plotTitle <- switch(type,
        "np" = gettext("Frequency"),
        "c" = gettext("Frequency"),
        "u" = gettext("Average")
      )
    } else if (type == "p") {
      qccOutput <- qcc::qcc(data = defects, sizes = totals, type = "p", plot = FALSE)
      center <- qccOutput[["center"]]
      ucl <- qccOutput[["limits"]][, 2]
      lcl <- qccOutput[["limits"]][, 1]
      if (min(totals) / max(totals) >= 0.75) {
        p.hat <- mean(qccOutput[["statistics"]])
        n.mean <- mean(totals)
        ucl <- p.hat + 3 * (sqrt(p.hat * (1 - p.hat))) / (sqrt(n.mean))
        lcl <- p.hat - 3 * (sqrt(p.hat * (1 - p.hat))) / (sqrt(n.mean))
      }
      statistics <- qccOutput$statistics
      fill <- ifelse(qccOutput$statistics > ucl | qccOutput$statistics < lcl, palette[3], palette[2])
    } else if (type == "Laney p'" || type == "Laney u'") {
      center <- sum(defects) / sum(totals)
      z_scores <- switch(type,
        "Laney p'" = (proportions - center) / sqrt(center * (1 - center) / totals[1]),
        "Laney u'" = (proportions - center) / sqrt(center / totals[1])
      )
      R <- numeric(length(z_scores) - 1)
      for (i in 1:length(R)) {
        R[i] <- abs(z_scores[i + 1] - z_scores[i])
      }
      MR_mean <- sum(R) / (length(proportions) - 1)
      sigma <- MR_mean / 1.128
      if (type == "Laney p'") {
        lcl <- ifelse(center - 3 * sqrt(center * (1 - center) / totals) * sigma < 0, yes = 0, no = center - 3 * sqrt(center * (1 - center) / totals) * sigma)
        ucl <- center + 3 * sqrt(center * (1 - center) / totals) * sigma
      } else {
        lcl <- ifelse(center - 3 * sqrt(center / totals) * sigma < 0, yes = 0, no = center - 3 * sqrt(center / totals) * sigma)
        ucl <- center + 3 * sqrt(center / totals) * sigma
      }
      statistics <- proportions
      fill <- ifelse(proportions > ucl | proportions < lcl, palette[3], palette[2])
    } else if (type == "xbar") {
      qccOutput <- qcc::qcc(data = proportions, type = "xbar.one", plot = FALSE)
      center <- qccOutput[["center"]]
      ucl <- qccOutput[["limits"]][, 2]
      lcl <- qccOutput[["limits"]][, 1]
      statistics <- qccOutput$statistics
      fill <- ifelse(qccOutput$statistics > ucl | qccOutput$statistics < lcl, palette[3], palette[2])
    } else if (type == "R") {
      qccOutput <- qcc::qcc(data = matrix(c(proportions[1:length(proportions) - 1], proportions[2:length(proportions)]), ncol = 2), type = "R", plot = FALSE)
      center <- qccOutput[["center"]]
      ucl <- max(qccOutput[["limits"]])
      lcl <- min(qccOutput[["limits"]])
      statistics <- qccOutput[["statistics"]]
      fill <- ifelse(statistics > ucl | statistics < lcl, palette[3], palette[2])
    }

    plotData <- data.frame(group = 1:length(statistics), stat = statistics, fill = fill)
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, lcl, plotData[["stat"]], ucl))

    # TODO: Find a way to nicely display x-breaks
    xBreaks <- seq(1, nrow(plotData), length.out = min(20, nrow(plotData)))
    if (options[["timeStamp"]] != "") {
      xLabels <- dataset[[options[["timeStamp"]]]][xBreaks]
    } else {
      xLabels <- xBreaks
    }

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
      ggplot2::scale_x_continuous(name = gettext("Sample"), breaks = xBreaks, limits = c(1, max(xBreaks) * 1.15), labels = xLabels) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(legend.position = "none")

    if (options[["timeStamp"]] != "") {
      p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5))
    }
  })

  if (isTryError(p_try)) {
    plot$setError(gettextf("Plotting not possible: %s", jaspBase:::.extractErrorMessage(p_try)))
  } else {
    plot$plotObject <- p
  }
}

.qcAttributesChartsNelsonTable <- function(container, dataset, options, ready, type, positionInContainer) {
  table <- createJaspTable(title = gettextf("Nelson Law Tests for %s Chart", type))
  table$dependOn("timeStamp")
  table$position <- positionInContainer

  table$showSpecifiedColumnsOnly <- TRUE
  table$addFootnote(message = gettext("Numbers are data points (rows) where test violations occur."))

  jaspTitle <- paste0(type, "ChartTable")
  container[[jaspTitle]] <- table

  if (!ready || container$getError()) {
    return()
  }

  defects <- dataset[[options[["defects"]]]]
  totals <- dataset[[options[["sampleSize"]]]]
  proportions <- defects / totals
  if (type == "np" || type == "c" || type == "u" || type == "Laney p'" || type == "Laney u'" || type == "p") {
    qccOutput <- qcc::qcc(data = defects, sizes = totals, type = "p", plot = FALSE)
  } else if (type == "xbar") {
    qccOutput <- qcc::qcc(data = proportions, type = "xbar.one", plot = FALSE)
  } else if (type == "R") {
    qccOutput <- qcc::qcc(data = matrix(c(proportions[1:length(proportions) - 1], proportions[2:length(proportions)]), ncol = 2), type = "R", plot = FALSE)
  }
  labels <- if (options[["timeStamp"]] != "") options[["timeStamp"]] else 1:nrow(dataset)

  if (type == "np" || type == "c" || type == "u" || type == "Laney p'" || type == "Laney u'" || type == "R") {
    test <- NelsonLaws(data = qccOutput, xLabels = labels, chart = "c")
  } else if (type == "p") {
    test <- NelsonLaws(data = qccOutput, xLabels = labels, chart = "p")
  } else {
    test <- NelsonLaws(data = qccOutput, xLabels = labels)
  }

  if (length(test$Rules$R1) > 0) {
    table$addColumnInfo(name = "test1", title = gettextf("Test 1: Beyond limit"), type = "integer")
  }

  if (length(test$Rules$R2) > 0) {
    table$addColumnInfo(name = "test2", title = gettextf("Test 2: Shift"), type = "integer")
  }

  if (length(test$Rules$R3) > 0) {
    table$addColumnInfo(name = "test3", title = gettextf("Test 3: Trend"), type = "integer")
  }

  if (type == "Range" & length(labels) == 0) {
    table$setData(list(
      "test1" = c(test$Rules$R1 + 1),
      "test2" = c(test$Rules$R2 + 1),
      "test3" = c(test$Rules$R3 + 1)
    ))
  } else {
    table$setData(list(
      "test1" = c(test$Rules$R1),
      "test2" = c(test$Rules$R2),
      "test3" = c(test$Rules$R3)
    ))
  }
}

# .AReport <- function(ccTitle = "", ccName = "", ccOperator = "", ccID = "", ccMisc = "", ccAppraiser = "", ccMeasurement = "", ccSize = "",
#                      ccTime = "", ccFrequency = "") {
#   Report <- createJaspContainer(gettext("Report"))

#   if (ccTitle == "") {
#     title <- gettext("Control Charts for Attributes Report")
#   } else {
#     title <- ccTitle
#   }
#   name <- gettextf("Service name: %s", ccName)
#   operator <- gettextf("Operator: %s", ccOperator)
#   ID <- gettextf("Identification: %s", ccID)
#   Appraiser <- gettextf("Appraiser: %s", ccAppraiser)
#   Measurement <- gettextf("Measurement system: %s", ccMeasurement)
#   Size <- gettextf("Name of Size: %s", ccSize)
#   Time <- gettextf("Time: %s", ccTime)
#   Frequency <- gettextf("Frequency: %s", ccFrequency)


#   text1 <- c(name, operator)
#   text2 <- c(ID, Appraiser)
#   text3 <- c(Measurement, Time)
#   text4 <- c(Size, Frequency)

#   matrixPlot <- createJaspPlot(width = 1200, aspectRatio = 1, position = 1)
#   plotMat <- matrix(list(), 2, 2, T)
#   plotMat[[1, 1]] <- .ggplotWithText(text1)
#   plotMat[[1, 2]] <- .ggplotWithText(text2)
#   plotMat[[2, 1]] <- .ggplotWithText(text3)
#   plotMat[[2, 2]] <- .ggplotWithText(text4)

#   p <- jaspGraphs::ggMatrixPlot(plotMat, topLabels = c(title, ""))
#   matrixPlot$plotObject <- p

#   return(matrixPlot)
# }
