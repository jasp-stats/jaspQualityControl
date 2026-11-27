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
attributesChartsInternal <- function(jaspResults, dataset, options) {

  # reading variables in from the GUI
  total <- options$total
  D <- options[["defectiveOrDefect"]]
  timeStamp <- options$timeStamp

  numeric_variables <- c(total, D)
  numeric_variables  <- numeric_variables[numeric_variables != ""]

  # Data reading
  if (is.null(dataset)) {
    if (identical(timeStamp, ""))
      dataset <- .readDataSetToEnd(columns.as.numeric = numeric_variables)
    else
      dataset <- .readDataSetToEnd(columns.as.numeric = numeric_variables, columns.as.factor = timeStamp)
  }

  # Checking if the analysis is ready
  ready <- options[["defectiveOrDefect"]] != "" && options$total != ""

  if ((options[["attributesChart"]] == "defectives" || options[["attributesChart"]] == "defects" || options[["attributesChart"]] == "xmr") && !ready) {
    plot <- createJaspPlot(title = gettext("Attributes Control Charts"), width = 700, height = 400)
    jaspResults[["plot"]] <- plot
    plot$dependOn(c("attributesChart", "defectiveOrDefect", "total"))
    return()
  }

  # Validate only the active family; hidden chart selections retain their values.
  binomialChart <- options[["attributesChart"]] == "defectives"
  rateChart <- options[["attributesChart"]] == "defects" &&
    options[["attributesChartDefectsChartType"]] %in% c("uChart", "laneyUPrimeChart")
  countChart <- binomialChart || options[["attributesChart"]] == "defects"

  # Rows with a missing count, sample size or timestamp (e.g. blank rows below shorter columns)
  # are removed and reported in a note instead of stopping the analysis.
  completeRows <- stats::complete.cases(dataset)
  nRemovedRows <- sum(!completeRows)
  sampleNumbers <- which(completeRows)
  dataset      <- dataset[completeRows, , drop = FALSE]
  xLabs <- if (identical(timeStamp, "")) sampleNumbers else as.vector(dataset[[timeStamp]])
  axisLabels <- if (!identical(timeStamp, "") || !identical(sampleNumbers, seq_len(nrow(dataset)))) xLabs
  if (nRemovedRows > 0) {
    missingValuesNote <- createJaspHtml(gettextf("<i>Note.</i> Removed %i observation(s) with missing values.", nRemovedRows))
    missingValuesNote$dependOn(c("defectiveOrDefect", "total", "timeStamp", "attributesChart"))
    missingValuesNote$position <- 3
    jaspResults[["missingValuesNote"]] <- missingValuesNote
  } else {
    jaspResults[["missingValuesNote"]] <- NULL
  }
  if (nrow(dataset) == 0)
    .quitAnalysis(gettext("No observations remain after removing rows with missing values."))

  movingRangeChart <- options[["attributesChart"]] == "xmr" ||
    (binomialChart && options[["attributesChartDefectivesChartType"]] == "laneyPPrimeChart") ||
    (rateChart && options[["attributesChartDefectsChartType"]] == "laneyUPrimeChart")
  if (movingRangeChart && !any(diff(sampleNumbers) == 1L))
    .quitAnalysis(gettext("At least two consecutive samples with complete data are required to estimate moving ranges."))

  # c charts also consume the assigned sample-size column for the equal-size check.
  .hasErrors(dataset, type = c("infinity", "negativeValues"),
             all.target = c(D, total), exitAnalysisIfErrors = TRUE)

  if (countChart) {
    .hasErrors(dataset, custom = function() {
      counts <- dataset[[D]]
      if (any(counts != floor(counts)))
        return(gettext("Defective item and defect counts must be nonnegative integers."))
      if (binomialChart || rateChart) {
        sizes <- dataset[[total]]
        if (any(sizes <= 0))
          return(gettext("Sample sizes or exposures must be positive."))
        if (binomialChart && any(sizes != floor(sizes)))
          return(gettext("Sample sizes for defective items must be positive integers."))
        if (binomialChart && any(counts > sizes))
          return(gettext("The number of defective items cannot exceed the sample size."))
      }
    }, exitAnalysisIfErrors = TRUE)
  }

  if (options[["attributesChart"]] == "defectives" && ready) {
    dependedDefectives <- c("total", "defectiveOrDefect", "attributesChart", "attributesChartDefectivesChartType", 'timeStamp')

    #P chart
    if (options[["attributesChartDefectivesChartType"]] == "pChart" && is.null(jaspResults[["PchartPlot"]])) {
      jaspResults[["PchartPlot"]] <- createJaspPlot(title =  gettext("p Chart"), width = 1200, height = 500)
      jaspResults[["PchartPlot"]]$dependOn(dependedDefectives)
      jaspResults[["PchartPlot"]]$position <- 1
      Pchart <- .Pchart(dataset, options, sampleNumbers, axisLabels)
      jaspResults[["PchartPlot"]]$plotObject <- Pchart$p

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer("")
        jaspResults[["NelsonTable"]] <- .NelsonTable(Pchart$sixsigma, sampleNumbers, xLabs, "P")
        jaspResults[["NelsonTable"]]$dependOn(dependedDefectives)
        jaspResults[["NelsonTable"]]$position <- 2
      }
    }
    #NP chart
    if (options[["attributesChartDefectivesChartType"]] == "npChart" && is.null(jaspResults[["NPchartPlot"]])) {
      jaspResults[["NPchartPlot"]] <- createJaspPlot(title =  gettext("np Chart"), width = 1200, height = 500)
      jaspResults[["NPchartPlot"]]$dependOn(dependedDefectives)
      jaspResults[["NPchartPlot"]]$position <- 1
      NPchart <- .NPchart(dataset, options, sampleNumbers, axisLabels)
      jaspResults[["NPchartPlot"]]$plotObject <- NPchart$p

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer("")
        jaspResults[["NelsonTable"]] <- .NelsonTable(NPchart$sixsigma, sampleNumbers, xLabs, "np")
        jaspResults[["NelsonTable"]]$position <- 2
        jaspResults[["NelsonTable"]]$dependOn(dependedDefectives)
      }
    }

    #Laney P chart
    if (options[["attributesChartDefectivesChartType"]] == "laneyPPrimeChart" && is.null(jaspResults[["LaneyPPlot"]])) {
      jaspResults[["LaneyPPlot"]] <- createJaspPlot(title =  gettext("Laney p' Chart"), width = 1200, height = 500, position = 1)
      Lanychart <- .LanyP(dataset, options, sampleNumbers, axisLabels)
      jaspResults[["LaneyPPlot"]]$plotObject <- Lanychart$p
      jaspResults[["LaneyPPlot"]]$dependOn(dependedDefectives)

      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspPlot(title =  "", position = 2)
        jaspResults[["NelsonTable"]] <- .NelsonTable(Lanychart$sixsigma, sampleNumbers, xLabs, "Laney p'")
        jaspResults[["NelsonTable"]]$dependOn(dependedDefectives)
      }
    }
  }

  if (options[["attributesChart"]] == "defects" && ready) {
    dependedDefects <- c("defectiveOrDefect", "attributesChart", "attributesChartDefectsChartType","total", "timeStamp")
    #Cchart
    if (options[["attributesChartDefectsChartType"]] == "cChart" && is.null(jaspResults[["CchartPlot"]])) {
      jaspResults[["CchartPlot"]] <- createJaspPlot(title =  gettext("c Chart"), width = 1200, height = 500)
      jaspResults[["CchartPlot"]]$dependOn(dependedDefects)
      jaspResults[["CchartPlot"]]$position <- 1
      Cchart <- .Cchart(dataset, options, sampleNumbers, axisLabels)
      jaspResults[["CchartPlot"]]$plotObject <- PlotReport <- Cchart$p

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer()
        jaspResults[["NelsonTable"]] <- .NelsonTable(Cchart$sixsigma, sampleNumbers, xLabs, "c")
        jaspResults[["NelsonTable"]]$dependOn(dependedDefects)
        jaspResults[["NelsonTable"]]$position <- 2
      }
    }
    #Uchart
    if (options[["attributesChartDefectsChartType"]] == "uChart" && is.null(jaspResults[["UchartPlot"]])) {
      jaspResults[["UchartPlot"]] <- createJaspPlot(title =  gettext("u Chart"), width = 1200, height = 500)
      jaspResults[["UchartPlot"]]$dependOn(dependedDefects)
      jaspResults[["UchartPlot"]]$position <- 1
      Uchart <- .Uchart(dataset, options, sampleNumbers, axisLabels)
      jaspResults[["UchartPlot"]]$plotObject <- Uchart$p
        PlotReport <- Uchart$p

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer("")
        jaspResults[["NelsonTable"]] <- .NelsonTable(Uchart$sixsigma, sampleNumbers, xLabs, "u")
        jaspResults[["NelsonTable"]]$dependOn(dependedDefects)
        jaspResults[["NelsonTable"]]$position <- 2
      }
    }
    #Laney U chart
    if (options[["attributesChartDefectsChartType"]] == "laneyUPrimeChart" && is.null(jaspResults[["LaneyUPlot"]])) {
      jaspResults[["LaneyUPlot"]] <- createJaspPlot(title = gettext("Laney u' Chart"), width = 1200, height = 500, position = 1)
      jaspResults[["LaneyUPlot"]]$dependOn(dependedDefects)
      LanyUchart <- .LanyU(dataset, options, sampleNumbers, axisLabels)
      jaspResults[["LaneyUPlot"]]$plotObject <- PlotReport <- LanyUchart$p

      jaspResults[["NelsonTable"]] <- createJaspContainer(title = "", position = 2)
      jaspResults[["NelsonTable"]] <- .NelsonTable(LanyUchart$sixsigma, sampleNumbers, xLabs, "Laney u'")
      jaspResults[["NelsonTable"]]$dependOn(dependedDefects)
    }
  }
  #ImRchart for attributes
  if (options[["attributesChart"]] == "xmr" && !identical(D, "")) {
    jaspResults[["IPlotA"]] <- createJaspPlot(title = gettext("Individuals and Moving Range Chart"), width = 1200, height = 500, position = 1)
    IMRchart <- .Ichart_attributes(dataset, options, sampleNumbers, axisLabels)
    jaspResults[["IPlotA"]]$plotObject <- PlotReport <- IMRchart$p
    jaspResults[["IPlotA"]]$dependOn(c("defectiveOrDefect", "total", "attributesChart", "timeStamp"))

    # Nelson tests tables
    if (is.null(jaspResults[["NelsonTableIMR"]])) {
      NelsonTables <- createJaspContainer(title =  "", position = 2)
      NelsonTables$dependOn(c("defectiveOrDefect", "total", "attributesChart", "timeStamp"))


      NelsonTables[["NelsonTableI"]] <- .NelsonTable(IMRchart$sixsigma_I, sampleNumbers, xLabs, "Individuals", allsix = TRUE)
      rangeEnds <- IMRchart$rangeEnds
      NelsonTables[["NelsonTableMR"]] <- .NelsonTable(IMRchart$sixsigma_R, sampleNumbers[rangeEnds], xLabs[rangeEnds], "Moving Range")

      jaspResults[["NelsonTableIMR"]] <- NelsonTables
    }
  }

  #Report
  if (options[["report"]]) {
    jaspResults[["PchartPlot"]]     <- NULL
    jaspResults[["NelsonTable"]]    <- NULL
    jaspResults[["NPchartPlot"]]    <- NULL
    jaspResults[["LaneyPPlot"]]     <- NULL
    jaspResults[["CchartPlot"]]     <- NULL
    jaspResults[["UchartPlot"]]     <- NULL
    jaspResults[["LaneyUPlot"]]     <- NULL
    jaspResults[["IPlotA"]]         <- NULL
    jaspResults[["NelsonTableIMR"]] <- NULL

    plotHeight <- if (options[["attributesChart"]] == "xmr") 1000 else 500
    reportPlot <- createJaspPlot(title = gettext("Report for Attribute Control Charts"), width = 1250, height = plotHeight)
    jaspResults[["report"]] <- reportPlot
    jaspResults[["report"]]$dependOn(c("defectiveOrDefect", "total", "timeStamp", "attributesChart",
                                       "attributesChartDefectivesChartType", "attributesChartDefectsChartType",
                                       "report", "reportMetaData", "reportTitle", "reportMeasurementName", "reportMeasurementNameText",
                                       "reportPerformedBy", "reportPerformedByText", "reportId", "reportIdText",
                                       "reportAppraiser", "reportAppraiserText", "reportMeasusrementSystemName",
                                       "reportMeasusrementSystemNameText", "reportSubgroupSize", "reportSubgroupSizeText",
                                       "reportTime", "reportTimeText", "reportFrequency", "reportFrequencyText"))

    # Plot meta data
    if (options[["reportTitle"]] ) {
      title <- if (options[["reportTitleText"]] == "") gettext("Report for Attribute Control Charts") else options[["reportTitleText"]]
    } else {
      title <- ""
    }

    if (options[["reportMetaData"]]) {
      text <- c()
      text <- if (options[["reportMeasurementName"]]) c(text, gettextf("Measurement name: %s", options[["reportMeasurementNameText"]])) else text
      text <- if (options[["reportPerformedBy"]]) c(text, gettextf("Performed by: %s", options[["reportPerformedByText"]])) else text
      text <- if (options[["reportId"]]) c(text, gettextf("ID: %s", options[["reportIdText"]])) else text
      text <- if (options[["reportAppraiser"]]) c(text, gettextf("Appraiser: %s", options[["reportAppraiserText"]])) else text
      text <- if (options[["reportMeasusrementSystemName"]]) c(text, gettextf("Measurement system: %s", options[["reportMeasusrementSystemNameText"]])) else text
      text <- if (options[["reportSubgroupSize"]]) c(text, gettextf("Subgroup size: %s", options[["reportSubgroupSizeText"]])) else text
      text <- if (options[["reportTime"]]) c(text, gettextf("Time: %s", options[["reportTimeText"]])) else text
      text <- if (options[["reportFrequency"]]) c(text, gettextf("Frequency: %s", options[["reportFrequencyText"]])) else text
    } else {
      text <- NULL
    }

    if (options[["attributesChart"]] == "defectives" & options[["attributesChartDefectivesChartType"]] == "pChart")
      attributeChart <- .Pchart(dataset, options, sampleNumbers, axisLabels)$p
    else if (options[["attributesChart"]] == "defectives" & options[["attributesChartDefectivesChartType"]] == "npChart")
      attributeChart <- .NPchart(dataset, options, sampleNumbers, axisLabels)$p
    else if (options[["attributesChart"]] == "defectives" & options[["attributesChartDefectivesChartType"]] == "laneyPPrimeChart")
      attributeChart <- .LanyP(dataset, options, sampleNumbers, axisLabels)$p
    else if (options[["attributesChart"]] == "defects" & options[["attributesChartDefectsChartType"]] == "cChart")
      attributeChart <- .Cchart(dataset, options, sampleNumbers, axisLabels)$p
    else if (options[["attributesChart"]] == "defects" & options[["attributesChartDefectsChartType"]] == "uChart")
      attributeChart <- .Uchart(dataset, options, sampleNumbers, axisLabels)$p
    else if (options[["attributesChart"]] == "defects" & options[["attributesChartDefectsChartType"]] == "laneyUPrimeChart")
      attributeChart <- .LanyU(dataset, options, sampleNumbers, axisLabels)$p
    else if (options[["attributesChart"]] == "xmr")
      attributeChart <- .Ichart_attributes(dataset, options, sampleNumbers, axisLabels)$p

    plots <- list(attributeChart)
    reportPlotObject <- .qcReport(text = text, plots = plots, textMaxRows = 8,
                                  reportTitle = title)
    reportPlot$plotObject <- reportPlotObject
  }
}

.Pchart <- function(dataset, options, sampleNumbers, axisLabels) {
  data1 <- data.frame(D = dataset[, options[["defectiveOrDefect"]]], sample = dataset[, options$total])
  sixsigma <- with(data1, qcc::qcc(D, sample, type = "p", plot = FALSE))
  sample <- data1$sample
  D <- data1$D
  subgroups = c(1:length(sixsigma$statistics))
  data_plot <- data.frame(subgroups = subgroups, P = sixsigma$statistics)
  center <- sixsigma$center
  # Limits per subgroup size around the pooled CL, bounded to [0, 1] (qcc), as in Minitab
  UCL <- sixsigma$limits[,2]
  LCL <- sixsigma$limits[,1]
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data_plot$P ,UCL))
  yLimits <- range(c(0,yBreaks * 1.2))
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1]) + 0.5
  else
    xBreaks <- c(subgroups) + 0.5
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center),
    l = c(
      gettextf("CL = %g", round(center, 4))
    )
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = center, col = "green") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  # both limits are checked: with UCL capped at 1 for every subgroup, LCL can still vary
  if (length(unique(UCL)) == 1 && length(unique(LCL)) == 1){
    UCL <- UCL[1]
    LCL <- LCL[1]
    dfLabel <- data.frame(
      x = max(xLimits) * .95,
      y = c(center, UCL, LCL),
      l = c(
        gettextf("CL = %g", round(center, 4)),
        gettextf("UCL = %g",   round(UCL, 5)),
        gettextf("LCL = %g",   round(LCL, 5))
      )
    )
    xBreaks <- xBreaks -0.5

    p <- p +
      ggplot2::geom_hline(yintercept = c(UCL, LCL), col = "red", linetype = "dashed", size = 1.5) +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
      .attributesLine(sampleNumbers, data = data_plot, ggplot2::aes(x = subgroups, y = P),col = "blue") +
      jaspGraphs::geom_point(data = data_plot, ggplot2::aes(x = subgroups, y = P),size = 4, fill = ifelse(.attributesTests(sixsigma, sampleNumbers)$red_points, 'red', 'blue'))
  } else{
    n <- length(UCL)
    p <- p + ggplot2::geom_step(ggplot2::aes(x = subgroups, y = UCL, col = "red"), size = 1.5, linetype = "F1") +
      .attributesLine(sampleNumbers, ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),col = "blue") +
      jaspGraphs::geom_point(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),size = 4, fill = ifelse(.attributesTests(sixsigma, sampleNumbers)$red_points, 'red', 'blue')) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = xBreaks - 0.5) +
      ggplot2::geom_step(ggplot2::aes(x = subgroups, y = LCL, col = "red"), size = 1.5, linetype = "F1") +
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = UCL[n], col = "red"), size = 1.5)+
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = LCL[n], col = "red"), size = 1.5)
  }

  if (!is.null(axisLabels))
    p <- p + ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = .attributesSampleLabels(axisLabels, xBreaks))

  return(list(p = p, sixsigma = sixsigma))
}
.NPchart <- function(dataset, options, sampleNumbers, axisLabels) {
  .Check_equal_samples(dataset, options)

  data1 <- data.frame(D = dataset[, options[["defectiveOrDefect"]]], sample = dataset[, options$total])
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, D = data1$D)

  sixsigma <- with(data1, qcc::qcc(D, sample, type = "np", plot = FALSE))
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data1$D ,UCL))
  yLimits <- range(yBreaks)
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 4)),
      gettextf("UCL = %g",   round(UCL, 5)),
      gettextf("LCL = %g",   round(LCL, 5))
    )
  )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = D)) +
    ggplot2::geom_hline(yintercept =  center, col = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), col = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Number of defectives"),limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_rangeframe() +
    .attributesLine(sampleNumbers, col = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(.attributesTests(sixsigma, sampleNumbers)$red_points, 'red', 'blue')) +
    jaspGraphs::themeJaspRaw()

  if (!is.null(axisLabels))
    p <- p + ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = .attributesSampleLabels(axisLabels, xBreaks))

  return(list(p = p, sixsigma = sixsigma))
}
.Cchart <- function(dataset, options, sampleNumbers, axisLabels) {

  .Check_equal_samples(dataset, options)

  data1 <- data.frame(D = dataset[, options[["defectiveOrDefect"]]], sample = dataset[, options$total])
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, D = data1$D)
  sixsigma <- with(data1, qcc::qcc(D, sample, type = "c", plot = FALSE))
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data1$D ,UCL))
  yLimits <- range(yBreaks)
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 4)),
      gettextf("UCL = %g",   round(UCL, 5)),
      gettextf("LCL = %g",   round(LCL, 5))
    )
  )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = D)) +
    ggplot2::geom_hline(yintercept =  center, col = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), col = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("D") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    .attributesLine(sampleNumbers, col = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(.attributesTests(sixsigma, sampleNumbers)$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (!is.null(axisLabels))
    p <- p + ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = .attributesSampleLabels(axisLabels, xBreaks))

  return(list(p = p, sixsigma = sixsigma))
}

.Uchart <- function(dataset, options, sampleNumbers, axisLabels) {
  data1 <- data.frame(D = dataset[, options[["defectiveOrDefect"]]], sample = dataset[, options$total])
  data1$P <- data1$D/data1$sample
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, P = data1$P)

  sixsigma <- with(data1, qcc::qcc(D, sample, type = "u", plot = FALSE))
  center <- sixsigma$center
  UCL <- sixsigma$limits[,2]
  LCL <- sixsigma$limits[,1]
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data_plot$P ,UCL))
  yLimits <- range(c(0,yBreaks * 1.2))
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1]) + 0.5
  else
    xBreaks <- c(subgroups) + 0.5
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center),
    l = c(
      gettextf("CL = %g", round(center, 4))
    )
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = center, col = "green") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Defects per unit") ,limits = yLimits, breaks = yBreaks) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (length(unique(UCL)) == 1){
    dfLabel <- data.frame(
      x = max(xLimits) * 0.95,
      y = c(center, UCL, LCL),
      l = c(
        gettextf("CL = %g", round(center, 4)),
        gettextf("UCL = %g",   round(UCL, 5)),
        gettextf("LCL = %g",   round(LCL, 5))
      )
    )
    xBreaks <- xBreaks -0.5

    p <- p +
      ggplot2::geom_hline(yintercept = c(UCL, LCL), col = "red", linetype = "dashed", size = 1.5) +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
      .attributesLine(sampleNumbers, data = data_plot, ggplot2::aes(x = subgroups, y = P),col = "blue") +
      jaspGraphs::geom_point(data = data_plot, ggplot2::aes(x = subgroups, y = P),size = 4, fill = ifelse(.attributesTests(sixsigma, sampleNumbers)$red_points, 'red', 'blue'))
  } else{

    n <- length(UCL)
    p <- p + ggplot2::geom_step(ggplot2::aes(x = subgroups, y = UCL, col = "red"), size = 1.5, linetype = "F1") +
      .attributesLine(sampleNumbers, ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),col = "blue") +
      jaspGraphs::geom_point(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),size = 4, fill = ifelse(.attributesTests(sixsigma, sampleNumbers)$red_points, 'red', 'blue')) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = xBreaks - 0.5) +
      ggplot2::geom_step(ggplot2::aes(x = subgroups, y = LCL, col = "red"), size = 1.5, linetype = "F1") +
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = UCL[n], col = "red"), size = 1.5)+
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = LCL[n], col = "red"), size = 1.5)

  }

  if (!is.null(axisLabels))
    p <- p + ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = .attributesSampleLabels(axisLabels, xBreaks))

  return(list(p = p, sixsigma = sixsigma))
}

##Imr for attributes
.Ichart_attributes <- function(dataset, options, sampleNumbers, axisLabels) {
  proportions <- dataset[[options[["defectiveOrDefect"]]]] / dataset[[options$total]]
  rangeEnds <- which(diff(sampleNumbers) == 1L) + 1L
  rangePairs <- cbind(proportions[rangeEnds - 1L], proportions[rangeEnds])
  sixsigma_R <- qcc::qcc(rangePairs, type = "R", plot = FALSE)
  # Use the same moving-range estimate as qcc's Individuals chart, with complete adjacent pairs.
  sigma <- sixsigma_R$center / qcc::qcc.options("exp.R.unscaled")[2L]
  sixsigma_I <- qcc::qcc(proportions, type = "xbar.one", std.dev = sigma, plot = FALSE)
  subgroups <- seq_along(proportions)
  data_plot <- data.frame(subgroups = subgroups, P = proportions)

  center <- sixsigma_I$center
  UCL <- max(sixsigma_I$limits)
  LCL <- min(sixsigma_I$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, data_plot$P, UCL))
  yLimits <- range(yBreaks)
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 4)),
      gettextf("UCL = %g",   round(UCL, 5)),
      gettextf("LCL = %g",   round(LCL, 5))
    )
  )

  p1 <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = P)) +
    ggplot2::geom_hline(yintercept =  center, col = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), col = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l), inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    .attributesLine(sampleNumbers, col = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(.attributesTests(sixsigma_I, sampleNumbers, allsix = TRUE)$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (!is.null(axisLabels))
    p1 <- p1 + ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = .attributesSampleLabels(axisLabels, xBreaks))

  subgroups <- seq_len(length(proportions) - 1L)
  data_plot <- data.frame(subgroups = rangeEnds - 1L, data = sixsigma_R$statistics)
  center <- sixsigma_R$center
  UCL <- max(sixsigma_R$limits)
  LCL <- min(sixsigma_R$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, data_plot$data, UCL))
  yLimits <- range(yBreaks)
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks) * 1.15)
  Xlabels <- xBreaks + 1
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 4)),
      gettextf("UCL = %g",   round(UCL, 5)),
      gettextf("LCL = %g",   round(LCL, 5))
    )
  )

  p2 <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = data)) +
    ggplot2::geom_hline(yintercept =  center, col = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), col = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Moving Range") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = Xlabels) +
    .attributesLine(sampleNumbers[rangeEnds], col = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(.attributesTests(sixsigma_R, sampleNumbers[rangeEnds])$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  # a moving range belongs to the later of its two samples, as in the default labels above
  if (!is.null(axisLabels))
    p2 <- p2 + ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = .attributesSampleLabels(axisLabels, xBreaks, offset = 1))

  p3 <-  jaspGraphs::ggMatrixPlot(plotList = list(p1, p2), layout = matrix(1:2, 2), removeXYlabels= "x")

  return(list(p = p3, sixsigma_I = sixsigma_I, sixsigma_R = sixsigma_R, rangeEnds = rangeEnds))
}

### Lany's charts
.LanyU <- function(dataset, options, sampleNumbers, axisLabels) {
  data1 <- data.frame(D = dataset[,options[["defectiveOrDefect"]]], sample = dataset[,options$total])
  data1$P <- data1$D/data1$sample
  subgroups <- c(1:nrow(data1))
  data_plot <- data.frame(subgroups = subgroups, P = data1$P)

  #Sixsigma
  center = sum(data1$D)/sum(data1$sample)
  z_scores <- (data1$P - center)/sqrt(center/data1$sample)
  movingRanges <- abs(diff(z_scores))[diff(sampleNumbers) == 1L]
  sigma <- mean(movingRanges) / 1.128
  LCL <- ifelse(center - 3*sqrt(center/data1$sample) * sigma < 0, 0, center - 3*sqrt(center/data1$sample) * sigma)
  UCL <- center + 3*sqrt(center/data1$sample) * sigma
  sixsigma <- list(statistics = data1$P, limits = data.frame(LCL, UCL), center = center)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data_plot$P ,UCL))
  yLimits <- range(c(0,yBreaks * 1.2))
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1]) + 0.5
  else
    xBreaks <- c(subgroups) + 0.5
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center),
    l = c(
      gettextf("CL = %g", round(center, 4))
    )
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = center, col = "green") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Defects per unit") ,limits = yLimits, breaks = yBreaks) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (length(UCL) == 1){
    dfLabel <- data.frame(
      x = max(xLimits) * .95,
      y = c(center, UCL, LCL),
      l = c(
        gettextf("CL = %g", round(center, 4)),
        gettextf("UCL = %g",   round(UCL, 5)),
        gettextf("LCL = %g",   round(LCL, 5))
      )
    )
    xBreaks <- xBreaks - 0.5

    p <- p +
      ggplot2::geom_hline(yintercept = c(UCL, LCL), col = "red", linetype = "dashed", size = 1.5) +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
      .attributesLine(sampleNumbers, data = data_plot, ggplot2::aes(x = subgroups, y = P),col = "blue") +
      jaspGraphs::geom_point(data = data_plot, ggplot2::aes(x = subgroups, y = P),size = 4, fill = ifelse(.attributesTests(sixsigma, sampleNumbers)$red_points, 'red', 'blue'))
  } else{
    n <- length(UCL)
    p <- p + ggplot2::geom_step(ggplot2::aes(x = subgroups, y = UCL, col = "red"), size = 1.5, linetype = "F1") +
      .attributesLine(sampleNumbers, ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),col = "blue") +
      jaspGraphs::geom_point(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),size = 4, fill = ifelse(.attributesTests(sixsigma, sampleNumbers)$red_points, 'red', 'blue')) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = xBreaks - 0.5) +
      ggplot2::geom_step(ggplot2::aes(x = subgroups, y = LCL, col = "red"), size = 1.5, linetype = "F1") +
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = UCL[n], col = "red"), size = 1.5)+
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = LCL[n], col = "red"), size = 1.5)

  }

  if (!is.null(axisLabels))
    p <- p + ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = .attributesSampleLabels(axisLabels, xBreaks))

  return(list(p = p, sixsigma = sixsigma))
}

.LanyP <- function(dataset, options, sampleNumbers, axisLabels) {
  data1 <- data.frame(D = dataset[,options[["defectiveOrDefect"]]], sample = dataset[,options$total])
  data1$P <- data1$D/data1$sample
  subgroups <- c(1:nrow(data1))
  data_plot <- data.frame(subgroups = subgroups, P = data1$P)

  #Sixsigma
  center = sum(data1$D)/sum(data1$sample)
  z_scores <- (data1$P - center)/sqrt(center*(1 - center)/data1$sample)
  movingRanges <- abs(diff(z_scores))[diff(sampleNumbers) == 1L]
  sigma <- mean(movingRanges) / 1.128
  LCL <- ifelse(center - 3*sqrt(center*(1 - center)/data1$sample) * sigma < 0, 0, center - 3*sqrt(center*(1 - center)/data1$sample) * sigma)
  UCL <- pmin(1, center + 3*sqrt(center*(1 - center)/data1$sample) * sigma)
  sixsigma <- list(statistics = data1$P, limits = data.frame(LCL, UCL), center = center)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data_plot$P ,UCL))
  yLimits <- range(c(0,yBreaks * 1.2))
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1]) + 0.5
  else
    xBreaks <- c(subgroups) + 0.5
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center),
    l = c(
      gettextf("CL = %g", round(center, 4))
    )
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = center, col = "green") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (length(UCL) == 1){
    dfLabel <- data.frame(
      x = max(xLimits) * .95,
      y = c(center, UCL, LCL),
      l = c(
        gettextf("CL = %g", round(center, 4)),
        gettextf("UCL = %g",   round(UCL, 5)),
        gettextf("LCL = %g",   round(LCL, 5))
      )
    )
    xBreaks <- xBreaks -0.5

    p <- p +
      ggplot2::geom_hline(yintercept = c(UCL, LCL), col = "red", linetype = "dashed", size = 1.5) +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
      .attributesLine(sampleNumbers, data = data_plot, ggplot2::aes(x = subgroups, y = P),col = "blue") +
      jaspGraphs::geom_point(data = data_plot, ggplot2::aes(x = subgroups, y = P),size = 4, fill = ifelse(.attributesTests(sixsigma, sampleNumbers)$red_points, 'red', 'blue'))
  } else{
    n <- length(UCL)
    p <- p + ggplot2::geom_step(ggplot2::aes(x = subgroups, y = UCL, col = "red"), size = 1.5, linetype = "F1") +
      .attributesLine(sampleNumbers, ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),col = "blue") +
      jaspGraphs::geom_point(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),size = 4, fill = ifelse(.attributesTests(sixsigma, sampleNumbers)$red_points, 'red', 'blue')) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = xBreaks - 0.5) +
      ggplot2::geom_step(ggplot2::aes(x = subgroups, y = LCL, col = "red"), size = 1.5, linetype = "F1") +
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = UCL[n], col = "red"), size = 1.5)+
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = LCL[n], col = "red"), size = 1.5)

  }

  if (!is.null(axisLabels))
    p <- p + ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = .attributesSampleLabels(axisLabels, xBreaks))

  return(list(p = p, sixsigma = sixsigma))
}
.attributesLine <- function(sampleNumbers, mapping = NULL, ...) {
  if (any(diff(sampleNumbers) != 1L)) {
    if (is.null(mapping))
      mapping <- ggplot2::aes()
    mapping$group <- ggplot2::aes(group = cumsum(c(TRUE, diff(sampleNumbers) != 1L)))$group
  }
  jaspGraphs::geom_line(mapping = mapping, ...)
}

# Evaluate runs within each uninterrupted stretch, using the overall chart limits.
.attributesTests <- function(sixsigma, sampleNumbers, allsix = FALSE) {
  chart <- if (allsix) "i" else "c"
  if (length(sampleNumbers) > 1L && all(diff(sampleNumbers) == 1L))
    return(NelsonLaws(sixsigma, allsix = allsix, chart = chart))

  rules <- if (allsix) c("R1", "R2", "R3", "R5", "R7", "R8") else c("R1", "R2", "R3")
  flagged <- setNames(vector("list", length(rules)), rules)
  redPoints <- rep(FALSE, length(sampleNumbers))
  runs <- split(seq_along(sampleNumbers), cumsum(c(TRUE, diff(sampleNumbers) != 1L)))
  for (rows in runs) {
    limits <- sixsigma$limits
    if (nrow(limits) > 1L)
      limits <- limits[rows, , drop = FALSE]
    statistics <- sixsigma$statistics[rows]
    if (length(rows) == 1L) {
      beyond <- (statistics < limits[, 1] | statistics > limits[, 2]) %in% TRUE
      redPoints[rows] <- beyond
      flagged$R1 <- c(flagged$R1, rows[beyond])
    } else {
      result <- NelsonLaws(list(statistics = statistics, limits = limits, center = sixsigma$center),
                           allsix = allsix, chart = chart)
      redPoints[rows] <- result$red_points
      flagged <- Map(function(previous, current) c(previous, rows[current]), flagged, result$Rules)
    }
  }
  list(red_points = redPoints, Rules = flagged)
}

# Breaks can be thinned or shifted by half a sample for stepped limits.
.attributesSampleLabels <- function(labels, breaks, offset = 0L) {
  labels <- as.character(labels)[floor(breaks + 1e-8) + offset]
  labels[is.na(labels)] <- ""
  labels
}

.Check_equal_samples <- function(dataset,options) {
  .hasErrors(
    dataset,
    all.target = options$total,
    custom = function() {
      if (length(unique(dataset[[options$total]])) > 1)
        return("Samples must be equal in size")
    },
    exitAnalysisIfErrors = TRUE
  )
}

.NelsonTable <- function(sixsigma, sampleNumbers, xLabels, name, allsix = FALSE) {
  table <- createJaspTable(title = gettextf("Test results for %s chart", name))
  if (length(sixsigma$statistics) == 1L)
    return(table)

  labels <- list(
    R1 = gettext("Test 1: Beyond limit"),
    R2 = gettext("Test 2: Shift"),
    R3 = gettext("Test 3: Trend"),
    R5 = gettext("Test 5: Beyond 2 sigma, same side"),
    R7 = gettext("Test 7: Reducing variation"),
    R8 = gettext("Test 8: Bimodal distribution")
  )
  flagged <- .attributesTests(sixsigma, sampleNumbers, allsix)$Rules
  columns <- paste0("test", substring(names(flagged), 2L))
  for (i in which(lengths(flagged) > 0L))
    table$addColumnInfo(name = columns[i], title = labels[[names(flagged)[i]]], type = "integer")
  table$setData(setNames(lapply(flagged, function(rows) xLabels[rows]), columns))

  table$showSpecifiedColumnsOnly <- TRUE
  table$addFootnote(message = gettext("Points where a test failed."))
  return(table)
}
