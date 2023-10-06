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
attributesCharts <- function(jaspResults, dataset, options) {

  # reading variables in from the GUI
  total <- options$total
  D <- options[["defectiveOrDefect"]]
  timeStamp <- options$timeStamp

  numeric_variables <- c(total, D)
  numeric_variables  <- numeric_variables[numeric_variables != ""]

  # Data reading
  if (is.null(dataset))
    if (!identical(timeStamp, "")) {
      dataset <- .readDataSetToEnd(columns.as.numeric = numeric_variables, columns.as.factor = timeStamp)
      xLabs <- as.vector(dataset[, timeStamp])
    } else {
      dataset <- .readDataSetToEnd(columns.as.numeric = numeric_variables)
      xLabs <- NULL
    }

  # Checking if the analysis is ready
  ready <- options[["defectiveOrDefect"]] != "" && options$total != ""

  #Checking for errors in the dataset
  .hasErrors(dataset, type = c('infinity', 'missingValues', 'negativeValues'),
             all.target = c(options[["defectiveOrDefect"]],options$total),
             exitAnalysisIfErrors = TRUE)

  if (options[["attributesChartDefectivesChartType"]] == "pChart" || options[["attributesChartDefectivesChartType"]] == "laneyPPrimeChart" || options[["attributesChartDefectsChartType"]] == "uChart" || options[["attributesChartDefectsChartType"]] == "laneyUPrimeChart")
    .hasErrors(dataset,
               custom = function() {
                 if (any(dataset[,D] > dataset[,total]))
                   return(gettext("The number of Defectives/Defects is larger than the sample  size."))},
               exitAnalysisIfErrors = TRUE)


  if ((options[["attributesChart"]] == "defectives" || options[["attributesChart"]] == "defects" || options[["attributesChart"]] == "xmr") && !ready) {
    plot <- createJaspPlot(title = gettext("Attributes Control Charts"), width = 700, height = 400)
    jaspResults[["plot"]] <- plot
    plot$dependOn(c("attributesChart", "defectiveOrDefect", "total"))
    return()
  }

  dataset <- na.omit(dataset)

  if (options[["attributesChart"]] == "defectives" && ready) {
    dependedDefectives <- c("total", "defectiveOrDefect", "attributesChart", "attributesChartDefectivesChartType", 'timeStamp')

    #P chart
    if (options[["attributesChartDefectivesChartType"]] == "pChart" && is.null(jaspResults[["PchartPlot"]])) {
      jaspResults[["PchartPlot"]] <- createJaspPlot(title =  gettext("p Chart"), width = 1200, height = 500)
      jaspResults[["PchartPlot"]]$dependOn(dependedDefectives)
      jaspResults[["PchartPlot"]]$position <- 1
      Pchart <- .Pchart(dataset = dataset, options = options, timeStamp = timeStamp)
      jaspResults[["PchartPlot"]]$plotObject <- Pchart$p

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer("")
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = Pchart$sixsigma, name = "P", xLabels = xLabs)
        jaspResults[["NelsonTable"]]$dependOn(dependedDefectives)
        jaspResults[["NelsonTable"]]$position <- 2
      }
    }
    #NP chart
    if (options[["attributesChartDefectivesChartType"]] == "npChart" && is.null(jaspResults[["NPchartPlot"]])) {
      jaspResults[["NPchartPlot"]] <- createJaspPlot(title =  gettext("np Chart"), width = 1200, height = 500)
      jaspResults[["NPchartPlot"]]$dependOn(dependedDefectives)
      jaspResults[["NPchartPlot"]]$position <- 1
      NPchart <- .NPchart(dataset = dataset, options = options, timeStamp = timeStamp)
      jaspResults[["NPchartPlot"]]$plotObject <- NPchart$p

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer("")
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = NPchart$sixsigma, name = "np", xLabels = xLabs)
        jaspResults[["NelsonTable"]]$position <- 2
        jaspResults[["NelsonTable"]]$dependOn(dependedDefectives)
      }
    }

    #Laney P chart
    if (options[["attributesChartDefectivesChartType"]] == "laneyPPrimeChart" && is.null(jaspResults[["LaneyPPlot"]])) {
      jaspResults[["LaneyPPlot"]] <- createJaspPlot(title =  gettext("Laney p' Chart"), width = 1200, height = 500, position = 1)
      Lanychart <- .LanyP(dataset = dataset, options = options, timeStamp = timeStamp)
      jaspResults[["LaneyPPlot"]]$plotObject <- Lanychart$p
      jaspResults[["LaneyPPlot"]]$dependOn(dependedDefectives)

      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspPlot(title =  "", position = 2)
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = Lanychart$sixsigma, name = "Laney p'", xLabels = xLabs)
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
      Cchart <- .Cchart(dataset = dataset, options = options, timeStamp = timeStamp)
      jaspResults[["CchartPlot"]]$plotObject <- PlotReport <- Cchart$p

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer()
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = Cchart$sixsigma, name = "c", xLabels = xLabs)
        jaspResults[["NelsonTable"]]$dependOn(dependedDefects)
        jaspResults[["NelsonTable"]]$position <- 2
      }
    }
    #Uchart
    if (options[["attributesChartDefectsChartType"]] == "uChart" && is.null(jaspResults[["UchartPlot"]])) {
      jaspResults[["UchartPlot"]] <- createJaspPlot(title =  gettext("u Chart"), width = 1200, height = 500)
      jaspResults[["UchartPlot"]]$dependOn(dependedDefects)
      jaspResults[["UchartPlot"]]$position <- 1
      Uchart <- .Uchart(dataset = dataset, options = options, timeStamp = timeStamp)
      jaspResults[["UchartPlot"]]$plotObject <- Uchart$p
        PlotReport <- Uchart$p

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer("")
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = Uchart$sixsigma, name = "u", xLabels = xLabs)
        jaspResults[["NelsonTable"]]$dependOn(dependedDefects)
        jaspResults[["NelsonTable"]]$position <- 2
      }
    }
    #Laney U chart
    if (options[["attributesChartDefectsChartType"]] == "laneyUPrimeChart" && is.null(jaspResults[["LaneyUPlot"]])) {
      jaspResults[["LaneyUPlot"]] <- createJaspPlot(title = gettext("Laney u' Chart"), width = 1200, height = 500, position = 1)
      jaspResults[["LaneyUPlot"]]$dependOn(dependedDefects)
      LanyUchart <- .LanyU(dataset = dataset, options = options, timeStamp = timeStamp)
      jaspResults[["LaneyUPlot"]]$plotObject <- PlotReport <- LanyUchart$p

      jaspResults[["NelsonTable"]] <- createJaspContainer(title = "", position = 2)
      jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = LanyUchart$sixsigma, name = "Laney u'", xLabels = xLabs)
      jaspResults[["NelsonTable"]]$dependOn(dependedDefects)
    }
  }
  #ImRchart for attributes
  if (options[["attributesChart"]] == "xmr" && !identical(D, "")) {
    jaspResults[["IPlotA"]] <- createJaspPlot(title = gettext("Individuals and Moving Range Chart"), width = 1200, height = 500, position = 1)
    IMRchart <- .Ichart_attributes(dataset = dataset, options = options, timeStamp = timeStamp)
    jaspResults[["IPlotA"]]$plotObject <- PlotReport <- IMRchart$p
    jaspResults[["IPlotA"]]$dependOn(c("defectiveOrDefect", "total", "attributesChart", "timeStamp"))

    # Nelson tests tables
    if (is.null(jaspResults[["NelsonTableIMR"]])) {
      NelsonTables <- createJaspContainer(title =  "", position = 2)
      NelsonTables$dependOn(c("defectiveOrDefect", "total", "attributesChart", "timeStamp"))


      NelsonTables[["NelsonTableI"]] <- .NelsonTable(dataset = dataset, options = options, type = "xbar.one", sixsigma = IMRchart$sixsigma_I, name = "Individuals", xLabels = xLabs)
      NelsonTables[["NelsonTableMR"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = IMRchart$sixsigma_R, name = "Moving Range", xLabels = xLabs)

      jaspResults[["NelsonTableIMR"]] <- NelsonTables
    }
  }

  #Report
  if (options[["report"]] && is.null(jaspResults[["AReport"]]) && ready) {

    jaspResults[["PchartPlot"]]     <- NULL
    jaspResults[["NelsonTable"]]    <- NULL
    jaspResults[["NPchartPlot"]]    <- NULL
    jaspResults[["LaneyPPlot"]]     <- NULL
    jaspResults[["CchartPlot"]]     <- NULL
    jaspResults[["UchartPlot"]]     <- NULL
    jaspResults[["LaneyUPlot"]]     <- NULL
    jaspResults[["IPlotA"]]         <- NULL
    jaspResults[["NelsonTableIMR"]] <- NULL

    jaspResults[["AReport"]] <- createJaspContainer(title = gettextf("Report for Attribute Control Charts"))
    jaspResults[["AReport"]]$dependOn(c("report", "reportTitle", "reportMeasurementName", "reportReportedBy", "reportId", "reportMiscellaneous", "reportAppraiser", "reportMeasusrementSystemName", "reportSubgroupSize", "reportTime", "reportFrequency",
                                        "defectiveOrDefect", "total", "attributesChart", "attributesChartDefectsChartType", "attributesChartDefectivesChartType", "timeStamp"))
    Report <- jaspResults[["AReport"]]
    Report[["Report"]] <- .AReport(ccTitle = options[["reportTitle"]], ccName = options[["reportMeasurementName"]],
                                 ccOperator = options[["reportReportedBy"]], ccID = options[["reportId"]], ccMisc = options[["reportMiscellaneous"]], ccAppraiser = options[["reportAppraiser"]],
                                 ccMeasurement = options[["reportMeasusrementSystemName"]], ccSize = options[["reportSubgroupSize"]], ccTime = options[["reportTime"]], ccFrequency = options[["reportFrequency"]])

    Report[["Plot"]] <- createJaspPlot(width = 1000, height = 800, position = 2)
    if (options[["attributesChart"]] == "defectives" & options[["attributesChartDefectivesChartType"]] == "pChart")
      PlotReport <- .Pchart(dataset = dataset, options = options, timeStamp = timeStamp)$p
    else if (options[["attributesChart"]] == "defectives" & options[["attributesChartDefectivesChartType"]] == "npChart")
      PlotReport <- .NPchart(dataset = dataset, options = options, timeStamp = timeStamp)$p
    else if (options[["attributesChart"]] == "defectives" & options[["attributesChartDefectivesChartType"]] == "laneyPPrimeChart")
      PlotReport <- .LanyP(dataset = dataset, options = options, timeStamp = timeStamp)$p
    else if (options[["attributesChart"]] == "defects" & options[["attributesChartDefectsChartType"]] == "cChart")
      PlotReport <- .Cchart(dataset = dataset, options = options, timeStamp = timeStamp)$p
    else if (options[["attributesChart"]] == "defects" & options[["attributesChartDefectsChartType"]] == "uChart")
      PlotReport <- .Uchart(dataset = dataset, options = options, timeStamp = timeStamp)$p
    else if (options[["attributesChart"]] == "defects" & options[["attributesChartDefectsChartType"]] == "laneyUPrimeChart")
      PlotReport <- .LanyU(dataset = dataset, options = options, timeStamp = timeStamp)$p
    else if (options[["attributesChart"]] == "xmr")
      PlotReport <- .Ichart_attributes(dataset = dataset, options = options, timeStamp = timeStamp)$p

    Report[["Plot"]]$plotObject <- PlotReport
  }
}

.Pchart <- function(dataset, options, timeStamp) {
  data1 <- data.frame(D = dataset[, options[["defectiveOrDefect"]]], sample = dataset[, options$total])
  sixsigma <- with(data1, qcc::qcc(D, sample, type = "p", plot = FALSE))
  sample <- data1$sample
  D <- data1$D
  subgroups = c(1:length(sixsigma$statistics))
  data_plot <- data.frame(subgroups = subgroups, P = sixsigma$statistics)
  center <- sixsigma$center
  UCL <- sixsigma$limits[,2]
  LCL <- sixsigma$limits[,1]
  if (min(sample)/max(sample) >= 0.75){
    p.hat <- mean(sixsigma$statistics)
    n.mean <- mean(sample)
    UCL <- p.hat + 3 * (sqrt(p.hat*(1 - p.hat))) / (sqrt(n.mean))
    LCL <- p.hat - 3 * (sqrt(p.hat*(1 - p.hat))) / (sqrt(n.mean))
  }
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
    ggplot2::geom_hline(yintercept = center, color = "green") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (length(unique(UCL)) == 1){
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
      ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
      jaspGraphs::geom_line(data = data_plot, ggplot2::aes(x = subgroups, y = P),color = "blue") +
      jaspGraphs::geom_point(data = data_plot, ggplot2::aes(x = subgroups, y = P),size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue'))
  } else{
    n <- length(UCL)
    p <- p + ggplot2::geom_step(ggplot2::aes(x = subgroups, y = UCL, color = "red"), size = 1.5, linetype = "F1") +
      jaspGraphs::geom_line(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),color = "blue") +
      jaspGraphs::geom_point(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue')) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = xBreaks - 0.5) +
      ggplot2::geom_step(ggplot2::aes(x = subgroups, y = LCL, color = "red"), size = 1.5, linetype = "F1") +
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = UCL[n], color = "red"), size = 1.5)+
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = LCL[n], color = "red"), size = 1.5)
  }

  if (!identical(timeStamp, ""))
    p <- p + ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = as.vector(dataset[, timeStamp]))

  return(list(p = p, sixsigma = sixsigma))
}
.NPchart <- function(dataset, options, timeStamp) {
  .Check_equal_samples(dataset, options)

  data1 <- data.frame(D = dataset[, options[["defectiveOrDefect"]]], sample = dataset[, options$total])
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, D = data1$D)

  sixsigma <- with(data1, qcc::qcc(D, sample, type = "np", plot = FALSE))
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data1$D ,UCL))
  yLimits <- range(yBreaks * 1.2)
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
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Number of defectives"),limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma, chart = "c")$red_points, 'red', 'blue')) +
    jaspGraphs::themeJaspRaw()

  if (!identical(timeStamp, ""))
    p <- p + ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = as.vector(dataset[, timeStamp]))

  return(list(p = p, sixsigma = sixsigma))
}
.Cchart <- function(dataset, options, timeStamp) {

  .Check_equal_samples(dataset, options)

  data1 <- data.frame(D = dataset[, options[["defectiveOrDefect"]]], sample = dataset[, options$total])
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, D = data1$D)
  sixsigma <- with(data1, qcc::qcc(D, sample, type = "c", plot = FALSE))
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data1$D ,UCL))
  yLimits <- range(yBreaks * 1.2)
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
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("D") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma, chart = "c")$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (!identical(timeStamp, ""))
    p <- p + ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = as.vector(dataset[, timeStamp]))

  return(list(p = p, sixsigma = sixsigma))
}

.Uchart <- function(dataset, options, timeStamp) {
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
    ggplot2::geom_hline(yintercept = center, color = "green") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
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
      ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
      jaspGraphs::geom_line(data = data_plot, ggplot2::aes(x = subgroups, y = P),color = "blue") +
      jaspGraphs::geom_point(data = data_plot, ggplot2::aes(x = subgroups, y = P),size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue'))
  } else{

    n <- length(UCL)
    p <- p + ggplot2::geom_step(ggplot2::aes(x = subgroups, y = UCL, color = "red"), size = 1.5, linetype = "F1") +
      jaspGraphs::geom_line(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),color = "blue") +
      jaspGraphs::geom_point(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue')) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = xBreaks - 0.5) +
      ggplot2::geom_step(ggplot2::aes(x = subgroups, y = LCL, color = "red"), size = 1.5, linetype = "F1") +
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = UCL[n], color = "red"), size = 1.5)+
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = LCL[n], color = "red"), size = 1.5)

  }

  if (!identical(timeStamp, ""))
    p <- p + ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = as.vector(dataset[, timeStamp]))

  return(list(p = p, sixsigma = sixsigma))
}

##Imr for attributes
.Ichart_attributes <- function(dataset, options, timeStamp) {
  data1 <- data.frame(D = dataset[, options[["defectiveOrDefect"]]], sample = dataset[, options$total])
  data1$P <- data1$D/data1$sample
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, P = data1$P)

  sixsigma_I <- qcc::qcc(data_plot$P, type ='xbar.one', plot=FALSE)
  center <- sixsigma_I$center
  UCL <- max(sixsigma_I$limits)
  LCL <- min(sixsigma_I$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL))
  yLimits <- range(yBreaks * 1.2)
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
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l), inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma_I, chart = "i")$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (!identical(timeStamp, ""))
    p1 <- p1 + ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = as.vector(dataset[, timeStamp])[xBreaks])

  #data
  data <- data.frame(process = dataset[, options[["defectiveOrDefect"]]] / dataset[, options$total])
  xmr.raw.r <- matrix(cbind(data$process[1:length(data$process)-1], data$process[2:length(data$process)]), ncol=2)
  sixsigma_R <- qcc::qcc(xmr.raw.r, type="R", plot = FALSE)
  subgroups = c(1:length(sixsigma_R$statistics))
  data_plot <- data.frame(subgroups = subgroups, data = sixsigma_R$statistics)
  center <- sixsigma_R$center
  UCL <- max(sixsigma_R$limits)
  LCL <- min(sixsigma_R$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL))
  yLimits <- range(yBreaks * 1.2)
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
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Moving Range") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = Xlabels) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma_R, chart = "c")$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (!identical(timeStamp, ""))
    p2 <- p2 + ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = as.vector(dataset[, timeStamp])[xBreaks])

  p3 <-  jaspGraphs::ggMatrixPlot(plotList = list(p1, p2), layout = matrix(1:2, 2), removeXYlabels= "x")

  return(list(p = p3, sixsigma_I = sixsigma_I, sixsigma_R = sixsigma_R))
}

### Lany's charts
.LanyU <- function(dataset, options, timeStamp) {
  data1 <- data.frame(D = dataset[,options[["defectiveOrDefect"]]], sample = dataset[,options$total])
  data1$P <- data1$D/data1$sample
  subgroups <- c(1:nrow(data1))
  data_plot <- data.frame(subgroups = subgroups, P = data1$P)

  #Sixsigma
  center = sum(data1$D)/sum(data1$sample)
  z_scores <- (data1$P - center)/sqrt(center/data1$sample[1])
  R <- vector()
  for (i in c(1:length(z_scores) - 1))
    R[i] <- abs(z_scores[i+1] - z_scores[i])

  MR_mean <- sum(R)/ (length(data1$P) - 1)
  sigma <- MR_mean/1.128
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
    ggplot2::geom_hline(yintercept = center, color = "green") +
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
    xBreaks <- xBreaks - 0.5

    p <- p +
      ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
      jaspGraphs::geom_line(data = data_plot, ggplot2::aes(x = subgroups, y = P),color = "blue") +
      jaspGraphs::geom_point(data = data_plot, ggplot2::aes(x = subgroups, y = P),size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue'))
  } else{
    n <- length(UCL)
    p <- p + ggplot2::geom_step(ggplot2::aes(x = subgroups, y = UCL, color = "red"), size = 1.5, linetype = "F1") +
      jaspGraphs::geom_line(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),color = "blue") +
      jaspGraphs::geom_point(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue')) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = xBreaks - 0.5) +
      ggplot2::geom_step(ggplot2::aes(x = subgroups, y = LCL, color = "red"), size = 1.5, linetype = "F1") +
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = UCL[n], color = "red"), size = 1.5)+
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = LCL[n], color = "red"), size = 1.5)

  }

  if (!identical(timeStamp, ""))
    p <- p + ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = as.vector(dataset[, timeStamp]))

  return(list(p = p, sixsigma = sixsigma))
}

.LanyP <- function(dataset, options, timeStamp) {
  data1 <- data.frame(D = dataset[,options[["defectiveOrDefect"]]], sample = dataset[,options$total])
  data1$P <- data1$D/data1$sample
  subgroups <- c(1:nrow(data1))
  data_plot <- data.frame(subgroups = subgroups, P = data1$P)

  #Sixsigma
  center = sum(data1$D)/sum(data1$sample)
  z_scores <- (data1$P - center)/sqrt(center*(1 - center)/data1$sample[1])
  R <- vector()
  for (i in c(1:length(z_scores) - 1))
    R[i] <- abs(z_scores[i+1] - z_scores[i])

  MR_mean <- sum(R)/ (length(data1$P) - 1)
  sigma <- MR_mean/1.128
  LCL <- ifelse(center - 3*sqrt(center*(1 - center)/data1$sample) * sigma < 0, 0, center - 3*sqrt(center*(1 - center)/data1$sample) * sigma)
  UCL <- center + 3*sqrt(center*(1 - center)/data1$sample) * sigma
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
    ggplot2::geom_hline(yintercept = center, color = "green") +
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
      ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
      jaspGraphs::geom_line(data = data_plot, ggplot2::aes(x = subgroups, y = P),color = "blue") +
      jaspGraphs::geom_point(data = data_plot, ggplot2::aes(x = subgroups, y = P),size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue'))
  } else{
    n <- length(UCL)
    p <- p + ggplot2::geom_step(ggplot2::aes(x = subgroups, y = UCL, color = "red"), size = 1.5, linetype = "F1") +
      jaspGraphs::geom_line(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),color = "blue") +
      jaspGraphs::geom_point(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue')) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = xBreaks - 0.5) +
      ggplot2::geom_step(ggplot2::aes(x = subgroups, y = LCL, color = "red"), size = 1.5, linetype = "F1") +
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = UCL[n], color = "red"), size = 1.5)+
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = LCL[n], color = "red"), size = 1.5)

  }

  if (!identical(timeStamp, ""))
    p <- p + ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = dataset[, options$timeStamp])

  return(list(p = p, sixsigma = sixsigma))
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

.AReport <- function(ccTitle = "", ccName = "", ccOperator = "", ccID = "", ccMisc = "" , ccAppraiser = "", ccMeasurement = "", ccSize = "",
                     ccTime = "", ccFrequency = ""){

  Report <- createJaspContainer(gettext("Report"))

  if (identical(ccTitle, "")) {
    title <- gettext("Control Charts for Attributes Report")
  }else{
    title <- ccTitle
  }
  name <- gettextf("Service name: %s", ccName)
  operator <- gettextf("Operator: %s", ccOperator)
  ID <- gettextf("Identification: %s", ccID)
  Appraiser <- gettextf("Appraiser: %s", ccAppraiser)
  Measurement <- gettextf("Measurement system: %s", ccMeasurement)
  Size <- gettextf("Name of Size: %s", ccSize)
  Time <- gettextf("Time: %s", ccTime)
  Frequency <- gettextf("Frequency: %s", ccFrequency)


  text1 <- c(name, operator)
  text2 <- c(ID, Appraiser)
  text3 <- c(Measurement, Time)
  text4 <- c(Size, Frequency)

  matrixPlot <- createJaspPlot(width = 1200, aspectRatio = 1, position = 1)
  plotMat <- matrix(list(), 2, 2, T)
  plotMat[[1, 1]] <- .ggplotWithText(text1)
  plotMat[[1, 2]] <- .ggplotWithText(text2)
  plotMat[[2, 1]] <- .ggplotWithText(text3)
  plotMat[[2, 2]] <- .ggplotWithText(text4)

  p <- jaspGraphs::ggMatrixPlot(plotMat, topLabels = c(title, ""))
  matrixPlot$plotObject <- p

  return(matrixPlot)
}

.NelsonTable <- function(dataset, options, sixsigma, type = "xbar", Phase2 = TRUE, name = "X-bar", xLabels = NULL) {

  table <- createJaspTable(title = gettextf("Test results for %s chart", name))

  if (length(sixsigma$statistics) == 1) # no need for table with only 1 group
    return(table)

  if (!Phase2 || type == "xbar.one") {

    Test <- NelsonLaws(data = sixsigma, allsix = TRUE, xLabels = xLabels)

    if (length(Test$Rules$R1) > 0)
      table$addColumnInfo(name = "test1",              title = gettextf("Test 1: Beyond limit")               , type = "integer")

    if (length(Test$Rules$R2) > 0)
      table$addColumnInfo(name = "test2",              title = gettextf("Test 2: Shift")                   , type = "integer")

    if (length(Test$Rules$R3) > 0)
      table$addColumnInfo(name = "test3",              title = gettextf("Test 3: Trend")                        , type = "integer")

    if (length(Test$Rules$R4) > 0)
      table$addColumnInfo(name = "test4",              title = gettextf("Test 4: Increasing variation")         , type = "integer")

    if (length(Test$Rules$R5) > 0)
      table$addColumnInfo(name = "test5",              title = gettextf("Test 5: Reducing variation")           , type = "integer")

    if (length(Test$Rules$R6) > 0)
      table$addColumnInfo(name = "test6",              title = gettextf("Test 6: Bimodal distribution")         , type = "integer")



    table$setData(list(
      "test1" = c(Test$Rules$R1),
      "test2" = c(Test$Rules$R2),
      "test3" = c(Test$Rules$R3),
      "test4" = c(Test$Rules$R4),
      "test5" = c(Test$Rules$R5),
      "test6" = c(Test$Rules$R6)
    ))

  }
  else {

    if (name == "np" || name == "c" || name == "u" || name == "Laney p'" || name == "Laney u'")
      Test <- NelsonLaws(data = sixsigma, xLabels = xLabels, chart = "c")
    else if (name == "P")
      Test <- NelsonLaws(data = sixsigma, xLabels = xLabels, chart = "p")
    else
      Test <- NelsonLaws(data = sixsigma, xLabels = xLabels)

    if (length(Test$Rules$R1) > 0)
      table$addColumnInfo(name = "test1",              title = gettextf("Test 1: Beyond limit")               , type = "integer")

    if (length(Test$Rules$R2) > 0)
      table$addColumnInfo(name = "test2",              title = gettextf("Test 2: Shift")                   , type = "integer")

    if (length(Test$Rules$R3) > 0)
      table$addColumnInfo(name = "test3",              title = gettextf("Test 3: Trend")                        , type = "integer")

    if (type == "Range" & length(xLabels) == 0){
      table$setData(list(
        "test1" = c(Test$Rules$R1 + 1),
        "test2" = c(Test$Rules$R2 + 1),
        "test3" = c(Test$Rules$R3 + 1)
      ))
    } else{
      table$setData(list(
        "test1" = c(Test$Rules$R1),
        "test2" = c(Test$Rules$R2),
        "test3" = c(Test$Rules$R3)
      ))
    }
  }

  table$showSpecifiedColumnsOnly <- TRUE
  table$addFootnote(message = gettext("Numbers are data points where test violations occur."))
  return(table)
}
