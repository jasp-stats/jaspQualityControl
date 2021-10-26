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
variablesChartsSubgroups <- function(jaspResults, dataset, options) {

  variables <- unlist(options$variables)
  splitName <- options$subgroups
  makeSplit <- splitName != ""

  if (options[["CCDataFormat"]] == "CCwideFormat"){
    measurements <- unlist(options$variables)
  }else{
    measurements <- unlist(options$variablesLong)
  }
  measurements <- measurements[measurements != ""]
  subgroups <- unlist(options$subgroups)

  # Data reading
  if (is.null(dataset)) {
    if (options[["subgroups"]] != "") {
      dataset <- .readDataSetToEnd(columns.as.numeric = measurements, columns.as.factor = splitName)
      dataset.factors <- .readDataSetToEnd(columns=variables, columns.as.factor=splitName)
    } else {
      dataset <- .readDataSetToEnd(columns.as.numeric = measurements)
    }
  }


  # Check if analysis is ready
  if (options[["CCDataFormat"]] == "CCwideFormat"){
    ready <- length(measurements) > 1
  }else{
    ready <- length(measurements) > 0
  }

  if ((options$CCReport | options$Schart | options$Xbarchart) && !ready) {
    plot <- createJaspPlot(title = gettext("Control Charts"), width = 700, height = 400)
    jaspResults[["plot"]] <- plot
    plot$setError(gettext("Please insert more measurements."))
    plot$dependOn(c("CCReport", "TypeChart", "variablesLong", "variables"))
    return()
  }

  if (makeSplit && ready) {
    splitFactor      <- dataset[[.v(splitName)]]
    splitLevels      <- levels(splitFactor)
    # remove missing values from the grouping variable
    dataset <- dataset[!is.na(splitFactor), ]
    dataset.factors <- dataset.factors[!is.na(splitFactor), ]

    numberMissingSplitBy <- sum(is.na(splitFactor))

    # Actually remove missing values from the split factor
    splitFactor <- na.omit(splitFactor)

    if(subgroups != "")
      subgroups <- splitLevels
  }

  if (options[["CCDataFormat"]] == "CClongFormat" && ready){
    k <- options[["CCSubgroupSize"]]
    n <- nrow(dataset)
    dataset <- .PClongTowide(dataset, k, measurements)
    if (dataset == "error"){
      plot <- createJaspPlot(title = gettext("Control Charts"), width = 700, height = 400)
      jaspResults[["plot"]] <- plot
      plot$setError(gettextf("Could not equally divide data points into groups of size %i.", k))
      plot$dependOn("CCSubgroupSize")
      return()
    }
    measurements <- colnames(dataset)
  }

  dataset <- na.omit(dataset)

  #Checking for errors in the dataset

  .hasErrors(dataset, type = c('observations', 'infinity', 'missingValues'),
             all.target = options$variables,
             observations.amount =  c('< 0'), exitAnalysisIfErrors = TRUE)

  #X bar & R chart
  if (options$TypeChart == "Xbarchart" && is.null(jaspResults[["XbarPlot"]]) && ready) {
    jaspResults[["XbarPlot"]] <- createJaspPlot(title =  gettext("X-bar & R Control Chart"), width = 1200, height = 500)
    jaspResults[["XbarPlot"]]$dependOn(c("TypeChart", "variables", "Wlimits", "Phase2", "mean", "SD", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong"))

    Xchart <- .XbarchartNoId(dataset = dataset[measurements], options = options,  manualXaxis = subgroups ,warningLimits = options[["Wlimits"]], Phase2 = options$Phase2, target = options$mean, sd = options$SD)
    Rchart <- .RchartNoId(dataset = dataset[measurements], options = options, manualXaxis = subgroups, warningLimits = FALSE, Phase2 = options$Phase2, target = options$mean, sd = options$SD)
    jaspResults[["XbarPlot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(Rchart$p, Xchart$p), layout = matrix(2:1, 2), removeXYlabels= "x")
    jaspResults[["XbarPlot"]]$position <- 1

    # Nelson tests tables
    if (is.null(jaspResults[["NelsonTableX"]]) & is.null(jaspResults[["NelsonTableR"]]) & is.null(jaspResults[["NelsonTables"]])) {
      jaspResults[["NelsonTables"]] <- createJaspContainer(title = gettext("Out of-control Signals"))
      jaspResults[["NelsonTables"]]$dependOn(c("TypeChart", "variables", "Phase2", "mean", "SD", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong"))
      jaspResults[["NelsonTables"]]$position <- 2
      AllTables <- jaspResults[["NelsonTables"]]

      AllTables[["NelsonTableX"]]  <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Xchart$sixsigma, Phase2 = options$Phase2, xLabels = Xchart$xLabels)
      AllTables[["NelsonTableR"]] <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Rchart$sixsigma, name = "R", xLabels = Rchart$xLabels)
    }
  }

  #S Chart
  if (options$TypeChart == "Schart" && is.null(jaspResults[["SPlot"]]) && ready) {
    jaspResults[["SPlot"]] <- createJaspPlot(title = gettext("Xbar & s Control Chart"), width = 1200, height = 500)
    jaspResults[["SPlot"]]$dependOn(c("TypeChart", "variables", "Wlimits", "Phase2", "mean", "SD", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong"))

    Schart <- .XbarSchart(dataset = dataset[measurements], options = options, manualXaxis = subgroups)
    Xchart <- .XbarchartNoId(dataset = dataset[measurements], options = options, warningLimits = options[["Wlimits"]], manualXaxis = subgroups, Phase2 = options$Phase2, target = options$mean, sd = options$SD)
    jaspResults[["SPlot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(Schart$p, Xchart$p), layout = matrix(2:1, 2), removeXYlabels= "x")
    jaspResults[["SPlot"]]$position <- 1

    # Nelson tests tables
    if (is.null(jaspResults[["NelsonTableS"]]) & is.null(jaspResults[["NelsonTableX"]]) & is.null(jaspResults[["NelsonTables"]])) {
      jaspResults[["NelsonTables"]] <- createJaspContainer(title = gettext("Out of-control Signals"))
      jaspResults[["NelsonTables"]]$dependOn(c("TypeChart", "variables", "Phase2", "mean", "SD", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong", "Wlimits"))
      jaspResults[["NelsonTables"]]$position <- 2
      AllTables <- jaspResults[["NelsonTables"]]

      AllTables[["NelsonTableS"]] <- .NelsonTable(dataset = dataset[measurements], options = options, name = "s", sixsigma = Schart$sixsigma, xLabels = Schart$xLabels)
      AllTables[["NelsonTableX"]] <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Xchart$sixsigma, Phase2 = options$Phase2, xLabels = Xchart$xLabels)
    }
  }
  # Report
  if (options[["CCReport"]] && is.null(jaspResults[["CCReport"]]) && ready) {
    jaspResults[["CCReport"]] <- createJaspContainer(gettext("Report"))
    jaspResults[["CCReport"]]$dependOn(c("CCReport", "TypeChart", "variables", "variablesLong", "CCDataFormat", "subgroups", "ccTitle", "ccName", "ccMisc","ccReportedBy","ccDate"))
    jaspResults[["CCReport"]]$position <- 9
    Iplot <- jaspResults[["CCReport"]]

    Iplot[["ccReport"]] <- .CCReport(dataset = dataset[measurements], options = options, manualXaxis = subgroups, warningLimits_X = options[["Wlimits"]], Phase2_X = options$Phase2, target_X = options$mean, sd_X = options$SD,
                                        Type = options$TypeChart)

    #Xchart_XR <- .XbarchartNoId(dataset = dataset[measurements], options = options,  manualXaxis = subgroups ,warningLimits = options[["Wlimits"]], Phase2 = options$Phase2, target = options$mean, sd = options$SD)
    #Rchart    <- .RchartNoId(dataset = dataset[measurements], options = options, manualXaxis = subgroups, warningLimits = FALSE, Phase2 = options$Phase2, target = options$mean, sd = options$SD)
    #Schart    <- .XbarSchart(dataset = dataset[measurements], options = options, manualXaxis = subgroups)
    #Xchart_S  <- .XbarchartNoId(dataset = dataset[measurements], options = options, warningLimits = options[["Wlimits"]], manualXaxis = subgroups, Phase2 = options$Phase2, target = options$mean, sd = options$SD)

    #Iplot[["ccTableXR"]] <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Xchart_XR$sixsigma, Phase2 = options$Phase2, xLabels = Xchart_XR$xLabels, name = "X-bar (R)")
    #Iplot[["ccTableR"]]  <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Rchart$sixsigma, name = "R", xLabels = Rchart$xLabels)
    #Iplot[["ccTableXS"]] <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Xchart_S$sixsigma, Phase2 = options$Phase2, xLabels = Xchart_S$xLabels, name = "X-bar (s)")
    #Iplot[["ccTableS"]]  <- .NelsonTable(dataset = dataset[measurements], options = options, name = "s", sixsigma = Schart$sixsigma, xLabels = Schart$xLabels)
  }
}

#Functions for control charts
.XbarSchart <- function(dataset, options, manualXaxis = "") {
  data1 <- dataset[, unlist(lapply(dataset, is.numeric))]
  Stdv <- apply(data1, 1, function(x) sd(x))
  subgroups <- c(1:length(Stdv))
  data_plot <- data.frame(subgroups = subgroups, Stdv = Stdv)
  sixsigma <- qcc::qcc(data1, type ='S', plot=FALSE)
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, data_plot$Stdv, UCL))
  yLimits <- range(yBreaks)
  if (length(subgroups) <= 10){
    nxBreaks <- length(subgroups)
  }else{
    nxBreaks <- 5
  }
  prettyxBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups, n = nxBreaks)
  prettyxBreaks[prettyxBreaks == 0] <- 1
  xBreaks <- c(prettyxBreaks[1], prettyxBreaks[-1])
  xLimits <- c(1,max(xBreaks) + 2.5)
  dfLabel <- data.frame(
    x = max(xBreaks) * 1.2,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, decimalplaces(data1[1,1]) + 1)),
      gettextf("UCL = %g",   round(UCL, decimalplaces(data1[1,1]) + 2)),
      gettextf("LCL = %g",   round(LCL, decimalplaces(data1[1,1]) + 2))
    )
  )

  xLimits <- range(c(xBreaks, dfLabel$x))

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = Stdv)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggrepel::geom_label_repel(data = dfLabel, ggplot2::aes(x = x, y = y, label = l), direction = "both", size = 4) +
    ggplot2::scale_y_continuous(name =  gettext("Standard Deviation"), breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(name = gettext('Subgroup'), breaks = xBreaks) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma)$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (manualXaxis != "") {
    xLabels <- factor(manualXaxis, levels = manualXaxis)
    p <- p + ggplot2::scale_x_continuous(breaks = 1:length(manualXaxis), labels = xLabels)
  }

  if (manualXaxis != "")
    return(list(p = p, sixsigma = sixsigma, xLabels = levels(xLabels)))
  else return(list(p = p, sixsigma = sixsigma))
}
.CCReport <- function(dataset, options, manualXaxis, warningLimits_X, Phase2_X, target_X, sd_X, Type = "Xbarchart"){

  if (options[["ccTitle"]] == ""){
    title <- "Measurement"
  }else{
    title <- options[["ccTitle"]]
  }
  name <- gettextf("Name: %s", options[["ccName"]])
  date <- gettextf("Date: %s", options[["ccDate"]])
  text1 <- c(name, date)

  reportedBy <- gettextf("Reported by: %s", options[["ccReportedBy"]])
  misc <- gettextf("Misc: %s", options[["ccMisc"]])
  text2 <- c(reportedBy, misc)

  #Create X-bar and R chart
  Xchart_R <- .XbarchartNoId(dataset = dataset, options = options,  manualXaxis = manualXaxis ,warningLimits = warningLimits_X, Phase2 = Phase2_X,
                           target = target_X, sd = sd_X)
  if (Type == "Xbarchart")
    RorSchart <- .RchartNoId(dataset = dataset, options = options, manualXaxis = manualXaxis, warningLimits = FALSE, Phase2 = Phase2_X,
                        target = target_X, sd = sd_X)
  else
    RorSchart <- .XbarSchart(dataset = dataset, options = options, manualXaxis = manualXaxis)
  matrixPlot <- createJaspPlot(width = 1200, height = 1000)
  plotMat <- matrix(list(), 2, 2)
  plotMat[[1, 1]] <- .ggplotWithText(text1)
  plotMat[[1, 2]] <- .ggplotWithText(text2)
  plotMat[[2, 1]] <- Xchart_R$p
  plotMat[[2, 2]] <- RorSchart$p

  p <- jaspGraphs::ggMatrixPlot(plotMat, topLabels = c(gettextf("Control Charts Report for %s", title), ""))
  matrixPlot$plotObject <- p

  return(matrixPlot)
}

