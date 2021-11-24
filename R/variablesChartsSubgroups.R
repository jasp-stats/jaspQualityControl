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

  if ((options$CCReport | options$TypeChart == "Xbarchart" | options$TypeChart == "Schart") && !ready) {
    plot <- createJaspPlot(title = gettext("Control Charts"), width = 700, height = 400)
    jaspResults[["plot"]] <- plot
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

  if (ready && nrow(dataset[measurements]) == 0){
    jaspResults[["plot"]] <- createJaspPlot(title = gettext("Control Charts"), width = 700, height = 400)
    jaspResults[["plot"]]$setError(gettextf("No valid measurements in %s.", measurements))
    jaspResults[["plot"]]$position <- 1
    jaspResults[["plot"]]$dependOn(c("variables", "variablesLong"))
    return()
  }

  #X bar & R chart
  if (options$TypeChart == "Xbarchart" && is.null(jaspResults[["XbarPlot"]]) && ready) {
    jaspResults[["XbarPlot"]] <- createJaspPlot(title =  gettext("X-bar & R Control Chart"), width = 1200, height = 500)
    jaspResults[["XbarPlot"]]$dependOn(c("TypeChart", "variables", "Wlimits", "Phase2", "mean", "SD", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong", "CCReport", "ccTitle", "ccName", "ccMisc","ccReportedBy","ccDate", "ccSubTitle", "ccChartName"))

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
    jaspResults[["SPlot"]] <- createJaspPlot(title = gettext("X-bar & s Control Chart"), width = 1200, height = 500)
    jaspResults[["SPlot"]]$dependOn(c("TypeChart", "variables", "Wlimits", "Phase2", "mean", "SD", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong", "CCReport", "ccTitle", "ccName", "ccMisc","ccReportedBy","ccDate", "ccSubTitle", "ccChartName"))

    Schart <- .XbarSchart(dataset = dataset[measurements], options = options, manualXaxis = subgroups, Phase2 = options$Phase2, sd = options$SD)
    Xchart <- .XbarchartNoId(dataset = dataset[measurements], options = options, warningLimits = options[["Wlimits"]], manualXaxis = subgroups, Phase2 = options$Phase2, target = options$mean, sd = options$SD)
    jaspResults[["SPlot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(Schart$p, Xchart$p), layout = matrix(2:1, 2), removeXYlabels= "x")
    jaspResults[["SPlot"]]$position <- 1

    # Nelson tests tables
    if (is.null(jaspResults[["NelsonTableS"]]) & is.null(jaspResults[["NelsonTableX"]]) & is.null(jaspResults[["NelsonTables"]])) {
      jaspResults[["NelsonTables"]] <- createJaspContainer(title = gettext("Out of-control Signals"))
      jaspResults[["NelsonTables"]]$dependOn(c("TypeChart", "variables", "Phase2", "mean", "SD", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong", "Wlimits"))
      jaspResults[["NelsonTables"]]$position <- 2
      AllTables <- jaspResults[["NelsonTables"]]

      AllTables[["NelsonTableX"]] <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Xchart$sixsigma, Phase2 = options$Phase2, xLabels = Xchart$xLabels)
      AllTables[["NelsonTableS"]] <- .NelsonTable(dataset = dataset[measurements], options = options, name = "s", sixsigma = Schart$sixsigma, xLabels = Schart$xLabels)
    }
  }
  # Report
  if (options[["CCReport"]] && is.null(jaspResults[["CCReport"]]) && ready) {
    jaspResults[["CCReport"]] <- createJaspContainer(gettext("Report"))
    jaspResults[["CCReport"]]$dependOn(c("CCReport", "TypeChart", "variables", "variablesLong", "CCDataFormat", "subgroups", "ccTitle", "ccName", "ccMisc","ccReportedBy","ccDate", "ccSubTitle", "ccChartName"))
    jaspResults[["CCReport"]]$position <- 9
    Iplot <- jaspResults[["CCReport"]]

    if (options$TypeChart == "Schart")
      Iplot[["ccReport"]] <- .CCReport(p1 = Xchart$p, p2 = Schart$p, ccTitle = options$ccTitle,
                                       ccName = options$ccName, ccDate = options$ccDate, ccReportedBy = options$ccReportedBy, ccSubTitle = options$ccSubTitle,
                                       ccChartName = options$ccChartName)
    else
      Iplot[["ccReport"]] <- .CCReport(p1 = Xchart$p, p2 = Rchart$p , ccTitle = options$ccTitle,
                                       ccName = options$ccName, ccDate = options$ccDate, ccReportedBy = options$ccReportedBy, ccSubTitle = options$ccSubTitle,
                                       ccChartName = options$ccChartName)

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
.XbarSchart <- function(dataset, options, manualXaxis = "", Phase2 = options$Phase2, sd = "") {
  data1 <- dataset[, unlist(lapply(dataset, is.numeric))]
  sixsigma <- qcc::qcc(data1, type ='S', plot = FALSE)
  subgroups <- c(1:length(sixsigma$statistics))
  data_plot <- data.frame(subgroups = subgroups, Stdv = sixsigma$statistics)

  if(Phase2 && sd != "")
    sixsigma <- list(statistics = sixsigma$statistics,
                     limits = KnownControlStats.RS(sixsigma$sizes[1], as.numeric(sd))$limits,
                     center = KnownControlStats.RS(sixsigma$sizes[1], as.numeric(sd))$center)

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
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
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
    ggplot2::geom_label(data = dfLabel, ggplot2::aes(x = x, y = y, label = l), direction = "both", size = 4) +
    ggplot2::scale_y_continuous(name =  gettext("Subgroup st dev"), breaks = yBreaks, limits = range(yBreaks)) +
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
.CCReport <- function(ImR = FALSE,p1 = "", p2 = "", ccTitle = "", ccName = "", ccDate = "", ccReportedBy = "", ccMisc = "" , ccSubTitle = "", ccChartName = ""){

  if (ccTitle == ""){
    title <- "Report for Control Charts"
  }else{
    title <- ccTitle
  }
  name <- gettextf("Name: %s", ccName)
  date <- gettextf("Date: %s", ccDate)
  text1 <- c(name, date)

  reportedBy <- gettextf("Reported by: %s", ccReportedBy)
  misc <- gettextf("Misc: %s", ccMisc)
  text2 <- c(reportedBy, misc)


  matrixPlot <- createJaspPlot(width = 1200, height = 1000)
  plotMat <- matrix(list(), 3, 2)
  plotMat[[1, 1]] <- .ggplotWithText(text1)
  plotMat[[1, 2]] <- .ggplotWithText(text2)
  plotMat[[2, 1]] <- .ggplotWithText(gettextf("Sub-title: %s", ccSubTitle))
  plotMat[[2, 2]] <- .ggplotWithText(gettextf("Name of chart: %s", ccChartName))
  plotMat[[3, 1]] <- p1
  plotMat[[3, 2]] <- p2


  p <- jaspGraphs::ggMatrixPlot(plotMat, topLabels = c(gettext(title), ""))
  matrixPlot$plotObject <- p

  return(matrixPlot)
}
KnownControlStats.RS <- function(N, sigma) {

  Data.d3 <- data.frame(
    n = 2:25,
    d3 = c(0.8525 ,0.8884, 0.8798, 0.8641, 0.8480, 0.8332, 0.8198, 0.8078, 0.7971, 0.7873, 0.7785, 0.7704, 0.7630,
           0.7562, 0.7499, 0.7441, 0.7386, 0.7335, 0.7287, 0.7242, 0.7199, 0.7159, 0.7121, 0.7084))

  Data.d2 <- data.frame(
    n = 2:50,
    d2 = c( 1.128, 1.693 ,2.059, 2.326, 2.534, 2.704, 2.847, 2.970, 3.078, 3.173, 3.258, 3.336, 3.407, 3.472, 3.532,
            3.588 ,3.640 ,3.689, 3.735, 3.778, 3.819, 3.858, 3.895, 3.931, 3.964, 3.997, 4.027, 4.057, 4.086, 4.113,
            4.139 ,4.165 ,4.189, 4.213, 4.236, 4.259, 4.280, 4.301, 4.322, 4.341, 4.361, 4.379, 4.398, 4.415, 4.433,
            4.450 ,4.466, 4.482, 4.498))

  if (N > 25 && N <= 50){
    d3 <- 0.80818 - 0.0051871 * N + 0.00005098 * N^2 - 0.00000019 * N^3
    d2 <- Data.d2[N == Data.d2$n,2]
  }
  else if (N > 50){
    d3 <- 0.80818 - 0.0051871 * N + 0.00005098 * N^2 - 0.00000019 * N^3
    d2 <- 2.88606 + 0.051313 * N - 0.00049243 * N^2 + 0.00000188 * N^3
  } else{
    d2 <- Data.d2[N == Data.d2$n,2]
    d3 <- Data.d3[N == Data.d3$n,2]
  }

  UCL <- d2 * sigma + 3 * d3 * sigma
  CL <- d2 * sigma
  LCL <- max(0, d2 * sigma - 3 * d3 * sigma)

  return(list(constants = c(d2,d3), limits = data.frame(LCL,UCL), center = CL))
}

