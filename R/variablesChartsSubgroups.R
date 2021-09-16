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



  if (options[["CCDataFormat"]] == "CCwideFormat"){
    if (is.null(dataset)) {
      if (options[["subgroups"]] != "") {
        dataset <- .readDataSetToEnd(columns.as.numeric = options[["variables"]], columns.as.factor = splitName)
        dataset.factors <- .readDataSetToEnd(columns=variables, columns.as.factor=splitName)
      } else {
        dataset <- .readDataSetToEnd(columns.as.numeric = options[["variables"]])
      }
    }
  }else{
    dataset <- .readDataSetToEnd(columns.as.numeric = measurements)
  }

  if (makeSplit && length(variables) > 0) {
    splitFactor      <- dataset[[.v(splitName)]]
    splitLevels      <- levels(splitFactor)
    # remove missing values from the grouping variable
    dataset <- dataset[!is.na(splitFactor), ]
    dataset.factors <- dataset.factors[!is.na(splitFactor), ]

    numberMissingSplitBy <- sum(is.na(splitFactor))

    # Actually remove missing values from the split factor
    splitFactor <- na.omit(splitFactor)
  }

  # Check if analysis is ready
  ready <- length(measurements > 0)

  if (options[["CCDataFormat"]] == "CClongFormat" && ready){
    k <- options[["CCSubgroupSize"]]
    dataset <- .PClongTowide(dataset, k, measurements)
    measurements <- colnames(dataset)
  }

  dataset <- na.omit(dataset)
  if(subgroups != "")
    subgroups <- splitLevels

  #Checking for errors in the dataset

  .hasErrors(dataset, type = c('observations', 'infinity', 'missingValues'),
             all.target = options$variables,
             observations.amount =  c('< 0'), exitAnalysisIfErrors = TRUE)

  #X bar & R chart
  if (options$Xbarchart && is.null(jaspResults[["XbarPlot"]])) {
    jaspResults[["XbarPlot"]] <- createJaspPlot(title =  gettext("X-bar & R Control Chart"), width = 1200, height = 500)
    jaspResults[["XbarPlot"]]$dependOn(c("Xbarchart", "variables", "Wlimits", "Phase2_XR", "mean_XR", "SD_XR", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong"))

    if (length(measurements) < 2) {
      jaspResults[["XbarPlot"]]$setError(gettext("You must enter more measurements to get this output."))
      return()
    }
    Xchart <- .XbarchartNoId(dataset = dataset[measurements], options = options,  manualXaxis = subgroups ,warningLimits = options[["Wlimits"]], Phase2 = options$Phase2_XR, target = options$mean_XR, sd = options$SD_XR)
    Rchart <- .RchartNoId(dataset = dataset[measurements], options = options, manualXaxis = subgroups, warningLimits = FALSE)
    jaspResults[["XbarPlot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(Rchart$p, Xchart$p), layout = matrix(2:1, 2), removeXYlabels= "x")

    # Nelson tests tables
    if (is.null(jaspResults[["NelsonTable"]]) & is.null(jaspResults[["NelsonTable2"]])) {
      jaspResults[["NelsonTable"]]  <- createJaspContainer(gettext(""))
      jaspResults[["NelsonTable2"]] <- createJaspContainer(gettext(""))
      jaspResults[["NelsonTable"]]  <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Xchart$sixsigma, Phase2 = options$Phase2_XR, xLabels = Xchart$xLabels)
      jaspResults[["NelsonTable2"]] <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Rchart$sixsigma, name = "R", xLabels = Rchart$xLabels)

      jaspResults[["NelsonTable"]]$dependOn(c("Xbarchart", "variables", "Phase2_XR", "mean_XR", "SD_XR", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong"))
      jaspResults[["NelsonTable2"]]$dependOn(c("Xbarchart", "variables", "Phase2_XR", "mean_XR", "SD_XR", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong"))
    }
  }

  #S Chart
  if (options$Schart && is.null(jaspResults[["SPlot"]])) {
    jaspResults[["SPlot"]] <- createJaspPlot(title = gettext("Xbar & s Control Chart"), width = 1200, height = 500)
    jaspResults[["SPlot"]]$dependOn(c("Schart", "variables", "Wlimits2", "Phase2_S", "mean_S", "SD_S", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong"))
    if (length(measurements) < 2) {
      jaspResults[["SPlot"]]$setError(gettext("You must enter more measurements to get this output."))
      return()
    }
    Schart <- .XbarSchart(dataset = dataset[measurements], options = options, manualXaxis = subgroups)
    Xchart <- .XbarchartNoId(dataset = dataset[measurements], options = options, warningLimits = options[["Wlimits2"]], manualXaxis = subgroups, Phase2 = options$Phase2_S, target = options$mean_S, sd = options$SD_S)
    jaspResults[["SPlot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(Schart$p, Xchart$p), layout = matrix(2:1, 2), removeXYlabels= "x")

    # Nelson tests tables
    if (is.null(jaspResults[["NelsonTableS"]]) & is.null(jaspResults[["NelsonTableX"]])) {
      jaspResults[["NelsonTableS"]] <- createJaspContainer(gettext(""))
      jaspResults[["NelsonTableX"]] <- createJaspContainer(gettext(""))
      jaspResults[["NelsonTableS"]] <- .NelsonTable(dataset = dataset[measurements], options = options, name = "s", sixsigma = Schart$sixsigma, xLabels = Schart$xLabels)
      jaspResults[["NelsonTableX"]] <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Xchart$sixsigma, Phase2 = options$Phase2_S, xLabels = Xchart$xLabels)

      jaspResults[["NelsonTableS"]]$dependOn(c("Schart", "variables", "Phase2_S", "mean_S", "SD_S", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong"))
      jaspResults[["NelsonTableX"]]$dependOn(c("Schart", "variables", "Phase2_S", "mean_S", "SD_S", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong" , "Wlimits2"))
    }
  }
  # Report
  if (options[["CCReport"]]) {
    if (is.null(jaspResults[["CCReport"]])) {

      jaspResults[["Report"]] <- createJaspContainer(gettext("Report"))
      jaspResults[["Report"]]$dependOn(c("CCReport", "Schart", "Xbarchart", "variables", "variablesLong", "CCDataFormat", "subgroups"))
      jaspResults[["Report"]]$position <- 9
      Iplot <- jaspResults[["Report"]]
    }
    Iplot[["ccReport"]] <- .CCReport(dataset = dataset[measurements], options = options, manualXaxis = subgroups, warningLimits_X = options[["Wlimits"]], Phase2_X = options$Phase2_XR, target_X = options$mean_XR, sd_X = options$SD_XR,
                                           warningLimits_S = options[["Wlimits2"]], Phase2_S = options$Phase2_S, target_S = options$mean_S, sd_S = options$SD_S)

    Xchart_XR <- .XbarchartNoId(dataset = dataset[measurements], options = options,  manualXaxis = subgroups ,warningLimits = options[["Wlimits"]], Phase2 = options$Phase2_XR, target = options$mean_XR, sd = options$SD_XR)
    Rchart    <- .RchartNoId(dataset = dataset[measurements], options = options, manualXaxis = subgroups, warningLimits = FALSE)
    Schart    <- .XbarSchart(dataset = dataset[measurements], options = options, manualXaxis = subgroups)
    Xchart_S  <- .XbarchartNoId(dataset = dataset[measurements], options = options, warningLimits = options[["Wlimits2"]], manualXaxis = subgroups, Phase2 = options$Phase2_S, target = options$mean_S, sd = options$SD_S)

    Iplot[["ccTableXR"]] <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Xchart_XR$sixsigma, Phase2 = options$Phase2_XR, xLabels = Xchart_XR$xLabels, name = "X-bar (R)")
    Iplot[["ccTableR"]]  <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Rchart$sixsigma, name = "R", xLabels = Rchart$xLabels)
    Iplot[["ccTableXS"]] <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Xchart_S$sixsigma, Phase2 = options$Phase2_S, xLabels = Xchart_S$xLabels, name = "X-bar (s)")
    Iplot[["ccTableS"]]  <- .NelsonTable(dataset = dataset[measurements], options = options, name = "s", sixsigma = Schart$sixsigma, xLabels = Schart$xLabels)
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
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks) + 2.5)
  dfLabel <- data.frame(
    x = max(xLimits - 1),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, decimalplaces(data1[1,1]) + 1)),
      gettextf("UCL = %g",   round(UCL, decimalplaces(data1[1,1]) + 2)),
      gettextf("LCL = %g",   round(LCL, decimalplaces(data1[1,1]) + 2))
    )
  )

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
.CCReport <- function(dataset, options, manualXaxis, warningLimits_X, Phase2_X, target_X, sd_X,
                      warningLimits_S, Phase2_S, target_S, sd_S){

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
  Rchart <- .RchartNoId(dataset = dataset, options = options, manualXaxis = manualXaxis, warningLimits = FALSE)

  #Create X-bar and S chart
  Schart <- .XbarSchart(dataset = dataset, options = options, manualXaxis = manualXaxis)
  Xchart_S <- .XbarchartNoId(dataset = dataset, options = options, warningLimits = warningLimits_S, manualXaxis = manualXaxis, Phase2 = Phase2_S, target = target_S, sd = sd_S)

  matrixPlot <- createJaspPlot(width = 1200, height = 1000)
  plotMat <- matrix(list(), 3, 2)
  plotMat[[1, 1]] <- .ggplotWithText(text1)
  plotMat[[1, 2]] <- .ggplotWithText(text2)
  plotMat[[2, 1]] <- Xchart_R$p
  plotMat[[2, 2]] <- Rchart$p
  plotMat[[3, 1]] <- Xchart_S$p
  plotMat[[3, 2]] <- Schart$p

  p <- jaspGraphs::ggMatrixPlot(plotMat, topLabels = c(gettextf("Control Charts Report for %s", title), ""))
  matrixPlot$plotObject <- p

  return(matrixPlot)
}

