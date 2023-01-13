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
variablesChartsSubgroups <- function(jaspResults, dataset, options) {
  
  wideFormat <- options[["CCDataFormat"]] == "CCwideFormat"
  
  # In wide format we have one subgroup per row, else we need a either a grouping variable or later specify subgroup size manually
  if (wideFormat) {
    measurements <- unlist(options$variables)
    subgroupVariableGiven <- FALSE
  } else {
    measurements <- options$variablesLong
    subgroupVariable <- options$subgroups
    subgroupVariableGiven <- subgroupVariable != ""
  }
  
  measurements <- measurements[measurements != ""]
  
  # Check if analysis is ready
  if (wideFormat) {
    ready <- length(measurements) > 1
  } else if (!wideFormat && options[["subgroupSizeType"]] == "manual"){
    ready <- length(measurements) == 1
  } else if (!wideFormat && options[["subgroupSizeType"]] == "groupingVariable") {
    ready <- length(measurements) == 1 && subgroupVariableGiven
  }
  
  # Return an empty plot as default
  if ((options$CCReport || options$TypeChart == "Xbarchart" || options$TypeChart == "Schart") && !ready) {
    plot <- createJaspPlot(title = gettext("Control Charts"), width = 700, height = 400)
    jaspResults[["plot"]] <- plot
    plot$dependOn(c("CCReport", "TypeChart", "variablesLong", "variables"))
    return()
  }
  
  # Data reading
  if (is.null(dataset) && ready) {
    if (subgroupVariableGiven) {
      dataset <- .readDataSetToEnd(columns.as.numeric = measurements, columns.as.factor = subgroupVariable)
    } else {
      dataset <- .readDataSetToEnd(columns.as.numeric = measurements)
    }
  }
  
  # Rearrange data if not already one group per row
  if (!wideFormat && ready){
    # if subgroup size is set manual, use that. Else determine subgroup size from largest level in subgroups variable
    if (options[["subgroupSizeType"]] == "manual") {
      k <- options[["CCSubgroupSize"]]
      # fill up with NA to allow all subgroup sizes
      if(length(dataset[[measurements]]) %% k != 0) {
        rest <- length(dataset[[measurements]]) %% k
        dataset_expanded <- c(dataset[[measurements]], rep(NA, k - rest))
        dataset <- as.data.frame(matrix(dataset_expanded, ncol = k, byrow = TRUE))
      } else {
        dataset <- as.data.frame(matrix(dataset[[measurements]], ncol = k, byrow = TRUE))
      }
      
      measurements <- colnames(dataset)
    } else{
      subgroups <- dataset[[subgroupVariable]]
      subgroups <- na.omit(subgroups)
      
      # add sequence of occurence to allow pivot_wider
      dataset$occurence <- with(dataset, ave(seq_along(subgroups), subgroups, FUN = seq_along))
      # transform into one group per row
      dataset <- tidyr::pivot_wider(data = dataset, values_from = measurements, names_from = occurence)
      # arrange into dataframe 
      dataset <- as.data.frame(dataset)
      measurements <- colnames(dataset)[-1]
    }

    
    
    # if (identical(dataset, "error")) {
    #   plot <- createJaspPlot(title = gettext("Control Charts"), width = 700, height = 400)
    #   jaspResults[["plot"]] <- plot
    #   plot$setError(gettextf("Could not equally divide data points into groups of size %i.", k))
    #   plot$dependOn("CCSubgroupSize")
    #   return()
    # }
    
  }
  
  #Checking for errors in the dataset
  .hasErrors(dataset, type = c('infinity', 'missingValues'),
             all.target = c(options$variables, options$subgroups),
             exitAnalysisIfErrors = TRUE)
  
  .hasErrors(dataset, type = c('infinity', 'missingValues'),
             infinity.target = c(measurements, options$subgroups),
             missingValues.target = c(options$subgroups),
             exitAnalysisIfErrors = TRUE)
  
  #X bar & R chart
  if (ready){
    if (options$TypeChart == "Xbarchart" && is.null(jaspResults[["XbarPlot"]])) {
      jaspResults[["XbarPlot"]] <- createJaspPlot(title =  gettext("X-bar & R Control Chart"), width = 1200, height = 500)
      jaspResults[["XbarPlot"]]$dependOn(c("TypeChart", "variables", "Wlimits", "Phase2", "mean", "manualTicks", 'nTicks',"SD", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong", "CCReport", "ccTitle", "ccName", "ccMisc","ccReportedBy","ccDate", "ccSubTitle", "ccChartName"))
      
      Xchart <- .Xbarchart(dataset = dataset[measurements], options = options, warningLimits = options[["Wlimits"]], Phase2 = options$Phase2, target = options$mean, sd = options$SD, Wide = wideFormat, manualTicks = options$manualTicks)
      Rchart <- .Rchart(dataset = dataset[measurements], options = options, warningLimits = FALSE, Phase2 = options$Phase2, target = options$mean, sd = options$SD, Wide = wideFormat, manualTicks = options$manualTicks)
      jaspResults[["XbarPlot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(Rchart$p, Xchart$p), layout = matrix(2:1, 2), removeXYlabels= "x")
      jaspResults[["XbarPlot"]]$position <- 1
      
      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTableX"]]) && is.null(jaspResults[["NelsonTableR"]]) && is.null(jaspResults[["NelsonTables"]])) {
        jaspResults[["NelsonTables"]] <- createJaspContainer(title = gettext("Out-of-control Signals"))
        jaspResults[["NelsonTables"]]$dependOn(c("TypeChart", "variables", "Phase2", "mean", "SD", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong"))
        jaspResults[["NelsonTables"]]$position <- 2
        AllTables <- jaspResults[["NelsonTables"]]
        
        AllTables[["NelsonTableX"]]  <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Xchart$sixsigma, Phase2 = options$Phase2, xLabels = Xchart$xLabels)
        AllTables[["NelsonTableR"]] <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Rchart$sixsigma, name = "R", xLabels = Rchart$xLabels)
      }
    }
    
    #S Chart
    if (options$TypeChart == "Schart" && is.null(jaspResults[["SPlot"]])) {
      jaspResults[["SPlot"]] <- createJaspPlot(title = gettext("X-bar & s Control Chart"), width = 1200, height = 500)
      jaspResults[["SPlot"]]$dependOn(c("TypeChart", "variables", "Wlimits", "Phase2", "mean", "SD", "manualTicks", 'nTicks', "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong", "CCReport", "ccTitle", "ccName", "ccMisc","ccReportedBy","ccDate", "ccSubTitle", "ccChartName"))
      
      Schart <- .XbarSchart(dataset = dataset[measurements], options = options, Phase2 = options$Phase2, sd = options$SD, Wide = wideFormat)
      Xchart <- .Xbarchart(dataset = dataset[measurements], options = options, warningLimits = options[["Wlimits"]], Phase2 = options$Phase2, target = options$mean, sd = options$SD, Wide = wideFormat, manualTicks = options$manualTicks)
      jaspResults[["SPlot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(Schart$p, Xchart$p), layout = matrix(2:1, 2), removeXYlabels= "x")
      jaspResults[["SPlot"]]$position <- 1
      
      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTableS"]]) && is.null(jaspResults[["NelsonTableX"]]) && is.null(jaspResults[["NelsonTables"]])) {
        jaspResults[["NelsonTables"]] <- createJaspContainer(title = gettext("Out-of-control Signals"))
        jaspResults[["NelsonTables"]]$dependOn(c("TypeChart", "variables", "Phase2", "mean", "SD", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong", "Wlimits"))
        jaspResults[["NelsonTables"]]$position <- 2
        AllTables <- jaspResults[["NelsonTables"]]
        
        AllTables[["NelsonTableX"]] <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Xchart$sixsigma, Phase2 = options$Phase2, xLabels = Xchart$xLabels)
        AllTables[["NelsonTableS"]] <- .NelsonTable(dataset = dataset[measurements], options = options, name = "s", sixsigma = Schart$sixsigma, xLabels = Schart$xLabels)
      }
    }
    # Report
    if (options[["CCReport"]] && is.null(jaspResults[["CCReport"]])) {
      
      jaspResults[["SPlot"]] <- NULL
      jaspResults[["XbarPlot"]] <- NULL
      jaspResults[["NelsonTables"]] <- NULL
      
      jaspResults[["CCReport"]] <- createJaspContainer(gettext("Report"))
      jaspResults[["CCReport"]]$dependOn(c("CCReport", "CCSubgroupSize","TypeChart", "variables", "variablesLong", "manualTicks", 'nTicks',"CCDataFormat", "subgroups", "ccTitle", "ccName", "ccMisc","ccReportedBy","ccDate", "ccSubTitle", "ccChartName"))
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
    }
  }
}

#Functions for control charts
.XbarSchart <- function(dataset, options, manualXaxis = "", Phase2 = options$Phase2, sd = "", Wide = FALSE) {
  data <- dataset[, unlist(lapply(dataset, is.numeric))]
  decimals <- max(.decimalplaces(data))
  sixsigma <- qcc::qcc(data, type ='S', plot = FALSE)
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
  if (options$manualTicks)
    nxBreaks <- options$nTicks
  else
    nxBreaks <- 5
  xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups, n = nxBreaks)[-1])
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, decimals + 1)),
      gettextf("UCL = %g",   round(UCL, decimals + 2)),
      gettextf("LCL = %g",   round(LCL, decimals + 2)))
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
  
  if (!identical(manualXaxis, "")) {
    if (Wide){
      xBreaks_Out <- manualXaxis
      p <- p + ggplot2::scale_x_continuous(breaks = xBreaks, labels = xBreaks_Out[xBreaks])
    } else{
      xBreaks_Out <- manualXaxis[seq(1,length(manualXaxis), ncol(data))]
      xLabels <- xBreaks_Out[xBreaks]
      
      xLimits <- c(range(xBreaks)[1], range(xBreaks)[2] * 1.15)
      dfLabel <- data.frame(
        x = max(xLimits) * 0.95,
        y = c(center, UCL, LCL),
        l = c(
          gettextf("CL = %g", round(center, decimals + 1)),
          gettextf("UCL = %g",   round(UCL, decimals + 2)),
          gettextf("LCL = %g",   round(LCL, decimals + 2)))
      )
      
      p <- p + ggplot2::scale_x_continuous(breaks = xBreaks, labels = xLabels, limits = xLimits)
    }
  }
  
  if (!identical(manualXaxis, ""))
    return(list(p = p, sixsigma = sixsigma, xLabels = as.vector(xBreaks_Out)))
  else 
    return(list(p = p, sixsigma = sixsigma))
}

.CCReport <- function(ImR = FALSE,p1 = "", p2 = "", ccTitle = "", ccName = "", ccDate = "",
                      ccReportedBy = "", ccMisc = "" , ccSubTitle = "", ccChartName = "") {
  
  if (ccTitle == "")
    title <- "Report for Control Charts"
  else
    title <- ccTitle
  name <- gettextf("Name: %s", ccName)
  date <- gettextf("Date: %s", ccDate)
  text1 <- c(name, date)
  
  reportedBy <- gettextf("Reported by: %s", ccReportedBy)
  misc <- gettextf("Misc: %s", ccMisc)
  text2 <- c(reportedBy, misc)
  
  matrixPlot <- createJaspPlot(width = 1200, aspectRatio = 1)
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
