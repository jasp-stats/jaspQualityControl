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
  
  #dataset <- read.csv("C:/Users/Jonee/Google Drive/SKF Six Sigma/JASP Data Library/2_1_VariablesChartsForSubgroups/SubgroupChartLongFormatStagesMultiDefined.csv")
  #dataset <- read.csv("C:/Users/Jonee/Google Drive/SKF Six Sigma/Datasets/ControlChartError.csv")
  
  wideFormat <- (options[["CCDataFormat"]] == "CCwideFormat")
  
  # In wide format we have one subgroup per row, else we need a either a grouping variable or later specify subgroup size manually
  if (wideFormat) {
    measurements <- unlist(options[["variables"]])
    axisLabels <- options[["axisLabels"]]
    factorVariables <- axisLabels
  } else {
    measurements <- options[["variablesLong"]]
    subgroupVariable <- options[["subgroups"]]
    factorVariables <- subgroupVariable
  }
  
  stages <- options[["stages"]]
  factorVariables <- c(factorVariables, stages)
  measurements <- measurements[measurements != ""]
  factorVariables <- factorVariables[factorVariables != ""]
  
  # Check if analysis is ready
  if (wideFormat) {
    ready <- length(measurements) > 1
  } else if (!wideFormat && options[["subgroupSizeType"]] == "manual"){
    ready <- length(measurements) == 1
  } else if (!wideFormat && options[["subgroupSizeType"]] == "groupingVariable") {
    ready <- length(measurements) == 1 && subgroupVariable != ""
  }

  # Return an empty plot as default
  if (!ready) {
    plot <- createJaspPlot(title = gettext("Control Charts"), width = 700, height = 400)
    jaspResults[["plot"]] <- plot
    plot$dependOn(c("CCReport", "TypeChart", "variablesLong", "variables", "stages"))
    return()
  }

  # Data reading
  if (is.null(dataset) && ready) {
    if (length(factorVariables) >= 1) {
      dataset <- .readDataSetToEnd(columns.as.numeric = measurements, columns.as.factor = factorVariables)
    } else {
      dataset <- .readDataSetToEnd(columns.as.numeric = measurements)
    }
  }
  
  # Rearrange data if not already wide format (one group per row)
  if (!wideFormat && ready) {
    # if subgroup size is set manual, use that. Else determine subgroup size from largest level in subgroups variable
    if (options[["subgroupSizeType"]] == "manual") {
      k <- options[["CCSubgroupSize"]]
      if (stages != "") {
        # Only take the first stage of each subgroup, to avoid multiple stages being defined
        stagesPerSubgroup <- dataset[[stages]][seq(1, length(dataset[[stages]]), k)]
        # this line checks whether there are any subgroups with more than one stage assigned, so we can display a message later that only the first stage is used
        multipleStagesPerSubgroupDefined <- any(lapply(split(dataset[[stages]], ceiling(seq_along(dataset[[stages]])/k)), FUN = function(x)length(unique(x))) > 1)
      }
      # fill up with NA to allow all subgroup sizes
      if(length(dataset[[measurements]]) %% k != 0) {
        rest <- length(dataset[[measurements]]) %% k
        dataset_expanded <- c(dataset[[measurements]], rep(NA, k - rest))
        dataset <- as.data.frame(matrix(dataset_expanded, ncol = k, byrow = TRUE))
      } else {
        dataset <- as.data.frame(matrix(dataset[[measurements]], ncol = k, byrow = TRUE))
      }
      measurements <- colnames(dataset)
      axisLabels <- as.character(seq_len(nrow(dataset)))
      if (stages != "") {
        dataset[[stages]] <- stagesPerSubgroup
        axisLabels <- axisLabels[order(dataset[[stages]])]
      }
    } else {
      subgroups <- dataset[[subgroupVariable]]
      subgroups <- na.omit(subgroups)
      # add sequence of occurence to allow pivot_wider
      if (stages != "") {
        # this line checks whether there are any subgroups with more than one stage assigned, so we can display a message later that only the first stage is used
        multipleStagesPerSubgroupDefined <- any(table(dplyr::count_(dataset, vars = c(stages, subgroupVariable))[subgroupVariable]) > 1)
        # Only take the first defined stage of each subgroup, to avoid multiple stages being defined
        stagesPerSubgroup <- dataset[[stages]][match(unique(subgroups), subgroups)]
      }
      occurenceVector <- with(dataset, ave(seq_along(subgroups), subgroups, FUN = seq_along))
      dataset$occurence <- occurenceVector
      # transform into one group per row
      dataset <- tidyr::pivot_wider(data = dataset[c(measurements, subgroupVariable, "occurence")],
                                    values_from = tidyr::all_of(measurements), names_from = occurence)
      # arrange into dataframe 
      dataset <- as.data.frame(dataset)
      measurements <- as.character(unique(occurenceVector))
      axisLabels <- dataset[[subgroupVariable]]
      if (stages != ""){
        dataset[[stages]] <- stagesPerSubgroup
        axisLabels <- axisLabels[order(dataset[[stages]])]
      }
    }
  }  else if (wideFormat && ready) {
    multipleStagesPerSubgroupDefined <- FALSE # not possible in this format
    if (axisLabels != "")
      axisLabels <- dataset[[axisLabels]]
  }
  
  #Checking for errors in the dataset
  .hasErrors(dataset, type = c('infinity', 'missingValues'),
             all.target = c(options[["measurementsWideFormat"]], options[["subgroup"]]),
             exitAnalysisIfErrors = TRUE)

  .hasErrors(dataset, type = c('infinity', 'missingValues'),
             infinity.target = c(measurements, options[["subgroup"]]),
             missingValues.target = c(options[["subgroup"]]),
             exitAnalysisIfErrors = TRUE)
  
  #X bar & R/s chart
  if (ready) {
    if (is.null(jaspResults[["controlCharts"]])) {
      secondPlotType <- ifelse(options[["TypeChart"]] == "xBarRchart", "R", "s")
      jaspResults[["controlCharts"]] <- createJaspPlot(title =  gettextf("X-bar & %1$s Control Chart", secondPlotType), width = 1200, height = 500)
      jaspResults[["controlCharts"]]$dependOn(c("TypeChart", "variables", "Wlimits", "Phase2", "mean", "manualTicks", 'nTicks',
                                                "SD", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong",
                                                "CCReport", "ccTitle", "ccName", "ccMisc","ccReportedBy","ccDate", "ccSubTitle",
                                                "ccChartName", "subgroupSizeUnequal", "axisLabels", "stages"))
      jaspResults[["controlCharts"]]$position <- 1
      
      if (length(measurements) > 50 && secondPlotType == "R") { # if the subgroup size is above 50, the R package cannot calculate R charts.
        jaspResults[["controlCharts"]]$setError(gettextf("Subgroup size is >50, R chart calculation is not possible. Use S-chart instead."))
        return()
      } else {
        columnsToPass <- c(measurements, stages)
        columnsToPass <- columnsToPass[columnsToPass != ""]
        xBarSdType <- tolower(secondPlotType)
        
        # first chart is always xBar-chart, second is either R- or s-chart
        xBarChart <- .controlChartPlotFunction(dataset = dataset[columnsToPass], plotType = "xBar", stages = stages, xBarSdType = xBarSdType,
                                               phase2 = options[["Phase2"]], phase2Mu = options[["mean"]], phase2Sd = options[["SD"]],
                                               limitsPerSubgroup = (options[["subgroupSizeUnequal"]] == "actualSizes"),
                                               warningLimits = options[["Wlimits"]], xAxisLabels = axisLabels)
        secondChart <- .controlChartPlotFunction(dataset = dataset[columnsToPass], plotType = secondPlotType, , stages = stages, phase2 = options[["Phase2"]],
                                                 phase2Sd = options[["SD"]], limitsPerSubgroup = (options[["subgroupSizeUnequal"]] == "actualSizes"),
                                                 xAxisLabels = axisLabels)
        jaspResults[["controlCharts"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(secondChart$plotObject, xBarChart$plotObject), layout = matrix(2:1, 2), removeXYlabels= "x")
      }

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTables"]])) {
        jaspResults[["NelsonTables"]] <- createJaspContainer(title = gettext("Out-of-control Signals"))
        jaspResults[["NelsonTables"]]$dependOn(c("TypeChart", "variables", "Phase2", "mean", "SD", "CCSubgroupSize",
                                                 "CCDataFormat", "subgroups", "variablesLong", "stages"))
        jaspResults[["NelsonTables"]]$position <- 2
        allTables <- jaspResults[["NelsonTables"]]
        
        allTables[["xBarTable"]]  <- xBarChart$table
        allTables[["secondTable"]] <- secondChart$table
        
        if (stages != "" ) {
          if (multipleStagesPerSubgroupDefined)
          allTables[["xBarTable"]]$addFootnote(gettext("One or more subgroups are assigned to more than one stage. Only first stage is considered."))
        }
        if (length(measurements) > 5 && secondPlotType == "R") # if the subgroup size is above 5, R chart is not recommended
          allTables[["secondTable"]]$addFootnote(gettext("Subgroup size is >5, results may be biased. S-chart is recommended."))
      }
    }
    # Report
    if (options[["report"]] && is.null(jaspResults[["CCReport"]])) {

      jaspResults[["SPlot"]] <- NULL
      jaspResults[["XbarPlot"]] <- NULL
      jaspResults[["NelsonTables"]] <- NULL

      jaspResults[["CCReport"]] <- createJaspContainer(gettext("Report"))
      jaspResults[["CCReport"]]$dependOn(c("CCReport", "CCSubgroupSize","TypeChart", "variables", "variablesLong",
                                           "manualTicks", 'nTicks',"CCDataFormat", "subgroups", "ccTitle", "ccName",
                                           "ccMisc","ccReportedBy","ccDate", "ccSubTitle", "ccChartName", "stages"))
      jaspResults[["CCReport"]]$position <- 9
      Iplot <- jaspResults[["CCReport"]]
      
      if (options$TypeChart == "xBarSchart")
        Iplot[["ccReport"]] <- .CCReport(p1 = Xchart$p, p2 = Schart$p, ccTitle = options$ccTitle,
                                         ccName = options$ccName, ccDate = options$ccDate, ccReportedBy = options$ccReportedBy, ccSubTitle = options$ccSubTitle,
                                         ccChartName = options$ccChartName)
      else
        Iplot[["ccReport"]] <- .CCReport(p1 = Xchart$p, p2 = Rchart$p , ccTitle = options[["reportTitle"]],
                                         ccName = options[["reportMeasurementName"]], ccDate = options[["reportDate"]], ccReportedBy = options[["reportReportedBy"]], ccSubTitle = options[["reportSubtitle"]],
                                         ccChartName = options[["reportChartName"]], ccMisc = options[["reportMiscellaneous"]])
    }
  }
}

#Functions for control charts
.XbarSchart <- function(dataset, options, manualXaxis = "", Phase2 = FALSE, sd = "", Wide = FALSE, OnlyOutofLimit = FALSE,
                        controlLimitsPerGroup = FALSE) {

  #remove rows with single observation as no meaningful sd and no CL can be computed
  rowRemovalIndex <- which(apply(dataset, 1, function(x) sum(!is.na(x)) < 2)) #get index of rows with less than 2 obs.
  if (length(rowRemovalIndex) != 0)
    dataset <- dataset[-rowRemovalIndex, ]

  data <- dataset[, unlist(lapply(dataset, is.numeric))]
  decimals <- max(.decimalplaces(data))

  sigma <- .sdXbar(data, type = "s")
  sixsigma <- qcc::qcc(data, type ='S', plot = FALSE, center = sigma, sizes = ncol(data))

  if(Phase2 && sd != ""){
    sixsigma <- list(statistics = sixsigma$statistics,
                     limits = KnownControlStats.RS(sixsigma$sizes[1], as.numeric(sd))$limits,
                     center = KnownControlStats.RS(sixsigma$sizes[1], as.numeric(sd))$center)
  }


  n <- apply(data, 1, function(x) return(sum(!is.na(x)))) # returns the number of non NA values per row
  if (!controlLimitsPerGroup) # if control limits are not calculated per group they are based on largest group size
    n <- max(n)

  if (length(sixsigma$statistics) == 1)
    OnlyOutofLimit <- TRUE  # other rules don't apply if only 1 group

  subgroups <- c(1:length(sixsigma$statistics))
  data_plot <- data.frame(subgroups = subgroups, Stdv = sixsigma$statistics)

  limits <- .controlLimits(sigma = sigma, n = n, type = "s")
  center <- sigma
  UCL <- limits$UCL
  LCL <- limits$LCL
  # arrange data for CL in df
  cl_plot <- data.frame(LCL = LCL, UCL = UCL, center = center, subgroups = subgroups)
  # repeat last observation and offset all but first subgroup by -.5 to align on x-axis
  cl_plot <- rbind(cl_plot, data.frame(LCL = cl_plot$LCL[nrow(cl_plot)],
                                       UCL = cl_plot$UCL[nrow(cl_plot)],
                                       center = cl_plot$center[nrow(cl_plot)],
                                       subgroups = cl_plot$subgroups[nrow(cl_plot)] + 1))
  cl_plot$subgroups[-1] <- cl_plot$subgroups[-1] - .5

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, data_plot$Stdv, UCL))
  yLimits <- range(yBreaks)
  if (options[["manualTicksXAxis"]])
    nxBreaks <- options[["manualTicksXAxisValue"]]
  else
    nxBreaks <- 5
  xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups, n = nxBreaks)[-1])
  xLimits <- c(1,max(xBreaks) * 1.15)

  # get (one of) the most frequent centers, LCL and UCL to display them
  centerDisplay <- as.numeric(names(sort(-table(center)))[1])
  LCLDisplay <- as.numeric(names(sort(-table(LCL)))[1])
  UCLDisplay <- as.numeric(names(sort(-table(UCL)))[1])

  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(centerDisplay, UCLDisplay, LCLDisplay),
    l = c(
      gettextf("CL = %g", round(centerDisplay, decimals + 1)),
      gettextf("UCL = %g",   round(UCLDisplay, decimals + 2)),
      gettextf("LCL = %g",   round(LCLDisplay, decimals + 2))
    )
  )
  xLimits <- range(c(xBreaks, dfLabel$x))


  ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = range)) +
    ggplot2::geom_step(data = cl_plot, mapping = ggplot2::aes(x = subgroups, y = UCL), col = "red",
                       size = 1.5, linetype = "dashed") +
    ggplot2::geom_step(data = cl_plot, mapping = ggplot2::aes(x = subgroups, y = LCL), col = "red",
                       size = 1.5, linetype = "dashed") +
    ggplot2::geom_step(data = cl_plot, mapping = ggplot2::aes(x = subgroups, y = center), col = "green", size = 1)

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = Stdv)) +
    ggplot2::geom_step(data = cl_plot, mapping = ggplot2::aes(x = subgroups, y = UCL), col = "red",
                       size = 1.5, linetype = "dashed") +
    ggplot2::geom_step(data = cl_plot, mapping = ggplot2::aes(x = subgroups, y = LCL), col = "red",
                       size = 1.5, linetype = "dashed") +
    ggplot2::geom_step(data = cl_plot, mapping = ggplot2::aes(x = subgroups, y = center), col = "green", size = 1) +
    ggplot2::geom_label(data = dfLabel, ggplot2::aes(x = x, y = y, label = l), size = 4) +
    ggplot2::scale_y_continuous(name =  gettext("Subgroup st dev"), breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(name = gettext('Subgroup'), breaks = xBreaks) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (OnlyOutofLimit)
    p <- p + jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$Stdv > UCL | data_plot$Stdv < LCL, "red", "blue"))
  else
    p <- p + jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma)$red_points, "red", "blue"))


  if (!identical(manualXaxis, "")) {
    if (Wide){
      xBreaks_Out <- manualXaxis
      p <- p + ggplot2::scale_x_continuous(breaks = xBreaks, labels = xBreaks_Out[xBreaks])
    } else{
      xBreaks_Out <- unique(manualXaxis) # use unique to preserve original order unlike levels
      xLabels <- xBreaks_Out[xBreaks]

      xLimits <- c(range(xBreaks)[1], range(xBreaks)[2] * 1.15)

      p <- p + ggplot2::scale_x_continuous(breaks = xBreaks, labels = xLabels, limits = xLimits)
    }
  }

  if (!identical(manualXaxis, ""))
    return(list(p = p, sixsigma = sixsigma, xLabels = as.vector(xBreaks_Out)))
  else
    return(list(p = p, sixsigma = sixsigma))
}

.CCReport <- function(p1 = "", p2 = "", ccTitle = "", ccName = "", ccDate = "",
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
