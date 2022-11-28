
# imports from packages
#' @importFrom jaspBase createJaspContainer createJaspPlot createJaspState createJaspTable createJaspHtml
#' .extractErrorMessage .hasErrors isTryError .readDataSetToEnd


#############################################################
## Common functions for plots ###############################
#############################################################

# Function to create the x-bar and r-chart section
.qcXbarAndRContainer <- function(options, dataset, ready, jaspResults, measurements, subgroups, wideFormat) {

  if (!is.null(jaspResults[["controlCharts"]]))
    return()

  container <- createJaspContainer(title = gettext("Control Chart"))
  container$dependOn(options = c("controlChartsType", "pcReportDisplay", "variables", "subgroups", "variablesLong",
                                 "pcSubgroupSize", "manualSubgroupSize", "manualTicks", 'nTicks', "xbarR", "IMR"))
  container$position <- 1
  jaspResults[["controlCharts"]] <- container

  matrixPlot <- createJaspPlot(title = "X-bar & R control chart", width = 1200, height = 550)
  container[["plot"]] <- matrixPlot

  if (!ready)
    return()

  if (length(measurements) < 2) {
    matrixPlot$setError(gettext("Subgroup size must be > 1 to display X-bar & R Chart."))
    return()
  }

  plotMat <- matrix(list(), 2, 1)
  plotMat[[1,1]] <- .Xbarchart(dataset = dataset[measurements], options = options, manualXaxis = subgroups,
                               warningLimits = FALSE, Wide = wideFormat, manualTicks = options$manualTicks)$p
  plotMat[[2,1]] <- .Rchart(dataset = dataset[measurements], options = options, manualXaxis = subgroups,
                            warningLimits = FALSE, Wide = wideFormat, manualTicks = options$manualTicks)$p
  matrixPlot$plotObject <- cowplot::plot_grid(plotlist = plotMat, ncol = 1, nrow = 2)
}

.qcReadData <- function(dataset, options, type) {
  if (type == "capabilityStudy") {
    if (is.null(dataset)) {
      if (options[["subgroups"]] != "") {
        dataset <- .readDataSetToEnd(columns.as.numeric = options[["variables"]], columns.as.factor = options[["subgroups"]])
      } else {
        dataset <- .readDataSetToEnd(columns.as.numeric = options[["variables"]])
      }
    }
  }
  return(dataset)
}

# Function to create X-bar chart
.Xbarchart <- function(dataset, options, manualLimits = "", warningLimits = TRUE, manualSubgroups = "", yAxis = TRUE,
                       plotLimitLabels = TRUE, yAxisLab = gettext("Sample average"), xAxisLab = gettext("Subgroup"), manualDataYaxis = "",
                       manualXaxis = "", manualTicks = FALSE, title = "", smallLabels = FALSE, Phase2 = FALSE,
                       target = NULL, sd = NULL, OnlyOutofLimit = FALSE, GaugeRR = FALSE, Wide = FALSE) {
  data <- dataset[, unlist(lapply(dataset, is.numeric))]
  decimals <- max(.decimalplaces(data))
  if(Phase2)
    sixsigma <- qcc::qcc(data, type ='xbar', plot=FALSE, center = as.numeric(target), std.dev = as.numeric(sd))
  else
    sixsigma <- qcc::qcc(data, type ='xbar', plot=FALSE)

  if (!identical(manualSubgroups, "")) {
    subgroups <- manualSubgroups
  } else {
    subgroups = c(1:length(sixsigma$statistics))
  }
  means = sixsigma$statistics
  data_plot <- data.frame(subgroups = subgroups, means = means)
  sd1 <- sixsigma$std.dev
  if (!identical(manualLimits, "")) {
    LCL <- manualLimits[1]
    center <- manualLimits[2]
    UCL <- manualLimits[3]
  } else {
    center <- sixsigma$center
    UCL <- max(sixsigma$limits)
    LCL <- min(sixsigma$limits)
  }
  if (!identical(manualDataYaxis, "")) {
    manualMeans <- rowMeans(manualDataYaxis)
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL, manualMeans))
  } else {
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL, means))
  }
  yLimits <- range(yBreaks)
  if (manualTicks)
    nxBreaks <- options$nTicks
  else
    nxBreaks <- 5
  xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups, n = nxBreaks)[-1])
  xLimits <- c(range(xBreaks)[1], range(xBreaks)[2] * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, decimals + 1)),
      gettextf("UCL = %g",   round(UCL, decimals + 2)),
      gettextf("LCL = %g",   round(LCL, decimals + 2))
    )
  )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = means)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green', size = 1) +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5)
  if (warningLimits) {
    warn.limits <- c(qcc::limits.xbar(sixsigma$center, sixsigma$std.dev, sixsigma$sizes, 1),
                     qcc::limits.xbar(sixsigma$center, sixsigma$std.dev, sixsigma$sizes, 2))
    p <- p + ggplot2::geom_hline(yintercept = warn.limits, color = "orange", linetype = "dashed", size = 1)
  }
  if (yAxis) {
    p <- p + ggplot2::scale_y_continuous(name = gettext(yAxisLab) ,limits = yLimits, breaks = yBreaks)
  } else {
    p <- p + ggplot2::scale_y_continuous(name = ggplot2::element_blank(), limits = yLimits, breaks = yBreaks, labels = NULL) +
      ggplot2::theme(axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
  }

  if(smallLabels){
    labelSize <- 3.5
    lineSize <- 0.5
    pointsSize <- 3
  }else{
    labelSize <- 4.5
    lineSize <- 1
    pointsSize <- 4
  }

  if (plotLimitLabels)
    p <- p + ggplot2::geom_label(data = dfLabel, ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = labelSize)

  p <- p + ggplot2::scale_x_continuous(name = gettext(xAxisLab), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue", size = lineSize) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(fontsize = jaspGraphs::setGraphOption("fontsize", 15))

  if (warningLimits) {
    warn.limits <- c(qcc::limits.xbar(sixsigma$center, sixsigma$std.dev, sixsigma$sizes, 1),
                     qcc::limits.xbar(sixsigma$center, sixsigma$std.dev, sixsigma$sizes, 2))
    p <- p + ggplot2::geom_hline(yintercept = warn.limits, color = "orange", linetype = "dashed", size = 1)
  }

  # Out of control red dots marking
  if (Phase2)
    p <- p + jaspGraphs::geom_point(size = pointsSize, fill = ifelse(NelsonLaws(sixsigma)$red_points, "red", "blue"))
  else if (OnlyOutofLimit)
    p <- p + jaspGraphs::geom_point(size = pointsSize, fill = ifelse(data_plot$means > UCL | data_plot$means < LCL, "red", "blue"))
  else
    p <- p + jaspGraphs::geom_point(size = pointsSize, fill = ifelse(NelsonLaws(sixsigma, allsix = TRUE)$red_points, "red", "blue"))

  # if more than half of the dots are violations, do not show red dots.
  n.outOfLimits <- sum(data_plot$means > UCL , data_plot$means < LCL)
  if ( n.outOfLimits > (nrow(data_plot) / 2) )
    p <- p + jaspGraphs::geom_point(size = pointsSize, fill = "blue")

  if (!identical(manualXaxis, "")) {
    if (GaugeRR | Wide){
      xBreaks_Out <- manualXaxis
      p <- p + ggplot2::scale_x_continuous(breaks = xBreaks, labels = xBreaks_Out[xBreaks])
    }
    else{
      xBreaks_Out <- manualXaxis[seq(1,length(manualXaxis), ncol(data))]
      xLabels <- xBreaks_Out[xBreaks]
      xLimits <- c(range(xBreaks)[1], range(xBreaks)[2] * 1.15)
      dfLabel <- data.frame(
        x = max(xLimits) * 0.95,
        y = c(center, UCL, LCL),
        l = c(
          gettextf("CL = %g", round(center, decimals + 1)),
          gettextf("UCL = %g",   round(UCL, decimals + 2)),
          gettextf("LCL = %g",   round(LCL, decimals + 2))
        )
      )

      p <- p + ggplot2::scale_x_continuous(name = xAxisLab, breaks = xBreaks, labels = xLabels, limits = xLimits)
    }
  }


  if (title != "")
    p <- p + ggplot2::ggtitle(title)

  if (!identical(manualXaxis, ""))
    return(list(p = p, sixsigma = sixsigma, xLabels = as.vector(xBreaks_Out)))
  else return(list(p = p, sixsigma = sixsigma))
}

# Function to create R chart
.Rchart <- function(dataset, options, manualLimits = "", warningLimits = TRUE, manualSubgroups = "", yAxis = TRUE,
                    plotLimitLabels = TRUE, Phase2 = FALSE, target = NULL, sd = "", yAxisLab = gettext("Sample range"),
                    xAxisLab = gettext("Subgroup"), manualDataYaxis = "", manualXaxis = "", title = "", smallLabels = FALSE,
                    OnlyOutofLimit = FALSE, GaugeRR = FALSE, Wide = FALSE, manualTicks = FALSE) {

  #Arrange data and compute
  data <- dataset[, unlist(lapply(dataset, is.numeric))]
  decimals <- max(.decimalplaces(data))
  sixsigma <- qcc::qcc(data, type ='R', plot = FALSE)

  if(Phase2 && sd != "")
    sixsigma <- list(statistics = sixsigma$statistics,
                     limits = KnownControlStats.RS(sixsigma$sizes[1], as.numeric(sd))$limits,
                     center = KnownControlStats.RS(sixsigma$sizes[1], as.numeric(sd))$center)

  range <- sixsigma$statistics
  if (!identical(manualSubgroups, "")) {
    subgroups <- manualSubgroups
  } else {
    subgroups = c(1:length(sixsigma$statistics))
  }
  data_plot <- data.frame(subgroups = subgroups, range = range)
  if (!identical(manualLimits, "")) {
    LCL <- manualLimits[1]
    center <- manualLimits[2]
    UCL <- manualLimits[3]
  }else{
    center <- sixsigma$center
    UCL <- max(sixsigma$limits)
    LCL <- min(sixsigma$limits)
  }
  if (!identical(manualDataYaxis, "")) {
    manualRange <- apply(manualDataYaxis, 1, function(x) max(x) - min(x))
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL, manualRange))
  }else{
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL - (0.10 * abs(LCL)), range, UCL + (0.1 * UCL)), min.n = 4)
  }
  yLimits <- range(yBreaks)
  if (manualTicks)
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
      gettextf("LCL = %g",   round(LCL, decimals + 2))
    )
  )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = range)) +
    ggplot2::geom_hline(yintercept = center,  color = 'green', size = 1) +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", , linetype = "dashed", size = 1.5)
  if (warningLimits) {
    warn.limits <- c(qcc::limits.xbar(sixsigma$center, sixsigma$std.dev, sixsigma$sizes, 1),
                     qcc::limits.xbar(sixsigma$center, sixsigma$std.dev, sixsigma$sizes, 2))
    p <- p + ggplot2::geom_hline(yintercept = warn.limits, color = "orange", linetype = "dashed", size = 1)
  }
  if (yAxis){
    p <- p + ggplot2::scale_y_continuous(name = gettext(yAxisLab) ,limits = yLimits, breaks = yBreaks)
  }else{
    p <- p +
      ggplot2::scale_y_continuous(name = ggplot2::element_blank() ,limits = yLimits, breaks = yBreaks, labels = NULL) +
      ggplot2::theme(axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
  }

  if(smallLabels){
    labelSize <- 3.5
    lineSize <- 0.5
    pointsSize <- 3
  }else{
    labelSize <- 4.5
    lineSize <- 1
    pointsSize <- 4
  }
  if (plotLimitLabels)
    p <- p + ggplot2::geom_label(data = dfLabel, ggplot2::aes(x = x, y = y, label = l), inherit.aes = FALSE, size = labelSize)

  p <- p + ggplot2::scale_x_continuous(name= gettext(xAxisLab), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue", size = lineSize) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(fontsize = jaspGraphs::setGraphOption("fontsize", 15))

  if (!identical(manualXaxis, "")) {
    if (GaugeRR | Wide){
      xBreaks_Out <- manualXaxis
      p <- p + ggplot2::scale_x_continuous(name = xAxisLab, breaks = xBreaks, labels = xBreaks_Out[xBreaks])
    }
    else{
      xBreaks_Out <- manualXaxis[seq(1,length(manualXaxis), ncol(data))]
      xLabels <- xBreaks_Out[xBreaks]

      xLimits <- c(range(xBreaks)[1], range(xBreaks)[2] * 1.15)
      dfLabel <- data.frame(
        x = max(xLimits) * 0.95,
        y = c(center, UCL, LCL),
        l = c(
          gettextf("CL = %g", round(center, decimals + 1)),
          gettextf("UCL = %g",   round(UCL, decimals + 2)),
          gettextf("LCL = %g",   round(LCL, decimals + 2))
        )
      )

      p <- p + ggplot2::scale_x_continuous(name = xAxisLab, breaks = xBreaks, labels = xLabels, limits = xLimits)
    }
  }

  if (OnlyOutofLimit)
    p <- p + jaspGraphs::geom_point(size = pointsSize, fill = ifelse(data_plot$range > UCL | data_plot$range < LCL, "red", "blue"))
  else
    p <- p + jaspGraphs::geom_point(size = pointsSize, fill = ifelse(NelsonLaws(sixsigma)$red_points, "red", "blue"))

  # if more than half of the dots are violations, do not show red dots.
  n.outOfLimits <- sum(data_plot$range > UCL , data_plot$range < LCL)
  if ( n.outOfLimits > (nrow(data_plot) / 2) )
    p <- p + jaspGraphs::geom_point(size = pointsSize, fill = "blue")

  if (title != "")
    p <- p + ggplot2::ggtitle(title)

  if (!identical(manualXaxis, ""))
    return(list(p = p, sixsigma = sixsigma, xLabels = as.vector(xBreaks_Out)))
  else return(list(p = p, sixsigma = sixsigma))
}

NelsonLaws <- function(data, allsix = FALSE, chart = "i", xLabels = NULL) {

  # Adjust Rules to SKF
  pars <- Rspc::SetParameters()
  pars$Rule2$nPoints = 7
  pars$Rule3$nPoints = 7
  pars$Rule3$convention = "minitab"
  pars$Rule4$convention = "minitab"

  #Evaluate all rules
  if (chart == "p"){
    n = length(data$statistics)
    warnings <- data.frame(x = rep(1,n), Rule1 = rep(1,n), Rule2 = rep(1,n), Rule3 = rep(1,n))
    for( i in 1:length(data$statistics)){
      warningsRaw <- Rspc::EvaluateRules(x = c(data$statistics[i],0), type = "c", lcl = data$limits[i,1], ucl = data$limits[i,2], cl = data$center, parRules = pars,
                                         whichRules = c(1:3,5,7:8))[1,]
      warnings[i,] <- warningsRaw
    }
  } else{
    warnings <- Rspc::EvaluateRules(x = data$statistics, type = chart, lcl = data$limits[1,1], ucl = data$limits[1,2], cl = data$center, parRules = pars,
                                    whichRules = c(1:3,5,7:8))
  }

  if (allsix) {
    if (length(xLabels) == 0) {
      Rules <- list(R1 = which(warnings[,2] == 1),
                    R2 = which(warnings[,3] == 1),
                    R3 = which(warnings[,4] == 1),
                    R4 = which(warnings[,5] == 1),
                    R5 = which(warnings[,6] == 1),
                    R6 = which(warnings[,7] == 1))
    }
    else {
      Rules <- list(R1 = xLabels[which(warnings[,2] == 1)],
                    R2 = xLabels[which(warnings[,3] == 1)],
                    R3 = xLabels[which(warnings[,4] == 1)],
                    R4 = xLabels[which(warnings[,5] == 1)],
                    R5 = xLabels[which(warnings[,6] == 1)],
                    R6 = xLabels[which(warnings[,7] == 1)])
    }
    red_points = apply(warnings[,-1], 1, sum) > 0
  }
  else {
    if (length(xLabels) == 0) {
      Rules <- list(R1 = which(warnings[,2] == 1),
                    R2 = which(warnings[,3] == 1),
                    R3 = which(warnings[,4] == 1))
    }
    else {
      Rules <- list(R1 = xLabels[which(warnings[,2] == 1)],
                    R2 = xLabels[which(warnings[,3] == 1)],
                    R3 = xLabels[which(warnings[,4] == 1)])
    }
    red_points = apply(warnings[,c(2,3,4)], 1, sum) > 0
  }

  return(list(red_points = red_points, Rules = Rules))
}

.NelsonTable <- function(dataset, options, sixsigma, type = "xbar", Phase2 = TRUE, name = "X-bar", xLabels = NULL) {

  table <- createJaspTable(title = gettextf("Test results for %s chart", name))

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

.decimalplaces <- function(x) {
  x <- unlist(x)
  nDecimals <- numeric(length(x))
  for(i in seq_along(x)) {
    if (round(x[i] %% 1, 10) != 0) {   # never more than 10 decimals
      formattedx <- format(x[i], scientific = FALSE)
      nDecimals[i] <- nchar(strsplit(sub('0+$', '', as.character(formattedx)), ".", fixed=TRUE)[[1]][[2]])
    } else {
      nDecimals[i] <- 0
    }
  }
  return(nDecimals)
}

.IMRchart <- function(dataset, options, variable = "", measurements = "", cowPlot = FALSE, manualXaxis = "", Wide = FALSE,
                      stages = "") {

  if (identical(stages, "")) {
    nStages <- 1
  } else {
    nStages <- length(unique(dataset[[stages]]))
  }
  
  ppPlot <- createJaspPlot(width = 900 + nStages * 100, height = 650)
  
  # Calculate values
  dataPlotI <- list(allValues = list())
  dataPlotR <- list(allValues = list())
  for (i in seq_len(nStages)) {
    if (identical(stages, "")) {
      dataForPlot <- dataset
    } else {
      dataForPlot <- subset(dataset, dataset[[stages]] == unique(dataset[[stages]])[i])
    }
    
    if (identical(measurements, "") && !identical(variable, "")) {
      ppPlot$dependOn(optionContainsValue = list(variables = variable))
      data <- data.frame(process = dataForPlot[[variable]])
      sixsigma_I <- qcc::qcc(data$process, type ='xbar.one', plot=FALSE)
      xmr.raw.r <- matrix(cbind(data$process[1:length(data$process)-1], data$process[2:length(data$process)]), ncol = options$ncol)
      sixsigma_R <- qcc::qcc(xmr.raw.r, type="R", plot = FALSE)
    } else {
      data <- as.vector((t(dataForPlot[measurements])))
      sixsigma_I <- qcc::qcc(data, type ='xbar.one', plot=FALSE)
      xmr.raw.r <- matrix(cbind(data[1:length(data)-1],data[2:length(data)]), ncol = 2)
      sixsigma_R <- qcc::qcc(xmr.raw.r, type="R", plot = FALSE)
    }
    dataPlotI[[1]] <- c(unlist(dataPlotI[[1]]), unlist(sixsigma_I$statistics), min(sixsigma_I$limits), max(sixsigma_I$limits))  # all values and limits to calculate decimals and axes
    if (i != 1) {
      subgroupsI <- seq(max(dataPlotI[[i]]$subgroups) + 1, max(dataPlotI[[i]]$subgroups) + length(sixsigma_I$statistics))  # to keep counting across groups
      subgroupsR <- seq(max(dataPlotR[[i]]$subgroups) + 1, max(dataPlotR[[i]]$subgroups) + length(sixsigma_R$statistics) + 1)
      } else {
      subgroupsI <- c(1:length(sixsigma_I$statistics))
      subgroupsR <- seq_len(length(sixsigma_R$statistics) + 1)
    }
    dataPlotI[[i + 1]] <- list(subgroups = subgroupsI, process = sixsigma_I$statistics, center = sixsigma_I$center,
                               UCL = max(sixsigma_I$limits), LCL = min(sixsigma_I$limits), sixsigma_I = sixsigma_I)
    dataPlotR[[1]] <- c(unlist(dataPlotR[[1]]), unlist(sixsigma_R$statistics), max(sixsigma_R$limits, min(sixsigma_R$limits)))
    dataPlotR[[i + 1]] <- list(subgroups = subgroupsR, movingRange = c(NA, sixsigma_R$statistics),
                            center = sixsigma_R$center, UCL = max(sixsigma_R$limits), LCL = min(sixsigma_R$limits), sixsigma_R = sixsigma_R)
  }
  decimals1 <- max(.decimalplaces(unlist(dataPlotI$allValues)))
  decimals2 <- max(.decimalplaces(dataPlotR$allValues))
  yBreaks1 <- jaspGraphs::getPrettyAxisBreaks(dataPlotI$allValues)
  yBreaks2 <- jaspGraphs::getPrettyAxisBreaks(dataPlotR$allValues)

  # remove all values list, to allow looping over other lists
  dataPlotI <- dataPlotI[-1]
  dataPlotR <- dataPlotR[-1]
  
  plotMat <- matrix(list(), 2, nStages)

  # Create plots
  for (i in seq_len(nStages)) {
    subgroups = dataPlotI[[i]]$subgroups
    center <- dataPlotI[[i]]$center
    UCL <- dataPlotI[[i]]$UCL
    LCL <- dataPlotI[[i]]$LCL
    if (options$manualTicks){
      nxBreaks <- options$nTicks
      xBreaks1 <- as.integer(c(jaspGraphs::getPrettyAxisBreaks(subgroups, n = nxBreaks)))
    } else {
      xBreaks1 <- as.integer(c(jaspGraphs::getPrettyAxisBreaks(subgroups)))
    }
    if (min(xBreaks1) == 0)  # never start counting at 0 on x axis
      xBreaks1[1] <- 1
    
    xLimits <- c(min(xBreaks1), max(xBreaks1) * 1.15)
    dfLabel <- data.frame(
      x = max(xLimits) * 0.95,
      y = c(center, UCL, LCL),
      l = c(
        gettextf("CL = %g",  round(center, decimals1 + 1)),
        gettextf("UCL = %g", round(UCL, decimals1 + 2)),
        gettextf("LCL = %g", round(LCL, decimals1 + 2))
      )
    )
    
    df1 <- data.frame(process = dataPlotI[[i]]$process, subgroups = subgroups)

    p1 <- ggplot2::ggplot(df1, ggplot2::aes(x = subgroups, y = process)) +
      ggplot2::geom_hline(yintercept = center, color = 'green') +
      ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
      ggplot2::scale_y_continuous(name = ifelse(variable != "" , gettextf("%s", variable), "Individual value"),
                                  breaks = yBreaks1, limits = range(yBreaks1)) +
      ggplot2::scale_x_continuous(name = gettext('Observation'), breaks = xBreaks1, limits = xLimits) +
      jaspGraphs::geom_line(color = "blue") +
      jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(dataPlotI[[i]]$sixsigma_I, allsix = TRUE)$red_points, 'red', 'blue')) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
    
    if(i != 1)
      p1 <- p1 + ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank())

    #Moving range chart
    subgroups <- dataPlotR[[i]]$subgroups
    center <- dataPlotR[[i]]$center
    UCL <- dataPlotR[[i]]$UCL
    LCL <- dataPlotR[[i]]$LCL
    if (options$manualTicks){
      nxBreaks <- options$nTicks
      xBreaks2 <- as.integer(c(jaspGraphs::getPrettyAxisBreaks(subgroups, n = nxBreaks)))
    } else {
      xBreaks2 <- as.integer(c(jaspGraphs::getPrettyAxisBreaks(subgroups)))
    }
    if (min(xBreaks2) == 0) # never start counting at 0 on x axis
      xBreaks2[1] <- 1
    xLimits <- c(min(xBreaks2),max(xBreaks2) * 1.15)
    dfLabel <- data.frame(
      x = max(xLimits) * 0.95,
      y = c(center, UCL, LCL),
      l = c(
        gettextf("CL = %g", round(center, decimals2 + 1)),
        gettextf("UCL = %g",   round(UCL, decimals2 + 2)),
        gettextf("LCL = %g",   round(LCL, decimals2 + 2))
      )
    )
    
    df2 <- data.frame(subgroups = subgroups, movingRange = dataPlotR[[i]]$movingRange)

    p2 <- ggplot2::ggplot(df2, ggplot2::aes(x = subgroups, y = movingRange)) +
      ggplot2::geom_hline(yintercept = center, color = 'green') +
      ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red",linetype = "dashed", size = 1.5) +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
      ggplot2::scale_y_continuous(name = gettext("Moving Range"), breaks = yBreaks2, limits = range(yBreaks2)) +
      ggplot2::scale_x_continuous(name = gettext('Observation'), breaks = xBreaks2, limits = xLimits) +
      jaspGraphs::geom_line(color = "blue") +
      jaspGraphs::geom_point(size = 4, fill = ifelse(c(NA, NelsonLaws(dataPlotR[[i]]$sixsigma_R)$red_points), 'red', 'blue')) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
    
    if(i != 1)
      p2 <- p2 + ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                                axis.title.y = ggplot2::element_blank())
    

    if (!identical(manualXaxis, "")) {
      if (!identical(measurements, "")) {
        if (Wide)
          xLabels <- as.vector(sapply(1:length(manualXaxis), function(x) {rep(manualXaxis[x], ncol(dataForPlot[measurements]))}))
        else
          xLabels <- manualXaxis
      }
      else
        xLabels <- manualXaxis

      p1 <- p1 + ggplot2::scale_x_continuous(breaks = xBreaks1, labels = xLabels[xBreaks1])
      p2 <- p2 + ggplot2::scale_x_continuous(breaks = xBreaks2, labels = xLabels[xBreaks2])
    }

    plotMat[[1,i]] <- p1
    plotMat[[2,i]] <- p2
  }

  if(!cowPlot){
    ppPlot$plotObject <-  jaspGraphs::ggMatrixPlot(plotList = plotMat, removeXYlabels= "x")
  }else{
    ppPlot$plotObject <- cowplot::plot_grid(plotlist = plotMat, ncol = nStages, nrow = 2, byrow = FALSE)
  }

  if (!identical(manualXaxis, ""))
    return(list(p = ppPlot, sixsigma_I = sixsigma_I, sixsigma_R = sixsigma_R, xLabels = as.vector(xLabels), p1 = p1, p2 = p2))
  else
    return(list(p = ppPlot, sixsigma_I = sixsigma_I, sixsigma_R = sixsigma_R, p1 = p1, p2 = p2))
}
