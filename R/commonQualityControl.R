
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
                            Wide = wideFormat, manualTicks = options$manualTicks)$p
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
                       target = NULL, sd = NULL, OnlyOutofLimit = FALSE, GaugeRR = FALSE, Wide = FALSE, sdType = c("r", "s"),
                       controlLimitsPerGroup = FALSE) {
  data <- dataset[, unlist(lapply(dataset, is.numeric))]
  decimals <- max(.decimalplaces(data))
  sdType <- match.arg(sdType) 
  if(Phase2) {
    mu <- as.numeric(target)
    sigma <- as.numeric(sd)
  } else {
    #hand calculate mean and sd as the package gives wrong results with NAs
    mu <- mean(unlist(data), na.rm = TRUE)
    sigma <- .sdXbar(df = data, type = sdType)
  }
  sixsigma <- qcc::qcc(data, type ='xbar', plot = FALSE, center = mu, sizes = ncol(data), std.dev = sigma)
  
  #calculate group sizes
  n <- apply(data, 1, function(x) return(sum(!is.na(x)))) # returns the number of non NA values per row
  if (!controlLimitsPerGroup) # if control limits are not calculated per group they are based on largest group size
    n <- max(n)
  
  if (length(sixsigma$statistics) == 1)
    OnlyOutofLimit <- TRUE  # other rules don't apply if only 1 group
  
  if (!identical(manualSubgroups, "")) {
    subgroups <- manualSubgroups
  } else {
    subgroups <- c(1:length(sixsigma$statistics))
  }
  means <- sixsigma$statistics
  data_plot <- data.frame(subgroups = subgroups, means = means)
  sd1 <- sixsigma$std.dev
  if (!identical(manualLimits, "")) {
    LCL <- manualLimits[1]
    center <- manualLimits[2]
    UCL <- manualLimits[3]
  } else {
    limits <- .controlLimits(mu, sigma, n = n, type = "xbar")
    center <- mu
    UCL <- limits$UCL
    LCL <- limits$LCL
  }
  # upper and lower warning limits at 1 sd and 2sd
  WL1 <- .controlLimits(mu, sigma, n = n, type = "xbar", k = 1)
  WL2 <- .controlLimits(mu, sigma, n = n, type = "xbar", k = 2)
  UWL1 <- WL1$UCL
  LWL1 <- WL1$LCL
  UWL2 <- WL2$UCL
  LWL2 <- WL2$LCL
  
  # arrange data for CL in df
  cl_plot <- data.frame(LCL = LCL, UCL = UCL, center = center, subgroups = subgroups,
                        UWL1 = UWL1, LWL1 = LWL1, UWL2 = UWL2, LWL2 = LWL2)
  # repeat last observation and offset all but first subgroup by -.5 to align on x-axis
  cl_plot <- rbind(cl_plot, data.frame(LCL = cl_plot$LCL[nrow(cl_plot)],
                                       UCL = cl_plot$UCL[nrow(cl_plot)],
                                       center = cl_plot$center[nrow(cl_plot)],
                                       subgroups = cl_plot$subgroups[nrow(cl_plot)] + 1,
                                       UWL1 = cl_plot$UWL1[nrow(cl_plot)],
                                       LWL1 = cl_plot$LWL1[nrow(cl_plot)],
                                       UWL2 = cl_plot$UWL2[nrow(cl_plot)],
                                       LWL2 = cl_plot$LWL2[nrow(cl_plot)]
  ))
  cl_plot$subgroups[-1] <- cl_plot$subgroups[-1] - .5
  
  
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
  xLimits <- c(1, max(xBreaks) * 1.15)
  
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
  
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = means)) +
    ggplot2::geom_step(data = cl_plot, mapping = ggplot2::aes(x = subgroups, y = UCL), col = "red",
                       size = 1.5, linetype = "dashed") +
    ggplot2::geom_step(data = cl_plot, mapping = ggplot2::aes(x = subgroups, y = LCL), col = "red",
                       size = 1.5, linetype = "dashed") +
    ggplot2::geom_step(data = cl_plot, mapping = ggplot2::aes(x = subgroups, y = center), col = "green", size = 1)
  
  if (warningLimits) {
    p <- p + ggplot2::geom_step(data = cl_plot, mapping = ggplot2::aes(x = subgroups, y = UWL1), col = "orange",
                                size = 1, linetype = "dashed") +
      ggplot2::geom_step(data = cl_plot, mapping = ggplot2::aes(x = subgroups, y = LWL1), col = "orange",
                         size = 1, linetype = "dashed") +
      ggplot2::geom_step(data = cl_plot, mapping = ggplot2::aes(x = subgroups, y = UWL2), col = "orange",
                         size = 1, linetype = "dashed") +
      ggplot2::geom_step(data = cl_plot, mapping = ggplot2::aes(x = subgroups, y = LWL2), col = "orange",
                         size = 1, linetype = "dashed")
  }
  if (yAxis) {
    p <- p + ggplot2::scale_y_continuous(name = gettext(yAxisLab), limits = yLimits, breaks = yBreaks)
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
    } else {
      
      xBreaks_Out <- unique(manualXaxis) # use unique to preserve original order unlike levels
      xLabels <- xBreaks_Out[xBreaks]
      xLimits <- c(range(xBreaks)[1], range(xBreaks)[2] * 1.15)
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
.Rchart <- function(dataset, options, manualLimits = "", manualSubgroups = "", yAxis = TRUE,
                    plotLimitLabels = TRUE, Phase2 = FALSE, target = NULL, sd = "", yAxisLab = gettext("Sample range"),
                    xAxisLab = gettext("Subgroup"), manualDataYaxis = "", manualXaxis = "", title = "", smallLabels = FALSE,
                    OnlyOutofLimit = FALSE, GaugeRR = FALSE, Wide = FALSE, manualTicks = FALSE,
                    controlLimitsPerGroup = FALSE) {
  
  #remove rows with single observation as no meaningful range and no CL can be computed
  rowRemovalIndex <- which(apply(dataset, 1, function(x) sum(!is.na(x)) < 2)) #get index of rows with less than 2 obs.
  if (length(rowRemovalIndex) != 0)
    dataset <- dataset[-rowRemovalIndex, ]
  
  #Arrange data and compute decimals
  data <- dataset[, unlist(lapply(dataset, is.numeric))]
  decimals <- max(.decimalplaces(data))
  
  n <- apply(data, 1, function(x) return(sum(!is.na(x)))) # returns the number of non NA values per row
  if (!controlLimitsPerGroup) # if control limits are not calculated per group they are based on largest group size
    n <- max(n)
  #hand calculate mean and sd as the package gives wrong results with NAs
  sigma <- .sdXbar(df = data, type = "r")
  d2 <- sapply(n, function(x) KnownControlStats.RS(x, 0)$constants[1])
  mu <- sigma * d2
  sixsigma <- qcc::qcc(data, type ='R', plot = FALSE, center = mu, std.dev = sigma, sizes = ncol(data))
  
  if (length(sixsigma$statistics) == 1)
    OnlyOutofLimit <- TRUE  # other rules don't apply if only 1 group
  
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
    limits <- .controlLimits(mu, sigma, n = n, type = "r")
    center <- mu
    UCL <- limits$UCL
    LCL <- limits$LCL
  }
  # arrange data for CL in df
  cl_plot <- data.frame(LCL = LCL, UCL = UCL, center = center, subgroups = subgroups)
  # repeat last observation and offset all but first subgroup by -.5 to align on x-axis
  cl_plot <- rbind(cl_plot, data.frame(LCL = cl_plot$LCL[nrow(cl_plot)],
                                       UCL = cl_plot$UCL[nrow(cl_plot)],
                                       center = cl_plot$center[nrow(cl_plot)],
                                       subgroups = cl_plot$subgroups[nrow(cl_plot)] + 1))
  cl_plot$subgroups[-1] <- cl_plot$subgroups[-1] - .5
  
  
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
  
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = range)) +
    ggplot2::geom_step(data = cl_plot, mapping = ggplot2::aes(x = subgroups, y = UCL), col = "red",
                       size = 1.5, linetype = "dashed") +
    ggplot2::geom_step(data = cl_plot, mapping = ggplot2::aes(x = subgroups, y = LCL), col = "red",
                       size = 1.5, linetype = "dashed") +
    ggplot2::geom_step(data = cl_plot, mapping = ggplot2::aes(x = subgroups, y = center), col = "green", size = 1)
  
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
      xBreaks_Out <- unique(manualXaxis)  # use unique to preserve original order unlike levels
      xLabels <- xBreaks_Out[xBreaks]
      xLimits <- c(range(xBreaks)[1], range(xBreaks)[2] * 1.15)
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
    warnings <- Rspc::EvaluateRules(x = data$statistics, type = chart, lcl = data$limits[1,1], ucl = data$limits[1,2], cl = data$center[1], parRules = pars,
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

.NelsonTableList <- function(dataset, options, sixsigma, type = "xbar", Phase2 = TRUE, name = "X-bar", xLabels = NULL) {
  
  violationsList <- list()
  
  if (length(sixsigma$statistics) == 1) # no need for table with only 1 group
    return(list())
  
  if (!Phase2 || type == "xbar.one") {
    Test <- NelsonLaws(data = sixsigma, allsix = TRUE, xLabels = xLabels)
    
    if (length(Test$Rules$R1) > 0)
      violationsList[["test1"]] <- Test$Rules$R1
    if (length(Test$Rules$R2) > 0)
      violationsList[["test2"]] <- Test$Rules$R2
    if (length(Test$Rules$R3) > 0)
      violationsList[["test3"]] <- Test$Rules$R3
    if (length(Test$Rules$R4) > 0)
      violationsList[["test4"]] <- Test$Rules$R4
    if (length(Test$Rules$R5) > 0)
      violationsList[["test5"]] <- Test$Rules$R5
    if (length(Test$Rules$R6) > 0)
      violationsList[["test6"]] <- Test$Rules$R6
  } else {
    if (name == "np" || name == "c" || name == "u" || name == "Laney p'" || name == "Laney u'")
      Test <- NelsonLaws(data = sixsigma, xLabels = xLabels, chart = "c")
    else if (name == "P")
      Test <- NelsonLaws(data = sixsigma, xLabels = xLabels, chart = "p")
    else
      Test <- NelsonLaws(data = sixsigma, xLabels = xLabels)
    
    if (type == "Range" & length(xLabels) == 0) {
      Test$Rules$R1 <- Test$Rules$R1 + 1  
      Test$Rules$R2 <- Test$Rules$R2 + 1
      Test$Rules$R3 <- Test$Rules$R3 + 1
    }
    
    if (length(Test$Rules$R1) > 0)
      violationsList[["test1"]] <- Test$Rules$R1
    if (length(Test$Rules$R2) > 0)
      violationsList[["test2"]] <- Test$Rules$R2
    if (length(Test$Rules$R3) > 0)
      violationsList[["test3"]] <- Test$Rules$R3
  }
  return(violationsList)
}

.decimalplaces <- function(x) {
  x <- na.omit(unlist(x))
  nDecimals <- numeric(length(x))
  for(i in seq_along(x)) {
    if (round(x[i], 10) %% 1 != 0) {   # never more than 10 decimals
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
  
  ppPlot <- createJaspPlot(width = 900, height = 650)
  tableI <- createJaspTable(title = gettextf("Test results for Individuals chart"))
  tableR <- createJaspTable(title = gettextf("Test results for Range chart"))

  if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
    
    # Error conditions for stages
    if(any(table(dataset[[stages]]) < options[["movingRangeLength"]])) {
      ppPlot$setError(gettext("Moving range length is larger than one of the stages."))
      return(list(p = ppPlot))
    } 
  } else {
    nStages <- 1
    dataset$stage <- 1
    stages <- "stage"
  }
  
  ppPlot$width <- 900 + nStages * 100
  
  # Calculate values per subplot/stage
  
  
  ### TODO: Why does it crash if k = subgroup length?
  dataPlotI <- data.frame(matrix(ncol = 7, nrow = 0))
  dataPlotR <- data.frame(matrix(ncol = 7, nrow = 0))
  tableIList <- list()
  tableRList <- list()
  colnames(dataPlotI) <- c("process", "subgroup", "stage", "LCL", "UCL", "center", "dotColor")
  colnames(dataPlotR) <- c("movingRange", "subgroup", "stage", "LCL", "UCL", "center", "dotColor")
  dfLabelI <- data.frame(matrix(ncol = 3, nrow = 0))
  dfLabelR <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(dfLabelI) <- colnames(dfLabelR) <- c("x", "y", "label")
  seperationLinesI <- c()
  seperationLinesR <- c()
  for (i in seq_len(nStages)) {
    stage <- unique(dataset[[stages]])[i]
    dataForPlot <- subset(dataset, dataset[[stages]] == stage)
    if (identical(measurements, "") && !identical(variable, "")) {
      #ppPlot$dependOn(optionContainsValue = list(variables = variable))
      data <- data.frame(process = dataForPlot[[variable]])
      k <- options[["movingRangeLength"]]
      sd <- qcc::sd.xbar.one(data$process, k = k)
      sixsigma_I <- qcc::qcc(data$process, type ='xbar.one', plot = FALSE, std.dev = sd)
      # qcc has no moving range plot, so we need to arrange data in a matrix with the observation + k future observation per row and calculate the range chart
      mrMatrix <- matrix(data$process[1:(length(data$process) - (k - 1))])   # remove last k - 1 elements
      for (j in 2:k) {
        mrMatrix <- cbind(mrMatrix, matrix(data$process[j:(length(data$process) - (k - j))]))   # remove first i and last (k - i) elements
      }
      sixsigma_R <- qcc::qcc(mrMatrix, type = "R", plot = FALSE)
    } else {
      data <- as.vector((t(dataForPlot[measurements])))
      k <- options[["movingRangeLength"]]
      sd <- qcc::sd.xbar.one(data, k = k)
      sixsigma_I <- qcc::qcc(data, type ='xbar.one', plot=FALSE, std.dev = sd)
      # qcc has no moving range plot, so we need to arrange data in a matrix with the observation + k future observation per row and calculate the range chart
      mrMatrix <- matrix(data[1:(length(data) - (k - 1))])   # remove last k - 1 elements
      for (j in 2:k) {
        mrMatrix <- cbind(mrMatrix, matrix(data[j:(length(data) - (k - j))]))   # remove first i and last (k - i) elements
      }
      sixsigma_R <- qcc::qcc(mrMatrix, type = "R", plot = FALSE)
    }
      if (i != 1) {
        subgroupsI <- seq(max(dataPlotI$subgroup) + 1, max(dataPlotI$subgroup) + length(sixsigma_I$statistics))  # to keep counting across groups
        subgroupsR <- seq(max(dataPlotR$subgroup) + 1, max(dataPlotR$subgroup) + length(sixsigma_R$statistics) + 1)
        seperationLinesI <- c(seperationLinesI, max(dataPlotI$subgroup) + .5)
        seperationLinesR <- c(seperationLinesR, max(dataPlotR$subgroup) + .5)
      } else {
        subgroupsI <- c(1:length(sixsigma_I$statistics))
        subgroupsR <- seq_len(length(sixsigma_R$statistics) + 1)
      }
      if (length(sixsigma_I$statistics) > 1) {
        dotColorI <- ifelse(NelsonLaws(sixsigma_I, allsix = TRUE)$red_points, 'red', 'blue')
      } else {
        dotColorI <- 'blue'
      }
      if (length(sixsigma_R$statistics) > 1) {
        dotColorR <- ifelse(c(NA, NelsonLaws(sixsigma_R)$red_points), 'red', 'blue')
      } else {
        dotColorR <- 'blue'
      }
    processI <- sixsigma_I$statistics
    LCLI <- min(sixsigma_I$limits)
    UCLI <- max(sixsigma_I$limits)
    centerI <- sixsigma_I$center
    dataPlotI <- rbind(dataPlotI, data.frame("process" = processI,
                                             "subgroup" = subgroupsI,
                                             "stage" = stage,
                                             "LCL" = LCLI,
                                             "UCL" = UCLI,
                                             "center" = centerI,
                                             "dotColor" = dotColorI))
    movingRange <- c(NA, sixsigma_R$statistics)
    LCLR <- min(sixsigma_R$limits)
    UCLR <- max(sixsigma_R$limits)
    centerR <- sixsigma_R$center
    dataPlotR <- rbind(dataPlotR, data.frame("movingRange" = movingRange,
                                             "subgroup" = subgroupsR,
                                             "stage" = stage,
                                             "LCL" = LCLR,
                                             "UCL" = UCLR,
                                             "center" = centerR,
                                             "dotColor" = dotColorR))
    allStageValuesI <- c(processI, LCLI, UCLI, centerI)
    allStageValuesR <- c(movingRange, LCLR, UCLR, centerR)
    decimals1 <- max(.decimalplaces(allStageValuesI))
    decimals2 <- max(.decimalplaces(allStageValuesR))
    dfLabelI <- rbind(dfLabelI, data.frame(x = max(subgroupsI) + .5,
                                           y = c(centerI, UCLI, LCLI),
                                           label = c(
                                             gettextf("CL = %g",  round(centerI, decimals1 + 1)),
                                             gettextf("UCL = %g", round(UCLI, decimals1 + 2)),
                                             gettextf("LCL = %g", round(LCLI, decimals1 + 2))
                                           )))
    dfLabelR <- rbind(dfLabelR, data.frame(x = max(subgroupsR) + .5,
                                           y = c(centerR, UCLR, LCLR),
                                           label = c(
                                             gettextf("CL = %g",  round(centerR, decimals1 + 1)),
                                             gettextf("UCL = %g", round(UCLR, decimals1 + 2)),
                                             gettextf("LCL = %g", round(LCLR, decimals1 + 2))
                                           )))
    tableIList[[i]] <- .NelsonTableList(dataset = dataset, options = options, type = "xbar.one", sixsigma = sixsigma_I, xLabels = subgroupsI)
    if (length(tableIList[[i]]) > 0)
      tableIList[[i]][["stage"]] <- c(as.character(stage), rep("", max(sapply(tableIList[[i]], length)) - 1))
    tableRList[[i]] <- .NelsonTableList(dataset = dataset, options = options, type = "Range", sixsigma = sixsigma_R, xLabels = subgroupsR)
    if (length(tableRList[[i]]) > 0)
      tableRList[[i]][["stage"]] <- c(as.character(stage), rep("", max(sapply(tableRList[[i]], length)) - 1))
  }
  
  tableIListVectorized <- unlist(tableIList, recursive = FALSE)
  if (length(tableIListVectorized) > 0) {
    tableIListCombined <- tapply(tableIListVectorized, names(tableIListVectorized), function(x) unlist(x, FALSE, FALSE))
    tableIListCombined <- as.list(tableIListCombined)
    tableI$addColumnInfo(name = "stage",              title = gettextf("Stage")               , type = "string")
    if("test1" %in% names(tableIListCombined))
      tableI$addColumnInfo(name = "test1",              title = gettextf("Test 1: Beyond limit")               , type = "integer")
    if("test2" %in% names(tableIListCombined))
      tableI$addColumnInfo(name = "test2",              title = gettextf("Test 2: Shift")                   , type = "integer")
    if("test3" %in% names(tableIListCombined))
      tableI$addColumnInfo(name = "test3",              title = gettextf("Test 3: Trend")                        , type = "integer")
    if("test4" %in% names(tableIListCombined))
      tableI$addColumnInfo(name = "test4",              title = gettextf("Test 4: Increasing variation")         , type = "integer")
    if("test5" %in% names(tableIListCombined))
      tableI$addColumnInfo(name = "test5",              title = gettextf("Test 5: Reducing variation")           , type = "integer")
    if("test6" %in% names(tableIListCombined))
      tableI$addColumnInfo(name = "test6",              title = gettextf("Test 6: Bimodal distribution")         , type = "integer")
    tableI$setData(tableIListCombined)
  }
  tableRListVectorized <- unlist(tableRList, recursive = FALSE)
  if (length(tableRListVectorized) > 0) {
    tableRListCombined <- tapply(tableRListVectorized, names(tableRListVectorized), function(x) unlist(x, FALSE, FALSE))
    tableRListCombined <- as.list(tableRListCombined)
    tableR$addColumnInfo(name = "stage",              title = gettextf("Stage")               , type = "string")
    if("test1" %in% names(tableRListCombined))
      tableR$addColumnInfo(name = "test1",              title = gettextf("Test 1: Beyond limit")               , type = "integer")
    if("test2" %in% names(tableRListCombined))
      tableR$addColumnInfo(name = "test2",              title = gettextf("Test 2: Shift")                   , type = "integer")
    if("test3" %in% names(tableRListCombined))
      tableR$addColumnInfo(name = "test3",              title = gettextf("Test 3: Trend")                        , type = "integer")
    if("test4" %in% names(tableRListCombined))
      tableR$addColumnInfo(name = "test4",              title = gettextf("Test 4: Increasing variation")         , type = "integer")
    if("test5" %in% names(tableRListCombined))
      tableR$addColumnInfo(name = "test5",              title = gettextf("Test 5: Reducing variation")           , type = "integer")
    if("test6" %in% names(tableRListCombined))
      tableR$addColumnInfo(name = "test6",              title = gettextf("Test 6: Bimodal distribution")         , type = "integer")
    #tableR$setData(tableRListCombined)
  }
  
  # Calculations that apply to the whole plot
  yBreaks1 <- jaspGraphs::getPrettyAxisBreaks(c(dataPlotI$process, dataPlotI$LCL, dataPlotI$UCL, dataPlotI$center))
  yBreaks2 <- jaspGraphs::getPrettyAxisBreaks(c(dataPlotR$movingRange, dataPlotR$LCL, dataPlotR$UCL, dataPlotR$center))
  if (options$manualTicks) {
    nxBreaks <- options$nTicks
    xBreaks1 <- as.integer(c(jaspGraphs::getPrettyAxisBreaks(dataPlotI$subgroup, n = nxBreaks)))
    xBreaks2 <- as.integer(c(jaspGraphs::getPrettyAxisBreaks(dataPlotR$subgroup, n = nxBreaks)))
  } else {
    xBreaks1 <- as.integer(c(jaspGraphs::getPrettyAxisBreaks(dataPlotI$subgroup)))
    xBreaks2 <- as.integer(c(jaspGraphs::getPrettyAxisBreaks(dataPlotR$subgroup)))
  }
  if (xBreaks1[1] == 0)  # never start counting at 0 on x axis
    xBreaks1[1] <- 1
  if (xBreaks2[1] == 0)
    xBreaks2[1] <- 1
  xLimits <- c(min(xBreaks1), max(xBreaks1) * 1.15)
  
  # Create plots
  p1 <- ggplot2::ggplot(dataPlotI, ggplot2::aes(x = subgroup, y = process, group = stage)) +
    ggplot2::geom_vline(xintercept = seperationLinesI) +
    ggplot2::geom_step(mapping = ggplot2::aes(x = subgroup, y = center) , col = "green", linewidth = 1) +
    ggplot2::geom_step(mapping = ggplot2::aes(x = subgroup, y = UCL) , col = "red", linewidth = 1.5, linetype = "dashed") +
    ggplot2::geom_step(mapping = ggplot2::aes(x = subgroup, y = LCL) , col = "red", linewidth = 1.5, linetype = "dashed") +
    ggplot2::geom_label(data = dfLabelI, mapping = ggplot2::aes(x = x, y = y, label = label),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name = ifelse(variable != "" , gettextf("%s", variable), "Individual value"),
                                breaks = yBreaks1, limits = range(yBreaks1)) +
    ggplot2::scale_x_continuous(name = gettext('Observation'), breaks = xBreaks1, limits = xLimits) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = dataPlotI$dotColor, inherit.aes = TRUE) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  p2 <- ggplot2::ggplot(dataPlotR, ggplot2::aes(x = subgroup, y = movingRange, group = stage)) +
    ggplot2::geom_vline(xintercept = seperationLinesR) +
    ggplot2::geom_step(mapping = ggplot2::aes(x = subgroup, y = center) , col = "green", linewidth = 1) +
    ggplot2::geom_step(mapping = ggplot2::aes(x = subgroup, y = UCL) , col = "red",
                       linewidth = 1.5, linetype = "dashed") +
    ggplot2::geom_step(mapping = ggplot2::aes(x = subgroup, y = LCL) , col = "red",
                       linewidth = 1.5, linetype = "dashed") +
    ggplot2::geom_label(data = dfLabelR, mapping = ggplot2::aes(x = x, y = y, label = label),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name = gettext("Moving Range"), breaks = yBreaks2, limits = range(yBreaks2)) +
    ggplot2::scale_x_continuous(name = gettext('Observation'), breaks = xBreaks2, limits = xLimits) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = dataPlotR$dotColor, inherit.aes = TRUE) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
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
  
  plotMat <- matrix(data = list(), nrow = 2, ncol = 1)
  plotMat[[1,1]] <- p1
  plotMat[[2,1]] <- p2
  
  if(!cowPlot){
    ppPlot$plotObject <-  jaspGraphs::ggMatrixPlot(plotList = plotMat, removeXYlabels= "x")
  } else {
    ppPlot$plotObject <- cowplot::plot_grid(plotlist = plotMat, nrow = 2)
  }
  
  if (!identical(manualXaxis, ""))
    return(list(p = ppPlot, sixsigma_I = sixsigma_I, sixsigma_R = sixsigma_R, xLabels = as.vector(xLabels), p1 = p1, p2 = p2, tableI = tableI, tableR = tableR))
  else
    return(list(p = ppPlot, sixsigma_I = sixsigma_I, sixsigma_R = sixsigma_R, p1 = p1, p2 = p2, tableI = tableI, tableR = tableR))
}

.IMRchart_old <- function(dataset, options, variable = "", measurements = "", cowPlot = FALSE, manualXaxis = "", Wide = FALSE,
                      stages = "") {
  
  ppPlot <- createJaspPlot(width = 900, height = 650)

  if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
    
    # Error conditions for stages
    if(any(table(dataset[[stages]]) < options[["movingRangeLength"]])) {
      ppPlot$setError(gettext("Moving range length is larger than one of the stages."))
      return(list(p = ppPlot))
    } 
  } else {
    nStages <- 1
  }
  
  ppPlot$width <- 900 + nStages * 100
  
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
      k <- options[["movingRangeLength"]]
      sd <- qcc::sd.xbar.one(data$process, k = k)
      sixsigma_I <- qcc::qcc(data$process, type ='xbar.one', plot=FALSE, std.dev = sd)
      # qcc has no moving range plot, so we need to arrange data in a matrix with the observation + k future observation per row and calculate the range chart
      mrMatrix <- matrix(data$process[1:(length(data$process) - (k - 1))])   # remove last k - 1 elements
      for (j in 2:k) {
        mrMatrix <- cbind(mrMatrix, matrix(data$process[j:(length(data$process) - (k - j))]))   # remove first i and last (k - i) elements
      }
      sixsigma_R <- qcc::qcc(mrMatrix, type = "R", plot = FALSE)
    } else {
      data <- as.vector((t(dataForPlot[measurements])))
      k <- options[["movingRangeLength"]]
      sd <- qcc::sd.xbar.one(data, k = k)
      sixsigma_I <- qcc::qcc(data, type ='xbar.one', plot=FALSE, std.dev = sd)
      # qcc has no moving range plot, so we need to arrange data in a matrix with the observation + k future observation per row and calculate the range chart
      mrMatrix <- matrix(data[1:(length(data) - (k - 1))])   # remove last k - 1 elements
      for (j in 2:k) {
        mrMatrix <- cbind(mrMatrix, matrix(data[j:(length(data) - (k - j))]))   # remove first i and last (k - i) elements
      }
      sixsigma_R <- qcc::qcc(mrMatrix, type = "R", plot = FALSE)
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
    if (xBreaks1[1] == 0)  # never start counting at 0 on x axis
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
    
    if (length(dataPlotI[[i]]$sixsigma_I$statistics) > 1) {
      dotColor1 <- ifelse(NelsonLaws(dataPlotI[[i]]$sixsigma_I, allsix = TRUE)$red_points, 'red', 'blue')
    } else {
      dotColor1 <- 'blue'
    }
    
    p1 <- ggplot2::ggplot(df1, ggplot2::aes(x = subgroups, y = process)) +
      ggplot2::geom_hline(yintercept = center, color = 'green') +
      ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
      ggplot2::scale_y_continuous(name = ifelse(variable != "" , gettextf("%s", variable), "Individual value"),
                                  breaks = yBreaks1, limits = range(yBreaks1)) +
      ggplot2::scale_x_continuous(name = gettext('Observation'), breaks = xBreaks1, limits = xLimits) +
      jaspGraphs::geom_line(color = "blue") +
      jaspGraphs::geom_point(size = 4, fill = dotColor1, inherit.aes = TRUE) +
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
    if (xBreaks2[1] == 0) # never start counting at 0 on x axis
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
    
    if (length(dataPlotR[[i]]$sixsigma_R$statistics) > 1) {
      dotColor2 <- ifelse(c(NA, NelsonLaws(dataPlotR[[i]]$sixsigma_R)$red_points), 'red', 'blue')
    } else {
      dotColor2 <- 'blue'
    }
    
    p2 <- ggplot2::ggplot(df2, ggplot2::aes(x = subgroups, y = movingRange)) +
      ggplot2::geom_hline(yintercept = center, color = 'green') +
      ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red",linetype = "dashed", size = 1.5) +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
      ggplot2::scale_y_continuous(name = gettext("Moving Range"), breaks = yBreaks2, limits = range(yBreaks2)) +
      ggplot2::scale_x_continuous(name = gettext('Observation'), breaks = xBreaks2, limits = xLimits) +
      jaspGraphs::geom_line(color = "blue") +
      jaspGraphs::geom_point(size = 4, fill = dotColor2, inherit.aes = TRUE) +
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
  } else {
    ppPlot$plotObject <- cowplot::plot_grid(plotlist = plotMat, ncol = nStages, nrow = 2, byrow = FALSE)
  }
  
  if (!identical(manualXaxis, ""))
    return(list(p = ppPlot, sixsigma_I = sixsigma_I, sixsigma_R = sixsigma_R, xLabels = as.vector(xLabels), p1 = p1, p2 = p2))
  else
    return(list(p = ppPlot, sixsigma_I = sixsigma_I, sixsigma_R = sixsigma_R, p1 = p1, p2 = p2))
}

.sdXbar <- function(df, type = c("s", "r")) {
  if (type == "r"){
    rowRanges <- .rowRanges(df)$ranges
    n <- .rowRanges(df)$n
    d2s <- sapply(n, function(x) return(KnownControlStats.RS(x, 0)$constants[1]))
    sdWithin <- sum((n - 1) * rowRanges / d2s) / sum(n - 1) # Burr (1969), equation 11
  } else if (type == "s") {
    rowSd <- apply(df, 1, sd, na.rm = TRUE)
    sdWithin <- mean(rowSd, na.rm = TRUE)
  }
  return(sdWithin)
} 

.rowRanges <- function(df) {
  nrow <- nrow(df)
  ranges <- c()
  n <- c()
  for (i in seq_len(nrow)) {
    rowVector <- df[i,]
    if (sum((!is.na(rowVector))) < 2) # we need at least 2 values that are not NA to calculate range
      next
    ranges <- c(ranges, max(rowVector, na.rm = TRUE) - min(rowVector, na.rm = TRUE))
    n <- c(n, sum(!is.na(rowVector)))
  }
  return(list(ranges = ranges, n = n))
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
  } else if (N > 50) {
    d3 <- 0.80818 - 0.0051871 * N + 0.00005098 * N^2 - 0.00000019 * N^3
    d2 <- 2.88606 + 0.051313 * N - 0.00049243 * N^2 + 0.00000188 * N^3
  } else {
    d2 <- Data.d2[N == Data.d2$n,2]
    d3 <- Data.d3[N == Data.d3$n,2]
  }
  
  c4 <- sqrt(2/(N-1)) * gamma(N/2) / gamma((N-1)/2)
  c5 <- sqrt(1 - c4^2)
  
  UCL <- d2 * sigma + 3 * d3 * sigma
  CL <- d2 * sigma
  LCL <- max(0, d2 * sigma - 3 * d3 * sigma)
  
  return(list(constants = c(d2, d3, c4, c5), limits = data.frame(LCL,UCL), center = CL))
}

.controlLimits <- function(mu = NA, sigma, n, k = 3, type = c("xbar", "r", "s"), unbiasingConstantUsed = FALSE) {
  type = match.arg(type)
  UCLvector <- c()
  LCLvector <- c()
  for (i in seq_along(n)) {
    if (type == "xbar") {
      UCL <- mu + (k * sigma) / sqrt(n[i])
      LCL <- mu - (k * sigma) / sqrt(n[i])
    } else  if (type == "r") {
      d2 <- KnownControlStats.RS(n[i], 0)$constants[1]
      d3 <- KnownControlStats.RS(n[i], 0)$constants[2]
      UCL <- d2 * sigma + k * d3 * sigma
      LCL <- d2 * sigma - k * d3 * sigma
      LCL <- max(0, LCL) # LCL in R-chart must be >= 0
    } else  if (type == "s") {
      c4 <- KnownControlStats.RS(n[i], 0)$constants[3]
      c5 <- KnownControlStats.RS(n[i], 0)$constants[4]
      if (unbiasingConstantUsed) {
        UCL <- c4 * sigma + k * sigma * c5
        LCL <- c4 * sigma - k * sigma * c5
      } else {
        UCL <- sigma + k * (c5 / c4) * sigma
        LCL <- sigma - k * (c5 / c4) * sigma
      }
      LCL <- max(0, LCL) # LCL in S-chart must be >= 0
    }
    UCLvector <- c(UCLvector, UCL)
    LCLvector <- c(LCLvector, LCL)
  }
  return(list(LCL = LCLvector, UCL = UCLvector))
}

