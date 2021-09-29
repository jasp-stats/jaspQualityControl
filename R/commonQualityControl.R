#############################################################
## Common functions for preparatory work ####################
#############################################################

# Common function to read in data set
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

#############################################################
## Common functions for plots ###############################
#############################################################

# Function to create the x-bar and r-chart section
.qcXbarAndRContainer <- function(options, dataset, ready, jaspResults, measurements, subgroups, subgroups_ticks) {

  if (!is.null(jaspResults[["controlCharts"]]))
    return()

  container <- createJaspContainer(title = gettext("Control Chart"))
  container$dependOn(options = c("controlChartsType", "variables", "subgroups", "variablesLong", "pcSubgroupSize"))
  container$position <- 1
  jaspResults[["controlCharts"]] <- container

  matrixPlot <- createJaspPlot(title = "X-bar & R Chart", width = 1000, height = 550)
  container[["plot"]] <- matrixPlot

  if (!ready)
    return()

  if (length(measurements) < 2) {
    matrixPlot$setError(gettext("You must enter at least 2 measurements to get this output."))
    return()
  }

  if(subgroups != "")
    subgroups <- subgroups_ticks

  plotMat <- matrix(list(), 2, 1)
  plotMat[[1,1]] <- .XbarchartNoId(dataset = dataset[measurements], options = options, manualXaxis = subgroups, warningLimits = FALSE)$p
  plotMat[[2,1]] <- .RchartNoId(dataset = dataset[measurements], options = options, manualXaxis = subgroups, warningLimits = FALSE)$p
  matrixPlot$plotObject <- cowplot::plot_grid(plotlist = plotMat, ncol = 1, nrow = 2)
}

# Function to create X-bar chart
.XbarchartNoId <- function(dataset, options, manualLimits = "", warningLimits = TRUE, manualSubgroups = "", yAxis = TRUE, plotLimitLabels = TRUE, yAxisLab = "Sample average", xAxisLab = "Subgroup",
                           manualDataYaxis = "", manualXaxis = "", title = "", smallLabels = FALSE, Phase2 = FALSE, target = NULL, sd = NULL, NoWarningSignals = FALSE) {
  data <- dataset[, unlist(lapply(dataset, is.numeric))]
  if(Phase2)
    sixsigma <- qcc::qcc(data, type ='xbar', plot=FALSE, center = as.numeric(target), std.dev = as.numeric(sd))
  else
    sixsigma <- qcc::qcc(data, type ='xbar', plot=FALSE)
  subgroups = c(1:length(sixsigma$statistics))
  means = sixsigma$statistics
  data_plot <- data.frame(subgroups = subgroups, means = means)
  sd1 <- sixsigma$std.dev
  if (manualLimits != "") {
    LCL <- manualLimits[1]
    center <- manualLimits[2]
    UCL <- manualLimits[3]
  }else{
    center <- sixsigma$center
    UCL <- max(sixsigma$limits)
    LCL <- min(sixsigma$limits)
  }
  if (manualDataYaxis != ""){
    manualMeans <- rowMeans(manualDataYaxis)
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL, manualMeans))
  }else{
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL, means))
  }
  yLimits <- range(yBreaks)
  if (length(subgroups) <= 10){
    nxBreaks <- length(subgroups)
  }else{
    nxBreaks <- 5
  }
  prettyxBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups, n = nxBreaks)
  prettyxBreaks[prettyxBreaks == 0] <- 1
  xBreaks <- c(prettyxBreaks[1], prettyxBreaks[-1])

  dfLabel <- data.frame(
    x = max(xBreaks) * 1.2,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, decimalplaces(data[1,1]) + 1)),
      gettextf("UCL = %g",   round(UCL, decimalplaces(data[1,1]) + 2)),
      gettextf("LCL = %g",   round(LCL, decimalplaces(data[1,1]) + 2))
    )
  )

  xLimits <- range(c(xBreaks, dfLabel$x))

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = means)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green', size = 1) +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5)
  if (warningLimits) {
    warn.limits <- c(qcc::limits.xbar(sixsigma$center, sixsigma$std.dev, sixsigma$sizes, 1),
                     qcc::limits.xbar(sixsigma$center, sixsigma$std.dev, sixsigma$sizes, 2))
    p <- p + ggplot2::geom_hline(yintercept = warn.limits, color = "orange", linetype = "dashed", size = 1)
  }
  if (yAxis){
    p <- p + ggplot2::scale_y_continuous(name = gettext(yAxisLab) ,limits = yLimits, breaks = yBreaks)
  }else{
    p <- p + ggplot2::scale_y_continuous(name = ggplot2::element_blank(), limits = yLimits, breaks = yBreaks, labels = NULL) +
      ggplot2::theme(axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
  }

  if(smallLabels){
    labelSize <- 2
  }else{
    labelSize <- 4
  }

  if (plotLimitLabels)
    p <- p + ggrepel::geom_label_repel(data = dfLabel, ggplot2::aes(x = x, y = y, label = l), direction = "both", size = labelSize)

  p <- p + ggplot2::scale_x_continuous(name = gettext(xAxisLab), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(fontsize = jaspGraphs::setGraphOption("fontsize", 15))

  if (warningLimits) {
    warn.limits <- c(qcc::limits.xbar(sixsigma$center, sixsigma$std.dev, sixsigma$sizes, 1),
                     qcc::limits.xbar(sixsigma$center, sixsigma$std.dev, sixsigma$sizes, 2))
    p <- p + ggplot2::geom_hline(yintercept = warn.limits, color = "orange", linetype = "dashed", size = 1)
  }

  if (Phase2)
    p <- p + jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma)$red_points, "red", "blue"))
  else if (NoWarningSignals)
    p <- p + jaspGraphs::geom_point(size = 4, fill = "blue")
  else
    p <- p + jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma, allsix = TRUE)$red_points, "red", "blue"))

  if (manualXaxis != "") {
    xLabels <- factor(manualXaxis, levels = manualXaxis)
    p <- p + ggplot2::scale_x_continuous(name = xAxisLab, breaks = 1:length(manualXaxis), labels = xLabels)
  }

  if (title != "")
    p <- p + ggplot2::ggtitle(title)

  if (manualXaxis != "")
    return(list(p = p, sixsigma = sixsigma, xLabels = levels(xLabels)))
  else return(list(p = p, sixsigma = sixsigma))
}

# Function to create R chart
.RchartNoId <- function(dataset, options, manualLimits = "", warningLimits = TRUE, manualSubgroups = "", yAxis = TRUE,  plotLimitLabels = TRUE,
                        yAxisLab = "Sample range", xAxisLab = "Subgroup", manualDataYaxis = "", manualXaxis = "", title = "", smallLabels = FALSE, OnlyOutofLimit = FALSE) {
  #Arrange data and compute
  data <- dataset[, unlist(lapply(dataset, is.numeric))]
  sixsigma <- qcc::qcc(data, type ='R', plot = FALSE)
  range = sixsigma$statistics
  if (manualSubgroups != ""){
    subgroups <- manualSubgroups
  }else{
    subgroups = c(1:length(sixsigma$statistics))
  }
  data_plot <- data.frame(subgroups = subgroups, range = range)
  if (manualLimits != "") {
    LCL <- manualLimits[1]
    center <- manualLimits[2]
    UCL <- manualLimits[3]
  }else{
    center <- sixsigma$center
    UCL <- max(sixsigma$limits)
    LCL <- min(sixsigma$limits)
  }
  if (manualDataYaxis != ""){
    manualRange <- apply(manualDataYaxis, 1, function(x) max(x) - min(x))
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL, manualRange))
  }else{
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL - (0.10 * abs(LCL)), range, UCL + (0.1 * UCL)), min.n = 4)
  }
  yLimits <- range(yBreaks)
  if (length(subgroups) <= 15){
    nxBreaks <- length(subgroups)
  }else{
    nxBreaks <- 5
  }
  prettyxBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups, n = nxBreaks)
  prettyxBreaks[prettyxBreaks == 0] <- 1
  xBreaks <- c(prettyxBreaks[1], prettyxBreaks[-1])
  dfLabel <- data.frame(
    x = max(xBreaks)  * 1.2,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, decimalplaces(data[1,1]) + 1)),
      gettextf("UCL = %g",   round(UCL, decimalplaces(data[1,1]) + 2)),
      gettextf("LCL = %g",   round(LCL, decimalplaces(data[1,1]) + 2))
    )
  )
  xLimits <- range(c(xBreaks, dfLabel$x))

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
    labelSize <- 2
  }else{
    labelSize <- 4
  }
  if (plotLimitLabels)
    p <- p + ggrepel::geom_label_repel(data = dfLabel, ggplot2::aes(x = x, y = y, label = l), direction = "both", size = labelSize)

  p <- p + ggplot2::scale_x_continuous(name= gettext(xAxisLab) ,breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma)$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(fontsize = jaspGraphs::setGraphOption("fontsize", 15))

  if (manualXaxis != "") {
    xLabels <- factor(manualXaxis, levels = manualXaxis)
    p <- p + ggplot2::scale_x_continuous(name = xAxisLab, breaks = 1:length(manualXaxis), labels = xLabels)
  }

  if (OnlyOutofLimit){
    p <- p + jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$range > UCL | data_plot$range < LCL, 'red', 'blue'))
  }

  if (title != "")
    p <- p + ggplot2::ggtitle(title)

  if (manualXaxis != "")
    return(list(p = p, sixsigma = sixsigma, xLabels = levels(xLabels)))
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
  warnings <- Rspc::EvaluateRules(x = data$statistics, type = chart, lcl = data$limits[1,1], ucl = data$limits[1,2], cl = data$center, parRules = pars,
                                  whichRules = c(1:3,5,7:8))

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
.NelsonTable <- function(dataset, options, sixsigma, type = "xbar", Phase2 = FALSE, name = "X-bar", xLabels = NULL) {

  table <- createJaspTable(title = gettextf("Test result for %s chart", name))

  if (Phase2 == "TRUE" || type == "xbar.one") {

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

    if (name == "P" || name == "NP" || name == "C" || name == "U" || name == "Laney P'" || name == "Laney U'")
      Test <- NelsonLaws(data = sixsigma, xLabels = xLabels, chart = "c")
    else
      Test <- NelsonLaws(data = sixsigma, xLabels = xLabels)

    if (length(Test$Rules$R1) > 0)
      table$addColumnInfo(name = "test1",              title = gettextf("Test 1: Beyond limit")               , type = "integer")

    if (length(Test$Rules$R2) > 0)
      table$addColumnInfo(name = "test2",              title = gettextf("Test 2: Shift")                   , type = "integer")

    if (length(Test$Rules$R3) > 0)
      table$addColumnInfo(name = "test3",              title = gettextf("Test 3: Trend")                        , type = "integer")


    table$setData(list(
      "test1" = c(Test$Rules$R1),
      "test2" = c(Test$Rules$R2),
      "test3" = c(Test$Rules$R3)
    ))
  }

  table$showSpecifiedColumnsOnly <- TRUE
  table$addFootnote(message = gettext("Numbers index data points where test violations occur."))
  return(table)
}

decimalplaces <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}
