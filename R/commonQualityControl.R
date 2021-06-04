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

# Common function to check if options are ready
.qcOptionsReady <- function(options, type) {
  if (type == "capabilityStudy") {
    ready <- length(unlist(options[["variables"]])) > 0
  }
  return(ready)
}

#############################################################
## Common functions for plots ###############################
#############################################################

# Function to create the x-bar and r-chart section
.qcXbarAndRContainer <- function(options, dataset, ready, jaspResults, measurements, subgroups) {

  if (!options[["controlCharts"]] || !is.null(jaspResults[["controlCharts"]]))
    return()

  container <- createJaspContainer(title = gettext("Control Chart"))
  container$dependOn(options = c("controlCharts", "variables", "subgroups"))
  container$position <- 1
  jaspResults[["controlCharts"]] <- container

  matrixPlot <- createJaspPlot(title = "X-bar & R Chart", width = 700, height = 500)
  container[["plot"]] <- matrixPlot

  if (!ready)
    return()

  if (length(options[["variables"]]) < 2) {
    xplot$setError(gettext("You must enter at least 2 measurements to get this output."))
    rplot$setError(gettext("You must enter at least 2 measurements to get this output."))
    return()
  }

  if(subgroups != "")
    subgroups <- dataset[[subgroups]]

  plotMat <- matrix(list(), 2, 1)
  plotMat[[1,1]] <- .XbarchartNoId(dataset = dataset[measurements], options = options, manualXaxis = subgroups)
  plotMat[[2,1]] <- .RchartNoId(dataset = dataset[measurements], options = options, manualXaxis = subgroups)
  matrixPlot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)
}

# Function to create X-bar chart
.XbarchartNoId <- function(dataset, options, manualLimits = "", warningLimits = TRUE, time = FALSE, manualSubgroups = "", yAxis = TRUE, plotLimitLabels = TRUE, yAxisLab = "Subgroup mean", xAxisLab = "Subgroup",
                           manualDataYaxis = "", manualXaxis = "", title = "", smallLabels = FALSE) {
  data <- dataset[, unlist(lapply(dataset, is.numeric))]
  means <- rowMeans(data)
  if (manualSubgroups != ""){
    subgroups <- manualSubgroups
  }else{
    subgroups <- c(1:length(means))
  }
  data_plot <- data.frame(subgroups = subgroups, means = means)
  sixsigma <- qcc::qcc(data, type ='xbar', plot=FALSE)
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
  if (length(subgroups) <= 15){
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
      gettextf("CL = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
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
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(means, UCL = UCL, LCL = LCL, center = center)$red_points, "red", "blue")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(fontsize = jaspGraphs::setGraphOption("fontsize", 15))

  if (time) {
    xLabels <- factor(dataset[[.v(options$time)]], levels = unique(as.character(dataset[[.v(options$time)]])))
    p <- p + ggplot2::scale_x_continuous(name = gettext('Time'), breaks = 1:length(subgroups), labels = xLabels)
  }

  if (manualXaxis != "") {
    xLabels <- factor(manualXaxis, levels = manualXaxis)
    p <- p + ggplot2::scale_x_continuous(name = xAxisLab, breaks = 1:length(manualXaxis), labels = xLabels)
  }

  if (title != "")
    p <- p + ggplot2::ggtitle(title)
  return(p)
}

# Function to create R chart
.RchartNoId <- function(dataset, options, manualLimits = "", warningLimits = TRUE, time = FALSE, manualSubgroups = "", yAxis = TRUE,  plotLimitLabels = TRUE,
                        yAxisLab = "Subgroup range", xAxisLab = "Subgroup", manualDataYaxis = "", manualXaxis = "", title = "", smallLabels = FALSE) {
  #Arrange data and compute
  data <- dataset[, unlist(lapply(dataset, is.numeric))]
  range <- apply(data, 1, function(x) max(x) - min(x))
  if (manualSubgroups != ""){
    subgroups <- manualSubgroups
  }else{
    subgroups <- c(1:length(range))
  }
  data_plot <- data.frame(subgroups = subgroups, range = range)
  sixsigma <- qcc::qcc(data, type= 'R', plot = FALSE)
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
      gettextf("CL = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
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
    p <- p + ggrepel::geom_label_repel(data = dfLabel, ggplot2::aes(x = x, y = y, label = l), vjust="top",hjust="inward", size = labelSize)

  p <- p + ggplot2::scale_x_continuous(name= gettext(xAxisLab) ,breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(range, UCL = UCL, LCL = LCL, center = center)$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(fontsize = jaspGraphs::setGraphOption("fontsize", 15))

  if (time) {
    xLabels <- factor(dataset[[.v(options$time)]], levels = unique(as.character(dataset[[.v(options$time)]])))
    p <- p + ggplot2::scale_x_continuous(name = gettext('Time'), breaks = 1:length(subgroups), labels = xLabels)
  }

  if (manualXaxis != "") {
    xLabels <- factor(manualXaxis, levels = manualXaxis)
    p <- p + ggplot2::scale_x_continuous(name = xAxisLab, breaks = 1:length(manualXaxis), labels = xLabels)
  }

  if (title != "")
    p <- p + ggplot2::ggtitle(title)

  return(p)
}

NelsonLaws <- function(data, UCL, LCL, center, which = c(1:3,5,7:8), chart = "i") {

  # Adjust Rules to SKF
  pars <- Rspc::SetParameters()
  pars$Rule2$nPoints = 7
  pars$Rule3$nPoints = 7
  pars$Rule3$convention = "minitab"
  pars$Rule4$convention = "minitab"

  #Evalute all rules
  warnings <- Rspc::EvaluateRules(x = data, type = chart, lcl = LCL, ucl = UCL, cl = center, parRules = pars,
                                  whichRules = which)

  if (chart == "i") {
    Rules <- list(R1 = which(warnings[,2] == 1),
                  R2 = which(warnings[,3] == 1),
                  R3 = which(warnings[,4] == 1),
                  R4 = which(warnings[,5] == 1),
                  R5 = which(warnings[,6] == 1),
                  R6 = which(warnings[,7] == 1))
  }
  else {
    Rules <- list(R1 = which(warnings[,2] == 1),
                  R2 = which(warnings[,3] == 1),
                  R3 = which(warnings[,4] == 1),
                  R4 = which(warnings[,5] == 1))
  }


  red_points = apply(warnings[,-1], 1, sum) > 0
  return(list(red_points = red_points, Rules = Rules))
}
.NelsonTable <- function(dataset, options, type = "R") {

  table <- createJaspTable(title = gettextf("Test Results for %s Chart", toupper(type)))


  if (type == "R" || type == "xbar" || type == "S") {
    sixsigma <- qcc::qcc(dataset, type = type, plot = FALSE)
    Test <- c(NelsonLaws(data = sixsigma$statistics, UCL = sixsigma$limits[2], LCL = sixsigma$limits[1], center = sixsigma$center))

    if (length(Test$Rules$R1) > 0)
      table$addColumnInfo(name = "test1",              title = gettextf("Test 1: Sporadic issue.")               , type = "integer")

    if (length(Test$Rules$R2) > 0)
      table$addColumnInfo(name = "test2",              title = gettextf("Test 2: Mean shift.")                   , type = "integer")

    if (length(Test$Rules$R3) > 0)
      table$addColumnInfo(name = "test3",              title = gettextf("Test 3: Trend.")                        , type = "integer")

    if (length(Test$Rules$R4) > 0)
      table$addColumnInfo(name = "test4",              title = gettextf("Test 4: Increasing variation.")         , type = "integer")

    if (length(Test$Rules$R5) > 0)
      table$addColumnInfo(name = "test5",              title = gettextf("Test 5: Reducing variation.")           , type = "integer")

    if (length(Test$Rules$R6) > 0)
      table$addColumnInfo(name = "test6",              title = gettextf("Test 6: Bimodal distribution.")         , type = "integer")


    table$setData(list(
      "test1" = c(Test$Rules$R1),
      "test2" = c(Test$Rules$R2),
      "test3" = c(Test$Rules$R3),
      "test4" = c(Test$Rules$R4),
      "test5" = c(Test$Rules$R5),
      "test6" = c(Test$Rules$R6)
    ))

  }

  else{
    sixsigma <-  with(dataset, qcc::qcc(dataset[, options$D], dataset[, options$total], type = type, plot = FALSE))
    Test <- c(NelsonLaws(data = sixsigma$statistics, UCL = max(sixsigma$limits), LCL = min(sixsigma$limits),
                         center = sixsigma$center, chart = "c", which = c(1:4)))

    if (length(Test$Rules$R1) > 0)
      table$addColumnInfo(name = "test1",              title = gettextf("Test 1: Sporadic issue.")               , type = "integer")

    if (length(Test$Rules$R2) > 0)
      table$addColumnInfo(name = "test2",              title = gettextf("Test 2: Mean shift.")                   , type = "integer")

    if (length(Test$Rules$R3) > 0)
      table$addColumnInfo(name = "test3",              title = gettextf("Test 3: Trend.")                        , type = "integer")


    table$setData(list(
      "test1" = c(Test$Rules$R1),
      "test2" = c(Test$Rules$R2),
      "test3" = c(Test$Rules$R3)
    ))
  }

  table$showSpecifiedColumnsOnly <- TRUE

  return(table)
}

