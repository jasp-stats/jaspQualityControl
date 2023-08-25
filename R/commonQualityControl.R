
# imports from packages
#' @importFrom jaspBase createJaspContainer createJaspPlot createJaspState createJaspTable createJaspHtml
#' .extractErrorMessage .hasErrors isTryError .readDataSetToEnd


#############################################################
## Common functions for plots ###############################
#############################################################
.qcReadData <- function(dataset, options, type) {
  if (type == "capabilityStudy") {
    if (is.null(dataset)) {
      if (options[["subgroup"]] != "") {
        dataset <- .readDataSetToEnd(columns.as.numeric = options[["measurementsWideFormat"]], columns.as.factor = options[["subgroup"]])
      } else {
        dataset <- .readDataSetToEnd(columns.as.numeric = options[["measurementsWideFormat"]])
      }
    }
  }
  return(dataset)
}

NelsonLaws <- function(data, allsix = FALSE, chart = "i", xLabels = NULL) {

  # Adjust Rules to SKF
  pars <- Rspc::SetParameters()
  pars$Rule2$nPoints = 7
  pars$Rule3$nPoints = 7
  pars$Rule3$convention = "minitab"
  pars$Rule4$convention = "minitab"

  #Evaluate all rules
  if (chart == "p") {
    n = length(data$statistics)
    warnings <- data.frame(x = rep(1,n), Rule1 = rep(1,n), Rule2 = rep(1,n), Rule3 = rep(1,n))
    for( i in 1:length(data$statistics)) {
      warningsRaw <- Rspc::EvaluateRules(x = c(data$statistics[i],0), type = "c", lcl = data$limits[i,1], ucl = data$limits[i,2], cl = data$center, parRules = pars,
                                         whichRules = c(1:3,5,7:8))[1,]
      warnings[i,] <- warningsRaw
    }
  } else {
      lcl <- ifelse(is.nan(data$limits[1,1]) || is.na(data$limits[1,1]), NA, data$limits[1,1])
      ucl <- ifelse(is.nan(data$limits[1,2])  || is.na(data$limits[1,2]), NA, data$limits[1,2])
      warnings <- Rspc::EvaluateRules(x = data$statistics, type = chart, lcl = lcl, ucl = ucl, cl = data$center[1], parRules = pars,
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
    return(violationsList)

  if (!Phase2 || type == "xbar.one") {
    Test <- NelsonLaws(data = sixsigma, allsix = TRUE, xLabels = xLabels)
    violationsList[["test1"]] <- Test$Rules$R1
    violationsList[["test2"]] <- Test$Rules$R2
    violationsList[["test3"]] <- Test$Rules$R3
    violationsList[["test4"]] <- Test$Rules$R4
    violationsList[["test5"]] <- Test$Rules$R5
    violationsList[["test6"]] <- Test$Rules$R6
  } else {
    if (name == "np" || name == "c" || name == "u" || name == "Laney p'" || name == "Laney u'")
      Test <- NelsonLaws(data = sixsigma, xLabels = xLabels, chart = "c")
    else if (name == "P")
      Test <- NelsonLaws(data = sixsigma, xLabels = xLabels, chart = "p")
    else
      Test <- NelsonLaws(data = sixsigma, xLabels = xLabels)

    if (type == "Range") {
      Test$Rules$R1 <- Test$Rules$R1 + 1
      Test$Rules$R2 <- Test$Rules$R2 + 1
      Test$Rules$R3 <- Test$Rules$R3 + 1
    }
    violationsList[["test1"]] <- Test$Rules$R1
    violationsList[["test2"]] <- Test$Rules$R2
    violationsList[["test3"]] <- Test$Rules$R3
  }
  return(violationsList)
}

.IMRchart <- function(dataset, options, variable = "", measurements = "", cowPlot = FALSE, manualXaxis = "", Wide = FALSE,
                          stages = "") {

  ppPlot <- createJaspPlot(width = 900, height = 650)
  tableI <- createJaspTable(title = gettextf("Test results for individuals chart"))
  tableR <- createJaspTable(title = gettextf("Test results for range chart"))

  if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))

    # Error conditions for stages
    if(any(table(dataset[[stages]]) < options[["xmrChartMovingRangeLength"]])) {
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
      ppPlot$dependOn(optionContainsValue = list(variables = variable))
      data <- data.frame(process = dataForPlot[[variable]])
      k <- options[["xmrChartMovingRangeLength"]]
      # qcc has no moving range plot, so we need to arrange data in a matrix with the observation + k future observation per row and calculate the range chart
      mrMatrix <- matrix(data$process[1:(length(data$process) - (k - 1))])   # remove last k - 1 elements
      for (j in 2:k) {
        mrMatrix <- cbind(mrMatrix, matrix(data$process[j:(length(data$process) - (k - j))]))   # remove first i and last (k - i) elements
      }
      meanMovingRange <- mean(.rowRanges(mrMatrix)$ranges)
      d2 <- KnownControlStats.RS(k, 3)[[1]][1]
      sd <- meanMovingRange/d2
      sixsigma_I <- qcc::qcc(data$process, type ='xbar.one', plot = FALSE, std.dev = sd)
      sixsigma_R <- qcc::qcc(mrMatrix, type = "R", plot = FALSE, std.dev = sd)
    } else {
      data <- as.vector((t(dataForPlot[measurements])))
      k <- options[["xmrChartMovingRangeLength"]]
      sd <- qcc::sd.xbar.one(data, k = k)
      sixsigma_I <- qcc::qcc(data, type ='xbar.one', plot = FALSE, std.dev = sd)
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
    decimals1 <- .numDecimals
    decimals2 <- .numDecimals
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
    tableIListLengths <- sapply(tableIList[[i]], length)
    if (any(tableIListLengths > 0)) {
      tableIList[[i]] <- tableIList[[i]][tableIListLengths > 0]
      tableIList[[i]][["stage"]] <- as.character(stage)
      tableIList[[i]] <- lapply(tableIList[[i]], "length<-", max(lengths(tableIList[[i]]))) # this fills up all elements of the list with NAs so all elements are the same size
    }
    tableRList[[i]] <- .NelsonTableList(dataset = dataset, options = options, type = "Range", sixsigma = sixsigma_R, xLabels = subgroupsR)
    tableRListLengths <- sapply(tableRList[[i]], length)
    if (any(tableRListLengths > 0)) {
      tableRList[[i]] <- tableRList[[i]][tableRListLengths > 0]
      tableRList[[i]][["stage"]] <- as.character(stage)
      tableRList[[i]] <- lapply(tableRList[[i]], "length<-", max(lengths(tableRList[[i]]))) # this fills up all elements of the list with NAs so all elements are the same size
    }
  }

  # filling up tables for individuals and moving range charts
  tableIListVectorized <- unlist(tableIList, recursive = FALSE)
  tableILongestVector <- max(sapply(tableIListVectorized, length))
  if (tableILongestVector > 0) {
    tableIListCombined <- tapply(tableIListVectorized, names(tableIListVectorized), function(x) unlist(x, FALSE, FALSE))
    if (nStages > 1)
      tableI$addColumnInfo(name = "stage",              title = gettextf("Stage")               , type = "string")
    if (length(tableIListCombined[["test1"]]) > 0)
      tableI$addColumnInfo(name = "test1",              title = gettextf("Test 1: Beyond limit")               , type = "integer")
    if (length(tableIListCombined[["test2"]]) > 0)
      tableI$addColumnInfo(name = "test2",              title = gettextf("Test 2: Shift")                   , type = "integer")
    if (length(tableIListCombined[["test3"]]) > 0)
      tableI$addColumnInfo(name = "test3",              title = gettextf("Test 3: Trend")                        , type = "integer")
    if (length(tableIListCombined[["test4"]]) > 0)
      tableI$addColumnInfo(name = "test4",              title = gettextf("Test 4: Increasing variation")         , type = "integer")
    if (length(tableIListCombined[["test5"]]) > 0)
      tableI$addColumnInfo(name = "test5",              title = gettextf("Test 5: Reducing variation")           , type = "integer")
    if (length(tableIListCombined[["test6"]]) > 0)
      tableI$addColumnInfo(name = "test6",              title = gettextf("Test 6: Bimodal distribution")         , type = "integer")
    tableI$setData(list(
      "stage" = tableIListCombined[["stage"]],
      "test1" = tableIListCombined[["test1"]],
      "test2" = tableIListCombined[["test2"]],
      "test3" = tableIListCombined[["test3"]],
      "test4" = tableIListCombined[["test4"]],
      "test5" = tableIListCombined[["test5"]],
      "test6" = tableIListCombined[["test6"]]
    ))
    tableI$showSpecifiedColumnsOnly <- TRUE
  }
  tableRListVectorized <- unlist(tableRList, recursive = FALSE)
  tableRLongestVector <- max(sapply(tableRListVectorized, length))
  if (tableRLongestVector > 0) {
    tableRListCombined <- tapply(tableRListVectorized, names(tableRListVectorized), function(x) unlist(x, FALSE, FALSE))
    if (nStages > 1)
      tableR$addColumnInfo(name = "stage",              title = gettextf("Stage")               , type = "string")
    if (length(tableRListCombined[["test1"]]) > 0)
      tableR$addColumnInfo(name = "test1",              title = gettextf("Test 1: Beyond limit")               , type = "integer")
    if (length(tableRListCombined[["test2"]]) > 0)
      tableR$addColumnInfo(name = "test2",              title = gettextf("Test 2: Shift")                   , type = "integer")
    if (length(tableRListCombined[["test3"]]) > 0)
      tableR$addColumnInfo(name = "test3",              title = gettextf("Test 3: Trend")                        , type = "integer")
    if (length(tableRListCombined[["test4"]]) > 0)
      tableR$addColumnInfo(name = "test4",              title = gettextf("Test 4: Increasing variation")         , type = "integer")
    if (length(tableRListCombined[["test5"]]) > 0)
      tableR$addColumnInfo(name = "test5",              title = gettextf("Test 5: Reducing variation")           , type = "integer")
    if (length(tableRListCombined[["test6"]]) > 0)
      tableR$addColumnInfo(name = "test6",              title = gettextf("Test 6: Bimodal distribution")         , type = "integer")
    tableR$setData(list(
      "stage" = tableRListCombined[["stage"]],
      "test1" = tableRListCombined[["test1"]],
      "test2" = tableRListCombined[["test2"]],
      "test3" = tableRListCombined[["test3"]],
      "test4" = tableRListCombined[["test4"]],
      "test5" = tableRListCombined[["test5"]],
      "test6" = tableRListCombined[["test6"]]
    ))
    tableR$showSpecifiedColumnsOnly <- TRUE
  }

  # Calculations that apply to the whole plot
  yBreaks1 <- jaspGraphs::getPrettyAxisBreaks(c(dataPlotI$process, dataPlotI$LCL, dataPlotI$UCL, dataPlotI$center))
  yBreaks2 <- jaspGraphs::getPrettyAxisBreaks(c(dataPlotR$movingRange, dataPlotR$LCL, dataPlotR$UCL, dataPlotR$center))
  if (options[["manualTicksXAxis"]]) {
    nxBreaks <- options[["manualTicksXAxisValue"]]
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
  ## individual plots
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

  ## moving range plots
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

.sdXbar <- function(df, type = c("s", "r")) {
  type <- match.arg(type)
  
  # exclude groups with single observation from calculation
  rowRemovalIndex <- which(apply(df, 1, function(x) sum(!is.na(x)) < 2)) #get index of rows with less than 2 obs.
   if (length(rowRemovalIndex) != 0)
    df <- df[-rowRemovalIndex, ]
  
  if (type == "r"){
    rowRanges <- .rowRanges(df)$ranges
    n <- .rowRanges(df)$n
    if (sum(n) < 2) {
      sdWithin <- 0
    } else {
      d2s <- sapply(n, function(x) return(KnownControlStats.RS(x, 0)$constants[1]))
      sdWithin <- sum((n - 1) * rowRanges / d2s) / sum(n - 1) # Burr (1969), equation 11
    }
  } else if (type == "s") {
    rowSd <- apply(df, 1, sd, na.rm = TRUE)
    if (sum(!is.na(rowSd)) == 0) {
      sdWithin <- 0
    } else {
    sdWithin <- mean(rowSd, na.rm = TRUE)
    }
  }
  return(sdWithin)
}

.rowRanges <- function(df) {
  nrow <- nrow(df)
  ranges <- c()
  n <- c()
  for (i in seq_len(nrow)) {
    rowVector <- df[i,]
    ranges <- c(ranges, max(rowVector, na.rm = TRUE) - min(rowVector, na.rm = TRUE))
    n <- c(n, sum(!is.na(rowVector)))
  }
  return(list(ranges = ranges, n = n))
}

KnownControlStats.RS <- function(N, sigma) {

  Data.d3 <- data.frame(
    n = 1:25,
    d3 = c(NA, 0.8525 ,0.8884, 0.8798, 0.8641, 0.8480, 0.8332, 0.8198, 0.8078, 0.7971, 0.7873, 0.7785, 0.7704, 0.7630,
           0.7562, 0.7499, 0.7441, 0.7386, 0.7335, 0.7287, 0.7242, 0.7199, 0.7159, 0.7121, 0.7084))

  Data.d2 <- data.frame(
    n = 1:50,
    d2 = c(NA, 1.128, 1.693 ,2.059, 2.326, 2.534, 2.704, 2.847, 2.970, 3.078, 3.173, 3.258, 3.336, 3.407, 3.472, 3.532,
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
  
  if (N > 1) {
    c4 <- sqrt(2/(N-1)) * gamma(N/2) / gamma((N-1)/2)
    c5 <- sqrt(1 - c4^2)
  } else {
    c4 <- 0
    c5 <- 0
  }
  
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
      if (n[i] > 1) {
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
      } else {
        LCL <- NA
        UCL <- NA
      }
    }
    UCLvector <- c(UCLvector, UCL)
    LCLvector <- c(LCLvector, LCL)
  }
  return(list(LCL = LCLvector, UCL = UCLvector))
}




###################
### FOR TESTING ###
###   |    |    ###
###   V    V    ###
###################


# Xbar
#dataset <- read.csv("C:/Users/Jonee/Google Drive/SKF Six Sigma/JASP Data Library/2_1_VariablesChartsForSubgroups/SubgroupChartWideFormat.csv")
#dataset <- dataset[2:6]
#  options <- list()
# 
# .controlChartPlotFunction(dataset, plotType = "s")
# 
# 
# dataset <- read.csv("C:/Users/Jonee/Google Drive/SKF Six Sigma/Datasets/ControlChartError.csv")
#dataset <- dataset[c(3,7,8)]
# 

# dataset <- read.csv("C:/Users/Jonee/Google Drive/SKF Six Sigma/JASP Data Library/2_2_VariablesChartsForIndividuals/IndividualChartStages.csv")
# xLabels <- dataset$Month
# dataset <- dataset[c(1,3)]
# .controlChartPlotFunction(dataset, plotType = "I", stages = "Stage", xAxisLabels = xLabels)
# .controlChartPlotFunction(dataset, plotType = "MR", stages = "Stage", xAxisLabels = xLabels)

###################
###################
###################


.controlChartPlotFunction <- function(dataset, plotType = c("xBar", "R", "I", "MR", "s"), stages = "",
                                      xBarSdType = c("r", "s"), phase2 = FALSE, phase2Mu = "", phase2Sd = "", limitsPerSubgroup = FALSE,
                                      warningLimits = FALSE, xAxisLabels = "", movingRangeLength = 2) {
  tableTitle <- switch (plotType,
    "xBar" = "x-bar",
    "R" = "range",
    "I" = "individuals",
    "MR" = "moving range",
    "s" = "s"
  )
  table <- createJaspTable(title = gettextf("Test results for %1$s chart", tableTitle))
  
  if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
    # Error conditions for stages
    if(plotType == "MR" && any(table(dataset[[stages]]) < movingRangeLength)) {
      ppPlot$setError(gettext("Moving range length is larger than one of the stages."))
      return(list(p = ppPlot))
    } 
  } else {
    nStages <- 1
    dataset[["stage"]] <- 1
    stages <- "stage"
  }
  
  ### Calculate plot values per stage and combine into single dataframe ###
  plotData <- data.frame(matrix(ncol = 4, nrow = 0))
  clData <- data.frame(matrix(ncol = 5, nrow = 0))
  tableList <- list()
  colnames(plotData) <- c("plotStatistic", "subgroup", "stage", "dotColor")
  colnames(clData) <- c("subgroup", "stage", "LCL", "UCL", "center")
  if (warningLimits) {
    warningLimitsDf <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(warningLimitsDf) <- c("UWL1", "LWL1", "UWL2", "LWL2")
    clData <- cbind(clData, warningLimitsDf) 
  }
  dfLabel <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(dfLabel) <- c("x", "y", "label")
  seperationLines <- c()
  dfStageLabels <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(dfStageLabels) <- c("x", "y", "label")
  for (i in seq_len(nStages)) {
    stage <- unique(dataset[[stages]])[i]
    dataCurrentStage <- dataset[which(dataset[[stages]] == stage), ][!names(dataset) %in% stages]
    if (plotType == "I" || plotType == "MR" ) {
      k <- movingRangeLength
      # qcc has no moving range plot, so we need to arrange data in a matrix with the observation + k future observation per row and calculate the range chart
      dataCurrentStageVector <- unlist(dataCurrentStage)
      mrMatrix <- matrix(dataCurrentStageVector[1:(length(dataCurrentStageVector) - (k - 1))])   # remove last k - 1 elements
      for (j in 2:k) {
        mrMatrix <- cbind(mrMatrix, matrix(dataCurrentStageVector[j:(length(dataCurrentStageVector) - (k - j))]))   # remove first i and last (k - i) elements
      }
      meanMovingRange <- mean(.rowRanges(mrMatrix)$ranges)
      d2 <- KnownControlStats.RS(k, 3)[[1]][1]
      sd <- meanMovingRange/d2
      if (plotType == "I") {
        qccObject <- qcc::qcc(dataCurrentStage, type ='xbar.one', plot = FALSE, std.dev = sd)
        plotStatistic <- qccObject$statistics
      } else if (plotType == "MR") {
        qccObject <- qcc::qcc(mrMatrix, type = "R", plot = FALSE, std.dev = sd)
        plotStatistic <- c(NA, qccObject$statistics)
      }
      limits <- qccObject$limits
      LCL <- limits[1]
      UCL <- limits[2]
      center <- qccObject$center
    } else if (plotType == "R") { 
      n <- apply(dataCurrentStage, 1, function(x) return(sum(!is.na(x)))) # returns the number of non NA values per row
      if (!limitsPerSubgroup) # if control limits are not calculated per group they are based on largest group size
        n <- max(n)
      # manually calculate mean and sd as the package gives wrong results with NAs
      if(phase2) {
        sigma <- phase2Sd
      } else {
        sigma <- .sdXbar(df = dataCurrentStage, type = "r")
      }
      d2 <- sapply(n, function(x) KnownControlStats.RS(x, 0)$constants[1])
      mu <- sigma * d2
      qccObject <- qcc::qcc(dataCurrentStage, type ='R', plot = FALSE, center = mu, std.dev = sigma, sizes = ncol(dataCurrentStage))
      plotStatistic <- qccObject$statistics
      
      limits <- .controlLimits(mu, sigma, n = n, type = "r")
      center <- mu
      UCL <- limits$UCL
      LCL <- limits$LCL
    } else if (plotType == "xBar") {
      #xBarSdType <- match.arg(xBarSdType)
      if (phase2) {
          mu <- as.numeric(phase2Mu)
          sigma <- as.numeric(phase2Sd)
      } else {
        # manually calculate mean and sd as the package gives wrong results with NAs
        mu <- mean(unlist(dataCurrentStage), na.rm = TRUE)
        sigma <- .sdXbar(df = dataCurrentStage, type = xBarSdType)
      }
      qccObject <- qcc::qcc(dataCurrentStage, type ='xbar', plot = FALSE, center = mu, sizes = ncol(dataCurrentStage), std.dev = sigma)
      plotStatistic <- qccObject$statistics
      
      #calculate group sizes
      n <- apply(dataCurrentStage, 1, function(x) return(sum(!is.na(x)))) # returns the number of non NA values per row
      if (!limitsPerSubgroup) # if control limits are not calculated per group they are based on largest group size
        n <- max(n)
      
      limits <- .controlLimits(mu, sigma, n = n, type = "xbar")
      center <- mu
      UCL <- limits$UCL
      LCL <- limits$LCL
      
      # upper and lower warning limits at 1 sd and 2 sd
      WL1 <- .controlLimits(mu, sigma, n = n, type = "xbar", k = 1)
      WL2 <- .controlLimits(mu, sigma, n = n, type = "xbar", k = 2)
      UWL1 <- WL1$UCL
      LWL1 <- WL1$LCL
      UWL2 <- WL2$UCL
      LWL2 <- WL2$LCL
    } else if (plotType == "s") { 
      if(phase2) {
        sigma <- phase2Sd
      } else {
        sigma <- .sdXbar(df = dataCurrentStage, type = "s")
      }
      qccObject <- qcc::qcc(dataCurrentStage, type ='S', plot = FALSE, center = sigma, sizes = ncol(dataCurrentStage))
      plotStatistic <- qccObject$statistics
     
      
      n <- apply(dataCurrentStage, 1, function(x) return(sum(!is.na(x)))) # returns the number of non NA values per row
      if (!limitsPerSubgroup) # if control limits are not calculated per group they are based on largest group size
        n <- max(n)
      
      limits <- .controlLimits(sigma = sigma, n = n, type = "s")
      center <- sigma
      UCL <- limits$UCL
      LCL <- limits$LCL
    }
    if (i != 1) {
      if (plotType == "MR") {
        subgroups <- seq(max(plotData$subgroup) + 1, max(plotData$subgroup) + length(qccObject$statistics) + 1)
      } else {
        subgroups <- seq(max(plotData$subgroup) + 1, max(plotData$subgroup) + length(qccObject$statistics))
      }
      seperationLines <- c(seperationLines, max(plotData$subgroup) + .5)
      dfStageLabels <- rbind(dfStageLabels, data.frame(x = max(plotData$subgroup) + length(subgroups)/2, y = NA, label = stage))  # the y value will be filled in later
    } else {
      if (plotType == "MR") {
        subgroups <- seq_len(length(qccObject$statistics) + 1)
      } else {
        subgroups <- seq_len(length(qccObject$statistics))
      }
      if (nStages > 1)
        dfStageLabels <- rbind(dfStageLabels, data.frame(x = max(subgroups)/2, y = NA, label = stage))  # the y value will be filled in later
    }
    
    if (length(plotStatistic) > 1) {
      if (plotType == "MR") {
        dotColor <- ifelse(c(NA, NelsonLaws(qccObject)$red_points), 'red', 'blue')
      } else {
        dotColor <- ifelse(NelsonLaws(qccObject, allsix = plotType == "I")$red_points, 'red', 'blue')
      }
    } else {
      dotColor <- ifelse(plotStatistic > UCL | plotStatistic < LCL, "red", "blue")
      dotColor[is.na(dotColor)] <- "blue"
    }
    # if more than half of the dots are violations, do not show red dots.
    nOutOfLimits <- sum(dotColor[!is.na(dotColor)] == "red")
    if (nOutOfLimits > length(qccObject$statistics)/2)
      dotColor <- "blue"
    
    
    stagePlotData <- data.frame("plotStatistic" = plotStatistic,
                            "subgroup" = subgroups,
                            "stage" = stage,
                            "dotColor" = dotColor)
    stageClData <- data.frame("subgroup" = subgroups,
                              "stage" = stage,
                              "LCL" = LCL,
                              "UCL" = UCL,
                              "center" = center)
    
    if (warningLimits) {
      stageClData <- cbind(stageClData,
                         data.frame("UWL1" = UWL1,
                                    "LWL1" = LWL1,
                                    "UWL2" = UWL2,
                                    "LWL2" = LWL2))
    }
    
    # offset to align geom_step lines with observations
    stageClData <- rbind(stageClData, stageClData[nrow(stageClData),])
    stageClData[["subgroup"]][nrow(stageClData)] <- stageClData[["subgroup"]][nrow(stageClData)] + 1
    stageClData[["subgroup"]] <- stageClData[["subgroup"]] - 0.5
    
    
    plotData <- rbind(plotData, stagePlotData)
    clData <- rbind(clData, stageClData)
    decimals <- .numDecimals
    # even if there are multiple different centers, LCL and UCL, only the last one is shown on the label
    lastCenter <- center[length(center)]
    lastLCL <- LCL[length(LCL)]
    lastUCL <- UCL[length(UCL)]
    dfLabel <- rbind(dfLabel, data.frame(x = max(subgroups) + .5,
                                         y = c(lastCenter, lastLCL, lastUCL),
                                         label = c(
                                           gettextf("CL = %g",  round(lastCenter, decimals)),
                                           gettextf("UCL = %g", round(lastLCL, decimals)),
                                           gettextf("LCL = %g", round(lastUCL, decimals))
                                         )))
    tableList[[i]] <- .NelsonTableList(qccObject = qccObject, type = plotType, subgroups = subgroups)
    tableListLengths <- sapply(tableList[[i]], length)
    if (any(tableListLengths > 0)) {
      tableList[[i]][["stage"]] <- as.character(stage)
      tableList[[i]] <- lapply(tableList[[i]], "length<-", max(lengths(tableList[[i]]))) # this fills up all elements of the list with NAs so all elements are the same size
    }
  }
  
  # filling up JASP table
  tableListVectorized <- unlist(tableList, recursive = FALSE)
  tableLongestVector <- max(sapply(tableListVectorized, length))
  if (tableLongestVector > 0) {
    tableListCombined <- tapply(tableListVectorized, names(tableListVectorized), function(x) unlist(x, FALSE, FALSE))
    if (nStages > 1)
      table$addColumnInfo(name = "stage",              title = gettextf("Stage"),                          type = "string")
    if (length(tableListCombined[["test1"]][!is.na(tableListCombined[["test1"]])]) > 0)
      table$addColumnInfo(name = "test1",              title = gettextf("Test 1: Beyond limit"),           type = "integer")
    if (length(tableListCombined[["test2"]][!is.na(tableListCombined[["test2"]])]) > 0)
      table$addColumnInfo(name = "test2",              title = gettextf("Test 2: Shift"),                  type = "integer")
    if (length(tableListCombined[["test3"]][!is.na(tableListCombined[["test3"]])]) > 0)
      table$addColumnInfo(name = "test3",              title = gettextf("Test 3: Trend"),                  type = "integer")
    if (plotType == "I") {
    if (length(tableListCombined[["test4"]][!is.na(tableListCombined[["test4"]])]) > 0)
      table$addColumnInfo(name = "test4",              title = gettextf("Test 4: Increasing variation"),   type = "integer")
    if (length(tableListCombined[["test5"]][!is.na(tableListCombined[["test5"]])]) > 0)
      table$addColumnInfo(name = "test5",              title = gettextf("Test 5: Reducing variation"),     type = "integer")
    if (length(tableListCombined[["test6"]][!is.na(tableListCombined[["test6"]])]) > 0)
      table$addColumnInfo(name = "test6",              title = gettextf("Test 6: Bimodal distribution"),   type = "integer")
    }
    tableData <- list(
      "stage" = tableListCombined[["stage"]],
      "test1" = tableListCombined[["test1"]],
      "test2" = tableListCombined[["test2"]],
      "test3" = tableListCombined[["test3"]]
    )
    if (plotType == "I") {
      tableData[["test4"]] <- tableListCombined[["test4"]]
      tableData[["test5"]] <- tableListCombined[["test5"]]
      tableData[["test6"]] <- tableListCombined[["test6"]]
    }
    table$setData(tableData)
    table$showSpecifiedColumnsOnly <- TRUE
  }
  
  # Calculations that apply to the whole plot
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(plotData$plotStatistic, clData$LCL, clData$UCL, clData$center))
  xBreaks <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(plotData$subgroup))) # we only want integers on the x-axis
  
  if (xBreaks[1] == 0)  # never start counting at 0 on x axis
    xBreaks[1] <- 1
  xLimits <- c(0.5, max(xBreaks) * 1.15)
  
  if (!identical(xAxisLabels, "")) {
    if (max(xBreaks) > length(xAxisLabels)) # sometimes pretty creates breaks that go beyond the labels that are given, this must be avoided else it will display an NA on this tick
      xBreaks[length(xBreaks)] <- length(xAxisLabels)
    xLabels <- xAxisLabels[xBreaks]
  } else {
    xLabels <- xBreaks
  }
  
  xTitle <- switch (plotType,
                    "xBar" = "Sample average",
                    "R" = "Sample range",
                    "I" = "Individual value",
                    "MR" = "Moving range",
                    "s" = "Sample std. dev."
  )
  if (nStages > 1)
    dfStageLabels$y <- max(yBreaks)
  
  # Create plot
  plotObject <- ggplot2::ggplot(clData, ggplot2::aes(x = subgroup, y = plotStatistic, group = stage)) +
    ggplot2::geom_vline(xintercept = seperationLines) +
    ggplot2::geom_step(mapping = ggplot2::aes(x = subgroup, y = center) , col = "green", linewidth = 1) +
    ggplot2::geom_step(mapping = ggplot2::aes(x = subgroup, y = UCL) , col = "red", linewidth = 1.5, linetype = "dashed") +
    ggplot2::geom_step(mapping = ggplot2::aes(x = subgroup, y = LCL) , col = "red", linewidth = 1.5, linetype = "dashed")
  if (nStages > 1)
    plotObject <- plotObject + ggplot2::geom_text(data = dfStageLabels, mapping = ggplot2::aes(x = x, y = y, label = label), size = 6, fontface="bold")
  if (warningLimits) {
    plotObject <- plotObject + ggplot2::geom_step(data = clData, mapping = ggplot2::aes(x = subgroup, y = UWL1), col = "orange",
                                                  linewidth = 1, linetype = "dashed") +
      ggplot2::geom_step(data = clData, mapping = ggplot2::aes(x = subgroup, y = LWL1), col = "orange",
                         linewidth = 1, linetype = "dashed") +
      ggplot2::geom_step(data = clData, mapping = ggplot2::aes(x = subgroup, y = UWL2), col = "orange",
                         linewidth = 1, linetype = "dashed") +
      ggplot2::geom_step(data = clData, mapping = ggplot2::aes(x = subgroup, y = LWL2), col = "orange",
                         linewidth = 1, linetype = "dashed")
  }
  plotObject <- plotObject + ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = label),
                                                 inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name = xTitle, breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(name = gettext("Subgroup"), breaks = xBreaks, limits = xLimits, labels = xLabels) +
    jaspGraphs::geom_line(plotData, mapping = ggplot2::aes(x = subgroup, y = plotStatistic, group = stage), color = "blue") +
    jaspGraphs::geom_point(plotData, mapping = ggplot2::aes(x = subgroup, y = plotStatistic, group = stage), 
                           size = 4, fill = plotData$dotColor, inherit.aes = TRUE) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return(list(plotObject = plotObject, table = table, qccObject = qccObject, plotData = plotData))
}


.NelsonTableList <- function(qccObject, type = "xBar", phase2 = TRUE, subgroups = NULL) {
  violationsList <- list("test1" = NULL, "test2" = NULL, "test3" = NULL)
  
  if (length(qccObject$statistics) == 1) # no need for table with only 1 group
    return(violationsList)
  
  if (!phase2 || type == "I") {
    Test <- NelsonLaws(data = qccObject, allsix = TRUE, xLabels = subgroups)
    violationsList[["test4"]] <- Test$Rules$R4
    violationsList[["test5"]] <- Test$Rules$R5
    violationsList[["test6"]] <- Test$Rules$R6
  } else if (type == "np" || type == "c" || type == "u" || type == "Laney p'" || type == "Laney u'") {
    Test <- NelsonLaws(data = qccObject, xLabels = subgroups, chart = "c")
  } else if (type == "P") {
    Test <- NelsonLaws(data = qccObject, xLabels = subgroups, chart = "p")
  } else {
    Test <- NelsonLaws(data = qccObject, xLabels = subgroups)
  }
  
  violationsList[["test1"]] <- Test$Rules$R1
  violationsList[["test2"]] <- Test$Rules$R2
  violationsList[["test3"]] <- Test$Rules$R3

  return(violationsList)
}
