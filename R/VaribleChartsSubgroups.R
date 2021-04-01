VaribleChartsSubgroups <- function(jaspResults, dataset, options){
  variables <- unlist(options$variables)
  time <- options$time
  makeTime <- time != ""
  numberMissingSplitBy <- 0

  if (is.null(dataset)) {
    if (makeTime) {
      dataset         <- .readDataSetToEnd(columns.as.numeric = variables, columns.as.factor = time)
      dataset.factors <- .readDataSetToEnd(columns=variables, columns.as.factor=time)
    } else {
      dataset         <- .readDataSetToEnd(columns.as.numeric=variables)
      dataset.factors <- .readDataSetToEnd(columns=variables)
    }
  }
  if (makeTime && length(variables) > 0) {
    # For the time-variable we first convert the original factor to a character so that the order of input is kept!
    dataset[[.v(time)]] <- as.character(dataset[[.v(time)]])
  }
  #Checking for errors in the dataset
  .hasErrors(dataset, type = c('observations', 'infinity', 'missingValues'),
             all.target = options$variables,
             observations.amount = c(' < 2'), exitAnalysisIfErrors = TRUE)

#X bar chart
  if(options$Xbarchart && is.null(jaspResults[["Xbarchart"]]) &&  length(options$variables) > 1){
    jaspResults[["XbarPlot"]] <- createJaspPlot(title =  gettext("X-bar & R Control Chart"), width = 700, height = 350)
    jaspResults[["XbarPlot"]]$dependOn(c("Xbarchart", "variables"))
    XbarPlot <- jaspResults[["XbarPlot"]]
    XbarPlot$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(.RchartNoId(dataset = dataset, options = options, time = ifelse(makeTime, TRUE, FALSE)), .XbarchartNoId(dataset = dataset, options = options, time = ifelse(makeTime, TRUE, FALSE))), layout = matrix(1:2, 2), removeXYlabels= "x")
  }

#S Chart
  if(options$Schart && is.null(jaspResults[["Schart"]]) &&  length(options$variables) > 1){
    jaspResults[["SPlot"]] <- createJaspPlot(title = gettext("Xbar & s Control Chart"), width = 700, height = 350)
    jaspResults[["SPlot"]]$dependOn(c("Schart", "variables"))
    SPlot<- jaspResults[["SPlot"]]
    SPlot$plotObject <- .XbarSchart(dataset = dataset, options = options, time = ifelse(makeTime, TRUE, FALSE))
  }
}

#Functions for control charts
.XbarSchart <- function(dataset, options, time = FALSE){
  data1 <- dataset[, options$variables]
  Stdv <- apply(data1, 1, function(x) sd(x))
  subgroups <- c(1:length(Stdv))
  data_plot <- data.frame(subgroups = subgroups, Stdv = Stdv)

  sixsigma <- qcc::qcc(data1, type ='S', plot=FALSE)
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL + 0.1))
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups)
  xLimits <- c(0,max(xBreaks) + 5)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL))
  yLimits <- range(yBreaks)
  dfLabel <- data.frame(
    x = max(xLimits - 1),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )

  p1 <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = Stdv)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Standard Deviation") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = gettext('Subgroup'), breaks = xBreaks) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$Stdv > UCL | data_plot$Stdv < LCL, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  p2 <- .XbarchartNoId(dataset = dataset, options = options)

  if (time){
    xLabels <- factor(dataset[[.v(options$time)]], levels = unique(as.character(dataset[[.v(options$time)]])))
    p1 <- p1 + ggplot2::scale_x_continuous(name = gettext('Subgroup'), breaks = 1:length(subgroups), labels = xLabels)
    p2 <- .XbarchartNoId(dataset = dataset, options = options) + ggplot2::scale_x_continuous(name = gettext('Subgroup'), breaks = 1:length(subgroups), labels = xLabels)
  }
  p3 <- jaspGraphs::ggMatrixPlot(plotList = list(p1, p2), layout = matrix(1:2, 2), removeXYlabels= "x")

  return(p3)
}
