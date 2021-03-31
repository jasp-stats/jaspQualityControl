VaribleChartsSubgroups <- function(jaspResults, dataset, options){
  variables <- options$variables
  numeric_variables <- variables
  numeric_variables  <- numeric_variables[numeric_variables != ""]

  dataset         <- .readDataSetToEnd(columns.as.numeric = numeric_variables, exclude.na.listwise = numeric_variables)
  #Checking for errors in the dataset
  .hasErrors(dataset, type = c('observations', 'infinity', 'missingValues'),
             all.target = options$variables,
             observations.amount = c(' < 2'), exitAnalysisIfErrors = TRUE)

  jaspResults[["intro"]] <- createJaspHtml(gettext("Select one of the control charts from the interface."))
  jaspResults[["intro"]]$position <- 0
#X bar chart
  if(options$Xbarchart && is.null(jaspResults[["Xbarchart"]]) &&  length(options$variables) > 1){
    jaspResults[["XbarPlot"]] <- createJaspPlot(title =  gettext("X bar & R chart"), width = 700, height = 350)
    jaspResults[["XbarPlot"]]$dependOn(c("Xbarchart", "variables"))
    XbarPlot <- jaspResults[["XbarPlot"]]
    XbarPlot$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(.RchartNoId(dataset = dataset, options = options), .XbarchartNoId(dataset = dataset, options = options)), layout = matrix(1:2, 2), removeXYlabels= "x")
  }

#S Chart
  if(options$Schart && is.null(jaspResults[["Schart"]]) &&  length(options$variables) > 1){
    jaspResults[["SPlot"]] <- createJaspPlot(title = gettext("Xbar & S Chart"), width = 700, height = 350)
    jaspResults[["SPlot"]]$dependOn(c("Schart", "variables"))
    SPlot<- jaspResults[["SPlot"]]
    SPlot$plotObject <- .XbarSchart(dataset = dataset, options = options)
  }
}

#Functions for control charts
.XbarSchart <- function(dataset, options){
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
    ggplot2::geom_hline(yintercept =  center, color = 'green', size = 1) +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Standard Deviation") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Subgroup'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$Stdv > UCL | data_plot$Stdv < LCL, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  p2 <- .XbarchartNoId(dataset = dataset, options = options)

  p3 <- jaspGraphs::ggMatrixPlot(plotList = list(p1, p2), layout = matrix(1:2, 2), removeXYlabels= "x")

  return(p3)
}
