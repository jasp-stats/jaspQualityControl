VaribleChartsIndviduals <- function(jaspResults, dataset, options){
  variables <- options$variables
  numeric_variables  <- variables[variables != ""]
  dataset         <- .readDataSetToEnd(columns.as.numeric = numeric_variables, exclude.na.listwise = numeric_variables)
  #Checking for errors in the dataset
  .hasErrors(dataset, type = c('observations', 'infinity', 'missingValues'),
             all.target = options$variables,
             observations.amount = c(' < 2'), exitAnalysisIfErrors = TRUE)

  jaspResults[["intro"]] <- createJaspHtml(gettext("Select one of the control charts from the interface."))
  jaspResults[["intro"]]$position <- 0

  #ImR chart
  if(options$ImRchart){
    if(is.null(jaspResults[["Ichart"]])){
      jaspResults[["Ichart"]] <- createJaspContainer(gettext("Charts per variable"))
    }

    Iplot <- jaspResults[["Ichart"]]

    for(var in variables){
      Iplot[[var]] <- .IMRchart(dataset = dataset, options = options, variable = var)
    }
  }
}
.IMRchart <- function(dataset, options, variable){

  title <- gettextf("Variable: %s", variable )
  ppPlot <- createJaspPlot(width = 700, height = 350, title = title)
  ppPlot$dependOn(optionContainsValue = list(variables = variable))

  #Individual chart
  #data
  data <- data.frame(process = dataset[[.v(variable)]])
  subgroups <- c(1:length(data$process))
  sixsigma <- qcc::qcc(data$process, type ='xbar.one', plot=FALSE)
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL - 1, UCL + 1))
  yLimits <- range(yBreaks)
  xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  xLimits <- c(0,max(xBreaks) + 5)
  dfLabel <- data.frame(
    x = max(xLimits - 1),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )

  p1 <- ggplot2::ggplot(data, ggplot2::aes(x = subgroups, y = process)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Value") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Observation'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data$process > UCL | data$process < LCL, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  #Moving rage chart
  #data
  data2 <- data.frame(process = dataset[[.v(variable)]])
  xmr.raw.r <- matrix(cbind(data2$process[1:length(data2$process)-1], data2$process[2:length(data2$process)]), ncol=2)
  sixsigma <- qcc::qcc(xmr.raw.r, type="R", plot = FALSE)
  data_plot <- data.frame(subgroups = c(1:length(sixsigma$statistics)), data2 = sixsigma$statistics)
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL + .1))
  yLimits <- range(yBreaks)
  xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  xLimits <- c(0,max(xBreaks) + 5)
  dfLabel <- data.frame(
    x = max(xLimits - 1),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )

  p2 <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = data2)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red",linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Moving Range") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Observation'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$data > UCL | data_plot$data < LCL, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  ppPlot$plotObject <-  jaspGraphs::ggMatrixPlot(plotList = list(p1, p2), layout = matrix(1:2, 2), removeXYlabels= "x")
  return(ppPlot)
}

