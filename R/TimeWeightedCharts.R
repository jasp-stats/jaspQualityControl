TimeWeightedCharts <- function(jaspResults, dataset, options){
  variables <- options$variables
  numeric_variables  <- variables[variables != ""]

  dataset         <- .readDataSetToEnd(columns.as.numeric = numeric_variables, exclude.na.listwise = numeric_variables)
  #Checking for errors in the dataset
  .hasErrors(dataset, type = c('infinity', 'missingValues'),
             all.target = options$variables, exitAnalysisIfErrors = TRUE)

  #Cusum chart
  if(options$Cumulativechart && is.null(jaspResults[["Cumulativechart"]])){
    jaspResults[["CusumPlot"]] <- createJaspPlot(title = "Cumulative sum chart", width = 900, height = 400)
    jaspResults[["CusumPlot"]]$dependOn(c("Cumulativechart", "variables"))
    CusumPlotPlot <- jaspResults[["CusumPlot"]]
    CusumPlotPlot$plotObject <- .Cusumchart(dataset = dataset, options = options)
  }
#EWMA chart
  if(options$Exponentialchart && is.null(jaspResults[["Exponentialchart"]])){
    jaspResults[["EWMAPlot"]] <- createJaspPlot(title = "Exponentially weighted moving average chart", width = 900, height = 400)
    jaspResults[["EWMAPlot"]]$dependOn(c("Exponentialchart", "variables"))
    EWMAPlot <- jaspResults[["EWMAPlot"]]
    EWMAPlot$plotObject <- .EWMA(dataset = dataset, options = options)
  }
}
.Cusumchart <- function(dataset, options){
  ready <- options$variables != ""
  if (!ready)
    return()

  data1 <- dataset[, options$variables]
  sixsigma <- qcc::cusum(data1, decision.interval = options$h, se.shift = options$k, plot = FALSE)
  subgroups = c(1:length(sixsigma$pos))
  data_plot <- data.frame(y_neg = sixsigma$neg , y_pos = sixsigma$pos, x = subgroups)

  center <- 0
  UCL <- sixsigma$decision.interval
  LCL <- c(-UCL)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL - 1 , UCL + 1))
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups)
  xLimits <- c(0,max(xBreaks) + 5)
  dfLabel <- data.frame(
    x = max(xLimits - 1),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", center),
      gettextf("UCL = %g", UCL),
      gettextf("LCL = %g", LCL)
    )
  )
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x))  +
    jaspGraphs::geom_line(ggplot2::aes(y = y_neg), col = "black") +
    jaspGraphs::geom_line(ggplot2::aes(y = y_pos), col = "green")+
    jaspGraphs::geom_point(ggplot2::aes(y = y_neg), size = 4, fill = ifelse(data_plot$y_neg < LCL, 'red', 'black')) +
    jaspGraphs::geom_point(ggplot2::aes(y = y_pos), size = 4, fill = ifelse(data_plot$y_pos > UCL, 'red', 'green')) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name = gettext("Cumulative sum") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = gettext('Subgroups'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(p)
}
.EWMA <- function(dataset, options){
  ready <- options$variables != ""
  if (!ready)
    return()

  data1 <- dataset[, options$variables]
  sixsigma <- qcc::ewma(data1, lambda = options$lambda, nsigmas= options$sigma, plot = FALSE)
  subgroups = c(1:length(sixsigma$sizes))
  center <- sixsigma$center
  UCL =  sixsigma$limits[,2]
  LCL = sixsigma$limits[,1]
  data_plot <- data.frame(y = sixsigma$y, x = subgroups, UCL = UCL, LCL = LCL)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(LCL) - 0.001 , max(UCL) +  0.001))
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups)
  xLimits <- c(1,max(xBreaks) + 5)
  dfLabel <- data.frame(
    x = max(xLimits - 1),
    y = c(center),
    l = c(
      gettextf("CL = %g", center)
    )
  )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = x, y = y)) +
    geom_line(ggplot2::aes(x = x, y = UCL, color = "red")) +
    geom_line(ggplot2::aes(x = x, y = LCL, color = "red")) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Standard Deviation") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Subgroup'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$y > UCL | data_plot$y < LCL, 'red', 'gray')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(p)
}
