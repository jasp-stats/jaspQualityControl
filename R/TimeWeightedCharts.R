#' @export
timeWeightedCharts <- function(jaspResults, dataset, options) {
  variables <- options[["measurements"]]
  numeric_variables  <- variables[variables != ""]
  dataset         <- .readDataSetToEnd(columns.as.numeric = numeric_variables, exclude.na.listwise = numeric_variables)
  #Checking for errors in the dataset
  .hasErrors(dataset, type = c('infinity', 'missingValues'),
             all.target = options[["measurements"]], exitAnalysisIfErrors = TRUE)

  ready <- length(variables) > 0 

  if (length(variables) > 0) {
    #Cusum chart
    if (options[["cumulativeSumChart"]] && is.null(jaspResults[["CusumPlot"]])) {
      jaspResults[["CusumPlot"]] <- createJaspPlot(title = gettext("Cumulative sum chart"), width = 1200, height = 500)
      jaspResults[["CusumPlot"]]$dependOn(c("cumulativeSumChart", "measurements"))
      jaspResults[["CusumPlot"]]$plotObject <- .Cusumchart(dataset = dataset, options = options, ready = ready)
    }
    #EWMA chart
    if (options[["ExponentiallyWeightedMovingAverageChart"]] && is.null(jaspResults[["EWMAPlot"]])) {
      jaspResults[["EWMAPlot"]] <- createJaspPlot(title = gettext("Exponentially weighted moving average chart"), width = 1200, height = 500)
      jaspResults[["EWMAPlot"]]$dependOn(c("ExponentiallyWeightedMovingAverageChart", "measurements"))
      jaspResults[["EWMAPlot"]]$plotObject <- .EWMA(dataset = dataset, options = options, ready = ready)
    }
    #G chart
    if (options[["gChart"]] && is.null(jaspResults[["GPlot"]])) {
      jaspResults[["GPlot"]] <- createJaspPlot(title = gettext("G chart"), width = 1200, height = 500)
      jaspResults[["GPlot"]]$dependOn(c("gChart", "measurements"))
      jaspResults[["GPlot"]]$plotObject <- .Gchart(dataset = dataset, options = options, ready = ready)$p
    }
    #T chart
    # if (options[["tChart"]] && is.null(jaspResults[["TPlot"]])) {
    #   jaspResults[["TPlot"]] <- createJaspPlot(title = gettext("T chart"), width = 1200, height = 500)
    #   jaspResults[["TPlot"]]$dependOn(c("tChart", "measurements"))
    #   jaspResults[["TPlot"]]$plotObject <- .Tchart(dataset = dataset, options = options, ready = ready)$p
    # }
  }
}

.Cusumchart <- function(dataset, options, ready) {
  if (!ready)
    return()
  
  data1 <- dataset[, options$variables]
  sixsigma <- qcc::cusum(data1, decision.interval = options$h, se.shift = options$k, plot = FALSE)
  subgroups <- c(1:length(sixsigma$pos))
  data_plot <- data.frame(y_neg = sixsigma$neg , y_pos = sixsigma$pos, x = subgroups)
  center <- 0
  UCL <- sixsigma$decision.interval
  LCL <- -UCL
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL, data_plot$y_neg,  data_plot$y_pos))
  yLimits <- range(yBreaks)
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks) + 2.5)
  dfLabel <- data.frame(
    x = max(xLimits - 1),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 4)),
      gettextf("UCL = %g",   round(UCL, 5)),
      gettextf("LCL = %g",   round(LCL, 5))
    )
  )
  
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x))  +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name = gettext("Cumulative sum") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = gettext('Subgroups'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(ggplot2::aes(y = y_neg), col = "blue") +
    jaspGraphs::geom_line(ggplot2::aes(y = y_pos), col = "blue")+
    jaspGraphs::geom_point(ggplot2::aes(y = y_neg), size = 4, fill = ifelse(data_plot$y_neg < LCL, 'red', 'blue')) +
    jaspGraphs::geom_point(ggplot2::aes(y = y_pos), size = 4, fill = ifelse(data_plot$y_pos > UCL, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return(p)
}
.EWMA <- function(dataset, options, ready) {
  if (!ready)
    return()

  data1 <- dataset[, options[["measurements"]]]
  sixsigma <- qcc::ewma(data1, center = options[["ExponentiallyWeightedMovingAverageChartCenter"]] , lambda = options[["ExponentiallyWeightedMovingAverageChartLambda"]],
                        std.dev = options[["ExponentiallyWeightedMovingAverageChartSd"]], nsigmas = options[["ExponentiallyWeightedMovingAverageChartSigmaControlLimits"]], plot = FALSE)
  subgroups <- 1:length(sixsigma$sizes)
  center <- sixsigma$center
  UCL <-  sixsigma$limits[,2]
  LCL <- sixsigma$limits[,1]
  data_plot <- data.frame(y = sixsigma$y, x = subgroups, UCL = UCL, LCL = LCL)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL, data_plot$y))
  yLimits <- range(yBreaks)
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks-0.5) * 1.15)
  UCL.label <- center + options[["ExponentiallyWeightedMovingAverageChartSigmaControlLimits"]] * sqrt(options[["ExponentiallyWeightedMovingAverageChartLambda"]] / (2-options[["ExponentiallyWeightedMovingAverageChartLambda"]])) * options[["ExponentiallyWeightedMovingAverageChartSd"]]
  LCL.label <- center - options[["ExponentiallyWeightedMovingAverageChartSigmaControlLimits"]] * sqrt(options[["ExponentiallyWeightedMovingAverageChartLambda"]] / (2-options[["ExponentiallyWeightedMovingAverageChartLambda"]])) * options[["ExponentiallyWeightedMovingAverageChartSd"]]
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center, UCL.label, LCL.label),
    l = c(
      gettextf("CL = %g", round(center, decimals + 1)),
      gettextf("UCL = %g",   round(UCL.label, decimals + 2)),
      gettextf("LCL = %g",   round(LCL.label, decimals + 2))
    )
  )
  
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_step(ggplot2::aes(x = x, y = UCL, color = "red"),linetype = "dashed", size = 1.5) +
    ggplot2::geom_step(ggplot2::aes(x = x, y = LCL, color = "red"), linetype = "dashed", size = 1.5) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("EWMA") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Subgroup'), breaks = xBreaks, limits = range(xLimits)) +
    ggplot2::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$y > UCL | data_plot$y < LCL, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return(p)
}
.Gchart <- function(dataset, options){
  if (!ready)
    return()

  data1 <- dataset[, options[["measurements"]]]
  subgroups <- c(1:length(data1))
  data_plot <- data.frame(x =  subgroups, y = data1)
  center = mean(data1)
  UCL = center+3*sqrt(center*(center + 1))
  LCL = center-3*sqrt(center*(center + 1))
  LCL <- ifelse(LCL < 0 , 0, LCL)
  sixsigma <- list(statistics = data1, limits = data.frame(LCL, UCL), center = center)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL, data_plot$y))
  yLimits <- range(yBreaks)
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks) + 2.5)
  dfLabel <- data.frame(
    x = max(xLimits - 1),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 4)),
      gettextf("UCL = %g",   round(UCL, 5)),
      gettextf("LCL = %g",   round(LCL, 5))
    )
  )
  
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = UCL, color = "red"),linetype = "dashed", size = 1.5) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = LCL, color = "red"), linetype = "dashed", size = 1.5) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Counts") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Subgroup'), breaks = xBreaks, limits = range(xLimits)) +
    ggplot2::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$y > UCL | data_plot$y < LCL, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return(list(p = p, sixsigma = sixsigma))
}
.Tchart <- function(dataset, options){
  if (!ready)
    return()

  data1 <- dataset[, options[["measurements"]]]
  subgroups <- c(1:length(data1))
  data_plot <- data.frame(x = subgroups , y = data1^0.2777)
  data2 <- data.frame(process = data1)
  MR_T <- qcc::qcc(matrix(cbind(data2$process[1:length(data2$process)-1], data2$process[2:length(data2$process)]), ncol=2)
                   , type="R", plot = FALSE)$statistics
  center = mean(data_plot$y)^3.6
  UCL = (mean(data_plot$y) + 2.66 * mean(MR_T))^3.6
  LCL = (mean(data_plot$y, na.rm = TRUE) - 2.66 * mean(MR_T, na.rm =))^3.6
  LCL <- ifelse(LCL < 0 , 0, LCL)
  sixsigma <- list(statistics = data1, limits = data.frame(LCL, UCL), center = center)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL, data_plot$y))
  yLimits <- range(yBreaks)
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks) + 2.5)
  dfLabel <- data.frame(
    x = max(xLimits - 1),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 4)),
      gettextf("UCL = %g",   round(UCL, 5)),
      gettextf("LCL = %g",   round(LCL, 5))
    )
  )
  
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = UCL, color = "red"),linetype = "dashed", linewidth = 1.5) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = LCL, color = "red"), linetype = "dashed", linewidth = 1.5) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l), inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Counts") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Subgroup'), breaks = xBreaks, limits = range(xLimits)) +
    ggplot2::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$y > UCL | data_plot$y < LCL, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return(list(p = p, sixsigma = sixsigma))
}
