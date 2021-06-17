variablesChartsIndividuals <- function(jaspResults, dataset, options) {
  variables <- options$variables
  numeric_variables  <- variables[variables != ""]
  dataset         <- .readDataSetToEnd(columns.as.numeric = numeric_variables, exclude.na.listwise = numeric_variables)
  #Checking for errors in the dataset
  .hasErrors(dataset, type = c('observations', 'infinity', 'missingValues'),
             all.target = options$variables,
             observations.amount = c(' < 2'), exitAnalysisIfErrors = TRUE)

  #ImR chart
  if (options$ImRchart) {
    if(is.null(jaspResults[["Ichart"]])){
      jaspResults[["Ichart"]] <- createJaspContainer(position = 1)
      jaspResults[["Ichart"]]$dependOn(c("ImRchart", "variables"))
      Iplot <- jaspResults[["Ichart"]]

      for (var in variables) {

        ALL <- createJaspContainer(gettextf("Charts and Tests for %s", var))

        ALL[["Plot"]] <- .IMRchart(dataset = dataset, options = options, variable = var)$p

        ALL[["Table1"]] <- .NelsonTable(dataset = dataset, options = options, type = "xbar.one", name = "Individual", sixsigma = .IMRchart(dataset = dataset, options = options, variable = var)$sixsigma_I)

        ALL[["Table2"]] <- .NelsonTable(dataset = dataset, options = options, name = "R", sixsigma = .IMRchart(dataset = dataset, options = options, variable = var)$sixsigma_R)

        Iplot[[var]] <- ALL
      }
    }
  }
}

.IMRchart <- function(dataset, options, variable, cowPlot = FALSE) {

  title <- gettextf("Charts for: %s", variable)
  ppPlot <- createJaspPlot(width = 1200, height = 500, title = title)
  ppPlot$dependOn(optionContainsValue = list(variables = variable))

  #Individual chart
  #data
  data <- data.frame(process = dataset[[variable]])
  subgroups <- c(1:length(data$process))
  sixsigma_I <- qcc::qcc(data$process, type ='xbar.one', plot=FALSE)
  center <- sixsigma_I$center
  UCL <- max(sixsigma_I$limits)
  LCL <- min(sixsigma_I$limits)
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
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, data$process, UCL))

  p1 <- ggplot2::ggplot(data, ggplot2::aes(x = subgroups, y = process)) +
    ggplot2::geom_hline(yintercept = center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name = gettext("Value"), breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(name = gettext('Observation'), breaks = xBreaks, limits = xLimits) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma_I, allsix = TRUE)$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  #Moving range chart
  #data
  data2 <- data.frame(process = dataset[[.v(variable)]])
  xmr.raw.r <- matrix(cbind(data2$process[1:length(data2$process)-1], data2$process[2:length(data2$process)]), ncol=2)
  sixsigma_R <- qcc::qcc(xmr.raw.r, type="R", plot = FALSE)
  data_plot <- data.frame(subgroups = c(1:length(sixsigma_R$statistics)), data2 = sixsigma_R$statistics)
  center <- sixsigma_R$center
  UCL <- max(sixsigma_R$limits)
  LCL <- min(sixsigma_R$limits)
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
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, data_plot$data2, UCL))

  p2 <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = data2)) +
    ggplot2::geom_hline(yintercept = center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red",linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name = gettext("Moving Range"), breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(name = gettext('Observation'), breaks = xBreaks, limits = xLimits) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma_R)$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plotMat <- matrix(list(), 2, 1)
  plotMat[[1,1]] <- p1
  plotMat[[2,1]] <- p2

  if(!cowPlot){
    ppPlot$plotObject <-  jaspGraphs::ggMatrixPlot(plotList = plotMat, removeXYlabels= "x")
  }else{
    ppPlot$plotObject <- cowplot::plot_grid(plotlist = plotMat, ncol = 1, nrow = 2)
  }

  return(list(p = ppPlot, sixsigma_I = sixsigma_I, sixsigma_R = sixsigma_R))
}

