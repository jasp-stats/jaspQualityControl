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
      jaspResults[["Ichart"]]$dependOn(c("ImRchart", "variables", "ncol"))
      Iplot <- jaspResults[["Ichart"]]

      for (var in variables) {

        ALL <- createJaspContainer(gettextf("X-mR control chart"))

        ALL[["Plot"]] <- .IMRchart(dataset = dataset, options = options, variable = var)$p

        ALL[["Table1"]] <- .NelsonTable(dataset = dataset, options = options, type = "xbar.one", name = gettextf("%s for Individual", var), sixsigma = .IMRchart(dataset = dataset, options = options, variable = var)$sixsigma_I)

        ALL[["Table2"]] <- .NelsonTable(dataset = dataset, options = options, name = gettextf("%s for R", var), sixsigma = .IMRchart(dataset = dataset, options = options, variable = var)$sixsigma_R)
        Iplot[[var]] <- ALL
      }
    }
  }

  # Autocorrelation Plot
  if(options$CorPlot){
    jaspResults[["CorPlot"]] <- createJaspContainer(position = 2, title = "Autocorrelation plot")
    jaspResults[["CorPlot"]]$dependOn(c("CorPlot", "variables", "nLag"))
    Corplot <- jaspResults[["CorPlot"]]

    for (var in variables) {
      Corplot[[var]] <- .CorPlot(dataset = dataset, options = options, variable = var, CI = options$CI, Lags = options$nLag)
    }
  }
}

.IMRchart <- function(dataset, options, variable = "", measurements = "", cowPlot = FALSE) {

  ppPlot <- createJaspPlot(width = 1000, height = 550)

  #Individual chart
  #data
  if (measurements == "" & variable != ""){
    ppPlot$dependOn(optionContainsValue = list(variables = variable))
    data <- data.frame(process = dataset[[variable]])
    sixsigma_I <- qcc::qcc(data$process, type ='xbar.one', plot=FALSE)
    xmr.raw.r <- matrix(cbind(data$process[1:length(data$process)-1], data$process[2:length(data$process)]), ncol = options$ncol, byrow = T)
    sixsigma_R <- qcc::qcc(xmr.raw.r, type="R", plot = FALSE)
  } else{
    data <- unlist(dataset[, measurements])
    sixsigma_I <- qcc::qcc(data, type ='xbar.one', plot=FALSE)
    xmr.raw.r <- matrix(cbind(data[1:length(data)-1],data[2:length(data)]), ncol = 2, byrow = T)
    sixsigma_R <- qcc::qcc(xmr.raw.r, type="R", plot = FALSE)
  }
  subgroups = c(1:length(sixsigma_I$statistics))
  data_plot <- data.frame(subgroups = subgroups ,process = sixsigma_I$statistics)
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
      gettextf("CL = %g", round(center, decimalplaces(sample(sixsigma_I$data,1)) + 1)),
      gettextf("UCL = %g",   round(UCL, decimalplaces(sample(sixsigma_I$data,1)) + 2)),
      gettextf("LCL = %g",   round(LCL, decimalplaces(sample(sixsigma_I$data,1)) + 2))
    )
  )
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, data_plot$process, UCL))

  p1 <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = process)) +
    ggplot2::geom_hline(yintercept = center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name = ifelse(variable != "" , gettextf("%s", variable), "Individual value"), breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(name = gettext('Observation'), breaks = xBreaks, limits = xLimits) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma_I, allsix = TRUE)$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  #Moving range chart
  data_plot <- data.frame(subgroups = c(1:length(sixsigma_R$statistics)), data2 = sixsigma_R$statistics)
  center <- sixsigma_R$center
  UCL <- max(sixsigma_R$limits)
  LCL <- min(sixsigma_R$limits)
  if (length(subgroups) > 60)
    xBreaks <- c(2,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(2,max(xBreaks) + 2.5)
  dfLabel <- data.frame(
    x = max(xLimits - 1),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, decimalplaces(sample(sixsigma_I$data,1)) + 1)),
      gettextf("UCL = %g",   round(UCL, decimalplaces(sample(sixsigma_I$data,1)) + 2)),
      gettextf("LCL = %g",   round(LCL, decimalplaces(sample(sixsigma_I$data,1)) + 2))
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

.CorPlot <- function(dataset = dataset, options = options, variable = var, Lags = NULL, CI = 0.95){
  ppPlot <- createJaspPlot(width = 1200, height = 500, title = gettextf("%s",variable))
  ppPlot$dependOn(optionContainsValue = list(variables = variable))

  list.acf <- stats::acf(dataset[[variable]], lag.max = Lags, type = "correlation", ci.type = "ma", plot = FALSE, ci = CI)
  N <- as.numeric(list.acf$n.used)
  df1 <- data.frame(lag = list.acf$lag, acf = list.acf$acf)
  df1$lag.acf <- dplyr::lag(df1$acf, default = 0)
  df1$lag.acf[2] <- 0
  df1$lag.acf.cumsum <- cumsum((df1$lag.acf)^2)
  df1$acfstd <- sqrt(1/N * (1 + 2 * df1$lag.acf.cumsum))
  df1$acfstd[1] <- 0
  df1 <- dplyr::select(df1, lag, acf, acfstd)

  p <- ggplot2::ggplot(data = df1, ggplot2::aes(x = lag, y = acf)) +
    ggplot2::geom_col(fill = "#4373B6", width = 0.2) +
    jaspGraphs::geom_line(ggplot2::aes(x = lag, y = qnorm((1+CI)/2)*acfstd), color = "red") +
    jaspGraphs::geom_line(ggplot2::aes(x = lag, y = -qnorm((1+CI)/2)*acfstd), color = "red") +
    ggplot2::geom_hline(yintercept = 0, color = 'green') +
    ggplot2::scale_y_continuous(name = gettext("Autocorrelation"), limits = c(-1,1), breaks = seq(-1,1,0.2)) +
    ggplot2::scale_x_continuous(name = gettext('Lag'), breaks = seq(0,max(df1$lag),2)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  ppPlot$plotObject <- p

  return(ppPlot)
}
