#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
variablesChartsIndividuals <- function(jaspResults, dataset, options) {
  # reading variables in from the GUI
  variables <- options$variables
  splitName <- options$subgroups
  subgroups <- unlist(options$subgroups)
  makeSplit <- splitName != ""

  numeric_variables  <- variables[variables != ""]

  ready <- length(numeric_variables) == 1

  if (is.null(dataset)) {
    if (options[["subgroups"]] != "") {
      dataset <- .readDataSetToEnd(columns.as.numeric = numeric_variables, columns.as.factor = splitName)
      dataset.factors <- .readDataSetToEnd(columns=variables, columns.as.factor=splitName)
    } else {
      dataset <- .readDataSetToEnd(columns.as.numeric = numeric_variables)
    }
  }

  if (makeSplit & ready) {
    splitFactor      <- dataset[[.v(splitName)]]
    splitLevels      <- levels(splitFactor)
    # remove missing values from the grouping variable
    dataset <- dataset[!is.na(splitFactor), ]
    dataset.factors <- dataset.factors[!is.na(splitFactor), ]

    numberMissingSplitBy <- sum(is.na(splitFactor))

    # Actually remove missing values from the split factor
    splitFactor <- na.omit(splitFactor)

    if(subgroups != "")
      subgroups <- splitFactor
  }

   #Checking for errors in the dataset
  .hasErrors(dataset, type = c('infinity', 'missingValues', "observations"),
             infinity.target = c(options$variables, options$subgroups),
             missingValues.target = c(options$variables, options$subgroups),
             observations.amount = c("< 2"),
             observations.target = c(options$variables),
             exitAnalysisIfErrors = TRUE)

  if (options$ImRchart && length(variables) == 0) {
    plot <- createJaspPlot(title = gettext("Individuals Charts"), width = 700, height = 400)
    jaspResults[["plot"]] <- plot
    plot$dependOn(c("ImRchart", "variables", "subgroups"))
    return()
  }

  dataset <- na.omit(dataset)

  # default plot
  if (!ready) {
    plot <- createJaspPlot(title = gettext("Variables Charts for Individuals"), width = 700, height = 400)
    jaspResults[["plot"]] <- plot
    plot$dependOn(c("ImRchart", "CorPlot", "CCReport", "variables"))
    return()
  }
  #ImR chart
  if (options$ImRchart && ready) {
    if(is.null(jaspResults[["Ichart"]])){
      jaspResults[["Ichart"]] <- createJaspContainer(position = 1)
      jaspResults[["Ichart"]]$dependOn(c("ImRchart", "variables", "ncol", "subgroups", "manualTicks", "nTicks", "ccTitle", "ccName", "ccMisc","ccReportedBy","ccDate", "ccSubTitle", "ccChartName", "ccReport"))
      Iplot <- jaspResults[["Ichart"]]

      for (var in variables) {
        ALL <- createJaspContainer(gettextf("X-mR control chart"))
        IMR <- .IMRchart(dataset = dataset, options = options, variable = var, manualXaxis = subgroups)
        ALL[["Plot"]] <- IMR$p
        ALL[["Table1"]] <- .NelsonTable(dataset = dataset, options = options, type = "xbar.one", name = gettextf("%s for Individuals", var), sixsigma = IMR$sixsigma_I, xLabels = IMR$xLabels)
        ALL[["Table2"]] <- .NelsonTable(dataset = dataset, options = options, name = gettextf("%s for Range", var), sixsigma = IMR$sixsigma_R, xLabels = IMR$xLabels, type = "Range")
        Iplot[[var]] <- ALL
      }
    }
  }

  # Autocorrelation Plot
  if(options$CorPlot && ready){
    jaspResults[["CorPlot"]] <- createJaspContainer(position = 2, title = "Autocorrelation Function")
    jaspResults[["CorPlot"]]$dependOn(c("CorPlot", "variables", "nLag"))
    Corplot <- jaspResults[["CorPlot"]]

    for (var in variables)
      Corplot[[var]] <- .CorPlot(dataset = dataset, options = options, variable = var, CI = options$CI, Lags = options$nLag)
  }

  # Report
  if (options[["CCReport"]] && is.null(jaspResults[["CCReport"]]) && options$ImRchart) {

    jaspResults[["CorPlot"]] <- NULL
    jaspResults[["Ichart"]] <- NULL


    jaspResults[["CCReport"]] <- createJaspContainer(gettext("Report"))
    jaspResults[["CCReport"]]$dependOn(c("CCReport", "ImRchart", "variables","ncol", "manualTicks", "nTicks", "subgroups", "ccTitle", "ccName", "ccMisc","ccReportedBy","ccDate", "ccSubTitle", "ccChartName"))
    jaspResults[["CCReport"]]$position <- 9
    Iplot <- jaspResults[["CCReport"]]

    IMR <- .IMRchart(dataset = dataset, options = options, variable = variables, manualXaxis = subgroups)
    Iplot[["ccReport"]] <- .CCReport(p1 = IMR$p1, p2 = IMR$p2, ccTitle = options$ccTitle,
                                       ccName = options$ccName, ccDate = options$ccDate, ccReportedBy = options$ccReportedBy, ccSubTitle = options$ccSubTitle,
                                       ccChartName = options$ccChartName)
  }

  # Error handling
  if (options$CCReport && (!options$ImRchart || length(variables) < 1)){
    plot <- createJaspPlot(title = gettext("Report"), width = 700, height = 400)
    jaspResults[["plot"]] <- plot
    jaspResults[["plot"]]$setError(gettext("Please insert more measurements and check the X-mR chart."))
    plot$dependOn(c("CCReport", "ImRchart", "variables"))
    return()
  }
}

.IMRchart <- function(dataset, options, variable = "", measurements = "", cowPlot = FALSE, manualXaxis = "", Wide = FALSE) {

  ppPlot <- createJaspPlot(width = 1000, height = 550)
  #Individual chart
  #data
  if (identical(measurements, "") && !identical(variable, "")) {
    ppPlot$dependOn(optionContainsValue = list(variables = variable))
    data <- data.frame(process = dataset[[variable]])
    sixsigma_I <- qcc::qcc(data$process, type ='xbar.one', plot=FALSE)
    xmr.raw.r <- matrix(cbind(data$process[1:length(data$process)-1], data$process[2:length(data$process)]), ncol = options$ncol)
    sixsigma_R <- qcc::qcc(xmr.raw.r, type="R", plot = FALSE)
  } else{
    data <- as.vector((t(dataset[measurements])))
    sixsigma_I <- qcc::qcc(data, type ='xbar.one', plot=FALSE)
    xmr.raw.r <- matrix(cbind(data[1:length(data)-1],data[2:length(data)]), ncol = 2)
    sixsigma_R <- qcc::qcc(xmr.raw.r, type="R", plot = FALSE)
  }
  subgroups = c(1:length(sixsigma_I$statistics))
  data_plot <- data.frame(subgroups = subgroups ,process = sixsigma_I$statistics)
  center <- sixsigma_I$center
  UCL <- max(sixsigma_I$limits)
  LCL <- min(sixsigma_I$limits)
  if (options$manualTicks)
    nxBreaks <- options$nTicks
  else
    nxBreaks <- 5
  xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups, n = nxBreaks)[-1])
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
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
  Xlabels <- c(2, xBreaks[-1])
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
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
    ggplot2::scale_x_continuous(name = gettext('Observation'), breaks = xBreaks, limits = xLimits, labels = Xlabels) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma_R)$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (!identical(manualXaxis, "")) {
    if (!identical(measurements, "")) {
      if (Wide)
        xLabels <- as.vector(sapply(1:length(manualXaxis), function(x) {rep(manualXaxis[x], ncol(dataset[measurements]))}))
      else
        xLabels <- manualXaxis
    }
    else
      xLabels <- manualXaxis

    p1 <- p1 + ggplot2::scale_x_continuous(breaks = xBreaks, labels = xLabels[xBreaks])
    p2 <- p2 + ggplot2::scale_x_continuous(breaks = xBreaks, labels = xLabels[xBreaks])
  }

  plotMat <- matrix(list(), 2, 1)
  plotMat[[1,1]] <- p1
  plotMat[[2,1]] <- p2

  if(!cowPlot){
    ppPlot$plotObject <-  jaspGraphs::ggMatrixPlot(plotList = plotMat, removeXYlabels= "x")
  }else{
    ppPlot$plotObject <- cowplot::plot_grid(plotlist = plotMat, ncol = 1, nrow = 2)
  }

  if (!identical(manualXaxis, ""))
    return(list(p = ppPlot, sixsigma_I = sixsigma_I, sixsigma_R = sixsigma_R, xLabels = as.vector(xLabels), p1 = p1, p2 = p2))
  else
    return(list(p = ppPlot, sixsigma_I = sixsigma_I, sixsigma_R = sixsigma_R, p1 = p1, p2 = p2))
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
    ggplot2::scale_x_continuous(name = gettext('Lag'), breaks = seq(1,max(df1$lag),2)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  ppPlot$plotObject <- p

  return(ppPlot)
}
