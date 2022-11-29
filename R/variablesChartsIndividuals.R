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

#' @export
variablesChartsIndividuals <- function(jaspResults, dataset, options) {
  # reading variables in from the GUI
  variables <- unlist(options$variables)
  stages <- unlist(options$split)
  subgroups <- unlist(options$subgroups)
  makeSplit <- subgroups != ""

  numeric_variables  <- variables
  numeric_variables  <- numeric_variables[numeric_variables != ""]
  factorVariables <- c(stages, subgroups)
  factorVariables  <- factorVariables[factorVariables != ""]

  ready <- length(numeric_variables) == 1

  if (is.null(dataset)) {
      dataset <- .readDataSetToEnd(columns.as.numeric = numeric_variables, columns.as.factor = factorVariables)
  }

  if (makeSplit & ready) {
    splitFactor      <- dataset[[.v(subgroups)]]
    splitLevels      <- levels(splitFactor)
    # remove missing values from the grouping variable
    dataset <- dataset[!is.na(splitFactor), ]

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
    plot$dependOn(c("ImRchart", "variables", "subgroups", "split"))
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
      jaspResults[["Ichart"]]$dependOn(c("ImRchart", "variables", "movingRangeLength", "subgroups", "manualTicks", "nTicks", "ccTitle",
                                         "ccName", "ccMisc","ccReportedBy","ccDate", "ccSubTitle", "ccChartName", "ccReport",
                                         "split"))
      Iplot <- jaspResults[["Ichart"]]

      for (var in variables) {
        ALL <- createJaspContainer(gettextf("X-mR control chart"))
        IMR <- .IMRchart(dataset = dataset, options = options, variable = var, manualXaxis = subgroups, stages = stages)
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
    jaspResults[["CCReport"]]$dependOn(c("CCReport", "ImRchart", "variables","ncol", "manualTicks", "nTicks", "subgroups",
                                         "ccTitle", "ccName", "ccMisc","ccReportedBy","ccDate", "ccSubTitle", "ccChartName",
                                         "split"))
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
