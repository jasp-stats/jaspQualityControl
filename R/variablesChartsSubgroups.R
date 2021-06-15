variablesChartsSubgroups <- function(jaspResults, dataset, options) {
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
    dataset[[time]] <- as.character(dataset[[time]])
  }
  #Checking for errors in the dataset
  .hasErrors(dataset, type = c('observations', 'infinity', 'missingValues'),
             all.target = options$variables,
             observations.amount = c(' < 2'), exitAnalysisIfErrors = TRUE)

  #X bar & R chart
  if (options$Xbarchart && is.null(jaspResults[["XbarPlot"]]) &&  length(options$variables) > 1) {
    jaspResults[["XbarPlot"]] <- createJaspPlot(title =  gettext("X-bar & R Control Chart"), width = 1200, height = 500)
    Xchart <- .XbarchartNoId(dataset = dataset, options = options, warningLimits = options[["Wlimits"]], time = makeTime, Phase2 = options$Phase2_XR, target = options$mean_XR, sd = options$SD_XR)
    Rchart <- .RchartNoId(dataset = dataset, options = options, time = makeTime, warningLimits = FALSE)
    jaspResults[["XbarPlot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(Rchart$p, Xchart$p), layout = matrix(2:1, 2), removeXYlabels= "x")
    jaspResults[["XbarPlot"]]$dependOn(c("Xbarchart", "variables", "Wlimits", "Phase2_XR", "mean_XR", "SD_XR"))

    # Nelson tests tables
    if (is.null(jaspResults[["NelsonTable"]]) & is.null(jaspResults[["NelsonTable2"]])) {
      jaspResults[["NelsonTable"]] <- createJaspContainer(gettext(""))
      jaspResults[["NelsonTable2"]] <- createJaspContainer(gettext(""))
      jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = Rchart$sixsigma, name = "R", xLabels = Rchart$xLabels)
      jaspResults[["NelsonTable2"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = Xchart$sixsigma, Phase2 = options$Phase2_XR, xLabels = Xchart$xLabels)

      jaspResults[["NelsonTable"]]$dependOn(c("Xbarchart", "variables", "time", "Phase2_XR", "mean_XR", "SD_XR"))
      jaspResults[["NelsonTable2"]]$dependOn(c("Xbarchart", "variables", "time", "Phase2_XR", "mean_XR", "SD_XR"))
    }
  }

  #S Chart
  if (options$Schart && is.null(jaspResults[["SPlot"]]) &&  length(options$variables) > 1) {
    jaspResults[["SPlot"]] <- createJaspPlot(title = gettext("Xbar & s Control Chart"), width = 1200, height = 500)
    jaspResults[["SPlot"]]$dependOn(c("Schart", "variables", "Wlimits2", "Phase2_S", "time", "mean_S", "SD_S"))
    Schart <- .XbarSchart(dataset = dataset, options = options, time = makeTime)
    Xchart <- .XbarchartNoId(dataset = dataset, options = options, warningLimits = options[["Wlimits2"]], time = makeTime,Phase2 = options$Phase2_S, target = options$mean_S, sd = options$SD_S)
    jaspResults[["SPlot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(Schart$p, Xchart$p), layout = matrix(2:1, 2), removeXYlabels= "x")

    # Nelson tests tables
    if (is.null(jaspResults[["NelsonTableS"]]) & is.null(jaspResults[["NelsonTableX"]])) {
      jaspResults[["NelsonTableS"]] <- createJaspContainer(gettext(""))
      jaspResults[["NelsonTableX"]] <- createJaspContainer(gettext(""))
      jaspResults[["NelsonTableS"]] <- .NelsonTable(dataset = dataset, options = options, name = "s", sixsigma = Schart$sixsigma, xLabels = Schart$xLabels)
      jaspResults[["NelsonTableX"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = Xchart$sixsigma, Phase2 = options$Phase2_S, xLabels = Xchart$xLabels)

      jaspResults[["NelsonTableS"]]$dependOn(c("Schart", "variables", "time", "Phase2_S", "mean_S", "SD_S"))
      jaspResults[["NelsonTableX"]]$dependOn(c("Schart", "variables", "time", "Phase2_S", "mean_S", "SD_S"))
    }

  }
}

#Functions for control charts
.XbarSchart <- function(dataset, options, time = FALSE) {
  data1 <- dataset[, options$variables]
  Stdv <- apply(data1, 1, function(x) sd(x))
  subgroups <- c(1:length(Stdv))
  data_plot <- data.frame(subgroups = subgroups, Stdv = Stdv)
  sixsigma <- qcc::qcc(data1, type ='S', plot=FALSE)
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, data_plot$Stdv, UCL))
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

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = Stdv)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Standard Deviation"), breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(name = gettext('Subgroup'), breaks = xBreaks) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma)$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (time) {
    xLabels <- factor(dataset[[.v(options$time)]], levels = unique(as.character(dataset[[.v(options$time)]])))
    p <- p + ggplot2::scale_x_continuous(name = gettext('Time'), breaks = 1:length(subgroups), labels = xLabels)
    return(list(p = p, sixsigma = sixsigma, xLabels = dataset[[.v(options$time)]]))
  }
  else {return(list(p = p, sixsigma = sixsigma))}
}
