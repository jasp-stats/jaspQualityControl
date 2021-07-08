variablesChartsSubgroups <- function(jaspResults, dataset, options) {

  if (options[["CCDataFormat"]] == "CCwideFormat"){
    measurements <- unlist(options$variables)
  }else{
    measurements <- unlist(options$variablesLong)
  }
  measurements <- measurements[measurements != ""]
  subgroups <- unlist(options$subgroups)

  if (options[["CCDataFormat"]] == "CCwideFormat"){
    dataset <- .qcReadData(dataset, options, type = "capabilityStudy")
  }else{
    dataset <- .readDataSetToEnd(columns.as.numeric = measurements)
  }

  # Check if analysis is ready
  ready <- length(measurements > 0)

  if (options[["CCDataFormat"]] == "CClongFormat" && ready){
    k <- options[["CCSubgroupSize"]]
    dataset <- .PClongTowide(dataset, k, measurements)
    measurements <- colnames(dataset)
  }

  dataset <- na.omit(dataset)
  if(subgroups != "")
    subgroups <- dataset[[subgroups]]

  #Checking for errors in the dataset

  .hasErrors(dataset, type = c('observations', 'infinity', 'missingValues'),
             all.target = options$variables,
             observations.amount =  c('< 0'), exitAnalysisIfErrors = TRUE)

  #X bar & R chart
  if (options$Xbarchart && is.null(jaspResults[["XbarPlot"]])) {
    jaspResults[["XbarPlot"]] <- createJaspPlot(title =  gettext("X-bar & R Control Chart"), width = 1200, height = 500)
    jaspResults[["XbarPlot"]]$dependOn(c("Xbarchart", "variables", "Wlimits", "Phase2_XR", "mean_XR", "SD_XR", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong"))

    if (length(measurements) < 2) {
      jaspResults[["XbarPlot"]]$setError(gettext("You must enter more measurements to get this output."))
      return()
    }
    Xchart <- .XbarchartNoId(dataset = dataset[measurements], options = options,  manualXaxis = subgroups ,warningLimits = options[["Wlimits"]], Phase2 = options$Phase2_XR, target = options$mean_XR, sd = options$SD_XR)
    Rchart <- .RchartNoId(dataset = dataset[measurements], options = options, manualXaxis = subgroups, warningLimits = FALSE)
    jaspResults[["XbarPlot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(Rchart$p, Xchart$p), layout = matrix(2:1, 2), removeXYlabels= "x")

    # Nelson tests tables
    if (is.null(jaspResults[["NelsonTable"]]) & is.null(jaspResults[["NelsonTable2"]])) {
      jaspResults[["NelsonTable"]]  <- createJaspContainer(gettext(""))
      jaspResults[["NelsonTable2"]] <- createJaspContainer(gettext(""))
      jaspResults[["NelsonTable"]]  <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Xchart$sixsigma, Phase2 = options$Phase2_XR, xLabels = Xchart$xLabels)
      jaspResults[["NelsonTable2"]] <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Rchart$sixsigma, name = "R", xLabels = Rchart$xLabels)

      jaspResults[["NelsonTable"]]$dependOn(c("Xbarchart", "variables", "Phase2_XR", "mean_XR", "SD_XR", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong"))
      jaspResults[["NelsonTable2"]]$dependOn(c("Xbarchart", "variables", "Phase2_XR", "mean_XR", "SD_XR", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong"))
    }
  }

  #S Chart
  if (options$Schart && is.null(jaspResults[["SPlot"]])) {
    jaspResults[["SPlot"]] <- createJaspPlot(title = gettext("Xbar & s Control Chart"), width = 1200, height = 500)
    jaspResults[["SPlot"]]$dependOn(c("Schart", "variables", "Wlimits2", "Phase2_S", "mean_S", "SD_S", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong"))
    if (length(measurements) < 2) {
      jaspResults[["SPlot"]]$setError(gettext("You must enter more measurements to get this output."))
      return()
    }
    Schart <- .XbarSchart(dataset = dataset[measurements], options = options, manualXaxis = subgroups)
    Xchart <- .XbarchartNoId(dataset = dataset[measurements], options = options, warningLimits = options[["Wlimits2"]], manualXaxis = subgroups, Phase2 = options$Phase2_S, target = options$mean_S, sd = options$SD_S)
    jaspResults[["SPlot"]]$plotObject <- jaspGraphs::ggMatrixPlot(plotList = list(Schart$p, Xchart$p), layout = matrix(2:1, 2), removeXYlabels= "x")

    # Nelson tests tables
    if (is.null(jaspResults[["NelsonTableS"]]) & is.null(jaspResults[["NelsonTableX"]])) {
      jaspResults[["NelsonTableS"]] <- createJaspContainer(gettext(""))
      jaspResults[["NelsonTableX"]] <- createJaspContainer(gettext(""))
      jaspResults[["NelsonTableS"]] <- .NelsonTable(dataset = dataset[measurements], options = options, name = "s", sixsigma = Schart$sixsigma, xLabels = Schart$xLabels)
      jaspResults[["NelsonTableX"]] <- .NelsonTable(dataset = dataset[measurements], options = options, sixsigma = Xchart$sixsigma, Phase2 = options$Phase2_S, xLabels = Xchart$xLabels)

      jaspResults[["NelsonTableS"]]$dependOn(c("Schart", "variables", "Phase2_S", "mean_S", "SD_S", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong"))
      jaspResults[["NelsonTableX"]]$dependOn(c("Schart", "variables", "Phase2_S", "mean_S", "SD_S", "CCSubgroupSize", "CCDataFormat", "subgroups", "variablesLong"))
    }

  }
}
#Functions for control charts
.XbarSchart <- function(dataset, options, manualXaxis = "") {
  data1 <- dataset[, unlist(lapply(dataset, is.numeric))]
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
      gettextf("CL = %g", round(center, decimalplaces(data1[1,1]) + 1)),
      gettextf("UCL = %g",   round(UCL, decimalplaces(data1[1,1]) + 2)),
      gettextf("LCL = %g",   round(LCL, decimalplaces(data1[1,1]) + 2))
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

  if (manualXaxis != "") {
    xLabels <- factor(manualXaxis, levels = manualXaxis)
    p <- p + ggplot2::scale_x_continuous(breaks = 1:length(manualXaxis), labels = xLabels)
  }

  if (manualXaxis != "")
    return(list(p = p, sixsigma = sixsigma, xLabels = levels(xLabels)))
  else return(list(p = p, sixsigma = sixsigma))
}

