#############################################################
## Common functions for preparatory work ####################
#############################################################

# Common function to read in data set
.qcReadData <- function(dataset, options, type){
  if(type == "capabilityStudy"){
    if(is.null(dataset)) {
      if (options[["subgroups"]] != "") {
        dataset <- .readDataSetToEnd(columns.as.numeric = options[["variables"]], columns.as.factor = options[["subgroups"]])
      } else {
        dataset <- .readDataSetToEnd(columns.as.numeric = options[["variables"]])
      }
    }
  }
  return(dataset)
}

# Common function to check if options are ready
.qcOptionsReady <- function(options, type){
  if(type == "capabilityStudy"){
    ready <- length(unlist(options[["variables"]])) > 0
  }
  return(ready)
}

#############################################################
## Common functions for plots ###############################
#############################################################

# Function to create the x-bar and r-chart section
.qcXbarAndRContainer <- function(options, dataset, ready, jaspResults){

  if(!options[["controlCharts"]] || !is.null(jaspResults[["controlCharts"]]))
    return()

  container <- createJaspContainer(title = gettext("Control Charts"))
  container$dependOn(options = c("controlCharts", "variables"))
  container$position <- 1
  jaspResults[["controlCharts"]] <- container

  xplot <- createJaspPlot(title = "X-bar Chart", width = 600, height = 300)
  container[["xplot"]] <- xplot # Always has position = 1 in container

  rplot <- createJaspPlot(title = "R Chart", width = 600, height = 300)
  container[["rplot"]] <- rplot # Always has position = 2 in container

  if(!ready)
    return()

  if(length(options[["variables"]]) < 2){
    xplot$setError(gettext("You must enter at least 2 measurements to get this output."))
    rplot$setError(gettext("You must enter at least 2 measurements to get this output."))
    return()
  }

  xplot$plotObject <- .XbarchartNoId(dataset = dataset, options = options)
  rplot$plotObject <- .RchartNoId(dataset = dataset, options = options)
}

# Function to create X-bar chart
.XbarchartNoId <- function(dataset, options){
  #Xbar
  data <- dataset[, unlist(lapply(dataset, is.numeric))]
  means <- rowMeans(data)
  subgroups <- c(1:length(means))
  data_plot <- data.frame(subgroups = subgroups, means = means)
  sixsigma <- qcc::qcc(data, type ='xbar', plot=FALSE)
  center <- sixsigma$center
  sd1 <- sixsigma$std.dev
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
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
  warn.limits <- c(qcc::limits.xbar(sixsigma$center, sixsigma$std.dev, sixsigma$sizes, 1),
                    qcc::limits.xbar(sixsigma$center, sixsigma$std.dev, sixsigma$sizes, 2))

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = means)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green', size = 1) +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1) +
    ggplot2::geom_hline(yintercept = warn.limits, color = "orange", linetype = "dashed", size = 1) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name = gettext("Subgroup mean") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = gettext('Subgroup'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$means > UCL | data_plot$means < LCL, "red", "blue")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(p)
}

.RchartNoId <- function(dataset, options){
  #R-chart
  data1 <- dataset[, unlist(lapply(dataset, is.numeric))]
  range <- apply(data1, 1, function(x) max(x) - min(x))
  subgroups <- 1:length(range)
  data_plot <- data.frame(subgroups = subgroups, range = range)
  sixsigma <- qcc::qcc(data1, type = 'R', plot = FALSE)
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL - (0.10 * abs(LCL)), range, UCL + (0.1 * UCL)), min.n = 4)
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups)
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
  warn.limits <- c(qcc::limits.xbar(sixsigma$center, sixsigma$std.dev, sixsigma$sizes, 1),
                   qcc::limits.xbar(sixsigma$center, sixsigma$std.dev, sixsigma$sizes, 2))

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = range)) +
    ggplot2::geom_hline(yintercept = center,  color = 'green', size = 1) +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", , linetype = "dashed", size = 1) +
    ggplot2::geom_hline(yintercept = warn.limits, color = "orange", linetype = "dashed", size = 1) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name = gettext("Subgroup range") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name= gettext("Subgroup") ,breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$range > UCL | data_plot$range < LCL, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(p)
}

