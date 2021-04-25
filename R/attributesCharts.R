attributesCharts <- function(jaspResults, dataset, options) {
  variables <- options$variables
  total <- options$total
  D <- options$D
  numeric_variables <- c(variables, total, D)
  numeric_variables  <- numeric_variables[numeric_variables != ""]

  dataset         <- .readDataSetToEnd(columns.as.numeric = numeric_variables, exclude.na.listwise = numeric_variables)
  #Checking for errors in the dataset
  .hasErrors(dataset, type = c('observations', 'infinity', 'missingValues', "negativeValues"),
             all.target = options$variables,
             observations.amount = c(' < 2'), exitAnalysisIfErrors = TRUE)

  if (options$Attributes == "Defectives" && options$D != "" && options$total != "") {
    #P chart
    if (options$TypeDefectives == "pchart" && is.null(jaspResults[["pchart"]])) {
      jaspResults[["PchartPlot"]] <- createJaspPlot(title =  gettext("P Control Chart"), width = 700, height = 350)
      jaspResults[["PchartPlot"]]$dependOn(c("total", "D", "Attributes", "TypeDefectives"))
      jaspResults[["PchartPlot"]]$position <- 1
      PPlot <- jaspResults[["PchartPlot"]]
      PPlot$plotObject <- .Pchart(dataset = dataset, options = options)

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer(gettext(""))
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, type = "p")
        jaspResults[["NelsonTable"]]$dependOn(c("total", "D", "Attributes", "TypeDefectives"))
        jaspResults[["NelsonTable"]]$position <- 2
      }
    }
    #NP chart
    if (options$TypeDefectives == "npchart" && is.null(jaspResults[["npchart"]])) {
      jaspResults[["NPchartPlot"]] <- createJaspPlot(title =  gettext("NP Control Chart"), width = 700, height = 350)
      jaspResults[["NPchartPlot"]]$dependOn(c("total", "D", "Attributes", "TypeDefectives"))
      jaspResults[["NPchartPlot"]]$position <- 1
      PPlot <- jaspResults[["NPchartPlot"]]
      PPlot$plotObject <- .NPchart(dataset = dataset, options = options)

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer(gettext(""))
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, type = "np")
        jaspResults[["NelsonTable"]]$dependOn(c("total", "D", "Attributes", "TypeDefectives"))
        jaspResults[["NelsonTable"]]$position <- 2
      }
    }

    #Laney P chart
    if (options$TypeDefectives == "Laneyprimechart" && is.null(jaspResults[["LaneyPchart"]])) {
      jaspResults[["LaneyPPlot"]] <- createJaspPlot(title =  gettext("Laney P' Control Chart"), width = 700, height = 350)
      jaspResults[["LaneyPPlot"]]$dependOn(c("total", "D", "Attributes", "TypeDefectives"))
      PPlot <- jaspResults[["LaneyPPlot"]]
      PPlot$plotObject <- .LanyP(dataset = dataset, options = options)
    }
  }

  if (options$Attributes == "Defects" && options$D != "" && options$total != "") {
    #Cchart
    if (options$TypeDefects == "cchart" && is.null(jaspResults[["Cchart"]])) {
      jaspResults[["CchartPlot"]] <- createJaspPlot(title =  gettext("C Control Chart"), width = 700, height = 350)
      jaspResults[["CchartPlot"]]$dependOn(c("D", "Attributes", "TypeDefects","total"))
      jaspResults[["CchartPlot"]]$position <- 1
      PPlot <- jaspResults[["CchartPlot"]]
      PPlot$plotObject <- .Cchart(dataset = dataset, options = options)

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer(gettext(""))
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, type = "c")
        jaspResults[["NelsonTable"]]$dependOn(c("total", "D", "Attributes", "TypeDefects"))
        jaspResults[["NelsonTable"]]$position <- 2
      }
    }
    #Uchart
    if (options$TypeDefects == "uchart" && is.null(jaspResults[["Uchart"]])) {
      jaspResults[["UchartPlot"]] <- createJaspPlot(title =  gettext("U Control Chart"), width = 700, height = 350)
      jaspResults[["UchartPlot"]]$dependOn(c("D", "total","Attributes", "TypeDefects"))
      jaspResults[["UchartPlot"]]$position <- 1
      PPlot <- jaspResults[["UchartPlot"]]
      PPlot$plotObject <- .Uchart(dataset = dataset, options = options)

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer(gettext(""))
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, type = "u")
        jaspResults[["NelsonTable"]]$dependOn(c("total", "D", "Attributes", "TypeDefects"))
        jaspResults[["NelsonTable"]]$position <- 2
      }
    }
    #Laney U chart
    if (options$TypeDefects == "Laneychart" && is.null(jaspResults[["LaneyUchart"]])) {
      jaspResults[["LaneyUPlot"]] <- createJaspPlot(title = gettext("Laney U' Control Chart"), width = 700, height = 350)
      jaspResults[["LaneyUPlot"]]$dependOn(c("D", "total","Attributes", "TypeDefects"))
      PPlot <- jaspResults[["LaneyUPlot"]]
      PPlot$plotObject <- .LanyU(dataset = dataset, options = options)
    }
  }
  #ImRchart for attributes
  if(options$ImRchart2 && length(options$D) > 0){
    jaspResults[["IPlotA"]] <- createJaspPlot(title = gettext("Individual and Moving Range Control Charts"), width = 700, height = 350)
    jaspResults[["IPlotA"]]$dependOn(c("D", "total","ImRchart2"))

    IPlot <- jaspResults[["IPlotA"]]
    IPlot$plotObject <- .Ichart_attributes(dataset = dataset, options = options)
  }
}

.Pchart <- function(dataset, options) {
  ready <- options$D != "" && options$total != ""
  if (!ready)
    return()

  data1 <- data.frame(D = dataset[, options$D], sample = dataset[, options$total])
  data1$P <- data1$D/data1$sample
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, P = data1$P)

  sixsigma <- with(data1, qcc::qcc(D, sample, type = "p", plot = FALSE))
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data1$P ,UCL + 0.1))
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

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = P)) +
    ggplot2::geom_hline(yintercept =  center, color = "green") +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue')) +
    jaspGraphs::themeJaspRaw()

  return(p)
}
.NPchart <- function(dataset, options) {
  ready <- options$D != "" && options$total != ""
  if (!ready)
    return()

  .Check_equal_samples(dataset, options)

  data1 <- data.frame(D = dataset[, options$D], sample = dataset[, options$total])
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, D = data1$D)

  sixsigma <- with(data1, qcc::qcc(D, sample, type = "np", plot = FALSE))
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data1$D ,UCL + 0.1))
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

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = D)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("D"),limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$D > UCL | data_plot$D < LCL, 'red', 'blue')) +
    jaspGraphs::themeJaspRaw()

  return(p)
}
.Cchart <- function(dataset, options) {
  ready <- options$D != "" && options$total != ""
  if (!ready)
    return()

  .Check_equal_samples(dataset, options)

  .Check_equal_samples(dataset, options)

  .Check_equal_samples(dataset, options)

  data1 <- data.frame(D = dataset[, options$D], sample = dataset[, options$total])
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, D = data1$D)

  sixsigma <- with(data1, qcc::qcc(D, sample, type = "c", plot = FALSE))
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data1$D ,UCL + 0.1))
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

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = D)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("D") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$D > UCL | data_plot$D < LCL, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()


  return(p)
}
.Uchart <- function(dataset, options) {
  ready <- options$D != "" && options$total != ""
  if (!ready)
    return()

  data1 <- data.frame(D = dataset[, options$D], sample = dataset[, options$total])
  data1$P <- data1$D/data1$sample
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, P = data1$P)

  sixsigma <- with(data1, qcc::qcc(D, sample, type = "u", plot = FALSE))
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data1$P ,UCL + 0.1))
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

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = P)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue')) +
    jaspGraphs::themeJaspRaw()

  return(p)
}

##Imr for attributes
.Ichart_attributes <- function(dataset, options) {
  ready <- options$D != "" && options$total != ""
  if (!ready)
    return()

  data1 <- data.frame(D = dataset[, options$D], sample = dataset[, options$total])
  data1$P <- data1$D/data1$sample
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, P = data1$P)

  sixsigma <- qcc::qcc(data_plot$P, type ='xbar.one', plot=FALSE)
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL - 0.1, UCL + 0.1))
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

  p1 <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = P)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  #data
  data <- data.frame(process = dataset[, options$D] / dataset[, options$total])
  xmr.raw.r <- matrix(cbind(data$process[1:length(data$process)-1], data$process[2:length(data$process)]), ncol=2)
  sixsigma <- qcc::qcc(xmr.raw.r, type="R", plot = FALSE)
  data_plot <- data.frame(subgroups = c(1:length(sixsigma$statistics)), data = sixsigma$statistics)
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL + 0.1))
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

  p2 <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = data)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Moving Range") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Observation'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$data > UCL | data_plot$data < LCL, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  p3 <-  jaspGraphs::ggMatrixPlot(plotList = list(p1, p2), layout = matrix(1:2, 2), removeXYlabels= "x")

  return(p3)
}

### Lanys charts
.LanyU <- function(dataset, options) {
  ready <- options$D != "" && options$total != ""
  if (!ready)
    return()

  data1 <- data.frame(D = dataset[,options$D], sample = dataset[,options$total])
  data1$P <- data1$D/data1$sample
  subgroups <- c(1:nrow(data1))
  data_plot <- data.frame(subgroups = subgroups, P = data1$P)

  #Sixsigma
  center = sum(data1$D)/sum(data1$sample)
  z_scores <- (data1$P - center)/sqrt(center/data1$sample[1])
  R <- vector()
  for (i in c(1:length(z_scores) - 1))
    R[i] <- abs(z_scores[i+1] - z_scores[i])

  MR_mean <- sum(R)/ (length(data1$P) - 1)
  sigma <- MR_mean/1.128
  LCL <- ifelse(center - 3*sqrt(center/data1$sample[1]) * sigma < 0, 0, center - 3*sqrt(center/data1$sample[1]) * sigma)
  UCL <- center + 3*sqrt(center/data1$sample[1]) * sigma
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL - 0.1, UCL + 0.1))
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

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = P)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue')) +
    jaspGraphs::themeJaspRaw()

  return(p)
}

.LanyP <- function(dataset, options) {
  ready <- options$D != "" && options$total != ""
  if (!ready)
    return()

  data1 <- data.frame(D = dataset[,options$D], sample = dataset[,options$total])
  data1$P <- data1$D/data1$sample
  subgroups <- c(1:nrow(data1))
  data_plot <- data.frame(subgroups = subgroups, P = data1$P)

  #Sixsigma
  center = sum(data1$D)/sum(data1$sample)
  z_scores <- (data1$P - center)/sqrt(center*(1 - center)/data1$sample[1])
  R <- vector()
  for (i in c(1:length(z_scores) - 1))
    R[i] <- abs(z_scores[i+1] - z_scores[i])

  MR_mean <- sum(R)/ (length(data1$P) - 1)
  sigma <- MR_mean/1.128
  LCL <- ifelse(center - 3*sqrt(center*(1 - center)/data1$sample[1]) * sigma < 0, 0, center - 3*sqrt(center*(1 - center)/data1$sample[1]) * sigma)
  UCL <- center + 3*sqrt(center*(1 - center)/data1$sample[1]) * sigma
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL - 0.1, UCL + 0.1))
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

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = P)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue')) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name = gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(p)
}
.Check_equal_samples <- function(dataset,options) {
  .hasErrors(
    dataset,
    all.target = options$total,
    custom = function() {
      if (length(unique(dataset[[options$total]])) > 1)
        return("Samples must be equal in size")
    },
    exitAnalysisIfErrors = TRUE
  )
}
