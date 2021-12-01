attributesCharts <- function(jaspResults, dataset, options) {

  total <- options$total
  D <- options$D
  numeric_variables <- c(total, D)
  numeric_variables  <- numeric_variables[numeric_variables != ""]

  dataset <- .readDataSetToEnd(columns.as.numeric = numeric_variables)

  # Checking if the analysis is ready
  ready <- options$D != "" && options$total != ""

  #Checking for errors in the dataset
  .hasErrors(dataset, type = c('infinity', 'missingValues'),
             all.target = c(options$D,options$total),
             exitAnalysisIfErrors = TRUE)

  if ((options$Attributes == "Defectives" | options$Attributes == "Defects" | options$Attributes == "ImR") && !ready) {
    plot <- createJaspPlot(title = gettext("Attributes Control Charts"), width = 700, height = 400)
    jaspResults[["plot"]] <- plot
    plot$dependOn(c("Attributes", "D", "total"))
    return()
  }

  dataset <- na.omit(dataset)

  if (options$Attributes == "Defectives" && ready) {

    #P chart
    if (options$TypeDefectives == "pchart" && is.null(jaspResults[["PchartPlot"]])) {
      jaspResults[["PchartPlot"]] <- createJaspPlot(title =  gettext("p Chart"), width = 1200, height = 500)
      jaspResults[["PchartPlot"]]$dependOn(c("total", "D", "Attributes", "TypeDefectives"))
      jaspResults[["PchartPlot"]]$position <- 1
      Pchart <- .Pchart(dataset = dataset, options = options)
      jaspResults[["PchartPlot"]]$plotObject <- Pchart$p



      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer(gettext(""))
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = Pchart$sixsigma, name = "P")
        jaspResults[["NelsonTable"]]$dependOn(c("total", "D", "Attributes", "TypeDefectives"))
        jaspResults[["NelsonTable"]]$position <- 2
      }
    }
    #NP chart
    if (options$TypeDefectives == "npchart" && is.null(jaspResults[["NPchartPlot"]])) {
      jaspResults[["NPchartPlot"]] <- createJaspPlot(title =  gettext("np Chart"), width = 1200, height = 500)
      jaspResults[["NPchartPlot"]]$dependOn(c("total", "D", "Attributes", "TypeDefectives"))
      jaspResults[["NPchartPlot"]]$position <- 1
      NPchart <- .NPchart(dataset = dataset, options = options)
      jaspResults[["NPchartPlot"]]$plotObject <- NPchart$p

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer(gettext(""))
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = NPchart$sixsigma, name = "np")
        jaspResults[["NelsonTable"]]$position <- 2
        jaspResults[["NelsonTable"]]$dependOn(c("total", "D", "Attributes", "TypeDefectives"))
      }
    }

    #Laney P chart
    if (options$TypeDefectives == "Laneyprimechart" && is.null(jaspResults[["LaneyPPlot"]])) {
      jaspResults[["LaneyPPlot"]] <- createJaspPlot(title =  gettext("Laney p' Chart"), width = 1200, height = 500, position = 1)
      Lanychart <- .LanyP(dataset = dataset, options = options)
      jaspResults[["LaneyPPlot"]]$plotObject <- Lanychart$p
      jaspResults[["LaneyPPlot"]]$dependOn(c("total", "D", "Attributes", "TypeDefectives"))

      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspPlot(title =  gettext(""), position = 2)
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = Lanychart$sixsigma, name = "Laney p'")
        jaspResults[["NelsonTable"]]$dependOn(c("total", "D", "Attributes", "TypeDefectives"))
      }
    }
  }

  if (options$Attributes == "Defects" && ready) {
    #Cchart
    if (options$TypeDefects == "cchart" && is.null(jaspResults[["CchartPlot"]])) {
      jaspResults[["CchartPlot"]] <- createJaspPlot(title =  gettext("c Chart"), width = 1200, height = 500)
      jaspResults[["CchartPlot"]]$dependOn(c("D", "Attributes", "TypeDefects","total"))
      jaspResults[["CchartPlot"]]$position <- 1
      Cchart <- .Cchart(dataset = dataset, options = options)
      jaspResults[["CchartPlot"]]$plotObject <- PlotReport <- Cchart$p

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer()
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = Cchart$sixsigma, name = "c")
        jaspResults[["NelsonTable"]]$dependOn(c("total", "D", "Attributes", "TypeDefects"))
        jaspResults[["NelsonTable"]]$position <- 2
      }
    }
    #Uchart
    if (options$TypeDefects == "uchart" && is.null(jaspResults[["UchartPlot"]])) {
      jaspResults[["UchartPlot"]] <- createJaspPlot(title =  gettext("u Chart"), width = 1200, height = 500)
      jaspResults[["UchartPlot"]]$dependOn(c("D", "total","Attributes", "TypeDefects"))
      jaspResults[["UchartPlot"]]$position <- 1
      Uchart <- .Uchart(dataset = dataset, options = options)
      jaspResults[["UchartPlot"]]$plotObject <- Uchart$p
        PlotReport <- Uchart$p

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer(gettext(""))
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = Uchart$sixsigma, name = "u")
        jaspResults[["NelsonTable"]]$dependOn(c("total", "D", "Attributes", "TypeDefects"))
        jaspResults[["NelsonTable"]]$position <- 2
      }
    }
    #Laney U chart
    if (options$TypeDefects == "Laneychart" && is.null(jaspResults[["LaneyUPlot"]])) {
      jaspResults[["LaneyUPlot"]] <- createJaspPlot(title = gettext("Laney u' Chart"), width = 1200, height = 500, position = 1)
      jaspResults[["LaneyUPlot"]]$dependOn(c("D", "total","Attributes", "TypeDefects"))
      LanyUchart <- .LanyU(dataset = dataset, options = options)
      jaspResults[["LaneyUPlot"]]$plotObject <- PlotReport <- LanyUchart$p

      jaspResults[["NelsonTable"]] <- createJaspContainer(title =  gettext(""), position = 2)
      jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = LanyUchart$sixsigma, name = "Laney u'")
      jaspResults[["NelsonTable"]]$dependOn(c("D", "total","Attributes", "TypeDefects"))
    }
  }
  #ImRchart for attributes
  if (options$Attributes == "ImR" && D != "") {
    jaspResults[["IPlotA"]] <- createJaspPlot(title = gettext("Individuals and Moving Range Chart"), width = 1200, height = 500, position = 1)
    IMRchart <- .Ichart_attributes(dataset = dataset, options = options)
    jaspResults[["IPlotA"]]$plotObject <- PlotReport <- IMRchart$p
    jaspResults[["IPlotA"]]$dependOn(c("D", "total", "Attributes"))

    # Nelson tests tables
    if (is.null(jaspResults[["NelsonTableI"]]) & is.null(jaspResults[["NelsonTabeMR"]])) {
      jaspResults[["NelsonTableI"]] <- .NelsonTable(dataset = dataset, options = options, type = "xbar.one", sixsigma = IMRchart$sixsigma_I, name = "Individuals")
      jaspResults[["NelsonTableMR"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = IMRchart$sixsigma_R, name = "Moving Range")

      jaspResults[["NelsonTableI"]]$dependOn(c("D", "total", "Attributes"))
      jaspResults[["NelsonTableMR"]]$dependOn(c("D", "total", "Attributes"))
    }
  }

  #Report
  if (options[["AReport"]] && is.null(jaspResults[["AReport"]]) && ready) {
    jaspResults[["AReport"]] <- createJaspContainer(title = gettextf("Report for Attribute Control Charts"))
    jaspResults[["AReport"]]$dependOn(c("AReport", "ATitle", "AName", "AOperator", "AID", "AMisc", "AAppraiser", "AMeasurement", "ASize", "ATime", "AFrequency",
                                        "D", "total", "Attributes", "TypeDefects", "TypeDefectives"))
    Report <- jaspResults[["AReport"]]

    Report[["Report"]] <- .AReport(ccTitle = options$ATitle, ccName = options$AName,
                                 ccOperator = options$AOperator, ccID = options$AID, ccMisc = options$AMisc, ccAppraiser = options$AAppraiser,
                                 ccMeasurement = options$AMeasurement, ccSize = options$ASize, ccTime = options$ATime, ccFrequency = options$AFrequency)

    Report[["Plot"]] <- createJaspPlot(width = 1000, height = 800, position = 2)
    if (options$Attributes == "Defectives" & options$TypeDefectives == "pchart")
      PlotReport <- .Pchart(dataset = dataset, options = options)$p
    else if (options$Attributes == "Defectives" & options$TypeDefectives == "npchart")
      PlotReport <- .NPchart(dataset = dataset, options = options)$p
    else if (options$Attributes == "Defectives" & options$TypeDefectives == "Laneyprimechart")
      PlotReport <- .LanyP(dataset = dataset, options = options)$p
    else if (options$Attributes == "Defects" & options$TypeDefects == "cchart")
      PlotReport <- .Cchart(dataset = dataset, options = options)$p
    else if (options$Attributes == "Defects" & options$TypeDefects == "uchart")
      PlotReport <- .Uchart(dataset = dataset, options = options)$p
    else if (options$Attributes == "Defects" & options$TypeDefects == "Laneychart")
      PlotReport <- .LanyU(dataset = dataset, options = options)$p
    else if (options$Attributes == "ImR")
      PlotReport <- .Ichart_attributes(dataset = dataset, options = options)$p

    Report[["Plot"]]$plotObject <- PlotReport
  }
}

.Pchart <- function(dataset, options) {
  data1 <- data.frame(D = dataset[, options$D], sample = dataset[, options$total])
  sixsigma <- with(data1, qcc::qcc(D, sample, type = "p", plot = FALSE))
  sample <- data1$sample
  D <- data1$D
  subgroups = c(1:length(sixsigma$statistics))
  data_plot <- data.frame(subgroups = subgroups, P = sixsigma$statistics)
  center <- sixsigma$center
  UCL <- sixsigma$limits[,2]
  LCL <- sixsigma$limits[,1]
  if (min(sample)/max(sample) >= 0.75){
    p.hat <- mean(sixsigma$statistics)
    n.mean <- mean(sample)
    UCL <- p.hat + 3 * (sqrt(p.hat*(1 - p.hat))) / (sqrt(n.mean))
    LCL <- p.hat - 3 * (sqrt(p.hat*(1 - p.hat))) / (sqrt(n.mean))
  }
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data_plot$P ,UCL))
  yLimits <- range(c(0,yBreaks * 1.2))
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1]) + 0.5
  else
    xBreaks <- c(subgroups) + 0.5
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center),
    l = c(
      gettextf("CL = %g", round(center, 4))
    )
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = center, color = "green") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (length(unique(UCL)) == 1){
    dfLabel <- data.frame(
      x = max(xLimits) * .95,
      y = c(center, UCL, LCL),
      l = c(
        gettextf("CL = %g", round(center, 4)),
        gettextf("UCL = %g",   round(UCL, 5)),
        gettextf("LCL = %g",   round(LCL, 5))
      )
    )

    p <- p +
      ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks -0.5, limits = range(xLimits)) +
      jaspGraphs::geom_line(data = data_plot, ggplot2::aes(x = subgroups, y = P),color = "blue") +
      jaspGraphs::geom_point(data = data_plot, ggplot2::aes(x = subgroups, y = P),size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue'))
  } else{
    n <- length(UCL)
    p <- p + ggplot2::geom_step(ggplot2::aes(x = subgroups, y = UCL, color = "red"), size = 1.5, linetype = "F1") +
      jaspGraphs::geom_line(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),color = "blue") +
      jaspGraphs::geom_point(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue')) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = xBreaks - 0.5) +
      ggplot2::geom_step(ggplot2::aes(x = subgroups, y = LCL, color = "red"), size = 1.5, linetype = "F1") +
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = UCL[n], color = "red"), size = 1.5)+
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = LCL[n], color = "red"), size = 1.5)

  }

  return(list(p = p, sixsigma = sixsigma))
}
.NPchart <- function(dataset, options) {
  .Check_equal_samples(dataset, options)

  data1 <- data.frame(D = dataset[, options$D], sample = dataset[, options$total])
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, D = data1$D)

  sixsigma <- with(data1, qcc::qcc(D, sample, type = "np", plot = FALSE))
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data1$D ,UCL))
  yLimits <- range(yBreaks * 1.2)
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 4)),
      gettextf("UCL = %g",   round(UCL, 5)),
      gettextf("LCL = %g",   round(LCL, 5))
    )
  )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = D)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Number of defectives"),limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma, chart = "c")$red_points, 'red', 'blue')) +
    jaspGraphs::themeJaspRaw()

  return(list(p = p, sixsigma = sixsigma))
}
.Cchart <- function(dataset, options) {
  .Check_equal_samples(dataset, options)

  data1 <- data.frame(D = dataset[, options$D], sample = dataset[, options$total])
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, D = data1$D)
  sixsigma <- with(data1, qcc::qcc(D, sample, type = "c", plot = FALSE))
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data1$D ,UCL))
  yLimits <- range(yBreaks * 1.2)
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 4)),
      gettextf("UCL = %g",   round(UCL, 5)),
      gettextf("LCL = %g",   round(LCL, 5))
    )
  )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = D)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("D") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma, chart = "c")$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()


  return(list(p = p, sixsigma = sixsigma))
}

.Uchart <- function(dataset, options) {
  data1 <- data.frame(D = dataset[, options$D], sample = dataset[, options$total])
  data1$P <- data1$D/data1$sample
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, P = data1$P)

  sixsigma <- with(data1, qcc::qcc(D, sample, type = "u", plot = FALSE))
  center <- sixsigma$center
  UCL <- sixsigma$limits[,2]
  LCL <- sixsigma$limits[,1]
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data_plot$P ,UCL))
  yLimits <- range(c(0,yBreaks * 1.2))
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1]) + 0.5
  else
    xBreaks <- c(subgroups) + 0.5
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center),
    l = c(
      gettextf("CL = %g", round(center, 4))
    )
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = center, color = "green") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (length(unique(UCL)) == 1){
    dfLabel <- data.frame(
      x = max(xLimits) * 0.95,
      y = c(center, UCL, LCL),
      l = c(
        gettextf("CL = %g", round(center, 4)),
        gettextf("UCL = %g",   round(UCL, 5)),
        gettextf("LCL = %g",   round(LCL, 5))
      )
    )

    p <- p +
      ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks -0.5, limits = range(xLimits)) +
      jaspGraphs::geom_line(data = data_plot, ggplot2::aes(x = subgroups, y = P),color = "blue") +
      jaspGraphs::geom_point(data = data_plot, ggplot2::aes(x = subgroups, y = P),size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue'))
  } else{
    n <- length(UCL)
    p <- p + ggplot2::geom_step(ggplot2::aes(x = subgroups, y = UCL, color = "red"), size = 1.5, linetype = "F1") +
      jaspGraphs::geom_line(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),color = "blue") +
      jaspGraphs::geom_point(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue')) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = xBreaks - 0.5) +
      ggplot2::geom_step(ggplot2::aes(x = subgroups, y = LCL, color = "red"), size = 1.5, linetype = "F1") +
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = UCL[n], color = "red"), size = 1.5)+
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = LCL[n], color = "red"), size = 1.5)

  }

  return(list(p = p, sixsigma = sixsigma))
}

##Imr for attributes
.Ichart_attributes <- function(dataset, options) {
  data1 <- data.frame(D = dataset[, options$D], sample = dataset[, options$total])
  data1$P <- data1$D/data1$sample
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, P = data1$P)

  sixsigma_I <- qcc::qcc(data_plot$P, type ='xbar.one', plot=FALSE)
  center <- sixsigma_I$center
  UCL <- max(sixsigma_I$limits)
  LCL <- min(sixsigma_I$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL))
  yLimits <- range(yBreaks * 1.2)
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 4)),
      gettextf("UCL = %g",   round(UCL, 5)),
      gettextf("LCL = %g",   round(LCL, 5))
    )
  )

  p1 <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = P)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma_I, chart = "i")$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  #data
  data <- data.frame(process = dataset[, options$D] / dataset[, options$total])
  xmr.raw.r <- matrix(cbind(data$process[1:length(data$process)-1], data$process[2:length(data$process)]), ncol=2)
  sixsigma_R <- qcc::qcc(xmr.raw.r, type="R", plot = FALSE)
  subgroups = c(1:length(sixsigma_R$statistics))
  data_plot <- data.frame(subgroups = subgroups, data = sixsigma_R$statistics)
  center <- sixsigma_R$center
  UCL <- max(sixsigma_R$limits)
  LCL <- min(sixsigma_R$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL))
  yLimits <- range(yBreaks * 1.2)
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks) * 1.15)
  Xlabels <- xBreaks + 1
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 4)),
      gettextf("UCL = %g",   round(UCL, 5)),
      gettextf("LCL = %g",   round(LCL, 5))
    )
  )

  p2 <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = data)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Moving Range") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = Xlabels) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma_R, chart = "c")$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  p3 <-  jaspGraphs::ggMatrixPlot(plotList = list(p1, p2), layout = matrix(1:2, 2), removeXYlabels= "x")

  return(list(p = p3, sixsigma_I = sixsigma_I, sixsigma_R = sixsigma_R))
}

### Lanys charts
.LanyU <- function(dataset, options) {
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
  LCL <- ifelse(center - 3*sqrt(center/data1$sample) * sigma < 0, 0, center - 3*sqrt(center/data1$sample) * sigma)
  UCL <- center + 3*sqrt(center/data1$sample) * sigma
  sixsigma <- list(statistics = data1$P, limits = data.frame(LCL, UCL), center = center)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data_plot$P ,UCL))
  yLimits <- range(c(0,yBreaks * 1.2))
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1]) + 0.5
  else
    xBreaks <- c(subgroups) + 0.5
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center),
    l = c(
      gettextf("CL = %g", round(center, 4))
    )
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = center, color = "green") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (length(UCL) == 1){
    dfLabel <- data.frame(
      x = max(xLimits) * .95,
      y = c(center, UCL, LCL),
      l = c(
        gettextf("CL = %g", round(center, 4)),
        gettextf("UCL = %g",   round(UCL, 5)),
        gettextf("LCL = %g",   round(LCL, 5))
      )
    )

    p <- p +
      ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks -0.5, limits = range(xLimits)) +
      jaspGraphs::geom_line(data = data_plot, ggplot2::aes(x = subgroups, y = P),color = "blue") +
      jaspGraphs::geom_point(data = data_plot, ggplot2::aes(x = subgroups, y = P),size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue'))
  } else{
    n <- length(UCL)
    p <- p + ggplot2::geom_step(ggplot2::aes(x = subgroups, y = UCL, color = "red"), size = 1.5, linetype = "F1") +
      jaspGraphs::geom_line(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),color = "blue") +
      jaspGraphs::geom_point(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue')) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = xBreaks - 0.5) +
      ggplot2::geom_step(ggplot2::aes(x = subgroups, y = LCL, color = "red"), size = 1.5, linetype = "F1") +
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = UCL[n], color = "red"), size = 1.5)+
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = LCL[n], color = "red"), size = 1.5)

  }

  return(list(p = p, sixsigma = sixsigma))
}

.LanyP <- function(dataset, options) {
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
  LCL <- ifelse(center - 3*sqrt(center*(1 - center)/data1$sample) * sigma < 0, 0, center - 3*sqrt(center*(1 - center)/data1$sample) * sigma)
  UCL <- center + 3*sqrt(center*(1 - center)/data1$sample) * sigma
  sixsigma <- list(statistics = data1$P, limits = data.frame(LCL, UCL), center = center)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data_plot$P ,UCL))
  yLimits <- range(c(0,yBreaks * 1.2))
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1]) + 0.5
  else
    xBreaks <- c(subgroups) + 0.5
  xLimits <- c(1,max(xBreaks) * 1.15)
  dfLabel <- data.frame(
    x = max(xLimits) * 0.95,
    y = c(center),
    l = c(
      gettextf("CL = %g", round(center, 4))
    )
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = center, color = "green") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (length(UCL) == 1){
    dfLabel <- data.frame(
      x = max(xLimits) * .95,
      y = c(center, UCL, LCL),
      l = c(
        gettextf("CL = %g", round(center, 4)),
        gettextf("UCL = %g",   round(UCL, 5)),
        gettextf("LCL = %g",   round(LCL, 5))
      )
    )

    p <- p +
      ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks -0.5, limits = range(xLimits)) +
      jaspGraphs::geom_line(data = data_plot, ggplot2::aes(x = subgroups, y = P),color = "blue") +
      jaspGraphs::geom_point(data = data_plot, ggplot2::aes(x = subgroups, y = P),size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue'))
  } else{
    n <- length(UCL)
    p <- p + ggplot2::geom_step(ggplot2::aes(x = subgroups, y = UCL, color = "red"), size = 1.5, linetype = "F1") +
      jaspGraphs::geom_line(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),color = "blue") +
      jaspGraphs::geom_point(ggplot2::aes(x = subgroups  + 0.5, y = data_plot$P),size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue')) +
      ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits), labels = xBreaks - 0.5) +
      ggplot2::geom_step(ggplot2::aes(x = subgroups, y = LCL, color = "red"), size = 1.5, linetype = "F1") +
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = UCL[n], color = "red"), size = 1.5)+
      ggplot2::geom_step(ggplot2::aes(x = c(n, n + 1), y = LCL[n], color = "red"), size = 1.5)

  }

  return(list(p = p, sixsigma = sixsigma))
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

.AReport <- function(ccTitle = "", ccName = "", ccOperator = "", ccID = "", ccMisc = "" , ccAppraiser = "", ccMeasurement = "", ccSize = "",
                     ccTime = "", ccFrequency = ""){

  Report <- createJaspContainer("Report")

  if (ccTitle == ""){
    title <- "Measurement"
  }else{
    title <- ccTitle
  }
  name <- gettextf("Service name: %s", ccName)
  operator <- gettextf("Operator: %s", ccOperator)
  ID <- gettextf("Identification: %s", ccID)
  Appraiser <- gettextf("Appraiser: %s", ccAppraiser)
  Measurement <- gettextf("Measurement system: %s", ccMeasurement)
  Size <- gettextf("Name of Size: %s", ccSize)
  Time <- gettextf("Time: %s", ccTime)
  Frequency <- gettextf("Frequency: %s", ccFrequency)


  text1 <- c(name, operator)
  text2 <- c(ID, Appraiser)
  text3 <- c(Measurement, Time)
  text4 <- c(Size, Frequency)

  matrixPlot <- createJaspPlot(width = 1200, height = 1000, position = 1)
  plotMat <- matrix(list(), 2, 2, T)
  plotMat[[1, 1]] <- .ggplotWithText(text1)
  plotMat[[1, 2]] <- .ggplotWithText(text2)
  plotMat[[2, 1]] <- .ggplotWithText(text3)
  plotMat[[2, 2]] <- .ggplotWithText(text4)

  p <- jaspGraphs::ggMatrixPlot(plotMat, topLabels = c(gettextf("Control Charts Report for %s", title), ""))
  matrixPlot$plotObject <- p

  return(matrixPlot)
}
