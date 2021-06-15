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
    if (options$TypeDefectives == "pchart" && is.null(jaspResults[["PchartPlot"]])) {
      jaspResults[["PchartPlot"]] <- createJaspPlot(title =  gettext("P Control Chart"), width = 1200, height = 500)
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
      jaspResults[["NPchartPlot"]] <- createJaspPlot(title =  gettext("NP Control Chart"), width = 1200, height = 500)
      jaspResults[["NPchartPlot"]]$dependOn(c("total", "D", "Attributes", "TypeDefectives"))
      jaspResults[["NPchartPlot"]]$position <- 1
      NPchart <- .NPchart(dataset = dataset, options = options)
      jaspResults[["NPchartPlot"]]$plotObject <- NPchart$p

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer(gettext(""))
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = NPchart$sixsigma, name = "NP")
        jaspResults[["NelsonTable"]]$position <- 2
        jaspResults[["NelsonTable"]]$dependOn(c("total", "D", "Attributes", "TypeDefectives"))
      }
    }

    #Laney P chart
    if (options$TypeDefectives == "Laneyprimechart" && is.null(jaspResults[["LaneyPPlot"]])) {
      jaspResults[["LaneyPPlot"]] <- createJaspPlot(title =  gettext("Laney P' Control Chart"), width = 1200, height = 500, position = 1)
      Lanychart <- .LanyP(dataset = dataset, options = options)
      jaspResults[["LaneyPPlot"]]$plotObject <- Lanychart$p
      jaspResults[["LaneyPPlot"]]$dependOn(c("total", "D", "Attributes", "TypeDefectives"))

      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspPlot(title =  gettext(""), position = 2)
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = Lanychart$sixsigma, name = "Laney P'")
        jaspResults[["NelsonTable"]]$dependOn(c("total", "D", "Attributes", "TypeDefectives"))
      }
    }
  }

  if (options$Attributes == "Defects" && options$D != "" && options$total != "") {
    #Cchart
    if (options$TypeDefects == "cchart" && is.null(jaspResults[["CchartPlot"]])) {
      jaspResults[["CchartPlot"]] <- createJaspPlot(title =  gettext("C Control Chart"), width = 1200, height = 500)
      jaspResults[["CchartPlot"]]$dependOn(c("D", "Attributes", "TypeDefects","total"))
      jaspResults[["CchartPlot"]]$position <- 1
      Cchart <- .Cchart(dataset = dataset, options = options)
      jaspResults[["CchartPlot"]]$plotObject <- Cchart$p

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer(gettext(""))
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = Cchart$sixsigma, name = "C")
        jaspResults[["NelsonTable"]]$dependOn(c("total", "D", "Attributes", "TypeDefects"))
        jaspResults[["NelsonTable"]]$position <- 2
      }
    }
    #Uchart
    if (options$TypeDefects == "uchart" && is.null(jaspResults[["UchartPlot"]])) {
      jaspResults[["UchartPlot"]] <- createJaspPlot(title =  gettext("U Control Chart"), width = 1200, height = 500)
      jaspResults[["UchartPlot"]]$dependOn(c("D", "total","Attributes", "TypeDefects"))
      jaspResults[["UchartPlot"]]$position <- 1
      Uchart <- .Uchart(dataset = dataset, options = options)
      jaspResults[["UchartPlot"]]$plotObject <- Uchart$p

      # Nelson tests tables
      if (is.null(jaspResults[["NelsonTable"]])) {
        jaspResults[["NelsonTable"]] <- createJaspContainer(gettext(""))
        jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = Uchart$sixsigma, name = "U")
        jaspResults[["NelsonTable"]]$dependOn(c("total", "D", "Attributes", "TypeDefects"))
        jaspResults[["NelsonTable"]]$position <- 2
      }
    }
    #Laney U chart
    if (options$TypeDefects == "Laneychart" && is.null(jaspResults[["LaneyUPlot"]])) {
      jaspResults[["LaneyUPlot"]] <- createJaspPlot(title = gettext("Laney U' Control Chart"), width = 1200, height = 500, position = 1)
      jaspResults[["LaneyUPlot"]]$dependOn(c("D", "total","Attributes", "TypeDefects"))
      LanyUchart <- .LanyU(dataset = dataset, options = options)
      jaspResults[["LaneyUPlot"]]$plotObject <- LanyUchart$p

      jaspResults[["NelsonTable"]] <- createJaspContainer(title =  gettext(""), position = 2)
      jaspResults[["NelsonTable"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = LanyUchart$sixsigma, name = "Laney U'")
      jaspResults[["NelsonTable"]]$dependOn(c("D", "total","Attributes", "TypeDefects"))
    }
  }
  #ImRchart for attributes
  if (options$Attributes == "ImR" && length(options$D) > 0) {
    jaspResults[["IPlotA"]] <- createJaspPlot(title = gettext("Individual and Moving Range Control Charts"), width = 1200, height = 500, position = 1)
    IMRchart <- .Ichart_attributes(dataset = dataset, options = options)
    jaspResults[["IPlotA"]]$plotObject <- IMRchart$p
    jaspResults[["IPlotA"]]$dependOn(c("D", "total","ImRchart2", "Attributes"))

    # Nelson tests tables
    if (is.null(jaspResults[["NelsonTableI"]]) & is.null(jaspResults[["NelsonTabeMR"]])) {
      jaspResults[["NelsonTableI"]] <- createJaspContainer(gettext("Indvidual"), position = 2)
      jaspResults[["NelsonTabeMR"]] <- createJaspContainer(gettext("Range"), position = 3)

      jaspResults[["NelsonTableI"]] <- .NelsonTable(dataset = dataset, options = options, type = "xbar.one", sixsigma = IMRchart$sixsigma_I, name = "Individual")
      jaspResults[["NelsonTableMR"]] <- .NelsonTable(dataset = dataset, options = options, sixsigma = IMRchart$sixsigma_R, name = "Moving Range")

      jaspResults[["NelsonTableI"]]$dependOn(c("D", "total","ImRchart2", "Attributes"))
      jaspResults[["NelsonTableMR"]]$dependOn(c("D", "total","ImRchart2", "Attributes"))
    }

  }
}

.Pchart <- function(dataset, options) {
  ready <- options$D != "" && options$total != ""
  if (!ready)
    return()

  data1 <- data.frame(D = dataset[, options$D], sample = dataset[, options$total])
  sixsigma <- with(data1, qcc::qcc(D, sample, type = "p", plot = FALSE))
  subgroups = c(1:length(sixsigma$statistics))
  data_plot <- data.frame(subgroups = subgroups, P = sixsigma$statistics)
  center <- sixsigma$center
  UCL <- sixsigma$limits[,2]
  LCL <- sixsigma$limits[,1]
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data1$P ,UCL + 0.1))
  yLimits <- range(yBreaks)
  if (length(subgroups) > 60)
    xBreaks <- c(1,jaspGraphs::getPrettyAxisBreaks(subgroups)[-1])
  else
    xBreaks <- c(subgroups)
  xLimits <- c(1,max(xBreaks) + 2.5)
  dfLabel <- data.frame(
    x = max(xLimits - 1),
    y = c(center),
    l = c(
      gettextf("CL = %g", round(center, 4))
    )
  )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = P)) +
    ggplot2::geom_hline(yintercept =  center, color = "green") +
    ggplot2::geom_step(ggplot2::aes(x = subgroups, y = UCL, color = "red"), size = 1.5, linetype = "F1") +
    ggplot2::geom_step(ggplot2::aes(x = subgroups, y = LCL, color = "red"), size = 1.5, linetype = "F1") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'blue')) +
    jaspGraphs::themeJaspRaw()

  return(list(p = p, sixsigma = sixsigma))
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

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = D)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("D"),limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma, chart = "c")$red_points, 'red', 'blue')) +
    jaspGraphs::themeJaspRaw()

  return(list(p = p, sixsigma = sixsigma))
}
.Cchart <- function(dataset, options) {
  ready <- options$D != "" && options$total != ""
  if (!ready)
    return()

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

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = P)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Proportion") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Sample'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma, chart = "c")$red_points, 'red', 'blue')) +
    jaspGraphs::themeJaspRaw()

  return(list(p = p, sixsigma = sixsigma))
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

  sixsigma_I <- qcc::qcc(data_plot$P, type ='xbar.one', plot=FALSE)
  center <- sixsigma_I$center
  UCL <- max(sixsigma_I$limits)
  LCL <- min(sixsigma_I$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL - 0.1, UCL + 0.1))
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
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL + 0.1))
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

  p2 <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = data)) +
    ggplot2::geom_hline(yintercept =  center, color = 'green') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE, size = 4.5) +
    ggplot2::scale_y_continuous(name =  gettext("Moving Range") ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name =  gettext('Observation'), breaks = xBreaks, limits = range(xLimits)) +
    jaspGraphs::geom_line(color = "blue") +
    jaspGraphs::geom_point(size = 4, fill = ifelse(NelsonLaws(sixsigma_R, chart = "c")$red_points, 'red', 'blue')) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  p3 <-  jaspGraphs::ggMatrixPlot(plotList = list(p1, p2), layout = matrix(1:2, 2), removeXYlabels= "x")

  return(list(p = p3, sixsigma_I = sixsigma_I, sixsigma_R = sixsigma_R))
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
  sixsigma <- list(statistics = data1$P, limits = data.frame(LCL, UCL), center = center)

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL - 0.1, UCL + 0.1))
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

  return(list(p = p, sixsigma = sixsigma))
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
  sixsigma <- list(statistics = data1$P, limits = data.frame(LCL, UCL), center = center)

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL - 0.1, UCL + 0.1))
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
