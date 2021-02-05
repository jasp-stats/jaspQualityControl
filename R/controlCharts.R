controlCharts <- function(jaspResults, dataset, options){
  variables <- options$variables
  total <- options$total
  D <- options$D
  numeric_variables <- c(variables, total, D)
  numeric_variables  <- numeric_variables[numeric_variables != ""]
  
  dataset         <- .readDataSetToEnd(columns.as.numeric = numeric_variables, exclude.na.listwise = numeric_variables)
  #Checking for errors in the dataset
  .hasErrors(dataset, type = c('observations', 'infinity', 'missingValues'),
             all.target = options$variables,
             observations.amount = c(' < 2'), exitAnalysisIfErrors = TRUE)
  
  jaspResults[["intro"]] <- createJaspHtml(gettext("Select one of the control charts to from the interface."))
  jaspResults[["intro"]]$position <- 0
#X bar chart
  if(options$Xbarchart && is.null(jaspResults[["Xbarchart"]])){
    jaspResults[["XbarPlot"]] <- createJaspPlot(title =  gettext("X bar chart"), width = 700, height = 350)
    jaspResults[["XbarPlot"]]$dependOn(c("Xbarchart", "variables"))
    jaspResults[["XbarPlot"]]$position <- 11
    XbarPlot <- jaspResults[["XbarPlot"]]
    XbarPlot$plotObject <- .XbarchartNoId(dataset = dataset[, options$variables], options = options)
  }
#R Chart
  if(options$Rchart && is.null(jaspResults[["Rchart"]])){
    jaspResults[["RPlot"]] <- createJaspPlot(title =  gettext("R chart"), width = 700, height = 350)
    jaspResults[["RPlot"]]$dependOn(c("Rchart", "variables"))
    jaspResults[["RPlot"]]$position <- 11
    RPlot<- jaspResults[["RPlot"]]
    RPlot$plotObject <- .RchartNoId(dataset = dataset[, options$variables], options = options)
  }
#S Chart
  if(options$Schart && is.null(jaspResults[["Schart"]])){
    jaspResults[["SPlot"]] <- createJaspPlot(title = gettext("S Chart"), width = 700, height = 350)
    jaspResults[["SPlot"]]$dependOn(c("Schart", "variables"))
    jaspResults[["SPlot"]]$position <- 11
    SPlot<- jaspResults[["SPlot"]]
    SPlot$plotObject <- .Schart(dataset = dataset, options = options)
  }
#ImR chart
  if(options$ImRchart){
    if(is.null(jaspResults[["ImRchart"]])){
      jaspResults[["ImRchart"]] <- createJaspContainer(gettext("ImR charts"))
      jaspResults[["ImRchart"]]$dependOn(c("ImRchart", "variables"))
      jaspResults[["ImRchart"]]$position <- 11
    }
    
    ImRplot <- jaspResults[["ImRchart"]]
    
    for(var in variables){
      ImRplot[[var]] <- .ImRchart(dataset = dataset, options = options, variable = var)
    }
  }
#P chart
  if(options$Defectivescharts){
    if(options$TypeDefectives == "pchart" && is.null(jaspResults[["pchart"]])){
      jaspResults[["PchartPlot"]] <- createJaspPlot(title =  gettext("P chart"), width = 700, height = 350)
      jaspResults[["PchartPlot"]]$dependOn(c("total", "D", "Defectivescharts", "TypeDefectives"))
      jaspResults[["PchartPlot"]]$position <- 11
      PPlot <- jaspResults[["PchartPlot"]]
      PPlot$plotObject <- .Pchart(dataset = dataset, options = options)
    }
  }
#NP chart
if(options$Defectivescharts){
  if(options$TypeDefectives == "npchart" && is.null(jaspResults[["npchart"]])){
    jaspResults[["NPchartPlot"]] <- createJaspPlot(title =  gettext("NP chart"), width = 700, height = 350)
    jaspResults[["NPchartPlot"]]$dependOn(c("total", "D", "Defectivescharts", "TypeDefectives"))
    jaspResults[["NPchartPlot"]]$position <- 11
    PPlot <- jaspResults[["NPchartPlot"]]
    PPlot$plotObject <- .NPchart(dataset = dataset, options = options)
  }
}
#Cchart  
  if(options$Defectscharts){
    if(options$TypeDefects == "cchart" && is.null(jaspResults[["Cchart"]])){
      jaspResults[["CchartPlot"]] <- createJaspPlot(title =  gettext("C chart"), width = 700, height = 350)
      jaspResults[["CchartPlot"]]$dependOn(c("D", "Defectscharts", "TypeDefects","total"))
      jaspResults[["CchartPlot"]]$position <- 11
      PPlot <- jaspResults[["CchartPlot"]]
      PPlot$plotObject <- .Cchart(dataset = dataset, options = options)
    }
  }
#Uchart  
  if(options$Defectscharts){
    if(options$TypeDefects == "uchart" && is.null(jaspResults[["Uchart"]])){
      jaspResults[["UchartPlot"]] <- createJaspPlot(title =  gettext("U chart"), width = 700, height = 350)
      jaspResults[["UchartPlot"]]$dependOn(c("D", "total","Defectscharts", "TypeDefects"))
      jaspResults[["UchartPlot"]]$position <- 11
      PPlot <- jaspResults[["UchartPlot"]]
      PPlot$plotObject <- .Uchart(dataset = dataset, options = options)
    }
  }
  #Laney U chart 
  if(options$Defectscharts){
    if(options$TypeDefects == "Laneychart" && is.null(jaspResults[["LaneyUchart"]])){
      jaspResults[["LaneyUPlot"]] <- createJaspPlot(title = "Laney U' chart", width = 700, height = 350)
      jaspResults[["LaneyUPlot"]]$dependOn(c("D", "total","Defectscharts", "TypeDefects"))
      jaspResults[["LaneyUPlot"]]$position <- 11
      PPlot <- jaspResults[["LaneyUPlot"]]
      PPlot$plotObject <- .LanyU(dataset = dataset, options = options)
    }
  }
  #Laney P chart 
  if(options$Defectivescharts){
    if(options$TypeDefectives == "Laneyprimechart" && is.null(jaspResults[["LaneyPchart"]])){
      jaspResults[["LaneyPPlot"]] <- createJaspPlot(title =  gettext("Laney P' chart"), width = 700, height = 350)
      jaspResults[["LaneyPPlot"]]$dependOn(c("total", "D", "Defectivescharts", "TypeDefectives"))
      jaspResults[["LaneyPPlot"]]$position <- 11
      PPlot <- jaspResults[["LaneyPPlot"]]
      PPlot$plotObject <- .LanyP(dataset = dataset, options = options)
    }
  }
  #ImRchart for attributes
  if(options$ImRchart2 && is.null(jaspResults[["ImRchart2"]])){
    jaspResults[["IMRPlot"]] <- createJaspPlot(title = "ImR chart", width = 900, height = 400)
    jaspResults[["IMRPlot"]]$dependOn(c("D", "total","ImRchart2"))
    jaspResults[["IMRPlot"]]$position <- 11
    PPlot <- jaspResults[["IMRPlot"]]
    PPlot$plotObject <- .ImRchart_attributes(dataset = dataset, options = options)
  }
}

#Functions for control charts 
.Schart <- function(dataset, options){
  ready <- (length(options$variables) > 1)
  if (!ready)
    return()
  
  data1 <- dataset[, options$variables]
  Stdv <- apply(data1, 1, function(x) sd(x))
  subgroups <- c(1:length(Stdv))
  data_plot <- data.frame(subgroups = subgroups, Stdv = Stdv)
  
  sixsigma <- qcc::qcc(data1, type ='S', plot=FALSE)
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, data_plot$Stdv, UCL + 1))
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups)
  xLimits <- range(xBreaks + 3)
  dfLabel <- data.frame(
    x = max(xLimits),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = Stdv)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$Stdv > UCL | data_plot$Stdv < LCL, 'red', 'gray')) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(name = "Subgroup Standard Deviation" ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = 'Subgroup', breaks = xBreaks, limits = c(min(xLimits), max(xLimits) + 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return(p)
}
.ImRchart <- function(dataset, options, variable){
  title <- gettextf("Variable: %s", variable )
  ppPlot <- createJaspPlot(width = 700, height = 350, title = title)
  ppPlot$dependOn(optionContainsValue = list(variables = variable))
  
  #data
  data <- data.frame(process = dataset[[.v(variable)]])
  subgroups <- c(1:length(data$process))
  sixsigma <- qcc::qcc(data$process, type ='xbar.one', plot=FALSE)
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL - 1, UCL + 1))
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups)
  xLimits <- range(xBreaks + 3)
  dfLabel <- data.frame(
    x = max(xLimits),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = subgroups, y = process)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data$process > UCL | data$process < LCL, 'red', 'gray')) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(name = "Value" ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = 'Observations', breaks = xBreaks, limits = c(min(xLimits), max(xLimits) + 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  ppPlot$plotObject <-  p
  return(ppPlot)
}
.Pchart <- function(dataset, options){
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
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups)
  xLimits <- range(xBreaks + 3)
  dfLabel <- data.frame(
    x = max(xLimits),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )
  
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = P)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'gray')) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(name = "Subgroups' Proportions" ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = 'Subgroups', breaks = xBreaks, limits = c(min(xLimits), max(xLimits) + 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return(p)
}
.NPchart <- function(dataset, options){
  ready <- options$D != "" && options$total != ""
  if (!ready)
    return()
  
  data1 <- data.frame(D = dataset[, options$D], sample = dataset[, options$total])
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, D = data1$D)
  
  sixsigma <- with(data1, qcc::qcc(D, sample, type = "np", plot = FALSE))
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data1$D ,UCL + 0.1))
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups)
  xLimits <- range(xBreaks + 3)
  dfLabel <- data.frame(
    x = max(xLimits),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )
  
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = D)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$D > UCL | data_plot$D < LCL, 'red', 'gray')) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(name = "Subgroups' Proportions" ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = 'Subgroups', breaks = xBreaks, limits = c(min(xLimits), max(xLimits) + 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return(p)
}
.Cchart <- function(dataset, options){
  ready <- options$D != "" && options$total != ""
  if (!ready)
    return()
  
  data1 <- data.frame(D = dataset[, options$D], sample = dataset[, options$total])
  subgroups <- 1:nrow(data1)
  data_plot <- data.frame(subgroups = subgroups, D = data1$D)
  
  sixsigma <- with(data1, qcc::qcc(D, sample, type = "c", plot = FALSE))
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL,data1$D ,UCL + 0.1))
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups)
  xLimits <- range(xBreaks + 3)
  dfLabel <- data.frame(
    x = max(xLimits),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )
 
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = D)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$D > UCL | data_plot$D < LCL, 'red', 'gray')) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(name = "Subgroups' Proportions" ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = 'Subgroups', breaks = xBreaks, limits = c(min(xLimits), max(xLimits) + 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  return(p)
}
.Uchart <- function(dataset, options){
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
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups)
  xLimits <- range(xBreaks + 3)
  dfLabel <- data.frame(
    x = max(xLimits),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )
  
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = P)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'gray')) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(name = "Subgroups' Proportions" ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = 'Subgroups', breaks = xBreaks, limits = c(min(xLimits), max(xLimits) + 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return(p)
}

##Imr for attributes 
.ImRchart_attributes <- function(dataset, options){
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
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(subgroups)
  xLimits <- range(xBreaks + 3)
  dfLabel <- data.frame(
    x = max(xLimits),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )
  
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = P)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'gray')) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(name = "Subgroups' Proportions" ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = 'Subgroups', breaks = xBreaks, limits = c(min(xLimits), max(xLimits) + 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return(p)
}

### Lanys charts 
.LanyU <- function(dataset, options){
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
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(data_plot$subgroups)
  xLimits <- range(xBreaks + 3)
  dfLabel <- data.frame(
    x = max(xLimits),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )
  
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = P)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'gray')) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(name = "Sample count per unit" ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = 'Subgroups', breaks = xBreaks, limits = c(min(xLimits), max(xLimits) + 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return(p)
}

.LanyP <- function(dataset, options){
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
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(data_plot$subgroups)
  xLimits <- range(xBreaks + 3)
  dfLabel <- data.frame(
    x = max(xLimits),
    y = c(center, UCL, LCL),
    l = c(
      gettextf("CL = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
    )
  )
  
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = subgroups, y = P)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point(size = 4, fill = ifelse(data_plot$P > UCL | data_plot$P < LCL, 'red', 'gray')) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(name = "Sample count per unit" ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = 'Subgroups', breaks = xBreaks, limits = c(min(xLimits), max(xLimits) + 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return(p)
}

