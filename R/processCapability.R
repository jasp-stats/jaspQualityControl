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

processCapability <- function(jaspResults, dataset, options){

  diameter <- unlist(options$diameter)
  subgroupsName <- options$subgroups
  makeSubgroups <- subgroupsName != ""

  if(length(diameter) == 0)
	return()

  if (is.null(dataset)) {
    if (makeSubgroups) {
      dataset         <- .readDataSetToEnd(columns.as.numeric = diameter, columns.as.factor = subgroupsName)
      dataset.factors <- .readDataSetToEnd(columns = diameter, columns.as.factor = subgroupsName)
    } else {
      dataset         <- .readDataSetToEnd(columns.as.numeric = diameter)
      dataset.factors <- .readDataSetToEnd(columns = diameter)
    }
  }
  
  # X-bar & Range Control Chart (by Tom)
  if(options$controlCharts & is.null(jaspResults[["controlCharts"]])) {
    jaspResults[["XbarPlot"]] <- createJaspPlot(title = "X bar chart", width = 1100, height = 400)
    jaspResults[["XbarPlot"]]$dependOn(c("XbarRchart"))
    jaspResults[["XbarPlot"]]$position <- 1
    XbarPlot <- jaspResults[["XbarPlot"]]
    XbarPlot$plotObject <- .XbarchartNoId(dataset = dataset, options = options)
    XbarPlot$dependOn(optionContainsValue= list(diameter=diameter))
    
    jaspResults[["RPlot"]] <- createJaspPlot(title = "R chart", width = 1100, height= 400)
    jaspResults[["RPlot"]]$dependOn(c("XbarRchart"))
    jaspResults[["RPlot"]]$position <- 2
    RPlot<- jaspResults[["RPlot"]]
    RPlot$plotObject <- .RchartNoId(dataset = dataset, options = options)
    RPlot$dependOn(optionContainsValue= list(diameter=diameter))
  }
  
  # Histogram 
  if (options$histogram) {
    if(is.null(jaspResults[["histogram"]])) {
      jaspResults[["histogram"]] <- createJaspContainer(gettext("Histogram"))
      jaspResults[["histogram"]]$dependOn("histogram")
      jaspResults[["histogram"]] <- .histogram(options, dataset, diameter, subgroupsName)
      jaspResults[["histogram"]]$position <- 3
    }
  }

  #Probability Plot
  if(options$probabilityPlot | options$probabilityPlot) {
    if (is.null(jaspResults[["probabilityPlot"]])){
      jaspResults[["probabilityPlot"]] <- createJaspContainer(gettext("Probability Plots"))
      jaspResults[["probabilityPlot"]]$dependOn(c("probabilityPlot"))
      jaspResults[["probabilityPlot"]]$position <- 4
      jaspResults[["PPtables"]] <- createJaspContainer(gettext("Probability Plots Tables"))
      jaspResults[["PPtables"]]$dependOn(c("probabilityPlot"))
      jaspResults[["PPtables"]]$position <- 5
    }
    
    PPplots <- jaspResults[["probabilityPlot"]]
    PPtables <- jaspResults[["PPtables"]]
    for (var in diameter){
      PPplots[[var]] <- .ProbabilityPlotNoId(dataset = dataset, options = options, variable = var, dis = options$Nulldis)
      PPtables[[var]] <- .PPtable(dataset = dataset, options = options, variable = var, dis = options$Nulldis)
    }
  }

  # Initial Capability Analysis 
  if (options[["capabilityStudy"]] == "initialCapabilityAnalysis"){
    if(is.null(jaspResults[["initialCapabilityAnalysis"]])) {
      jaspResults[["initialCapabilityAnalysis"]] <- createJaspContainer(gettext("Process Capability of Measurements (Initial Capability Study)"))
      jaspResults[["initialCapabilityAnalysis"]]$dependOn(c("initialCapabilityAnalysis","diameter","subgroups", "lowerSpecification", "upperSpecification", "targetValue"))
      jaspResults[["initialCapabilityAnalysis"]]$position <- 6
    }

    initialCapabilityAnalysis <- jaspResults[["initialCapabilityAnalysis"]]
    initialCapabilityAnalysis[["processDataTable"]] <- .processDataTable(options, dataset, diameter, subgroupsName)
    initialCapabilityAnalysis[["capabilityPlot"]] <- .capabilityPlot(options, dataset, diameter, subgroupsName)
    initialCapabilityTables <- .capabilityTable(options, dataset, diameter, subgroupsName)
    initialCapabilityAnalysis[["potentialCapabilityTable"]] <- initialCapabilityTables[[1]]
    initialCapabilityAnalysis[["overallCapabilityTable"]] <- initialCapabilityTables[[2]]
  }
  
  # Follow-up Capability Analysis 
  if (options[["capabilityStudy"]] == "followupCapabilityAnalysis"){
    if(is.null(jaspResults[["followupCapabilityAnalysis"]])) {
      jaspResults[["followupCapabilityAnalysis"]] <- createJaspContainer(gettext("Process Capability of Measurements (Follow-up Capability Study)"))
      jaspResults[["followupCapabilityAnalysis"]]$dependOn(c("followupCapabilityAnalysis","diameter","subgroups", "lowerSpecification", "upperSpecification", "targetValue"))
      jaspResults[["followupCapabilityAnalysis"]]$position <- 6
    }

    followupCapabilityAnalysis <- jaspResults[["followupCapabilityAnalysis"]]
    followupCapabilityAnalysis[["processDataTable"]] <- .processDataTable(options, dataset, diameter, subgroupsName)
    followupCapabilityAnalysis[["capabilityPlot"]] <- .capabilityPlot(options, dataset, diameter, subgroupsName)
    followupCapabilityTables <- .capabilityTable(options, dataset, diameter, subgroupsName)
    followupCapabilityAnalysis[["potentialCapabilityTable"]] <- followupCapabilityTables[[1]]
    followupCapabilityAnalysis[["overallCapabilityTable"]] <- followupCapabilityTables[[2]]
  }

  return()
}

.histogram <- function(options, dataset, diameter, subgroupsName){
  
  diameter <- encodeColNames(diameter)
  subgroupsName <- encodeColNames(subgroupsName)
  thePlot <- createJaspPlot(title = gettext("Histogram"))
  plotDat <- data.frame(measurements = as.numeric(unlist(dataset[, diameter])))
  
  p <- ggplot2::ggplot(plotDat, ggplot2::aes(x = measurements)) + 
    ggplot2::geom_histogram(ggplot2::aes(y =..density..), 
                            fill = "grey",
                            col = "black",
                            size = .7) +
    ggplot2::stat_function(fun = dnorm, 
                           args = list(mean = mean(plotDat[["measurements"]]), 
                           sd = sd(plotDat[["measurements"]])),
                           color = "blue") +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  thePlot$plotObject <- p
  return(thePlot)
}

.capabilityPlot <- function(options, dataset, diameter, subgroupsName){
  
  diameter <- encodeColNames(diameter)
  subgroupsName <- encodeColNames(subgroupsName)
  thePlot <- createJaspPlot(title = gettext("Capability of the Process"))
  plotDat <- data.frame(measurements = as.numeric(unlist(dataset[, diameter])))
  
  diameter <- encodeColNames(diameter)
  subgroupsName <- encodeColNames(subgroupsName)
  
  data <- dataset[,diameter]
  subgroups <- dataset[,subgroupsName]
  subgroupLevels <- levels(subgroups)
  dataDiameter <- data.frame() 
  for(level in subgroupLevels){
    subdata <- subset(dataset[,diameter], subgroups == level)
    dataDiameter <- rbind(data, as.numeric(unlist(subdata)))
  }
  USL <- as.numeric(options$upperSpecification)
  LSL <- as.numeric(options$lowerSpecification)
  targetValue <- as.numeric(options$targetValue)
  sampleMean <- mean(as.matrix(dataDiameter),na.rm = TRUE)
  sampleN <- ncol(dataDiameter)
  
  q <- qcc::qcc(dataDiameter, type ='S', plot=FALSE)
  stDevWithin <- q$std.dev   #stDevWithin <- rBar/d2
  stDevOverall <- sd(as.matrix(dataDiameter),na.rm = TRUE)
  
  p <- ggplot2::ggplot(plotDat, ggplot2::aes(x = measurements)) + 
    ggplot2::geom_histogram(ggplot2::aes(y =..density..), 
                            fill = "grey",
                            col = "black",
                            size = .7) + 
    ggplot2::stat_function(fun = dnorm, 
                           args = list(mean = sampleMean, 
                           sd = stDevOverall),
                           color = "black") +
    ggplot2::stat_function(fun = dnorm, 
                           args = list(mean = sampleMean, 
                           sd = stDevWithin),
                           color = "red") +
    ggplot2::geom_vline(xintercept = c(LSL,USL), 
                        linetype = "dotted",
                        color = "red") +
    ggplot2::geom_vline(xintercept = targetValue, 
                        linetype = "dotted",
                        color = "green") +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  thePlot$plotObject <- p
  return(thePlot)
}

.processDataTable <- function(options, dataset, diameter, subgroupsName){

  processDataTable <- createJaspTable(title = gettext("Process Data"))
  processDataTable$dependOn(c("upperSpecification","lowerSpecification","targetValue","diameter","subgroups"))

  processDataTable$addColumnInfo(name = "lowerSpecificationLimit", type = "number", title = gettext("LSL"))
  processDataTable$addColumnInfo(name = "targetValue", type = "number", title = gettext("Target"))
  processDataTable$addColumnInfo(name = "upperSpecificationLimit", type = "number", title = gettext("USL"))
  processDataTable$addColumnInfo(name = "sampleMean", type = "number", title = gettext("Sample Mean"))
  processDataTable$addColumnInfo(name = "sampleN", type = "number", title = gettext("Sample N"))
  processDataTable$addColumnInfo(name = "stDevWithin", type = "number", title = gettext("StDev (Within)"))
  processDataTable$addColumnInfo(name = "stDevOverall", type = "number", title = gettext("StDev (Overall)"))
  
  diameter <- encodeColNames(diameter)
  subgroupsName <- encodeColNames(subgroupsName)

  data <- dataset[,diameter]
  subgroups <- dataset[,subgroupsName]
  subgroupLevels <- levels(subgroups)
  dataDiameter <- data.frame() 
  for(level in subgroupLevels){
    subdata <- subset(dataset[,diameter], subgroups == level)
    dataDiameter <- rbind(data, as.numeric(unlist(subdata)))
  }
  USL <- as.numeric(options$upperSpecification)
  LSL <- as.numeric(options$lowerSpecification)
  targetValue <- as.numeric(options$targetValue)
  sampleMean <- mean(as.matrix(dataDiameter),na.rm = TRUE)
  sampleN <- ncol(dataDiameter)
  
  q <- qcc::qcc(dataDiameter, type ='S', plot=FALSE)
  stDevWithin <- q$std.dev   #stDevWithin <- rBar/d2
  stDevOverall <- sd(as.matrix(dataDiameter),na.rm = TRUE)
  
  processDataTable$addRows(list("lowerSpecificationLimit" = LSL,
                                "targetValue" = targetValue,
                                "upperSpecificationLimit" = USL,
                                "sampleMean" = sampleMean,
                                "sampleN" = sampleN,
                                "stDevWithin" = stDevWithin,
                                "stDevOverall" = stDevOverall))

  return(processDataTable)
}

.capabilityTable <- function(options, dataset, diameter, subgroupsName){
  potentialCapabilityTable <- createJaspTable(title = gettext("Potential (Within) Capability"))

  diameter <- encodeColNames(diameter)
  subgroupsName <- encodeColNames(subgroupsName)
  
  data <- dataset[,diameter]
  subgroups <- dataset[,subgroupsName]
  subgroupLevels <- levels(subgroups)
  dataDiameter <- data.frame() 
  for(level in subgroupLevels){
    subdata <- subset(dataset[,diameter], subgroups == level)
    dataDiameter <- rbind(data, as.numeric(unlist(subdata)))
  }
  USL <- as.numeric(options$upperSpecification)
  LSL <- as.numeric(options$lowerSpecification)
  targetValue <- as.numeric(options$targetValue)
  sampleMean <- mean(as.matrix(dataDiameter),na.rm = TRUE)
  sampleN <- ncol(dataDiameter)
  
  q <- qcc::qcc(dataDiameter, type ='S', plot=FALSE)
  stDevWithin <- q$std.dev   #stDevWithin <- rBar/d2
  stDevOverall <- sd(as.matrix(dataDiameter),na.rm = TRUE)

  potentialCapabilityTable$addColumnInfo(name = "CP", type = "number", title = gettext("CP"))
  potentialCapabilityTable$addColumnInfo(name = "CPL", type = "number", title = gettext("CPL"))
  potentialCapabilityTable$addColumnInfo(name = "CPU", type = "number", title = gettext("CPU"))
  potentialCapabilityTable$addColumnInfo(name = "CPK", type = "number", title = gettext("CPK"))
  potentialCapabilityTable$addColumnInfo(name = "Z", type = "number", title = gettext("Z (Sigma Score)"))
  
  #Capability Indices (short term)
  CP <- (USL - LSL) / (6*stDevWithin)
  CPL <- (sampleMean - LSL) / (3*stDevWithin)
  CPU <- (USL - sampleMean) / (3*stDevWithin)
  CPK <- min(CPU, CPL)
  Z <- CPK * 3
  potentialCapabilityTable$addRows(list("CP" = CP,
                                        "CPL" = CPL,
                                        "CPU" = CPU,
                                        "CPK" = CPK,
                                        "Z" = Z))

  overallCapabilityTable <- createJaspTable(title = gettext("Overall Capability"))
  overallCapabilityTable$addColumnInfo(name = "PP", type = "number", title = gettext("PP"))
  overallCapabilityTable$addColumnInfo(name = "PPL", type = "number", title = gettext("PPL"))
  overallCapabilityTable$addColumnInfo(name = "PPU", type = "number", title = gettext("PPU"))
  overallCapabilityTable$addColumnInfo(name = "PPK", type = "number", title = gettext("PPK"))
  overallCapabilityTable$addColumnInfo(name = "CPM", type = "number", title = gettext("CPM"))
  
  #Performance Indices (long term)
  PP <- (USL - LSL) / (6*stDevOverall)
  PPL <- (sampleMean - LSL) / (3*stDevOverall)
  PPU <- (USL - sampleMean) / (3*stDevOverall)
  PPK <- min(PPU, PPL)
  CPM <- CP / sqrt(1 + ((sampleMean - targetValue) / stDevWithin)^2)

  overallCapabilityTable$addRows(list("PP" = PP,
                                 "PPL" = PPL,
                                 "PPU" = PPU,
                                 "PPK" = PPK,
                                 "CPM" = CPM))

  capabilityTables <- list(potentialCapabilityTable, overallCapabilityTable)

  return(capabilityTables)
}
