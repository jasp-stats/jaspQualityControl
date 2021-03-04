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

processCapabilityStudies <- function(jaspResults, dataset, options){

  diameter <- unlist(options$diameter)
  subgroupsName <- options$subgroups
  makeSubgroups <- subgroupsName != ""

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
    jaspResults[["XbarPlot"]] <- createJaspPlot(title = "X bar chart", width = 600, height = 300)
    jaspResults[["XbarPlot"]]$dependOn(c("XbarRchart"))
    jaspResults[["XbarPlot"]]$position <- 1
    XbarPlot <- jaspResults[["XbarPlot"]]
    XbarPlot$plotObject <- .XbarchartNoId(dataset = dataset, options = options)
    XbarPlot$dependOn(optionContainsValue= list(diameter=diameter))

    jaspResults[["RPlot"]] <- createJaspPlot(title = "R chart", width = 600, height= 300)
    jaspResults[["RPlot"]]$dependOn(c("XbarRchart"))
    jaspResults[["RPlot"]]$position <- 2
    RPlot<- jaspResults[["RPlot"]]
    RPlot$plotObject <- .RchartNoId(dataset = dataset, options = options)
    RPlot$dependOn(optionContainsValue= list(diameter=diameter))
  }

  # Histogram
  if (options$histogram) {
    if(is.null(jaspResults[["histogram"]])) {
      jaspResults[["histogram"]] <- .histogram(options, dataset, diameter, subgroupsName)
      jaspResults[["histogram"]]$position <- 3
    }
  }

  #Probability Plot
  if(options$probabilityPlot) {
    if (is.null(jaspResults[["probabilityPlot"]])){
      jaspResults[["probabilityPlot"]] <- createJaspContainer(gettext("Probability Plots"))
      jaspResults[["probabilityPlot"]]$dependOn(c("probabilityPlot"))
      jaspResults[["probabilityPlot"]]$position <- 5
      jaspResults[["PPtables"]] <- createJaspContainer(gettext("Probability Plot Table"))
      jaspResults[["PPtables"]]$dependOn(c("probabilityPlot"))
      jaspResults[["PPtables"]]$position <- 4
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
      jaspResults[["initialCapabilityAnalysis"]]$dependOn(c("capabilityStudy","diameter","subgroups", "lowerSpecification", "upperSpecification", "targetValue"))
      jaspResults[["initialCapabilityAnalysis"]]$position <- 5
    }

    initialCapabilityAnalysis <- jaspResults[["initialCapabilityAnalysis"]]
    initialCapabilityAnalysis[["processDataTable"]] <- .processDataTable(options, dataset, diameter, subgroupsName, initialCapabilityAnalysis)
    initialCapabilityAnalysis[["capabilityPlot"]] <- .capabilityPlot(options, dataset, diameter, subgroupsName)
    initialCapabilityAnalysis[["capabilityTable"]] <- .capabilityTable(options, dataset, diameter, subgroupsName, initialCapabilityAnalysis)
  }

  # Follow-up Capability Analysis
  if (options[["capabilityStudy"]] == "followupCapabilityAnalysis"){
    if(is.null(jaspResults[["followupCapabilityAnalysis"]])) {
      jaspResults[["followupCapabilityAnalysis"]] <- createJaspContainer(gettext("Process Capability of Measurements (Follow-up Capability Study)"))
      jaspResults[["followupCapabilityAnalysis"]]$dependOn(c("capabilityStudy","diameter","subgroups", "lowerSpecification", "upperSpecification", "targetValue"))
      jaspResults[["followupCapabilityAnalysis"]]$position <- 5
    }

    followupCapabilityAnalysis <- jaspResults[["followupCapabilityAnalysis"]]
    followupCapabilityAnalysis[["processDataTable"]] <- .processDataTable(options, dataset, diameter, subgroupsName, followupCapabilityAnalysis)
    followupCapabilityAnalysis[["capabilityPlot"]] <- .capabilityPlot(options, dataset, diameter, subgroupsName)
    followupCapabilityAnalysis[["capabilityTable"]] <- .capabilityTable(options, dataset, diameter, subgroupsName, followupCapabilityAnalysis)
  }

#Non normal Capability Analysis
if(options[["nonNormalCapabilityStudy"]]){
  if (is.null(jaspResults[["nonNormalCapabilityStudy"]])){
    jaspResults[["nonNormalCapabilityStudy"]] <- createJaspContainer(gettext("Nonnormal Capability Analysis"))
    jaspResults[["nonNormalCapabilityStudy"]]$dependOn(c("nonNormalCapabilityStudy","diameter","subgroups", "lowerSpecification", "upperSpecification"))
    jaspResults[["nonNormalCapabilityStudy"]]$position <- 5
  }
    jaspResults[["nonNormalCapabilityStudy"]] <- .nonNormalCapabilityTable(options, dataset, diameter, subgroupsName,dis = options$nonNormalDist)
}
}  

.histogram <- function(options, dataset, diameter, subgroupsName){

  if(!options[["histogram"]] || is.null(unlist(options[["diameter"]])) || options[["subgroups"]] == "")
    return()

  diameter <- encodeColNames(diameter)
  subgroupsName <- encodeColNames(subgroupsName)
  thePlot <- createJaspPlot(title = gettext("Histogram"), width = 400, height = 400)
  thePlot$dependOn(options = "histogram")
  plotDat <- data.frame(measurements = as.numeric(unlist(dataset[, diameter])))
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(plotDat[["measurements"]], min.n = 4)

  p <- ggplot2::ggplot(plotDat, ggplot2::aes(x = measurements)) +
    ggplot2::scale_x_continuous(name = gettext("Measurements"), breaks = xBreaks) +
    ggplot2::geom_histogram(ggplot2::aes(y =..density..), fill = "grey", col = "black", size = .7) +
    ggplot2::stat_function(fun = dnorm, color = "blue",
                           args = list(mean = mean(plotDat[["measurements"]]), sd = sd(plotDat[["measurements"]])))
  p <- jaspGraphs::themeJasp(p)

  thePlot$plotObject <- p
  return(thePlot)
}

.capabilityPlot <- function(options, dataset, diameter, subgroupsName){

  thePlot <- createJaspPlot(title = gettext("Capability of the Process"), width = 600, height = 300)

  plotDat <- data.frame(measurements = as.numeric(unlist(dataset[, diameter])))
  dataDiameter <- .convertDatasetToQccReady(dataset, diameter, subgroupsName)

  USL <- as.numeric(options$upperSpecification)
  LSL <- as.numeric(options$lowerSpecification)
  targetValue <- as.numeric(options$targetValue)
  sampleMean <- mean(as.matrix(dataDiameter),na.rm = TRUE)
  sampleN <- ncol(dataDiameter)

  stDevWithin <- qcc::qcc(dataDiameter, type ='R', plot=FALSE)$std.dev #stDevWithin <- rBar/d2
  stDevOverall <- sd(as.matrix(dataDiameter),na.rm = TRUE)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(unlist(dataDiameter), targetValue, LSL, USL), min.n = 4)

  p <- ggplot2::ggplot(plotDat, ggplot2::aes(x = measurements)) +
    ggplot2::scale_x_continuous(name = gettext("Measurements"), breaks = xBreaks, limits = range(xBreaks)) +
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
                        color = "green")
  p <- jaspGraphs::themeJasp(p)

  thePlot$plotObject <- p
  return(thePlot)
}

.processDataTable <- function(options, dataset, diameter, subgroupsName, initialCapabilityAnalysis){

  processDataTable <- createJaspTable(title = gettext("Process Data"))
  processDataTable$dependOn(c("upperSpecificationField","lowerSpecificationField","upperSpecification","lowerSpecification","targetValue","diameter","subgroups"))

  if (options$lowerSpecificationField) {
  processDataTable$addColumnInfo(name = "lowerSpecificationLimit", type = "number", title = gettext("LSL"))
  }
  if (options$targetValueField) {
  processDataTable$addColumnInfo(name = "targetValue", type = "number", title = gettext("Target"))
  }
  if (options$upperSpecificationField) {
  processDataTable$addColumnInfo(name = "upperSpecificationLimit", type = "number", title = gettext("USL"))
  }
  processDataTable$addColumnInfo(name = "sampleMean", type = "number", title = gettext("Sample Average"))
  processDataTable$addColumnInfo(name = "sampleN", type = "integer", title = gettext("Sample N"))
  processDataTable$addColumnInfo(name = "stDevOverall", type = "number", title = gettext("Sd (Total)"))
  processDataTable$addColumnInfo(name = "stDevWithin", type = "number", title = gettext("Sd (Within)"))

  processDataTable$showSpecifiedColumnsOnly <- TRUE
  dataDiameter <- .convertDatasetToQccReady(dataset, diameter, subgroupsName)

  USL <- as.numeric(options$upperSpecification)
  LSL <- as.numeric(options$lowerSpecification)
  targetValue <- as.numeric(options$targetValue)
  sampleMean <- mean(as.matrix(dataDiameter),na.rm = TRUE)
  sampleN <- ncol(dataDiameter)

  stDevWithin <- qcc::qcc(dataDiameter, type ='R', plot=FALSE)$std.dev
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

.capabilityTable <- function(options, dataset, diameter, subgroupsName, initialCapabilityAnalysis){
  
  potentialCapabilityTable <- createJaspTable(title = gettext("Process Capability (Within)"))
  potentialCapabilityTable$dependOn(c("upperSpecificationField","lowerSpecificationField","upperSpecification","lowerSpecification","targetValue","diameter","subgroups"))
  
  if (options$lowerSpecificationField) {
    potentialCapabilityTable$addColumnInfo(name = "CPL", type = "number", title = gettext("CPL"))
  }
  
  if (options$upperSpecificationField) {
    potentialCapabilityTable$addColumnInfo(name = "CPU", type = "number", title = gettext("CPU"))
  }
  
  if (options$lowerSpecificationField & options$upperSpecificationField) {
    potentialCapabilityTable$addColumnInfo(name = "CP", type = "number", title = gettext("Cp"))
    potentialCapabilityTable$addColumnInfo(name = "CPK", type = "number", title = gettext("Cpk"))
    potentialCapabilityTable$addColumnInfo(name = "Z", type = "number", title = gettext("ppm"))
  }
  
  potentialCapabilityTable$showSpecifiedColumnsOnly <- TRUE
  
  initialCapabilityAnalysis[["potentialCapabilityTable"]] <- potentialCapabilityTable
  
  overallCapabilityTable <- createJaspTable(title = gettext("Process Performance (Total)"))
  overallCapabilityTable$dependOn(c("upperSpecificationField","lowerSpecificationField","upperSpecification","lowerSpecification","targetValue","diameter","subgroups"))
  
  if (options$lowerSpecificationField) {
    overallCapabilityTable$addColumnInfo(name = "PPL", type = "number", title = gettext("PPL"))
  }
  
  if (options$upperSpecificationField) {
    overallCapabilityTable$addColumnInfo(name = "PPU", type = "number", title = gettext("PPU"))
  }
  
  if (options$lowerSpecificationField & options$upperSpecificationField) {
    overallCapabilityTable$addColumnInfo(name = "PP", type = "number", title = gettext("Pp"))
    overallCapabilityTable$addColumnInfo(name = "PPK", type = "number", title = gettext("Ppk"))
    overallCapabilityTable$addColumnInfo(name = "ppm", type = "number", title = gettext("ppm"))
  }
  
  overallCapabilityTable$showSpecifiedColumnsOnly <- TRUE
  
  initialCapabilityAnalysis[["overallCapabilityTable"]] <- overallCapabilityTable
  
  dataDiameter <- .convertDatasetToQccReady(dataset, diameter, subgroupsName)
  
  USL <- as.numeric(options$upperSpecification)
  LSL <- as.numeric(options$lowerSpecification)
  targetValue <- as.numeric(options$targetValue)
  sampleMean <- mean(as.matrix(dataDiameter),na.rm = TRUE)
  sampleN <- ncol(dataDiameter)

  stDevWithin <- qcc::qcc(dataDiameter, type ='R', plot=FALSE)$std.dev #stDevWithin <- rBar/d2
  stDevOverall <- sd(as.matrix(dataDiameter),na.rm = TRUE)

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

  #Performance Indices (long term)
  PP <- (USL - LSL) / (6*stDevOverall)
  PPL <- (sampleMean - LSL) / (3*stDevOverall)
  PPU <- (USL - sampleMean) / (3*stDevOverall)
  PPK <- min(PPU, PPL)
  ppm <- CP / sqrt(1 + ((sampleMean - targetValue) / stDevWithin)^2)

  overallCapabilityTable$addRows(list("PP" = PP,
                                      "PPL" = PPL,
                                      "PPU" = PPU,
                                      "PPK" = PPK,
                                      "ppm" = ppm))
  
  capabilityTables <- list(potentialCapabilityTable, overallCapabilityTable)
  
  return(capabilityTables)
}

.nonNormalCapabilityTable <- function(options, dataset, diameter, subgroupsName, dis){
  
  dataDiameter <- .convertDatasetToQccReady(dataset, diameter, subgroupsName)
  USL <- as.numeric(options$upperSpecification)
  LSL <- as.numeric(options$lowerSpecification)
  targetValue <- as.numeric(options$targetValue)
  average <- mean(as.matrix(dataDiameter),na.rm = TRUE)
  standardDeviation <- sd(as.matrix(dataDiameter),na.rm = TRUE) #overall sd 
  
  if (dis == "Lognormal"){
    
    nonNormalCapabilityTable <- createJaspTable(title = gettext("Process Capability based on Lognormal Distribution"))
    nonNormalCapabilityTable$dependOn(c("upperSpecificationField","lowerSpecificationField","upperSpecification","lowerSpecification","targetValue","diameter","subgroups"))
    
    nonNormalCapabilityTable$addColumnInfo(name = "average", type = "number", title = gettext("Average"))
    nonNormalCapabilityTable$addColumnInfo(name = "standardDeviation", type = "number", title = gettext("Sd"))
    
    if (options$lowerSpecificationField) {
      nonNormalCapabilityTable$addColumnInfo(name = "LSL", type = "number", title = gettext("LSL"))
      nonNormalCapabilityTable$addColumnInfo(name = "partsLower", type = "number", title = gettext("P (X < LSL)"))
    }
    
    if (options$upperSpecificationField) {
      nonNormalCapabilityTable$addColumnInfo(name = "USL", type = "number", title = gettext("USL"))
      nonNormalCapabilityTable$addColumnInfo(name = "partsUpper", type = "number", title = gettext("P (X > USL)"))
    }
    
    if (options$lowerSpecificationField & options$upperSpecificationField) {
      nonNormalCapabilityTable$addColumnInfo(name = "CPK", type = "number", title = gettext("Cpk"))
    }
    
    nonNormalCapabilityTable$showSpecifiedColumnsOnly <- TRUE
    
    tau <- standardDeviation / average
    sdLog <- sqrt(log(tau^2 +1))
    meanLog <- log(average) - ((sdLog^2) / 2)
    partsLower <- plnorm(q = LSL, meanlog = meanLog, sdlog = sdLog)
    partsUpper <- 1- plnorm(q = USL,meanlog = meanLog, sdlog = sdLog)
    CPK <- 1- plnorm((max(partsLower,partsUpper)))/3
    
    nonNormalCapabilityTable$addRows(list("average" = average,
                                          "standardDeviation" = standardDeviation,
                                          "LSL" = LSL,
                                          "USL" = USL,
                                          "CPK" = CPK,
                                          "partsLower" = partsLower,
                                          "partsUpper" = partsUpper))
  } else if (dis == "Weibull"){
    nonNormalCapabilityTable <- createJaspTable(title = gettext("Process Capability based on Weibull Distribution"))
    nonNormalCapabilityTable$dependOn(c("upperSpecificationField","lowerSpecificationField","upperSpecification","lowerSpecification","targetValue","diameter","subgroups"))
    
    nonNormalCapabilityTable$addColumnInfo(name = "average", type = "number", title = gettext("Average"))
    nonNormalCapabilityTable$addColumnInfo(name = "standardDeviation", type = "number", title = gettext("Sd"))
    
    if (options$lowerSpecificationField) {
      nonNormalCapabilityTable$addColumnInfo(name = "LSL", type = "number", title = gettext("LSL"))
    }
    
    if (options$upperSpecificationField) {
      nonNormalCapabilityTable$addColumnInfo(name = "USL", type = "number", title = gettext("USL"))
    }
    
    nonNormalCapabilityTable$addColumnInfo(name = "beta", type = "number", title = gettext("Beta"))
    nonNormalCapabilityTable$addColumnInfo(name = "theta", type = "number", title = gettext("Theta"))
    #nonNormalCapabilityTable$addColumnInfo(name = "CPK", type = "number", title = gettext("Cpk"))
    
    nonNormalCapabilityTable$showSpecifiedColumnsOnly <- TRUE
    
    beta <- mixdist::weibullpar(mu = average, sigma = standardDeviation, loc = 0)$shape
    theta <- mixdist::weibullpar(mu = average, sigma = standardDeviation, loc = 0)$scale
    #CPK <- 0
    
    nonNormalCapabilityTable$addRows(list("average" = average,
                                          "standardDeviation" = standardDeviation,
                                          "LSL" = LSL,
                                          "USL" = USL,
                                          "beta" = beta,
                                          "theta" = theta))
  }
  
  return(nonNormalCapabilityTable)
}




