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
  
  ## Initial Process Capability Study
  
  # X-bar & Range Control Chart (by Tom)
  if(options$controlCharts & is.null(jaspResults[["controlCharts"]])) {
    jaspResults[["XbarPlot"]] <- createJaspPlot(title = "X bar chart", width = 1100, height = 400)
    jaspResults[["XbarPlot"]]$dependOn(c("XbarRchart"))
    jaspResults[["XbarPlot"]]$position <- 11
    XbarPlot <- jaspResults[["XbarPlot"]]
    XbarPlot$plotObject <- .XbarchartNoId(dataset = dataset, options = options)
    XbarPlot$dependOn(optionContainsValue= list(diameter=diameter))
    
    jaspResults[["RPlot"]] <- createJaspPlot(title = "R chart", width = 1100, height= 400)
    jaspResults[["RPlot"]]$dependOn(c("XbarRchart"))
    jaspResults[["RPlot"]]$position <- 11
    RPlot<- jaspResults[["RPlot"]]
    RPlot$plotObject <- .RchartNoId(dataset = dataset, options = options)
    RPlot$dependOn(optionContainsValue= list(diameter=diameter))
  }
  
  # Histogram 

  #Probability Plot
  if(options$probabilityPlot | options$probabilityPlot) {
    if (is.null(jaspResults[["probabilityPlot"]])){
      jaspResults[["probabilityPlot"]] <- createJaspContainer(gettext("Probability Plots"))
      jaspResults[["probabilityPlot"]]$dependOn(c("probabilityPlot"))
      jaspResults[["probabilityPlot"]]$position <- 11
      jaspResults[["PPtables"]] <- createJaspContainer(gettext("Probability Plots Tables"))
      jaspResults[["PPtables"]]$dependOn(c("probabilityPlot"))
      jaspResults[["PPtables"]]$position <- 11
    }
    
    PPplots <- jaspResults[["probabilityPlot"]]
    PPtables <- jaspResults[["PPtables"]]
    for (var in diameter){
      PPplots[[var]] <- .ProbabilityPlotNoId(dataset = dataset, options = options, variable = var, dis = options$Nulldis)
      PPtables[[var]] <- .PPtable(dataset = dataset, options = options, variable = var, dis = options$Nulldis)
    }
  }

  # Initial Capability Analysis 
  
  if (options$initialCapabilityAnalysis){
    if(is.null(jaspResults[["initialCapabilityAnalysis"]])) {
      jaspResults[["initialCapabilityAnalysis"]] <- createJaspContainer(gettext("Process Capability of Diameter (Initial Capability Study)"))
      jaspResults[["initialCapabilityAnalysis"]]$dependOn(c("initialCapabilityAnalysis","diameter","subgroups", "lowerSpecification", "upperSpecification", "targetValue"))
      jaspResults[["initialCapabilityAnalysis"]]$position <- 4
    }

    initialCapabilityAnalysis <- jaspResults[["initialCapabilityAnalysis"]]
    initialCapabilityAnalysis[["processDataTable"]] <- .processDataTable(options, dataset, diameter, subgroupsName)
    initialCapabilityAnalysis[["observedPerformanceTable"]]<- .observedPerformanceTable(options, dataset)
    initialCapabilityAnalysis[["targetPerformanceTable"]]<- .targetPerformanceTable(options, dataset)
    initialCapabilityAnalysis[["expOverallPerformanceTable"]]<- .expOverallPerformanceTable(options, dataset)
    initialCapabilityAnalysis[["expWithinPerformanceTable"]]<- .expWithinPerformanceTable(options, dataset)
    initialCapabilityTables <- .capabilityTable(options, dataset, diameter, subgroupsName)
    initialCapabilityAnalysis[["potentialCapabilityTable"]] <- initialCapabilityTables[[1]]
    initialCapabilityAnalysis[["overallCapabilityTable"]] <- initialCapabilityTables[[2]]
  }
  
  # Follow-up Capability Analysis 
  if (options$followupCapabilityAnalysis){
    if(is.null(jaspResults[["followupCapabilityAnalysis"]])) {
      jaspResults[["followupCapabilityAnalysis"]] <- createJaspContainer(gettext("Process Capability of Diameter (Follow-up Capability Study)"))
      jaspResults[["followupCapabilityAnalysis"]]$dependOn(c("followupCapabilityAnalysis","diameter","subgroups", "lowerSpecification", "upperSpecification", "targetValue"))
      jaspResults[["followupCapabilityAnalysis"]]$position <- 8
    }

    followupCapabilityAnalysis <- jaspResults[["followupCapabilityAnalysis"]]
    followupCapabilityAnalysis[["processDataTable"]] <- .processDataTable(options, dataset, diameter, subgroupsName)
    followupCapabilityAnalysis[["observedPerformanceTable"]]<- .observedPerformanceTable(options, dataset)
    followupCapabilityAnalysis[["targetPerformanceTable"]]<- .targetPerformanceTable(options, dataset)
    followupCapabilityAnalysis[["expOverallPerformanceTable"]]<- .expOverallPerformanceTable(options, dataset)
    followupCapabilityAnalysis[["expWithinPerformanceTable"]]<- .expWithinPerformanceTable(options, dataset)
    followupCapabilityTables <- .capabilityTable(options, dataset, diameter, subgroupsName)
    followupCapabilityAnalysis[["potentialCapabilityTable"]] <- followupCapabilityTables[[1]]
    followupCapabilityAnalysis[["overallCapabilityTable"]] <- followupCapabilityTables[[2]]
  }

  return()
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

  USL <- as.numeric(options$upperSpecification)
  LSL <- as.numeric(options$lowerSpecification)
  targetValue <- as.numeric(options$targetValue)
  sampleMean <- mean(dataset[[diameter]])
  sampleN <- length(dataset[[diameter]])

  getInfo <- function(x) {
    c("diff" = diff(range(x)), "sd2" = (sd(x))^2)
  }

  subgroupInfo <- by(dataset[[diameter]], list(dataset[[subgroupsName]]), getInfo)
  subgroupInfoMatrix <- matrix(unlist(subgroupInfo), ncol = length(subgroupInfo[[1]]), byrow = TRUE)
  colnames(subgroupInfoMatrix) <- names(subgroupInfo[[1]])
  rownames(subgroupInfoMatrix) <- names(subgroupInfo)


  subgroupInfo <- by(dataset[[diameter]], list(dataset[[subgroupsName]]), getInfo)
  subgroupInfoMatrix <- matrix(unlist(subgroupInfo), ncol = length(subgroupInfo[[1]]), byrow = TRUE)
  colnames(subgroupInfoMatrix) <- names(subgroupInfo[[1]])
  rownames(subgroupInfoMatrix) <- names(subgroupInfo)

  rBar <- sum(subgroupInfoMatrix[, "diff"]) / length(dataset[[subgroupsName]])
  d2 <- 2.337 #based on subgroup size (needs to be updated)

  stDevWithin <- rBar/d2
  stDevOverall <- sd(dataset[[diameter]])
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

  USL <- as.numeric(options$upperSpecification)
  LSL <- as.numeric(options$lowerSpecification)
  targetValue <- as.numeric(options$targetValue)
  sampleN <- length(dataset[[diameter]])

  getInfo <- function(x) {
    c("diff" = diff(range(x)), "mean" = mean(x), "sd" = sd(x))
  }

  subgroupInfo <- by(dataset[[diameter]], list(dataset[[subgroupsName]]), getInfo)
  subgroupInfoMatrix <- matrix(unlist(subgroupInfo), ncol = length(subgroupInfo[[1]]), byrow = TRUE)
  colnames(subgroupInfoMatrix) <- names(subgroupInfo[[1]])
  rownames(subgroupInfoMatrix) <- names(subgroupInfo)

  rBar <- sum(subgroupInfoMatrix[, "diff"]) / length(dataset[[subgroupsName]])
  d2 <- 2.337 #based on subgroup size (needs to be updated)


  subgroupInfo <- by(dataset[[diameter]], list(dataset[[subgroupsName]]), getInfo)
  subgroupInfoMatrix <- matrix(unlist(subgroupInfo), ncol = length(subgroupInfo[[1]]), byrow = TRUE)
  colnames(subgroupInfoMatrix) <- names(subgroupInfo[[1]])
  rownames(subgroupInfoMatrix) <- names(subgroupInfo)

  rBar <- sum(subgroupInfoMatrix[, "diff"]) / length(dataset[[subgroupsName]])

  stDevWithin <- rBar/d2
  stDevOverall <- sd(dataset[[diameter]])
  grandAverage <- mean(dataset[[diameter]])

  potentialCapabilityTable$addColumnInfo(name = "CP", type = "number", title = gettext("CP"))
  potentialCapabilityTable$addColumnInfo(name = "CPL", type = "number", title = gettext("CPL"))
  potentialCapabilityTable$addColumnInfo(name = "CPU", type = "number", title = gettext("CPU"))
  potentialCapabilityTable$addColumnInfo(name = "CPK", type = "number", title = gettext("CPK"))

  #Capability Indices
  CP <- (USL - LSL) / (6*stDevWithin)
  CPL <- (grandAverage - LSL) / (3*stDevWithin)
  CPU <- (USL - grandAverage) / (3*stDevWithin)
  CPK <- min(CPU, CPL)

  potentialCapabilityTable$addRows(list("CP" = CP,
                                   "CPL" = CPL,
                                   "CPU" = CPU,
                                   "CPK" = CPK))

  overallCapabilityTable <- createJaspTable(title = gettext("Overall Capability"))
  overallCapabilityTable$addColumnInfo(name = "PP", type = "number", title = gettext("PP"))
  overallCapabilityTable$addColumnInfo(name = "PPL", type = "number", title = gettext("PPL"))
  overallCapabilityTable$addColumnInfo(name = "PPU", type = "number", title = gettext("PPU"))
  overallCapabilityTable$addColumnInfo(name = "PPK", type = "number", title = gettext("PPK"))
  overallCapabilityTable$addColumnInfo(name = "CPM", type = "number", title = gettext("CPM"))
  #Performance Indices
  PP <- (USL - LSL) / (6*stDevOverall)
  PPL <- (grandAverage - LSL) / (3*stDevOverall)
  PPU <- (USL - grandAverage) / (3*stDevOverall)
  PPK <- min(PPU, PPL)
  CPM <- CP / sqrt(1 + ((grandAverage - targetValue) / stDevWithin)^2)

  overallCapabilityTable$addRows(list("PP" = PP,
                                 "PPL" = PPL,
                                 "PPU" = PPU,
                                 "PPK" = PPK,
                                 "CPM" = CPM))

  capabilityTables <- list(potentialCapabilityTable, overallCapabilityTable)

  return(capabilityTables)
}

.observedPerformanceTable <- function(options, dataset){
  observedPerformanceTable <- createJaspTable(title = gettext("Observed Performance"))
  return(observedPerformanceTable)

}

.targetPerformanceTable <- function(options, dataset){
  targetPerformanceTable <- createJaspTable(title = gettext("Target Performance"))
  targetPerformanceTable$addColumnInfo(name = "CPM", type = "number", title = gettext("CPM"))
  targetPerformanceTable$addColumnInfo(name = "lowerCL", type = "number", title = gettext("Lower CL"))

  CPM <- numeric()
  lowerCL <- numeric()

  targetPerformanceTable$addRows(list("CPM" = CPM,
                                      "lowerCL" = lowerCL))
  return(targetPerformanceTable)

}

.expWithinPerformanceTable <- function(options, dataset){
  expWithinPerformanceTable <- createJaspTable(title = gettext("Exp. Within Performance"))
  return(expWithinPerformanceTable)
}

.expOverallPerformanceTable <- function(options, dataset){
  expOverallPerformanceTable <- createJaspTable(title = gettext("Exp. Overall Performance"))
  return(expOverallPerformanceTable)
}
