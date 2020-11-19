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

  variables <- unlist(options$variables)
  subgroupsName <- options$subgroups
  makeSubgroups <- subgroupsName != ""

  if (is.null(dataset)) {
    if (makeSubgroups) {
      dataset         <- .readDataSetToEnd(columns.as.numeric = variables, columns.as.factor = subgroupsName)
      dataset.factors <- .readDataSetToEnd(columns = variables, columns.as.factor = subgroupsName)
    } else {
      dataset         <- .readDataSetToEnd(columns.as.numeric = variables)
      dataset.factors <- .readDataSetToEnd(columns = variables)
    }
  }
  
  # Initial Process Capability Study
  
  # X-bar chart (by Tom)
  if (options$initialControlchart) {
    
  }
  # Histogram (by Jonas)
  if (options$initialHistogram) {
    
  }
  # Normal Probability Plot
  if (options$initialProbabilityPlot) {
    
  }
  # Process Capability of Diameter (by Milena)
  if (options$initialCapabilityAnalysis && is.null(jaspResults[["initialCapabilityAnalysis"]])){
    if(is.null(jaspResults[["initialCapabilityAnalysis"]])) {
      jaspResults[["initialCapabilityAnalysis"]] <- createJaspContainer(gettext("Process Capability of Diameter (Initial Capability Study)"))
      jaspResults[["initialCapabilityAnalysis"]]$dependOn(c("initialCapabilityAnalysis","diameter","subgroups"))
      jaspResults[["initialCapabilityAnalysis"]]$position <- 4
    }
    
    initialCapabilityAnalysis <- jaspResults[["initialCapabilityAnalysis"]]
    initialCapabilityAnalysis[["processDataTable"]] <- .processDataTable(options, dataset, diameter, subgroupsName)
    initialCapabilityTables <- .capabilityTable(options, dataset, diameter, subgroupsName)
    initialCapabilityAnalysis[["potentialCapabilityTable"]] <- initialCapabilityTables[[1]]
    initialCapabilityAnalysis[["overallCapabilityTable"]] <- initialCapabilityTables[[2]]
  }
  
  # Follow-up Process Capability Study
  
  # X-bar & Range Control Chart (by Tom)
  if (options$followupControlchart) {
    
  }
  # Histogram (by Jonas)
  if (options$followupHistogram) {
    
  }
  # Normal Probability Plot
  if (options$followupProbabilityPlot) {
    
  }
  # Process Capability of Diameter (by Milena)
  if (options$followupCapabilityAnalysis && is.null(jaspResults[["followupCapabilityAnalysis"]])){
    if(is.null(jaspResults[["followupCapabilityAnalysis"]])) {
      jaspResults[["followupCapabilityAnalysis"]] <- createJaspContainer(gettext("Process Capability of Diameter (Follow-up Capability Study)"))
      jaspResults[["followupCapabilityAnalysis"]]$dependOn(c("followupCapabilityAnalysis","diameter","subgroups"))
      jaspResults[["followupCapabilityAnalysis"]]$position <- 8
    }
    
    followupCapabilityAnalysis <- jaspResults[["followupCapabilityAnalysis"]]
    followupCapabilityAnalysis[["processDataTable"]] <- .processDataTable(options, dataset, diameter, subgroupsName)
    followupCapabilityTables <- .capabilityTable(options, dataset, diameter, subgroupsName)
    followupCapabilityAnalysis[["potentialCapabilityTable"]] <- followupCapabilityTables[[1]]
    followupCapabilityAnalysis[["overallCapabilityTable"]] <- followupCapabilityTables[[2]]
  }
  
  return()

.processDataTable <- function(options, dataset, diameter, subgroupsName){
  
  processDataTable <- createJaspTable(title = gettext("Process Data"))
  processDataTable$dependOn(c("upperSpecification","lowerSpecification","targetValue","diameter","subgroups"))
  
  
  processDataTable$addColumnInfo(name = "lowerSpecificationLimit", type = "integer", title = gettext("LSL"), format = "dp:")
  processDataTable$addColumnInfo(name = "targetValue", type = "integer", title = gettext("Target"), format = "dp:0")
  processDataTable$addColumnInfo(name = "upperSpecificationLimit", type = "integer", title = gettext("USL"), format = "dp:0")
  processDataTable$addColumnInfo(name = "sampleMean", type = "integer", title = gettext("Sample Mean"), format = "dp:4")
  processDataTable$addColumnInfo(name = "sampleN", type = "number", title = gettext("Sample N"), format = "dp:0")
  processDataTable$addColumnInfo(name = "stDevWithin", type = "integer", title = gettext("StDev (Within)"), format = "dp:4")
  processDataTable$addColumnInfo(name = "stDevOverall", type = "integer", title = gettext("StDev (Overall)"), format = "dp:4")
  
  USL <- options$upperSpecification
  LSL <- options$lowerSpecification
  targetValue <- options$targetValue
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
  d2 <- 2 #based on subgroup size (needs to be updated)
  
  stDevWithin <- rBar/d2
  stDevOverall <- sqrt((1/(sampleN-1)) * sum(subgroupInfoMatrix[, "sd2"]))
  processDataTable$addRows(list("lowerSpecificationLimit" = LSL,
                                "targetValue" = targetValue,
                                "upperSpecificationLimit" = USL,
                                "sampleMean" = round(sampleMean,2),
                                "sampleN" = sampleN,
                                "stDevWithin" = round(stDevWithin,5),
                                "stDevOverall" = round(stDevOverall,5)))

  return(processDataTable)
  
}

#Probability Plot
  if(options$initialProbabilityPlot | options$followupProbabilityPlot) {
    if (is.null(jaspResults[["initialProbabilityPlot"]])){
      jaspResults[["initialProbabilityPlot"]] <- createJaspContainer(gettext("Probability Plots"))
      jaspResults[["initialProbabilityPlot"]]$dependOn(c("initialProbabilityPlot"))
      jaspResults[["initialProbabilityPlot"]]$position <- 11

      jaspResults[["PPtables"]] <- createJaspContainer(gettext("Probability Plots Tables"))
      jaspResults[["PPtables"]]$dependOn(c("initialProbabilityPlot"))
      jaspResults[["PPtables"]]$position <- 11
    }

    PPplots <- jaspResults[["initialProbabilityPlot"]]
    PPtables <- jaspResults[["PPtables"]]

    for (var in variables){
      PPplots[[var]] <- .ProbabilityPlotNoId(dataset = dataset, options = options, variable = var, dis = options$Nulldis)
      PPtables[[var]] <- .PPtable(dataset = dataset, options = options, variable = var, dis = options$Nulldis)
    }
  }
#Xbar chart intial
  if(options$initialXbarchart & is.null(jaspResults[["initialXbarchart"]])) {
     jaspResults[["XbarPlot"]] <- createJaspPlot(title = "X bar chart", width = 1100, height = 400)
     jaspResults[["XbarPlot"]]$dependOn(c("XbarRchart"))
     jaspResults[["XbarPlot"]]$position <- 11
     XbarPlot <- jaspResults[["XbarPlot"]]
     XbarPlot$plotObject <- .XbarchartNoId(dataset = dataset, options = options)
     XbarPlot$dependOn(optionContainsValue= list(variables=variables))
  }
#Xbar & R cahrts
.capabilityTable <- function(options, dataset, diameter, subgroupsName){
  
  potentialCapabilityTable <- createJaspTable(title = gettext("Potential (Within) Capability"))
  
  USL <- options$upperSpecification
  LSL <- options$lowerSpecification
  targetValue <- options$targetValue
  sampleN <- length(dataset[[diameter]])
  
  getInfo <- function(x) {  
    c("diff" = diff(range(x)), "mean" = mean(x), "sd2" = (sd(x))^2)
  }
  
  subgroupInfo <- by(dataset[[diameter]], list(dataset[[subgroupsName]]), getInfo)
  subgroupInfoMatrix <- matrix(unlist(subgroupInfo), ncol = length(subgroupInfo[[1]]), byrow = TRUE)
  colnames(subgroupInfoMatrix) <- names(subgroupInfo[[1]])
  rownames(subgroupInfoMatrix) <- names(subgroupInfo)
  
  rBar <- sum(subgroupInfoMatrix[, "diff"]) / length(dataset[[subgroupsName]])
  d2 <- 2 #based on subgroup size (needs to be updated)
  
  
  subgroupInfo <- by(dataset[[diameter]], list(dataset[[subgroupsName]]), getInfo)
  subgroupInfoMatrix <- matrix(unlist(subgroupInfo), ncol = length(subgroupInfo[[1]]), byrow = TRUE)
  colnames(subgroupInfoMatrix) <- names(subgroupInfo[[1]])
  rownames(subgroupInfoMatrix) <- names(subgroupInfo)
  
  rBar <- sum(subgroupInfoMatrix[, "diff"]) / length(dataset[[subgroupsName]])
  d2 <- 2 #based on subgroup size (needs to be updated)
  
  stDevWithin <- rBar/d2
  stDevOverall <- sqrt((1/(sampleN-1)) * sum(subgroupInfoMatrix[, "sd2"]))
  grandAverage <- mean(subgroupInfoMatrix[, "mean"])
  
  processSigma <- numeric()    
  
  potentialCapabilityTable$addColumnInfo(name = "CP", type = "integer", title = gettext("CP"))
  potentialCapabilityTable$addColumnInfo(name = "CPL", type = "integer", title = gettext("CPL"))                                            
  potentialCapabilityTable$addColumnInfo(name = "CPU", type = "integer", title = gettext("CPU"))
  potentialCapabilityTable$addColumnInfo(name = "CPK", type = "integer", title = gettext("CPK"))  
  
  #Capability Indices
  CP <- (USL - LSL) / (6*stDevWithin)
  CPL <- (grandAverage - LSL) / (3*stDevWithin)
  CPU <- (USL - grandAverage) / (3*stDevWithin)
  CPK <- min(CPU, CPL)
  
  potentialCapabilityTable$addRows(list("CP" = round(CP,2),
                                   "CPL" = round(CPL,2),
                                   "CPU" = round(CPU,2),
                                   "CPK" = round(CPK,2)))
  
  overallCapabilityTable <- createJaspTable(title = gettext("Overall Capability"))
  overallCapabilityTable$addColumnInfo(name = "PP", type = "integer", title = gettext("PP"))
  overallCapabilityTable$addColumnInfo(name = "PPL", type = "integer", title = gettext("PPL"))                                            
  overallCapabilityTable$addColumnInfo(name = "PPU", type = "integer", title = gettext("PPU"))
  overallCapabilityTable$addColumnInfo(name = "PPK", type = "integer", title = gettext("PPK")) 
  overallCapabilityTable$addColumnInfo(name = "CPM", type = "integer", title = gettext("CPM"))
  #Performance Indices
  PP <- (USL - LSL) / (6*stDevOverall)
  PPL <- (grandAverage - LSL) / (3*stDevOverall)
  PPU <- (USL - grandAverage) / (3*stDevOverall)
  PPK <- min(PPU, PPL)
  CPM <- CP / sqrt(1 + (grandAverage - targetValue)^2 / processSigma^2)
  
  overallCapabilityTable$addRows(list("PP" = round(PP,2),                                                                                                                                         
                                 "PPL" = round(PPL,2),
                                 "PPU" = round(PPU,2),
                                 "PPK" = round(PPK,2),
                                 "CPM" = round(CPM,2)))
  
  capabilityTables <- list(potentialCapabilityTable, overallCapabilityTable)
  
  return(capabilityTables)
}  

  if(options$followupControlchart & is.null(jaspResults[["followupControlchart"]])) {
    jaspResults[["XbarPlot"]] <- createJaspPlot(title = "X bar chart", width = 1100, height = 400)
    jaspResults[["XbarPlot"]]$dependOn(c("XbarRchart"))
    jaspResults[["XbarPlot"]]$position <- 11
    XbarPlot <- jaspResults[["XbarPlot"]]
    XbarPlot$plotObject <- .XbarchartNoId(dataset = dataset, options = options)
    XbarPlot$dependOn(optionContainsValue= list(variables=variables))

    jaspResults[["RPlot"]] <- createJaspPlot(title = "R chart", width = 1100, height= 400)
    jaspResults[["RPlot"]]$dependOn(c("XbarRchart"))
    jaspResults[["RPlot"]]$position <- 11
    RPlot<- jaspResults[["RPlot"]]
    RPlot$plotObject <- .RchartNoId(dataset = dataset, options = options)
    RPlot$dependOn(optionContainsValue= list(variables=variables))
  }
