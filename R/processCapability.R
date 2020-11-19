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
  
  if (is.null(dataset)) {
    if (makeSubgroups) {
      dataset         <- .readDataSetToEnd(columns.as.numeric = diameter, columns.as.factor = subgroupsName)
      dataset.factors <- .readDataSetToEnd(columns = diameter, columns.as.factor = subgroupsName)
    } else {
      dataset         <- .readDataSetToEnd(columns.as.numeric = diameter)
      dataset.factors <- .readDataSetToEnd(columns = diameter)
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
    initialCapabilityAnalysis[["capabilityTable"]] <- .capabilityTable(options, dataset)
    
    
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
    followupCapabilityAnalysis[["capabilityTable"]] <- .capabilityTable(options, dataset)
  }
  
  return()
}

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
  
  subgroupRanges <- aggregate(dataset[[diameter]], list(dataset[[subgroupsName]]),range) 
  names(subgroupRanges)[1] <- "subgroup"
  names(subgroupRanges)[2] <- "range"
  
  rBar <- sum(subgroupRanges$range) / length(subgroupRanges$subgroup)
  d2 <- 2 #based on subgroup size
  
  stDevWithin <- rBar/d2
  
  subgroupDev <- aggregate(dataset[[diameter]], list(dataset[[subgroupsName]]),sd) 
  names(subgroupDev)[1] <- "subgroup"
  names(subgroupDev)[2] <- "standardDeviation"
  stDevOverall <- sqrt((1/sampleN) * (subgroupDev$standardDeviation)^2)
  
  processDataTable$addRows(list("lowerSpecificationLimit" = LSL,
                                "targetValue" = targetValue,
                                "upperSpecificationLimit" = USL,
                                "sampleMean" = sampleMean,
                                "sampleN" = sampleN,
                                "stDevWithin" = stDevWithin,
                                "stDevOverall" = stDevOverall))
  
  return(processDataTable)
  
}


.observedPerformanceTable <- function(options, dataset){
  observedPerformanceTable <- createJaspTable(title = gettext("Observed Performance"))
  
}

.expWithinPerformanceTable <- function(options, dataset){
  expWithinPerformanceTable <- createJaspTable(title = gettext("Exp. Within Performance"))
  
}

.expOverallPerformanceTable <- function(options, dataset){
  expOverallPerformanceTable <- createJaspTable(title = gettext("Exp. Overall Performance"))
  
}

.capabilityTable <- function(options, dataset){
  
  potentialCapabilityTable <- createJaspTable(title = gettext("Potential (Within) Capability"))
  
  USL <- options$upperSpecification
  LSL <- options$lowerSpecification
  targetValue <- options$targetValue
  sampleN <- length(dataset[[diameter]])
  
  subgroupRanges <- aggregate(dataset[[diameter]], list(dataset[[subgroupsName]]),range) 
  names(subgroupRanges)[1] <- "subgroup"
  names(subgroupRanges)[2] <- "range"
  
  rBar <- sum(subgroupRanges$range) / length(subgroupRanges$subgroup)
  d2 <- 2 #based on subgroup size
  
  stDevWithin <- rBar/d2
  
  subgroupDev <- aggregate(dataset[[diameter]], list(dataset[[subgroupsName]]),sd) 
  names(subgroupDev)[1] <- "subgroup"
  names(subgroupDev)[2] <- "standardDeviation"
  stDevOverall <- sqrt((1/sampleN) * (subgroupDev$standardDeviation)^2)
  
  targetValue <- options$targetValue
  
  subgroupAverage <- aggregate(dataset[[diameter]], list(dataset[[subgroupsName]]),mean) 
  names(subgroupDev)[1] <- "subgroup"
  names(subgroupDev)[2] <- "average"
  grandAverage <- mean(subgroupAverage$average)
  
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
  potentialCapabilityTable$addRows("CP" = CP,
                                   "CPL" = CPL,
                                   "CPU" = CPU,
                                   "CPK" = CPK)
  
  overallCapabilityTable <- createJaspTable(title = gettext("Overall Capability"))
  overallCapabilityTable$addColumnInfo(name = "PP", type = "integer", title = gettext("CP"))
  overallCapabilityTable$addColumnInfo(name = "PPL", type = "integer", title = gettext("CPL"))                                            
  overallCapabilityTable$addColumnInfo(name = "PPU", type = "integer", title = gettext("CPU"))
  overallCapabilityTable$addColumnInfo(name = "PPK", type = "integer", title = gettext("CPK")) 
  overallCapabilityTable$addColumnInfo(name = "CPM", type = "integer", title = gettext("CPM"))
  
  ´#Performance Indices
  PP <- (USL - LSL) / (6*stDevOverall)
  PPL <- (grandAverage - LSL) / (3*stDevOverall)
  PPU <- (USL - grandAverage) / (3*stDevOverall)
  PPK <- min(PPU, PPL)
  CPM <- CP / sqrt(1 + (grandAverage - targetValue)^2 / processSigma^2)
  overallCapabilityTable$addRows("PP" = PP,                                                                                                                                         
                                 "PPL" = PPL,
                                 "PPU" = PPU,
                                 "PPK" = PPK,
                                 "CPM" = CPM)
  return()                                          
}  


