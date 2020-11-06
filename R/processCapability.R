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
  
  variables <- unlist(options$diameter)
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
    initialCapabilityAnalysis[["processDataTable"]] <- .processDataTable(options, dataset)
    
    
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
    followupCapabilityAnalysis[["processDataTable"]] <- .processDataTable(options, dataset)
  }
  
  return()
}
  
.processDataTable <- function(options, dataset){
  
  processDataTable <- createJaspTable(title = gettext("Process Data"))
  processDataTable$dependOn(c("upperSpecification","lowerSpecification","targetValue","diameter","subgroups"))

  
  processDataTable$addColumnInfo(name = "lowerSpecificationLimit", type = "number", title = gettext("LSL"), format = "dp:")
  processDataTable$addColumnInfo(name = "targetValue", type = "number", title = gettext("Target"), format = "dp:0")
  processDataTable$addColumnInfo(name = "upperSpecificationLimit", type = "number", title = gettext("USL"), format = "dp:0")
  processDataTable$addColumnInfo(name = "sampleMean", type = "number", title = gettext("Sample Mean"), format = "dp:4")
  processDataTable$addColumnInfo(name = "sampleN", type = "number", title = gettext("Sample N"), format = "dp:0")
  processDataTable$addColumnInfo(name = "stDevWithin", type = "number", title = gettext("StDev (Within)"), format = "dp:4")
  processDataTable$addColumnInfo(name = "stDevOverall", type = "number", title = gettext("StDev (Overall)"), format = "dp:4")
  
  upperSpecificationLimit <- options$upperSpecification
  lowerSpecificationLimit <- options$lowerSpecification
  targetValue <- options$targetValue
  sampleMean <- numeric()
  sampleN <- numeric()
  
  stDevWithin <- numeric()
  stDevOverall <- sd(options$diameter)
  
  processDataTable$addRows(list("lowerSpecificationLimit" = lowerSpecificationLimit,
                                "targetValue" = targetValue,
                                "upperSpecificationLimit" = upperSpecificationLimit,
                                "sampleMean" = sampleMean,
                                "sampleN" = sampleN,
                                "stDevWithin" = stDevWithin,
                                "stDevOverall" = stDevOverall))
  
  return(processDataTable)
  
}


  

  