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

processCapability <- function(jaspResults, dataset, options, ...){
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
    # Process Capability of Diameter (implement myself)
    if (options$initialCapabilityAnalysis) {
    
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
    # Process Capability of Diameter (implement myself)
    if (options$followupCapabilityAnalysis) {
    
    }
  
}