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
      dataset         <- na.omit(.readDataSetToEnd(columns.as.numeric = variables, columns.as.factor = subgroupsName))
      dataset.factors <- na.omit(.readDataSetToEnd(columns = variables, columns.as.factor = subgroupsName))
    } else {
      dataset         <- na.omit(.readDataSetToEnd(columns.as.numeric = variables))
      dataset.factors <- na.omit(.readDataSetToEnd(columns = variables))
    }
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
}
