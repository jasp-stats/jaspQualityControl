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

processControl <- function(jaspResults, dataset, options){

  # Make the Process ID and clean the dataset
  variables <- unlist(options$variables)
  ID <- options$ProcessID
  makeID <- ID != ""
  numberMissing <- 0

  if (is.null(dataset)) {
    if (makeID) {
      dataset         <- .readDataSetToEnd(columns.as.numeric = variables, columns.as.factor = ID)
      dataset.factors <- .readDataSetToEnd(columns = variables, columns.as.factor=ID)
    } else {
      dataset         <- .readDataSetToEnd(columns.as.numeric = variables)
      dataset.factors <- .readDataSetToEnd(columns = variables)
    }
  }
  # Remove missing values
  if (makeID & length(variables) > 0) {
    processID <- dataset[[.v(ID)]]
    dataset <- dataset[!is.na(processID), ]
    dataset.factors <- dataset.factors[!is.na(processID), ]
    processID <- na.omit(processID)
    dataset <- na.omit(dataset)
  } else {dataset <- na.omit(dataset)}

  #X bar R Chart
  if(options$XbarRchart && is.null(jaspResults[["XbarRchart"]])){

    jaspResults[["XbarPlot"]] <- createJaspPlot(title = "X bar chart", width = 1100, height = 400)
    jaspResults[["XbarPlot"]]$dependOn(c("XbarRchart"))
    jaspResults[["XbarPlot"]]$position <- 11
    XbarPlot <- jaspResults[["XbarPlot"]]
    XbarPlot$plotObject <- .XbarchartNoId(dataset = dataset, options = options)

    jaspResults[["RPlot"]] <- createJaspPlot(title = "R chart", width = 1100, height= 400)
    jaspResults[["RPlot"]]$dependOn(c("XbarRchart"))
    jaspResults[["RPlot"]]$position <- 11
    RPlot<- jaspResults[["RPlot"]]
    RPlot$plotObject <- .RchartNoId(dataset = dataset, options = options)
  }
}
