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

#' @export
rareEventCharts <- function(jaspResults, dataset, options) {
  # reading variables in from the GUI
  variable <- unlist(options[["variable"]])
  stages <- unlist(options[["stage"]])
  variable <- variable[variable != ""]
  stages <- stages[stages != ""]
  factorVariables <- c(variable, stages)
  ready <- length(variable) == 1



  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.factor = factorVariables)
  }


  # Checking for errors in the dataset
  .hasErrors(dataset, type = c('infinity'),
             infinity.target = c(options$variable),
             exitAnalysisIfErrors = TRUE)


  # If variable is data/time transform into same format


  # Transform variable to intervals between events



  # G chart
  if (options[["gChart"]] && is.null(jaspResults[["gChart"]])) {
    jaspResults[["gChart"]] <- .gChart(intervals, stages, intervalType, options, ready)
  }

  # T chart
  if (options[["tChart"]] && is.null(jaspResults[["tChart"]])) {
    jaspResults[["tChart"]] <- .tChart(intervals, stages, intervalType, options, ready)
  }
}


.gChart <- function(intervals, stages, intervalType, options, ready) {
  plot <-  createJaspPlot(title = gettext("G chart"), width = 1200, height = 500)
  plot$dependOn(c(""))

  if (!ready)
    return(plot)

  plotObject <- .rareEventPlottingFunction()
  plot$plotObject <- plotObject

  return(plot)

}

.tChart <- function(intervals, stages, intervalType, options, ready) {
  plot <-  createJaspPlot(title = gettext("T chart"), width = 1200, height = 500)
  plot$dependOn(c(""))

   if (!ready)
    return(plot)

  plotObject <- .rareEventPlottingFunction()
  plot$plotObject <- plotObject

  return(plot)
}

.rareEventPlottingFunction <- function(intervals,
                                       intervalType = c("days", "hours", "minutes", "opportunities"),
                                       chartType = c("g", "t"),
                                       tChartDistribution = c("weibull", "exponential")) {

}
