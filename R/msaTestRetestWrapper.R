#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' Test-retest (Range method)
#'
msaTestRetest <- function(
          data = NULL,
          version = "0.97.1",
          dataFormat = "longFormat",
          manualProcessSd = FALSE,
          manualProcessSdValue = 1,
          measurementLongFormat = list(types = list(), value = ""),
          measurementsWideFormat = list(types = list(), value = list()),
          operator = list(types = list(), value = ""),
          partLongFormat = list(types = list(), value = ""),
          partWideFormat = list(types = list(), value = ""),
          plotHeight = 320,
          plotWidth = 480,
          rChart = FALSE,
          repeatabilityAndReproducibilityTable = TRUE,
          runChartPart = FALSE,
          scatterPlotMeasurement = TRUE,
          scatterPlotMeasurementAllValues = FALSE,
          scatterPlotMeasurementFitLine = TRUE,
          tolerance = FALSE,
          toleranceValue = 1,
          trafficLightChart = FALSE) {

   defaultArgCalls <- formals(jaspQualityControl::msaTestRetest)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("dataFormat", "measurementLongFormat", "measurementsWideFormat", "operator", "partLongFormat", "partWideFormat")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspQualityControl", "msaTestRetest", "msaTestRetest.qml", options, version, FALSE))
}