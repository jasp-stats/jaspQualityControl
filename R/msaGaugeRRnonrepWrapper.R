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

#' Gauge r&R Study (Non-replicable Measurements)
#'
msaGaugeRRnonrep <- function(
          data = NULL,
          version = "1",
          anova = TRUE,
          dataFormat = "longFormat",
          historicalSdValue = 3,
          measurementLongFormat = list(types = list(), value = ""),
          measurementsWideFormat = list(types = list(), value = list()),
          operatorLongFormat = list(types = list(), value = ""),
          operatorMeasurementPlot = FALSE,
          operatorWideFormat = list(types = list(), value = ""),
          partLongFormat = list(types = list(), value = ""),
          partMeasurementPlot = FALSE,
          partMeasurementPlotAllValues = FALSE,
          partWideFormat = list(types = list(), value = ""),
          plotHeight = 320,
          plotWidth = 480,
          processVariationReference = "studySd",
          rChart = FALSE,
          report = FALSE,
          reportAverageChartByOperator = TRUE,
          reportCharacteristic = TRUE,
          reportCharacteristicText = "",
          reportDate = TRUE,
          reportDateText = "",
          reportGaugeName = TRUE,
          reportGaugeNameText = "",
          reportGaugeNumber = TRUE,
          reportGaugeNumberText = "",
          reportGaugeTable = TRUE,
          reportLocation = TRUE,
          reportLocationText = "",
          reportMeasurementsByOperatorPlot = TRUE,
          reportMetaData = TRUE,
          reportPartName = TRUE,
          reportPartNameText = "",
          reportPerformedBy = TRUE,
          reportPerformedByText = "",
          reportRChartByOperator = TRUE,
          reportTitle = TRUE,
          reportTitleText = "",
          reportTolerance = TRUE,
          reportToleranceText = "",
          reportTrafficLightChart = TRUE,
          reportVariationComponents = TRUE,
          rule1 = TRUE,
          rule2 = TRUE,
          rule2Value = 7,
          rule3 = TRUE,
          rule3Value = 7,
          rule4 = TRUE,
          rule4Value = 2,
          rule5 = TRUE,
          rule5Value = 15,
          rule6 = TRUE,
          rule6Value = 8,
          rule7 = FALSE,
          rule7Value = 4,
          rule8 = FALSE,
          rule8Value = 14,
          studyVarianceMultiplierType = "sd",
          studyVarianceMultiplierValue = 6,
          testSet = "jaspDefault",
          tolerance = FALSE,
          toleranceValue = 1,
          trafficLightChart = FALSE,
          varianceComponentsGraph = TRUE,
          xBarChart = FALSE) {

   defaultArgCalls <- formals(jaspQualityControl::msaGaugeRRnonrep)
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

   optionsWithFormula <- c("dataFormat", "measurementLongFormat", "measurementsWideFormat", "operatorLongFormat", "operatorWideFormat", "partLongFormat", "partWideFormat", "processVariationReference", "studyVarianceMultiplierType", "testSet")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspQualityControl", "msaGaugeRRnonrep", "msaGaugeRRnonrep.qml", options, version, FALSE))
}