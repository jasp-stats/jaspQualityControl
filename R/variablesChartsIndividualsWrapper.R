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

#' Variables Charts for Individuals
#'
variablesChartsIndividuals <- function(
          data = NULL,
          version = "1",
          autocorrelationPlot = FALSE,
          autocorrelationPlotCiLevel = 0.95,
          autocorrelationPlotLagsNumber = 25,
          axisLabels = list(types = list(), value = ""),
          controlLimitsNumberOfSigmas = 3,
          measurement = list(types = list(), value = ""),
          plotHeight = 320,
          plotWidth = 480,
          report = FALSE,
          reportAutocorrelationChart = FALSE,
          reportChartName = TRUE,
          reportChartNameText = "",
          reportDate = TRUE,
          reportDateText = "",
          reportFootnote = TRUE,
          reportFootnoteText = "",
          reportIMRChart = TRUE,
          reportLocation = TRUE,
          reportLocationText = "",
          reportMeasurementName = TRUE,
          reportMeasurementNameText = "",
          reportMetaData = TRUE,
          reportPerformedBy = TRUE,
          reportPerformedByText = "",
          reportPrintDate = TRUE,
          reportPrintDateText = "",
          reportSubtitle = TRUE,
          reportSubtitleText = "",
          reportTitle = TRUE,
          reportTitleText = "",
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
          stage = list(types = list(), value = ""),
          testSet = "jaspDefault",
          xmrChart = TRUE,
          xmrChartMovingRangeLength = 2) {

   defaultArgCalls <- formals(jaspQualityControl::variablesChartsIndividuals)
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

   optionsWithFormula <- c("axisLabels", "measurement", "stage", "testSet")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspQualityControl", "variablesChartsIndividuals", "variablesChartsIndividuals.qml", options, version, FALSE))
}