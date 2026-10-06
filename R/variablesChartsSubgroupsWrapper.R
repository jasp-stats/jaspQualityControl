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

#' Variables Charts for Subgroups
#'
variablesChartsSubgroups <- function(
          data = NULL,
          version = "0.97.1",
          axisLabels = list(types = list(), value = ""),
          chartType = "xBarAndS",
          controlLimitsNumberOfSigmas = 3,
          dataFormat = "longFormat",
          fixedSubgroupSizeValue = 5,
          groupingVariableMethod = "newLabel",
          knownParameters = FALSE,
          knownParametersMean = 0,
          knownParametersSd = 3,
          manualSubgroupSizeValue = 5,
          measurementLongFormat = list(types = list(), value = ""),
          measurementsWideFormat = list(types = list(), value = list()),
          plotHeight = 320,
          plotWidth = 480,
          report = FALSE,
          reportChartName = TRUE,
          reportChartNameText = "",
          reportDate = TRUE,
          reportDateText = "",
          reportFootnote = TRUE,
          reportFootnoteText = "",
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
          stagesLongFormat = list(types = list(), value = ""),
          stagesWideFormat = list(types = list(), value = ""),
          subgroup = list(types = list(), value = ""),
          subgroupSizeType = "manual",
          subgroupSizeUnequal = "actualSizes",
          testSet = "jaspDefault",
          warningLimits = FALSE,
          xBarAndSUnbiasingConstant = TRUE) {

   defaultArgCalls <- formals(jaspQualityControl::variablesChartsSubgroups)
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

   optionsWithFormula <- c("axisLabels", "dataFormat", "groupingVariableMethod", "measurementLongFormat", "measurementsWideFormat", "stagesLongFormat", "stagesWideFormat", "subgroup", "testSet")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspQualityControl", "variablesChartsSubgroups", "variablesChartsSubgroups.qml", options, version, FALSE))
}