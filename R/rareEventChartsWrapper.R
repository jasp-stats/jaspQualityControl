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

#' Rare Event Charts
#'
rareEventCharts <- function(
          data = NULL,
          version = "0.97.1",
          dataType = "dataTypeDates",
          dataTypeDatesFormatDate = "dm",
          dataTypeDatesFormatTime = "HM",
          dataTypeDatesStructure = "dateTime",
          dataTypeIntervalTimeFormat = "HM",
          dataTypeIntervalType = "opportunities",
          gChart = TRUE,
          gChartHistoricalProportion = 0.5,
          gChartProportionSource = "data",
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
          rule2 = FALSE,
          rule2Value = 9,
          rule3 = FALSE,
          rule3Value = 6,
          rule8 = FALSE,
          rule8Value = 14,
          rule9 = FALSE,
          rule9Value = 3,
          stage = list(types = list(), value = ""),
          tChart = FALSE,
          tChartDistribution = "weibull",
          tChartDistributionParameterSource = "data",
          tChartHistoricalParametersScale = 2,
          tChartHistoricalParametersWeibullShape = 2,
          testSet = "jaspDefault",
          variable = list(types = list(), value = "")) {

   defaultArgCalls <- formals(jaspQualityControl::rareEventCharts)
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

   optionsWithFormula <- c("dataTypeDatesFormatDate", "dataTypeDatesFormatTime", "dataTypeDatesStructure", "dataTypeIntervalTimeFormat", "dataTypeIntervalType", "gChartProportionSource", "stage", "tChartDistribution", "tChartDistributionParameterSource", "testSet", "variable")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspQualityControl", "rareEventCharts", "rareEventCharts.qml", options, version, FALSE))
}