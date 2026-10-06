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

#' Control Charts for Attributes
#'
attributesCharts <- function(
          data = NULL,
          version = "1",
          attributesChart = "defectives",
          attributesChartDefectivesChartType = "npChart",
          attributesChartDefectsChartType = "cChart",
          defectiveOrDefect = list(types = list(), value = ""),
          plotHeight = 320,
          plotWidth = 480,
          report = FALSE,
          reportAppraiser = TRUE,
          reportAppraiserText = "",
          reportFrequency = TRUE,
          reportFrequencyText = "",
          reportId = TRUE,
          reportIdText = "",
          reportMeasurementName = TRUE,
          reportMeasurementNameText = "",
          reportMeasusrementSystemName = TRUE,
          reportMeasusrementSystemNameText = "",
          reportMetaData = TRUE,
          reportPerformedBy = TRUE,
          reportPerformedByText = "",
          reportSubgroupSize = TRUE,
          reportSubgroupSizeText = "",
          reportTime = TRUE,
          reportTimeText = "",
          reportTitle = TRUE,
          reportTitleText = "",
          timeStamp = list(types = list(), value = ""),
          total = list(types = list(), value = "")) {

   defaultArgCalls <- formals(jaspQualityControl::attributesCharts)
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

   optionsWithFormula <- c("defectiveOrDefect", "timeStamp", "total")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspQualityControl", "attributesCharts", "attributesCharts.qml", options, version, FALSE))
}