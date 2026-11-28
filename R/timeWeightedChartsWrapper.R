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

#' Time Weighted Charts
#'
#' @param stagesLongFormat, Column that assigns a stage to each subgroup. A new stage starts each time the stage value changes from one row to the next, so a recurring value (e.g., A, B, A) forms separate stages, plotted in row order and each with its own limits and estimates.
#' @param stagesWideFormat, Column that assigns a stage to each subgroup. A new stage starts each time the stage value changes from one row to the next, so a recurring value (e.g., A, B, A) forms separate stages, plotted in row order and each with its own limits and estimates.
#' @param ... options that are not an argument of this function: the options of an older version of the
#'   analysis, to be used together with `version`. They are upgraded to the options of this version.
timeWeightedCharts <- function(
          data = NULL,
          version = "1",
          axisLabels = list(types = list(), value = ""),
          cumulativeSumChart = TRUE,
          cumulativeSumChartAverageMovingRangeLength = 2,
          cumulativeSumChartNumberSd = 4,
          cumulativeSumChartSdMethod = "averageMovingRange",
          cumulativeSumChartSdSource = "data",
          cumulativeSumChartSdValue = 3,
          cumulativeSumChartShiftSize = 0.5,
          cumulativeSumChartTarget = 0,
          dataFormat = "longFormat",
          exponentiallyWeightedMovingAverageChart = FALSE,
          exponentiallyWeightedMovingAverageChartLambda = 0.3,
          exponentiallyWeightedMovingAverageChartMovingRangeLength = 2,
          exponentiallyWeightedMovingAverageChartSdMethod = "averageMovingRange",
          exponentiallyWeightedMovingAverageChartSdSource = "data",
          exponentiallyWeightedMovingAverageChartSdValue = 3,
          exponentiallyWeightedMovingAverageChartSigmaControlLimits = 3,
          groupingVariableMethod = "newLabel",
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
          stagesLongFormat = list(types = list(), value = ""),
          stagesWideFormat = list(types = list(), value = ""),
          subgroup = list(types = list(), value = ""),
          subgroupSizeType = "individual",
          ...) {

   options <- mget(setdiff(names(formals(sys.function())), "..."), envir = environment(), inherits = FALSE)
   options <- c(options, list(...))
   if (!all(nzchar(names(options))) || anyDuplicated(names(options)))
      stop("The extra options given to timeWeightedCharts must be named, each name once.")
   if (...length() > 0L && missing(version))
      stop("timeWeightedCharts has no option ", paste(names(list(...)), collapse = ", "), ". Options of an older version need the `version` they were written for.")
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("axisLabels", "cumulativeSumChartSdMethod", "cumulativeSumChartSdSource", "dataFormat", "exponentiallyWeightedMovingAverageChartSdMethod", "exponentiallyWeightedMovingAverageChartSdSource", "groupingVariableMethod", "measurementLongFormat", "measurementsWideFormat", "stagesLongFormat", "stagesWideFormat", "subgroup")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspQualityControl", "timeWeightedCharts", "timeWeightedCharts.qml", options, version, FALSE))
}