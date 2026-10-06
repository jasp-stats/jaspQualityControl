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

#' Multivariate Control Charts
#'
#' @param addTsqToData, Adds the computed Hotelling T² values as a new column in the dataset.
#'    Defaults to \code{FALSE}.
#' @param axisLabels, Optional column used as x-axis labels (e.g., a timestamp or row identifier).
#' @param centerTable, Show the mean of each variable.
#'    Defaults to \code{FALSE}.
#' @param confidenceLevel, Custom confidence level for the control limits. Only active when automatic confidence level is unchecked.
#' @param confidenceLevelAutomatic, When checked, confidence level is automatically set to (1 - 0.0027)^p, where p is the number of variables. This is the standard Bonferroni-adjusted level for multivariate charts.
#'    Defaults to \code{TRUE}.
#' @param covarianceMatrixTable, Show the sample covariance matrix of the selected variables.
#'    Defaults to \code{FALSE}.
#' @param plotColorScheme, Choose colors for limits and out-of-control points. The colorblind-friendly scheme uses orange instead of red.
#' @param stage, Optional grouping variable that splits the data into a training phase (Phase I) and a test phase (Phase II). Control limits are estimated from the training phase and applied to the test phase for anomaly detection.
#' @param tSquaredValuesTable, Show a table with the T² statistic for each observation, along with the UCL and in/out-of-control status.
#'    Defaults to \code{FALSE}.
#' @param trainingLevel, Select which level of the stage variable represents the training (in-control) phase. Control limits and the covariance matrix are estimated from this phase only.
#' @param variables, Two or more continuous quality characteristics to monitor jointly using a Hotelling T² chart.
multivariateControlCharts <- function(
          data = NULL,
          version = "0.97.1",
          addTsqToData = FALSE,
          axisLabels = list(types = list(), value = ""),
          centerTable = FALSE,
          confidenceLevel = 0.95,
          confidenceLevelAutomatic = TRUE,
          covarianceMatrixTable = FALSE,
          plotColorScheme = "standard",
          plotHeight = 320,
          plotWidth = 480,
          stage = list(types = list(), value = ""),
          tSquaredValuesTable = FALSE,
          trainingLevel = "",
          tsqColumn = "",
          variables = list(types = list(), value = list())) {

   defaultArgCalls <- formals(jaspQualityControl::multivariateControlCharts)
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

   optionsWithFormula <- c("axisLabels", "plotColorScheme", "stage", "trainingLevel", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspQualityControl", "multivariateControlCharts", "multivariateControlCharts.qml", options, version, TRUE))
}