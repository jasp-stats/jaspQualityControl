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

#' Type 4 Linearity Study
#'
#' @param ... options that are not an argument of this function: the options of an older version of the
#'   analysis, to be used together with `version`. They are upgraded to the options of this version.
msaGaugeLinearity <- function(
          data = NULL,
          version = "1",
          biasTable = TRUE,
          linearityAndBiasPlot = TRUE,
          linearityTable = TRUE,
          manualProcessVariation = FALSE,
          manualProcessVariationValue = 1,
          measurement = list(types = list(), value = ""),
          part = list(types = list(), value = ""),
          percentProcessVariationPlot = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          standard = list(types = list(), value = ""),
          ...) {

   options <- mget(setdiff(names(formals(sys.function())), "..."), envir = environment(), inherits = FALSE)
   options <- c(options, list(...))
   if (!all(nzchar(names(options))) || anyDuplicated(names(options)))
      stop("The extra options given to msaGaugeLinearity must be named, each name once.")
   if (...length() > 0L && missing(version))
      stop("msaGaugeLinearity has no option ", paste(names(list(...)), collapse = ", "), ". Options of an older version need the `version` they were written for.")
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("measurement", "part", "standard")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspQualityControl", "msaGaugeLinearity", "msaGaugeLinearity.qml", options, version, FALSE))
}