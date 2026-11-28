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

#' Attributes Agreement Analysis
#'
#' @param positiveReference, The rating category treated as positive in the kappa studies, as it is written in the results (for example, "Yes", "Go", "Good").
#' @param positiveReferenceCategory, Whether the positive reference is the accept (good) or the reject (defect) category. The miss rate (bad part accepted) and false alarm rate (good part rejected) of the study effectiveness summary are computed relative to the accept category.
#' \itemize{
#'   \item \code{"accept"}
#'   \item \code{"reject"}
#' }
#' @param ... options that are not an argument of this function: the options of an older version of the
#'   analysis, to be used together with `version`. They are upgraded to the options of this version.
msaAttribute <- function(
          data = NULL,
          version = "1",
          cohensKappa = FALSE,
          dataFormat = "longFormat",
          fleissKappa = FALSE,
          kendallsTau = FALSE,
          measurementLongFormat = list(types = list(), value = ""),
          measurementsWideFormat = list(types = list(), value = list()),
          operatorLongFormat = list(types = list(), value = ""),
          operatorWideFormat = list(types = list(), value = ""),
          partLongFormat = list(types = list(), value = ""),
          partWideFormat = list(types = list(), value = ""),
          plotHeight = 320,
          plotWidth = 480,
          positiveReference = "",
          positiveReferenceCategory = "accept",
          standardLongFormat = list(types = list(), value = ""),
          standardWideFormat = list(types = list(), value = ""),
          ...) {

   options <- mget(setdiff(names(formals(sys.function())), "..."), envir = environment(), inherits = FALSE)
   options <- c(options, list(...))
   if (!all(nzchar(names(options))) || anyDuplicated(names(options)))
      stop("The extra options given to msaAttribute must be named, each name once.")
   if (...length() > 0L && missing(version))
      stop("msaAttribute has no option ", paste(names(list(...)), collapse = ", "), ". Options of an older version need the `version` they were written for.")
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("dataFormat", "measurementLongFormat", "measurementsWideFormat", "operatorLongFormat", "operatorWideFormat", "partLongFormat", "partWideFormat", "standardLongFormat", "standardWideFormat")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspQualityControl", "msaAttribute", "msaAttribute.qml", options, version, FALSE))
}