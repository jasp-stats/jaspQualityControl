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

#' Create Factorial Worksheet
#'
doeFactorial <- function(
          data = NULL,
          version = "1",
          actualExporter = FALSE,
          blocks = "1",
          categoricalNoLevels = 2,
          categoricalVariables = list(list(levels = list("Row 0", "Row 1", "Row 2"), name = "data 1", values = list("A", "B", "C")), list(levels = list("Row 0", "Row 1", "Row 2"), name = "data 2", values = list("a", "a", "a")), list(levels = list("Row 0", "Row 1", "Row 2"), name = "data 3", values = list("b", "b", "b"))),
          centerpoints = 0,
          codedOutput = FALSE,
          displayDesign = TRUE,
          exportDesignFile = "",
          factorialDesignTypeSplitPlotNumberHardToChangeFactors = 1,
          factorialType = "factorialTypeDefault",
          factorialTypeSpecifyGenerators = "",
          numberOfCategorical = 3,
          plotHeight = 320,
          plotWidth = 480,
          replications = 1,
          runOrder = "runOrderRandom",
          seed = 1,
          selectedCol = -1,
          selectedDesign2 = list(list(levels = list("Row 0", "Row 1"), name = "data 1", values = list(4, 8)), list(levels = list("Row 0", "Row 1"), name = "data 2", values = list(0, 0))),
          selectedRow = -1,
          setSeed = FALSE,
          showAliasStructure = FALSE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("blocks", "categoricalVariables", "factorialTypeSpecifyGenerators", "selectedDesign2")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspQualityControl", "doeFactorial", "doeFactorial.qml", options, version, FALSE))
}