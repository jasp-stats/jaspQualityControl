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

#' Create Response Surface Worksheet
#'
doeResponseSurfaceMethodology <- function(
          data = NULL,
          version = "1",
          actualExporter = FALSE,
          alphaType = "default",
          categoricalNoLevels = 2,
          categoricalVariables = list(list(levels = list(), name = "data 1", values = list()), list(levels = list(), name = "data 2", values = list()), list(levels = list(), name = "data 3", values = list())),
          centerPointType = "default",
          codedOutput = FALSE,
          continuousVariables = list(list(levels = list("Row 0", "Row 1"), name = "data 1", values = list("A", "B")), list(levels = list("Row 0", "Row 1"), name = "data 2", values = list(-1, -1)), list(levels = list("Row 0", "Row 1"), name = "data 3", values = list(1, 1))),
          customAlphaValue = 0,
          customAxialBlock = 0,
          customCubeBlock = 0,
          designType = "centralCompositeDesign",
          displayDesign = TRUE,
          exportDesignFile = "",
          numberOfCategorical = 0,
          numberOfContinuous = 2,
          plotHeight = 320,
          plotWidth = 480,
          replicates = 1,
          runOrder = "runOrderRandom",
          seed = 1,
          selectedCol = -1,
          selectedDesign2 = list(list(levels = list("Row 0", "Row 1"), name = "data 1", values = list(13, 14)), list(levels = list("Row 0", "Row 1"), name = "data 2", values = list(5, 6)), list(levels = list("Row 0", "Row 1"), name = "data 3", values = list(0, 3)), list(levels = list("Row 0", "Row 1"), name = "data 4", values = list(0, 3)), list(levels = list("Row 0", "Row 1"), name = "data 5", values = list(1.414, 1.414))),
          selectedRow = -1,
          setSeed = FALSE) {

   defaultArgCalls <- formals(jaspQualityControl::doeResponseSurfaceMethodology)
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

   optionsWithFormula <- c("categoricalVariables", "continuousVariables", "selectedDesign2")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspQualityControl", "doeResponseSurfaceMethodology", "doeResponseSurfaceMethodology.qml", options, version, FALSE))
}