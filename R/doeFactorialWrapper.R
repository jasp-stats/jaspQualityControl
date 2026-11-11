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
#' A factorial design is an experimental setup used to study the effects of multiple factors by varying them simultaneously. Each factor is tested at different levels and in different combinations with other factors. This design allows for the examination of both the main effects of each factor and the interactions between factors. It provides a systematic approach to understanding how different variables influence the outcome.
#'
#' @param actualExporter, Check to save the selected design to the specified file.
#'    Defaults to \code{FALSE}.
#' @param blocks, Select the number of blocks in the design. Not available for split-plot and general full factorial designs.
#' @param categoricalNoLevels, Set the maximum number of levels for the factors in the design. Only applicable for general full factorial designs, else factors always have two levels. If a factor has fewer levels than the maximum number of levels in the design, leave the remaining level cells empty or add a space.
#' @param categoricalVariables, Set the names of the factors and give the values for each level.
#' @param centerpoints, Select the number of centre points per block to add to the design. Not available for split-plot and general full factorial designs.
#' @param codedOutput, Display the factor levels in coded units instead of the given units.
#'    Defaults to \code{FALSE}.
#' @param displayDesign, Display the design table in the output. If a design table is shown in the options, one of its designs needs to be selected first.
#'    Defaults to \code{TRUE}.
#' @param exportDesignFile, Option to save the design as a .csv file. Specify a name and path for the file or use the browse option.
#' @param factorialDesignTypeSplitPlotNumberHardToChangeFactors, Select the number of hard-to-change factors.
#' @param factorialType, The type of factorial design to generate.
#' \itemize{
#'   \item \code{"factorialTypeDefault"}: Generate a two-level factorial design using the default generator.
#'   \item \code{"factorialTypeSpecify"}: Generate a two-level factorial design using a custom generator.
#'   \item \code{"factorialTypeSplit"}: Generate a two-level split-plot design containing hard-to-change factors.
#'   \item \code{"generalFullFactorial"}: Generate a general full factorial design.
#' }
#' @param factorialTypeSpecifyGenerators, Specify the generator for the design.
#' @param numberOfCategorical, Set the total number of factors in the design.
#' @param replications, Select the number of replications of the whole design.
#' @param runOrder, Whether to sort the runs in the design by run order or standard order.
#' \itemize{
#'   \item \code{"runOrderRandom"}: Sort the runs in the randomized order in which they should be carried out.
#'   \item \code{"runOrderStandard"}: Sort the runs in standard (non-randomized) order.
#' }
#' @param selectedDesign2, Select one of the pre-set designs by clicking on it in the table.
#' @param showAliasStructure, Check to display the alias structure of the generated design. Only available for two-level factorial designs with the default generator.
#'    Defaults to \code{FALSE}.
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