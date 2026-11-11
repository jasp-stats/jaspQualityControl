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
#' A response surface design is an experimental framework used to optimize and understand the relationships between several explanatory variables and one or more response variables. By systematically varying the input variables, researchers can explore the effects of these variables on the response, identify optimal conditions, and understand the interactions between variables. Response surface designs are particularly useful in situations where the goal is to optimize a process or product, as they provide a detailed map of the response landscape, enabling precise fine-tuning and improvement.
#'
#' @param actualExporter, Check to save the selected design to the specified file.
#'    Defaults to \code{FALSE}.
#' @param alphaType, Select how the alpha value (distance of the axial points from the centre) in the design is determined.
#' \itemize{
#'   \item \code{"default"}: Use the default alpha value displayed in the design table.
#'   \item \code{"faceCentered"}: Use a face centred alpha value, i.e. the axial points lie on the faces of the cube.
#'   \item \code{"custom"}: Enter a custom alpha value.
#' }
#' @param categoricalNoLevels, Set the maximum number of levels for the discrete factors in the design. Continuous factors always have two defined levels (low/high). If a discrete factor has fewer levels than the maximum number of levels in the design, leave the remaining level cells empty or add a space.
#' @param categoricalVariables, Set the names of the discrete factors and give the values for each level.
#' @param centerPointType, Select how the number of centre points in the design is determined.
#' \itemize{
#'   \item \code{"default"}: Use the default number of centre points displayed in the design table.
#'   \item \code{"custom"}: Enter a custom number of centre points.
#' }
#' @param codedOutput, Display the factor levels in coded units instead of the given units.
#'    Defaults to \code{FALSE}.
#' @param continuousVariables, Set the names and the low and high levels of the continuous factors.
#' @param customAxialBlock, Enter the number of axial centre points.
#' @param customCubeBlock, Enter the number of cube centre points.
#' @param designType, Select the type of response surface design to generate. See the background section below.
#' \itemize{
#'   \item \code{"centralCompositeDesign"}: Generate a central composite design.
#'   \item \code{"boxBehnkenDesign"}: Generate a Box-Behnken design.
#' }
#' @param displayDesign, Display the design table in the output. If a design table is shown in the options, one of its designs needs to be selected first.
#'    Defaults to \code{TRUE}.
#' @param exportDesignFile, Option to save the design as a .csv file. Specify a name and path for the file or use the browse option.
#' @param numberOfCategorical, Set the total number of discrete factors in the design.
#' @param numberOfContinuous, Set the total number of continuous factors in the design.
#' @param replicates, Select the number of replications of the whole design.
#' @param runOrder, Whether to sort the runs in the design by run order or standard order.
#' \itemize{
#'   \item \code{"runOrderRandom"}: Sort the runs in the randomized order in which they should be carried out.
#'   \item \code{"runOrderStandard"}: Sort the runs in standard (non-randomized) order.
#' }
#' @param selectedDesign2, Select one of the pre-set designs by clicking on it in the table.
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

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("categoricalVariables", "continuousVariables", "selectedDesign2")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspQualityControl", "doeResponseSurfaceMethodology", "doeResponseSurfaceMethodology.qml", options, version, FALSE))
}