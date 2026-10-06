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

#' Analyse Design
#'
#' @param stepwiseMethodFactorial, Specify the order in which the predictors are entered into the model. A block of one or more predictors represents one step in the hierarchy. Note that the present release does not allow for more than one block. The backward and forward method are based on model AIC.
#' \itemize{
#'   \item \code{"enter"} (default) : All predictors are entered into the model simultaneously.
#'   \item \code{"backward"}: Starting with the full model, predictors are removed sequentially based on AIC.
#'   \item \code{"forward"}: Starting with the intercept-only model, predictors are entered sequentially based on AIC.
#'   \item \code{"both"}: Starting with the intercept-only model, predictors are entered or removed sequentially based on AIC, combining both forward addition and backward elimination at each step.
#' }
#' @param stepwiseMethodResponseSurface, Specify the order in which the predictors are entered into the model. A block of one or more predictors represents one step in the hierarchy. Note that the present release does not allow for more than one block.
#' \itemize{
#'   \item \code{"enter"} (default) : All predictors are entered into the model simultaneously.
#'   \item \code{"backward"}: Starting with the full model, predictors are removed sequentially based on AIC.
#'   \item \code{"forward"}: Starting with the intercept-only model, predictors are entered sequentially based on AIC.
#'   \item \code{"both"}: Starting with the intercept-only model, predictors are entered or removed sequentially based on AIC, combining both forward addition and backward elimination at each step.
#' }
doeAnalysis <- function(
          data = NULL,
          version = "1",
          blocksFactorial = list(types = list(), value = ""),
          blocksResponseSurface = list(types = list(), value = ""),
          codeFactors = TRUE,
          codeFactorsManualTable = list(optionKey = "predictors", types = list(), value = list()),
          codeFactorsMethod = "automatic",
          continuousFactorsFactorial = list(types = list(), value = list()),
          continuousFactorsResponseSurface = list(types = list(), value = list()),
          contourSurfacePlot = FALSE,
          contourSurfacePlotResponseDivision = 5,
          contourSurfacePlotType = "contourPlot",
          contourSurfacePlotVariables = list(types = list(), value = list()),
          covariates = list(types = list(), value = list()),
          dependentFactorial = list(types = list(), value = list()),
          dependentResponseSurface = list(types = list(), value = list()),
          designType = "factorialDesign",
          fixedFactorsFactorial = list(types = list(), value = list()),
          fixedFactorsResponseSurface = list(types = list(), value = list()),
          fourInOneResidualPlot = FALSE,
          highestOrder = TRUE,
          histogramBinWidthType = "sturges",
          histogramManualNumberOfBins = 30,
          modelTerms = list(optionKey = "components", types = list(), value = list()),
          normalEffectsPlot = FALSE,
          optimizationPlot = TRUE,
          optimizationPlotCustomParameterValues = list(optionKey = "variable", types = list(), value = list()),
          optimizationPlotCustomParameters = FALSE,
          optimizationPlotPredictionType = "response",
          optimizationSolutionTable = TRUE,
          order = 2,
          plotFitted = FALSE,
          plotHeight = 320,
          plotHist = FALSE,
          plotNorm = FALSE,
          plotPareto = FALSE,
          plotRunOrder = FALSE,
          plotWidth = 480,
          responseOptimizerManualBounds = FALSE,
          responseOptimizerManualTarget = FALSE,
          responsesResponseOptimizer = list(optionKey = "variable", types = list(), value = list()),
          rsmPredefinedModel = FALSE,
          rsmPredefinedTerms = "fullQuadratic",
          squaredTerms = list(types = list(), value = list()),
          squaredTermsCoded = FALSE,
          stepwiseMethodFactorial = "enter",
          stepwiseMethodResponseSurface = "enter",
          sumOfSquaresType = "type3",
          surfacePlotHorizontalRotation = 330,
          surfacePlotVerticalRotation = 20,
          tableAlias = TRUE,
          tableEquation = TRUE) {

   defaultArgCalls <- formals(jaspQualityControl::doeAnalysis)
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

   optionsWithFormula <- c("blocksFactorial", "blocksResponseSurface", "codeFactorsManualTable", "continuousFactorsFactorial", "continuousFactorsResponseSurface", "contourSurfacePlotVariables", "covariates", "dependentFactorial", "dependentResponseSurface", "designType", "fixedFactorsFactorial", "fixedFactorsResponseSurface", "histogramBinWidthType", "modelTerms", "optimizationPlotCustomParameterValues", "optimizationPlotPredictionType", "responsesResponseOptimizer", "rsmPredefinedTerms", "squaredTerms", "stepwiseMethodFactorial", "stepwiseMethodResponseSurface", "sumOfSquaresType")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspQualityControl", "doeAnalysis", "doeAnalysis.qml", options, version, FALSE))
}