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
#' The purpose of analysing factorial and response surface designs is to identify influential factors and optimal response conditions. The analysis shows which factors significantly affect the response and how they interact. This allows to systematically optimize processes and improve product quality.
#'
#' @param blocksFactorial, The column corresponding to the blocks in the design. The difference between a block variable and a discrete factor is that the blocks will not be analysed for interaction effects and are excluded from effect plots.
#' @param blocksResponseSurface, The column corresponding to the blocks in the design. The difference between a block variable and a discrete factor is that the blocks will not be analysed for interaction effects and are excluded from effect plots.
#' @param codeFactors, Display the results in coded units. This means that the levels of continuous factors and of discrete factors with two levels are rescaled to lie between -1 and 1. Discrete factors with more than two levels are not rescaled: a factor with k levels enters the model with k-1 terms (sum-to-zero contrasts), as in uncoded units, and their low/high levels are not used. For factorial designs, the ANOVA table is calculated in coded units, so it does not change with this option, unless coding changes the model (an interaction or squared term without its main effects).
#'    Defaults to \code{TRUE}.
#' @param codeFactorsManualTable, Enter the low and high level of each factor. Blocks and discrete factors with more than two levels are not rescaled to -1/1, so they have no low and high level ("Not available").
#' @param codeFactorsMethod, For the ordering of contrasts and to identify the alpha points in a response surface design, the low and high levels of all factors need to be specified. There are two options to handle this.
#' \itemize{
#'   \item \code{"automatic"}: Attempts to automatically detect the low and high levels by taking the minimum and maximum for continuous factors, and by ordering discrete factors alphabetically. For response surface designs, this might recognize alpha values as low/high levels. In this case, manual specification is needed.
#'   \item \code{"manual"}: Shows all factors in the analysis to allow for manual specification of the low and high levels.
#' }
#' @param continuousFactorsFactorial, The columns corresponding to the continuous factors in the design.
#' @param continuousFactorsResponseSurface, The columns corresponding to the continuous factors in the design.
#' @param contourSurfacePlot, Show contour (2D) or surface (3D) plots of the predicted response.
#'    Defaults to \code{FALSE}.
#' @param contourSurfacePlotResponseDivision, Select the number of divisions of the response in the contour/surface plot.
#' @param contourSurfacePlotVariables, The continuous factors to plot. At least two are needed. A plot is shown for each pair of factors.
#' @param covariates, The columns corresponding to the covariates in the design. The difference between a covariate and a continuous factor is that the covariate will not be analysed for interaction effects and is excluded from effect plots.
#' @param dependentFactorial, The measured outcome variables of the process under investigation. Each response is analysed separately.
#' @param dependentResponseSurface, The measured outcome variables of the process under investigation. Each response is analysed separately.
#' @param designType, Select the type of experimental design you want to analyse.
#' - **Factorial design**: A factorial design is an experimental setup used to study the effects of multiple factors by varying them simultaneously. Each factor is tested at different levels and in different combinations with other factors. This design allows for the examination of both the main effects of each factor and the interactions between factors.
#' - **Response surface design**: A response surface design is an experimental framework used to optimize and understand the relationships between several explanatory variables and one or more response variables. Besides main effects and interactions, it allows to estimate squared terms, which describe the curvature of the response. This gives a detailed map of the response landscape, which helps to find optimal conditions.
#' @param fixedFactorsFactorial, The columns corresponding to the discrete factors in the design.
#' @param fixedFactorsResponseSurface, The columns corresponding to the discrete factors in the design.
#' @param fourInOneResidualPlot, Show all of the above plots in a 2x2 matrix.
#'    Defaults to \code{FALSE}.
#' @param highestOrder, Include all main effects and all interactions up to the specified order. Uncheck to define the model terms manually.
#'    Defaults to \code{TRUE}.
#' @param histogramBinWidthType, Select the method used to calculate the bin widths of the residual histogram, or specify a manual number of bins.
#' @param histogramManualNumberOfBins, Number of bins when the bin width type is set to manual.
#' @param modelTerms, Allows manually defining the main effects and interactions in the model.
#' @param normalEffectsPlot, Show a normal probability plot of the standardized effects.
#'    Defaults to \code{FALSE}.
#' @param optimizationIntervals, Predicted response at the optimal (or manually set) factor levels, with the confidence interval for the mean response and the prediction interval for a single new run. Blocks are averaged out; covariates are held at their mean.
#'    Defaults to \code{FALSE}.
#' @param optimizationIntervalsLevel, Confidence level of the intervals.
#' @param optimizationPlot, Show a matrix of plots with one column per factor. The top row shows the composite desirability, the other rows one per response. Each plot varies one factor while the other factors are held at the optimal (or manually set) levels, which are marked in red.
#'    Defaults to \code{TRUE}.
#' @param optimizationPlotCustomParameters, Draw the optimization plot at manually entered factor levels instead of the optimal solution. Enter a value for every factor; discrete factors need one of their observed levels.
#'    Defaults to \code{FALSE}.
#' @param optimizationPlotPredictionType, What the response rows of the optimization plot show: the predicted response or its individual desirability.
#' @param optimizationSolutionTable, Show the optimizer settings per response and the optimal solution: the factor levels with the highest composite desirability, that desirability, and the predicted (fitted) responses at these levels.
#'    Defaults to \code{TRUE}.
#' @param order, The highest order of the interactions to include. For example, 2 includes all two-way interactions.
#' @param plotFitted, Show a plot of the residuals per fitted value.
#'    Defaults to \code{FALSE}.
#' @param plotHist, Show a histogram of the residuals.
#'    Defaults to \code{FALSE}.
#' @param plotNorm, Show a normal probability plot of the residuals.
#'    Defaults to \code{FALSE}.
#' @param plotPareto, Show a Pareto plot of the standardized effects.
#'    Defaults to \code{FALSE}.
#' @param plotRunOrder, Show a plot of the residuals per run order value.
#'    Defaults to \code{FALSE}.
#' @param responseOptimizerManualBounds, Use the entered lower and upper bounds instead of the minimum and maximum of the observed responses.
#'    Defaults to \code{FALSE}.
#' @param responseOptimizerManualTarget, Use the entered targets for the goals maximize and minimize instead of the maximum or minimum of the observed responses.
#'    Defaults to \code{FALSE}.
#' @param responsesResponseOptimizer, The responses to optimize. For each response, the individual desirability (between 0 and 1) is set by the goal, bounds, target and weight. The composite desirability is the weighted geometric mean of the individual desirabilities, with the importances (normalized to sum to 1) as weights.
#' @param rsmPredefinedModel, Use one of the predefined models offered for analysing response surface designs. Uncheck to define the model terms manually.
#'    Defaults to \code{FALSE}.
#' @param rsmPredefinedTerms, The terms of the predefined model.
#' - **Linear**: Main effects only.
#' - **Linear and interaction terms**: Main effects and all two-way interactions.
#' - **Linear and squared terms**: Main effects and the squared terms of all continuous factors.
#' - **Full quadratic**: Main effects, all two-way interactions and the squared terms of all continuous factors.
#' @param squaredTerms, When analysing response surface designs, the continuous factors whose squared terms are included in the manually defined model.
#' @param squaredTermsCoded, Calculate the ANOVA table from the factors in coded units (-1 to 1), so that squared terms are calculated from coded values. Only has an effect when the results are not displayed in coded units. It can change the type II and III sums of squares of the linear terms.
#'    Defaults to \code{FALSE}.
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
#' @param sumOfSquaresType, How the sum of squares of each term in the ANOVA table is calculated. The types differ in which other terms a term is adjusted for. In balanced designs with orthogonal (coded) terms, such as complete two-level factorial designs, all types give the same result. In unbalanced designs, e.g. with missing runs, covariates or squared terms, they can differ. The Model, Error and Total rows are the same for all types. See the description of the ANOVA table below.
#' - **Type I**: Sequential sums of squares: each term is adjusted only for the terms entered before it, so the results depend on the order of the terms. The term rows add up to the Model row.
#' - **Type II**: Each term is adjusted for all other terms, except for higher-order terms that contain it. For example, A is adjusted for B, but not for A✻B.
#' - **Type III**: Each term is adjusted for all other terms in the model, i.e. it measures what the term adds when it is entered last. Results for lower-order terms depend on how the factors are coded. Not available when some coefficients are aliased.
#' @param surfacePlotHorizontalRotation, Rotation of the surface plot in the horizontal plane, in degrees.
#' @param surfacePlotVerticalRotation, Rotation of the surface plot in the vertical plane, in degrees.
#' @param tableAlias, Assign alias names to the factors. Useful when factors have long names.
#'    Defaults to \code{TRUE}.
#' @param tableCoefficientsCi, Add confidence intervals for the effects and the coefficients to the coefficients table, based on the t-distribution with the error degrees of freedom. The interval for an effect is twice the interval for its coefficient (in coded units).
#'    Defaults to \code{FALSE}.
#' @param tableCoefficientsCiLevel, Confidence level of the intervals.
#' @param tableEquation, Display the regression coefficients as an equation predicting the response.
#'    Defaults to \code{TRUE}.
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
          optimizationIntervals = FALSE,
          optimizationIntervalsLevel = 0.95,
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
          tableCoefficientsCi = FALSE,
          tableCoefficientsCiLevel = 0.95,
          tableEquation = TRUE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("blocksFactorial", "blocksResponseSurface", "codeFactorsManualTable", "continuousFactorsFactorial", "continuousFactorsResponseSurface", "contourSurfacePlotVariables", "covariates", "dependentFactorial", "dependentResponseSurface", "designType", "fixedFactorsFactorial", "fixedFactorsResponseSurface", "histogramBinWidthType", "modelTerms", "optimizationPlotCustomParameterValues", "optimizationPlotPredictionType", "responsesResponseOptimizer", "rsmPredefinedTerms", "squaredTerms", "stepwiseMethodFactorial", "stepwiseMethodResponseSurface", "sumOfSquaresType")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspQualityControl", "doeAnalysis", "doeAnalysis.qml", options, version, FALSE))
}