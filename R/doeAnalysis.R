#
# Copyright (C) 2013-2021 University of Amsterdam
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

# Enforce marginality on a fitted model: a squared term I(x^2) requires its
# main effect x. step() treats I(x^2) as unrelated to x (unlike interactions),
# so stepwise selection may retain a squared term without its linear term.
# Re-add any missing main effects, refit, and report which were forced in.
.enforceSquaredTermMarginality <- function(model,
                                           data = eval(stats::getCall(model)[["data"]], environment(stats::formula(model)))) {
  modelTerms  <- attr(stats::terms(model), "term.labels")
  squaredVars <- gsub("^I\\((.+)\\^2\\)$", "\\1", grep("^I\\(.+\\^2\\)$", modelTerms, value = TRUE))
  forcedMainEffects <- setdiff(squaredVars, modelTerms)
  if (length(forcedMainEffects) > 0) {
    # update the formula (pure manipulation) and refit with explicit data, so the
    # refit does not depend on the data object being in scope by name (as update() would).
    newFormula <- stats::update(stats::formula(model), paste(". ~ . +", paste(forcedMainEffects, collapse = " + ")))
    model      <- stats::lm(newFormula, data = data)
  }
  return(list(model = model, forcedMainEffects = forcedMainEffects))
}

.formatCoefEquation <- function(x, digits = .numDecimals) {
  vapply(x, function(val) {
    if (val == 0) return("0")
    absVal <- abs(val)
    if (absVal < 1e4) {
      # avoid e-notation for small-to-moderate numbers
      mag <- floor(log10(absVal))
      sigDigits <- max(digits, mag + 1 + digits)
      formatted <- trimws(formatC(val, digits = sigDigits, format = "g", drop0trailing = TRUE))
    } else {
      formatted <- trimws(formatC(val, digits = digits, format = "g", drop0trailing = TRUE))
    }
    sub("^-", "\u2013", formatted)
  }, character(1))
}

.enforceSquaredTermMarginality <- function(model, data = stats::model.frame(model)) {
  modelTerms         <- attr(stats::terms(model), "term.labels")
  squaredVars        <- gsub("^I\\((.+)\\^2\\)$", "\\1", grep("^I\\(.+\\^2\\)$", modelTerms, value = TRUE))
  forcedMainEffects  <- setdiff(squaredVars, modelTerms)

  if (length(forcedMainEffects) > 0) {
    keep       <- model$keep
    newFormula <- stats::update(stats::formula(model), paste(". ~ . +", paste(forcedMainEffects, collapse = " + ")))
    model      <- stats::lm(newFormula, data = data)

    if (!is.null(keep))
      model$keep <- keep
  }

  list(model = model, forcedMainEffects = forcedMainEffects)
}

#' @export
doeAnalysisInternal <- function(jaspResults, dataset, options, ...) {

  if (options[["designType"]] == "factorialDesign") {
    ready <- sum(length(options[["fixedFactorsFactorial"]]), length(options[["continuousFactorsFactorial"]])) >= 1 &&
      length(options[["dependentFactorial"]]) >= 1 &&
      !is.null(unlist(options[["modelTerms"]]))
    discretePredictors <- options[["fixedFactorsFactorial"]]
    continuousPredictors <- options[["continuousFactorsFactorial"]]
    covariates <- options[["covariates"]]
    blocks <- options[["blocksFactorial"]]
    dependent <- options[["dependentFactorial"]]
    stepwiseMethod <- options[["stepwiseMethodFactorial"]]
  } else if (options[["designType"]] == "responseSurfaceDesign") {
    ready <- length(options[["continuousFactorsResponseSurface"]]) >= 1 && length(options[["dependentResponseSurface"]]) >= 1
    discretePredictors <- options[["fixedFactorsResponseSurface"]]
    continuousPredictors <- options[["continuousFactorsResponseSurface"]]
    covariates <- NULL
    blocks <- options[["blocksResponseSurface"]]
    dependent <- options[["dependentResponseSurface"]]
    stepwiseMethod <- options[["stepwiseMethodResponseSurface"]]
  }

  if (!ready) {
    jaspResults[["defaultPlot"]] <- createJaspPlot(title = gettext("Design of Experiments - Analysis"))
    jaspResults[["defaultPlot"]]$dependOn(.doeAnalysisBaseDependencies())
    return()
  }

  dataset <- .doeAnalysisReadData(dataset, options, continuousPredictors, discretePredictors, blocks, covariates, dependent)


  if (length(blocks) > 0 && !identical(blocks, "")) # data reading function renames the block variable to "block"
    blocks <- "Block"

  .doeAnalysisCheckErrors(dataset, options, continuousPredictors, discretePredictors, blocks, covariates, dependent, ready)

  # Create containers for response variable(s)
  for (dep in dependent) {
    jaspResults[[dep]] <- createJaspContainer(title = dep)
  }

  p <- try(.doeAnalysisMakeState(jaspResults, dataset, options, continuousPredictors, discretePredictors, blocks, covariates, dependent, stepwiseMethod, ready))

  if (isTryError(p)) {
    jaspResults[["errorPlot"]] <- createJaspPlot(title = gettext("Error"))
    stepwisePrefix <- if (stepwiseMethod != "enter") gettext("Stepwise method failed. Please consider a different method or starting model.") else NULL
    jaspResults[["errorPlot"]]$setError(paste(stepwisePrefix, .extractErrorMessage(p)))
    jaspResults[["errorPlot"]]$dependOn(.doeAnalysisBaseDependencies())
    return()
  }

  coded <- options[["codeFactors"]]

  .doeAnalysisSummaryTable(jaspResults, dependent, options, ready, coded)
  .doeAnalysisAnovaTable(jaspResults, dependent, options, ready, coded)
  .doeAnalysisCoefficientsTable(jaspResults, dependent, options, ready, coded)
  .doeAnalysisEquationTable(jaspResults, dependent, options, ready, coded)
  .doeAnalysisStepwiseTable(jaspResults, dependent, options, ready, coded, stepwiseMethod)
  .doeAnalysisPlotPareto(jaspResults, dependent, options, blocks, covariates, ready)
  .doeAnalysisPlotEffectNormalDistribution(jaspResults, dependent, options, blocks, covariates, ready)
  .doeAnalysisPlotQQResiduals(jaspResults, dependent, options, ready)
  .doeAnalysisPlotHistResiduals(jaspResults, dependent, options, ready)
  .doeAnalysisPlotFittedVsResiduals(jaspResults, dependent, options, ready)
  .doeAnalysisPlotResidualsVsOrder(jaspResults, dependent, dataset, options, ready)
  .doeAnalysisPlotMatrixResidualPlot(jaspResults, dependent, dataset, options, ready)
  .doeAnalysisPlotContourSurface(jaspResults, dataset, options, dependent, ready)
  .doeResponseOptimizerTables(jaspResults, options, dataset, continuousPredictors, discretePredictors, blocks, covariates, ready)
  .doeResponseOptimizerPredictionTable(jaspResults, options, dataset, continuousPredictors, discretePredictors, blocks, covariates, ready)
  .doeResponseOptimizerPlots(jaspResults, options, dataset, continuousPredictors, discretePredictors, blocks, covariates, ready)
  }

.doeAnalysisReadData <- function(dataset, options, continuousPredictors, discretePredictors, blocks, covariates, dependent) {
  if (!is.null(dataset)) {
    return(dataset)
  }
  factorVars <- NULL
  numericVars <- NULL
  if (!identical(dependent, "")) {
    numericVars <- c(numericVars, dependent)
  }
  if (length(continuousPredictors) > 0 && !identical(continuousPredictors, "")) {
    numericVars <- c(numericVars, unlist(continuousPredictors))
  }
  if (length(continuousPredictors) > 0 && !identical(continuousPredictors, "")) {
    numericVars <- c(numericVars, unlist(continuousPredictors))
  }
  if (length(covariates) > 0 && !identical(covariates, "")) {
    numericVars <- c(numericVars, unlist(covariates))
  }
  if (length(discretePredictors) > 0 && !identical(discretePredictors, "")) {
    factorVars <- c(factorVars, unlist(discretePredictors))
  }
  if (length(blocks) > 0 && !identical(blocks, "")) {
    factorVars <- c(factorVars, blocks)
  }
  dataset <- .readDataSetToEnd(columns.as.numeric = numericVars, columns.as.factor = factorVars)
  # Number of levels of each factor including levels without runs, i.e., the levels the form counts when it hides the
  # low/high fields of discrete factors with more than two levels (see .doeMultiLevelDiscretePredictors)
  allLevels <- lapply(dataset[intersect(factorVars, names(dataset))], levels)
  # droplevels: levels without runs (filtered or missing) would make lm() drop the sum-to-zero contrasts
  dataset <- droplevels(na.omit(dataset))
  attr(dataset, "allLevelCounts") <- lengths(allLevels)
  levelsWithoutRuns <- Map(function(var, varLevels) setdiff(varLevels, levels(dataset[[var]])), names(allLevels), allLevels)
  attr(dataset, "levelsWithoutRuns") <- Filter(length, levelsWithoutRuns) # reported below the coefficients table

  if (length(blocks) > 0 && !identical(blocks, "")) # name of variable should always be "Block"
    names(dataset)[names(dataset) == blocks] <- "Block"
  return(dataset)
}

.doeAnalysisBaseDependencies <- function() {
  deps <- c(
    "dependentResponseSurface", "fixedFactorsResponseSurface", "blocksResponseSurface", "runOrder",
    "highestOrder", "order", "continuousFactorsFactorial", "modelTerms", "blocksFactorial",
    "designType", "continuousFactorsResponseSurface", "codeFactors", "rsmPredefinedModel", "fixedFactorsFactorial",
    "rsmPredefinedTerms", "dependentFactorial", "covariates", "codeFactorsMethod", "codeFactorsManualTable",
    "squaredTerms", "sumOfSquaresType", "squaredTermsCoded", "stepwiseMethodResponseSurface", "stepwiseMethodFactorial")
  return(deps)
}

.scaleDOEvariable <- function(x){2*(x-min(x))/(max(x)-min(x))-1}

# Discrete factors with more than 2 levels, counting levels without runs (e.g., all runs missing) as the form does: the form
# shows no low/high fields for them, so they must not be coded with the (hidden) manual low/high levels. A factor with
# only 2 levels with runs then enters as a factor with sum-to-zero contrasts, which is the same as -1/1 coding.
.doeMultiLevelDiscretePredictors <- function(dataset, discretePredictors) {
  discreteVars <- unlist(discretePredictors)
  discreteVars <- discreteVars[discreteVars != ""]
  discreteVars[attr(dataset, "allLevelCounts")[discreteVars] > 2]
}

.doeAnalysisMakeState <- function(jaspResults, dataset, options, continuousPredictors, discretePredictors, blocks, covariates, dependent, stepwiseMethod, ready) {
  if (!ready || jaspResults$getError()) {
    return()
  }

  for (dep in dependent) {
    currentDependent <- dep

    result <- list()
    result[["regression"]] <- list()
    result[["anova"]] <- list()
    resultCoded <- list()
    resultCoded[["regression"]] <- list()
    resultCoded[["anova"]] <- list()
    result[["regression"]][["levelsWithoutRuns"]] <- resultCoded[["regression"]][["levelsWithoutRuns"]] <- attr(dataset, "levelsWithoutRuns")

    # set the contrasts for all categorical variables, add option to choose later
    for (fac in unlist(discretePredictors)) {
      contrasts(dataset[[fac]]) <- "contr.sum"
    }
    if (length(blocks) > 0 && !identical(blocks, ""))
      contrasts(dataset[[blocks]]) <- "contr.sum"

    # Transform to coded, -1 to 1 coding.
    allVars <- c(unlist(continuousPredictors), unlist(discretePredictors))
    allVars <- allVars[allVars != ""]
    # Discrete factors with more than 2 levels cannot be put on a -1 to 1 scale without forcing a linear trend over
    # their levels (and dropping df), so they are left as factors (sum-to-zero contrasts), as in the uncoded model.
    multiLevelVars <- .doeMultiLevelDiscretePredictors(dataset, discretePredictors)
    datasetCoded <- dataset
    if (options[["codeFactorsMethod"]] == "manual")
      manualCodingTable <- do.call(rbind.data.frame, options[["codeFactorsManualTable"]])
    for (i in seq_along(allVars)) {
      var <- allVars[i]
      if (var %in% multiLevelVars)
        next
      varData <- datasetCoded[[var]]
      levels <- sort(unique(varData)) # get levels before transforming to char to preserve possible order
      varData <- as.character(varData) # transform to char, otherwise you cannot add coded values to this variable as "factor level does not exist"
      if (options[["codeFactorsMethod"]] == "automatic") {
        if (var %in% unlist(discretePredictors)) {
          nLevels <- length(levels)
          steps <- 2/(nLevels - 1) # divide space between -1 and 1 into equal spaces
          codes <- seq(-1, 1, steps)
        } else if (var %in% unlist(continuousPredictors)) {
          codes <- .scaleDOEvariable(levels)
        }
      } else if (options[["codeFactorsMethod"]] == "manual") {
        indexCurrentVar <- which(manualCodingTable[["predictors"]] == var)
        lowLevel <- manualCodingTable[indexCurrentVar,][["lowValue"]]
        highLevel <- manualCodingTable[indexCurrentVar,][["highValue"]]
        if (lowLevel == highLevel) {
          stop(gettextf("The specified low/high levels for %1$s are not distinct.", var), call. = FALSE)
        }
        if (!lowLevel %in% levels || !highLevel %in% levels) {
          invalidLevels <- c(lowLevel, highLevel)[!c(lowLevel, highLevel) %in% levels]
          stop(gettextf("The specified low/high level(s) %1$s for %2$s do not match the levels in the dataset.",
                        paste(invalidLevels, collapse = ", "), var), call. = FALSE)
        }
        lowPos <- which(levels == lowLevel)
        highPos <- which(levels == highLevel)
        if (var %in% unlist(discretePredictors)) {
          levels <- c(lowLevel, levels[-c(lowPos, highPos)], highLevel)
          nLevels <- length(levels)
          steps <- 2/(nLevels - 1) # divide space between -1 and 1 into equal spaces
          codes <- seq(-1, 1, steps)
        } else if (var %in% unlist(continuousPredictors)) {
          codes <- .scaleDOEvariable(levels[lowPos:highPos])
          lowLevel <- as.numeric(lowLevel)
          highLevel <- as.numeric(highLevel)
          outerCodes <- 2 * (levels[-c(lowPos:highPos)]-lowLevel)/(highLevel-lowLevel)-1 # if any values are above the specified high value or below the specified low value
          codes <- sort(c(codes, outerCodes))
        }
      }
      for (j in seq_along(varData)) {
        codeIndex <- which(varData[j] == levels)
        varData[j] <- codes[codeIndex]
      }
      datasetCoded[[var]] <- as.numeric(varData)
    }

    if ((options[["designType"]] == "factorialDesign" && !options[["highestOrder"]]) ||
        (options[["designType"]] == "factorialDesign" && options[["highestOrder"]] && options[["order"]] == 1) ||
        (options[["designType"]] == "responseSurfaceDesign" && !options[["rsmPredefinedModel"]])) {
      reorderModelTerms <- .reorderModelTerms(options)
      modelTerms <- reorderModelTerms$modelTerms
      modelDef <- .modelFormula(modelTerms, options, currentDependent)
      formulaString <- modelDef$model.def
      if (options[["designType"]] == "responseSurfaceDesign" && length(unlist(options[["squaredTerms"]])) > 0) {
        squaredTerms <- options[["squaredTerms"]]
        squaredTermsString <- paste0(" + I(", squaredTerms, "^2)", collapse = "")
        formulaString <- paste0(formulaString, squaredTermsString)
      }
    } else if (options[["highestOrder"]] && options[["designType"]] == "factorialDesign") {
      independentVariables <- c(unlist(continuousPredictors), unlist(discretePredictors))
      independentVariables <- independentVariables[independentVariables != ""]
      formulaString <- .createHighestOrderInteractionFormula(currentDependent, independentVariables, interactionOrder = options[["order"]])
    } else if (options[["rsmPredefinedModel"]] && options[["designType"]] == "responseSurfaceDesign") {
      modelTerms <- options[["rsmPredefinedTerms"]]
      if (length(continuousPredictors) == 1 && modelTerms == "linearAndInteractions") {
        modelTerms <- "linear"
      } else if (length(continuousPredictors) == 1 && modelTerms == "fullQuadratic") {
        modelTerms <- "linearAndSquared"
      }
      numPred <- unlist(continuousPredictors)
      numPredStringMainEffects <- paste0(numPred, collapse = " + ")
      numPredStringSquaredEffects <- paste0(" + I(", numPred, "^2)", collapse = "")
      catPred <- unlist(discretePredictors)
      catPred <- catPred[catPred != ""]
      if (!is.null(catPred) && length(catPred) > 0) {
        catPredString <- paste0(" + ", catPred, collapse = "")
      } else {
        catPredString <- ""
      }
      if (!identical(catPredString, "")) {
        secondOrderInteractionEffects <- paste0("(", numPredStringMainEffects, " + ", catPredString, ")^2")
      } else {
        secondOrderInteractionEffects <- paste0("(", numPredStringMainEffects, ")^2")
      }
      formulaString <- switch(modelTerms,
                              "linear" = paste0(currentDependent, " ~ ", numPredStringMainEffects, catPredString),
                              "linearAndInteractions" = paste0(currentDependent, " ~ ", secondOrderInteractionEffects, catPredString),
                              "linearAndSquared" = paste0(currentDependent, " ~ ", numPredStringMainEffects, numPredStringSquaredEffects, catPredString),
                              "fullQuadratic" = paste0(currentDependent, " ~ ", secondOrderInteractionEffects, numPredStringSquaredEffects, catPredString)
      )
    }
    if (length(blocks) > 0 && !identical(blocks, ""))
      formulaString <- paste0(formulaString, " + ", blocks)
    if (length(covariates) > 0 && !identical(covariates, "")) {
      covariateString <- paste0(" + ", unlist(covariates), collapse = "")
      formulaString <- paste0(formulaString, covariateString)
    }

    formula <- as.formula(formulaString)

    if (stepwiseMethod != "enter") {

      stepwiseData <- if (options[["codeFactors"]]) datasetCoded else dataset
      fullModel <- lm(formula, data = stepwiseData)

      if (stepwiseMethod != "backward") {
        initialFormula <- as.formula(paste(dep, "~ 1"))
      } else {
        initialFormula <- formula
      }

      stepwiseModel <- step(lm(initialFormula, data = stepwiseData),
                            direction = stepwiseMethod,
                            trace = 0,
                            scope = formula,
                            keep = function(model, aic)
                              list(model = model,
                                   aic = aic,
                                   coefficients = coef(model)))

      # Enforce marginality before deriving the final model: step() may retain a
      # squared term I(x^2) without its main effect x (see .enforceSquaredTermMarginality).
      marginality       <- .enforceSquaredTermMarginality(stepwiseModel, stepwiseData)
      stepwiseModel     <- marginality[["model"]]
      forcedMainEffects <- marginality[["forcedMainEffects"]]

      excludedVars <- setdiff(names(fullModel$coefficients), names(stepwiseModel$coefficients))
      formula <- as.formula(stepwiseModel)

      # Make data frame from stepwise results, with step number, AIC of that model, and formula
      stepwiseResult <- data.frame(
        stepNr = 1:ncol(stepwiseModel$keep),
        AIC = unlist(stepwiseModel$keep[2, ]),
        formula = sapply(stepwiseModel$keep[3, ], function(x)
          # apply some gsub magic to turn I(A^2) into A^2
          paste(gsub("I\\((.*)\\^2\\)", "\\1^2", names(x)[-1]), collapse  = " + "))
      )

      result[["stepwiseResult"]] <- list(excludedVars = gsub("I\\((.*)\\^2\\)", "\\1^2", excludedVars),
                                         stepwiseResult = stepwiseResult,
                                         forcedMainEffects = forcedMainEffects)
    }

    regressionFit <- lm(formula, data = dataset)

    # A term is removed only when all its columns are aliased. A term with only some aliased columns (e.g., a discrete
    # factor with a level that only occurs in the runs of a level of another factor) keeps its estimable columns:
    # lm() gives the aliased columns NA coefficients and fits the model without them.
    removedTerms <- .doeAliasedTerms(regressionFit)[["full"]]
    if (length(removedTerms) > 0) {
      formula       <- as.formula(paste(paste(deparse(formula), collapse = ""), paste(removedTerms, collapse = " - "), sep = " - "))
      regressionFit <- lm(formula, data = dataset)
    }

    regressionFitCoded     <- stats::lm(formula, data = datasetCoded)
    regressionSummary      <- summary(regressionFit)
    regressionSummaryCoded <- summary(regressionFitCoded)
    regressionFit[["nullSpace"]]      <- list(value = .doeNullSpace(regressionFit)) # see .doeFitNullSpace
    regressionFitCoded[["nullSpace"]] <- list(value = .doeNullSpace(regressionFitCoded))

    # coding can alias other columns than in uncoded units, so each fit gets its own notes
    result[["regression"]][["aliasNotes"]]      <- .doeAliasNotes(regressionFit, removedTerms, dataset, multiLevelVars)
    resultCoded[["regression"]][["aliasNotes"]] <- .doeAliasNotes(regressionFitCoded, removedTerms, dataset, multiLevelVars)

    names(regressionFit$coefficients) <-  unname(sapply(c(names(regressionFit$coefficients)), .gsubIdentityFunction)) # remove potential identity function around squared terms
    rownames(regressionSummary$coefficients) <- unname(sapply(c(rownames(regressionSummary$coefficients)), .gsubIdentityFunction))
    names(regressionFitCoded$coefficients) <-  unname(sapply(c(names(regressionFitCoded$coefficients)), .gsubIdentityFunction))
    rownames(regressionSummaryCoded$coefficients) <- unname(sapply(c(rownames(regressionSummaryCoded$coefficients)), .gsubIdentityFunction))

    result[["regression"]][["formula"]] <- formula
    result[["regression"]][["object"]] <- regressionFit
    result[["regression"]][["objectSummary"]] <- regressionSummary
    result[["regression"]][["saturated"]] <- regressionSummary$df[2] == 0

    resultCoded[["regression"]][["formula"]] <- formula
    resultCoded[["regression"]][["object"]] <- regressionFitCoded
    resultCoded[["regression"]][["objectSummary"]] <- regressionSummaryCoded
    resultCoded[["regression"]][["saturated"]] <- regressionSummaryCoded$df[2] == 0

    result[["regression"]][c("s", "rsq", "adjrsq", "predrsq")]      <- .doeModelSummaryValues(regressionFit, regressionSummary)
    resultCoded[["regression"]][c("s", "rsq", "adjrsq", "predrsq")] <- .doeModelSummaryValues(regressionFitCoded, regressionSummaryCoded)

    # ANOVA fit and the data it was fitted on (the lack of fit test groups the runs on these variables). For factorial
    # designs, the ANOVA in uncoded units uses the predictors in coded units, so that it does not depend on the display
    # units, whenever both fits are the same model. Coding changes the model when an interaction or squared term
    # enters without its main effects; the ANOVA then follows the fit of the coefficients shown.
    sameModelCoded  <- .doeSameColumnSpace(stats::model.matrix(regressionFit), stats::model.matrix(regressionFitCoded))
    anovaFitCoded   <- options[["codeFactors"]] || (options[["designType"]] == "factorialDesign" && sameModelCoded) ||
      (options[["squaredTermsCoded"]] && options[["designType"]] == "responseSurfaceDesign")
    # say which units the ANOVA of an uncoded factorial analysis with continuous predictors uses
    if (options[["designType"]] == "factorialDesign" && !options[["codeFactors"]] && length(setdiff(unlist(continuousPredictors), "")) > 0) {
      result[["anova"]][["codingNote"]] <- if (sameModelCoded) {
        gettext("Calculated with the continuous predictors in coded (\u22121/+1) units, so the table is the same in coded and uncoded units.")
      } else {
        gettext("Calculated in uncoded units: coding the predictors changes this model (e.g., an interaction or squared term without its main effects).")
      }
    }
    anovaFitData    <- if (anovaFitCoded) regressionFitCoded else regressionFit
    anovaFitDataset <- if (anovaFitCoded) datasetCoded else dataset
    anovaSaturated  <- anovaFitData[["df.residual"]] == 0
    ssType          <- if (anovaSaturated) "type1" else options[["sumOfSquaresType"]] # aov gives sequential SS
    anovaFit        <- NULL

    # no type III SS with aliased columns: the test of a term then depends on how its remaining columns are parameterized
    # (ssType, not the option: a saturated model shows sequential SS whatever type is selected)
    if (ssType == "type3" && anyNA(stats::coef(anovaFitData))) {
      result[["anova"]][["error"]] <- resultCoded[["anova"]][["error"]] <-
        gettext("Type III sums of squares are unavailable for this model, because some of its coefficients are aliased. Use Type I or Type II sums of squares.")
    } else if (!anovaSaturated) {
      if (ssType == "type1") {
        anovaFit <- anova(anovaFitData)
      } else if (ssType == "type2") {
        anovaFit <- car::Anova(anovaFitData, type = 2)
      } else if (ssType == "type3") {
        anovaFit <- car::Anova(anovaFitData, type = 3)
        anovaFit <- anovaFit[-1,] # remove the intercept that is added when using type 3 SS
      }
      anovaFit[["Mean Sq"]] <- anovaFit[["Sum Sq"]] / anovaFit[["Df"]]
      anovaFit <- anovaFit[c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")] # rearrange, so it has the same order as the aov function
      anovaFit <- .addModelHeaderTerms(anovaFit, anovaFitData, ssType, unlist(covariates), anovaFitDataset)
    } else {
      anovaFit <- summary(aov(anovaFitData))[[1]]
      errorRow <- data.frame(Df = 0, SS = 0, MS = 0) # add an error row to keep the format consistent
      colnames(errorRow) <- colnames(anovaFit)
      rownames(errorRow) <- "Error"
      anovaFit <- rbind(anovaFit, errorRow)
      anovaFit$`F value` <- NA # add these empty columns to the saturated design so the anova fit object always has the same format
      anovaFit$`Pr(>F)` <- NA
      anovaFit <- .addModelHeaderTerms(anovaFit, anovaFitData, ssType, unlist(covariates), anovaFitDataset)
    }

    if (!is.null(anovaFit)) {
      result[["anova"]][["ssType"]]    <- resultCoded[["anova"]][["ssType"]]    <- ssType
      result[["anova"]][["saturated"]] <- resultCoded[["anova"]][["saturated"]] <- anovaSaturated
      result[["anova"]][["object"]] <- anovaFit
      result[["anova"]][["terms"]] <- gsubInteractionSymbol(rownames(anovaFit))
      result[["anova"]][["df"]] <- anovaFit$Df
      result[["anova"]][["adjss"]] <- anovaFit$`Sum Sq`
      result[["anova"]][["adjms"]] <- anovaFit$`Mean Sq`
      result[["anova"]][["F"]] <- anovaFit$`F value`
      result[["anova"]][["p"]] <- anovaFit$`Pr(>F)`

      resultCoded[["anova"]][["object"]] <- anovaFit
      resultCoded[["anova"]][["terms"]] <- gsubInteractionSymbol(rownames(anovaFit))
      resultCoded[["anova"]][["df"]] <- anovaFit$Df
      resultCoded[["anova"]][["adjss"]] <- anovaFit$`Sum Sq`
      resultCoded[["anova"]][["adjms"]] <-  anovaFit$`Mean Sq`
      resultCoded[["anova"]][["F"]] <- anovaFit$`F value`
      resultCoded[["anova"]][["p"]] <- anovaFit$`Pr(>F)`
    }

    ###############################
    ### Regression coefficients ###
    ###############################

    result[["regression"]][["coefficients"]] <- list()
    resultCoded[["regression"]][["coefficients"]] <- list()
    coefs <- as.data.frame(regressionSummary[["coefficients"]])
    coefsCoded <- as.data.frame(regressionSummaryCoded[["coefficients"]])
    valid_coefs <- which(!is.na(coefs[["Estimate"]]))
    valid_coefsCoded <- which(!is.na(coefsCoded[["Estimate"]]))
    termNames <- gsubInteractionSymbol(rownames(coefs)[valid_coefs])
    termNamesCoded <- gsubInteractionSymbol(rownames(coefsCoded)[valid_coefsCoded])

    #remove possible appended factor levels
    if ((options[["designType"]] == "responseSurfaceDesign" && options[["rsmPredefinedModel"]]) ||
        (options[["highestOrder"]] && options[["designType"]] == "factorialDesign")) {
      allPredictors <- c(unlist(continuousPredictors), unlist(discretePredictors))
    } else {
      allPredictors <- unique(unlist(options[["modelTerms"]]))
    }
    predictorsForLevelRemoval <- allPredictors
    if (length(blocks) > 0 && !identical(blocks, ""))
      predictorsForLevelRemoval <- c(predictorsForLevelRemoval, blocks)

    # Sort predictors by length in descending order to prevent matching of substrings
    predictorsForLevelRemoval <- predictorsForLevelRemoval[order(nchar(predictorsForLevelRemoval), decreasing = TRUE)]

    termNamesRemoved <- termNames
    # this regex removes the appended factor levels
    regexExpression <- paste0("(", paste(predictorsForLevelRemoval, collapse = "|"), ")((\\^2)?)([^✻]+)(✻?)")
    for (term_i in seq_along(termNamesRemoved)) {
      replacements <- if (grepl("^2", termNamesRemoved[term_i], fixed = TRUE)) "\\1\\4" else "\\1\\5"
      termNamesRemoved[term_i] <- gsub(regexExpression, replacements, termNamesRemoved[term_i], perl=TRUE)
      termNamesRemoved[term_i] <- gsub("\\s", "", termNamesRemoved[term_i])
    }

    termNamesRemovedCoded <- termNamesCoded
    # same regex as above, removes the appended factor levels
    for (term_i in seq_along(termNamesRemovedCoded)) {
      replacements <- if (grepl("^2", termNamesRemovedCoded[term_i], fixed = TRUE)) "\\1\\4" else "\\1\\5"
      termNamesRemovedCoded[term_i] <- gsub(regexExpression, replacements, termNamesRemovedCoded[term_i], perl=TRUE)
      termNamesRemovedCoded[term_i] <- gsub("\\s", "", termNamesRemovedCoded[term_i])
    }

    discretePredictorsIndices <- which(termNamesRemoved %in% discretePredictors)

    # Only terms of discrete factors with more than 2 levels have appended factor levels in the coded model, so just remove whitespace
    termNames <- gsub("\\s", "", termNames)
    termNamesCoded <- gsub("\\s", "", termNamesCoded)

    result[["regression"]][["coefficients"]][["terms"]] <- termNames
    resultCoded[["regression"]][["coefficients"]][["terms"]] <- termNamesCoded

    # calculate effects, but not for blocks, covariates or intercept
    # Effects do not depend on the coding, so for the uncoded table they are taken from a fit with the continuous predictors
    # on the -1/+1 scale. Discrete predictors are left as they are, so terms and signs match the uncoded coefficients.
    continuousVars <- unlist(continuousPredictors)
    continuousVars <- continuousVars[continuousVars != ""]
    datasetEffects <- dataset
    datasetEffects[continuousVars] <- datasetCoded[continuousVars]
    effectsFit <- lm(formula, data = datasetEffects)

    # Discrete factors with more than 2 levels have no effect, also when only 2 of their levels have runs: their sum-to-zero
    # contrast does not follow the low/high order, so a 2 * coefficient "effect" would have an arbitrary sign
    excludeFromEffects <- unlist(c(blocks, covariates, multiLevelVars))
    excludeFromEffects <- excludeFromEffects[excludeFromEffects != ""]
    coefEffects      <- .doeCoefficientEffects(effectsFit, excludeFromEffects)
    coefEffectsCoded <- .doeCoefficientEffects(regressionFitCoded, excludeFromEffects)
    if (length(coefEffects) != length(regressionFit$coefficients)) # should not happen, but never show effects of mismatched terms
      coefEffects <- rep(NA_real_, length(regressionFit$coefficients))
    # The uncoded effects come from the fit with coded continuous predictors, so they are only shown when that fit is
    # the same model as the uncoded coefficients (coding changes the model when, e.g., an interaction enters without
    # its main effects), and only where they are unique (aliased contrasts)
    effectsSameModel <- .doeSameColumnSpace(stats::model.matrix(regressionFit), stats::model.matrix(effectsFit))
    coefEffects[!effectsSameModel | !.doeEstimableCoefficients(effectsFit)] <- NA
    result[["regression"]][["coefficients"]][["effectsOtherModel"]] <- !effectsSameModel

    # The tables have one row per coefficient that lm() estimated (aliased columns have NA coefficients). With aliased
    # columns, some of these coefficients still depend on which columns lm() left out (e.g., on the order of the factor
    # levels): their values are not unique and are not shown.
    isFitted         <- !is.na(coef(regressionFit))
    isFittedCoded    <- !is.na(coef(regressionFitCoded))
    uniqueCoefs      <- .doeEstimableCoefficients(regressionFit)[isFitted]
    uniqueCoefsCoded <- .doeEstimableCoefficients(regressionFitCoded)[isFittedCoded]
    blankNonUnique   <- function(x, keep) replace(x, !keep, NA)

    result[["regression"]][["coefficients"]][["unique"]] <- uniqueCoefs
    result[["regression"]][["coefficients"]][["effects"]] <- blankNonUnique(coefEffects[isFitted], uniqueCoefs)
    result[["regression"]][["coefficients"]][["effectsSe"]] <- blankNonUnique(.doeEffectStandardErrors(effectsFit, coefEffects)[isFitted], uniqueCoefs)
    result[["regression"]][["coefficients"]][["effectsDf"]] <- effectsFit[["df.residual"]] # differs from regressionFit when coding changes the rank
    result[["regression"]][["coefficients"]][["est"]] <- blankNonUnique(coef(regressionFit)[isFitted], uniqueCoefs)
    result[["regression"]][["coefficients"]][["effects"]][1] <- NA
    result[["regression"]][["coefficients"]][["vif"]] <- .getVIF(regressionFit)[isFitted]

    resultCoded[["regression"]][["coefficients"]][["unique"]] <- uniqueCoefsCoded
    resultCoded[["regression"]][["coefficients"]][["effects"]] <- blankNonUnique(coefEffectsCoded[isFittedCoded], uniqueCoefsCoded)
    resultCoded[["regression"]][["coefficients"]][["effectsSe"]] <- blankNonUnique(.doeEffectStandardErrors(regressionFitCoded, coefEffectsCoded)[isFittedCoded], uniqueCoefsCoded)
    resultCoded[["regression"]][["coefficients"]][["effectsDf"]] <- regressionFitCoded[["df.residual"]]
    resultCoded[["regression"]][["coefficients"]][["est"]] <- blankNonUnique(coef(regressionFitCoded)[isFittedCoded], uniqueCoefsCoded)
    resultCoded[["regression"]][["coefficients"]][["effects"]][1] <- NA
    resultCoded[["regression"]][["coefficients"]][["vif"]] <- .getVIF(regressionFitCoded)[isFittedCoded]

    termNamesAliased <- termNames
    termNamesAliasedCoded <- termNamesCoded
    allPredictorsAliases <- LETTERS[seq_along(allPredictors)]
    for (pred_i in seq_along(allPredictors)) {
      termNamesAliased <- gsub(allPredictors[pred_i], allPredictorsAliases[pred_i], termNamesAliased)
      termNamesAliasedCoded <- gsub(allPredictors[pred_i], allPredictorsAliases[pred_i], termNamesAliasedCoded)
    }
    termNamesAliased <- gsub("✻", "", termNamesAliased)
    termNamesAliasedCoded <- gsub("✻", "", termNamesAliasedCoded)
    if (length(blocks) > 0 && !identical(blocks, "")) {
      blockNameIndices <- which(termNamesRemoved == blocks) # get the indices of the block variables
      blockNameIndicesCoded <- which(termNamesRemovedCoded == blocks) # get the indices of the block variables in the coded terms
      blockNamesAliased <- paste0("BLK", 1:length(blockNameIndices))
      termNamesAliased[blockNameIndices] <- blockNamesAliased
      termNamesAliasedCoded[blockNameIndicesCoded] <- blockNamesAliased
    }

    if (length(covariates) > 0 && !identical(covariates, "")) {
      covariateAliases <- paste0("COV", seq(1, length(covariates)))
      termNamesAliased[termNamesAliased %in% unlist(covariates)] <- covariateAliases
      termNamesAliasedCoded[termNamesAliasedCoded %in% unlist(covariates)] <- covariateAliases
    }

    # append number if duplicated
    for(term_j in seq_along(termNamesAliased)){
      n_occurences <- sum(termNamesAliased == termNamesAliased[term_j])
      if (n_occurences > 1) {
        term_indices <- which(termNamesAliased == termNamesAliased[term_j])
        termNamesAliased[term_indices] <- paste0(termNamesAliased[term_j], seq_len(n_occurences))
      }
    }
    termNamesAliased[1] <- ""  # no alias for intercept
    result[["regression"]][["coefficients"]][["termsAliased"]] <- termNamesAliased
    resultCoded[["regression"]][["coefficients"]][["termsAliased"]] <- termNamesAliasedCoded


    result[["regression"]][["factorLevels"]] <- list()
    result[["regression"]][["factorLevels"]][["factorNamesAliased"]] <- termNamesAliased[discretePredictorsIndices]
    result[["regression"]][["factorLevels"]][["factorNames"]] <- termNames[discretePredictorsIndices]
    result[["regression"]][["factorLevels"]][["levels"]] <- .doeCoefficientLevels(termNames[discretePredictorsIndices],
                                                                                 termNamesRemoved[discretePredictorsIndices], dataset)

    # In the coded model, only discrete factors with more than 2 levels have level-specific terms
    multiLevelIndicesCoded <- which(termNamesRemovedCoded %in% multiLevelVars)
    resultCoded[["regression"]][["factorLevels"]] <- list()
    resultCoded[["regression"]][["factorLevels"]][["factorNamesAliased"]] <- termNamesAliasedCoded[multiLevelIndicesCoded]
    resultCoded[["regression"]][["factorLevels"]][["factorNames"]] <- termNamesCoded[multiLevelIndicesCoded]
    resultCoded[["regression"]][["factorLevels"]][["levels"]] <- .doeCoefficientLevels(termNamesCoded[multiLevelIndicesCoded],
                                                                                      termNamesRemovedCoded[multiLevelIndicesCoded], dataset)


    coefficientTests <- function(coefTable, rows, saturated, keep) {
      tests <- if (saturated) {
        list(se = rep(NA, length(rows)), t = rep(NA, length(rows)), p = rep(NA, length(rows)))
      } else {
        list(se = coefTable[["Std. Error"]][rows], t = coefTable[["t value"]][rows], p = coefTable[["Pr(>|t|)"]][rows])
      }
      lapply(tests, blankNonUnique, keep = keep)
    }
    result[["regression"]][["coefficients"]][c("se", "t", "p")] <-
      coefficientTests(coefs, valid_coefs, result[["regression"]][["saturated"]], uniqueCoefs)
    resultCoded[["regression"]][["coefficients"]][c("se", "t", "p")] <-
      coefficientTests(coefsCoded, valid_coefsCoded, resultCoded[["regression"]][["saturated"]], uniqueCoefsCoded)

    ## Model formula for predictors, based on coding/aliasing
    if (options[["tableAlias"]]) {
      coefNames <- if (options[["codeFactors"]]) termNamesAliasedCoded else termNamesAliased
    } else {
      coefNames <- if (options[["codeFactors"]]) termNamesCoded else termNames
    }

    # Uncoded Formula
    fmtCoef <- .formatCoefEquation(abs(coefs[-1, 1]), digits = .numDecimals)
    coefFormula <- paste(ifelse(sign(coefs[-1,1])==1, " +", " \u2013"),
                         fmtCoef,
                         coefNames[-1],
                         collapse = "", sep = " ")

    # Now add dependent name and intercept
    filledFormula <- paste0(dep, " = ",
                            .formatCoefEquation(coefs[1, 1], digits = .numDecimals),
                            coefFormula)

    # Coded Formula
    fmtCoefCoded <- .formatCoefEquation(abs(coefsCoded[-1, 1]), digits = .numDecimals)
    coefFormulaCoded <- paste(ifelse(sign(coefsCoded[-1,1])==1, " +", " \u2013"),
                              fmtCoefCoded,
                              coefNames[-1],
                              collapse = "", sep = " ")


    filledFormulaCoded <- paste0(dep, " = ",
                                 .formatCoefEquation(coefsCoded[1, 1], digits = .numDecimals),
                                 coefFormulaCoded)

    result[["regression"]][["filledFormula"]] <- gsubInteractionSymbol(filledFormula)
    jaspResults[[currentDependent]][["doeResult"]] <- createJaspState(result)
    jaspResults[[currentDependent]][["doeResult"]]$dependOn(options = .doeAnalysisBaseDependencies())


    resultCoded[["regression"]][["filledFormula"]] <- gsubInteractionSymbol(filledFormulaCoded)
    jaspResults[[currentDependent]][["doeResultCoded"]] <- createJaspState(resultCoded)
    jaspResults[[currentDependent]][["doeResultCoded"]]$dependOn(options = .doeAnalysisBaseDependencies())
  }
}

# Level of each main-effect coefficient of a discrete factor, for the Discrete Predictor Levels legend. lm() names the
# k-th sum-to-zero contrast of factor `var` "<var>k", which belongs to its k-th level, so the level follows from the
# coefficient name itself, whatever the order of the model terms and also when some contrasts are aliased.
.doeCoefficientLevels <- function(coefNames, factorVars, dataset) {
  vapply(seq_along(coefNames), function(i) {
    levelIndex <- as.integer(substring(coefNames[i], nchar(factorVars[i]) + 1))
    levels(dataset[[factorVars[i]]])[levelIndex]
  }, character(1))
}

# VIF of each coefficient: car::vif() gives one (generalized) VIF per model term, so each coefficient gets the VIF of
# the term it belongs to (lm's "assign"; 0 = intercept, no VIF). For terms with more than 1 df this is the GVIF.
.getVIF <- function(regressionFit) {
  nCoefficients <- length(regressionFit$coefficients)
  if (ncol(regressionFit$model) < 3)
    return(rep(NA, nCoefficients))
  VIF <- try(car::vif(regressionFit), silent = TRUE) # fails with aliased coefficients (footnote in the table)
  if (jaspBase::isTryError(VIF))
    return(rep(NA, nCoefficients))
  VIF <- if (is.vector(VIF)) VIF else VIF[, 1]
  termLabels <- attr(stats::terms(regressionFit), "term.labels")
  return(unname(c(NA, VIF[termLabels])[regressionFit$assign + 1]))
}

.doeResponseOptimizerTables <- function(jaspResults, options, dataset, continuousPredictors, discretePredictors, blocks, covariates, readyModelPresent) {
  roOptionsDf <- do.call(rbind, lapply(options[["responsesResponseOptimizer"]], as.data.frame))
  readyRO <- length(roOptionsDf$variable) > 0 # variable selected

  if (!readyModelPresent || !readyRO)
    return()

  # the optimizer is the expensive part: rerun it only when the states are invalidated (optimizer options or model changed)
  if (is.null(jaspResults[["roOptions"]]) || is.null(jaspResults[["roResult"]])) {
    roOptionsDf <- .doeResolveOptimizerSettings(roOptionsDf, dataset, options[["responseOptimizerManualBounds"]], options[["responseOptimizerManualTarget"]])
    settingsErrors <- .doeOptimizerSettingsErrors(roOptionsDf, options[["responseOptimizerManualBounds"]])
    if (length(settingsErrors) > 0) {
      tb1 <- createJaspTable(gettext("Response Optimizer Settings"))
      tb1$dependOn(options = .doeResponseOptimizerDependencies())
      tb1$position <- 1
      jaspResults[["tableRoSettings"]] <- tb1
      fromData <- .doeOptimizerValuesFromData(roOptionsDf, options)
      if (fromData[["bounds"]] || fromData[["targets"]])
        settingsErrors <- c(settingsErrors, gettext("Bounds and targets that are not set manually are the observed minimum or maximum of the response."))
      tb1$setError(paste(settingsErrors, collapse = " "))
      return()
    }

    roOptionsState <- createJaspState()
    jaspResults[["roOptions"]] <- roOptionsState
    jaspResults[["roOptions"]]$dependOn(options = .doeResponseOptimizerDependencies())
    roOptionsState$object <- roOptionsDf

    models    <- .doeOptimizerModels(jaspResults, roOptionsDf$variable, dataset, blocks, covariates)
    roOutcome <- .calculateOptimalResponse(jaspResults, options, dataset, continuousPredictors, discretePredictors, roOptionsDf$variable, roOptionsDf, models)
    roOutcomeState <- createJaspState()
    jaspResults[["roResult"]] <- roOutcomeState
    jaspResults[["roResult"]]$dependOn(options = .doeResponseOptimizerDependencies())
    roOutcomeState$object <- roOutcome
  }
  roOptionsDf <- jaspResults[["roOptions"]]$object
  roOutcome   <- jaspResults[["roResult"]]$object
  roDependent <- roOptionsDf$variable
  models      <- .doeOptimizerModels(jaspResults, roDependent, dataset, blocks, covariates)

  optimParam  <- roOutcome$parameters
  hasSolution <- !is.null(optimParam) # NULL when no setting with a unique prediction was found (aliased contrasts)
  desi <- roOutcome$desirability
  desi <- pmax(0, pmin(1, desi)) # after optim. desi can be bounded to 0 and 1
  if (length(continuousPredictors) >= 1 && !identical(continuousPredictors, ""))
    contLevels <- optimParam[names(optimParam) %in% continuousPredictors]
  if (length(discretePredictors) >= 1 && !identical(discretePredictors, ""))
    discLevels <- .doeDiscreteSettings(optimParam, discretePredictors)

  if (hasSolution)
    predValues <- .doePredictResponses(continuousLevels = contLevels, continuousPredictors = continuousPredictors, discretePredictors = discretePredictors,
                                       currentDiscreteLevels = discLevels, models = models, dependent = roDependent)

  # Did all the required calculations and saved to state, if tables should not be displayed return now
  if (!options[["optimizationSolutionTable"]])
    return()

  tb1 <- createJaspTable(gettext("Response Optimizer Settings"))
  tb1$addColumnInfo(name = "response", title = "Response", type = "string")
  tb1$addColumnInfo(name = "goal", title = "Goal", type = "string")
  tb1$addColumnInfo(name = "lb", title = "Lower", type = "number")
  tb1$addColumnInfo(name = "target", title = "Target", type = "number")
  tb1$addColumnInfo(name = "ub", title = "Upper", type = "number")
  tb1$addColumnInfo(name = "weight", title = "Weight", type = "number")
  tb1$addColumnInfo(name = "importance", title = "Importance", type = "number")
  tb1$dependOn(options = c(.doeResponseOptimizerDependencies(), "optimizationSolutionTable"))
  tb1$position <- 1
  jaspResults[["tableRoSettings"]] <- tb1

  rows1 <- data.frame(response = roOptionsDf$variable, goal = roOptionsDf$responseOptimizerGoal,
                      lb = roOptionsDf$responseOptimizerLowerBound, target = roOptionsDf$responseOptimizerTarget,
                      ub = roOptionsDf$responseOptimizerUpperBound, weight = roOptionsDf$responseOptimizerWeight,
                      importance = roOptionsDf$responseOptimizerImportance)
  rows1$goal <- paste0(toupper(substr(rows1$goal, 1, 1)), substr(rows1$goal, 2, nchar(rows1$goal))) # Capitalize
  tb1$addRows(rows1)

  fromData <- .doeOptimizerValuesFromData(roOptionsDf, options)
  if (fromData[["bounds"]] && fromData[["targets"]])
    tb1$addFootnote(gettext("The lower and upper bounds, and the targets of the goals maximize and minimize, are estimated from data."))
  else if (fromData[["bounds"]])
    tb1$addFootnote(gettext("The lower and upper bounds are estimated from data."))
  else if (fromData[["targets"]])
    tb1$addFootnote(gettext("The targets of the goals maximize and minimize are estimated from data."))
  for (note in .doeOptimizerTargetOutsideRangeNotes(roOptionsDf, options[["responseOptimizerManualBounds"]]))
    tb1$addFootnote(note)

  tb2 <- createJaspTable(gettext("Response Optimizer Solution"))
  tb2$addColumnInfo(name = "desi", title = "Composite desirability", type = "number")
  allPredictors <- c(continuousPredictors, unlist(discretePredictors))
  allPredictors <- allPredictors[allPredictors != ""]
  for (pred in allPredictors) {
    predType <- if (pred %in% continuousPredictors) "number" else "string"
    tb2$addColumnInfo(name = pred, title = pred, type = predType)
  }
  for (dep in roDependent) {
    colName  <- paste0(dep, "_fit")
    colTitle <- gettextf("%s fit", dep)
    tb2$addColumnInfo(name = colName, title = colTitle, type = "number")
  }
  tb2$dependOn(options = c(.doeResponseOptimizerDependencies(), "optimizationSolutionTable"))

  tb2$position <- 2
  jaspResults[["tableRoSolution"]] <- tb2
  if (!hasSolution) {
    tb2$setError(.doeOptimizerNotEstimableError())
    return()
  }

  rows2 <- data.frame(desi = desi)
  for (pred in allPredictors) {
    if (pred %in% continuousPredictors)
      rows2[[pred]] <- round(as.numeric(optimParam[[pred]]), .numDecimals)
    else
      rows2[[pred]] <- as.character(optimParam[[pred]])
  }
  for (dep in roDependent) {
    colName <- paste0(dep, "_fit")
    rows2[[colName]] <- round(as.numeric(predValues[[dep]]), .numDecimals)
  }
  tb2$addRows(rows2)
  if (desi <= 0)
    tb2$addFootnote(gettext("No factor setting with a composite desirability above 0 was found. The optimizer starts from the design points and can miss acceptable settings that lie between them."))
  .doeAddPredictionSettingsFootnotes(tb2, blocks, covariates)
  if (.doeOptimizerModelsAliased(models))
    tb2$addFootnote(.doeAliasedOptimizerNote())
}

.doeResponseOptimizerPlots <- function(jaspResults, options, dataset, continuousPredictors,
                                       discretePredictors, blocks, covariates, readyModelPresent) {
  if (is.null(jaspResults[["roOptions"]]))
    return()
  roOptionsDf <- jaspResults[["roOptions"]]$object
  roDependent <- roOptionsDf$variable

  readyRO <- length(roDependent) > 0

  if (!options[["optimizationPlot"]] || !readyModelPresent || !readyRO)
    return()
  roOutcome <- jaspResults[["roResult"]]$object
  models <- .doeOptimizerModels(jaspResults, roDependent, dataset, blocks, covariates)

  discretePredictors <- unlist(discretePredictors)
  continuousPredictors <- unlist(continuousPredictors)

  jaspResults[["plotRo"]] <- createJaspContainer(title = gettext("Optimization Plot"))
  jaspResults[["plotRo"]]$dependOn(options = .doeOptimizationPlotDependencies()) # no empty container after a settings error
  jaspResults[["plotRo"]]$position <- 4

  # Create Df if custom parameters are entered
  if (options[["optimizationPlotCustomParameters"]]) {
    currentSettings <- options[["optimizationPlotCustomParameterValues"]]
    currentSettings <- do.call(cbind, lapply(currentSettings, function(x) setNames(data.frame(x$value, stringsAsFactors = FALSE), x$variable)))
    # if any of the values is == "" then return empty plot and table with note that asks to set value
    if (any(unlist(currentSettings) == "")) {
      jaspTable <- createJaspTable(gettext("Response Optimizer Settings"))
      jaspTable$addColumnInfo(name = "response", title = "Response", type = "string")
      jaspTable$addColumnInfo(name = "goal", title = "Goal", type = "string")
      jaspTable$addColumnInfo(name = "lb", title = "Lower", type = "number")
      jaspTable$addColumnInfo(name = "target", title = "Target", type = "number")
      jaspTable$addColumnInfo(name = "ub", title = "Upper", type = "number")
      jaspTable$addColumnInfo(name = "weight", title = "Weight", type = "number")
      jaspTable$addColumnInfo(name = "importance", title = "Importance", type = "number")
      jaspTable$addFootnote(gettext("Enter values for all predictors to create an optimization Plot with manual input parameters."))
      jaspTable$dependOn(options = .doeOptimizationPlotDependencies())
      jaspResults[["plotRo"]][["table"]] <- jaspTable
      return()
    }

    if (length(discretePredictors) >= 1 && !identical(discretePredictors, "")) {
      # transform factors to correct type
      for (discPred in discretePredictors) {
        # check if entered factor level is plausible
        if (!unlist(currentSettings[discPred]) %in% levels(dataset[[discPred]])) {
          jaspPlot <- createJaspPlot(title = gettext("Summary Plot"), width = 500, height = 500)
          jaspPlot$setError(paste0(gettextf("Enter plausible values for all discrete predictors.
                                   Value %1$s is not a possible for the discrete predictor %2$s. Allowed values: ",
                                            unlist(currentSettings[discPred]), discPred), paste0(levels(dataset[[discPred]]), collapse = ", "), "."))
          jaspPlot$dependOn(options = .doeOptimizationPlotDependencies())
          jaspResults[["plotRo"]][["plot"]] <- jaspPlot
          return()
        }
        currentSettings[discPred] <- factor(currentSettings[discPred], levels = levels(dataset[[discPred]])) # a named data frame, also with one predictor
      }
      currentDiscreteLevels <- .doeDiscreteSettings(currentSettings, discretePredictors)
    } else {
      currentDiscreteLevels <- ""
    }

    if (length(continuousPredictors) >= 1 && !identical(continuousPredictors, "")) {
      # transform continuous predictors to numeric and throw error if not possible
      for (contPred in continuousPredictors) {
        # check if entered continuous value is plausible
        if (is.na(as.numeric(currentSettings[contPred]))) {
          jaspPlot <- createJaspPlot(title = gettext("Summary Plot"), width = 500, height = 500)
          jaspPlot$setError(gettext("Enter plausible values for all continuous predictors. Only numeric values are allowed."))
          jaspPlot$dependOn(options = .doeOptimizationPlotDependencies())
          jaspResults[["plotRo"]][["plot"]] <- jaspPlot
          return()
        }
        currentSettings[contPred] <- as.numeric(currentSettings[contPred]) # a named data frame, also with one predictor
      }
      continuousLevels <- currentSettings[continuousPredictors]
    } else {
      continuousLevels <- ""
    }
  } else { # if default optimal parameters should be used
    currentSettings <- roOutcome$parameters
    if (is.null(currentSettings)) { # no setting with a unique prediction was found; manual settings can still have one
      jaspPlot <- createJaspPlot(title = gettext("Summary Plot"), width = 500, height = 500)
      jaspPlot$dependOn(options = .doeOptimizationPlotDependencies())
      jaspResults[["plotRo"]][["plot"]] <- jaspPlot
      jaspPlot$setError(.doeOptimizerNotEstimableError())
      return()
    }
    if (length(continuousPredictors) >= 1 && !identical(continuousPredictors, "")) {
      continuousLevels <- currentSettings[continuousPredictors]
    } else {
      continuousLevels <- ""
    }
    if (length(discretePredictors) >= 1 && !identical(discretePredictors, "")) {
      currentDiscreteLevels <- .doeDiscreteSettings(currentSettings, discretePredictors)
    } else {
      currentDiscreteLevels <- ""
    }
  }

  nrow <- length(roDependent) + 1
  allPredictors <- c(discretePredictors, continuousPredictors)
  allPredictors <- allPredictors[allPredictors != ""]
  ncol <- length(allPredictors)
  plotMat <- matrix(list(), nrow = nrow, ncol = ncol)
  extrapolationNoteBol <- FALSE

  for (row in seq_len(nrow)) {
    for (col in seq_len(ncol)) {
      # First row is for comp. desi
      outcomeType <- if (row == 1) "compDesi" else options[["optimizationPlotPredictionType"]]
      dependentTarget <- if (row == 1) "" else roDependent[row-1]
      currentPred <- allPredictors[col]
      yAxisLabel <- switch(outcomeType,
                           "compDesi" = gettext("Comp. desirability"),
                           "response" = dependentTarget,
                           "individualDesirability" = paste0(gettext("Ind. desirability ("), dependentTarget, ")"))
      xAxisLabel <- currentPred
      if (currentPred %in% discretePredictors) {
        plottingWindowDf <- data.frame(x = unique(dataset[[currentPred]]))
        plottingWindowDf$y <- .individualValueROprediction(value = plottingWindowDf$x,
                                                           valueName = currentPred,
                                                           outcomeType = outcomeType,
                                                           continuousLevels = continuousLevels,
                                                           continuousPredictors = continuousPredictors,
                                                           discretePredictors = discretePredictors,
                                                           currentDiscreteLevels = currentDiscreteLevels,
                                                           models = models,
                                                           dependent = roDependent,
                                                           dependentTarget = dependentTarget,
                                                           roOptionsDf = roOptionsDf)
        if (outcomeType == "compDesi"| outcomeType == "individualDesirability")
          plottingWindowDf$y <- pmax(0, pmin(1, plottingWindowDf$y))
        currentLevel <- if (is.list(currentDiscreteLevels)) currentDiscreteLevels[[currentPred]] else currentDiscreteLevels
        plottingWindowDf$color <- ifelse(as.character(plottingWindowDf$x) == as.character(currentLevel), "red", "blue")
        yLimits <- .doeOptimizationPlotLimits(plottingWindowDf$y, outcomeType)
        yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(plottingWindowDf$y[!is.na(plottingWindowDf$y)], yLimits))
        plot <- ggplot2::ggplot(plottingWindowDf, ggplot2::aes(x = x, y = y, color = color)) +
          ggplot2::scale_y_continuous(name = yAxisLabel, limits = yLimits, breaks = yBreaks) +
          ggplot2::scale_color_identity() +
          ggplot2::xlab(xAxisLabel) +
          ggplot2::geom_point(size = 4) +
          jaspGraphs::themeJaspRaw() +
          jaspGraphs::geom_rangeframe()
      } else {
        currentPredValue <- as.numeric(continuousLevels[[currentPred]])
        plottingWindowDf <- data.frame(x = seq(from = min(dataset[[currentPred]], currentPredValue),
                                               to = max(dataset[[currentPred]], currentPredValue),
                                                length.out = 50))
        plottingWindowDf$y <- .individualValueROprediction(value = plottingWindowDf$x,
                                                           valueName = currentPred,
                                                           outcomeType = outcomeType,
                                                           continuousLevels = continuousLevels,
                                                           continuousPredictors = continuousPredictors,
                                                           discretePredictors = discretePredictors,
                                                           currentDiscreteLevels = currentDiscreteLevels,
                                                           models = models,
                                                           dependent = roDependent,
                                                           dependentTarget = dependentTarget,
                                                           roOptionsDf = roOptionsDf)
        if (outcomeType == "compDesi"| outcomeType == "individualDesirability")
          plottingWindowDf$y <- pmax(0, pmin(1, plottingWindowDf$y))
        xBreaks <- jaspGraphs::getPrettyAxisBreaks(plottingWindowDf$x)
        # Coordinates for the indication of the current parameter settings
        redPointCoordX <- currentPredValue
        if (redPointCoordX < min(dataset[[currentPred]]) | redPointCoordX > max(dataset[[currentPred]]))
          extrapolationNoteBol <- TRUE
        redPointCoordY <- .individualValueROprediction(value = redPointCoordX,
                                                       valueName = currentPred,
                                                       outcomeType = outcomeType,
                                                       continuousLevels = continuousLevels,
                                                       continuousPredictors = continuousPredictors,
                                                       discretePredictors = discretePredictors,
                                                       currentDiscreteLevels = currentDiscreteLevels,
                                                       models = models,
                                                       dependent = roDependent,
                                                       dependentTarget = dependentTarget,
                                                       roOptionsDf = roOptionsDf)
        if (outcomeType == "compDesi"| outcomeType == "individualDesirability")
          redPointCoordY <- pmax(0, pmin(1, redPointCoordY))
        # with aliased contrasts, the current setting can be the only one with a unique prediction, between the grid points
        # of the curve (e.g., the mean of a factor confounded with the blocks); then the axis must still show it
        yValues <- if (all(is.na(plottingWindowDf$y))) redPointCoordY else plottingWindowDf$y
        yLimits <- .doeOptimizationPlotLimits(yValues, outcomeType)
        yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(yValues[!is.na(yValues)], yLimits))
        plot <- ggplot2::ggplot(plottingWindowDf, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_line(color = "blue", linewidth = 1) +
          ggplot2::annotate("point", x = redPointCoordX, y = redPointCoordY, color = "red", size = 3) +
          ggplot2::scale_y_continuous(name = yAxisLabel, limits = yLimits, breaks = yBreaks) +
          ggplot2::scale_x_continuous(name = xAxisLabel, breaks = xBreaks) +
          jaspGraphs::themeJaspRaw() +
          jaspGraphs::geom_rangeframe()
      }
      plotMat[[row, col]] <- plot
    }
  }

  jaspPlot <- createJaspPlot(title = gettext("Summary Plot"), width = 300*ncol, height = 300*nrow)
  jaspPlot$dependOn(options = .doeOptimizationPlotDependencies())
  plotObject <- jaspGraphs::ggMatrixPlot(plotList = plotMat, nr = nrow, nc = ncol)
  jaspPlot$plotObject <- plotObject
  jaspResults[["plotRo"]][["plot"]] <- jaspPlot

  tbTitle <- if (options[["optimizationPlotCustomParameters"]]) gettext("Optimization Plot Summary (Manual Predictor Values)") else gettext("Optimization Plot Summary")
  tb <- createJaspTable(tbTitle)
  tb$addColumnInfo(name = "desi", title = gettext("Composite desirability"), type = "number")
  for (pred in allPredictors) {
    predType <- if (pred %in% continuousPredictors) "number" else "string"
    tb$addColumnInfo(name = pred, title = pred, type = predType)
  }

  for (dep in roDependent) {
    colName  <- paste0(dep, " fit")
    colTitle <- gettextf("%s fit", dep)
    tb$addColumnInfo(name = colName, title = colTitle, type = "number")
  }

  tb$dependOn(options = .doeOptimizationPlotDependencies())
  extrapolationNote <- gettext("One or more input parameters for continuous predictors are outside of the design space. Extrapolation beyond the design space might not be valid.")
  if (extrapolationNoteBol)
    tb$addFootnote(extrapolationNote)
  jaspResults[["plotRo"]][["table"]] <- tb

  desi <- .predictDesirability(continuousLevels, continuousPredictors, discretePredictors, currentDiscreteLevels, models, roDependent,
                                           roOptionsDf) * -1 # because it is returned as negative for optim. but in this case we want the true value
  desi <- pmax(0, pmin(1, desi))
  predValues <- .doePredictResponses(continuousLevels = continuousLevels, continuousPredictors = continuousPredictors, discretePredictors = discretePredictors,
                                     currentDiscreteLevels = currentDiscreteLevels, models = models, dependent = roDependent)

  rows <- data.frame(desi = desi)
  for (pred in allPredictors) {
    if (pred %in% continuousPredictors)
      rows[[pred]] <- round(as.numeric(currentSettings[[pred]]), .numDecimals)
    else
      rows[[pred]] <- as.character(currentSettings[[pred]])
  }
  for (dep in roDependent)
    rows[[paste0(dep, " fit")]] <- round(as.numeric(predValues[[dep]]), .numDecimals)
  tb$addRows(rows)
  if (options[["optimizationPlotCustomParameters"]]) # at the optimum, the solution table already carries these notes
    .doeAddPredictionSettingsFootnotes(tb, blocks, covariates)
  if (anyNA(predValues))
    tb$addFootnote(.doeNotEstimablePredictionNote())
  else if (.doeOptimizerModelsAliased(models))
    tb$addFootnote(.doeAliasedOptimizerNote())

  if (options[["optimizationIntervals"]] && options[["optimizationPlotCustomParameters"]]) {
    predictionTable <- createJaspTable(gettext("Predicted Responses (Manual Predictor Values)"))
    predictionTable$dependOn(options = c(.doeOptimizationPlotDependencies(), "optimizationPlotCustomParameterValues",
                                         "optimizationIntervals", "optimizationIntervalsLevel"))
    jaspResults[["plotRo"]][["predictionTable"]] <- predictionTable
    .doeFillPredictionTable(predictionTable, jaspResults, roDependent, .doeSettingsAsList(currentSettings, allPredictors), dataset,
                            blocks, covariates, options[["optimizationIntervalsLevel"]])
    if (extrapolationNoteBol) # the intervals rest on the model beyond the data as well
      predictionTable$addFootnote(extrapolationNote)
  }
}

.doeResponseOptimizerPredictionTable <- function(jaspResults, options, dataset, continuousPredictors, discretePredictors, blocks, covariates, readyModelPresent) {
  if (!options[["optimizationIntervals"]] || !readyModelPresent || is.null(jaspResults[["roResult"]]) || !is.null(jaspResults[["tableRoPrediction"]]))
    return()

  table <- createJaspTable(gettext("Predicted Responses (Optimal Solution)"))
  table$dependOn(options = c(.doeResponseOptimizerDependencies(), "optimizationIntervals", "optimizationIntervalsLevel"))
  table$position <- 3
  jaspResults[["tableRoPrediction"]] <- table
  if (is.null(jaspResults[["roResult"]]$object[["parameters"]])) { # no setting with a unique prediction was found
    table$setError(.doeOptimizerNotEstimableError())
    return()
  }

  predictors <- c(unlist(discretePredictors), unlist(continuousPredictors))
  predictors <- predictors[predictors != ""]
  settings   <- .doeSettingsAsList(jaspResults[["roResult"]]$object[["parameters"]], predictors)
  .doeFillPredictionTable(table, jaspResults, jaspResults[["roOptions"]]$object$variable, settings, dataset, blocks, covariates,
                          options[["optimizationIntervalsLevel"]])
}

# Fitted models of the optimized responses, with the data, blocks and covariates their predictions need
# (.doePredictResponses). The optimizer, the optimization plot and the prediction table all predict through
# .doePredictionModelRow, so they agree.
.doeOptimizerModels <- function(jaspResults, responses, dataset, blocks, covariates) {
  fits <- lapply(stats::setNames(responses, responses), function(response)
    jaspResults[[response]][["doeResult"]]$object[["regression"]][["object"]])
  list(fits = fits, dataset = dataset, blocks = blocks, covariates = covariates)
}

# Discrete predictor settings of the optimizer, as a named list so that each factor keeps its own levels
# (unlist() would merge them into one factor, jasp-issues#4665). A single setting stays as is.
.doeDiscreteSettings <- function(settings, discretePredictors) {
  if (length(settings) == 1)
    return(settings)
  return(as.list(settings)[unlist(discretePredictors)])
}

.doeResponseOptimizerDependencies <- function() {
  c("responsesResponseOptimizer", "responseOptimizerGoal", "responseOptimizerLowerBound", "responseOptimizerTarget",
    "responseOptimizerUpperBound", "responseOptimizerWeight", "responseOptimizerImportance",
    "responseOptimizerManualBounds", "responseOptimizerManualTarget",
    .doeAnalysisBaseDependencies())
}

# Whether any bound or target comes from the observed responses. The goal target always uses the entered target,
# so only maximize/minimize targets can come from the data.
.doeOptimizerValuesFromData <- function(roOptionsDf, options) {
  list(bounds  = !options[["responseOptimizerManualBounds"]],
       targets = !options[["responseOptimizerManualTarget"]] && any(roOptionsDf$responseOptimizerGoal != "target"))
}

# The optimization plot also depends on the plot-only options; the optimizer states above do not, so toggling them
# does not rerun the optimizer.
.doeOptimizationPlotDependencies <- function() {
  c(.doeResponseOptimizerDependencies(), "optimizationPlot", "optimizationPlotCustomParameters")
}

# Predictor settings (a one-row data frame, also with a single predictor) as a named list
.doeSettingsAsList <- function(settings, predictors) {
  return(as.list(settings[1, predictors, drop = FALSE]))
}

.doeFillPredictionTable <- function(table, jaspResults, responses, settings, dataset, blocks, covariates, level) {
  intervalLevel <- 100 * level
  table$addColumnInfo(name = "response", title = gettext("Response"), type = "string")
  table$addColumnInfo(name = "fit",      title = gettext("Fit"), type = "number")
  table$addColumnInfo(name = "se",       title = gettext("Standard error"), type = "number")
  table$addColumnInfo(name = "ciLower",  title = gettext("Lower"), type = "number", overtitle = gettextf("%s%% CI", intervalLevel))
  table$addColumnInfo(name = "ciUpper",  title = gettext("Upper"), type = "number", overtitle = gettextf("%s%% CI", intervalLevel))
  table$addColumnInfo(name = "piLower",  title = gettext("Lower"), type = "number", overtitle = gettextf("%s%% PI", intervalLevel))
  table$addColumnInfo(name = "piUpper",  title = gettext("Upper"), type = "number", overtitle = gettextf("%s%% PI", intervalLevel))

  rows <- do.call(rbind, lapply(responses, function(response) {
    fit <- jaspResults[[response]][["doeResult"]]$object[["regression"]][["object"]]
    cbind(response = response, .doePredictionWithIntervals(fit, settings, dataset, level, blocks, covariates))
  }))
  table$setData(rows[names(rows) != "estimable"])

  table$addFootnote(gettext("PI: prediction interval for a single new run."))
  if (any(is.na(rows[["se"]]) & rows[["estimable"]]))
    table$addFootnote(gettext("Intervals cannot be calculated when the model has no degrees of freedom for error."))
  if (!all(rows[["estimable"]]))
    table$addFootnote(.doeNotEstimablePredictionNote())
  .doeAddPredictionSettingsFootnotes(table, blocks, covariates, intervals = TRUE)
}

.doeAddPredictionSettingsFootnotes <- function(table, blocks, covariates, intervals = FALSE) {
  if (length(blocks) > 0 && !identical(blocks, "")) {
    # the block effects are averaged out, so the PI is for a run in an average block
    blockNote <- if (intervals) {
      gettext("Predictions are averaged over the blocks; the prediction interval excludes variation between blocks.")
    } else {
      gettext("Predictions are averaged over the blocks.")
    }
    table$addFootnote(blockNote)
  }
  if (length(covariates) > 0 && !identical(covariates, ""))
    table$addFootnote(gettext("Covariates are held at their mean."))
}

# Model matrix row (1 x fitted coefficients) of the fit at the given predictor settings, a named list.
# Blocks are averaged out: the block columns are averaged over the block levels of the fit, which holds for any
# contrasts. Covariates are held at their mean. Attribute "estimable": whether the prediction is unique, which fails
# with aliased columns at settings the data cannot estimate (e.g., a combination of factor levels without runs).
.doePredictionModelRow <- function(fit, settings, dataset, blocks = NULL, covariates = NULL) {
  blocks      <- unlist(blocks)
  blocks      <- blocks[blocks != ""]
  blockLevels <- if (length(blocks) > 0) fit[["xlevels"]][[blocks]] # NULL when the block term is not in the model

  newdata   <- dataset[rep(1, max(1, length(blockLevels))), , drop = FALSE] # one row per block level; keeps the column types
  newdata[] <- lapply(newdata, function(x) { attr(x, "contrasts") <- NULL; x }) # contrasts come from the fit
  for (predictor in names(settings)) {
    value <- unlist(settings[[predictor]])
    newdata[[predictor]] <- if (is.factor(dataset[[predictor]])) {
      factor(as.character(value), levels = levels(dataset[[predictor]]))
    } else {
      as.numeric(as.character(value))
    }
  }
  covariates <- unlist(covariates)
  for (covariate in covariates[covariates != ""])
    newdata[[covariate]] <- mean(dataset[[covariate]])
  if (length(blockLevels) > 0)
    newdata[[blocks]] <- factor(blockLevels, levels = blockLevels)

  modelTerms   <- stats::delete.response(stats::terms(fit))
  x            <- stats::model.matrix(modelTerms, stats::model.frame(modelTerms, newdata, xlev = fit[["xlevels"]]),
                                      contrasts.arg = fit[["contrasts"]])
  x0           <- x[1, , drop = FALSE]
  blockColumns <- attr(x, "assign") %in% which(attr(modelTerms, "term.labels") %in% blocks)
  x0[, blockColumns] <- colMeans(x[, blockColumns, drop = FALSE])
  estimable <- .doeEstimable(fit, x0)
  x0        <- x0[, !is.na(stats::coef(fit)), drop = FALSE]
  attr(x0, "estimable") <- estimable
  return(x0)
}

# Fitted response at the given predictor settings, with its standard error, the confidence interval for the mean
# response and the prediction interval for a single new observation. Blocks are averaged out and covariates are held
# at their mean (.doePredictionModelRow).
.doePredictionWithIntervals <- function(fit, settings, dataset, level, blocks = NULL, covariates = NULL) {
  x0       <- .doePredictionModelRow(fit, settings, dataset, blocks, covariates)
  coefs    <- stats::coef(fit)
  fitValue <- drop(x0 %*% coefs[!is.na(coefs)])

  errorDf <- fit[["df.residual"]]
  if (!attr(x0, "estimable"))
    return(data.frame(fit = NA_real_, se = NA_real_, ciLower = NA_real_, ciUpper = NA_real_, piLower = NA_real_, piUpper = NA_real_, estimable = FALSE))
  if (errorDf == 0)
    return(data.frame(fit = fitValue, se = NA_real_, ciLower = NA_real_, ciUpper = NA_real_, piLower = NA_real_, piUpper = NA_real_, estimable = TRUE))

  sigma  <- sqrt(sum(stats::residuals(fit)^2) / errorDf)
  seFit  <- sqrt(drop(x0 %*% stats::vcov(fit, complete = FALSE) %*% t(x0)))
  sePred <- sqrt(seFit^2 + sigma^2)
  tCrit  <- stats::qt((1 + level) / 2, errorDf)
  data.frame(fit     = fitValue,
             se      = seFit,
             ciLower = fitValue - tCrit * seFit,
             ciUpper = fitValue + tCrit * seFit,
             piLower = fitValue - tCrit * sePred,
             piUpper = fitValue + tCrit * sePred,
             estimable = TRUE)
}

.calculateOptimalResponse <- function(jaspResults, options, dataset, continuousPredictors, discretePredictors, dependent, roOptionsDf, models) {

  if (length(continuousPredictors) >= 1 && !identical(continuousPredictors, "")) {
    # starting points (the design points) and limits for cont. variables
    continuousPredictorsData <- dataset[continuousPredictors]
    continuousPredictorsStarts <- unique(as.matrix(continuousPredictorsData))
    continuousPredictorsMin <- sapply(continuousPredictorsData, min)
    continuousPredictorsMax <- sapply(continuousPredictorsData, max)
  }

  if (length(discretePredictors) >= 1 && !identical(discretePredictors, "")) {
    discretePredictorLevelList <- list()
    for (i in seq_along(discretePredictors)) {
      predictor_i <- discretePredictors[i]
      discretePredictorLevelList[[predictor_i]] <- levels(dataset[[predictor_i]])
    }

    discreteCoefNames <- unname(unlist(Map(function(v) paste0(v, seq_len(nlevels(dataset[[v]]) - 1)), discretePredictors)))

    gridSearchDf <- expand.grid(discretePredictorLevelList)
    optimalResultDf <- gridSearchDf
    optimalResultDf[["outcomeValue"]] <- rep(NA, nrow(optimalResultDf))
    if (length(continuousPredictors) >= 1 && !identical(continuousPredictors, "")) {
      optimalResultDf[continuousPredictors] <- rep(NA, nrow(optimalResultDf))
    }

    # Loop over all possible discrete value (combinations)
    for (i in seq_len(nrow(gridSearchDf))) {
      currentDiscreteLevels <- gridSearchDf[i,]
      if (length(continuousPredictors) >= 1 && !identical(continuousPredictors, "")) {
        currrentOptimResult <- .doeOptimizeContinuous(continuousPredictorsStarts, continuousPredictorsMin, continuousPredictorsMax,
                                                      continuousPredictors, discretePredictors, currentDiscreteLevels,
                                                      models, dependent, roOptionsDf)
        # NA (left out below) when this combination has no setting with a unique prediction (aliased contrasts)
        optimalResultDf[i, "outcomeValue"] <- currrentOptimResult$value * -1  # after optimization the sign can be switched again
        optimalResultDf[i, continuousPredictors] <- currrentOptimResult$par
      } else {
        optimalResultDf[i, "outcomeValue"]  <-  .predictDesirability(continuousLevels, continuousPredictors, discretePredictors, currentDiscreteLevels, models, dependent,
                                                                     roOptionsDf) * -1
      }
    }
  } else {
    currrentOptimResult <- .doeOptimizeContinuous(continuousPredictorsStarts, continuousPredictorsMin, continuousPredictorsMax,
                                                  continuousPredictors, discretePredictors, currentDiscreteLevels,
                                                  models, dependent, roOptionsDf)
    optimalResultDf <- data.frame("outcomeValue" = currrentOptimResult$value * -1)  # after optimization the sign can be switched again
    optimalResultDf[continuousPredictors] <- currrentOptimResult$par
  }
  # Settings without a unique prediction (aliased contrasts) have NA and are skipped. Averaged over the blocks, not even
  # the design points need to have one, e.g. when a factor is held constant within blocks (a block contrast aliased with
  # it); then only settings between the design points can have one, which the search can miss.
  if (all(is.na(optimalResultDf$outcomeValue)))
    return(list(parameters = NULL, desirability = NA_real_))
  # drop = FALSE keeps a named one-row data frame also with a single predictor
  optimalParameters <- optimalResultDf[which.max(optimalResultDf$outcomeValue), names(optimalResultDf) != "outcomeValue", drop = FALSE]
  maxDesirability <- max(optimalResultDf$outcomeValue, na.rm = TRUE)

  return(list(parameters = optimalParameters, desirability = maxDesirability))
}


# Maximizes the composite desirability over the continuous predictors (L-BFGS-B within the observed ranges).
# The desirability is flat (0 beyond a bound, 1 beyond a target), so a single start can stall there; instead start from the
# first design point (the former single start, so the result is never worse) and the nStarts design points with the
# highest desirability, and keep the best optimum.
.doeOptimizeContinuous <- function(startPoints, lower, upper, continuousPredictors, discretePredictors, currentDiscreteLevels,
                                   models, dependent, roOptionsDf, nStarts = 3) {
  # NA: no unique prediction at these settings (aliased contrasts). optim() needs a number, so the search treats them as
  # desirability 0; the result is checked afterwards.
  rawNegDesirability <- function(par) .predictDesirability(par, continuousPredictors, discretePredictors, currentDiscreteLevels,
                                                            models, dependent, roOptionsDf)
  negDesirability <- function(par) {
    value <- rawNegDesirability(par)
    if (is.na(value)) 0 else value
  }
  rawStartValue <- vapply(seq_len(nrow(startPoints)), function(s) rawNegDesirability(startPoints[s, ]), numeric(1))
  startValue    <- ifelse(is.na(rawStartValue), 0, rawStartValue)
  startRows     <- unique(c(1, order(startValue)[seq_len(min(nStarts, nrow(startPoints)))])) # order() keeps ties in design order
  best <- NULL
  for (s in startRows) {
    result <- optim(par = startPoints[s, ], fn = negDesirability, method = "L-BFGS-B", lower = lower, upper = upper)
    if (is.null(best) || result$value < best$value)
      best <- result
  }
  best$par   <- pmin(pmax(best$par, lower), upper) # L-BFGS-B can overshoot a bound by rounding (1 + 2e-16), which looks like extrapolation
  best$value <- rawNegDesirability(best$par)
  # An optimum without a unique prediction falls back to the best start point that has one; NA when none has
  if (is.na(best$value) && any(!is.na(rawStartValue))) {
    s          <- which.min(rawStartValue)
    best$par   <- startPoints[s, ]
    best$value <- rawStartValue[s]
  }
  return(best)
}

.individualValueROprediction <- function(value, valueName,
                                         outcomeType = c("response", "individualDesirability", "compDesi"),
                                         continuousLevels, continuousPredictors,
                                         discretePredictors, currentDiscreteLevels,
                                         models, dependent, dependentTarget = "",
                                         roOptionsDf) {
  returnedValue <- numeric(length = length(value))
  for (i in seq_along(value)) {
    if (valueName %in% continuousPredictors) {
      if (length(continuousPredictors) > 1) {
        valueIndex <- which(names(continuousLevels) == valueName)
        continuousLevels[valueIndex] <- value[i]
      } else {
        continuousLevels <- value[i]
      }
    } else if ((valueName %in% discretePredictors)) {
      if (length(discretePredictors) > 1) {
        currentDiscreteLevels[[valueName]] <- value[i] # [[ ]] keeps the factor; [ ] on a list would drop it to its code
      } else {
        currentDiscreteLevels <- value[i]
      }
    }
    compDesi <- .predictDesirability(continuousLevels, continuousPredictors, discretePredictors, currentDiscreteLevels, models, dependent,
                                     roOptionsDf) * -1  # flip sign again

    if (dependentTarget != "") {
      rawPrediction <- .doePredictResponses(continuousLevels, continuousPredictors, discretePredictors,
                                            currentDiscreteLevels, models, dependent)
      rawPrediction <- rawPrediction[dependentTarget]

      indDesi <- if (anyNA(rawPrediction)) NA_real_ else .calculateDesirability(rawPrediction, roOptionsDf)
    }
    returnedValue[i] <- switch(outcomeType,
                               "response" = rawPrediction,
                               "individualDesirability" = indDesi,
                               "compDesi" = compDesi)
  }
  return(returnedValue)
}

# Predicted responses (named by response) at the optimizer's predictor settings, from the models of
# .doeOptimizerModels. Each kind of setting is a data frame row, a named vector or list, or a single unnamed value
# when there is only one predictor of that kind. Unused settings are not evaluated (they can be missing arguments).
.doePredictResponses <- function(continuousLevels, continuousPredictors, discretePredictors,
                                 currentDiscreteLevels, models, dependent) {
  settingValue <- function(levels, predictor) if (predictor %in% names(levels)) levels[[predictor]] else levels[[1]]
  continuousPredictors <- unlist(continuousPredictors)
  discretePredictors   <- unlist(discretePredictors)
  settings <- list()
  for (predictor in continuousPredictors[continuousPredictors != ""])
    settings[[predictor]] <- settingValue(continuousLevels, predictor)
  for (predictor in discretePredictors[discretePredictors != ""])
    settings[[predictor]] <- settingValue(currentDiscreteLevels, predictor)

  vapply(dependent, function(response) {
    fit   <- models[["fits"]][[response]]
    coefs <- stats::coef(fit)
    x0    <- .doePredictionModelRow(fit, settings, models[["dataset"]], models[["blocks"]], models[["covariates"]])
    if (attr(x0, "estimable")) drop(x0 %*% coefs[!is.na(coefs)]) else NA_real_
  }, numeric(1))
}

# y-axis limits of an optimization plot panel; settings without a unique prediction (NA) are left out
.doeOptimizationPlotLimits <- function(y, outcomeType) {
  y <- y[!is.na(y)]
  if (outcomeType != "response" || length(y) == 0)
    return(c(0, 1))
  c(min(y) - 0.1 * abs(min(y)), max(y) + 0.1 * abs(max(y)))
}

.doeAliasedOptimizerNote <- function() {
  gettext("Some contrasts of the model are aliased, so the data cannot estimate the response at some settings (e.g., combinations of factor levels without runs). These settings are left out.")
}

.doeOptimizerNotEstimableError <- function() {
  gettext("The response optimizer cannot be used with this model: some contrasts are aliased, and it found no setting at which the data can estimate the response (e.g., when a factor is held constant within blocks, as the predictions are averaged over the blocks).")
}

.doeNotEstimablePredictionNote <- function() {
  gettext("Not shown: the data cannot estimate the response at these settings, because some contrasts of the model are aliased (e.g., a combination of factor levels without runs).")
}

# Whether any of the optimizer models has aliased columns, so that some settings have no unique prediction
.doeOptimizerModelsAliased <- function(models) {
  any(vapply(models[["fits"]], function(fit) !is.null(.doeFitNullSpace(fit)), logical(1)))
}


.predictDesirability <- function(continuousLevels, continuousPredictors, discretePredictors, currentDiscreteLevels, models, dependent,
                                 roOptionsDf) {
  outcome <- .doePredictResponses(continuousLevels, continuousPredictors, discretePredictors, currentDiscreteLevels, models, dependent)
  if (anyNA(outcome)) # no unique prediction at these settings
    return(NA_real_)
  desirabilityOutcome <- .calculateDesirability(outcome, roOptionsDf)
  desirabilityOutcome <- desirabilityOutcome * -1  # because optimization minimizes
  return(desirabilityOutcome)
}

# Desirability function formulas follow Derringer & Suich (1980), Journal of Quality Technology, 12(4), 214-219.
# roOptionsDf holds the resolved bounds and targets of .doeResolveOptimizerSettings, checked by .doeOptimizerSettingsErrors.
.calculateDesirability <- function(outcome, roOptionsDf) {
  outcomeNames <- names(outcome)
  desiVector   <- c()
  goals        <- setNames(roOptionsDf$responseOptimizerGoal, roOptionsDf$variable)
  targets      <- setNames(roOptionsDf$responseOptimizerTarget, roOptionsDf$variable)
  lowerbounds  <- setNames(roOptionsDf$responseOptimizerLowerBound, roOptionsDf$variable)
  upperbounds  <- setNames(roOptionsDf$responseOptimizerUpperBound, roOptionsDf$variable)
  weights      <- setNames(roOptionsDf$responseOptimizerWeight, roOptionsDf$variable)
  importances  <- setNames(roOptionsDf$responseOptimizerImportance, roOptionsDf$variable)

  for (outcomeName in outcomeNames) {
    value  <- unname(outcome[outcomeName])
    lb     <- lowerbounds[[outcomeName]]
    target <- targets[[outcomeName]]
    ub     <- upperbounds[[outcomeName]]
    weight <- weights[[outcomeName]]
    # the target is checked before the ratios, so a bound equal to the target gives no 0/0
    desi <- switch(goals[[outcomeName]],
      "maximize" = if (value < lb) 0 else if (value >= target) 1 else ((value - lb) / (target - lb))^weight,
      "minimize" = if (value > ub) 0 else if (value <= target) 1 else ((ub - value) / (ub - target))^weight,
      "target"   = if (value < lb || value > ub) 0 else if (value == target) 1 else if (value < target)
        ((value - lb) / (target - lb))^weight else ((ub - value) / (ub - target))^weight
    )
    desiVector[outcomeName] <- desi
  }
  if (length(outcome) > 1)
    desiVector <- pmax(0, pmin(1, desiVector)) # if compound desirability is calculated out of multiple desirabilities, these should be bound to 0 and 1, else only bound the compound desirability after optimization

  importances <- importances / sum(importances) # so it sums up to 1
  compoundDesi <- prod(desiVector^importances)
  return(compoundDesi)
}

# Lower bound, target and upper bound of each response as used by the desirability: the entered values, or the
# observed minimum/maximum of the response when they are not set manually. Bounds a goal does not use are NA.
.doeResolveOptimizerSettings <- function(roOptionsDf, dataset, manualBounds, manualTarget) {
  for (i in seq_len(nrow(roOptionsDf))) {
    response <- dataset[[roOptionsDf$variable[i]]]
    goal     <- roOptionsDf$responseOptimizerGoal[i]
    lower    <- if (manualBounds) roOptionsDf$responseOptimizerLowerBound[i] else min(response)
    upper    <- if (manualBounds) roOptionsDf$responseOptimizerUpperBound[i] else max(response)
    target   <- if (manualTarget || goal == "target") roOptionsDf$responseOptimizerTarget[i]
                else if (goal == "maximize") max(response) else min(response)
    roOptionsDf$responseOptimizerLowerBound[i] <- if (goal == "minimize") NA else lower
    roOptionsDf$responseOptimizerTarget[i]     <- target
    roOptionsDf$responseOptimizerUpperBound[i] <- if (goal == "maximize") NA else upper
  }
  return(roOptionsDf)
}

# Settings for which the desirability is undefined (e.g., maximize with the lower bound above the target).
# A target outside bounds from the data is allowed and gets a footnote instead (.doeOptimizerTargetOutsideRangeNotes).
.doeOptimizerSettingsErrors <- function(roOptionsDf, manualBounds) {
  fmt    <- .doeFormatOptimizerValue
  errors <- character(0)
  for (i in seq_len(nrow(roOptionsDf))) {
    response <- roOptionsDf$variable[i]
    lb       <- roOptionsDf$responseOptimizerLowerBound[i]
    target   <- roOptionsDf$responseOptimizerTarget[i]
    ub       <- roOptionsDf$responseOptimizerUpperBound[i]
    errors <- c(errors, switch(roOptionsDf$responseOptimizerGoal[i],
      "maximize" = if (lb > target) gettextf("%1$s: the lower bound (%2$s) is above the target (%3$s).", response, fmt(lb), fmt(target)),
      "minimize" = if (target > ub) gettextf("%1$s: the target (%2$s) is above the upper bound (%3$s).", response, fmt(target), fmt(ub)),
      "target"   = if (lb > ub) gettextf("%1$s: the lower bound (%2$s) is above the upper bound (%3$s).", response, fmt(lb), fmt(ub))
                   else if (manualBounds && (target < lb || target > ub)) gettextf("%1$s: the target (%2$s) is outside the bounds (%3$s to %4$s).", response, fmt(target), fmt(lb), fmt(ub))
    ))
  }
  return(errors)
}

# Goal target with the target outside bounds taken from the data: allowed, as the optimizer still moves towards the target.
.doeOptimizerTargetOutsideRangeNotes <- function(roOptionsDf, manualBounds) {
  if (manualBounds)
    return(character(0))
  outside <- roOptionsDf$responseOptimizerGoal == "target" &
    (roOptionsDf$responseOptimizerTarget < roOptionsDf$responseOptimizerLowerBound |
       roOptionsDf$responseOptimizerTarget > roOptionsDf$responseOptimizerUpperBound)
  gettextf("The target of %1$s is outside its observed range (%2$s to %3$s), so predictions beyond that range have desirability 0. Set the bounds manually to allow them.",
           roOptionsDf$variable[outside],
           .doeFormatOptimizerValue(roOptionsDf$responseOptimizerLowerBound[outside]),
           .doeFormatOptimizerValue(roOptionsDf$responseOptimizerUpperBound[outside]))
}

# Six significant digits, never scientific: distinguishes close bounds (10.004 vs 10) and keeps 100000 readable.
.doeFormatOptimizerValue <- function(x) trimws(formatC(x, digits = 6, format = "fg"))

.addModelHeaderTerms <- function(anovaFit, fit, ssType, covariates = "", data = NULL) {
  rownames(anovaFit) <- gsub(" ", "", row.names(anovaFit), fixed = TRUE)
  rownames(anovaFit) <- unname(sapply(rownames(anovaFit), .gsubIdentityFunction)) # remove identity function around squared terms

  errorDf <- anovaFit$Df[nrow(anovaFit)]
  msError <- anovaFit$`Mean Sq`[nrow(anovaFit)]

  # Model and total SS come from the fit itself. Summing the term rows only works for type I SS;
  # type II/III SS do not add up to the model SS in non-orthogonal designs.
  y       <- model.response(model.frame(fit))
  totalSS <- sum((y - mean(y))^2)
  modelSS <- totalSS - sum(residuals(fit)^2)
  # df from the fit rank: term df can be 0 when coding makes terms aliased, so their sum would be too small
  modelDf <- fit$rank - attr(terms(fit), "intercept")
  modelRow <- .doeAnovaRow(modelSS, modelDf, msError, errorDf, colnames(anovaFit), "Model")

  # calculate total row
  totalRow <- data.frame(df = length(y) - 1, ss = totalSS, ms = NA, f = NA, p = NA)
  colnames(totalRow) <- colnames(anovaFit)
  rownames(totalRow) <- "Total"

  # SS of a group of term rows (e.g., all linear terms), tested jointly
  groupRow <- function(rowIndices, label) {
    group <- .doeAnovaGroupSumOfSquares(anovaFit, rowIndices, fit, ssType)
    .doeAnovaRow(group[["ss"]], group[["df"]], msError, errorDf, colnames(anovaFit), label)
  }

  # calculate block row
  blockTermIndex <- which(rownames(anovaFit) == "Block")
  if (length(blockTermIndex) > 0) {
    anovaFitBlock <- anovaFit[blockTermIndex,]
    rownames(anovaFitBlock) <- sprintf("\u00A0 %s", rownames(anovaFitBlock)) # single indent
  }

  # calculate covariate row
  covariateTermIndices <- which(rownames(anovaFit) %in% covariates)
  if (length(covariateTermIndices) > 0) {
    anovaFitCovariate <- anovaFit[covariateTermIndices,]
    rownames(anovaFitCovariate) <- sprintf("\u00A0 \u00A0 %s", rownames(anovaFitCovariate)) # double indent
    covariateRow <- groupRow(covariateTermIndices, sprintf("\u00A0 %s", "Covariates"))
  }

  # calculate linear row and get all linear terms
  linearTermIndices <- which(!grepl("\\^2|:", rownames(anovaFit[-nrow(anovaFit),])) &
                               rownames(anovaFit[-nrow(anovaFit),]) != "Block" &
                               !rownames(anovaFit[-nrow(anovaFit),]) %in% covariates)  # all terms without squared symbol or colon or residuals or Block or covariates
  anovaFitLinear <- anovaFit[linearTermIndices,]
  rownames(anovaFitLinear) <- sprintf("\u00A0 \u00A0 %s", rownames(anovaFitLinear)) # double indent
  linearRow <- groupRow(linearTermIndices, sprintf("\u00A0 %s", "Linear terms"))

  # calculate squared row and get all squared terms
  squaredTermIndices <- which(grepl("\\^2$", rownames(anovaFit)))
  if (length(squaredTermIndices) > 0) {
    anovaFitSquared <- anovaFit[squaredTermIndices,]
    rownames(anovaFitSquared) <- sprintf("\u00A0 \u00A0 %s", rownames(anovaFitSquared)) # double indent
    squaredRow <- groupRow(squaredTermIndices, sprintf("\u00A0 %s", "Squared terms"))
  }

  # calculate interaction row and get all interaction terms
  interactionTermIndices <- which(grepl(":", rownames(anovaFit)))
  if (length(interactionTermIndices) > 0) {
    anovaFitInteraction <- anovaFit[interactionTermIndices, ]
    rownames(anovaFitInteraction) <- sprintf("\u00A0 \u00A0 %s", rownames(anovaFitInteraction)) # double indent
    interactionRow <- groupRow(interactionTermIndices, sprintf("\u00A0 %s", "Interaction terms"))
  }

  # Model error row, split into lack of fit and pure error when runs are replicated
  errorRow <- anovaFit[nrow(anovaFit),]
  rownames(errorRow) <- "Error"
  errorRow <- rbind(errorRow, .doeAnovaLackOfFitRows(fit, colnames(anovaFit), data))

  newAnovaFit <- modelRow
  if (length(blockTermIndex) > 0)
    newAnovaFit <- rbind(newAnovaFit, anovaFitBlock)
  if (length(covariateTermIndices) > 0)
    newAnovaFit <- rbind(newAnovaFit, covariateRow, anovaFitCovariate)
  newAnovaFit <- rbind(newAnovaFit, linearRow, anovaFitLinear)
  if (length(squaredTermIndices > 0))
    newAnovaFit <- rbind(newAnovaFit, squaredRow, anovaFitSquared)
  if (length(interactionTermIndices > 0))
    newAnovaFit <- rbind(newAnovaFit, interactionRow, anovaFitInteraction)
  newAnovaFit <- rbind(newAnovaFit, errorRow, totalRow)

  return(newAnovaFit)
}

.doeAnovaRow <- function(ss, df, msError, errorDf, colNames, label) {
  ms   <- if (df != 0) ss / df else NA
  fVal <- if (!is.na(ms) && isTRUE(msError > 0)) ms / msError else NA
  pVal <- if (!is.na(fVal)) pf(fVal, df, errorDf, lower.tail = FALSE) else NA
  row  <- data.frame(df = df, ss = ss, ms = ms, f = fVal, p = pVal)
  colnames(row) <- colNames
  rownames(row) <- label
  return(row)
}

# Joint SS and df for a group of ANOVA term rows. Type I: sums of the sequential SS and df (these add up correctly).
# Type II/III: extra SS from dropping all terms of the group at once, mirroring car::Anova for single terms;
# df is the rank difference of the two compared models, so aliased columns are not counted.
.doeAnovaGroupSumOfSquares <- function(anovaFit, rowIndices, fit, ssType) {
  if (length(rowIndices) == 0)
    return(list(ss = 0, df = 0))
  if (ssType == "type1")
    return(list(ss = sum(anovaFit$`Sum Sq`[rowIndices]), df = sum(anovaFit$Df[rowIndices])))

  termLabels      <- attr(terms(fit), "term.labels")
  cleanTermLabels <- unname(sapply(gsub(" ", "", termLabels, fixed = TRUE), .gsubIdentityFunction))
  groupTerms      <- match(rownames(anovaFit)[rowIndices], cleanTermLabels)
  if (anyNA(groupTerms))
    return(list(ss = NA, df = sum(anovaFit$Df[rowIndices])))

  modelMatrix <- model.matrix(fit)
  termAssign  <- attr(modelMatrix, "assign")
  y           <- model.response(model.frame(fit))
  reducedFit <- function(dropTerms) {
    keep    <- !(termAssign %in% dropTerms)
    lmFit   <- lm.fit(modelMatrix[, keep, drop = FALSE], y)
    list(rss = sum(lmFit$residuals^2), rank = lmFit$rank)
  }
  groupTest <- function(smaller, larger)
    list(ss = smaller[["rss"]] - larger[["rss"]], df = larger[["rank"]] - smaller[["rank"]])

  if (ssType == "type3")
    return(groupTest(reducedFit(groupTerms), reducedFit(integer(0))))

  # type II: adjust for all terms except the group and its higher-order relatives (terms containing a group term)
  factors    <- attr(terms(fit), "factors") > 0
  contains   <- function(outer, inner) all(!(factors[, inner] & !factors[, outer]))
  otherTerms <- setdiff(seq_along(termLabels), groupTerms)
  relatives  <- otherTerms[vapply(otherTerms, function(j) any(vapply(groupTerms, function(i) contains(j, i), logical(1))), logical(1))]
  return(groupTest(reducedFit(c(groupTerms, relatives)), reducedFit(relatives)))
}

# Model terms with aliased columns (NA coefficients): "full" when all columns of the term are aliased, "partial" when
# only some are (the term keeps its estimable columns, with fewer df).
.doeAliasedTerms <- function(fit) {
  termLabels <- attr(stats::terms(fit), "term.labels")
  aliased    <- is.na(stats::coef(fit))
  nColumns   <- vapply(seq_along(termLabels), function(j) sum(fit[["assign"]] == j), numeric(1))
  nAliased   <- vapply(seq_along(termLabels), function(j) sum(aliased[fit[["assign"]] == j]), numeric(1))
  list(full    = termLabels[nColumns > 0 & nAliased == nColumns],
       partial = termLabels[nAliased > 0 & nAliased < nColumns])
}

.doeFormatTermLabels <- function(termLabels) {
  if (length(termLabels) == 0)
    return(character(0))
  gsubInteractionSymbol(unname(vapply(gsub(" ", "", termLabels, fixed = TRUE), .gsubIdentityFunction, character(1))))
}

# Discrete factors with a level whose runs are exactly the runs of a level of another of these factors, e.g., center
# points entered as a level of each factor. The columns of these levels are aliased with each other.
.doeFactorsWithSharedLevels <- function(dataset, factorVars) {
  levelRuns <- lapply(stats::setNames(factorVars, factorVars), function(v) {
    x <- as.character(dataset[[v]])
    lapply(unique(x), function(level) which(x == level))
  })
  shared <- vapply(factorVars, function(v) {
    otherRuns <- unlist(levelRuns[setdiff(factorVars, v)], recursive = FALSE)
    any(vapply(levelRuns[[v]], function(runs) any(vapply(otherRuns, identical, logical(1), runs)), logical(1)))
  }, logical(1))
  factorVars[shared]
}

# Alias notes of a fit for the table footnotes: terms removed from the model (all their columns aliased), terms of
# the fit with all or some columns aliased (possible in coded units only, or after removal), and discrete factors of
# the model whose levels share runs.
.doeAliasNotes <- function(fit, removedTerms, dataset, multiLevelVars) {
  aliased   <- .doeAliasedTerms(fit)
  modelVars <- all.vars(stats::delete.response(stats::terms(fit)))
  list(
    removedTerms          = .doeFormatTermLabels(removedTerms),
    notEstimatedTerms     = .doeFormatTermLabels(aliased[["full"]]),
    partiallyAliasedTerms = .doeFormatTermLabels(aliased[["partial"]]),
    sharedLevelFactors    = if (length(aliased[["partial"]]) > 0) .doeFactorsWithSharedLevels(dataset, intersect(multiLevelVars, modelVars))
  )
}

# S, R², adjusted and predictive R² of a fit. A saturated fit has no error, so only R² (1) is defined.
.doeModelSummaryValues <- function(fit, fitSummary) {
  if (fit[["df.residual"]] == 0)
    return(list(s = NA, rsq = 1, adjrsq = NA, predrsq = NA))
  list(s       = fitSummary[["sigma"]],
       rsq     = fitSummary[["r.squared"]],
       adjrsq  = max(0, fitSummary[["adj.r.squared"]]), # can be negative
       predrsq = .pred_r_squared(fit))
}

# Model matrix columns scaled to length 1 (all-zero columns are kept as they are), so that rank and null space
# decisions do not depend on the units of the predictors
.doeColumnScales <- function(x) {
  norms <- sqrt(colSums(x^2))
  replace(norms, norms == 0, 1)
}

# Null space of the model matrix of a fit with aliased (NA) coefficients, on columns scaled by .doeColumnScales: an
# orthonormal basis and the column scales; NULL without aliasing. A linear combination of the coefficients, such as a
# prediction, has a unique estimate (does not depend on which aliased columns lm() left out) only when it lies in the
# row space of the model matrix, i.e. when (scaled) it is orthogonal to this null space.
.doeNullSpace <- function(fit) {
  modelMatrix <- stats::model.matrix(fit)
  if (fit[["rank"]] == ncol(modelMatrix))
    return(NULL)
  scales <- .doeColumnScales(modelMatrix)
  basis  <- svd(sweep(modelMatrix, 2, scales, "/"), nu = 0, nv = ncol(modelMatrix))[["v"]][, -seq_len(fit[["rank"]]), drop = FALSE]
  list(basis = basis, scales = scales)
}

# Null space of a fit, stored on the fit by .doeAnalysisMakeState so that the optimizer does not recompute it
.doeFitNullSpace <- function(fit) {
  if ("nullSpace" %in% names(fit)) fit[["nullSpace"]][["value"]] else .doeNullSpace(fit)
}

# Whether each row of x (rows of the model matrix of the fit, over all its columns including the aliased ones) has a
# unique estimate: the part of the scaled row in the null space is negligible relative to the row
.doeEstimable <- function(fit, x) {
  nullSpace <- .doeFitNullSpace(fit)
  if (is.null(nullSpace))
    return(rep(TRUE, nrow(x)))
  scaled <- sweep(x, 2, nullSpace[["scales"]], "/")
  sqrt(rowSums((scaled %*% nullSpace[["basis"]])^2)) <= 1e-7 * sqrt(rowSums(scaled^2))
}

# Whether two model matrices (same rows) span the same column space, i.e. are the same model whatever the response
.doeSameColumnSpace <- function(x1, x2) {
  rank <- function(x) qr(sweep(x, 2, .doeColumnScales(x), "/"), tol = 1e-7)[["rank"]]
  rank1 <- rank(x1)
  rank1 == rank(x2) && rank(cbind(x1, x2)) == rank1
}

# Whether each coefficient of the fit has a unique estimate; FALSE for the aliased (NA) coefficients
.doeEstimableCoefficients <- function(fit) {
  .doeEstimable(fit, diag(length(stats::coef(fit))))
}

# Whether the contour or surface plot of a pair of variables has a unique prediction everywhere. Like rsm::contour.lm,
# the other numeric variables are held at their mean and the predictions are averaged over the levels of the factors.
.doeContourEstimable <- function(fit, dataset, variablePair, nPoints = 5) {
  if (is.null(.doeFitNullSpace(fit)))
    return(TRUE)
  modelTerms <- stats::delete.response(stats::terms(fit))
  slice      <- lapply(stats::setNames(nm = all.vars(modelTerms)), function(v) {
    x <- dataset[[v]]
    if (v %in% variablePair) seq(min(x), max(x), length.out = nPoints) else if (is.factor(x)) factor(levels(x), levels = levels(x)) else mean(x)
  })
  newdata <- do.call(expand.grid, slice)
  x       <- stats::model.matrix(modelTerms, stats::model.frame(modelTerms, newdata, xlev = fit[["xlevels"]]),
                                 contrasts.arg = fit[["contrasts"]])
  x       <- rowsum(x, interaction(newdata[variablePair], drop = TRUE)) # sum over the factor levels (estimable as the average)
  all(.doeEstimable(fit, x))
}

.gsubIdentityFunction <- function(term) {
  splitTerm <- unlist(strsplit(term, "")) # split into individual letters
  if (all(splitTerm[c(1,2, length(splitTerm))] == c("I", "(", ")"))) {
    cleanTerm <- paste0(splitTerm[-c(1,2, length(splitTerm))], collapse = "") # remove the first two and the last element
    return(cleanTerm)
  } else {
    return(term)
  }
}

# Factorial effect: change in mean response from the -1 to the +1 setting of a term, i.e., 2 * coefficient.
# Holds for main effects and interactions of any order, as long as the predictors are on a -1/+1 scale
# (coded numeric predictors or 2-level factors with sum-to-zero contrasts).
# NA for the intercept, squared terms, terms with a factor of more than 2 levels, and terms with excluded variables (blocks, covariates).
.doeCoefficientEffects <- function(regressionFit, excludeVars = character(0)) {
  termFactors <- attr(stats::terms(regressionFit), "factors") # variables x terms
  if (length(termFactors) == 0) # intercept-only model
    return(rep(NA_real_, length(stats::coef(regressionFit))))
  modelFrame  <- regressionFit[["model"]]
  noEffectVar <- vapply(rownames(termFactors), function(v) {
    x <- modelFrame[[v]]
    (is.factor(x) && nlevels(x) > 2) || grepl("^I\\(.*\\^2\\)$", v) || v %in% excludeVars
  }, logical(1))
  noEffectTerm <- colSums(termFactors[noEffectVar, , drop = FALSE]) > 0
  termIndex    <- regressionFit[["assign"]] # maps each coefficient to its term, 0 = intercept

  effects <- 2 * unname(stats::coef(regressionFit))
  effects[termIndex == 0] <- NA
  effects[termIndex > 0][noEffectTerm[termIndex[termIndex > 0]]] <- NA
  return(effects)
}

# Standard error of each effect (2 * standard error of the coefficient), aligned with .doeCoefficientEffects().
# NA where no effect is defined, and for all terms when the model has no error degrees of freedom.
.doeEffectStandardErrors <- function(regressionFit, effects) {
  coefs     <- stats::coef(regressionFit)
  se        <- rep(NA_real_, length(coefs))
  if (length(coefs) != length(effects)) # effects of mismatched terms are not shown either
    return(rep(NA_real_, length(effects)))
  if (regressionFit[["df.residual"]] > 0)
    se[!is.na(coefs)] <- summary(regressionFit)[["coefficients"]][, "Std. Error"]
  se        <- 2 * se
  se[is.na(effects)] <- NA
  return(se)
}

# Lower and upper confidence limits for estimates with t-based standard errors; NA without error degrees of freedom.
.doeConfidenceLimits <- function(estimates, se, df, level) {
  tCrit <- if (isTRUE(df > 0)) stats::qt((1 + level) / 2, df) else NA_real_
  list(lower = estimates - tCrit * se, upper = estimates + tCrit * se)
}

# Lack of fit and pure error rows that split the error of an ANOVA table. Pure error is the variation between runs
# with identical settings of all variables in the model (replicates, e.g. center points). NULL when there is no
# pure error or no lack of fit to test (no replicates, or one model coefficient per distinct setting).
# The runs are grouped on the variables in `data` (the data the model was fitted on), not on the model frame columns:
# with I(A^2) but not A in the model, the runs at A = -1 and A = +1 are distinct settings, not replicates.
# Without `data`, the runs are grouped on the model frame columns, which are the variables when no term transforms them.
.doeAnovaLackOfFitRows <- function(fit, colNames, data = NULL) {
  modelFrame  <- stats::model.frame(fit)
  y           <- stats::model.response(modelFrame)
  runData     <- if (is.null(data)) {
    modelFrame[-1]
  } else {
    data[rownames(modelFrame), all.vars(stats::delete.response(stats::terms(fit))), drop = FALSE] # rows the model was fitted on
  }
  if (ncol(runData) == 0) # intercept-only model
    return(NULL)
  settings    <- do.call(paste, c(lapply(runData, as.character), sep = "\r"))
  pureErrorDf <- length(y) - length(unique(settings))
  lackOfFitDf <- fit[["df.residual"]] - pureErrorDf
  if (pureErrorDf == 0 || lackOfFitDf <= 0)
    return(NULL)

  pureErrorSS <- sum((y - stats::ave(y, settings))^2)
  lackOfFitSS <- max(0, sum(stats::residuals(fit)^2) - pureErrorSS) # max(): guard against rounding below zero
  pureErrorMS <- pureErrorSS / pureErrorDf

  lackOfFitRow <- .doeAnovaRow(lackOfFitSS, lackOfFitDf, pureErrorMS, pureErrorDf, colNames, sprintf("\u00A0 %s", gettext("Lack of fit")))
  pureErrorRow <- data.frame(df = pureErrorDf, ss = pureErrorSS, ms = pureErrorMS, f = NA, p = NA)
  colnames(pureErrorRow) <- colNames
  rownames(pureErrorRow) <- sprintf("\u00A0 %s", gettext("Pure error"))
  return(rbind(lackOfFitRow, pureErrorRow))
}

.createHighestOrderInteractionFormula <- function(dependentVariable, independentVariables, interactionOrder) {
  # Create a formula string with main effects
  formulaStr <- paste(independentVariables, collapse = " + ")

  # Add interaction terms up to the specified order
  if (interactionOrder > 1 & length(independentVariables) > 1) {
    for (i in 2:interactionOrder) {
      interactions <- combn(independentVariables, i, simplify = FALSE)
      interaction_terms <- sapply(interactions, function(x) paste(x, collapse = ":"))
      formulaStr <- paste(formulaStr, "+", paste(interaction_terms, collapse = " + "))
    }
  }

  # Construct and return the formula
  return(paste(dependentVariable, "~", formulaStr))
}

.doeAnalysisSummaryTable <- function(jaspResults, dependent, options, ready, coded) {
  for (dep in dependent) {
    if (!is.null(jaspResults[[dep]][["tableSummary"]])) {
      return()
    }
    tb <- createJaspTable(gettext("Model Summary"))
    tb$addColumnInfo(name = "s", title = "S", type = "number")
    tb$addColumnInfo(name = "rsq", title = "R\u00B2", type = "number")
    tb$addColumnInfo(name = "adjrsq", title = gettextf("Adjusted %s", "R\u00B2"), type = "number")
    tb$addColumnInfo(name = "predrsq", title = gettextf("Predictive %s", "R\u00B2"), type = "number")
    tb$dependOn(options = .doeAnalysisBaseDependencies())
    tb$position <- 1
    jaspResults[[dep]][["tableSummary"]] <- tb
    if (!ready || is.null(jaspResults[[dep]][["doeResult"]]) || jaspResults[[dep]]$getError()) {
      return()
    }
    result <- if (coded) jaspResults[[dep]][["doeResultCoded"]]$object[["regression"]] else jaspResults[[dep]][["doeResult"]]$object[["regression"]]
    row <- data.frame(
      s = result[["s"]], rsq = result[["rsq"]], adjrsq = result[["adjrsq"]], predrsq = result[["predrsq"]]
    )
    tb$addRows(row)
    .doeAddAliasFootnotes(tb, result[["aliasNotes"]])
  }
}

.doeAddAliasFootnotes <- function(table, aliasNotes) {
  if (length(aliasNotes[["removedTerms"]]) > 0)
    table$addFootnote(gettextf("The following aliased terms were removed: %s.", paste(aliasNotes[["removedTerms"]], collapse = ", ")))
  if (length(aliasNotes[["notEstimatedTerms"]]) > 0)
    table$addFootnote(gettextf("The following terms are aliased and were not estimated: %s.", paste(aliasNotes[["notEstimatedTerms"]], collapse = ", ")))
  if (length(aliasNotes[["partiallyAliasedTerms"]]) > 0)
    table$addFootnote(gettextf("Some contrasts of the following terms are aliased and were not estimated: %s.",
                               paste(aliasNotes[["partiallyAliasedTerms"]], collapse = ", ")))
  if (length(aliasNotes[["sharedLevelFactors"]]) > 0)
    table$addFootnote(gettextf("Each of these factors has a level that only occurs in the same runs as a level of another of these factors (e.g., center points entered as a factor level): %s. To estimate one effect per factor, enter these factors as continuous predictors.",
                               paste(aliasNotes[["sharedLevelFactors"]], collapse = ", ")))
}

.doeAnalysisAnovaTable <- function(jaspResults, dependent, options, ready, coded) {
  for (dep in dependent) {
    if (!is.null(jaspResults[["tableAnova"]])) {
      return()
    }

    tb <- createJaspTable(gettext("Analysis of Variance"))
    tb$addColumnInfo(name = "terms", title = gettext("Source"), type = "string")
    tb$addColumnInfo(name = "adjss", title = gettext("Sum of squares"), type = "number")
    tb$addColumnInfo(name = "df",    title = gettext("df"), type = "integer")
    tb$addColumnInfo(name = "adjms", title = gettext("Mean square"), type = "number")
    tb$addColumnInfo(name = "fval",  title = "F", type = "number")
    tb$addColumnInfo(name = "pval",  title = "p", type = "pvalue")
    tb$dependOn(options = .doeAnalysisBaseDependencies())
    tb$position <- 2
    jaspResults[[dep]][["tableAnova"]] <- tb
    if (!ready || is.null(jaspResults[[dep]][["doeResult"]]) || jaspResults[[dep]]$getError()) {
      return()
    }
    result <- if (coded) jaspResults[[dep]][["doeResultCoded"]]$object[["anova"]] else jaspResults[[dep]][["doeResult"]]$object[["anova"]]
    if (!is.null(result[["error"]])) {
      tb$setError(result[["error"]])
      next
    }
    rows <- data.frame(
      terms = result[["terms"]], adjss = result[["adjss"]], df = result[["df"]],
      adjms = result[["adjms"]], fval = result[["F"]], pval = result[["p"]]
    )
    tb$addRows(rows)

    ssFootnote <- if (result[["saturated"]]) {
      gettext("Type I (sequential) sums of squares, whatever type is selected: the model has no degrees of freedom for error, so there are no F-tests.")
    } else {
      switch (result[["ssType"]],
              "type1" = gettext("Type I sums of squares."),
              "type2" = gettext("Type II sums of squares."),
              "type3" = gettext("Type III sums of squares."))
    }
    tb$addFootnote(ssFootnote)
    if (!is.null(result[["codingNote"]]))
      tb$addFootnote(result[["codingNote"]])
  }
}

.doeAnalysisCoefficientsTable <- function(jaspResults, dependent, options, ready, coded) {
  for (dep in dependent) {
    if (!is.null(jaspResults[[dep]][["tableCoefficients"]])) {
      return()
    }
    codedString <- ifelse(options[["codeFactors"]], gettext("Coded"), gettext("Uncoded"))
    tb <- createJaspTable(gettextf("%s Coefficients", codedString))
    if (options[["tableAlias"]])
      tb$addColumnInfo(name = "alias", title = gettext("Alias"), type = "string")
    tb$addColumnInfo(name = "terms", title = gettext("Term"), type = "string")
    showCi  <- options[["tableCoefficientsCi"]]
    ciLevel <- options[["tableCoefficientsCiLevel"]]
    tb$addColumnInfo(name = "effects", title = gettext("Effect"), type = "number")
    if (showCi) {
      effectCiTitle <- gettextf("%s%% CI for effect", 100 * ciLevel)
      tb$addColumnInfo(name = "effectsLower", title = gettext("Lower"), type = "number", overtitle = effectCiTitle)
      tb$addColumnInfo(name = "effectsUpper", title = gettext("Upper"), type = "number", overtitle = effectCiTitle)
    }
    tb$addColumnInfo(name = "coef", title = gettext("Coefficient"), type = "number")
    if (showCi) {
      coefCiTitle <- gettextf("%s%% CI for coefficient", 100 * ciLevel)
      tb$addColumnInfo(name = "coefLower", title = gettext("Lower"), type = "number", overtitle = coefCiTitle)
      tb$addColumnInfo(name = "coefUpper", title = gettext("Upper"), type = "number", overtitle = coefCiTitle)
    }
    tb$addColumnInfo(name = "se", title = gettext("Standard error"), type = "number")
    tb$addColumnInfo(name = "tval", title = "t", type = "number")
    tb$addColumnInfo(name = "pval", title = "p", type = "pvalue")
    tb$addColumnInfo(name = "vif", title = "VIF", type = "number")
    tb$dependOn(options = c("tableEquation", "tableAlias", "tableCoefficientsCi", "tableCoefficientsCiLevel", .doeAnalysisBaseDependencies()))
    tb$position <- 3
    jaspResults[[dep]][["tableCoefficients"]] <- tb
    if (!ready || is.null(jaspResults[[dep]][["doeResult"]]) || jaspResults[[dep]]$getError()) {
      return()
    }
    resultRegression <- if (coded) jaspResults[[dep]][["doeResultCoded"]]$object[["regression"]] else jaspResults[[dep]][["doeResult"]]$object[["regression"]]
    result <- resultRegression[["coefficients"]]
    effectNote <- if (coded) {
      gettext("Effect = 2 \u00D7 coefficient: the change in the response when a term goes from its \u22121 to its +1 setting.")
    } else {
      gettext("Effect = 2 \u00D7 coefficient in coded (\u22121/+1) units, not 2 \u00D7 the coefficient shown: the change in the response when a term goes from its \u22121 to its +1 setting. For discrete predictors the sign follows the level shown in the Discrete Predictor Levels table.")
    }
    tb$addFootnote(paste(effectNote, gettext("Not shown for squared terms, discrete predictors with more than two levels, blocks, and covariates.")))
    levelsWithoutRuns <- resultRegression[["levelsWithoutRuns"]]
    if (length(levelsWithoutRuns) > 0) {
      levelsText <- paste0(names(levelsWithoutRuns), " (", vapply(levelsWithoutRuns, paste, character(1), collapse = ", "), ")", collapse = "; ")
      tb$addFootnote(gettextf("Levels without runs are left out of the model: %s.", levelsText))
    }
    rows <- data.frame(
      terms = result[["terms"]], effects = result[["effects"]], coef = result[["est"]],
      se = result[["se"]], tval = result[["t"]], pval = result[["p"]], vif = result[["vif"]]
    )
    if (options[["tableAlias"]])
      rows["alias"] <- result[["termsAliased"]]
    if (showCi) {
      effectLimits <- .doeConfidenceLimits(result[["effects"]], result[["effectsSe"]], result[["effectsDf"]], ciLevel)
      coefLimits   <- .doeConfidenceLimits(result[["est"]], result[["se"]], resultRegression[["object"]][["df.residual"]], ciLevel)
      rows[["effectsLower"]] <- effectLimits[["lower"]]
      rows[["effectsUpper"]] <- effectLimits[["upper"]]
      rows[["coefLower"]]    <- coefLimits[["lower"]]
      rows[["coefUpper"]]    <- coefLimits[["upper"]]
      if (resultRegression[["saturated"]])
        tb$addFootnote(gettext("Confidence intervals cannot be calculated: the model has no degrees of freedom for error."))
    }
    tb$addRows(rows)
    if (!all(result[["unique"]]))
      tb$addFootnote(.doeNonUniqueCoefficientsNote())
    if (isTRUE(result[["effectsOtherModel"]])) # only set for the uncoded table
      tb$addFootnote(gettext("Effects are not shown: coding the continuous predictors changes this model (e.g., an interaction or squared term without its main effects)."))
    if (anyNA(stats::coef(resultRegression[["object"]])))
      tb$addFootnote(gettext("VIF is not available when some coefficients are aliased."))

    # coded: only discrete factors with more than 2 levels have level-specific terms that need a legend
    result2 <- if (coded) jaspResults[[dep]][["doeResultCoded"]]$object[["regression"]][["factorLevels"]] else jaspResults[[dep]][["doeResult"]]$object[["regression"]][["factorLevels"]]
    if ((length(options[["fixedFactorsFactorial"]]) > 0 | length(options[["fixedFactorsResponseSurface"]])) && (!coded || length(result2[["factorNames"]]) > 0)) {
      tb2 <- createJaspTable(gettext("Discrete Predictor Levels"))
      tb2$addColumnInfo(name = "factorName", title = gettext("Name"), type = "string")
      tb2$addColumnInfo(name = "factorLevel", title = gettext("Level"), type = "string")
      tb$dependOn(options = c("codeFactors", "codeFactorsManualTable", "codeFactorsMethod", "tableEquation", "tableAlias", .doeAnalysisBaseDependencies()))
      tb2$position <- 4
      jaspResults[[dep]][["tableCoefficientsLegend"]] <- tb2
      factorName <- if (options[["tableAlias"]]) result2[["factorNamesAliased"]] else result2[["factorNames"]]
      if (length(factorName) > 0) {
        rows2 <- data.frame(
          factorName = factorName,
          factorLevel = result2[["levels"]]
        )

        tb2$addRows(rows2)
      }
    }
  }
}

.doeAnalysisEquationTable <- function(jaspResults, dependent, options, ready, coded) {
  for (dep in dependent) {
    if (!is.null(jaspResults[[dep]][["tableEquation"]]) || !options[["tableEquation"]]) {
      return()
    }
    codedString <- ifelse(options[["codeFactors"]], gettext("Coded"), gettext("Uncoded"))
    tb <- createJaspTable(gettextf("Regression equation in %s Units", codedString))
    tb$addColumnInfo(name = "formula", title = "", type = "string")
    tb$dependOn(options = .doeAnalysisBaseDependencies())
    tb$position <- 4
    jaspResults[[dep]][["tableEquation"]] <- tb
    if (!ready || is.null(jaspResults[[dep]][["doeResult"]]) || jaspResults[[dep]]$getError()) {
      return()
    }
    result <- if (coded) jaspResults[[dep]][["doeResultCoded"]]$object[["regression"]] else jaspResults[[dep]][["doeResult"]]$object[["regression"]]
    # with coefficients that are not unique, any equation would be an arbitrary one of several (same fitted values at the
    # runs, but different predictions at settings without runs), so none is shown
    if (!all(result[["coefficients"]][["unique"]])) {
      tb$addFootnote(gettext("Not shown: some contrasts of the model are aliased, so the coefficients of the equation are not unique (see the coefficients table)."))
      next
    }
    row <- data.frame(formula = result[["filledFormula"]])
    tb$addRows(row)
  }
}

.doeNonUniqueCoefficientsNote <- function() {
  gettext("Some contrasts of the model are aliased. Values left empty depend on which aliased contrasts are left out (e.g., on the order of the factor levels), so they are not shown.")
}

.doeNoUniqueEffectsError <- function() {
  gettext("No effects to plot: some contrasts of the model are aliased, so no effect has a unique estimate.")
}

# Error of an effect plot without effects (after leaving out blocks and covariates, which can have unique estimates):
# because of aliased contrasts, or because the model has no effect terms
.doeNoEffectsError <- function(result) {
  if (all(result[["coefficients"]][["unique"]]))
    return(gettext("There are no effects to plot."))
  return(.doeNoUniqueEffectsError())
}

.doeAnalysisStepwiseTable <- function(jaspResults, dependent, options, ready, coded, stepwiseMethod) {
  for (dep in dependent) {

    if (stepwiseMethod == "enter" || !is.null(jaspResults[[dep]][["stepwiseTable"]])) {
      return()
    }

    tb <- createJaspTable(gettext("Stepwise results"))

    tb$addColumnInfo(name = "stepNr",  title = gettext("Step"), type = "string", combine = TRUE)
    tb$addColumnInfo(name = "AIC",     title = "AIC", type = "number")
    tb$addColumnInfo(name = "formula", title =  gettext("Formula"), type = "string")

    tb$dependOn(options = .doeAnalysisBaseDependencies())
    tb$position <- 4

    jaspResults[[dep]][["stepwiseTable"]] <- tb

    if (!ready || is.null(jaspResults[[dep]][["doeResult"]]) || jaspResults[[dep]]$getError()) {
      return()
    }

    result <- jaspResults[[dep]][["doeResult"]]$object[["stepwiseResult"]]

    tb$setData(result[["stepwiseResult"]])

    excludedVars <- result[["excludedVars"]]
    if (length(excludedVars) > 0) {
      message <- sprintf(ngettext(length(excludedVars),
                                  "The following covariate was considered but not included: %1$s.",
                                  "The following covariates were considered but not included: %1$s."),
                         paste(excludedVars, collapse=", "))
      tb$addFootnote(message)
    }

    forcedMainEffects <- result[["forcedMainEffects"]]
    if (length(forcedMainEffects) > 0) {
      message <- sprintf(ngettext(length(forcedMainEffects),
                                  "The main effect %1$s was retained to respect marginality (its squared term is in the model).",
                                  "The main effects %1$s were retained to respect marginality (their squared terms are in the model)."),
                         paste(forcedMainEffects, collapse=", "))
      tb$addFootnote(message)
    }

  }
}


# Terms and values for the Pareto and normal effect plots: t-values, or the effects when the model is saturated
# (no error df), without the intercept, blocks, covariates and coefficients that are not unique (aliased contrasts)
.doeEffectPlotData <- function(result, options, blocks, covariates) {
  coefs     <- result[["coefficients"]]
  saturated <- result[["saturated"]]
  plotData  <- data.frame(value  = if (saturated) coefs[["effects"]] else result[["objectSummary"]]$coefficients[, "t value"],
                          terms  = coefs[["terms"]],
                          fac    = if (options[["tableAlias"]]) coefs[["termsAliased"]] else coefs[["terms"]],
                          pValue = coefs[["p"]])
  plotData <- plotData[c(FALSE, coefs[["unique"]][-1]), , drop = FALSE]
  if (length(blocks) > 0 && !identical(blocks, ""))
    plotData <- plotData[!grepl(blocks, plotData$terms), , drop = FALSE]
  if (length(covariates) > 0 && !identical(covariates, ""))
    plotData <- plotData[!plotData$terms %in% unlist(covariates), , drop = FALSE]
  return(plotData)
}

# Lenth's (1989) pseudo standard error (PSE) and margin of error (ME) of the effects of an unreplicated two-level design,
# with m / 3 degrees of freedom for m effects. NULL when it cannot be computed: no effects, or PSE = 0 (a PSE up to
# zeroTolerance, the rounding error of the effects, counts as 0).
.doeLenthMarginOfError <- function(effects, zeroTolerance = 0, alpha = 0.05) {
  absEffects <- abs(effects)
  if (length(absEffects) == 0)
    return(NULL)
  s0  <- 1.5 * stats::median(absEffects)
  pse <- 1.5 * stats::median(absEffects[absEffects < 2.5 * s0])
  if (!is.finite(pse) || pse <= zeroTolerance)
    return(NULL)
  df <- length(absEffects) / 3
  return(list(pse = pse, df = df, me = stats::qt(1 - alpha / 2, df) * pse))
}

# Lenth's method for the effect plots of a saturated model. Like Minitab, only for two-level factorial terms (Minitab
# draws no effect plots for saturated response surface or general full factorial models). Returns the PSE and ME, or an
# error message.
.doeLenthEffects <- function(result, plotData) {
  if (isTRUE(result[["coefficients"]][["effectsOtherModel"]])) # only set for the uncoded fit
    return(gettext("The model is saturated (no error degrees of freedom) and its effects are not available in uncoded units, because coding changes the model."))
  if (anyNA(plotData$value))
    return(gettext("The model is saturated (no error degrees of freedom). Lenth's method for assessing its effects applies to two-level factorial terms only, but the model has squared terms or terms of discrete factors with more than two levels."))
  # Like Minitab, no check that the effects are uncorrelated with equal variance (orthogonal design), which Lenth's
  # method assumes; e.g. a two-level design with missing runs still gets Lenth's method.
  # rounding error of the effects is of the order of n * eps * max|y|; a PSE below 100 times that counts as 0
  response      <- result[["object"]][["model"]][[1]]
  zeroTolerance <- 100 * length(response) * .Machine$double.eps * max(abs(response))
  lenth <- .doeLenthMarginOfError(plotData$value, zeroTolerance)
  if (is.null(lenth))
    return(gettext("The model is saturated (no error degrees of freedom) and Lenth's pseudo standard error of its effects is zero, so the effects cannot be assessed."))
  return(lenth)
}

.doeAnalysisPlotPareto <- function(jaspResults, dependent, options, blocks, covariates, ready) {
  for (dep in dependent) {
    if (!is.null(jaspResults[[dep]][["plotPareto"]]) || !options[["plotPareto"]]) {
      return()
    }
    plot <- createJaspPlot(title = gettext("Pareto Chart of Standardized Effects"), width = 600, height = 400)
    plot$dependOn(options = c("plotPareto", "tableAlias", .doeAnalysisBaseDependencies()))
    plot$position <- 6
    jaspResults[[dep]][["plotPareto"]] <- plot
    if (!ready || is.null(jaspResults[[dep]][["doeResult"]]) || jaspResults[[dep]]$getError()) {
      return()
    }
    result <- if (options[["codeFactors"]]) jaspResults[[dep]][["doeResultCoded"]]$object[["regression"]] else jaspResults[[dep]][["doeResult"]]$object[["regression"]]
    if (result[["saturated"]])
      plot$title <- gettext("Pareto Chart of Effects")
    plotData <- .doeEffectPlotData(result, options, blocks, covariates)
    if (nrow(plotData) == 0) {
      plot$setError(.doeNoEffectsError(result))
      next
    }

    if (result[["saturated"]]) {
      # no error df for t-values: absolute effects against Lenth's margin of error
      lenth <- .doeLenthEffects(result, plotData)
      if (is.character(lenth)) {
        plot$setError(lenth)
        next
      }
      crit       <- lenth[["me"]]
      critLabel  <- gettextf("ME = %s", trimws(formatC(crit, digits = 4, format = "fg"))) # effects are in response units, so significant digits
      valueLabel <- gettext("Effect")
    } else {
      crit       <- abs(qt(0.025, result[["objectSummary"]]$df[2]))
      critLabel  <- sprintf("t = %.2f", crit)
      valueLabel <- gettext("Standardized Effect")
    }

    t <- abs(plotData[["value"]])
    fac_t <- cbind.data.frame(fac = plotData[["fac"]], t)
    fac_t <- cbind(fac_t[order(fac_t$t), ], y = seq_len(length(t)))
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, t, crit))
    critLabelDf <- data.frame(x = 0, y = crit, label = critLabel)
    p <- ggplot2::ggplot(data = fac_t, mapping = ggplot2::aes(y = t, x = y)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_hline(yintercept = crit, linetype = "dashed", color = "red") +
      ggplot2::geom_label(data = critLabelDf, mapping = ggplot2::aes(x = x, y = y, label = label), col = "red", size = 5) +
      ggplot2::scale_x_continuous(name = gettext("Term"), breaks = fac_t$y, labels = fac_t$fac) +
      ggplot2::scale_y_continuous(name = valueLabel, breaks = xBreaks, limits = range(xBreaks)) +
      ggplot2::coord_flip() +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
    plot$plotObject <- p
  }
}

.doeAnalysisPlotEffectNormalDistribution <- function(jaspResults, dependent, options, blocks, covariates, ready) {
  for (dep in dependent) {
    if (!is.null(jaspResults[[dep]][["normalEffectsPlot"]]) || !options[["normalEffectsPlot"]]) {
      return()
    }
    plot <- createJaspPlot(title = gettext("Normal Plot of Standardized Effects"), width = 600, height = 600)
    plot$dependOn(options = c("normalEffectsPlot", "tableAlias", .doeAnalysisBaseDependencies()))
    plot$position <- 11
    jaspResults[[dep]][["normalEffectsPlot"]] <- plot
    if (!ready || is.null(jaspResults[[dep]][["doeResult"]]) || jaspResults[[dep]]$getError()) {
      return()
    }
    result <- if (options[["codeFactors"]]) jaspResults[[dep]][["doeResultCoded"]]$object[["regression"]] else jaspResults[[dep]][["doeResult"]]$object[["regression"]]
    if (result[["saturated"]])
      plot$title <- gettext("Normal Plot of Effects")
    tDf <- .doeEffectPlotData(result, options, blocks, covariates)
    if (nrow(tDf) == 0) {
      plot$setError(.doeNoEffectsError(result))
      next
    }

    if (result[["saturated"]]) {
      # no error df for t-values (Daniel plot): effects against N(0, PSE), significant when |effect| > Lenth's ME
      lenth <- .doeLenthEffects(result, tDf)
      if (is.character(lenth)) {
        plot$setError(lenth)
        next
      }
      referenceSd <- lenth[["pse"]]
      tDf$significant <- ifelse(abs(tDf$value) > lenth[["me"]], "S", "N")
      valueLabel <- gettext("Effect")
    } else {
      referenceSd <- 1
      tDf$significant <- ifelse(tDf$pValue < 0.05, "S", "N")
      valueLabel <- gettext("Standardized Effect")
    }

    # median rank order function
    n <- nrow(tDf)
    tDf$percentile <- (rank(tDf$value) - 0.3) / (n + 0.4)

    tDf$labelYPos <- stats::pnorm(stats::qnorm(tDf$percentile) + 0.2) # offset the label by a small amount (0.2) so it is displayed above the point
    yLabels <- c(0.1, 1, 5, seq(10, 90, 10), 95, 99, 99.9)
    yBreaks <- yLabels/100
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(tDf$value, -3 * referenceSd, 3 * referenceSd))
    xLimits <- range(c(tDf$value, xBreaks))

    # Create the ggplot with probit transformation
    p <- ggplot2::ggplot(data = tDf, mapping = ggplot2::aes(x = value, y = percentile)) +
      ggplot2::stat_function(fun = stats::pnorm, args = list(sd = referenceSd), linewidth = 1) +  # Reference line
      jaspGraphs::geom_point(mapping = ggplot2::aes(fill = significant), size = 4) +
      ggplot2::scale_y_continuous(trans = 'probit', labels = yLabels, breaks = yBreaks, name = gettext("Percent"), limits = c(0.0001, 0.9999)) +
      ggplot2::scale_x_continuous(name = valueLabel, breaks = xBreaks, limits = xLimits) +
      ggplot2::scale_fill_manual(values = c("S" = "darkred", "N" = "grey"), name = NULL, labels = c(gettext("Significant"), gettext("Not Significant")), breaks = c("S", "N")) +
      ggplot2::geom_label(mapping = ggplot2::aes(label = fac, y = labelYPos), size = 4) +
      jaspGraphs::themeJaspRaw() +
      jaspGraphs::geom_rangeframe() +
      ggplot2::theme(legend.position = "right")

    plot$plotObject <- p
  }
}

.doeAnalysisPlotQQResiduals <- function(jaspResults, dependent, options, ready) {
  for (dep in dependent) {
    if (!is.null(jaspResults[[dep]][["plotNorm"]]) || !options[["plotNorm"]]) {
      return()
    }
    plot <- createJaspPlot(title = gettext("Normal Probability Plot of Residuals"), width = 500, height = 500)
    plot$dependOn(options = c("plotNorm", .doeAnalysisBaseDependencies()))
    plot$position <- 7
    jaspResults[[dep]][["plotNorm"]] <- plot
    if (!ready || is.null(jaspResults[[dep]][["doeResult"]]) || jaspResults[[dep]]$getError()) {
      return()
    }
    result <- jaspResults[[dep]][["doeResult"]]$object[["regression"]]
    plot$plotObject <- jaspGraphs::plotQQnorm(resid(result[["object"]]), abline = TRUE) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1), name = gettext("Theoretical quantiles")) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1), name = gettext("Observed quantiles"))
  }
}

.doeAnalysisPlotHistResiduals <- function(jaspResults, dependent, options, ready) {
  for (dep in dependent) {
    if (!is.null(jaspResults[[dep]][["plotHist"]]) || !options[["plotHist"]]) {
      return()
    }
    plot <- createJaspPlot(title = gettext("Histogram of Residuals"), width = 500, height = 500)
    plot$dependOn(options = c("plotHist", "histogramBinWidthType", "histogramManualNumberOfBins", .doeAnalysisBaseDependencies()))
    plot$position <- 8
    jaspResults[[dep]][["plotHist"]] <- plot
    if (!ready || is.null(jaspResults[[dep]][["doeResult"]]) || jaspResults[[dep]]$getError()) {
      return()
    }
    result <- jaspResults[[dep]][["doeResult"]]$object[["regression"]]
    plot$plotObject <- jaspGraphs::jaspHistogram(resid(result[["object"]]), gettext("Residuals"), binWidthType = options[["histogramBinWidthType"]],
                                                 numberOfBins = options[["histogramManualNumberOfBins"]])
  }
}

.doeAnalysisPlotFittedVsResiduals <- function(jaspResults, dependent, options, ready) {
  for (dep in dependent) {
    if (!is.null(jaspResults[[dep]][["plotFitted"]]) || !options[["plotFitted"]]) {
      return()
    }
    plot <- createJaspPlot(title = gettext("Residuals versus Fitted Values"), width = 500, height = 500)
    plot$dependOn(options = c("plotFitted", .doeAnalysisBaseDependencies()))
    plot$position <- 9
    jaspResults[[dep]][["plotFitted"]] <- plot
    if (!ready || is.null(jaspResults[[dep]][["doeResult"]]) || jaspResults[[dep]]$getError()) {
      return()
    }
    result <- jaspResults[[dep]][["doeResult"]]$object[["regression"]]
    plot$plotObject <- .doeAnalysisPlotFittedVsResidualsPlotObject(result, options)
  }
}

.doeAnalysisPlotFittedVsResidualsPlotObject <- function(result, options) {
  plotData <- data.frame(x = fitted(result[["object"]]), y = resid(result[["object"]]))
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData[["x"]])
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData[["y"]])
  p <- ggplot2::ggplot(plotData, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
    jaspGraphs::geom_point() +
    ggplot2::scale_x_continuous(name = gettext("Fitted values"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext("Residuals"), breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  return(p)
}

.doeAnalysisPlotResidualsVsOrder <- function(jaspResults, dependent, dataset, options, ready) {
  for (dep in dependent) {
    if (!is.null(jaspResults[[dep]][["plotRunOrder"]]) || !options[["plotRunOrder"]]) {
      return()
    }
    plot <- createJaspPlot(title = gettext("Residuals versus Run Order"), width = 500, height = 500)
    plot$dependOn(options = c("plotRunOrder", .doeAnalysisBaseDependencies()))
    plot$position <- 10
    jaspResults[[dep]][["plotRunOrder"]] <- plot
    if (!ready || is.null(jaspResults[[dep]][["doeResult"]]) || jaspResults[[dep]]$getError()) {
      return()
    }
    result <- jaspResults[[dep]][["doeResult"]]$object[["regression"]]
    p <- .doeAnalysisPlotResidualsVsOrderPlotObject(result, dataset, options)
    plot$plotObject <- p
  }
}

.doeAnalysisPlotResidualsVsOrderPlotObject <- function(result, dataset, options) {
  runOrder <- seq_len(nrow(dataset))
  plotData <- data.frame(x = runOrder, y = resid(result[["object"]]))
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$x)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData$y)
  p <- ggplot2::ggplot(data = plotData, ggplot2::aes(x = x, y = y)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
    ggplot2::scale_x_continuous(name = gettext("Run order"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext("Residuals"), breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  return(p)
}

.doeAnalysisPlotMatrixResidualPlot <- function(jaspResults, dependent, dataset, options, ready) {
  for (dep in dependent) {
    if (!is.null(jaspResults[[dep]][["fourInOneResidualPlot"]]) || !options[["fourInOneResidualPlot"]]) {
      return()
    }
    plot <- createJaspPlot(title = gettext("Matrix residual plot"), width = 1000, height = 1000)
    plot$dependOn(options = c("histogramBinWidthType", "histogramManualNumberOfBins",
                              "fourInOneResidualPlot", .doeAnalysisBaseDependencies()))
    plot$position <- 11
    jaspResults[[dep]][["fourInOneResidualPlot"]] <- plot
    if (!ready || is.null(jaspResults[[dep]][["doeResult"]]) || jaspResults[[dep]]$getError()) {
      return()
    }
    result <- jaspResults[[dep]][["doeResult"]]$object[["regression"]]
    plotMat <- matrix(list(), 2, 2)
    plotMat[[1, 1]] <- .doeAnalysisPlotResidualsVsOrderPlotObject(result, dataset, options)
    plotMat[[2, 1]] <- .doeAnalysisPlotFittedVsResidualsPlotObject(result, options)
    plotMat[[1, 2]] <- jaspGraphs::jaspHistogram(resid(result[["object"]]), gettext("Residuals"), binWidthType = options[["histogramBinWidthType"]],
                                                 numberOfBins = options[["histogramManualNumberOfBins"]])
    plotMat[[2, 2]] <- jaspGraphs::plotQQnorm(resid(result[["object"]]), abline = TRUE) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1), name = gettext("Theoretical quantiles")) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1), name = gettext("Observed quantiles"))
    plot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)
  }
}

.doeAnalysisCheckErrors <- function(dataset, options, continuousPredictors, discretePredictors, blocks, covariates, dependent, ready) {
  if (!ready) {
    return()
  }

  factorLevels.target <- c(blocks, discretePredictors)[c(blocks, discretePredictors) != ""]
  variance.target <- c(continuousPredictors, covariates)[c(continuousPredictors, covariates) != ""]

  .hasErrors(
    dataset              = dataset,
    type                 = c("infinity", "factorLevels", "variance"),
    infinity.target      = c(dependent, continuousPredictors, discretePredictors, blocks, covariates),
    factorLevels.target  = factorLevels.target,
    factorLevels.amount  = "< 2",
    variance.target      = variance.target,
    variance.equalTo     = 0,
    exitAnalysisIfErrors = TRUE
  )
}

.doeAnalysisPlotContourSurface <- function(jaspResults, dataset, options, dependent, ready) {
  for (dep in dependent) {
    if (!is.null(jaspResults[[dep]][["contourSurfacePlot"]]) || !options[["contourSurfacePlot"]]) {
      return()
    }
    plotType <- options[["contourSurfacePlotType"]]
    plotTypeString <- ifelse(plotType == "contourPlot", gettext("Contour plot"), gettext("Surface plot"))
    containerTitle <- ifelse(plotType == "contourPlot", gettext("Contour plots"), gettext("Surface plots"))
    container <- createJaspContainer(title = containerTitle)
    container$dependOn(options = c("contourSurfacePlot", "contourSurfacePlotType",
                                   "contourSurfacePlotVariables",
                                   "contourSurfacePlotResponseDivision", "surfacePlotVerticalRotation",
                                   "surfacePlotHorizontalRotation", .doeAnalysisBaseDependencies()))
    container$position <- 12
    jaspResults[[dep]][["contourSurfacePlot"]] <- container

    if (!ready || is.null(jaspResults[[dep]][["doeResult"]]) || jaspResults[[dep]]$getError() ||
        length(options[["contourSurfacePlotVariables"]]) < 2) {
      plot <- createJaspPlot(title = plotTypeString)
      jaspResults[[dep]][["contourSurfacePlot"]][["plot"]] <- plot
      return()
    }

    variables <- unlist(options[["contourSurfacePlotVariables"]])
    variablePairs <- t(utils::combn(variables, 2))
    nPlots <- nrow(variablePairs)

    for (i in seq_len(nPlots)) {
      variablePair <- variablePairs[i, ]
      variablePairString <- paste(variablePair, collapse = gettext(" and "))
      plotTitle <- gettextf("%1$s of %2$s vs %3$s", plotTypeString, dep, variablePairString)
      plot <- createJaspPlot(title = plotTitle, width = 560, height = 460)
      result <- jaspResults[[dep]][["doeResult"]]$object[["regression"]]
      modelVars <- all.vars(attr(stats::terms(result[["object"]]), "variables"))
      missingVars <- setdiff(variablePair, modelVars)
      if (length(missingVars) > 0) {
        plot$setError(gettextf(
          "Could not plot because the following selected variable(s) were not included in the final model: %1$s. They might have been removed during model selection.",
          paste(missingVars, collapse = ", ")
        ))
        jaspResults[[dep]][["contourSurfacePlot"]][[plotTitle]] <- plot
        next
      }
      if (!.doeContourEstimable(result[["object"]], dataset, variablePair)) {
        plot$setError(gettext("Could not plot: some contrasts of the model are aliased, so the data cannot estimate the predicted response over this plot (averaged over the levels of the discrete predictors)."))
        jaspResults[[dep]][["contourSurfacePlot"]][[plotTitle]] <- plot
        next
      }
      if (plotType == "contourPlot") {
        plot$plotObject <- .doeContourPlotObject(result, options, dep, variablePair)
      } else if (plotType == "surfacePlot") {
        plot$plotObject <- function(){.doeContourSurfacePlotObject(result, options, dep, variablePair)}
      }
      jaspResults[[dep]][["contourSurfacePlot"]][[plotTitle]] <- plot
    }
  }
}

.doeContourPlotObject <- function(result, options, dependent, variablePair) {
  regressionFit       <- result[["object"]]
  formula             <- as.formula(paste0("~", paste0(variablePair, collapse = " + ")))
  nResponsePartitions <- options[["contourSurfacePlotResponseDivision"]]

  plotData <- rsm::contour.lm(regressionFit, formula, plot.it = FALSE)[[1]]

  df      <- expand.grid(x = plotData$x, y = plotData$y)
  df$z    <- as.vector(plotData$z)
  zBreaks <- seq(min(df$z), max(df$z), length.out = nResponsePartitions + 1)
  df$zBin <- cut(df$z, breaks = zBreaks, include.lowest = TRUE)
  # format legend labels "(a,b]" -> "a – b" and keep all N partitions (restores pre-ggplot behavior)
  levels(df$zBin) <- gsub(",", " – ", gsub("[][()]", "", levels(df$zBin)))

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = zBin)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_viridis_d(name = dependent, direction = -1, drop = FALSE) +
    ggplot2::scale_x_continuous(name = plotData$labs[1]) +
    ggplot2::scale_y_continuous(name = plotData$labs[2]) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")
}

.doeContourSurfacePlotObject <- function(result, options, dependent, variablePair) {
  regressionFit       <- result[["object"]]
  formula             <- as.formula(paste0("~", paste0(variablePair, collapse = " + ")))
  nResponsePartitions <- options[["contourSurfacePlotResponseDivision"]]
  colorSet            <- heat.colors(nResponsePartitions)
  theta               <- options[["surfacePlotHorizontalRotation"]]
  phi                 <- options[["surfacePlotVerticalRotation"]]

  rsm::persp.lm(regressionFit, formula, theta = theta, phi = phi, zlab = dependent, col = colorSet)
}

# the two functions below are taken from https://rpubs.com/RatherBit/102428

.PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model) / (1 - lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)

  return(PRESS)
}

.pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$"Sum Sq")
  # Calculate the predictive R^2
  pred.r.squared <- 1 - .PRESS(linear.model) / (tss)
  pred.r.squared <- max(0, pred.r.squared) # no negative values

  return(pred.r.squared)
}

# The function below are duplicates from jaspAnova

.reorderModelTerms <- function(options) {
  if (length(options$modelTerms) > 0) {
    fixedFactors <- list()
    continuousFactorsFactorial <- list()

    k <- 1
    l <- 1

    for (i in 1:length(options$modelTerms)) {
      if (sum(unlist(options$modelTerms[[i]]$components) %in% options$continuousFactorsFactorial) > 0) {
        continuousFactorsFactorial[[k]] <- options$modelTerms[[i]]
        k <- k + 1
      } else {
        fixedFactors[[l]] <- options$modelTerms[[i]]
        l <- l + 1
      }
    }

    if (length(continuousFactorsFactorial) > length(options$continuousFactorsFactorial)) {
      modelTerms <- options$modelTerms
      interactions <- TRUE
    } else {
      modelTerms <- c(fixedFactors, continuousFactorsFactorial)
      modelTerms <- modelTerms[match(modelTerms, options$modelTerms)]
      interactions <- FALSE
    }
  } else {
    modelTerms <- list()
    interactions <- FALSE
  }

  list(modelTerms = modelTerms, interactions = interactions)
}

.modelFormula <- function(modelTerms, options, dependent) {
  dependent.normal <- dependent
  dependent.base64 <- dependent

  terms.base64 <- c()
  terms.normal <- c()

  for (term in modelTerms) {
    components <- unlist(term$components)
    term.base64 <- paste(components, collapse = ":", sep = "")
    term.normal <- paste(components, collapse = " \u273B ", sep = "")

    terms.base64 <- c(terms.base64, term.base64)
    terms.normal <- c(terms.normal, term.normal)
  }

  model.def <- paste(dependent.base64, "~", paste(terms.base64, collapse = "+"))

  list(model.def = model.def, terms.base64 = terms.base64, terms.normal = terms.normal)
}
