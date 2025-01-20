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

#' @export
doeAnalysis <- function(jaspResults, dataset, options, ...) {

  if (options[["designType"]] == "factorialDesign") {
    ready <- sum(length(options[["fixedFactorsFactorial"]]), length(options[["continuousFactorsFactorial"]])) >= 1 &&
      length(options[["dependentFactorial"]]) >= 1 &&
      !is.null(unlist(options[["modelTerms"]]))
    discretePredictors <- options[["fixedFactorsFactorial"]]
    continuousPredictors <- options[["continuousFactorsFactorial"]]
    covariates <- options[["covariates"]]
    blocks <- options[["blocksFactorial"]]
    dependent <- options[["dependentFactorial"]]
  } else if (options[["designType"]] == "responseSurfaceDesign") {
    ready <- length(options[["continuousFactorsResponseSurface"]]) >= 1 && length(options[["dependentResponseSurface"]]) >= 1
    discretePredictors <- options[["fixedFactorsResponseSurface"]]
    continuousPredictors <- options[["continuousFactorsResponseSurface"]]
    covariates <- NULL
    blocks <- options[["blocksResponseSurface"]]
    dependent <- options[["dependentResponseSurface"]]
  }

  dataset <- .doeAnalysisReadData(dataset, options, continuousPredictors, discretePredictors, blocks, covariates, dependent)

  if (length(blocks) > 0 && !identical(blocks, "")) # data reading function renames the block variable to "block"
    blocks <- "Block"

  .doeAnalysisCheckErrors(dataset, options, continuousPredictors, discretePredictors, blocks, covariates, dependent, ready)

  p <- try(.doeAnalysisMakeState(jaspResults, dataset, options, continuousPredictors, discretePredictors, blocks, covariates, dependent, ready))

  if (isTryError(p)) {
    jaspResults[["errorPlot"]] <- createJaspPlot(title = gettext("Error"))
    jaspResults[["errorPlot"]]$setError(p[1])
    jaspResults[["errorPlot"]]$dependOn(.doeAnalysisBaseDependencies())
    return()
  }

  coded <- options[["codeFactors"]]

  .doeAnalysisSummaryTable(jaspResults, options, ready, coded)
  .doeAnalysisAnovaTable(jaspResults, options, ready, coded)
  .doeAnalysisCoefficientsTable(jaspResults, options, ready, coded)
  .doeAnalysisEquationTable(jaspResults, options, ready, coded)
  .doeAnalysisPlotPareto(jaspResults, options, blocks, covariates, ready)
  .doeAnalysisPlotEffectNormalDistribution(jaspResults, options, blocks, covariates, ready)
  .doeAnalysisPlotQQResiduals(jaspResults, options, ready)
  .doeAnalysisPlotHistResiduals(jaspResults, options, ready)
  .doeAnalysisPlotFittedVsResiduals(jaspResults, options, ready)
  .doeAnalysisPlotResidualsVsOrder(jaspResults, dataset, options, ready)
  .doeAnalysisPlotMatrixResidualPlot(jaspResults, dataset, options, ready)
  .doeAnalysisPlotContourSurface(jaspResults, dataset, options, dependent, ready)
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
  dataset <- na.omit(dataset)

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
    "squaredTerms", "sumOfSquaresType", "squaredTermsCoded")
  return(deps)
}

.scaleDOEvariable <- function(x){2*(x-min(x))/(max(x)-min(x))-1}

.doeAnalysisMakeState <- function(jaspResults, dataset, options, continuousPredictors, discretePredictors, blocks, covariates, dependent, ready) {
  if (!ready || jaspResults$getError()) {
    return()
  }

  result <- list()
  result[["regression"]] <- list()
  result[["anova"]] <- list()
  resultCoded <- list()
  resultCoded[["regression"]] <- list()
  resultCoded[["anova"]] <- list()

  # set the contrasts for all categorical variables, add option to choose later
  for (fac in unlist(discretePredictors)) {
    contrasts(dataset[[fac]]) <- "contr.sum"
  }
  if (length(blocks) > 0 && !identical(blocks, ""))
    contrasts(dataset[[blocks]]) <- "contr.sum"

  # Transform to coded, -1 to 1 coding.
  allVars <- c(unlist(continuousPredictors), unlist(discretePredictors))
  allVars <- allVars[allVars != ""]
  datasetCoded <- dataset
  if (options[["codeFactorsMethod"]] == "manual")
    manualCodingTable <- do.call(rbind.data.frame, options[["codeFactorsManualTable"]])
  for (i in seq_along(allVars)) {
    var <- allVars[i]
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
        outerCodes <- 2*(levels[-c(lowPos:highPos)]-lowLevel)/(highLevel-lowLevel)-1 # if any values are above the specified high value or below the specified low value
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
    modelDef <- .modelFormula(modelTerms, options, dependent)
    formulaString <- modelDef$model.def
    if (options[["designType"]] == "responseSurfaceDesign" && length(unlist(options[["squaredTerms"]])) > 0) {
      squaredTerms <- options[["squaredTerms"]]
      squaredTermsString <- paste0(" + I(", squaredTerms, "^2)", collapse = "")
      formulaString <- paste0(formulaString, squaredTermsString)
    }
  } else if (options[["highestOrder"]] && options[["designType"]] == "factorialDesign") {
    independentVariables <- c(unlist(continuousPredictors), unlist(discretePredictors))
    independentVariables <- independentVariables[independentVariables != ""]
    formulaString <- .createHighestOrderInteractionFormula(dependent, independentVariables, interactionOrder = options[["order"]])
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
                            "linear" = paste0(dependent, " ~ ", numPredStringMainEffects, catPredString),
                            "linearAndInteractions" = paste0(dependent, " ~ ", secondOrderInteractionEffects, catPredString),
                            "linearAndSquared" = paste0(dependent, " ~ ", numPredStringMainEffects, numPredStringSquaredEffects, catPredString),
                            "fullQuadratic" = paste0(dependent, " ~ ", secondOrderInteractionEffects, numPredStringSquaredEffects, catPredString)
    )
  }
  if (length(blocks) > 0 && !identical(blocks, ""))
    formulaString <- paste0(formulaString, " + ", blocks)
  if (length(covariates) > 0 && !identical(covariates, "")) {
    covariateString <- paste0(" + ", unlist(covariates), collapse = "")
    formulaString <- paste0(formulaString, covariateString)
  }
  formula <- as.formula(formulaString)

  regressionFit <- lm(formula, data = dataset)
  regressionFitCoded <- lm(formula, data = datasetCoded)
  regressionSummary <- summary(regressionFit)
  regressionSummaryCoded <- summary(regressionFitCoded)

  aliasedTerms <- attributes(alias(regressionFit)$Complete)$dimnames[[1]]

  if (!is.null(aliasedTerms)) {
    allPredictors <- unlist(c(continuousPredictors, discretePredictors, blocks, covariates))
    allPredictors <- allPredictors[allPredictors != ""]
    aliasedTerms <- .removeAppendedFactorLevels(predictorNames = allPredictors, terms = aliasedTerms, interactionSymbol = ":")

    # store for footnote
    aliasedTermsFootnote <- aliasedTerms
    aliasedTermsFootnote <- unname(sapply(aliasedTermsFootnote, .gsubIdentityFunction))
    aliasedTermsFootnote <- gsubInteractionSymbol(aliasedTermsFootnote)
    result[["regression"]][["aliasedTerms"]] <- aliasedTermsFootnote
    resultCoded[["regression"]][["aliasedTerms"]] <- aliasedTermsFootnote

    formula <- as.formula(paste(paste(deparse(formula), collapse=""), paste(aliasedTerms, collapse="-"), sep="-")) # remove the aliased term(s) from the model
    # fit the model again
    regressionFit <- lm(formula, data = dataset)
    regressionFitCoded <- lm(formula, data = datasetCoded)
    regressionSummary <- summary(regressionFit)
    regressionSummaryCoded <- summary(regressionFitCoded)
  }

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

  if (!result[["regression"]][["saturated"]]) {
    result[["regression"]][["s"]] <- regressionSummary[["sigma"]]
    result[["regression"]][["rsq"]] <- regressionSummary[["r.squared"]]
    result[["regression"]][["adjrsq"]] <- max(0, regressionSummary[["adj.r.squared"]]) # Sometimes returns a negative value, so need this
    result[["regression"]][["predrsq"]] <- .pred_r_squared(regressionFit)

    resultCoded[["regression"]][["s"]] <- regressionSummaryCoded[["sigma"]]
    resultCoded[["regression"]][["rsq"]] <- regressionSummaryCoded[["r.squared"]]
    resultCoded[["regression"]][["adjrsq"]] <- max(0, regressionSummaryCoded[["adj.r.squared"]]) # Sometimes returns a negative value, so need this
    resultCoded[["regression"]][["predrsq"]] <- .pred_r_squared(regressionFitCoded)

    ssType <- options[["sumOfSquaresType"]]
    anovaFitData <- if (options[["squaredTermsCoded"]]) regressionFitCoded else regressionFit
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
    anovaFit <- .addModelHeaderTerms(anovaFit, unlist(covariates))
  } else {
    result[["regression"]][["s"]] <- NA
    result[["regression"]][["rsq"]] <- 1
    result[["regression"]][["adjrsq"]] <- NA
    result[["regression"]][["predrsq"]] <- NA

    resultCoded[["regression"]][["s"]] <- NA
    resultCoded[["regression"]][["rsq"]] <- 1
    resultCoded[["regression"]][["adjrsq"]] <- NA
    resultCoded[["regression"]][["predrsq"]] <- NA

    anovaFitData <- if (options[["squaredTermsCoded"]]) regressionFitCoded else regressionFit
    anovaFit <- summary(aov(anovaFitData))[[1]]
    errorRow <- data.frame(Df = 0, SS = 0, MS = 0) # add an error row to keep the format consistent
    colnames(errorRow) <- colnames(anovaFit)
    rownames(errorRow) <- "Error"
    anovaFit <- rbind(anovaFit, errorRow)
    anovaFit$`F value` <- NA # add these empty columns to the saturated design so the anova fit object always has the same format
    anovaFit$`Pr(>F)` <- NA
    anovaFit <- .addModelHeaderTerms(anovaFit, unlist(covariates))

    }

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

  discretePredictorsIndices <- which(termNamesRemoved %in% discretePredictors)
  nDiscretePredictorLevels <- sapply(discretePredictors, function(x) sum(termNamesRemoved == x))

  # Coded terms never have appended factor levels, so just remove whitespace
  termNames <- gsub("\\s", "", termNames)
  termNamesCoded <- gsub("\\s", "", termNamesCoded)

  result[["regression"]][["coefficients"]][["terms"]] <- termNames
  resultCoded[["regression"]][["coefficients"]][["terms"]] <- termNamesCoded

  # calculate effects, but not for blocks, covariates or intercept
  coefEffects <- .doeCoefficientEffects(regressionFit)
  coefEffectsCoded <- .doeCoefficientEffects(regressionFitCoded)
  if (length(blocks) > 0 && !identical(blocks, "")) {
    blockNameIndices <- which(termNamesRemoved == blocks) # get the indices of the block variables
    coefEffects[blockNameIndices] <- NA
    coefEffectsCoded[blockNameIndices] <- NA
  }
  if (length(covariates) > 0 && !identical(covariates, "")) {
    coefEffects[names(coefEffects) %in% unlist(covariates)] <- NA
    coefEffectsCoded[names(coefEffectsCoded) %in% unlist(covariates)] <- NA
  }

  result[["regression"]][["coefficients"]][["effects"]] <- coefEffects
  result[["regression"]][["coefficients"]][["est"]] <- coef(regressionFit)[!is.na(coef(regressionFit))]
  result[["regression"]][["coefficients"]][["effects"]][1] <- NA
  result[["regression"]][["coefficients"]][["vif"]] <- .getVIF(regressionFit, predictorsForLevelRemoval)

  resultCoded[["regression"]][["coefficients"]][["effects"]] <- coefEffectsCoded
  resultCoded[["regression"]][["coefficients"]][["est"]] <- coef(regressionFitCoded)[!is.na(coef(regressionFitCoded))]
  resultCoded[["regression"]][["coefficients"]][["effects"]][1] <- NA
  resultCoded[["regression"]][["coefficients"]][["vif"]] <- .getVIF(regressionFitCoded, predictorsForLevelRemoval)

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
    blockNamesAliased <- paste0("BLK", 1:length(blockNameIndices))
    termNamesAliased[blockNameIndices] <- blockNamesAliased
    termNamesAliasedCoded[blockNameIndices] <- blockNamesAliased
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
  discretePredictorLevels <- unlist(lapply(names(nDiscretePredictorLevels), function(x) get_levels(x, nDiscretePredictorLevels[x], dataset)))
  result[["regression"]][["factorLevels"]][["levels"]] <- discretePredictorLevels


  if (!result[["regression"]][["saturated"]]) {
    result[["regression"]][["coefficients"]][["se"]] <- coefs[["Std. Error"]][valid_coefs]
    result[["regression"]][["coefficients"]][["t"]] <- coefs[["t value"]][valid_coefs]
    result[["regression"]][["coefficients"]][["p"]] <- coefs[["Pr(>|t|)"]][valid_coefs]
    resultCoded[["regression"]][["coefficients"]][["se"]] <- coefsCoded[["Std. Error"]][valid_coefsCoded]
    resultCoded[["regression"]][["coefficients"]][["t"]] <- coefsCoded[["t value"]][valid_coefsCoded]
    resultCoded[["regression"]][["coefficients"]][["p"]] <- coefsCoded[["Pr(>|t|)"]][valid_coefsCoded]
  } else {
    result[["regression"]][["coefficients"]][["se"]] <- rep(NA, length(valid_coefs))
    result[["regression"]][["coefficients"]][["t"]] <- rep(NA, length(valid_coefs))
    result[["regression"]][["coefficients"]][["p"]] <- rep(NA, length(valid_coefs))
    resultCoded[["regression"]][["coefficients"]][["se"]] <- rep(NA, length(valid_coefsCoded))
    resultCoded[["regression"]][["coefficients"]][["t"]] <- rep(NA, length(valid_coefsCoded))
    resultCoded[["regression"]][["coefficients"]][["p"]] <- rep(NA, length(valid_coefsCoded))
  }

  ## Model formula
  ## uncoded
  coefs <- coef(regressionFit)[!is.na(coef(regressionFit))]
  coefs <- round(coefs, .numDecimals)
  coefNames <- if (options[["tableAlias"]]) termNamesAliased else termNames
  plusOrMin <- sapply(seq_len(length(coefs)), function(x) {
    if (coefs[x] > 0) "+" else "-"
  })
  filledFormula <- sprintf("%s = %s %s %s %s %s", dependent, coefs[1], coefNames[1], plusOrMin[2], abs(coefs[2]), coefNames[2])
  if (length(coefs) > 2) {
    for (i in 3:length(coefs)) {
      filledFormula <- sprintf("%s %s %s %s", filledFormula, plusOrMin[i], abs(coefs[i]), coefNames[i])
    }
  }

  #coded
  coefsCoded <- coef(regressionFitCoded)[!is.na(coef(regressionFitCoded))]
  coefsCoded <- round(coefsCoded, .numDecimals)
  coefNames <- if (options[["tableAlias"]]) termNamesAliasedCoded else termNames
  plusOrMin <- sapply(seq_len(length(coefsCoded)), function(x) {
    if (coefsCoded[x] > 0) "+" else "-"
  })
  filledFormulaCoded <- sprintf("%s = %s %s %s %s %s", dependent, coefsCoded[1], coefNames[1], plusOrMin[2], abs(coefsCoded[2]), coefNames[2])
  if (length(coefsCoded) > 2) {
    for (i in 3:length(coefsCoded)) {
      filledFormulaCoded <- sprintf("%s %s %s %s", filledFormulaCoded, plusOrMin[i], abs(coefsCoded[i]), coefNames[i])
    }
  }
  result[["regression"]][["filledFormula"]] <- gsubInteractionSymbol(filledFormula)
  jaspResults[["doeResult"]] <- createJaspState(result)
  jaspResults[["doeResult"]]$dependOn(options = .doeAnalysisBaseDependencies())


  resultCoded[["regression"]][["filledFormula"]] <- gsubInteractionSymbol(filledFormulaCoded)
  jaspResults[["doeResultCoded"]] <- createJaspState(resultCoded)
  jaspResults[["doeResultCoded"]]$dependOn(options = .doeAnalysisBaseDependencies())
}

get_levels <- function(var, num_levels, dataset) {
  levels_var <- levels(dataset[[var]])
  levels_var[2:(num_levels + 1)]
}

.getVIF <- function(regressionFit, predictors) {
  if (ncol(regressionFit$model) < 3) {
    VIF <- rep(NA, length(regressionFit$coefficients))
  } else {
    VIF <- try(car::vif(regressionFit))
    if (!jaspBase::isTryError(VIF)) {
      VIF <- if (is.vector(VIF)) VIF else VIF[,1]
      terms <- names(regressionFit$coefficients)
      regexExpression <- paste0("(", paste(predictors, collapse = "|"), ")((\\^2)?)([^✻]+)(✻?)")
      for (term_i in seq_along(terms)) {
        replacements <- if (grepl("^2", terms[term_i], fixed = TRUE)) "\\1\\4" else "\\1\\5"
        terms[term_i] <- gsub(regexExpression, replacements, terms[term_i], perl=TRUE)
        terms[term_i] <- gsub("\\s", "", terms[term_i])
      }
      VIF <- VIF[terms]
    } else {
      VIF <- rep(NA, length(regressionFit$coefficients))
    }
  }
  return(VIF)
}

.calculateOptimalResponse <- function(continuousPredictors, discretePredictors) {
  # need the fitted model object(s) or some other way to get the formula with coefficients
  # need the min. and max. levels of the predictors
  # need the goal of the optimization process
  coefficients <- result$regression$coefficients$est

  # This function should take values as input and use the lm to predict
  ?predict

  # I have some predictor levels
  InjPress <- dataset$InjPress[1]
  InjTemp <- dataset$InjTemp[1]
  CoolTemp <- dataset$CoolTemp[1]
  Material1 <- as.numeric(dataset$Material)[1]-1


  predictionFrame <- data.frame("PredictorName" = names(coefficients),
                                "Coeff" = unname(coefficients))
 attr(resultCoded$regression$object$terms, "factors") # This matrix can probably be used to do everything. Just multiple it with the input vector and sum up the whole matrix
  objective_function <- function(x, predNames) {
    nPred <- length(x)
    predNames <- predictionFrame$PredictorName[2:(1+nPred)]
    values <- x
    result <- predictionFrame$Coeff[1] # Intercept
    for(term_i in (length(values)+2):nrow(predictionFrame)) {
      input_string <- predictionFrame$PredictorName[term_i]
      # Split the input string by ":"
      split_strings <- strsplit(input_string, ":")[[1]]

      # Find indices of matches
      indices <- which(predNames %in% split_strings)
      values <- c(values, prod(values[indices]))
    }
    values <- c(1, values)
    y <- sum(predictionFrame$Coeff * values)
    return(y)
  }
  # result <- optim(
  #   par = c(1, 1),               # Initial guesses for x1 and x2
  #   fn = objective_function,     # Objective function
  #   method = "L-BFGS-B",         # Optimization method
  #   lower = c(0, 0),             # Lower bounds for x1 and x2
  #   upper = c(10, 10)            # Upper bounds for x1 and x2
  # )

}

.addModelHeaderTerms <- function(anovaFit, covariates = "") {
  rownames(anovaFit) <- gsub(" ", "", row.names(anovaFit), fixed = TRUE)
  rownames(anovaFit) <- unname(sapply(rownames(anovaFit), .gsubIdentityFunction)) # remove identity function around squared terms

  # calculate model row
  modelSS <- sum(anovaFit$`Sum Sq`[-nrow(anovaFit)])
  modelDf <- sum(anovaFit$Df[-nrow(anovaFit)])
  modelMS <- modelSS / modelDf
  msError <- anovaFit$`Mean Sq`[nrow(anovaFit)]
  modelFValue <- if (msError != 0) modelMS / msError else NA
  modelPValue <- if (!is.na(modelFValue)) pf(modelFValue, modelDf, anovaFit$Df[nrow(anovaFit)], lower.tail = FALSE) else NA
  modelRow <- data.frame(df = modelDf, ss = modelSS, ms = modelMS, f = modelFValue, p = modelPValue)
  colnames(modelRow) <- colnames(anovaFit)
  rownames(modelRow) <- "Model"

  # calculate total row
  totalRow <- data.frame(df = sum(anovaFit$Df), ss = sum(anovaFit$`Sum Sq`), ms = NA, f = NA, p = NA)
  colnames(totalRow) <- colnames(anovaFit)
  rownames(totalRow) <- "Total"

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
    covariateRow <- data.frame(df = sum(anovaFitCovariate$Df), ss = sum(anovaFitCovariate$`Sum Sq`), ms = NA, f = NA, p = NA)
    colnames(covariateRow) <- colnames(anovaFit)
    rownames(covariateRow) <- sprintf("\u00A0 %s", "Covariates")
  }

  # calculate linear row and get all linear terms
  linearTermIndices <- which(!grepl("\\^2|:", rownames(anovaFit[-nrow(anovaFit),])) &
                               rownames(anovaFit[-nrow(anovaFit),]) != "Block" &
                               !rownames(anovaFit[-nrow(anovaFit),]) %in% covariates)  # all terms without squared symbol or colon or residuals or Block or covariates
  anovaFitLinear <- anovaFit[linearTermIndices,]
  rownames(anovaFitLinear) <- sprintf("\u00A0 \u00A0 %s", rownames(anovaFitLinear)) # double indent
  linearRow <- data.frame(df = sum(anovaFitLinear$Df), ss = sum(anovaFitLinear$`Sum Sq`), ms = NA, f = NA, p = NA)
  colnames(linearRow) <- colnames(anovaFit)
  rownames(linearRow) <- sprintf("\u00A0 %s", "Linear terms")

  # calculate squared row and get all squared terms
  squaredTermIndices <- which(grepl("\\^2$", rownames(anovaFit)))
  if (length(squaredTermIndices) > 0) {
    anovaFitSquared <- anovaFit[squaredTermIndices,]
    rownames(anovaFitSquared) <- sprintf("\u00A0 \u00A0 %s", rownames(anovaFitSquared)) # double indent
    squaredRow <- data.frame(df = sum(anovaFitSquared$Df), ss = sum(anovaFitSquared$`Sum Sq`), ms = NA, f = NA, p = NA)
    colnames(squaredRow) <- colnames(anovaFit)
    rownames(squaredRow) <- sprintf("\u00A0 %s", "Squared terms")
  }

  # calculate interaction row and get all interaction terms
  interactionTermIndices <- which(grepl(":", rownames(anovaFit)))
  if (length(interactionTermIndices) > 0) {
    anovaFitInteraction <- anovaFit[interactionTermIndices, ]
    rownames(anovaFitInteraction) <- sprintf("\u00A0 \u00A0 %s", rownames(anovaFitInteraction)) # double indent
    interactionRow <- data.frame(df = sum(anovaFitInteraction$Df), ss = sum(anovaFitInteraction$`Sum Sq`), ms = NA, f = NA, p = NA)
    colnames(interactionRow) <- colnames(anovaFit)
    rownames(interactionRow) <- sprintf("\u00A0 %s", "Interaction terms")
  }

  # Model error row
  errorRow <- anovaFit[nrow(anovaFit),]
  rownames(errorRow) <- "Error"

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

.removeAppendedFactorLevels <- function(predictorNames, terms, interactionSymbol = "✻"){
  regexExpression <- paste0("(", paste(predictorNames, collapse = "|"), ")((\\^2)?)([^", interactionSymbol, "]+)(", interactionSymbol, "?)")
  for (term_i in seq_along(terms)) {
    if (grepl("I\\(", terms[term_i])) # if wrapped in identify function, don't do anything, as it will be a squared term and have no appended factor level
      next()
    replacements <- if (grepl("^2", terms[term_i], fixed = TRUE)) "\\1\\4" else "\\1\\5"
    terms[term_i] <- gsub(regexExpression, replacements, terms[term_i], perl=TRUE)
    terms[term_i] <- gsub("\\s", "", terms[term_i])
  }
  return(terms)
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

.doeCoefficientEffects <- function(regressionFit) {
  effectVector <- c()
  for (i in seq_along(regressionFit$coefficients)) {
    termName <- names(regressionFit$coefficients)[i]
    if (termName == "(Intercept)") {
      effect <- NA
    } else {
      coef <- regressionFit$coefficients[i]
      coefLevels <- unique(unlist(regressionFit$model[which(sapply(colnames(regressionFit$model), function(v) grepl(v, termName)))]))
      factorRange <- if (is.numeric(coefLevels)) max(coefLevels) - min(coefLevels) else length(coefLevels)
      effect <- coef * factorRange
    }
    effectVector <- c(effectVector, effect)
  }
  return(effectVector)
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

.doeAnalysisSummaryTable <- function(jaspResults, options, ready, coded) {
  if (!is.null(jaspResults[["tableSummary"]])) {
    return()
  }
  tb <- createJaspTable(gettext("Model Summary"))
  tb$addColumnInfo(name = "s", title = "S", type = "number")
  tb$addColumnInfo(name = "rsq", title = "R\u00B2", type = "number")
  tb$addColumnInfo(name = "adjrsq", title = "Adjusted R\u00B2", type = "number")
  tb$addColumnInfo(name = "predrsq", title = "Predictive R\u00B2", type = "number")
  tb$dependOn(options = .doeAnalysisBaseDependencies())
  tb$position <- 1
  jaspResults[["tableSummary"]] <- tb
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- if (coded) jaspResults[["doeResultCoded"]]$object[["regression"]] else jaspResults[["doeResult"]]$object[["regression"]]
  row <- data.frame(
    s = result[["s"]], rsq = result[["rsq"]], adjrsq = result[["adjrsq"]], predrsq = result[["predrsq"]]
  )
  tb$addRows(row)
  if (!is.null(result[["aliasedTerms"]])) {
    tb$addFootnote(gettextf("The following aliased terms were removed: %s.", paste(result[["aliasedTerms"]], collapse = ", ")))
  }
}

.doeAnalysisAnovaTable <- function(jaspResults, options, ready, coded) {
  if (!is.null(jaspResults[["tableAnova"]])) {
    return()
  }
  tb <- createJaspTable(gettext("ANOVA"))
  tb$addColumnInfo(name = "terms", title = "Source", type = "string")
  tb$addColumnInfo(name = "adjss", title = "Sum of squares", type = "number")
  tb$addColumnInfo(name = "df", title = "df", type = "integer")
  tb$addColumnInfo(name = "adjms", title = "Mean square", type = "number")
  tb$addColumnInfo(name = "fval", title = "F", type = "number")
  tb$addColumnInfo(name = "pval", title = "p", type = "pvalue")
  tb$dependOn(options = .doeAnalysisBaseDependencies())
  tb$position <- 2
  jaspResults[["tableAnova"]] <- tb
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- if (coded) jaspResults[["doeResultCoded"]]$object[["anova"]] else jaspResults[["doeResult"]]$object[["anova"]]
  rows <- data.frame(
    terms = result[["terms"]], adjss = result[["adjss"]], df = result[["df"]],
    adjms = result[["adjms"]], fval = result[["F"]], pval = result[["p"]]
  )
  tb$addRows(rows)
}

.doeAnalysisCoefficientsTable <- function(jaspResults, options, ready, coded) {
  if (!is.null(jaspResults[["tableCoefficients"]])) {
    return()
  }
  codedString <- ifelse(options[["codeFactors"]], gettext("Coded"), gettext("Uncoded"))
  tb <- createJaspTable(gettextf("%s Coefficients", codedString))
  if (options[["tableAlias"]])
    tb$addColumnInfo(name = "alias", title = gettext("Alias"), type = "string")
  tb$addColumnInfo(name = "terms", title = gettext("Term"), type = "string")
  tb$addColumnInfo(name = "effects", title = gettext("Effect"), type = "number")
  tb$addColumnInfo(name = "coef", title = gettext("Coefficient"), type = "number")
  tb$addColumnInfo(name = "se", title = gettext("Standard error"), type = "number")
  tb$addColumnInfo(name = "tval", title = "t", type = "number")
  tb$addColumnInfo(name = "pval", title = "p", type = "pvalue")
  tb$addColumnInfo(name = "vif", title = "VIF", type = "number")
  tb$dependOn(options = c("tableEquation", "tableAlias", .doeAnalysisBaseDependencies()))
  tb$position <- 3
  jaspResults[["tableCoefficients"]] <- tb
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- if (coded) jaspResults[["doeResultCoded"]]$object[["regression"]][["coefficients"]] else jaspResults[["doeResult"]]$object[["regression"]][["coefficients"]]
  rows <- data.frame(
    terms = result[["terms"]], effects = result[["effects"]], coef = result[["est"]],
    se = result[["se"]], tval = result[["t"]], pval = result[["p"]], vif = result[["vif"]]
  )
  if (options[["tableAlias"]])
    rows["alias"] <- result[["termsAliased"]]
  tb$addRows(rows)

  if ((length(options[["fixedFactorsFactorial"]]) > 0 | length(options[["fixedFactorsResponseSurface"]])) && !coded) {
    tb2 <- createJaspTable(gettext("Discrete Predictor Levels"))
    tb2$addColumnInfo(name = "factorName", title = gettext("Name"), type = "string")
    tb2$addColumnInfo(name = "factorLevel", title = gettext("Level"), type = "string")
    tb$dependOn(options = c("codeFactors", "codeFactorsManualTable", "codeFactorsMethod", "tableEquation", "tableAlias", .doeAnalysisBaseDependencies()))
    tb2$position <- 4
    jaspResults[["tableCoefficientsLegend"]] <- tb2

    result2 <- jaspResults[["doeResult"]]$object[["regression"]][["factorLevels"]]
    factorName <- if (options[["tableAlias"]]) result2[["factorNamesAliased"]] else result2[["factorNames"]]
    rows2 <- data.frame(
      factorName = factorName,
      factorLevel = result2[["levels"]]
    )

    tb2$addRows(rows2)
  }
}

.doeAnalysisEquationTable <- function(jaspResults, options, ready, coded) {
  if (!is.null(jaspResults[["tableEquation"]]) || !options[["tableEquation"]]) {
    return()
  }
  codedString <- ifelse(options[["codeFactors"]], gettext("Coded"), gettext("Uncoded"))
  tb <- createJaspTable(gettextf("Regression equation in %s Units", codedString))
  tb$addColumnInfo(name = "formula", title = "", type = "string")
  tb$dependOn(options = .doeAnalysisBaseDependencies())
  tb$position <- 4
  jaspResults[["tableEquation"]] <- tb
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- if (coded) jaspResults[["doeResultCoded"]]$object[["regression"]] else jaspResults[["doeResult"]]$object[["regression"]]
  row <- data.frame(formula = result[["filledFormula"]])
  tb$addRows(row)
}

.doeAnalysisPlotPareto <- function(jaspResults, options, blocks, covariates, ready) {
  if (!is.null(jaspResults[["plotPareto"]]) || !options[["plotPareto"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Pareto Chart of Standardized Effects"), width = 600, height = 400)
  plot$dependOn(options = c("plotPareto", "tableAlias", .doeAnalysisBaseDependencies()))
  plot$position <- 6
  jaspResults[["plotPareto"]] <- plot
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- if (options[["codeFactors"]]) jaspResults[["doeResultCoded"]]$object[["regression"]] else jaspResults[["doeResult"]]$object[["regression"]]
  fac <- if (options[["tableAlias"]]) result[["coefficients"]][["termsAliased"]][-1] else result[["coefficients"]][["terms"]][-1]
  coefDf <- data.frame(result[["objectSummary"]]$coefficients)
  tDf <- data.frame("tValue" = coefDf[["t.value"]],
                    terms = result[["coefficients"]][["terms"]])

  # Do not include intercept, covariates and blocks in pareto plot
  tDf <- tDf[-1, ] # remove intercept
  if (length(blocks) > 0 && !identical(blocks, "")) {
    tDf <- tDf[!grepl(blocks, tDf$terms),]
    fac <- if (options[["tableAlias"]]) fac[!grepl("BLK", fac)] else fac[!grepl(blocks, fac)]
  }
  if (length(covariates) > 0 && !identical(covariates, "")) {
    tDf <- tDf[!tDf$terms %in% unlist(covariates), ] # remove the covariate(s)
    fac <- if (options[["tableAlias"]]) fac[!grepl("COV", fac)] else fac[!fac %in% unlist(covariates)]
  }

  t <- abs(tDf[["tValue"]])
  df <- result[["objectSummary"]]$df[2]
  crit <- abs(qt(0.025, df))
  fac_t <- cbind.data.frame(fac, t)
  fac_t <- cbind(fac_t[order(fac_t$t), ], y = seq_len(length(t)))
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, t, crit))
  critLabelDf <- data.frame(x = 0, y = crit, label = sprintf("t = %.2f", crit))
  p <- ggplot2::ggplot(data = fac_t, mapping = ggplot2::aes(y = t, x = y)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = crit, linetype = "dashed", color = "red") +
    ggplot2::geom_label(data = critLabelDf, mapping = ggplot2::aes(x = x, y = y, label = label), col = "red", size = 5) +
    ggplot2::scale_x_continuous(name = gettext("Term"), breaks = fac_t$y, labels = fac_t$fac) +
    ggplot2::scale_y_continuous(name =
                                  gettext("Standardized Effect"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::coord_flip() +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  plot$plotObject <- p
}

.doeAnalysisPlotEffectNormalDistribution <- function(jaspResults, options, blocks, covariates, ready) {
  if (!is.null(jaspResults[["normalEffectsPlot"]]) || !options[["normalEffectsPlot"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Normal Plot of Standardized Effects"), width = 600, height = 600)
  plot$dependOn(options = c("normalEffectsPlot", "tableAlias", .doeAnalysisBaseDependencies()))
  plot$position <- 11
  jaspResults[["normalEffectsPlot"]] <- plot
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- if (options[["codeFactors"]]) jaspResults[["doeResultCoded"]]$object[["regression"]] else jaspResults[["doeResult"]]$object[["regression"]]
  fac <- if (options[["tableAlias"]]) result[["coefficients"]][["termsAliased"]][-1] else result[["coefficients"]][["terms"]][-1]
  coefDf <- data.frame(result[["objectSummary"]]$coefficients)
  tDf <- data.frame("tValue" = coefDf[["t.value"]],
                    "terms" = result[["coefficients"]][["terms"]],
                    "pValue" = result[["coefficients"]][["p"]])

  # Do not include intercept, covariates and blocks in normal effects plot
  tDf <- tDf[-1, ] # remove intercept
  if (length(blocks) > 0 && !identical(blocks, "")) {
    tDf <- tDf[!grepl(blocks, tDf$terms),]
    fac <- if (options[["tableAlias"]]) fac[!grepl("BLK", fac)] else fac[!grepl(blocks, fac)]
  }
  if (length(covariates) > 0 && !identical(covariates, "")) {
    tDf <- tDf[!tDf$terms %in% unlist(covariates), ] # remove the covariate(s)
    fac <- if (options[["tableAlias"]]) fac[!grepl("COV", fac)] else fac[!fac %in% unlist(covariates)]
  }

  tDf$fac <- fac

  # median rank order function
  x <- tDf$tValue[order(tDf$tValue)]
  n <- length(x)
  i <- rank(x)
  p <- (i - 0.3) / (n + 0.4)
  tDf$percentile <- p[order(tDf$tValue)]

  # statistical significance
  tDf$significant <- ifelse(tDf$pValue < 0.05, "S", "N")
  tDf$labelYPos <- stats::pnorm(stats::qnorm(tDf$percentile) + 0.2) # offset the label by a small amount (0.2) so it is displayed above the point
  yLabels <- c(0.1, 1, 5, seq(10, 90, 10), 95, 99, 99.9)
  yBreaks <- yLabels/100
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(tDf$tValue, -3, 3))
  xLimits <- range(c(tDf$tValue, xBreaks))

  # Create the ggplot with probit transformation
  p <- ggplot2::ggplot(data = tDf, mapping = ggplot2::aes(x = tValue, y = percentile)) +
    ggplot2::stat_function(fun = pnorm, linewidth = 1) +  # Reference line using the pnorm function
    jaspGraphs::geom_point(mapping = ggplot2::aes(fill = significant), size = 4) +
    ggplot2::scale_y_continuous(trans = 'probit', labels = yLabels, breaks = yBreaks, name = "Percent", limits = c(0.0001, 0.9999)) +
    ggplot2::scale_x_continuous(name = "Standardized Effect", breaks = xBreaks, limits = xLimits) +
    ggplot2::scale_fill_manual(values = c("S" = "darkred", "N" = "grey"), name = NULL, labels = c(gettext("Significant"), gettext("Not Significant")), breaks = c("S", "N")) +
    ggplot2::geom_label(mapping = ggplot2::aes(label = fac, y = labelYPos), size = 4) +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe() +
    ggplot2::theme(legend.position = "right")

  plot$plotObject <- p
}

.doeAnalysisPlotQQResiduals <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["plotNorm"]]) || !options[["plotNorm"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Normal Probability Plot of Residuals"), width = 500, height = 500)
  plot$dependOn(options = c("plotNorm", .doeAnalysisBaseDependencies()))
  plot$position <- 7
  jaspResults[["plotNorm"]] <- plot
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  plot$plotObject <- jaspGraphs::plotQQnorm(resid(result[["object"]]))
}

.doeAnalysisPlotHistResiduals <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["plotHist"]]) || !options[["plotHist"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Histogram of Residuals"), width = 500, height = 500)
  plot$dependOn(options = c("plotHist", "histogramBinWidthType", "histogramManualNumberOfBins", .doeAnalysisBaseDependencies()))
  plot$position <- 8
  jaspResults[["plotHist"]] <- plot
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  plot$plotObject <- jaspDescriptives::.plotMarginal(resid(result[["object"]]), NULL, binWidthType = options[["histogramBinWidthType"]],
                                                     numberOfBins = options[["histogramManualNumberOfBins"]])
}

.doeAnalysisPlotFittedVsResiduals <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["plotFitted"]]) || !options[["plotFitted"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Residuals versus Fitted Values"), width = 500, height = 500)
  plot$dependOn(options = c("plotFitted", .doeAnalysisBaseDependencies()))
  plot$position <- 9
  jaspResults[["plotFitted"]] <- plot
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  plot$plotObject <- .doeAnalysisPlotFittedVsResidualsPlotObject(jaspResults, options)
}

.doeAnalysisPlotFittedVsResidualsPlotObject <- function(jaspResults, options) {
  result <- jaspResults[["doeResult"]]$object[["regression"]]
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

.doeAnalysisPlotResidualsVsOrder <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["plotRunOrder"]]) || !options[["plotRunOrder"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Residuals versus Run Order"), width = 500, height = 500)
  plot$dependOn(options = c("plotRunOrder", .doeAnalysisBaseDependencies()))
  plot$position <- 10
  jaspResults[["plotRunOrder"]] <- plot
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  p <- .doeAnalysisPlotResidualsVsOrderPlotObject(jaspResults, dataset, options)
  plot$plotObject <- p
}

.doeAnalysisPlotResidualsVsOrderPlotObject <- function(jaspResults, dataset, options) {
  result <- jaspResults[["doeResult"]]$object[["regression"]]
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

.doeAnalysisPlotMatrixResidualPlot <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["fourInOneResidualPlot"]]) || !options[["fourInOneResidualPlot"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Matrix residual plot"), width = 1000, height = 1000)
  plot$dependOn(options = c("fourInOneResidualPlot", .doeAnalysisBaseDependencies()))
  plot$position <- 11
  jaspResults[["fourInOneResidualPlot"]] <- plot
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  plotMat <- matrix(list(), 2, 2)
  plotMat[[1, 1]] <- .doeAnalysisPlotResidualsVsOrderPlotObject(jaspResults, dataset, options)
  plotMat[[2, 1]] <- .doeAnalysisPlotFittedVsResidualsPlotObject(jaspResults, options)
  plotMat[[1, 2]] <- jaspDescriptives::.plotMarginal(resid(result[["object"]]), NULL)
  plotMat[[2, 2]] <- jaspGraphs::plotQQnorm(resid(result[["object"]]))
  plot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)
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
  if (!is.null(jaspResults[["contourSurfacePlot"]]) || !options[["contourSurfacePlot"]]) {
    return()
  }
  plotType <- options[["contourSurfacePlotType"]]
  plotTypeString <- ifelse(plotType == "contourPlot", gettext("Contour plot"), gettext("Surface plot"))
  containerTitle <- ifelse(plotType == "contourPlot", gettext("Contour plots"), gettext("Surface plots"))
  container <- createJaspContainer(title = containerTitle)
  container$dependOn(options = c("contourSurfacePlot", "contourSurfacePlotType",
                                 "contourSurfacePlotVariables", "contourSurfacePlotLegend",
                                 "contourSurfacePlotResponseDivision", "surfacePlotVerticalRotation",
                                 "surfacePlotHorizontalRotation", .doeAnalysisBaseDependencies()))
  container$position <- 12
  jaspResults[["contourSurfacePlot"]] <- container

  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError() ||
      length(options[["contourSurfacePlotVariables"]]) < 2) {
    plot <- createJaspPlot(title = plotTypeString)
    jaspResults[["contourSurfacePlot"]][["plot"]] <- plot
    return()
  }

  variables <- unlist(options[["contourSurfacePlotVariables"]])
  variablePairs <- t(utils::combn(variables, 2))
  nPlots <- nrow(variablePairs)

  for (i in seq_len(nPlots)) {
    variablePair <- variablePairs[i, ]
    variablePairString <- paste(variablePair, collapse = gettext(" and "))
    plotTitle <- gettextf("%1$s of %2$s vs %3$s", plotTypeString, dependent, variablePairString)
    plot <- createJaspPlot(title = plotTitle, width = 500, height = 500)
    if(plotType == "contourPlot") {
      plot$plotObject <- function(){.doeContourSurfacePlotObject(jaspResults, options, dependent, variablePair, type = "contour")}
    } else if (plotType == "surfacePlot") {
      plot$plotObject <- function(){.doeContourSurfacePlotObject(jaspResults, options, dependent, variablePair, type = "surface")}
    }
    jaspResults[["contourSurfacePlot"]][[plotTitle]] <- plot
  }
}

.doeContourSurfacePlotObject <- function(jaspResults, options, dependent, variablePair, type = c("contour", "surface")) {
  type <- match.arg(type)
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  regressionFit <- result[["object"]]
  formula <- as.formula(paste0("~", paste0(variablePair, collapse = " + ")))
  nResponsePartitions <- options[["contourSurfacePlotResponseDivision"]]
  colorSet <- heat.colors(nResponsePartitions)
  if (type == "contour"){
    po <- rsm::image.lm(regressionFit, formula, las = 1, col = colorSet)
  } else if (type == "surface") {
    theta <- options[["surfacePlotHorizontalRotation"]]
    phi <- options[["surfacePlotVerticalRotation"]]
    po <- rsm::persp.lm(regressionFit, formula, theta = theta, phi = phi, zlab = dependent,
                        col = colorSet)
  }
  if (options[["contourSurfacePlotLegend"]]){
    partitionRanges <- levels(cut(po[[1]]$z, breaks = nResponsePartitions))
    partitionRanges <- gsub(",", " \U2013 ", partitionRanges)
    partitionRanges <- gsub("\\(", "", partitionRanges)
    partitionRanges <- gsub("\\]", "", partitionRanges)
    legend(x = "topright", legend = partitionRanges, fill = colorSet)
  }
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
