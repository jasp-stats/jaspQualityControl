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
    ready <- sum(length(options[["fixedFactorsFactorial"]]), length(options[["continuousFactorsFactorial"]])) >= 2 && options[["dependentFactorial"]] != "" && !is.null(unlist(options[["modelTerms"]]))
    discretePredictors <- options[["fixedFactorsFactorial"]]
    continuousPredictors <- options[["continuousFactorsFactorial"]]
    covariates <- options[["covariates"]]
    blocks <- options[["blocksFactorial"]]
    dependent <- options[["dependentFactorial"]]
  } else if (options[["designType"]] == "responseSurfaceDesign") {
    ready <- length(options[["continuousFactorsResponseSurface"]]) >= 1 && options[["dependentResponseSurface"]] != ""
    discretePredictors <- options[["fixedFactorsResponseSurface"]]
    continuousPredictors <- options[["continuousFactorsResponseSurface"]]
    covariates <- NULL
    blocks <- options[["blocksResponseSurface"]]
    dependent <- options[["dependentResponseSurface"]]
  }

  dataset <- .doeAnalysisReadData(dataset, options, continuousPredictors, discretePredictors, blocks, covariates, dependent)

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
  return(dataset)
}

.doeAnalysisBaseDependencies <- function() {
  deps <- c(
    "dependentResponseSurface", "fixedFactorsResponseSurface", "blocksResponseSurface", "runOrder",
    "highestOrder", "order", "continuousFactorsFactorial", "modelTerms", "blocksFactorial",
    "designType", "continuousFactorsResponseSurface", "codeFactors", "rsmPredefinedModel", "fixedFactorsFactorial",
    "rsmPredefinedTerms", "dependentFactorial")
  return(deps)
}

# dataset <- read.csv("C:/Users/Jonee/Google Drive/JASP/SKF Six Sigma/JASP Data Library/4_3_AnalyzeDesign/ResponseSurfaceDesignAnalysis.csv")
# dataset <- dataset[4:7]
# options <- list()
# options[["continuousFactorsResponseSurface"]] <- c("Inlet_feeding", "Time", "Oil_temperature")
#
# options[["dependent"]] <- "Vdk"
# options[["fixedFactors"]] <- NULL
#
# options$modelTerms <- list(list(components = "Inlet_feeding"), list(components = "Time"),
#                            list(components = "Oil_temperature"), list(components = c("Inlet_feeding",
#                                                                                     "Time")), list(components = c("Time", "Oil_temperature"
#                                                                                     )), list(components = c("Inlet_feeding", "Oil_temperature")),
#                            list(components = c("Inlet_feeding", "Time", "Oil_temperature"
#                            )))
#
# options[["continuousFactorsResponseSurface"]] <- c("Inlet_feeding")
# options$modelTerms <- list(list(components = "Inlet_feeding"))
#
# dataset <- read.csv("C:/Users/Jonee/Google Drive/JASP/SKF Six Sigma/JASP Data Library/4_3_AnalyzeDesign/FactorialDesignAnalysis.csv")
# options <- list()
# dataset <- dataset[5:8]
# dataset[1] <- as.factor(dataset[[1]])
# dataset[2] <- as.factor(dataset[[2]])
# dataset[3] <- as.factor(dataset[[3]])
# options[["continuousFactorsResponseSurface"]] <- NULL
# options[["dependent"]] <- "Yield"
# options[["fixedFactors"]] <- c("Exposure_time", "Develop_time", "Mask_dimension")
# options$modelTerms <- list(list(components = "Exposure_time"),
#                            list(components = "Develop_time"),
#                            list(components = "Mask_dimension"),
#                            list(components = c("Mask_dimension", "Exposure_time")))
#
# dataset <- read.csv("C:/Users/Jonee/Google Drive/JASP/SKF Six Sigma/JASP Data Library/4_3_AnalyzeDesign/FactorialDesignAnalysis.csv")
# options <- list()
# dataset <- dataset[5:8]
# dataset[1] <- as.factor(dataset[[1]])
# dataset[3] <- as.factor(dataset[[3]])
# options[["continuousFactorsResponseSurface"]] <- NULL
# options[["dependent"]] <- "Yield"
# options[["fixedFactorsFactorial"]] <- c("Exposure_time", "Mask_dimension")
# options[["covariates"]] <- c("Develop_time")
# options$modelTerms <- list(list(components = "Exposure_time"),
#                            list(components = "Mask_dimension"),
#                            list(components = c("Mask_dimension", "Exposure_time")))
#
#
# dataset <- read.csv("C:/Users/Jonee/Google Drive/JASP/SKF Six Sigma/Datasets/DOE_FAC_withBlocks.csv", sep = ",")
# options <- list()
# dataset <- dataset[4:8]
# dataset[1] <- as.factor(dataset[[1]])
# dataset[2] <- as.factor(dataset[[2]])
# dataset[3] <- as.factor(dataset[[3]])
# dataset[4] <- as.factor(dataset[[4]])
# options[["continuousFactorsResponseSurface"]] <- NULL
# options[["dependent"]] <- "Response"
# options[["fixedFactorsFactorial"]] <- c("A", "B", "C")
# options[["blocks"]] <- c("Blocks")
# options$modelTerms <- list(list(components = "A"),
#                            list(components = "B"),
#                            list(components = "C"),
#                            list(components = c("A", "B")),
#                            list(components = c("A", "C")),
#                            list(components = c("B", "C")))



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

  # Transform to coded, -1 to 1 coding.
  allVars <- c(unlist(continuousPredictors), unlist(discretePredictors), blocks)
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
  } else if (options[["highestOrder"]] && options[["designType"]] == "factorialDesign") {
    formulaString <- paste0(dependent, " ~ (.)^", options[["order"]])
  } else if (options[["rsmPredefinedModel"]] && options[["designType"]] == "responseSurfaceDesign") {
    modelTerms <- options[["rsmPredefinedTerms"]]
    if (length(continuousPredictors) == 1 && modelTerms == "linearAndInteractions") {
      modelTerms <- "linear"
    } else if (length(continuousPredictors) == 1 && modelTerms == "fullQuadratic") {
      modelTerms <- "linearAndSquared"
    }
    numPred <- unlist(continuousPredictors)
    catPred <- unlist(discretePredictors)
    catPred <- catPred[catPred != ""]
    numPredString <- paste0(numPred, collapse = ", ")
    if (!is.null(catPred) && length(catPred) > 0){
      catPredString <- paste0(" + ", catPred, collapse = "")
    } else {
      catPredString <- ""
    }
    formulaString <- switch(modelTerms,
                            "linear" = paste0(dependent, " ~ rsm::FO(", numPredString, ")", catPredString),
                            "linearAndInteractions" = paste0(dependent, " ~ rsm::FO(", numPredString, ")", catPredString, " + rsm::TWI(", numPredString, ")"),
                            "linearAndSquared" = paste0(dependent, " ~ rsm::FO(", numPredString, ") ", catPredString, " +  rsm::PQ(", numPredString, ")"),
                            "fullQuadratic" = paste0(dependent, " ~ rsm::FO(", numPredString, ")", catPredString, " + rsm::TWI(", numPredString, ") +  rsm::PQ(", numPredString, ")")
    )
  }
  if (length(blocks) > 0 && !identical(blocks, ""))
    formulaString <- paste0(formulaString, " + ", blocks)
  if (length(covariates) > 0 && !identical(covariates, "")) {
    covariateString <- paste0(" + ", unlist(covariates), collapse = "")
    formulaString <- paste0(formulaString, covariateString)
  }
  formula <- as.formula(formulaString)

  if (options[["designType"]] == "factorialDesign") {
    regressionFit <- lm(formula, data = dataset)
    regressionFitCoded <- lm(formula, data = datasetCoded)
    regressionSummary <- summary(regressionFit)
    regressionSummaryCoded <- summary(regressionFitCoded)
  } else if (options[["designType"]] == "responseSurfaceDesign") {
    regressionFit <- rsm::rsm(formula, data = dataset, threshold = 0)
    regressionFitCoded <- rsm::rsm(formula, data = datasetCoded, threshold = 0)
    regressionSummary <- summary(regressionFit, threshold = 0) # threshold to 0 so the canonical does not throw an error
    regressionSummaryCoded <- summary(regressionFitCoded, threshold = 0) # threshold to 0 so the canonical does not throw an error
  }

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
    result[["regression"]][["adjrsq"]] <- regressionSummary[["adj.r.squared"]]
    result[["regression"]][["predrsq"]] <- .pred_r_squared(regressionFit)

    resultCoded[["regression"]][["s"]] <- regressionSummaryCoded[["sigma"]]
    resultCoded[["regression"]][["rsq"]] <- regressionSummaryCoded[["r.squared"]]
    resultCoded[["regression"]][["adjrsq"]] <- regressionSummaryCoded[["adj.r.squared"]]
    resultCoded[["regression"]][["predrsq"]] <- .pred_r_squared(regressionFitCoded)

    if (options[["designType"]] == "factorialDesign") {
      anovaFit <- car::Anova(regressionFit)
    } else if (options[["designType"]] == "responseSurfaceDesign") {
      anovaFit <- regressionSummary$lof
      # store lof and pure error, remove them for now and add back in later to not interfere with other calculations
      pureError <- anovaFit["Pure error", ]
      lackOfFit <- anovaFit["Lack of fit", ]
      rowsToRemove <- c("Pure error", "Lack of fit")
      anovaFit <- anovaFit[!row.names(anovaFit) %in% rowsToRemove,]
    }
    anovaFit[["Mean Sq"]] <- anovaFit[["Sum Sq"]] / anovaFit[["Df"]]
    null.names <- names(regressionFit[["coefficients"]])[is.na(regressionFit[["coefficients"]])]
    names <- c("Model", gsub(" ", "", row.names(anovaFit)[-length(row.names(anovaFit))], fixed = TRUE), null.names, "Error", "Total")
    anovaNames <- gsub(" ", "", row.names(anovaFit))
    errorIndex <- which(anovaNames == "Residuals")
    ssm <- sum(anovaFit$`Sum Sq`[-errorIndex])
    msm <- sum(anovaFit$`Sum Sq`[-errorIndex]) / sum(anovaFit$Df[-errorIndex])
    fval <- msm / anovaFit$`Mean Sq`[errorIndex]
    pval <- pf(fval, sum(anovaFit$Df[-errorIndex]), anovaFit$Df[errorIndex], lower.tail = FALSE)
    df <- c(sum(anovaFit[["Df"]][-errorIndex]), anovaFit[["Df"]][-errorIndex], rep(NA, length(null.names)), anovaFit[["Df"]][errorIndex], sum(anovaFit[["Df"]]))
    adjss <- c(ssm, anovaFit[["Sum Sq"]][-errorIndex], rep(NA, length(null.names)), anovaFit[["Sum Sq"]][errorIndex], sum(anovaFit[["Sum Sq"]]))
    adjms <- c(msm, anovaFit[["Mean Sq"]][-errorIndex], rep(NA, length(null.names)), anovaFit[["Mean Sq"]][errorIndex], NA)
    fval <- c(fval, anovaFit[["F value"]], rep(NA, length(null.names)), NA)
    pval <- c(pval, anovaFit[["Pr(>F)"]], rep(NA, length(null.names)), NA)

    #add the lof and pure error rows back in
    if (options[["designType"]] == "responseSurfaceDesign") {
      #imputate it in all ANOVA table vectors before the total row
      df <- c(df[1:length(df)-1], lackOfFit$Df, pureError$Df, df[length(df)])
      names <- c(names[1:length(names)-1], "Lack of fit", "Pure error", names[length(names)])
      names <- gsub("rsm::FO\\(", "Linear terms\\(", names)
      names <- gsub("rsm::TWI\\(", "Two-way interaction terms\\(", names)
      names <- gsub("rsm::PQ\\(", "Squared terms\\(", names)
      adjss <- c(adjss[1:length(adjss)-1], lackOfFit$`Sum Sq`, pureError$`Sum Sq`, adjss[length(adjss)])
      adjms <- c(adjms[1:length(adjms)-1], lackOfFit$`Mean Sq`, pureError$`Mean Sq`, adjms[length(adjms)])
      fval <- c(fval[1:length(fval)-1], lackOfFit$`F value`, NA, fval[length(fval)])
      pval <- c(pval[1:length(pval)-1], lackOfFit$`F value`, NA, pval[length(pval)])
    }

  } else {
    result[["regression"]][["s"]] <- NA
    result[["regression"]][["rsq"]] <- 1
    result[["regression"]][["adjrsq"]] <- NA
    result[["regression"]][["predrsq"]] <- NA

    resultCoded[["regression"]][["s"]] <- NA
    resultCoded[["regression"]][["rsq"]] <- 1
    resultCoded[["regression"]][["adjrsq"]] <- NA
    resultCoded[["regression"]][["predrsq"]] <- NA

    anovaFit <- summary(aov(regressionFit))[[1]]
    ssm <- sum(anovaFit[["Sum Sq"]])
    msm <- ssm / nrow(anovaFit)
    anovaNames <- row.names(anovaFit)
    names <- c("Model", gsub(" ", "", row.names(anovaFit), fixed = TRUE), "Error", "Total")
    df <- c(sum(anovaFit[["Df"]]), anovaFit[["Df"]], 0, sum(anovaFit[["Df"]]))
    adjss <- c(sum(anovaFit[["Sum Sq"]]), anovaFit[["Sum Sq"]], NA, sum(anovaFit[["Sum Sq"]]))
    adjms <- c(sum(anovaFit[["Sum Sq"]]) / nrow(anovaFit), anovaFit[["Mean Sq"]], NA, NA)
    fval <- rep(NA, length(names))
    pval <- rep(NA, length(names))
  }
  result[["anova"]][["object"]] <- anovaFit
  result[["anova"]][["terms"]] <- jaspBase::gsubInteractionSymbol(names)
  result[["anova"]][["df"]] <- df
  result[["anova"]][["adjss"]] <- adjss
  result[["anova"]][["adjms"]] <- adjms
  result[["anova"]][["F"]] <- fval
  result[["anova"]][["p"]] <- pval

  resultCoded[["anova"]][["object"]] <- anovaFit
  resultCoded[["anova"]][["terms"]] <- jaspBase::gsubInteractionSymbol(names)
  resultCoded[["anova"]][["df"]] <- df
  resultCoded[["anova"]][["adjss"]] <- adjss
  resultCoded[["anova"]][["adjms"]] <- adjms
  resultCoded[["anova"]][["F"]] <- fval
  resultCoded[["anova"]][["p"]] <- pval

  # Regression coefficients
  result[["regression"]][["coefficients"]] <- list()
  resultCoded[["regression"]][["coefficients"]] <- list()
  coefs <- as.data.frame(regressionSummary[["coefficients"]])
  coefsCoded <- as.data.frame(regressionSummaryCoded[["coefficients"]])
  valid_coefs <- which(!is.na(coefs[["Estimate"]]))
  termNames <- jaspBase::gsubInteractionSymbol(rownames(coefs)[valid_coefs])

  #remove possible appended factor levels
  if ((options[["rsmPredefinedModel"]] && options[["designType"]] == "responseSurfaceDesign") ||
      (options[["highestOrder"]] && options[["designType"]] == "factorialDesign")) {
    allPredictors <- c(unlist(continuousPredictors), unlist(discretePredictors))
  } else {
    allPredictors <- unique(unlist(options[["modelTerms"]]))
  }
  predictorsForLevelRemoval <- allPredictors
  if (length(blocks) > 0 && !identical(blocks, ""))
    predictorsForLevelRemoval <- c(predictorsForLevelRemoval, blocks)

  # this regex removes the appended factor levels
  regexExpression <- paste0("(", paste(predictorsForLevelRemoval, collapse = "|"), ")((\\^2)?)([^✻]+)(✻?)")
  for (term_i in seq_along(termNames)) {
    replacements <- if (grepl("^2", termNames[term_i], fixed = TRUE)) "\\1\\4" else "\\1\\5"
    termNames[term_i] <- gsub(regexExpression, replacements, termNames[term_i], perl=TRUE)
    termNames[term_i] <- gsub("\\s", "", termNames[term_i])
  }

  result[["regression"]][["coefficients"]][["terms"]] <- termNames
  resultCoded[["regression"]][["coefficients"]][["terms"]] <- termNames

  # calculate effects, but not for blocks, covariates or intercept
  coefEffects <- coefsCoded$Estimate * 2
  coefEffects[1] <- NA
  if (length(blocks) > 0 && !identical(blocks, ""))
    coefEffects[names(coefEffects) == blocks] <- NA
  if (length(covariates) > 0 && !identical(covariates, ""))
    coefEffects[names(coefEffects) %in% unlist(covariates)] <- NA

  coefEffectsUncoded <- coefEffects
  coefEffectsUncoded[coefs$Estimate > 0] <- abs(coefEffectsUncoded) # sign of effect should match uncoded coefficient
  result[["regression"]][["coefficients"]][["effects"]] <- coefEffectsUncoded
  result[["regression"]][["coefficients"]][["est"]] <- coef(regressionFit)[!is.na(coef(regressionFit))]
  result[["regression"]][["coefficients"]][["effects"]][1] <- NA
  result[["regression"]][["coefficients"]][["vif"]] <- c(NA, car::vif(regressionFit)) # Add NA in front for intercept
  result[["regression"]][["coefficients"]][["tValues"]] <- data.frame(summary(regressionFit)$coefficients)$t.value

  resultCoded[["regression"]][["coefficients"]][["effects"]] <- coefEffects
  resultCoded[["regression"]][["coefficients"]][["est"]] <- coef(regressionFitCoded)[!is.na(coef(regressionFit))]
  resultCoded[["regression"]][["coefficients"]][["effects"]][1] <- NA
  resultCoded[["regression"]][["coefficients"]][["vif"]] <- c(NA, car::vif(regressionFitCoded)) # Add NA in front for intercept
  resultCoded[["regression"]][["coefficients"]][["tValues"]] <- data.frame(summary(regressionFitCoded)$coefficients)$t.value

  termNamesAliased <- termNames
  allPredictorsAliases <- LETTERS[seq_along(allPredictors)]
  for (pred_i in seq_along(allPredictors)) {
    termNamesAliased <- gsub(allPredictors[pred_i], allPredictorsAliases[pred_i], termNamesAliased)
  }
  termNamesAliased <- gsub("✻", "", termNamesAliased)
  # covariates and blocks should not get an alias in the table (but keep their default names in the equation, so specifying it here)
  if (length(blocks) > 0 && !identical(blocks, ""))
    termNamesAliased[termNamesAliased == blocks] <- "BLK"
  if (length(covariates) > 0 && !identical(covariates, "")) {
    covariateAliases <- paste0("COV", seq(1, length(covariates)))
    termNamesAliased[termNamesAliased %in% unlist(covariates)] <- covariateAliases
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
  resultCoded[["regression"]][["coefficients"]][["termsAliased"]] <- termNamesAliased

  if (!result[["regression"]][["saturated"]]) {
    result[["regression"]][["coefficients"]][["se"]] <- coefs[["Std. Error"]][valid_coefs]
    result[["regression"]][["coefficients"]][["t"]] <- coefs[["t value"]][valid_coefs]
    result[["regression"]][["coefficients"]][["p"]] <- coefs[["Pr(>|t|)"]][valid_coefs]
    resultCoded[["regression"]][["coefficients"]][["se"]] <- coefsCoded[["Std. Error"]][valid_coefs]
    resultCoded[["regression"]][["coefficients"]][["t"]] <- coefsCoded[["t value"]][valid_coefs]
    resultCoded[["regression"]][["coefficients"]][["p"]] <- coefsCoded[["Pr(>|t|)"]][valid_coefs]
  } else {
    result[["regression"]][["coefficients"]][["se"]] <- rep(NA, length(valid_coefs))
    result[["regression"]][["coefficients"]][["t"]] <- rep(NA, length(valid_coefs))
    result[["regression"]][["coefficients"]][["p"]] <- rep(NA, length(valid_coefs))
    resultCoded[["regression"]][["coefficients"]][["se"]] <- rep(NA, length(valid_coefs))
    resultCoded[["regression"]][["coefficients"]][["t"]] <- rep(NA, length(valid_coefs))
    resultCoded[["regression"]][["coefficients"]][["p"]] <- rep(NA, length(valid_coefs))
  }

  ## Model formula
  ## uncoded
  coefs <- coef(regressionFit)[!is.na(coef(regressionFit))]
  coefNames <- if (options[["tableAlias"]]) termNamesAliased else termNames
  plusOrMin <- sapply(seq_len(length(coefs)), function(x) {
    if (coefs[x] > 0) "+" else "-"
  })
  filledFormula <- sprintf("%s = %.5g %s %s %.5g %s", dependent, coefs[1], coefNames[1], plusOrMin[2], abs(coefs[2]), coefNames[2])
  if (length(coefs) > 2) {
    for (i in 3:length(coefs)) {
      filledFormula <- sprintf("%s %s %.5g %s", filledFormula, plusOrMin[i], abs(coefs[i]), coefNames[i])
    }
  }

  #coded
  coefsCoded <- coef(regressionFitCoded)[!is.na(coef(regressionFitCoded))]
  coefNames <- if (options[["tableAlias"]]) termNamesAliased else termNames
  plusOrMin <- sapply(seq_len(length(coefsCoded)), function(x) {
    if (coefsCoded[x] > 0) "+" else "-"
  })
  filledFormulaCoded <- sprintf("%s = %.5g %s %s %.5g %s", dependent, coefsCoded[1], coefNames[1], plusOrMin[2], abs(coefsCoded[2]), coefNames[2])
  if (length(coefs) > 2) {
    for (i in 3:length(coefs)) {
      filledFormulaCoded <- sprintf("%s %s %.5g %s", filledFormula, plusOrMin[i], abs(coefsCoded[i]), coefNames[i])
    }
  }

  result[["regression"]][["filledFormula"]] <- jaspBase::gsubInteractionSymbol(filledFormula)
  jaspResults[["doeResult"]] <- createJaspState(result)
  jaspResults[["doeResult"]]$dependOn(options = .doeAnalysisBaseDependencies())


  resultCoded[["regression"]][["filledFormula"]] <- jaspBase::gsubInteractionSymbol(filledFormulaCoded)
  jaspResults[["doeResultCoded"]] <- createJaspState(resultCoded)
  jaspResults[["doeResultCoded"]]$dependOn(options = .doeAnalysisBaseDependencies())
}

.doeCoefficientEffects <- function(coefDf, dataset) {
  effectVector <- c()
  for (i in seq_len(nrow(coefDf))) {
    termName <- coefDf$term[i]
    if (termName == "(Intercept)") {
      effect <- NA
    } else {
      coef <- coefDf$coefs[i]
      dataCol <- unlist(dataset[colnames(dataset) == termName])
      factorRange <- min(dataCol)
    }



  }

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
}

.doeAnalysisAnovaTable <- function(jaspResults, options, ready, coded) {
  if (!is.null(jaspResults[["tableAnova"]])) {
    return()
  }
  tb <- createJaspTable(gettext("ANOVA"))
  tb$addColumnInfo(name = "terms", title = "Source", type = "string")
  tb$addColumnInfo(name = "adjss", title = "Sum of Squares", type = "number")
  tb$addColumnInfo(name = "df", title = "df", type = "integer")
  tb$addColumnInfo(name = "adjms", title = "Mean Square", type = "number")
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
  tb$addColumnInfo(name = "se", title = gettext("Standard Error"), type = "number")
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
}

.doeAnalysisEquationTable <- function(jaspResults, options, ready, coded) {
  if (!is.null(jaspResults[["tableEquation"]]) || !options[["tableEquation"]]) {
    return()
  }
  codedString <- ifelse(options[["codeFactors"]], gettext("coded"), gettext("uncoded"))
  tb <- createJaspTable(gettextf("Regression Equation in %s Units", codedString))
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
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  fac <- if (options[["tableAlias"]]) result[["coefficients"]][["termsAliased"]][-1] else result[["coefficients"]][["terms"]][-1]
  coefDf <- data.frame(result[["objectSummary"]]$coefficients)
  tDf <- data.frame("tValue" = coefDf[["t.value"]],
                       terms = result[["coefficients"]][["terms"]])

  # Do not include intercept, covariates and blocks in pareto plot
  tDf <- tDf[-1, ] # remove intercept
  if (length(blocks) > 0 && !identical(blocks, "")) {
    tDf <- tDf[tDf$terms != blocks, ] # remove the block variable
    fac <- if (options[["tableAlias"]]) fac[!grepl("BLK", fac)] else fac[fac != blocks]
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
  plot$dependOn(options = c("plotHist", .doeAnalysisBaseDependencies()))
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
    term.base64 <- paste(.v(components), collapse = ":", sep = "")
    term.normal <- paste(components, collapse = " \u273B ", sep = "")

    terms.base64 <- c(terms.base64, term.base64)
    terms.normal <- c(terms.normal, term.normal)
  }

  model.def <- paste(dependent.base64, "~", paste(terms.base64, collapse = "+"))

  list(model.def = model.def, terms.base64 = terms.base64, terms.normal = terms.normal)
}
