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
  dataset <- .doeAnalysisReadData(dataset, options)

  if (options[["designType"]] == "factorialDesign") {
    ready <- sum(length(options[["fixedFactors"]]), length(options[["continuousFactors"]])) >= 2 && options[["dependent"]] != "" && !is.null(unlist(options[["modelTerms"]]))
  } else if (options[["designType"]] == "responseSurfaceDesign") {
    ready <- length(options[["continuousFactors"]]) >= 1 && options[["dependent"]] != ""
  }
  .doeAnalysisCheckErrors(dataset, options, ready)

  # p <- try({
     .doeAnalysisMakeState(jaspResults, dataset, options, ready)
  # })
#
#   if (isTryError(p)) {
#     jaspResults$setError(gettextf("The analysis crashed with the following error message: %1$s", .extractErrorMessage(p)))
#   }

  .doeAnalysisSummaryTable(jaspResults, options, ready)
  .doeAnalysisAnovaTable(jaspResults, options, ready)
  .doeAnalysisCoefficientsTable(jaspResults, options, ready)
  .doeAnalysisEquationTable(jaspResults, options, ready)
  .doeAnalysisPlotPareto(jaspResults, options, ready)
  .doeAnalysisPlotQQResiduals(jaspResults, options, ready)
  .doeAnalysisPlotHistResiduals(jaspResults, options, ready)
  .doeAnalysisPlotFittedVsResiduals(jaspResults, options, ready)
  .doeAnalysisPlotResidualsVsOrder(jaspResults, dataset, options, ready)
  .doeAnalysisPlotMatrixResidualPlot(jaspResults, dataset, options, ready)
  .doeAnalysisPlotContourSurface(jaspResults, dataset, options, ready)
}

.doeAnalysisReadData <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  }
  factorVars <- NULL
  numericVars <- NULL
  if (!identical(options[["dependent"]], "")) {
    numericVars <- c(numericVars, options[["dependent"]])
  }
  if (length(options[["continuousFactors"]]) > 0 && !identical(options[["continuousFactors"]], "")) {
    numericVars <- c(numericVars, unlist(options[["continuousFactors"]]))
  }
  if (length(options[["fixedFactors"]]) > 0 && !identical(options[["fixedFactors"]], "")) {
    factorVars <- c(factorVars, unlist(options[["fixedFactors"]]))
  }
  if (length(options[["blocks"]]) > 0 && !identical(options[["blocks"]], "")) {
    factorVars <- c(factorVars, options[["blocks"]])
  }
  if (length(options[["covariates"]]) > 0 && !identical(options[["covariates"]], "")) {
    numericVars <- c(numericVars, unlist(options[["covariates"]]))
  }
  dataset <- .readDataSetToEnd(columns.as.numeric = numericVars, columns.as.factor = factorVars)
  dataset <- na.omit(dataset)
  return(dataset)
}

.doeAnalysisCheckErrors <- function(dataset, options) {
  if (is.null(dataset)) {
    return()
  }
  .hasErrors(dataset,
             type = c("infinity", "missingValues", "factorLevels"),
             all.target = c(options[["dependent"]], options[["fixedFactors"]], options[["blocks"]], options[["continuousFactors"]]),
             factorLevels.amount  = "< 2",
             exitAnalysisIfErrors = TRUE
  )
}



.doeAnalysisBaseDependencies <- function() {
  deps <- c(
    "dependent", "fixedFactors", "blocks", "runOrder",
    "highestOrder", "order", "covariates", "modelTerms",
    "designType", "continuousFactors", "codeFactors", "rsmPredefinedModel",
    "rsmPredefinedTerms"
  )
  return(deps)
}

.doeAnalysisMakeState <- function(jaspResults, dataset, options, ready) {
  if (!ready || jaspResults$getError()) {
    return()
  }


  # set the contrasts for all categorical variables, add option to choose later
  for (fac in unlist(options[["fixedFactors"]])) {
    contrasts(dataset[[fac]]) <- "contr.sum"
  }

  # Transform to coded, -1 to 1 coding.
  if (options[["codeFactors"]]) {
    allVars <- c(unlist(options[["continuousFactors"]]), unlist(options[["fixedFactors"]]), options[["blocks"]])
    allVars <- allVars[allVars != ""]
    for (i in seq_along(allVars)) {
        var <- allVars[i]
        varData <- dataset[[var]]
        levels <- sort(unique(varData)) # get levels before transforming to char to preserve possible order
        varData <- as.character(varData) # transform to char, otherwise you cannot add coded values to this variable as "factor level does not exist"
        nLevels <- length(unique(varData))
        steps <- 2/(nLevels - 1) # divide space between -1 and 1 into equal spaces, always including 0
        codes <- seq(-1, 1, steps)
        for (j in seq_along(varData)) {
          codeIndex <- which(varData[j] == levels)
          varData[j] <- codes[codeIndex]
        }
        dataset[[var]] <- as.numeric(varData)
      }
    }

  result <- list()
  result[["regression"]] <- list()
  result[["anova"]] <- list()


  if ((!options[["highestOrder"]] && !options[["rsmPredefinedModel"]]) ||
      (options[["highestOrder"]] && options[["order"]] == 1 && options[["designType"]] == "factorialDesign")) {
    reorderModelTerms <- .reorderModelTerms(options)
    modelTerms <- reorderModelTerms$modelTerms
    modelDef <- .modelFormula(modelTerms, options)
    formulaString <- modelDef$model.def
  } else if (options[["highestOrder"]] && options[["designType"]] == "factorialDesign") {
    formulaString <- paste0(options[["dependent"]], " ~ (.)^", options[["order"]])
  } else if (options[["rsmPredefinedModel"]] && options[["designType"]] == "responseSurfaceDesign") {
    modelTerms <- options[["rsmPredefinedTerms"]]
    if (length(options[["continuousFactors"]]) == 1 && modelTerms == "linearAndInteractions") {
      modelTerms <- "linear"
    } else if (length(options[["continuousFactors"]]) == 1 && modelTerms == "fullQuadratic") {
      modelTerms <- "linearAndSquared"
    }
    numPred <- unlist(options[["continuousFactors"]])
    catPred <- unlist(options[["fixedFactors"]])
    catPred <- catPred[catPred != ""]
    numPredString <- paste0(numPred, collapse = ", ")
    if (!is.null(catPred) && length(catPred) > 0){
      catPredString <- paste0(" + ", catPred, collapse = "")
    } else {
      catPredString <- ""
    }
    formulaString <- switch(modelTerms,
                            "linear" = paste0(options[["dependent"]], " ~ rsm::FO(", numPredString, ")", catPredString),
                            "linearAndInteractions" = paste0(options[["dependent"]], " ~ rsm::FO(", numPredString, ")", catPredString, " + rsm::TWI(", numPredString, ")"),
                            "linearAndSquared" = paste0(options[["dependent"]], " ~ rsm::FO(", numPredString, ") ", catPredString, " +  rsm::PQ(", numPredString, ")"),
                            "fullQuadratic" = paste0(options[["dependent"]], " ~ rsm::FO(", numPredString, ")", catPredString, " + rsm::TWI(", numPredString, ") +  rsm::PQ(", numPredString, ")")
    )
  }
  if (length(options[["blocks"]]) > 0 && !identical(options[["blocks"]], "")) {
    formulaString <- paste0(formulaString, " + ", options[["blocks"]])
  }
  formula <- as.formula(formulaString)

  if (options[["designType"]] == "factorialDesign") {
    regressionFit <- lm(formula, data = dataset)
    regressionSummary <- summary(regressionFit)
  } else if (options[["designType"]] == "responseSurfaceDesign") {
    regressionFit <- rsm::rsm(formula, data = dataset, threshold = 0)
    regressionSummary <- summary(regressionFit, threshold = 0) # threshold to 0 so the canonical does not throw an error
  }

  result[["regression"]][["formula"]] <- formula
  result[["regression"]][["object"]] <- regressionFit
  result[["regression"]][["objectSummary"]] <- regressionSummary
  result[["regression"]][["saturated"]] <- regressionSummary$df[2] == 0

  if (!result[["regression"]][["saturated"]]) {
    result[["regression"]][["s"]] <- regressionSummary[["sigma"]]
    result[["regression"]][["rsq"]] <- regressionSummary[["r.squared"]]
    result[["regression"]][["adjrsq"]] <- regressionSummary[["adj.r.squared"]]
    result[["regression"]][["predrsq"]] <- .pred_r_squared(regressionFit)

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

    anovaFit <- summary(aov(regressionFit))[[1]]
    ssm <- sum(anovaFit[["Sum Sq"]])
    msm <- ssm / nrow(anovaFit)
    names <- c("Model", names(coef(regressionFit))[!is.na(coef(regressionFit))][-1], "Error", "Total")
    df <- c(sum(anovaFit[["Df"]][seq_along(options[["fixedFactors"]])]), anovaFit[["Df"]], 0, sum(anovaFit[["Df"]]))
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

  # Regression coefficients
  result[["regression"]][["coefficients"]] <- list()
  coefs <- as.data.frame(regressionSummary[["coefficients"]])
  valid_coefs <- which(!is.na(coefs[["Estimate"]]))
  termNames <- jaspBase::gsubInteractionSymbol(rownames(coefs)[valid_coefs])
  result[["regression"]][["coefficients"]][["terms"]] <- termNames
  result[["regression"]][["coefficients"]][["effects"]] <- effects(regressionFit, set.sign = TRUE)[valid_coefs]
  result[["regression"]][["coefficients"]][["est"]] <- coef(regressionFit)[!is.na(coef(regressionFit))]
  result[["regression"]][["coefficients"]][["effects"]][1] <- NA

  # Aliasing
  if ((options[["rsmPredefinedModel"]] && options[["designType"]] == "responseSurfaceDesign") ||
      (options[["highestOrder"]] && options[["designType"]] == "factorialDesign")) {
    allPredictors <- c(unlist(options[["continuousFactors"]]), unlist(options[["fixedFactors"]]))
  } else {
    allPredictors <- unique(unlist(options[["modelTerms"]]))
  }
  termNamesAliased <- termNames
  # remove possible appended factor levels
  regexExpression <- paste0("(", paste(allPredictors, collapse = "|"), ")((\\^2)?)([^^✻]+)(✻?)")
  for (term_i in seq_along(termNamesAliased)) {
    termNamesAliased[term_i] <- gsub(regexExpression, "\\1\\2", termNamesAliased[term_i], perl=TRUE)
    termNamesAliased[term_i] <- gsub("\\s", "", termNamesAliased[term_i])
  }
  allPredictorsAliases <- LETTERS[seq_along(allPredictors)]
  for (pred_i in seq_along(allPredictors)) {
    termNamesAliased <- gsub(allPredictors[pred_i], allPredictorsAliases[pred_i], termNamesAliased)
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

  if (!result[["regression"]][["saturated"]]) {
    result[["regression"]][["coefficients"]][["se"]] <- coefs[["Std. Error"]][valid_coefs]
    result[["regression"]][["coefficients"]][["t"]] <- coefs[["t value"]][valid_coefs]
    result[["regression"]][["coefficients"]][["p"]] <- coefs[["Pr(>|t|)"]][valid_coefs]
  } else {
    result[["regression"]][["coefficients"]][["se"]] <- rep(NA, length(valid_coefs))
    result[["regression"]][["coefficients"]][["t"]] <- rep(NA, length(valid_coefs))
    result[["regression"]][["coefficients"]][["p"]] <- rep(NA, length(valid_coefs))
  }

  ## Model formula
  coefs <- coef(regressionFit)[!is.na(coef(regressionFit))]
  coefNames <- if (options[["tableAlias"]]) termNamesAliased else termNames
  plusOrMin <- sapply(seq_len(length(coefs)), function(x) {
    if (coefs[x] > 0) "+" else "-"
  })
  filledFormula <- sprintf("%s = %.5g %s %s %.5g %s", options[["dependent"]], coefs[1], coefNames[1], plusOrMin[2], abs(coefs[2]), coefNames[2])
  for (i in 3:length(coefs)) {
    filledFormula <- sprintf("%s %s %.5g %s", filledFormula, plusOrMin[i], abs(coefs[i]), coefNames[i])
  }
  result[["regression"]][["filledFormula"]] <- jaspBase::gsubInteractionSymbol(filledFormula)

  jaspResults[["doeResult"]] <- createJaspState(result)
  jaspResults[["doeResult"]]$dependOn(options = .doeAnalysisBaseDependencies())
}

.doeAnalysisSummaryTable <- function(jaspResults, options, ready) {
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
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  row <- data.frame(
    s = result[["s"]], rsq = result[["rsq"]], adjrsq = result[["adjrsq"]], predrsq = result[["predrsq"]]
  )
  tb$addRows(row)
}

.doeAnalysisAnovaTable <- function(jaspResults, options, ready) {
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
  result <- jaspResults[["doeResult"]]$object[["anova"]]
  rows <- data.frame(
    terms = result[["terms"]], adjss = result[["adjss"]], df = result[["df"]],
    adjms = result[["adjms"]], fval = result[["F"]], pval = result[["p"]]
  )
  tb$addRows(rows)
}

.doeAnalysisCoefficientsTable <- function(jaspResults, options, ready) {
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
  result <- jaspResults[["doeResult"]]$object[["regression"]][["coefficients"]]
  rows <- data.frame(
    terms = result[["terms"]], effects = result[["effects"]], coef = result[["est"]],
    se = result[["se"]], tval = result[["t"]], pval = result[["p"]], vif = NA
  )
  if (options[["tableAlias"]])
    rows["alias"] <- result[["termsAliased"]]
  tb$addRows(rows)
}

.doeAnalysisEquationTable <- function(jaspResults, options, ready) {
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
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  row <- data.frame(formula = result[["filledFormula"]])
  tb$addRows(row)
}

.doeAnalysisPlotPareto <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["plotPareto"]]) || !options[["plotPareto"]]) {
    return()
  }
  plot <- createJaspPlot(title = if (options[["codeFactors"]]) {
    gettext("Pareto Chart of Standardized Effects")
  } else {
    gettext("Pareto Chart of Unstandardized Effects")
  }, width = 600, height = 400)
  plot$dependOn(options = c("plotPareto", "tableAlias", .doeAnalysisBaseDependencies()))
  plot$position <- 6
  jaspResults[["plotPareto"]] <- plot
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  t <- abs(data.frame(result[["objectSummary"]]$coefficients)$t.value[-1])
  fac <- if (options[["tableAlias"]]) result[["coefficients"]][["termsAliased"]][-1] else result[["coefficients"]][["terms"]][-1]
  df <- result[["objectSummary"]]$df[2]
  crit <- abs(qt(0.025, df))
  fac_t <- cbind.data.frame(fac, t)
  fac_t <- cbind(fac_t[order(fac_t$t), ], y = seq_len(length(t)))
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, t, crit))
  p <- ggplot2::ggplot(data = fac_t, mapping = ggplot2::aes(y = t, x = y)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = crit, linetype = "dashed", color = "red") +
    ggplot2::scale_x_continuous(name = gettext("Term"), breaks = fac_t$y, labels = fac_t$fac) +
    ggplot2::scale_y_continuous(name = if (options[["codeFactors"]]) {
      gettext("Standardized Effect")
    } else {
      gettext("Unstandardized Effect")
    }, breaks = xBreaks, limits = range(xBreaks)) +
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
  plot$plotObject <- jaspDescriptives::.plotMarginal(resid(result[["object"]]), NULL)
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

.doeAnalysisCheckErrors <- function(dataset, options, ready) {
  if (!ready) {
    return()
  }

  modelTerms <- unlist(options$modelTerms, recursive = FALSE)
  factorModelTerms <- options$modelTerms[sapply(modelTerms, function(x) !any(x %in% options$covariates))]
  allComponents <- unique(unlist(lapply(factorModelTerms, `[[`, "components"), use.names = FALSE))

  .hasErrors(
    dataset              = dataset,
    type                 = c("infinity", "factorLevels", "variance"),
    infinity.target      = c(options$dependent, allComponents),
    factorLevels.target  = options[["fixedFactors"]],
    factorLevels.amount  = "< 2",
    exitAnalysisIfErrors = TRUE
  )
}

.doeAnalysisPlotContourSurface <- function(jaspResults, dataset, options, ready) {
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
    plotTitle <- gettextf("%1$s of %2$s vs %3$s", plotTypeString, options[["dependent"]], variablePairString)
    plot <- createJaspPlot(title = plotTitle, width = 500, height = 500)
    if(plotType == "contourPlot") {
      plot$plotObject <- function(){.doeContourSurfacePlotObject(jaspResults, options, variablePair, type = "contour")}
    } else if (plotType == "surfacePlot") {
      plot$plotObject <- function(){.doeContourSurfacePlotObject(jaspResults, options, variablePair, type = "surface")}
    }
    jaspResults[["contourSurfacePlot"]][[plotTitle]] <- plot
  }
}

.doeContourSurfacePlotObject <- function(jaspResults, options, variablePair, type = c("contour", "surface")) {
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
    po <- rsm::persp.lm(regressionFit, formula, theta = theta, phi = phi, zlab = options[["dependent"]],
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
    covariates <- list()

    k <- 1
    l <- 1

    for (i in 1:length(options$modelTerms)) {
      if (sum(unlist(options$modelTerms[[i]]$components) %in% options$covariates) > 0) {
        covariates[[k]] <- options$modelTerms[[i]]
        k <- k + 1
      } else {
        fixedFactors[[l]] <- options$modelTerms[[i]]
        l <- l + 1
      }
    }

    if (length(covariates) > length(options$covariates)) {
      modelTerms <- options$modelTerms
      interactions <- TRUE
    } else {
      modelTerms <- c(fixedFactors, covariates)
      modelTerms <- modelTerms[match(modelTerms, options$modelTerms)]
      interactions <- FALSE
    }
  } else {
    modelTerms <- list()
    interactions <- FALSE
  }

  list(modelTerms = modelTerms, interactions = interactions)
}

.modelFormula <- function(modelTerms, options) {
  dependent.normal <- options$dependent
  dependent.base64 <- .v(options$dependent)

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
