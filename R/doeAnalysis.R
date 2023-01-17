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

  ready <- length(options[["fixedFactors"]]) >= 2 && options[["dependent"]] != "" && !is.null(unlist(options[["modelTerms"]]))
  .doeAnalysisCheckErrors(dataset, options, ready)

  p <- try({
    .doeAnalysisMakeState(jaspResults, dataset, options, ready)
  })
  if (isTryError(p)) {
    jaspResults$setError(gettextf("The analysis crashed with the following error message: %1$s", .extractErrorMessage(p)))
  }

  .doeAnalysisSummaryTable(jaspResults, options, ready)
  .doeAnalysisAnovaTable(jaspResults, options, ready)
  .doeAnalysisCoefficientsTable(jaspResults, options, ready)
  .doeAnalysisEquationTable(jaspResults, options, ready)
  .doeAnalysisAliasTable(jaspResults, options, ready)

  .doeAnalysisPlotPareto(jaspResults, options, ready)
  .doeAnalysisPlotQQResiduals(jaspResults, options, ready)
  .doeAnalysisPlotHistResiduals(jaspResults, options, ready)
  .doeAnalysisPlotFittedVsResiduals(jaspResults, options, ready)
  .doeAnalysisPlotResidualsVsOrder(jaspResults, dataset, options, ready)
}

.doeAnalysisReadData <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  }
  factorVars <- NULL
  numericVars <- NULL
  if (options[["dependent"]] != "") {
    numericVars <- c(numericVars, options[["dependent"]])
  }
  if (length(options[["fixedFactors"]]) > 0 && options[["fixedFactors"]] != "") {
    factorVars <- c(factorVars, unlist(options[["fixedFactors"]]))
  }
  if (options[["blocks"]] != "") {
    factorVars <- c(factorVars, options[["blocks"]])
  }
  if (length(options[["covariates"]]) > 0 && options[["covariates"]] != "") {
    numericVars <- c(numericVars, unlist(options[["covariates"]]))
  }
  dataset <- .readDataSetToEnd(columns.as.numeric = numericVars, columns.as.factor = factorVars)
  return(dataset)
}

.doeAnalysisCheckErrors <- function(dataset, options) {
  if (is.null(dataset)) {
    return()
  }
  .hasErrors(dataset,
    type = c("infinity", "missingValues"),
    all.target = c(options[["dependent"]], options[["fixedFactors"]]),
    exitAnalysisIfErrors = TRUE
  )
}

.doeAnalysisBaseDependencies <- function() {
  deps <- c(
    "dependent", "fixedFactors", "blocks", "runOrder",
    "highestOrder", "order", "covariates", "modelTerms"
  )
  return(deps)
}

.doeAnalysisMakeState <- function(jaspResults, dataset, options, ready) {
  if (!ready || jaspResults$getError()) {
    return()
  }
  #   # Transform to coded
  #   factors <- unlist(dataset[, options[["fixedFactors"]]], use.names = FALSE)
  #   response <- unlist(dataset[, options[["dependent"]]], use.names = FALSE)

  #   perF <- length(factors) / length(options[["fixedFactors"]])
  #   factorsDF <- data.frame(split(factors, ceiling(seq_along(factors) / perF)))

  # #   for (i in 1:ncol(factorsDF)) {
  # #     factorsDF[, i][factorsDF[, i] == min(factorsDF[, i])] <- -1
  # #     factorsDF[, i][factorsDF[, i] == max(factorsDF[, i])] <- 1
  # #   }

  #   # Use original variables names

  #   if (options[["runOrder"]] != "") {
  #     runOrder <- dataset[, unlist(options$runOrder)]
  #     names <- c(options[["runOrder"]], options[["fixedFactors"]], options[["dependent"]])
  #     datasetRow <- cbind.data.frame(runOrder, factorsDF, response)
  #   } else {
  #     names <- c(options[["fixedFactors"]], options[["dependent"]])
  #     datasetRow <- cbind.data.frame(factorsDF, response)
  #   }

  #   names(datasetRow) <- names
  #   dataset <- datasetRow

  result <- list()
  result[["regression"]] <- list()
  result[["anova"]] <- list()

  if (!options[["highestOrder"]] || (options[["highestOrder"]] && options[["order"]] == 1)) {
    reorderModelTerms <- .reorderModelTerms(options)
    modelTerms <- reorderModelTerms$modelTerms
    modelDef <- .modelFormula(modelTerms, options)
    formula <- as.formula(modelDef$model.def)
  } else {
    formula <- as.formula(paste0(options[["dependent"]], " ~ (.)^", options[["order"]]))
  }

  regressionFit <- lm(formula, data = dataset)
  result[["regression"]][["formula"]] <- formula
  result[["regression"]][["object"]] <- regressionFit
  result[["regression"]][["saturated"]] <- summary(regressionFit)$df[2] == 0

  if (!result[["regression"]][["saturated"]]) {
    result[["regression"]][["s"]] <- summary(regressionFit)[["sigma"]]
    result[["regression"]][["rsq"]] <- summary(regressionFit)[["r.squared"]]
    result[["regression"]][["adjrsq"]] <- summary(regressionFit)[["adj.r.squared"]]
    result[["regression"]][["predrsq"]] <- .pred_r_squared(regressionFit)

    anovaFit <- car::Anova(regressionFit)
    anovaFit[["Mean Sq"]] <- anovaFit[["Sum Sq"]] / anovaFit[["Df"]]
    null.names <- names(regressionFit[["coefficients"]])[is.na(regressionFit[["coefficients"]])]
    names <- c("Model", gsub(" ", "", row.names(anovaFit)[-length(row.names(anovaFit))], fixed = TRUE), null.names, "Error", "Total")
    anovaNames <- gsub(" ", "", row.names(anovaFit))
    errorIndex <- which(anovaNames == "Residuals")
    ssm <- sum(anovaFit$`Sum Sq`[-errorIndex])
    msm <- sum(anovaFit$`Sum Sq`[-errorIndex]) / sum(anovaFit$Df[-errorIndex])
    fval <- msm / anovaFit$`Mean Sq`[errorIndex]
    pval <- pf(fval, sum(anovaFit$Df[-errorIndex]), anovaFit$Df[errorIndex], lower.tail = FALSE)

    #   # Pure error for replicates
    # lhs <- names(regressionFit$model)[1]
    # rhs <- names(regressionFit$model)[-1]
    # formula <- rhs[1]
    # for (i in 2:length(rhs)) {
    #   formula <- paste0(c(formula, rhs[i]), collapse = ",")
    # } # create RSM model formula
    # rsmFormula <- as.formula(paste0(lhs, " ~ FO(", formula, ")"))
    # rsmFit <- rsm::rsm(rsmFormula, regressionFit$model)
    # rsm <- summary(fit.rsm)
    # anova.rsm <- rsm[[13]] # anova table
    #   # If Pure error
    #   if (anova.rsm[3, ][1] != 0 && anova.rsm[4, ][1] != 0) {
    #     LackFit <- t(as.data.frame(c(terms = "Lack of fit", round(unlist(anova.rsm[3, ]), 3))))
    #     Pure.Error <- t(as.data.frame(c(terms = "Pure error", round(unlist(anova.rsm[4, ]), 3))))
    #     Total.index <- length(names)
    #     colnames(LackFit) <- colnames(anovaFill)
    #     colnames(Pure.Error) <- colnames(anovaFill)
    #     anovaFill.Total <- anovaFill[Total.index, ]
    #     anovaFill <- rbind(anovaFill[-Total.index, ], LackFit, Pure.Error, anovaFill[Total.index, ])
    #   }

    df <- c(sum(anovaFit[["Df"]][-errorIndex]), anovaFit[["Df"]][-errorIndex], rep(NA, length(null.names)), anovaFit[["Df"]][errorIndex], sum(anovaFit[["Df"]]))
    adjss <- c(ssm, anovaFit[["Sum Sq"]][-errorIndex], rep(NA, length(null.names)), anovaFit[["Sum Sq"]][errorIndex], sum(anovaFit[["Sum Sq"]]))
    adjms <- c(msm, anovaFit[["Mean Sq"]][-errorIndex], rep(NA, length(null.names)), anovaFit[["Mean Sq"]][errorIndex], NA)
    fval <- c(fval, anovaFit[["F value"]], rep(NA, length(null.names)), NA)
    pval <- c(pval, anovaFit[["Pr(>F)"]], rep(NA, length(null.names)), NA)
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
  coefs <- as.data.frame(summary(regressionFit)[["coefficients"]])
  valid_coefs <- which(!is.na(coefs[["Estimate"]]))
  result[["regression"]][["coefficients"]][["terms"]] <- jaspBase::gsubInteractionSymbol(rownames(coefs)[valid_coefs])
  result[["regression"]][["coefficients"]][["est"]] <- coefs[["Estimate"]][valid_coefs]
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
  coefNames <- names(coefs)
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
  tb$addColumnInfo(name = "terms", title = "", type = "string")
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
  tb <- createJaspTable(gettext("Coefficients"))
  tb$addColumnInfo(name = "terms", title = "", type = "string")
  tb$addColumnInfo(name = "coef", title = gettext("Coefficient"), type = "number")
  tb$addColumnInfo(name = "se", title = gettext("Standard Error"), type = "number")
  tb$addColumnInfo(name = "tval", title = "t", type = "number")
  tb$addColumnInfo(name = "pval", title = "p", type = "pvalue")
  tb$dependOn(options = c("tableEquation", .doeAnalysisBaseDependencies()))
  tb$position <- 3
  jaspResults[["tableCoefficients"]] <- tb
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]][["coefficients"]]
  rows <- data.frame(
    terms = result[["terms"]], coef = result[["est"]], se = result[["se"]],
    tval = result[["t"]], pval = result[["p"]]
  )
  tb$addRows(rows)
}

.doeAnalysisEquationTable <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["tableEquation"]]) || !options[["tableEquation"]]) {
    return()
  }
  tb <- createJaspTable(gettext("Regression Equation in Uncoded Units"))
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

.doeAnalysisAliasTable <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["tableAlias"]]) || !options[["tableAlias"]]) {
    return()
  }
  tb <- createJaspTable(gettext("Alias Structure"))
  tb$addColumnInfo(name = "formula", title = "", type = "string")
  tb$dependOn(options = c("tableAlias", .doeAnalysisBaseDependencies()))
  tb$position <- 5
  jaspResults[["tableAlias"]] <- tb
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["anova"]]
  name <- names(coef(result[["object"]]))[-1]
  note <- paste(LETTERS[seq_len(length(name))], name, collapse = ";", sep = " = ")
  tb$addFootnote(note)
  #   rows <- data.frame(Aliases = c(FrF2::aliasprint(factorialDesign)$main, FrF2::aliasprint(factorialDesign)$fi2))
}

.doeAnalysisPlotPareto <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["plotPareto"]]) || !options[["plotPareto"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Pareto Chart of Standardized Effects"))
  plot$dependOn(options = c("plotPareto", .doeAnalysisBaseDependencies()))
  plot$position <- 6
  jaspResults[["plotPareto"]] <- plot
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  if (result[["saturated"]]) {
    plot$setError(gettext("Plotting not possible: The experiment is not a full factorial design."))
    return()
  }
  t <- abs(data.frame(summary(result[["object"]])$coefficients)$t.value[-1])
  fac <- names(coef(result[["object"]]))[-1]
  df <- summary(result[["object"]])$df[2]
  crit <- abs(qt(0.025, df))
  fac_t <- cbind.data.frame(fac, t)
  fac_t <- cbind(fac_t[order(fac_t$t), ], y = seq_len(length(t)))
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(t, crit))
  p <- ggplot2::ggplot(data = fac_t, mapping = ggplot2::aes(y = t, x = y)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = crit, linetype = "dashed", color = "red") +
    ggplot2::scale_x_continuous(name = gettext("Term"), breaks = fac_t$y, labels = fac_t$fac) +
    ggplot2::scale_y_continuous(name = gettext("Standardized Effect"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::coord_flip() +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  plot$plotObject <- p
}

.doeAnalysisPlotQQResiduals <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["plotNorm"]]) || !options[["plotNorm"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Normal Probability Plot of Residuals"))
  plot$dependOn(options = c("plotNorm", .doeAnalysisBaseDependencies()))
  plot$position <- 7
  jaspResults[["plotNorm"]] <- plot
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  if (result[["saturated"]]) {
    plot$setError(gettext("Plotting not possible: The experiment is not a full factorial design."))
    return()
  }
  plot$plotObject <- jaspGraphs::plotQQnorm(resid(result[["object"]]))
}

.doeAnalysisPlotHistResiduals <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["plotHist"]]) || !options[["plotHist"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Histogram of Residuals"))
  plot$dependOn(options = c("plotHist", .doeAnalysisBaseDependencies()))
  plot$position <- 8
  jaspResults[["plotHist"]] <- plot
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  if (result[["saturated"]]) {
    plot$setError(gettext("Plotting not possible: The experiment is not a full factorial design."))
    return()
  }
  plot$plotObject <- jaspDescriptives::.plotMarginal(resid(result[["object"]]), NULL)
}

.doeAnalysisPlotFittedVsResiduals <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["plotFitted"]]) || !options[["plotFitted"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Residuals versus Fitted Values"))
  plot$dependOn(options = c("plotFitted", .doeAnalysisBaseDependencies()))
  plot$position <- 9
  jaspResults[["plotFitted"]] <- plot
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  if (result[["saturated"]]) {
    plot$setError(gettext("Plotting not possible: The experiment is not a full factorial design."))
    return()
  }
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
  plot$plotObject <- p
}

.doeAnalysisPlotResidualsVsOrder <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["plotRunOrder"]]) || !options[["plotRunOrder"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Residuals versus Run Order"))
  plot$dependOn(options = c("plotRunOrder", .doeAnalysisBaseDependencies()))
  plot$position <- 10
  jaspResults[["plotRunOrder"]] <- plot
  if (!ready || is.null(jaspResults[["doeResult"]]) || jaspResults$getError()) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  if (result[["saturated"]]) {
    plot$setError(gettext("Plotting not possible: The experiment is not a full factorial design."))
    return()
  }
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
  plot$plotObject <- p
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

# the two functions below are taken from https://rpubs.com/RatherBit/102428

.PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}

.pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-.PRESS(linear.model)/(tss)
  
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
