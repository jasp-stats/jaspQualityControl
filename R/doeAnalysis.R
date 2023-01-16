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
  .doeAnalysisCheckErrors(dataset, options)

  .doeAnalysisMakeState(dataset, options)

  .doeAnalysisSummaryTable(jaspResults, options)
  .doeAnalysisAnovaTable(jaspResults, options)
  .doeAnalysisCoefficientsTable(jaspResults, options)
  .doeAnalysisEquationTable(jaspResults, options)
  .doeAnalysisAliasTable(jaspResults, options)

  .doeAnalysisPlotPareto(jaspResults, options)
  .doeAnalysisPlotQQResiduals(jaspResults, options)
  .doeAnalysisPlotHistResiduals(jaspResults, options)
  .doeAnalysisPlotFittedVsResiduals(jaspResults, options)
  .doeAnalysisPlotResidualsVsOrder(jaspResults, dataset, options)
}

.doeAnalysisReadData <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  }
  factorVars <- NULL
  numericVars <- NULL
  if (length(options[["factors"]]) > 0 && options[["factors"]] != "") {
    factorVars <- c(factorVars, unlist(options[["factors"]]))
  }
  if (options[["response"]] != "") {
    numericVars <- c(numericVars, options[["response"]])
  }
  if (options[["runorder"]] != "") {
    numericVars <- c(numericVars, options[["runorder"]])
  }
  if (options[["blocks"]] != "") {
    factorVars <- c(factorVars, options[["blocks"]])
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
    all.target = c(options[["response"]], options[["runorder"]], options[["factors"]]),
    exitAnalysisIfErrors = TRUE
  )
}

.doeAnalysisReorderModelTerms <- function(options) {
  modelTerms <- list()
  modelTerms[["interactions"]] <- FALSE
  terms <- list()
  for (i in seq_len(length(options[["modelTerms"]]))) {
    terms[[length(terms) + 1]] <- options[["modelTerms"]][[i]]
  }
  terms <- terms[match(terms, options[["modelTerms"]])]
  return(modelTerms)
}

.modelFormula <- function(options, terms) {
  dependent <- options[["response"]]
  terms.base64 <- c()
  terms.normal <- c()

  for (term in terms) {
    components <- unlist(term$components)
    term.base64 <- paste(.v(components), collapse = ":", sep = "")
    term.normal <- paste(components, collapse = " \u00d7 ", sep = "")

    terms.base64 <- c(terms.base64, term.base64)
    terms.normal <- c(terms.normal, term.normal)
  }

  model.def <- paste(dependent.base64, "~", paste(terms.base64, collapse = "+"))

  list(model.def = model.def, terms.base64 = terms.base64, terms.normal = terms.normal)
}

.doeAnalysisMakeState <- function(jaspResults, dataset, options, model, terms) {
  terms <- .doeAnalysisReorderModelTerms(options)
  model <- .modelFormula(terms, options)
  ready <- ready <- (length(options[["factors"]]) >= 2 && options[["response"]] != "" && (!is.null(unlist(options[["modelTerms"]])) || options[["enabledIntOrder"]]))
  if (!ready || jaspResults$getError()) {
    return()
  }

  result <- list()
  result[["regression"]] <- list()
  result[["anova"]] <- list()

  if (!options[["enabledIntOrder"]]) {
    formula <- as.formula(model[["model.def"]])
    regressionFit <- lm(formula, data = dataset)
  } else {
    # reponseName <- unlist(options$FAresponse)
    # factorsNames <- unlist(options$FAassignedFactors)
    # runOrderName <- unlist(options$FArunOrder)

    # factors <- unlist(dataset[, options[["factors"]]], use.names = FALSE)
    # response <- unlist(dataset[, options[["response"]]], use.names = FALSE)

    # perF <- length(factors) / length(options[["factors"]])
    # factorsDF <- data.frame(split(factors, ceiling(seq_along(factors) / perF)))
    # forFit <- cbind.data.frame(factorsDF, response)
    # names <- factorsNames
    # colnames(forFit) <- c(names, "response")
    # order <- options[["intOrder"]]

    # if (order == 1) {
    #   regressionFit <- lm(response ~ ., forFit)
    # } else {
    #   regressionFit <- lm(paste0("response ~ (.)^", order), forFit)
    # }
  }
  result[["regression"]][["formula"]] <- formula
  result[["regression"]][["object"]] <- regressionFit
  result[["regression"]][["saturated"]] <- summary(regressionFit)$df[2] == 0
  if (!result[["regression"]][["saturated"]]) {
    result[["regression"]][["s"]] <- summary(regressionFit)[["sigma"]]
    result[["regression"]][["rsq"]] <- summary(regressionFit)[["r.squared"]] * 100
    result[["regression"]][["adjrsq"]] <- summary(regressionFit)[["adj.r.squared"]] * 100
  }

  if (!result[["regression"]][["saturated"]]) {
    anovaFit <- car::Anova(regressionFit)
    anovaFit[["Mean Sq"]] <- anovaFit[["Sum Sq"]] / anovaFit[["Df"]]
  } else {
    anovaFit <- summary(aov(regressionFit))[[1]]
  }
  result[["anova"]][["object"]] <- anova

  if (!result[["regression"]][["saturated"]]) {
    null.names <- names(regressionFit[["coefficients"]])[is.na(regressionFit[["coefficients"]])]
    names <- c("Model", gsub(" ", "", row.names(anovaFit)[-length(row.names(anovaFit))], fixed = TRUE), null.names, "Error", "Total")
    anovaNames <- gsub(" ", "", row.names(anovaFit))
    errorIndex <- which(anovaNames == "Residuals")

    ssm <- sum(anovaFit$`Sum Sq`[-errorIndex])
    msm <- sum(anovaFit$`Sum Sq`[-errorIndex]) / sum(anovaFit$Df[-errorIndex])
    fval <- msm / anovaFit$`Mean Sq`[errorIndex]
    pval <- pf(fval, sum(anovaFit$Df[-errorIndex]), anovaFit$Df[errorIndex], lower.tail = FALSE)
  } else {
    ssm <- sum(anovaFit$`Sum Sq`)
    msm <- ssm / nrow(anova)
    names <- c("Model", names(coef(regressionFit))[!is.na(coef(regressionFit))][-1], "Error", "Total")
  }

  #   facotrsName <- unlist(options$FAassignedFactors)
  #   response <- unlist(options$FAresponse)
  #   n.factors <- length(facotrsName)

  #   if (ready) {

  #     # Transform to coded
  #     factors <- unlist(dataset[, options[["FAassignedFactors"]]], use.names = FALSE)
  #     response <- unlist(dataset[, options[["FAresponse"]]], use.names = FALSE)

  #     perF <- length(factors) / length(options[["FAassignedFactors"]])
  #     factorsDF <- data.frame(split(factors, ceiling(seq_along(factors) / perF)))

  #     for (i in 1:ncol(factorsDF)) {
  #       factorsDF[, i][factorsDF[, i] == min(factorsDF[, i])] <- -1
  #       factorsDF[, i][factorsDF[, i] == max(factorsDF[, i])] <- 1
  #     }

  #     # Use original variables names

  #     if (options[["FArunOrder"]] != "") {
  #       runOrder <- dataset[, unlist(options$FArunOrder)]
  #       names <- c(options$FArunOrder, options$FAassignedFactors, options$FAresponse)
  #       datasetRow <- cbind.data.frame(runOrder, factorsDF, response)
  #     } else {
  #       names <- c(options$FAassignedFactors, options$FAresponse)
  #       datasetRow <- cbind.data.frame(factorsDF, response)
  #     }

  #     names(datasetRow) <- names

  #     dataset <- datasetRow
  #   }

  #   # Pure error for replicates
  #   fit.names <- names(fit$model)[-1]
  #   formula.facotrsName <- fit.names[1]
  #   for (i in 2:length(facotrsName)) {
  #     formula.facotrsName <- gettextf("%s,%s", formula.facotrsName, fit.names[i])
  #   } # create RSM model formula

  #   formula.rsm <- gettextf("%s ~ FO(%s)", names(fit$model)[1], formula.facotrsName)
  #   fit.rsm <- rsm::rsm(as.formula(formula.rsm), fit$model) # fit rsm model
  #   rsm <- summary(fit.rsm)
  #   anova.rsm <- rsm[[13]] # anova table

  #   if (!saturated) {
  #     df <- c(sum(anova$Df[-errorIndex]), anova$Df[-errorIndex], rep(NA, length(null.names)), anova$Df[errorIndex], sum(anova$Df))
  #     Adj.SS <- c(model.SS, anova$`Sum Sq`[-errorIndex], rep(NA, length(null.names)), anova$`Sum Sq`[errorIndex], sum(anova$`Sum Sq`))
  #     Adj.MS <- c(model.MS, anova$`Mean Sq`[-errorIndex], rep(NA, length(null.names)), anova$`Mean Sq`[errorIndex], NA)
  #     `F` <- c(model.F, anova$`F value`, rep(NA, length(null.names)), NA)
  #     p <- c(model.Pval, anova$`Pr(>F)`, rep(NA, length(null.names)), NA)

  #     anovaFill <- data.frame(
  #       Terms = names,
  #       df = round(df, 3),
  #       Adj.SS = round(Adj.SS, 3),
  #       Adj.MS = round(Adj.MS, 3),
  #       `F` = round(`F`, 3),
  #       p = round(p, 3)
  #     )

  #     # If Pure error
  #     if (anova.rsm[3, ][1] != 0 && anova.rsm[4, ][1] != 0) {
  #       LackFit <- t(as.data.frame(c(terms = "Lack of fit", round(unlist(anova.rsm[3, ]), 3))))
  #       Pure.Error <- t(as.data.frame(c(terms = "Pure error", round(unlist(anova.rsm[4, ]), 3))))
  #       Total.index <- length(names)
  #       colnames(LackFit) <- colnames(anovaFill)
  #       colnames(Pure.Error) <- colnames(anovaFill)
  #       anovaFill.Total <- anovaFill[Total.index, ]
  #       anovaFill <- rbind(anovaFill[-Total.index, ], LackFit, Pure.Error, anovaFill[Total.index, ])
  #     }

  #   } else {
  #     model.SS <- sum(anova$`Sum Sq`)
  #     model.MS <- model.SS / nrow(anova)
  #     names <- c("Model", names(coef(fit))[!is.na(coef(fit))][-1], "Error", "Total")

  #     anovaFill <- data.frame(
  #       Terms = names,
  #       df = c(sum(anova$Df[1:n.factors]), anova$Df, 0, sum(anova$Df)),
  #       Adj.SS = c(round(c(model.SS, anova$`Sum Sq`), 3), "*", round(sum(anova$`Sum Sq`), 3)),
  #       Adj.MS = c(round(c(model.MS, anova$`Mean Sq`), 3), "*", "*"),
  #       `F` = rep("*", length(names)),
  #       p = rep("*", length(names))
  #     )

  #   }

  jaspResults[["doeResult"]] <- createJaspState(result)
  jaspResults[["doeResult"]]$dependOn(options = "none")
}

.doeAnalysisSummaryTable <- function(jaspResults, options) {
  if (!is.null(jaspResults[["tableSummary"]])) {
    return()
  }
  tb <- createJaspTable(gettext("Model summary"))
  tb$addColumnInfo(name = "s", title = "S", type = "number")
  tb$addColumnInfo(name = "rsq", title = "R\u00B2", type = "number")
  tb$addColumnInfo(name = "adjrsq", title = "Adjusted R\u00B2", type = "number")
  tb$position <- 1
  jaspResults[["tableSummary"]] <- tb
  if (is.null(jaspResults[["doeResult"]])) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  row <- data.frame(s = result[["s"]], rsq = result[["rsq"]], adjrsq = result[["adjrsq"]])
  tb$addRows(row)
}

.doeAnalysisAnovaTable <- function(jaspResults, options) {
  if (!is.null(jaspResults[["tableAnova"]])) {
    return()
  }
  tb <- createJaspTable(gettext("Analysis of variance"))
  tb$addColumnInfo(name = "terms", title = "", type = "string")
  tb$addColumnInfo(name = "df", title = "df", type = "integer")
  tb$addColumnInfo(name = "adjss", title = "Adj.SS", type = "number")
  tb$addColumnInfo(name = "adjms", title = "Adj.MS", type = "number")
  tb$addColumnInfo(name = "fval", title = "F", type = "number")
  tb$addColumnInfo(name = "pval", title = "p", type = "pvalue")
  tb$position <- 2
  jaspResults[["tableAnova"]] <- tb
  if (is.null(jaspResults[["doeResult"]])) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["anova"]]
}

.doeAnalysisCoefficientsTable <- function(jaspResults, options) {
  if (!is.null(jaspResults[["tableCoefficients"]])) {
    return()
  }
  tb <- createJaspTable(gettext("Analysis of variance"))
  tb$addColumnInfo(name = "terms", title = "", type = "string")
  tb$addColumnInfo(name = "coef", title = gettext("Coefficient"), type = "number")
  tb$addColumnInfo(name = "se", title = gettext("Standard Error"), type = "number")
  tb$addColumnInfo(name = "tval", title = "t", type = "number")
  tb$addColumnInfo(name = "pval", title = "p", type = "pvalue")
  tb$addColumnInfo(name = "vif", title = "VIF", type = "number")
  tb$position <- 3
  jaspResults[["tableCoefficients"]] <- tb
  if (is.null(jaspResults[["doeResult"]])) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["anova"]]
}

.doeAnalysisEquationTable <- function(jaspResults, options) {
  if (!is.null(jaspResults[["tableEquation"]])) {
    return()
  }
  tb <- createJaspTable(gettext("Regression Equation in Uncoded Units"))
  tb$addColumnInfo(name = "formula", title = "", type = "string")
  tb$position <- 4
  jaspResults[["tableEquation"]] <- tb
  if (is.null(jaspResults[["doeResult"]])) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]]
}

.doeAnalysisAliasTable <- function(jaspResults, options) {
  if (!is.null(jaspResults[["tableAlias"]]) || !options[["tableAlias"]]) {
    return()
  }
  tb <- createJaspTable(gettext("Alias Structure"))
  tb$addColumnInfo(name = "formula", title = "", type = "string")
  tb$dependOn(options = "tableAlias")
  tb$position <- 5
  jaspResults[["tableAlias"]] <- tb
  if (is.null(jaspResults[["doeResult"]])) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["anova"]]
  name <- names(coef(result[["object"]]))[-1]
  note <- paste(LETTERS[seq_len(length(name))], name, collapse = ";", sep = " = ")
  tb$addFootnote(note)
  #   rows <- data.frame(Aliases = c(FrF2::aliasprint(factorialDesign)$main, FrF2::aliasprint(factorialDesign)$fi2))
}


.doeAnalysisPlotPareto <- function(jaspResults, options) {
  if (!is.null(jaspResults[["plotPareto"]]) || !options[["plotPareto"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Pareto Chart of Standardized Effects"))
  plot$dependOn(options = "plotPareto")
  plot$possition <- 6
  jaspResults[["plotPareto"]] <- plot
  if (is.null(jaspResults[["doeResult"]])) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  if (result[["regression"]][["saturated"]]) {
    plot$setError(gettext("Plot unavailable for saturated designs."))
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

.doeAnalysisPlotQQResiduals <- function(jaspResults, options) {
  if (!is.null(jaspResults[["plotNorm"]]) || !options[["plotNorm"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Normal Probability Plot of Residuals"))
  plot$dependOn(options = "plotNorm")
  plot$possition <- 7
  jaspResults[["plotNorm"]] <- plot
  if (is.null(jaspResults[["doeResult"]])) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  if (result[["regression"]][["saturated"]]) {
    plot$setError(gettext("Plot unavailable for saturated designs."))
    return()
  }
  plot$plotObject <- jaspGraphs::plotQQnorm(resid(result[["object"]]))
}

.doeAnalysisPlotHistResiduals <- function(jaspResults, options) {
  if (!is.null(jaspResults[["plotHist"]]) || !options[["plotHist"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Histogram of Residuals"))
  plot$dependOn(options = "plotHist")
  plot$possition <- 8
  jaspResults[["plotHist"]] <- plot
  if (is.null(jaspResults[["doeResult"]])) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  plot$plotObject <- jaspDescriptives::.plotMarginal(resid(result[["object"]]))
}

.doeAnalysisPlotFittedVsResiduals <- function(jaspResults, options) {
  if (!is.null(jaspResults[["plotFitted"]]) || !options[["plotFitted"]]) {
    return()
  }
  plot <- createJaspPlot(title = gettext("Residuals versus Fitted Values"))
  plot$dependOn(options = "plotFitted")
  plot$possition <- 9
  jaspResults[["plotFitted"]] <- plot
  if (is.null(jaspResults[["doeResult"]])) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  if (result[["regression"]][["saturated"]]) {
    plot$setError(gettext("Plot unavailable for saturated designs."))
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

.doeAnalysisPlotResidualsVsOrder <- function(jaspResults, dataset, options) {
  if (!is.null(jaspResults[["plotRunOrder"]]) || !options[["plotRunOrder"]]) {
    return()
  }
  if (options[["plotOrder"]] == "plotOrderStandard") {
    title <- gettext("Residuals versus Standard Order")
  } else {
    title <- gettext("Residuals versus Run Order")
  }
  plot <- createJaspPlot(title = title)
  plot$dependOn(options = "plotRunOrder")
  plot$possition <- 10
  jaspResults[["plotRunOrder"]] <- plot
  if (is.null(jaspResults[["doeResult"]])) {
    return()
  }
  result <- jaspResults[["doeResult"]]$object[["regression"]]
  if (result[["regression"]][["saturated"]]) {
    plot$setError(gettext("Plot unavailable for saturated designs."))
    return()
  }
  runOrder <- dataset[[options[["runorder"]]]]
  df <- data.frame(order = dataset[[options[["runorder"]]]])
  df[["x"]] <- 1:nrow(df)
  df[["y"]] <- resid(result[["object"]])
  if (options[["plotOrder"]] == "plotOrderStandard") {
    df <- df[order(runOrder), ]
    df$x <- df$runOrder <- 1:nrow(df)
    xlabel <- gettext("Standard order")
  }
  xlabel <- gettext("Run order")
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(df$y)
  xBreaks <- df$x
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    jaspGraphs::geom_point() +
    jaspGraphs::geom_line() +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
    ggplot2::scale_x_continuous(name = xlabel, limits = c(min(xBreaks), max(xBreaks)), breaks = xBreaks, labels = df$runOrder) +
    ggplot2::scale_y_continuous(name = gettext("Residuals"), limits = c(min(yBreaks), max(yBreaks)), breaks = yBreaks) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  plot$plotObject <- p
}

.factorialRegressionANOVAfillTable <- function(factorialRegressionANOVA, factorialRegressionSummaryFit, options, fit, saturated, model) {
  anovaFill$Terms <- jaspBase::gsubInteractionSymbol(anovaFill$Terms)

  factorialRegressionANOVA <- createJaspTable(gettext("Analysis of variance"), position = 1)
  factorialRegressionSummaryFit <- createJaspTable(gettext("Model Summary"), position = 2)

  factorialRegressionANOVA$dependOn(c("FAassignedFactors", "modelTerms", "FAresponse", "intOrder", "FArunOrder", "enabledIntOrder"))
  factorialRegressionSummaryFit$dependOn(c("FAassignedFactors", "modelTerms", "FAresponse", "intOrder", "FArunOrder", "enabledIntOrder"))

  factorialRegressionSummaryFit$addColumnInfo(name = "S", title = "S", type = "number")
  factorialRegressionSummaryFit$addColumnInfo(name = "R1", title = "R-sq", type = "number")
  factorialRegressionSummaryFit$addColumnInfo(name = "R2", title = "R-sq (adj)", type = "number")

  factorialRegressionANOVA$setData(anovaFill)
  factorialRegressionSummaryFit$setData(modelSummaryFill)

  return(list(ANOVA = factorialRegressionANOVA, SummaryFit = factorialRegressionSummaryFit))
}

.factorialRegressionCoefficientsCreateTable <- function(options, ready, fit, saturated, model) {
  factorialRegressionCoefficients <- createJaspTable(gettext("Coefficients"))
  factorialRegressionFormula <- createJaspTable(gettext("Regression equation in uncoded units"))

  factorialRegressionCoefficients$dependOn(c("FAassignedFactors", "modelTerms", "FAresponse", "FArunOrder", "enabledIntOrder"))
  factorialRegressionFormula$dependOn(c("FAassignedFactors", "modelTerms", "FAresponse", "FArunOrder", "enabledIntOrder"))

  if (!is.null(fit) && ready) {
    reponseVar <- unlist(options$FAresponse)
    factors <- names(coef(fit))[!is.na(coef(fit))]
    coefs <- as.vector(coef(fit))[!is.na(coef(fit))]
    plusOrMin <- sapply(1:length(coefs), function(x) {
      if (coefs[x] > 0) "+" else "-"
    })

    formula <- sprintf("%s = %.5g %s %s %.5g %s", reponseVar, coefs[1], factors[1], plusOrMin[2], abs(coefs[2]), factors[2])
    for (i in 3:length(coefs)) {
      formula <- sprintf("%s %s %.5g %s", formula, plusOrMin[i], abs(coefs[i]), factors[i])
    }

    # Replace : by \u00d7
    formula <- jaspBase::gsubInteractionSymbol(formula)

    factorialRegressionFormula$addRows(list(Formula = formula))
  }

  factorialRegressionCoefficients$addColumnInfo(name = "terms", title = "", type = "string")
  factorialRegressionCoefficients$addColumnInfo(name = "coef", title = gettext("Coefficient"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "se", title = gettext("Standard Error"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "t", title = gettext("t"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "p", title = gettext("<i>p</i>-value"), type = "pvalue")
  factorialRegressionCoefficients$addColumnInfo(name = "vif", title = gettext("VIF"), type = "number")

  if (!is.null(fit)) {
    .factorialRegressionCoefficientsFillTable(factorialRegressionCoefficients, options, fit, saturated, model)
  }

  tableCoef <- factorialRegressionCoefficients
  tableFormula <- factorialRegressionFormula

  return(list(tableCoef = tableCoef, tableFormula = tableFormula))
}

.factorialRegressionCoefficientsFillTable <- function(factorialRegressionCoefficients, options, fit, saturated, model) {
  if (!saturated) {
    coefs <- as.data.frame(summary(fit)$coefficients)
    names.64 <- names(coef(fit))
    null.names <- names(fit$coefficients)[is.na(fit$coefficients)]

    vif <- try(car::vif(fit))

    if (inherits(vif, "try-error")) {
      errorMessage <- as.character(vif)
      factorialRegressionCoefficients$setError(errorMessage)
      return()
    }

    coefsFill <- data.frame(
      terms = names.64,
      coef = c(coefs$Estimate, rep(NA, length(null.names))),
      se = c(coefs$`Std. Error`, rep(NA, length(null.names))),
      t = c(coefs$`t value`, rep(NA, length(null.names))),
      p = c(coefs$`Pr(>|t|)`, rep(NA, length(null.names))),
      vif = c(NA, vif, rep(NA, length(null.names)))
    )
  } else {
    coefs <- as.vector(coef(fit))[!is.na(coef(fit))]
    names <- names(coef(fit))[!is.na(coef(fit))]

    coefsFill <- data.frame(
      terms = names,
      coef = coefs,
      se = rep("*", length(names)),
      t = rep("*", length(names)),
      p = rep("*", length(names)),
      vif = rep("*", length(names))
    )
  }

  # Replace : by \u00d7
  coefsFill$terms <- jaspBase::gsubInteractionSymbol(coefsFill$terms)

  factorialRegressionCoefficients$setData(coefsFill)
  return()
}
