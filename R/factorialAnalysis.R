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

factorialAnalysis <- function(jaspResults, dataset, options, ...){

  factorVariables <- unlist(options$FAassignedFactors)
  factorVariables <- factorVariables[factorVariables != ""]

  reorderModelTerms <-  .reorderModelTerms(options)
  modelTerms <- reorderModelTerms$modelTerms
  model <- .modelFormula(modelTerms, options)

  ready <- (length(options[["FAassignedFactors"]]) >= 2 && options[["FAresponse"]] != "" && options[["FArunOrder"]] != "" && !is.null(unlist(options$modelTerms)))

  if(ready)
    dataset <- .factorialAnalysisReadData(dataset, options)

  fit <- .factorialRegression(jaspResults, dataset, options, model, ready)

  if(is.null(jaspResults[["factorialRegressionANOVA"]]))
    .factorialRegressionANOVAcreateTable(jaspResults, options, ready, fit)

  if(is.null(jaspResults[["factorialRegressionCoefficients"]]))
    .factorialRegressionCoefficientsCreateTable(jaspResults, options, ready, fit)

  if(is.null(jaspResults[["showAliasStructure"]]))
    .factorialShowAliasStructure(jaspResults, options, dataset, ready, fit)

  if (is.null(jaspResults[["NormalPlot"]]) && options[["NormalPlot"]])
    .factorialNormalPlot(jaspResults, dataset, options, fit)
}

.factorialAnalysisReadData <- function(dataset, options){

  if(!is.null(dataset)){
    return(dataset)
  } else {
    dataset <-
      if(options[["FArunOrder"]] != ""){
        .readDataSetToEnd(columns.as.numeric = c(options[["FAresponse"]],
                                                 options[["FArunOrder"]],
                                                 options[["FAassignedFactors"]]))
      } else {
        .readDataSetToEnd(columns.as.numeric = c(options[["FAresponse"]],
                                                 options[["FAassignedFactors"]]))
      }
    return(dataset)
  }

}

.factorialRegression <- function(jaspResults, dataset, options, model, ready, ...){

  if(!ready)
    return()

  model.formula <- as.formula(model$model.def)
  fit <- lm(model.formula, dataset)

  if(options[["resNorm"]]){
    if(is.null(jaspResults[["resNorm"]])){
      jaspResults[["resNorm"]] <- createJaspContainer(gettext("Normal Probability Plot of Residuals"))
    }
    jaspResults[["resNorm"]] <- .factorialResNorm(jaspResults = jaspResults, options = options, fit = fit)
    jaspResults[["resNorm"]]$position <- 3
  }

  if(options[["resHist"]]){
    if(is.null(jaspResults[["resHist"]])){
      jaspResults[["resHist"]] <- createJaspContainer(gettext("Histogram of Residuals"))
    }
    jaspResults[["resHist"]] <- .factorialResHist(jaspResults = jaspResults, options = options, fit = fit)
    jaspResults[["resHist"]]$position <- 4
  }

  if(options[["resFitted"]]){
    if(is.null(jaspResults[["resFitted"]])){
      jaspResults[["resFitted"]] <- createJaspContainer(gettext("Residuals vs. Fitted Value"))
    }
    jaspResults[["resFitted"]] <- .factorialResFitted(jaspResults = jaspResults, options = options, fit = fit)
    jaspResults[["resFitted"]]$position <- 5
  }

  if(options[["resOrder"]]){
    if(is.null(jaspResults[["resOrder"]])){
      jaspResults[["resOrder"]] <- createJaspContainer(gettext("Residuals vs. Run Order"))
    }
    jaspResults[["resOrder"]] <- .factorialResOrder(jaspResults = jaspResults, dataset = dataset, options = options, fit = fit)
    jaspResults[["resOrder"]]$position <- 6
  }

  if(options[["paretoPlot"]]){
    if(is.null(jaspResults[["paretoPlot"]])){
      jaspResults[["paretoPlot"]] <- createJaspContainer(gettext("Pareto Plot of Standardized Effects"))
    }
    jaspResults[["paretoPlot"]] <- .factorialPareto(jaspResults = jaspResults, options = options, fit = fit)
    jaspResults[["paretoPlot"]]$position <- 8
  }

  return(fit)
}

.factorialResNorm <- function(jaspResults, options, fit){

  plot <- createJaspPlot(title = "Normal Probability Plot of Residuals", width = 400, height = 400)
  plot$dependOn("resNorm")

  p <- jaspGraphs::plotQQnorm(resid(fit))

  plot$plotObject <- p

  return(plot)
}

.factorialResHist <- function(jaspResults, options, fit){

  plot <- createJaspPlot(title = "Histogram of Residuals", width = 400, height = 400)
  plot$dependOn("resHist")

  x <- resid(fit)
  h <- hist(x, plot = FALSE)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(h$breaks * 1.1)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(h$counts)

  p <- ggplot2::ggplot(data.frame(x), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(binwidth = abs(x[3] - x[1])/15) +
    ggplot2::scale_x_continuous(name = "Residuals", limits = range(xBreaks), breaks = xBreaks) +
    ggplot2::scale_y_continuous(name = "Count", limits = range(yBreaks), breaks = yBreaks)

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}

.factorialResFitted <- function(jaspResults, options, fit){

  plot <- createJaspPlot(title = "Residuals vs. Fitted Value", width = 400, height = 400)
  plot$dependOn("resFitted")

  df <- data.frame(x = fitted(fit), y = resid(fit))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df$x)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(df$y)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
    ggplot2::scale_x_continuous(name = gettext("Fitted values"), limits = c(min(xBreaks), max(xBreaks)), breaks = xBreaks) +
    ggplot2::scale_y_continuous(name = gettext("Residuals"), limits = c(min(yBreaks), max(yBreaks)), breaks = yBreaks)

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}

.factorialResOrder <- function(jaspResults, dataset, options, fit){

  plot <- createJaspPlot(title = "Residuals vs. Run Order", width = 400, height = 400)
  plot$dependOn("resOrder")

  runOrder <- unlist(dataset[,options[["FArunOrder"]]], use.names = FALSE)
  df <- data.frame(runOrder = runOrder, x = 1:length(runOrder),y = resid(fit))

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(df$y)
  xBreaks <- df$x

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
    ggplot2::scale_x_continuous(name = gettext("Run order"), limits = c(min(xBreaks), max(xBreaks)), breaks = xBreaks, labels = df$runOrder) +
    ggplot2::scale_y_continuous(name = gettext("Residuals"), limits = c(min(yBreaks), max(yBreaks)), breaks = yBreaks)

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}

.factorialPareto <- function(jaspResults, options, fit){

  plot <- createJaspPlot(title = "Pareto Plot of Standardized Effects", width = 400, height = 400)
  plot$dependOn("paretoPlot")
  saturated <- summary(fit)$df[2] == 0

  if (!saturated) {
    t <- abs(data.frame(summary(fit)$coefficients)$t.value[-1])
    fac <- names(coef(fit))[-1]
    df <- summary(fit)$df[2]
    crit <- abs(qt(0.025, df))
    yLab <- gettext('Standardized Effect')
  } else {
    t <- as.vector(coef(fit))[!is.na(as.vector(coef(fit)))][-1]
    fac <- names(coef(fit))[!is.na(as.vector(coef(fit)))][-1]
    crit <- unrepx::PSE(t*2, "Lenth")
    yLab <- gettext('Effect')
  }

  fac_t <- cbind.data.frame(fac, t)
  fac_t <- cbind(fac_t[order(fac_t$t),], y = 1:length(t))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(t)

  p <- ggplot2::ggplot(fac_t, ggplot2::aes(y = t, x = y)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = crit, linetype = "dashed", color = "red") +
    ggplot2::labs(x = 'Standardized Effect') +
    ggplot2::scale_x_continuous(name = gettext("Term"), breaks = fac_t$y, labels = fac) +
    ggplot2::scale_y_continuous(name = yLab, breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::coord_flip()

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}

.factorialRegressionANOVAcreateTable <- function(jaspResults, options, ready, fit){

    factorialRegressionANOVA <- createJaspTable(gettext("ANOVA"), position = 1)
    factorialRegressionSummaryFit <- createJaspTable(gettext("Model Summary"), position = 2)

    factorialRegressionANOVA$dependOn(options = c("FAassignedFactors", "modelTerms","FAresponse", "intOrder", "FArunOrder"))
    factorialRegressionSummaryFit$dependOn(options = c("FAassignedFactors", "modelTerms", "FAresponse", "intOrder", "FArunOrder"))

    factorialRegressionANOVA$addColumnInfo(name = "terms", title = "", type = "string")
    factorialRegressionANOVA$addColumnInfo(name = "df", title = gettext("df"), type = "integer")
    factorialRegressionANOVA$addColumnInfo(name = "SS", title = gettext("SS"), type = "number")
    factorialRegressionANOVA$addColumnInfo(name = "adjMS", title = gettext("MS"), type = "number")
    factorialRegressionANOVA$addColumnInfo(name = "F", title = gettext("F"), type = "number")
    factorialRegressionANOVA$addColumnInfo(name = "p", title = gettext("<i>p</i>-value"), type = "pvalue")

    factorialRegressionSummaryFit$addColumnInfo(name = "S", title = "S", type = "number")
    factorialRegressionSummaryFit$addColumnInfo(name = "R1", title = "R-sq", type = "number")
    factorialRegressionSummaryFit$addColumnInfo(name = "R2", title = "R-sq (adj)", type = "number")

    jaspResults[["factorialRegressionANOVA"]] <- factorialRegressionANOVA
    jaspResults[["factorialRegressionSummaryFit"]] <- factorialRegressionSummaryFit

    if(!is.null(fit)) {

      if (summary(fit)$df[2] != 0) {
        .factorialRegressionANOVAfillTable(factorialRegressionANOVA, factorialRegressionSummaryFit, options, fit)

        jaspResults[["factorialRegressionANOVA"]] <- factorialRegressionANOVA
        jaspResults[["factorialRegressionSummaryFit"]] <- factorialRegressionSummaryFit
      } else {
        results <- .factorialRegressionANOVAfillTable(factorialRegressionANOVA, factorialRegressionSummaryFit, options, fit)

        jaspResults[["factorialRegressionANOVA"]] <- results$ANOVA
        jaspResults[["factorialRegressionSummaryFit"]] <- results$SummaryFit
      }
    }
}

.factorialRegressionANOVAfillTable <- function(factorialRegressionANOVA, factorialRegressionSummaryFit, options, fit){

  facotrsName <- unlist(options$FAassignedFactors)
  n.factors <- length(facotrsName)
  errorIndex <- n.factors + 1
  anova <- summary(aov(fit))
  saturated <- summary(fit)$df[2] == 0

  names <- c("Model", names(aov(fit)$coefficients)[-1], "Error", "Total")

  model.SS <- sum(anova[[1]]$`Sum Sq`[-errorIndex])
  model.MS <- sum(anova[[1]]$`Sum Sq`[-errorIndex]) / sum(anova[[1]]$Df[-errorIndex])
  model.F <- model.MS/anova[[1]]$`Mean Sq`[errorIndex]
  model.Pval <- pf(model.F, sum(anova[[1]]$Df[-errorIndex]), anova[[1]]$Df[errorIndex], lower.tail = F)

  if (!saturated) {
    anovaFill <- data.frame(
      terms = names,
      df    = c(sum(anova[[1]]$Df[-errorIndex]), anova[[1]]$Df, sum(anova[[1]]$Df)),
      SS    = c(model.SS, anova[[1]]$`Sum Sq`, sum(anova[[1]]$`Sum Sq`)),
      adjMS = c(model.MS, anova[[1]]$`Mean Sq`, NA),
      `F`   = c(model.F, anova[[1]]$`F value`, NA),
      p     = c(model.Pval, anova[[1]]$`Pr(>F)`, NA)
    )

    modelSummaryFill <- data.frame(
      S = summary(fit)$sigma,
      R1 = summary(fit)$r.squared,
      R2 = summary(fit)$adj.r.squared
    )

    factorialRegressionANOVA$setData(anovaFill)
    factorialRegressionSummaryFit$setData(modelSummaryFill)
    return()
  } else {
    names <- c("Model", names(coef(fit))[!is.na(coef(fit))][-1], "Error", "Total")
    model.SS <- sum(anova[[1]]$`Sum Sq`)
    model.MS <- model.SS / nrow(anova[[1]])

    anovaFill <- data.frame(
      Terms = names,
      df    = c(sum(anova[[1]]$Df[1:n.factors]), anova[[1]]$Df, 0,sum(anova[[1]]$Df)),
      SS    = c(round(c(model.SS, anova[[1]]$`Sum Sq`), 3), "*", round(sum(anova[[1]]$`Sum Sq`), 3)),
      MS    =  c(round(c(model.MS, anova[[1]]$`Mean Sq`), 3), "*", "*"),
      `F`   = rep("*", length(names)),
      p     = rep("*", length(names))
    )

    modelSummaryFill <- data.frame(
      S = NA,
      R1 = NA,
      R2 = NA
    )

    factorialRegressionANOVA <- createJaspTable(gettext("ANOVA"), position = 1)
    factorialRegressionSummaryFit <- createJaspTable(gettext("Model Summary"), position = 2)

    factorialRegressionANOVA$dependOn(options = c("FAassignedFactors", "modelTerms", "FAresponse", "intOrder", "FArunOrder"))
    factorialRegressionSummaryFit$dependOn(options = c("FAassignedFactors", "modelTerms", "FAresponse", "intOrder", "FArunOrder"))

    factorialRegressionSummaryFit$addColumnInfo(name = "S", title = "S", type = "number")
    factorialRegressionSummaryFit$addColumnInfo(name = "R1", title = "R-sq", type = "number")
    factorialRegressionSummaryFit$addColumnInfo(name = "R2", title = "R-sq (adj)", type = "number")

    factorialRegressionANOVA$setData(anovaFill)
    factorialRegressionSummaryFit$setData(modelSummaryFill)

    return(list(ANOVA = factorialRegressionANOVA, SummaryFit = factorialRegressionSummaryFit))
  }
}

.factorialRegressionCoefficientsCreateTable <- function(jaspResults, options, ready, fit){

  factorialRegressionCoefficients <- createJaspTable(gettext("Coefficients"))
  factorialRegressionFormula <- createJaspTable(gettext("Regression equation in uncoded units"))

  factorialRegressionCoefficients$position <- 3
  factorialRegressionCoefficients$position <- 4

  factorialRegressionCoefficients$dependOn(options = c("FAassignedFactors", "modelTerms", "FAresponse", "intOrder"))
  factorialRegressionFormula$dependOn(options = c("FAassignedFactors", "modelTerms", "FAresponse", "intOrder"))

  if (!is.null(fit) && ready) {
    reponseVar <- unlist(options$FAresponse)
    factors <- names(coef(fit))[!is.na(coef(fit))]
    coefs <- as.vector(coef(fit))[!is.na(coef(fit))]
    plusOrMin <- sapply(1:length(coefs), function(x) {if (coefs[x] > 0) "+" else "-"})

    formula <- sprintf("%s = %.5g%s %s %.5g%s", reponseVar, coefs[1], factors[1],plusOrMin[2], abs(coefs[2]), factors[2])
    for (i in 3:length(coefs)) {
      formula <- sprintf("%s %s %.5g%s", formula, plusOrMin[i], abs(coefs[i]), factors[i])
    }

    factorialRegressionFormula$addRows(list(Formula = formula))
  }

  factorialRegressionCoefficients$addColumnInfo(name = "terms", title = "", type = "string")
  factorialRegressionCoefficients$addColumnInfo(name = "coef", title = gettext("Coefficient"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "se", title = gettext("Standard Error"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "t", title = gettext("t"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "p", title = gettext("p"), type = "number")

  if(!is.null(fit))
    .factorialRegressionCoefficientsFillTable(factorialRegressionCoefficients, options, fit)

  jaspResults[["factorialRegressionCoefficients"]] <- factorialRegressionCoefficients
  jaspResults[["factorialRegressionFormula"]] <- factorialRegressionFormula
}

.factorialRegressionCoefficientsFillTable <- function(factorialRegressionCoefficients, options, fit){

  saturated <- summary(fit)$df[2] == 0

  if (!saturated) {
    coefs <- as.data.frame(summary(fit)$coefficients)
    names <- names(coef(fit))

    coefsFill <- data.frame(
      terms = names,
      coef  = coefs$Estimate,
      se    = coefs$`Std. Error`,
      t     = coefs$`t value`,
      p     = coefs$`Pr(>|t|)`)

  } else {
    coefs <- as.vector(coef(fit))[!is.na(coef(fit))]
    names <- names(coef(fit))[!is.na(coef(fit))]

    coefsFill <- data.frame(
      terms = names,
      coef  = coefs,
      se    = rep("*", length(names)),
      t     = rep("*", length(names)),
      p     = rep("*", length(names))
    )
  }

  factorialRegressionCoefficients$setData(coefsFill)

  return()
}

.factorialShowAliasStructure <- function(jaspResults, options, dataset, ready, fit){

  if (!ready)
    return()
  else if (options$showAliasStructure) {

    aliasContainter <- createJaspContainer(gettext("Alias Structure"), position = 5)
    aliasContainter$dependOn(options = c("FAassignedFactors", "modelTerms","FAresponse", "intOrder", "FArunOrder", "showAliasStructure"))

    factorialAliasIndex <- createJaspTable(gettext("Index"))
    factorialAliasIndex$addColumnInfo(name = "Factor", title = gettext("Factor"), type = "string")
    factorialAliasIndex$addColumnInfo(name = "Name", title = gettext("Name"), type = "string")

    Name = names(coef(fit))[-1]

    factorialAliasIndex$setData(list(
      Factor = LETTERS[1:length(Name)],
      Name = Name
    ))

    aliasContainter[["factorialAliasIndex"]] <- factorialAliasIndex

    aliasContainter[["factorialStructure"]] <- .doeFactorialShowAliasStructure(options = options, jaspResults = jaspResults,
                                    factorialDesign = FrF2::FrF2(nruns = max(dataset[, unlist(options$FArunOrder)]),
                                                                 nfactors = length(unlist(options$FAassignedFactors))),
                                    position = 6,
                                    onlyTable = TRUE)

    jaspResults[["aliasStructure"]] <- aliasContainter
  }
}

.factorialNormalPlot <- function( jaspResults, dataset, options, fit){


  jaspResults[["NormalPlot"]] <- createJaspContainer(gettext("Normal Plot of the Standardized Effect"))
  jaspResults[["NormalPlot"]]$dependOn(c("FAassignedFactors", "modelTerms","NormalPlot","FAresponse", "intOrder", "FArunOrder", 'addGridlines'))
  jaspResults[["NormalPlot"]]$position <- 7

  plot <- createJaspPlot(title = "Normal Plot of the Standardized Effect", width = 400, height = 400)

  jaspResults[["NormalPlot"]] <- .qcProbabilityPlot(dataset, options, fit = fit)
}

.modelFormula <- function(modelTerms, options) {

  dependent.normal <- options$FAresponse
  dependent.base64 <- .v(options$FAresponse)
  reorderModelTerms <-  .reorderModelTerms(options)
  modelTerms <- reorderModelTerms$modelTerms

  terms.base64 <- c()
  terms.normal <- c()

  for (term in modelTerms) {

    components <- unlist(term$components)
    term.base64 <- paste(.v(components), collapse=":", sep="")
    term.normal <- paste(components, collapse=" \u273B ", sep="")

    terms.base64 <- c(terms.base64, term.base64)
    terms.normal <- c(terms.normal, term.normal)
  }

  model.def <- paste(dependent.base64, "~", paste(terms.base64, collapse="+"))

  list(model.def = model.def, terms.base64 = terms.base64, terms.normal = terms.normal)
}

.reorderModelTerms <- function(options) {

  if(length(options$modelTerms) > 0) {

    fixedFactors <- list()
    covariates <- list()

    k <- 1
    l <- 1

    for(i in 1:length(options$modelTerms)) {
        fixedFactors[[l]] <- options$modelTerms[[i]]
        l <- l + 1
    }
    modelTerms <- c(fixedFactors, covariates)
    modelTerms <- modelTerms[match(modelTerms, options$modelTerms)]
    interactions <- FALSE


  } else {
    modelTerms <- list()
    interactions <- FALSE
  }

  list(modelTerms = modelTerms, interactions = interactions)
}
