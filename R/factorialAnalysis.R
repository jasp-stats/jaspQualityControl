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

  # Ready check: More than two factors, a response and order variables, and either model terms or interaction order specified.
  ready <- (length(options[["FAassignedFactors"]]) >= 2 && options[["FAresponse"]] != "" && options[["FArunOrder"]] != ""
            && (!is.null(unlist(options$modelTerms)) || options$enabledIntOrder))

  if(ready)
    dataset <- .factorialAnalysisReadData(dataset, options)

  model.fit <- .factorialRegression(jaspResults, dataset, options, model, ready)
  fit <- model.fit$fit
  saturated <- model.fit$saturated

  if(is.null(jaspResults[["factorialRegressionANOVA"]]))
    .factorialRegressionANOVAcreateTable(jaspResults, options, ready, fit, saturated)

  if(is.null(jaspResults[["factorialRegressionCoefficients"]]))
    .factorialRegressionCoefficientsCreateTable(jaspResults, options, ready, fit, saturated)

  if(is.null(jaspResults[["showAliasStructure"]]))
    .factorialShowAliasStructure(jaspResults, options, dataset, ready, fit)

  if (ready) {

    # Normal Plot of Standardized Effects
    if (is.null(jaspResults[["NormalPlot"]]) && options[["NormalPlot"]])
      .factorialNormalPlot(jaspResults, dataset, options, fit, saturated)

    # Create error plot for saturated designs
    if (saturated){
      errorPlot <- createJaspPlot()
      errorPlot$setError(gettext("This plot is unavailable for saturated designs."))
      errorPlot$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder"))
    }

    # Normal Probability Plot of Residuals
    if(options[["resNorm"]] && is.null(jaspResults[["resNorm"]])){
      resNorm <- createJaspContainer(gettext("Normal Probability Plot of Residuals"))
      resNorm$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder"))
      resNorm$position <- 8

      # Error plot for saturated designs
      if (saturated)
        resNorm[["plot"]] <- errorPlot
      else
        resNorm[["plot"]] <- .factorialResNorm(jaspResults = jaspResults, options = options, fit = fit)$jaspPlot

      jaspResults[["resNorm"]] <- resNorm
    }

    #Histogram of Residuals
    if(options[["resHist"]] && is.null(jaspResults[["resHist"]])){
      resHist <- createJaspContainer(gettext("Histogram of Residuals"))
      resHist$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder"))
      resHist$position <- 9

      if (saturated)
        resHist[["plot"]] <- errorPlot
      else
        resHist[["plot"]] <- .factorialResHist(jaspResults = jaspResults, options = options, fit = fit)$jaspPlot

      jaspResults[["resHist"]] <- resHist
    }

    # Residuals vs. Fitted Value
    if(options[["resFitted"]] && is.null(jaspResults[["resFitted"]])){
      resFitted <- createJaspContainer(gettext("Residuals vs. Fitted Value"))
      resFitted$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder"))
      resFitted$position <- 10

      if (saturated)
        resFitted[["plot"]] <- errorPlot
      else
        resFitted[["plot"]] <- .factorialResFitted(jaspResults = jaspResults, options = options, fit = fit)$jaspPlot

      jaspResults[["resFitted"]] <- resFitted
    }

    # Residuals vs. Run Order
    if(options[["resOrder"]] && is.null(jaspResults[["resOrder"]])){
      resOrder <- createJaspContainer(gettext("Residuals vs. Run Order"))
      resOrder$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder"))
      resOrder$position <- 11

      if (saturated)
        resOrder[["plot"]] <- errorPlot
      else
        resOrder[["plot"]] <- .factorialResOrder(jaspResults = jaspResults, dataset = dataset ,options = options, fit = fit)$jaspPlot

      jaspResults[["resOrder"]] <- resOrder
    }

    # Pareto Plot of Standardized Effects
    if(options[["paretoPlot"]] && is.null(jaspResults[["paretoPlot"]])){
      paretoPlot <- createJaspContainer(gettext("Pareto Plot of Standardized Effects"))
      paretoPlot$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder"))
      paretoPlot$position <- 7

      if (saturated)
        paretoPlot[["plot"]] <- errorPlot
      else
        paretoPlot[["plot"]] <- .factorialPareto(jaspResults = jaspResults, options = options, fit = fit)

      jaspResults[["paretoPlot"]] <- paretoPlot
    }

    # Four in one plot
    if(options[["fourInOne"]] && is.null(jaspResults[["fourInOne"]])){
      fourInOne <- createJaspContainer(gettextf("Residual Plots for %s", unlist(options$FAresponse)))
      fourInOne$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder", "fourInOne"))
      fourInOne$position <- 7

      if (saturated)
        fourInOne[["plot"]] <- errorPlot
      else {
        matrixPlot <- createJaspPlot(width = 800, height = 800)
        plotMat <- matrix(list(), 2, 2)
        plotMat[[1, 1]] <- .factorialResNorm(jaspResults = jaspResults, options = options, fit = fit)$ggPlot
        plotMat[[1, 2]] <- .factorialResFitted(jaspResults = jaspResults, options = options, fit = fit)$ggPlot
        plotMat[[2, 1]] <- .factorialResHist(jaspResults = jaspResults, options = options, fit = fit)$ggPlot
        plotMat[[2, 2]] <- .factorialResOrder(jaspResults = jaspResults, dataset = dataset ,options = options, fit = fit)$ggPlot

        matrixPlot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)
        fourInOne[["plot"]] <- matrixPlot
      }

      jaspResults[["fourInOne"]] <- fourInOne
    }
  }
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

  if (options$enabledIntOrder) {
    reponseName <- unlist(options$FAresponse)
    factorsNames <- unlist(options$FAassignedFactors)
    runOrderName <- unlist(options$FArunOrder)

    factors <- unlist(dataset[,options[["FAassignedFactors"]]], use.names = FALSE)
    response <- unlist(dataset[,options[["FAresponse"]]], use.names = FALSE)

    perF <- length(factors) / length(options[["FAassignedFactors"]])
    factorsDF <- data.frame(split(factors, ceiling(seq_along(factors) / perF)))
    forFit <- cbind.data.frame(factorsDF, response)

    names <- factorsNames
    colnames(forFit) <- c(names, "response")

    order <- as.numeric(options[["intOrder"]])

    fit <- if(order == 1)
      lm(response ~., forFit)
    else
      lm(paste0("response ~ (.)^", order), forFit)

  } else {

    model.formula <- as.formula(model$model.def)
    fit <- lm(model.formula, dataset)
  }
  saturated <- summary(fit)$df[2] == 0

  return(list(fit = fit, saturated = saturated))
}

.factorialResNorm <- function(jaspResults, options, fit){

  plot <- createJaspPlot(width = 400, height = 400)
  plot$dependOn("resNorm")

  p <- jaspGraphs::plotQQnorm(resid(fit))

  plot$plotObject <- p

  return(list(jaspPlot = plot, ggPlot = p))
}

.factorialResHist <- function(jaspResults, options, fit){

  plot <- createJaspPlot(width = 400, height = 400)
  plot$dependOn("resHist")

  x <- resid(fit)
  h <- hist(x, plot = FALSE)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(h$breaks * 1.1)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(h$counts)
  yLabs <- round((yBreaks / sum(yBreaks)) * 100, 0)

  p <- ggplot2::ggplot(data.frame(x), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram() +
    ggplot2::scale_x_continuous(name = "Residuals", limits = range(xBreaks), breaks = xBreaks) +
    ggplot2::scale_y_continuous(name = "Percent", limits = range(yBreaks), breaks = yBreaks, labels = yLabs)

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(list(jaspPlot = plot, ggPlot = p))
}

.factorialResFitted <- function(jaspResults, options, fit){

  plot <- createJaspPlot(width = 400, height = 400)
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

  return(list(jaspPlot = plot, ggPlot = p))
}

.factorialResOrder <- function(jaspResults, dataset, options, fit){

  plot <- createJaspPlot(width = 400, height = 400)
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

  return(list(jaspPlot = plot, ggPlot = p))
}

.factorialPareto <- function(jaspResults, options, fit){

  plot <- createJaspPlot(width = 400, height = 400)
  plot$dependOn("paretoPlot")

  t <- abs(data.frame(summary(fit)$coefficients)$t.value[-1])
  fac <- names(coef(fit))[-1]
  df <- summary(fit)$df[2]
  crit <- abs(qt(0.025, df))
  yLab <- gettext('Standardized Effect')

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

.factorialRegressionANOVAcreateTable <- function(jaspResults, options, ready, fit, saturated){

    factorialRegressionANOVA <- createJaspTable(gettext("Analysis of variance"), position = 1)
    factorialRegressionSummaryFit <- createJaspTable(gettext("Model Summary"), position = 2)

    factorialRegressionANOVA$dependOn(c("FAassignedFactors", "modelTerms","FAresponse", "intOrder", "FArunOrder", "enabledIntOrder"))
    factorialRegressionSummaryFit$dependOn(c("FAassignedFactors", "modelTerms", "FAresponse", "intOrder", "FArunOrder", "enabledIntOrder"))

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

    if(!is.null(fit) && ready) {

      if (!saturated) {
        .factorialRegressionANOVAfillTable(factorialRegressionANOVA, factorialRegressionSummaryFit, options, fit, saturated)

        jaspResults[["factorialRegressionANOVA"]] <- factorialRegressionANOVA
        jaspResults[["factorialRegressionSummaryFit"]] <- factorialRegressionSummaryFit
      } else {
        results <- .factorialRegressionANOVAfillTable(factorialRegressionANOVA, factorialRegressionSummaryFit, options, fit, saturated)

        jaspResults[["factorialRegressionANOVA"]] <- results$ANOVA
        jaspResults[["factorialRegressionSummaryFit"]] <- results$SummaryFit
      }
    }
}

.factorialRegressionANOVAfillTable <- function(factorialRegressionANOVA, factorialRegressionSummaryFit, options, fit, saturated){

  facotrsName <- unlist(options$FAassignedFactors)
  n.factors <- length(facotrsName)
  errorIndex <- n.factors + 1
  anova <- summary(aov(fit))

  names.64 <- names(coef(fit))
  null.names <- names(fit$coefficients)[is.na(fit$coefficients)]
  names <- c("Model", gsub(" ", "", row.names(anova[[1]])[-length(row.names(anova[[1]]))], fixed = TRUE), null.names,"Error", "Total")

  model.SS <- sum(anova[[1]]$`Sum Sq`[-errorIndex])
  model.MS <- sum(anova[[1]]$`Sum Sq`[-errorIndex]) / sum(anova[[1]]$Df[-errorIndex])
  model.F <- model.MS/anova[[1]]$`Mean Sq`[errorIndex]
  model.Pval <- pf(model.F, sum(anova[[1]]$Df[-errorIndex]), anova[[1]]$Df[errorIndex], lower.tail = F)

  if (!saturated) {

    anovaFill <- data.frame(
      terms = names,
      df    = c(sum(anova[[1]]$Df[-errorIndex]),anova[[1]]$Df[-errorIndex], rep(NA, length(null.names)),anova[[1]]$Df[errorIndex],sum(anova[[1]]$Df)),
      SS    = c(model.SS, anova[[1]]$`Sum Sq`[-errorIndex], rep(NA, length(null.names)), anova[[1]]$`Sum Sq`[errorIndex],sum(anova[[1]]$`Sum Sq`)),
      adjMS = c(model.MS, anova[[1]]$`Mean Sq`[-errorIndex], rep(NA, length(null.names)), anova[[1]]$`Mean Sq`[errorIndex],NA),
      `F`   = c(model.F, anova[[1]]$`F value`, rep(NA, length(null.names)),NA),
      p     = c(model.Pval, anova[[1]]$`Pr(>F)`,rep(NA, length(null.names)), NA)
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
    model.SS <- sum(anova[[1]]$`Sum Sq`)
    model.MS <- model.SS / nrow(anova[[1]])
    names <- c("Model", names(coef(fit))[!is.na(coef(fit))][-1], "Error", "Total")

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
}

.factorialRegressionCoefficientsCreateTable <- function(jaspResults, options, ready, fit, saturated){

  factorialRegressionCoefficients <- createJaspTable(gettext("Coefficients"))
  factorialRegressionFormula <- createJaspTable(gettext("Regression equation in uncoded units"))

  factorialRegressionCoefficients$position <- 3
  factorialRegressionFormula$position <- 4

  factorialRegressionCoefficients$dependOn(c("FAassignedFactors", "modelTerms", "FAresponse", "FArunOrder", "enabledIntOrder"))
  factorialRegressionFormula$dependOn(c("FAassignedFactors", "modelTerms", "FAresponse", "FArunOrder", "enabledIntOrder"))

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
    .factorialRegressionCoefficientsFillTable(factorialRegressionCoefficients, options, fit, saturated)

  jaspResults[["factorialRegressionCoefficients"]] <- factorialRegressionCoefficients
  jaspResults[["factorialRegressionFormula"]] <- factorialRegressionFormula
}

.factorialRegressionCoefficientsFillTable <- function(factorialRegressionCoefficients, options, fit, saturated){

  if (!saturated) {
    coefs <- as.data.frame(summary(fit)$coefficients)
    names.64 <- names(coef(fit))
    null.names <- names(fit$coefficients)[is.na(fit$coefficients)]

    coefsFill <- data.frame(
      terms = names.64,
      coef  = c(coefs$Estimate, rep(NA, length(null.names)) ),
      se    = c(coefs$`Std. Error`, rep(NA, length(null.names))),
      t     = c(coefs$`t value`, rep(NA, length(null.names))),
      p     = c(coefs$`Pr(>|t|)`, rep(NA, length(null.names))))

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
    aliasContainter$dependOn(c("FAassignedFactors", "modelTerms","FAresponse", "intOrder", "FArunOrder", "showAliasStructure", "enabledIntOrder"))

    factorialAliasIndex <- createJaspTable(gettext("Index"))
    factorialAliasIndex$addColumnInfo(name = "Factor", title = gettext("Factor"), type = "string")
    factorialAliasIndex$addColumnInfo(name = "Name", title = gettext("Name"), type = "string")


    Name = names(coef(fit))[-1]

    factorialAliasIndex$setData(list(
      Factor = LETTERS[1:length(Name)],
      Name = Name
    ))

    aliasContainter[["factorialAliasIndex"]] <- factorialAliasIndex

    if (any(max(dataset[, unlist(options$FArunOrder)]) != 2^(1:100))) {
      errorTable <- createJaspTable(gettext("Alias Structure Error"))
      errorTable$dependOn(c("FAassignedFactors", "modelTerms","FAresponse", "intOrder", "FArunOrder", "showAliasStructure", "enabledIntOrder"))
      errorTable$setError(gettext("The number of runs must be a power of 2."))

      jaspResults[["aliasStructure"]] <- errorTable
      return()
    }

    aliasContainter[["factorialStructure"]] <- .doeFactorialShowAliasStructure(options = options, jaspResults = jaspResults,
                                    factorialDesign = FrF2::FrF2(nruns = max(dataset[, unlist(options$FArunOrder)]),
                                                                 nfactors = length(unlist(options$FAassignedFactors))),
                                    position = 6,
                                    onlyTable = TRUE)

    jaspResults[["aliasStructure"]] <- aliasContainter
  }
}

.factorialNormalPlot <- function( jaspResults, dataset, options, fit, saturated){


  NormalPlot <- createJaspContainer(gettext("Normal Plot of Standardized Effects"))
  NormalPlot$dependOn(c("FAassignedFactors", "modelTerms","NormalPlot","FAresponse", "intOrder", "FArunOrder", 'addGridlines', "enabledIntOrder"))
  NormalPlot$position <- 6

  if (saturated) {
    plot <- createJaspPlot()
    plot$setError(gettext("Normal Plot of Standardized Effects is unavailable for saturated designs."))
    NormalPlot[["plot"]] <- plot
    jaspResults[["NormalPlot"]] <- NormalPlot
    return()
  }

  NormalPlot[["plot"]] <- .qcProbabilityPlot(dataset, options, fit = fit)
  jaspResults[["NormalPlot"]] <- NormalPlot
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
