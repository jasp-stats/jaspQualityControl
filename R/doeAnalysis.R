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
  ################### Analysis ###################
  factorVariables <- unlist(options$FAassignedFactors)
  factorVariables <- factorVariables[factorVariables != ""]

  reorderModelTerms <- .reorderModelTerms(options)
  modelTerms <- reorderModelTerms$modelTerms
  model <- .modelFormula(modelTerms, options)

  # Ready check: More than two factors, a response and order variables, and either model terms or interaction order specified.
  ready <- (length(options[["FAassignedFactors"]]) >= 2 && options[["FAresponse"]] != "" && (!is.null(unlist(options$modelTerms)) || options$enabledIntOrder))
  if (ready) {
    dataset <- .factorialAnalysisReadData(dataset, options)

    # Transform to coded
    factors <- unlist(dataset[, options[["FAassignedFactors"]]], use.names = FALSE)
    response <- unlist(dataset[, options[["FAresponse"]]], use.names = FALSE)

    perF <- length(factors) / length(options[["FAassignedFactors"]])
    factorsDF <- data.frame(split(factors, ceiling(seq_along(factors) / perF)))

    for (i in 1:ncol(factorsDF)) {
      factorsDF[, i][factorsDF[, i] == min(factorsDF[, i])] <- -1
      factorsDF[, i][factorsDF[, i] == max(factorsDF[, i])] <- 1
    }

    # Use original variables names

    if (options[["FArunOrder"]] != "") {
      runOrder <- dataset[, unlist(options$FArunOrder)]
      names <- c(options$FArunOrder, options$FAassignedFactors, options$FAresponse)
      datasetRow <- cbind.data.frame(runOrder, factorsDF, response)
    } else {
      names <- c(options$FAassignedFactors, options$FAresponse)
      datasetRow <- cbind.data.frame(factorsDF, response)
    }

    names(datasetRow) <- names

    dataset <- datasetRow
  }

  model.fit <- .factorialRegression(jaspResults, dataset, options, model, ready)
  fit <- model.fit$fit
  saturated <- model.fit$saturated

  analysisContainer <- createJaspContainer("Analysis", position = 2)

  if (is.null(jaspResults[["factorialRegressionANOVA"]])) {
    analysisContainer[["tableAnova"]] <- .factorialRegressionANOVAcreateTable(options, ready, fit, saturated, model)$tableAnova
    analysisContainer[["tableFit"]] <- .factorialRegressionANOVAcreateTable(options, ready, fit, saturated, model)$tableFit
  }

  if (is.null(jaspResults[["factorialRegressionCoefficients"]])) {
    analysisContainer[["tableCoef"]] <- .factorialRegressionCoefficientsCreateTable(options, ready, fit, saturated, model)$tableCoef
    analysisContainer[["tableFormula"]] <- .factorialRegressionCoefficientsCreateTable(options, ready, fit, saturated, model)$tableFormula
  }

  if (ready) {
    jaspResults[["Design"]] <- NULL

    # Create error plot for saturated designs
    if (saturated) {
      errorPlot <- createJaspPlot()
      errorPlot$setError(gettext("This plot is unavailable for saturated designs."))
      errorPlot$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder"))
    }

    if (options[["showAliasStructure2"]]) {
      # Error plot for saturated designs
      analysisContainer[["showAliasStructure2"]] <- .factorialShowAliasStructure(jaspResults, options, dataset, fit)
    }


    # Normal Probability Plot of Residuals
    if (is.null(jaspResults[["resNorm"]]) && options[["resNorm"]]) {
      NormalPlot <- createJaspContainer(gettext("Normal Probability Plot of Residuals"))
      NormalPlot$dependOn(c("FAassignedFactors", "modelTerms", "NormalPlot", "FAresponse", "intOrder", "FArunOrder", "addGridlines", "enabledIntOrder"))

      # Error plot for saturated designs
      if (saturated) {
        NormalPlot[["plot"]] <- errorPlot
      } else {
        NormalPlot[["plot"]] <- .qcProbabilityPlot(dataset, options, fit = fit)
      }

      analysisContainer[["resNorm"]] <- NormalPlot
    }

    # Histogram of Residuals
    if (options[["resHist"]] && is.null(jaspResults[["resHist"]])) {
      resHist <- createJaspContainer(gettext("Histogram of Residuals"))
      resHist$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder"))

      if (saturated) {
        resHist[["plot"]] <- errorPlot
      } else {
        resHist[["plot"]] <- .factorialResHist(jaspResults = jaspResults, options = options, fit = fit)$jaspPlot
      }

      analysisContainer[["resHist"]] <- resHist
    }

    # Residuals vs. Fitted Value
    if (options[["resFitted"]] && is.null(jaspResults[["resFitted"]])) {
      resFitted <- createJaspContainer(gettext("Residuals vs. Fitted Value"))
      resFitted$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder"))

      if (saturated) {
        resFitted[["plot"]] <- errorPlot
      } else {
        resFitted[["plot"]] <- .factorialResFitted(jaspResults = jaspResults, options = options, fit = fit)$jaspPlot
      }

      analysisContainer[["resFitted"]] <- resFitted
    }

    # Residuals vs. Run Order
    if (options[["resOrder"]] && is.null(jaspResults[["resOrder"]])) {
      if (options$runOrderPlot == "runOrderRandomPlot") {
        resOrder <- createJaspContainer(gettext("Residuals vs. Run Order"))
      } else {
        resOrder <- createJaspContainer(gettext("Residuals vs. Standard Order"))
      }

      resOrder$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder"))

      if (saturated) {
        resOrder[["plot"]] <- errorPlot
      } else {
        resOrder[["plot"]] <- .factorialResOrder(jaspResults = jaspResults, dataset = dataset, options = options, fit = fit)$jaspPlot
      }

      analysisContainer[["resOrder"]] <- resOrder
    }

    # Pareto Plot of Standardized Effects
    if (options[["paretoPlot"]] && is.null(jaspResults[["paretoPlot"]])) {
      if (saturated) {
        analysisContainer[["paretoPlot"]] <- errorPlot
      } else {
        analysisContainer[["paretoPlot"]] <- .factorialPareto(jaspResults = jaspResults, options = options, fit = fit)
      }
    }

    # Four in one plot
    if (options[["fourInOne"]] && is.null(jaspResults[["fourInOne"]])) {
      if (saturated) {
        fourInOne[["fourInOne"]] <- errorPlot
      } else {
        matrixPlot <- createJaspPlot(title = gettextf("Four in one for %s", unlist(options$FAresponse)), width = 1100, aspectRatio = 1)
        matrixPlot$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder", "fourInOne"))

        plotMat <- matrix(list(), 2, 2)
        plotMat[[1, 1]] <- .qcProbabilityPlot(dataset, options, fit = fit, ggPlot = TRUE)
        plotMat[[1, 2]] <- .factorialResFitted(jaspResults = jaspResults, options = options, fit = fit)$ggPlot
        plotMat[[2, 1]] <- .factorialResHist(jaspResults = jaspResults, options = options, fit = fit)$ggPlot
        plotMat[[2, 2]] <- .factorialResOrder(jaspResults = jaspResults, dataset = dataset, options = options, fit = fit)$ggPlot

        matrixPlot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)
      }

      analysisContainer[["fourInOne"]] <- matrixPlot
    }
    jaspResults[["Analysis"]] <- analysisContainer
  }
}

.factorialAnalysisReadData <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  } else {
    dataset <-
      if (options[["FArunOrder"]] != "") {
        .readDataSetToEnd(columns.as.numeric = c(
          options[["FAresponse"]],
          options[["FArunOrder"]],
          options[["FAassignedFactors"]]
        ))
      } else {
        .readDataSetToEnd(columns.as.numeric = c(
          options[["FAresponse"]],
          options[["FAassignedFactors"]]
        ))
      }

    # Error check
    .hasErrors(dataset,
      type = c("infinity", "missingValues"),
      all.target = c(options[["FAresponse"]], options[["FArunOrder"]], options[["FAassignedFactors"]]),
      exitAnalysisIfErrors = TRUE
    )
    return(dataset)
  }
}

.factorialRegression <- function(jaspResults, dataset, options, model, ready, ...) {
  if (!ready) {
    return()
  }

  if (options$enabledIntOrder) {
    reponseName <- unlist(options$FAresponse)
    factorsNames <- unlist(options$FAassignedFactors)
    runOrderName <- unlist(options$FArunOrder)

    factors <- unlist(dataset[, options[["FAassignedFactors"]]], use.names = FALSE)
    response <- unlist(dataset[, options[["FAresponse"]]], use.names = FALSE)

    perF <- length(factors) / length(options[["FAassignedFactors"]])
    factorsDF <- data.frame(split(factors, ceiling(seq_along(factors) / perF)))
    forFit <- cbind.data.frame(factorsDF, response)

    names <- factorsNames
    colnames(forFit) <- c(names, "response")

    order <- as.numeric(options[["intOrder"]])

    fit <- if (order == 1) {
      lm(response ~ ., forFit)
    } else {
      lm(paste0("response ~ (.)^", order), forFit)
    }
  } else {
    model.formula <- as.formula(model$model.def)
    fit <- lm(model.formula, dataset)
  }
  saturated <- summary(fit)$df[2] == 0

  return(list(fit = fit, saturated = saturated))
}

.factorialResNorm <- function(jaspResults, options, fit) {
  plot <- createJaspPlot(width = 400, height = 400)
  plot$dependOn("resNorm")

  p <- jaspGraphs::plotQQnorm(resid(fit))

  plot$plotObject <- p

  return(list(jaspPlot = plot, ggPlot = p))
}

.factorialResHist <- function(jaspResults, options, fit) {
  plot <- createJaspPlot(width = 400, height = 400)
  plot$dependOn("resHist")

  x <- resid(fit)
  h <- hist(x, plot = FALSE)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(h$breaks * 1.1)
  yBreaks <- c(0, jaspGraphs::getPrettyAxisBreaks(h$counts))

  p <- ggplot2::ggplot(data.frame(x), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(fill = "grey", col = "black") +
    ggplot2::scale_x_continuous(name = "Residuals", limits = range(xBreaks), breaks = xBreaks) +
    ggplot2::scale_y_continuous(name = "Frequency", limits = range(yBreaks), breaks = yBreaks)

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(list(jaspPlot = plot, ggPlot = p))
}

.factorialResFitted <- function(jaspResults, options, fit) {
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

.factorialResOrder <- function(jaspResults, dataset, options, fit) {
  plot <- createJaspPlot(width = 400, height = 400)
  plot$dependOn("resOrder")

  runOrder <- unlist(dataset[, options[["FArunOrder"]]], use.names = FALSE)

  df <- data.frame(runOrder = runOrder, x = 1:length(runOrder), y = resid(fit))

  if (options$runOrderPlot == "runOrderStandardPlot") {
    df <- df[order(runOrder), ]
    df$x <- df$runOrder <- 1:nrow(df)
    xlabel <- gettext("Standard order")
  }

  xlabel <- gettext("Run order")
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(df$y)
  xBreaks <- df$x

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
    ggplot2::scale_x_continuous(name = xlabel, limits = c(min(xBreaks), max(xBreaks)), breaks = xBreaks, labels = df$runOrder) +
    ggplot2::scale_y_continuous(name = gettext("Residuals"), limits = c(min(yBreaks), max(yBreaks)), breaks = yBreaks)

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(list(jaspPlot = plot, ggPlot = p))
}

.factorialPareto <- function(jaspResults, options, fit, onlyPlot = FALSE) {
  plot <- createJaspPlot(title = gettext("Pareto Chart of Standardized Effects"), width = 400, height = 400)
  plot$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder", "paretoPlot"))

  t <- abs(data.frame(summary(fit)$coefficients)$t.value[-1])
  fac <- names(coef(fit))[-1]
  df <- summary(fit)$df[2]
  crit <- abs(qt(0.025, df))
  yLab <- gettext("Standardized Effect")

  fac_t <- cbind.data.frame(fac, t)
  fac_t <- cbind(fac_t[order(fac_t$t), ], y = 1:length(t))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(t, crit))

  p <- ggplot2::ggplot(fac_t, ggplot2::aes(y = t, x = y)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = crit, linetype = "dashed", color = "red") +
    ggplot2::scale_x_continuous(name = gettext("Term"), breaks = fac_t$y, labels = fac_t$fac) +
    ggplot2::scale_y_continuous(name = yLab, breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::coord_flip()

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  if (onlyPlot) {
    return(p)
  } else {
    return(plot)
  }
}

.factorialRegressionANOVAcreateTable <- function(options, ready, fit, saturated, model) {
  if (!is.null(fit) && ready) {
    results <- .factorialRegressionANOVAfillTable(factorialRegressionANOVA, factorialRegressionSummaryFit, options, fit, saturated, model)
    tableAnova <- results$ANOVA
    tableFit <- results$SummaryFit
  } else {
    tableAnova <- createJaspTable(gettext("Analysis of variance"), position = 1)
    tableFit <- createJaspTable(gettext("Model summary"), position = 2)
  }

  return(list(tableAnova = tableAnova, tableFit = tableFit))
}

.factorialRegressionANOVAfillTable <- function(factorialRegressionANOVA, factorialRegressionSummaryFit, options, fit, saturated, model) {
  facotrsName <- unlist(options$FAassignedFactors)
  response <- unlist(options$FAresponse)
  n.factors <- length(facotrsName)

  if (!saturated) {
    anova <- car::Anova(fit)
    anova$`Mean Sq` <- anova$`Sum Sq` / anova$Df
  } else {
    anova <- summary(aov(fit))
    anova <- anova[[1]]
  }

  names.64 <- names(coef(fit))
  null.names <- names(fit$coefficients)[is.na(fit$coefficients)]
  names <- c("Model", gsub(" ", "", row.names(anova)[-length(row.names(anova))], fixed = TRUE), null.names, "Error", "Total")
  anovaNames <- gsub(" ", "", row.names(anova))
  errorIndex <- which(anovaNames == "Residuals")

  model.SS <- sum(anova$`Sum Sq`[-errorIndex])
  model.MS <- sum(anova$`Sum Sq`[-errorIndex]) / sum(anova$Df[-errorIndex])
  model.F <- model.MS / anova$`Mean Sq`[errorIndex]
  model.Pval <- pf(model.F, sum(anova$Df[-errorIndex]), anova$Df[errorIndex], lower.tail = F)

  # Pure error for replicates
  fit.names <- names(fit$model)[-1]
  formula.facotrsName <- fit.names[1]
  for (i in 2:length(facotrsName)) {
    formula.facotrsName <- gettextf("%s,%s", formula.facotrsName, fit.names[i])
  } # create RSM model formula

  formula.rsm <- gettextf("%s ~ FO(%s)", names(fit$model)[1], formula.facotrsName)
  fit.rsm <- rsm::rsm(as.formula(formula.rsm), fit$model) # fit rsm model
  rsm <- summary(fit.rsm)
  anova.rsm <- rsm[[13]] # anova table

  if (!saturated) {
    df <- c(sum(anova$Df[-errorIndex]), anova$Df[-errorIndex], rep(NA, length(null.names)), anova$Df[errorIndex], sum(anova$Df))
    Adj.SS <- c(model.SS, anova$`Sum Sq`[-errorIndex], rep(NA, length(null.names)), anova$`Sum Sq`[errorIndex], sum(anova$`Sum Sq`))
    Adj.MS <- c(model.MS, anova$`Mean Sq`[-errorIndex], rep(NA, length(null.names)), anova$`Mean Sq`[errorIndex], NA)
    `F` <- c(model.F, anova$`F value`, rep(NA, length(null.names)), NA)
    p <- c(model.Pval, anova$`Pr(>F)`, rep(NA, length(null.names)), NA)

    anovaFill <- data.frame(
      Terms = names,
      df = round(df, 3),
      Adj.SS = round(Adj.SS, 3),
      Adj.MS = round(Adj.MS, 3),
      `F` = round(`F`, 3),
      p = round(p, 3)
    )

    # If Pure error
    if (anova.rsm[3, ][1] != 0 && anova.rsm[4, ][1] != 0) {
      LackFit <- t(as.data.frame(c(terms = "Lack of fit", round(unlist(anova.rsm[3, ]), 3))))
      Pure.Error <- t(as.data.frame(c(terms = "Pure error", round(unlist(anova.rsm[4, ]), 3))))
      Total.index <- length(names)
      colnames(LackFit) <- colnames(anovaFill)
      colnames(Pure.Error) <- colnames(anovaFill)
      anovaFill.Total <- anovaFill[Total.index, ]
      anovaFill <- rbind(anovaFill[-Total.index, ], LackFit, Pure.Error, anovaFill[Total.index, ])
    }

    modelSummaryFill <- data.frame(
      S = summary(fit)$sigma,
      R1 = summary(fit)$r.squared * 100,
      R2 = summary(fit)$adj.r.squared * 100
    )
  } else {
    model.SS <- sum(anova$`Sum Sq`)
    model.MS <- model.SS / nrow(anova)
    names <- c("Model", names(coef(fit))[!is.na(coef(fit))][-1], "Error", "Total")

    anovaFill <- data.frame(
      Terms = names,
      df = c(sum(anova$Df[1:n.factors]), anova$Df, 0, sum(anova$Df)),
      Adj.SS = c(round(c(model.SS, anova$`Sum Sq`), 3), "*", round(sum(anova$`Sum Sq`), 3)),
      Adj.MS = c(round(c(model.MS, anova$`Mean Sq`), 3), "*", "*"),
      `F` = rep("*", length(names)),
      p = rep("*", length(names))
    )

    modelSummaryFill <- data.frame(
      S = NA,
      R1 = NA,
      R2 = NA
    )
  }

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

.factorialShowAliasStructure <- function(jaspResults, options, dataset, fit) {
  aliasContainter <- createJaspContainer(gettext("Alias Structure"))
  aliasContainter$dependOn(c("FAassignedFactors", "modelTerms", "FAresponse", "intOrder", "FArunOrder", "showAliasStructure2", "enabledIntOrder"))

  factorialAliasIndex <- createJaspTable(gettext("Index"))
  factorialAliasIndex$addColumnInfo(name = "Factor", title = gettext("Factor"), type = "string")
  factorialAliasIndex$addColumnInfo(name = "Name", title = gettext("Name"), type = "string")


  # Index
  Name <- names(coef(fit))[-1]

  factorialAliasIndex$setData(list(
    Factor = LETTERS[1:length(Name)],
    Name = Name
  ))
  aliasContainter[["factorialAliasIndex"]] <- factorialAliasIndex

  # Structure
  table <- createJaspTable(gettext("Alias Structure"))
  table$dependOn(c("FAassignedFactors", "modelTerms", "FAresponse", "intOrder", "FArunOrder", "showAliasStructure", "enabledIntOrder"))
  aliasContainter[["factorialAliasStructure"]] <- table

  factorialDesign <- try(FrF2::FrF2(
    nruns = max(dataset[, unlist(options$FArunOrder)]),
    nfactors = length(unlist(options$FAassignedFactors))
  ))

  if (inherits(factorialDesign, "try-error")) {
    errorMessage <- as.character(factorialDesign)
    table$setError(errorMessage)
    return(aliasContainter)
  }

  rows <- data.frame(Aliases = c(FrF2::aliasprint(factorialDesign)$main, FrF2::aliasprint(factorialDesign)$fi2))

  table$setData(rows)

  return(aliasContainter)
}

.modelFormula <- function(modelTerms, options) {
  dependent.normal <- options$FAresponse
  dependent.base64 <- .v(options$FAresponse)
  reorderModelTerms <- .reorderModelTerms(options)
  modelTerms <- reorderModelTerms$modelTerms

  terms.base64 <- c()
  terms.normal <- c()

  for (term in modelTerms) {
    components <- unlist(term$components)
    term.base64 <- paste(.v(components), collapse = ":", sep = "")
    term.normal <- paste(components, collapse = " \u00d7 ", sep = "")

    terms.base64 <- c(terms.base64, term.base64)
    terms.normal <- c(terms.normal, term.normal)
  }

  model.def <- paste(dependent.base64, "~", paste(terms.base64, collapse = "+"))

  list(model.def = model.def, terms.base64 = terms.base64, terms.normal = terms.normal)
}

.reorderModelTerms <- function(options) {
  if (length(options$modelTerms) > 0) {
    fixedFactors <- list()
    covariates <- list()

    k <- 1
    l <- 1

    for (i in 1:length(options$modelTerms)) {
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
