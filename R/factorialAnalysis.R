#
# Copyright (C) 2013-2018 University of Amsterdam
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

  ready <- (length(options[["FAassignedFactors"]]) >= 2 && !is.null(options[["FAresponse"]]) && !is.null(options[["FArunOrder"]]))

  if(ready)
    dataset <- .factorialAnalysisReadData(dataset, options)

  fit <- .factorialRegression(jaspResults, dataset, options, ready)

  if(is.null(jaspResults[["factorialRegressionANOVA"]]))
    .factorialRegressionANOVAcreateTable(jaspResults, options, ready, fit)

  if(is.null(jaspResults[["factorialRegressionCoefficients"]]))
    .factorialRegressionCoefficientsCreateTable(jaspResults, options, ready, fit)
}

.factorialAnalysisReadData <- function(dataset, options){

  if(!is.null(dataset))
    return(dataset)
  else
    return(.readDataSetToEnd(columns.as.numeric = c(options[["FArunOrder"]], options[["FAresponse"]]),
                             columns.as.factor = options[["FAassignedFactors"]]))

}

.factorialRegression <- function(jaspResults, dataset, options, ready, ...){

  if(!ready)
    return()

  factors <- unlist(dataset[,options[["FAassignedFactors"]]], use.names = FALSE)
  response <- unlist(dataset[,options[["FAresponse"]]], use.names = FALSE)

  perF <- length(factors) / length(options[["FAassignedFactors"]])
  factorsDF <- data.frame(split(factors, ceiling(seq_along(factors) / perF)))
  forFit <- cbind.data.frame(factorsDF, response)

  names <- LETTERS[1:length(factors)]
  colnames(forFit) <- c(names, "response")

  order <- as.numeric(options[["intOrder"]])

  fit <- if(order == 1){
    lm(response ~., forFit)
  } else {
    lm(paste0("response ~ (.)^", order), forFit)
  }

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
    jaspResults[["paretoPlot"]]$position <- 7
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

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(h$breaks)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(h$counts)

  p <- ggplot2::ggplot(data.frame(x), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(binwidth = abs(h$breaks[1] - h$breaks[2])) +
    ggplot2::scale_x_continuous(name = "Residuals", limits = c(min(xBreaks), max(xBreaks)), breaks = xBreaks) +
    ggplot2::scale_y_continuous(name = "Count", limits = c(min(yBreaks), max(yBreaks)), breaks = yBreaks)

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
  df <- data.frame(x = runOrder, y = resid(fit))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df$x)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(df$y)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
    ggplot2::scale_x_continuous(name = gettext("Run order"), limits = c(min(xBreaks), max(xBreaks)), breaks = xBreaks) +
    ggplot2::scale_y_continuous(name = gettext("Residuals"), limits = c(min(yBreaks), max(yBreaks)), breaks = yBreaks)

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}

.factorialPareto <- function(jaspResults, options, fit){

  plot <- createJaspPlot(title = "Pareto Plot of Standardized Effects", width = 400, height = 400)
  plot$dependOn("paretoPlot")

  t <- abs(data.frame(summary(fit)$coefficients)$t.value[-1])
  fac <- names(coef(fit))[-1]
  fac_t <- cbind.data.frame(fac, t)
  fac_t <- fac_t[rev(order(fac_t$t)),]

  df <- summary(fit)$df[2]
  crit <- abs(qt(0.025, df))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(t)

  p <- ggplot2::ggplot(fac_t, ggplot2::aes(y = fac)) +
    ggplot2::geom_bar(ggplot2::aes(x = t), stat = "identity") +
    ggplot2::geom_vline(xintercept = crit, linetype = "dashed", color = "red") +
    ggplot2::labs(x = 'Standardized Effect', y ='Term') +
    ggplot2::scale_x_continuous(name = gettext("Standardized Effect"), limits = c(min(xBreaks), max(xBreaks)), breaks = xBreaks)

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}

.factorialRegressionANOVAcreateTable <- function(jaspResults, options, ready, fit){

  factorialRegressionANOVA <- createJaspTable(gettext("ANOVA"))
  factorialRegressionANOVA$position <- 1

  factorialRegressionANOVA$dependOn(options = c("FAassginedFactors",
                                                "FAresponse",
                                                "intOrder"))

  factorialRegressionANOVA$addColumnInfo(name = "terms", title = "", type = "string")
  factorialRegressionANOVA$addColumnInfo(name = "df", title = gettext("df"), type = "integer")
  factorialRegressionANOVA$addColumnInfo(name = "SS", title = gettext("SS"), type = "number")
  factorialRegressionANOVA$addColumnInfo(name = "adjMS", title = gettext("MS"), type = "number")
  factorialRegressionANOVA$addColumnInfo(name = "F", title = gettext("F"), type = "number")
  factorialRegressionANOVA$addColumnInfo(name = "p", title = gettext("p"), type = "number")

  if(!is.null(fit))
    .factorialRegressionANOVAfillTable(factorialRegressionANOVA, options, fit)

  jaspResults[["factorialRegressionANOVA"]] <- factorialRegressionANOVA

}

.factorialRegressionANOVAfillTable <- function(factorialRegressionANOVA, options, fit){

  anova <- summary(aov(fit))

  names <- c(names(aov(fit)$coefficients)[-1], "Residuals")

  anovaFill <- data.frame(
    terms = names,
    df    = anova[[1]]$Df,
    SS    = anova[[1]]$`Sum Sq`,
    adjMS = anova[[1]]$`Mean Sq`,
    `F`   = anova[[1]]$`F value`,
    p     = anova[[1]]$`Pr(>F)`
  )

  factorialRegressionANOVA$setData(anovaFill)

  return()
}

.factorialRegressionCoefficientsCreateTable <- function(jaspResults, options, ready, fit){

  factorialRegressionCoefficients <- createJaspTable(gettext("Coefficients"))
  factorialRegressionCoefficients$position <- 2

  factorialRegressionCoefficients$dependOn(options = c("FAassginedFactors",
                                                       "FAresponse",
                                                       "intOrder"))

  factorialRegressionCoefficients$addColumnInfo(name = "terms", title = "", type = "string")
  factorialRegressionCoefficients$addColumnInfo(name = "coef", title = gettext("Coefficient"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "se", title = gettext("Standard Error"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "t", title = gettext("t"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "p", title = gettext("p"), type = "number")

  if(!is.null(fit))
    .factorialRegressionCoefficientsFillTable(factorialRegressionCoefficients, options, fit)

  jaspResults[["factorialRegressionCoefficients"]] <- factorialRegressionCoefficients
}

.factorialRegressionCoefficientsFillTable <- function(factorialRegressionCoefficients, options, fit){

  coefs <- as.data.frame(summary(fit)$coefficients)

  names <- names(coef(fit))

  coefsFill <- data.frame(
    terms = names,
    coef  = coefs$Estimate,
    se    = coefs$`Std. Error`,
    t     = coefs$`t value`,
    p     = coefs$`Pr(>|t|)`
  )

  factorialRegressionCoefficients$setData(coefsFill)

  return()
}
