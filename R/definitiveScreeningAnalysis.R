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

definitiveScreeningAnalysis <- function(jaspResults, dataset, options, ...){

  ready <- (length(options[["DSAassignedFactors"]]) > 3 && !is.null(options[["DSAresponse"]]))

  if(ready)
    dataset <- .defScreenAnalysisReadData(dataset, options)

  fit <- .defScreenRegression(jaspResults, dataset, options, ready)

  # if(is.null(jaspResults[["defScreenRegressionANOVA"]]))
  #   .defScreenRegressionANOVAcreateTable(jaspResults, options, ready, fit)

  if(is.null(jaspResults[["defScreenRegressionCoefficients"]]))
    .defScreenRegressionCoefficientsCreateTable(jaspResults, options, ready, fit)

}

.defScreenAnalysisReadData <- function(dataset, options){

  if(!is.null(dataset)){
    return(dataset)
  } else {
    dataset <-
      if(options[["DSArunOrder"]]!=""){
      .readDataSetToEnd(columns.as.numeric = c(options[["DSAresponse"]],
                                               options[["DSArunOrder"]],
                                               options[["DSAassignedFactors"]]))
    } else {
      .readDataSetToEnd(columns.as.numeric = c(options[["DSAresponse"]],
                                               options[["DSAassignedFactors"]]))
    }
    return(dataset)
  }
}

.defScreenRegression <- function(jaspResults, dataset, options, ready){

  if(!ready)
    return()

  factors <- unlist(dataset[,options[["DSAassignedFactors"]]], use.names = FALSE)
  response <- unlist(dataset[,options[["DSAresponse"]]], use.names = FALSE)
  perF <- length(factors) / length(options[["DSAassignedFactors"]])
  factorsDF <- data.frame(split(factors, ceiling(seq_along(factors) / perF)))

  colnames(factorsDF) <- LETTERS[1:ncol(factorsDF)]

  copyFitDefSc <- body(daewr::FitDefSc)
  copyFitDefSc[[length(copyFitDefSc)]] <- quote(return(so))
  newFitDefSc <- as.function(c(formals(daewr::FitDefSc), copyFitDefSc), asNamespace('daewr'))

  fit <- newFitDefSc(response, factorsDF, alpha = options[["DSAalpha"]])

  if(options[["DSresNorm"]]){
    if(is.null(jaspResults[["DSresNorm"]])){
      jaspResults[["DSresNorm"]] <- createJaspContainer(gettext("Normal Probability Plot of Residuals"))
    }
    jaspResults[["DSresNorm"]] <- .defScreenResNorm(jaspResults = jaspResults, options = options, fit = fit)
    jaspResults[["DSresNorm"]]$position <- 3
  }

  if(options[["DSresHist"]]){
    if(is.null(jaspResults[["DSresHist"]])){
      jaspResults[["DSresHist"]] <- createJaspContainer(gettext("Histogram of Residuals"))
    }
    jaspResults[["DSresHist"]] <- .defScreenResHist(jaspResults = jaspResults, options = options, fit = fit)
    jaspResults[["DSresHist"]]$position <- 4
  }

  if(options[["DSresFitted"]]){
    if(is.null(jaspResults[["DSresFitted"]])){
      jaspResults[["DSresFitted"]] <- createJaspContainer(gettext("Residuals vs. Fitted Value"))
    }
    jaspResults[["DSresFitted"]] <- .defScreenResFitted(jaspResults = jaspResults, options = options, fit = fit)
    jaspResults[["DSresFitted"]]$position <- 5
  }

  if(options[["DSresOrder"]]){
    if(is.null(jaspResults[["DSresOrder"]])){
      jaspResults[["DSresOrder"]] <- createJaspContainer(gettext("Residuals vs. Run Order"))
    }
    jaspResults[["DSresOrder"]] <- .defScreenResOrder(jaspResults = jaspResults, dataset = dataset, options = options, fit = fit)
    jaspResults[["DSresOrder"]]$position <- 6
  }

  if(options[["DSparetoPlot"]]){
    if(is.null(jaspResults[["DSparetoPlot"]])){
      jaspResults[["DSparetoPlot"]] <- createJaspContainer(gettext("Pareto Plot of Standardized Effects"))
    }
    jaspResults[["DSparetoPlot"]] <- .defScreenPareto(jaspResults = jaspResults, options = options, fit = fit)
    jaspResults[["DSparetoPlot"]]$position <- 7
  }

  return(fit)

}

.defScreenResNorm <- function(jaspResults, options, fit){

  plot <- createJaspPlot(title = "Normal Probability Plot of Residuals", width = 400, height = 400)
  plot$dependOn("DSresNorm")

  p <- jaspGraphs::plotQQnorm(resid(fit))

  plot$plotObject <- p

  return(plot)
}

.defScreenResHist <- function(jaspResults, options, fit){

  plot <- createJaspPlot(title = "Histogram of Residuals", width = 400, height = 400)
  plot$dependOn("DSresHist")

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

.defScreenResFitted <- function(jaspResults, options, fit){

  plot <- createJaspPlot(title = "Residuals vs. Fitted Value", width = 400, height = 400)
  plot$dependOn("DSresFitted")

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

.defScreenResOrder <- function(jaspResults, dataset, options, fit){

  plot <- createJaspPlot(title = "Residuals vs. Run Order", width = 400, height = 400)
  plot$dependOn("DSresOrder")

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

.defScreenPareto <- function(jaspResults, options, fit){

  plot <- createJaspPlot(title = "Pareto Plot of Standardized Effects", width = 400, height = 400)
  plot$dependOn("DSparetoPlot")

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

.defScreenRegressionCoefficientsCreateTable <- function(jaspResults, options, ready, fit){

  defScreenRegressionCoefficients <- createJaspTable(gettext("Coefficients"))
  defScreenRegressionCoefficients$position <- 2

  defScreenRegressionCoefficients$dependOn(options = c("FAassginedFactors",
                                                       "FAresponse",
                                                       "intOrder"))

  defScreenRegressionCoefficients$addColumnInfo(name = "terms", title = "", type = "string")
  defScreenRegressionCoefficients$addColumnInfo(name = "coef", title = gettext("Coefficient"), type = "number")
  defScreenRegressionCoefficients$addColumnInfo(name = "se", title = gettext("Standard Error"), type = "number")
  defScreenRegressionCoefficients$addColumnInfo(name = "t", title = gettext("t"), type = "number")
  defScreenRegressionCoefficients$addColumnInfo(name = "p", title = gettext("p"), type = "number")

  if(!is.null(fit))
    .defScreenRegressionCoefficientsFillTable(defScreenRegressionCoefficients, options, fit)

  jaspResults[["defScreenRegressionCoefficients"]] <- defScreenRegressionCoefficients
}

.defScreenRegressionCoefficientsFillTable <- function(defScreenRegressionCoefficients, options, fit){

  coefs <- coef(fit)
  names <- row.names(coef(fit))

  print(fit)
  print(coefs)
  print(names)

  coefsFill <- data.frame(
    terms = names,
    coef  = coefs[,1],
    se    = coefs[,2],
    t     = coefs[,3],
    p     = coefs[,4]
  )

  print(coefsFill)

  defScreenRegressionCoefficients$setData(coefsFill)

  return()
}
