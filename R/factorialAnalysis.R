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

  ready <- (length(options[["FAassignedFactors"]]) >= 2 && !is.null(options[["FAresponse"]]))

  if(ready)
    dataset <- .factorialAnalysisReadData(dataset, options)

  if(is.null(jaspResults[["factorialRegressionANOVA"]]))
    .factorialRegressionANOVAcreateTable(jaspResults, options)

  if(is.null(jaspResults[["factorialRegressionCoefficients"]]))
    .factorialRegressionCoefficientsCreateTable(jaspResults, options)

  .factorialRegression(jaspResults, dataset, options, ready)

}

.factorialAnalysisReadData <- function(dataset, options){

  if(!is.null(dataset))
    return(dataset)
  else
    return(.readDataSetToEnd(columns.as.numeric = c(options[["FArunOrder"]],
                                                    options[["FAresponse"]]),
                             columns.as.factor = options[["FAassignedFactors"]]))

}

.factorialRegression <- function(jaspResults, dataset, options, ready, ...){

  if(!ready)
    return()

  factors <- unlist(dataset[,options[["FAassignedFactors"]]])
  factors <- data.frame(factors)
  response <- unlist(dataset[,options[["FAresponse"]]])

  forFit <- cbind.data.frame(factors, response)
  order <- as.numeric(options[["intOrder"]])

  ifelse(order == 1,
         fit <- lm(response ~., forFit),
         fit <- lm(paste0("response ~ (.)^", order), forFit))

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
    jaspResults[["resOrder"]] <- .factorialResOrder(jaspResults = jaspResults, options = options, fit = fit)
    jaspResults[["resOrder"]]$position <- 6
  }

  return()
}

.factorialResNorm <- function(jaspResults, options, fit){

  plot <- createJaspPlot(title = "Normal Probability Plot of Residuals", width = 500, height = 500)
  plot$dependOn("resNorm")

  cool <- data.frame(fit$residuals, fit$fitted.values)

  p <- ggplot2::ggplot(cool, ggplot2::aes(sample = fit.residuals)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line() +
    ggplot2::coord_flip()

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}

.factorialResHist <- function(jaspResults, options, fit){

  plot <- createJaspPlot(title = "Histogram of Residuals", width = 500, height = 500)
  plot$dependOn("resHist")

  cool <- data.frame(fit$residuals, fit$fitted.values)

  h <- 3.49 * sd(fit$residuals) * length(fit$residuals)^(-1/3)

  p <- ggplot2::ggplot(cool, ggplot2::aes(x = fit.residuals)) +
    ggplot2::geom_histogram(binwidth = h)

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}

.factorialResFitted <- function(jaspResults, options, fit){

  plot <- createJaspPlot(title = "Residuals vs. Fitted Value", width = 500, height = 500)
  plot$dependOn("resFitted")

  cool <- data.frame(fit$residuals, fit$fitted.values)

  p <- ggplot2::ggplot(cool, ggplot2::aes(x = fit.fitted.values, y = fit.residuals)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed")

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}

.factorialResOrder <- function(jaspResults, options, fit){

  plot <- createJaspPlot(title = "Residuals vs. Run Order", width = 500, height = 500)
  plot$dependOn("resOrder")

  cool <- data.frame(fit$residuals, fit$fitted.values)

  p <- ggplot2::ggplot(cool, ggplot2::aes(x = 1:length(fit.residuals), y = fit.residuals)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed")

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}

.factorialRegressionANOVAcreateTable <- function(jaspResults, options){

  factorialRegressionANOVA <- createJaspTable(gettext("ANOVA"))
  factorialRegressionANOVA$position <- 1

  factorialRegressionANOVA$dependOn(c("FAassginedFactors",
                                           "FAresponse"))

  factorialRegressionANOVA$addColumnInfo(name = "terms", title = "", type = "string")
  factorialRegressionANOVA$addColumnInfo(name = "df", title = gettext("df"), type = "integer")
  factorialRegressionANOVA$addColumnInfo(name = "SS", title = gettext("SS"), type = "number")
  factorialRegressionANOVA$addColumnInfo(name = "adjMS", title = gettext("MS"), type = "number")
  factorialRegressionANOVA$addColumnInfo(name = "F", title = gettext("F"), type = "number")
  factorialRegressionANOVA$addColumnInfo(name = "p", title = gettext("p"), type = "number")

  jaspResults[["factorialRegressionANOVA"]] <- factorialRegressionANOVA
}

.factorialRegressionCoefficientsCreateTable <- function(jaspResults, options){

  factorialRegressionCoefficients <- createJaspTable(gettext("Coefficients"))
  factorialRegressionCoefficients$position <- 2

  factorialRegressionCoefficients$dependOn(c("FAassginedFactors",
                                           "FAresponse"))

  factorialRegressionCoefficients$addColumnInfo(name = "terms", title = "", type = "string")
  factorialRegressionCoefficients$addColumnInfo(name = "effect", title = gettext("Effect"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "unst", title = gettext("Unstandardized"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "se", title = gettext("Standard Error"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "st", title = gettext("Standardized"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "t", title = gettext("t"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "p", title = gettext("p"), type = "number")

  jaspResults[["factorialRegressionCoefficients"]] <- factorialRegressionCoefficients
}
