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

#' @export
msaGaugeLinearity <- function(jaspResults, dataset, options, ...) {

  measurements <- unlist(options[["measurement"]])
  parts <- unlist(options[["part"]])
  standards <- unlist(options[["standard"]])

  ready <- (!identical(measurements, "") && !identical(parts, "") && !identical(standards, ""))

  numeric.vars <- c(measurements, standards)
  numeric.vars <- numeric.vars[numeric.vars != ""]

  factor.vars <- parts
  factor.vars <- factor.vars[factor.vars != ""]

  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = numeric.vars, columns.as.factor = factor.vars,
                                         exclude.na.listwise = c(numeric.vars, factor.vars))
  }

  # Linearity and Bias Analysis

  if (is.null(jaspResults[["LB"]])) {
    jaspResults[["LB"]] <- createJaspContainer(gettext("Linearity and bias"))
    jaspResults[["LB"]]$position <- 1
  }
  jaspResults[["LB"]] <- .linearityAndBias(ready = ready, dataset = dataset, options = options, measurements = measurements, parts = parts, standards = standards)


  return()
}

.linearityAndBias <- function(ready, dataset, options, measurements, parts, standards) {

  tablesAndGraphs <- createJaspContainer(gettext("Linearity and bias"))

  tableGaugeBias <- createJaspTable(title = gettext("Gauge bias"))
  tableGaugeBias$addColumnInfo(name = "part",  title = gettext("Part"), type = "string")
  tableGaugeBias$addColumnInfo(name = "referenceValue",  title = gettext("Reference value"), type = "number")
  tableGaugeBias$addColumnInfo(name = "observedMean", title = gettext("Observed mean"), type = "number")
  tableGaugeBias$addColumnInfo(name = "bias",            title = gettext("Mean bias"), type = "number")
  tableGaugeBias$addColumnInfo(name = "pvalue",            title = gettext("<i>p</i>-value"), type = "pvalue")
  if (options[["manualProcessVariation"]])
    tableGaugeBias$addColumnInfo(name = "percentBias", title = gettext("Percent bias"), type = "number")

  tableRegression <- createJaspTable(title = gettext("Regression model"))
  tableRegression$addColumnInfo(name = "predictor",  title = gettext("Predictor"), type = "string")
  tableRegression$addColumnInfo(name = "coefficient", title = gettext("Coefficient"), type = "number")
  tableRegression$addColumnInfo(name = "SEcoefficient", title = gettext("Std. error"), type = "number")
  tableRegression$addColumnInfo(name = "Tvalues", title = gettext("<i>t</i>-value"), type = "number")
  tableRegression$addColumnInfo(name = "pvalue",            title = gettext("<i>p</i>-value"), type = "pvalue")

  tableEquation <- createJaspTable(gettext("Regression equation"))
  tableEquation$addColumnInfo(name = "formula", title = "", type = "string")

  tableGaugeLinearity <- createJaspTable(title = gettext("Gauge linearity"))

  tableGaugeLinearity$addColumnInfo(name = "S",  title = gettext("Std. error"), type = "number")
  tableGaugeLinearity$addColumnInfo(name = "rsq",       title = gettextf("R%1$s", "\u00B2"), type = "number")
  if (options[["manualProcessVariation"]]) {
    tableGaugeLinearity$addColumnInfo(name = "linearity", title = gettext("Linearity"), type = "number")
    tableGaugeLinearity$addColumnInfo(name = "percentLin", title = gettextf("%% Linearity"), type = "number")
  }

  plotBias <- createJaspPlot(title = gettext("Bias and linearity"), width = 500, height = 500)
  plotProcessVar <- createJaspPlot(title = gettext("Percentage process variation graph"), width = 500, height = 500)


  if (options[["biasTable"]])
    tablesAndGraphs[["tableGaugeBias"]] <- tableGaugeBias


  if (options[["linearityTable"]]) {
    tablesAndGraphs[["tableRegression"]] <- tableRegression
    tablesAndGraphs[["tableEquation"]] <- tableEquation
    tablesAndGraphs[["tableGaugeLinearity"]] <- tableGaugeLinearity
  }

  if (options[["linearityAndBiasPlot"]])
    tablesAndGraphs[["plotBias"]] <- plotBias

  if (options[["percentProcessVariationPlot"]])
    tablesAndGraphs[["plotProcessVar"]] <- plotProcessVar

  if (!ready)
    return(tablesAndGraphs)

  # Error conditions
  if (length(dataset[[measurements]]) < 2) {
    tableRegression$setError(gettextf("t-test requires more than 1 measurement. %1$i valid measurement(s) detected in %2$s.", length(dataset[[measurements]]), measurements))
    return(tableRegression)
  }
  if (length(unique(dataset[[standards]])) != length(unique(dataset[[parts]]))) {
    tableRegression$setError(gettextf("Every unique part must have one corresponding reference value. %1$i reference values were found for %2$s unique parts.", length(unique(dataset[[standards]])), length(unique(dataset[[parts]]))))
    return(tableRegression)
  }
  if (any(table(dataset[[parts]]) < 2)) {
    singleMeasurementParts <- paste(names(which(table(dataset[[parts]]) < 2)), collapse = ", ")
    tableRegression$setError(gettextf("t-test requires more than 1 measurement per part. Less than 2 valid measurement(s) detected in Part(s) %s.", singleMeasurementParts))
    return(tableRegression)
  }
  variancePerPart <- tapply(dataset[[measurements]], dataset[[parts]], var)
  if (any(variancePerPart == 0)) {
    noVarParts <- paste(names(which(variancePerPart == 0)), collapse = ", ")
    tableRegression$setError(gettextf("t-test not possible. No variance detected in Part(s) %s.", noVarParts))
    return(tableRegression)
  }

  partsVec <- c()
  refsVec <- c()
  observedMeansVec <- c()
  biasesVec <- c()
  pValuesVec <- c()
  for (part in unique(dataset[[parts]])) {
    currentData <- dataset[dataset[[parts]] == part,]
    currentRef <- unique(currentData[[standards]]) # Check above establishes that length == 1
    currentMean <- mean(currentData[[measurements]], na.rm = TRUE)
    currentBias <- currentMean - currentRef
    currentPVal <-  t.test(currentData[[measurements]] - currentRef, mu = 0)$p.value

    partsVec <- c(partsVec, part)
    refsVec <- c(refsVec, currentRef)
    observedMeansVec <- c(observedMeansVec, currentMean)
    biasesVec <- c(biasesVec, currentBias)
    pValuesVec <- c(pValuesVec, currentPVal)
  }

  biasDf <- data.frame("Part" = partsVec,
                       "Ref" = refsVec,
                       "ObservedMean" = observedMeansVec,
                       "Bias" = biasesVec,
                       "pvalue" = pValuesVec)

  rawBiases <- dataset[[measurements]] - dataset[[standards]]
  meanBias <- mean(rawBiases)

  biasPlotDf <- data.frame("Bias" = rawBiases, "Ref" = dataset[[standards]])
  biasPlotMeanDf <- data.frame("meanBias" = biasesVec, "Ref" = refsVec)
  regressionFitRaw <- lm(Bias ~ Ref, biasPlotDf)
  regressionFit <- summary(regressionFitRaw)

  totalBiasPValueModel <- car::linearHypothesis(regressionFitRaw, c("(Intercept)" = 1, "reference" = mean(biasPlotDf[["Ref"]])))
  totalBiasPValue <- totalBiasPValueModel[["Pr(>F)"]][2]
  regressionFitConstant <- regressionFit$coefficients[1]
  regressionFitSlope <- regressionFit$coefficients[2]
  coefficients <- c(regressionFitConstant, regressionFitSlope)
  coefficientsSE <- regressionFit$coefficients[c(3,4)]
  regressionFitTvalues <- regressionFit$coefficients[c(5,6)]
  regressionFitPvalues <- regressionFit$coefficients[c(7,8)]
  regressionFitSigma <- regressionFit$sigma
  regressionFitRsq <- regressionFit$r.squared
  plusOrMin <- if (regressionFitSlope > 0) "+" else "-"
  regressionEquation <- gettextf("Bias = %1$.2f %2$s %3$.2f * Reference value", regressionFitConstant, plusOrMin, abs(regressionFitSlope))

  plotBiasPlotObject <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, lty = 2, color = "grey") +
    ggplot2::geom_smooth(data = biasPlotDf, mapping = ggplot2::aes(x = Ref, y = Bias), method = "lm", color = "red") +
    ggplot2::scale_x_continuous(name = gettext("Reference")) +
    ggplot2::geom_point(data = biasPlotDf, mapping = ggplot2::aes(x = Ref, y = Bias), size = 4, shape = "X") +
    ggplot2::geom_point(data = biasPlotMeanDf, mapping = ggplot2::aes(x = Ref, y = meanBias), fill = "red", shape = 21, col = "black", size = 4) +
    ggplot2::scale_y_continuous(limits = c(min(biasPlotDf$Bias, na.rm = TRUE), max(biasPlotDf$Bias, na.rm = TRUE) * 2)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plotBias$plotObject <- plotBiasPlotObject

  tableRegression$setData(list(
    "predictor" = c("Intercept", "Slope"),
    "coefficient" = coefficients,
    "Tvalues" = regressionFitTvalues,
    "SEcoefficient" = coefficientsSE,
    "pvalue" = regressionFitPvalues))

  regressionEquationRow <- data.frame(formula = regressionEquation)
  tableEquation$addRows(regressionEquationRow)

  gaugeBiasDataList <- list("part" = c(biasDf[["Part"]], gettext("Total")),
                            "referenceValue" = biasDf[["Ref"]],
                            "observedMean" = biasDf[["ObservedMean"]],
                            "bias" = c(biasDf[["Bias"]], meanBias),
                            "pvalue" = c(biasDf[["pvalue"]], totalBiasPValue))
  gaugeLinearityDataList <- list("S" = regressionFitSigma,
                                 "rsq" = regressionFitRsq)


  if (options[["manualProcessVariation"]]) {
    percentBiasParts <- abs(biasDf[["Bias"]]) / options[["manualProcessVariationValue"]] * 100
    percentBiasTotal <- abs(meanBias) / options[["manualProcessVariationValue"]] * 100
    gaugeBiasDataList[["percentBias"]] <- c(percentBiasParts, percentBiasTotal)

    linearity <- abs(regressionFitSlope) * options[["manualProcessVariationValue"]]
    percentLin <- (linearity / options[["manualProcessVariationValue"]]) * 100

    gaugeLinearityDataList[["linearity"]] <- linearity
    gaugeLinearityDataList[["percentLin"]] <- percentLin

    plotProcessVarDf <- data.frame("Source" = factor(x = c("Linearity", "Bias"), levels = c("Linearity", "Bias")),
                                   "Percent" = c(percentLin, percentBiasTotal)
    )
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, plotProcessVarDf[["Percent"]]))
    yLimits <- range(yBreaks)

    plotProcessVarPlotObject <- ggplot2::ggplot() +
      ggplot2::geom_col(data = plotProcessVarDf, mapping = ggplot2::aes(x = Source, y = Percent), fill = "gray", col = "black", linewidth = 1) +
      ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits) +
      ggplot2::xlab("") +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()

    plotProcessVar$plotObject <- plotProcessVarPlotObject
  }

  tableGaugeBias$setData(gaugeBiasDataList)
  tableGaugeLinearity$setData(gaugeLinearityDataList)

  return(tablesAndGraphs)
}
