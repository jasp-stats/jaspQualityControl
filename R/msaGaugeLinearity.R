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
  standards <- unlist(options$standard)

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

  table1 <- createJaspTable(title = gettext("Gauge bias"))

  table1$addColumnInfo(name = "part",  title = gettext("Part"), type = "string")
  table1$addColumnInfo(name = "referenceValue",  title = gettext("Reference value"), type = "number")
  table1$addColumnInfo(name = "observedMean", title = gettext("Observed mean"), type = "number")
  table1$addColumnInfo(name = "bias",            title = gettext("Mean bias"), type = "number")
  table1$addColumnInfo(name = "pvalue",            title = gettext("<i>p</i>-value"), type = "pvalue")


  table2 <- createJaspTable(title = gettext("Regression model"))

  table2$addColumnInfo(name = "predictor",  title = gettext("Predictor"), type = "string")
  table2$addColumnInfo(name = "coefficient", title = gettext("Coefficient"), type = "number")
  table2$addColumnInfo(name = "SEcoefficient", title = gettext("Std. error"), type = "number")
  table2$addColumnInfo(name = "Tvalues", title = gettext("<i>t</i>-value"), type = "number")
  table2$addColumnInfo(name = "pvalue",            title = gettext("<i>p</i>-value"), type = "pvalue")

  tableEquation <- createJaspTable(gettext("Regression equation"))
  tableEquation$addColumnInfo(name = "formula", title = "", type = "string")

  table3 <- createJaspTable(title = gettext("Gauge linearity"))

  table3$addColumnInfo(name = "S",  title = gettext("Std. error"), type = "number")
  table3$addColumnInfo(name = "rsq",       title = gettextf("R%1$s", "\u00B2"), type = "number")
  table3$addColumnInfo(name = "percentLin", title = gettextf("%% Linearity"), type = "number")

  Note <- gettext("Calculations are based on 6 times the historical standard variation unless specified otherwise.")
  table1$addFootnote(Note)
  table2$addFootnote(Note)
  table3$addFootnote(Note)

  plot1 <- createJaspPlot(title = gettext("Bias and linearity"), width = 500, height = 500)

  plot2 <- createJaspPlot(title = gettext("Percentage process variation graph"), width = 500, height = 500)

  if (ready) {

    # Error conditions
    if (length(dataset[[measurements]]) < 2){
      table2$setError(gettextf("t-test requires more than 1 measurement. %1$i valid measurement(s) detected in %2$s.", length(dataset[[measurements]]), measurements))
      return(table2)
    }
    else if (length(unique(dataset[[standards]])) != length(unique(dataset[[parts]]))) {
      table2$setError(gettextf("Every unique part must have one corresponding reference value. %1$i reference values were found for %2$s unique parts.", length(unique(dataset[[standards]])), length(unique(dataset[[parts]]))))
      return(table2)
    } else if(any(table(dataset[[parts]]) < 2)) {
      singleMeasurementParts <- paste(names(which(table(dataset[[parts]]) < 2)), collapse = ", ")
      table2$setError(gettextf("t-test requires more than 1 measurement per part. Less than 2 valid measurement(s) detected in Part(s) %s.", singleMeasurementParts))
      return(table2)
    }
    variancePerPart <- tapply(dataset[[measurements]], dataset[[parts]], var)
    if(any(variancePerPart == 0)) {
      noVarParts <- paste(names(which(variancePerPart == 0)), collapse = ", ")
      table2$setError(gettextf("t-test not possible. No variance detected in Part(s) %s.", noVarParts))
      return(table2)
    } else if(any(table(dataset[[parts]]) < 2)) {
      singleMeasurementParts <- paste(names(which(table(dataset[[parts]]) < 2)), collapse = ", ")
      table2$setError(gettextf("t-test requires more than 1 measurement per part. Less than 2 valid measurement(s) detected in Part(s) %s.", singleMeasurementParts))
      return(table2)
    }
    variancePerPart <- tapply(dataset[[measurements]], dataset[[parts]], var)
    if(any(variancePerPart == 0)) {
      noVarParts <- paste(names(which(variancePerPart == 0)), collapse = ", ")
      table2$setError(gettextf("t-est not possible. No variance detected in part(s) %s.", noVarParts))
      return(table2)
    }

    ReferenceValues <- unique(dataset[[standards]])
    df <- data.frame()
    biases <- vector()
    Part <- vector()
    references <- vector()

    for (i in ReferenceValues) {
      ReferenceValue <- i
      ReferenceData <- subset.data.frame(dataset, dataset[[standards]] == i)
      Ref <- ReferenceData[[standards]][1]
      Part <- unique(ReferenceData[[parts]])
      ObservedMean <- mean(ReferenceData[[measurements]])
      Bias <-  ObservedMean - Ref
      pvalue <- t.test(ReferenceData[[measurements]] - Ref, mu = 0)$p.value
      df <- rbind(df, list(Part = Part,Ref = rep(Ref,length(Part)), ObservedMean =  rep(ObservedMean,length(Part)), Bias =  rep(Bias,length(Part)), pvalue =  rep(pvalue,length(Part))))
      biases <- c(biases, ReferenceData[[measurements]] - Ref)
      references <- c(references, ReferenceData[[standards]])
    }

    averageBias <- mean(df$Bias)
    averagePvalue <- t.test(biases, mu = 0)$p.value


    df2 <- data.frame(Bias = biases, Ref = references)
    lm <- summary(lm(Bias ~ Ref, df2))
    coefficientConstant <- lm$coefficients[1]
    coefficientSlope <- lm$coefficients[2]
    coefficients <- c(coefficientConstant, coefficientSlope)
    SEcoefficients <- lm$coefficients[c(3,4)]
    Tvalues <- lm$coefficients[c(5,6)]
    pvalues <- lm$coefficients[c(7,8)]
    S <- lm$sigma
    rsq <- lm$r.squared
    linearity <- abs(coefficientSlope) * options[["manualProcessVariationValue"]]
    percentLin <- (linearity / options[["manualProcessVariationValue"]]) * 100
    plusOrMin <- if (coefficientSlope > 0) "+" else "-"
    regressionEquation <- gettextf("Bias = %1$.2f %2$s %3$.2f * Reference value", coefficientConstant, plusOrMin, abs(coefficientSlope))

    p1 <- ggplot2::ggplot(data = df2, mapping = ggplot2::aes(x = Ref, y = Bias)) +
      ggplot2::geom_hline(yintercept = 0, lty = 2, color = "grey") +
      ggplot2::geom_smooth(data = df2, mapping = ggplot2::aes(x = Ref, y = Bias), method = "lm", color = "red") +
      ggplot2::scale_x_continuous(name = gettext("Reference")) +
      jaspGraphs::geom_point(data = df2, mapping = ggplot2::aes(x = Ref, y = Bias), fill = "blue", size = 4, shape = "X") +
      jaspGraphs::geom_point(data = df, mapping = ggplot2::aes(x = Ref, y = Bias), fill = "red",size = 4) +
      ggplot2::scale_y_continuous(limits = c(min(df2$Bias), max(df2$Bias) * 2)) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()

    plot1$plotObject <- p1

    table2$setData(list("predictor" = c("Intercept", "Slope"),
                        "coefficient" = coefficients,
                        "Tvalues" = Tvalues,
                        "SEcoefficient" = SEcoefficients,
                        "pvalue" = pvalues))

    regressionEquationRow <- data.frame(formula = regressionEquation)
    tableEquation$addRows(regressionEquationRow)

    if (options[["manualProcessVariation"]]) {
      table1$addColumnInfo(name = "percentBias",            title = gettext("Percent bias"), type = "number")
      table3$addColumnInfo(name = "linearity", title = gettext("Linearity"), type = "number")

      table1$setData(list("part" = c(df$Part,gettext("Total")),
                          "referenceValue" = df$Ref,
                          "observedMean" = df$ObservedMean,
                          "bias" = c(df$Bias, averageBias),
                          "percentBias" = (abs(c(df$Bias, averageBias)) / options[["manualProcessVariationValue"]]) * 100,
                          "pvalue" = c(df$pvalue, averagePvalue)))

      table3$setData(list("S" = S,
                          "linearity" = linearity,
                          "rsq" = rsq,
                          "percentLin" = percentLin))
    } else {
      table1$setData(list("part" = c(df$Part,gettext("Total")),
                          "referenceValue" = df$Ref,
                          "observedMean" = df$ObservedMean,
                          "bias" = c(df$Bias, averageBias),
                          "pvalue" = c(df$pvalue, averagePvalue)))

      table3$setData(list("S" = S,
                          "rsq" = rsq,
                          "percentLin" = percentLin))
    }

    df3 <- data.frame(Source = factor(x = c("Linearity", "Bias"), levels = c("Linearity", "Bias")), Percent = c(percentLin, (abs(averageBias) / options[["manualProcessVariationValue"]]) * 100))
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, df3$Percent))
    yLimits <- range(yBreaks)

    p2 <- ggplot2::ggplot() +
      ggplot2::geom_col(data = df3, mapping = ggplot2::aes(x = Source, y = Percent), fill = "gray", col = "black", linewidth = 1) +
      ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits) +
      ggplot2::xlab(ggplot2::element_blank()) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()

    plot2$plotObject <- p2
  }

  if (options[["biasTable"]])
    tablesAndGraphs[["table1"]] <- table1


  if (options[["linearityTable"]]) {
    tablesAndGraphs[["table2"]] <- table2
    tablesAndGraphs[["tableEquation"]] <- tableEquation
    tablesAndGraphs[["table3"]] <- table3
  }

  if (options[["linearityAndBiasPlot"]])
    tablesAndGraphs[["plot1"]] <- plot1

  if (options[["percentProcessVariationPlot"]])
    tablesAndGraphs[["plot2"]] <- plot2

  return(tablesAndGraphs)

}
