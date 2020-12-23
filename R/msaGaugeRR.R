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

msaGaugeRR <- function(jaspResults, dataset, options, ...){

  measurements <- unlist(options$measurements)
  parts <- unlist(options$parts)
  operators <- unlist(options$operators)

  numeric.vars <- measurements

  factor.vars <- c(parts, operators)
  factor.vars <- factor.vars[factor.vars != ""]


  ready <- (length(measurements) != 0 & operators != "" & parts != "")


  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = numeric.vars, columns.as.factor = factor.vars)
  }

  .msaCheckErrors(dataset, options)

  .rAndRtableGauge(ready = ready, dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, jaspResults)

  # Range Method r and R table
  if (options[["rangeRr"]]) {
    .rAndRtableRange(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, jaspResults)
  }

  # Scatter Plot Operators
  if (options[["rangeScatterPlotOperators"]]) {
    if(is.null(jaspResults[["ScatterOperators"]])) {
      jaspResults[["ScatterOperators"]] <- createJaspContainer(gettext("Scatterplot Operators"))
      jaspResults[["ScatterOperators"]]$position <- 3
    }

    jaspResults[["ScatterOperators"]] <- .ScatterPlotOperators(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)

  }


  # Scatter Plot Operators vs Parts
  if (options[["rangeScatterPlotOperatorParts"]]) {
    if(is.null(jaspResults[["ScatterOperatorParts"]])) {
      jaspResults[["ScatterOperatorParts"]] <- createJaspContainer(gettext("Scatterplot Operators vs Parts"))
      jaspResults[["ScatterOperatorParts"]]$position <- 4
    }

    jaspResults[["ScatterOperatorParts"]] <- .ScatterPlotOperatorParts(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Gauge r&R ANOVA Table
  if (options[["gaugeANOVA"]]) {
    if(is.null(jaspResults[["gaugeANOVA"]])) {
      jaspResults[["gaugeANOVA"]] <- createJaspContainer(gettext("Gauge r&R ANOVA Table"))
      jaspResults[["gaugeANOVA"]]$position <- 5
    }

    jaspResults[["gaugeANOVA"]] <- .gaugeANOVA(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Measurement by Part Graph
  if (options[["gaugeByPart"]]) {
    if(is.null(jaspResults[["gaugeByPart"]])) {
      jaspResults[["gaugeByPart"]] <- createJaspContainer(gettext("Measurement by Part Graph"))
      jaspResults[["gaugeByPart"]]$position <- 6
    }

    jaspResults[["gaugeByPart"]] <- .gaugeByPartGraph(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Measurement by Operator Box Plot
  if (options[["gaugeByOperator"]]) {
    if(is.null(jaspResults[["gaugeByOperator"]])) {
      jaspResults[["gaugeByOperator"]] <- createJaspContainer(gettext("Measurement by Operator Graph"))
      jaspResults[["gaugeByOperator"]]$position <- 7
    }

    jaspResults[["gaugeByOperator"]] <- .gaugeByOperatorGraph(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Parts by Operator Interaction Plot
  if (options[["gaugeByInteraction"]]) {
    if(is.null(jaspResults[["gaugeByInteraction"]])) {
      jaspResults[["gaugeByInteraction"]] <- createJaspContainer(gettext("Parts by Operator Interaction Graph"))
      jaspResults[["gaugeByInteraction"]]$position <- 8
    }

    jaspResults[["gaugeByInteraction"]] <- .gaugeByInteractionGraph(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Xbar chart by operator
  if (options[["gaugeXbarChart"]]) {
    if(is.null(jaspResults[["gaugeXbarChart"]])) {
      jaspResults[["gaugeXbarChart"]] <- createJaspContainer(gettext("Xbar Chart by Operator"))
      jaspResults[["gaugeXbarChart"]]$position <- 9
    }

    XbarCharts <- jaspResults[["gaugeXbarChart"]]

    operatorData <- dataset[[operators]]
    operatorLevels <- levels(operatorData)


    for(operator in operatorLevels){
      operatorSplit <- subset(dataset, dataset[operators] == operator)
      XbarCharts[[as.character(operator)]] <- .XbarChart(dataset = operatorSplit, measurements = measurements, parts = parts, operators = operators, options =  options, title = operator)
    }
  }

  # R chart by operator
  if (options[["gaugeRchart"]]) {
    if(is.null(jaspResults[["gaugeRchart"]])) {
      jaspResults[["gaugeRchart"]] <- createJaspContainer(gettext("Range Chart by Operator"))
      jaspResults[["gaugeRchart"]]$position <- 10
    }

    rangeCharts <- jaspResults[["gaugeRchart"]]

    rangeCharts$dependOn("gaugeRchart")

    operatorData <- dataset[[operators]]
    operatorLevels <- levels(operatorData)


    for(operator in operatorLevels){
      operatorSplit <- subset(dataset, dataset[operators] == operator)
      rangeCharts[[as.character(operator)]] <- .RangeChart(dataset = operatorSplit, measurements = measurements, parts = parts, operators = operators, options =  options, title = operator)
    }
  }

  # Determine Bias Table
  if (options[["biasTable"]]) {
    if(is.null(jaspResults[["biasTable"]])) {
      jaspResults[["biasTable"]] <- createJaspContainer(gettext("Bias Table"))
      jaspResults[["biasTable"]]$position <- 11
    }

    jaspResults[["biasTable"]] <- .biasTable(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Determine Bias t-Test
  if (options[["biasTtest"]]) {
    if(is.null(jaspResults[["biasTtest"]])) {
      jaspResults[["biasTtest"]] <- createJaspContainer(gettext("t-Test Bias"))
      jaspResults[["biasTtest"]]$position <- 12
    }

    jaspResults[["biasTtest"]] <- .biasTtest(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Determine Bias Histogram
  if (options[["biasHistogram"]]) {
    if(is.null(jaspResults[["biasHistogram"]])) {
      jaspResults[["biasHistogram"]] <- createJaspContainer(gettext("Histogram Bias"))
      jaspResults[["biasHistogram"]]$position <- 13
    }

    jaspResults[["biasHistogram"]] <- .biasHistogram(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Rchart Range method
  if (options[["rangeRchart"]]) {
    if(is.null(jaspResults[["rangeRchart"]])) {
      jaspResults[["rangeRchart"]] <- createJaspContainer(gettext("Range Method R Chart"))
      jaspResults[["rangeRchart"]]$position <- 14
    }


    jaspResults[["rangeRchart"]] <- .RangeChart(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, title = "vs. Operator")
    jaspResults[["rangeRchart"]]$dependOn("rangeRchart")
  }

  # Bias Run Chart
  if (options[["biasRun"]]) {
    if(is.null(jaspResults[["biasRun"]])) {
      jaspResults[["biasRun"]] <- createJaspContainer(gettext("Run Chart"))
      jaspResults[["biasRun"]]$position <- 15
    }

    jaspResults[["biasRun"]] <- .biasRunChart(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # gauge Scatter Plot Operators
  if (options[["gaugeScatterPlotOperators"]]) {
    if(is.null(jaspResults[["gaugeScatterOperators"]])) {
      jaspResults[["gaugeScatterOperators"]] <- createJaspContainer(gettext("Scatterplot Operators"))
      jaspResults[["gaugeScatterOperators"]]$position <- 16
    }

    jaspResults[["gaugeScatterOperators"]] <- .gaugeScatterPlotOperators(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)

  }

  return()
}


.ScatterPlotOperators <- function(dataset, measurements, parts, operators, options){

  plot <- createJaspPlot(title = "Scatterplot of Operator A vs Operator B")

  plot$dependOn(c("rangeScatterPlotOperators", "rangeScatterPlotFitLine", "rangeScatterPlotOriginLine"))

  p <- ggplot2::ggplot(data = dataset, ggplot2::aes_string(x = measurements[1], y = measurements[2])) +
    jaspGraphs::geom_point() + ggplot2::scale_x_continuous(limits = c(min(dataset[measurements])*0.9,max(dataset[measurements])*1.1)) +
    ggplot2::scale_y_continuous(limits = c(min(dataset[measurements])*0.9,max(dataset[measurements])*1.1))

  if (options[["rangeScatterPlotFitLine"]])
    p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE)

  if (options[["rangeScatterPlotOriginLine"]])
    p <- p + ggplot2::geom_abline(col = "gray", linetype = "dashed")

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}


.ScatterPlotOperatorParts <- function(dataset, measurements, parts, operators, options){

  partIndex <- 1:length(dataset[[measurements[1]]])

  dataset <- cbind(dataset, Parts = factor(partIndex, partIndex))


  plot <- createJaspPlot(title = "Scatterplot of Operator A, Operator B vs Part", width = 500, height = 320)

  plot$dependOn(c("rangeScatterPlotOperatorParts"))

  p <- ggplot2::ggplot() +
    jaspGraphs::geom_point(data = dataset, ggplot2::aes_string(x = "Parts", y = measurements[1]), fill = "red",size = 4) +
    jaspGraphs::geom_point(data = dataset, ggplot2::aes_string(x = "Parts", y = measurements[2]), fill = "green",size = 4)

  p <- jaspGraphs::themeJasp(p) + ggplot2::theme(legend.position = "right")

  plot$plotObject <- p

  return(plot)
}

.rAndRtableRange <- function(dataset, measurements, parts, operators, options, jaspResults){

  if(!is.null(jaspResults[["rAndR2"]]))
    return()

  table <- createJaspTable(title = gettext("r & R Table"))
  table$position <- 2

  table$dependOn(c("rangeRr"))

  table$addColumnInfo(name = "Rbar", title = gettext("R-bar"), type = "number")
  table$addColumnInfo(name = "d2", title = gettext("d2"), type = "number")
  table$addColumnInfo(name = "PSD", title = gettext("Process SD"), type = "number")
  table$addColumnInfo(name = "GRR", title = gettext("GRR"), type = "number")
  table$addColumnInfo(name = "GRRpercent", title = gettext("%GRR"), type = "number")

  jaspResults[["rAndR2"]] <- table

  Rbar <- sum(abs(dataset[measurements[1]] - dataset[measurements[2]]) / length(dataset[[measurements[1]]]))
  d2 <- 1.19105
  SD <- options$processVariationOrTolerance
  GRR <- Rbar/d2
  GRRpercent <- GRR/SD*100


  table$addRows(list(      "Rbar"       = Rbar,
                           "d2"         = d2,
                           "PSD"        = SD,
                           "GRR"        = GRR,
                           "GRRpercent" = GRRpercent))
}

.rAndRtableGauge <- function(ready, dataset, measurements, parts, operators, options, jaspResults){

	if(!is.null(jaspResults[["rAndR1"]]))
		return()

  table <- createJaspTable(title = gettext("r & R Table"))
  table$position <- 1

  table$dependOn(c("operators", "parts", "measurements"))

  table$addColumnInfo(name = "Source", title = gettext("Source"), type = "string")
  table$addColumnInfo(name = "Variation", title = gettext("Variation"), type = "number")

  jaspResults[["rAndR1"]] <- table

  if(ready){

    interval <- 5.15

    data <- dataset

    data <- tidyr::gather(data, repetition, measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

    formula <- as.formula(paste("measurement ~",operators,"*",parts))

    anova <- summary(aov(formula = formula, data = data))




    repeatability <- interval*sqrt(anova[[1]]$`Mean Sq`[4])
    reproducibility <- interval*sqrt((anova[[1]]$`Mean Sq`[1]-anova[[1]]$`Mean Sq`[3])/(length(measurements)*length(unique(data[[parts]]))))
    interaction <- ifelse(anova[[1]]$`Mean Sq`[3] - anova[[1]]$`Mean Sq`[4] < 0, 0,
                          interval*sqrt((anova[[1]]$`Mean Sq`[3] - anova[[1]]$`Mean Sq`[4])/(length(measurements))))
    rR <- sqrt((repeatability^2)+(reproducibility^2)+(interaction^2))
    partvar <- interval*sqrt((anova[[1]]$`Mean Sq`[2] - anova[[1]]$`Mean Sq`[3])/(length(measurements)*length(unique(data[[operators]]))))
    totalvar <- sqrt((rR^2) + (partvar^2))

    table$setData(list(      "Source"       = c("r & R", "Repeatability", "Reproducibility", "Interaction", "Part Variation", "Total Variation"),
                             "Variation"    = c(rR, repeatability, reproducibility, interaction, partvar, totalvar)))

  } else {
    table$setData(list(      "Source"       = c("r & R", "Repeatability", "Reproducibility", "Interaction", "Part Variation", "Total Variation"),
                             "Variation"    = rep(".", 6)))
  }
}

.gaugeANOVA <- function(dataset, measurements, parts, operators, options){

  data <- dataset

  data <- tidyr::gather(data, repetition, measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

  anovaTable <- createJaspTable(title = gettext("ANOVA Table"))
  anovaTable$position <- 5

  anovaTable$dependOn(c("gaugeANOVA"))

  anovaTable$addColumnInfo(title = gettext("Cases"),          name = "cases",   type = "string" )
  anovaTable$addColumnInfo(title = gettext("Sum of Squares"), name = "Sum Sq",  type = "number")
  anovaTable$addColumnInfo(title = gettext("df"),             name = "Df",      type = "integer")
  anovaTable$addColumnInfo(title = gettext("Mean Square"),    name = "Mean Sq", type = "number")
  anovaTable$addColumnInfo(title = gettext("F"),              name = "F value", type = "number")
  anovaTable$addColumnInfo(title = gettext("p"),              name = "Pr(>F)",  type = "pvalue")

  formula <- as.formula(paste("measurement ~",operators,"*",parts))

  anova <- summary(aov(formula = formula, data = data))


  anovaTable$setData(list( "cases"              = c(operators, parts, paste(parts," x ", operators), "Residuals"),
                           "Sum Sq"             = anova[[1]]$`Sum Sq`,
                           "Df"                 = anova[[1]]$Df,
                           "Mean Sq"            = anova[[1]]$`Mean Sq`,
                           "F value"            = anova[[1]]$`F value`,
                           "Pr(>F)"             = anova[[1]]$`Pr(>F)`))


  return(anovaTable)
}


.gaugeByPartGraph <- function(dataset, measurements, parts, operators, options){

  dataset <- tidyr::gather(dataset, Repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

  means <- aggregate(dataset["Measurement"], dataset[parts], mean)


  plot <- createJaspPlot(title = "Measurements by Part")

  plot$dependOn(c("gaugeByPart", "gaugeByPartAll"))

  p <- ggplot2::ggplot()

  if(options$gaugeByPartAll)
    p <- p + jaspGraphs::geom_point(data = dataset, ggplot2::aes_string(x = parts, y = "Measurement"), col = "gray")


  p <- p + jaspGraphs::geom_point(data = means, ggplot2::aes_string(x = parts, y = "Measurement")) +
    ggplot2::scale_y_continuous(limits = c(min(dataset["Measurement"]) * 0.9, max(dataset["Measurement"]) * 1.1))

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}

.gaugeByOperatorGraph <- function(dataset, measurements, parts, operators, options){


  dataset <- tidyr::gather(dataset, Repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

  plot <- createJaspPlot(title = "Measurements by Operator")

  plot$dependOn(c("gaugeByOperator"))

  p <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = dataset, ggplot2::aes_string(x = operators, y = "Measurement"))

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}



.gaugeByInteractionGraph <- function(dataset, measurements, parts, operators, options){

  byOperator <- split.data.frame(dataset, dataset[operators])

  partNames <- levels(as.factor(dataset[[parts]]))

  meansPerOperator <- data.frame(Part = factor(partNames, partNames))

  for(i in names(byOperator)){
    meansPerOperator <- cbind(meansPerOperator, rowMeans(byOperator[[i]][c(measurements)]))
  }
  colnames(meansPerOperator)[-1] <- names(byOperator)


  plot <- createJaspPlot(title = "Parts by Operator Interaction", width = 600, height = 300)

  plot$dependOn(c("gaugeByInteraction"))

  p <- ggplot2::ggplot()

  colors <- rainbow(length(names(byOperator)))

  for(i in 1:length(names(byOperator))){
    p <- p + jaspGraphs::geom_line(data = meansPerOperator, ggplot2::aes_string(x = "Part", y = names(byOperator)[i],
                                                                                group = i), col = colors[i]) +
      jaspGraphs::geom_point(data = meansPerOperator, ggplot2::aes_string(x = "Part", y = names(byOperator)[i],
                                                                          group = i))
  }


  p <- jaspGraphs::themeJasp(p) +
    ggplot2::ylab("Measurement") +
    ggplot2::scale_y_continuous(limits = c(min(meansPerOperator[names(byOperator)]) * 0.9, max(meansPerOperator[names(byOperator)]) * 1.1))


  plot$plotObject <- p

  return(plot)
}

.XbarChart <- function(dataset, measurements, parts, operators, options, title){

  plot <- createJaspPlot(title = paste("Operator", title), width = 600, height = 300)

  p <- .XbarchartNoId(dataset = dataset[measurements], options = options)

  plot$dependOn(c("gaugeXbarChart"))

  plot$plotObject <- p

  return(plot)

}

.RangeChart <- function(dataset, measurements, parts, operators, options, title){

  plot <- createJaspPlot(title = paste("Operator", title), width = 600, height = 300)

  p <- .RchartNoId(dataset = dataset[measurements], options = options)

  plot$plotObject <- p

  return(plot)

}

.biasTable <- function(dataset, measurements, parts, operators, options){

  dataForPart <- dataset[[measurements]]

  table <- createJaspTable(title = gettext("Bias Table for Part"))

  table$dependOn(c("biasReferenceValue", "biasTable", "biasTolerance"))

  table$addColumnInfo(name = "referenceValue",  title = gettext("Reference Value"), type = "number")
  table$addColumnInfo(name = "observedAverage", title = gettext("Observed Average"), type = "number")
  table$addColumnInfo(name = "bias",            title = gettext("Bias"), type = "number")
  table$addColumnInfo(name = "tolerance",       title = gettext("Tolerance"), type = "number")
  table$addColumnInfo(name = "biasPercent",     title = gettext("Bias Percent"), type = "number")

  observedAverage <- mean(unlist(dataForPart))
  bias <- options$biasReferenceValue - observedAverage
  biasPercent <- abs(bias) / options$biasTolerance * 100


  table$addRows(list(      "referenceValue"       = options$biasReferenceValue,
                           "observedAverage"      = observedAverage,
                           "bias"                 = bias,
                           "tolerance"            = options$biasTolerance,
                           "biasPercent"          = biasPercent))



  return(table)
}

.biasTtest <- function(dataset, measurements, parts, operators, options){

  dataForPart <- dataset[[measurements]]

  table <- createJaspTable(title = gettext("t-Test of Observed Value agaist Reference Value for Part"))

  table$dependOn(c("biasReferenceValue", "biasTtest", "biasTolerance"))

  table$addColumnInfo(name = "df",              title = gettext("df"), type = "integer")
  table$addColumnInfo(name = "mean",            title = gettext("Mean"), type = "number")
  table$addColumnInfo(name = "referenceValue",  title = gettext("Reference Value"), type = "number")
  table$addColumnInfo(name = "sd",              title = gettext("SD"), type = "number")
  table$addColumnInfo(name = "se",              title = gettext("SE"), type = "number")
  table$addColumnInfo(name = "lci",             title = gettext("LB"), type = "number", overtitle = gettext("95% CI"))
  table$addColumnInfo(name = "uci",             title = gettext("UB"), type = "number", overtitle = gettext("95% CI"))
  table$addColumnInfo(name = "t",               title = gettext("t"), type = "number")
  table$addColumnInfo(name = "p",               title = gettext("p"), type = "pvalue")

  tTest <- t.test(dataForPart, mu = options$biasReferenceValue)

  table$addRows(list(      "df"                   = tTest$parameter,
                           "mean"                 = tTest$estimate,
                           "referenceValue"       = options$biasReferenceValue,
                           "sd"                   = sd(unlist(dataForPart)),
                           "se"                   = tTest$stderr,
                           "lci"                  = tTest$conf.int[1],
                           "uci"                  = tTest$conf.int[2],
                           "t"                    = tTest$statistic,
                           "p"                    = tTest$p.value))



  return(table)
}


.biasHistogram <- function(dataset, measurements, parts, operators, options, title){

  dataForPart <- dataset[[measurements]]

  plot <- createJaspPlot(title = "Bias Histogram")

  p <- jaspDescriptives:::.plotMarginal(column = dataForPart, variableName = "Measurement")

  plot$dependOn(c("biasHistogram"))

  plot$plotObject <- p

  return(plot)

}


.biasRunChart <- function(dataset, measurements, parts, operators, options){


  dataset <- tidyr::gather(dataset, Repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

  plot <- createJaspPlot(title = "Run Chart", width = 600, height = 300)

  index <- 1:length(dataset[["Measurement"]])

  dataset <- cbind(dataset, index = factor(index, index))

  plot$dependOn(c("biasRun"))

  p <- ggplot2::ggplot() +
    jaspGraphs::geom_line(data = dataset, ggplot2::aes(x = index, y = Measurement, group = 1)) +
    jaspGraphs::geom_point(data = dataset, ggplot2::aes(x = index, y = Measurement)) +
    ggplot2::scale_x_discrete(name = "Index", breaks = c(seq(1, max(index), 5),max(index)))

  p <- jaspGraphs::themeJasp(p) + ggplot2::theme(plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

  plot$plotObject <- p

  return(plot)
}


.gaugeScatterPlotOperators <- function(dataset, measurements, parts, operators, options){

  plot <- createJaspPlot(title = "Scatterplot of Operator A vs Operator B")
  plot$dependOn(c("gaugeScatterPlotOperators", "gaugeScatterPlotFitLine", "gaugeScatterPlotOriginLine"))

  if(length(unique(dataset[[operators]])) > 2){
    plot$setError(gettextf("Plotting not possible: More than 2 Operators"))
  }else{

    operatorSplit <- split.data.frame(dataset, dataset[operators])

    data <- data.frame(OperatorA = rowMeans(operatorSplit[[1]][measurements]), OperatorB = rowMeans(operatorSplit[[2]][measurements]))

    p <- ggplot2::ggplot(data = data, ggplot2::aes_string(x = "OperatorA", y = "OperatorB")) +
      jaspGraphs::geom_point() + ggplot2::scale_x_continuous(limits = c(min(dataset[measurements])*0.9,max(dataset[measurements])*1.1)) +
      ggplot2::scale_y_continuous(limits = c(min(dataset[measurements])*0.9,max(dataset[measurements])*1.1))

    if (options[["gaugeScatterPlotFitLine"]])
      p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE)

    if (options[["gaugeScatterPlotOriginLine"]])
      p <- p + ggplot2::geom_abline(col = "gray", linetype = "dashed")

    p <- jaspGraphs::themeJasp(p)

    plot$plotObject <- p
  }

  return(plot)
}

.msaCheckErrors <- function(dataset, options) {

  #if (options[["gaugeScatterPlotOperators"]]){
  #  .hasErrors(dataset = dataset, type = "factorLevels",
  #             factorLevels.target  = options$operators, factorLevels.amount  = "> 2",
  #             exitAnalysisIfErrors = TRUE)
  #}

}
