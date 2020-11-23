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

measurementSystemAnalysis <- function(jaspResults, dataset, options, ...){

  measurements <- unlist(options$measurements)
  parts <- unlist(options$parts)
  operators <- unlist(options$operators)

  numeric.vars <- measurements

  factor.vars <- c(parts, operators)


  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = numeric.vars, columns.as.factor = factor.vars)
  }

  # r and R table
  if (options[["rangeRr"]]) {
    if(is.null(jaspResults[["rAndR"]])) {
      jaspResults[["rAndR"]] <- createJaspContainer(gettext("r & R Table"))
      jaspResults[["rAndR"]]$position <- 1
    }

    jaspResults[["IsoPlot"]] <- .rAndRtable(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)

  }


  # Iso Plot
  if (options[["IsoPlot"]]) {
    if(is.null(jaspResults[["IsoPlot"]])) {
      jaspResults[["IsoPlot"]] <- createJaspContainer(gettext("Iso Plot"))
      jaspResults[["IsoPlot"]]$position <- 2
    }

    jaspResults[["IsoPlot"]] <- .IsoPlot(dataset = dataset, measurements = measurements, parts = parts, options =  options)

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

    operatorData <- dataset[[operators]]
    operatorLevels <- levels(operatorData)


    for(operator in operatorLevels){
      operatorSplit <- subset(dataset, dataset[operators] == operator)
      rangeCharts[[as.character(operator)]] <- .RangeChart(dataset = operatorSplit, measurements = measurements, parts = parts, operators = operators, options =  options, title = operator)
    }
  }
  return()
}

.IsoPlot <- function(dataset, measurements, parts, options){

  #restructure data

  measurement1 <- numeric()
  measurement2 <- numeric()

  for (i in unique(dataset[,parts])){
    d <- subset(dataset[measurements], dataset[parts] == i)
    measurement1 <- c(measurement1, d[1,])
    measurement2 <- c(measurement2, d[2,])
  }

  d <- data.frame("Measurement1" = measurement1, "Measurement2" = measurement2)

  #halfCirc <-  .circleFun(c(0,7.625), 9.8, start=1.5, end=2.5)

  IsoPlot <- createJaspPlot(title = "Iso Plot")

  IsoPlot$dependOn(c("IsoPlot"))

  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = Measurement1, y = Measurement2)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::geom_abline(intercept = 10, slope = 1) +
    ggplot2::geom_abline(intercept = -10, slope = 1)
  # ggplot2::geom_path(data = halfCirc, mapping = ggplot2::aes(x = x, y = y))

  p <- jaspGraphs::themeJasp(p)



  IsoPlot$plotObject <- p

  IsoTable <- createJaspTable(gettextf("Isoplot Calculations"))

  return(IsoPlot)
}


.ScatterPlotOperators <- function(dataset, measurements, parts, operators, options){

  byOperators <- split(dataset, dataset[operators])

  d <- data.frame("Operator1" = unlist(byOperators[[1]][measurements]),  # Assumes that parts are ordered!
                  "Operator2" = unlist(byOperators[[2]][measurements]))


  plot <- createJaspPlot(title = "Scatterplot of Operator A vs Operator B")

  plot$dependOn(c("rangeScatterPlotOperators", "rangeScatterPlotFitLine", "rangeScatterPlotOriginLine"))

  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = Operator2, y = Operator1)) +
    ggplot2::geom_point()

  if (options[["rangeScatterPlotFitLine"]])
    p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE)

  if (options[["rangeScatterPlotOriginLine"]])
    p <- p + ggplot2::geom_abline(col = "gray", linetype = "dashed")

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}


.ScatterPlotOperatorParts <- function(dataset, measurements, parts, operators, options){

  byOperators <- split(dataset, dataset[operators])

  d1 <- data.frame("Parts" = unlist(byOperators[[1]][parts]),
                   "Measurement" = unlist(byOperators[[1]][measurements]))

  d2 <- data.frame("Parts" = unlist(byOperators[[2]][parts]),
                   "Measurement" = unlist(byOperators[[2]][measurements]))

  plot <- createJaspPlot(title = "Scatterplot of Operator A, Operator B vs Part", width = 500, height = 320)

  plot$dependOn(c("rangeScatterPlotOperatorParts"))

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = d1, ggplot2::aes(x = Parts, y = Measurement, col = "c1", shape = "s1"),size = 3) +
    ggplot2::geom_point(data = d2, ggplot2::aes(x = Parts, y = Measurement, col = "c2", shape = "s2"), size = 3) +
    ggplot2::scale_color_manual(name = "Operators",
                                breaks = c("c1", "c2"),
                                values = c( "c1" = "black", "c2" = "red"),
                                labels = c("Operator A", "Operator B")) +
    ggplot2::scale_shape_manual(name = "Operators",
                                breaks = c("s1", "s2"),
                                values = c( "s1" = 19, "s2" = 15),
                                labels = c("Operator A", "Operator B"))


  p <- jaspGraphs::themeJasp(p) + ggplot2::theme(legend.position = "right")

  plot$plotObject <- p

  return(plot)
}

.rAndRtable <- function(dataset, measurements, parts, operators, options){

  byOperators <- split(dataset, dataset[operators])

  d1 <- data.frame("Parts" = unlist(byOperators[[1]][parts]),
                   "Measurement" = unlist(byOperators[[1]][measurements]))

  d2 <- data.frame("Parts" = unlist(byOperators[[2]][parts]),
                   "Measurement" = unlist(byOperators[[2]][measurements]))

  table <- createJaspTable(title = gettext("r & R Table"))

  table$dependOn(c("rangeRr", "processSD"))

  table$addColumnInfo(name = "Rbar", title = gettext("R-bar"), type = "number")
  table$addColumnInfo(name = "d2", title = gettext("d2"), type = "number")
  table$addColumnInfo(name = "PSD", title = gettext("Process SD"), type = "number")
  table$addColumnInfo(name = "GRR", title = gettext("GRR"), type = "number")
  table$addColumnInfo(name = "GRRpercent", title = gettext("%GRR"), type = "number")

  Rbar <- sum(abs(d1[["Measurement"]] - d2[["Measurement"]]) / length(d1[["Measurement"]]))



  table$addRows(list(      "Rbar"       = Rbar,
                           "d2"         = 0,
                           "PSD"        = options$processSD,
                           "GRR"        = 0,
                           "GRRpercent" = 5))



  return(table)
}

.gaugeANOVA <- function(dataset, measurements, parts, operators, options){

  data <- dataset

  anovaTable <- createJaspTable(title = gettext("ANOVA Table"))

  anovaTable$dependOn(c("gaugeANOVA"))

  anovaTable$addColumnInfo(title = gettext("Cases"),          name = "cases",   type = "string" )
  anovaTable$addColumnInfo(title = gettext("Sum of Squares"), name = "Sum Sq",  type = "number")
  anovaTable$addColumnInfo(title = gettext("df"),             name = "Df",      type = "integer")
  anovaTable$addColumnInfo(title = gettext("Mean Square"),    name = "Mean Sq", type = "number")
  anovaTable$addColumnInfo(title = gettext("F"),              name = "F value", type = "number")
  anovaTable$addColumnInfo(title = gettext("p"),              name = "Pr(>F)",  type = "pvalue")

  formula <- as.formula(paste(measurements,"~",parts,"*",operators,"+ Error(",measurements,")"))

  AnovaResults <- afex::aov_car(formula = formula,
                                data = data)


  anovaTable$setData(list( "cases"              = c(parts, operators, paste(parts," x ", operators), "Residuals"),
                           "Sum Sq"             = AnovaResults$Anova$`Sum Sq`[2:5],
                           "Df"                 = AnovaResults$Anova$Df[2:5],
                           "Mean Sq"            = AnovaResults$anova_table$MSE,
                           "F value"            = AnovaResults$anova_table$F,
                           "Pr(>F)"             = AnovaResults$anova_table$`Pr(>F)`))


  return(anovaTable)
}


.gaugeByPartGraph <- function(dataset, measurements, parts, operators, options){

  means <- aggregate(dataset[measurements], dataset[parts], mean)

  plot <- createJaspPlot(title = "Measurements by Part")

  plot$dependOn(c("gaugeByPart", "gaugeByPartAll"))

  p <- ggplot2::ggplot()

  if(options$gaugeByPartAll)
    p <- p + ggplot2::geom_point(data = dataset, ggplot2::aes_string(x = parts, y = measurements), col = "gray")


  p <- p + ggplot2::geom_point(data = means, ggplot2::aes_string(x = parts, y = measurements)) +
    ggplot2::scale_y_continuous(limits = c(min(dataset[measurements]) * 0.7, max(dataset[measurements]) * 1.3))

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}

.gaugeByOperatorGraph <- function(dataset, measurements, parts, operators, options){

  plot <- createJaspPlot(title = "Measurements by Operator")

  plot$dependOn(c("gaugeByOperator"))

  p <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = dataset, ggplot2::aes_string(x = operators, y = measurements))

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}



.gaugeByInteractionGraph <- function(dataset, measurements, parts, operators, options){

  byOperator <- split.data.frame(dataset, dataset[operators])

  meansPerOperator <- list()

  for(i in 1:length(byOperator)){
    meansPerOperator[[i]] <- aggregate(byOperator[[i]][measurements], byOperator[[i]][parts], mean)
  }


  plot <- createJaspPlot(title = "Parts by Operator Interaction", width = 500, height = 320)

  plot$dependOn(c("gaugeByInteraction"))

  p <- ggplot2::ggplot()

  c <- c('c1','c2','c3')

  colors <- rainbow(length(meansPerOperator))

  for(i in 1:length(meansPerOperator)){
    p <- p + ggplot2::geom_point(data = meansPerOperator[[i]], ggplot2::aes_string(x = parts, y = measurements),
                                 col = colors[i])
  }


  p <- jaspGraphs::themeJasp(p) + ggplot2::theme(legend.position = "right")

  plot$plotObject <- p

  return(plot)
}

.XbarChart <- function(dataset, measurements, parts, operators, options, title){

  plot <- createJaspPlot(title = paste("Operator", title))

  p <- .XbarchartNoId(dataset = dataset[measurements], options = options)

  plot$dependOn(c("gaugeXbarChart"))

  plot$plotObject <- p

  return(plot)

}

.RangeChart <- function(dataset, measurements, parts, operators, options, title){

  plot <- createJaspPlot(title = paste("Operator", title))

  p <- .RchartNoId(dataset = dataset[measurements], options = options)

  plot$dependOn(c("gaugeRchart"))

  plot$plotObject <- p

  return(plot)

}

#.circleFun <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2)
#{
#  tt <- seq(start*pi, end*pi, length.out=npoints)
#  data.frame(x = center[1] + diameter / 2 * cos(tt),
#             y = center[2] + diameter / 2 * sin(tt))
#}
