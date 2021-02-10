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

  # Gauge r&R ANOVA Table
  if (options[["gaugeANOVA"]]) {
    if(is.null(jaspResults[["gaugeANOVA"]])) {
      jaspResults[["gaugeANOVA"]] <- createJaspContainer(gettext("Gauge r&R ANOVA Table"))
      jaspResults[["gaugeANOVA"]]$dependOn(c("historicalStandardDeviation", "studyStandardDeviation", "standardDeviationReference", "historicalStandardDeviationValue"))
      jaspResults[["gaugeANOVA"]]$position <- 1
    }

    jaspResults[["gaugeANOVA"]] <- .gaugeANOVA(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready)
  }

  # R chart by operator
  if (options[["gaugeRchart"]]) {
    if(is.null(jaspResults[["gaugeRchart"]])) {
      jaspResults[["gaugeRchart"]] <- createJaspContainer(gettext("Range Chart by Operator"))
      jaspResults[["gaugeRchart"]]$position <- 2
    }
    jaspResults[["gaugeRchart"]]$dependOn("gaugeRchart")

    operatorData <- dataset[[operators]]
    operatorLevels <- levels(operatorData)

    for(operator in operatorLevels){
      operatorSplit <- subset(dataset, dataset[operators] == operator)
      jaspResults[["gaugeRchart"]][[as.character(operator)]] <- .RangeChart(dataset = operatorSplit, measurements = measurements, parts = parts, operators = operators, options =  options, title = operator, ready = ready)
    }
  }

  # Xbar chart by operator
  if (options[["gaugeXbarChart"]]) {
    if(is.null(jaspResults[["gaugeXbarChart"]])) {
      jaspResults[["gaugeXbarChart"]] <- createJaspContainer(gettext("Xbar Chart by Operator"))
      jaspResults[["gaugeXbarChart"]]$position <- 3
    }
    XbarCharts <- jaspResults[["gaugeXbarChart"]]
    XbarCharts$dependOn("gaugeXbarChart")

    operatorData <- dataset[[operators]]
    operatorLevels <- levels(operatorData)

    for(operator in operatorLevels){
      operatorSplit <- subset(dataset, dataset[operators] == operator)
      XbarCharts[[as.character(operator)]] <- .XbarChart(dataset = operatorSplit, measurements = measurements, parts = parts, operators = operators, options =  options, title = operator, ready = ready)
    }
  }

  # gauge Scatter Plot Operators
  if (options[["gaugeScatterPlotOperators"]]) {
    if(is.null(jaspResults[["gaugeScatterOperators"]])) {
      jaspResults[["gaugeScatterOperators"]] <- createJaspContainer(gettext("Scatterplot Operators"))
      jaspResults[["gaugeScatterOperators"]]$position <- 4
    }
    jaspResults[["gaugeScatterOperators"]] <- .gaugeScatterPlotOperators(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Measurement by Part Graph
  if (options[["gaugeByPart"]]) {
    if(is.null(jaspResults[["gaugeByPart"]])) {
      jaspResults[["gaugeByPart"]] <- createJaspContainer(gettext("Measurement by Part Graph"))
      jaspResults[["gaugeByPart"]]$position <- 5
    }

    jaspResults[["gaugeByPart"]] <- .gaugeByPartGraph(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Measurement by Operator Box Plot
  if (options[["gaugeByOperator"]]) {
    if(is.null(jaspResults[["gaugeByOperator"]])) {
      jaspResults[["gaugeByOperator"]] <- createJaspContainer(gettext("Measurement by Operator Graph"))
      jaspResults[["gaugeByOperator"]]$position <- 6
    }
    jaspResults[["gaugeByOperator"]] <- .gaugeByOperatorGraph(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Parts by Operator Interaction Plot
  if (options[["gaugeByInteraction"]]) {
    if(is.null(jaspResults[["gaugeByInteraction"]])) {
      jaspResults[["gaugeByInteraction"]] <- createJaspContainer(gettext("Parts by Operator Interaction Graph"))
      jaspResults[["gaugeByInteraction"]]$position <- 7
    }
    jaspResults[["gaugeByInteraction"]] <- .gaugeByInteractionGraph(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Range Method r and R table
  if (options[["rangeRr"]]) {
    .rAndRtableRange(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, jaspResults)
  }

  # Scatter Plot Operators vs Parts
  if (options[["rangeScatterPlotOperatorParts"]]) {
    if(is.null(jaspResults[["ScatterOperatorParts"]])) {
      jaspResults[["ScatterOperatorParts"]] <- createJaspContainer(gettext("Scatterplot Operators vs Parts"))
      jaspResults[["ScatterOperatorParts"]]$position <- 9
    }
    jaspResults[["ScatterOperatorParts"]] <- .ScatterPlotOperatorParts(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Scatter Plot Operators
  if (options[["rangeScatterPlotOperators"]]) {
    if(is.null(jaspResults[["ScatterOperators"]])) {
      jaspResults[["ScatterOperators"]] <- createJaspContainer(gettext("Scatterplot Operators"))
      jaspResults[["ScatterOperators"]]$position <- 10
    }
    jaspResults[["ScatterOperators"]] <- .ScatterPlotOperators(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Rchart Range method
  if (options[["rangeRchart"]]) {
    if(is.null(jaspResults[["rangeRchart"]])) {
      jaspResults[["rangeRchart"]] <- createJaspContainer(gettext("Range Method R Chart"))
      jaspResults[["rangeRchart"]]$dependOn("rangeRchart")
      jaspResults[["rangeRchart"]]$position <- 11
    }
    jaspResults[["rangeRchart"]] <- .RangeChart(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, title = "vs. Operator")
  }


  # Determine Bias Table
  if (options[["biasTable"]]) {
    if(is.null(jaspResults[["biasTable"]])) {
      jaspResults[["biasTable"]] <- createJaspContainer(gettext("Bias Table"))
      jaspResults[["biasTable"]]$position <- 111
    }

    jaspResults[["biasTable"]] <- .biasTable(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Determine Bias t-Test
  if (options[["biasTtest"]]) {
    if(is.null(jaspResults[["biasTtest"]])) {
      jaspResults[["biasTtest"]] <- createJaspContainer(gettext("t-Test Bias"))
      jaspResults[["biasTtest"]]$position <- 121
    }

    jaspResults[["biasTtest"]] <- .biasTtest(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Determine Bias Histogram
  if (options[["biasHistogram"]]) {
    if(is.null(jaspResults[["biasHistogram"]])) {
      jaspResults[["biasHistogram"]] <- createJaspContainer(gettext("Histogram Bias"))
      jaspResults[["biasHistogram"]]$position <- 131
    }

    jaspResults[["biasHistogram"]] <- .biasHistogram(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }

  # Bias Run Chart
  if (options[["biasRun"]]) {
    if(is.null(jaspResults[["biasRun"]])) {
      jaspResults[["biasRun"]] <- createJaspContainer(gettext("Run Chart"))
      jaspResults[["biasRun"]]$position <- 151
    }

    jaspResults[["biasRun"]] <- .biasRunChart(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }



  return()
}


.gaugeANOVA <- function(dataset, measurements, parts, operators, options, ready){

  anovaTables <- createJaspContainer(gettext("ANOVA Tables"))
  anovaTables$dependOn(c("gaugeANOVA"))
  anovaTables$position <- 1

  if (ready && length(measurements) >= 2){
    data <- dataset

    data <- tidyr::gather(data, repetition, measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

    if (options$studyVarMultiplierType == "svmSD"){
      studyVarMultiplier <- options$studyVarMultiplier
    }else{
      percent <- options$studyVarMultiplier/100
      q <- (1 - percent)/2
      studyVarMultiplier <- abs(2*qnorm(q))
    }
    anovaTable1 <- createJaspTable(title = gettext("Two-Way ANOVA Table with Interaction"))

    anovaTable1$addColumnInfo(title = gettext("Cases"),          name = "cases",   type = "string" )
    anovaTable1$addColumnInfo(title = gettext("df"),             name = "Df",      type = "integer")
    anovaTable1$addColumnInfo(title = gettext("Sum of Squares"), name = "Sum Sq",  type = "number")
    anovaTable1$addColumnInfo(title = gettext("Mean Square"),    name = "Mean Sq", type = "number")
    anovaTable1$addColumnInfo(title = gettext("F"),              name = "F value", type = "number")
    anovaTable1$addColumnInfo(title = gettext("p"),              name = "Pr(>F)",  type = "pvalue")

    formula1 <- as.formula(paste("measurement ~", parts,"*", operators))

    anova1 <- summary(aov(formula = formula1, data = data))


    anovaTable1$setData(list( "cases"              = c(parts, operators, paste(parts," x ", operators), "Repeatability", "Total"),
                              "Df"                 = c(anova1[[1]]$Df, sum(anova1[[1]]$Df)),
                              "Sum Sq"             = c(anova1[[1]]$`Sum Sq`, sum(anova1[[1]]$`Sum Sq`)),
                              "Mean Sq"            = anova1[[1]]$`Mean Sq`,
                              "F value"            = anova1[[1]]$`F value`,
                              "Pr(>F)"             = anova1[[1]]$`Pr(>F)`))

    anovaTables[['anovaTable1']] <- anovaTable1

    if(anova1[[1]]$`Pr(>F)`[3] < options$alphaForANOVA){


      RRtable1 <- createJaspTable(title = gettext("r & R Table"))

      RRtable1$dependOn(c("gaugeANOVA", "operators", "parts", "measurements"))

      RRtable1$addColumnInfo(name = "Source", title = gettext("Source"), type = "string")
      RRtable1$addColumnInfo(name = "Variation", title = gettext("Variation"), type = "number")
      RRtable1$addColumnInfo(name = "Percent", title = gettext("Percent Contribution to Total"), type = "number")



      repeatability <- anova1[[1]]$`Mean Sq`[4]

      if(anova1[[1]]$`Mean Sq`[2] < anova1[[1]]$`Mean Sq`[3]){
        operator <- 0
      }else{
        operator <- (anova1[[1]]$`Mean Sq`[2] - anova1[[1]]$`Mean Sq`[3]) / (length(unique(data[[parts]])) * length(measurements))
      }

      if(anova1[[1]]$`Mean Sq`[3] < anova1[[1]]$`Mean Sq`[4]){
        operatorXpart <- 0
      }else{
        operatorXpart <- (anova1[[1]]$`Mean Sq`[3] - anova1[[1]]$`Mean Sq`[4]) / length(measurements)
      }

      partToPart <- (anova1[[1]]$`Mean Sq`[1] - anova1[[1]]$`Mean Sq`[3]) / (length(measurements) * length(unique(data[[operators]])))
      reproducibility <- operator + operatorXpart
      totalRR <- repeatability + reproducibility
      totalVar <- totalRR + partToPart



      RRtable1$setData(list(      "Source"       = gettext(c("Total r & R", "Repeatability", "Reproducibility", "Operator", "Part-To-Part", "Operator x Part", "Total Variation")),
                                  "Variation"    = c(totalRR, repeatability, reproducibility, operator, partToPart, operatorXpart, totalVar),
                                  "Percent"      = c(totalRR, repeatability, reproducibility, operator, partToPart, operatorXpart, totalVar) / totalVar * 100))


      anovaTables[['RRtable1']] <- RRtable1

      RRtable2 <- createJaspTable(title = gettext("r & R Table"))

      RRtable2$dependOn(c("gaugeANOVA", "operators", "parts", "measurements"))

      RRtable2$addColumnInfo(name = "source", title = gettext("Source"), type = "string")
      RRtable2$addColumnInfo(name = "SD", title = gettext("SD"), type = "number")
      RRtable2$addColumnInfo(name = "studyVar", title = gettextf("Study Variation (SD * %.2f)", studyVarMultiplier), type = "number")
      RRtable2$addColumnInfo(name = "percentStudyVar", title = gettext("Percent Study Variation"), type = "number")
      RRtable2$addColumnInfo(name = "percentTolerance", title = gettext("Percent Tolerance"), type = "number")

      histSD <- options$historicalStandardDeviationValue

      if(options$standardDeviationReference == "historicalStandardDeviation" && histSD >= sqrt(totalRR)){
        SD <- c(sqrt(c(totalRR, repeatability, reproducibility, operator)),
                sqrt(histSD^2 - totalRR),
                sqrt(operatorXpart), histSD)

      }else{
        SD <- sqrt(c(totalRR, repeatability, reproducibility, operator, partToPart, operatorXpart, totalVar))
      }
      studyVar <- SD * studyVarMultiplier


      RRtable2$setData(list(      "source"       = gettext(c("Total r & R", "Repeatability", "Reproducibility", "Operator", "Part-To-Part", "Operator x Part", "Total Variation")),
                                  "SD"           = SD,
                                  "studyVar"    = studyVar,
                                  "percentStudyVar"    = studyVar/max(studyVar) * 100,
                                  "percentTolerance" = studyVar / options$tolerance * 100))


      anovaTables[['RRtable2']] <- RRtable2

    }else if(anova1[[1]]$`Pr(>F)`[3] > options$alphaForANOVA){
      anovaTable2 <- createJaspTable(title = gettext("Two-Way ANOVA Table without Interaction"))

      anovaTable2$addColumnInfo(title = gettext("Cases"),          name = "cases",   type = "string" )
      anovaTable2$addColumnInfo(title = gettext("df"),             name = "Df",      type = "integer")
      anovaTable2$addColumnInfo(title = gettext("Sum of Squares"), name = "Sum Sq",  type = "number")
      anovaTable2$addColumnInfo(title = gettext("Mean Square"),    name = "Mean Sq", type = "number")
      anovaTable2$addColumnInfo(title = gettext("F"),              name = "F value", type = "number")
      anovaTable2$addColumnInfo(title = gettext("p"),              name = "Pr(>F)",  type = "pvalue")

      formula2 <- as.formula(paste("measurement ~",parts, "+",operators))

      anova2 <- summary(aov(formula = formula2, data = data))


      anovaTable2$setData(list( "cases"              = c(parts, operators, "Repeatability", "Total"),
                                "Df"                 = c(anova2[[1]]$Df, sum(anova2[[1]]$Df)),
                                "Sum Sq"             = c(anova2[[1]]$`Sum Sq`, sum(anova2[[1]]$`Sum Sq`)),
                                "Mean Sq"            = anova2[[1]]$`Mean Sq`,
                                "F value"            = anova2[[1]]$`F value`,
                                "Pr(>F)"             = anova2[[1]]$`Pr(>F)`))


      anovaTables[['anovaTable2']] <- anovaTable2


      RRtable1 <- createJaspTable(title = gettext("r & R Table"))

      RRtable1$dependOn(c("gaugeANOVA", "operators", "parts", "measurements"))

      RRtable1$addColumnInfo(name = "Source", title = gettext("Source"), type = "string")
      RRtable1$addColumnInfo(name = "Variation", title = gettext("Variation"), type = "number")
      RRtable1$addColumnInfo(name = "Percent", title = gettext("Percent Contribution to Total"), type = "number")



      repeatability <- anova2[[1]]$`Mean Sq`[3]

      if(anova2[[1]]$`Mean Sq`[2] < anova2[[1]]$`Mean Sq`[3]){
        operator <- 0
      }else{
        operator <- (anova2[[1]]$`Mean Sq`[2] - anova2[[1]]$`Mean Sq`[3]) / (length(unique(data[[parts]])) * length(measurements))
      }

      partToPart <- (anova2[[1]]$`Mean Sq`[1] - anova2[[1]]$`Mean Sq`[3]) / (length(measurements) * length(unique(data[[operators]])))
      reproducibility <- operator
      totalRR <- repeatability + reproducibility
      totalVar <- totalRR + partToPart



      RRtable1$setData(list(      "Source"       = gettext(c("Total r & R", "Repeatability", "Reproducibility", "Operator", "Part-To-Part", "Total Variation")),
                                  "Variation"    = c(totalRR, repeatability, reproducibility, operator, partToPart, totalVar),
                                  "Percent"      = c(totalRR, repeatability, reproducibility, operator, partToPart, totalVar) / totalVar * 100))


      anovaTables[['RRtable1']] <- RRtable1

      RRtable2 <- createJaspTable(title = gettext("r & R Table"))

      RRtable2$dependOn(c("gaugeANOVA", "operators", "parts", "measurements"))

      RRtable2$addColumnInfo(name = "source", title = gettext("Source"), type = "string")
      RRtable2$addColumnInfo(name = "SD", title = gettext("SD"), type = "number")
      RRtable2$addColumnInfo(name = "studyVar", title = gettextf("Study Variation (SD * %.2f)", studyVarMultiplier), type = "number")
      RRtable2$addColumnInfo(name = "percentStudyVar", title = gettext("Percent Study Variation"), type = "number")
      RRtable2$addColumnInfo(name = "percentTolerance", title = gettext("Percent Tolerance"), type = "number")


      histSD <- options$historicalStandardDeviationValue

      if(options$standardDeviationReference == "historicalStandardDeviation" && histSD >= sqrt(totalRR)){
        SD <- c(sqrt(c(totalRR, repeatability, reproducibility, operator)),
                sqrt(histSD^2 - totalRR), histSD)
      }else{
        SD <- sqrt(c(totalRR, repeatability, reproducibility, operator, partToPart, totalVar))
      }

      studyVar <- SD * studyVarMultiplier


      RRtable2$setData(list(      "source"       = gettext(c("Total r & R", "Repeatability", "Reproducibility", "Operator", "Part-To-Part", "Total Variation")),
                                  "SD"           = SD,
                                  "studyVar"    = studyVar,
                                  "percentStudyVar"    = studyVar/max(studyVar) * 100,
                                  "percentTolerance" = studyVar / options$tolerance * 100))


      anovaTables[['RRtable2']] <- RRtable2
    }

    if (options[["gaugeVarCompGraph"]]) {

      plot <- createJaspPlot(title = gettext("Variation Components Graph"), width = 850, height = 500)

      plot$dependOn(c("gaugeVarCompGraph"))

      percentContributionValues <- c(totalRR, repeatability, reproducibility, partToPart)/totalVar * 100
      SDs <- sqrt(c(totalRR, repeatability, reproducibility, partToPart))
      studyvars <- SDs * studyVarMultiplier
      studyVariationValues <- studyvars/max(studyVar) * 100
      percentToleranceValues <- studyvars / options$tolerance * 100

      plotframe <- data.frame(source = rep(gettext(c('Gauge r&R', 'Repeat', 'Reprod', 'Part-to-Part')), 3),
                              reference = rep(gettext(c('Percent Contribution', 'Percent Study Variation', 'Percent Tolerance')), each = 4),
                              value = c(percentContributionValues, studyVariationValues, percentToleranceValues))

      p <- ggplot2::ggplot() + ggplot2::geom_bar(data = plotframe,
                                                 mapping = ggplot2::aes(fill =  reference,  y = value, x = source),
                                                 position="dodge", stat = "identity")
      p <- jaspGraphs::themeJasp(p) + ggplot2::theme(legend.position = 'right', legend.title = ggplot2::element_blank()) +
        ggplot2::xlab('') + ggplot2::ylab('Percent')
      if (max(plotframe['value']) < 110)
        p <- p + ggplot2::scale_y_continuous(breaks = c(0, 25, 50, 75, 100))

      plot$plotObject <- p

      anovaTables[['VarCompGraph']] <- plot
    }
  }else {
    RRtable3 <- createJaspTable(title = gettext("r & R Table"))

    RRtable3$dependOn(c("gaugeANOVA", "operators", "parts", "measurements"))

    RRtable3$addColumnInfo(name = "Source", title = gettext("Source"), type = "string")
    RRtable3$addColumnInfo(name = "Variation", title = gettext("Variation"), type = "number")
    RRtable3$addColumnInfo(name = "Percent", title = gettext("Percent Contribution to Total"), type = "number")

    RRtable3$setData(list(      "Source"       = gettext(c("Total r & R", "Repeatability", "Reproducibility", "Operator", "Part-To-Part", "Operator x Part", "Total Variation"))))

    anovaTables[['RRtable3']] <- RRtable3

    if(ready){
      RRtable3$setError(gettextf("Number of observations is < 2 in %s after grouping on %s", parts, operators))
    }
  }
  return(anovaTables)
}

.RangeChart <- function(dataset, measurements, parts, operators, options, title, ready){
  if (ready) {
    plot <- createJaspPlot(title = gettextf("Operator %s", title), width = 600, height = 300)
    if (length(measurements) == 1){
      plot$setError(gettext("At least 2 measurements per operator required."))
    }else{
      p <- .RchartNoId(dataset = dataset[measurements], options = options)
      plot$plotObject <- p
    }
    return(plot)
  }
}

.XbarChart <- function(dataset, measurements, parts, operators, options, title, ready){
  if (ready){
    plot <- createJaspPlot(title = gettextf("Operator %s", title), width = 600, height = 300)
    if (length(measurements) == 1){
      plot$setError(gettext("At least 2 measurements per operator required."))
    }else{
      p <- .XbarchartNoId(dataset = dataset[measurements], options = options)
      plot$plotObject <- p
    }
    return(plot)
  }
}

.ScatterPlotOperators <- function(dataset, measurements, parts, operators, options){

  plot <- createJaspPlot(title = gettext("Scatterplot of Operator A vs Operator B"))

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

.gaugeByPartGraph <- function(dataset, measurements, parts, operators, options){

  dataset <- tidyr::gather(dataset, Repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

  means <- aggregate(dataset["Measurement"], dataset[parts], mean)


  plot <- createJaspPlot(title = gettext("Measurements by Part"))

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

  plot <- createJaspPlot(title = gettext("Measurements by Operator"))

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

  tidydata <- tidyr::gather(meansPerOperator, key = "Operator", value = "Measurements", -Part )


  plot <- createJaspPlot(title = gettext("Parts by Operator Interaction"), width = 700, height = 400)

  plot$dependOn(c("gaugeByInteraction"))

  p <- ggplot2::ggplot(tidydata, ggplot2::aes(x = Part, y = Measurements, col = Operator, group = Operator)) +
    jaspGraphs::geom_line() + jaspGraphs::geom_point()


  p <- jaspGraphs::themeJasp(p) + ggplot2::theme(legend.position = 'right')
  ggplot2::ylab("Measurement") +
    ggplot2::scale_y_continuous(limits = c(min(meansPerOperator[names(byOperator)]) * 0.9, max(meansPerOperator[names(byOperator)]) * 1.1))



  plot$plotObject <- p

  return(plot)
}

.ScatterPlotOperatorParts <- function(dataset, measurements, parts, operators, options){

  partIndex <- 1:length(dataset[[measurements[1]]])

  dataset <- cbind(dataset, Parts = factor(partIndex, partIndex))


  plot <- createJaspPlot(title = gettext("Scatterplot of Operator A, Operator B vs Part"), width = 500, height = 320)

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

  plot <- createJaspPlot(title = gettext("Bias Histogram"))

  p <- jaspDescriptives:::.plotMarginal(column = dataForPart, variableName = "Measurement")

  plot$dependOn(c("biasHistogram"))

  plot$plotObject <- p

  return(plot)

}

.biasRunChart <- function(dataset, measurements, parts, operators, options){


  dataset <- tidyr::gather(dataset, Repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

  plot <- createJaspPlot(title = "Run Chart", width = 700, height = 300)

  index <- 1:length(dataset[["Measurement"]])

  dataset <- cbind(dataset, index = factor(index, index))

  plot$dependOn(c("biasRun"))

  p <- ggplot2::ggplot() +
    jaspGraphs::geom_line(data = dataset, mapping = ggplot2::aes(x = index, y = Measurement, group = parts)) +
    jaspGraphs::geom_point(data = dataset, ggplot2::aes(x = index, y = Measurement)) +
    ggplot2::geom_hline(yintercept = options$biasReferenceValue, data = dataset,
                        mapping = ggplot2::aes(x = index, y = Measurement), color = "darkgreen") +
    ggplot2::geom_label(data = data.frame(x = max(index), y = options$biasReferenceValue, l = "Ref"),
                        ggplot2::aes(x = x, y = y, label = l), vjust="inward",hjust="inward", color = "darkgreen" ) +
    ggplot2::scale_x_discrete(name = "Index", breaks = c(seq(1, max(index), 5),max(index)))

  #p <- p +

  p <- jaspGraphs::themeJasp(p) + ggplot2::theme(plot.margin = ggplot2::unit(c(.5, .5, .5, .5), "cm"))

  plot$plotObject <- p

  return(plot)
}

.gaugeScatterPlotOperators <- function(dataset, measurements, parts, operators, options){

  plot <- createJaspPlot(title = gettext("Scatterplot of Operator A vs Operator B"))
  plot$dependOn(c("gaugeScatterPlotOperators", "gaugeScatterPlotFitLine", "gaugeScatterPlotOriginLine"))

  if(length(unique(dataset[[operators]])) > 2){
    plot$setError(gettext("Plotting not possible: More than 2 Operators"))
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
