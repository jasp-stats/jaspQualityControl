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

msaGaugeRR <- function(jaspResults, dataset, options, ...) {

  measurements <- unlist(options$measurements)
  parts <- unlist(options$parts)
  operators <- unlist(options$operators)

  numeric.vars <- measurements

  factor.vars <- c(parts, operators)
  factor.vars <- factor.vars[factor.vars != ""]

  ready <- (length(measurements) != 0 && operators != "" && parts != "")
  readyRangeMethod <- length(measurements) == 2

  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = numeric.vars, columns.as.factor = factor.vars,
                                         exclude.na.listwise = c(numeric.vars, factor.vars))
  }


  .msaCheckErrors(dataset, options)


  #ANOVA method

  if (options$gaugeRRmethod == "anovaMethod") {

    # Gauge r&R ANOVA Table
    if (options[["gaugeANOVA"]]) {
      if (is.null(jaspResults[["gaugeANOVA"]])) {
        jaspResults[["gaugeANOVA"]] <- createJaspContainer(gettext("Gauge r&R ANOVA Table"))
        jaspResults[["gaugeANOVA"]]$dependOn(c("historicalStandardDeviation", "studyStandardDeviation", "standardDeviationReference", "historicalStandardDeviationValue"))
        jaspResults[["gaugeANOVA"]]$position <- 1
      }

      jaspResults[["gaugeANOVA"]] <- .gaugeANOVA(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready)
    }

    # Gauge r&R ANOVA Table
    if (options[["gaugeDescriptives"]]) {
      if (is.null(jaspResults[["gaugeDescriptives"]])) {
        jaspResults[["gaugeDescriptives"]] <- createJaspContainer(gettext("Descriptives Table"))
        jaspResults[["gaugeDescriptives"]]$position <- 2
      }
      jaspResults[["gaugeDescriptives"]] <- .gaugeDescriptives(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready)
    }

    # R chart by operator
    if (options[["gaugeRchart"]]) {
      if (is.null(jaspResults[["gaugeRchart"]])) {
        jaspResults[["gaugeRchart"]] <- createJaspContainer(gettext("Range Chart by Operator"))
        jaspResults[["gaugeRchart"]]$position <- 3
      }
      jaspResults[["gaugeRchart"]]$dependOn("gaugeRchart")
      jaspResults[["gaugeRchart"]] <- .xBarOrRangeChart(type = "Range", dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready)
    }

    # Xbar chart by operator
    if (options[["gaugeXbarChart"]]) {
      if (is.null(jaspResults[["gaugeXbarChart"]])) {
        jaspResults[["gaugeXbarChart"]] <- createJaspContainer(gettext("Xbar Chart by Operator"))
        jaspResults[["gaugeXbarChart"]]$position <- 4
      }
      jaspResults[["gaugeXbarChart"]]$dependOn("gaugeXbarChart")
      jaspResults[["gaugeXbarChart"]] <- .xBarOrRangeChart(type = "Xbar",dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready)
    }

    # gauge Scatter Plot Operators
    if (options[["gaugeScatterPlotOperators"]]) {
      if (is.null(jaspResults[["gaugeScatterOperators"]])) {
        jaspResults[["gaugeScatterOperators"]] <- createJaspContainer(gettext("Scatterplot Operators"))
        jaspResults[["gaugeScatterOperators"]]$position <- 5
      }
      jaspResults[["gaugeScatterOperators"]] <- .gaugeScatterPlotOperators(jaspResults = jaspResults, dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready)
    }

    # Measurement by Part Graph
    if (options[["gaugeByPart"]]) {
      if (is.null(jaspResults[["gaugeByPart"]])) {
        jaspResults[["gaugeByPart"]] <- createJaspContainer(gettext("Measurement by Part Graph"))
        jaspResults[["gaugeByPart"]]$position <- 6
      }
      jaspResults[["gaugeByPart"]] <- .gaugeByPartGraph(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options = options)
    }

    # Measurement by Operator Box Plot
    if (options[["gaugeByOperator"]]) {
      if (is.null(jaspResults[["gaugeByOperator"]])) {
        jaspResults[["gaugeByOperator"]] <- createJaspContainer(gettext("Measurement by Operator Graph"))
        jaspResults[["gaugeByOperator"]]$position <- 7
      }
      jaspResults[["gaugeByOperator"]] <- .gaugeByOperatorGraph(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready)
    }

    # Parts by Operator Interaction Plot
    if (options[["gaugeByInteraction"]]) {
      if (is.null(jaspResults[["gaugeByInteraction"]])) {
        jaspResults[["gaugeByInteraction"]] <- createJaspContainer(gettext("Parts by Operator Interaction Graph"))
        jaspResults[["gaugeByInteraction"]]$position <- 8
      }
      jaspResults[["gaugeByInteraction"]] <- .gaugeByInteractionGraph(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready)
    }

  }


  # Range Method

  if (options$gaugeRRmethod == "rangeMethod") {

    # Range Method r and R table
    if (options[["rangeRr"]]) {
      .rAndRtableRange(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, jaspResults, ready = readyRangeMethod)
    }

    # Scatter Plot Operators vs Parts
    if (options[["rangeScatterPlotOperatorParts"]]) {
      if (is.null(jaspResults[["ScatterOperatorParts"]])) {
        jaspResults[["ScatterOperatorParts"]] <- createJaspContainer(gettext("Scatterplot Operators vs Parts"))
        jaspResults[["ScatterOperatorParts"]]$position <- 9
      }
      jaspResults[["ScatterOperatorParts"]] <- .ScatterPlotOperatorParts(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = readyRangeMethod)
    }

    # Scatter Plot Operators
    if (options[["rangeScatterPlotOperators"]]) {
      if (is.null(jaspResults[["ScatterOperators"]])) {
        jaspResults[["ScatterOperators"]] <- createJaspContainer(gettext("Scatterplot Operators"))
        jaspResults[["ScatterOperators"]]$position <- 10
      }
      jaspResults[["ScatterOperators"]] <- .ScatterPlotOperators(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = readyRangeMethod)
    }

    # Rchart Range method
    if (options[["rangeRchart"]]) {
      if (is.null(jaspResults[["rangeRchart"]])) {
        jaspResults[["rangeRchart"]] <- createJaspContainer(gettext("Range Method R Chart"))
        jaspResults[["rangeRchart"]]$position <- 11
      }
      plot <- createJaspPlot(title = gettext("Range Chart by Operator"), width = 600, height = 300)
      plot$dependOn(c("rangeRchart"))
      p <- .RchartNoId(dataset = dataset[measurements], options = options, warningLimits = FALSE)
      plot$plotObject <- p
      jaspResults[["rangeRchart"]] <- plot
    }



  }
  return()
}

.gaugeANOVA <- function(dataset, measurements, parts, operators, options, ready) {

  anovaTables <- createJaspContainer(gettext("ANOVA Tables"))
  anovaTables$dependOn(c("gaugeANOVA", "gaugeRRmethod"))
  anovaTables$position <- 1

  for (measurement in measurements) {
    if (length(dataset[[measurement]]) <= 1)
      ready <- FALSE
  }

  if (ready && length(measurements) >= 2) {
    data <- dataset

    data <- tidyr::gather(data, repetition, measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

    if (options$studyVarMultiplierType == "svmSD") {
      studyVarMultiplier <- options$studyVarMultiplier
    }else{
      percent <- options$studyVarMultiplier/100
      q <- (1 - percent)/2
      studyVarMultiplier <- abs(2*qnorm(q))
    }
    anovaTable1 <- createJaspTable(title = gettext("Two-Way ANOVA Table with Interaction"))

    anovaTable1$addColumnInfo(title = gettext("Sources"),       name = "sources",   type = "string" )
    anovaTable1$addColumnInfo(title = gettext("df"),             name = "Df",      type = "integer")
    anovaTable1$addColumnInfo(title = gettext("Sum of Squares"), name = "Sum Sq",  type = "number")
    anovaTable1$addColumnInfo(title = gettext("Mean Square"),    name = "Mean Sq", type = "number")
    anovaTable1$addColumnInfo(title = gettext("F"),              name = "F value", type = "number")
    anovaTable1$addColumnInfo(title = gettext("p"),              name = "Pr(>F)",  type = "pvalue")

    formula1 <- as.formula(paste("measurement ~", parts,"*", operators))

    anova1 <- summary(aov(formula = formula1, data = data))
    fvaluePart <- anova1[[1]]$`Mean Sq`[1] / anova1[[1]]$`Mean Sq`[3]
    fvalueOperator <- anova1[[1]]$`Mean Sq`[2] / anova1[[1]]$`Mean Sq`[3]
    fvaluePartxOperator <- anova1[[1]]$`Mean Sq`[3] / anova1[[1]]$`Mean Sq`[4]
    fValues <- c(fvaluePart, fvalueOperator, fvaluePartxOperator)
    df <- anova1[[1]]$Df
    p1 <- pf(fValues[1], df[1], df[3], lower.tail = F)
    p2 <- pf(fValues[2], df[2], df[3], lower.tail = F)
    p3 <- anova1[[1]]$`Pr(>F)`[3]
    pValues <- c(p1, p2, p3)

    anovaTable1$setData(list( "sources"              = c(parts, operators, paste(parts," x ", operators), "Repeatability", "Total"),
                              "Df"                 = c(df, sum(anova1[[1]]$Df)),
                              "Sum Sq"             = c(anova1[[1]]$`Sum Sq`, sum(anova1[[1]]$`Sum Sq`)),
                              "Mean Sq"            = anova1[[1]]$`Mean Sq`,
                              "F value"            = fValues,
                              "Pr(>F)"             = pValues))

    anovaTables[['anovaTable1']] <- anovaTable1

    if (anova1[[1]]$`Pr(>F)`[3] < options$alphaForANOVA) {


      RRtable1 <- createJaspTable(title = gettext("Gauge r & R Variance Components"))

      RRtable1$dependOn(c("gaugeANOVA", "operators", "parts", "measurements"))

      RRtable1$addColumnInfo(name = "Source", title = gettext("Source"), type = "string")
      RRtable1$addColumnInfo(name = "Variation", title = gettext("Variation"), type = "number")
      RRtable1$addColumnInfo(name = "Percent", title = gettext("% Contribution"), type = "number")



      repeatability <- anova1[[1]]$`Mean Sq`[4]

      if (anova1[[1]]$`Mean Sq`[2] < anova1[[1]]$`Mean Sq`[3]) {
        operator <- 0
      }else{
        operator <- (anova1[[1]]$`Mean Sq`[2] - anova1[[1]]$`Mean Sq`[3]) / (length(unique(data[[parts]])) * length(measurements))
      }

      if (anova1[[1]]$`Mean Sq`[3] < anova1[[1]]$`Mean Sq`[4]) {
        operatorXpart <- 0
      }else{
        operatorXpart <- (anova1[[1]]$`Mean Sq`[3] - anova1[[1]]$`Mean Sq`[4]) / length(measurements)
      }

      partToPart <- (anova1[[1]]$`Mean Sq`[1] - anova1[[1]]$`Mean Sq`[3]) / (length(measurements) * length(unique(data[[operators]])))
      reproducibility <- operator + operatorXpart
      totalRR <- repeatability + reproducibility
      totalVar <- totalRR + partToPart



      RRtable1$setData(list(      "Source"       = gettext(c("Total Gauge r & R", "Repeatability", "Reproducibility", "Operator", "Part-To-Part", "Operator x Part", "Total Variation")),
                                  "Variation"    = c(totalRR, repeatability, reproducibility, operator, partToPart, operatorXpart, totalVar),
                                  "Percent"      = c(totalRR, repeatability, reproducibility, operator, partToPart, operatorXpart, totalVar) / totalVar * 100))


      anovaTables[['RRtable1']] <- RRtable1

      RRtable2 <- createJaspTable(title = gettext("Gauge Evaluation"))

      RRtable2$dependOn(c("gaugeANOVA", "operators", "parts", "measurements"))

      RRtable2$addColumnInfo(name = "source", title = gettext("Source"), type = "string")
      RRtable2$addColumnInfo(name = "SD", title = gettext("Std. Deviation"), type = "number")
      RRtable2$addColumnInfo(name = "studyVar", title = gettextf("Study variation"), type = "number")
      RRtable2$addColumnInfo(name = "percentStudyVar", title = gettext("% Study variation"), type = "number")
      RRtable2$addColumnInfo(name = "percentTolerance", title = gettext("% Tolerance"), type = "number")

      histSD <- options$historicalStandardDeviationValue

      if (options$standardDeviationReference == "historicalStandardDeviation" && histSD >= sqrt(totalRR)) {
        SD <- c(sqrt(c(totalRR, repeatability, reproducibility, operator)),
                sqrt(histSD^2 - totalRR),
                sqrt(operatorXpart), histSD)

      }else{
        SD <- sqrt(c(totalRR, repeatability, reproducibility, operator, partToPart, operatorXpart, totalVar))
      }
      studyVar <- SD * studyVarMultiplier


      RRtable2$setData(list(      "source"       = gettext(c("Total Gauge r & R", "Repeatability", "Reproducibility", "Operator", "Part-To-Part", "Operator x Part", "Total Variation")),
                                  "SD"           = SD,
                                  "studyVar"    = studyVar,
                                  "percentStudyVar"    = studyVar/max(studyVar) * 100,
                                  "percentTolerance" = studyVar / options$tolerance * 100))
      nCategories <- .gaugeNumberDistinctCategories(SD[5], SD[1])
      RRtable2$addFootnote(gettextf("Number of distinct categories = %i", nCategories))
      RRtable2$addFootnote(gettextf("Study variation is calculated as Std. Deviation * %.2f", studyVarMultiplier))


      anovaTables[['RRtable2']] <- RRtable2

    }else if (anova1[[1]]$`Pr(>F)`[3] > options$alphaForANOVA) {
      anovaTable2 <- createJaspTable(title = gettext("Two-Way ANOVA Table without Interaction"))

      anovaTable2$addColumnInfo(title = gettext("Sources"),        name = "sources",   type = "string" )
      anovaTable2$addColumnInfo(title = gettext("df"),             name = "Df",      type = "integer")
      anovaTable2$addColumnInfo(title = gettext("Sum of Squares"), name = "Sum Sq",  type = "number")
      anovaTable2$addColumnInfo(title = gettext("Mean Square"),    name = "Mean Sq", type = "number")
      anovaTable2$addColumnInfo(title = gettext("F"),              name = "F value", type = "number")
      anovaTable2$addColumnInfo(title = gettext("p"),              name = "Pr(>F)",  type = "pvalue")

      formula2 <- as.formula(paste("measurement ~",parts, "+",operators))

      anova2 <- summary(aov(formula = formula2, data = data))


      anovaTable2$setData(list( "sources"              = c(parts, operators, "Repeatability", "Total"),
                                "Df"                 = c(anova2[[1]]$Df, sum(anova2[[1]]$Df)),
                                "Sum Sq"             = c(anova2[[1]]$`Sum Sq`, sum(anova2[[1]]$`Sum Sq`)),
                                "Mean Sq"            = anova2[[1]]$`Mean Sq`,
                                "F value"            = anova2[[1]]$`F value`,
                                "Pr(>F)"             = anova2[[1]]$`Pr(>F)`))


      anovaTables[['anovaTable2']] <- anovaTable2


      RRtable1 <- createJaspTable(title = gettext("Gauge r & R Variance Components"))

      RRtable1$dependOn(c("gaugeANOVA", "operators", "parts", "measurements"))

      RRtable1$addColumnInfo(name = "Source", title = gettext("Source"), type = "string")
      RRtable1$addColumnInfo(name = "Variation", title = gettext("Variation"), type = "number")
      RRtable1$addColumnInfo(name = "Percent", title = gettext("% Contribution"), type = "number")



      repeatability <- anova2[[1]]$`Mean Sq`[3]

      if (anova2[[1]]$`Mean Sq`[2] < anova2[[1]]$`Mean Sq`[3]) {
        operator <- 0
      }else{
        operator <- (anova2[[1]]$`Mean Sq`[2] - anova2[[1]]$`Mean Sq`[3]) / (length(unique(data[[parts]])) * length(measurements))
      }

      if (anova2[[1]]$`Mean Sq`[1] < anova2[[1]]$`Mean Sq`[3]) {
        partToPart <- 0
      }else{
        partToPart <- (anova2[[1]]$`Mean Sq`[1] - anova2[[1]]$`Mean Sq`[3]) / (length(measurements) * length(unique(data[[operators]])))
      }
      reproducibility <- operator
      totalRR <- repeatability + reproducibility
      totalVar <- totalRR + partToPart



      RRtable1$setData(list(      "Source"       = gettext(c("Total Gauge r & R", "Repeatability", "Reproducibility", "Operator", "Part-To-Part", "Total Variation")),
                                  "Variation"    = c(totalRR, repeatability, reproducibility, operator, partToPart, totalVar),
                                  "Percent"      = c(totalRR, repeatability, reproducibility, operator, partToPart, totalVar) / totalVar * 100))


      anovaTables[['RRtable1']] <- RRtable1

      RRtable2 <- createJaspTable(title = gettext("Gauge Evaluation"))

      RRtable2$dependOn(c("gaugeANOVA", "operators", "parts", "measurements"))

      RRtable2$addColumnInfo(name = "source", title = gettext("Source"), type = "string")
      RRtable2$addColumnInfo(name = "SD", title = gettext("Std. Deviation"), type = "number")
      RRtable2$addColumnInfo(name = "studyVar", title = gettextf("Study variation"), type = "number")
      RRtable2$addColumnInfo(name = "percentStudyVar", title = gettext("% Study variation"), type = "number")
      RRtable2$addColumnInfo(name = "percentTolerance", title = gettext("% Tolerance"), type = "number")

      histSD <- options$historicalStandardDeviationValue

      if (options$standardDeviationReference == "historicalStandardDeviation" && histSD >= sqrt(totalRR)) {
        SD <- c(sqrt(c(totalRR, repeatability, reproducibility, operator)),
                sqrt(histSD^2 - totalRR), histSD)
      }else{
        SD <- sqrt(c(totalRR, repeatability, reproducibility, operator, partToPart, totalVar))
      }

      studyVar <- SD * studyVarMultiplier


      RRtable2$setData(list(      "source"       = gettext(c("Total Gauge r & R", "Repeatability", "Reproducibility", "Operator", "Part-To-Part", "Total Variation")),
                                  "SD"           = SD,
                                  "studyVar"    = studyVar,
                                  "percentStudyVar"    = studyVar/max(studyVar) * 100,
                                  "percentTolerance" = studyVar / options$tolerance * 100))
      nCategories <- .gaugeNumberDistinctCategories(SD[5], SD[1])
      RRtable2$addFootnote(gettextf("Number of distinct categories = %i", nCategories))
      RRtable2$addFootnote(gettextf("Study variation is calculated as Std. Deviation * %.2f", studyVarMultiplier))


      anovaTables[['RRtable2']] <- RRtable2
    }

    if (options[["gaugeVarCompGraph"]]) {

      plot <- createJaspPlot(title = gettext("Components of Variation"), width = 850, height = 500)

      plot$dependOn(c("gaugeVarCompGraph"))

      percentContributionValues <- c(totalRR, repeatability, reproducibility, partToPart)/totalVar * 100
      SDs <- sqrt(c(totalRR, repeatability, reproducibility, partToPart))
      studyvars <- SDs * studyVarMultiplier
      studyVariationValues <- studyvars/max(studyVar) * 100
      percentToleranceValues <- studyvars / options$tolerance * 100

      plot$plotObject <- .gaugeVarCompGraph(percentContributionValues, studyVariationValues, percentToleranceValues)

      anovaTables[['VarCompGraph']] <- plot
    }
  }else {

    anovaTable1 <- createJaspTable(title = gettext("Two-Way ANOVA Table with Interaction"))

    anovaTable1$addColumnInfo(title = gettext("Sources"),          name = "sources",   type = "string" )
    anovaTable1$addColumnInfo(title = gettext("df"),             name = "Df",      type = "integer")
    anovaTable1$addColumnInfo(title = gettext("Sum of Squares"), name = "Sum Sq",  type = "number")
    anovaTable1$addColumnInfo(title = gettext("Mean Square"),    name = "Mean Sq", type = "number")
    anovaTable1$addColumnInfo(title = gettext("F"),              name = "F value", type = "number")
    anovaTable1$addColumnInfo(title = gettext("p"),              name = "Pr(>F)",  type = "pvalue")

    if (options$studyVarMultiplierType == "svmSD") {
      studyVarMultiplier <- options$studyVarMultiplier
    }else{
      percent <- options$studyVarMultiplier/100
      q <- (1 - percent)/2
      studyVarMultiplier <- abs(2*qnorm(q))
    }

    RRtable1 <- createJaspTable(title = gettext("Gauge r & R Variance Components"))

    RRtable1$dependOn(c("gaugeANOVA", "operators", "parts", "measurements"))

    RRtable1$addColumnInfo(name = "Source", title = gettext("Source"), type = "string")
    RRtable1$addColumnInfo(name = "Variation", title = gettext("Variation"), type = "number")
    RRtable1$addColumnInfo(name = "Percent", title = gettext("% Contribution"), type = "number")

    RRtable1$setData(list(      "Source"       = gettext(c("Total Gauge r & R", "Repeatability", "Reproducibility", "Operator", "Part-To-Part", "Operator x Part", "Total Variation"))))


    RRtable2 <- createJaspTable(title = gettext("Gauge Evaluation"))

    RRtable2$dependOn(c("gaugeANOVA", "operators", "parts", "measurements"))

    RRtable2$addColumnInfo(name = "source", title = gettext("Source"), type = "string")
    RRtable2$addColumnInfo(name = "SD", title = gettext("Std. Deviation"), type = "number")
    RRtable2$addColumnInfo(name = "studyVar", title = gettextf("Study variation"), type = "number")
    RRtable2$addColumnInfo(name = "percentStudyVar", title = gettext("% Study variation"), type = "number")
    RRtable2$addColumnInfo(name = "percentTolerance", title = gettext("% Tolerance"), type = "number")

    RRtable2$addFootnote(gettextf("Study variation is calculated as Std. Deviation * %.2f", studyVarMultiplier))

    RRtable2$setData(list("source"       = gettext(c("Total Gauge r & R", "Repeatability", "Reproducibility", "Operator", "Part-To-Part", "Operator x Part", "Total Variation"))))

    plot <- createJaspPlot(title = gettext("Components of Variation"), width = 850, height = 500)
    plot$dependOn(c("gaugeVarCompGraph"))

    anovaTables[['anovaTable1']] <- anovaTable1
    anovaTables[['RRtable1']] <- RRtable1
    anovaTables[['RRtable2']] <- RRtable2
    anovaTables[['plot']] <- plot


    if (length(measurements) >= 1 && operators != "" && parts != "") {
      RRtable1$setError(gettextf("Number of observations is < 2 in %s after grouping on %s", parts, operators))
      RRtable2$setError(gettextf("Number of observations is < 2 in %s after grouping on %s", parts, operators))
    }
  }
  return(anovaTables)
}

.gaugeDescriptives <- function(dataset, measurements, parts, operators, options, ready) {
  descriptivesTable <- createJaspTable(title = gettext("Descriptives"))
  descriptivesTable$dependOn("gaugeDescriptives")

  descriptivesTable$addColumnInfo(title = gettext("Operator"), name = "operator", type = "string" )
  descriptivesTable$addColumnInfo(title = gettext("Mean"), name = "mean",  type = "number")
  descriptivesTable$addColumnInfo(title = gettext("Std. Dev."), name = "sd",  type = "number")
  descriptivesTable$addColumnInfo(title = gettext("Min."), name = "min",  type = "number")
  descriptivesTable$addColumnInfo(title = gettext("Max."), name = "max",  type = "number")

  if (ready) {

    operatorVector <- as.character(unique(dataset[[operators]]))
    meanVector <- vector(mode = "numeric")
    sdVector <- vector(mode = "numeric")
    minVector <- vector(mode = "numeric")
    maxVector <- vector(mode = "numeric")

    for (op in operatorVector) {
      measurementsOneOperator <- subset.data.frame(dataset[measurements], dataset[operators] == op)
      measurementsOneOperator <- unlist(measurementsOneOperator)
      meanVector <- c(meanVector, mean(measurementsOneOperator))
      sdVector <- c(sdVector, sd(measurementsOneOperator))
      minVector <- c(minVector, min(measurementsOneOperator))
      maxVector <- c(maxVector, max(measurementsOneOperator))
    }

    allMeasurements <- unlist(dataset[measurements])

    operatorVector <- c(operatorVector, "Overall")
    meanVector <- c(meanVector, mean(allMeasurements))
    sdVector <- c(sdVector, sd(allMeasurements))
    minVector <- c(minVector, min(allMeasurements))
    maxVector <- c(maxVector, max(allMeasurements))

    descriptivesTable$setData(list("operator" = operatorVector,
                                   "mean" = meanVector,
                                   "sd" = sdVector,
                                   "min" = minVector,
                                   "max" = maxVector))
  }
  return(descriptivesTable)
}

.xBarOrRangeChart <- function(type = c("Xbar", "Range"), dataset, measurements, parts, operators, options, ready) {
  if (!ready) {
    plot <- createJaspPlot(title = gettextf("%s Chart by Operator", type), width = 600, height = 300)
    return(plot)
  }
  if (length(measurements) < 2) {
    plot <- createJaspPlot(title = gettextf("%s Chart by Operator"), width = 600, height = 300)
    plot$setError(gettext("More than 1 Measurement per Operator required."))
    return(plot)
  }
  plotContainer <- createJaspContainer(gettextf("%s Chart by Operator", type))
  operatorVector <- unique(dataset[[operators]])
  data <- dataset[measurements]
  if (type == "Range") {
    ChartData <- qcc::qcc(data, type= 'R', plot = FALSE)
  }else{
    ChartData <- qcc::qcc(data, type= 'xbar', plot = FALSE)
  }
  center <- ChartData$center
  UCL <- max(ChartData$limits)
  LCL <- min(ChartData$limits)
  manualLimits <- c(LCL, center, UCL)
  for (op in operatorVector) {
    dataPerOP <- subset(dataset, dataset[operators] == op)
    plot <- createJaspPlot(title = gettextf("Operator %s", op), width = 600, height = 300)
    if (type == "Range") {
      p <- .RchartNoId(dataset = dataPerOP[measurements], options = options, manualLimits = manualLimits, warningLimits = FALSE)
    }else{
      p <- .XbarchartNoId(dataset = dataPerOP[measurements], options = options, manualLimits = manualLimits, warningLimits = FALSE)
    }
    plot$plotObject <- p
    plotContainer[[op]] <- plot
  }
  return(plotContainer)
}

.ScatterPlotOperators <- function(dataset, measurements, parts, operators, options, ready) {

  plot <- createJaspPlot(title = gettext("Scatterplot of Operator A vs Operator B"))
  plot$dependOn(c("rangeScatterPlotOperators", "rangeScatterPlotFitLine", "rangeScatterPlotOriginLine", "gaugeRRmethod"))

  if (ready) {

    p <- ggplot2::ggplot(data = dataset, ggplot2::aes_string(x = measurements[1], y = measurements[2])) +
      jaspGraphs::geom_point() + ggplot2::scale_x_continuous(limits = c(min(dataset[measurements])*0.9,max(dataset[measurements])*1.1)) +
      ggplot2::scale_y_continuous(limits = c(min(dataset[measurements])*0.9,max(dataset[measurements])*1.1))

    if (options[["rangeScatterPlotFitLine"]])
      p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE)

    if (options[["rangeScatterPlotOriginLine"]])
      p <- p + ggplot2::geom_abline(col = "gray", linetype = "dashed")

    p <- jaspGraphs::themeJasp(p)

    plot$plotObject <- p

  }

  return(plot)
}

.gaugeByPartGraph <- function(dataset, measurements, parts, operators, options) {
  plot <- createJaspPlot(title = gettext("Measurements by Part"))
  p <- .gaugeByPartGraphPlotObject(dataset, measurements, parts, operators, displayAll = options$gaugeByPartAll)
  plot$plotObject <- p
  options$gaugeByPartAll
  return(plot)
}

.gaugeByPartGraphPlotObject <- function(dataset, measurements, parts, operators, displayAll = FALSE) {
  dataset <- tidyr::gather(dataset, Repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)
  means <- aggregate(dataset["Measurement"], dataset[parts], mean)
  p <- ggplot2::ggplot()
  if (displayAll)
    p <- p + jaspGraphs::geom_point(data = dataset, ggplot2::aes_string(x = parts, y = "Measurement"), col = "gray")
  p <- p + jaspGraphs::geom_point(data = means, ggplot2::aes_string(x = parts, y = "Measurement")) +
    ggplot2::scale_y_continuous(limits = c(min(dataset["Measurement"]) * 0.9, max(dataset["Measurement"]) * 1.1)) +
    jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()
  return(p)
}


.gaugeByOperatorGraph <- function(dataset, measurements, parts, operators, options, ready) {

  plot <- createJaspPlot(title = gettext("Measurement by Operator"))

  plot$dependOn(c("gaugeByOperator", "gaugeRRmethod"))

  if (ready) {
    dataset <- tidyr::gather(dataset, Repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

    p <- ggplot2::ggplot() +
      ggplot2::geom_boxplot(data = dataset, ggplot2::aes_string(x = operators, y = "Measurement"))

    p <- jaspGraphs::themeJasp(p)

    plot$plotObject <- p
  }
  return(plot)
}

.gaugeByInteractionGraph <- function(dataset, measurements, parts, operators, options, ready) {

  plot <- createJaspPlot(title = gettext("Parts by Operator Interaction"), width = 700, height = 400)
  plot$dependOn(c("gaugeByInteraction", "gaugeRRmethod"))

  if (ready) {
    byOperator <- split.data.frame(dataset, dataset[operators])
    partNames <- unique(dataset[[parts]])
    meansPerOperator <- data.frame(Part = factor(partNames, partNames))

    for (name in names(byOperator)) {
      if (nrow(byOperator[[name]][measurements]) != length(partNames)) {
        plot$setError(gettext("Operators measured different number of parts."))
        return(plot)
      }
      meansPerOperator <- cbind(meansPerOperator, rowMeans(byOperator[[name]][c(measurements)]))
    }

    colnames(meansPerOperator)[-1] <- names(byOperator)
    tidydata <- tidyr::gather(meansPerOperator, key = "Operator", value = "Measurements", -Part )

    p <- ggplot2::ggplot(tidydata, ggplot2::aes(x = Part, y = Measurements, col = Operator, group = Operator)) +
      jaspGraphs::geom_line() +
      jaspGraphs::geom_point() +
      ggplot2::scale_y_continuous(name = "Measurement", limits = c(min(meansPerOperator[names(byOperator)]) * 0.9, max(meansPerOperator[names(byOperator)]) * 1.1)) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(legend.position = 'right')
    plot$plotObject <- p
  }
  return(plot)
}

.ScatterPlotOperatorParts <- function(dataset, measurements, parts, operators, options, ready) {


  plot <- createJaspPlot(title = gettext("Scatterplot of Operator A, Operator B vs Part"), width = 500, height = 320)
  plot$dependOn(c("rangeScatterPlotOperatorParts", "gaugeRRmethod"))

  if (ready) {
    partIndex <- 1:length(dataset[[measurements[1]]])
    dataset <- cbind(dataset, Parts = factor(partIndex, partIndex))

    p <- ggplot2::ggplot() +
      jaspGraphs::geom_point(data = dataset, ggplot2::aes_string(x = "Parts", y = measurements[1]), fill = "red", size = 4) +
      jaspGraphs::geom_point(data = dataset, ggplot2::aes_string(x = "Parts", y = measurements[2]), fill = "green", size = 4) +
      ggplot2::ylab("Measurements")

    p <- jaspGraphs::themeJasp(p) + ggplot2::theme(legend.position = "right")

    plot$plotObject <- p
  }

  return(plot)
}

.rAndRtableRange <- function(dataset, measurements, parts, operators, options, jaspResults, ready) {

  table <- createJaspTable(title = gettext("r & R Table"))
  table$position <- 2
  table$dependOn(c("rangeRr", "gaugeRRmethod"))

  table$addColumnInfo(name = "Rbar", title = gettext("R-bar"), type = "number")
  table$addColumnInfo(name = "d2", title = gettext("d2"), type = "number")
  table$addColumnInfo(name = "PSD", title = gettext("Process Std. Dev."), type = "number")
  table$addColumnInfo(name = "GRR", title = gettext("GRR"), type = "number")
  table$addColumnInfo(name = "GRRpercent", title = gettext("%GRR"), type = "number")

  jaspResults[["rAndR2"]] <- table

  if (ready) {

    if (nrow(dataset[measurements]) > 20)
      table$setError("Range Method available for max. 20 unique Parts")

    n <- length(dataset[[measurements[1]]])
    Rbar <- sum(abs(dataset[measurements[1]] - dataset[measurements[2]]) / n)
    d2 <- .d2Value(n)
    SD <- options$rangePSD
    GRR <- Rbar/d2
    GRRpercent <- GRR/SD*100


    table$addRows(list(      "Rbar"       = Rbar,
                             "d2"         = d2,
                             "PSD"        = SD,
                             "GRR"        = GRR,
                             "GRRpercent" = GRRpercent))
  }
}


.gaugeScatterPlotOperators <- function(jaspResults, dataset, measurements, parts, operators, options, ready) {

  singleEmptyPlot <- createJaspPlot(title = gettext("Scatterplot of Operator vs Operator"))
  singleEmptyPlot$dependOn(c("gaugeScatterPlotOperators", "gaugeScatterPlotFitLine", "gaugeScatterPlotOriginLine", "gaugeRRmethod"))

  if (!ready)
    return(singleEmptyPlot)

  operatorVector <- as.character(unique(dataset[[operators]]))
  len <- length(operatorVector)

  if (len < 2) {
    singleEmptyPlot$setError(gettext("Cannot plot scatterplot for less than 2 operators."))
    return(singleEmptyPlot)
  }else{
    singlePlot <- createJaspPlot(title = gettextf("Scatterplot of Operator  %s vs Operator %s", operatorVector[1], operatorVector[2]))
    singlePlot$dependOn(c("gaugeScatterPlotOperators", "gaugeScatterPlotFitLine", "gaugeScatterPlotOriginLine", "gaugeRRmethod"))
    operatorSplit <- split.data.frame(dataset, dataset[operators])
    nparts <- length(unique(subset(dataset, dataset[operators] == operatorVector[1])[[parts]]))
    for (op in operatorVector) {
      if (length(unique(subset(dataset, dataset[operators] == op)[[parts]])) != nparts) {
        singlePlot$setError(gettext("Operators measured different number of parts."))
        return(singlePlot)
      }
    }
    if (len == 2) {
      singlePlot$plotObject <- .singleScatterPlot(operator1 = operatorVector[1], operator2 = operatorVector[2],
                                                  data = operatorSplit, options = options, measurements = measurements)
      return(singlePlot)
    }else{
      matrixPlot <- createJaspPlot(title = gettext("Matrix Plot for Operators"), width = 700, height = 700)
      plotMat <- matrix(list(), len, len)
      for (row in 1:len) {
        for (col in 1:len) {
          if (row >= col) {
            plotMat[[row, col]] <- ggplot2::ggplot() + ggplot2::theme_void()
          }else{
            plotMat[[row, col]] <- .singleScatterPlot(operator1 = operatorVector[row], operator2 = operatorVector[col],
                                                      data = operatorSplit, options = options, measurements = measurements, axisLabels = FALSE)
          }
        }
      }
      labels <- paste(operators, operatorVector)
      p <- jaspGraphs::ggMatrixPlot(plotMat, leftLabels = labels, topLabels = labels)
      matrixPlot$plotObject <- p
      return(matrixPlot)
    }
  }
}

.singleScatterPlot <- function(operator1, operator2, data, options, measurements, axisLabels = T) {
  df <- data.frame(Operator1 = rowMeans(data[[operator1]][measurements]), Operator2 = rowMeans(data[[operator2]][measurements]))
  if (axisLabels) {
    xlab <- paste("Operator", operator2)
    ylab <- paste("Operator", operator1)
  }else{
    xlab <- ylab <- ggplot2::element_blank()
  }
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(df$Operator1)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df$Operator2)
  p <- ggplot2::ggplot(data = df, ggplot2::aes(x = Operator2, y = Operator1)) +
    jaspGraphs::geom_point() + ggplot2::scale_x_continuous(name = xlab, breaks = xBreaks, limits = range(c(xBreaks, df$Operator2))) +
    ggplot2::scale_y_continuous(name = ylab, breaks = yBreaks, limits = range(c(yBreaks, df$Operator1)))
  if (options[["gaugeScatterPlotFitLine"]])
    p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE)
  if (options[["gaugeScatterPlotOriginLine"]])
    p <- p + ggplot2::geom_abline(col = "gray", linetype = "dashed")

  p <- jaspGraphs::themeJasp(p)

  return(p)
}

.msaCheckErrors <- function(dataset, options) {

  #if (options[["gaugeScatterPlotOperators"]]) {
  #  .hasErrors(dataset = dataset, type = "factorLevels",
  #             factorLevels.target  = options$operators, factorLevels.amount  = "> 2",
  #             exitAnalysisIfErrors = TRUE)
  #}

}

.d2Value <- function(n) {
  d2table <- data.frame(n = 1:20, d2 = c(1.41421, 1.27931, 1.23105, 1.20621, 1.19105, 1.18083, 1.17348, 1.16794, 1.16361, 1.16014,
                                         1.15729, 1.15490, 1.15289, 1.15115, 1.14965, 1.14833, 1.14717, 1.14613, 1.14520, 1.14437))

  return(d2table$d2[d2table$n == n])
}

.gaugeVarCompGraph <- function(percentContributionValues, studyVariationValues, percentToleranceValues) {

  plotframe <- data.frame(source = rep(gettext(c('Gauge r&R', 'Repeat', 'Reprod', 'Part-to-Part')), 3),
                          reference = rep(gettext(c('Percent Contribution', 'Percent Study Variation', 'Percent Tolerance')), each = 4),
                          value = c(percentContributionValues, studyVariationValues, percentToleranceValues))
  plotframe$source <- factor(plotframe$source, levels = c('Gauge r&R', 'Repeat', 'Reprod', 'Part-to-Part'))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotframe$value)
  p <- ggplot2::ggplot() + ggplot2::geom_bar(data = plotframe,
                                             mapping = ggplot2::aes(fill =  reference,  y = value, x = source),
                                             position="dodge", stat = "identity")
  p <- jaspGraphs::themeJasp(p) + ggplot2::theme(legend.position = 'right', legend.title = ggplot2::element_blank(),
                                                 plot.margin = ggplot2::unit(c(1,1,1,1),"cm")) +
    ggplot2::xlab("") + ggplot2::scale_y_continuous(name = "Percent", breaks = yBreaks, limits = range(c(yBreaks, plotframe$value)))
  return(p)

}

.gaugeNumberDistinctCategories <- function(sdPart, sdGauge) {
  nCategories <- (sdPart / sdGauge) * 1.41
  if (nCategories < 1)
    nCategories <- 1
  return(as.integer(nCategories))
}
