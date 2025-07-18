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
msaGaugeRRnonrep <- function(jaspResults, dataset, options, ...) {
  # Reading the data in the correct format
  wideFormat <- options[["dataFormat"]] == "wideFormat"
  if (wideFormat) {
    measurements <- unlist(options[["measurementsWideFormat"]])
    parts <- unlist(options[["partWideFormat"]])
    operators <- unlist(options[["operatorWideFormat"]])
  } else {
    measurements <- unlist(options[["measurementLongFormat"]])
    parts <- unlist(options[["partLongFormat"]])
    operators <- unlist(options[["operatorLongFormat"]])
  }
  numeric.vars <- measurements
  numeric.vars <- numeric.vars[numeric.vars != ""]
  factor.vars <- c(parts, operators)
  factor.vars <- factor.vars[factor.vars != ""]

  if(!wideFormat){
    ready <- (!identical(measurements, "") && !identical(operators, "") && !identical(parts, ""))
  }else{
    ready <- (length(measurements) > 1 && !identical(operators, "") && !identical(parts, ""))
  }
  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = numeric.vars, columns.as.factor = factor.vars)
  }

  .hasErrors(dataset, type = c('infinity', 'missingValues'),
             all.target = c(measurements, parts, operators),
             exitAnalysisIfErrors = TRUE)

  # check for balanced design
  if (ready) {
    replicatesTable <- table(dataset[[parts]])
    if (!all(replicatesTable == replicatesTable[1])) {
      plot <- createJaspPlot(title = gettext("Gauge r&R - nested"), width = 700, height = 400)
      jaspResults[["gaugeRRNonRep"]] <- plot
      plot$setError(gettext("Unbalanced design. Some parts have more repeated measurements than others."))
      return()
    }
  }

  if (ready && !wideFormat){
    dataset <- dataset[order(dataset[[operators]]),]
    dataset <- dataset[order(dataset[[parts]]),]
    datasetLong <- dataset
    datasetWide <- .reshapeToWide(dataset, measurements, parts, operators)
    wideMeasurementCols <- colnames(datasetWide)[colnames(datasetWide) != c(parts, operators)]
    longMeasurementCols <- measurements
  }

  if (ready && wideFormat){
    dataset <- dataset[order(dataset[[operators]]),]
    dataset <- dataset[order(dataset[[parts]]),]
    datasetWide <- dataset
    datasetLong <- .reshapeToLong(dataset, measurements, parts, operators)
    wideMeasurementCols <- measurements
    longMeasurementCols <- "Measurement"
  }

  # Get Rule List
  if (ready) {
    ruleList1 <- .getRuleListSubgroupCharts(options, type = "xBar")
    ruleList2 <- .getRuleListSubgroupCharts(options, type = "R")
  }

  # Report
  if (options[["report"]]) {
    nElements <- sum(options[["reportVariationComponents"]], options[["reportRChartByOperator"]], options[["reportMeasurementsByOperatorPlot"]],
                     options[["reportAverageChartByOperator"]], options[["reportMetaData"]])
    plotHeight <- ceiling(nElements/2) * 500
    reportPlot <- createJaspPlot(title = gettext("Gauge r&R (non-replicable) report"), width = 1250, height = plotHeight)
    jaspResults[["report"]] <- reportPlot
    jaspResults[["report"]]$dependOn(c("measurementLongFormat", "operatorLongFormat", "partLongFormat", "measurementsWideFormat",
                                       "operatorWideFormat", "partWideFormat", "processVariationReference", "historicalSdValue",
                                       "tolerance", "toleranceValue", "studyVarianceMultiplierType", "studyVarianceMultiplierValue",
                                       "scatterPlotFitLine", "report", "reportMetaData", "reportTitle",
                                       "reportTitleText", "reportPartName", "reportPartNameText", "reportGaugeName",
                                       "reportGaugeNameText", "reportCharacteristic", "reportCharacteristicText",
                                       "reportGaugeNumber", "reportGaugeNumberText", "reportTolerance", "reportToleranceText",
                                       "reportLocation", "reportLocationText", "reportPerformedBy", "reportPerformedByText",
                                       "reportDate", "reportDateText", "reportVariationComponents", "reportMeasurementsByPartPlot",
                                       "reportRChartByOperator", "reportMeasurementsByOperatorPlot", "reportAverageChartByOperator",
                                       "reportPartByOperatorPlot", "reportTrafficLightCHart", "reportMetaData", .getDependenciesControlChartRules()))

    if (nElements == 0) {
      reportPlot$setError(gettext("No report components selected."))
      return()
    }

    if(!ready)
      return()

    # Plot meta data
    if (options[["reportTitle"]] ) {
      title <- if (options[["reportTitleText"]] == "") gettext("Gauge r&R (non-replicable) report") else options[["reportTitleText"]]
    } else {
      title <- ""
    }

    if (options[["reportMetaData"]]) {
      text <- c()
      text <- if (options[["reportPartName"]]) c(text, gettextf("Part name: %s", options[["reportPartNameText"]])) else text
      text <- if (options[["reportGaugeName"]]) c(text, gettextf("Gauge name: %s", options[["reportGaugeNameText"]])) else text
      text <- if (options[["reportCharacteristic"]]) c(text, gettextf("Characteristic: %s", options[["reportCharacteristicText"]])) else text
      text <- if (options[["reportGaugeNumber"]]) c(text, gettextf("Gauge number: %s", options[["reportGaugeNumberText"]])) else text
      text <- if (options[["reportTolerance"]]) c(text, gettextf("Tolerance: %s", options[["reportToleranceText"]])) else text
      text <- if (options[["reportLocation"]]) c(text, gettextf("Location: %s", options[["reportLocationText"]])) else text
      text <- if (options[["reportPerformedBy"]]) c(text, gettextf("Performed by: %s", options[["reportPerformedByText"]])) else text
      text <- if (options[["reportDate"]]) c(text, gettextf("Date: %s", options[["reportDateText"]])) else text
    } else {
      text <- NULL
    }

    plots <- list()
    plotIndexCounter <- 1
    if (options[["reportVariationComponents"]]) {
      plots[[plotIndexCounter]] <- .gaugeRRNonRep(datasetLong, longMeasurementCols, parts = parts, operators = operators,
                                                  options =  options, ready = TRUE, plotOnly = TRUE)   #var. comp. plot
      plotIndexCounter <- plotIndexCounter + 1
    }
    if (options[["reportRChartByOperator"]]) {
      plots[[plotIndexCounter]] <- .controlChart(dataset = datasetWide[c(wideMeasurementCols, operators)], ruleList = ruleList2,
                                                 plotType = "R", stages = operators,
                                                 xAxisLabels = datasetWide[[parts]][order(datasetWide[[operators]])],
                                                 stagesSeparateCalculation = FALSE)$plotObject #R chart by operator
      plotIndexCounter <- plotIndexCounter + 1
    }
    if (options[["reportMeasurementsByOperatorPlot"]]) {
      plots[[plotIndexCounter]] <- .gaugeByOperatorGraphPlotObject(datasetWide, wideMeasurementCols, parts, operators, options)   #Measurements by operator plot
      plotIndexCounter <- plotIndexCounter + 1
    }
    if (options[["reportAverageChartByOperator"]]) {
      plots[[plotIndexCounter]] <- .controlChart(dataset = datasetWide[c(wideMeasurementCols, operators)], ruleList = ruleList1,
                                                 plotType = "xBar", xBarSdType = "r", stages = operators,
                                                 xAxisLabels = datasetWide[[parts]][order(datasetWide[[operators]])],
                                                 stagesSeparateCalculation = FALSE)$plotObject #Average chart by operator
      plotIndexCounter <- plotIndexCounter + 1
    }

    if (options[["reportTrafficLightChart"]]) {
      traffPlotValues <- .gaugeRRNonRep(datasetLong, longMeasurementCols, parts, operators, options, ready, plotOnly = FALSE, trafficPlotValuesOnly = TRUE)
      percentMSVar <- traffPlotValues[1]
      percentMSTolerance <- traffPlotValues[2]
      trafficPlots <- .trafficplot(StudyVar = percentMSVar, ToleranceUsed = options[["tolerance"]],
                                   ToleranceVar = percentMSTolerance, options = options, ready = ready, ggPlot = TRUE)
      if (options[["tolerance"]]) {
        plots[[plotIndexCounter]] <- trafficPlots$p1
        plotIndexCounter <- plotIndexCounter + 1
        plots[[plotIndexCounter]] <- trafficPlots$p2
      } else {
        plots[[plotIndexCounter]] <- trafficPlots
      }
    }

    # Gauge evaluation table
    tables <- list()
    tableTitles <- list()
    if (options[["reportGaugeTable"]]) {
      gaugeEvalOutput <- .gaugeRRNonRep(datasetLong, longMeasurementCols, parts, operators, options, ready, plotOnly = FALSE,
                                        trafficPlotValuesOnly = FALSE, gaugeEvaluationDfOnly = TRUE)
      gaugeEvalDf <- gaugeEvalOutput[["gaugeEvalDf"]]
      nCategories <- gaugeEvalOutput[["nCategories"]]
      nCategoriesDf <- data.frame("x1" = gettext("Number of distinct categories"), "x2" = nCategories)
      names(nCategoriesDf) <- NULL
      tables[[1]] <- list(gaugeEvalDf, nCategoriesDf)
      tableTitles[[1]] <- list("Gauge evaluation", "")
    }

    reportPlotObject <- .qcReport(text = text, plots = plots, tables = tables, textMaxRows = 8,
                                  tableTitles = tableTitles, reportTitle = title, tableSize = 6)
    reportPlot$plotObject <- reportPlotObject
  } else {
    # Gauge r&R non replicable
    if (options[["anova"]]) {
      if (is.null(jaspResults[["gaugeRRNonRep"]])) {
        jaspResults[["gaugeRRNonRep"]] <- createJaspContainer(gettext("Gauge r&R tables"))
        jaspResults[["gaugeRRNonRep"]]$position <- 1
      }
      jaspResults[["gaugeRRNonRep"]] <- .gaugeRRNonRep(dataset = datasetLong, measurements = longMeasurementCols, parts = parts, operators = operators, options =  options, ready = ready)
      jaspResults[["gaugeRRNonRep"]]$dependOn(c("processVariationReference", "historicalSdValue", "toleranceValue", "studyVarianceMultiplierType",
                                                "studyVarianceMultiplierValue", "varianceComponentsGraph", "report", "anova"))
    }

    # R chart by operator
    if (options[["rChart"]] && is.null(jaspResults[["rChart"]])) {
      jaspResults[["rChart"]] <- createJaspContainer(gettext("Range chart by operator"))
      jaspResults[["rChart"]]$position <- 2
      jaspResults[["rChart"]]$dependOn(c("rChart", "measurementLongFormat", "measurementsWideFormat", "report", .getDependenciesControlChartRules()))
      jaspResults[["rChart"]][["plot"]] <- createJaspPlot(title = gettext("Range chart by operator"), width = 1200, height = 500)

      if (ready) {
        rChart <- .controlChart(dataset = datasetWide[c(wideMeasurementCols, operators)], ruleList = ruleList2,
                                plotType = "R", stages = operators,
                                xAxisLabels = datasetWide[[parts]][order(datasetWide[[operators]])],
                                stagesSeparateCalculation = FALSE)
        jaspResults[["rChart"]][["plot"]]$plotObject <- rChart$plotObject
        jaspResults[["rChart"]][["table"]] <- rChart$table
      }
    }

    # Xbar chart by operator
    if (options[["xBarChart"]] && is.null(jaspResults[["xBarChart"]])) {
      jaspResults[["xBarChart"]] <- createJaspContainer(gettext("Xbar chart by operator"))
      jaspResults[["xBarChart"]]$position <- 3
      jaspResults[["xBarChart"]]$dependOn(c("xBarChart", "measurementLongFormat", "measurementsWideFormat", "report", .getDependenciesControlChartRules()))
      jaspResults[["xBarChart"]][["plot"]] <- createJaspPlot(title = gettext("Average chart by operator"), width = 1200, height = 500)
      if (ready) {
        xBarChart <- .controlChart(dataset = datasetWide[c(wideMeasurementCols, operators)], ruleList = ruleList1,
                                   plotType = "xBar", xBarSdType = "r", stages = operators,
                                   xAxisLabels = datasetWide[[parts]][order(datasetWide[[operators]])],
                                   stagesSeparateCalculation = FALSE)
        jaspResults[["xBarChart"]][["plot"]]$plotObject <- xBarChart$plotObject
        jaspResults[["xBarChart"]][["table"]] <- xBarChart$table

      }
    }

    #Measurement by part x operator plot
    if (options[["partMeasurementPlot"]] & ready) {
      if (is.null(jaspResults[["NRpartOperatorGraph"]])) {
        jaspResults[["NRpartOperatorGraph"]] <- createJaspContainer(gettext("Measurement by part x operator plot"))
        jaspResults[["NRpartOperatorGraph"]]$position <- 4
      }
      jaspResults[["NRpartOperatorGraph"]] <- .gaugeMeasurmentsByPartXOperator(dataset = datasetWide, measurements = wideMeasurementCols, parts = parts, operators = operators, options = options)
      jaspResults[["NRpartOperatorGraph"]]$dependOn(c("partMeasurementPlot", "partMeasurementPlotAllValues", "report"))
    }

    #Measurement by operator plot
    if (options[["operatorMeasurementPlot"]]) {
      if (is.null(jaspResults[["NRoperatorGraph"]])) {
        jaspResults[["NRoperatorGraph"]] <- createJaspContainer(gettext("Gauge r&R tables"))
        jaspResults[["NRoperatorGraph"]]$position <- 5
      }

      jaspResults[["NRoperatorGraph"]] <- .gaugeByOperatorGraph(dataset = datasetWide, measurements = wideMeasurementCols, parts = parts, operators = operators, options = options, ready = ready)
      jaspResults[["NRoperatorGraph"]]$dependOn(c("NRoperatorGraph", "report"))
    }


    if (options[["trafficLightChart"]]) {
      if (is.null(jaspResults[["trafficLightChart"]])) {
        jaspResults[["trafficLightChart"]] <- createJaspContainer(gettext("Gauge r&R tables"))
        jaspResults[["trafficLightChart"]]$position <- 6
      }
      traffPlotValues <- .gaugeRRNonRep(datasetLong, longMeasurementCols, parts, operators, options, ready, plotOnly = FALSE,
                                        trafficPlotValuesOnly = TRUE)
      percentMSVar <- traffPlotValues[1]
      percentMSTolerance <- traffPlotValues[2]
      jaspResults[["trafficLightChart"]] <- .trafficplot(StudyVar = percentMSVar, ToleranceUsed = options[["tolerance"]],
                                                         ToleranceVar = percentMSTolerance, options = options,
                                                         ready = ready)
      jaspResults[["trafficLightChart"]]$dependOn(c("trafficLightChart", "report"))
    }
  }
}

.gaugeRRNonRep <- function(dataset, measurements, parts, operators, options, ready, plotOnly = FALSE, trafficPlotValuesOnly = FALSE,
                           gaugeEvaluationDfOnly = FALSE) {
  gaugeRRNonRepTables <- createJaspContainer(gettext("Gauge r&R study - nested ANOVA"))
  gaugeRRNonRepTables$position <- 1

  gaugeRRNonRepTable1 <- createJaspTable(title = gettext("Gauge r&R (nested)"))
  gaugeRRNonRepTable1$addColumnInfo(title = gettext("Source"),       name = "sources",   type = "string" )
  gaugeRRNonRepTable1$addColumnInfo(title = gettext("df"),             name = "DF",      type = "integer")
  gaugeRRNonRepTable1$addColumnInfo(title = gettext("Sum of squares"), name = "SS",  type = "number")
  gaugeRRNonRepTable1$addColumnInfo(title = gettext("Mean square"),    name = "MS", type = "number")
  gaugeRRNonRepTable1$addColumnInfo(title = gettext("F"),              name = "F.value", type = "number")
  gaugeRRNonRepTable1$addColumnInfo(title = gettext("p"),              name = "p.value",  type = "pvalue")

  gaugeRRNonRepTable2 <- createJaspTable(title = gettext("Variance components"))
  gaugeRRNonRepTable2$addColumnInfo(title = gettext("Source"),       name = "sources",   type = "string" )
  gaugeRRNonRepTable2$addColumnInfo(title = gettext("Variation"), name = "Var",  type = "number")
  gaugeRRNonRepTable2$addColumnInfo(title = gettextf("%% Contribution"),    name = "percentVar", type = "number")

  gaugeRRNonRepTable3 <- createJaspTable(title = gettext("Gauge evaluation"))
  gaugeRRNonRepTable3$addColumnInfo(name = "source", title = gettext("Source"), type = "string")
  gaugeRRNonRepTable3$addColumnInfo(name = "SD", title = gettext("Std. dev."), type = "number")
  gaugeRRNonRepTable3$addColumnInfo(name = "studyVar", title = gettextf("Study variation"), type = "number")
  gaugeRRNonRepTable3$addColumnInfo(name = "percentStudyVar", title = gettextf("%% Study variation"), type = "number")
  if(options[["tolerance"]])
    gaugeRRNonRepTable3$addColumnInfo(name = "percentTolerance", title = gettextf("%% Tolerance"), type = "number")

  if (ready) {
    anovaTable <- .gaugeNestedANOVA(dataset, operators, parts, measurements)
    anovaSources <- as.character(anovaTable$Source)
    df <- anovaTable$DF
    ss <- anovaTable$SS
    ms <- anovaTable$MS
    f <- anovaTable$F.value
    p <- anovaTable$p.value
    if (any(df < 1)) {
      if (plotOnly) {
        p <- ggplot2::ggplot() +
          jaspGraphs::themeJaspRaw() +
          jaspGraphs::geom_rangeframe()
        return(p)
      } else {
        gaugeRRNonRepTable1$setError(gettextf("No degrees of freedom for %s.", paste(anovaSources[which(df < 1)], collapse = ", ")))
        gaugeRRNonRepTables[["Table1"]] <- gaugeRRNonRepTable1
        return(gaugeRRNonRepTables)
      }
    }
    gaugeRRNonRepTable1$setData(list("sources" = anovaSources,
                                     "DF" = df,
                                     "SS" = ss,
                                     "MS" = ms,
                                     "F.value" = f,
                                     "p.value" = p))

    histSD <- options[["historicalSdValue"]]
    varCompTable <- .gaugeNestedVarComponents(dataset, operators, parts, measurements, ms)
    if (options[["processVariationReference"]] == "historicalSd") {
      varCompTable$varPart <-  histSD^2 - varCompTable$varGaugeTotal
      varCompTable$varTotal <- histSD^2
      }
    varSources <- gettext(c("Total gauge r&R", "Repeatability", "Reproducibility", "Part-To-part", "Total variation"))
    varComps <- c(varCompTable$varGaugeTotal, varCompTable$varRepeat, varCompTable$varReprod, varCompTable$varPart, varCompTable$varTotal)
    percentVarComps <- (varComps / varCompTable$varTotal) * 100
    gaugeRRNonRepTable2$setData(list("sources" = varSources,
                                     "Var" = varComps,
                                     "percentVar" = percentVarComps))
    if (options[["processVariationReference"]] == "historicalSd" && histSD >= sqrt(varCompTable$varGaugeTotal)) {
      stdDevs <- c(sqrt(c(varCompTable$varGaugeTotal, varCompTable$varRepeat, varCompTable$varReprod)),
                   sqrt(histSD^2 - varCompTable$varGaugeTotal), histSD)
    } else {
      stdDevs <- sqrt(varComps)
    }
    if (options[["studyVarianceMultiplierType"]] == "sd") {
      studyVarMultiplier <- options[["studyVarianceMultiplierValue"]]
    }else{
      percent <- options[["studyVarianceMultiplierValue"]] / 100
      q <- (1 - percent)/2
      studyVarMultiplier <- abs(2*qnorm(q))
    }
    studyVar <- stdDevs * studyVarMultiplier
    percentStudyVar <- studyVar/max(studyVar) * 100
    gaugeRRNonRepTable3DataList <- list("source" = varSources,
                                        "SD" = stdDevs,
                                        "studyVar" = studyVar,
                                        "percentStudyVar" = percentStudyVar)
    if(options[["tolerance"]]){
      percentTolerance <- studyVar / options[["toleranceValue"]] * 100
      gaugeRRNonRepTable3DataList <- append(gaugeRRNonRepTable3DataList, list("percentTolerance" = percentTolerance))
    }
    gaugeRRNonRepTable3$setData(gaugeRRNonRepTable3DataList)
    nCategories <- .gaugeNumberDistinctCategories(sqrt(varCompTable$varPart), sqrt(varCompTable$varGaugeTotal))

    if (gaugeEvaluationDfOnly) {
      gaugeEvalDf <- as.data.frame(gaugeRRNonRepTable3DataList)
      gaugeEvalDf[,-1] <- round(gaugeEvalDf[,-1], .numDecimals) # Round everything while including the source column
      names(gaugeEvalDf) <- if (ncol(gaugeEvalDf) == 5) c("Source", "Std. dev.", "Study variation", "%Study variation", "%Tolerance") else c("Source", "Std. dev.", "Study variation", "%Study variation")
      return(list("gaugeEvalDf" = gaugeEvalDf, "nCategories" = nCategories))
    }

    if (as.integer(studyVarMultiplier) == round(studyVarMultiplier, 2)){
      gaugeRRNonRepTable3$addFootnote(gettextf("Study variation is calculated as std. dev. * %i", as.integer(studyVarMultiplier)))
    } else {
      gaugeRRNonRepTable3$addFootnote(gettextf("Study variation is calculated as std. dev. * %.2f", studyVarMultiplier))
    }
    gaugeRRNonRepTable3$addFootnote(gettextf("Number of distinct categories = %i", nCategories))
  }
  gaugeRRNonRepTables[["Table1"]] <- gaugeRRNonRepTable1
  gaugeRRNonRepTables[["Table2"]] <- gaugeRRNonRepTable2
  gaugeRRNonRepTables[["Table3"]] <- gaugeRRNonRepTable3

  if (options[["varianceComponentsGraph"]]) {
    plot <- createJaspPlot(title = gettext("Components of variation"), width = 850, height = 500)
    plot$dependOn(c("varianceComponentsGraph", "report"))
    if (ready){
      if(options[["tolerance"]]){
        percentToleranceForGraph <- percentTolerance[1:4]
      } else {
        percentToleranceForGraph <- NA
      }
      p <- .gaugeVarCompGraph(percentVarComps[1:4], percentStudyVar[1:4], percentToleranceForGraph)
      plot$plotObject <- p
    }
    if (plotOnly)
      return(p)
    gaugeRRNonRepTables[["Plot"]] <- plot
  }

  # Traffic light chart values
  if (trafficPlotValuesOnly) {
    percentMSVar <- round((studyVar[1]/max(studyVar))*100, .numDecimals)
    percentMSTolerance <- round((studyVar[1]/options[["toleranceValue"]])*100, .numDecimals)
    return(c(percentMSVar, percentMSTolerance))
  }

  return(gaugeRRNonRepTables)
}

.ssOperator <- function(dataset, operators, parts, measurements) {
  grandmean    <- mean(dataset[[measurements]])
  operatormean <- tapply(dataset[[measurements]], dataset[operators], mean)
  nreplicates   <- table(dataset[parts])[1]
  nparts  <- table(dataset[operators])[1] / nreplicates
  ssOperator <- as.vector(nreplicates) * as.vector(nparts) * sum((grandmean - operatormean)^2)
  return(ssOperator)
}

.ssPartOperator <- function(dataset, operators, parts, measurements) {
  dataset[operators] <- as.character(dataset[[operators]])
  dataset[parts] <- as.character(dataset[[parts]])
  operatorVector <- unique(dataset[[operators]])
  nreplicates       <- table(dataset[parts])[1]
  nparts  <- table(dataset[operators])[1] / nreplicates
  ssPartOperator <- 0
  for (op in operatorVector) {
    dataPerOP <- subset(dataset, dataset[[operators]] == op)
    opMean <- mean(dataPerOP[[measurements]])
    partMean <- tapply(dataPerOP[[measurements]], dataPerOP[parts], mean)
    ssPartOperator <- ssPartOperator + sum((partMean - opMean)^2)
  }
  ssPartOperator <- as.vector(nreplicates) * ssPartOperator
  return(ssPartOperator)
}

.ssRepeat <- function(dataset, operators, parts, measurements) {
  dataset[operators] <- as.character(dataset[operators])
  partVector <- unique(dataset[[parts]])
  ssRepeat <- 0
  for (p in partVector) {
    dataPerP <- subset(dataset, dataset[parts] == p)
    partMean <- as.vector(tapply(dataPerP[[measurements]], dataPerP[operators], mean))
    measurementsPerP <- dataPerP[[measurements]]
    ssRepeat <- ssRepeat + sum((measurementsPerP - partMean)^2)
  }
  return(ssRepeat)
}

.ssGaugeNested <- function(dataset, operators, parts, measurements) {
  ssOperator <- .ssOperator(dataset, operators, parts, measurements)
  ssPartOperator <- .ssPartOperator(dataset, operators, parts, measurements)
  ssRepeat <- .ssRepeat(dataset, operators, parts, measurements)
  ssTotal <- sum(ssOperator, ssPartOperator, ssRepeat)

  return(c(ssOperator, ssPartOperator, ssRepeat, ssTotal))
}

.dfGaugeNested <- function(dataset, operators, parts, measurements) {
  noperators <- length(unique(dataset[[operators]]))
  nreplicates <- as.vector(table(dataset[parts])[1])
  nparts  <- as.vector(table(dataset[operators])[1] / nreplicates)

  dfOperator <- noperators - 1
  dfPartOperator <- noperators * (nparts - 1)
  dfRepeat <- nparts * noperators * (nreplicates - 1)
  dfTotal <- noperators * nreplicates * nparts - 1

  return(c(dfOperator, dfPartOperator, dfRepeat, dfTotal))
}

.msGaugeNested <- function(ss, df) {
  msOperator <- ss[1] / df[1]
  msPartOperator <- ss[2] / df[2]
  msRepeat <- ss[3] / df[3]
  return(c(msOperator, msPartOperator, msRepeat))
}

.fGaugeNested <- function(ms) {
  fOperator <- ms[1] /  ms[2]
  fPartOperator <- ms[2] / ms[3]
  return(c(fOperator, fPartOperator))
}

.pGaugeNested <- function(f, df) {
  p1 <- pf(f[1], df[1], df[2], lower.tail = F)
  p2 <- pf(f[2], df[2], df[3], lower.tail = F)
  return(c(p1, p2))
}

.gaugeNestedANOVA <- function(dataset, operators, parts, measurements) {
  anovaFormula <- as.formula(paste0(measurements, " ~ ", operators, "/", parts))
  anovaOutput <- summary(aov(anovaFormula, data = dataset))
  anovaOutputDf <- as.data.frame(anovaOutput[[1]])

  ss <- c(anovaOutputDf$`Sum Sq`, sum(anovaOutputDf$`Sum Sq`))
  df <- c(anovaOutputDf$Df, sum(anovaOutputDf$Df))
  ms <- c(anovaOutputDf$`Mean Sq`)
  f <- c(ms[1]/ms[2], anovaOutputDf$`F value`[2])
  p <- .pGaugeNested(f, df)
  sources <- c(operators, paste(parts, "(", operators, ")", sep = ""), "Repeatability", "Total")

  anovaTable <- data.frame(Source = sources,
                           DF = df,
                           SS = ss,
                           MS = c(ms, NA),
                           "F-value" = c(f, rep(NA, 2)),
                           "p-value" = c(p, rep(NA, 2)))
  return(anovaTable)
}

.gaugeNestedVarComponents <- function(dataset, operators, parts, measurements, ms) {
  nOperators <- length(unique(dataset[[operators]]))
  replicatesTable <- table(dataset[[parts]])
  nReplicates <- unname(replicatesTable)[1] # assuming constant repetitions across parts
  nParts  <- length(unique(dataset[[parts]]))
  msOperator <- ms[1]
  msOperatorPart <- ms[2]
  msRepeat <- ms[3]
  varRepeat <- msRepeat
  varReprod <- (msOperator - msOperatorPart) / (nParts * nReplicates)
  if (varReprod < 0)
    varReprod <- 0
  varPart <- (msOperatorPart - msRepeat) / nReplicates
  if (varPart < 0)
    varPart <- 0
  varGaugeTotal <- varRepeat + varReprod
  varTotal <- varGaugeTotal + varPart
  dframe <- data.frame("varGaugeTotal" = varGaugeTotal,
                       "varRepeat" = varRepeat,
                       "varReprod" = varReprod,
                       "varPart" = varPart,
                       "varTotal" = varTotal)
  return(dframe)
}

.reshapeToWide <- function(dataset, measurements, parts, operators) {
  dataset <- dataset[order(dataset[[parts]]),]
  index <- sequence(dplyr::count(dataset, dplyr::across(dplyr::all_of(c(parts, operators))))$n)
  dataset$index <- index
  dataset <- tidyr::spread(dataset, index, measurements)
  measurementColNames <- paste("M", 1:max(index), sep = "")
  colnames(dataset) <- c(parts, operators, measurementColNames)
  return(dataset)
}

.gaugeMeasurmentsByPartXOperator <- function(dataset, measurements, parts, operators, options) {
  plotContainer <- createJaspContainer(gettext("Measurements by part x operator"))
  operatorVector <- unique(dataset[[operators]])
  for (op in operatorVector) {
    dataPerOP <- subset(dataset, dataset[operators] == op)
    plot <- createJaspPlot(title = gettextf("Operator %s", op), width = 600, height = 300)
    plot$plotObject <- .gaugeByPartGraphPlotObject(dataset = dataPerOP, measurements = measurements, parts = parts, operators = operators, displayAll = options[["partMeasurementPlotAllValues"]])
    plotContainer[[op]] <- plot
  }
  return(plotContainer)
}

.reshapeToLong <- function(dataset, measurements, parts, operators){
  longData <- tidyr::gather(dataset, repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)
  longData <- longData[c(parts, operators, "Measurement" )]
  return(longData)
}
