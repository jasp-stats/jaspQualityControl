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
msaGaugeRR <- function(jaspResults, dataset, options, ...) {
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

  #ready statement
  if (wideFormat && !options[["type3"]]) {
    ready <- (length(measurements) > 1 && !identical(operators, "") && !identical(parts, ""))
  } else if (wideFormat && options[["type3"]]) {
    ready <- (length(measurements) > 1 && !identical(parts, ""))
  } else if (!wideFormat && !options[["type3"]]) {
    ready <- (measurements != "" && !identical(operators, "") && !identical(parts, ""))
  }  else if (!wideFormat && options[["type3"]]) {
    ready <- (!identical(measurements, "") && !identical(parts, ""))
  }


  numeric.vars <- measurements
  numeric.vars <- numeric.vars[numeric.vars != ""]
  factor.vars <- c(parts, operators)
  factor.vars <- factor.vars[factor.vars != ""]

  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = numeric.vars, columns.as.factor = factor.vars)
    if (options$type3){
      dataset$operators <- rep(1, nrow(dataset))
      operators <- "operators"
    }
  }

  # Checking for infinity and missingValues
  .hasErrors(dataset, type = c('infinity', 'missingValues'),
             infinity.target = measurements,
             missingValues.target = c(measurements, parts, operators),
             exitAnalysisIfErrors = TRUE)

  # Checking for crossed design
  if (ready && !options[["type3"]]) {
    crossed <- .checkIfCrossed(dataset, operators, parts, measurements)
    if(!crossed){
      plot <- createJaspPlot(title = gettext("Gauge r&R"), width = 700, height = 400)
      jaspResults[["gaugeANOVA"]] <- plot
      plot$setError(gettext("Design is not balanced: not every operator measured every part the same number of times."))
      return()
    }
  }

  # Checking for balanced design
  if (ready && options[["type3"]]) {
    checkTab <- table(dataset[[parts]])
    if (!all(checkTab == checkTab[1])) {
      plot <- createJaspPlot(title = gettext("Gauge r&R"), width = 700, height = 400)
      jaspResults[["gaugeANOVA"]] <- plot
      plot$setError(gettext("Design is not balanced: not every part was measured the same number of times."))
      return()
    }
  }


  # Converting long to wide data
  if (!wideFormat && ready) {
    dataset <- dataset[order(dataset[[operators]]),]
    dataset <- dataset[order(dataset[[parts]]),]
    nrep <- table(dataset[operators])[[1]]/length(unique(dataset[[parts]]))
    index <- rep(paste("V", 1:nrep, sep = ""), nrow(dataset)/nrep)
    dataset <- cbind(dataset, data.frame(index = index))
    dataset <- tidyr::spread(dataset, index, measurements)
    measurements <- unique(index)
    dataset <- dataset[,c(operators, parts, measurements)]
  } else if (ready) {
    dataset <- dataset[order(dataset[[parts]]),]
  }


  # Checking type 3
  Type3 <- c(length(unique(dataset[[operators]])) == 1 || options$type3)

  # Get Rule List
  if (ready) {
    ruleList1 <- .getRuleListSubgroupCharts(options, type = "xBar")
    ruleList2 <- .getRuleListSubgroupCharts(options, type = "R")
  }

  # Errors #
  # Checking whether type3 is used correctly
  .hasErrors(dataset,
             target = measurements,
             custom = function() {
               if (Type3 && !options$type3)
                 return("This dataset seems to have only a single unique operator. Please use the Type 3 study by checking the box below.")},
             exitAnalysisIfErrors = TRUE)
  # Checking whether the format wide is used correctly
  if (ready)
    .hasErrors(dataset,
             target = measurements,
             custom = function() {
               dataToBeChecked <- dataset[dataset[[operators]] == dataset[[operators]][1],]
               partsLevels <- length(levels(dataToBeChecked[[parts]]))
               partsLength <- length(dataToBeChecked[[parts]])
               if (wideFormat &&  partsLevels != partsLength && !Type3)
                 return(gettextf("The measurements selected seem to be in a 'Single Column' format as every operator's part is measured %d times.", partsLength/partsLevels))},
             exitAnalysisIfErrors = FALSE)

  # Report
  if (options[["report"]] && ready) {
    nElements <- sum(options[["reportVariationComponents"]], options[["reportMeasurementsByPartPlot"]], options[["reportRChartByOperator"]],
                     options[["reportMeasurementsByOperatorPlot"]], options[["reportAverageChartByOperator"]],
                     options[["reportPartByOperatorPlot"]], options[["reportTrafficLightChart"]], options[["reportMetaData"]])
    plotHeight <- ceiling(nElements/2) * 500
    reportPlot <- createJaspPlot(title = gettext("Gauge r&R report"), width = 1250, height = plotHeight)
    jaspResults[["report"]] <- reportPlot
    jaspResults[["report"]]$dependOn(c("measurementLongFormat", "operatorLongFormat", "partLongFormat", "measurementsWideFormat",
                                       "operatorWideFormat", "partWideFormat", "type3", "processVariationReference", "historicalSdValue",
                                       "tolerance", "toleranceValue", "anovaModelType", "studyVarianceMultiplierType",
                                       "studyVarianceMultiplierValue", "scatterPlotFitLine", "scatterPlotOriginLine",
                                       "partMeasurementPlotAllValues", "report", "reportMetaData", "reportTitle",
                                       "reportTitleText", "reportPartName", "reportPartNameText", "reportGaugeName",
                                       "reportGaugeNameText", "reportCharacteristic", "reportCharacteristicText",
                                       "reportGaugeNumber", "reportGaugeNumberText", "reportTolerance", "reportToleranceText",
                                       "reportLocation", "reportLocationText", "reportPerformedBy", "reportPerformedByText",
                                       "reportDate", "reportDateText", "reportVariationComponents", "reportMeasurementsByPartPlot",
                                       "reportRChartByOperator", "reportMeasurementsByOperatorPlot", "reportAverageChartByOperator",
                                       "reportPartByOperatorPlot", "reportTrafficLightChart", "reportMetaData", .getDependenciesControlChartRules()))

    if (nElements == 0) {
      reportPlot$setError(gettext("No report components selected."))
      return()
    }

    if(!ready)
      return()

    # Plot meta data
    if (options[["reportTitle"]] ) {
      title <- if (options[["reportTitleText"]] == "") gettext("Gauge r&R report") else options[["reportTitleText"]]
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
      plots[[plotIndexCounter]] <- .gaugeANOVA(dataset, measurements, parts, operators, options, ready = TRUE,
                                              returnPlotOnly = TRUE, Type3 = Type3)   #var. comp. plot
      plotIndexCounter <- plotIndexCounter + 1
    }
    if (options[["reportMeasurementsByPartPlot"]]) {
      plots[[plotIndexCounter]] <- .gaugeByPartGraphPlotObject(dataset, measurements, parts, operators, displayAll = FALSE) #measurement by part plot
      plotIndexCounter <- plotIndexCounter + 1
    }
    if (options[["reportRChartByOperator"]]) {
      plots[[plotIndexCounter]] <- .controlChart(dataset = dataset[c(measurements, operators)], ruleList = ruleList2,
                                                plotType = "R", stages = operators,
                                                xAxisLabels = dataset[[parts]][order(dataset[[operators]])],
                                                stagesSeparateCalculation = FALSE)$plotObject
      plotIndexCounter <- plotIndexCounter + 1
    }
    if (options[["reportMeasurementsByOperatorPlot"]]) {
      plots[[plotIndexCounter]] <- .gaugeByOperatorGraphPlotObject(dataset, measurements, parts, operators, options, Type3 = Type3)   #Measurements by operator plot
      plotIndexCounter <- plotIndexCounter + 1
      }
    if (options[["reportAverageChartByOperator"]]) {
      plots[[plotIndexCounter]] <- .controlChart(dataset = dataset[c(measurements, operators)], ruleList = ruleList1,
                                                plotType = "xBar", xBarSdType = "r", stages = operators,
                                                xAxisLabels = dataset[[parts]][order(dataset[[operators]])],
                                                stagesSeparateCalculation = FALSE)$plotObject
      plotIndexCounter <- plotIndexCounter + 1
    }
    if (options[["reportPartByOperatorPlot"]]) {
      plots[[plotIndexCounter]] <- .gaugeByInteractionGraphPlotFunction(dataset, measurements, parts, operators, options,
                                                                       Type3 = Type3, ggPlot = TRUE) # Part x Operator interaction plot
      plotIndexCounter <- plotIndexCounter + 1
    }

    if (options[["reportTrafficLightChart"]]) {
      valuesVec <- .gaugeANOVA(dataset = dataset, measurements = measurements, parts = parts, operators = operators,
                               options =  options, ready = TRUE, returnTrafficValues = TRUE, Type3 = Type3)
      trafficPlots <- .trafficplot(StudyVar = valuesVec$study, ToleranceUsed = options$tolerance,
                            ToleranceVar = valuesVec$tol, options = options, ready = TRUE, ggPlot = TRUE)
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
      gaugeEvalOutput <- .gaugeANOVA(dataset = dataset, measurements = measurements, parts = parts, operators = operators,
                                     options =  options, ready = TRUE, returnTrafficValues = FALSE, Type3 = Type3,
                                     gaugeEvaluationDfOnly = TRUE)
      if (!all(is.na(gaugeEvalOutput))) {
        gaugeEvalDf <- gaugeEvalOutput[["gaugeEvalDf"]]
        nCategories <- gaugeEvalOutput[["nCategories"]]
        nCategoriesDf <- data.frame("x1" = gettext("Number of distinct categories"), "x2" = nCategories)
        names(nCategoriesDf) <- NULL
        tables[[1]] <- list(gaugeEvalDf, nCategoriesDf)
        tableTitles[[1]] <- list("Gauge evaluation", "")
      }
    }

    reportPlotObject <- .qcReport(text = text, plots = plots, tables = tables, textMaxRows = 8,
                                  tableTitles = tableTitles, reportTitle = title, tableSize = 6)
    reportPlot$plotObject <- reportPlotObject
  } else {
    # Gauge r&R ANOVA Table
    if (options[["anova"]]) {
      if (is.null(jaspResults[["gaugeANOVA"]])) {
        jaspResults[["gaugeANOVA"]] <- createJaspContainer(gettext("Gauge r&R ANOVA table"))
        jaspResults[["gaugeANOVA"]]$dependOn(c("processVariationReference", "historicalSdValue", "report"))
        jaspResults[["gaugeANOVA"]]$position <- 1
      }
      jaspResults[["gaugeANOVA"]] <- .gaugeANOVA(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready, Type3 = Type3)
    }

    # R chart by operator
    if (options[["rChart"]] && is.null(jaspResults[["rChart"]])) {
      jaspResults[["rChart"]] <- createJaspContainer(gettext("Range chart by operator"))
      jaspResults[["rChart"]]$position <- 3
      jaspResults[["rChart"]]$dependOn(c("rChart", "gaugeRRmethod", "anovaGaugeReport", "measurementLongFormat",
                                         "measurementsWideFormat", "report", .getDependenciesControlChartRules()))
      jaspResults[["rChart"]][["plot"]] <- createJaspPlot(title = gettext("Range chart by operator"), width = 1200, height = 500)
      if (ready) {
        rChart <- .controlChart(dataset = dataset[c(measurements, operators)], plotType = "R", ruleList = ruleList2,
                                            stages = operators, xAxisLabels = dataset[[parts]][order(dataset[[operators]])],
                                            stagesSeparateCalculation = FALSE)

        jaspResults[["rChart"]][["plot"]]$plotObject <- rChart$plotObject
        jaspResults[["rChart"]][["table"]] <- rChart$table
      }
    }

    # Xbar chart by operator
    if (options[["xBarChart"]] && is.null(jaspResults[["xBarChart"]])) {
      jaspResults[["xBarChart"]] <- createJaspContainer(gettext("Xbar chart by operator"))
      jaspResults[["xBarChart"]]$position <- 4
      jaspResults[["xBarChart"]]$dependOn(c("xBarChart", "gaugeRRmethod", "anovaGaugeReport", "measurementLongFormat",
                                            "measurementsWideFormat", "report", .getDependenciesControlChartRules()))
      jaspResults[["xBarChart"]][["plot"]] <- createJaspPlot(title = gettext("Average chart by operator"), width = 1200, height = 500)
      if (ready) {
        xBarChart <- .controlChart(dataset = dataset[c(measurements, operators)], ruleList = ruleList1,
                                               plotType = "xBar", xBarSdType = "r", stages = operators,
                                               xAxisLabels = dataset[[parts]][order(dataset[[operators]])],
                                               stagesSeparateCalculation = FALSE)
        jaspResults[["xBarChart"]][["plot"]]$plotObject <- xBarChart$plotObject
        jaspResults[["xBarChart"]][["table"]] <- xBarChart$table
      }
    }

    # gauge Scatter Plot Operators
    if (options[["scatterPlot"]]) {
      if (is.null(jaspResults[["gaugeScatterOperators"]])) {
        jaspResults[["gaugeScatterOperators"]] <- createJaspContainer(gettext("Scatterplot operators"))
        jaspResults[["gaugeScatterOperators"]]$position <- 5
      }
      jaspResults[["gaugeScatterOperators"]] <- .gaugeScatterPlotOperators(jaspResults = jaspResults, dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready)
      jaspResults[["gaugeScatterOperators"]]$dependOn(c("gaugeRRmethod", "report"))
    }

    # Measurement by Part Graph
    if (options[["partMeasurementPlot"]] & ready) {
      if (is.null(jaspResults[["gaugeByPart"]])) {
        jaspResults[["gaugeByPart"]] <- createJaspContainer(gettext("Measurement by part graph"))
        jaspResults[["gaugeByPart"]]$dependOn("report")
        jaspResults[["gaugeByPart"]]$position <- 6
      }
      jaspResults[["gaugeByPart"]] <- .gaugeByPartGraph(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options = options)
    }

    # Measurement by Operator Box Plot
    if (options[["operatorMeasurementPlot"]]) {
      if (is.null(jaspResults[["gaugeByOperator"]])) {
        jaspResults[["gaugeByOperator"]] <- createJaspContainer(gettext("Measurements by operator graph"))
        jaspResults[["gaugeByOperator"]]$dependOn("report")
        jaspResults[["gaugeByOperator"]]$position <- 7
      }
      jaspResults[["gaugeByOperator"]] <- .gaugeByOperatorGraph(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready, Type3 = Type3)
    }

    # Parts by Operator Interaction Plot
    if (options[["partByOperatorMeasurementPlot"]]) {
      if (is.null(jaspResults[["gaugeByInteraction"]])) {
        jaspResults[["gaugeByInteraction"]] <- createJaspContainer(gettext("Part by operator interaction graph"))
        jaspResults[["gaugeByInteraction"]]$dependOn("report")
        jaspResults[["gaugeByInteraction"]]$position <- 8
      }
      jaspResults[["gaugeByInteraction"]] <- .gaugeByInteractionGraph(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready, Type3 = Type3)
    }
    # Traffic light plot
    if(options[["trafficLightChart"]] & is.null(jaspResults[["trafficPlot"]] )) {
      jaspResults[["trafficPlot"]] <- createJaspContainer(gettext("Traffic light chart"))
      jaspResults[["trafficPlot"]]$position <- 9
      jaspResults[["trafficPlot"]]$dependOn(c("trafficLightChart", "toleranceValue", "tolerance", "gaugeRRmethod", "processVariationReference", "historicalSdValue", "report"))
      trafficContainer <- jaspResults[["trafficPlot"]]

      valuesVec <- .gaugeANOVA(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready, returnTrafficValues = TRUE, Type3 = Type3)
      trafficContainer[["plot"]] <- .trafficplot(StudyVar = valuesVec$study, ToleranceUsed = options[["tolerance"]],ToleranceVar = valuesVec$tol, options = options, ready = ready)

    }
  }

  return()
}

.gaugeANOVA <- function(dataset, measurements, parts, operators, options, ready, returnPlotOnly = FALSE, returnTrafficValues = FALSE,
                        gaugeEvaluationDfOnly = FALSE, Type3 = FALSE) {
  anovaTables <- createJaspContainer(gettext("Gauge r&R study - crossed ANOVA"))
  anovaTables$dependOn(c("anova", "gaugeRRmethod", "report"))
  anovaTables$position <- 1

  anovaTable1 <- createJaspTable(title = ifelse(Type3, gettext("One-way ANOVA table"), gettext("Two-way ANOVA table with interaction")))
  anovaTable1$addColumnInfo(title = gettext("Source"),       name = "source",   type = "string" )
  anovaTable1$addColumnInfo(title = gettext("df"),             name = "Df",      type = "integer")
  anovaTable1$addColumnInfo(title = gettext("Sum of squares"), name = "Sum Sq",  type = "number")
  anovaTable1$addColumnInfo(title = gettext("Mean squares"),    name = "Mean Sq", type = "number")
  anovaTable1$addColumnInfo(title = gettext("F"),              name = "F value", type = "number")
  anovaTable1$addColumnInfo(title = gettext("<i>p</i>-value"),              name = "Pr(>F)",  type = "pvalue")

  RRtable1 <- createJaspTable(title = gettext("Variance components"))
  RRtable1$dependOn(c("anova", "operatorWideFormat", "operatorLongFormat", "partWideFormat", "partLongFormat", "measurementsWideFormat",
                      "measurementLongFormat"))
  RRtable1$addColumnInfo(name = "Source", title = gettext("Source"), type = "string")
  RRtable1$addColumnInfo(name = "Variation", title = gettext("Variance"), type = "number")
  RRtable1$addColumnInfo(name = "Percent", title = gettextf("%% Contribution"), type = "integer")

  RRtable2 <- createJaspTable(title = gettext("Gauge evaluation"))
  RRtable2$dependOn(c("anova", "operatorWideFormat", "operatorLongFormat", "partWideFormat", "partLongFormat",
                      "measurementsWideFormat", "measurementLongFormat"))
  RRtable2$addColumnInfo(name = "source", title = gettext("Source"), type = "string")
  RRtable2$addColumnInfo(name = "SD", title = gettext("Std. dev."), type = "number")
  RRtable2$addColumnInfo(name = "studyVar", title = gettextf("Study variation"), type = "number")
  RRtable2$addColumnInfo(name = "percentStudyVar", title = gettextf("%% Study variation"), type = "integer")
  if(options[["tolerance"]])
    RRtable2$addColumnInfo(name = "percentTolerance", title = gettextf("%% Tolerance"), type = "integer")

  for (measurement in measurements) {
    if (length(dataset[[measurement]]) <= 1)
      ready <- FALSE
  }

  if (ready && length(measurements) >= 2) {
    singleOperator <- Type3

    data <- dataset

    data <- tidyr::gather(data, repetition, measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

    if (options[["studyVarianceMultiplierType"]] == "sd") {
      studyVarMultiplier <- options[["studyVarianceMultiplierValue"]]
    }else{
      percent <- options[["studyVarianceMultiplierValue"]] / 100
      q <- (1 - percent)/2
      studyVarMultiplier <- abs(2*qnorm(q))
    }

    measurements <- "measurement"
    ssPart <- .ssPart(data, operators, parts, measurements)
    ssRepWithInteraction <- .ssRepWithInteraction(data, operators, parts, measurements)
    ssTotal <- .ssTotal(data, operators, parts, measurements)
    if(!singleOperator){
      ssOperator <- .ssOperator(data, operators, parts, measurements)
      ssInteraction <- ssTotal - sum(ssPart, ssOperator, ssRepWithInteraction)
    }

    DFlist <- .dfGaugeCrossed(data, operators, parts, measurements)
    dfPart <- DFlist$dfPart
    dfRepWithInteraction <- DFlist$dfRepeat
    dfTotal <- DFlist$dfTotal
    if(!singleOperator){
      dfOperators <- DFlist$dfOperator
      dfInteraction <- DFlist$dfPartXOperator
    }

    msPart <- ssPart / dfPart
    msRepWithInteraction <- ssRepWithInteraction / dfRepWithInteraction
    if(!singleOperator){
      msOperator <- ssOperator / dfOperators
      msInteraction <- ssInteraction / dfInteraction
    }

    if(singleOperator){
      fPart <- msPart / msRepWithInteraction
    }else{
      fPart <- msPart / msInteraction
      fOperator <- msOperator / msInteraction
      fInteraction <- msInteraction / msRepWithInteraction
    }

    if (options[["anovaModelType"]] == "fixedEffect"){
      fPart <- msPart / msRepWithInteraction
      fOperator <- msOperator / msRepWithInteraction
    }

    if(singleOperator){
      pPart <- pf(fPart, dfPart, dfRepWithInteraction, lower.tail = F)
    }else{
      pPart <- pf(fPart, dfPart, dfInteraction, lower.tail = F)
      pOperator <- pf(fOperator, dfOperators, dfInteraction, lower.tail = F)
      pInteraction <- pf(fInteraction, dfInteraction, dfRepWithInteraction, lower.tail = F)
    }

    if(singleOperator){
      sources <- c(parts, 'Repeatability', 'Total')
      dfVector <- c(dfPart, dfRepWithInteraction, dfTotal)
      ssVector <- c(ssPart, ssRepWithInteraction, ssTotal)
      msVector <- c(msPart, msRepWithInteraction)
      fVector <- fPart
      pVector <- pPart
    }else{
      sources <- c(parts, operators, paste(parts," * ", operators), "Repeatability", "Total")
      dfVector <- c(dfPart, dfOperators, dfInteraction, dfRepWithInteraction, dfTotal)
      ssVector <- c(ssPart, ssOperator, ssInteraction, ssRepWithInteraction, ssTotal)
      msVector <- c(msPart, msOperator, msInteraction, msRepWithInteraction)
      fVector <- c(fPart, fOperator, fInteraction)
      pVector <- c(pPart, pOperator, pInteraction)
    }

    anovaTable1$setData(list( "source"              = sources,
                              "Df"                 = dfVector,
                              "Sum Sq"             = ssVector,
                              "Mean Sq"            = msVector,
                              "F value"            = fVector,
                              "Pr(>F)"             = pVector))

    anovaTables[['anovaTable1']] <- anovaTable1

    if(!singleOperator)
      interactionSignificant <- pInteraction < options[["anovaAlphaForInteractionRemoval"]]
    else
      interactionSignificant <- FALSE

    if (singleOperator || interactionSignificant){

      #r & R varcomps
      if (singleOperator) {
        varCompList <- .gaugeCrossedVarCompsSingleOP(data, operators, parts, measurements, msPart, msRepWithInteraction)
        varCompRepeat <- varCompList$repeatability
        varCompPart <- varCompList$part
        varCompTotalGauge <- varCompList$totalGauge
        varCompTotalVar <- varCompList$totalVar
        varCompVector <- c("varCompTotalGauge" = varCompTotalGauge, "varCompRepeat" = varCompRepeat,
                           "varCompPart" = varCompPart, "varCompTotalVar" = varCompTotalVar)
        sources <- gettext(c("Total gauge r&R", "Repeatability", "Part-to-part", "Total variation"))
      } else {
        varCompList <- .gaugeCrossedVarComps(data, operators, parts, measurements, msPart, msOperator, msRepWithInteraction, msInteraction)
        varCompRepeat <- varCompList$repeatability
        varCompOperator <- varCompList$operator
        varCompPart <- varCompList$part
        varCompReprod <- varCompList$reprod
        varCompTotalGauge <- varCompList$totalGauge
        varCompTotalVar <- varCompList$totalVar
        varCompInteraction <- varCompList$interaction
        varCompVector <- c("varCompTotalGauge" = varCompTotalGauge, "varCompRepeat" = varCompRepeat, "varCompReprod" = varCompReprod,
                           "varCompOperator" = varCompOperator, "varCompInteraction" = varCompInteraction, "varCompPart" = varCompPart,
                           "varCompTotalVar" = varCompTotalVar)
        sources <- gettext(c("Total gauge r&R", "Repeatability", "Reproducibility", operators, paste(parts," * ", operators), "Part-to-part",  "Total variation"))
      }

      if (options[["processVariationReference"]] == "historicalSd") {
        if (Type3)
          varCompVector <- c("varCompTotalGauge" = varCompTotalGauge, "varCompRepeat" = varCompRepeat, "varCompPart" = varCompPart, "varCompTotalVar" = varCompTotalVar)
        histSD <- options[["historicalSdValue"]]
        varCompVector[["varCompTotalVar"]] <- varCompTotalVar <- histSD^2
        varCompVector[["varCompPart"]] <- varCompVector$varCompTotalVar - varCompTotalGauge
      }


      varCompPercent <- unlist(varCompVector) / varCompTotalVar * 100

      RRtable1$setData(list(      "Source"       = sources,
                                  "Variation"    = varCompVector,
                                  "Percent"      = round(varCompPercent,2)))

      anovaTables[['RRtable1']] <- RRtable1

      #Gauge evaluation

      histSD <- options[["historicalSdValue"]]

      if (!singleOperator) {
        if (options[["processVariationReference"]] == "historicalSd" && histSD >= sqrt(varCompTotalGauge)) {
          SD <- c(sqrt(c(varCompTotalGauge, varCompRepeat, varCompReprod, varCompOperator)),
                  sqrt(varCompInteraction), sqrt(histSD^2 - varCompTotalGauge), histSD)

        } else {
          SD <- sqrt(varCompVector)
        }

      } else {
        if (options[["processVariationReference"]] == "historicalSd" && histSD >= sqrt(varCompTotalGauge)) {
          SD <- c(sqrt(c(varCompTotalGauge, varCompRepeat)), sqrt(histSD^2 - varCompTotalGauge), histSD)
        } else {
          SD <- sqrt(varCompVector)
        }
      }
      sdParts <- sqrt(varCompVector[["varCompPart"]])
      sdGauge <- sqrt(varCompVector[["varCompTotalGauge"]])
      SD <- ifelse(SD == "NaN", 0, SD)
      studyVar <- SD * studyVarMultiplier
      RRtable2DataList <- list("source"       = sources,
                               "SD"           = SD,
                               "studyVar"    = studyVar,
                               "percentStudyVar"    = c(round(studyVar/max(studyVar) * 100,2)))
      if(options[["tolerance"]])
        RRtable2DataList <- append(RRtable2DataList, list("percentTolerance" = c(round(studyVar / options[["toleranceValue"]] * 100,2))))
      RRtable2$setData(RRtable2DataList)
      if (!is.na(sdParts)) {
        nCategories <- .gaugeNumberDistinctCategories(sdParts, sdGauge)
        RRtable2$addFootnote(gettextf("Number of distinct categories = %i", nCategories))
      }
      if (as.integer(studyVarMultiplier) == round(studyVarMultiplier, 2)){
        RRtable2$addFootnote(gettextf("Study Variation is calculated as std. dev. <spam>&#215;</spam> %i", as.integer(studyVarMultiplier)))
      }else{
        RRtable2$addFootnote(gettextf("Study Variation is calculated as std. dev. <spam>&#215;</spam> %.2f", studyVarMultiplier))
      }

      if(options[["processVariationReference"]] == "historicalSd"){
        RRtable2$addFootnote(gettextf("Historical standard deviation is used to calculate some values for std. dev., study variation, and %%study variation."))
        RRtable2$addFootnote(gettextf("Values for %%process variation are not displayed because they are identical to values for %%study variation."))
      }


      anovaTables[['RRtable2']] <- RRtable2

    } else {

      anovaTable2 <- createJaspTable(title = gettext("Two-way ANOVA table without interaction"))
      anovaTable2$addColumnInfo(title = gettext("Source"),        name = "source",   type = "string" )
      anovaTable2$addColumnInfo(title = gettext("df"),             name = "Df",      type = "integer")
      anovaTable2$addColumnInfo(title = gettext("Sum of squares"), name = "Sum Sq",  type = "number")
      anovaTable2$addColumnInfo(title = gettext("Mean squares"),    name = "Mean Sq", type = "number")
      anovaTable2$addColumnInfo(title = gettext("F"),              name = "F value", type = "number")
      anovaTable2$addColumnInfo(title = gettext("<i>p</i>-value"),              name = "Pr(>F)",  type = "pvalue")

      ssRepWithoutInteraction <- ssTotal - ssPart - ssOperator
      dfRepWithoutInteraction <- dfTotal - dfPart - dfOperators
      msRepWithoutInteraction <- ssRepWithoutInteraction / dfRepWithoutInteraction

      fPartWithoutInteraction <- msPart / msRepWithoutInteraction
      fOperatorWithoutInteraction <- msOperator / msRepWithoutInteraction

      pPartWithoutInteraction <- pf(fPartWithoutInteraction, dfPart, dfRepWithoutInteraction, lower.tail = F)
      pOperatorWithoutInteraction  <- pf(fOperatorWithoutInteraction, dfOperators, dfRepWithoutInteraction, lower.tail = F)


      anovaTable2$setData(list( "source"              = c(parts, operators, "Repeatability", "Total"),
                                "Df"                 = c(dfPart, dfOperators, dfRepWithoutInteraction, dfTotal),
                                "Sum Sq"             = c(ssPart, ssOperator, ssRepWithoutInteraction, ssTotal),
                                "Mean Sq"            = c(msPart, msOperator, msRepWithoutInteraction),
                                "F value"            = c(fPartWithoutInteraction, fOperatorWithoutInteraction),
                                "Pr(>F)"             = c(pPartWithoutInteraction, pOperatorWithoutInteraction)))


      anovaTables[['anovaTable2']] <- anovaTable2

      #r & R varcomps

      varCompList <- .gaugeCrossedVarComps(data, operators, parts, measurements, msPart, msOperator, msRepWithoutInteraction)
      varCompRepeat <- varCompList$repeatability
      varCompOperator <- varCompList$operator
      varCompPart <- varCompList$part
      varCompReprod <- varCompList$reprod
      varCompTotalGauge <- varCompList$totalGauge
      varCompTotalVar <- varCompList$totalVar

      if (options[["processVariationReference"]] == "historicalSd"){
        histSD <- options[["historicalSdValue"]]
        varCompTotalVar <- histSD^2
        varCompPart <- varCompTotalVar - varCompTotalGauge
      }

      varCompVector <- c(varCompTotalGauge, varCompRepeat, varCompReprod, varCompOperator, varCompPart, varCompTotalVar)
      varCompPercent <- varCompVector / varCompTotalVar * 100

      RRtable1$setData(list(      "Source"       = gettext(c("Total gauge r&R", "Repeatability", "Reproducibility", operators, "Part-to-part", "Total variation")),
                                  "Variation"    = varCompVector,
                                  "Percent"      = round(varCompPercent,2)))


      anovaTables[['RRtable1']] <- RRtable1

      #Gauge evaluation

      histSD <- options[["historicalSdValue"]]

      if (options[["processVariationReference"]] == "historicalSd" && histSD >= sqrt(varCompTotalGauge)) {
        SD <- c(sqrt(c(varCompTotalGauge, varCompRepeat, varCompReprod, varCompOperator)),
                sqrt(histSD^2 - varCompTotalGauge), histSD)
      }else{
        SD <- sqrt(varCompVector)
      }

      SD <- ifelse(SD == "NaN", 0, SD)
      studyVar <- SD * studyVarMultiplier
      RRtable2DataList <- list("source"       = gettext(c("Total gauge r&R", "Repeatability", "Reproducibility", operators, "Part-to-part", "Total variation")),
                               "SD"           = SD,
                               "studyVar"    = studyVar,
                               "percentStudyVar"    = c(round(studyVar/max(studyVar) * 100,2)))
      if(options[["tolerance"]])
        RRtable2DataList <- append(RRtable2DataList, list("percentTolerance" = c(round(studyVar / options[["toleranceValue"]] * 100,2))))
      RRtable2$setData(RRtable2DataList)
      nCategories <- .gaugeNumberDistinctCategories(sqrt(varCompPart), sqrt(varCompTotalGauge))
      RRtable2$addFootnote(gettextf("Number of distinct categories = %i", nCategories))
      if (as.integer(studyVarMultiplier) == round(studyVarMultiplier, 2)){
        RRtable2$addFootnote(gettextf("Study variation is calculated as std. dev. <spam>&#215;</spam> %i", as.integer(studyVarMultiplier)))
      }else{
        RRtable2$addFootnote(gettextf("Study variation is calculated as std. dev. <spam>&#215;</spam> %.2f", studyVarMultiplier))
      }

      if(options[["processVariationReference"]] == "historicalSd"){
        RRtable2$addFootnote(gettextf("Historical standard deviation is used to calculate some values for std. dev., study variation, and %%study variation."))
        RRtable2$addFootnote(gettextf("Values for %%process variation are not displayed because they are identical to values for %%study variation."))
      }
      anovaTables[['RRtable2']] <- RRtable2
    }

    if(singleOperator)
      varCompReprod <- 0

    percentContributionValues <- c(varCompTotalGauge, varCompRepeat, varCompReprod, varCompPart)/varCompTotalVar * 100
    SDs <- sqrt(c(varCompTotalGauge, varCompRepeat, varCompReprod, varCompPart))
    studyvars <- SDs * studyVarMultiplier
    studyVariationValues <- studyvars/max(studyVar) * 100
    if(options[["tolerance"]]){
      percentToleranceValues <- studyvars / options[["toleranceValue"]] * 100
    }else{
      percentToleranceValues <- NA
    }

    if (gaugeEvaluationDfOnly) {
      gaugeEvalDf <- as.data.frame(RRtable2DataList)
      gaugeEvalDf[,-1] <- round(gaugeEvalDf[,-1], .numDecimals) # Round everything while including the source column
      names(gaugeEvalDf) <- if (ncol(gaugeEvalDf) == 5) c("Source", "Std. dev.", "Study variation", "%Study variation", "%Tolerance") else c("Source", "Std. dev.", "Study variation", "%Study variation")
      return(list("gaugeEvalDf" = gaugeEvalDf, "nCategories" = nCategories))
    }

    p <- .gaugeVarCompGraph(percentContributionValues, studyVariationValues, percentToleranceValues, Type3)

    if (returnPlotOnly)
      return(p)
    else if (returnTrafficValues)
      return(list(study = c(round(studyVar/max(studyVar) * 100,2))[1], tol = c(round(studyVar / options[["toleranceValue"]] * 100,2))[1]))

    if (options[["varianceComponentsGraph"]]) {
      plot <- createJaspPlot(title = gettext("Components of variation"), width = 850, height = 500)
      plot$dependOn(c("varianceComponentsGraph", "report"))
      plot$plotObject <- p
      anovaTables[['VarCompGraph']] <- plot
    }
  } else {

    plot <- createJaspPlot(title = gettext("Components of variation"), width = 850, height = 500)
    plot$dependOn(c("gaugeVarCompGraph", "report"))

    anovaTables[['anovaTable1']] <- anovaTable1
    anovaTables[['RRtable1']] <- RRtable1
    anovaTables[['RRtable2']] <- RRtable2
    anovaTables[['plot']] <- plot


    if (length(measurements) >= 1 && !identical(operators, "") && !identical(parts, "") && ready) {
      if (returnTrafficValues)
        return(list(study = NA, tol = NA))
      if (returnPlotOnly)
        return (ggplot2::ggplot() +
                  ggplot2::theme_void() +
                  ggplot2::annotate("text", x = 0.5, y = 0.5, label = gettextf("Number of observations is < 2 in %1$s after grouping on %2$s.", parts, operators))) # return an empty plot if not possible to calculate anything
      if (gaugeEvaluationDfOnly)
       return(list("gaugeEvalDf" = NA, "nCategories" = NA))
      RRtable1$setError(gettextf("Number of observations is < 2 in %1$s after grouping on %2$s", parts, operators))
      RRtable2$setError(gettextf("Number of observations is < 2 in %1$s after grouping on %2$s", parts, operators))
    }
  }
  return(anovaTables)
}

.gaugeByPartGraph <- function(dataset, measurements, parts, operators, options) {
  plot <- createJaspPlot(title = gettext("Measurements by part"), width = 700, height = 300)
  plot$dependOn(c("partMeasurementPlot", "gaugeRRmethod", "report"))
  p <- .gaugeByPartGraphPlotObject(dataset, measurements, parts, operators, displayAll = options[["partMeasurementPlotAllValues"]])
  plot$plotObject <- p
  return(plot)
}

.gaugeByPartGraphPlotObject <- function(dataset, measurements, parts, operators, displayAll = FALSE) {
  dataset <- tidyr::gather(dataset, Repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)
  means <- aggregate(dataset["Measurement"], dataset[parts], mean)
  p <- ggplot2::ggplot()
  if (displayAll){
    yAxisBreaks <- jaspGraphs::getPrettyAxisBreaks(dataset[["Measurement"]])
    yAxisLimits <- range(c(dataset[["Measurement"]],yAxisBreaks))
    p <- p + jaspGraphs::geom_point(data = dataset, ggplot2::aes_string(x = parts, y = "Measurement"), col = "gray")
  }else{
    yAxisBreaks <- jaspGraphs::getPrettyAxisBreaks(means[["Measurement"]])
    yAxisLimits <- range(c(means[["Measurement"]], yAxisBreaks))
  }
  p <- p + jaspGraphs::geom_point(data = means, ggplot2::aes_string(x = parts, y = "Measurement")) +
    ggplot2::scale_y_continuous(limits = yAxisLimits, breaks = yAxisBreaks) +
    jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()
  return(p)
}


.gaugeByOperatorGraph <- function(dataset, measurements, parts, operators, options, ready, Type3 = FALSE) {

  plot <- createJaspPlot(title = gettext("Measurements by operator"), width = 600, height = 600)

  plot$dependOn(c("operatorMeasurementPlot", "gaugeRRmethod", "report"))

  if (ready) {
    plot$plotObject <- .gaugeByOperatorGraphPlotObject(dataset, measurements, parts, operators, options, Type3)
  }
  return(plot)
}

.gaugeByOperatorGraphPlotObject <- function(dataset, measurements, parts, operators, options, Type3 = FALSE){
  dataset <- tidyr::gather(dataset, Repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)
  yBreaks <- dataset["Measurement"]
  yLimits <- range(yBreaks)

  p <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = dataset, ggplot2::aes_string(x = operators, y = "Measurement"))  +
    ggplot2::scale_y_continuous(limits = yLimits) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (Type3)
    p <- p + ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(name = NULL)

  return(p)
}

.gaugeByInteractionGraph <- function(dataset, measurements, parts, operators, options, ready, Type3 = FALSE) {

  if (ready) {
    plot <- .gaugeByInteractionGraphPlotFunction(dataset, measurements, parts, operators, options, Type3 = Type3)
  } else {
    plot <- createJaspPlot(title = gettext("Part by operator interaction"), width = 700, height = 400)
    plot$dependOn(c("partByOperatorMeasurementPlot", "gaugeRRmethod", "report"))
  }
  return(plot)
}

.gaugeByInteractionGraphPlotFunction <- function(dataset, measurements, parts, operators, options, Type3 = FALSE, ggPlot = FALSE) {

  plot <- createJaspPlot(title = gettext("Part by operator interaction"), width = 700, height = 400)
  plot$dependOn(c("partByOperatorMeasurementPlot", "gaugeRRmethod", "report"))

  byOperator <- split.data.frame(dataset, dataset[operators])
  partNames <- unique(dataset[[parts]])

  for (name in names(byOperator)) {
    if (nrow(byOperator[[name]][measurements]) != length(partNames)) {
      plot <- createJaspPlot(title = gettext("Part by operator interaction"), width = 700, height = 400)
      plot$setError(gettext("Operators measured different number of parts."))
      return(plot)
    }
  }

  means <- rowMeans(dataset[measurements])
  df <- data.frame(Part = dataset[[parts]], Operator = dataset[[operators]], Means = means)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(means)
  yLimits <- range(c(yBreaks, means))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Part, y = Means, col = Operator, group = Operator)) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point() +
    ggplot2::scale_y_continuous(name = "Average", breaks = yBreaks, limits = yLimits) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (!Type3)
    p <- p + ggplot2::theme(legend.position = 'right')

  plot$plotObject <- p

  if (ggPlot)
    return(p)
  else
    return(plot)
}

.gaugeScatterPlotOperators <- function(jaspResults, dataset, measurements, parts, operators, options, ready) {

  singleEmptyPlot <- createJaspPlot(title = gettext("Scatterplot of operator vs operator"))
  singleEmptyPlot$dependOn(c("scatterPlot", "scatterPlotFitLine", "scatterPlotOriginLine", "gaugeRRmethod", "report"))

  if (!ready)
    return(singleEmptyPlot)

  operatorVector <- as.character(unique(dataset[[operators]]))
  len <- length(operatorVector)

  if (len < 2) {
    singleEmptyPlot$setError(gettext("Cannot plot scatterplot for less than 2 operators."))
    return(singleEmptyPlot)
  }else{
    singlePlot <- createJaspPlot(title = gettextf("Scatterplot of operator  %1$s vs operator %2$s", operatorVector[1], operatorVector[2]))
    singlePlot$dependOn(c("scatterPlot", "scatterPlotFitLine", "scatterPlotOriginLine", "gaugeRRmethod", "report"))
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
      matrixPlot <- createJaspPlot(title = gettext("Matrix plot for operators"), width = 700, height = 700)
      matrixPlot$dependOn(c("scatterPlot", "report"))
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
      if (!is.na(as.numeric(operatorVector[1])) | nchar(operatorVector[1]) == 1){
        labels <- paste(operators, operatorVector)
      }else{
        labels <- operatorVector
      }
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
    ggplot2::scale_y_continuous(name = ylab, breaks = yBreaks, limits = range(c(yBreaks, df$Operator1))) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  if (options[["scatterPlotFitLine"]])
    p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE)
  if (options[["scatterPlotOriginLine"]])
    p <- p + ggplot2::geom_abline(col = "gray", linetype = "dashed")

  return(p)
}

.gaugeVarCompGraph <- function(percentContributionValues, studyVariationValues, percentToleranceValues, Type3 = FALSE) {
  sources <- gettext(c('Gauge r&R', 'Repeat', 'Reprod', 'Part-to-part'))
  if (!all(is.na(percentToleranceValues))) {
    references <- gettextf(c('%% Contribution', '%% Study variation', '%% Tolerance'))
    values <- c(percentContributionValues, studyVariationValues, percentToleranceValues)
  } else {
    references <- gettextf(c('%% Contribution', '%% Study Variation'))
    values <- c(percentContributionValues, studyVariationValues)
  }
  plotframe <- data.frame(source = rep(sources, length(references)),
                          reference = rep(references, each = 4),
                          value = values)
  plotframe$source <- factor(plotframe$source, levels = sources)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, plotframe$value))


  if (Type3)
    plotframe <- subset(plotframe, source != "Reprod")

  p <- ggplot2::ggplot() +
    ggplot2::geom_bar(data = plotframe, mapping = ggplot2::aes(fill =  reference,  y = value, x = source),
                      position="dodge", stat = "identity") +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe() +
    ggplot2::theme(legend.position = 'right', legend.title = ggplot2::element_blank()) +
    ggplot2::xlab(NULL) +
    ggplot2::scale_y_continuous(name = "Percent", breaks = yBreaks, limits = range(c(yBreaks, plotframe$value)))
  return(p)
}

.gaugeNumberDistinctCategories <- function(sdPart, sdGauge) {
  nCategories <- (sdPart / sdGauge) * 1.41
  if (nCategories < 1)
    nCategories <- 1
  return(as.integer(nCategories))
}

.ssPart <- function(dataset, operators, parts, measurements){
  nOperator <- length(unique(dataset[[operators]]))
  nReplicates <- table(dataset[parts])[1] / nOperator
  grandMean <- mean(dataset[[measurements]])
  partVector <- as.character(unique(dataset[[parts]]))
  SS <- 0
  for(part in partVector){
    dataPerPart <- subset(dataset, dataset[parts] == part)
    meanPerPart <- mean(dataPerPart[[measurements]])
    SS <- SS + (meanPerPart - grandMean)^2
  }
  SS <- nOperator * nReplicates * SS
  return(as.vector(SS))
}


.ssRepWithInteraction <- function(dataset, operators, parts, measurements){
  operatorVector <- as.character(unique(dataset[[operators]]))
  partVector <- as.character(unique(dataset[[parts]]))
  SS <- 0

  if (length(operatorVector) == 1) {
    for(part in partVector){
      partDataPerOP <- subset(dataset, dataset[parts] == part)
      meanPartPerOP <- mean(partDataPerOP[[measurements]])
      SS <- SS + sum((partDataPerOP[[measurements]] - meanPartPerOP)^2)
    }
  } else {
    for(op in operatorVector){
      dataPerOP <- subset(dataset, dataset[operators] == op)
      for(part in partVector){
        partDataPerOP <- subset(dataPerOP, dataPerOP[parts] == part)
        meanPartPerOP <- mean(partDataPerOP[[measurements]])
        SS <- SS + sum((partDataPerOP[[measurements]] - meanPartPerOP)^2)
      }
    }
  }
  return(SS)
}


.ssTotal <- function(dataset, operators, parts, measurements){
  grandMean <- mean(dataset[[measurements]])
  SS <- sum((dataset[[measurements]] - grandMean)^2)
  return(SS)
}

.dfGaugeCrossed <- function(dataset, operators, parts, measurements){
  nParts <- length(unique(dataset[[parts]]))
  nOperators <- length(unique(dataset[[operators]]))
  nReplicates <- as.vector(table(dataset[parts])[1] / nOperators)
  dfList <- list(dfPart          = nParts - 1,
                 dfOperator      = nOperators - 1,
                 dfPartXOperator = (nParts - 1) * (nOperators - 1),
                 dfRepeat        = nParts * nOperators * (nReplicates - 1),
                 dfTotal         = (nParts * nOperators * nReplicates) - 1)
  return(dfList)
}

.gaugeCrossedVarComps <- function(dataset, operators, parts, measurements, msPart, msOperator, msRep, msInteraction = ""){
  nParts <- length(unique(dataset[[parts]]))
  nOperators <- length(unique(dataset[[operators]]))
  nReplicates <- as.vector(table(dataset[parts])[1] / nOperators)
  repeatability <- msRep
  if (msInteraction != ""){
    operator <- (msOperator - msInteraction) / (nParts * nReplicates)
    interaction <- (msInteraction - msRep) / nReplicates
    part <- (msPart - msInteraction) / (nOperators * nReplicates)
    reprod <- operator + interaction
  } else {
    operator <- (msOperator - msRep) / (nParts * nReplicates)
    part <- (msPart - msRep) / (nOperators * nReplicates)
    reprod <- operator
  }
  operator <- max(0, operator)
  part <- max(0, part)
  reprod <- max(0, reprod)
  totalGauge <- repeatability + reprod
  totalVar <- totalGauge + part
  varcompList <- list(repeatability = repeatability,
                      operator      = operator,
                      part          = part,
                      reprod        = reprod,
                      totalGauge    = totalGauge,
                      totalVar      = totalVar)
  if (msInteraction != ""){
    varcompList[["interaction"]] <- interaction
  }
  for (i in 1:length(varcompList)){
    if (varcompList[[i]] < 0){
      varcompList[[i]] <- 0
    }
  }
  return(varcompList)
}

.gaugeCrossedVarCompsSingleOP <- function(dataset, operators, parts, measurements, msPart, msRep){
  nParts <- length(unique(dataset[[parts]]))
  nOperators <- length(unique(dataset[[operators]]))
  nReplicates <- length(dataset[[measurements]]) / nParts
  repeatability <- msRep
  totalGauge <- repeatability
  part <- (msPart - msRep) / (nOperators * nReplicates); if (part < 0) part <- 0 # correct for zero part-part variance.
  totalVar <- totalGauge + part
  varcompList <- list(repeatability = repeatability,
                      part          = part,
                      totalGauge    = totalGauge,
                      totalVar      = totalVar)
  return(varcompList)
}

.checkIfCrossed <- function(dataset, operators, parts, measurements){
  partVector <- as.character(unique(dataset[[parts]]))
  operatorVector <- as.character(unique(dataset[[operators]]))
  for(part in partVector){
    partData <- subset.data.frame(dataset, dataset[parts] == part)
    if(!all(operatorVector %in% partData[[operators]])){
      return(FALSE)
    }
  }
  return(TRUE)
}
.trafficplot <- function(StudyVar = "", ToleranceUsed = FALSE, ToleranceVar = "", options, ready, Xlab.StudySD = "", Xlab.Tol = "", ggPlot = FALSE) {

  if (!ready)
    return()

  Plot <- createJaspPlot(width = 1000)

  mat <- data.frame(
    x = rep(c(70,20,10),2),
    Yes = c(rep('A',3), rep("B",3)),
    fill = rep(c("G","R","Y"),2)
  )

  if (StudyVar == "" | ToleranceVar == "" | is.na(StudyVar) | is.na(ToleranceVar)) {
    plotObject <-  ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::annotate("text", x = 0.5, y = 0.5, label = gettext("Error: Gauge study failed. Could not create traffic light chart."))
    Plot$plotObject <- plotObject
    return(if (ggPlot) plotObject else Plot)
  }

  if (StudyVar >= 100) {StudyVar = 100}
  if (ToleranceVar >= 100) {ToleranceVar = 100}

  p1 <- ggplot2::ggplot(mat[c(1:3),], ggplot2::aes(x = x, y = Yes, fill = fill)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values= rev(c('#008450','#EFB700', '#B81D13'))) +
    ggplot2::geom_vline(xintercept = StudyVar) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(fontsize = jaspGraphs::setGraphOption("fontsize", 15)) +
    ggplot2::scale_y_discrete(name = NULL) +
    ggplot2::theme(legend.position="none",
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(breaks = c(0,10,30,100), labels = c("0%","10%","30%","100%"), name = "Percent measurement system variation of the process variation") +
    ggplot2::annotate("text", x = StudyVar + 5, y = 1.52, size = 5, label = paste0(gettextf("%.2f", StudyVar), "%"))

  if (Xlab.StudySD != "")
    p1 <- p1 + ggplot2::scale_x_continuous(breaks = c(0,10,30,100), labels = c("0%","10%","30%","100%"), name = gettext(Xlab.StudySD))


  if (ToleranceUsed){
    p2 <- ggplot2::ggplot(mat[c(4:6),], ggplot2::aes(x = x, y = Yes, fill = fill)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values= rev(c('#008450','#EFB700', '#B81D13')))+
      ggplot2::geom_vline(xintercept = ToleranceVar) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(fontsize = jaspGraphs::setGraphOption("fontsize", 15)) +
      ggplot2::theme(legend.position="none",
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank()) +
      ggplot2::scale_x_continuous(breaks = c(0,10,30,100), labels = c("0%","10%","30%","100%"), name = "Percent measurement system variation of the tolerance") +
      ggplot2::annotate("text", x = ToleranceVar + 5, y = 1.52, size = 5, label = paste0(gettextf("%.2f", ToleranceVar), "%"))

    if (Xlab.Tol != "")
      p2 <- p2 + ggplot2::scale_x_continuous(breaks = c(0,10,30,100), labels = c("0%","10%","30%","100%"), name = gettext(Xlab.Tol))

    p3 <- jaspGraphs::ggMatrixPlot(plotList = list(p2, p1), layout = matrix(2:1, 2))
    Plot$plotObject <- p3

    if (!ggPlot)
      return(Plot)
    else
      return(list(p1 = p2, p2 = p1))
  } else {

    Plot$plotObject <- p1

    if (!ggPlot)
      return(Plot)
    else
      return(p1)
  }
}
