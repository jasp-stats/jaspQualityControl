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


  if (options[["gaugeRRdataFormat"]] == "gaugeRRwideFormat"){
    measurements <- unlist(options$measurements)
  }else{
    measurements <- unlist(options$measurementsLong)
  }
  parts <- unlist(options$parts)
  operators <- unlist(options$operators)

  if (options[["gaugeRRdataFormat"]] == "gaugeRRwideFormat" && !options$Type3)
    ready <- (length(measurements) != 0 && operators != "" && parts != "")
  else if (options$Type3)
    ready <- (measurements != "" && parts != "" & length(measurements) != 0)
  else
    ready <- (measurements != "" && operators != "" && parts != "")


  numeric.vars <- measurements
  numeric.vars <- numeric.vars[numeric.vars != ""]
  factor.vars <- c(parts, operators)
  factor.vars <- factor.vars[factor.vars != ""]

  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = numeric.vars, columns.as.factor = factor.vars,
                                         exclude.na.listwise = c(numeric.vars, factor.vars))
    if (options$Type3){
      dataset$operators <- rep(1, nrow(dataset))
      operators <- "operators"
    }
  }

  if(ready & !options$Type3){
    crossed <- .checkIfCrossed(dataset, operators, parts, measurements)
    if(!crossed){
      plot <- createJaspPlot(title = gettext("Gauge r&R"), width = 700, height = 400)
      jaspResults[["plot"]] <- plot
      plot$setError(gettext("Design is not balanced: not every operator measured every part. Use non-replicable gauge r&R."))
      return()
    }
  }

  if(ready){
    crossed <- .checkIfCrossed(dataset, operators, parts, measurements)
    if(!crossed){
      plot <- createJaspPlot(title = gettext("Gauge r&R"), width = 700, height = 400)
      jaspResults[["plot"]] <- plot
      plot$setError(gettext("Design is not balanced: not every operator measured every part. Use non-replicable gauge r&R."))
      return()
    }
  }

  if (ready && nrow(dataset[measurements]) == 0){
    jaspResults[["plot"]] <- createJaspPlot(title = gettext("Gauge r&R"), width = 700, height = 400)
    jaspResults[["plot"]]$setError(gettextf("No valid measurements in %s.", measurements))
    jaspResults[["plot"]]$position <- 1
    jaspResults[["plot"]]$dependOn(c("measurements", "measurementsLong"))
    return()
  }

  if (options[["gaugeRRdataFormat"]] == "gaugeRRlongFormat" && ready) {
    dataset <- dataset[order(dataset[operators]),]
    dataset <- dataset[order(dataset[parts]),]
    nrep <- table(dataset[operators])[[1]]/length(unique(dataset[[parts]]))
    index <- rep(paste("V", 1:nrep, sep = ""), nrow(dataset)/nrep)
    dataset <- cbind(dataset, data.frame(index = index))
    dataset <- tidyr::spread(dataset, index, measurements)
    measurements <- unique(index)
    dataset <- dataset[,c(operators, parts, measurements)]
  }
  Type3 <- c(length(unique(dataset[[operators]])) == 1 || options$Type3)

  .msaCheckErrors(dataset, options)


  #ANOVA method

  # Gauge r&R ANOVA Table
  if (options[["gaugeANOVA"]]) {
    if (is.null(jaspResults[["gaugeANOVA"]])) {
      jaspResults[["gaugeANOVA"]] <- createJaspContainer(gettext("Gauge r&R ANOVA Table"))
      jaspResults[["gaugeANOVA"]]$dependOn(c("historicalStandardDeviation", "studyStandardDeviation", "standardDeviationReference", "historicalStandardDeviationValue"))
      jaspResults[["gaugeANOVA"]]$position <- 1
    }

    jaspResults[["gaugeANOVA"]] <- .gaugeANOVA(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready, Type3 = Type3)
  }

  # R chart by operator
  if (options[["gaugeRchart"]]) {
    if (is.null(jaspResults[["gaugeRchart"]])) {
      jaspResults[["gaugeRchart"]] <- createJaspContainer(gettext("Range Chart by Operator"))
      jaspResults[["gaugeRchart"]]$position <- 3
    }
    jaspResults[["gaugeRchart"]] <- .xBarOrRangeChart(type = "Range", dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready, Type3 = Type3)
    jaspResults[["gaugeRchart"]]$dependOn(c("gaugeRchart", "gaugeRRmethod"))
  }

  # Xbar chart by operator
  if (options[["gaugeXbarChart"]]) {
    if (is.null(jaspResults[["gaugeXbarChart"]])) {
      jaspResults[["gaugeXbarChart"]] <- createJaspContainer(gettext("Xbar Chart by Operator"))
      jaspResults[["gaugeXbarChart"]]$position <- 4
    }
    jaspResults[["gaugeXbarChart"]] <- .xBarOrRangeChart(type = "Average",dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready, Type3 = Type3)
    jaspResults[["gaugeXbarChart"]]$dependOn(c("gaugeXbarChart", "gaugeRRmethod"))
  }

  # gauge Scatter Plot Operators
  if (options[["gaugeScatterPlotOperators"]]) {
    if (is.null(jaspResults[["gaugeScatterOperators"]])) {
      jaspResults[["gaugeScatterOperators"]] <- createJaspContainer(gettext("Scatterplot Operators"))
      jaspResults[["gaugeScatterOperators"]]$position <- 5
    }
    jaspResults[["gaugeScatterOperators"]] <- .gaugeScatterPlotOperators(jaspResults = jaspResults, dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready)
    jaspResults[["gaugeScatterOperators"]]$dependOn("gaugeRRmethod")
  }

  # Measurement by Part Graph
  if (options[["gaugeByPart"]] & ready) {
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
    jaspResults[["gaugeByOperator"]] <- .gaugeByOperatorGraph(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready, Type3 = Type3)
  }

  # Parts by Operator Interaction Plot
  if (options[["gaugeByInteraction"]]) {
    if (is.null(jaspResults[["gaugeByInteraction"]])) {
      jaspResults[["gaugeByInteraction"]] <- createJaspContainer(gettext("Parts by Operator Interaction Graph"))
      jaspResults[["gaugeByInteraction"]]$position <- 8
    }
    jaspResults[["gaugeByInteraction"]] <- .gaugeByInteractionGraph(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready, Type3 = Type3)
  }
  # Traffic light plot
  if(options[["trafficPlot"]] & is.null(jaspResults[["trafficPlot"]] )) {
    jaspResults[["trafficPlot"]] <- createJaspContainer(gettext("Traffic light graph"))
    jaspResults[["trafficPlot"]]$position <- 9
    jaspResults[["trafficPlot"]]$dependOn(c("trafficPlot", "tolerance","gaugeToleranceEnabled", "gaugeRRmethod", "historicalStandardDeviation", "studyStandardDeviation", "standardDeviationReference", "historicalStandardDeviationValue"))
    trafficContainer <- jaspResults[["trafficPlot"]]

    valuesVec <- .gaugeANOVA(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = ready, returnTrafficValues = TRUE, Type3 = Type3)
    trafficContainer[["plot"]] <- .trafficplot(StudyVar = valuesVec$study, ToleranceUsed = options$gaugeToleranceEnabled,ToleranceVar = valuesVec$tol, options = options, ready = ready)

  }

  # Report
  if (options[["anovaGaugeReport"]]) {
    if (is.null(jaspResults[["anovaGaugeReport"]])) {
      jaspResults[["anovaGaugeReport"]] <- createJaspContainer(gettext("Report"))
      jaspResults[["anovaGaugeReport"]]$position <- 9
    }
    jaspResults[["anovaGaugeReport"]] <- .anovaGaugeReport(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options = options, Type3 = Type3)
    jaspResults[["anovaGaugeReport"]]$dependOn(c("anovaGaugeReportedBy", "anovaGaugeTitle", "anovaGaugeName", "anovaGaugeDate",
                                               "anovaGaugeMisc", "anovaGaugeReport"))
  }



  return()
}

.gaugeANOVA <- function(dataset, measurements, parts, operators, options, ready, returnPlotOnly = FALSE, returnTrafficValues = FALSE, Type3 = FALSE) {
  anovaTables <- createJaspContainer(gettext("ANOVA Table"))
  anovaTables$dependOn(c("gaugeANOVA", "gaugeRRmethod"))
  anovaTables$position <- 1

  anovaTable1 <- createJaspTable(title = ifelse(Type3, gettext("Two-way ANOVA Table"), gettext("Two-way ANOVA Table with Interaction")))
  anovaTable1$addColumnInfo(title = gettext("Source"),       name = "source",   type = "string" )
  anovaTable1$addColumnInfo(title = gettext("df"),             name = "Df",      type = "integer")
  anovaTable1$addColumnInfo(title = gettext("Sum of Squares"), name = "Sum Sq",  type = "number")
  anovaTable1$addColumnInfo(title = gettext("Mean Squares"),    name = "Mean Sq", type = "number")
  anovaTable1$addColumnInfo(title = gettext("F"),              name = "F value", type = "number")
  anovaTable1$addColumnInfo(title = gettext("<i>p</i>-value"),              name = "Pr(>F)",  type = "pvalue")

  RRtable1 <- createJaspTable(title = gettext("Variance Components"))
  RRtable1$dependOn(c("gaugeANOVA", "operators", "parts", "measurements"))
  RRtable1$addColumnInfo(name = "Source", title = gettext("Source"), type = "string")
  RRtable1$addColumnInfo(name = "Variation", title = gettext("Variance"), type = "number")
  RRtable1$addColumnInfo(name = "Percent", title = gettext("% Contribution"), type = "integer")

  RRtable2 <- createJaspTable(title = gettext("Gauge Evaluation"))
  RRtable2$dependOn(c("gaugeANOVA", "operators", "parts", "measurements"))
  RRtable2$addColumnInfo(name = "source", title = gettext("Source"), type = "string")
  RRtable2$addColumnInfo(name = "SD", title = gettext("Std. Deviation"), type = "number")
  RRtable2$addColumnInfo(name = "studyVar", title = gettextf("Study Variation"), type = "number")
  RRtable2$addColumnInfo(name = "percentStudyVar", title = gettext("% Study Variation"), type = "integer")
  if(options[["gaugeToleranceEnabled"]])
    RRtable2$addColumnInfo(name = "percentTolerance", title = gettext("% Tolerance"), type = "integer")

  for (measurement in measurements) {
    if (length(dataset[[measurement]]) <= 1)
      ready <- FALSE
  }

  if (ready && length(measurements) >= 2) {
    singleOperator <- Type3

    data <- dataset

    data <- tidyr::gather(data, repetition, measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

    if (options$studyVarMultiplierType == "svmSD") {
      studyVarMultiplier <- options$studyVarMultiplier
    }else{
      percent <- options$studyVarMultiplier/100
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

    if (options$TypeForFstat == 'FixedEffects'){
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
      sources <- c(parts, operators, paste(parts," x ", operators), "Repeatability", "Total")
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

    if(!singleOperator){
      interactionSignificant <- pInteraction < options$alphaForANOVA
    }else{
      interactionSignificant <- 'not applicable'
    }

    if (singleOperator || interactionSignificant){

      #r & R varcomps

      if(singleOperator){
        varCompList <- .gaugeCrossedVarCompsSingleOP(data, operators, parts, measurements, msPart, msRepWithInteraction)
        varCompRepeat <- varCompList$repeatability
        varCompPart <- varCompList$part
        varCompTotalGauge <- varCompList$totalGauge
        varCompTotalVar <- varCompList$totalVar
        varCompVector <- c(varCompTotalGauge, varCompRepeat, varCompPart, varCompTotalVar)
        sources <- gettext(c("Total Gauge r&R", "Repeatability", "Part-to-Part", "Total Variation"))
      }else{
        varCompList <- .gaugeCrossedVarComps(data, operators, parts, measurements, msPart, msOperator, msRepWithInteraction, msInteraction)
        varCompRepeat <- varCompList$repeatability
        varCompOperator <- varCompList$operator
        varCompPart <- varCompList$part
        varCompReprod <- varCompList$reprod
        varCompTotalGauge <- varCompList$totalGauge
        varCompTotalVar <- varCompList$totalVar
        varCompInteraction <- varCompList$interaction
        varCompVector <- c(varCompTotalGauge, varCompRepeat, varCompReprod, varCompOperator, varCompPart, varCompInteraction, varCompTotalVar)
        sources <- gettext(c("Total Gauge r&R", "Repeatability", "Reproducibility", operators, "Part-to-Part", paste(parts," x ", operators), "Total Variation"))
      }

      varCompPercent <- varCompVector / varCompTotalVar * 100

      if (options$standardDeviationReference == "historicalStandardDeviation"){
        histSD <- options$historicalStandardDeviationValue
        varCompVector$varCompTotalVar <- histSD^2
        varCompVector$varCompPart <- varCompVector$varCompTotalVar - varCompTotalGauge
      }


      RRtable1$setData(list(      "Source"       = sources,
                                  "Variation"    = varCompVector,
                                  "Percent"      = round(varCompPercent,2)))

      anovaTables[['RRtable1']] <- RRtable1

      #Gauge evaluation

      histSD <- options$historicalStandardDeviationValue

      if(!singleOperator){
        if (options$standardDeviationReference == "historicalStandardDeviation" && histSD >= sqrt(varCompTotalGauge)) {
          SD <- c(sqrt(c(varCompTotalGauge, varCompRepeat, varCompReprod, varCompOperator)),
                  sqrt(histSD^2 - varCompTotalGauge),
                  sqrt(varCompInteraction), histSD)

        }else{
          SD <- sqrt(varCompVector)
        }
        sdParts <- SD[5]
        sdGauge <- SD[1]
      }else{
        if (options$standardDeviationReference == "historicalStandardDeviation" && histSD >= sqrt(varCompTotalGauge)) {
          SD <- c(sqrt(c(varCompTotalGauge, varCompRepeat)), sqrt(histSD^2 - varCompTotalGauge), histSD)
        }else{
          SD <- sqrt(varCompVector)
        }
        sdParts <- SD[3]
        sdGauge <- SD[1]

      }
      studyVar <- SD * studyVarMultiplier
      RRtable2DataList <- list("source"       = sources,
                               "SD"           = SD,
                               "studyVar"    = studyVar,
                               "percentStudyVar"    = c(round(studyVar/max(studyVar) * 100,2)))
      if(options[["gaugeToleranceEnabled"]])
        RRtable2DataList <- append(RRtable2DataList, list("percentTolerance" = c(round(studyVar / options$tolerance * 100,2))))
      RRtable2$setData(RRtable2DataList)
      nCategories <- .gaugeNumberDistinctCategories(sdParts, sdGauge)
      RRtable2$addFootnote(gettextf("Number of distinct categories = %i", nCategories))
      if (as.integer(studyVarMultiplier) == round(studyVarMultiplier, 2)){
        RRtable2$addFootnote(gettextf("Study Variation is calculated as Std. Deviation x %i", as.integer(studyVarMultiplier)))
      }else{
        RRtable2$addFootnote(gettextf("Study Variation is calculated as Std. Deviation x %.2f", studyVarMultiplier))
      }

      if(options$standardDeviationReference == "historicalStandardDeviation"){
        RRtable2$addFootnote(gettext("Historical standard deviation is used to calculate some values for Std. Deviation, Study Variation, and %Study Variation."))
        RRtable2$addFootnote(gettext("Values for %Process Variation are not displayed because they are identical to values for %Study Variation."))
      }


      anovaTables[['RRtable2']] <- RRtable2

    }else{

      anovaTable2 <- createJaspTable(title = gettext("Two-way ANOVA Table without Interaction"))
      anovaTable2$addColumnInfo(title = gettext("Source"),        name = "source",   type = "string" )
      anovaTable2$addColumnInfo(title = gettext("df"),             name = "Df",      type = "integer")
      anovaTable2$addColumnInfo(title = gettext("Sum of Squares"), name = "Sum Sq",  type = "number")
      anovaTable2$addColumnInfo(title = gettext("Mean Squares"),    name = "Mean Sq", type = "number")
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

      if (options$standardDeviationReference == "historicalStandardDeviation"){
        histSD <- options$historicalStandardDeviationValue
        varCompTotalVar <- histSD^2
        varCompPart <- varCompTotalVar - varCompTotalGauge
      }

      varCompVector <- c(varCompTotalGauge, varCompRepeat, varCompReprod, varCompOperator, varCompPart, varCompTotalVar)
      varCompPercent <- varCompVector / varCompTotalVar * 100

      RRtable1$setData(list(      "Source"       = gettext(c("Total Gauge r&R", "Repeatability", "Reproducibility", operators, "Part-to-Part", "Total Variation")),
                                  "Variation"    = varCompVector,
                                  "Percent"      = round(varCompPercent,2)))


      anovaTables[['RRtable1']] <- RRtable1

      #Gauge evaluation

      histSD <- options$historicalStandardDeviationValue

      if (options$standardDeviationReference == "historicalStandardDeviation" && histSD >= sqrt(varCompTotalGauge)) {
        SD <- c(sqrt(c(varCompTotalGauge, varCompRepeat, varCompReprod, varCompOperator)),
                sqrt(histSD^2 - varCompTotalGauge), histSD)
      }else{
        SD <- sqrt(varCompVector)
      }

      studyVar <- SD * studyVarMultiplier
      RRtable2DataList <- list("source"       = gettext(c("Total Gauge r&R", "Repeatability", "Reproducibility", operators, "Part-to-Part", "Total Variation")),
                               "SD"           = SD,
                               "studyVar"    = studyVar,
                               "percentStudyVar"    = c(round(studyVar/max(studyVar) * 100,2)))
      if(options[["gaugeToleranceEnabled"]])
        RRtable2DataList <- append(RRtable2DataList, list("percentTolerance" = c(round(studyVar / options$tolerance * 100,2))))
      RRtable2$setData(RRtable2DataList)
      nCategories <- .gaugeNumberDistinctCategories(SD[5], SD[1])
      RRtable2$addFootnote(gettextf("Number of distinct categories = %i", nCategories))
      if (as.integer(studyVarMultiplier) == round(studyVarMultiplier, 2)){
        RRtable2$addFootnote(gettextf("Study Variation is calculated as Std. Deviation x %i", as.integer(studyVarMultiplier)))
      }else{
        RRtable2$addFootnote(gettextf("Study Variation is calculated as Std. Deviation x %.2f", studyVarMultiplier))
      }

      if(options$standardDeviationReference == "historicalStandardDeviation"){
        RRtable2$addFootnote(gettext("Historical standard deviation is used to calculate some values for Std. Deviation, Study Variation, and %Study Variation."))
        RRtable2$addFootnote(gettext("Values for %Process Variation are not displayed because they are identical to values for %Study Variation."))
      }
      anovaTables[['RRtable2']] <- RRtable2
    }

    if(singleOperator)
      varCompReprod <- 0

    percentContributionValues <- c(varCompTotalGauge, varCompRepeat, varCompReprod, varCompPart)/varCompTotalVar * 100
    SDs <- sqrt(c(varCompTotalGauge, varCompRepeat, varCompReprod, varCompPart))
    studyvars <- SDs * studyVarMultiplier
    studyVariationValues <- studyvars/max(studyVar) * 100
    if(options[["gaugeToleranceEnabled"]]){
      percentToleranceValues <- studyvars / options$tolerance * 100
    }else{
      percentToleranceValues <- NA
    }

    p <- .gaugeVarCompGraph(percentContributionValues, studyVariationValues, percentToleranceValues, Type3)

    if (returnPlotOnly)
      return(p)
    else if (returnTrafficValues)
      return(list(study = c(round(studyVar/max(studyVar) * 100,2))[1], tol = c(round(studyVar / options$tolerance * 100,2))[1]))

    if (options[["gaugeVarCompGraph"]]) {
      plot <- createJaspPlot(title = gettext("Components of Variation"), width = 850, height = 500)
      plot$dependOn(c("gaugeVarCompGraph"))
      plot$plotObject <- p
      anovaTables[['VarCompGraph']] <- plot
    }
  }else {

    plot <- createJaspPlot(title = gettext("Components of Variation"), width = 850, height = 500)
    plot$dependOn(c("gaugeVarCompGraph"))

    anovaTables[['anovaTable1']] <- anovaTable1
    anovaTables[['RRtable1']] <- RRtable1
    anovaTables[['RRtable2']] <- RRtable2
    anovaTables[['plot']] <- plot


    if (length(measurements) >= 1 && operators != "" && parts != "" && ready) {
      RRtable1$setError(gettextf("Number of observations is < 2 in %s after grouping on %s", parts, operators))
      RRtable2$setError(gettextf("Number of observations is < 2 in %s after grouping on %s", parts, operators))
    }
  }
  return(anovaTables)
}

.xBarOrRangeChart <- function(type = c("Average", "Range"), dataset, measurements, parts, operators, options, ready, Type3 = FALSE) {
  if (!ready) {
    plot <- createJaspPlot(title = gettextf("%s Chart by Operator", type), width = 900, height = 300)
    return(plot)
  }
  if (length(measurements) < 2) {
    plot <- createJaspPlot(title = gettextf("%s Chart by Operator"), width = 900, height = 300)
    plot$setError(gettext("More than 1 Measurement per Operator required."))
    return(plot)
  }
  plot <- createJaspPlot(title = gettextf("%s Chart by Operator", type), width = 2500, height = 300)
  plot$plotObject <- .xBarOrRangeChartPlotFunction(type, dataset, measurements, parts, operators, options, smallLabels = TRUE, Type3)
  return(plot)
}

.xBarOrRangeChartPlotFunction <- function(type = c("Average", "Range"), dataset, measurements, parts, operators, options, smallLabels = FALSE, Type3 = FALSE){
  operatorVector <- unique(dataset[[operators]])
  nOperators <- length(operatorVector)
  data <- dataset[measurements]
  if (type == "Range") {
    ChartData <- qcc::qcc(data, type= 'R', plot = FALSE)
    leftLabel <- "Subgroup range"
  }else{
    ChartData <- qcc::qcc(data, type= 'xbar', plot = FALSE)
    leftLabel <- "Subgroup average"
  }
  center <- ChartData$center
  UCL <- max(ChartData$limits)
  LCL <- min(ChartData$limits)
  manualLimits <- c(LCL, center, UCL)

  plotMat <- list()
  titleVector <- vector(mode = "character")

  for (i in 1:nOperators) {
    op <- as.character(operatorVector[i])

    if (Type3)
      dataPerOP <- dataset
    else
      dataPerOP <- subset(dataset, dataset[operators] == op)

    if (!is.na(as.numeric(op)) | nchar(op) == 1)
      title <- gettextf("Operator %s", op)
    else if (Type3)
      title <- NULL
    else
      title <- op

    titleVector <- c(titleVector, title)
    manualSubgroups <- as.numeric(dataPerOP[[parts]])
    if (type == "Range"){
      if (i == 1){
        p1 <- .RchartNoId(dataset = dataPerOP[measurements], options = options, manualLimits = manualLimits, warningLimits = FALSE, manualSubgroups = manualSubgroups, plotLimitLabels = ifelse(Type3, TRUE, FALSE),
                          xAxisLab = parts, yAxisLab = leftLabel, manualDataYaxis = dataset[measurements], manualXaxis = unique(dataset[[parts]]), title = title, smallLabels = smallLabels, OnlyOutofLimit = TRUE, GaugeRR = TRUE)$p
      }else if(i == nOperators){
        p1 <- p1 <- .RchartNoId(dataset = dataPerOP[measurements], options = options, manualLimits = manualLimits, warningLimits = FALSE, manualSubgroups = manualSubgroups, yAxis = FALSE, GaugeRR = TRUE,
                                xAxisLab = parts, yAxisLab = ggplot2::element_blank(), manualDataYaxis = dataset[measurements], manualXaxis = unique(dataset[[parts]]), title = title, smallLabels = smallLabels, OnlyOutofLimit = TRUE)$p
      }
      else{
        p1 <- p1 <- .RchartNoId(dataset = dataPerOP[measurements], options = options, manualLimits = manualLimits, warningLimits = FALSE, manualSubgroups = manualSubgroups, yAxis = FALSE, plotLimitLabels = FALSE, GaugeRR = TRUE,
                                xAxisLab = parts, yAxisLab = ggplot2::element_blank(), manualDataYaxis = dataset[measurements], manualXaxis = unique(dataset[[parts]]), title = title, smallLabels = smallLabels, OnlyOutofLimit = TRUE)$p
      }
    }else{
      if (i == 1){
        p1 <- .XbarchartNoId(dataset = dataPerOP[measurements], options = options, manualLimits = manualLimits,
                             warningLimits = FALSE, manualSubgroups = manualSubgroups, plotLimitLabels = ifelse(Type3, TRUE, FALSE), GaugeRR = TRUE,
                             xAxisLab = parts, yAxisLab = leftLabel, manualDataYaxis = dataset[measurements], manualXaxis = unique(dataset[[parts]]), title = title, smallLabels = smallLabels, OnlyOutofLimit = TRUE)$p
      }else if(i == nOperators){
        p1 <- .XbarchartNoId(dataset = dataPerOP[measurements], options = options, manualLimits = manualLimits, GaugeRR = TRUE,
                             warningLimits = FALSE, manualSubgroups = manualSubgroups, yAxis = FALSE, xAxisLab = parts, manualDataYaxis = dataset[measurements], manualXaxis = unique(dataset[[parts]]), title = title,
                             smallLabels = smallLabels, OnlyOutofLimit = TRUE)$p
      }else{
        p1 <- .XbarchartNoId(dataset = dataPerOP[measurements], options = options, manualLimits = manualLimits, GaugeRR = TRUE,
                             warningLimits = FALSE, manualSubgroups = manualSubgroups, yAxis = FALSE, plotLimitLabels = FALSE,
                             xAxisLab = parts, manualDataYaxis = dataset[measurements], manualXaxis = unique(dataset[[parts]]), title = title,
                             smallLabels = smallLabels, OnlyOutofLimit = TRUE)$p
      }
    }
    plotMat[[i]] <- p1

  }
  p2 <- cowplot::plot_grid(plotlist = plotMat, ncol = nOperators, nrow = 1)

  return(p2)
}

.gaugeByPartGraph <- function(dataset, measurements, parts, operators, options) {
  plot <- createJaspPlot(title = gettext("Measurements by Part"), width = 700, height = 300)
  plot$dependOn('gaugeByPart')
  p <- .gaugeByPartGraphPlotObject(dataset, measurements, parts, operators, displayAll = options$gaugeByPartAll)
  plot$plotObject <- p
  options$gaugeByPartAll
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

  plot <- createJaspPlot(title = gettext("Measurement by Operator"), width = 500, height = 500)

  plot$dependOn(c("gaugeByOperator", "gaugeRRmethod"))

  if (ready) {
    plot$plotObject <- .gaugeByOperatorGraphPlotObject(dataset, measurements, parts, operators, options, Type3)
  }
  return(plot)
}

.gaugeByOperatorGraphPlotObject <- function(dataset, measurements, parts, operators, options, Type3){
  dataset <- tidyr::gather(dataset, Repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)
  yBreaks <- dataset["Measurement"]
  yLimits <- range(yBreaks)
  p <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = dataset, ggplot2::aes_string(x = operators, y = "Measurement"))  +
    ggplot2::scale_y_continuous(limits = yLimits)
  p <- jaspGraphs::themeJasp(p)

  if (Type3)
    p <- p + ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(name = NULL)

  return(p)
}

.gaugeByInteractionGraph <- function(dataset, measurements, parts, operators, options, ready, Type3 = FALSE) {

  plot <- createJaspPlot(title = gettext("Parts by Operator Interaction"), width = 700, height = 400)
  plot$dependOn(c("gaugeByInteraction", "gaugeRRmethod"))

  if (ready) {
    plot$plotObject <- .gaugeByInteractionGraphPlotFunction(dataset, measurements, parts, operators, options, Type3 = Type3)
  }
  return(plot)
}

.gaugeByInteractionGraphPlotFunction <- function(dataset, measurements, parts, operators, options, Type3 = FALSE) {
  byOperator <- split.data.frame(dataset, dataset[operators])
  partNames <- unique(dataset[[parts]])

  for (name in names(byOperator)) {
    if (nrow(byOperator[[name]][measurements]) != length(partNames)) {
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
    ggplot2::scale_y_continuous(name = "Measurement", breaks = yBreaks, limits = yLimits) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  if (!Type3)
    p <- p + ggplot2::theme(legend.position = 'right')

  return(p)
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
      matrixPlot$dependOn('gaugeScatterPlotOperators')
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

.gaugeVarCompGraph <- function(percentContributionValues, studyVariationValues, percentToleranceValues, Type3) {
  sources <- gettext(c('Gauge r&R', 'Repeat', 'Reprod', 'Part-to-Part'))
  if (!is.na(percentToleranceValues)){
    references <- gettext(c('% Contribution', '% Study Variation', '% Tolerance'))
    values <- c(percentContributionValues, studyVariationValues, percentToleranceValues)
  }else{
    references <- gettext(c('% Contribution', '% Study Variation'))
    values <- c(percentContributionValues, studyVariationValues)
  }
  plotframe <- data.frame(source = rep(sources, length(references)),
                          reference = rep(references, each = 4),
                          value = values)
  plotframe$source <- factor(plotframe$source, levels = c('Gauge r&R', 'Repeat', 'Reprod', 'Part-to-Part'))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotframe$value)


  if (Type3)
    plotframe <- subset(plotframe, source != "Reprod")

  p <- ggplot2::ggplot() + ggplot2::geom_bar(data = plotframe,
                                             mapping = ggplot2::aes(fill =  reference,  y = value, x = source),
                                             position="dodge", stat = "identity")
  p <- jaspGraphs::themeJasp(p) + ggplot2::theme(legend.position = 'right', legend.title = ggplot2::element_blank()) +
    ggplot2::xlab("") + ggplot2::scale_y_continuous(name = "Percent", breaks = yBreaks, limits = range(c(yBreaks, plotframe$value)))
  return(p)

}

.gaugeNumberDistinctCategories <- function(sdPart, sdGauge) {
  nCategories <- (sdPart / sdGauge) * 1.41
  if (nCategories < 1)
    nCategories <- 1
  return(as.integer(nCategories))
}


.anovaGaugeReport <- function(dataset, measurements, parts, operators, options, Type3 = FALSE){

  if (options[["anovaGaugeTitle"]] == ""){
    title <- "Gauge r&R Report"
  }else{
    title <- options[["anovaGaugeTitle"]]
  }
  name <- gettextf("Gauge name: %s", options[["anovaGaugeName"]])
  date <- gettextf("Date of study: %s", options[["anovaGaugeDate"]])
  text1 <- c(name, date)

  reportedBy <- gettextf("Reported by: %s", options[["anovaGaugeReportedBy"]])
  misc <- gettextf("Misc: %s", options[["anovaGaugeMisc"]])
  if (options[["gaugeToleranceEnabled"]]){
    tolerance <- gettextf("Tolerance: %s", options[["tolerance"]])
    text2 <- c(reportedBy, tolerance, misc)
  }else{
    text2 <- c(reportedBy, misc)
  }

  matrixPlot <- createJaspPlot(title = gettext("Report"), width = 3800, height = 1800)
  plotMat <- matrix(list(), 4, 2)
  plotMat[[1, 1]] <- .ggplotWithText(text1)
  plotMat[[1, 2]] <- .ggplotWithText(text2)
  plotMat[[2, 1]] <- .gaugeANOVA(dataset, measurements, parts, operators, options, ready = TRUE, returnPlotOnly = TRUE, Type3 = Type3)
  plotMat[[2, 2]] <- .gaugeByPartGraphPlotObject(dataset, measurements, parts, operators, displayAll = FALSE)
  plotMat[[3, 1]] <- .xBarOrRangeChartPlotFunction("Range", dataset, measurements, parts, operators, options, smallLabels = TRUE, Type3 = Type3)
  plotMat[[3, 2]] <- .gaugeByOperatorGraphPlotObject(dataset, measurements, parts, operators, options, Type3 = Type3)
  plotMat[[4, 1]] <- .xBarOrRangeChartPlotFunction("Average", dataset, measurements, parts, operators, options, smallLabels = TRUE, Type3 = Type3)
  plotMat[[4, 2]] <- .gaugeByInteractionGraphPlotFunction(dataset, measurements, parts, operators, options, Type3 = Type3)


  p <- jaspGraphs::ggMatrixPlot(plotMat)
  matrixPlot$plotObject <- p

  return(matrixPlot)
}

.ggplotWithText <- function(text){
  nText <- length(text)
  annotation <- data.frame(x = rep(1, nText), y = nText:1, label = text)
  p <- ggplot2::ggplot() + ggplot2::theme_void() +
    ggplot2::geom_text(data=annotation, ggplot2::aes(x = x, y = y, label = label), size = 5) +
    ggplot2::scale_y_continuous(breaks = 0:nText, limits = c(0, nText))
  return(p)
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
  }else{
    operator <- (msOperator - msRep) / (nParts * nReplicates)
    part <- (msPart - msRep) / (nOperators * nReplicates)
    reprod <- operator
  }
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
  part <- (msPart - msRep) / (nOperators * nReplicates)
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
.trafficplot <- function(StudyVar = "", ToleranceUsed = FALSE,ToleranceVar = "", options, ready, horizontal = FALSE, Xlab.StudySD = "", Xlab.Tol = ""){

  if (!ready)
    return()

  Plot <- createJaspPlot(width = 1000)

  mat <- data.frame(
    x = rep(c(70,20,10),2),
    Yes = c(rep('A',3), rep("B",3)),
    fill = rep(c("G","R","Y"),2)
  )

  if (StudyVar >= 100) {StudyVar = 100}
  if (ToleranceVar >= 100) {ToleranceVar = 100}

  p1 <- ggplot2::ggplot(mat[c(1:3),], ggplot2::aes(x = x, y = Yes, fill = fill)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values= rev(c('#008450','#EFB700', '#B81D13'))) +
    ggplot2::geom_vline(xintercept = StudyVar) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(fontsize = jaspGraphs::setGraphOption("fontsize", 15)) +
    ggplot2::scale_y_discrete(name = NULL)

  if (Xlab.StudySD != "")
    p1 <- p1 + ggplot2::scale_x_continuous(name = gettext(Xlab.StudySD))


  if (ToleranceUsed){
    p2 <- ggplot2::ggplot(mat[c(4:6),], ggplot2::aes(x = x, y = Yes, fill = fill)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values= rev(c('#008450','#EFB700', '#B81D13')))+
      ggplot2::geom_vline(xintercept = ToleranceVar) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(fontsize = jaspGraphs::setGraphOption("fontsize", 15))
    if (Xlab.Tol != "")
      p2 <- p2 + ggplot2::scale_x_continuous(name = gettext(Xlab.Tol))
  }

  if (horizontal) {
    p1 <- p1 +
      ggplot2::theme(legend.position="none",
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank()) +
      ggplot2::coord_flip() +
      ggplot2::scale_x_continuous(breaks = c(0,10,30,100), labels = c("0%","10%","30%","100%"), name = "")

    if (ToleranceUsed == TRUE){
      p2 <- p2 +
        ggplot2::theme(legend.position="none",
                       axis.text.x = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank()) +
        ggplot2::coord_flip() +
        ggplot2::scale_x_continuous(breaks = c(0,10,30,100), labels = c("0%","10%","30%","100%"), name = "")
    }

    Plot <- createJaspPlot(width = 250, height = 600)
  } else{
    p1 <- p1 +
      ggplot2::theme(legend.position="none",
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank()) +
      ggplot2::scale_x_continuous(breaks = c(0,10,30,100), labels = c("0%","10%","30%","100%"), name = "Percent of Process variation")

    if (ToleranceUsed == TRUE){
      p2 <- p2 +
        ggplot2::theme(legend.position="none",
                       axis.text.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(breaks = c(0,10,30,100), labels = c("0%","10%","30%","100%"), name = "Percent of the Tolerance")
    }
  }

  if (ToleranceUsed == TRUE){
    p3 <- jaspGraphs::ggMatrixPlot(plotList = list(p1, p2), layout = matrix(2:1, ifelse(horizontal, 1,2)))
    Plot$plotObject <- p3
  } else {
    Plot$plotObject <- p1
  }
  return(Plot)
}
