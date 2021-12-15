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

msaGaugeRRnonrep <- function(jaspResults, dataset, options, ...) {

  if(options[["gaugeRRNonRepDataFormat"]] == "gaugeRRNonRepLongFormat"){
  measurements <- unlist(options$measurements)
  }else{
    measurements <- unlist(options$measurementsWide)
  }
  parts <- unlist(options$parts)
  operators <- unlist(options$operators)
  numeric.vars <- measurements
  numeric.vars <- numeric.vars[numeric.vars != ""]
  factor.vars <- c(parts, operators)
  factor.vars <- factor.vars[factor.vars != ""]

  if(options[["gaugeRRNonRepDataFormat"]] == "gaugeRRNonRepLongFormat"){
    ready <- (measurements != "" && operators != "" && parts != "")
  }else{
    ready <- (length(measurements) > 1 && operators != "" && parts != "")
  }
  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = numeric.vars, columns.as.factor = factor.vars,
                                         exclude.na.listwise = c(numeric.vars, factor.vars))
  }

  if (ready && nrow(dataset[measurements]) == 0){
    jaspResults[["plot"]] <- createJaspPlot(title = gettext("Gauge r&R"), width = 700, height = 400)
    jaspResults[["plot"]]$setError(gettextf("No valid measurements in %s.", measurements))
    jaspResults[["plot"]]$position <- 1
    jaspResults[["plot"]]$dependOn(c("measurements", "measurementsWide"))
    return()
  }

  if (ready && options[["gaugeRRNonRepDataFormat"]] == "gaugeRRNonRepLongFormat"){
    datasetLong <- dataset
    datasetWide <- .reshapeToWide(dataset, measurements, parts, operators)
    wideMeasurementCols <- colnames(datasetWide)[colnames(datasetWide) != c(parts, operators)]
    longMeasurementCols <- measurements
  }

  if (ready && options[["gaugeRRNonRepDataFormat"]] == "gaugeRRNonRepWideFormat"){
    datasetWide <- dataset
    datasetLong <- .reshapeToLong(dataset, measurements, parts, operators)
    wideMeasurementCols <- measurements
    longMeasurementCols <- "Measurement"
  }

  # Gauge r&R non replicable
  if (options[["NRgaugeRR"]]) {
    if (is.null(jaspResults[["gaugeRRNonRep"]])) {
      jaspResults[["gaugeRRNonRep"]] <- createJaspContainer(gettext("Gauge r&R Tables"))
      jaspResults[["gaugeRRNonRep"]]$position <- 1
    }
    jaspResults[["gaugeRRNonRep"]] <- .gaugeRRNonRep(dataset = datasetLong, measurements = longMeasurementCols, parts = parts, operators = operators, options =  options, ready = ready)
    jaspResults[["gaugeRRNonRep"]]$dependOn(c("NRstandardDeviationReference", "NRhistoricalStandardDeviationValue", "NRtolerance", "NRstudyVarMultiplierType",
                                              "NRstudyVarMultiplier", "NRgaugeVarCompGraph"))
  }

  # R chart by operator
  if (options[["NRrCharts"]]) {
    if (is.null(jaspResults[["NRrCharts"]])) {
      jaspResults[["NRrCharts"]] <- createJaspContainer(gettext("Range Chart by Operator"))
      jaspResults[["NRrCharts"]]$position <- 2
    }
    jaspResults[["NRrCharts"]] <- .xBarOrRangeChart(type = "Range", dataset = datasetWide, measurements = wideMeasurementCols, parts = parts, operators = operators, options =  options, ready = ready)
    jaspResults[["NRrCharts"]]$dependOn("NRrCharts")
  }

  # Xbar chart by operator
  if (options[["NRxbarCharts"]]) {
    if (is.null(jaspResults[["NRxbarCharts"]])) {
      jaspResults[["NRxbarCharts"]] <- createJaspContainer(gettext("Xbar Chart by Operator"))
      jaspResults[["NRxbarCharts"]]$position <- 3
    }
    jaspResults[["NRxbarCharts"]] <- .xBarOrRangeChart(type = "Xbar", dataset = datasetWide, measurements = wideMeasurementCols, parts = parts, operators = operators, options =  options, ready = ready)
    jaspResults[["NRxbarCharts"]]$dependOn("NRxbarCharts")
  }

  #Measurement by part x operator plot
  if (options[["NRpartOperatorGraph"]]) {
    if (is.null(jaspResults[["NRpartOperatorGraph"]])) {
      jaspResults[["NRpartOperatorGraph"]] <- createJaspContainer(gettext("Measurement by part x operator plot"))
      jaspResults[["NRpartOperatorGraph"]]$position <- 4
    }
    jaspResults[["NRpartOperatorGraph"]] <- .gaugeMeasurmentsByPartXOperator(dataset = datasetWide, measurements = wideMeasurementCols, parts = parts, operators = operators, options = options)
    jaspResults[["NRpartOperatorGraph"]]$dependOn(c("NRpartOperatorGraph", "NRpartOperatorGraphAll"))
  }

  #Measurement by operator plot
  if (options[["NRoperatorGraph"]]) {
    if (is.null(jaspResults[["NRoperatorGraph"]])) {
      jaspResults[["NRoperatorGraph"]] <- createJaspContainer(gettext("Gauge r&R Tables"))
      jaspResults[["NRoperatorGraph"]]$position <- 5
    }

    jaspResults[["NRoperatorGraph"]] <- .gaugeByOperatorGraph(dataset = datasetWide, measurements = wideMeasurementCols, parts = parts, operators = operators, options = options, ready = ready)
    jaspResults[["NRoperatorGraph"]]$dependOn(c("NRoperatorGraph"))
  }

  # Report
  if (options[["anovaGaugeNestedReport"]]) {
    if (is.null(jaspResults[["anovaGaugeNestedReport"]])) {
      jaspResults[["anovaGaugeNestedReport"]] <- createJaspContainer(gettext("Report"))
      jaspResults[["anovaGaugeNestedReport"]]$position <- 6
    }
    jaspResults[["anovaGaugeNestedReport"]] <- .anovaGaugeNestedReport(datasetWide, datasetLong, wideMeasurementCols, longMeasurementCols, parts = parts, operators = operators, options = options)
  }

  return()
}

.gaugeRRNonRep <- function(dataset, measurements, parts, operators, options, ready, plotOnly = F) {
  gaugeRRNonRepTables <- createJaspContainer(gettext("Gauge r&R Tables"))
  gaugeRRNonRepTables$position <- 1

  gaugeRRNonRepTable1 <- createJaspTable(title = gettext("Gauge r&R (Nested)"))
  gaugeRRNonRepTable1$addColumnInfo(title = gettext("Source"),       name = "sources",   type = "string" )
  gaugeRRNonRepTable1$addColumnInfo(title = gettext("df"),             name = "DF",      type = "integer")
  gaugeRRNonRepTable1$addColumnInfo(title = gettext("Sum of Squares"), name = "SS",  type = "number")
  gaugeRRNonRepTable1$addColumnInfo(title = gettext("Mean Square"),    name = "MS", type = "number")
  gaugeRRNonRepTable1$addColumnInfo(title = gettext("F"),              name = "F.value", type = "number")
  gaugeRRNonRepTable1$addColumnInfo(title = gettext("p"),              name = "p.value",  type = "pvalue")

  gaugeRRNonRepTable2 <- createJaspTable(title = gettext("Variance Components"))
  gaugeRRNonRepTable2$addColumnInfo(title = gettext("Source"),       name = "sources",   type = "string" )
  gaugeRRNonRepTable2$addColumnInfo(title = gettext("Variation"), name = "Var",  type = "number")
  gaugeRRNonRepTable2$addColumnInfo(title = gettext("% Contribution"),    name = "percentVar", type = "number")

  gaugeRRNonRepTable3 <- createJaspTable(title = gettext("Gauge Evaluation"))
  gaugeRRNonRepTable3$addColumnInfo(name = "source", title = gettext("Source"), type = "string")
  gaugeRRNonRepTable3$addColumnInfo(name = "SD", title = gettext("Std. Deviation"), type = "number")
  gaugeRRNonRepTable3$addColumnInfo(name = "studyVar", title = gettextf("Study Variation"), type = "number")
  gaugeRRNonRepTable3$addColumnInfo(name = "percentStudyVar", title = gettext("% Study Variation"), type = "number")
  if(options[["gaugeNRToleranceEnabled"]])
    gaugeRRNonRepTable3$addColumnInfo(name = "percentTolerance", title = gettext("% Tolerance"), type = "number")

  if (ready) {
    anovaTable <- .gaugeNestedANOVA(dataset, operators, parts, measurements)
    anovaSources <- as.character(anovaTable$Source)
    df <- anovaTable$DF
    ss <- anovaTable$SS
    ms <- anovaTable$MS
    f <- anovaTable$F.value
    p <- anovaTable$p.value
    gaugeRRNonRepTable1$setData(list("sources" = anovaSources,
                                     "DF" = df,
                                     "SS" = ss,
                                     "MS" = ms,
                                     "F.value" = f,
                                     "p.value" = p))

    varCompTable <- .gaugeNestedVarComponents(dataset, operators, parts, measurements, ms)
    varSources <- gettext(c("Total Gauge r & R", "Repeatability", "Reproducibility", "Part-To-Part", "Total Variation"))
    varComps <- c(varCompTable$varGaugeTotal, varCompTable$varRepeat, varCompTable$varReprod, varCompTable$varPart, varCompTable$varTotal)
    percentVarComps <- (varComps / varCompTable$varTotal) * 100
    gaugeRRNonRepTable2$setData(list("sources" = varSources,
                                     "Var" = varComps,
                                     "percentVar" = percentVarComps))
    histSD <- options$NRhistoricalStandardDeviationValue
    if (options$NRstandardDeviationReference == "historicalStandardDeviation" && histSD >= sqrt(varCompTable$varGaugeTotal)) {
      stdDevs <- c(sqrt(c(varCompTable$varGaugeTotal, varCompTable$varRepeat, varCompTable$varReprod)),
                   sqrt(histSD^2 - varCompTable$varGaugeTotal), histSD)
    }else{
      stdDevs <- sqrt(varComps)
    }
    if (options$NRstudyVarMultiplierType == "svmSD") {
      studyVarMultiplier <- options$NRstudyVarMultiplier
    }else{
      percent <- options$NRstudyVarMultiplier/100
      q <- (1 - percent)/2
      studyVarMultiplier <- abs(2*qnorm(q))
    }
    studyVar <- stdDevs * studyVarMultiplier
    percentStudyVar <- studyVar/max(studyVar) * 100
    gaugeRRNonRepTable3DataList <- list("source" = varSources,
                                        "SD" = stdDevs,
                                        "studyVar" = studyVar,
                                        "percentStudyVar" = percentStudyVar)
    if(options[["gaugeNRToleranceEnabled"]]){
      percentTolerance <- studyVar / options$NRtolerance * 100
      gaugeRRNonRepTable3DataList <- append(gaugeRRNonRepTable3DataList, list("percentTolerance" = percentTolerance))
    }
    gaugeRRNonRepTable3$setData(gaugeRRNonRepTable3DataList)
    if (as.integer(studyVarMultiplier) == round(studyVarMultiplier, 2)){
      gaugeRRNonRepTable3$addFootnote(gettextf("Study Variation is calculated as Std. Deviation * %i", as.integer(studyVarMultiplier)))
    }else{
      gaugeRRNonRepTable3$addFootnote(gettextf("Study Variation is calculated as Std. Deviation * %.2f", studyVarMultiplier))
    }
    nCategories <- .gaugeNumberDistinctCategories(stdDevs[4], stdDevs[1])
    gaugeRRNonRepTable3$addFootnote(gettextf("Number of distinct categories = %i", nCategories))
  }
  gaugeRRNonRepTables[["Table1"]] <- gaugeRRNonRepTable1
  gaugeRRNonRepTables[["Table2"]] <- gaugeRRNonRepTable2
  gaugeRRNonRepTables[["Table3"]] <- gaugeRRNonRepTable3

  if (options[["NRgaugeVarCompGraph"]]) {
    plot <- createJaspPlot(title = gettext("Components of Variation"), width = 850, height = 500)
    plot$dependOn(c("NRgaugeVarCompGraph"))
    if (ready){
      if(options[["gaugeNRToleranceEnabled"]]){
        percentToleranceForGraph <- percentTolerance[1:4]
      }else{
        percentToleranceForGraph <- NA
      }
      p <- .gaugeVarCompGraph(percentVarComps[1:4], percentStudyVar[1:4], percentToleranceForGraph)
      plot$plotObject <- p
    }
    if(plotOnly)
      return(p)
    gaugeRRNonRepTables[["Plot"]] <- plot
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

  ss <- .ssGaugeNested(dataset, operators, parts, measurements)
  df <- .dfGaugeNested(dataset, operators, parts, measurements)
  ms <- .msGaugeNested(ss, df)
  f <- .fGaugeNested(ms)
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
  nReplicates <- as.vector(table(dataset[parts])[1])
  nParts  <- as.vector(table(dataset[operators])[1] / nReplicates)
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
  nreplicates       <- as.vector(table(dataset[parts])[1])
  nparts <- length(unique(dataset[[parts]]))
  dataset <- dataset[order(dataset[parts]),]
  dataset <- cbind(dataset, data.frame(index = rep(1:nreplicates, nparts)))
  dataset <- tidyr::spread(dataset, index, measurements)
  measurementColNames <- paste("M", 1:nreplicates, sep = "")
  colnames(dataset) <- c(parts, operators, measurementColNames)
  return(dataset)
}

.gaugeMeasurmentsByPartXOperator <- function(dataset, measurements, parts, operators, options) {
  plotContainer <- createJaspContainer(gettext("Measurements by part x operator"))
  operatorVector <- unique(dataset[[operators]])
  for (op in operatorVector) {
    dataPerOP <- subset(dataset, dataset[operators] == op)
    plot <- createJaspPlot(title = gettextf("Operator %s", op), width = 600, height = 300)
    plot$plotObject <- .gaugeByPartGraphPlotObject(dataset = dataPerOP, measurements = measurements, parts = parts, operators = operators, displayAll = options[["NRpartOperatorGraphAll"]])
    plotContainer[[op]] <- plot
  }
  return(plotContainer)
}

 .reshapeToLong <- function(dataset, measurements, parts, operators){
   longData <- tidyr::gather(dataset, repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)
   longData <- longData[c(parts, operators, "Measurement" )]
   return(longData)
 }

 .anovaGaugeNestedReport <- function(datasetWide, datasetLong, measurementsWide, measurementsLong, parts, operators, options){

   if (options[["anovaGaugeNestedTitle"]] == ""){
     title <- "Gauge r&R Report"
   }else{
     title <- options[["anovaGaugeNestedTitle"]]
   }
   name <- gettextf("Gauge name: %s", options[["anovaGaugeNestedName"]])
   date <- gettextf("Date of study: %s", options[["anovaGaugeNestedDate"]])
   text1 <- c(name, date)

   reportedBy <- gettextf("Reported by: %s", options[["anovaGaugeNestedReportedBy"]])
   misc <- gettextf("Misc: %s", options[["anovaGaugeNestedMisc"]])
   if (options[["gaugeNRToleranceEnabled"]]){
     tolerance <- gettextf("Tolerance: %s", options[["NRtolerance"]])
     text2 <- c(reportedBy, tolerance, misc)
   }else{
     text2 <- c(reportedBy, misc)
   }

   matrixPlot <- createJaspPlot(title = gettext("Report"), width = 1200, height = 1000)
   plotMat <- matrix(list(), 4, 2)
   plotMat[[1, 1]] <- .ggplotWithText(text1)
   plotMat[[1, 2]] <- .ggplotWithText(text2)
   plotMat[[2, 1]] <- .gaugeRRNonRep(datasetLong, measurementsLong, parts = parts, operators = operators, options =  options, ready = TRUE, plotOnly = TRUE)
   plotMat[[2, 2]] <- ggplot2::ggplot() + ggplot2::theme_void()
   plotMat[[3, 1]] <- .xBarOrRangeChartPlotFunction("Range", datasetWide, measurementsWide, parts, operators, options, smallLabels = TRUE)
   plotMat[[3, 2]] <- .gaugeByOperatorGraphPlotObject(datasetWide, measurementsWide, parts, operators, options)
   plotMat[[4, 1]] <- .xBarOrRangeChartPlotFunction("Xbar", datasetWide, measurementsWide, parts, operators, options, smallLabels = TRUE)
   plotMat[[4, 2]] <- ggplot2::ggplot() + ggplot2::theme_void()

   p <- jaspGraphs::ggMatrixPlot(plotMat, topLabels = c(gettext(title), ""))
   matrixPlot$plotObject <- p

   return(matrixPlot)
 }
