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
processCapabilityStudies <- function(jaspResults, dataset, options) {
  if (options[["pcDataFormat"]] == "PCwideFormat"){
    measurements <- unlist(options$variables)
  }else{
    measurements <- unlist(options$variablesLong)
  }
  subgroups <- unlist(options[["subgroup"]])
  num.vars <- measurements[measurements != ""]
  fac.vars <- subgroups[subgroups != ""]
  splitName <- options[["subgroup"]]
  makeSplit <- splitName != ""

  dataset <- .readDataSetToEnd(columns.as.numeric = num.vars, columns.as.factor = fac.vars)

  # Check if the analysis is ready
  wideFormat <- options[["pcDataFormat"]] == "PCwideFormat"
  if (wideFormat)
    ready <- length(measurements) > 0
  else
    ready <- (!identical(measurements, "") && (options[["manualSubgroupSize"]] | !identical(subgroups, "")))

  if (makeSplit && ready) {
    dataset.factors <- .readDataSetToEnd(columns=num.vars, columns.as.factor=splitName)
    splitFactor      <- dataset[[.v(splitName)]]
    splitLevels      <- levels(splitFactor)
    # remove missing values from the grouping variable
    dataset <- dataset[!is.na(splitFactor), ]
    dataset.factors <- dataset.factors[!is.na(splitFactor), ]

    numberMissingSplitBy <- sum(is.na(splitFactor))

    # Actually remove missing values from the split factor
    splitFactor <- na.omit(splitFactor)
  } else {
    splitFactor <- ""
  }

  if (options[["pcDataFormat"]] == "PClongFormat" && ready){
    if(options[["manualSubgroupSize"]]){
      k <- options[["pcSubgroupSize"]]
      n <- nrow(dataset)
      dataset <- .PClongTowide(dataset, k, measurements, mode = "manual")
      if (identical(dataset, "error")) {
        plot <- createJaspPlot(title = gettext("Capability of the process"), width = 700, height = 400)
        jaspResults[["plot"]] <- plot
        plot$setError(gettextf("Could not equally divide %1$i data points into groups of size %2$i.", n, k))
        return()
      }
      measurements <- colnames(dataset)
      subgroups <- ""
    }else{
      k <- subgroups
      dataset <- .PClongTowide(dataset, k, measurements, mode = "subgroups")
      measurements <- colnames(dataset)
      measurements <- measurements[measurements != k]
    }
  }

  # Error Handling
  .hasErrors(dataset, type = c('infinity', 'missingValues'),
             all.target = measurements, exitAnalysisIfErrors = TRUE)
  if (options[["capabilityStudyType"]] == "nonnormalCapabilityAnalysis" && ready) {
    .hasErrors(dataset,
               all.target = measurements,
               custom = function () {
                 if (any(unlist(dataset[measurements]) < 0))
                   return(gettext("Values must be positive to fit a Weibull/Lognormal distribution."))},
               exitAnalysisIfErrors = TRUE)
  }

  dataset <- na.omit(dataset)
  # correction for zero values for non-normal capability
  if (options$capabilityStudyType == "nonnormalCapabilityAnalysis" && ready) {
    x <- unlist(dataset[measurements])
    zeroCorrect <- any(x == 0)
    dataset[measurements] <- ifelse(x == 0, min(x[x > 0])/2 , x)

    if (zeroCorrect) {
      jaspResults[["zeroWarning"]] <- createJaspHtml(text = gettext("All zero values have been replaced with a value equal to one-half of the smallest data point."), elementType = "p",
                                                     title = "Zero values found in non-normal capability study:",
                                                     position = 1)
      jaspResults[["zeroWarning"]]$dependOn(c('variablesLong', 'variables', 'capabilityStudyType', 'nullDistribution'))
    }
  }

  # Report
  if (options[["pcReportDisplay"]]) {
    if (is.null(jaspResults[["pcReport"]])) {
      jaspResults[["pcReport"]] <- createJaspContainer(gettext("Report"))
      jaspResults[["pcReport"]]$position <- 6
    }
    jaspResults[["pcReport"]] <- .pcReport(dataset, measurements, parts, operators, options, ready, jaspResults, splitFactor, wideFormat)
    jaspResults[["pcReport"]]$dependOn(c('pcReportDisplay'))
  } else {

    # X-bar and R Chart OR ImR Chart
    if(options$xbarR){
      .qcXbarAndRContainer(options, dataset, ready, jaspResults, measurements = measurements, subgroups = splitFactor, wideFormat = wideFormat)
    } else if(options[["IMR"]]){
      .qcImRChart(options, dataset, ready, jaspResults, measurements, subgroups = splitFactor, wideFormat = wideFormat)
    }

    # Distribution plot - moved jaspResults ref here to avoid big files
    .qcDistributionPlot(options, dataset, ready, jaspResults, measurements = measurements)

    # Probability plots section
    .qcProbabilityPlotContainer(options, dataset, ready, jaspResults, measurements = measurements)

    # Perform capability analysis
    .qcCapabilityAnalysis(options, dataset, ready, jaspResults, measurements = measurements)
  }
}

#############################################################
## Functions for capability analysis section ################
#############################################################

################
## Containers ##
################

.qcCapabilityAnalysis <- function(options, dataset, ready, jaspResults, measurements) {

  container <- createJaspContainer(gettext("Capability Studies"))
  container$dependOn(options = c("CapabilityStudyType", "variables", "subgroup", "lowerSpecification", "upperSpecification", "targetValue", "variablesLong", "pcSubgroupSize", "pcDataFormat",
                                 "CapabilityStudyPlot", "CapabilityStudyTables", "manualSubgroupSize", "pcReportDisplay"))
  container$position <- 4

  if (!ready)
    return(container)


  if( sum(options[["upperSpecificationField"]], options[["lowerSpecificationField"]], options[["targetValueField"]]) >= 2){
    if (options[["lowerSpecificationField"]]){
      lowSpec <- options[["lowerSpecification"]]
      if (options[["upperSpecificationField"]] && options[["upperSpecification"]] < lowSpec){
        container$setError(gettext("Error: LSL > USL."))
      }
    }
    if (options[["targetValueField"]]){
      target <- options[["targetValue"]]
      if ((options[["upperSpecificationField"]] && options[["upperSpecification"]] < target) || (options[["lowerSpecificationField"]] && options[["lowerSpecification"]] > target)){
        container$setError(gettext("Error: Target outside of specification limits."))
      }
    }
  }




  ready <- (length(measurements) > 0 && (options[["lowerSpecificationField"]] | options[["upperSpecificationField"]]))

  jaspResults[["capabilityAnalysis"]] <- container

  if (options[["capabilityStudyType"]] == "normalCapabilityAnalysis") {

    normalContainer <- createJaspContainer(gettext("Process Capability"))
    normalContainer$position <- 1
    container[["normalCapabilityAnalysis"]] <- normalContainer

    .qcProcessSummaryTable(options, dataset, ready, normalContainer, measurements)

    if (options[["CapabilityStudyPlot"]])
      .qcProcessCapabilityPlot(options, dataset, ready, normalContainer, measurements, distribution = 'normal')

    if (options[["CapabilityStudyTables"]]){
      .qcProcessCapabilityTableWithin(options, dataset, ready, normalContainer, measurements)
      .qcProcessCapabilityTableOverall(options, dataset, ready, normalContainer, measurements)
    }

  }

  if (options[["capabilityStudyType"]] == "nonnormalCapabilityAnalysis") {

    nonNormalContainer <- createJaspContainer(gettext("Process Capability (Non-normal Capability Analysis)"))
    nonNormalContainer$position <- 2

    container[["nonNormalCapabilityAnalysis"]] <- nonNormalContainer

    if (options[["CapabilityStudyPlot"]])
      .qcProcessCapabilityPlot(options, dataset, ready, nonNormalContainer, measurements, distribution = options[["nonNormalDist"]])

    if (options[["CapabilityStudyTables"]])
      .qcProcessCapabilityTableNonNormal(options, dataset, ready, nonNormalContainer, measurements)
  }
}

################
## Output ######
################

.qcProcessSummaryTable <- function(options, dataset, ready, container, measurements, returnDataframe = FALSE) {

  table <- createJaspTable(title = gettext("Process summary"))
  table$position <- 1

  if (options[["lowerSpecificationField"]]){
    table$addColumnInfo(name = "lsl", type = "number", title = gettext("LSL"))
  }
  if (options[["targetValueField"]])
    table$addColumnInfo(name = "target", type = "number", title = gettext("Target"))
  if (options[["upperSpecificationField"]])
    table$addColumnInfo(name = "usl", type = "number", title = gettext("USL"))

  table$addColumnInfo(name = "n", type = "integer", title = gettext("Sample size"))
  table$addColumnInfo(name = "mean", type = "number", title = gettext("Average"))
  table$addColumnInfo(name = "sd", type = "number", title = gettext("Std. deviation (total)"))
  table$addColumnInfo(name = "sdw", type = "number", title = gettext("Std. deviation (within)"))

  table$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  # Take a look at this input! Is is supposed to be like this or must it be transposed?
  # Transposed gives NA often as std.dev
  if(length(measurements) < 2){
    type <- 'xbar.one'
  }else{
    type <- 'R'
  }
  qccFit <- qcc::qcc(as.data.frame(dataset[, measurements]), type = type, plot = FALSE)
  allData <- unlist(dataset[, measurements])

  if (is.na(qccFit[["std.dev"]]))
    table$addFootnote(gettext("The within standard deviation could not be calculated."))

  rows <- list(
    "lsl"    = options[["lowerSpecification"]],
    "target" = options[["targetValue"]],
    "usl"    = options[["upperSpecification"]],
    "mean"   = mean(allData, na.rm = TRUE),
    "n"      = length(allData),
    "sd"     = sd(allData, na.rm = TRUE),
    "sdw"    = qccFit[["std.dev"]]
  )
  table$addRows(rows)

  nDecimals <- max(.decimalplaces(dataset[measurements]))

  if(returnDataframe){
    sourceVector <- c('LSL', 'Target', 'USL', 'Sample size', 'Mean', "Std. Deviation (Total)", "Std. Deviation (Within)")
    lsl <- options[["lowerSpecification"]]
    target <- options[["targetValue"]]
    usl <- options[["upperSpecification"]]
    if (!options[["lowerSpecificationField"]])
      lsl <- '*'
    if (!options[["targetValueField"]])
      target <- '*'
    if (!options[["upperSpecificationField"]])
      usl <- '*'
    mean <- mean(allData, na.rm = TRUE)
    n <- as.integer(length(allData))
    sd <- sd(allData, na.rm = TRUE)
    sdw <- qccFit[["std.dev"]]
    valueVector <- c(lsl, target, usl, n, mean, sd, sdw)
    df <- data.frame(sources = sourceVector,
                     values = round(as.numeric(valueVector), nDecimals))
    return(df)
  }
  container[["processSummaryTable"]] <- table
}

.qcProcessCapabilityPlotObject <- function(options, dataset, measurements, distribution = c('normal', 'Weibull', 'Lognormal', '3lognormal', '3weibull')) {

  # Take a look at this input! Is is supposed to be like this or must it be transposed?
  # Transposed gives NA often as std.dev
  if(length(measurements) < 2){
    type <- 'xbar.one'
  }else{
    type <- 'R'
  }
  qccFit <- qcc::qcc(as.data.frame(dataset[, measurements]), type = type, plot = FALSE)
  allData <- unlist(dataset[, measurements])
  plotData <- data.frame(x = allData)

  sdw <- qccFit[["std.dev"]]
  sdo <- sd(allData, na.rm = TRUE)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(plotData[["x"]], min(plotData[["x"]]) - 1 * sdo, max(plotData[["x"]]) + 1 * sdo), min.n = 4)
  xLimits <- range(xBreaks)
  binWidthType <- options$csNumberOfBins

  # } else if (binWidthType == "fd" && nclass.FD(variable) > 10000) { # FD-method will produce extreme number of bins and crash ggplot, mention this in footnote
  #   binWidthType <- 10000

  h <- hist(allData, plot = F, breaks = binWidthType)
  binWidth <- (h$breaks[2] - h$breaks[1])

  p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(ggplot2::aes(y =..density..), closed = "left", fill = "grey", col = "black", size = .7, binwidth = binWidth, center = binWidth/2) +
    ggplot2::scale_y_continuous(name = gettext("Density")) +
    ggplot2::scale_x_continuous(name = gettext("Measurement"), breaks = xBreaks, limits = xLimits)
  if(distribution == 'normal'){
    p <- p + ggplot2::stat_function(fun = dnorm, args = list(mean = mean(allData), sd = sd(allData)), color = "dodgerblue") +
      ggplot2::stat_function(fun = dnorm, args = list(mean = mean(allData), sd = sdw), color = "red")
  }else if(distribution == 'Weibull'){
    shape <- .distributionParameters(data = allData, distribution = distribution)$beta
    scale <- .distributionParameters(data = allData, distribution = distribution)$theta
    p <- p + ggplot2::stat_function(fun = dweibull, args = list(shape = shape, scale = scale), color = "red")
  }else if(distribution == 'Lognormal'){
    shape <- .distributionParameters(data = allData, distribution = distribution)$beta
    scale <- .distributionParameters(data = allData, distribution = distribution)$theta
    p <- p + ggplot2::stat_function(fun = dlnorm, args = list(meanlog = shape, sdlog = scale), color = "red")
  }else if(distribution == '3lognormal'){
    shape <- .distributionParameters(data = allData, distribution = distribution)$theta
    scale <- .distributionParameters(data = allData, distribution = distribution)$beta
    threshold <- .distributionParameters(data = allData, distribution = distribution)$threshold
    p <- p + ggplot2::stat_function(fun = FAdist::dlnorm3 , args = list(shape = shape, scale = scale, thres = threshold), color = "red")
  }else if(distribution == '3weibull'){
    shape <- .distributionParameters(data = allData, distribution = distribution)$beta
    scale <- .distributionParameters(data = allData, distribution = distribution)$theta
    threshold <- .distributionParameters(data = allData, distribution = distribution)$threshold
    p <- p + ggplot2::stat_function(fun = FAdist::dweibull3 , args = list(shape = shape, scale = scale, thres = threshold), color = "red")
  }

  if (options[["targetValueField"]])
    p <- p + ggplot2::geom_vline(xintercept = options[["targetValue"]], linetype = "dotted", color = "darkgreen", size = 1)
  if (options[["lowerSpecificationField"]])
    p <- p + ggplot2::geom_vline(xintercept = options[["lowerSpecification"]], linetype = "dotted", color = "darkred", size = 1)
  if (options[["upperSpecificationField"]])
    p <- p + ggplot2::geom_vline(xintercept = options[["upperSpecification"]], linetype = "dotted", color = "darkred", size = 1)

  p <- jaspGraphs::themeJasp(p) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())

  return(p)
}

.qcProcessCapabilityPlot <- function(options, dataset, ready, container, measurements, distribution = c('normal', 'Weibull', 'Lognormal', '3lognormal', '3weibull')) {

  plot <- createJaspPlot(title = gettext("Capability of the process"), width = 700, height = 400)
  plot$dependOn(c("csBinWidthType", "csNumberOfBins"))
  plot$position <- 2


  if(!options[["upperSpecificationField"]] && !options[["lowerSpecificationField"]]){
    plot$setError(gettext("No specification limits set."))
    return()
  }

  if (ready) plot$plotObject <- .qcProcessCapabilityPlotObject(options, dataset, measurements, distribution)

  container[["capabilityPlot"]] <- plot;

  return(plot)
}

.qcProcessCapabilityTableWithin <- function(options, dataset, ready, container, measurements, returnDataframe = FALSE) {

  if (!options[["lowerSpecificationField"]] && !options[["upperSpecificationField"]])
    return()

  if (!ready)
    return()

  table <- createJaspTable(title = gettext("Process capability (within)"))
  sourceVector <- vector()

  ciLevel <- options[["csConfidenceIntervalPercent"]]
  ciLevelPercent <- ciLevel * 100

  if (options[["lowerSpecificationField"]] && options[["upperSpecificationField"]]) {
    table$addColumnInfo(name = "cp", type = "integer", title = gettext("Cp"))
    sourceVector <- c(sourceVector, 'Cp')
    if (options[["csConfidenceInterval"]]){
      table$addColumnInfo(name = "cplci", title = gettext("Lower"), type = "integer", overtitle = gettextf("%s CI for Cp", paste(ciLevelPercent, "%")))
      table$addColumnInfo(name = "cpuci", title = gettext("Upper"), type = "integer", overtitle = gettextf("%s CI for Cp", paste(ciLevelPercent, "%")))
    }
  }
  if (options[["lowerSpecificationField"]]){
    table$addColumnInfo(name = "cpl",   type = "integer", title = gettext("CpL"))
    sourceVector <- c(sourceVector, 'CpL')
  }
  if (options[["upperSpecificationField"]]){
    table$addColumnInfo(name = "cpu",   type = "integer", title = gettext("CpU"))
    sourceVector <- c(sourceVector, 'CpU')
  }

  table$addColumnInfo(name = "cpk",   type = "integer", title = gettext("Cpk"))
  sourceVector <- c(sourceVector, 'Cpk')
  if (options[["csConfidenceInterval"]]){
    table$addColumnInfo(name = "cpklci", title = gettext("Lower"), type = "integer", overtitle = gettextf("%s CI for Cpk", paste(ciLevelPercent, "%")))
    table$addColumnInfo(name = "cpkuci", title = gettext("Upper"), type = "integer", overtitle = gettextf("%s CI for Cpk", paste(ciLevelPercent, "%")))
  }


  table$showSpecifiedColumnsOnly <- TRUE

  # Take a look at this input! Is is supposed to be like this or must it be transposed?
  # Transposed gives NA often as std.dev
  if(length(measurements) < 2){
    type <- 'xbar.one'
  }else{
    type <- 'R'
  }
  qccFit <- qcc::qcc(as.data.frame(dataset[, measurements]), type = type, plot = FALSE)
  allData <- unlist(dataset[, measurements])

  # Calculate capability indices
  usl <- options[["upperSpecification"]]
  lsl <- options[["lowerSpecification"]]
  n <- length(allData)
  k <- length(measurements)
  tolMultiplier <- 6
  cp <- (usl - lsl) / (tolMultiplier * qccFit[["std.dev"]])
  cpl <- (mean(allData) - lsl) / ((tolMultiplier/2) * qccFit[["std.dev"]])
  cpu <- (usl - mean(allData)) / ((tolMultiplier/2) * qccFit[["std.dev"]])
  if (options[["lowerSpecificationField"]] && options[["upperSpecificationField"]]){
    cpk <- min(cpu, cpl)
  }else if(options[["lowerSpecificationField"]] && !options[["upperSpecificationField"]]){
    cpk <- cpl
  }else{
    cpk <- cpu
  }

  rows <- list("cp" = round(cp, 2), "cpl" = round(cpl, 2), "cpu" = round(cpu, 2), "cpk" = round(cpk, 2))

  if (options[["csConfidenceInterval"]]){
    ciAlpha <- 1 - ciLevel

    #CI for Cp
    dfCp <- 0.9 * k * ((n/k) - 1)
    ciLbCp <- cp * sqrt( qchisq(p = ciAlpha/2, df = dfCp) /dfCp)
    ciUbCp <- cp * sqrt( qchisq(p = 1 - (ciAlpha/2), df = dfCp) /dfCp)

    #CI for Cpk
    dfCpk <- 0.9 * k * ((n/k) - 1)
    normCIrange <- qnorm(1 - (ciAlpha / 2))
    intervalCpk <- sqrt(1 / (((tolMultiplier / 2)^2) * n)  +  ((cpk^2)/ (2 * dfCpk)))
    ciLbCpk <- cpk - (normCIrange * intervalCpk)
    ciUbCpk <- cpk + (normCIrange * intervalCpk)

    rows[["cplci"]] <- round(ciLbCp, 2)
    rows[["cpuci"]] <- round(ciUbCp, 2)
    rows[["cpklci"]] <- round(ciLbCpk, 2)
    rows[["cpkuci"]] <- round(ciUbCpk, 2)
  }
  table$addRows(rows)

  if(returnDataframe){
    if(!options[["upperSpecificationField"]])
      cpu <- NA
    if(!options[["lowerSpecificationField"]])
      cpl <- NA
    if(!(options[["lowerSpecificationField"]] && options[["upperSpecificationField"]]))
      cp <- NA
    valueVector <- na.omit(c(cp, cpl, cpu, cpk))
    df <- data.frame(sources = sourceVector,
                     values = round(valueVector,2))
    return(df)
  }

  container[["capabilityTableWithin"]] <- table
}

.qcProcessCapabilityTableOverall <- function(options, dataset, ready, container, measurements, returnOverallCapDataframe = FALSE,
                                             returnPerformanceDataframe = FALSE) {

  if (!ready)
    return()

  table <- createJaspTable(title = gettext("Process performance (total)"))
  sourceVector1 <- vector()

  ciLevel <- options[["csConfidenceIntervalPercent"]]
  ciLevelPercent <- ciLevel * 100
  ciAlpha <- 1 - ciLevel

  if (options[["lowerSpecificationField"]] && options[["upperSpecificationField"]]){
    table$addColumnInfo(name = "pp",  type = "integer", title = gettext("Pp"))
    sourceVector1 <- c(sourceVector1, 'Pp')
    if (options[["csConfidenceInterval"]]){
      table$addColumnInfo(name = "pplci", title = gettext("Lower"), type = "integer", overtitle = gettextf("%s CI for Pp", paste(ciLevelPercent, "%")))
      table$addColumnInfo(name = "ppuci", title = gettext("Upper"), type = "integer", overtitle = gettextf("%s CI for Pp", paste(ciLevelPercent, "%")))
    }
  }
  if (options[["lowerSpecificationField"]]){
    table$addColumnInfo(name = "ppl", type = "integer", title = gettext("PpL"))
    sourceVector1 <- c(sourceVector1, 'PpL')
  }
  if (options[["upperSpecificationField"]]){
    table$addColumnInfo(name = "ppu", type = "integer", title = gettext("PpU"))
    sourceVector1 <- c(sourceVector1, 'PpU')
  }
  table$addColumnInfo(name = "ppk",   type = "integer", title = gettext("Ppk"))
  sourceVector1 <- c(sourceVector1, 'Ppk')
  if (options[["csConfidenceInterval"]]){
    table$addColumnInfo(name = "ppklci", title = gettext("Lower"), type = "integer", overtitle = gettextf("%s CI for Ppk", paste(ciLevelPercent, "%")))
    table$addColumnInfo(name = "ppkuci", title = gettext("Upper"), type = "integer", overtitle = gettextf("%s CI for Ppk", paste(ciLevelPercent, "%")))
  }
  if (options[["targetValueField"]]){
    table$addColumnInfo(name = "cpm", type = "integer", title = gettext("Cpm"))
    sourceVector1 <- c(sourceVector1, 'Cpm')
    if (options[["csConfidenceInterval"]]){
      table$addColumnInfo(name = "cpmlci", title = gettext("Lower"), type = "integer", overtitle = gettextf("%s CI for Cpm", paste(ciLevelPercent, "%")))
      table$addColumnInfo(name = "cpmuci", title = gettext("Upper"), type = "integer", overtitle = gettextf("%s CI for Cpm", paste(ciLevelPercent, "%")))
    }
  }

  table$showSpecifiedColumnsOnly <- TRUE

  # Take a look at this input! Is is supposed to be like this or must it be transposed?
  # Transposed gives NA often as std.dev
  if(length(measurements) < 2){
    type <- 'xbar.one'
  }else{
    type <- 'R'
  }
  qccFit <- qcc::qcc(as.data.frame(dataset[, measurements]), type = type, plot = FALSE)
  allData <- unlist(dataset[, measurements])
  sdo <- sd(allData)
  sdw <- qccFit[["std.dev"]]
  meanOverall <- mean(allData)
  usl <- options[["upperSpecification"]]
  lsl <- options[["lowerSpecification"]]
  m <- (options[["upperSpecification"]] + options[["lowerSpecification"]])/2
  n <- length(allData)
  t <- options[["targetValue"]]
  tolMultiplier <- 6

  pp <- (usl - lsl) / (tolMultiplier * sdo)
  ppl <- (meanOverall - lsl) / ((tolMultiplier/2) * sdo)
  ppu <- (usl - mean(allData)) / ((tolMultiplier/2) * sdo)

  if (options[["lowerSpecificationField"]] && options[["upperSpecificationField"]]){
    ppk <- min(ppu, ppl)
  }else if(options[["lowerSpecificationField"]] && !options[["upperSpecificationField"]]){
    ppk <- ppl
  }else{
    ppk <- ppu
  }
  cp <- (usl - lsl) / (tolMultiplier * qccFit[["std.dev"]])

  if (options[["lowerSpecificationField"]] && options[["upperSpecificationField"]] && options[["targetValueField"]]){
    if (t == m){
      cpm <- (usl - lsl) / (tolMultiplier * sqrt((sum((allData - t)^2)) / n))
    }else{
      cpm <- min(c(t - lsl, usl - t)) / ((tolMultiplier / 2) * sqrt((sum((allData - t)^2)) / n))
    }
  }else if (options[["upperSpecificationField"]] && options[["targetValueField"]]){
    cpm <- (usl - t) / ((tolMultiplier / 2) * sqrt((sum((allData - t)^2)) / n))
  }else if (options[["lowerSpecificationField"]] && options[["targetValueField"]]){
    cpm <- (t - lsl) / ((tolMultiplier / 2) * sqrt((sum((allData - t)^2)) / n))
  }

  rows <- list("pp" = round(pp,2), "ppl" = round(ppl,2), "ppu" = round(ppu,2), "ppk" = round(ppk,2))
  if (options[["targetValueField"]])
    rows[["cpm"]] <- round(cpm,2)

  if (options[["csConfidenceInterval"]]){

    #CI for Pp
    dfPp <- n - 1
    ciLbPp <- pp * sqrt( qchisq(p = ciAlpha/2, df = dfPp) /dfPp)
    ciUbPp <- pp * sqrt(qchisq(p = 1 - (ciAlpha/2), df = dfPp) / dfPp)

    #CI for Ppk
    dfPpk <- n - 1
    normCIrange <- qnorm(1 - (ciAlpha / 2))
    intervalPpk <- sqrt(1 / (((tolMultiplier / 2)^2) * n)  +  ((ppk^2)/ (2 * dfPpk)))
    ciLbPpk <- ppk - (normCIrange * intervalPpk)
    ciUbPpk <- ppk + (normCIrange * intervalPpk)



    rows[["pplci"]] <- round(ciLbPp,2)
    rows[["ppuci"]] <- round(ciUbPp,2)
    rows[["ppklci"]] <- round(ciLbPpk,2)
    rows[["ppkuci"]] <- round(ciUbPpk,2)

    if (options[["targetValueField"]]){

      #CI for Cpm
      a <- (meanOverall - t) / sdo
      dfCpm <- (n * ((1 + (a^2))^2)) / (1 + (2 * (a^2)))
      ciLbCpm <- cpm * sqrt( qchisq(p = ciAlpha/2, df = dfCpm) /dfCpm)
      ciUbCpm <- cpm * sqrt(qchisq(p = 1 - (ciAlpha/2), df = dfCpm) / dfCpm)


      rows[["cpmlci"]] <- round(ciLbCpm,2)
      rows[["cpmuci"]] <- round(ciUbCpm,2)

    }
  }
  table$addRows(rows)

  if(returnOverallCapDataframe){
    if(!options[["upperSpecificationField"]])
      ppu <- NA
    if(!options[["lowerSpecificationField"]])
      ppl <- NA
    if(!(options[["lowerSpecificationField"]] && options[["upperSpecificationField"]]))
      pp <- NA
    if(!options[["targetValueField"]])
      cpm <- NA

    valueVector1 <- na.omit(c(pp, ppl, ppu, ppk, cpm))
    df <- data.frame(sources = sourceVector1,
                     values = round(valueVector1,2))
    return(df)
  }

  table2 <- createJaspTable(title = gettext("Non-conformance statistics"))
  table2$addColumnInfo(name = "rowNames", type = "string", title = "")
  table2$addColumnInfo(name = "observed", type = "integer", title = "Observed")
  table2$addColumnInfo(name = "expOverall", type = "number", title = "Expected overall")
  table2$addColumnInfo(name = "expWithin", type = "number", title = "Expected within")

  table2$showSpecifiedColumnsOnly <- TRUE


  #Calculate performance
  allDataVector <- as.vector(allData)
  rowNames <- c("ppm < LSL", "ppm > USL", "ppm total")

  #observed
  if (options[["lowerSpecificationField"]]){
    oLSL <- (1e6*length(allDataVector[allDataVector < lsl])) / n
  }else{
    oLSL <- NA
  }
  if (options[["upperSpecificationField"]]){
    oUSL <- (1e6*length(allDataVector[allDataVector > usl])) / n
  }else{
    oUSL <- NA
  }
  oTOT <- sum(c(oLSL, oUSL), na.rm = T)
  observed <- round(c(oLSL, oUSL, oTOT),2)

  # expected overall
  if (options[["lowerSpecificationField"]]){
    eoLSL <- 1e6 * (1 - pnorm((meanOverall - lsl)/sdo))
  }else{
    eoLSL <- NA
  }
  if (options[["upperSpecificationField"]]){
    eoUSL <- 1e6 * (1 - pnorm((usl - meanOverall)/sdo))
  }else{
    eoUSL <- NA
  }
  eoTOT <- sum(c(eoLSL, eoUSL), na.rm = T)
  expOverall <- c(eoLSL, eoUSL, eoTOT)


  # expected within
  if (options[["lowerSpecificationField"]]){
    ewLSL <- 1e6 * (1 - pnorm((meanOverall - lsl)/sdw))
  }else{
    ewLSL <- NA
  }
  if (options[["upperSpecificationField"]]){
    ewUSL <- 1e6 * (1 - pnorm((usl - meanOverall)/sdw))
  }else{
    ewUSL <- NA
  }
  ewTOT <- sum(c(ewLSL, ewUSL), na.rm = T)
  expWithin <- c(ewLSL, ewUSL, ewTOT)


  nDecimals <- max(.decimalplaces(dataset[measurements]))
  if(returnPerformanceDataframe){
    df <- data.frame("Source" = rowNames,
                     "Observed" = observed,
                     "Expected Overall" = round(expOverall, nDecimals),
                     "Expected Within"  = round(expWithin, nDecimals))
    return(df)

  }



  table2List <- list("rowNames" = rowNames,
                     "observed" = observed,
                     "expOverall" = expOverall,
                     "expWithin" = expWithin)

  table2$setData(table2List)

  container[["capabilityTableOverall"]] <- table
  container[["capabilityTablePerformance"]] <- table2

}

.qcProcessCapabilityTableNonNormal <- function(options, dataset, ready, container, measurements, returnSummaryDF = FALSE, returnCapabilityDF = FALSE,
                                               returnPerformanceDF = FALSE) {

  table <- createJaspTable(title = gettextf("Process summary"))

  if (((options[["nullDistribution"]] == 'Lognormal') || options[["nullDistribution"]] == 'Weibull') && any(dataset[measurements] < 0)){
    table$setError(gettext("Dataset contains negative numbers. Not compatible with the selected distribution."))
    container[["summaryTableNonNormal"]] <- table
    return()
  }

  if(!options[["upperSpecificationField"]] && !options[["lowerSpecificationField"]]){
    table$setError(gettext("No specification limits set."))
    container[["summaryTableNonNormal"]] <- table
    return()
  }

  sourceVector1 <- vector()

  if (options[["lowerSpecificationField"]])
    table$addColumnInfo(name = "lsl", type = "number", title = gettext("LSL"))

  if (options[["targetValueField"]])
    table$addColumnInfo(name = "target", type = "number", title = gettext("Target"))

  if (options[["upperSpecificationField"]])
    table$addColumnInfo(name = "usl", type = "number", title = gettext("USL"))

  table$addColumnInfo(name = "n", type = "integer", title = gettext("Sample size"))
  table$addColumnInfo(name = "mean", type = "number", title = gettext("Average"))
  table$addColumnInfo(name = "sd", type = "number", title = gettext("Std. deviation"))
  if(options[["nonNormalDist"]] == "3lognormal" | options[["nonNormalDist"]] == "Lognormal"){
    table$addColumnInfo(name = "beta", type = "number", title = gettextf("Log mean (%1$s)", "\u03BC"))
    table$addColumnInfo(name = "theta", type = "number", title = gettextf("Log std.dev (%1$s)", "\u03C3"))
  }
  else{
    table$addColumnInfo(name = "beta", type = "number", title = gettextf("Shape (%1$s)", "\u03BB"))
    table$addColumnInfo(name = "theta", type = "number", title = gettext("Scale (<i>k</i>)"))
  }
  sourceVector1 <- c(sourceVector1, 'LSL', 'Target', 'USL', 'Sample size', 'Mean', 'Std. Deviation', "Beta", "Theta")

  if(options[["nonNormalDist"]] == "3lognormal" | options[["nonNormalDist"]] == "3weibull"){
    table$addColumnInfo(name = "threshold", type = "number", title = gettext('Threshold'))
    sourceVector1 <- c(sourceVector1, 'Threshold')
  }


  table$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  allData <- unlist(dataset[, measurements])
  n <- length(allData)
  lsl <- options[["lowerSpecification"]]
  usl <- options[["upperSpecification"]]
  target <- options[["targetValue"]]
  sd <- sd(allData)
  mean <- mean(allData, na.rm = TRUE)

  distParameters <- .distributionParameters(data = allData, distribution = options[["nonNormalDist"]])
  beta <- distParameters$beta
  theta <- distParameters$theta

  rows <- list("n" = n,"mean" = mean, "sd" = sd, "lsl" = lsl,
               "usl" = usl, "target" = target, "beta" = beta, "theta" = theta)
  if(options[["nonNormalDist"]] == "3lognormal" | options[["nonNormalDist"]] == "3weibull"){
    threshold <- distParameters$threshold
    rows[['threshold']] <- threshold
  }

  if(returnSummaryDF){
    if (!options[["lowerSpecificationField"]])
      lsl <- '*'
    if (!options[["targetValueField"]])
      target <- '*'
    if (!options[["upperSpecificationField"]])
      usl <- '*'
    valueVector <- c(lsl, target, usl, n, mean, sd, beta, theta)
    if(options[["nonNormalDist"]] == "3lognormal" | options[["nonNormalDist"]] == "3weibull")
      valueVector <- c(valueVector, threshold)
    df <- data.frame(sources = sourceVector1,
                     values = valueVector)
    return(df)
  }

  table2 <- createJaspTable(title = gettextf("Process performance (total)"))

  sourceVector2 <- vector()

  if (options[["upperSpecificationField"]] && options[["lowerSpecificationField"]]){
    table2$addColumnInfo(name = "pp", type = "integer", title = gettext("Pp"))
    sourceVector2 <- c(sourceVector2, 'Pp')
  }
  if (options[["lowerSpecificationField"]]){
    table2$addColumnInfo(name = "ppl", type = "integer", title = gettext("PpL"))
    sourceVector2 <- c(sourceVector2, 'PpL')
  }
  if (options[["upperSpecificationField"]]){
    table2$addColumnInfo(name = "ppu", type = "integer", title = gettext("PpU"))
    sourceVector2 <- c(sourceVector2, 'PpU')
  }
  table2$addColumnInfo(name = "ppk", type = "integer", title = gettext("Ppk"))
  sourceVector2 <- c(sourceVector2, 'Ppk')

  table2data <- list()

  if (options[["nonNormalDist"]] == "Lognormal") {
    if (options[["lowerSpecificationField"]]){
      if(options[['nonNormalMethod' ]] == "nonconformance"){
        p1 <- plnorm(q = lsl, meanlog = beta, sdlog = theta, lower.tail = T)
        zLSL <- qnorm(p1)
        ppl <- -zLSL/3
      }else{
        x135 <- qlnorm(p = 0.00135, meanlog = beta, sdlog = theta)
        x05 <- qlnorm(p = 0.5, meanlog = beta, sdlog = theta)
        ppl <- (x05 - lsl) / (x05 - x135)
      }
      table2data[["ppl"]] <- round(ppl,2)
    }else{
      ppl <- NA
    }
    if (options[["upperSpecificationField"]]){
      if(options[['nonNormalMethod' ]] == "nonconformance"){
        p2 <- plnorm(q = usl,  meanlog = beta, sdlog = theta)
        zUSL <- qnorm(p2)
        ppu <- zUSL/3
      }else{
        x99 <- qlnorm(p = 0.99865, meanlog = beta, sdlog = theta)
        x05 <- qlnorm(p = 0.5, meanlog = beta, sdlog = theta)
        ppu <- (usl - x05) / (x99 - x05)
      }
      table2data[["ppu"]] <- round(ppu,2)
    }else{
      ppu <- NA
    }
  }else if (options[["nonNormalDist"]] == "Weibull") {
    if (options[["lowerSpecificationField"]]){
      if(options[['nonNormalMethod' ]] == "nonconformance"){
        p1 <- pweibull(q = lsl, shape = beta, scale = theta, lower.tail = T)
        zLSL <- qnorm(p1)
        ppl <- -zLSL/3
      }else{
        x135 <- qweibull(p = 0.00135, shape = beta, scale = theta)
        x05 <- qweibull(p = 0.5, shape = beta, scale = theta)
        ppl <- (x05 - lsl) / (x05 - x135)
      }
      table2data[["ppl"]] <- round(ppl,2)
    }else{
      ppl <- NA
    }
    if (options[["upperSpecificationField"]]){
      if(options[['nonNormalMethod' ]] == "nonconformance"){
        p2 <- pweibull(q = usl, shape = beta, scale = theta)
        zUSL <- qnorm(p2)
        ppu <- zUSL/3
      }else{
        x99 <- qweibull(p = 0.99865, shape = beta, scale = theta)
        x05 <- qweibull(p = 0.5, shape = beta, scale = theta)
        ppu <- (usl - x05) / (x99 - x05)
      }
      table2data[["ppu"]] <- round(ppu,2)
    }else{
      ppu <- NA
    }
  }else if (options[["nonNormalDist"]] == "3lognormal") {
    if (options[["lowerSpecificationField"]]){
      if(options[['nonNormalMethod' ]] == "nonconformance"){
        p1 <- FAdist::plnorm3(q = lsl, shape = theta, scale = beta, thres = threshold, lower.tail = T)
        zLSL <- qnorm(p1)
        ppl <- -zLSL/3
      }else{
        x135 <- FAdist::qlnorm3(p = 0.00135, shape = theta, scale = beta, thres = threshold)
        x05 <- FAdist::qlnorm3(p = 0.5, shape = theta, scale = beta, thres = threshold)
        ppl <- (x05 - lsl) / (x05 - x135)
      }
      table2data[["ppl"]] <- round(ppl,2)
    }else{
      ppl <- NA
    }
    if (options[["upperSpecificationField"]]){
      if(options[['nonNormalMethod' ]] == "nonconformance"){
        p2 <- FAdist::plnorm3(q = usl, shape = theta, scale = beta, thres = threshold)
        zUSL <- qnorm(p2)
        ppu <- zUSL/3
      }else{
        x99 <- FAdist::qlnorm3(p = 0.99865, shape = theta, scale = beta, thres = threshold)
        x05 <- FAdist::qlnorm3(p = 0.5, shape = theta, scale = beta, thres = threshold)
        ppu <- (usl - x05) / (x99 - x05)
      }
      table2data[["ppu"]] <- round(ppu,2)
    }else{
      ppu <- NA
    }
  }else if (options[["nonNormalDist"]] == "3weibull") {
    if (options[["lowerSpecificationField"]]){
      if(options[['nonNormalMethod' ]] == "nonconformance"){
        p1 <- FAdist::pweibull3(q = lsl, shape = beta, scale = theta, thres = threshold, lower.tail = T)
        zLSL <- qnorm(p1)
        ppl <- -zLSL/3
      }else{
        x135 <- FAdist::qweibull3(p = 0.00135, shape = beta, scale = theta, thres = threshold)
        x05 <- FAdist::qweibull3(p = 0.5, shape = beta, scale = theta, thres = threshold)
        ppl <- (x05 - lsl) / (x05 - x135)
      }
      table2data[["ppl"]] <- round(ppl,2)
    }else{
      ppl <- NA
    }
    if (options[["upperSpecificationField"]]){
      if(options[['nonNormalMethod' ]] == "nonconformance"){
        p2 <- FAdist::pweibull3(q = usl, shape = beta, scale = theta, thres = threshold)
        zUSL <- qnorm(p2)
        ppu <- zUSL/3
      }else{
        x99 <- FAdist::qweibull3(p = 0.99865, shape = beta, scale = theta, thres = threshold)
        x05 <- FAdist::qweibull3(p = 0.5, shape = beta, scale = theta, thres = threshold)
        ppu <- (usl - x05) / (x99 - x05)
      }
      table2data[["ppu"]] <- round(ppu,2)
    }else{
      ppu <- NA
    }
  }

  if (options[["upperSpecificationField"]] && options[["lowerSpecificationField"]]){
    if(options[['nonNormalMethod' ]] == "nonconformance"){
      pp <- (zUSL - zLSL)/6
    }else{
      pp <- (usl - lsl) / (x99 - x135)
    }
    table2data[["pp"]] <- round(pp,2)
  }else{
    pp <- NA
  }
  ppk <- min(c(ppl, ppu), na.rm = T)
  table2data[["ppk"]] <- round(ppk,2)

  if(returnCapabilityDF){
    valueVector <- c(pp, ppl, ppu, ppk)
    df <- data.frame(source = sourceVector2,
                     values = na.omit(valueVector))
    return(df)
  }

  table3 <- createJaspTable(title = gettextf("Non-conformance statistics"))

  table3$addColumnInfo(name = "rowNames", type = "string", title = "")
  table3$addColumnInfo(name = "observed", type = "number", title = gettext("Observed"))
  table3$addColumnInfo(name = "expOverall", type = "number", title = gettext("Expected overall"))


  allDataVector <- as.vector(allData)
  rowNames <- c("ppm < LSL", "ppm > USL", "Total ppm")

  #observed
  if (options[["lowerSpecificationField"]]){
    oLSL <- (1e6*length(allDataVector[allDataVector < lsl])) / n
  }else{
    oLSL <- NA
  }
  if (options[["upperSpecificationField"]]){
    oUSL <- (1e6*length(allDataVector[allDataVector > usl])) / n
  }else{
    oUSL <- NA
  }
  oTOT <- sum(c(oLSL, oUSL), na.rm = T)
  observed <- c(oLSL, oUSL, oTOT)

  # expected overall

  if (options[["nonNormalDist"]] == "Lognormal") {
    distname <- "Lognormal"
    if (options[["lowerSpecificationField"]]){
      eoLSL <- 1e6 * plnorm(q = lsl, meanlog = beta, sdlog = theta, lower.tail = T)
    }else{
      eoLSL <- NA
    }
    if (options[["upperSpecificationField"]]){
      eoUSL <- 1e6 * (1 - plnorm(q = usl, meanlog = beta, sdlog = theta))
    }else{
      eoUSL <- NA
    }
  } else if (options[["nonNormalDist"]] == "Weibull") {
    distname <- "Weibull"
    if (options[["lowerSpecificationField"]]){
      eoLSL <- 1e6 * pweibull(q = lsl, shape = beta, scale = theta, lower.tail = T)
    }else{
      eoLSL <- NA
    }
    if (options[["upperSpecificationField"]]){
      eoUSL <- 1e6 * (1 - pweibull(q = usl, shape = beta, scale = theta))
    }else{
      eoUSL <- NA
    }
  }else if (options[["nonNormalDist"]] == "3lognormal") {
    distname <- "3-parameter-lognormal"
    if (options[["lowerSpecificationField"]]){
      eoLSL <- 1e6 * FAdist::plnorm3(q = usl, shape = theta, scale = beta, thres = threshold, lower.tail = T)
    }else{
      eoLSL <- NA
    }
    if (options[["upperSpecificationField"]]){
      eoUSL <- 1e6 * (1 - FAdist::plnorm3(q = usl, shape = theta, scale = beta, thres = threshold))
    }else{
      eoUSL <- NA
    }
  }else if (options[["nonNormalDist"]] == "3weibull") {
    distname <- "3-parameter-weibull"
    if (options[["lowerSpecificationField"]]){
      eoLSL <- 1e6 * FAdist::pweibull3(q = usl, shape = beta, scale = theta, thres = threshold, lower.tail = T)
    }else{
      eoLSL <- NA
    }
    if (options[["upperSpecificationField"]]){
      eoUSL <- 1e6 * (1 - FAdist::pweibull3(q = usl, shape = beta, scale = theta, thres = threshold))
    }else{
      eoUSL <- NA
    }
  }
  eoTOT <- sum(c(eoLSL, eoUSL), na.rm = T)
  expOverall <- c(eoLSL, eoUSL, eoTOT)

  table3data <- list("rowNames" = rowNames, "observed" = observed, "expOverall" = expOverall)

  if(returnPerformanceDF){
    df <- data.frame("Source" = rowNames,
                     "Observed" = observed,
                     "Expected Overall" = expOverall)
    return(df)
  }

  table$addRows(rows)
  table$addFootnote(gettextf("Calculations based on %s distribution.", distname))
  container[["summaryTableNonNormal"]] <- table

  table2$setData(table2data)
  container[["overallCapabilityNonNormal"]] <- table2

  table3$setData(table3data)
  container[["PerformanceNonNormal"]] <- table3


}



#############################################################
## Functions for probability plot section ###################
#############################################################

################
## Containers ##
################

.qcProbabilityPlotContainer <- function(options, dataset, ready, jaspResults, measurements) {

  if (!options[["probabilityPlot"]] || !is.null(jaspResults[["probabilityContainer"]]))
    return()

  container <- createJaspContainer(gettext("Probability Table and Plot"))
  container$dependOn(options = c("variables", "probabilityPlot", "rank", "nullDistribution", "addGridlines", "variablesLong", "pcSubgroupSize",
                                 "manualSubgroupSize", "subgroup", "pcReportDisplay"))
  container$position <- 3

  jaspResults[["probabilityContainer"]] <- container

  if (!ready)
    return()

  .qcProbabilityTable(dataset, options, container, measurements)

  if (is.null(container[["ProbabilityPlot"]]))
    container[["ProbabilityPlot"]]  <- .qcProbabilityPlot(dataset, options, measurements)
}

################
## Output ######
################

.qcProbabilityTable <- function(dataset, options, container, measurements) {

  table <- createJaspTable(title = gettextf("Summary of test against the %1$s distribution", options[["nullDistribution"]]))
  table$position <- 1

  table$addColumnInfo(name = "n",      	title = gettext("N"),  		type = "integer")

  if (options[["nullDistribution"]] == 'Normal') {
    table$addColumnInfo(name = "mean",  title = gettextf("Mean (%1$s)", "\u03BC"), 				type = "number")
    table$addColumnInfo(name = "sd",    title = gettextf("Std. deviation (%1$s)", "\u03C3"), 	type = "number")
  } else if (options[["nullDistribution"]] == 'Lognormal') {
    table$addColumnInfo(name = "mean",  title = gettextf("Log mean (%1$s)", "\u03BC"),  		type = "number")
    table$addColumnInfo(name = "sd",    title = gettextf("Log std.dev (%1$s)", "\u03C3"), 			type = "number")
  } else if (options[["nullDistribution"]] == 'Weibull') {
    table$addColumnInfo(name = "mean",  title = gettextf("Shape (%1$s)", "\u03BB"), 			type = "number")
    table$addColumnInfo(name = "sd",    title = gettext("Scale (<i>k</i>)"),        			type = "number")
  }

  table$addColumnInfo(name = "ad",     	title = gettext("AD"), type = "number")
  table$addColumnInfo(name = "p",		title = gettext("<i>p</i>-value"), type = "pvalue")

  table$addFootnote(gettextf("The Anderson-Darling statistic A<i>D</i> is calculated against the %2$s distribution.", "\u00B2", options[["nullDistribution"]]))
  table$addFootnote(gettextf("Red dotted lines in the probability plot below represent a 95%% confidence interval."))

  if (((options[["nullDistribution"]] == 'Lognormal') || options[["nullDistribution"]] == 'Weibull') && any(dataset[measurements] < 0)){
    table$setError(gettext("Dataset contains negative numbers. Not compatible with the selected distribution."))
    container[["probabilityTable"]] <- table
    return()
  }

  values <- as.vector(unlist(dataset[measurements]))

  if (options[["nullDistribution"]] == 'Normal') {
    meanx   <- mean(values)
    sdx     <- sd(values)
    test    <- goftest::ad.test(x = values, "norm", mean = meanx, sd = sdx)
  } else if (options[["nullDistribution"]] == 'Lognormal') {
    fit    <- fitdistrplus::fitdist(values, 'lnorm')
    meanx  <- fit$estimate[1]
    sdx    <- fit$estimate[2]
    test   <- goftest::ad.test(x = values, "plnorm", meanlog = meanx, sdlog = sdx)
  } else if (options[["nullDistribution"]] == 'Weibull') {
    fit    <- fitdistrplus::fitdist(values, 'weibull')
    meanx  <- fit$estimate[1]
    sdx    <- fit$estimate[2]
    test   <- goftest::ad.test(x = values, "pweibull", shape = meanx, scale = sdx)
  }

  n      <- length(values)
  ad     <- test$statistic
  adStar <- ad*(1 + (0.75/n) + (2.25/(n^2)))
  if(ad >= 0.6){
    p <- exp(1.2937 - (5.709 * adStar) + 0.0186 * (adStar^2))
  }else if(adStar < 0.6 && adStar > 0.34){
    p <- exp(0.9177 - (4.279 * adStar) - 1.38 * (adStar^2))
  }else if(adStar < 0.34 && adStar > 0.2){
    p <- 1 - exp(-8.318 + (42.796 * adStar) - 59.938 * (adStar^2))
  }else if(adStar <= 0.2){
    p <- 1 - exp(-13.436 + (101.14 * adStar) - 223.73 * (adStar^2))      #Jaentschi & Bolboaca (2018)
  } else {
    p <- test$p.value
  }

  row <- list(mean = meanx, sd = sdx, n = n, ad = ad, p = p)
  table$addRows(row)


  container[["probabilityTable"]] <- table
}

.qcProbabilityPlot <- function(dataset, options, measurements = NULL, fit = "", ggPlot = FALSE) {

  plot <- createJaspPlot(width = 600, aspectRatio = 1, title = "Probability Plot")
  plot$dependOn(c("variablesLong", "pcSubgroupSize"))

  if (((options[["nullDistribution"]] == 'Lognormal') || options[["nullDistribution"]] == 'Weibull') && any(dataset[measurements] < 0)){
    plot$setError(gettext("Dataset contains negative numbers. Not compatible with the selected distribution."))
    return(plot)
  }

  FactorialFit <- fit != ""

  # Arrange data
  if (FactorialFit){
    x <- as.vector(resid(fit))
    order1 <- order(x)
    options[["addGridlines"]] <- FALSE
    #factorsNames <- rownames(summary(fit)$coefficients)[-1][order1]
    #p.sig <- as.vector(summary(fit)$coefficients[,4][-1][order1] < 0.05)
  } else {
    x <- as.vector(unlist(dataset[measurements]))
  }

  x <- x[order(x)]
  label_x <- x
  n <- length(x)
  i <- rank(x)

  # Method for rank
  Rank_funs <- matrix(list(.qcPpMedian, .qcPpMean, .qcPpKmModif, .qcPpKm), ncol = 1,
                      dimnames = list(c("Bernard", "Herd-Johnson", "Hazen", 'Kaplan-Meier'), c("p")), byrow = TRUE)
  if (FactorialFit)
    rankByUser <- "Bernard"
  else
    rankByUser <- options[["rank"]]
  p <- Rank_funs[[rankByUser, 'p']](x)

  # Functions for computing y
  y_funs <- matrix(list(qnorm,qnorm,.qcWeibull), ncol = 1,
                   dimnames = list(c('Normal', 'Lognormal', 'Weibull'), c('y')), byrow = TRUE)
  if (FactorialFit)
    DisByUser <- "Normal"
  else
    DisByUser <- options[["nullDistribution"]]

  y <- y_funs[[DisByUser, 'y']](p)

  # Quantities
  pSeq <- seq(0.001, 0.999, 0.001)
  ticks <- c(0.1, 1, 5, seq(10, 90, 10), 95, 99, 99.9)

  # Computing according to the distribution
  if (options[["nullDistribution"]] == 'Normal' || FactorialFit) {
    lpdf <- quote(-log(sigma) - 0.5 / sigma ^ 2 * (x - mu) ^ 2)
    matrix <- mle.tools::observed.varcov(logdensity = lpdf, X = x, parms = c("mu", "sigma"), mle = c(mean(x), sd(x)))
    varMu <- matrix$varcov[1, 1]
    varSigma <- matrix$varcov[2,2]
    covarMuSigma <- matrix$varcov[1, 2]
    zp <- qnorm(p = pSeq)
    zalpha <- qnorm(0.975)
    percentileEstimate <- mean(x) + zp * sd(x)
    varPercentile <- varMu + zp^2 * varSigma + 2*zp * covarMuSigma
    percentileLower <- percentileEstimate - zalpha * sqrt(varPercentile)
    percentileUpper <- percentileEstimate + zalpha * sqrt(varPercentile)
    yBreaks <- qnorm(ticks / 100)

    xBreaks <- label_x <- jaspGraphs::getPrettyAxisBreaks(x)
    xLimits <- range(xBreaks)
  } else if (options[["nullDistribution"]] == 'Lognormal') {
    fit <- fitdistrplus::fitdist(x, 'lnorm')
    meanlog <- as.numeric(fit$estimate[1])
    sdlog <- as.numeric(fit$estimate[2])
    lpdf <- quote(log(1/(sqrt(2*pi)*x*sdlog) * exp(-(log(x)- meanlog)^2/(2*sdlog^2))))
    matrix <- mle.tools::observed.varcov(logdensity = lpdf, X = x, parms = c("meanlog", "sdlog"), mle = fit$estimate)
    varmeanlog <- matrix$varcov[1, 1]
    varsdlog <- matrix$varcov[2,2]
    covarSS <- matrix$varcov[1, 2]
    zp <- qnorm(p = pSeq)
    zalpha <- qnorm(0.975)
    percentileEstimate <- exp(meanlog + zp*sdlog)
    varPercentile <- percentileEstimate^2*( varmeanlog+zp^2*varsdlog + 2*zp * covarSS)
    percentileLower <- exp( log(percentileEstimate) - zalpha * (sqrt(varPercentile)/percentileEstimate))
    percentileUpper <- exp(log(percentileEstimate) + zalpha * (sqrt(varPercentile)/percentileEstimate))

    yBreaks <- qnorm(ticks / 100)
    x <- log(label_x)
    percentileEstimate <- log(percentileEstimate)
    percentileLower <- log(percentileLower)
    percentileUpper <- log(percentileUpper)

    labelFrame <- data.frame(labs = label_x, value = x)
    index <- c(1,jaspGraphs::getPrettyAxisBreaks(1:nrow(labelFrame), 4)[-1])
    xBreaks <- labelFrame[index,2]
    label_x <- labelFrame[index,1]
    xLimits <- range(xBreaks)
  } else if (options[["nullDistribution"]] == 'Weibull') {
    fit <- fitdistrplus::fitdist(x, 'weibull')
    shape <- as.numeric(fit$estimate[1])
    scale <- as.numeric(fit$estimate[2])
    lpdf <- quote(log(shape) - shape * log(scale) + shape * log(x) - (x / scale)^ shape )
    matrix <- mle.tools::observed.varcov(logdensity = lpdf, X = x, parms = c("shape", "scale"), mle = fit$estimate)
    varShape <- matrix$varcov[1,1]
    varScale <- matrix$varcov[2,2]
    covarSS <- matrix$varcov[1,2]
    zp <- log(-1*log(1-pSeq))
    zalpha <- log(-1*log(1-0.975))
    percentileEstimate <- scale * (- log(1 - pSeq))^(1/shape)
    varPercentile <- (percentileEstimate^2 / scale^2) * varScale + (percentileEstimate^2/shape^4)*zp^2*varShape - 2*((zp*percentileEstimate^2) / (scale * shape^2))*covarSS
    percentileLower <- exp( log(percentileEstimate) - zalpha * (sqrt(varPercentile)/percentileEstimate))
    percentileUpper <- exp(log(percentileEstimate) + zalpha * (sqrt(varPercentile)/percentileEstimate))

    yBreaks <- log(-1*log(1-(ticks / 100)))
    x <- log(label_x)
    percentileEstimate <- log(percentileEstimate)
    percentileLower <- log(percentileLower)
    percentileUpper <- log(percentileUpper)

    labelFrame <- data.frame(labs = label_x, value = x)
    index <- c(1,jaspGraphs::getPrettyAxisBreaks(1:nrow(labelFrame), 4)[-1])
    xBreaks <- labelFrame[index,2]
    label_x <- labelFrame[index,1]
    xLimits <- range(xBreaks) * 1.2
  }
  data1 <- data.frame(x = x, y = y)
  yLimits <- range(yBreaks)

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileEstimate)) +
    jaspGraphs::geom_point(ggplot2::aes(x = x, y = y))

  if (options[["addGridlines"]])
    p <- p + ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "lightgray"))

  if (FactorialFit) {
    p <- p +
      ggplot2::scale_x_continuous(gettext("Residuals"), breaks = xBreaks, limits = xLimits * 1.2, labels = label_x) +
      ggplot2::scale_y_continuous(gettext('Percent'), labels = ticks, breaks = yBreaks, limits = yLimits)


    #ordered.Factors <- factorsNames[p.sig]
    #p <- p + jaspGraphs::geom_point(ggplot2::aes(x = x, y = y, color = ifelse(as.vector(p.sig), "Significant", "Not significant"))) +
    #  ggplot2::theme(legend.position = 'right', legend.title = ggplot2::element_blank()) +
    #  ggplot2::scale_x_continuous("Standardized Effect", breaks = xBreaks, limits = xLimits * 1.2, labels = label_x)

    #x.sig <- x[p.sig]
    #y.sig <- y[p.sig]
    #for (i in 1:length(ordered.Factors))
    #  p <- p + ggplot2::annotate("text", x = x.sig[i] * 1.05, y = y.sig[i] * 1.05, label = sprintf("%s", ordered.Factors[i]))
  } else {
    p <- p + ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileLower), col = "darkred", linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileUpper), col = "darkred", linetype = "dashed") +
      ggplot2::scale_x_continuous(gettext("Measurement"), breaks = xBreaks, limits = xLimits, labels = label_x) +
      ggplot2::scale_y_continuous(gettext('Percent'), labels = ticks, breaks = yBreaks, limits = yLimits)
  }

  p <- jaspGraphs::themeJasp(p)
  plot$plotObject <- p

  if (ggPlot)
    return(p)
  else
    return(plot)
}

.qcPpMedian <- function(x) {
  x <- x[order(x)]
  n <- length(x)
  i <- rank(x)
  p <- (i - 0.3) / (n + 0.4)
  return(p)
}

.qcPpMean <- function(x) {
  x <- x[order(x)]
  n <- length(x)
  i <- rank(x)
  p <- (i) / (n + 1)
  return(p)
}

.qcPpKmModif <- function(x,i,n) {
  x <- x[order(x)]
  n <- length(x)
  i <- rank(x)
  p <- (i - 0.5) / (n)
  return(p)
}

.qcPpKm <- function(x) {
  x <- x[order(x)]
  n <- length(x)
  i <- rank(x)
  p <- (i) / (n)
  return(p)
}

.qcWeibull <- function(p) {
  return(log(-log(1 - p)))
}

#############################################################
## Functions for distribution plot section ##################
#############################################################

.qcDistributionPlot <- function(options, dataset, ready, jaspResults, measurements) {

  if(!options[["histogram"]] || !is.null(jaspResults[["histogram"]]))
    return()

  plot <- createJaspPlot(title = gettext("Histogram"), width = 400, height = 400)
  plot$dependOn(options = c("histogram", "displayDensity", "variables", "pcNumberOfBins", "pcBinWidthType", "pcReportDisplay", "variablesLong", "pcSubgroupSize", "manualSubgroupSize", "subgroup", 'nullDistribution'))
  plot$position <- 2

  jaspResults[["histogram"]] <- plot

  if (!ready)
    return()

  plot$plotObject <- .qcDistributionPlotObject(options, dataset, measurements = measurements)

}

.qcDistributionPlotObject <- function(options, dataset, measurements) {

  data <- unlist(dataset[measurements])
  binWidthType <- options$pcNumberOfBins

  # } else if (binWidthType == "fd" && nclass.FD(variable) > 10000) { # FD-method will produce extreme number of bins and crash ggplot, mention this in footnote
  #   binWidthType <- 10000

  n <- length(data)
  df <- data.frame(measurements = data)
  h <- hist(data, plot = F, breaks = binWidthType)
  binWidth <- (h$breaks[2] - h$breaks[1])
  freqs <- h$counts
  yLabels <- jaspGraphs::getPrettyAxisBreaks(c(0, freqs, max(freqs) + 5))
  yBreaks <- yLabels / (n * binWidth)
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(h$breaks, data), min.n = 4)
  xLimits <- range(xBreaks)

  p <- ggplot2::ggplot() + ggplot2::geom_histogram(data = df, mapping = ggplot2::aes(y =..density.., x = measurements), closed = "left", fill = "grey", col = "black", size = .7, binwidth = binWidth, center = binWidth/2) +
    ggplot2::scale_x_continuous(name = gettext("Measurement"), breaks = xBreaks, limits = xLimits) +
    ggplot2::scale_y_continuous(name =  gettext("Counts"), labels = yLabels, breaks = yBreaks, limits = yLimits) +
    jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()

  if (options[["displayDensity"]]) {
    if(options[['nullDistribution']]  == 'Normal'){
      p <- p + ggplot2::stat_function(fun = dnorm, args = list(mean = mean(data), sd = sd(data)), color = "dodgerblue")
    }else if(options[['nullDistribution']]  == 'Weibull'){
      fit_Weibull <- fitdistrplus::fitdist(data, "weibull", method = "mle",
                                          control = list(maxit = 500, abstol = .Machine$double.eps, reltol = .Machine$double.eps))
      shape <- fit_Weibull$estimate[[1]]
      scale <- fit_Weibull$estimate[[2]]
      p <- p + ggplot2::stat_function(fun = dweibull, args = list(shape = shape, scale = scale), color = "dodgerblue")
    }else if(options[['nullDistribution']]  == 'Lognormal'){
      fit_Lnorm <- fitdistrplus::fitdist(data, "lnorm", method = "mle",
                                           control = list(maxit = 500, abstol = .Machine$double.eps, reltol = .Machine$double.eps))
      shape <- fit_Lnorm$estimate[[1]]
      scale <- fit_Lnorm$estimate[[2]]
      p <- p + ggplot2::stat_function(fun = dlnorm, args = list(meanlog = shape, sdlog = scale), color = "dodgerblue")
    }
  }

  return(p)
}


.qcImRChart<- function(options, dataset, ready, jaspResults, measurements, subgroups, wideFormat){
  if (!ready)
    return()
  Container <- createJaspContainer(gettextf("X-mR control chart"))
<<<<<<< HEAD
  Container$dependOn(options = c("xbarR", "variables", "subgroups", "variablesLong", "pcSubgroupSize", "pcReportDisplay", "movingRangeLength"))
=======
  Container$dependOn(options = c("xbarR", "variables", "subgroup", "variablesLong", "pcSubgroupSize", "pcReportDisplay"))
>>>>>>> 5b6e001 (Renaming Variable Charts for Subgroups)
  Container$position <- 1
  jaspResults[["ImR Charts"]] <- Container
  Container[["plot"]] <- .IMRchart(dataset = dataset, measurements = measurements, options = options, manualXaxis = subgroups, cowPlot = TRUE, Wide = wideFormat)$p
}

.PClongTowide<- function(dataset, k, measurements, mode = c("manual", "subgroup")){
  if(identical(mode, "manual")){
    dataset <- dataset[measurements]
    n <- nrow(dataset)
    nGroups <- n/k

    if (nGroups != as.integer(nGroups)){
      return("error")
    }
    groupVector <- paste("V", 1:k, sep = "")
    group <- rep(groupVector, nGroups)
    wideDataset <- data.frame(row = 1:nGroups)
    dataset <- cbind(dataset, group)
    for(g in groupVector){
      groupData <- data.frame(x = dataset[[measurements]][dataset$group == g])
      colnames(groupData) <- g
      wideDataset <- cbind(wideDataset, groupData)
    }
    wideDataset <- wideDataset[groupVector]
    return(wideDataset)
  }else{
    nrep <- table(dataset[k])[[1]]
    dataset <- dataset[order(dataset[k]),]
    dataset <- cbind(dataset, data.frame(rep = rep(paste("V", 1:nrep, sep = ""))))
    wideDataset <- tidyr::spread(dataset, rep, measurements)
    return(wideDataset)
  }
}


.pcReport <- function(dataset, measurements, parts, operators, options, ready, container, splitFactor, wideFormat){

  if (options[["pcReportTitle"]] == ""){
    title <- "Process Capability Report"
  }else{
    title <- options[["pcReportTitle"]]
  }
  name <- gettextf("Process name: %s", options[["pcReportName"]])
  date <- gettextf("Date of study: %s", options[["pcReportDate"]])
  text1 <- c(name, date)

  reportedBy <- gettextf("Reported by: %s", options[["pcReportReportedBy"]])
  misc <- gettextf("Misc: %s", options[["pcReportMisc"]])
  text2 <- c(reportedBy, misc)

  if(!options[["upperSpecificationField"]] && !options[["lowerSpecificationField"]]){
    plot <- createJaspPlot(title = gettext("Report"), width = 1200, height = 1000)
    plot$setError(gettext("No specification limits set."))
    return(plot)
  }

  if(!ready) {
    plot <- createJaspPlot(title = gettext("Report"), width = 1200, height = 1000)
    return(plot)

<<<<<<< HEAD
=======
  # X-bar and R Chart OR ImR Chart
  if(options$xbarR){
    p1 <- .Xbarchart(dataset = dataset[measurements], options = options, manualXaxis = splitFactor, warningLimits = FALSE, Wide = wideFormat, manualTicks = options[["manualTicksXAxis"]])$p
    p2 <- .Rchart(dataset = dataset[measurements], options = options, manualXaxis = splitFactor, warningLimits = FALSE, Wide = wideFormat, manualTicks = options[["manualTicksXAxis"]])$p
  } else{
    IMRPlots <- .IMRchart(dataset = dataset, measurements = measurements, options = options, manualXaxis = splitFactor, cowPlot = TRUE, Wide = wideFormat)
    p1 <- IMRPlots$p1
    p2 <- IMRPlots$p2
>>>>>>> 5b6e001 (Renaming Variable Charts for Subgroups)
  }

  plotList <- list()
  indexCounter <- 0

  if (options[["reportMetaData"]]) {
    indexCounter <- indexCounter + 1
    plotList[[indexCounter]] <- .ggplotWithText(text1)
    indexCounter <- indexCounter + 1
    plotList[[indexCounter]] <- .ggplotWithText(text2)
  }
  if (options[["reportProcessStability"]]) {
    # X-bar and R Chart OR ImR Chart
    if(options$xbarR){
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- .Xbarchart(dataset = dataset[measurements], options = options, manualXaxis = splitFactor, warningLimits = FALSE, Wide = wideFormat, manualTicks = options$manualTicks)$p
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- .Rchart(dataset = dataset[measurements], options = options, manualXaxis = splitFactor, Wide = wideFormat, manualTicks = options$manualTicks)$p
    } else {
      IMRPlots <- .IMRchart(dataset = dataset, measurements = measurements, options = options, manualXaxis = splitFactor, cowPlot = TRUE, Wide = wideFormat)

      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- IMRPlots$p1
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- IMRPlots$p2
    }
  }
  if (options[["reportProcessCapabilityPlot"]]) {
    indexCounter <- indexCounter + 1
    plotList[[indexCounter]] <- .qcProcessCapabilityPlotObject(options, dataset, measurements, distribution = "normal")
  }
  if (options[["reportProbabilityPlot"]]) {
    indexCounter <- indexCounter + 1
    plotList[[indexCounter]] <- .qcProbabilityPlot(dataset, options, measurements, ggPlot = TRUE)
  }
  if (options[["reportProcessCapabilityTables"]]) {
    if (options[["capabilityStudyType"]] == "normalCapabilityAnalysis"){
      processSummaryDF <- .qcProcessSummaryTable(options, dataset, ready, container, measurements, returnDataframe = TRUE)
      potentialWithinDF <- .qcProcessCapabilityTableWithin(options, dataset, ready, container, measurements, returnDataframe = TRUE)
      overallCapDF <- .qcProcessCapabilityTableOverall(options, dataset, ready, container, measurements, returnOverallCapDataframe = TRUE)
      performanceDF <- .qcProcessCapabilityTableOverall(options, dataset, ready, container, measurements, returnPerformanceDataframe = TRUE)

      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- ggplotTable(processSummaryDF) #process summary
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- ggplotTable(performanceDF, displayColNames = TRUE)   # performance
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- ggplotTable(potentialWithinDF)  #Potential within
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- ggplotTable(overallCapDF) #overall capability
    } else {
      processSummaryDF <- .qcProcessCapabilityTableNonNormal(options, dataset, ready, container, measurements, returnSummaryDF = TRUE)
      overallCapDF <- .qcProcessCapabilityTableNonNormal(options, dataset, ready, container, measurements, returnCapabilityDF = TRUE)
      performanceDF <- .qcProcessCapabilityTableNonNormal(options, dataset, ready, container, measurements, returnPerformanceDF = TRUE)

      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- ggplotTable(processSummaryDF) #process summary
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- ggplotTable(performanceDF, displayColNames = TRUE)   # performance
      indexCounter <- indexCounter + 1
      plotList[[indexCounter]] <- ggplotTable(overallCapDF)  #overall capability
    }
  }

  if (indexCounter == 0) {
    plot <- createJaspPlot(title = title, width = 400, height = 400)
    plot$setError(gettext("No report components selected."))
    return(plot)
  } else if (indexCounter %% 2 != 0){
    indexCounter <- indexCounter + 1
    plotList[[indexCounter]] <- ggplot2::ggplot() + ggplot2::theme_void()
  }


  matrixNCols <- 2
  matrixNRows <- indexCounter / matrixNCols
  matrixPlot <- createJaspPlot(title = title, width = 1200, height = 400 * matrixNRows)
  plotMat <- matrix(plotList, matrixNRows, matrixNCols, byrow = TRUE)
  p <- jaspGraphs::ggMatrixPlot(plotMat)
  matrixPlot$plotObject <- p

  return(matrixPlot)
}

ggplotTable <- function(dataframe, displayColNames = FALSE){
  df <- tibble::tibble(dataframe)
  p <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggpp::geom_table(data = data.frame(x = 1, y = 1), ggplot2::aes(x = x, y = y), label = list(df),
                                                                    table.colnames = displayColNames, size = 7)

  return(p)
}

.distributionParameters <- function(data, distribution = c('Lognormal', 'Weibull', '3lognormal', '3weibull')){
  if (distribution == "Lognormal") {
    fit_Lnorm <- fitdistrplus::fitdist(data, "lnorm", method = "mle",
                                       control = list(maxit = 500, abstol = .Machine$double.eps, reltol = .Machine$double.eps))
    beta <- fit_Lnorm$estimate[[1]]
    theta <- fit_Lnorm$estimate[[2]]
  } else if (distribution == "Weibull") {
    fit_Weibull <- fitdistrplus::fitdist(data, "weibull", method = "mle",
                                         control = list(maxit = 500, abstol = .Machine$double.eps, reltol = .Machine$double.eps))
    beta <- fit_Weibull$estimate[[1]]
    theta <- fit_Weibull$estimate[[2]]
  }else if(distribution == "3lognormal"){
    temp <- EnvStats::elnorm3(data)
    beta <- temp$parameters[[1]]
    theta <- temp$parameters[[2]]
    threshold <- temp$parameters[[3]]
  }else if(distribution == "3weibull"){
    temp <- weibullness::weibull.mle(data)
    beta <- temp[[1]]
    theta <- temp[[2]]
    threshold <- as.vector(temp[[3]])
  }
  list <- list(beta = beta,
               theta = theta)
  if(distribution == '3weibull' | distribution == '3lognormal')
    list['threshold'] <- threshold
  return(list)
}

