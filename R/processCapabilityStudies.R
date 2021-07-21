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

processCapabilityStudies <- function(jaspResults, dataset, options) {

  if (options[["pcDataFormat"]] == "PCwideFormat"){
    measurements <- unlist(options$variables)
  }else{
    measurements <- unlist(options$variablesLong)
  }
  subgroups <- unlist(options$subgroups)
  num.vars <- measurements[measurements != ""]
  fac.vars <- subgroups[subgroups != ""]

  dataset <- .readDataSetToEnd(columns.as.numeric = num.vars, columns.as.factor = fac.vars)

  if (options[["pcDataFormat"]] == "PCwideFormat"){
    ready <- length(measurements > 0)
  }else{
    ready <- (length(measurements > 0) && (options[["manualSubgroupSize"]] | subgroups != ""))
  }

  if (options[["pcDataFormat"]] == "PClongFormat" && ready){
    if(options[["manualSubgroupSize"]]){
      k <- options[["pcSubgroupSize"]]
      n <- nrow(dataset)
      dataset <- .PClongTowide(dataset, k, measurements, mode = "manual")
      if (dataset == "error"){
        plot <- createJaspPlot(title = gettext("Capability of the Process"), width = 700, height = 400)
        jaspResults[["plot"]] <- plot
        plot$setError(gettextf("Could not equally divide %i data points into groups of size %i.", n, k))
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

  dataset <- na.omit(dataset)

  # X-bar and R Chart OR ImR Chart
  if(options[["controlChartsType"]] == "xbarR"){
    .qcXbarAndRContainer(options, dataset, ready, jaspResults, measurements = measurements, subgroups = subgroups)
  }else{
    .qcImRChart(options, dataset, ready, jaspResults, measurements)
  }

  # Distribution plot
  .qcDistributionPlot(options, dataset, ready, jaspResults, measurements = measurements)

  # Probability plots section
  .qcProbabilityPlotContainer(options, dataset, ready, jaspResults, measurements = measurements)

  # Perform capability analysis
  .qcCapabilityAnalysis(options, dataset, ready, jaspResults, measurements = measurements)

  # Report
  if (options[["pcReportDisplay"]]) {
    if (is.null(jaspResults[["pcReport"]])) {
      jaspResults[["pcReport"]] <- createJaspContainer(gettext("Report"))
      jaspResults[["pcReport"]]$position <- 6
    }
    jaspResults[["pcReport"]] <- .pcReport(dataset, measurements, parts, operators, options, ready, jaspResults)
    jaspResults[["pcReport"]]$dependOn(c('pcReportDisplay'))
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
  container$dependOn(options = c("CapabilityStudyType", "variables", "subgroups", "lowerSpecification", "upperSpecification", "targetValue", "variablesLong", "pcSubgroupSize", "pcDataFormat",
                                 "CapabilityStudyPlot", "CapabilityStudyTables", "manualSubgroupSize"))
  container$position <- 4


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

    title <- gettext("Process Capability of Measurements")

    childContainer <- createJaspContainer(title)
    childContainer$position <- 1
    container[["normalCapabilityAnalysis"]] <- childContainer

    .qcProcessSummaryTable(options, dataset, ready, childContainer, measurements)

    if (options[["CapabilityStudyPlot"]])
      .qcProcessCapabilityPlot(options, dataset, ready, childContainer, measurements, distribution = 'normal')
    if (options[["CapabilityStudyTables"]]){
      .qcProcessCapabilityTableWithin(options, dataset, ready, childContainer, measurements)
      .qcProcessCapabilityTableOverall(options, dataset, ready, childContainer, measurements)
    }

  }

  if (options[["capabilityStudyType"]] == "nonnormalCapabilityAnalysis") {

    childContainer2 <- createJaspContainer(gettext("Process Capability of Measurements (Non-Normal Capability Study)"))
    childContainer2$position <- 2

    container[["nonNormalCapabilityAnalysis"]] <- childContainer2
    if (options[["CapabilityStudyPlot"]])
      .qcProcessCapabilityPlot(options, dataset, ready, childContainer2, measurements, distribution = options[["nonNormalDist"]])
    if (options[["CapabilityStudyTables"]])
      .qcProcessCapabilityTableNonNormal(options, dataset, ready, childContainer2, measurements)
  }
}

################
## Output ######
################

.qcProcessSummaryTable <- function(options, dataset, ready, container, measurements, returnDataframe = FALSE) {

  table <- createJaspTable(title = gettext("Process Summary"))
  table$position <- 1

  if (options[["lowerSpecificationField"]]){
    table$addColumnInfo(name = "lsl", type = "number", title = gettext("LSL"))
  }
  if (options[["targetValueField"]])
    table$addColumnInfo(name = "target", type = "number", title = gettext("Target"))
  if (options[["upperSpecificationField"]])
    table$addColumnInfo(name = "usl", type = "number", title = gettext("USL"))

  table$addColumnInfo(name = "n", type = "integer", title = gettext("Sample size"))
  table$addColumnInfo(name = "mean", type = "number", title = gettext("Mean"))
  table$addColumnInfo(name = "sd", type = "number", title = gettext("Std. Deviation (Total)"))
  table$addColumnInfo(name = "sdw", type = "number", title = gettext("Std. Deviation (Within)"))

  table$showSpecifiedColumnsOnly <- TRUE

  container[["processSummaryTable"]] <- table

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
                     values = valueVector)
    return(df)
  }


}

.qcProcessCapabilityPlot <- function(options, dataset, ready, container, measurements, returnPlotObject = FALSE, distribution = c('normal', 'Weibull',
                                                                                                                                  'Lognormal', '3lognormal', '3weibull')) {

  plot <- createJaspPlot(title = gettext("Capability of the Process"), width = 700, height = 400)
  plot$dependOn(c("csBinWidthType", "csNumberOfBins"))
  plot$position <- 2
  container[["capabilityPlot"]] <- plot

  if(!options[["upperSpecificationField"]] && !options[["lowerSpecificationField"]]){
    plot$setError(gettext("No specification limits set."))
    return()
  }

  if (!ready){
    return()
  }

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

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(plotData[["x"]], options[["lowerSpecification"]], options[["upperSpecification"]]), min.n = 4)
  xLimits <- range(xBreaks)

  binWidthType <- options$csBinWidthType

  if (binWidthType == "doane") {  # https://en.wikipedia.org/wiki/Histogram#Doane's_formula
    sigma.g1 <- sqrt((6*(length(allData) - 2)) / ((length(allData) + 1)*(length(allData) + 3)))
    g1 <- mean(abs(allData)^3)
    k <- 1 + log2(length(allData)) + log2(1 + (g1 / sigma.g1))
    binWidthType <- k
  } else if (binWidthType == "manual") {
    binWidthType <- options$csNumberOfBins
  }

  # } else if (binWidthType == "fd" && nclass.FD(variable) > 10000) { # FD-method will produce extreme number of bins and crash ggplot, mention this in footnote
  #   binWidthType <- 10000

  h <- hist(allData, plot = F, breaks = binWidthType)
  binWidth <- (h$breaks[2] - h$breaks[1])

  p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x)) +
    ggplot2::scale_y_continuous(name = gettext("Density")) +
    ggplot2::geom_histogram(ggplot2::aes(y =..density..), fill = "grey", col = "black", size = .7, binwidth = binWidth, center = binWidth/2) +
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
    p <- p + ggplot2::geom_vline(xintercept = options[["targetValue"]], linetype = "dotted", color = "darkgreen")
  if (options[["lowerSpecificationField"]])
    p <- p + ggplot2::geom_vline(xintercept = options[["lowerSpecification"]], linetype = "dotted", color = "darkred")
  if (options[["upperSpecificationField"]])
    p <- p + ggplot2::geom_vline(xintercept = options[["upperSpecification"]], linetype = "dotted", color = "darkred")

  p <- jaspGraphs::themeJasp(p) + ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())

  if(returnPlotObject)
    return(p)

  plot$plotObject <- p
}

.qcProcessCapabilityTableWithin <- function(options, dataset, ready, container, measurements, returnDataframe = FALSE) {

  if (!options[["lowerSpecificationField"]] && !options[["upperSpecificationField"]])
    return()

  if (!ready)
    return()

  table <- createJaspTable(title = gettext("Potential Capability (Within)"))
  sourceVector <- vector()

  ciLevel <- options[["csConfidenceIntervalPercent"]]
  ciLevelPercent <- ciLevel * 100

  if (options[["lowerSpecificationField"]] && options[["upperSpecificationField"]]) {
    table$addColumnInfo(name = "cp",    type = "number", title = gettext("Cp"))
    sourceVector <- c(sourceVector, 'Cp')
    if (options[["csConfidenceInterval"]]){
      table$addColumnInfo(name = "cplci", title = gettext("Lower"), type = "number", overtitle = gettextf("%s CI for Cp", paste(ciLevelPercent, "%")))
      table$addColumnInfo(name = "cpuci", title = gettext("Upper"), type = "number", overtitle = gettextf("%s CI for Cp", paste(ciLevelPercent, "%")))
    }

  if (options[["lowerSpecificationField"]]){
    table$addColumnInfo(name = "cpl",   type = "number", title = gettext("CPL"))
    sourceVector <- c(sourceVector, 'CPL')
  }
  if (options[["upperSpecificationField"]]){
    table$addColumnInfo(name = "cpu",   type = "number", title = gettext("CPU"))
    sourceVector <- c(sourceVector, 'CPU')
  }

  }
  table$addColumnInfo(name = "cpk",   type = "number", title = gettext("Cpk"))
  sourceVector <- c(sourceVector, 'Cpk')
  if (options[["csConfidenceInterval"]]){
    table$addColumnInfo(name = "cpklci", title = gettext("Lower"), type = "number", overtitle = gettextf("%s CI for Cpk", paste(ciLevelPercent, "%")))
    table$addColumnInfo(name = "cpkuci", title = gettext("Upper"), type = "number", overtitle = gettextf("%s CI for Cpk", paste(ciLevelPercent, "%")))
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

   rows <- list("cp" = cp, "cpl" = cpl, "cpu" = cpu, "cpk" = cpk)

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

    rows[["cplci"]] <- ciLbCp
    rows[["cpuci"]] <- ciUbCp
    rows[["cpklci"]] <- ciLbCpk
    rows[["cpkuci"]] <- ciUbCpk
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
                     values = valueVector)
    return(df)
  }

  container[["capabilityTableWithin"]] <- table
}

.qcProcessCapabilityTableOverall <- function(options, dataset, ready, container, measurements, returnOverallCapDataframe = FALSE,
                                             returnPerformanceDataframe = FALSE) {

  if (!ready)
    return()

  table <- createJaspTable(title = gettext("Overall Capability"))
  sourceVector1 <- vector()

  ciLevel <- options[["csConfidenceIntervalPercent"]]
  ciLevelPercent <- ciLevel * 100
  ciAlpha <- 1 - ciLevel

  if (options[["lowerSpecificationField"]]){
    table$addColumnInfo(name = "ppl", type = "number", title = gettext("PPL"))
    sourceVector1 <- c(sourceVector1, 'PPL')
  }
  if (options[["upperSpecificationField"]]){
    table$addColumnInfo(name = "ppu", type = "number", title = gettext("PPU"))
    sourceVector1 <- c(sourceVector1, 'PPU')
  }
  if (options[["lowerSpecificationField"]] && options[["upperSpecificationField"]]){
    table$addColumnInfo(name = "pp",  type = "number", title = gettext("Pp"))
    sourceVector1 <- c(sourceVector1, 'Pp')
    if (options[["csConfidenceInterval"]]){
      table$addColumnInfo(name = "pplci", title = gettext("Lower"), type = "number", overtitle = gettextf("%s CI for Pp", paste(ciLevelPercent, "%")))
      table$addColumnInfo(name = "ppuci", title = gettext("Upper"), type = "number", overtitle = gettextf("%s CI for Pp", paste(ciLevelPercent, "%")))
    }
  }
  table$addColumnInfo(name = "ppk",   type = "number", title = gettext("Ppk"))
  sourceVector1 <- c(sourceVector1, 'Ppk')
  if (options[["csConfidenceInterval"]]){
    table$addColumnInfo(name = "ppklci", title = gettext("Lower"), type = "number", overtitle = gettextf("%s CI for Ppk", paste(ciLevelPercent, "%")))
    table$addColumnInfo(name = "ppkuci", title = gettext("Upper"), type = "number", overtitle = gettextf("%s CI for Ppk", paste(ciLevelPercent, "%")))
  }
  if (options[["targetValueField"]]){
    table$addColumnInfo(name = "cpm", type = "number", title = gettext("Cpm"))
    sourceVector1 <- c(sourceVector1, 'Cpm')
    if (options[["csConfidenceInterval"]]){
      table$addColumnInfo(name = "cpmlci", title = gettext("Lower"), type = "number", overtitle = gettextf("%s CI for Cpm", paste(ciLevelPercent, "%")))
      table$addColumnInfo(name = "cpmuci", title = gettext("Upper"), type = "number", overtitle = gettextf("%s CI for Cpm", paste(ciLevelPercent, "%")))
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

  rows <- list("pp" = pp, "ppl" = ppl, "ppu" = ppu, "ppk" = ppk)
  if (options[["targetValueField"]])
    rows[["cpm"]] <- cpm

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



    rows[["pplci"]] <- ciLbPp
    rows[["ppuci"]] <- ciUbPp
    rows[["ppklci"]] <- ciLbPpk
    rows[["ppkuci"]] <- ciUbPpk

    if (options[["targetValueField"]]){

      #CI for Cpm
      a <- (meanOverall - t) / sdo
      dfCpm <- (n * ((1 + (a^2))^2)) / (1 + (2 * (a^2)))
      ciLbCpm <- cpm * sqrt( qchisq(p = ciAlpha/2, df = dfCpm) /dfCpm)
      ciUbCpm <- cpm * sqrt(qchisq(p = 1 - (ciAlpha/2), df = dfCpm) / dfCpm)


      rows[["cpmlci"]] <- ciLbCpm
      rows[["cpmuci"]] <- ciUbCpm

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
    valueVector1 <- na.omit(c(ppl, ppu, pp, ppk, cpm))
    df <- data.frame(sources = sourceVector1,
                     values = valueVector1)
    return(df)
  }

  table2 <- createJaspTable(title = gettext("Performance"))
  table2$addColumnInfo(name = "rowNames", type = "string", title = "")
  table2$addColumnInfo(name = "observed", type = "number", title = "Observed")
  table2$addColumnInfo(name = "expOverall", type = "number", title = "Expected overall")
  table2$addColumnInfo(name = "expWithin", type = "number", title = "Expected within")

  table2$showSpecifiedColumnsOnly <- TRUE


  #Calculate performance
  allDataVector <- as.vector(allData)
  rowNames <- c("PPM < LSL", "PPM > USL", "PPM total")

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

  if(returnPerformanceDataframe){
    df <- data.frame("Source" = rowNames,
                     "Observed" = observed,
                     "Expected Overall" = expOverall,
                     "Expected Within"  = expWithin)
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

.qcProcessCapabilityTableNonNormal <- function(options, dataset, ready, container, measurements) {

  table <- createJaspTable(title = gettextf("Process Summary"))

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

  if (options[["lowerSpecificationField"]])
    table$addColumnInfo(name = "lsl", type = "number", title = gettext("LSL"))
  if (options[["upperSpecificationField"]])
    table$addColumnInfo(name = "usl", type = "number", title = gettext("USL"))
  if (options[["targetValueField"]])
    table$addColumnInfo(name = "target", type = "number", title = gettext("Target"))

  table$addColumnInfo(name = "n", type = "integer", title = gettext("Sample size"))
  table$addColumnInfo(name = "mean", type = "number", title = gettext("Mean"))
  table$addColumnInfo(name = "sd", type = "number", title = gettext("Std. Deviation"))
  table$addColumnInfo(name = "beta", type = "number", title = gettextf("%1$s", "\u03B2"))
  table$addColumnInfo(name = "theta", type = "number", title = gettextf("%1$s", "\u03B8"))
  if(options[["nonNormalDist"]] == "3lognormal" | options[["nonNormalDist"]] == "3weibull")
    table$addColumnInfo(name = "threshold", type = "number", title = gettext('Threshold'))


  table$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  allData <- unlist(dataset[, measurements])
  n <- length(allData)
  lsl <- options[["lowerSpecification"]]
  usl <- options[["upperSpecification"]]

  distParameters <- .distributionParameters(data = allData, distribution = options[["nonNormalDist"]])
  beta <- distParameters$beta
  theta <- distParameters$theta

  rows <- list("n" = n,"mean" = mean(allData), "sd" = sd(allData), "lsl" = options[["lowerSpecification"]],
               "usl" = options[["upperSpecification"]], "target" = options[["targetValue"]], "beta" = beta, "theta" = theta)
  if(options[["nonNormalDist"]] == "3lognormal" | options[["nonNormalDist"]] == "3weibull"){
    threshold <- distParameters$threshold
    rows[['threshold']] <- threshold
  }

  table$addRows(rows)

  container[["summaryTableNonNormal"]] <- table


  table2 <- createJaspTable(title = gettextf("Overall Capability"))

  if (options[["upperSpecificationField"]] && options[["lowerSpecificationField"]])
    table2$addColumnInfo(name = "pp", type = "number", title = gettext("Pp"))
  if (options[["lowerSpecificationField"]])
    table2$addColumnInfo(name = "ppl", type = "number", title = gettext("PPL"))
  if (options[["upperSpecificationField"]])
    table2$addColumnInfo(name = "ppu", type = "number", title = gettext("PPU"))
  table2$addColumnInfo(name = "ppk", type = "number", title = gettext("Ppk"))

  table2data <- list()

  if (options[["nonNormalDist"]] == "Lognormal") {
    if (options[["lowerSpecificationField"]]){
      p1 <- plnorm(q = lsl, meanlog = beta, sdlog = theta, lower.tail = T)
      zLSL <- qnorm(p1)
      ppl <- -zLSL/3
      table2data[["ppl"]] <- ppl
    }else{
      ppl <- NA
    }
    if (options[["upperSpecificationField"]]){
      p2 <- plnorm(q = usl,  meanlog = beta, sdlog = theta)
      zUSL <- qnorm(p2)
      ppu <- zUSL/3
      table2data[["ppu"]] <- ppu
    }else{
      ppu <- NA
    }
  }else if (options[["nonNormalDist"]] == "Weibull") {
    if (options[["lowerSpecificationField"]]){
      p1 <- pweibull(q = lsl, shape = beta, scale = theta, lower.tail = T)
      zLSL <- qnorm(p1)
      ppl <- -zLSL/3
      table2data[["ppl"]] <- ppl
    }else{
      ppl <- NA
    }
    if (options[["upperSpecificationField"]]){
      p2 <- pweibull(q = usl, shape = beta, scale = theta)
      zUSL <- qnorm(p2)
      ppu <- zUSL/3
      table2data[["ppu"]] <- ppu
    }else{
      ppu <- NA
    }
  }else if (options[["nonNormalDist"]] == "3lognormal") {
    if (options[["lowerSpecificationField"]]){
      p1 <- FAdist::plnorm3(q = lsl, shape = theta, scale = beta, thres = threshold, lower.tail = T)
      zLSL <- qnorm(p1)
      ppl <- -zLSL/3
      table2data[["ppl"]] <- ppl
    }else{
      ppl <- NA
    }
    if (options[["upperSpecificationField"]]){
      p2 <- FAdist::plnorm3(q = usl, shape = theta, scale = beta, thres = threshold)
      zUSL <- qnorm(p2)
      ppu <- zUSL/3
      table2data[["ppu"]] <- ppu
    }else{
      ppu <- NA
    }
  }else if (options[["nonNormalDist"]] == "3weibull") {
    if (options[["lowerSpecificationField"]]){
      p1 <- FAdist::pweibull3(q = lsl, shape = beta, scale = theta, thres = threshold, lower.tail = T)
      zLSL <- qnorm(p1)
      ppl <- -zLSL/3
      table2data[["ppl"]] <- ppl
    }else{
      ppl <- NA
    }
    if (options[["upperSpecificationField"]]){
      p2 <- FAdist::pweibull3(q = usl, shape = beta, scale = theta, thres = threshold)
      zUSL <- qnorm(p2)
      ppu <- zUSL/3
      table2data[["ppu"]] <- ppu
    }else{
      ppu <- NA
    }
  }

  if (options[["upperSpecificationField"]] && options[["lowerSpecificationField"]]){
    pp <- (zUSL - zLSL)/6
    table2data[["pp"]] <- pp
  }
  ppk <- min(c(ppl, ppu), na.rm = T)
  table2data[["ppk"]] <- ppk

  table2$setData(table2data)
  container[["overallCapabilityNonNormal"]] <- table2

  table3 <- createJaspTable(title = gettextf("Performance"))

  table3$addColumnInfo(name = "rowNames", type = "string", title = gettext(""))
  table3$addColumnInfo(name = "observed", type = "number", title = gettext("Observed"))
  table3$addColumnInfo(name = "expOverall", type = "number", title = gettext("Expected overall"))


  allDataVector <- as.vector(allData)
  rowNames <- c("PPM < LSL", "PPM > USL", "PPM total")

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
  container$dependOn(options = c("variables", "probabilityPlot", "rank", "nullDistribution", "addGridlines", "variablesLong", "pcSubgroupSize"))
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

  table$addColumnInfo(name = "n",      	title = gettext("n"),  		type = "integer")

  if (options[["nullDistribution"]] == 'Normal') {
    table$addColumnInfo(name = "mean",  title = gettextf("Mean (%1$s)", "\u03BC"), 				type = "number")
    table$addColumnInfo(name = "sd",    title = gettextf("Std. deviation (%1$s)", "\u03C3"), 	type = "number")
  } else if (options[["nullDistribution"]] == 'Lognormal') {
    table$addColumnInfo(name = "mean",  title = gettextf("Location (%1$s)", "\u03BC"),  		type = "number")
    table$addColumnInfo(name = "sd",    title = gettextf("Scale (%1$s)", "\u03C3"), 			type = "number")
  } else if (options[["nullDistribution"]] == 'Weibull') {
    table$addColumnInfo(name = "mean",  title = gettextf("Shape (%1$s)", "\u03BB"), 			type = "number")
    table$addColumnInfo(name = "sd",    title = gettext("Scale (<i>k</i>)"),        			type = "number")
  }

  table$addColumnInfo(name = "ad",     	title = gettext("<i>A</i>"), type = "number")
  table$addColumnInfo(name = "p",		title = gettext("<i>p</i>"), type = "pvalue")

  table$addFootnote(gettextf("The Anderson-Darling statistic <i>A</i> is calculated against the %2$s distribution.", "\u00B2", options[["nullDistribution"]]))

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
  p      <- test$p.value

  row <- list(mean = meanx, sd = sdx, n = n, ad = ad, p = p)
  table$addRows(row)


  container[["probabilityTable"]] <- table
}

.qcProbabilityPlot <- function(dataset, options, measurements) {

  plot <- createJaspPlot(width = 400, aspectRatio = 1, title = "Probability Plot")
  plot$dependOn(c("variablesLong", "pcSubgroupSize"))

  if (((options[["nullDistribution"]] == 'Lognormal') || options[["nullDistribution"]] == 'Weibull') && any(dataset[measurements] < 0)){
    plot$setError(gettext("Dataset contains negative numbers. Not compatible with the selected distribution."))
    return(plot)
  }

  # Arrange data
  x <- as.vector(unlist(dataset[measurements]))
  x <- x[order(x)]
  n <- length(x)
  i <- rank(x)

  # Method for rank
  Rank_funs <- matrix(list(.qcPpMedian, .qcPpMean, .qcPpKmModif, .qcPpKm), ncol = 1,
                      dimnames = list(c("Bernard", "Herd-Johnson", "Hazen", 'Kaplan-Meier'), c("p")), byrow = TRUE)
  rankByUser <- options[["rank"]]
  p <- Rank_funs[[rankByUser, 'p']](x)

  # Functions for computing y
  y_funs <- matrix(list(qnorm,qnorm,.qcWeibull), ncol = 1,
                   dimnames = list(c('Normal', 'Lognormal', 'Weibull'), c('y')), byrow = TRUE)
  DisByUser <- options[["nullDistribution"]]
  y <- y_funs[[DisByUser, 'y']](p)

  data1 <- data.frame(x = x, y = y)

  # Quantities
  pSeq <- seq(0.001, 0.999, 0.001)
  ticks <- c(0.1, 1, 5, seq(10, 90, 10), 95, 99, 99.9)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(x)
  xLimits <- range(xBreaks)

  # Computing according to the distribution
  if (options[["nullDistribution"]] == 'Normal') {

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

  }
  yLimits <- range(yBreaks)
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileEstimate)) +
    ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileLower), col = "darkred", linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileUpper), col = "darkred", linetype = "dashed") +
    jaspGraphs::geom_point(ggplot2::aes(x = data1[["x"]], y = data1[["y"]])) +
    ggplot2::scale_x_continuous("Measurement", breaks = xBreaks, limits = xLimits) +
    ggplot2::scale_y_continuous('Percent', labels = ticks, breaks = yBreaks, limits = yLimits)

  p <- jaspGraphs::themeJasp(p)

  if (options[["addGridlines"]])
    p <- p + ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "lightgray"))

  plot$plotObject <- p

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

  if (!options[["histogram"]] || !is.null(jaspResults[["histogram"]]))
    return()

  plot <- createJaspPlot(title = gettext("Histogram"), width = 400, height = 400)
  plot$dependOn(options = c("histogram", "displayDensity", "variables", "pcNumberOfBins", "pcBinWidthType", "variablesLong", "pcSubgroupSize", "manualSubgroupSize", "subgroups"))
  plot$position <- 2

  jaspResults[["histogram"]] <- plot

  if (!ready)
    return()

  data <- unlist(dataset[measurements])

  binWidthType <- options$pcBinWidthType

  if (binWidthType == "doane") {  # https://en.wikipedia.org/wiki/Histogram#Doane's_formula
    sigma.g1 <- sqrt((6*(length(data) - 2)) / ((length(data) + 1)*(length(data) + 3)))
    g1 <- mean(abs(data)^3)
    k <- 1 + log2(length(data)) + log2(1 + (g1 / sigma.g1))
    binWidthType <- k
  } else if (binWidthType == "manual") {
    binWidthType <- options$pcNumberOfBins
  }

  # } else if (binWidthType == "fd" && nclass.FD(variable) > 10000) { # FD-method will produce extreme number of bins and crash ggplot, mention this in footnote
  #   binWidthType <- 10000

  n <- length(data)
  df <- data.frame(measurements = data)
  h <- hist(data, plot = F, breaks = binWidthType)
  binWidth <- (h$breaks[2] - h$breaks[1])
  freqs <- h$counts
  yLabels <- jaspGraphs::getPrettyAxisBreaks(c(0, freqs))
  yBreaks <- yLabels / (n * binWidth)
  yLimits <- range(yBreaks)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(h$breaks, data), min.n = 4)
  xLimits <- range(xBreaks)

  p <- ggplot2::ggplot() + ggplot2::geom_histogram(data = df, mapping = ggplot2::aes(y =..density.., x = measurements), fill = "grey", col = "black", size = .7, binwidth = binWidth, center = binWidth/2) +
    ggplot2::scale_x_continuous(name = gettext("Measurement"), breaks = xBreaks, limits = xLimits) +
    ggplot2::scale_y_continuous(name =  gettext("Counts"), labels = yLabels, breaks = yBreaks, limits = yLimits) +
    jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe()


  if (options[["displayDensity"]]) {
    p <- p + ggplot2::stat_function(fun = dnorm, color = "dodgerblue", args = list(mean = mean(data), sd = sd(data)))
  }

  plot$plotObject <- p
}


.qcImRChart<- function(options, dataset, ready, jaspResults, measurements){
  container <- createJaspContainer(title = gettext("Control Chart"))
  container$dependOn(options = c("controlChartsType", "variables", "subgroups", "variablesLong", "pcSubgroupSize"))
  container$position <- 1
  jaspResults[["ImR Charts"]] <- container

  for(measurement in measurements){
    container[[measurement]] <- .IMRchart(dataset, options, variable = measurement, cowPlot = TRUE)$p
  }

}

.PClongTowide<- function(dataset, k, measurements, mode = c("manual", "subgroups")){
  if(mode == "manual"){
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


.pcReport <- function(dataset, measurements, parts, operators, options, ready, container){

  if (options[["pcReportTitle"]] == ""){
    title <- "Measurement"
  }else{
    title <- options[["pcReportTitle"]]
  }
  name <- gettextf("Process name: %s", options[["pcReportName"]])
  date <- gettextf("Date of study: %s", options[["pcReportDate"]])
  text1 <- c(name, date)

  reportedBy <- gettextf("Reported by: %s", options[["pcReportReportedBy"]])
  misc <- gettextf("Misc: %s", options[["pcReportMisc"]])
  text2 <- c(reportedBy, misc)


  processSummaryDF <- .qcProcessSummaryTable(options, dataset, ready, container, measurements, returnDataframe = TRUE)
  potentialWithinDF <- .qcProcessCapabilityTableWithin(options, dataset, ready, container, measurements, returnDataframe = TRUE)
  overallCapDF <- .qcProcessCapabilityTableOverall(options, dataset, ready, container, measurements, returnOverallCapDataframe = TRUE)
  performanceDF <- .qcProcessCapabilityTableOverall(options, dataset, ready, container, measurements, returnPerformanceDataframe = TRUE)

  matrixPlot <- createJaspPlot(title = gettext("Report"), width = 1200, height = 1000)

  if(!options[["upperSpecificationField"]] && !options[["lowerSpecificationField"]]){
    matrixPlot$setError(gettext("No specification limits set."))
    return(matrixPlot)
  }

  if(!ready)
    return(matrixPlot)

  plotMat <- matrix(list(), 4, 2)
  plotMat[[1, 1]] <- .ggplotWithText(text1)
  plotMat[[1, 2]] <- .ggplotWithText(text2)
  plotMat[[2, 1]] <- ggplotTable(processSummaryDF) #process summary
  plotMat[[2, 2]] <- .qcProcessCapabilityPlot(options, dataset, ready, container, measurements, returnPlotObject = TRUE)
  plotMat[[3, 1]] <- ggplotTable(performanceDF, displayColNames = TRUE)   # performance
  plotMat[[3, 2]] <- ggplotTable(potentialWithinDF)  #Potential within
  plotMat[[4, 1]] <- ggplot2::ggplot() + ggplot2::theme_void()
  plotMat[[4, 2]] <- ggplotTable(overallCapDF) #overall capability

  p <- jaspGraphs::ggMatrixPlot(plotMat, topLabels = c(gettextf("Process Capability Report for %s", title), ""))
  matrixPlot$plotObject <- p

  return(matrixPlot)
}

ggplotTable <- function(dataframe, displayColNames = FALSE){
  df <- tibble::tibble(dataframe)
  p <- ggplot2::ggplot() + ggplot2::theme_void() + ggpp::geom_table(data = data.frame(x = 1, y = 1), ggplot2::aes(x = x, y = y), label = list(df),
                                                                    table.colnames = displayColNames, size = 7)

  return(p)
}

.distributionParameters <- function(data, distribution = c('Lognormal', 'Weibull', '3lognormal', '3weibull')){
  if (distribution == "Lognormal") {
    beta <- fitdistrplus::fitdist(data, 'lnorm')$estimate[[1]]
    theta <- fitdistrplus::fitdist(data, 'lnorm')$estimate[[2]]
  } else if (distribution == "Weibull") {
    beta    <- mixdist::weibullpar(mu = mean(data), sigma = sd(data), loc = 0)$shape
    theta   <- mixdist::weibullpar(mu = mean(data), sigma = sd(data), loc = 0)$scale
  }else if(distribution == "3lognormal"){
    beta <- EnvStats::elnorm3(data)$parameters[[1]]
    theta <- EnvStats::elnorm3(data)$parameters[[2]]
    threshold <- EnvStats::elnorm3(data)$parameters[[3]]
  }else if(distribution == "3weibull"){
    beta <- weibullness::weibull.mle(data)[[1]]
    theta <- weibullness::weibull.mle(data)[[2]]
    threshold <- as.vector(weibullness::weibull.mle(data)[[3]])
  }
  list <- list(beta = beta,
               theta = theta)
  if(distribution == '3weibull' | distribution == '3lognormal')
    list['threshold'] <- threshold
  return(list)
}

