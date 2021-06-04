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

  measurements <- unlist(options$variables)
  subgroups <- unlist(options$subgroups)

  # Preparatory work
  dataset <- .qcReadData(dataset, options, type = "capabilityStudy")

  # Check if analysis is ready
  ready <- .qcOptionsReady(options, type = "capabilityStudy")

  # X-bar and R Chart
  .qcXbarAndRContainer(options, dataset, ready, jaspResults, measurements = measurements, subgroups = subgroups)

  # Distribution plot
  .qcDistributionPlot(options, dataset, ready, jaspResults, measurements = measurements)

  # Probability plots section
  .qcProbabilityPlotContainer(options, dataset, ready, jaspResults)

  # Perform capability analysis
  .qcCapabilityAnalysis(options, dataset, ready, jaspResults)
}

#############################################################
## Functions for capability analysis section ################
#############################################################

################
## Containers ##
################

.qcCapabilityAnalysis <- function(options, dataset, ready, jaspResults) {

  container <- createJaspContainer(gettext("Capability Studies"))
  container$dependOn(options = c("normalCapabilityStudy", "capabilityStudy", "nonNormalCapabilityStudy", "variables", "subgroups", "lowerSpecification", "upperSpecification", "targetValue"))
  container$position <- 4

  ready <- length(options[["variables"]]) > 1L

  jaspResults[["capabilityAnalysis"]] <- container

  if (options[["normalCapabilityStudy"]]) {

    if (options[["capabilityStudy"]] == "initialCapabilityAnalysis")
      title <- gettext("Process Capability of Measurements (Initial Capability Study)")
    if (options[["capabilityStudy"]] == "followupCapabilityAnalysis")
      title <- gettext("Process Capability of Measurements (Follow-Up Capability Study)")

    childContainer <- createJaspContainer(title)
    childContainer$position <- 1
    container[["normalCapabilityAnalysis"]] <- childContainer

    .qcProcessSummaryTable(options, dataset, ready, childContainer)
    .qcProcessCapabilityPlot(options, dataset, ready, childContainer)
    .qcProcessCapabilityTableWithin(options, dataset, ready, childContainer)
    .qcProcessCapabilityTableOverall(options, dataset, ready, childContainer)

  }

  if (options[["nonNormalCapabilityStudy"]]) {

    childContainer2 <- createJaspContainer(gettext("Process Capability of Measurements (Non-Normal Capability Study)"))
    childContainer2$position <- 2
    container[["nonNormalCapabilityAnalysis"]] <- childContainer2

    .qcProcessCapabilityTableNonNormal(options, dataset, ready, childContainer2)
  }
}

################
## Output ######
################

.qcProcessSummaryTable <- function(options, dataset, ready, container) {

  table <- createJaspTable(title = gettext("Process Summary"))
  table$position <- 1

  if (options[["lowerSpecificationField"]])
    table$addColumnInfo(name = "lsl", type = "number", title = gettext("LSL"))
  if (options[["targetValueField"]])
    table$addColumnInfo(name = "target", type = "number", title = gettext("Target"))
  if (options[["upperSpecificationField"]])
    table$addColumnInfo(name = "usl", type = "number", title = gettext("USL"))

  table$addColumnInfo(name = "n", type = "integer", title = gettext("Measurements"))
  table$addColumnInfo(name = "mean", type = "number", title = gettext("Mean"))
  table$addColumnInfo(name = "sd", type = "number", title = gettext("Std. Deviation (Total)"))
  table$addColumnInfo(name = "sdw", type = "number", title = gettext("Std. Deviation (Within)"))

  table$showSpecifiedColumnsOnly <- TRUE

  container[["processSummaryTable"]] <- table

  if (!ready)
    return()

  # Take a look at this input! Is is supposed to be like this or must it be transposed?
  # Transposed gives NA often as std.dev
  qccFit <- qcc::qcc(as.data.frame(dataset[, options[["variables"]]]), type = 'R', plot = FALSE)
  allData <- unlist(dataset[, options[["variables"]]])

  if (is.na(qccFit[["std.dev"]]))
    table$addFootnote(gettext("The within standard deviation could not be calculated."))

  rows <- list(
    "lsl"    = options[["lowerSpecification"]],
    "target" = options[["targetValue"]],
    "usl"    = options[["upperSpecification"]],
    "mean"   = mean(allData, na.rm = TRUE),
    "n"      = nrow(dataset),
    "sd"     = sd(allData, na.rm = TRUE),
    "sdw"    = qccFit[["std.dev"]]
  )
  table$addRows(rows)
}

.qcProcessCapabilityPlot <- function(options, dataset, ready, container) {

  plot <- createJaspPlot(title = gettext("Capability of the Process"), width = 600, height = 300)
  plot$position <- 2
  container[["capabilityPlot"]] <- plot

  if (!ready)
    return()

  # Take a look at this input! Is is supposed to be like this or must it be transposed?
  # Transposed gives NA often as std.dev
  qccFit <- qcc::qcc(as.data.frame(dataset[, options[["variables"]]]), type = 'R', plot = FALSE)
  allData <- unlist(dataset[, options[["variables"]]])
  plotData <- data.frame(x = allData)

  sdw <- qccFit[["std.dev"]]
  sdo <- sd(allData, na.rm = TRUE)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(plotData[["x"]], options[["lowerSpecification"]], options[["upperSpecification"]]), min.n = 4)

  p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x)) +
    ggplot2::scale_x_continuous(name = gettext("Measurements"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext("Density")) +
    ggplot2::geom_histogram(ggplot2::aes(y =..density..), fill = "grey", col = "black", size = .7) +
    ggplot2::stat_function(fun = dnorm, args = list(mean = mean(allData), sd = sd(allData)), color = "dodgerblue") +
    ggplot2::stat_function(fun = dnorm, args = list(mean = mean(allData), sd = sdw), color = "red")

  if (options[["targetValueField"]])
    p <- p + ggplot2::geom_vline(xintercept = options[["targetValue"]], linetype = "dotted", color = "darkgreen")
  if (options[["lowerSpecificationField"]])
    p <- p + ggplot2::geom_vline(xintercept = options[["lowerSpecification"]], linetype = "dotted", color = "darkred")
  if (options[["upperSpecificationField"]])
    p <- p + ggplot2::geom_vline(xintercept = options[["upperSpecification"]], linetype = "dotted", color = "darkred")

  p <- jaspGraphs::themeJasp(p) + ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())

  plot$plotObject <- p
}

.qcProcessCapabilityTableWithin <- function(options, dataset, ready, container) {

  if (!options[["lowerSpecificationField"]] && !options[["upperSpecificationField"]])
    return()

  table <- createJaspTable(title = gettext("Process Capability (Within)"))

  if (options[["lowerSpecificationField"]])
    table$addColumnInfo(name = "cpl",   type = "number", title = gettext("CPL"))
  if (options[["upperSpecificationField"]])
    table$addColumnInfo(name = "cpu",   type = "number", title = gettext("CPU"))
  if (options[["lowerSpecificationField"]] && options[["upperSpecificationField"]]) {
    table$addColumnInfo(name = "cp",    type = "number", title = gettext("Cp"))
    table$addColumnInfo(name = "cpk",   type = "number", title = gettext("Cpk"))
    table$addColumnInfo(name = "z",     type = "number", title = gettext("ppm"))
  }

  table$showSpecifiedColumnsOnly <- TRUE

  container[["capabilityTableWithin"]] <- table

  if (!ready)
    return()

  # Take a look at this input! Is is supposed to be like this or must it be transposed?
  # Transposed gives NA often as std.dev
  qccFit <- qcc::qcc(as.data.frame(dataset[, options[["variables"]]]), type = 'R', plot = FALSE)
  allData <- unlist(dataset[, options[["variables"]]])

  # Calculate capability indices
  cp <- (options[["upperSpecification"]] - options[["lowerSpecification"]]) / (6 * qccFit[["std.dev"]])
  cpl <- (mean(allData) - options[["lowerSpecification"]]) / (3 * qccFit[["std.dev"]])
  cpu <- (options[["upperSpecification"]] - mean(allData)) / (3 * qccFit[["std.dev"]])
  cpk <- min(cpu, cpl)
  z <- cpk * 3

  rows <- list("cp" = cp, "cpl" = cpl, "cpu" = cpu, "cpk" = cpk, "z" = z)
  table$addRows(rows)
}

.qcProcessCapabilityTableOverall <- function(options, dataset, ready, container) {

  table <- createJaspTable(title = gettext("Process Performance (Total)"))

  if (options[["lowerSpecificationField"]])
    table$addColumnInfo(name = "ppl", type = "number", title = gettext("PPL"))
  if (options[["upperSpecificationField"]])
    table$addColumnInfo(name = "ppu", type = "number", title = gettext("PPU"))
  if (options[["lowerSpecificationField"]] && options[["upperSpecificationField"]])
    table$addColumnInfo(name = "pp",  type = "number", title = gettext("Pp"))
  table$addColumnInfo(name = "ppk",   type = "number", title = gettext("Ppk"))
  table$addColumnInfo(name = "ppm",   type = "number", title = gettext("ppm"))

  table$showSpecifiedColumnsOnly <- TRUE

  container[["capabilityTableOverall"]] <- table

  if (!ready)
    return()

  # Take a look at this input! Is is supposed to be like this or must it be transposed?
  # Transposed gives NA often as std.dev
  qccFit <- qcc::qcc(as.data.frame(dataset[, options[["variables"]]]), type = 'R', plot = FALSE)
  allData <- unlist(dataset[, options[["variables"]]])
  sdo <- sd(allData)

  pp <- (options[["upperSpecification"]] - options[["lowerSpecification"]]) / (6 * sdo)
  ppl <- (mean(allData) - options[["lowerSpecification"]]) / (3 * sdo)
  ppu <- (options[["upperSpecification"]] - mean(allData)) / (3 * sdo)
  ppk <- min(ppu, ppl)
  cp <- (options[["upperSpecification"]] - options[["lowerSpecification"]]) / (6 * qccFit[["std.dev"]])
  ppm <- cp / sqrt(1 + ((mean(allData) - options[["targetValue"]]) / qccFit[["std.dev"]])^2)

  rows <- list("pp" = pp, "ppl" = ppl, "ppu" = ppu, "ppk" = ppk, "ppm" = ppm)
  table$addRows(rows)
}

.qcProcessCapabilityTableNonNormal <- function(options, dataset, ready, container) {

  table <- createJaspTable(title = gettextf("Process Capability based on the %1$s Distribution", options[["nonNormalDist"]]))

  table$addColumnInfo(name = "mean", type = "number", title = gettext("Mean"))
  table$addColumnInfo(name = "sd", type = "number", title = gettext("Std. Deviation"))

  if (options[["nonNormalDist"]] == "Lognormal") {

    if (options[["lowerSpecificationField"]]) {
      table$addColumnInfo(name = "lsl", type = "number", title = gettext("LSL"))
      table$addColumnInfo(name = "lower", type = "number", title = gettext("P (X < LSL)"))
    }
    if (options[["upperSpecificationField"]]) {
      table$addColumnInfo(name = "usl", type = "number", title = gettext("USL"))
      table$addColumnInfo(name = "upper", type = "number", title = gettext("P (X > USL)"))
    }
    if (options[["lowerSpecificationField"]] && options[["upperSpecificationField"]]) {
      table$addColumnInfo(name = "cpk", type = "number", title = gettext("Cpk"))
    }

  } else if (options[["nonNormalDist"]] == "Weibull") {

    if (options[["lowerSpecificationField"]])
      table$addColumnInfo(name = "lsl", type = "number", title = gettext("LSL"))
    if (options[["upperSpecificationField"]])
      table$addColumnInfo(name = "usl", type = "number", title = gettext("USL"))

    table$addColumnInfo(name = "beta", type = "number", title = gettextf("%1$s", "\u03B2"))
    table$addColumnInfo(name = "theta", type = "number", title = gettextf("%1$s", "\u03B8"))

  }

  table$showSpecifiedColumnsOnly <- TRUE

  container[["capabilityTableNonNormal"]] <- table

  if (!ready)
    return()

  # Take a look at this input! Is is supposed to be like this or must it be transposed?
  # Transposed gives NA often as std.dev
  qccFit <- qcc::qcc(as.data.frame(dataset[, options[["variables"]]]), type = 'R', plot = FALSE)
  allData <- unlist(dataset[, options[["variables"]]])

  if (options[["nonNormalDist"]] == "Lognormal") {

    tau     <- sd(allData) / mean(allData)
    sdLog   <- sqrt(log(tau^2 +1))
    meanLog <- log(mean(allData)) - ((sdLog^2) / 2)
    lower   <- plnorm(q = options[["lowerSpecification"]], meanlog = meanLog, sdlog = sdLog)
    upper   <- 1 - plnorm(q = options[["upperSpecification"]], meanlog = meanLog, sdlog = sdLog)
    cpk     <- 1 - plnorm((max(lower, upper))) / 3

    rows <- list("mean" = mean(allData), "sd" = sd(allData), "lsl" = options[["lowerSpecification"]],
                 "usl" = options[["upperSpecification"]], "cpk" = cpk, "lower" = lower, "upper" = upper)

  } else if (options[["nonNormalDist"]] == "Weibull") {

    beta    <- mixdist::weibullpar(mu = mean(allData), sigma = sd(allData), loc = 0)$shape
    theta   <- mixdist::weibullpar(mu = mean(allData), sigma = sd(allData), loc = 0)$scale

    rows <- list("mean" = mean(allData), "sd" = sd(allData), "lsl" = options[["lowerSpecification"]],
                 "usl" = options[["upperSpecification"]], "beta" = beta, "theta" = theta)

  }

  table$addRows(rows)
}

#############################################################
## Functions for probability plot section ###################
#############################################################

################
## Containers ##
################

.qcProbabilityPlotContainer <- function(options, dataset, ready, jaspResults) {

  if (!options[["probabilityPlot"]] || !is.null(jaspResults[["probabilityContainer"]]))
    return()

  container <- createJaspContainer(gettext("Probability Tables and Plots"))
  container$dependOn(options = c("variables", "probabilityPlot", "rank", "nullDistribution", "addGridlines"))
  container$position <- 3

  plotContainer <- createJaspContainer(gettext("Probability Plots"))
  container[["plotContainer"]] <- plotContainer

  jaspResults[["probabilityContainer"]] <- container

  if (!ready)
    return()

  .qcProbabilityTable(dataset, options, container)

  for (variable in options[["variables"]]) {
    if (is.null(plotContainer[[variable]]))
      plotContainer[[variable]]  <- .qcProbabilityPlot(dataset, options, variable)
  }
}

################
## Output ######
################

.qcProbabilityTable <- function(dataset, options, container) {

  table <- createJaspTable(title = gettextf("Summary of Tests Against the %1$s Distribution", options[["nullDistribution"]]))
  table$position <- 1

  table$addColumnInfo(name = "v", 		title = "",					type = "number")
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
  table$addColumnInfo(name = "reject",	title = gettext("Reject null distribution?"),      		type = "string")

  table$addFootnote(gettextf("The Anderson-Darling statistic <i>A</i> is calculated against the %2$s distribution using the %3$s ranking method.", "\u00B2", options[["nullDistribution"]], options[["rank"]]))

  container[["probabilityTable"]] <- table

  for (variable in options[["variables"]]) {

    values <- dataset[[variable]]

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
    reject <- if (p < 0.05) "Yes" else "No"

    row <- list(v = variable, mean = meanx, sd = sdx, n = n, ad = ad, p = p, reject = reject)
    table$addRows(row)
  }
}

.qcProbabilityPlot <- function(dataset, options, variable) {

  plot <- createJaspPlot(width = 400, aspectRatio = 1, title = variable)
  plot$dependOn(optionContainsValue = list(variables = variable))

  # Arrange data
  x <- dataset[[variable]]
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
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(x), max(x)))

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

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileEstimate)) +
    ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileLower), col = "darkred", linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileUpper), col = "darkred", linetype = "dashed") +
    jaspGraphs::geom_point(ggplot2::aes(x = data1[["x"]], y = data1[["y"]])) +
    ggplot2::scale_x_continuous(variable, breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous('Percent', labels = ticks, breaks = yBreaks)

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

  data <- unlist(dataset[measurements])

  if (!options[["histogram"]] || !is.null(jaspResults[["histogram"]]))
    return()

  plot <- createJaspPlot(title = gettext("Histogram"), width = 400, height = 400)
  plot$dependOn(options = c("histogram", "displayDensity", "variables", "pcNumberOfBins", "pcBinWidthType"))
  plot$position <- 2

  jaspResults[["histogram"]] <- plot

  if (!ready)
    return()

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(data, min.n = 4)

  p <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(name = gettext("Measurement"), breaks = xBreaks, limits = range(xBreaks))

  if (options[["displayDensity"]]) {
    #yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, hist(data, freq = T, plot = F, breaks = bins)$freq), min.n = 4)
    p <- p + ggplot2::geom_histogram(data, ggplot2::aes(y =..density..), fill = "grey", col = "black", size = .7) +     #ggplot2::scale_y_continuous(name = gettext("Density"), breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::stat_function(fun = dnorm, color = "dodgerblue", args = list(mean = mean(data), sd = sd(data)))
    p <- jaspGraphs::themeJasp(p) + ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
  }

  plot$plotObject <- p
}
