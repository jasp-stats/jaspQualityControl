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
msaBayesianGaugeRR <- function(jaspResults, dataset, options, ...) {
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

  # note this should also be in a function (I could also just make the dropdown include full model, main effects only and automatic)
  if(options$estimationType == "manual"){
    if(options$fullModel || options$mainEffectsOnly) {
      ready <- ready
    } else {
      ready <- FALSE
    }
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

  #Converting long to wide data
  # if (!wideFormat && ready) {
  #   dataset <- dataset[order(dataset[[operators]]),]
  #   dataset <- dataset[order(dataset[[parts]]),]
  #   nrep <- table(dataset[operators])[[1]]/length(unique(dataset[[parts]]))
  #   index <- rep(paste("V", 1:nrep, sep = ""), nrow(dataset)/nrep)
  #   dataset <- cbind(dataset, data.frame(index = index))
  #   dataset <- tidyr::spread(dataset, index, measurements)
  #   measurements <- unique(index)
  #   dataset <- dataset[,c(operators, parts, measurements)]
  # } else if (ready) {
  #   dataset <- dataset[order(dataset[[parts]]),]
  # }

  # note: I would probably have to convert the wide to long data for my analysis

  if(ready && !options[["type3"]]){
    crossed <- .checkIfCrossed(dataset, operators, parts, measurements)
    if(!crossed){
      plot <- createJaspPlot(title = gettext("Gauge r&R"), width = 700, height = 400)
      jaspResults[["plot"]] <- plot
      plot$setError(gettext("Design is not balanced: not every operator measured every part. Use non-replicable gauge r&R."))
      return()
    }
  }

  # Checking type 3
  Type3 <- c(length(unique(dataset[[operators]])) == 1 || options$type3)

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


  saveRDS(options, "/Users/julian/Documents/Jasp files/options.rds")
  saveRDS(dataset, "/Users/julian/Documents/Jasp files/dataset.rds")
  saveRDS(measurements, "/Users/julian/Documents/Jasp files/measurements.rds")
  saveRDS(operators, "/Users/julian/Documents/Jasp files/operators.rds")
  saveRDS(parts, "/Users/julian/Documents/Jasp files/parts.rds")

  # Results from model comparison
  if(ready){
    .runBFtest(jaspResults, dataset, measurements, parts, operators, options)
  }

  # Model comparison table
  if(options[["RRTable"]]){
    .createBFtable(jaspResults, dataset, options, measurements, parts, operators, ready)
  }

  # # Effects table
  # if(options[["effectsTable"]]){
  #   .createEffectsTable(effectsRes, jaspResults, measurements, parts, operators, ready)
  # }

  # MCMC
  if(ready) {
    .runMCMC(jaspResults, dataset, measurements, parts, operators, options)
    .fitDistToSamples(jaspResults, options, samplesMat = jaspResults[["MCMCsamples"]][["object"]])
  }

  # Variance components table
  .createVarCompTable(jaspResults, parts, operators, ready, options)

  # % Contribution to total variation table
  .createPercContribTable(jaspResults, options, parts, operators, ready)

  # Gauge evaluation table
  .createGaugeEval(jaspResults, parts, operators, options, ready)

  # posteriors
  if(ready && options$posteriorPlot){
    .fillPostSummaryTable(jaspResults, options, parts, operators)
    .plotVariancePosteriors(jaspResults, options, parts, operators)

    # summary table
    if(options$posteriorCi || options$posteriorPointEstimate) {
      .createPostSummaryTable(jaspResults, options, parts, operators)
    }
  }

  # contour plot
  if(ready && options$contourPlot) {
    .createContourPlot(jaspResults, parts, operators, measurements, dataset, options)
  }

  if(options$rChart) {
    .createRChart(jaspResults, dataset, measurements, operators, parts, options, ready)
  }

}






.createBFtable <- function(jaspResults, dataset, options, measurements, parts, operators, ready) {
  if(!is.null(jaspResults[["BFtable"]])) {
    return()
  }

  BFtable <- createJaspTable(title = gettext("Model Comparison"))
  BFtable$position <- 1
  BFtable$dependOn(.bfTableDependencies())

  jaspResults[["BFtable"]] <- BFtable

  BFtable$addColumnInfo(name = "modelName",      title = gettext("Models"),          type = "string")
  BFtable$addColumnInfo(name = "comparisonBF",   title = gettext("BF<sub>10</sub>"), type = "number")
  BFtable$addColumnInfo(name = "error",          title = gettext("error %"),         type = "number")

  # set data
  if(ready) { # this could also be sth like if(ncol(dataset) == 3)
    BFtable$setData(jaspResults[["modelComparison"]][["object"]])
    BFtable$addFootnote(gettext("BF<sub>10</sub> compares the full model to the indicated model in each row."))
  }

  return()
}

.runBFtest <- function(jaspResults, dataset, measurements, parts, operators, options) {
  if(is.null(jaspResults[["modelComparison"]])) {
    modelComparison <- createJaspState()
    modelComparison$dependOn(c("operatorWideFormat", "operatorLongFormat", "partWideFormat", "partLongFormat", "measurementsWideFormat",
                             "measurementLongFormat", "seed", "setSeed", "rscalePrior"))
    jaspResults[["modelComparison"]] <- modelComparison
  } else {
    return()
  }


  formula <- as.formula(paste(measurements, "~", parts, "*", operators))


  if(options$setSeed) {
    set.seed(options$seed)
  }

  # run general comparison for all potential models
  bfFit <- BayesFactor::generalTestBF(formula, data = dataset,
                                       whichRandom = c(operators, parts),
                                       rscaleRandom = options$rscalePrior,
                                       progress = FALSE)
  bfDf <- as.data.frame(bfFit)

  # extract full model and model with only main effects
  main <- paste(parts, "+", operators)
  full <- paste0(parts, " + ", operators, " + ", parts, ":", operators)
  bfDf <- bfDf[c(main, full), ]

  # dropping unnecessary columns
  bfDf <- bfDf[, !colnames(bfDf) %in% c("time", "code")]

  # obtain BF comparing full model to other models
  bfFullNull <- bfDf[full, ]$bf
  bfDf$bf <- bfFullNull / bfDf$bf

  # add null model
  bfDf["Null model", ] <- c(bfFullNull,
                            bfDf[full, ]$error)

  bfDf[full, ]$error <- ""

  # add model names & change colnames
  colnames(bfDf) <- c("comparisonBF", "error")
  bfDf$modelName <- jaspBase::gsubInteractionSymbol(rownames(bfDf))

  bfDF <- bfDf[order(bfDf$comparisonBF), ]

  jaspResults[["modelComparison"]][["object"]] <- bfDf

  return()

}


.createVarCompTable <- function(jaspResults, parts, operators, ready, options) {
  if(!is.null(jaspResults[["varCompTable"]])) {
    return()
  }

  varCompTable <- createJaspTable(title = gettext("Variance Components"))
  varCompTable$position <- 2
  varCompTable$dependOn(.varCompTableDependencies())

  jaspResults[["varCompTable"]] <- varCompTable

  varCompTable$addColumnInfo(name = "sourceName",   title = gettext("Source"),                type = "string")
  varCompTable$addColumnInfo(name = "postMeans",    title = gettext("Mean"),                  type = "number")
  varCompTable$addColumnInfo(name = "postSds",      title = gettext("Std. Deviation"),        type = "number")
  varCompTable$addColumnInfo(name = "postCrIlower", title = gettext("Lower"),                 type = "number", overtitle = gettext("95% Credible Interval"))
  varCompTable$addColumnInfo(name = "postCrIupper", title = gettext("Upper"),                 type = "number", overtitle = gettext("95% Credible Interval"))

  # set data
  if(ready) {
    varCompTable$setData(.getVarianceComponents(jaspResults, parts, operators, options))

    if(.evalInter(jaspResults, parts, operators, options)) {
      varCompTable$addFootnote("The components are based on the model only including the main effects.")
    } else {
      varCompTable$addFootnote("The components are based on the full model.")
    }

  } else {
    return()
  }

  return()
}

.createPercContribTable <- function(jaspResults, options, parts, operators, ready) {
  if(!is.null(jaspResults[["contribTable"]])) {
    return()
  }
  contribTable <- createJaspTable(title = gettext("% Contribution to Total Variation"))
  contribTable$position <- 3
  contribTable$dependOn(.varCompTableDependencies())
  jaspResults[["contribTable"]] <- contribTable

  contribTable$addColumnInfo(name = "sourceName", title = gettext("Source"),  type = "string")
  contribTable$addColumnInfo(name = "means",      title = gettext("Mean"),    type = "number")
  contribTable$addColumnInfo(name = "lower",      title = gettext("Lower"),   type = "number", overtitle = "95% Credible Interval")
  contribTable$addColumnInfo(name = "upper",      title = gettext("Upper"),   type = "number", overtitle = "95% Credible Interval")

  if(ready) {
    .getPercContrib(jaspResults, parts, operators, options)
    contribTable$setData(jaspResults[["percContribSamples"]][["object"]])
  } else {
    return()
  }
  return()
}





.runMCMC <- function(jaspResults, dataset, measurements, parts, operators, options){
  if(is.null(jaspResults[["MCMCsamples"]])){
    MCMCsamples <- createJaspState()
    MCMCsamples$dependOn(.mcmcDependencies())
    jaspResults[["MCMCsamples"]] <- MCMCsamples
  } else {
    return()
  }

  # obtain BF in favor of full over main effects model
  excludeInter <- .evalInter(jaspResults, parts, operators, options)
  if(excludeInter){
    formula <- as.formula(paste(measurements, "~", parts, "+", operators))
  } else {
    formula <- as.formula(paste(measurements, "~", parts, "*", operators))
  }

  # fit the model with BayesFactor
  fit <- BayesFactor::lmBF(formula, whichRandom = c(parts, operators),
                           data = dataset, rscaleRandom = options$rscalePrior)

  nchains <- options$mcmcChains
  burnin <- options$mcmcBurnin
  iter <- options$mcmcIterations

  chains <- coda::mcmc.list()

  if(options$setSeed) {
    set.seed(options$seed)
  }

  for(i in 1:nchains) {
    # run chain
    mcmcChain <- BayesFactor::posterior(fit, iterations = iter)

    # exclude burn-in samples
    chains[[i]] <- coda::as.mcmc(mcmcChain[-(1:burnin), ])
  }


  # select relevant parameters
  # names
  paramNames <- .bfParameterNames(parts, operators, excludeInter)

  chains <- chains[, c(paramNames, "sig2")] # including error variance
  samplesMat <- as.matrix(chains)

  # multiply variances with the error variance to reverse standardization
  for(i in paramNames) {
    samplesMat[, i] <- samplesMat[, i] * samplesMat[, "sig2"]
  }

  MCMCsamples[["object"]] <- samplesMat

  return()
}


.getVarianceComponents <- function(jaspResults, parts, operators, options) {
  excludeInter <- .evalInter(jaspResults, parts, operators, options)

  # get components from MCMC samples
  internalDF <- .getComponentsFromSamples(jaspResults, parts, operators, options, excludeInter)

  # calculate summary stats
  postMeans <- colMeans(internalDF)
  postSds <- apply(internalDF, 2, sd)
  postCrIlower <- apply(internalDF, 2, quantile, probs = 0.025)
  postCrIupper <- apply(internalDF, 2, quantile, probs = 0.975)

  # remove some stats when historicalSd is specified
  if(options$processVariationReference == "historicalSd"){
    postSds["part"] <- ""
    postSds["total"] <- ""
    postCrIlower["part"] <- ""
    postCrIlower["total"] <- ""
    postCrIupper["part"] <- ""
    postCrIupper["total"] <- ""
  }


  sourceName <- .sourceNames()

  return(data.frame(sourceName,
                    postMeans,
                    postSds,
                    postCrIlower,
                    postCrIupper)
         )
}



.evalInter <- function(jaspResults, parts, operators, options) {
  if(options$estimationType == "automatic") {
    bfDf <- jaspResults[["modelComparison"]][["object"]]
    main <- paste(parts, "+", operators)

    excludeInter <- bfDf[main, ]$comparisonBF <= options$bfFavorFull
  }

  if(options$estimationType == "manual"){
    if(options$fullModel){
      excludeInter <- FALSE
    }

    if(options$mainEffectsOnly){
      excludeInter <- TRUE
    }
  }

  return(excludeInter)
}

.getComponentsFromSamples <- function(jaspResults, parts, operators, options, excludeInter){
  # note this could be written into a helper function
  sigmaPart <- paste0("g_", parts)
  sigmaOperator <- paste0("g_", operators)
  sigmaInter <- paste0("g_", parts, ":", operators)

  samplesMat <- jaspResults[["MCMCsamples"]][["object"]]

  # obtain relevant components
  if(excludeInter){
    reprod <- samplesMat[, sigmaOperator]
  } else {
    reprod <- samplesMat[, sigmaOperator] + samplesMat[, sigmaInter]
  }
  repeatability <- samplesMat[, "sig2"]
  gauge <- reprod + repeatability
  operator <- samplesMat[, sigmaOperator]
  part <- samplesMat[, sigmaPart]
  total <- gauge + part

  # replace total variation with historical variance and adjust
  # part variation accordingly
  # note: these calculations might be problematic since the uncertainty in gauge does not affect part anymore
  if(options$processVariationReference == "historicalSd"){
    totalOld <- mean(total)
    total <- rep(options$historicalSdValue^2, length(repeatability))
    diffTotals <- total - totalOld
    part <- mean(part) + diffTotals
  }

  internalDF <- data.frame(gauge,
                           repeatability,
                           reprod,
                           operator,
                           part,
                           total
  )
  return(internalDF)
}

# .fitMetaLog <- function(jaspResults) {
#   if(is.null(jaspResults[["metaLogFit"]])){
#     metaLogFit <- createJaspState()
#     metaLogFit$dependOn(.mcmcDependencies())
#     jaspResults[["metaLogFit"]] <- metaLogFit
#   } else {
#     return()
#   }
#
#   samplesMat <- jaspResults[["MCMCsamples"]][["object"]]
#
#   # fit metalog to each parameter
#   metaLogList <- apply(samplesMat, 2,
#                        function(x) rmetalog::metalog(x, bounds = 0, boundedness = "sl"))
#
#   # find optimal number of terms for each parameter
#   optimalTerms <- Map(.optimalMetaLog, metaLogList, names(metaLogList),
#                       MoreArgs = list(samplesMat = samplesMat))
#
#   # add optimal terms to list
#   metaLogList <- Map(function(x, optimalTerms){
#     x[["optimalTerms"]] <- optimalTerms
#     x
#   }, metaLogList, optimalTerms)
#
#   metaLogFit[["object"]] <- metaLogList
#
#   return()
#
# }

.createPostSummaryTable <- function(jaspResults, options, parts, operators){
  if(!is.null(jaspResults[["variancePosteriors"]][["postSummary"]])){
    return()
  }

  postSummary <- createJaspTable(title = gettext("Posterior Summary"))
  postSummary$position <- 1
  postSummary$dependOn(c(.mcmcDependencies(),
                       .postPlotDependencies()))

  jaspResults[["variancePosteriors"]][["postSummary"]] <- postSummary

  # title for point estimate
  pointEst <- switch (options$posteriorPointEstimateType,
    "mean" = "Mean",
    "mode" = "Mode",
    "median" = "Median"
  )

  # overtitle for CrI
  if(options$posteriorCiType == "central" || options$posteriorCiType == "HPD") {
    mass <- round(options$posteriorCiMass * 100)
  }

  if(options$posteriorCiType == "custom") {
    mass <- round((options$posteriorCiUpper - options$posteriorCiLower) * 100)
  }

  overtitle <- paste0(mass, "% ", "Credible Interval")


  postSummary$addColumnInfo(name = "parameter",     title = gettext("Parameter"), type = "string")

  if(options$posteriorPointEstimate) {
    postSummary$addColumnInfo(name = "pointEstimate", title = gettext(pointEst),    type = "number")
  }

  if(options$posteriorCi) {
    postSummary$addColumnInfo(name = "ciLower",       title = gettext("Lower"),     type = "number", overtitle = gettext(overtitle))
    postSummary$addColumnInfo(name = "ciUpper",       title = gettext("Upper"),     type = "number", overtitle = gettext(overtitle))
  }


  postSummary$setData(jaspResults[["postSummaryStats"]][["object"]])

  return()
}


.createContourPlot <- function(jaspResults, parts, operators, measurements, dataset, options) {
  if(!is.null(jaspResults[["contourPlot"]])) {
    return()
  }

  contourPlot <- createJaspContainer(title = gettext("Contour Plot"))
  contourPlot$position <- 6
  contourPlot$dependOn(c(.varCompTableDependencies(),
                         "studyVarianceMultiplierType", "studyVarianceMultiplierValue",
                         "contourPlot", "contourUSL", "contourLSL"))

  jaspResults[["contourPlot"]] <- contourPlot


  tempPlot <- createJaspPlot(width = 600, height = 600)
  tempPlot$position <- 2

  samplesMat <- jaspResults[["MCMCsamples"]][["object"]]
  excludeInter <- .evalInter(jaspResults, parts, operators, options)
  compDf <-.getComponentsFromSamples(jaspResults, parts, operators, options, excludeInter) # note: should the historcial sd influence this if entered by the user?

  # obtain necessary data
  contourDf <- compDf[, c("total", "part")]
  mu <- mean(dataset[[measurements]]) # note: do I have to transform the variances to get a sensible result

  # data frame for plotting
  meanEllipse = TRUE
  plotDf <- .getEllipses(contourDf, mu, meanEllipse = meanEllipse, options = options) #note: add number of ellipses here; this could also be done with one ellipse based on the post. mean of the variances

  if(meanEllipse) {
    p <- ggplot2::ggplot(plotDf, ggplot2::aes(x = x, y = y))
  } else {
    p <- ggplot2::ggplot(plotDf, ggplot2::aes(x = x, y = y, group = iter))
  }

  p <- p +
    ggplot2::geom_vline(xintercept = c(options$contourLSL, options$contourUSL), linetype = "dashed", color = "black", linewidth = 1) +
    ggplot2::geom_hline(yintercept = c(options$contourLSL, options$contourUSL), linetype = "dashed", color = "black", linewidth = 1) +
    ggplot2::geom_path(alpha = 0.5, colour = "steelblue", linewidth = 1)


  # axes
  xLower <- min(options$contourLSL, plotDf$x)
  xUpper <- max(options$contourUSL, plotDf$x)
  xLims <- c(xLower, xUpper)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(xLims)

  yLower <- min(options$contourLSL, plotDf$y)
  yUpper <- max(options$contourUSL, plotDf$y)
  yLims <- c(yLower, yUpper)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yLims)

  p <- p +
    ggplot2::scale_x_continuous(name = "True Value", breaks = xBreaks,
                                limits = xLims, labels = xBreaks) +
    ggplot2::scale_y_continuous(name = "Measurement", breaks = yBreaks,
                                limits = yLims, labels = yBreaks) +
    ggplot2::coord_equal()

  # theme
  p <- p +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe(sides = "bl")

  tempPlot$plotObject <- p

  contourPlot[["plot"]] <- tempPlot

  # table with the posterior means and CrIs for the risks
  risksTable <- createJaspTable(title = gettext("Producer's (\u03b4) and Consumer's (\u03b2) Risk"))
  risksTable$position <- 1

  risksTable$addColumnInfo(name = "risks", title = gettext("Risk"),  type = "string")
  risksTable$addColumnInfo(name = "means", title = gettext("Mean"),  type = "number")
  risksTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = "95% Credible Interval")
  risksTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = "95% Credible Interval")

  risksTable$setData(.getRisks(contourDf, mu, options))

  contourPlot[["table"]] <- risksTable


  return()
}





# helper functions
.bfParameterNames <- function(parts, operators, excludeInter) {
  sigmaPart <- paste0("g_", parts)
  sigmaOperator <- paste0("g_", operators)
  sigmaInter <- paste0("g_", parts, ":", operators)

  if(excludeInter) {
    res <- c(sigmaPart, sigmaOperator)
  } else {
    res <- c(sigmaPart, sigmaOperator, sigmaInter)
  }
  return(res)
}

.sourceNames <- function() {
  return(c("Total gauge r&R",
           "Repeatability",
           "Reproducibility",
           "Operator",
           "Part-to-part",
           "Total variation"))
}

.bfTableDependencies <- function() {
  return(c("operatorWideFormat", "operatorLongFormat", "partWideFormat", "partLongFormat", "measurementsWideFormat",
           "measurementLongFormat", "seed", "setSeed", "rscalePrior", "RRTable", "bfFavorFull"))
}

.varCompTableDependencies <- function() {
  return(c("operatorWideFormat", "operatorLongFormat", "partWideFormat", "partLongFormat", "measurementsWideFormat",
           "measurementLongFormat", "seed", "setSeed", "rscalePrior", "bfFavorFull",
           "mcmcChains", "mcmcBurnin", "mcmcIterations", "historicalSdValue", "processVariationReference",
           "estimationType", "fullModel", "mainEffectsOnly"))
}

.mcmcDependencies <- function() {
  return(c("operatorWideFormat", "operatorLongFormat", "partWideFormat", "partLongFormat", "measurementsWideFormat",
           "measurementLongFormat", "seed", "setSeed", "rscalePrior", "bfFavorFull",
           "mcmcChains", "mcmcBurnin", "mcmcIterations",
           "estimationType", "fullModel", "mainEffectsOnly"))
}

.postPlotDependencies <- function() {
  return(c("posteriorCi", "posteriorCiLower", "posteriorCiMass", "posteriorCiType", "posteriorCiUpper",
           "posteriorPointEstimate", "posteriorPointEstimateType", "posteriorPlot",
           "distType"))
}

# .optimalMetaLog <- function(fit, parameter, samplesMat) {
#   terms <- fit$params$term_limit
#
#   error <- numeric(length(terms))
#
#   for(j in 2:terms){
#     # quantiles
#     j <- as.numeric(j)
#     qmeta <- rmetalog::qmetalog(m = fit, y = c(0.025, 0.975), term = j)
#     qdata <- quantile(samplesMat[, parameter], probs = c(0.025, 0.975))
#
#     errorCrI <- sum(abs(qdata - qmeta))
#
#     # mean
#     meanMeta <- integrate(rmetalog::qmetalog, m = fit, term = j, lower = 0, upper = 1)$value # integrate over quantile function
#     meanData <- mean(samplesMat[, parameter])
#
#     errorMean <- abs(meanData - meanMeta)
#
#     error[j] <- sum(errorCrI, errorMean)
#   }
#   #print(error)
#   return(which.min(error[-1]) + 1)
# }


.convertOutputNames <- function(name, parts, operators, includeSigma = TRUE) {
  sigmaPart <- paste0("g_", parts)
  sigmaOperator <- paste0("g_", operators)
  sigmaInter <- paste0("g_", parts, ":", operators)
  if(includeSigma) {
    replPart <- "\u03C3<sup>2</sup><sub>Part</sub>"
    replOperator <- "\u03C3<sup>2</sup><sub>Operator</sub>"
    replInter <- "\u03C3<sup>2</sup><sub>Part\u2009\u273B\u2009Operator</sub>"
    replError <- "\u03C3<sup>2</sup><sub>Error</sub>"
  } else {
    replPart <- "Part"
    replOperator <- "Operator"
    replInter <- "Part\u2009\u273B\u2009Operator"
    replError <- "Error"
  }

  name <- sub(sigmaInter, replInter, name)
  name <- sub(sigmaPart, replPart, name)
  name <- sub(sigmaOperator, replOperator, name)
  name <- sub("sig2", replError, name)

  return(name)
}

.getEllipses <- function(contourDf, mu, options, numberEllipses = 20, meanEllipse = FALSE) {

  if(options$setSeed) {
    set.seed(options$seed)
  }
  if(meanEllipse) {
    sigmaP <- mean(contourDf$part)
    sigmaTotal <- mean(contourDf$total)

    covMat <- matrix(c(sigmaTotal, sigmaP,
                       sigmaP, sigmaP),
                     nrow = 2, ncol = 2)
    res <- as.data.frame(ellipse::ellipse(covMat, centre = c(mu, mu), level = 0.95))
  } else {
    ind <- sample(1:nrow(contourDf), numberEllipses)
    ellipseList <- lapply(ind, function(i) {
      sigmaP <- contourDf[i, ]$part # part
      sigmaTotal <- contourDf[i, ]$total # total

      covMat <- matrix(c(sigmaTotal, sigmaP,
                         sigmaP, sigmaP),
                       nrow = 2, ncol = 2)

      # ellipse
      ellipseDf <- as.data.frame(ellipse::ellipse(covMat, centre = c(mu, mu), level = 0.95))
      ellipseDf$iter <- i

      return(ellipseDf)
    })
    res <- do.call(rbind.data.frame, ellipseList)
  }

  return(res)
}

.getRisks <- function(contourDf, mu, options) {

  USL <- options$contourUSL
  LSL <- options$contourLSL
  producers <- consumers <- c()

  for (i in 1:nrow(contourDf)) {
    sigmaP <- contourDf[i, ]$part # part
    sigmaTotal <- contourDf[i, ]$total # total

    covMat <- matrix(c(sigmaTotal, sigmaP,
                       sigmaP, sigmaP),
                     nrow = 2, ncol = 2)

    # producer's risk (delta)
    # probability that y falls outside although x is inside
    numerator <- mvtnorm::pmvnorm(lower = c(-Inf, LSL), upper = c(LSL, USL), mean = c(mu, mu),
                                  sigma = covMat) +
      mvtnorm::pmvnorm(lower = c(USL, LSL), upper = c(Inf, USL), mean = c(mu, mu), sigma = covMat)

    denom <- pnorm(USL, mean = mu, sd = sqrt(sigmaP)) - pnorm(LSL, mean = mu, sd = sqrt(sigmaP))

    producers[i] <- numerator / denom

    # consumers risk
    # probability that y is inside although x falls outside
    numerator <- mvtnorm::pmvnorm(lower = c(LSL, -Inf), upper = c(USL, LSL), mean = c(mu, mu),
                                  sigma = covMat) +
      mvtnorm::pmvnorm(lower = c(LSL, USL), upper = c(USL, Inf), mean = c(mu, mu), sigma = covMat)

    denom <- 1 - denom

    consumers[i] <- numerator / denom
  }

  df <- data.frame(delta = producers,
                   beta = consumers)
  # means
  means <- apply(df, 2, mean)

  # CrIs
  lower <- apply(df, 2, quantile, probs = 0.025)
  upper <- apply(df, 2, quantile, probs = 0.975)

  # unicodes
  risks <- c("\u03b4", "\u03b2")

  return(data.frame(risks,
                    means,
                    lower,
                    upper))

}

.getPercContrib <- function(jaspResults, parts, operators, options) {
  if(is.null(jaspResults[["percContribSamples"]])) {
    percContribSamples <- createJaspState()
    percContribSamples$dependOn(.varCompTableDependencies())
    jaspResults[["percContribSamples"]] <- percContribSamples
  } else {
    return()
  }

  excludeInter <- .evalInter(jaspResults, parts, operators, options)

  # get components from MCMC samples
  internalDF <- .getComponentsFromSamples(jaspResults, parts, operators, options, excludeInter)

  # %Contribution to total variance
  contribution <- matrix(ncol = ncol(internalDF), nrow = nrow(internalDF))
  for(i in 1:ncol(internalDF)){
    contribution[, i] <- internalDF[[i]] / internalDF$total * 100
  }

  sourceName <- .sourceNames()
  means <- colMeans(contribution)
  lower <- apply(contribution, 2, quantile, probs = 0.025)
  upper <- apply(contribution, 2, quantile, probs = 0.975)

  df <- data.frame(sourceName,
                   means,
                   lower,
                   upper)

  # remove upper and lower CrI for total variation
  df[df$sourceName == "Total variation", c("lower", "upper")] <- ""

  # remove upper and lower CrI for part variation if historicalSd is specified
  if(options$processVariationReference == "historicalSd") {
    df[df$sourceName == "Part-to-part", c("lower", "upper")] <- ""
  }

  percContribSamples[["object"]] <- df

  return()
}






.createGaugeEval <- function(jaspResults, parts, operators, options, ready) {
  if(!is.null(jaspResults[["gaugeEvaluation"]])) {
    return()
  }

  gaugeEvaluation <- createJaspContainer(title = gettext("Gauge Evaluation"))
  gaugeEvaluation$position <- 4
  gaugeEvaluation$dependOn(c(.varCompTableDependencies(),
                             "studyVarianceMultiplierType", "studyVarianceMultiplierValue",
                             "tolerance", "toleranceValue"))
  jaspResults[["gaugeEvaluation"]] <- gaugeEvaluation

  ### Standard deviation & study variation table
  stdTable <- createJaspTable(title = gettext("Standard Deviation & Study Variation"))
  stdTable$position <- 1
  gaugeEvaluation[["stdTable"]] <- stdTable

  stdTable$addColumnInfo(name = "sourceName",    title = gettext("Source"),                  type = "string")
  stdTable$addColumnInfo(name = "meansStd",      title = gettext("Mean<br>Std"),             type = "number")
  stdTable$addColumnInfo(name = "lowerStd",      title = gettext("Lower"),                   type = "number", overtitle = "95% Credible Interval<br>Std")
  stdTable$addColumnInfo(name = "upperStd",      title = gettext("Upper"),                   type = "number", overtitle = "95% Credible Interval<br>Std")
  stdTable$addColumnInfo(name = "meansStudyVar", title = gettext("Mean<br>Study Variation"), type = "number")
  stdTable$addColumnInfo(name = "lowerStudyVar", title = gettext("Lower"),                   type = "number", overtitle = "95% Credible Interval<br>Study Variation")
  stdTable$addColumnInfo(name = "upperStudyVar", title = gettext("Upper"),                   type = "number", overtitle = "95% Credible Interval<br>Study Variation")

  if(ready) {
    stdData <- .fillTablesGaugeEval(jaspResults, parts, operators, options, whichTable = "sd")
    colnames(stdData) <- c("sourceName", "meansStd", "lowerStd", "upperStd") # note: this could already be part of the function
    studyVarData <- .fillTablesGaugeEval(jaspResults, parts, operators, options, whichTable = "studyVar")[, -1] # remove source name
    colnames(studyVarData) <- c("meansStudyVar", "lowerStudyVar", "upperStudyVar")
    stdTable$setData(cbind(stdData, studyVarData))
  }


  # ### Study variation table
  # studyVarTable <- createJaspTable(title = gettext("Study variation"))
  # studyVarTable$position <- 2
  # gaugeEvaluation[["studyVarTable"]] <- studyVarTable
  #
  # studyVarTable$addColumnInfo(name = "sourceName", title = gettext("Source"),  type = "string")
  # studyVarTable$addColumnInfo(name = "means",      title = gettext("Mean"),    type = "number")
  # studyVarTable$addColumnInfo(name = "lower",      title = gettext("Lower"),   type = "number", overtitle = "95% Credible Interval")
  # studyVarTable$addColumnInfo(name = "upper",      title = gettext("Upper"),   type = "number", overtitle = "95% Credible Interval")
  #
  # if(ready) {
  #   studyVarTable$setData(.fillTablesGaugeEval(jaspResults, parts, operators, options, whichTable = "studyVar"))
  # }

  ### Percent study variation & percent tolerance table
  if(options$tolerance) {
    title <- "% Study Variation & % Tolerance"
  } else {
    title <- "% Study Variation"
  }
  percStudyVarTable <- createJaspTable(title = gettext(title))
  percStudyVarTable$position <- 2
  gaugeEvaluation[["percStudyVarTable"]] <- percStudyVarTable

  percStudyVarTable$addColumnInfo(name = "sourceName",          title = gettext("Source"),                    type = "string")
  percStudyVarTable$addColumnInfo(name = "meansPercStudy",      title = gettext("Mean<br>% Study Variation"), type = "number")
  percStudyVarTable$addColumnInfo(name = "lowerPercStudy",      title = gettext("Lower"),                     type = "number", overtitle = "95% Credible Interval<br>% Study Variation")
  percStudyVarTable$addColumnInfo(name = "upperPercStudy",      title = gettext("Upper"),                     type = "number", overtitle = "95% Credible Interval<br>% Study Variation")

  if(options$tolerance) {
    percStudyVarTable$addColumnInfo(name = "meansPercTol",      title = gettext("Mean<br>% Tolerance"), type = "number")
    percStudyVarTable$addColumnInfo(name = "lowerPercTol",      title = gettext("Lower"),               type = "number", overtitle = "95% Credible Interval<br>% Tolerance")
    percStudyVarTable$addColumnInfo(name = "upperPercTol",      title = gettext("Upper"),               type = "number", overtitle = "95% Credible Interval<br>% Tolerance")
  }

  if(ready) {
    percStudyData <- .fillTablesGaugeEval(jaspResults, parts, operators, options, whichTable = "percStudyVar")
    colnames(percStudyData) <- c("sourceName", "meansPercStudy", "lowerPercStudy", "upperPercStudy")

    if(!options$tolerance) {
      percStudyVarTable$setData(percStudyData)
    } else {
      percTolData <- .fillTablesGaugeEval(jaspResults, parts, operators, options, whichTable = "percTol")[, -1]
      colnames(percTolData) <- c("meansPercTol", "lowerPercTol", "upperPercTol")
      percStudyVarTable$setData(cbind(percStudyData, percTolData))
    }
  }


  # ### Percent tolerance table
  # if(options$tolerance) {
  #   percTolTable <- createJaspTable(title = gettext("% Tolerance"))
  #   percTolTable$position <- 3
  #   gaugeEvaluation[["percTolTable"]] <- percTolTable
  #
  #   percTolTable$addColumnInfo(name = "sourceName", title = gettext("Source"),  type = "string")
  #   percTolTable$addColumnInfo(name = "means",      title = gettext("Mean"),    type = "number")
  #   percTolTable$addColumnInfo(name = "lower",      title = gettext("Lower"),   type = "number", overtitle = "95% Credible Interval")
  #   percTolTable$addColumnInfo(name = "upper",      title = gettext("Upper"),   type = "number", overtitle = "95% Credible Interval")
  #
  #   if(ready) {
  #     percTolTable$setData(.fillTablesGaugeEval(jaspResults, parts, operators, options, whichTable = "percTol"))
  #   }
  # }

  return()

}

.getPercStudy <- function(jaspResults, studyVar) {
  if(is.null(jaspResults[["percStudySamples"]])) {
    percStudySamples <- createJaspState()
    percStudySamples$dependOn(c(.varCompTableDependencies(),
                         "studyVarianceMultiplierType", "studyVarianceMultiplierValue"))
    jaspResults[["percStudySamples"]] <- percStudySamples
  } else {
    return()
  }

  percStudy <- matrix(ncol = ncol(studyVar), nrow = nrow(studyVar))
  for(i in 1:ncol(studyVar)){
    percStudy[, i] <- studyVar[[i]] / studyVar$total * 100
  }

  percStudySamples[["object"]] <- percStudy

  return()
}

.getPercTol <- function(jaspResults, studyVar, options) {
  if(is.null(jaspResults[["percTolSamples"]])) {
    percTolSamples <- createJaspState()
    percTolSamples$dependOn(c(.varCompTableDependencies(),
                         "studyVarianceMultiplierType", "studyVarianceMultiplierValue"))
    jaspResults[["percTolSamples"]] <- percTolSamples
  } else {
    return()
  }

  percTol <- matrix(ncol = ncol(studyVar), nrow = nrow(studyVar))
  for(i in 1:ncol(studyVar)){
    percTol[, i] <- studyVar[[i]] / options$toleranceValue * 100
  }

  percTolSamples[["object"]] <- percTol

  return()
}

.fillTablesGaugeEval <- function(jaspResults, parts, operators, options, whichTable = "sd") {

  excludeInter <- .evalInter(jaspResults, parts, operators, options)

  # get components from MCMC samples
  internalDF <- .getComponentsFromSamples(jaspResults, parts, operators, options, excludeInter)

  sourceName <- .sourceNames()
  sdDf <- sqrt(internalDF)

  # get factor for multiplication
  if(options$studyVarianceMultiplierType == "sd") {
    factorSd <- options$studyVarianceMultiplierValue
  } else {
    val <- options$studyVarianceMultiplierValue / 100
    q <- (1 - val) / 2
    factorSd <- abs(2 * qnorm(q))
  }
  studyVar <- sdDf * factorSd

  # % Study Variation
  .getPercStudy(jaspResults, studyVar)
  percStudy <- jaspResults[["percStudySamples"]][["object"]]

  # % Tolerance
  if(options$tolerance) {
    .getPercTol(jaspResults, studyVar, options)
    percTol <- jaspResults[["percTolSamples"]][["object"]]
  }

  # output dependent on table
  if(whichTable == "sd") {
    # summaries
    means <- colMeans(sdDf)
    lower <- apply(sdDf, 2, quantile, probs = 0.025)
    upper <- apply(sdDf, 2, quantile, probs = 0.975)
  }

  if(whichTable == "studyVar") {
    # add footnote
    jaspResults[["gaugeEvaluation"]][["stdTable"]]$addFootnote(gettextf("Study variation is calculated as std. dev. <span>&#215;</span> %.2f", factorSd))

    # summaries
    means <- colMeans(studyVar)
    lower <- apply(studyVar, 2, quantile, probs = 0.025)
    upper <- apply(studyVar, 2, quantile, probs = 0.975)
  }

  if(whichTable == "percStudyVar") {
    # summaries
    means <- colMeans(percStudy)
    lower <- apply(percStudy, 2, quantile, probs = 0.025)
    upper <- apply(percStudy, 2, quantile, probs = 0.975)
  }

  if(whichTable == "percTol") {
    # summaries
    means <- colMeans(percTol)
    lower <- apply(percTol, 2, quantile, probs = 0.025)
    upper <- apply(percTol, 2, quantile, probs = 0.975)
  }

  df <- data.frame(sourceName,
                   means,
                   lower,
                   upper)

  if(whichTable == "percStudyVar") {
    # remove upper and lower CrI for total variation
    df[df$sourceName == "Total variation", c("lower", "upper")] <- ""
  }


  if(options$processVariationReference == "historicalSd") {
    # remove upper and lower CrI for part variation if historicalSd is specified
    df[df$sourceName == "Part-to-part", c("lower", "upper")] <- ""

    # remove upper and lower CrI for total variation
    df[df$sourceName == "Total variation", c("lower", "upper")] <- ""
  }

  return(df)

}

.createRChart <- function(jaspResults, dataset, measurements, operators, parts, options, ready) {
  if(!is.null(jaspResults[["rChart"]])) {
    return()
  }

  jaspResults[["rChart"]] <- createJaspContainer(gettext("Range chart by operator"))
  jaspResults[["rChart"]]$position <- 7
  jaspResults[["rChart"]]$dependOn(c("rChart", "measurementLongFormat",
                                     "measurementsWideFormat", "report"))
  jaspResults[["rChart"]][["plot"]] <- createJaspPlot(title = gettext("Range chart by operator"), width = 1200, height = 500)
  if (ready) {
    # converting data to wide format for the .controlChart function (note: this can be done more nicely)
    dataset <- .convertToWide(dataset, measurements, parts, operators)
    measurements <- c("V1", "V2", "V3")
    rChart <- .controlChart(dataset = dataset[c(measurements, operators)], plotType = "R",
                            stages = operators, xAxisLabels = dataset[[parts]][order(dataset[[operators]])],
                            stagesSeparateCalculation = FALSE)

    jaspResults[["rChart"]][["plot"]]$plotObject <- rChart$plotObject
    jaspResults[["rChart"]][["table"]] <- rChart$table
  }

  return()
}

.convertToWide <- function(dataset, measurements, parts, operators) {
  dataset <- dataset[order(dataset[[operators]]),]
  dataset <- dataset[order(dataset[[parts]]),]
  nrep <- table(dataset[operators])[[1]]/length(unique(dataset[[parts]]))
  index <- rep(paste("V", 1:nrep, sep = ""), nrow(dataset)/nrep)
  dataset <- cbind(dataset, data.frame(index = index))
  dataset <- tidyr::spread(dataset, index, measurements)
  measurements <- unique(index)
  dataset <- dataset[,c(operators, parts, measurements)]

  return(dataset)
}



###### Distribution fitting

### fit functions
.fitDistToSamples <- function(jaspResults, options, samplesMat) {
  if(is.null(jaspResults[["distFit"]])){
    distFit <- createJaspState() # note: add dependency on user input for dist from qml
    distFit$dependOn(c(.mcmcDependencies(), "distType"))
    jaspResults[["distFit"]] <- distFit
  } else {
    return()
  }

  # note: I could also pass a list with distribution names and fitting functions here
  distType <- options$distType

  if(options$setSeed) {
    set.seed(options$seed)
  }
  fit <- switch(distType,
                "metalog" = .fitMetaLog(samplesMat, bounds = 0, boundedness = "sl",
                                        term_lower_bound = 6, term_limit = 6), # 6 terms
                "gig" = .fitGIG(samplesMat))

  distFit[["object"]] <- fit

  return()
}

## MetaLog
.fitMetaLog <- function(samplesMat, ...) {
  # fit metalog to each parameter
  metaLogList <- apply(samplesMat, 2,
                       function(x) rmetalog::metalog(x, ...))
  return(metaLogList)
}

## Generalized Inverse Gaussian
.fitGIG <- function(samplesMat) {
  gigFitList <- apply(samplesMat, 2,
                      function(x) GeneralizedHyperbolic::gigFit(x))

  # add random samples
  gigFitList <- lapply(gigFitList,
                       function(x) {
                         x$randData <- GeneralizedHyperbolic::rgig(1e5, param = x$param)
                         return(x)
                       })
  return(gigFitList)
}


### posterior summary
.fillPostSummaryTable <- function(jaspResults, options, parts, operators) {
  if(is.null(jaspResults[["postSummaryStats"]]) && (options$posteriorCi || options$posteriorPointEstimate)){
    postSummaryStats <- createJaspState()
    postSummaryStats$dependOn(c(.mcmcDependencies(),
                                .postPlotDependencies()))
    jaspResults[["postSummaryStats"]] <- postSummaryStats
  } else {
    return()
  }

  fits <- jaspResults[["distFit"]][["object"]]

  parameter <- .convertOutputNames(names(fits), parts, operators) # note: this should only happen when the fits are based on the variance parameters

  # point estimates
  if(options$posteriorPointEstimate) {
    # list with functions for different distributions
    pointEstimateFunctions <- .pointEstimateFunctions()

    # select the right function
    pointFun <- pointEstimateFunctions[[options$distType]][[options$posteriorPointEstimateType]]

    pointEstimate <- switch(options$posteriorPointEstimateType,
                            "mean" = unlist(lapply(fits, pointFun)),
                            "median" = unlist(lapply(fits, pointFun)),
                            "mode" = unlist(lapply(fits, pointFun))) # note: the mode still seems to be a bit off
  }

  # intervals
  if(options$posteriorCi) {
    # list with functions for different distributions
    intervalFunctions <- .intervalFunctions()

    # select the right function
    interFun <- intervalFunctions[[options$distType]][[options$posteriorCiType]]

    if(options$setSeed) {
      set.seed(options$seed)
    }

    intervals <- switch(options$posteriorCiType,
                        "central" = Map(interFun, fits, mass = options$posteriorCiMass),
                        "HPD" = Map(interFun, fits, mass = options$posteriorCiMass),
                        "custom" = Map(interFun, fits,
                                       lower = options$posteriorCiLower,
                                       upper = options$posteriorCiUpper))

    # lower and upper bounds separately
    lower <- sapply(intervals, function(x) x[1])
    upper <- sapply(intervals, function(x) x[2])
  }

  if(options$posteriorPointEstimate && options$posteriorCi == FALSE) {
    df <- data.frame(parameter,
                     pointEstimate)
  } else if(options$posteriorPointEstimate == FALSE && options$posteriorCi) {
    df <- data.frame(parameter,
                     ciLower = lower,
                     ciUpper = upper)
  } else {
    df <- data.frame(parameter,
                     pointEstimate,
                     ciLower = lower,
                     ciUpper = upper)
  }
  postSummaryStats[["object"]] <- df

  return()
}


### functions point estimates
.pointEstimateFunctions <- function() {
  l <- list(
    metalog = list(
      mean = .meanMetaLog,
      median = .medianMetaLog,
      mode = .modeMetaLog
    ),
    gig = list(
      mean = .meanGIG,
      median = .medianGIG,
      mode = .modeGIG
    )
  )
  return(l)
}

## MetaLog
.meanMetaLog <- function(fit) {
  m <- integrate(rmetalog::qmetalog, m = fit, term = fit$params$term_limit,
                 lower = 0, upper = 1)$value
}

.medianMetaLog <- function(fit) {
  m <- rmetalog::qmetalog(m = fit, y = 0.5, term = fit$params$term_limit)
}

.modeMetaLog <- function(fit) {
  m <- optimize(rmetalog::dmetalog, interval = c(0, max(fit$dataValues[1])),
                m = fit, term = fit$params$term_limit, maximum = TRUE)$maximum
}

## Generalized Inverse Gaussian
.meanGIG <- function(fit) {
  m <- GeneralizedHyperbolic::gigMean(param = fit$param)
}

.medianGIG <- function(fit) {
  m <- quantile(fit$randData, probs = 0.5)
}

.modeGIG <- function(fit) {
  m <- GeneralizedHyperbolic::gigMode(param = fit$param)
}

### functions interval estimates
.intervalFunctions <- function() {
  l <- list(
    metalog = list(
      central = .centralInterMetaLog,
      HPD = .hdiMetaLog,
      custom = .customInterMetaLog
    ),
    gig = list(
      central = .centralInterGIG,
      HPD = .hdiGIG,
      custom = .customInterGIG
    )
  )
}

## MetaLog
.centralInterMetaLog <- function(fit, mass) {
  lower <- (1 - mass) / 2
  upper <- 1 - lower
  int <- rmetalog::qmetalog(m = fit, y = c(lower, upper), term = fit$params$term_limit)
}

.hdiMetaLog <- function(fit, mass) {
  samples <- rmetalog::rmetalog(m = fit, n = 1e5, term = fit$params$term_limit)
  int <- HDInterval::hdi(samples, credMass = mass)
}

.customInterMetaLog <- function(fit, lower, upper) {
  int <- rmetalog::qmetalog(m = fit, y = c(lower, upper), term = fit$params$term_limit)
}

## Generalized inverse Gaussian
.centralInterGIG <- function(fit, mass) {
  lower <- (1 - mass) / 2
  upper <- 1 - lower
  int <- quantile(fit$randData, probs = c(lower, upper))
}

.hdiGIG <- function(fit, mass) {
  int <- HDInterval::hdi(fit$randData, credMass = mass)
}

.customInterGIG <- function(fit, lower, upper) {
  int <- quantile(fit$randData, probs = c(lower, upper))
}


### posterior plots
.plotVariancePosteriors <- function(jaspResults, options, parts, operators){

  if(!is.null(jaspResults[["variancePosteriors"]])){
    return()
  }

  variancePosteriors <- createJaspContainer(title = gettext("Posterior Distributions"))
  variancePosteriors$position <- 5
  variancePosteriors$dependOn(c(.mcmcDependencies(),
                                .postPlotDependencies()))
  jaspResults[["variancePosteriors"]] <- variancePosteriors

  fits <- jaspResults[["distFit"]][["object"]]
  titles <- .convertOutputNames(names(fits), parts, operators, includeSigma = FALSE) # note: this function needs to be modified so it produces the right lables for the percentages as well
  postSummary <- jaspResults[["postSummaryStats"]][["object"]] # note: this needs to be replaced if I plot dists for the percentages



  for(i in seq_along(titles)) {
    tempPlot <- createJaspPlot(title = gettext(titles[i]), width = 600, height = 320)

    # select function for axis limits based on distribution
    axisFun <- .axisLimFuns()[[options$distType]]

    lims <- axisFun(fits[[i]], postSummary, options, iter = i)

    p <- ggplot2::ggplot()

    # credible interval
    if(options$posteriorCi) {
      ciUpper <- postSummary[i, "ciUpper"]
      ciLower <- postSummary[i, "ciLower"]

      p <- p +
        if(options$distType == "metalog") {
          ggplot2::stat_function(fun = rmetalog::dmetalog, args = list(m = fits[[i]], term = fits[[i]]$params$term_limit),
                                 geom = "area", xlim = c(ciLower, ciUpper), fill = "grey")
          # note: it might make sense to just pass some approximation of the density to the plotting function in case of the metalog
          # so it only has to evaluate the density function once
        } else { # note: this would be nicer with a list of functions again, but the functions take different arguments
          ggplot2::stat_function(fun = GeneralizedHyperbolic::dgig, args = list(param = fits[[i]]$param),
                                 geom = "area", xlim = c(ciLower, ciUpper), fill = "grey")
        }
    }

    p <- p +
      if(options$distType == "metalog") {
        ggplot2::stat_function(fun = rmetalog::dmetalog, args = list(m = fits[[i]], term = fits[[i]]$params$term_limit),
                               linewidth = 1)
      } else { # note: see above
        ggplot2::stat_function(fun = GeneralizedHyperbolic::dgig, args = list(param = fits[[i]]$param),
                               linewidth = 1)
      }


    # point estimate
    if(options$posteriorPointEstimate) {
      xPoint <- postSummary[i, "pointEstimate"]
      yPoint <- ifelse(options$distType == "metalog",
                       rmetalog::dmetalog(m = fits[[i]], q = xPoint, term = fits[[i]]$params$term_limit),
                       GeneralizedHyperbolic::dgig(x = xPoint, param = fits[[i]]$param))
      pointDf <- data.frame(xPoint, yPoint)

      p <- p + ggplot2::geom_point(data = pointDf, mapping = ggplot2::aes(x = xPoint, y = yPoint),
                                   shape = 21, size = 4, fill = "grey", stroke = 1)
    }

    # axes
    xLab <- titles[i]
    p <- p +
      ggplot2::scale_x_continuous(name = bquote(sigma[.(xLab)]^2), breaks = lims$x$breaks,
                                  limits = lims$x$limits, labels = lims$x$breaks) +
      ggplot2::scale_y_continuous(name = "Density", breaks = lims$y$breaks,
                                  limits = lims$y$limits, labels = lims$y$breaks)

    # theme
    p <- p + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe(sides = "bl")

    tempPlot$plotObject <- p
    variancePosteriors[[titles[i]]] <- tempPlot
  }
  return()
}

## axis limits
.axisLimFuns <- function() {
  l <- list(
    metalog = .axisLimsMetaLog,
    gig = .axisLimsGIG
  )
  return(l)
}

# Metalog
.axisLimsMetaLog <- function(fit, postSummary, options, iter) {

  dfTemp <- fit$dataValues
  m <- .modeMetaLog(fit)

  if(options$posteriorCi) {
    xUpper <- ceiling(max(dfTemp[dfTemp$probs >= 0.975, ]$x_new[1], postSummary[iter, "ciUpper"]))
  } else {
    xUpper <- ceiling(dfTemp[dfTemp$probs >= 0.975, ]$x_new[1])
  }
  xLower <- 0
  xLims <- c(xLower, xUpper)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(xLims)

  yUpper <- rmetalog::dmetalog(m = fit, q = m, term = fit$params$term_limit)
  yLower <- 0
  yLims <- c(yLower, yUpper)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yLims)

  l <- list(
    x = list(limits = xLims,
             breaks = xBreaks),
    y = list(limits = yLims,
             breaks = yBreaks)
  )
  return(l)
}

# Generalized inverse Gaussian
.axisLimsGIG <- function(fit, postSummary, options, iter) {

  quant <- quantile(fit$randData, 0.975) # for upper xLim
  m <- .modeGIG(fit)

  if(options$posteriorCi) {
    xUpper <- ceiling(max(quant, postSummary[iter, "ciUpper"]))
  } else {
    xUpper <- ceiling(quant)
  }
  xLower <- 0
  xLims <- c(xLower, xUpper)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(xLims)

  yUpper <- GeneralizedHyperbolic::dgig(x = m, param = fit$param)
  yLower <- 0
  yLims <- c(yLower, yUpper)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yLims)

  l <- list(
    x = list(limits = xLims,
             breaks = xBreaks),
    y = list(limits = yLims,
             breaks = yBreaks)
  )
  return(l)
}



