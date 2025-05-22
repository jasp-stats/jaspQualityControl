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
    compRes <- .runBFtest(dataset, measurements, parts, operators, options)
  }

  # Model comparison table
  if(options[["RRTable"]]){
    .createBFtable(compRes, jaspResults, dataset, options, measurements, parts, operators, ready)
  }

  # Results from analysis of effects (workaround for accessing the data in jaspResults) ; could be combined with the if statement above
  if(ready) {
    effectsRes <- .fillEffectsTable(compRes, parts, operators)
  }

  # Effects table
  if(options[["effectsTable"]]){
    .createEffectsTable(effectsRes, jaspResults, measurements, parts, operators, ready)
  }

  # MCMC
  if(ready) {
    samplesMat <- .runMCMC(effectsRes, dataset, measurements, parts, operators, options)
  }

  # Variance components table
  .createVarCompTable(effectsRes, samplesMat, jaspResults, parts, operators, ready, options)

}






.createBFtable <- function(compRes, jaspResults, dataset, options, measurements, parts, operators, ready) {
  if(!is.null(jaspResults[["BFtable"]])) {
    return()
  }

  BFtable <- createJaspTable(title = gettext("Model Comparison"))
  BFtable$position <- 1
  BFtable$dependOn(c("operatorWideFormat", "operatorLongFormat", "partWideFormat", "partLongFormat", "measurementsWideFormat",
                     "measurementLongFormat", "seed", "setSeed", "rscalePrior", "RRTable"))

  jaspResults[["BFtable"]] <- BFtable

  BFtable$addColumnInfo(name = "modelName",      title = gettext("Models"),          type = "string")
  BFtable$addColumnInfo(name = "modelPrior",     title = gettext("P(M)"),            type = "number")
  BFtable$addColumnInfo(name = "modelPosterior", title = gettext("P(M|data)"),       type = "number")
  BFtable$addColumnInfo(name = "modelBF",        title = gettext("BF<sub>M</sub>"),  type = "number")
  BFtable$addColumnInfo(name = "comparisonBF",   title = gettext("BF<sub>01</sub>"), type = "number")

  # set data
  if(ready) { # this could also be sth like if(ncol(dataset) == 3)
    BFtable$setData(compRes)
    BFtable$addFootnote("BF<sub>01</sub> compares every model to the null model.")
  }

  return()
}

.runBFtest <- function(dataset, measurements, parts, operators, options) {
  formula <- as.formula(paste(measurements, "~", parts, "*", operators))

  if(options$setSeed) {
    set.seed(options$seed)
  }
  # run general comparison for all potential models
  bf_fit <- BayesFactor::generalTestBF(formula, data = dataset,
                                       # whichRandom = c(operators, parts),
                                       # rscaleRandom = options$rscalePrior,
                                       progress = FALSE)
  bf_df <- as.data.frame(bf_fit)

  # add null model
  bf_df["Null model", ] <- c(1, rep(NA, 3))

  # add prior model probabilities
  bf_df$prior <- rep(1 / nrow(bf_df), nrow(bf_df)) # uniform for now

  # compute P(M | data)
  bf_df <- within(bf_df, unnormalised <- bf * prior)
  bf_df <- within(bf_df, posterior <- unnormalised / sum(unnormalised))

  # dropping unnecessary columns
  bf_df <- bf_df[, !colnames(bf_df) %in% c("error", "time", "code", "unnormalised")]
  colnames(bf_df) <- c("comparisonBF", "modelPrior", "modelPosterior")
  bf_df$modelName <- rownames(bf_df)

  # compute BF_M as the ratio of posterior to prior odds
  bf_df <- within(bf_df, modelBF <- ( modelPosterior / (1-modelPosterior) ) / ( modelPrior / (1-modelPrior) ) )

  bf_df <- bf_df[order(-bf_df$modelBF), ]

  return(bf_df)

}

.createEffectsTable <- function(effectsRes, jaspResults, measurements, parts, operators, ready) {
  if(!is.null(jaspResults[["effectsTable"]])) {
    return()
  }

  effectsTable <- createJaspTable(title = gettext(paste("Analysis of Effects -", measurements)))
  effectsTable$position <- 2
  effectsTable$dependOn(c("operatorWideFormat", "operatorLongFormat", "partWideFormat", "partLongFormat", "measurementsWideFormat",
                          "measurementLongFormat", "seed", "setSeed", "rscalePrior", "effectsTable"))

  jaspResults[["effectsTable"]] <- effectsTable

  effectsTable$addColumnInfo(name = "effectName",         title = gettext("Effects"),           type = "string")
  effectsTable$addColumnInfo(name = "priorInclusion",     title = gettext("P(incl)"),           type = "number")
  effectsTable$addColumnInfo(name = "priorExclusion",     title = gettext("P(excl)"),           type = "number")
  effectsTable$addColumnInfo(name = "posteriorInclusion", title = gettext("P(incl|data)"),      type = "number")
  effectsTable$addColumnInfo(name = "posteriorExclusion", title = gettext("P(excl|data)"),      type = "number")
  effectsTable$addColumnInfo(name = "inclusionBF",        title = gettext("BF<sub>incl</sub>"), type = "number")
  effectsTable$addColumnInfo(name = "exclusionBF",        title = gettext("BF<sub>excl</sub>"), type = "number")

  # set data
  if(ready) {
    #effectsTable$setData(.fillEffectsTable(compRes, parts, operators))
    effectsTable$setData(effectsRes)
  }

  return()
}

.fillEffectsTable <- function(compRes, parts, operators) {
  effectName <- c(parts, operators, paste0(parts, ":", operators))

  priorIncl <- priorExcl <- posteriorIncl <- posteriorExcl <- inclusionBF <- c()

  # loop over different effects and add P(M) and P(M | data) to obtain
  # prior and posterior inclusion probabilities
  for(i in seq_along(effectName)) {
    effect <- grepl(effectName[i], compRes$modelName)

    priorIncl[i] <- sum(compRes$modelPrior[effect])
    priorExcl[i] <- 1 - priorIncl[i]

    posteriorIncl[i] <- sum(compRes$modelPosterior[effect])
    posteriorExcl[i] <- 1 - posteriorIncl[i]

    inclusionBF[i] <- (posteriorIncl[i] / posteriorExcl[i]) / (priorIncl[i] / priorExcl[i])
  }
  return(data.frame(effectName = effectName,
                   priorInclusion = priorIncl,
                   priorExclusion = priorExcl,
                   posteriorInclusion = posteriorIncl,
                   posteriorExclusion = posteriorExcl,
                   inclusionBF = inclusionBF,
                   exclusionBF = 1 / inclusionBF)

  )
}

.createVarCompTable <- function(effectsRes, samplesMat, jaspResults, parts, operators, ready, options) {
  if(!is.null(jaspResults[["varCompTable"]])) {
    return()
  }

  varCompTable <- createJaspTable(title = gettext("Variance Components"))
  varCompTable$position <- 3
  varCompTable$dependOn(c("operatorWideFormat", "operatorLongFormat", "partWideFormat", "partLongFormat", "measurementsWideFormat",
                          "measurementLongFormat", "seed", "setSeed", "rscalePrior", "anovaBFForInteractionRemoval",
                          "mcmcChains", "mcmcBurnin", "mcmcIterations", "historicalSdValue", "processVariationReference"))

  jaspResults[["varCompTable"]] <- varCompTable

  varCompTable$addColumnInfo(name = "sourceName",   title = gettext("Source"),                type = "string")
  varCompTable$addColumnInfo(name = "postMeans",    title = gettext("Mean"),                  type = "number")
  varCompTable$addColumnInfo(name = "postSds",      title = gettext("Std. Deviation"),        type = "number")
  varCompTable$addColumnInfo(name = "postCrIlower", title = gettext("Lower"),                 type = "number", overtitle = gettext("95% Credible Interval"))
  varCompTable$addColumnInfo(name = "postCrIupper", title = gettext("Upper"),                 type = "number", overtitle = gettext("95% Credible Interval"))
  varCompTable$addColumnInfo(name = "contribution", title = gettext("% Contribution<br> (Mean)"), type = "number")


  # set data
  if(ready) {
    varCompTable$setData(.getVarianceComponents(effectsRes, samplesMat, parts, operators, options))
  } else {
    return()
  }

  return()
}

.runMCMC <- function(effectsRes, dataset, measurements, parts, operators, options){
  # extract exclBF for interaction
  excludeInter <- .evalInter(effectsRes, parts, operators, options)
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
  # note this could be written into a helper function
  sigmaPart <- paste0("g_", parts)
  sigmaOperator <- paste0("g_", operators)
  sigmaInter <- paste0("g_", parts, ":", operators)

  if(excludeInter){
    chains <- chains[, c(sigmaPart, sigmaOperator, "sig2")]
  } else {
    chains <- chains[, c(sigmaPart, sigmaOperator, sigmaInter, "sig2")]
  }

  samplesMat <- as.matrix(chains)

  return(samplesMat)
}


.getVarianceComponents <- function(effectsRes, samplesMat, parts, operators, options) {
  excludeInter <- .evalInter(effectsRes, parts, operators, options)

  # get components from MCMC samples
  internalDF <- .getComponentsFromSamples(samplesMat, parts, operators, options, excludeInter)

  # %Contribution to total variance
  contribution <- matrix(ncol = ncol(internalDF), nrow = nrow(internalDF))
  for(i in 1:ncol(internalDF)){
    contribution[, i] <- internalDF[[i]] / internalDF$total * 100
  }

  # calculate summary stats
  postMeans <- colMeans(internalDF)
  postSds <- apply(internalDF, 2, sd)
  postCrIlower <- apply(internalDF, 2, quantile, probs = 0.025)
  postCrIupper <- apply(internalDF, 2, quantile, probs = 0.975)
  contribution <- colMeans(contribution)

  # remove some stats when historicalSd is specified
  if(options$processVariationReference == "historicalSd"){
    postSds["part"] <- ""
    postSds["total"] <- ""
    postCrIlower["part"] <- ""
    postCrIlower["total"] <- ""
    postCrIupper["part"] <- ""
    postCrIupper["total"] <- ""
  }


  sourceName <- c("Total gauge r&R",
                  "Repeatability",
                  "Reproducibility",
                  "Operator",
                  "Part-to-part",
                  "Total variation")

  return(data.frame(sourceName,
                    postMeans,
                    postSds,
                    postCrIlower,
                    postCrIupper,
                    contribution)
         )
}

.getGaugeEval <- function(samplesMat){

}

.evalInter <- function(effectsRes, parts, operators, options) {
  ind <- effectsRes$effectName == paste0(parts, ":", operators)
  excludeInter <- effectsRes[ind, "exclusionBF"] >= options$anovaBFForInteractionRemoval

  return(excludeInter)
}

.getComponentsFromSamples <- function(samplesMat, parts, operators, options, excludeInter){
  # note this could be written into a helper function
  sigmaPart <- paste0("g_", parts)
  sigmaOperator <- paste0("g_", operators)
  sigmaInter <- paste0("g_", parts, ":", operators)

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
