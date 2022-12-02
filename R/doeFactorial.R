#
# Copyright (C) 2013-2021 University of Amsterdam
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
doeFactorial <- function(jaspResults, dataset, options, ...) {
  if (options[["setSeed"]]) {
    set.seed(options[["seed"]])
  }

  ################### Design ###################
  designContainer <- createJaspContainer("Design", position = 1)

  designContainer[["ShowAvailableDesigns"]] <- .doeFactorialShowAvailableDesigns(options, jaspResults)
  designContainer[["SummarySelectedDesign"]] <- .doeFactorialSummarySelectedDesign(options, jaspResults)
  designContainer[["ShowSelectedDesign"]] <- .doeFactorialShowSelectedDesign(options, jaspResults)$table
  designContainer[["aliasStructure"]] <- .doeFactorialShowSelectedDesign(options, jaspResults)$aliasStructure

  jaspResults[["Design"]] <- designContainer

  ################### Analysis ###################
  factorVariables <- unlist(options$FAassignedFactors)
  factorVariables <- factorVariables[factorVariables != ""]

  reorderModelTerms <- .reorderModelTerms(options)
  modelTerms <- reorderModelTerms$modelTerms
  model <- .modelFormula(modelTerms, options)

  # Ready check: More than two factors, a response and order variables, and either model terms or interaction order specified.
  ready <- (length(options[["FAassignedFactors"]]) >= 2 && options[["FAresponse"]] != "" && (!is.null(unlist(options$modelTerms)) || options$enabledIntOrder))
  if (ready) {
    dataset <- .factorialAnalysisReadData(dataset, options)

    # Transform to coded
    factors <- unlist(dataset[, options[["FAassignedFactors"]]], use.names = FALSE)
    response <- unlist(dataset[, options[["FAresponse"]]], use.names = FALSE)

    perF <- length(factors) / length(options[["FAassignedFactors"]])
    factorsDF <- data.frame(split(factors, ceiling(seq_along(factors) / perF)))

    for (i in 1:ncol(factorsDF)) {
      factorsDF[, i][factorsDF[, i] == min(factorsDF[, i])] <- -1
      factorsDF[, i][factorsDF[, i] == max(factorsDF[, i])] <- 1
    }

    # Use original variables names

    if (options[["FArunOrder"]] != "") {
      runOrder <- dataset[, unlist(options$FArunOrder)]
      names <- c(options$FArunOrder, options$FAassignedFactors, options$FAresponse)
      datasetRow <- cbind.data.frame(runOrder, factorsDF, response)
    } else {
      names <- c(options$FAassignedFactors, options$FAresponse)
      datasetRow <- cbind.data.frame(factorsDF, response)
    }

    names(datasetRow) <- names

    dataset <- datasetRow
  }

  model.fit <- .factorialRegression(jaspResults, dataset, options, model, ready)
  fit <- model.fit$fit
  saturated <- model.fit$saturated

  analysisContainer <- createJaspContainer(title = gettext("Analysis"), position = 2)

  if (is.null(jaspResults[["factorialRegressionANOVA"]])) {
    analysisContainer[["tableAnova"]] <- .factorialRegressionANOVAcreateTable(options, ready, fit, saturated, model)$tableAnova
    analysisContainer[["tableFit"]] <- .factorialRegressionANOVAcreateTable(options, ready, fit, saturated, model)$tableFit
  }

  if (is.null(jaspResults[["factorialRegressionCoefficients"]])) {
    analysisContainer[["tableCoef"]] <- .factorialRegressionCoefficientsCreateTable(options, ready, fit, saturated, model)$tableCoef
    analysisContainer[["tableFormula"]] <- .factorialRegressionCoefficientsCreateTable(options, ready, fit, saturated, model)$tableFormula
  }

  if (ready) {
    jaspResults[["Design"]] <- NULL

    # Create error plot for saturated designs
    if (saturated) {
      errorPlot <- createJaspPlot()
      errorPlot$setError(gettext("This plot is unavailable for saturated designs."))
      errorPlot$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder"))
    }

    if (options[["showAliasStructure2"]]) {
      # Error plot for saturated designs
      analysisContainer[["showAliasStructure2"]] <- .factorialShowAliasStructure(jaspResults, options, dataset, fit)
    }


    # Normal Probability Plot of Residuals
    if (is.null(jaspResults[["resNorm"]]) && options[["resNorm"]]) {
      NormalPlot <- createJaspContainer(title = gettext("Normal Probability Plot of Residuals"))
      NormalPlot$dependOn(c("FAassignedFactors", "modelTerms", "NormalPlot", "FAresponse", "intOrder", "FArunOrder", "addGridlines", "enabledIntOrder"))

      # Error plot for saturated designs
      if (saturated) {
        NormalPlot[["plot"]] <- errorPlot
      } else {
        NormalPlot[["plot"]] <- .qcProbabilityPlot(dataset, options, fit = fit)
      }

      analysisContainer[["resNorm"]] <- NormalPlot
    }

    # Histogram of Residuals
    if (options[["resHist"]] && is.null(jaspResults[["resHist"]])) {
      resHist <- createJaspContainer(gettext("Histogram of Residuals"))
      resHist$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder"))

      if (saturated) {
        resHist[["plot"]] <- errorPlot
      } else {
        resHist[["plot"]] <- .factorialResHist(jaspResults = jaspResults, options = options, fit = fit)$jaspPlot
      }

      analysisContainer[["resHist"]] <- resHist
    }

    # Residuals vs. Fitted Value
    if (options[["resFitted"]] && is.null(jaspResults[["resFitted"]])) {
      resFitted <- createJaspContainer(gettext("Residuals vs. Fitted Value"))
      resFitted$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder"))

      if (saturated) {
        resFitted[["plot"]] <- errorPlot
      } else {
        resFitted[["plot"]] <- .factorialResFitted(jaspResults = jaspResults, options = options, fit = fit)$jaspPlot
      }

      analysisContainer[["resFitted"]] <- resFitted
    }

    # Residuals vs. Run Order
    if (options[["resOrder"]] && is.null(jaspResults[["resOrder"]])) {
      if (options$runOrderPlot == "runOrderRandomPlot") {
        resOrder <- createJaspContainer(gettext("Residuals vs. Run Order"))
      } else {
        resOrder <- createJaspContainer(gettext("Residuals vs. Standard Order"))
      }

      resOrder$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder"))

      if (saturated) {
        resOrder[["plot"]] <- errorPlot
      } else {
        resOrder[["plot"]] <- .factorialResOrder(jaspResults = jaspResults, dataset = dataset, options = options, fit = fit)$jaspPlot
      }

      analysisContainer[["resOrder"]] <- resOrder
    }

    # Pareto Plot of Standardized Effects
    if (options[["paretoPlot"]] && is.null(jaspResults[["paretoPlot"]])) {
      if (saturated) {
        analysisContainer[["paretoPlot"]] <- errorPlot
      } else {
        analysisContainer[["paretoPlot"]] <- .factorialPareto(jaspResults = jaspResults, options = options, fit = fit)
      }
    }

    # Four in one plot
    if (options[["fourInOne"]] && is.null(jaspResults[["fourInOne"]])) {
      if (saturated) {
        fourInOne[["fourInOne"]] <- errorPlot
      } else {
        matrixPlot <- createJaspPlot(title = gettextf("Four in one for %s", unlist(options$FAresponse)), width = 1100, aspectRatio = 1)
        matrixPlot$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder", "fourInOne"))

        plotMat <- matrix(list(), 2, 2)
        plotMat[[1, 1]] <- .qcProbabilityPlot(dataset, options, fit = fit, ggPlot = TRUE)
        plotMat[[1, 2]] <- .factorialResFitted(jaspResults = jaspResults, options = options, fit = fit)$ggPlot
        plotMat[[2, 1]] <- .factorialResHist(jaspResults = jaspResults, options = options, fit = fit)$ggPlot
        plotMat[[2, 2]] <- .factorialResOrder(jaspResults = jaspResults, dataset = dataset, options = options, fit = fit)$ggPlot

        matrixPlot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)
      }

      analysisContainer[["fourInOne"]] <- matrixPlot
    }
    jaspResults[["Analysis"]] <- analysisContainer
  }
}

################### Design ###################
.doeFactorialShowAvailableDesigns <- function(options, jaspResults) {
  if (!options[["showAvailableDesigns"]]) {
    jaspResults[["displayDesigns"]] <- NULL
    return()
  }

  if (is.null(jaspResults[["displayDesigns"]])) {
    tableTitle <- gettext("Available Factorial Designs")

    table <- createJaspTable(tableTitle)

    table$dependOn(options = "numberOfFactors")

    table$addColumnInfo(name = "runs", title = "Runs", type = "integer")
    # table$addColumnInfo(name = 'res', title = "Resolution", type = 'string')

    runs <- 2^seq(floor(log2(options[["numberOfFactors"]])) + 1, 12)
    if (length(runs) > 5) {
      runs <- runs[1:5]
    }
    if (options[["numberOfFactors"]] > 32) {
      runs <- runs[1:4]
    }

    # res <- numeric(length(runs))
    # for(i in 1:length(runs)){
    #  if(log2(runs[i]) < options[["numberOfFactors"]]){
    #    res[i] <- as.character(as.roman(DoE.base::design.info(FrF2::FrF2(nfactors = options[["numberOfFactors"]],
    #                                                                     nruns = runs[i]))$catlg.entry[[1]]$res))
    #  } else {
    #    res[i] <- "Full"
    #  }
    # }
    rows <- runs

    # equal <- numeric(0)
    # for(i in 1:(nrow(rows)-1)){
    #  if(rows$res[i] == rows$res[i+1]){
    #    equal <- append(equal, i+1)
    #  }
    # }
    # rows <- rows[-equal,]

    jaspResults[["state"]] <- createJaspState(rows)
    jaspResults[["state"]]$dependOn("always_present")

    table$setData(rows)

    return(table)
  }
}

.doeFactorialSummarySelectedDesign <- function(options, jaspResults) {
  if (is.null(jaspResults[["selectedDesign"]])) {
    table <- createJaspTable(gettext("Selected Design"))

    table$dependOn(options = c(
      "factors",
      "factorialRuns",
      "factorialCenterPoints",
      "factorialCornerReplicates",
      "factorialBlocks",
      "factorialType",
      "designBy",
      "factorialResolution",
      "factorialFraction",
      "numberOfFactors",
      "repeatRuns"
    ))

    table$addColumnInfo(name = "type", title = gettext("Design"), type = "string")
    table$addColumnInfo(name = "factors", title = gettext("Factors"), type = "integer")
    table$addColumnInfo(name = "runs", title = gettext("Runs"), type = "integer")
    table$addColumnInfo(name = "resolution", title = gettext("Resolution"), type = "string")
    table$addColumnInfo(name = "centers", title = gettext("Centre points"), type = "integer")
    table$addColumnInfo(name = "replicates", title = gettext("Replicates"), type = "integer")
    table$addColumnInfo(name = "blocks", title = gettext("Blocks"), type = "integer")
    table$addColumnInfo(name = "repeats", title = gettext("Repeats"), type = "integer")

    designs <- jaspResults[["state"]]$object

    if (options[["designBy"]] == "designByRuns") {
      runs <- (as.numeric(options[["factorialRuns"]]) +
        options[["factorialCenterPoints"]] *
          options[["factorialBlocks"]]) *
        options[["factorialCornerReplicates"]] +
        options[["repeatRuns"]]

      # Resolution based on SKF's diagram
      sumResolution <- runs + options[["numberOfFactors"]]
      resolution.Full <- c(6, 11, 20, 37, 70, 135)
      resolution.six <- c(38, 137)
      resolution.Five <- c(21, 72, 138, 139)
      resolution.three <- c(7, 13, 14, 15, 25, 26, 27, 28, 29, 30, 31)


      if (sumResolution %in% resolution.Full) {
        res <- "Full"
      } else if (sumResolution %in% resolution.six) {
        res <- "VI"
      } else if (sumResolution %in% resolution.Five) {
        res <- "V"
      } else if (sumResolution %in% resolution.three) {
        res <- "III"
      } else if (any(sumResolution == 137)) {
        res <- "VIII"
      } else if (any(sumResolution == 71)) {
        res <- "VII"
      } else {
        res <- "III"
      }
    } else if (options[["designBy"]] == "designByResolution") {
      resolution <- if (options[["factorialResolution"]] == "Full") {
        100
      } else {
        as.numeric(as.roman(options[["factorialResolution"]]))
      }
      runs <- (DoE.base::design.info(FrF2::FrF2(
        nfactors = options[["numberOfFactors"]],
        resolution = resolution
      ))$nruns +
        options[["factorialCenterPoints"]] *
          options[["factorialBlocks"]]) *
        options[["factorialCornerReplicates"]] +
        options[["repeatRuns"]]
    } else {
      runs <- ((2^options[["numberOfFactors"]] * as.numeric(options[["factorialFraction"]])) +
        options[["factorialCenterPoints"]] *
          options[["factorialBlocks"]]) *
        options[["factorialCornerReplicates"]] +
        options[["repeatRuns"]]
    }

    design <- base::switch(options[["factorialType"]],
      "factorialTypeDefault" = gettext("2-level factorial")
    )

    rows <- data.frame(
      type = "Factorial",
      factors = options[["numberOfFactors"]],
      runs = runs,
      resolution = res,
      centers = options[["factorialCenterPoints"]],
      replicates = options[["factorialCornerReplicates"]],
      blocks = options[["factorialBlocks"]],
      repeats = options[["repeatRuns"]]
    )

    table$setData(rows)
    return(table)
  }
}

.doeFactorialShowSelectedDesign <- function(options, jaspResults) {
  if (!options[["displayDesign"]]) {
    return()
  }

  if (is.null(jaspResults[["displayDesign"]])) {
    table <- createJaspTable(gettext("Design Preview"))

    table$dependOn(options = c(
      "displayDesign",
      "dataCoding",
      "factors",
      "factorialType",
      "factorialRuns",
      "factorialCenterPoints",
      "factorialCornerReplicates",
      "factorialBlocks",
      "factorialRepeats",
      "factorialResolution",
      "factorialFraction",
      "numberOfFactors",
      "showAliasStructure",
      "factorialTypeSpecifyGenerators",
      "numberHTCFactors",
      "runOrder",
      "repeatRuns",
      "file",
      "actualExporter"
    ))

    results <- options[["factors"]]

    factorNames <- factorLows <- factorHighs <- character()

    for (i in 1:length(results)) {
      factorNames[i] <- paste0(results[[i]]$factorName, " (", i, ")")
      factorLows[i] <- results[[i]]$low
      factorHighs[i] <- results[[i]]$high1
    }

    rep <- options[["factorialRepeats"]] > 0

    if (options[["factorialType"]] == "factorialTypeDefault") {
      if (options[["designBy"]] == "designByRuns") {
        desx <- FrF2::FrF2(
          nfactors = options[["numberOfFactors"]],
          nruns = as.numeric(options[["factorialRuns"]]),
          ncenter = options[["factorialCenterPoints"]],
          replications = options[["factorialCornerReplicates"]],
          repeat.only = rep,
          blocks = options[["factorialBlocks"]]
        )
      } else if (options[["designBy"]] == "designByResolution") {
        desx <- FrF2::FrF2(
          nfactors = options[["numberOfFactors"]],
          resolution =
            if (options[["factorialResolution"]] != "Full") {
              as.numeric(as.roman(options[["factorialResolution"]]))
            } else {
              999
            },
          ncenter = options[["factorialCenterPoints"]],
          replications = options[["factorialCornerReplicates"]],
          repeat.only = rep
        )
      } else {
        desx <- FrF2::FrF2(
          nfactors = options[["numberOfFactors"]],
          nruns = (2^options[["numberOfFactors"]]) * as.numeric(options[["factorialFraction"]]),
          ncenter = options[["factorialCenterPoints"]],
          replications = options[["factorialCornerReplicates"]],
          repeat.only = rep,
          blocks = options[["factorialBlocks"]]
        )
      }
    } else if (options[["factorialType"]] == "factorialTypeSpecify") {
      whichHow <- strsplit(gsub(" ", "", strsplit(options[["factorialTypeSpecifyGenerators"]], ",")[[1]], fixed = TRUE), "=")
      if (length(whichHow) == 0) {
        return()
      }
      gen <- character(length(whichHow))
      for (i in 1:length(whichHow)) {
        gen[i] <- whichHow[[i]][length(whichHow[[i]])]
      }
      desx <- FrF2::FrF2(
        nfactors = options[["numberOfFactors"]],
        nruns = if (options[["designBy"]] == "designByRuns") {
          as.numeric(options[["factorialRuns"]])
        } else {
          2^options[["numberOfFactors"]] * as.numeric(options[["factorialFraction"]])
        },
        generators = gen,
        ncenter = options[["factorialCenterPoints"]],
        replications = options[["factorialCornerReplicates"]],
        repeat.only = rep
      )
    } else if (options[["factorialType"]] == "factorialTypeSplit") {
      if (options[["designBy"]] == "designByRuns") {
        desx <- FrF2::FrF2(
          nfactors = options[["numberOfFactors"]],
          nruns = as.numeric(options[["factorialRuns"]]),
          hard = options[["numberHTCFactors"]],
          replications = options[["factorialCornerReplicates"]],
          repeat.only = rep
        )
      } else if (options[["designBy"]] == "designByResolution") {
        desx <- FrF2::FrF2(
          nfactors = options[["numberOfFactors"]],
          resolution = if (options[["factorialResolution"]] != "Full") {
            as.numeric(as.roman(options[["factorialResolution"]]))
          } else {
            999
          },
          hard = options[["numberHTCFactors"]],
          replications = options[["factorialCornerReplicates"]],
          repeat.only = rep
        )
      } else {
        desx <- FrF2::FrF2(
          nfactors = options[["numberOfFactors"]],
          nruns = (2^options[["numberOfFactors"]]) * as.numeric(options[["factorialFraction"]]),
          hard = options[["numberHTCFactors"]],
          replications = options[["factorialCornerReplicates"]],
          repeat.only = rep
        )
      }
      table$addFootnote(paste("Hard-to-change factors: ", paste(factorNames[1:options[["numberHTCFactors"]]], collapse = ", "), sep = ""))
    }

    runOrder <- 1:nrow(desx)
    rows <- cbind.data.frame(runOrder, desx)

    blocks <- rows$Blocks
    rows <- rows[, !names(rows) %in% "Blocks"]

    # filling in table
    table$addColumnInfo(name = "runOrder", title = gettext("Run order"), type = "string")

    tableTitle <- if (options[["factorialBlocks"]] > 1) {
      "standard.block.perblock"
    } else {
      "Standard order"
    }

    table$addColumnInfo(name = "runOrderStandard", title = tableTitle, type = "integer")
    runOrderStandard <- DoE.base::run.order(desx)[, 1]
    rows <- cbind.data.frame(runOrder = runOrder, runOrderStandard = runOrderStandard, desx)

    rows <- rows[, !names(rows) %in% "Blocks"]

    for (i in 1:options[["numberOfFactors"]]) {
      colnames(rows)[i + 2] <- factorNames[i]
      table$addColumnInfo(name = factorNames[i], title = factorNames[i], type = "string")
    }

    # DATA CODING
    if (options[["factorialCenterPoints"]] >= 1) {
      rows[, (1 + 2):ncol(rows)] <- sapply(rows[, (1 + 2):ncol(rows)], as.numeric)
    } else {
      rows[, (1 + 2):ncol(rows)] <- sapply(rows[, (1 + 2):ncol(rows)], as.numeric) * 2 - 3
    }
    if (options[["dataCoding"]] == "dataUncoded") {
      for (i in 1:options[["numberOfFactors"]]) {
        rows[, i + 2][rows[, i + 2] == -1] <- if (any(factorLows[i] == "1")) {
          "1.0"
        } else {
          factorLows[i]
        }

        if (options[["factorialCenterPoints"]] >= 1) {
          rows[, i + 2][rows[, i + 2] == 0] <-
            if (!is.na(as.numeric(factorLows[i]) + as.numeric(factorHighs[i]))) {
              (as.numeric(factorLows[i]) + as.numeric(factorHighs[i])) / 2
            } else {
              "center"
            }
        }
        rows[, i + 2][rows[, i + 2] == 1] <- factorHighs[i]
      }
    }

    if (options[["repeatRuns"]] > 0) {
      repeats <- sample(1:nrow(rows), options[["repeatRuns"]])
      for (i in repeats) {
        rows <- rbind(rows[1:i, ], rows[i, ], rows[-(1:i), ])
      }
      rows[, 1] <- 1:nrow(rows)
    }

    if (options[["runOrder"]] == "runOrderRandom") {
      rows <- rows[order(rows$runOrder), ]
    } else {
      if (options[["factorialBlocks"]] > 1) {
        hmm <- DoE.base::run.order(desx)
        hmmRunDF <- as.data.frame(strsplit(as.character(hmm[, 1]), ".", fixed = T))
        k <- 0
        actualOrder <- numeric(0)
        for (i in 1:options[["factorialBlocks"]]) {
          actualOrder <- append(x = actualOrder, values = (order(as.numeric(hmmRunDF[, hmmRunDF[2, ] == i][1, ])) + k))
          k <- k + length(hmmRunDF[, hmmRunDF[2, ] == i][1, ])
        }
        rows <- rows[actualOrder, ]
      } else {
        rows <- rows[order(rows$runOrderStandard), ]
      }
    }

    table$setData(rows)
    aliasStructure <- .doeFactorialShowAliasStructure(options, jaspResults, factorialDesign = desx)

    # export design
    if (options[["actualExporter"]] && options[["file"]] != "") {
      exportDesign <- data.frame(rows)
      exportDesign <- cbind(exportDesign, rep(NA, nrow(exportDesign)))
      colnames(exportDesign)[ncol(exportDesign)] <- "Response"
      utils::write.csv(x = exportDesign, file = options[["file"]], row.names = FALSE, na = "", quote = FALSE)
    }

    return(list(table = table, aliasStructure = aliasStructure))
  }
}

.doeFactorialShowAliasStructure <- function(options, jaspResults, factorialDesign, onlyTable = FALSE) {
  if (!options[["showAliasStructure"]]) {
    jaspResults[["showAliasStructure"]] <- NULL
    return()
  }

  if (is.null(jaspResults[["showAliasStructure"]])) {
    table <- createJaspTable(gettext("Alias Structure"))
    table$dependOn(options = c(
      "numberOfFactors",
      "factorialRuns",
      "factorialResolution"
    ))

    Aliases <- FrF2::aliasprint(factorialDesign)
    rows <- data.frame(Aliases = c(Aliases$main, Aliases$fi2))

    jaspResults[["state2"]] <- createJaspState(rows)
    jaspResults[["state2"]]$dependOn("always_present")

    table$setData(rows)

    return(table)
  }
}

################### Analysis ###################
.factorialAnalysisReadData <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  } else {
    dataset <-
      if (options[["FArunOrder"]] != "") {
        .readDataSetToEnd(columns.as.numeric = c(
          options[["FAresponse"]],
          options[["FArunOrder"]],
          options[["FAassignedFactors"]]
        ))
      } else {
        .readDataSetToEnd(columns.as.numeric = c(
          options[["FAresponse"]],
          options[["FAassignedFactors"]]
        ))
      }

    # Error check
    .hasErrors(dataset,
      type = c("infinity", "missingValues"),
      all.target = c(options[["FAresponse"]], options[["FArunOrder"]], options[["FAassignedFactors"]]),
      exitAnalysisIfErrors = TRUE
    )
    return(dataset)
  }
}

.factorialRegression <- function(jaspResults, dataset, options, model, ready, ...) {
  if (!ready) {
    return()
  }

  if (options$enabledIntOrder) {
    reponseName <- unlist(options$FAresponse)
    factorsNames <- unlist(options$FAassignedFactors)
    runOrderName <- unlist(options$FArunOrder)

    factors <- unlist(dataset[, options[["FAassignedFactors"]]], use.names = FALSE)
    response <- unlist(dataset[, options[["FAresponse"]]], use.names = FALSE)

    perF <- length(factors) / length(options[["FAassignedFactors"]])
    factorsDF <- data.frame(split(factors, ceiling(seq_along(factors) / perF)))
    forFit <- cbind.data.frame(factorsDF, response)

    names <- factorsNames
    colnames(forFit) <- c(names, "response")

    order <- as.numeric(options[["intOrder"]])

    fit <- if (order == 1) {
      lm(response ~ ., forFit)
    } else {
      lm(paste0("response ~ (.)^", order), forFit)
    }
  } else {
    model.formula <- as.formula(model$model.def)
    fit <- lm(model.formula, dataset)
  }
  saturated <- summary(fit)$df[2] == 0

  return(list(fit = fit, saturated = saturated))
}

.factorialResNorm <- function(jaspResults, options, fit) {
  plot <- createJaspPlot(width = 400, height = 400)
  plot$dependOn("resNorm")

  p <- jaspGraphs::plotQQnorm(resid(fit))

  plot$plotObject <- p

  return(list(jaspPlot = plot, ggPlot = p))
}

.factorialResHist <- function(jaspResults, options, fit) {
  plot <- createJaspPlot(width = 400, height = 400)
  plot$dependOn("resHist")

  x <- resid(fit)
  h <- hist(x, plot = FALSE)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(h$breaks * 1.1)
  yBreaks <- c(0, jaspGraphs::getPrettyAxisBreaks(h$counts))

  p <- ggplot2::ggplot(data.frame(x), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(fill = "grey", col = "black") +
    ggplot2::scale_x_continuous(name = "Residuals", limits = range(xBreaks), breaks = xBreaks) +
    ggplot2::scale_y_continuous(name = "Frequency", limits = range(yBreaks), breaks = yBreaks)

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(list(jaspPlot = plot, ggPlot = p))
}

.factorialResFitted <- function(jaspResults, options, fit) {
  plot <- createJaspPlot(width = 400, height = 400)
  plot$dependOn("resFitted")

  df <- data.frame(x = fitted(fit), y = resid(fit))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df$x)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(df$y)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
    ggplot2::scale_x_continuous(name = gettext("Fitted values"), limits = c(min(xBreaks), max(xBreaks)), breaks = xBreaks) +
    ggplot2::scale_y_continuous(name = gettext("Residuals"), limits = c(min(yBreaks), max(yBreaks)), breaks = yBreaks)

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(list(jaspPlot = plot, ggPlot = p))
}

.factorialResOrder <- function(jaspResults, dataset, options, fit) {
  plot <- createJaspPlot(width = 400, height = 400)
  plot$dependOn("resOrder")

  runOrder <- unlist(dataset[, options[["FArunOrder"]]], use.names = FALSE)

  df <- data.frame(runOrder = runOrder, x = 1:length(runOrder), y = resid(fit))

  if (options$runOrderPlot == "runOrderStandardPlot") {
    df <- df[order(runOrder), ]
    df$x <- df$runOrder <- 1:nrow(df)
    xlabel <- gettext("Standard order")
  }

  xlabel <- gettext("Run order")
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(df$y)
  xBreaks <- df$x

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
    ggplot2::scale_x_continuous(name = xlabel, limits = c(min(xBreaks), max(xBreaks)), breaks = xBreaks, labels = df$runOrder) +
    ggplot2::scale_y_continuous(name = gettext("Residuals"), limits = c(min(yBreaks), max(yBreaks)), breaks = yBreaks)

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(list(jaspPlot = plot, ggPlot = p))
}

.factorialPareto <- function(jaspResults, options, fit, onlyPlot = FALSE) {
  plot <- createJaspPlot(title = gettext("Pareto Chart of Standardized Effects"), width = 400, height = 400)
  plot$dependOn(c("FAassignedFactors", "modelTerms", "intOrder", "FArunOrder", "enabledIntOrder", "paretoPlot"))

  t <- abs(data.frame(summary(fit)$coefficients)$t.value[-1])
  fac <- names(coef(fit))[-1]
  df <- summary(fit)$df[2]
  crit <- abs(qt(0.025, df))
  yLab <- gettext("Standardized Effect")

  fac_t <- cbind.data.frame(fac, t)
  fac_t <- cbind(fac_t[order(fac_t$t), ], y = 1:length(t))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(t, crit))

  p <- ggplot2::ggplot(fac_t, ggplot2::aes(y = t, x = y)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = crit, linetype = "dashed", color = "red") +
    ggplot2::scale_x_continuous(name = gettext("Term"), breaks = fac_t$y, labels = fac_t$fac) +
    ggplot2::scale_y_continuous(name = yLab, breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::coord_flip()

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  if (onlyPlot) {
    return(p)
  } else {
    return(plot)
  }
}

.factorialRegressionANOVAcreateTable <- function(options, ready, fit, saturated, model) {
  if (!is.null(fit) && ready) {
    results <- .factorialRegressionANOVAfillTable(factorialRegressionANOVA, factorialRegressionSummaryFit, options, fit, saturated, model)
    tableAnova <- results$ANOVA
    tableFit <- results$SummaryFit
  } else {
    tableAnova <- createJaspTable(gettext("Analysis of variance"), position = 1)
    tableFit <- createJaspTable(gettext("Model summary"), position = 2)
  }

  return(list(tableAnova = tableAnova, tableFit = tableFit))
}

.factorialRegressionANOVAfillTable <- function(factorialRegressionANOVA, factorialRegressionSummaryFit, options, fit, saturated, model) {
  facotrsName <- unlist(options$FAassignedFactors)
  response <- unlist(options$FAresponse)
  n.factors <- length(facotrsName)

  if (!saturated) {
    anova <- car::Anova(fit)
    anova$`Mean Sq` <- anova$`Sum Sq` / anova$Df
  } else {
    anova <- summary(aov(fit))
    anova <- anova[[1]]
  }

  names.64 <- names(coef(fit))
  null.names <- names(fit$coefficients)[is.na(fit$coefficients)]
  names <- c("Model", gsub(" ", "", row.names(anova)[-length(row.names(anova))], fixed = TRUE), null.names, "Error", "Total")
  anovaNames <- gsub(" ", "", row.names(anova))
  errorIndex <- which(anovaNames == "Residuals")

  model.SS <- sum(anova$`Sum Sq`[-errorIndex])
  model.MS <- sum(anova$`Sum Sq`[-errorIndex]) / sum(anova$Df[-errorIndex])
  model.F <- model.MS / anova$`Mean Sq`[errorIndex]
  model.Pval <- pf(model.F, sum(anova$Df[-errorIndex]), anova$Df[errorIndex], lower.tail = F)

  # Pure error for replicates
  fit.names <- names(fit$model)[-1]
  formula.facotrsName <- fit.names[1]
  for (i in 2:length(facotrsName)) {
    formula.facotrsName <- gettextf("%s,%s", formula.facotrsName, fit.names[i])
  } # create RSM model formula

  formula.rsm <- gettextf("%s ~ FO(%s)", names(fit$model)[1], formula.facotrsName)
  fit.rsm <- rsm::rsm(as.formula(formula.rsm), fit$model) # fit rsm model
  rsm <- summary(fit.rsm)
  anova.rsm <- rsm[[13]] # anova table

  if (!saturated) {
    df <- c(sum(anova$Df[-errorIndex]), anova$Df[-errorIndex], rep(NA, length(null.names)), anova$Df[errorIndex], sum(anova$Df))
    Adj.SS <- c(model.SS, anova$`Sum Sq`[-errorIndex], rep(NA, length(null.names)), anova$`Sum Sq`[errorIndex], sum(anova$`Sum Sq`))
    Adj.MS <- c(model.MS, anova$`Mean Sq`[-errorIndex], rep(NA, length(null.names)), anova$`Mean Sq`[errorIndex], NA)
    `F` <- c(model.F, anova$`F value`, rep(NA, length(null.names)), NA)
    p <- c(model.Pval, anova$`Pr(>F)`, rep(NA, length(null.names)), NA)

    anovaFill <- data.frame(
      Terms = names,
      df = round(df, 3),
      Adj.SS = round(Adj.SS, 3),
      Adj.MS = round(Adj.MS, 3),
      `F` = round(`F`, 3),
      p = round(p, 3)
    )

    # If Pure error
    if (anova.rsm[3, ][1] != 0 && anova.rsm[4, ][1] != 0) {
      LackFit <- t(as.data.frame(c(terms = "Lack of fit", round(unlist(anova.rsm[3, ]), 3))))
      Pure.Error <- t(as.data.frame(c(terms = "Pure error", round(unlist(anova.rsm[4, ]), 3))))
      Total.index <- length(names)
      colnames(LackFit) <- colnames(anovaFill)
      colnames(Pure.Error) <- colnames(anovaFill)
      anovaFill.Total <- anovaFill[Total.index, ]
      anovaFill <- rbind(anovaFill[-Total.index, ], LackFit, Pure.Error, anovaFill[Total.index, ])
    }

    modelSummaryFill <- data.frame(
      S = summary(fit)$sigma,
      R1 = summary(fit)$r.squared * 100,
      R2 = summary(fit)$adj.r.squared * 100
    )
  } else {
    model.SS <- sum(anova$`Sum Sq`)
    model.MS <- model.SS / nrow(anova)
    names <- c("Model", names(coef(fit))[!is.na(coef(fit))][-1], "Error", "Total")

    anovaFill <- data.frame(
      Terms = names,
      df = c(sum(anova$Df[1:n.factors]), anova$Df, 0, sum(anova$Df)),
      Adj.SS = c(round(c(model.SS, anova$`Sum Sq`), 3), "*", round(sum(anova$`Sum Sq`), 3)),
      Adj.MS = c(round(c(model.MS, anova$`Mean Sq`), 3), "*", "*"),
      `F` = rep("*", length(names)),
      p = rep("*", length(names))
    )

    modelSummaryFill <- data.frame(
      S = NA,
      R1 = NA,
      R2 = NA
    )
  }

  anovaFill$Terms <- jaspBase::gsubInteractionSymbol(anovaFill$Terms)

  factorialRegressionANOVA <- createJaspTable(gettext("Analysis of variance"), position = 1)
  factorialRegressionSummaryFit <- createJaspTable(gettext("Model Summary"), position = 2)

  factorialRegressionANOVA$dependOn(c("FAassignedFactors", "modelTerms", "FAresponse", "intOrder", "FArunOrder", "enabledIntOrder"))
  factorialRegressionSummaryFit$dependOn(c("FAassignedFactors", "modelTerms", "FAresponse", "intOrder", "FArunOrder", "enabledIntOrder"))

  factorialRegressionSummaryFit$addColumnInfo(name = "S", title = "S", type = "number")
  factorialRegressionSummaryFit$addColumnInfo(name = "R1", title = "R-sq", type = "number")
  factorialRegressionSummaryFit$addColumnInfo(name = "R2", title = "R-sq (adj)", type = "number")

  factorialRegressionANOVA$setData(anovaFill)
  factorialRegressionSummaryFit$setData(modelSummaryFill)

  return(list(ANOVA = factorialRegressionANOVA, SummaryFit = factorialRegressionSummaryFit))
}

.factorialRegressionCoefficientsCreateTable <- function(options, ready, fit, saturated, model) {
  factorialRegressionCoefficients <- createJaspTable(gettext("Coefficients"))
  factorialRegressionFormula <- createJaspTable(gettext("Regression equation in uncoded units"))

  factorialRegressionCoefficients$dependOn(c("FAassignedFactors", "modelTerms", "FAresponse", "FArunOrder", "enabledIntOrder"))
  factorialRegressionFormula$dependOn(c("FAassignedFactors", "modelTerms", "FAresponse", "FArunOrder", "enabledIntOrder"))

  if (!is.null(fit) && ready) {
    reponseVar <- unlist(options$FAresponse)
    factors <- names(coef(fit))[!is.na(coef(fit))]
    coefs <- as.vector(coef(fit))[!is.na(coef(fit))]
    plusOrMin <- sapply(1:length(coefs), function(x) {
      if (coefs[x] > 0) "+" else "-"
    })

    formula <- sprintf("%s = %.5g %s %s %.5g %s", reponseVar, coefs[1], factors[1], plusOrMin[2], abs(coefs[2]), factors[2])
    for (i in 3:length(coefs)) {
      formula <- sprintf("%s %s %.5g %s", formula, plusOrMin[i], abs(coefs[i]), factors[i])
    }

    # Replace : by \u00d7
    formula <- jaspBase::gsubInteractionSymbol(formula)

    factorialRegressionFormula$addRows(list(Formula = formula))
  }

  factorialRegressionCoefficients$addColumnInfo(name = "terms", title = "", type = "string")
  factorialRegressionCoefficients$addColumnInfo(name = "coef", title = gettext("Coefficient"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "se", title = gettext("Standard Error"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "t", title = gettext("t"), type = "number")
  factorialRegressionCoefficients$addColumnInfo(name = "p", title = gettext("<i>p</i>-value"), type = "pvalue")
  factorialRegressionCoefficients$addColumnInfo(name = "vif", title = gettext("VIF"), type = "number")

  if (!is.null(fit)) {
    .factorialRegressionCoefficientsFillTable(factorialRegressionCoefficients, options, fit, saturated, model)
  }

  tableCoef <- factorialRegressionCoefficients
  tableFormula <- factorialRegressionFormula

  return(list(tableCoef = tableCoef, tableFormula = tableFormula))
}

.factorialRegressionCoefficientsFillTable <- function(factorialRegressionCoefficients, options, fit, saturated, model) {
  if (!saturated) {
    coefs <- as.data.frame(summary(fit)$coefficients)
    names.64 <- names(coef(fit))
    null.names <- names(fit$coefficients)[is.na(fit$coefficients)]

    vif <- try(car::vif(fit))

    if (inherits(vif, "try-error")) {
      errorMessage <- as.character(vif)
      factorialRegressionCoefficients$setError(errorMessage)
      return()
    }

    coefsFill <- data.frame(
      terms = names.64,
      coef = c(coefs$Estimate, rep(NA, length(null.names))),
      se = c(coefs$`Std. Error`, rep(NA, length(null.names))),
      t = c(coefs$`t value`, rep(NA, length(null.names))),
      p = c(coefs$`Pr(>|t|)`, rep(NA, length(null.names))),
      vif = c(NA, vif, rep(NA, length(null.names)))
    )
  } else {
    coefs <- as.vector(coef(fit))[!is.na(coef(fit))]
    names <- names(coef(fit))[!is.na(coef(fit))]

    coefsFill <- data.frame(
      terms = names,
      coef = coefs,
      se = rep("*", length(names)),
      t = rep("*", length(names)),
      p = rep("*", length(names)),
      vif = rep("*", length(names))
    )
  }

  # Replace : by \u00d7
  coefsFill$terms <- jaspBase::gsubInteractionSymbol(coefsFill$terms)

  factorialRegressionCoefficients$setData(coefsFill)
  return()
}

.factorialShowAliasStructure <- function(jaspResults, options, dataset, fit) {
  aliasContainter <- createJaspContainer(gettext("Alias Structure"))
  aliasContainter$dependOn(c("FAassignedFactors", "modelTerms", "FAresponse", "intOrder", "FArunOrder", "showAliasStructure2", "enabledIntOrder"))

  factorialAliasIndex <- createJaspTable(gettext("Index"))
  factorialAliasIndex$addColumnInfo(name = "Factor", title = gettext("Factor"), type = "string")
  factorialAliasIndex$addColumnInfo(name = "Name", title = gettext("Name"), type = "string")


  # Index
  Name <- names(coef(fit))[-1]

  factorialAliasIndex$setData(list(
    Factor = LETTERS[1:length(Name)],
    Name = Name
  ))
  aliasContainter[["factorialAliasIndex"]] <- factorialAliasIndex

  # Structure
  table <- createJaspTable(gettext("Alias Structure"))
  table$dependOn(c("FAassignedFactors", "modelTerms", "FAresponse", "intOrder", "FArunOrder", "showAliasStructure", "enabledIntOrder"))
  aliasContainter[["factorialAliasStructure"]] <- table

  factorialDesign <- try(FrF2::FrF2(
    nruns = max(dataset[, unlist(options$FArunOrder)]),
    nfactors = length(unlist(options$FAassignedFactors))
  ))

  if (inherits(factorialDesign, "try-error")) {
    errorMessage <- as.character(factorialDesign)
    table$setError(errorMessage)
    return(aliasContainter)
  }

  rows <- data.frame(Aliases = c(FrF2::aliasprint(factorialDesign)$main, FrF2::aliasprint(factorialDesign)$fi2))

  table$setData(rows)

  return(aliasContainter)
}

.modelFormula <- function(modelTerms, options) {
  dependent.normal <- options$FAresponse
  dependent.base64 <- .v(options$FAresponse)
  reorderModelTerms <- .reorderModelTerms(options)
  modelTerms <- reorderModelTerms$modelTerms

  terms.base64 <- c()
  terms.normal <- c()

  for (term in modelTerms) {
    components <- unlist(term$components)
    term.base64 <- paste(.v(components), collapse = ":", sep = "")
    term.normal <- paste(components, collapse = " \u00d7 ", sep = "")

    terms.base64 <- c(terms.base64, term.base64)
    terms.normal <- c(terms.normal, term.normal)
  }

  model.def <- paste(dependent.base64, "~", paste(terms.base64, collapse = "+"))

  list(model.def = model.def, terms.base64 = terms.base64, terms.normal = terms.normal)
}

.reorderModelTerms <- function(options) {
  if (length(options$modelTerms) > 0) {
    fixedFactors <- list()
    covariates <- list()

    k <- 1
    l <- 1

    for (i in 1:length(options$modelTerms)) {
      fixedFactors[[l]] <- options$modelTerms[[i]]
      l <- l + 1
    }
    modelTerms <- c(fixedFactors, covariates)
    modelTerms <- modelTerms[match(modelTerms, options$modelTerms)]
    interactions <- FALSE
  } else {
    modelTerms <- list()
    interactions <- FALSE
  }

  list(modelTerms = modelTerms, interactions = interactions)
}
