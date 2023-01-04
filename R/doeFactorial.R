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
  ready <- options[["selectedRow"]] != -1L

  .doeFactorialDesignSummaryTable(jaspResults, options)

  if (ready) {
    error <- try({
      design <- .doeFactorialGenerateDesign(jaspResults, options)
    })

    if (isTryError(error)) {
      tb <- createJaspTable()
      tb$dependOn(options = .doeFactorialDependencies())
      tb$setError(gettextf("The analysis failed with the following error message: %s", .extractErrorMessage(error)))
      jaspResults[["errorTable"]] <- tb
      return()
    }

    .doeFactorialAliasTable(jaspResults, options, design)

    .doeFactorialGenerateDesignTable(jaspResults, options, design)

    .doeRsmExportDesign(options, design[["display"]])
  }
}

.doeFactorialDependencies <- function() {
  return(c(
    "numberOfCategorical",
    "categoricalNoLevels",
    "categoricalVariables",
    "selectedRow",
    "selectedDesign2",
    "factorialType",
    "blocks",
    "centerpoints",
    "replications",
    "repetitions",
    "setSeed",
    "seed",
    "runOrder"
  ))
}

.doeFactorialDesignSummaryTable <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["doeFactorialDesignSummaryTable"]])) {
    return()
  }
  twoLevelDesign <- options[["categoricalNoLevels"]] == 2
  tb <- createJaspTable(title = gettext("Design Summary"), position = 1L)
  tb$addColumnInfo(name = "title", title = gettext("Variable"), type = "string")
  tb$addColumnInfo(name = "catFactors", title = gettext("Factors"), type = "integer")
  tb$addColumnInfo(name = "baseRuns", title = gettext("Base runs"), type = "integer")
  if (options[["categoricalNoLevels"]] == 2) {
    tb$addColumnInfo(name = "baseBlocks", title = gettext("Blocks"), type = "integer")
    tb$addColumnInfo(name = "centerpoints", title = gettext("Centre points"), type = "integer")
  }
  tb$addColumnInfo(name = "replications", title = gettext("Replications"), type = "integer")
  tb$addColumnInfo(name = "repetitions", title = gettext("Repetitions"), type = "integer")
  tb$addColumnInfo(name = "totalRuns", title = gettext("Total runs"), type = "integer")
  tb$transpose <- TRUE
  tb$dependOn(options = .doeFactorialDependencies())
  jaspResults[["doeFactorialDesignSummaryTable"]] <- tb
  tb[["title"]] <- gettext("Value")
  tb[["catFactors"]] <- options[["numberOfCategorical"]]
  designSpec <- .doeFactorialGetSelectedDesign(jaspResults, options)
  if (length(designSpec) == 0L) { # user did not select a design
    tb$addFootnote(gettext("Please select a row in the design table."))
  } else {
    tb[["baseRuns"]] <- designSpec[["runs"]]
    if (options[["categoricalNoLevels"]] == 2) {
      tb[["baseBlocks"]] <- designSpec[["blocks"]]
      tb[["centerpoints"]] <- designSpec[["centerpoints"]]
    }
    tb[["replications"]] <- designSpec[["replications"]]
    tb[["repetitions"]] <- designSpec[["repetitions"]]
    if (twoLevelDesign) {
      tb[["totalRuns"]] <- (designSpec[["runs"]] * designSpec[["replications"]]) + (designSpec[["blocks"]] * designSpec[["centerpoints"]] * designSpec[["replications"]]) + designSpec[["repetitions"]]
    } else {
      tb[["totalRuns"]] <- designSpec[["runs"]] * designSpec[["replications"]] + designSpec[["repetitions"]] # TODO
    }
  }
  if (length(designSpec) != 0L && !options[["displayDesign"]]) {
    tb$addFootnote(gettext("Click 'Display design' to show the design."))
  }
}

.doeFactorialGetSelectedDesign <- function(jaspResults, options) {
  row <- options[["selectedRow"]] + 1L
  if (row <= 0L) {
    return(list())
  }
  design <- .doeFactorialDefaultDesigns(jaspResults, options, row)
  return(design)
}

.doeFactorialDefaultDesigns <- function(jaspResults, options, row) {
  nFactors <- options[["numberOfCategorical"]]
  twoLevelDesign <- options[["categoricalNoLevels"]] == 2
  if (!twoLevelDesign) {
    designs <- data.frame(name = "Full factorial", runs = options[["categoricalNoLevels"]]^nFactors, resolution = "Full")
  } else {
    if (nFactors == 2) {
      designs <- data.frame(name = "Full factorial", runs = 4, resolution = "Full")
    } else if (nFactors == 3) {
      designs <- data.frame(
        name = c("1/2 fraction", "Full factorial"),
        runs = c(4, 8),
        resolution = c("III", "Full")
      )
    } else if (nFactors == 4) {
      designs <- data.frame(
        name = c("1/2 fraction", "Full factorial"),
        runs = c(8, 16),
        resolution = c("IV", "Full")
      )
    } else if (nFactors == 5) {
      designs <- data.frame(
        name = c("1/4 fraction", "1/2 fraction", "Full factorial"),
        runs = c(8, 16, 32),
        resolution = c("III", "V", "Full")
      )
    } else if (nFactors == 6) {
      designs <- data.frame(
        name = c("1/8 fraction", "1/4 fraction", "1/2 fraction", "Full factorial"),
        runs = c(8, 16, 32, 64),
        resolution = c("III", "IV", "VI", "Full")
      )
    } else if (nFactors == 7) {
      designs <- data.frame(
        name = c("1/16 fraction", "1/8 fraction", "1/4 fraction", "1/2 fraction", "Full factorial"),
        runs = c(8, 16, 32, 64, 128),
        resolution = c("III", "IV", "IV", "VII", "Full")
      )
    } else if (nFactors == 8) {
      designs <- data.frame(
        name = c("1/16 fraction", "1/8 fraction", "1/4 fraction", "1/2 fraction"),
        runs = c(16, 32, 64, 128),
        resolution = c("IV", "IV", "V", "VIII")
      )
    } else if (nFactors == 9) {
      designs <- data.frame(
        name = c("1/32 fraction", "1/16 fraction", "1/8 fraction", "1/4 fraction"),
        runs = c(16, 32, 64, 128),
        resolution = c("III", "IV", "IV", "VI")
      )
    } else if (nFactors == 10) {
      designs <- data.frame(
        name = c("1/64 fraction", "1/32 fraction", "1/16 fraction", "1/8 fraction"),
        runs = c(16, 32, 64, 128),
        resolution = c("III", "IV", "IV", "V")
      )
    } else if (nFactors == 11) {
      designs <- data.frame(
        name = c("1/128 fraction", "1/64 fraction", "1/32 fraction", "1/16 fraction"),
        runs = c(16, 32, 64, 128),
        resolution = c("III", "IV", "IV", "V")
      )
    } else if (nFactors >= 12) {
      designs <- data.frame(
        name = c("1/256 fraction", "1/128 fraction", "1/64 fraction", "1/32 fraction"),
        runs = c(16, 32, 64, 128),
        resolution = c("III", "IV", "IV", "IV")
      )
    }
  }
  design <- designs[row, , drop = FALSE]
  design[["type"]] <- if (twoLevelDesign) "twoLevel" else "full"
  design[["factors"]] <- nFactors
  design[["replications"]] <- options[["replications"]]
  design[["repetitions"]] <- options[["repetitions"]]
  if (options[["categoricalNoLevels"]] == 2) {
    design[["centerpoints"]] <- options[["centerpoints"]]
    design[["blocks"]] <- options[["blocks"]]
  }
  return(design)
}

.doeFactorialGenerateDesign <- function(jaspResults, options) {
  df <- .doeRsmCategorical2df(options[["categoricalVariables"]])
  designSpec <- .doeFactorialGetSelectedDesign(jaspResults, options)
  twoLevelDesign <- options[["categoricalNoLevels"]] == 2
  seed <- if (options[["setSeed"]]) options[["seed"]] else NULL
  if (twoLevelDesign) {
    if (options[["factorialType"]] == "factorialTypeDefault") {
      design <- FrF2::FrF2(
        nfactors = designSpec[["factors"]],
        nruns = designSpec[["runs"]],
        ncenter = designSpec[["centerpoints"]],
        replications = designSpec[["replications"]],
        blocks = designSpec[["blocks"]],
        alias.block.2fis = TRUE,
        seed = seed
      )
    } else if (options[["factorialType"]] == "factorialTypeSpecify") {
      whichHow <- strsplit(gsub(" ", "", strsplit(options[["factorialTypeSpecifyGenerators"]], ",")[[1]], fixed = TRUE), "=")
      if (length(whichHow) == 0) {
        return()
      }
      gen <- character(length(whichHow))
      for (i in 1:length(whichHow)) {
        gen[i] <- whichHow[[i]][length(whichHow[[i]])]
      }
      design <- FrF2::FrF2(
        nfactors = designSpec[["factors"]],
        nruns = designSpec[["runs"]],
        generators = gen,
        ncenter = designSpec[["centerpoints"]],
        replications = designSpec[["replications"]],
        alias.block.2fis = TRUE,
        seed = seed
      )
    } else if (options[["factorialType"]] == "factorialTypeSplit") {
      design <- FrF2::FrF2(
        nfactors = designSpec[["factors"]],
        nruns = designSpec[["runs"]],
        hard = options[["numberHTCFactors"]],
        replications = designSpec[["replications"]],
        alias.block.2fis = TRUE,
        seed = seed
      )
    }
  } else {
    nLevels <- apply(df, MARGIN = 1, FUN = function(x) length(which(x[-1] != ".")))
    design <- DoE.base::fac.design(
      nfactors = designSpec[["factors"]],
      nlevels = nLevels,
      factor.names = df[["name"]],
      replications = designSpec[["replications"]],
      seed = seed
    )
  }
  result <- list()
  result[["type"]] <- if (twoLevelDesign) "twoLevel" else "full"
  result[["design"]] <- design
  if (twoLevelDesign) {
    aliases <- FrF2::aliasprint(design)
    result[["aliases"]] <- data.frame(Aliases = c(aliases[["main"]], aliases[["fi2"]]))
  }
  runOrder <- 1:nrow(design)
  standardOrder <- DoE.base::run.order(design)[, 1]
  display <- cbind(ro = runOrder, sro = standardOrder, as.data.frame(design))
  if ("Blocks" %in% colnames(display)) {
    display[["Blocks"]] <- as.numeric(display[["Blocks"]])
  }
  if (twoLevelDesign) {
    display <- .doeFactorialAddCenterPoints(options, display)
  }
  display <- .doeFactorialAddRepeatRuns(options, display)
  display <- .doeFactorialCodeDesign(options, display)
  display <- .doeFactorialSetRunOrder(options, display)
  result[["display"]] <- display
  return(result)
}

.doeFactorialAddCenterPoints <- function(options, display) {
  indices <- (1 + 2):ncol(display)
  if (options[["centerpoints"]] > 0) {
    display[, indices] <- sapply(display[, indices], as.numeric)
  } else {
    display[, indices] <- sapply(display[, indices], as.numeric) * 2 - 3
  }
  return(display)
}

.doeFactorialAddRepeatRuns <- function(options, display) {
  if (options[["repetitions"]] > 0) {
    replace <- options[["repetitions"]] > nrow(display)
    rep_rows <- sample(seq_len(nrow(display)), options[["repetitions"]], replace)
    for (i in rep_rows) {
      display <- rbind(display[1:i, ], display[i, ], display[-(1:i), ])
    }
    display[["ro"]] <- seq_len(nrow(display))
  }
  return(display)
}

.doeFactorialCodeDesign <- function(options, display) {
  df <- .doeRsmCategorical2df(options[["categoricalVariables"]])
  twoLevelDesign <- options[["categoricalNoLevels"]] == 2
  if (!options[["codedOutput"]]) {
    if (twoLevelDesign) {
      for (i in 1:options[["numberOfCategorical"]]) {
        if (df[i, 2] == "1") {
          display[, i + 2][display[, i + 2] == -1] <- "1.0"
        } else {
          display[, i + 2][display[, i + 2] == -1] <- df[i, 2]
        }
        if (options[["centerpoints"]] >= 1) {
          if (!is.na(as.numeric(df[i, 2]) + as.numeric(df[i, 3]))) {
            display[, i + 2][display[, i + 2] == 0] <- (as.numeric(df[i, 2]) + as.numeric(df[i, 3])) / 2
          } else {
            display[, i + 2][display[, i + 2] == 0] <- "center"
          }
        }
        display[, i + 2][display[, i + 2] == 1] <- df[i, 3]
      }
    } else {
      display <- as.data.frame(apply(display, 2, as.numeric))
      for (i in 1:options[["numberOfCategorical"]]) {
        column <- display[, 2 + i]
        for (j in 1:options[["categoricalNoLevels"]]) {
          column[which(column == j)] <- df[i, 1 + j]
        }
        display[, 2 + i] <- column
      }
    }
  }
  return(display)
}

.doeFactorialSetRunOrder <- function(options, display) {
  if (options[["runOrder"]] == "runOrderRandom") {
    display <- display[order(display[["ro"]]), ]
  } else {
    display <- display[order(display[["sro"]]), ]
  }
  return(display)
}

.doeFactorialAliasTable <- function(jaspResults, options, design) {
  if (!is.null(jaspResults[["showAliasStructure"]]) || !options[["showAliasStructure"]] || !options[["factorialType"]] == "factorialTypeDefault") {
    return()
  }
  tb <- createJaspTable(title = gettext("Alias Structure"), position = 2L)
  tb$dependOn(options = c("showAliasStructure", .doeFactorialDependencies()))
  jaspResults[["showAliasStructure"]] <- tb
  if (jaspResults$getError()) {
    return()
  }
  tb$setData(design[["aliases"]])
}

.doeFactorialGenerateDesignTable <- function(jaspResults, options, design) {
  if (!is.null(jaspResults[["displayDesign"]]) || !options[["displayDesign"]]) {
    return()
  }
  df <- .doeRsmCategorical2df(options[["categoricalVariables"]])
  tb <- createJaspTable(title = gettext("Factorial Design"), position = 3L)
  tb$addColumnInfo(name = "ro", title = gettext("Run Order"), type = "string")
  tb$addColumnInfo(name = "sro", title = "Standard Order", type = "integer")
  for (i in 1:options[["numberOfCategorical"]]) {
    tb$addColumnInfo(name = df[["name"]][i], title = df[["name"]][i], type = "string")
  }
  tb$dependOn(options = c("displayDesign", "codedOutput", .doeFactorialDependencies()))
  if (options[["factorialType"]] == "factorialTypeSplit") {
    tb$addFootnote(paste("Hard-to-change factors: ", paste(df[["name"]][1:options[["numberHTCFactors"]]], collapse = ", "), sep = ""))
  }
  jaspResults[["displayDesign"]] <- tb
  if (jaspResults$getError()) {
    return()
  }
  tb$setData(design[["display"]])
}

## NOT USED ###

.doeFactorialGetResolution <- function(options, runs) {
  # Resolution based on SKF's diagram
  sumResolution <- runs + options[["numberOfCategorical"]]
  resolution.Full <- c(6, 11, 20, 37, 70, 135)
  resolution.six <- c(38, 137)
  resolution.Five <- c(21, 72, 138, 139)
  resolution.three <- c(7, 13, 14, 15, 25, 26, 27, 28, 29, 30, 31)

  if (sumResolution %in% resolution.Full) {
    resolution <- "Full"
  } else if (sumResolution %in% resolution.six) {
    resolution <- "VI"
  } else if (sumResolution %in% resolution.Five) {
    resolution <- "V"
  } else if (sumResolution %in% resolution.three) {
    resolution <- "III"
  } else if (any(sumResolution == 137)) {
    resolution <- "VIII"
  } else if (any(sumResolution == 71)) {
    resolution <- "VII"
  } else {
    resolution <- "III"
  }
  return(resolution)
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
