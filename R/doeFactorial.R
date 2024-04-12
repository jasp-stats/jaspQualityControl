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
  ready <- options[["selectedRow"]] != -1L | options[["factorialType"]] == "generalFullFactorial" # If the design type is general full factorial no need to select a design

  .doeFactorialDesignSummaryTable(jaspResults, options)

  if (ready) {
    error <- try({
      design <- .doeFactorialGenerateDesign(jaspResults, options)
    })

    if (isTryError(error)) {
      tb <- createJaspTable()
      tb$dependOn(options = .doeFactorialBaseDependencies())
      tb$setError(gettextf("The analysis failed with the following error message: %s", .extractErrorMessage(error)))
      jaspResults[["errorTable"]] <- tb
      return()
    }

    .doeFactorialAliasTable(jaspResults, options, design)

    .doeFactorialGenerateDesignTable(jaspResults, options, design)

    .doeRsmExportDesign(options, design[["display"]])
  }
}

.doeFactorialBaseDependencies <- function() {
  return(c(
    "numberOfCategorical",
    "categoricalNoLevels",
    "categoricalVariables",
    "selectedRow",
    "selectedDesign2",
    "factorialType",
    "factorialTypeSpecifyGenerators",
    "factorialDesignTypeSplitPlotNumberHardToChangeFactors",
    "blocks",
    "centerpoints",
    "replications",
    "repetitions",
    "setSeed",
    "seed"
  ))
}

.doeFactorialDesignSummaryTable <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["doeFactorialDesignSummaryTable"]])) {
    return()
  }
  twoLevelDesign <- options[["factorialType"]] != "generalFullFactorial"
  tb <- createJaspTable(title = gettext("Design Summary"), position = 1L)
  tb$addColumnInfo(name = "title", title = gettext("Variable"), type = "string")
  tb$addColumnInfo(name = "catFactors", title = gettext("Discrete predictors"), type = "integer")
  tb$addColumnInfo(name = "baseRuns", title = gettext("Base runs"), type = "integer")
  if (twoLevelDesign) {
    tb$addColumnInfo(name = "baseBlocks", title = gettext("Base blocks"), type = "integer")
    tb$addColumnInfo(name = "centerpoints", title = gettext("Centre points per block"), type = "integer")
  }
  tb$addColumnInfo(name = "replications", title = gettext("Replications"), type = "integer")
  tb$addColumnInfo(name = "repetitions", title = gettext("Repetitions"), type = "integer")
  tb$addColumnInfo(name = "totalRuns", title = gettext("Total runs"), type = "integer")
  tb$addColumnInfo(name = "totalBlocks", title = gettext("Total blocks"), type = "integer")
  tb$transpose <- TRUE
  tb$dependOn(options = c("displayDesign", "actualExporter", "exportDesignFile", .doeFactorialBaseDependencies()))
  jaspResults[["doeFactorialDesignSummaryTable"]] <- tb
  tb[["title"]] <- gettext("Value")
  tb[["catFactors"]] <- options[["numberOfCategorical"]]
  designSpec <- .doeFactorialGetSelectedDesign(jaspResults, options)
  if (options[["factorialType"]] != "generalFullFactorial" && length(designSpec) == 0L) { # user did not select a design
    tb$addFootnote(gettext("Please select a row in the design table."))
    return()
  }
  tb[["replications"]] <- designSpec[["replications"]]
  tb[["repetitions"]] <- designSpec[["repetitions"]]
  if (twoLevelDesign) {
    tb[["baseRuns"]] <- designSpec[["runs"]]
    tb[["baseBlocks"]] <- designSpec[["blocks"]]
    tb[["centerpoints"]] <- designSpec[["centerpoints"]]
    runs <- (designSpec[["runs"]] * designSpec[["replications"]]) + (designSpec[["blocks"]] * designSpec[["centerpoints"]] * designSpec[["replications"]]) + designSpec[["repetitions"]]
    tb[["totalRuns"]] <- runs
    tb[["totalBlocks"]] <- designSpec[["replications"]] * designSpec[["blocks"]]
  } else {
    df <- .doeRsmCategorical2df(options[["categoricalVariables"]])
    nLevels <- apply(df, 1, function(x) length(which(x[-1] != "")))
    runs <- prod(nLevels) * designSpec[["replications"]] + designSpec[["repetitions"]]
    tb[["baseRuns"]] <- runs
    tb[["totalRuns"]] <- runs
    tb[["totalBlocks"]] <- designSpec[["replications"]]
  }
  if (runs > 1e+05) {
    tb$setError(gettext("Cannote create designs with more than 100000 total runs."))
    return()
  }
  if (!options[["displayDesign"]]) {
    tb$addFootnote(gettext("Click 'Display design' to show the design."))
  }
  if (options[["exportDesignFile"]] != "") {
    if (!options[["actualExporter"]]) {
      tb$addFootnote(gettext("The design is not exported until 'Export design' is checked."))
    } else {
      tb$addFootnote(gettextf("The design is exported as '%1$s'.", basename(options[["exportDesignFile"]])))
    }
  }
}

.doeFactorialGetSelectedDesign <- function(jaspResults, options) {
  row <- options[["selectedRow"]] + 1L
  if (row <= 0L && options[["factorialType"]] != "generalFullFactorial") {
    return(list())
  }
  design <- .doeFactorialDefaultDesigns(jaspResults, options, row)
  return(design)
}

.doeFactorialDefaultDesigns <- function(jaspResults, options, row) {
  nFactors <- options[["numberOfCategorical"]]
  twoLevelDesign <- options[["factorialType"]] != "generalFullFactorial"
  if (!twoLevelDesign) {
    row <- 1
    designs <- data.frame(
      name = "Full factorial",
      runs = options[["categoricalNoLevels"]]^nFactors,
      resolution = "Full"
    )
  } else {
    if (nFactors == 2) {
      designs <- data.frame(
        name = "Full factorial",
        runs = 4,
        resolution = "Full"
      )
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
  design <- designs[row, ]
  design[["type"]] <- if (twoLevelDesign) "twoLevel" else "full"
  design[["factors"]] <- nFactors
  design[["replications"]] <- options[["replications"]]
  design[["repetitions"]] <- options[["repetitions"]]
  if (twoLevelDesign) {
    design[["centerpoints"]] <- options[["centerpoints"]]
    design[["blocks"]] <- options[["blocks"]]
  }
  return(design)
}

.doeFactorialGenerateDesign <- function(jaspResults, options) {
  seed <- .doeGetAndSetSeed(jaspResults, options)
  twoLevelDesign <- options[["factorialType"]] != "generalFullFactorial"
  df <- .doeRsmCategorical2df(options[["categoricalVariables"]])
  designSpec <- .doeFactorialGetSelectedDesign(jaspResults, options)
  if (length(designSpec) == 0) {
    return()
  }
  if (length(unique(df[["name"]])) != options[["numberOfCategorical"]]) {
    stop("Duplicate factor names are not allowed.")
  }
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
      for (i in seq_along(length(whichHow))) {
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
        hard = options[["factorialDesignTypeSplitPlotNumberHardToChangeFactors"]],
        replications = designSpec[["replications"]],
        alias.block.2fis = TRUE,
        seed = seed
      )
    }
  } else {
    nLevels <- apply(df, 1, function(x) length(which(x[-1] != "")))
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
    result[["aliases"]] <- list()
    result[["aliases"]]$data <- data.frame(Aliases = c(aliases[["main"]], aliases[["fi2"]]))
    if (nrow(result[["aliases"]]$data) == 0) {
      result[["aliases"]]$note <- gettext("No aliasing among main effects and 2-factor interactions.")
    }
  }
  runOrder <- 1:nrow(design)
  # TODO: Fix for blocks and repetitions
  standardOrder <- as.numeric(DoE.base::run.order(design)[["run.no.in.std.order"]])
  dfDesign <- as.data.frame(design)
  if (!twoLevelDesign) {
    dfDesign <- as.data.frame(apply(dfDesign, 2, as.numeric))
  }
  outDesign <- dfDesign[1:nrow(df)]
  colnames(outDesign) <- df[["name"]]
  display <- cbind(ro = runOrder, sro = standardOrder, outDesign)
  if (!is.null(dfDesign[["Blocks"]])) {
    display <- cbind(display, Block = as.numeric(dfDesign[["Blocks"]]))
  }
  if (twoLevelDesign) {
    display <- .doeFactorialAddCenterPoints(options, display)
  }
  display <- .doeFactorialAddRandomRepeats(options, display)
  display <- .doeFactorialSetDisplayOrder(options, display)
  display <- .doeFactorialDecodeDesign(options, display)
  result[["display"]] <- display
  jaspResults[["seed"]] <- createJaspState(seed)
  jaspResults[["seed"]]$dependOn(options = .doeFactorialBaseDependencies())
  return(result)
}

.doeGetAndSetSeed <- function(jaspResults, options) {
  if (!is.null(jaspResults[["seed"]])) {
    seed <- jaspResults[["seed"]]$object
  } else {
    if (options[["setSeed"]]) {
      seed <- options[["seed"]]
    } else {
      seed <- sample(1:1e6, 1)
    }
  }
  return(seed)
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

.doeFactorialAddRandomRepeats <- function(options, display) {
  if (options[["repetitions"]] > 0) {
    replace <- options[["repetitions"]] > nrow(display)
    reps <- sample(seq_len(nrow(display)), options[["repetitions"]], replace)
    for (i in reps) {
      display <- rbind(display[1:i, ], display[i, ], display[-(1:i), ])
    }
    display[["ro"]] <- seq_len(nrow(display))
  }
  return(display)
}

.doeFactorialSetDisplayOrder <- function(options, display) {
  if (options[["runOrder"]] == "runOrderRandom") {
    display <- display[order(display[["ro"]]), ]
  } else {
    display <- display[order(display[["sro"]]), ]
  }
  return(display)
}

.doeFactorialDecodeDesign <- function(options, display) {
  twoLevelDesign <- options[["categoricalNoLevels"]] == 2
  # Decode and order the blocks column
  if ("Block" %in% colnames(display)) {
    display[["Block"]] <- seq_along(unique(display[["Block"]]))[match(display[["Block"]], unique(display[["Block"]]))]
  }
  # Decode the design
  if (!options[["codedOutput"]]) {
    # Decode factor levels
    df <- .doeRsmCategorical2df(options[["categoricalVariables"]])
    factorIndices <- which(colnames(display) %in% df[["name"]])
    for (i in seq_len(length(factorIndices))) {
      column <- as.factor(display[, factorIndices[i]])
      factorLevels <- levels(column)
      factorLevels <- factorLevels[factorLevels != "0"]
      for (j in seq_len(length(factorLevels))) {
        rowIndices <- which(column == factorLevels[j])
        display[rowIndices, factorIndices[i]] <- df[i, 1 + j]
      }
      if (twoLevelDesign) {
        rowIndices <- which(column == "0")
        display[rowIndices, factorIndices[i]] <- "center"
      }
    }
  }
  return(display)
}

.doeFactorialAliasTable <- function(jaspResults, options, design) {
  if (!is.null(jaspResults[["showAliasStructure"]]) || !options[["showAliasStructure"]] || !options[["factorialType"]] == "factorialTypeDefault") {
    return()
  }
  tb <- createJaspTable(title = gettext("Alias Structure"), position = 2L)
  tb$dependOn(options = c("showAliasStructure", .doeFactorialBaseDependencies()))
  jaspResults[["showAliasStructure"]] <- tb
  if (jaspResults$getError()) {
    return()
  }
  tb$setData(design[["aliases"]]$data)
  if (!is.null(design[["aliases"]]$note)) {
    tb$addFootnote(design[["aliases"]]$note)
  }
}

.doeFactorialGenerateDesignTable <- function(jaspResults, options, design) {
  if (!is.null(jaspResults[["displayDesign"]]) || !options[["displayDesign"]]) {
    return()
  }
  df <- .doeRsmCategorical2df(options[["categoricalVariables"]])
  tb <- createJaspTable(title = gettext("Factorial Design"), position = 3L)
  tb$addColumnInfo(name = "ro", title = gettext("Run Order"), type = "integer")
  tb$addColumnInfo(name = "sro", title = gettext("Standard Order"), type = "integer")
  for (i in 1:options[["numberOfCategorical"]]) {
    tb$addColumnInfo(name = df[["name"]][i], title = df[["name"]][i], type = "string")
  }
  tb$dependOn(options = c("displayDesign", "codedOutput", "runOrder", .doeFactorialBaseDependencies()))
  if (options[["factorialType"]] == "factorialTypeSplit") {
    tb$addFootnote(gettextf("Hard-to-change factors: %1$s", paste0(df[["name"]][1:options[["factorialDesignTypeSplitPlotNumberHardToChangeFactors"]]], collapse = ", ")))
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
