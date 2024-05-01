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
doeResponseSurfaceMethodology <- function(jaspResults, dataset, options, ...) {

  ready <- options[["displayDesign"]] && options[["selectedRow"]] != -1L

  .doeRsmDesignSummaryTable(jaspResults, options)

  if (ready && !jaspResults$getError()) {

    design <- .doeRsmGenerateDesign(options, jaspResults)
    .doeRsmGenerateDesignTable(jaspResults, options, design)

    .doeRsmExportDesign(options, design)
  }

}

.doeRsmDesignSummaryTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["doeRsmDesignSummaryTable"]]))
    return()

  tb <- createJaspTable(title = gettext("Design Summary"), position = 1L)
  tb$addColumnInfo(name = "title",       title = gettext("Variable"),               type = "string")
  tb$addColumnInfo(name = "contFactors", title = gettext("Continuous predictors"),     type = "integer")
  tb$addColumnInfo(name = "catFactors",  title = gettext("Discrete predictors"),    type = "integer")

  tb$addColumnInfo(name = "baseRuns",    title = gettext("Base runs"),              type = "integer")

  if (!options[["designType"]] == "centralCompositeDesign")
    tb$addColumnInfo(name = "baseBlocks",  title = gettext("Base blocks"),            type = "integer")

  tb$addColumnInfo(name = "replicates",  title = gettext("Replicates"),             type = "integer")
  tb$addColumnInfo(name = "totalRuns",   title = gettext("Total runs"),             type = "integer")


  if (!options[["designType"]] == "centralCompositeDesign")
    tb$addColumnInfo(name = "totalBlocks", title = gettext("Total blocks"),           type = "integer")

  if (options[["designType"]] == "centralCompositeDesign") {
    tb$addColumnInfo(name = "cube",         title = gettext("Centre points in cube"),  type = "integer")
    tb$addColumnInfo(name = "axial",        title = gettext("Centre points in axial"), type = "integer")
    tb$addColumnInfo(name = "alpha",        title = "\U03B1",                          type = "number")
  } else {
    tb$addColumnInfo(name = "centerpoints", title = gettext("Centre points"),          type = "integer")
  }
  tb$transpose <- TRUE

  tb[["title"]]       <- gettext("Value")
  tb[["contFactors"]] <- options[["numberOfContinuous"]]
  tb[["catFactors"]]  <- options[["numberOfCategorical"]]

  if (options[["designType"]] == "boxBehnkenDesign" && options[["numberOfContinuous"]] == 8L) {
    # yes 8 variables is not possible, also not in Minitab. Ideally the GUI would forbid this but that's not possible.
    jaspResults$setError(gettext("Box-BehnkenDesign are not supported for 8 variables."))
    return()
  }

  designSpec <- .doeGetSelectedDesign(options)

  if (length(designSpec) == 0L) { # user did not select a design
    tb$addFootnote(gettext("Please select a row in the design table."))
  } else {

    tb[["baseRuns"]]    <- designSpec[["runs"]]
    tb[["replicates"]]  <- designSpec[["replicates"]]
    tb[["totalRuns"]]   <- designSpec[["runs"]]   * designSpec[["replicates"]]

    if (options[["designType"]] == "centralCompositeDesign") {
      tb[["cube"]]         <- designSpec[["cube"]]
      tb[["axial"]]        <- designSpec[["axial"]]
      tb[["alpha"]]        <- designSpec[["alpha"]]
    } else {
      tb[["centerpoints"]] <- designSpec[["centerpoints"]]
      tb[["baseBlocks"]]  <- designSpec[["blocks"]]
      tb[["totalBlocks"]] <- designSpec[["blocks"]] * designSpec[["replicates"]]
    }
  }

  tb$dependOn(options = c(
    "numberOfContinuous", "numberOfCategorical", "selectedRow", "replicates", "selectedDesign2",
    "alphaType", "customAlphaValue", "centerPointType", "customCubeBlock", "customAxialBlock",
    "designType"
  ))

  if (length(designSpec) != 0L && !options[["displayDesign"]])
    tb$addFootnote(gettext("Click 'Display design' to show the design."))

  jaspResults[["doeRsmDesignSummaryTable"]] <- tb

  return()

}

.doeRsmContinuous2df <- function(tableView) {
  df <- as.data.frame(lapply(tableView, `[[`, "values"))
  if (ncol(df) == 0L) return(df)
  colnames(df) <- c("name", "low", "high")
  return(df)
}

.doeRsmCategorical2df <- function(tableView) {
  df <- do.call(cbind.data.frame, lapply(tableView, `[[`, "values"))
  if (ncol(df) == 0L) return(df)
  df <- df[, !apply(df, 2, function(col) all(col == ""))]
  colnames(df) <- c("name", paste0("level", seq_len(ncol(df) - 1L)))
  return(df)
}

.doeRsmGenerateDesignTable <- function(jaspResults, options, design) {
  if (!is.null(jaspResults[["designTable"]]))
    return()

  tb <- createJaspTable(title = if (options[["designType"]] == "centralCompositeDesign") {
    gettext("Response Surface Design - Central Composite Design")
  } else {
    gettext("Response Surface Design - Box-Behnken design")
  })

  tb$addColumnInfo(name = "run.order", title = gettext("Run order"),      type = "integer")
  tb$addColumnInfo(name = "std.order", title = gettext("Standard order"), type = "integer")

  for (i in seq_len(options[["numberOfContinuous"]]))
    tb$addColumnInfo(name = paste0("x", i), title = options[["continuousVariables"]][[1L]][["values"]][i],     type = "number", overtitle = gettext("Continuous predictors"))

  noCat <- options[["numberOfCategorical"]]
  for (i in seq_len(noCat))
    tb$addColumnInfo(name = paste0("x_cat", i), title = options[["categoricalVariables"]][[1L]][["values"]][i], type = "number", overtitle = gettext("Discrete predictors"))

  # avoid any shenanigans with categorical factors having duplicate names
  if (noCat > 0L)
    colnames(design)[(ncol(design) - noCat + 1L):ncol(design)] <- paste0("x_cat", seq_len(noCat))

  tb$setData(design)

  tb$dependOn(options = c("numberOfContinuous", "numberOfCategorical", "categoricalVariables",
                          "selectedRow", "replicates", "selectedDesign2",
                          "alphaType", "customAlphaValue",
                          "centerPointType", "customCubeBlock", "customAxialBlock", "codedOutput",
                          "runOrder", "setSeed", "seed", "designType"))

  if (!options[["codedOutput"]])
    tb$dependOn(options = "continuousVariables")

  jaspResults[["designTable"]] <- tb

  return(design)

}

.doeRsmGenerateDesign <- function(options, jaspResults) {

  designSpec <- .doeGetSelectedDesign(options)

  # TODO: non-Full designs do not match minitab

  design <- if (options[["designType"]] == "centralCompositeDesign") {
    .doeRsmGenerateCentralCompositeDesign(
      noContinuous         = options[["numberOfContinuous"]],
      centerPointsCube     = designSpec[["cube"]],
      centerPointsAxial    = designSpec[["axial"]],
      alpha                = designSpec[["alpha"]],
      replicates           = designSpec[["replicates"]],
      randomize            = FALSE
    )
  } else {
    .doeRsmGenerateBoxBehnkenDesign(
      noContinuous         = options[["numberOfContinuous"]],
      centerPoints         = designSpec[["centerpoints"]],
      randomize            = FALSE
    )
  }

  # fix run.order and std.order
  if (options[["setSeed"]]) {
    jaspBase::.setSeedJASP(options)
  } else {
    set.seed(123)
  }
  design[["run.order"]] <- sample(seq_len(nrow(design)))
  design[["std.order"]] <- seq_len(nrow(design))

  design <- .doeResponseSurfaceSetDisplayOrder(options, design)

  if (options[["numberOfCategorical"]] > 0L)
    design <- .doeRsmReplicateDesignForCategoricalVariables(design, .doeRsmCategorical2df(options[["categoricalVariables"]]))

  if (!options[["codedOutput"]])
    design <- .doeRsmDecodeDesign(design, options)

  return(design)
}

.doeResponseSurfaceSetDisplayOrder <- function(options, design) {
  if (options[["runOrder"]] == "runOrderRandom") {
    design <- design[order(design[["run.order"]]), ]
  } else {
    design <- design[order(design[["std.order"]]), ]
  }
  return(design)
}

.doeRsmGenerateCentralCompositeDesign <- function(noContinuous, centerPointsCube, centerPointsAxial = 0, alpha = "rotatable",
                                                  replicates = 1, randomize = FALSE) {

  design <- rsm::ccd(
    basis      = noContinuous,
    n0         = c(centerPointsCube, centerPointsAxial),
    alpha      = alpha,
    #oneblock   = TRUE,
    bbreps     = replicates,
    randomize  = randomize,
    oneblock = TRUE#,
    # silences lintr check about missing arguments but sadly doesn't work
    # generators = rlang::missing_arg(),
    # coding     = rlang::missing_arg()
  )

  return(design)
}

.doeRsmGenerateBoxBehnkenDesign <- function(noContinuous, centerPoints, randomize = FALSE) {

  block <- if (noContinuous == 4 | noContinuous == 5) TRUE else FALSE

  design <- rsm::bbd(
    k         = noContinuous,
    n0        = centerPoints,
    block     = block,
    randomize = randomize#,
    # silences lintr check
    # coding    = rlang::missing_arg()
  )

  if (block) {
    blockIndex <- which(colnames(design) == "Block")
    design <- cbind(design[,-blockIndex], "Block" = design$Block)
  }

  return(design)
}

.doeRsmReplicateDesignForCategoricalVariables <- function(design, categoricalVariables) {

  if (length(categoricalVariables) <= 0L)
    return(design)

  categoricalVariablesList <- apply(categoricalVariables[-1L], 1L, \(x) {
    x <- trimws(x)  # disallow " " as level name
    x <- x[x != ""] # drop empty levels
    unique(x)       # keep only unique levels
  }, simplify = FALSE) # do NOT simplify because this will change the output if all variable have the same no. levels vs. when they do not
  names(categoricalVariablesList) <- categoricalVariables[[1L]]
  categoricalCombinations <- expand.grid(categoricalVariablesList)

  unrowname <- function(x) {
    rownames(x) <- NULL
    x
  }

  nr <- nrow(design)
  designCombined <- cbind(design, unrowname(categoricalCombinations[rep(1L, nr), , drop = FALSE]))

  for (i in 2L:nrow(categoricalCombinations)) {
    toAdd <- cbind(design, unrowname(categoricalCombinations[rep(i, nrow(design)), , drop = FALSE]))
    toAdd[["run.order"]] <- toAdd[["run.order"]] + nr * (i - 1L)
    toAdd[["std.order"]] <- toAdd[["std.order"]] + nr * (i - 1L)

    designCombined <- rbind(designCombined, toAdd)
  }

  return(designCombined)

}

.doeGetSelectedDesign <- function(options) {

  # TODO: implement BBD!
  # NOTE: could be stored as int except for alpha. Probably not necessary though

  row <- options[["selectedRow"]] + 1L

  if (row <= 0L)
    return(list())

  k <- options[["numberOfContinuous"]]
  print(sprintf("designType = %s", options[["designType"]]))
  designSpec <- .doeRsmDefaultDesigns(k, row, options[["designType"]])

  if (options[["designType"]] == "centralCompositeDesign") {
    if (options[["centerPointType"]] == "custom") {
      designSpec[["cube"]]  <- options[["customCubeBlock"]]
      designSpec[["axial"]] <- options[["customAxialBlock"]]
    }
    designSpec[["runs"]] <- .doeRsmComputeRuns(designSpec[["k"]], designSpec[["designType"]], designSpec[["cube"]], designSpec[["axial"]])

    # rsm provides a fourth option, spherical, which could be added (though SKF did not ask for it)
    # TODO: need to figure out when a design is rotatable!
    designSpec[["alpha"]] <- switch(options[["alphaType"]],
                                    # same as Minitab, but we need to compute alpha to obtain full precision
                                    "default"      = .doeRsmComputeAlpha(k                  = designSpec[["k"]],
                                                                         designtype         = designSpec[["designType"]],
                                                                         centerPointsInCube = designSpec[["cube"]],
                                                                         centerPointsInStar = designSpec[["axial"]],
                                                                         alphaType          = if (designSpec[["rotatable"]] == 1L) "rotatable" else "orthogonal"),
                                    "faceCentered" = 1, # yes this is correct, see ?rsm::ccd + search for "faces"
                                    "custom"       = options[["customAlphaValue"]]
    )
  } else { # boxBehnkenDesign
    if (options[["centerPointType"]] == "custom")
      designSpec[["centerpoints"]]  <- options[["customCubeBlock"]]
  }

  # This would be better, but it does not work because the QML tableview values do not update properly
  # designSpec <- unlist(lapply(options[["selectedDesign2"]], \(x) x[["values"]][idx]))
  # names(designSpec) <- c("runs", "blocks", "total", "cube", "axial", "alpha")
  #
  # if (options[["centerPointType"]] != "default") {
  #   designSpec[["cube"]]  <- options[["customCubeBlock"]]
  #   designSpec[["axial"]] <- options[["customAxialBlock"]]
  # } else {
  #   if (designSpec[["cube"]] == 0L && designSpec[["axial"]] == 0L)
  #     designSpec[["cube"]] <- designSpec[["total"]]
  # }

  designSpec <- c(designSpec, "replicates" = options[["replicates"]])

  return(designSpec)

}

.doeRsmSolveLowHigh <- function(low, high) {
  # solves c(-1, 1) * y - x == c(low, high)
  # matrix(c(-1, -1, -1, 1), 2, 2) %*% solveManually(low, high) == c(low, high)
  c((-low - high) / 2, (high - low) / 2)
}

.doeRsmDecodeDesign <- function(design, options) {

  continuousDf  <- .doeRsmContinuous2df(options[["continuousVariables"]])

  if (nrow(continuousDf) > 0L) {
    for (i in seq_len(nrow(continuousDf))) {
      coefs <- .doeRsmSolveLowHigh(continuousDf[i, "low"], continuousDf[i, "high"])
      design[[i + 2L]] <- design[[i + 2L]] * coefs[2L] - coefs[1L]
      # design[[i + 2L]] <- ifelse(design[[i + 2L]] <= 0, design[[i + 2L]] * abs(continuousDf[i, "low"]), design[[i + 2L]] * continuousDf[i, "high"])
    }
  }

  return(design)

}

.doeRsmExportDesign <- function(options, design) {

  outpath <- options[["exportDesignFile"]]
  if (identical(outpath, "") || !options[["actualExporter"]])
    return()

  if (!dir.exists(dirname(outpath)))
    return()

  out_design <- design
  colnames(out_design)[1:2] <- c("RunOrder", "StandardOrder")
  out_design[["Response"]] <- ""

  utils::write.csv(x = out_design, file = options[["exportDesignFile"]], row.names = FALSE, quote = FALSE)

}

.doeRsmDesignTypeFromInt <- function(i) {
  c("Full", "Half", "Quarter", "Eighth")[i]
}

.doeRsmDefaultDesigns <- function(k, row, type = c("centralCompositeDesign", "boxBehnkenDesign")) {

  type <- match.arg(type)

  if (type == "centralCompositeDesign") { # Central-composite design
    # Note designType is coded as follows: 0 => Full, 1 => Half, see .doeRsmDesignTypeFromInt

    defaultDesigns <- data.frame("k" = c(2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10),
               "cube" = c(5, 3, 6, 4, 7, 4, 10, 8, 14, 8, 10, 8, 10, 8, 10, 8, 10, 8),
               "axial" = c(0, 3, 0, 2, 0, 2, 0, 4, 0, 6, 0, 10, 0, 8, 0, 6, 0, 4),
               "designType" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
               "rotatable" = c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    )
  } else {# boxBehnkenDesign

    defaultDesigns <- structure(
      c(3, 4, 5, 6, 7, 9, 10, 15, 33, 46, 54, 62, 130, 170, 1, 3, 2, 1, 1, 1, 1, 3, 3, 3, 6, 6, 10, 10),
      dim = c(7L, 4L), dimnames = list(NULL, c("k", "runs", "blocks", "centerpoints")))
  }

  if (row < 1)
    return(NULL)

  idx <- which(defaultDesigns[, "k"] == k)
  if (row > length(idx)) {
    warning("rowindex outside of possible values!", domain = NA)
    return(NULL)
  }

  return(defaultDesigns[idx[row], ])

}

.doeRsmDesignIsRotatable <- function(k, ncf, na, nca) {

  # nf <- 2^k

  condition1 <- nc %% 2 == 0
  if (!condition1)
    return(FALSE)

  # from is.integer
  isWholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  condition2 <- isWholenumber((2 * k + nca) - 16 * ncf)

  return(condition2)

}

.doeRsmComputeAlphaRotational <- function(k) { 2^(k / 4) }

.doeRsmComputeAlphaOrthogonal <- function(k, centerPointsInCube, centerPointsInStar) {
  # based on rsm::star with reps set to 1
  cubeSize <- 2^k + centerPointsInCube
  # N <- 2 * k * reps + n0
  N <- 2 * k + centerPointsInStar
  ratio <- 2^k / cubeSize
  # sqrt(N * ratio / (2 * reps))
  sqrt(N * ratio / 2)
}

.doeRsmComputeAlpha <- function(k, designtype, centerPointsInCube, centerPointsInStar, alphaType = c("rotatable", "orthogonal")) {
  alphaType <- match.arg(alphaType)
  if (alphaType == "rotatable")
    .doeRsmComputeAlphaRotational(k - designtype)
  else
    .doeRsmComputeAlphaOrthogonal(k - designtype, centerPointsInCube, centerPointsInStar)
}

.doeRsmComputeRuns <- function(k, designtype, centerPointsInCube, centerPointsInStar) {
  cubeSize <- 2^(k - designtype) + centerPointsInCube
  starSize <- 2*(k - designtype) + centerPointsInStar
  # TODO: figure out why this is needed! and how it ends up in the design!
  designTypeCorrection <- 2 * designtype
  return(cubeSize + starSize + designTypeCorrection)
}

# helpers ----
doersmGetFactorNamesCoded <- function(options) {
  return(paste0("x", seq_len(options[["numberOfFactors"]])))
}

doersmGetFactorNamesUncoded <- function(options) {
  factorNames <- purrr::map_chr(options[["factors"]], `[[`, "factorName")
  badNamesIdx <- which(factorNames == "")
  factorNames[badNamesIdx] <- sprintf("x%d", badNamesIdx)
  return(factorNames)
}
