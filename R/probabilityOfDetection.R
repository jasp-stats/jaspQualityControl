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

#' @export
probabilityOfDetection <- function(jaspResults, dataset, options) {

  ready <- .podReady(options)

  dataset <- .podReadData(dataset, options, ready)

  mainContainer <- .podMainContainer(jaspResults, options)

  model <- .podMainTable(mainContainer, dataset, options, ready)
  model[["asymptotes"]] <- .podComputeAsymptotes(mainContainer, options, model, ready)

  .podFitTable      (mainContainer, dataset, options, model, ready)
  .podDetectionPlot (mainContainer, dataset, options, model, ready)
  .podAsymptoteTable(mainContainer, dataset, options, model, ready)

}

.podReady <- function(options) {
  (!identical(options[["outcome"]], "")    && length(options[["outcome"]]) >= 1L) &&
  (!identical(options[["covariate"]], "") && length(options[["covariate"]]) >= 1L)
}

.podReadData <- function(dataset, options, ready) {

  if (!ready)
    return(NULL)

  if (!is.null(dataset))
    return(dataset)

  dataset <- .readDataSetToEnd(
    columns.as.numeric  = options[["covariate"]],
    columns.as.factor   = unlist(options[["outcome"]]),
    exclude.na.listwise = c(options[["covariate"]], unlist(options[["outcome"]]))
  )

  .podCheckErrors(dataset, options)

  if (length(options[["outcome"]]) > 1L) {

    # poor man's reshaping (but not worth a package dependency)
    cnms <- colnames(dataset)
    outcomeNms <- setdiff(cnms, options[["covariate"]])
    dataset <- data.frame(
      size    = dataset[[options[["covariate"]]]],
      outcome = unlist(dataset[outcomeNms], use.names = FALSE),
      id      = rep(outcomeNms, each = nrow(dataset))
    )
    colnames(dataset)[1L] <- options[["covariate"]]

  } else {
    # call the outcome column outcome
    colnames(dataset)[colnames(dataset) == unlist(options[["outcome"]])] <- "outcome"
  }

  if (options[["logTransformedCovariate"]]) {
    dataset[[options[["covariate"]]]] <- log(dataset[[options[["covariate"]]]])
    # filter out any infinite values caused by the log transformation
    dataset <- dataset[is.finite(dataset[[options[["covariate"]]]]), ]
  }

  return(na.omit(dataset))

}

.podCheckErrors <- function(dataset, options) {

  allVariables <- c(options[["covariate"]], unlist(options[["outcome"]]))
  jaspBase::.hasErrors(
    dataset,
    type = c("infinity", "factorLevels", "variance", "observations"),

    infinity.target      = allVariables,

    factorLevels.amount  = " > 2",
    factorLevels.target  = unlist(options[["outcome"]]),

    variance.target      = options[["covariate"]],

    observations.target  = allVariables,
    observations.amount  = " < 3",

    exitAnalysisIfErrors = TRUE
  )

  if (options[["logarithmicXAxis"]] || options[["logTransformedCovariate"]])
    jaspBase::.hasErrors(dataset, type = "negativeValues", negativeValues.target = options[["covariate"]])

}

.podMainContainer <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mainContainer"]]))
    return(jaspResults[["mainContainer"]])

  container <- createJaspContainer()
  container$dependOn(c("outcome", "covariate", "linkFunction", "logTransformedCovariate"))
  jaspResults[["mainContainer"]] <- container
  return(container)

}

.podMainTable <- function(container, dataset, options, ready) {

  mainTable <- createJaspTable(title = gettext("Parameter Estimates"), position = 1)
  mainTable$addColumnInfo(name = "parameters",  title = gettext("Parameter"), type = "string")
  mainTable$addColumnInfo(name = "estimates",   title = gettext("Estimate"),  type = "number")

  container[["mainTable"]] <- mainTable

  if (!ready)
    return(NULL)

  model <- try(.podFitModel(
    as.formula(paste("outcome ~", options[["covariate"]])),
    dataset,
    options[["linkFunction"]]
  ))

  if (jaspBase::isTryError(model)) {

    container$setError(gettextf("Fitting the model failed with the following error message:\n%s", jaspBase::.extractErrorMessage(model)))
    model <- NULL

  } else {

    mainTable[["parameters"]] <- c(gettext("Intercept"), unlist(options[["covariate"]]))
    mainTable[["estimates"]]  <- coef(model)

    .podMainTableFootnotes(mainTable, model)

  }

  return(model)

}

.podMainTableFootnotes <- function(mainTable, model) {

  if (!model[["converged"]])
    mainTable$addFootnote(gettext("The IWLS algorithm did not converge. Interpret the results with caution."), symbol = .podGetWarningSymbol())

  tb <- table(model[["y"]])
  if (length(tb) == 1L) # other things than 1 means an error was already thrown
    mainTable$addFootnote(gettextf("There is only observed category (%s). The results are likely meaningless.", names(tb)), symbol = .podGetWarningSymbol())

}

.podFitTable <- function(container, dataset, options, model, ready) {

  if (!is.null(container[["fitTable"]]) || !options[["modelFitTable"]])
    return()

  fitTable <- createJaspTable(gettext("Fit Table"), dependencies = "modelFitTable", position = 2)
  fitTable$addColumnInfo(name = "AIC",  title = gettext("AIC"), type = "number")

  container[["fitTable"]] <- fitTable
  if (!ready || container$getError())
    return()

  # fill table
  fitTable[["AIC"]] <- model[["aic"]]

}

.podDetectionPlot <- function(container, dataset, options, model, ready) {

  if (!is.null(container[["detectionPlot"]]))
    return()

  detectionPlot <- createJaspPlot(title = gettext("Detection Plot"), width = 600, height = 600, position = 3)
  detectionPlot$dependOn(c(
    "detectionPlotDataDisplay", "detectionPlotDataDisplayType", "detectionPlotCi", "detectionPlotCiLevel", "xAxisTicksType", "detectionPlotDataDisplayTypePointsJitter", "detectionPlotDensityDisplay", "verticalAsymptotes",
    "horizontalAsymptotes", "logarithmicXAxis"#, "logarithmicYAxis"
  ))

  container[["detectionPlot"]] <- detectionPlot
  if (!ready || container$getError())
    return()

  plotObject <- try(.podFillDetectionPlot(
    formula          = stats::formula(model),
    dataset          = dataset,
    model            = model,
    xName            = if (options[["logTransformedCovariate"]]) gettextf("log(%s)", options[["covariate"]]) else options[["covariate"]],
    xBreaks          = options[["xAxisTicksType"]],
    addJitter        = options[["detectionPlotDataDisplayTypePointsJitter"]],
    geoms            = if (!options[["detectionPlotDataDisplay"]]) "none" else options[["detectionPlotDataDisplayType"]],
    showCI           = options[["detectionPlotCi"]],
    CIvalue          = options[["detectionPlotCiLevel"]],
    showDensity      = options[["detectionPlotDensityDisplay"]],
    xAsymptotes      = vapply(options[["verticalAsymptotes"]],   `[[`, FUN.VALUE = numeric(1), "verticalAsymptoteValue"),
    yAsymptotes      = vapply(options[["horizontalAsymptotes"]], `[[`, FUN.VALUE = numeric(1), "horizontalAsymptoteValue"),
    logarithmicXaxis = options[["logarithmicXAxis"]],
    logarithmicYaxis = FALSE#options[["logarithmicYAxis"]]
  ))

  if (jaspBase::isTryError(plotObject)) {
    detectionPlot$setError(gettextf("The detection plot failed with the following error message:\n%s", jaspBase::.extractErrorMessage(plotObject)))
  } else {
    detectionPlot$plotObject <- plotObject
  }

}

.podCreateModelPlotFunction <- function(model) {

  # returns a closure over model pdf -- this might be a bit memory inefficient
  nms <- all.vars(stats::formula(model))[-1L]
  distributionFunction <- function(x) {
    predict(model, setNames(data.frame(x = x), nms), type = "response")
  }

  return(distributionFunction)
}

.podFindModelPoint <- function(model, point, interval) {

  # this is a lazy, flexible, but expensive approach
  # better would be to use qlogis and/ or qnorm directly through
  # stats::family(model)$linkinv(.5)
  nms <- all.vars(stats::formula(model))[-1L]
  res <- try(uniroot(function(x) predict(model, setNames(data.frame(x = x), nms), type = "response") - point, interval = interval)$root, silent = TRUE)

  if (jaspBase::isTryError(res)) {

    # if this fails after 100 tries then the fit is VERY poor
    safety <- 0
    while (grepl(pattern = "opposite sign", res, fixed = TRUE) && safety < 100) {
      interval <- interval + c(-1, 1) * abs(interval[2] - interval[1])
      res <- try(uniroot(function(x) predict(model, setNames(data.frame(x = x), nms), type = "response") - point, interval = interval)$root, silent = TRUE)
      safety <- safety + 1
    }
  }

  return(res)

}

.podFillDetectionPlot <- function(
  formula, dataset, model,
  yAsymptotes      = NULL,
  xAsymptotes      = NULL,
  addJitter        = TRUE,
  jitterWidth      = 0.15,
  geoms            = c("points", "density", "rug", "none"),
  showDensity      = FALSE,
  useLevelForYName = TRUE,
  xName            = NULL,
  yName            = NULL,
  yBreaks          = ggplot2::waiver(),
  yLabels          = ggplot2::waiver(),
  xBreaks          = c("dataAndModelBased", "dataBased"),
  dataColor        = "grey50",
  yAsymptoteColor  = "orange",
  xAsymptoteColor  = "green",
  logarithmicXaxis = FALSE,
  logarithmicYaxis = FALSE,
  showCI           = FALSE,
  CIvalue          = 0.95,
  refLevel         = 2L
) {

  vars <- all.vars(formula)

  yvar <- vars[1L]
  xvar <- vars[2L]
  yvarLevels <- levels(dataset[[yvar]])
  dataset[[yvar]] <- as.integer(dataset[[yvar]] == yvarLevels[[min(length(yvarLevels), refLevel)]])

  if (!inherits(yLabels, "waiver") && !is.null(yLabels) && length(yBreaks) != length(yLabels))
    stop("Internal error: yBreaks and yLabels have different lenght!", domain = NA)

  if (is.character(xBreaks))
    xBreaks <- match.arg(xBreaks)

  geoms <- match.arg(geoms, several.ok = TRUE)

  # determine the x-range here
  if (is.character(xBreaks)) {
    if (xBreaks == "dataBased") {
      xBreaks <- pretty(dataset[[xvar]])
      xLimits <- range(xBreaks)
    } else if (xBreaks == "dataAndModelBased") {

      xBreaks  <- jaspGraphs::getPrettyAxisBreaks(dataset[[xvar]])
      xLimits  <- range(xBreaks)
      interval <- xLimits + c(-1, 1) * abs(xLimits)
      left     <- .podFindModelPoint(model, 0.01, interval)
      right    <- .podFindModelPoint(model, 0.99, interval)
      xBreaks  <- pretty(c(left, right, xBreaks))
      xLimits  <- range(xBreaks)
    }

  } else {
    xLimits <- range(xBreaks)
  }

  xscale <- if (logarithmicXaxis) {
    if (xLimits[1] <= sqrt(.Machine$double.eps))
      xLimits[1] <-  sqrt(.Machine$double.eps)
    if (xBreaks[1] <= sqrt(.Machine$double.eps))
      xBreaks[1] <-  sqrt(.Machine$double.eps)

    xLog10Limits <- c(floor(log10(xLimits[1])), ceiling(log10(xLimits[2])))

    xBreaks <- exp(log(10) * jaspGraphs::getPrettyAxisBreaks(xLog10Limits))
    xLimits <- range(xBreaks)

    # xLabels <- rep("", 10 * length(xBreaks0))
    # xLabels[seq(1, 10 * length(xBreaks0), 10)] <- scales::trans_format("log10", scales::math_format(10^.x))(xBreaks0)
    # xBreaks <- exp(log(10) * seq(xLog10Limits[1], xLog10Limits[2], length.out = length(xLabels)))

    ggplot2::scale_x_log10(name = xName, breaks = xBreaks, labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = xLimits)
  } else {
    ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = xLimits)
  }

  left    <- xLimits[1L]
  right   <- xLimits[2L]

  geomRug <- geomDensity <- geomBeeswarm <- geomPoint <-
    geomXasymptote <- geomYasymptote <- geomRibbon <- NULL
  if ("rug" %in% geoms) {

    idx0 <- dataset[[yvar]] == 0L
    geomRug0 <- ggplot2::geom_rug(
      data    = dataset[idx0, c(xvar, yvar)],
      mapping = ggplot2::aes_string(x = xvar, y = yvar),
      sides   = "bottom",
      color   = dataColor
    )
    geomRug1 <- ggplot2::geom_rug(
      data    = dataset[!idx0, c(xvar, yvar)],
      mapping = ggplot2::aes_string(x = xvar, y = yvar),
      sides   = "top",
      color   = dataColor
    )
    geomRug <- list(geomRug0, geomRug1)
  }

  if ("points" %in% geoms) {

    if (addJitter) {
      idx0  <- dataset[[yvar]] == 0L
      nidx0 <- sum(idx0)
      # this is what ggbeeswarm::geom_quasirandom does internally, but we tweaks it so the points are outside of the [0, 1] range
      y0 = rep(0:1, c(nidx0, nrow(dataset) - nidx0))
      df <- data.frame(
        x = c(dataset[[xvar]][idx0], dataset[[xvar]][!idx0]),
        y = y0 + c(
          -1 * abs(vipor::offsetX(dataset[[xvar]][ idx0], width = jitterWidth, varwidth = FALSE, adjust = 0.5, method = "quasirandom", nbins = NULL)),
               abs(vipor::offsetX(dataset[[xvar]][!idx0], width = jitterWidth, varwidth = FALSE, adjust = 0.5, method = "quasirandom", nbins = NULL))
        )
      )
    } else {
      df <- data.frame(x = dataset[[xvar]], y = dataset[[yvar]])
    }

    geomBeeswarm <- ggplot2::geom_point(data = df, mapping = ggplot2::aes_string(x = "x", y = "y"), color = "black",
                                        alpha = 0.8, shape = 21, size = 1.5, stroke = 0.25, fill = "grey")

  }

  if (showDensity) {

    npoints <- 512L
    idx0  <- dataset[[yvar]] == 0L
    dens0 <- dens1 <- NULL
    if (sum(idx0) > 2)
      dens0 <- density(dataset[[xvar]][ idx0], n = npoints, from = left, to = right)

    if (sum(!idx0) > 2)
      dens1 <- density(dataset[[xvar]][!idx0], n = npoints, from = left, to = right)

    # rescale max y to height of jitterWidth
    maxVal <- max(dens0[["y"]], dens1[["y"]])

    if (!is.null(dens0))
      dens0[["y"]] <- -1 * dens0[["y"]] / maxVal * jitterWidth

    if (!is.null(dens1))
      dens1[["y"]] <-  1 + dens1[["y"]] / maxVal * jitterWidth

    if (!is.null(dens0) || !is.null(dens1)) {
      dfDensity <- data.frame(
        x = c(dens0[["x"]], dens1[["x"]]),
        y = c(dens0[["y"]], dens1[["y"]]),
        g = factor(rep(0:1, each = npoints))
      )
      geomDensity <- ggplot2::geom_line(
        data = dfDensity,
        mapping = ggplot2::aes_string(x = "x", y = "y", group = "g"),
        color   = dataColor,
        show.legend = FALSE
      )
    }
  }

  if (showCI) {

    geomRibbon <- ggplot2::stat_function(
      fun = function(x) {

        inverseLinkFunction <- stats::family(model)[["linkinv"]]
        predictions <- predict(model, setNames(data.frame(x), xvar), se.fit = TRUE, type = "link")
        h <- (1 - CIvalue) / 2
        offset <- qnorm(CIvalue) * predictions[["se.fit"]]

        lower <- inverseLinkFunction(predictions[["fit"]] - offset)
        upper <- inverseLinkFunction(predictions[["fit"]] + offset)

        return(list(list(lower = lower, upper = upper)))
      }, mapping = ggplot2::aes(x = x, ymin = ..y..[[1]][["lower"]], ymax = ..y..[[1]][["upper"]]),
      geom = "ribbon", color = "black", alpha = 0.5, fill = "grey80", size = 0.5,
      linetype = "dashed", data.frame(x = range(xLimits))
    )

    # to test it locally
    # ggplot2::ggplot() + geomRibbon

  }

  if (!is.null(yAsymptotes) && length(yAsymptotes) > 0L) {
    interval <- xLimits + c(-1, 1) * abs(xLimits)
    dfYAsymptote <- data.frame(
      x = numeric(3 * length(yAsymptotes)),
      y = numeric(3 * length(yAsymptotes)),
      g = factor(rep(seq_along(yAsymptotes), each = 3))
    )

    for (i in seq_along(yAsymptotes)) {
      xAsymptote <- .podFindModelPoint(model, yAsymptotes[i], interval)
      idx <- (1 + 3 * (i - 1)) : (3 * i)
      dfYAsymptote[["x"]][idx] <- c(xLimits[1], xAsymptote, xAsymptote)
      dfYAsymptote[["y"]][idx] <- c(yAsymptotes[i], yAsymptotes[i], 0)
    }

    geomYasymptote <- ggplot2::geom_line(
      data    = dfYAsymptote,
      mapping = ggplot2::aes(x = x, y = y, linetype = g),
      color   = yAsymptoteColor
    )
  }

  if (!is.null(xAsymptotes) && length(xAsymptotes) > 0L) {

    dfXAsymptote <- data.frame(
      x = numeric(3 * length(xAsymptotes)),
      y = numeric(3 * length(xAsymptotes)),
      g = factor(rep(seq_along(xAsymptotes), each = 3))
    )

    yAsymptote <- predict(model, setNames(data.frame(xAsymptotes), xvar), type = "response")
    for (i in seq_along(xAsymptotes)) {
      idx <- seq(from = 1 + 3 * (i - 1), to = 3 * i, by = 1)
      dfXAsymptote[["x"]][idx] <- c(xLimits[1], xAsymptotes[i], xAsymptotes[i])
      dfXAsymptote[["y"]][idx] <- c(yAsymptote[i], yAsymptote[i], 0)
    }

    geomXasymptote <- ggplot2::geom_line(
      data    = dfXAsymptote,
      mapping = ggplot2::aes(x = x, y = y, linetype = g),
      color   = xAsymptoteColor
    )
  }

  # scale the y-axis dynamically to ensure we don't strip off the density or
  # the jittered points, or the asymptote
  yBreaks0 <- 0:1 # pretty(dataset[[yvar]])
  yLimits  <- 0:1 # range(yBreaks0)
  if (addJitter)
    yLimits <- yLimits + 1.001 * c(-jitterWidth, jitterWidth)

  if (showDensity)
    yLimits <- range(yLimits, dfDensity[["y"]])

  if (inherits(yBreaks, "waiver"))
    yBreaks <- seq(0, 1, .2)

  if (useLevelForYName)
    yName <- .podGetPlotYaxisLabel(yvarLevels[refLevel])

  # this makes no sense and is disabled in QML
  yscale <- if (logarithmicYaxis) {

    if (yLimits[1] <= sqrt(.Machine$double.eps))
      yLimits[1] <-  sqrt(.Machine$double.eps)
    if (xBreaks[1] <= sqrt(.Machine$double.eps))
      xBreaks[1] <-  sqrt(.Machine$double.eps)

    xLog10Limits <- c(floor(log10(xLimits[1])), ceiling(log10(xLimits[2])))
    xBreaks <- exp(log(10) * jaspGraphs::getPrettyAxisBreaks(xLog10Limits))
    xLimits <- range(xBreaks)
    ggplot2::scale_y_log10(name = yName, breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = yLimits)
  } else {
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = yLimits, labels = yLabels)
  }

  # ann <- if (!logarithmicXaxis && !logarithmicYaxis) NULL else if (logarithmicXaxis && logarithmicYaxis) ggplot2::annotation_logticks(sides = "bl") else if (logarithmicXaxis)
  #   ggplot2::annotation_logticks(sides = "b")
  # else
  #   ggplot2::annotation_logticks(sides = "l")

  ggplot2::ggplot() +
    geomRibbon +
    geomPoint +
    geomDensity +
    geomBeeswarm +
    geomRug +
    ggplot2::geom_function(fun = .podCreateModelPlotFunction(model)) +
    geomYasymptote + geomXasymptote +
    xscale + yscale + #ann +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

}

.podFitModel <- function(formula, dataset, link = c("logit", "probit")) {

  link <- match.arg(link)

  model <- stats::glm(
    formula = formula,
    data    = dataset,
    family  = binomial(link)
  )

  return(model)

}

.podComputeAsymptotes <- function(mainContainer, options, model, ready) {

  # we probably could store this in the state, but it's very cheap to compute.

  xAsymptotes <- vapply(options[["verticalAsymptotes"]],   `[[`, FUN.VALUE = numeric(1), "verticalAsymptoteValue")
  yAsymptotes <- vapply(options[["horizontalAsymptotes"]], `[[`, FUN.VALUE = numeric(1), "horizontalAsymptoteValue")

  if (!ready)
    return(list(vertical = NULL, horizontal = NULL))

  xvar <- .podGetXvarFromModel(model)
  yInterval <- range(model[["data"]][[xvar]])

  return(list(
    vertical   = vapply(yAsymptotes, function(y) .podFindModelPoint(model, y, yInterval), FUN.VALUE = numeric(1)),
    horizontal = if (length(xAsymptotes) == 0L) numeric() else predict(model, setNames(data.frame(xAsymptotes), xvar), type = "response")
  ))

}

.podAsymptoteTable <- function(container, dataset, options, model, ready) {

  asymptoteContainer <- createJaspContainer(title = gettext("Asymptotes"))

  yAsymptotes <- vapply(options[["horizontalAsymptotes"]], `[[`, FUN.VALUE = numeric(1), "horizontalAsymptoteValue")
  xAsymptotes <- vapply(options[["verticalAsymptotes"]],   `[[`, FUN.VALUE = numeric(1), "verticalAsymptoteValue")

  if (ready) {
    xvar <- .podGetXvarFromModel(model)
    yvar <- .podGetYvarFromModel(model)
    levelName <- levels(model[["data"]][[yvar]])[2L]
  } else {
    xvar <- gettext("covariate")
    levelName <- "..."
  }
  yLabel <- .podGetPlotYaxisLabel(levelName)

  if (length(yAsymptotes) > 0L)
    asymptoteContainer[["yAsymptotesTable"]] <- .podAsymptoteTableMeta(gettext("Horizontal asymptotes"), xvar, yLabel, position = 1, dependencies = "horizontalAsymptotes")

  if (length(xAsymptotes) > 0L)
    asymptoteContainer[["xAsymptotesTable"]] <- .podAsymptoteTableMeta(gettext("Vertical asymptotes"), xvar, yLabel, position = 2, dependencies = "verticalAsymptotes")

  container[["asymptoteContainer"]] <- asymptoteContainer
  if (!ready)
    return()

  yAsymptotesLabels <- vapply(options[["horizontalAsymptotes"]], `[[`, FUN.VALUE = character(1), "horizontalAsymptoteName")
  xAsymptotesLabels <- vapply(options[["verticalAsymptotes"]],   `[[`, FUN.VALUE = character(1), "verticallAsymptoteName")

  if (length(yAsymptotes) > 0L)
    .podFillAsymptoteTable(asymptoteContainer[["yAsymptotesTable"]], label = yAsymptotesLabels, x = model[["asymptotes"]][["vertical"]], y = yAsymptotes)

  if (length(xAsymptotes) > 0L)
    .podFillAsymptoteTable(asymptoteContainer[["xAsymptotesTable"]], label = xAsymptotesLabels, x = xAsymptotes, y = model[["asymptotes"]][["horizontal"]])

}

.podAsymptoteTableMeta <- function(title, xCoordName, yCoordName, position, dependencies) {
  table <- createJaspTable(title = title, position = position)
  table$dependOn(dependencies)
  table$addColumnInfo(name = "label", title = gettext("Label"), type = "string")
  table$addColumnInfo(name = "x",     title = xCoordName,       type = "number")
  table$addColumnInfo(name = "y",     title = yCoordName,       type = "number")
  return(table)
}

.podFillAsymptoteTable <- function(table, label, x, y) {

  table[["label"]] <- label
  table[["x"]]     <- x
  table[["y"]]     <- y

}

.podGetWarningSymbol <- function() {
  gettext("<b><em>Warning.</em></b>")
}

.podGetXvarFromModel <- function(model) {
  all.vars(formula(model))[2L]
}

.podGetYvarFromModel <- function(model) {
  all.vars(formula(model))[1L]
}

.podGetPlotYaxisLabel <- function(levelName) {
  gettextf("Probability of detecting %s", levelName)
}
