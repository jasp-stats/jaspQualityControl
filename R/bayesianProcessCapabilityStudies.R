#
# Copyright (C) 2013-2025 University of Amsterdam
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

#'@importFrom jaspBase jaspDeps %setOrRetrieve%
#'@importFrom rlang .data

# Suppress R CMD check notes for ggplot2 aesthetics
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".data"))
}

#'@export
bayesianProcessCapabilityStudies <- function(jaspResults, dataset, options) {

  fit <- .bpcsCapabilityTable(jaspResults, dataset, options)
  .bpcsCapabilityPlot(jaspResults, options, fit)
  .bpcsIntervalTable(jaspResults, options, fit)
  .bpcsPriorPlot(jaspResults, options, fit)
  .bpcsSequentialPointEstimatePlot(jaspResults, dataset, options, fit)
  .bpcsSequentialIntervalEstimatePlot(jaspResults, dataset, options, fit)
  .bpcsPosteriorPredictivePlot(jaspResults, options, fit)
  .bpcsPriorPredictivePlot(jaspResults, options, fit)

}

.bpcsIsReady <- function(options) {
  hasData <- if (options[["dataFormat"]] == "longFormat") {
    length(options[["measurementLongFormat"]]) > 0L && options[["measurementLongFormat"]] != ""
  } else {
    length(options[["measurementsWideFormat"]]) > 0L
  }
  hasData &&
    options[["lowerSpecificationLimit"]] &&
    options[["upperSpecificationLimit"]] &&
    options[["target"]]
}

.bpcsDefaultDeps <- function() {
    c(
      # data
      "dataFormat", "measurementLongFormat", "measurementsWideFormat",
      "subgroupSizeType", "manualSubgroupSizeValue", "subgroup", "groupingVariableMethod",
      "stagesLongFormat", "stagesWideFormat", "axisLabels",
      # specification
      "target",      "lowerSpecificationLimit",      "upperSpecificationLimit",
      "targetValue", "lowerSpecificationLimitValue", "upperSpecificationLimitValue",
      # metrics
      "Cp", "Cpu", "Cpl", "Cpk", "Cpc", "Cpm",
      # likelihood
      "capabilityStudyType",
      # prior
      "priorSettings", "normalModelComponentsList", "tModelComponentsList",
      # MCMC settings
      "noIterations", "noWarmup", "noChains"
  )
}

.bpcsPlotLayoutDeps <- function(base, hasPrior = TRUE, hasEstimate = TRUE, hasCi = TRUE, hasType = FALSE, hasAxes = TRUE) {
  c(
    base,
    if (hasEstimate) .bpcsPlotLayoutEstimateDeps(base),
    if (hasCi)       .bpcsPlotLayoutCiDeps(base),
    if (hasType)     .bpcsPlotLayoutTypeDeps(base),
    if (hasAxes)     .bpcsPlotLayoutAxesDeps(base),
    if (hasPrior)    .bpcsPlotLayoutPriorDeps(base)
  )
}

.bpcsPlotLayoutEstimateDeps <- function(base) { paste0(base, c("IndividualPointEstimate", "IndividualPointEstimateType")) }
.bpcsPlotLayoutCiDeps       <- function(base) { paste0(base, c("IndividualCi", "IndividualCiType", "IndividualCiMass", "IndividualCiLower", "IndividualCiUpper", "IndividualCiBf")) }
.bpcsPlotLayoutTypeDeps     <- function(base) { paste0(base, c("TypeLower", "TypeUpper")) }
.bpcsPlotLayoutAxesDeps     <- function(base) { paste0(base, c("PanelLayout", "Axes", "custom_x_min", "custom_x_max", "custom_y_min", "custom_y_max")) }
.bpcsPlotLayoutPriorDeps    <- function(base) { paste0(base, "PriorDistribution") }

.bpcsProcessCriteriaDeps <- function() {
  c(paste0("interval", 1:4), paste0("intervalLabel", 1:5))
}

.bpcsTpriorFromOptions <- function(options) {

  switch(options[["capabilityStudyType"]],
    "normalCapabilityAnalysis" = NULL,
    "tCapabilityAnalysis"      = BayesTools::prior("exp",  list(1)), # TODO: should be more generic

    stop("Unknown capability study type: ", options[["capabilityStudyType"]])
  )
}

# Tables ----
.bpcsCapabilityTable <- function(jaspResults, dataset, options) {

  # Check if we already have the results cached
  if (!is.null(jaspResults[["bpcsResultsObject"]]))
    return(jaspResults[["bpcsResultsObject"]]$object)

  table <- .bpcsCapabilityTableMeta(jaspResults, options)
  if (!.bpcsIsReady(options)) {

    if (options[["measurementLongFormat"]] != "" || length(options[["measurementsWideFormat"]]) > 0)
      table$addFootnote(gettext("Please specify the Lower Specification Limit, Upper Specification Limit, and Target Value to compute the capability measures."))

    return(NULL)
  }

  rawfit <- jaspResults[["bpsState"]] %setOrRetrieve% (
    qc::bpc(
      dataset[[1L]], chains = 1, warmup = 1000, iter = 5000, silent = TRUE, seed = 1,
      target   = options[["targetValue"]],
      LSL      = options[["lowerSpecificationLimitValue"]],
      USL      = options[["upperSpecificationLimitValue"]],
      prior_nu = .bpcsTpriorFromOptions(options)
   ) |>
    createJaspState(jaspDeps(.bpcsDefaultDeps()))
  )

  summaryObject <- jaspResults[["bpsSummaryState"]] %setOrRetrieve% (
    summary(
      rawfit, ci.level = options[["credibleIntervalWidth"]]
    ) |>
      createJaspState(jaspDeps(
        options = c(.bpcsDefaultDeps(), "credibleIntervalWidth")
      ))
  )

  resultsObject <- list(
    rawfit           = rawfit,
    summaryObject    = summaryObject
  )

  # Cache the results
  jaspResults[["bpcsResultsObject"]] <- createJaspState(resultsObject)

  .bpcsCapabilityTableFill(table, resultsObject, options)
  return(resultsObject)

}

.bpcsCapabilityTableMeta <- function(jaspResults, options) {

  table <- createJaspTable(title = gettext("Capability Table"), position = 0)
  table$addColumnInfo(name = "metric",  title = gettext("Measure"), type = "string")
  table$addColumnInfo(name = "mean",    title = gettext("Mean"),    type = "number")
  table$addColumnInfo(name = "median",  title = gettext("Median"),  type = "number")
  table$addColumnInfo(name = "sd",      title = gettext("Std"),     type = "number")

  overtitle <- gettextf("%s%% Credible Interval", 100 * options[["credibleIntervalWidth"]])
  table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
  table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)

  table$dependOn(.bpcsDefaultDeps())

  jaspResults[["bpcsCapabilityTable"]] <- table
  return(table)

}

.bpcsCapabilityTableFill <- function(table, resultsObject, options) {

  df <- as.data.frame(resultsObject[["summaryObject"]][["summary"]])

  # Filter metrics based on user selection
  selectedMetrics <- c()
  if (options[["Cp"]])  selectedMetrics <- c(selectedMetrics, "Cp")
  if (options[["Cpu"]]) selectedMetrics <- c(selectedMetrics, "CpU")
  if (options[["Cpl"]]) selectedMetrics <- c(selectedMetrics, "CpL")
  if (options[["Cpk"]]) selectedMetrics <- c(selectedMetrics, "Cpk")
  if (options[["Cpc"]]) selectedMetrics <- c(selectedMetrics, "Cpc")
  if (options[["Cpm"]]) selectedMetrics <- c(selectedMetrics, "Cpm")

  if (length(selectedMetrics) > 0) {
    df <- df[df$metric %in% selectedMetrics, , drop = FALSE]
  }

  table$setData(df)

}

.bpcsIntervalTable <- function(jaspResults, options, fit) {

  if (!options[["intervalTable"]])
    return()

  table <- .bpcsIntervalTableMeta(jaspResults, options)
  if (!.bpcsIsReady(options) || is.null(fit))
    return()

  tryCatch({

    # qc does c(-Inf, interval_probability, Inf)
    interval_probability <- unlist(options[paste0("interval", 1:4)], use.names = FALSE)
    interval_summary <- summary(fit[["rawfit"]], interval_probability = interval_probability)[["interval_summary"]]
    colnames(interval_summary) <- c("metric", paste0("interval", 1:5))
    table$setData(interval_summary)

  }, error = function(e) {

    table$setError(gettextf("Unexpected error in interval table: %s", e$message))

  })

  return()
}

.bpcsIntervalTableMeta <- function(jaspResults, options) {

  table <- createJaspTable(title = gettext("Interval Table"), position = 3)

  table$addColumnInfo(name = "metric", title = gettext("Capability\nMeasure"), type = "string")

  intervalBounds <- c(-Inf, unlist(options[paste0("interval",      1:4)], use.names = FALSE), Inf)
  intervalNames  <-         unlist(options[paste0("intervalLabel", 1:5)], use.names = FALSE)
  n <- length(intervalBounds)

  # custom format helper. we don't use e.g., %.3f directly because that adds trailing zeros (2.000 instead of 2)
  fmt <- \(x) formatC(x, digits = 3, format = "f", drop0trailing = TRUE)
  for (i in 1:(n - 1)) {
    j <- i + 1
    lhs <- if (i == 1)     "(" else "["
    rhs <- if (i == n - 1) ")" else "]"
    title <- sprintf("%s %s%s, %s%s", intervalNames[i], lhs, fmt(intervalBounds[i]), fmt(intervalBounds[j]), rhs)
    table$addColumnInfo(name = paste0("interval", i), title = title, type = "number")
  }
  table$dependOn(c("intervalTable", .bpcsDefaultDeps(), .bpcsProcessCriteriaDeps()))

  jaspResults[["bpcsIntervalTable"]] <- table
  return(table)
}


# Plots ----
.bpcsCapabilityPlot <- function(jaspResults, options, fit) {

  if (!options[["posteriorDistributionPlot"]])
    return()

  jaspResults[["posteriorDistributionPlot"]] %setOrRetrieve% (
    createJaspPlot(
      title = gettext("Posterior Distribution"),
      plot  = if (.bpcsIsReady(options) && !is.null(fit)) {

        # Get selected metrics
        selectedMetrics <- c()
        if (options[["Cp"]])  selectedMetrics <- c(selectedMetrics, "Cp")
        if (options[["Cpu"]]) selectedMetrics <- c(selectedMetrics, "CpU")
        if (options[["Cpl"]]) selectedMetrics <- c(selectedMetrics, "CpL")
        if (options[["Cpk"]]) selectedMetrics <- c(selectedMetrics, "Cpk")
        if (options[["Cpc"]]) selectedMetrics <- c(selectedMetrics, "Cpc")
        if (options[["Cpm"]]) selectedMetrics <- c(selectedMetrics, "Cpm")

        if (length(selectedMetrics) == 0) {
          NULL
        } else {
          qc::plot_density(
            fit$summaryObject,
            what = selectedMetrics,
            point_estimate  = with(options, if (posteriorDistributionPlotIndividualPointEstimate) posteriorDistributionPlotIndividualPointEstimateType else "none"),
            ci              = with(options, if (posteriorDistributionPlotIndividualCi)            posteriorDistributionPlotIndividualCiType            else "none"),
            ci_level        = options[["posteriorDistributionPlotIndividualCiMass"]],
            ci_custom_left  = options[["posteriorDistributionPlotIndividualCiLower"]],
            ci_custom_right = options[["posteriorDistributionPlotIndividualCiUpper"]],
            bf_support      = options[["posteriorDistributionPlotIndividualCiBf"]]
          ) +
            jaspGraphs::geom_rangeframe() +
            jaspGraphs::themeJaspRaw()
        }
      } else {
        NULL
      },
      width  = 400 * 3,
      height = 400 * 2,
      position = 1,
      dependencies = jaspDeps(
        options = c(.bpcsDefaultDeps(), .bpcsPosteriorPlotDeps(options))
      )
    )
  )
}

.bpcsPosteriorPlotDeps <- function(options) {
  c(
    "posteriorDistributionPlot",
    "posteriorDistributionPlotIndividualPointEstimate",
    "posteriorDistributionPlotIndividualPointEstimateType",
    "posteriorDistributionPlotPriorDistribution",
    "posteriorDistributionPlotIndividualCi",
    "posteriorDistributionPlotIndividualCiType",
    # these match which options are conditionally enabled in the qml file.
    switch(options[["posteriorDistributionPlotIndividualCiType"]],
      "central" = "posteriorDistributionPlotIndividualCiMass",
      "HPD"     = "posteriorDistributionPlotIndividualCiMass",
      "custom"  = c("posteriorDistributionPlotIndividualCiLower", "posteriorDistributionPlotIndividualCiUpper"),
      "support" = "posteriorDistributionPlotIndividualCiBf"
    )
  )
}

.bpcsSequentialPointEstimatePlot <- function(jaspResults, dataset, options, fit) {

  base <- "sequentialAnalysisPointEstimatePlot"
  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  w <- 400
  plt <- createJaspPlot(title = gettext("Sequential Analysis Point Estimate"), width = 3*w, height = 2*w,
                        position = 2,
                        dependencies = jaspDeps(c(
                          .bpcsDefaultDeps(),
                          .bpcsPlotLayoutDeps(base),
                          "sequentialAnalysisPlotAdditionalInfo"
                        )))
  jaspResults[[base]] <- plt

  if (!.bpcsIsReady(options) || jaspResults$getError()) return()

  tryCatch({
    baseData <- paste0(base, "Data")
    sequentialPlotData <- jaspResults[[baseData]] %setOrRetrieve% (
      .bpcsComputeSequentialAnalysis(dataset, options, fit) |>
        createJaspState(dependencies = jaspDeps(options = c(.bpcsDefaultDeps(), .bpcsPlotLayoutDeps(base, hasAxes = FALSE))))
    )

    plt$plotObject <- .bpcsMakeSequentialPlot(sequentialPlotData, options, base)

  }, error = function(e) {

    plt$setError(gettextf("Unexpected error in sequential analysis plot: %s", e$message))

  })

}

.bpcsComputeSequentialAnalysis <- function(dataset, options, fit) {

  n <- nrow(dataset)
  nfrom <- min(n, 3L) # Gaussian could do 2, but let's not push it
  nto   <- n
  nby   <- 1L
  nseq <- seq(nfrom, nto, by = nby)
  estimates <- array(NA, c(6, 4, length(nseq)))

  keys <- c("mean", "median", "lower", "upper")
  dimnames(estimates) <- list(list(), keys, list())

  x <- dataset[[1L]]

  jaspBase::startProgressbar(length(nseq), label = gettext("Running sequential analysis"))

  for (i in seq_along(nseq)) {

    x_i <- x[1:nseq[i]]
    fit_i <- qc::bpc(
      x_i, chains = 1, warmup = 1000, iter = 5000, silent = TRUE, seed = 1,
      target   = options[["targetValue"]],
      LSL      = options[["lowerSpecificationLimitValue"]],
      USL      = options[["upperSpecificationLimitValue"]],
      prior_nu = jaspQualityControl:::.bpcsTpriorFromOptions(options)
    )

    sum_i <- summary(fit_i)$summary

    if (is.null(rownames(estimates)))
      rownames(estimates) <- sum_i$metric

    estimates[, , i] <- as.matrix(sum_i[keys])
    jaspBase::progressbarTick()
  }

  attr(estimates, "nseq") <- nseq

  # we could use this one, but only if the CI width is exactly equal to the one requested here.
  # that would be nice to add at some point so the values in the table are identical to those in the plot
  # sum_n <- summary(fit)$summary
  # estimates[, , n] <- as.matrix(sum_n[keys])

  return(estimates)
}

.bpcsMakeSequentialPlot <- function(estimates, options, base) {

  # this is somewhat ugly, but we convert the 3d array to a tibble for plotting
  # we don't create the tibble immediately in the previous function, because
  # it takes up more space in the state (which means larger jasp files)

  categoryNames <- c(gettext("Incapable"), gettext("Capable"), gettext("Satisfactory"), gettext("Excellent"), gettext("Super"))
  gridLines <- c(1, 4/3, 3/2, 2)
  # the extrema are missing here, these should be determined based on any leftover space.
  defaultCategoryPositions <- (gridLines[-1] + gridLines[-length(gridLines)]) / 2

  nseq <- attr(estimates, "nseq")

  pointEstimateOption <- paste0(base, "IndividualPointEstimateType")
  pointEstimateName <- if (options[[pointEstimateOption]] == "mean") "mean" else "median"
  tb <- tibble::tibble(
    metric = factor(rep(rownames(estimates), times = length(nseq))),
    n      = rep(nseq, each = nrow(estimates)),
    mean   = as.vector(estimates[, pointEstimateName, ]),
    lower  = as.vector(estimates[, "lower", ]),
    upper  = as.vector(estimates[, "upper", ]),
  )

  # get y scales per facet
  y_breaks_per_scale <- tapply(tb, tb$metric, \(x) {
    observedRange <- range(x$lower, x$upper, na.rm = TRUE)
    dist <- observedRange[2L] - observedRange[1L]

    observedRange[1L] <- min(observedRange[1L], gridLines[1L] - 0.1 * dist)
    observedRange[2L] <- max(observedRange[2L], gridLines[length(gridLines)] + 0.1 * dist)

    leftBreaks <- jaspGraphs::getPrettyAxisBreaks(observedRange)
    leftLimits <- range(leftBreaks)

    rightAxis <- ggplot2::waiver()
    if (options[["sequentialAnalysisPlotAdditionalInfo"]]) {
      rightBreaksShown <- c(
        (leftLimits[1L] + gridLines[1L]) / 2,
        defaultCategoryPositions,
        (leftLimits[2L] + gridLines[length(gridLines)]) / 2
      )
      rightBreaks <- numeric(2L*length(rightBreaksShown) + 1L)
      rightBreaks[1L]                                 <- leftLimits[1L]
      rightBreaks[seq(2, length(rightBreaks), 2)]     <- rightBreaksShown
      rightBreaks[seq(3, length(rightBreaks) - 2, 2)] <- gridLines
      rightBreaks[length(rightBreaks)]                <- leftLimits[2L]

      rightLabels <- character(length(rightBreaks))
      rightLabels[seq(2, length(rightLabels), 2)]   <- categoryNames
      rightAxis <- ggplot2::sec_axis(identity, breaks = rightBreaks, labels = rightLabels)
    }

    ggplot2::scale_y_continuous(breaks = leftBreaks, limits = range(leftBreaks),
                                minor_breaks = gridLines,
                                sec.axis = rightAxis)
  }, simplify = FALSE)

  ribbon <- NULL
  ciOption <- paste0(base, "IndividualCi")
  if (options[[ciOption]])
    ribbon <- ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), alpha = 0.3)

  extraTheme <- gridLinesLayer <- NULL
  sides <- "bl"
  if (options[["sequentialAnalysisPlotAdditionalInfo"]]) {
    # there are 11 ticks, the outermost we hide (NA) because one of their bounds is infinite
    # the inner ticks alternate between black and NA, so there is a tick at the grid lines
    # but no tick at the criteria text (which is secretly an axis tick label).
    rightTickColors <- c(NA, rep(c(NA, "black"), length.out = 9), NA)
    extraTheme <- ggplot2::theme(axis.ticks.y.right = ggplot2::element_line(colour =rightTickColors))
    sides      <- "blr"
    # I tried using minor.breaks for this, but these are not drawn properly with facet_grid and facetted_pos_scales
    gridLinesLayer <- ggplot2::geom_hline(
      data = data.frame(yintercept = gridLines),
      ggplot2::aes(yintercept = .data$yintercept),
      # show.legend = FALSE,
      linewidth = .5, color = "lightgray", linetype = "dashed"
    )

  }

  ggplot2::ggplot(tb, ggplot2::aes(x = .data$n, y = .data$mean)) +
    gridLinesLayer +
    ribbon +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_wrap(~ metric, scales = "free_y") +
    ggh4x::facetted_pos_scales(y = y_breaks_per_scale) +
    ggplot2::labs(
      x = gettext("Number of observations"),
      y = gettext("Estimate with 95% credible interval")
    ) +
    jaspGraphs::geom_rangeframe(sides = sides) +
    jaspGraphs::themeJaspRaw() +
    extraTheme

}

# Additional plot functions ----
.bpcsPriorPlot <- function(jaspResults, options, fit) {

  if (!options[["priorDistributionPlot"]])
    return()

  jaspResults[["priorDistributionPlot"]] %setOrRetrieve% (
    createJaspPlot(
      title = gettext("Prior Distribution"),
      plot  = if (.bpcsIsReady(options) && !is.null(fit)) {
        # TODO: Implement prior distribution plotting
        NULL
      } else {
        NULL
      },
      width  = 400 * 3,
      height = 400 * 2,
      position = 4,
      dependencies = jaspDeps(
        options = c(.bpcsDefaultDeps(), .bpcsPlotLayoutDeps("priorDistributionPlot", hasPrior = FALSE))
      )
    )
  )
}

.bpcsSequentialIntervalEstimatePlot <- function(jaspResults, dataset, options, fit) {

  base <- "sequentialAnalysisPointIntervalPlot"
  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  w <- 400
  plt <- createJaspPlot(title = gettext("Sequential Analysis Interval Estimate"), width = 3*w, height = 2*w,
                        position = 3,
                        dependencies = jaspDeps(c(
                          .bpcsDefaultDeps(),
                          .bpcsPlotLayoutDeps(base, hasEstimate = FALSE, hasCi = FALSE, hasType = TRUE),
                          "sequentialAnalysisPlotAdditionalInfo"
                        )))
  jaspResults[[base]] <- plt

  if (!.bpcsIsReady(options) || jaspResults$getError()) return()

  tryCatch({
    baseData <- paste0(base, "Data")
    sequentialPlotData <- jaspResults[[baseData]] %setOrRetrieve% (
      .bpcsComputeSequentialAnalysis(dataset, options, fit) |>
        createJaspState(dependencies = jaspDeps(options = c(.bpcsDefaultDeps(), .bpcsPlotLayoutDeps(base, hasAxes = FALSE, hasEstimate = FALSE, hasCi = FALSE, hasType = TRUE))))
    )

    plt$plotObject <- .bpcsMakeSequentialIntervalPlot(sequentialPlotData, options, base)

  }, error = function(e) {

    plt$setError(gettextf("Unexpected error in sequential analysis interval plot: %s", e$message))

  })

}

.bpcsMakeSequentialIntervalPlot <- function(estimates, options, base) {

  nseq <- attr(estimates, "nseq")

  # Get the type bounds from options
  typeLowerOption <- paste0(base, "TypeLower")
  typeUpperOption <- paste0(base, "TypeUpper")
  typeLower <- options[[typeLowerOption]]
  typeUpper <- options[[typeUpperOption]]

  tb <- tibble::tibble(
    metric = factor(rep(rownames(estimates), times = length(nseq))),
    n      = rep(nseq, each = nrow(estimates)),
    lower  = as.vector(estimates[, "lower", ]),
    upper  = as.vector(estimates[, "upper", ]),
  )

  # Calculate proportion in interval [typeLower, typeUpper]
  # This is a simplified version - may need to access actual posterior samples
  tb$proportion <- pmin(pmax((tb$lower + tb$upper) / 2, typeLower), typeUpper)

  y_breaks_per_scale <- tapply(tb, tb$metric, \(x) {
    observedRange <- c(0, 1)
    leftBreaks <- jaspGraphs::getPrettyAxisBreaks(observedRange)
    ggplot2::scale_y_continuous(breaks = leftBreaks, limits = c(0, 1))
  }, simplify = FALSE)

  ggplot2::ggplot(tb, ggplot2::aes(x = .data$n, y = .data$proportion)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_wrap(~ metric, scales = "free_y") +
    ggh4x::facetted_pos_scales(y = y_breaks_per_scale) +
    ggplot2::labs(
      x = gettext("Number of observations"),
      y = gettextf("P(%s < θ < %s)", typeLower, typeUpper)
    ) +
    jaspGraphs::geom_rangeframe(sides = "bl") +
    jaspGraphs::themeJaspRaw()

}

.bpcsPosteriorPredictivePlot <- function(jaspResults, options, fit) {

  if (!options[["posteriorPredictiveDistributionPlot"]])
    return()

  jaspResults[["posteriorPredictiveDistributionPlot"]] %setOrRetrieve% (
    createJaspPlot(
      title = gettext("Posterior Predictive Distribution"),
      plot  = if (.bpcsIsReady(options) && !is.null(fit)) {
        # TODO: Implement posterior predictive distribution plotting
        NULL
      } else {
        NULL
      },
      width  = 400 * 3,
      height = 400 * 2,
      position = 5,
      dependencies = jaspDeps(
        options = c(.bpcsDefaultDeps(), .bpcsPlotLayoutDeps("posteriorPredictiveDistributionPlot", hasPrior = FALSE))
      )
    )
  )
}

.bpcsPriorPredictivePlot <- function(jaspResults, options, fit) {

  if (!options[["priorPredictiveDistributionPlot"]])
    return()

  jaspResults[["priorPredictiveDistributionPlot"]] %setOrRetrieve% (
    createJaspPlot(
      title = gettext("Prior Predictive Distribution"),
      plot  = if (.bpcsIsReady(options) && !is.null(fit)) {
        # TODO: Implement prior predictive distribution plotting
        NULL
      } else {
        NULL
      },
      width  = 400 * 3,
      height = 400 * 2,
      position = 6,
      dependencies = jaspDeps(
        options = c(.bpcsDefaultDeps(), .bpcsPlotLayoutDeps("priorPredictiveDistributionPlot", hasPrior = FALSE))
      )
    )
  )
}
