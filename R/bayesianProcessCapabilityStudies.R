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

#'@export
bayesianProcessCapabilityStudies <- function(jaspResults, dataset, options) {

  fit <- .bpcsCapabilityTable(jaspResults, dataset, options)
  .bpcsCapabilityPlot(jaspResults, options, fit)
  .bpcsIntervalTable(jaspResults, options, fit)
  .bpcsSequentialPlot(jaspResults, dataset, options, fit)

}

.bpcsIsReady <- function(options) {
  length(options[["measurementLongFormat"]]) > 0L &&
    options[["measurementLongFormat"]] != "" &&
    options[["lowerSpecificationLimit"]] &&
    options[["upperSpecificationLimit"]] &&
    options[["target"]]
}

.bpcsDefaultDeps <- function() {
    c(
      # data
      "measurementLongFormat",
      # specification
      "target",      "lowerSpecificationLimit",      "upperSpecificationLimit",
      "targetValue", "lowerSpecificationLimitValue", "upperSpecificationLimitValue",
      # likelihood
      "capabilityStudyType"
      # TODO: prior
  )
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

  if (!is.null(options[["bpcsCapabilityTable"]]))
    return()

  table <- .bpcsCapabilityTableMeta(jaspResults, options)
  if (!.bpcsIsReady(options)) {

    if (options[["measurementLongFormat"]] != "")
      table$addFootnote(gettext("Please specify the Lower Specification Limit, Upper Specification Limit, and Target Value to compute the capability measures."))

    return()
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

  .bpcsCapabilityTableFill(table, resultsObject, options)
  return(resultsObject)

}

.bpcsCapabilityTableMeta <- function(jaspResults, options) {

  table <- createJaspTable(title = gettext("Capability Table"), position = 0)
  table$addColumnInfo(name = "metric",  title = gettext("Capability\nMeasure"), type = "string")
  table$addColumnInfo(name = "mean",    title = gettext("Mean"),               type = "number")
  table$addColumnInfo(name = "median",  title = gettext("Median"),             type = "number")
  table$addColumnInfo(name = "sd",      title = gettext("Std"),                type = "number")

  overtitle <- gettextf("%s%% Credible Interval", 100 * options[["credibleIntervalWidth"]])
  table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
  table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)

  table$dependOn(c("measurementLongFormat", "credibleIntervalWidth"))

  jaspResults[["bpcsCapabilityTable"]] <- table
  return(table)

}

.bpcsCapabilityTableFill <- function(table, resultsObject, options) {

  df <- as.data.frame(resultsObject[["summaryObject"]][["summary"]])
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
    interval_probability <- unlist(options[paste0("interval", 1:5)], use.names = FALSE)
    interval_summary <- summary(fit[["rawfit"]], interval_probability = interval_probability)[["interval_summary"]]
    colnames(interval_summary) <- c("metric", paste0("interval", 1:6))
    table$setData(interval_summary)

  }, error = function(e) {

    table$setError(gettextf("Unexpected error in interval table: %s", e$message))

  })

  return()
}

.bpcsIntervalTableMeta <- function(jaspResults, options) {

  table <- createJaspTable(title = gettext("Interval Table"))

  table$addColumnInfo(name = "metric",  title = gettext("Capability\nMeasure"), type = "string",
                      position = 3)

  intervalBounds <- c(-Inf, unlist(options[paste0("interval", 1:5)], use.names = FALSE), Inf)
  n <- length(intervalBounds)
  for (i in 1:(n - 1)) {
    j <- i + 1
    lhs <- if (i == 1)     "(" else "["
    rhs <- if (i == n - 1) ")" else "]"
    title <- sprintf("%s%.3f ,  %.3f%s", lhs, intervalBounds[i], intervalBounds[j], rhs)
    table$addColumnInfo(name = paste0("interval", i), title = title, type = "number")
  }
  table$dependOn(c(
    "measurementLongFormat", "intervalTable", "credibleIntervalWidth",
    paste0("interval", 1:5), "targetValue", "lowerSpecificationLimitValue", "upperSpecificationLimitValue"
  ))

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
        qc::plot_density(
          fit$summaryObject,
          what = c("Cp", "CpU", "CpL", "Cpk", "Cpm"),
          point_estimate  = with(options, if (posteriorDistributionPlotIndividualPointEstimate) posteriorDistributionPlotIndividualPointEstimateType else "none"),
          ci              = with(options, if (posteriorDistributionPlotIndividualCi)            posteriorDistributionPlotIndividualCiType            else "none"),
          ci_level        = options[["posteriorDistributionPlotIndividualCiMass"]],
          ci_custom_left  = options[["posteriorDistributionPlotIndividualCiLower"]],
          ci_custom_right = options[["posteriorDistributionPlotIndividualCiUpper"]],
          bf_support      = options[["posteriorDistributionPlotIndividualCiBf"]]
        ) +
          jaspGraphs::geom_rangeframe() +
          jaspGraphs::themeJaspRaw()
      } else {
        NULL
      },
      width  = 320 * 6,
      height = 320,
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

.bpcsSequentialPlot <- function(jaspResults, dataset, options, fit) {

  if (!options[["sequentialAnalysisPlot"]] || !is.null(jaspResults[["sequentialAnalysisPlot"]]))
    return()

  w <- 400
  plt <- createJaspPlot(title = gettext("Sequential Analysis"), width = 2*w, height = w,
                        position = 2,
                        dependencies = jaspDeps(c(
                          .bpcsDefaultDeps(),
                          "sequentialAnalysisPlot",
                          "sequentialAnalysisPlotPointEstimateType",
                          "sequentialAnalysisPlotCi",
                          "sequentialAnalysisPlotCiMass"
                        )))
  jaspResults[["sequentialAnalysisPlot"]] <- plt

  if (!.bpcsIsReady(options) || jaspResults$getError()) return()

  tryCatch({

    sequentialPlotData <- jaspResults[["sequentialAnalysisPlotData"]] %setOrRetrieve% (
      .bpcsComputeSequentialAnalysis(dataset, options, fit) |>
        createJaspState(dependencies = jaspDeps(options = c(.bpcsDefaultDeps(), "sequentialAnalysisPlot", "sequentialAnalysisPlotCiMass")))
    )

    jaspResults[["sequentialAnalysisPlot"]]$plotObject <- .bpcsMakeSequentialPlot(sequentialPlotData, options)

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

  # we could use this one, but only if the CI width didn't change...
  # sum_n <- summary(fit)$summary
  # estimates[, , n] <- as.matrix(sum_n[keys])

  return(estimates)
}

.bpcsMakeSequentialPlot <- function(estimates, options) {

  # this is somewhat ugly, but we convert the 3d array to a tibble for plotting
  # we don't create the tibble immediately in the previous function, because
  # it takes up more space in the state (which means larger jasp files)

  nseq <- attr(estimates, "nseq")

  pointEstimateName <- if (options[["sequentialAnalysisPlotPointEstimateType"]] == "mean") "mean" else "median"
  tb <- tibble::tibble(
    metric = factor(rep(rownames(estimates), times = length(nseq))),
    n      = rep(nseq, each = nrow(estimates)),
    mean   = as.vector(estimates[, pointEstimateName, ]),
    lower  = as.vector(estimates[, "lower", ]),
    upper  = as.vector(estimates[, "upper", ]),
  )

  # get y scales per facet
  y_breaks_per_scale <- tapply(tb, tb$metric, \(x) {
    breaks <- jaspGraphs::getPrettyAxisBreaks(range(x$lower, x$upper, na.rm = TRUE))
    ggplot2::scale_y_continuous(breaks = breaks, limits = range(breaks))
  }, simplify = FALSE)

  ribbon <- NULL
  if (options[["sequentialAnalysisPlotCi"]])
    ribbon <- ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.3)

  ggplot2::ggplot(tb, ggplot2::aes(x = n, y = mean)) +
    ribbon +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ metric, scales = "free_y") +
    ggh4x::facetted_pos_scales(y = y_breaks_per_scale) +
    ggplot2::labs(
      x = gettext("Number of observations"),
      y = gettext("Estimate with 95% credible interval")
    ) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(linewidth = .5, color = "lightgray", linetype = "dashed"))

}
