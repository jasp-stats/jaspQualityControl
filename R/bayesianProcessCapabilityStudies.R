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


#'@export
bayesianProcessCapabilityStudies <- function(jaspResults, dataset, options) {

  fit <- .bpcsCapabilityTable(jaspResults, dataset, options, position = 1)
  priorFit <- .bpcsSamplePosteriorOrPrior(jaspResults, dataset, options, prior = TRUE)

  .bpcsProcessOverviewPlot(jaspResults, dataset, options, fit, position = 2)
  .bpcsTimeSeriesPlot(jaspResults, dataset, options, position = 3)
  .bpcsCapabilityPlot(jaspResults, options, fit, priorFit, position = 4)
  .bpcsCapabilityPlot(jaspResults, options, fit, priorFit, position = 5, base = "priorDistributionPlot")

  .bpcsIntervalTable(jaspResults, options, fit, position = 6)

  .bpcsSequentialPointEstimatePlot(   jaspResults, dataset, options, fit, position = 7)
  .bpcsSequentialIntervalEstimatePlot(jaspResults, dataset, options, fit, position = 8)

  .bpcsPlotPredictive(jaspResults, dataset, options, fit,      position = 9, base = "posteriorPredictiveDistributionPlot")
  .bpcsPlotPredictive(jaspResults, dataset, options, priorFit, position = 10, base = "priorPredictiveDistributionPlot")

}

.bpcsIsReady <- function(options) {
  # hasData <- if (options[["dataFormat"]] == "longFormat") {
  #   length(options[["measurementLongFormat"]]) > 0L && options[["measurementLongFormat"]] != ""
  # } else {
  #   length(options[["measurementsWideFormat"]]) > 0L
  # }
  hasData <- length(options[["measurementLongFormat"]]) > 0L && options[["measurementLongFormat"]] != ""
  hasData &&
    options[["lowerSpecificationLimit"]] &&
    options[["upperSpecificationLimit"]] &&
    options[["target"]]
}

.bpcsStateDeps <- function() {
  c(
      # data
      # "dataFormat", "measurementLongFormat", "measurementsWideFormat",
      # "subgroupSizeType", "manualSubgroupSizeValue", "subgroup", "groupingVariableMethod",
      # "stagesLongFormat", "stagesWideFormat",
      "measurementLongFormat",
      # specification
      "target",      "lowerSpecificationLimit",      "upperSpecificationLimit",
      "targetValue", "lowerSpecificationLimitValue", "upperSpecificationLimitValue",
      # likelihood
      "capabilityStudyType",
      # prior
      "priorSettings", "normalModelComponentsList", "tModelComponentsList",
      # MCMC settings
      "noIterations", "noWarmup", "noChains"
  )
}

.bpcsDefaultDeps <- function() {
  c(
      .bpcsStateDeps(),
      "axisLabels",
      # metrics
      "Cp", "Cpu", "Cpl", "Cpk", "Cpc", "Cpm"
  )
}

.bpcsDistributionFromOptions <- function(options) {
  switch(
    options[["capabilityStudyType"]],
    "normalCapabilityAnalysis" = "normal",
    "tCapabilityAnalysis" = "t",
    stop("Unknown capability study type: ", options[["capabilityStudyType"]])
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
  "processCriteria"
}

.bpcsOverviewCriteriaDeps <- function() {
  .bpcsProcessCriteriaDeps()
}

.bpcsProcessCriteria <- function(options) {
  criteria <- options[["processCriteria"]]

  if (is.null(criteria)) {
    cutoffs <- unlist(options[paste0("interval", 1:4)], use.names = FALSE)
    labels <- unlist(options[paste0("intervalLabel", 1:5)], use.names = FALSE)
    if (length(cutoffs) != 4L || length(labels) != 5L)
      stop("Specify at least two process criteria regions.", call. = FALSE)
    criteria <- Map(
      function(lower, label, upper) list(lower = lower, label = label, upper = upper),
      c(-Inf, cutoffs), labels, c(cutoffs, Inf)
    )
  }

  if (!is.list(criteria) || length(criteria) < 2L)
    stop("Specify at least two process criteria regions.", call. = FALSE)

  lower <- vapply(criteria, function(region) as.numeric(region[["lower"]]), numeric(1))
  upper <- vapply(criteria, function(region) as.numeric(region[["upper"]]), numeric(1))
  labels <- vapply(criteria, function(region) as.character(region[["label"]]), character(1))

  if (anyNA(lower) || anyNA(upper) || any(!is.finite(lower) & lower != -Inf) || any(!is.finite(upper) & upper != Inf))
    stop("Process criteria bounds must be numeric.", call. = FALSE)
  if (any(lower >= upper))
    stop("Each process criterion must have a left bound below its right bound.", call. = FALSE)
  if (lower[1L] != -Inf || upper[length(upper)] != Inf)
    stop("The first and last process criteria must extend to negative and positive infinity.", call. = FALSE)
  if (any(lower[-1L] != upper[-length(upper)]))
    stop("Adjacent process criteria must share a boundary.", call. = FALSE)
  if (any(!nzchar(labels)))
    stop("Each process criterion needs a classification label.", call. = FALSE)

  list(
    lower = lower,
    upper = upper,
    labels = make.unique(labels),
    values = sort(unique(c(lower, upper)[is.finite(c(lower, upper))])),
    thresholdLabels = make.unique(labels[-length(labels)])
  )
}

.bpcsPriorComponentByName <- function(options, name) {
  components <- options$normalModelComponentsList
  for (comp in components) {
    if (comp$name == name)
      return(comp)
  }
  return(NULL)
}

.bpcsPriorFromComponent <- function(optionsPrior, paramName) {
  if (is.null(optionsPrior))
    return(NULL)

  if (optionsPrior$type == "jeffreys")
    return(paste0("Jeffreys_", paramName))

  arguments <- list()

  arguments[["distribution"]] <- switch(
    optionsPrior[["type"]],
    "gammaAB" = "gamma",
    "gammaK0" = "gamma",
    optionsPrior[["type"]]
  )

  arguments[["parameters"]] <- switch(
    optionsPrior[["type"]],
    "normal"      = list("mean" = optionsPrior[["mu"]], "sd" = optionsPrior[["sigma"]]),
    "t"           = list("location" = optionsPrior[["mu"]], "scale" = optionsPrior[["sigma"]], "df" = optionsPrior[["nu"]]),
    "cauchy"      = list("location" = optionsPrior[["mu"]], "scale" = optionsPrior[["theta"]]),
    "gammaAB"     = list("shape" = optionsPrior[["alpha"]], "rate" = optionsPrior[["beta"]]),
    "gammaK0"     = list("shape" = optionsPrior[["k"]], "rate" = 1/optionsPrior[["theta"]]),
    "invgamma"    = list("shape" = optionsPrior[["alpha"]], "scale" = optionsPrior[["beta"]]),
    "lognormal"   = list("meanlog" = optionsPrior[["mu"]], "sdlog" = optionsPrior[["sigma"]]),
    "beta"        = list("alpha" = optionsPrior[["alpha"]], "beta" = optionsPrior[["beta"]]),
    "uniform"     = list("a" = optionsPrior[["a"]], "b" = optionsPrior[["b"]]),
    "exponential" = list("rate" = optionsPrior[["lambda"]]),
    "spike"       = list("location" = optionsPrior[["x0"]])
  )

  if(!arguments[["distribution"]] %in% c("spike", "uniform")) {
    arguments[["truncation"]] <- list(
      lower   = optionsPrior[["truncationLower"]],
      upper   = optionsPrior[["truncationUpper"]]
    )
  }

  return(do.call(BayesTools::prior, arguments))
}

.bpcsMuPriorFromOptions <- function(options) {
  if (options$priorSettings == "default") {
    return("Jeffreys_mu")
  } else {
    comp <- .bpcsPriorComponentByName(options, "mean")
    return(.bpcsPriorFromComponent(comp, "mu"))
  }
}

.bpcsSigmaPriorFromOptions <- function(options) {
  if (options$priorSettings == "default") {
    return("Jeffreys_sigma")
  } else {
    comp <- .bpcsPriorComponentByName(options, "sigma")
    return(.bpcsPriorFromComponent(comp, "sigma"))
  }
}
.bpcsTPriorFromOptions <- function(options) {

  switch(options[["capabilityStudyType"]],
    "normalCapabilityAnalysis" = NULL,
    "tCapabilityAnalysis"      = .bpcsPriorFromComponent(.bpcsPriorComponentByName(options, "df"), "df"),

    stop("Unknown capability study type: ", options[["capabilityStudyType"]])
  )
}

.bpcsPriorHelper <- function(options) {
  if (options$priorSettings == "default") {
    if (options[["capabilityStudyType"]] == "normalCapabilityAnalysis") {
      return("DCSI")
    }
    return("Jeffreys")
  }

  mu_prior    <- .bpcsMuPriorFromOptions(options)
  sigma_prior <- .bpcsSigmaPriorFromOptions(options)
  nu_prior    <- .bpcsTPriorFromOptions(options)

  args <- list(mu = mu_prior, sigma = sigma_prior)
  if (!is.null(nu_prior)) {
    args$nu <- nu_prior
  }

  do.call(qc::prior_independent, args)
}

# Tables ----
.bpcsCapabilityTable <- function(jaspResults, dataset, options, position) {

  # Check if we already have the results cached
  if (!is.null(jaspResults[["bpcsCapabilityTable"]]))
    return(.bpcsSamplePosteriorOrPrior(jaspResults, dataset, options)) # will return object from state (if it exists)

  table <- .bpcsCapabilityTableMeta(jaspResults, options, position = position)
  if (!.bpcsIsReady(options)) {

    if (options[["measurementLongFormat"]] != "" || length(options[["measurementsWideFormat"]]) > 0)
      table$addFootnote(gettext(
        "Please specify the Lower Specification Limit, Upper Specification Limit, and Target Value to compute the capability measures."
      ))

    return(NULL)
  }

  resultsObject <- .bpcsSamplePosteriorOrPrior(jaspResults, dataset, options)

  .bpcsCapabilityTableFill(table, resultsObject, options)
  return(resultsObject)

}

.bpcsSamplePosteriorOrPrior <- function(jaspResults, dataset, options, prior = FALSE) {

  base <- if (prior) "bpcsPriors" else "bpcs"
  if (prior && !.bpcsCanSampleFromPriors(options))
    return(NULL)

  if (!is.null(jaspResults[[paste0(base, "ResultsObject")]]))
    return(jaspResults[[paste0(base, "ResultsObject")]]$object)

  x <- if (ncol(dataset) > 0L) dataset[[1L]] else NULL

  rawfit <- jaspResults[[paste0(base, "State")]] %setOrRetrieve% (
    qc::bpc(
      x, chains = 1, warmup = 1000, iter = 5000, silent = TRUE, seed = 1,
      distribution  = .bpcsDistributionFromOptions(options),
      target        = options[["targetValue"]],
      LSL           = options[["lowerSpecificationLimitValue"]],
      USL           = options[["upperSpecificationLimitValue"]],
      prior         = .bpcsPriorHelper(options),
      sample_priors = prior
    ) |>
      createJaspState(jaspDeps(.bpcsStateDeps()))
  )

  summaryObject <- jaspResults[[paste0(base, "SummaryState")]] %setOrRetrieve% (
    summary(
      rawfit, ci.level = options[["credibleIntervalWidth"]]
    ) |>
      createJaspState(jaspDeps(
        options = c(.bpcsStateDeps(), "credibleIntervalWidth")
      ))
  )

  resultsObject <- list(
    rawfit           = rawfit,
    summaryObject    = summaryObject
  )

  jaspResults[[paste0(base, "ResultsObject")]] <- createJaspState(resultsObject)

  return(resultsObject)
}

.bpcsCanSampleFromPriors <- function(options) {
  if (options$priorSettings != "default") {
    return(TRUE)
  }
  options[["capabilityStudyType"]] == "normalCapabilityAnalysis"
}

.bpcsCapabilityTableMeta <- function(jaspResults, options, position) {

  table <- createJaspTable(title = gettext("Capability Table"), position = position)
  table$addColumnInfo(name = "metric",  title = gettext("Measure"), type = "string")
  table$addColumnInfo(name = "mean",    title = gettext("Mean"),    type = "number")
  table$addColumnInfo(name = "median",  title = gettext("Median"),  type = "number")
  table$addColumnInfo(name = "sd",      title = gettext("Std"),     type = "number")

  overtitle <- gettextf("%s%% Credible Interval", 100 * options[["credibleIntervalWidth"]])
  table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
  table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)

  table$dependOn(c(.bpcsDefaultDeps(), "credibleIntervalWidth"))

  jaspResults[["bpcsCapabilityTable"]] <- table
  return(table)

}

.bpcsGetSelectedMetrics <- function(options) {
  allMetrics <- c("Cp", "Cpu", "Cpl", "Cpk", "Cpc", "Cpm")
  selectedMetrics <- allMetrics[c(options[["Cp"]],   options[["Cpu"]],  options[["Cpl"]],
                                  options[["Cpk"]],  options[["Cpc"]],  options[["Cpm"]])]
  return(selectedMetrics)
}

getCustomAxisLimits <- function(options, base) {
  keys <- c(paste0(base, "custom_x_", c("min", "max")), paste0(base, "custom_y_", c("min", "max")))
  values <- lapply(keys, function(k) options[[k]])
  names(values) <- c("xmin", "xmax", "ymin", "ymax")
  values
}
# end utils

.bpcsCapabilityTableFill <- function(table, resultsObject, options) {

  df <- as.data.frame(resultsObject[["summaryObject"]][["summary"]])

  # Filter metrics based on user selection
  selectedMetrics <- .bpcsGetSelectedMetrics(options)

  if (length(selectedMetrics) > 0) {
    df <- df[df$metric %in% selectedMetrics, , drop = FALSE]
  }

  table$setData(df)

}

.bpcsIntervalTable <- function(jaspResults, options, fit, position) {

  if (!options[["intervalTable"]])
    return()

  table <- .bpcsIntervalTableMeta(jaspResults, options, position)
  if (!.bpcsIsReady(options) || is.null(fit))
    return()

  selectedMetrics <- .bpcsGetSelectedMetrics(options)
  tryCatch({

    criteria <- .bpcsProcessCriteria(options)
    interval_probability <- criteria$values
    interval_summary <- summary(fit[["rawfit"]], interval_probability = interval_probability)[["interval_summary"]]
    colnames(interval_summary) <- c("metric", paste0("interval", seq_along(criteria$labels)))
    interval_summary <- subset(interval_summary, metric %in% selectedMetrics)
    table$setData(interval_summary)

  }, error = function(e) {

    table$setError(gettextf("Unexpected error in interval table: %s", e$message))

  })

  return()
}

.bpcsIntervalTableMeta <- function(jaspResults, options, position) {

  table <- createJaspTable(title = gettext("Interval Table"), position = position)

  table$addColumnInfo(name = "metric", title = gettext("Capability\nMeasure"), type = "string")

  criteria <- .bpcsProcessCriteria(options)
  intervalBounds <- c(criteria$lower[1L], criteria$upper)
  intervalNames <- criteria$labels
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
.bpcsProcessOverviewPlot <- function(jaspResults, dataset, options, fit, position) {

  base <- "processOverview"
  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  plot <- createJaspPlot(
    title = gettext("Process Overview"), width = 1200, height = 800,
    position = position,
    dependencies = jaspDeps(c(
      base, "processOverviewMetric", "processOverviewThreshold", .bpcsDefaultDeps(), .bpcsOverviewCriteriaDeps()
    ))
  )
  jaspResults[[base]] <- plot

  if (!.bpcsIsReady(options) || is.null(fit) || jaspResults$getError())
    return()

  tryCatch({
    data <- .bpcsOverviewData(jaspResults, dataset, options, fit)
    plot$plotObject <- .bpcsMakeProcessOverviewPlot(dataset, options, fit, data)
  }, error = function(e) {
    plot$setError(gettextf("Unexpected error in process overview: %s", e$message))
  })
}

.bpcsOverviewData <- function(jaspResults, dataset, options, fit) {

  base <- "processOverviewData"
  jaspResults[[base]] %setOrRetrieve% (
    .bpcsComputeOverviewData(dataset, options, fit) |>
      createJaspState(jaspDeps(c("processOverviewMetric", .bpcsDefaultDeps(), .bpcsOverviewCriteriaDeps())))
  )
}

.bpcsOverviewCriteria <- function(options) {
  .bpcsProcessCriteria(options)
}

.bpcsOverviewThreshold <- function(options, criteria = .bpcsOverviewCriteria(options)) {
  thresholdOption <- options[["processOverviewThreshold"]]
  threshold <- suppressWarnings(as.numeric(thresholdOption))
  if (length(threshold) == 1L && is.finite(threshold) && threshold %in% criteria$values)
    return(threshold)

  legacyIndex <- match(thresholdOption, paste0("interval", 1:4))
  if (!is.na(legacyIndex) && legacyIndex <= length(criteria$values))
    return(criteria$values[[legacyIndex]])

  stop("Unknown process overview threshold.")
}

.bpcsOverviewRegionColors <- function(criteria) {
  colors <- qc::default_region_colors()
  if (length(criteria$labels) > length(colors))
    colors <- grDevices::hcl.colors(length(criteria$labels), palette = "Set 2")
  else
    colors <- colors[seq_len(length(criteria$labels))]

  names(colors) <- criteria$labels
  colors
}

.bpcsOverviewSampleSizes <- function(n) {
  if (!is.finite(n) || n < 3L)
    stop("Process overview requires at least 3 observations.", call. = FALSE)

  unique(as.integer(round(seq(3L, n, length.out = min(5L, n - 2L)))))
}

.bpcsOverviewMetric <- function(options) {
  metric <- options[["processOverviewMetric"]]
  if (is.null(metric) || !nzchar(metric))
    metric <- "Cpk"
  if (!metric %in% c("Cp", "Cpu", "Cpl", "Cpk", "Cpc", "Cpm"))
    stop("Unknown process overview capability metric.", call. = FALSE)
  metric
}

.bpcsMetricExceedanceProbabilities <- function(fit, metric, criteria) {
  intervals <- summary(fit, interval_probability = criteria)[["interval_summary"]]
  metricRow <- intervals[as.character(intervals$metric) == metric, -1L, drop = FALSE]
  if (nrow(metricRow) != 1L)
    stop(gettextf("The capability metric %s is unavailable.", metric), call. = FALSE)

  intervalProbabilities <- as.numeric(metricRow[1L, ])
  expectedLength <- length(criteria) + 1L
  if (length(intervalProbabilities) != expectedLength)
    stop(
      gettextf("Expected %d interval probabilities for %s, but received %d.", expectedLength, metric, length(intervalProbabilities)),
      call. = FALSE
    )

  probabilities <- rev(cumsum(rev(intervalProbabilities[-1L])))
  if (length(probabilities) != length(criteria))
    stop("Could not calculate process overview probabilities.", call. = FALSE)
  probabilities
}

.bpcsCpkExceedanceProbabilities <- function(fit, criteria) {
  .bpcsMetricExceedanceProbabilities(fit, "Cpk", criteria)
}

.bpcsFit <- function(x, options, prior = .bpcsPriorHelper(options)) {
  qc::bpc(
    x, chains = 1, warmup = 1000, iter = 5000, silent = TRUE, seed = 1,
    distribution = .bpcsDistributionFromOptions(options),
    target = options[["targetValue"]],
    LSL = options[["lowerSpecificationLimitValue"]],
    USL = options[["upperSpecificationLimitValue"]],
    prior = prior
  )
}

.bpcsComputeOverviewData <- function(dataset, options, fit) {

  criteria <- .bpcsOverviewCriteria(options)
  metric <- .bpcsOverviewMetric(options)
  sampleSizes <- .bpcsOverviewSampleSizes(nrow(dataset))
  probabilities <- matrix(NA_real_, nrow = length(sampleSizes), ncol = length(criteria$values))
  x <- dataset[[1L]]

  jaspBase::startProgressbar(length(sampleSizes), label = gettext("Running process overview"))
  failed <- 0L
  for (i in seq_along(sampleSizes)) {
    fit_i <- tryCatch(
      .bpcsFit(x[seq_len(sampleSizes[i])], options),
      error = function(e) NULL
    )

    if (is.null(fit_i)) {
      failed <- failed + 1L
    } else {
      probabilities[i, ] <- .bpcsMetricExceedanceProbabilities(fit_i, metric, criteria$values)
    }
    jaspBase::progressbarTick()
  }

  if (failed > 0L && failed / length(sampleSizes) > 0.1) {
    stop(
      sprintf(
        "%d of %d cumulative fits failed (%.0f%%). Cannot render process overview.",
        failed, length(sampleSizes), 100 * failed / length(sampleSizes)
      ),
      call. = FALSE
    )
  }

  list(
    sampleSizes = sampleSizes,
    probabilities = probabilities,
    sensitivity = .bpcsComputeOverviewSensitivity(dataset, options, fit, metric, criteria$values)
  )
}

.bpcsComputeOverviewSensitivity <- function(dataset, options, fit, metric, criteria) {

  activePrior <- .bpcsPriorHelper(options)
  entries <- stats::setNames(list(fit$rawfit), gettext("Active prior"))
  unavailable <- character()
  x <- dataset[[1L]]

  addPrior <- function(label, prior) {
    priorFit <- tryCatch(.bpcsFit(x, options, prior = prior), error = function(e) NULL)
    if (is.null(priorFit)) {
      unavailable <<- c(unavailable, gettextf("%s prior could not be fitted.", label))
    } else {
      entries[[label]] <<- priorFit
    }
  }

  if (.bpcsDistributionFromOptions(options) == "normal" && !identical(activePrior, "DCSI")) {
    addPrior(gettext("DCSI"), "DCSI")
  }
  if (!identical(activePrior, "Jeffreys")) {
    addPrior(gettext("Jeffreys"), "Jeffreys")
  }
  if (.bpcsDistributionFromOptions(options) == "t") {
    unavailable <- c(unavailable, gettext("DCSI is unavailable for the Student's t model."))
  }

  probabilities <- vapply(entries, .bpcsMetricExceedanceProbabilities, numeric(length(criteria)), metric = metric, criteria = criteria)
  if (is.null(dim(probabilities)))
    probabilities <- matrix(probabilities, ncol = 1L, dimnames = list(NULL, names(entries)))

  list(probabilities = probabilities, unavailable = paste(unavailable, collapse = "\n"))
}

.bpcsMakeProcessOverviewPlot <- function(dataset, options, fit, data) {

  criteria <- .bpcsOverviewCriteria(options)
  metric <- .bpcsOverviewMetric(options)
  threshold <- .bpcsOverviewThreshold(options, criteria)
  thresholdIndex <- match(threshold, criteria$values)
  regionColors <- .bpcsOverviewRegionColors(criteria)
  rawData <- dataset[[1L]]

  timeSeriesPlot <- qc::plot_time_series(
    rawData,
    LSL = options[["lowerSpecificationLimitValue"]],
    target = options[["targetValue"]],
    USL = options[["upperSpecificationLimitValue"]]
  ) +
    ggplot2::labs(title = gettext("Time series")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  densityPlot <- qc::plot_density(
    fit$summaryObject,
    what = metric,
    point_estimate = "none",
    ci = "none",
    single_panel = TRUE,
    show_regions = TRUE,
    textsize = 8,
    region_cutoffs = criteria$values,
    region_colors = regionColors
  ) +
    ggplot2::labs(title = gettextf("%s capability regions", metric)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")

  # hacky, should probably add this in qc
  densityPlot@layers$geom_line$show.legend  <- FALSE
  # densityPlot@layers$geom_point$show.legend <- FALSE

  overTimeData <- data.frame(
    observation = data$sampleSizes,
    probability = data$probabilities[, thresholdIndex]
  )
  overTimePlot <- ggplot2::ggplot(overTimeData, ggplot2::aes(x = observation, y = probability)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title = gettextf("P(%s > %g) over time", metric, threshold),
      x = gettext("Number of observations"), y = gettext("Posterior probability")
    ) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  sensitivityData <- data.frame(
    prior = factor(colnames(data$sensitivity$probabilities), levels = colnames(data$sensitivity$probabilities)),
    probability = data$sensitivity$probabilities[thresholdIndex, ]
  )
  sensitivityPlot <- ggplot2::ggplot(sensitivityData, ggplot2::aes(x = prior, y = probability)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title = gettextf("P(%s > %g) by prior", metric, threshold),
      x = NULL, y = gettext("Posterior probability"), caption = data$sensitivity$unavailable
    ) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  patchwork::wrap_plots(timeSeriesPlot, densityPlot, overTimePlot, sensitivityPlot, ncol = 2) +
    patchwork::plot_layout(guides = 'collect')
}

.bpcsTimeSeriesPlot <- function(jaspResults, dataset, options, position) {

  base <- "timeSeriesPlot"
  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  plot <- createJaspPlot(
    title = gettext("Time Series Plot"), width = 600, height = 400,
    position = position,
    dependencies = jaspDeps(c(
      base, "measurementLongFormat",
      "lowerSpecificationLimit", "lowerSpecificationLimitValue",
      "target", "targetValue",
      "upperSpecificationLimit", "upperSpecificationLimitValue"
    ))
  )
  jaspResults[[base]] <- plot

  if (ncol(dataset) == 0L || jaspResults$getError())
    return()

  tryCatch({
    plot$plotObject <- qc::plot_time_series(
      dataset[[1L]],
      LSL = if (options[["lowerSpecificationLimit"]]) options[["lowerSpecificationLimitValue"]] else NULL,
      target = if (options[["target"]]) options[["targetValue"]] else NULL,
      USL = if (options[["upperSpecificationLimit"]]) options[["upperSpecificationLimitValue"]] else NULL
    ) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
  }, error = function(e) {
    plot$setError(gettextf("Unexpected error in time series plot: %s", e$message))
  })
}

.bpcsCapabilityPlot <- function(jaspResults, options, fit, priorFit, position, base = "posteriorDistributionPlot") {

  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  singlePanel <- options[[paste0(base, "PanelLayout")]] != "multiplePanels"

  isPost <- base == "posteriorDistributionPlot"
  summaryObject <- if (isPost) fit$summaryObject else priorFit$summaryObject
  # only if the user asked for it
  priorSummaryObject <- if (isPost && options[[paste0(base, "PriorDistribution")]]) priorFit$summaryObject else NULL

  jaspPlt <- createJaspPlot(
    title = if (isPost) gettext("Posterior Distribution") else gettext("Prior Distribution"),
    width  = 400 * (if (singlePanel) 1 else 3),
    height = 400 * (if (singlePanel) 1 else 2),
    position = position,
    dependencies = jaspDeps(
      options = c(
        .bpcsDefaultDeps(),
        # .bpcsPosteriorPlotDeps(options),
        .bpcsPlotLayoutDeps(base, hasType = FALSE)
      )
    )
  )
  jaspResults[[base]] <- jaspPlt

  if (!.bpcsIsReady(options) || (isPost && is.null(fit)) || (!isPost && is.null(priorFit)))
    return()

  if (!isPost && !.bpcsCanSampleFromPriors(options)) {
    jaspPlt$width <- 400
    jaspPlt$height <- 400
    jaspPlt$setError(gettext("Prior distribution cannot be shown for improper priors."))
    return()
  }

  tryCatch({

    # Get selected metrics
    selectedMetrics <- .bpcsGetSelectedMetrics(options)

    if (length(selectedMetrics) == 0) {
      NULL
    } else {

      jaspPlt$plotObject <- qc::plot_density(
        summaryObject,
        what = selectedMetrics,
        point_estimate     = if (options[[paste0(base, "IndividualPointEstimate")]]) options[[paste0(base, "IndividualPointEstimateType")]] else "none",
        ci                 = if (options[[paste0(base, "IndividualCi")]])            options[[paste0(base, "IndividualCiType")]]            else "none",
        ci_level           = options[[paste0(base, "IndividualCiMass")]],
        ci_custom_left     = options[[paste0(base, "IndividualCiLower")]],
        ci_custom_right    = options[[paste0(base, "IndividualCiUpper")]],
        bf_support         = options[[paste0(base, "IndividualCiBf")]],
        single_panel       = singlePanel,
        axes               = options[[paste0(base, "Axes")]],
        axes_custom        = getCustomAxisLimits(options, base),
        priorSummaryObject = priorSummaryObject
      ) +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw()


    }
  }, error = function(e) {
    jaspPlt$width  <- 400
    jaspPlt$height <- 400
    jaspPlt$setError(
      if (isPost) gettextf("Unexpected error in posterior distribution plot: %s", e$message)
      else gettextf("Unexpected error in prior distribution plot: %s", e$message)
    )
  })

}

# .bpcsPosteriorPlotDeps <- function(options) {
#   c(
#     "posteriorDistributionPlot",
#     "posteriorDistributionPlotIndividualPointEstimate",
#     "posteriorDistributionPlotIndividualPointEstimateType",
#     "posteriorDistributionPlotPriorDistribution",
#     "posteriorDistributionPlotIndividualCi",
#     "posteriorDistributionPlotIndividualCiType",
#     # these match which options are conditionally enabled in the qml file.
#     switch(options[["posteriorDistributionPlotIndividualCiType"]],
#       "central" = "posteriorDistributionPlotIndividualCiMass",
#       "HPD"     = "posteriorDistributionPlotIndividualCiMass",
#       "custom"  = c("posteriorDistributionPlotIndividualCiLower", "posteriorDistributionPlotIndividualCiUpper"),
#       "support" = "posteriorDistributionPlotIndividualCiBf"
#     )
#   )
# }

.bpcsSequentialPointEstimatePlot <- function(jaspResults, dataset, options, fit, position) {

  base <- "sequentialAnalysisPointEstimatePlot"
  # "sequentialAnalysisPointIntervalPlot"
  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  w <- 400
  plt <- createJaspPlot(title = gettext("Sequential Analysis Point Estimate"), width = 3*w, height = 2*w,
                        position = position,
                        dependencies = jaspDeps(c(
                          .bpcsDefaultDeps(),
                          .bpcsPlotLayoutDeps(base, hasPrior = FALSE),
                          "sequentialAnalysisPlotAdditionalInfo"
                        )))
  jaspResults[[base]] <- plt

  if (!.bpcsIsReady(options) || jaspResults$getError()) return()

  sequentialPlotData <- .bpcsGetSequentialAnalysis(jaspResults, dataset, options, fit)

  if (!is.null(sequentialPlotData$error)) {
    plt$setError(sequentialPlotData$error)
  } else {
    tryCatch({
      plt$plotObject <- .bpcsMakeSequentialPlot(sequentialPlotData$data, options, base)
    }, error = function(e) {
      plt$setError(gettextf("Unexpected error in sequential analysis point estimate plot: %s", e$message))
    }
    )
  }
}

.bpcsSequentialIntervalEstimatePlot <- function(jaspResults, dataset, options, fit, position) {

  # base <- "sequentialAnalysisPointEstimatePlot"
  base <- "sequentialAnalysisPointIntervalPlot"
  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  w <- 400
  plt <- createJaspPlot(title = gettext("Sequential Analysis Interval Estimate"), width = 3*w, height = 2*w,
                        position = position,
                        dependencies = jaspDeps(c(
                          .bpcsDefaultDeps(),
                          .bpcsPlotLayoutDeps(base, hasPrior = FALSE)
                        )))
  jaspResults[[base]] <- plt

  if (!.bpcsIsReady(options) || jaspResults$getError()) return()

  sequentialPlotData <- .bpcsGetSequentialAnalysis(jaspResults, dataset, options, fit)

  if (!is.null(sequentialPlotData$error)) {
    plt$setError(sequentialPlotData$error)
  } else {
    tryCatch({
      plt$plotObject <- .bpcsMakeSequentialPlot(sequentialPlotData$data, options, base, custom = TRUE)
    }, error = function(e) {
      plt$setError(gettextf("Unexpected error in sequential analysis interval estimate plot: %s", e$message))
    }
    )
  }
}

.bpcsGetSequentialAnalysis <- function(jaspResults, dataset, options, fit) {

  if (!.bpcsIsReady(options) || jaspResults$getError()) return()

  base1 <- "sequentialAnalysisPointEstimatePlot"
  base2 <- "sequentialAnalysisPointIntervalPlot"

  baseData <- "SequentialAnalysisData"
  tryCatch({
    sequentialPlotData <- jaspResults[[baseData]] %setOrRetrieve% (
      .bpcsComputeSequentialAnalysis(dataset, options, fit) |>
        createJaspState(dependencies = jaspDeps(
          options = c(.bpcsStateDeps(),
                      paste0(base2, c("TypeLower", "TypeUpper")))
          ))
    )

    return(list(data = sequentialPlotData, error = NULL))

  }, error = function(e) {

    return(list(data = NULL, error = e$message))

  })

}

.bpcsComputeSequentialAnalysis <- function(dataset, options, fit) {

  n <- nrow(dataset)
  if (!is.finite(n) || n < 3L) {
    stop("Sequential analysis requires at least 3 observations.", call. = FALSE)
  }
  nfrom <- 3L
  nto   <- n
  nby   <- 1L
  nseq <- seq(nfrom, nto, by = nby)
  estimates <- array(NA, c(6, 5, length(nseq)))

  hasCustom <- options$sequentialAnalysisPointIntervalPlot
  customBounds <- c(options$sequentialAnalysisPointIntervalPlotTypeLower,
                    options$sequentialAnalysisPointIntervalPlotTypeUpper)

  keys <- c("mean", "median", "lower", "upper", "custom")
  dimnames(estimates) <- list(list(), keys, list())

  x <- dataset[[1L]]

  jaspBase::startProgressbar(length(nseq), label = gettext("Running sequential analysis"))

  prior <- .bpcsPriorHelper(options)
  n_failed <- 0L
  for (i in seq_along(nseq)) {

    x_i <- x[1:nseq[i]]
    fit_i <- tryCatch(
      qc::bpc(
        x_i, chains = 1, warmup = 1000, iter = 5000, silent = TRUE, seed = 1,
        distribution = .bpcsDistributionFromOptions(options),
        target      = options[["targetValue"]],
        LSL         = options[["lowerSpecificationLimitValue"]],
        USL         = options[["upperSpecificationLimitValue"]],
        prior       = prior
      ),
      error = function(e) NULL
    )

    if (is.null(fit_i)) {
      n_failed <- n_failed + 1L
      jaspBase::progressbarTick()
      next
    }

    sum_fit_i <- summary(fit_i, interval_probability = customBounds)
    sum_i <- sum_fit_i$summary
    custom_i <- sum_fit_i$interval_summary[, 3, drop = FALSE]
    colnames(custom_i) <- "custom"
    sum_i <- cbind(sum_i, custom_i)

    if (is.null(rownames(estimates)))
      rownames(estimates) <- sum_i$metric

    estimates[, , i] <- as.matrix(sum_i[keys])
    jaspBase::progressbarTick()
  }

  if (n_failed > 0L && n_failed / length(nseq) > 0.1) {
    stop(
      sprintf(
        "%d of %d sequential fits failed (%.0f%%). Cannot render plot.",
        n_failed, length(nseq), 100 * n_failed / length(nseq)
      ),
      call. = FALSE
    )
  }

  attr(estimates, "nseq") <- nseq

  # we could use this one, but only if the CI width is exactly equal to the one requested here.
  # that would be nice to add at some point so the values in the table are identical to those in the plot
  # sum_n <- summary(fit)$summary
  # estimates[, , n] <- as.matrix(sum_n[keys])

  return(estimates)
}

.bpcsMakeSequentialPlot <- function(estimates, options, base, custom = FALSE) {

  # this function should move to qc, and these are the arguments that should be passed to the arguments of that function
  single_panel <- options[[paste0(base, "PanelLayout")]] != "multiplePanels"
  axes         <- options[[paste0(base, "Axes")]]
  axes_custom  <- getCustomAxisLimits(options, base)

  pointEstimateOption <- paste0(base, "IndividualPointEstimateType")
  pointEstimateName <- if (options[[pointEstimateOption]] == "mean") "mean" else "median"
  add_additional_info <- options[["sequentialAnalysisPlotAdditionalInfo"]]

  selectedMetrics <- .bpcsGetSelectedMetrics(options)
  if (length(selectedMetrics) == 0L)
    return(NULL)

  ciOption <- paste0(base, "IndividualCi")
  has_ci <- options[[ciOption]]

  if (custom) {
    has_ci <- FALSE
    pointEstimateName <- "custom"
    add_additional_info <- FALSE
    y_limits <- c(0, 1)
    y_title <- gettextf("P(%1$.3f \u2264 x \u2264 %2$.3f)",
                       options$sequentialAnalysisPointIntervalPlotTypeLower,
                       options$sequentialAnalysisPointIntervalPlotTypeUpper)
  } else {

    y_title <- if (has_ci) {
      gettextf("Estimate with 95%% credible interval")
    } else {
      gettext("Estimate")
    }
  }

  # this is somewhat ugly, but we convert the 3d array to a tibble for plotting
  # we don't create the tibble immediately in the previous function, because
  # it takes up more space in the state (which means larger jasp files)

  categoryNames <- c(gettext("Incapable"), gettext("Capable"), gettext("Satisfactory"), gettext("Excellent"), gettext("Super"))
  gridLines <- c(1, 4/3, 3/2, 2)
  # the extrema are missing here, these should be determined based on any leftover space.
  defaultCategoryPositions <- (gridLines[-1] + gridLines[-length(gridLines)]) / 2

  nseq <- attr(estimates, "nseq")

  tb <- tibble::tibble(
    metric = factor(rep(rownames(estimates), times = length(nseq))),
    n      = rep(nseq, each = nrow(estimates)),
    mean   = as.vector(estimates[, pointEstimateName, ]),
    lower  = as.vector(estimates[, "lower", ]),
    upper  = as.vector(estimates[, "upper", ]),
  )
  tb <- tb[tb$metric %in% selectedMetrics, , drop = FALSE]
  if (length(selectedMetrics) == 1L)
    single_panel <- TRUE

  # get y scales per facet
  if (single_panel) {

    observedRange <- range(tb$lower, tb$upper, na.rm = TRUE)
    if (!all(is.finite(observedRange))) {
      observedRange <- c(0, 1)
    }
    dist <- observedRange[2L] - observedRange[1L]

    observedRange[1L] <- min(observedRange[1L], gridLines[1L] - 0.1 * dist)
    observedRange[2L] <- max(observedRange[2L], gridLines[length(gridLines)] + 0.1 * dist)

    leftBreaks <- jaspGraphs::getPrettyAxisBreaks(observedRange)
    leftLimits <- range(leftBreaks)

    rightAxis <- ggplot2::waiver()
    if (add_additional_info) {
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

    y_breaks_per_scale <- ggplot2::scale_y_continuous(breaks = leftBreaks, limits = range(leftBreaks),
                                minor_breaks = gridLines,
                                sec.axis = rightAxis)

  } else {
    y_breaks_per_scale <- tapply(tb, tb$metric, \(x) {

      # x <- tb[tb$metric == tb$metric[1L], , drop = FALSE]
      observedRange <- range(x$lower, x$upper, na.rm = TRUE)
      if (!all(is.finite(observedRange))) {
        observedRange <- c(0, 1)
      }
      dist <- observedRange[2L] - observedRange[1L]

      observedRange[1L] <- min(observedRange[1L], gridLines[1L] - 0.1 * dist)
      observedRange[2L] <- max(observedRange[2L], gridLines[length(gridLines)] + 0.1 * dist)

      if (custom) {
        observedRange[1L] <- max(observedRange[1L], y_limits[1L])
        observedRange[2L] <- min(observedRange[2L], y_limits[2L])
      }

      leftBreaks <- jaspGraphs::getPrettyAxisBreaks(observedRange)
      leftLimits <- range(leftBreaks)

      rightAxis <- ggplot2::waiver()
      if (add_additional_info) {
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
  }

  ribbon <- NULL
  if (has_ci)
    ribbon <- ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), alpha = 0.3)

  extraTheme <- gridLinesLayer <- NULL
  sides <- "bl"
  if (add_additional_info) {
    # there are 11 ticks, the outermost we hide (NA) because one of their bounds is infinite
    # the inner ticks alternate between black and NA, so there is a tick at the grid lines
    # but no tick at the criteria text (which is secretly an axis tick label).
    rightTickColors <- c(NA, rep(c(NA, "black"), length.out = 9), NA)
    extraTheme <- ggplot2::theme(axis.ticks.y.right = ggplot2::element_line(colour = rightTickColors))
    sides      <- "blr"
    # I tried using minor.breaks for this, but these are not drawn properly with facet_grid and facetted_pos_scales
    gridLinesLayer <- ggplot2::geom_hline(
      data = data.frame(yintercept = gridLines),
      ggplot2::aes(yintercept = .data$yintercept),
      # show.legend = FALSE,
      linewidth = .5, color = "lightgray", linetype = "dashed"
    )

  }

  scale_x <- scale_facet <- facet <- NULL
  noMetrics <- nrow(estimates)
  if (noMetrics == 1L || single_panel) {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(tb$n)
    xLimits <- range(tb$n)
    scale_x <- ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits)
    scale_facet <- y_breaks_per_scale
  } else {
    scales <- switch(axes,
                     "automatic" = "free_y",
                     "fixed"     = "fixed",
                     "free"      = "free_y",
                     "custom"    = "fixed",
                     stop("Unknown axes option.")
    )
    if (axes == "custom") {
      if (!is.null(axes_custom[["xmin"]]) && !is.null(axes_custom[["xmax"]])) {
        xbreaks <- jaspGraphs::getPrettyAxisBreaks(c(axes_custom[["xmin"]], axes_custom[["xmax"]]))
        scale_x <- ggplot2::scale_x_continuous(limits = sort(c(axes_custom[["xmin"]], axes_custom[["xmax"]])))
      }
      if (!is.null(axes_custom[["ymin"]]) && !is.null(axes_custom[["ymax"]])) {
        ybreaks <- jaspGraphs::getPrettyAxisBreaks(c(axes_custom[["ymin"]], axes_custom[["ymax"]]))
        leftLimits <- sort(c(axes_custom[["ymin"]], axes_custom[["ymax"]]))
        rightAxis <- ggplot2::waiver()
        if (add_additional_info) {
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
        scale_facet <- ggplot2::scale_y_continuous(breaks = ybreaks, limits = leftLimits,
                                                   minor_breaks = gridLines, sec.axis = rightAxis)
      }
    } else if (axes == "automatic" || axes == "free") {
      scale_facet <- ggh4x::facetted_pos_scales(y = y_breaks_per_scale)
    }
    facet <- ggplot2::facet_wrap(~metric, scales = scales)
  }

  ggplot2::ggplot(tb, ggplot2::aes(x = .data$n, y = .data$mean, group = .data$metric,
                                   color = .data$metric, fill = .data$metric)) +
    gridLinesLayer +
    ribbon +
    ggplot2::geom_line(linewidth = 1) +
    facet + scale_facet + scale_x +
    ggplot2::labs(
      x     = gettext("Number of observations"),
      y     = y_title,
      color = gettext("Metric"),
      fill  = gettext("Metric")
    ) +
    jaspGraphs::geom_rangeframe(sides = sides) +
    jaspGraphs::themeJaspRaw(legend.position = if (single_panel) "right" else "none") +
    extraTheme

}

# Additional plot functions ----
.bpcsPlotPredictive <- function(jaspResults, dataset, options, fit, position, base = c("posteriorPredictiveDistributionPlot", "priorPredictiveDistributionPlot")) {

  base <- match.arg(base)
  isPrior <- base == "priorPredictiveDistributionPlot"

  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  plot <- createJaspPlot(
    title = if (isPrior) gettext("Prior predictive distribution") else gettext("Posterior Predictive Distribution"),
    width = 400,
    height = 400,
    position = position,
    dependencies = c(
    .bpcsDefaultDeps(),
    base,
    paste0(base, "IndividualPointEstimate"),
    paste0(base, "IndividualPointEstimateType"),
    paste0(base, "IndividualCi"),
    paste0(base, "IndividualCiType"),
    paste0(base, "IndividualCiMass"),
    paste0(base, "IndividualCiLower"),
    paste0(base, "IndividualCiUpper")
  ))

  jaspResults[[base]] <- plot

  if (!.bpcsIsReady(options) || is.null(fit) || jaspResults$getError()) return()

  tryCatch({
    rawfit <- fit$rawfit
    if (identical(rawfit$method, "integration")) {
      if (inherits(rawfit$prior_resolved, "PriorConjugate")) {
        # based on Murphy, K. P. (2007). Conjugate Bayesian analysis of the Gaussian distribution. def, 1(2σ2), 16.
        # TODO: since we have access to the distribution we could avoid sampling and plot the density directly
        prior <- rawfit$prior_resolved
        state <- rawfit$integration_result$cached_state
        post  <- qc:::.nig_posterior(prior, state$n, state$x_bar, state$sse)
        df    <- 2 * post$alpha_n
        scale <- sqrt(post$beta_n * (1 + 1 / post$k_n) / post$alpha_n)
        predictiveSamples <- post$mu_n + scale * stats::rt(5000, df)
      } else {
        rawfit <- qc::bpc(
          x            = if (ncol(dataset) > 0L) dataset[[1L]] else NULL,
          method       = "mcmc",
          distribution = rawfit$distribution %||% "normal",
          prior        = rawfit$prior,
          LSL          = options[["lowerSpecificationLimitValue"]],
          USL          = options[["upperSpecificationLimitValue"]],
          target       = options[["targetValue"]],
          chains       = 1, warmup = 1000, iter = 5000, silent = TRUE, seed = 1,
          sample_priors = isPrior
        )
        raw_samples       <- qc:::extract_samples(rawfit, bootstrap = FALSE)
        samples           <- qc:::samples_to_mu_and_sigma(raw_samples)
        predictiveSamples <- qc:::samples_to_posterior_predictives(samples)
      }
    } else {
      raw_samples       <- qc:::extract_samples(rawfit, bootstrap = FALSE)
      samples           <- qc:::samples_to_mu_and_sigma(raw_samples)
      predictiveSamples <- qc:::samples_to_posterior_predictives(samples)
    }

    plt <- jaspGraphs::jaspHistogram(
      predictiveSamples,
      xName = if (isPrior) gettext("Prior predictive") else gettext("Posterior predictive"),
      density = TRUE
    )

    # Calculate density for positioning elements above histogram
    dens <- stats::density(predictiveSamples)
    maxDensity <- max(dens$y)

    # Add point estimate if requested
    if (options[[paste0(base, "IndividualPointEstimate")]]) {
      pointEstimateType <- options[[paste0(base, "IndividualPointEstimateType")]]
      pointEstimate <- switch(pointEstimateType,
        "mean"   = mean(predictiveSamples),
        "median" = stats::median(predictiveSamples),
        "mode"   = dens$x[which.max(dens$y)]
      )
      plt <- plt + ggplot2::geom_point(
        data = data.frame(x = pointEstimate, y = 0),
        ggplot2::aes(x = .data$x, y = .data$y),
        size = 3,
        inherit.aes = FALSE
      )
    }

    # Add CI if requested
    if (options[[paste0(base, "IndividualCi")]]) {
      ciType <- options[[paste0(base, "IndividualCiType")]]

      ciInterval <- if (ciType == "custom") {
        c(options[[paste0(base, "IndividualCiLower")]],
          options[[paste0(base, "IndividualCiUpper")]])
      } else {
        ciMass <- options[[paste0(base, "IndividualCiMass")]] / 100
        if (ciType == "central") {
          stats::quantile(predictiveSamples, probs = c((1 - ciMass) / 2, (1 + ciMass) / 2))
        } else if (ciType == "HPD") {
          # For HPD, we need HDInterval package or implement it
          if (requireNamespace("HDInterval", quietly = TRUE)) {
            HDInterval::hdi(predictiveSamples, credMass = ciMass)
          } else {
            # Fallback to central interval
            stats::quantile(predictiveSamples, probs = c((1 - ciMass) / 2, (1 + ciMass) / 2))
          }
        }
      }

      # Position errorbar above the histogram
      yPosition <- maxDensity * 1.1
      plt <- plt + ggplot2::geom_errorbarh(
        data = data.frame(x = mean(ciInterval), xmin = ciInterval[1], xmax = ciInterval[2], y = yPosition),
        ggplot2::aes(x = .data$x, xmin = .data$xmin, xmax = .data$xmax, y = .data$y),
        height = maxDensity * 0.05,
        linewidth = 0.75,
        inherit.aes = FALSE
      )
    }

    plot$plotObject <- plt
  }, error = function(e) {
    plot$setError(
      if (isPrior) gettextf("Unexpected error in prior predictive distribution plot: %s", e$message)
      else gettextf("Unexpected error in posterior predictive distribution plot: %s", e$message)
    )
  })
}
