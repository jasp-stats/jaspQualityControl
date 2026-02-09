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



  .bpcsPriorPosteriorTable(jaspResults, options, fit, priorFit, position = 2)
  .bpcsCapabilityPlot(jaspResults, options, fit, priorFit, position = 3)
  .bpcsCapabilityPlot(jaspResults, options, fit, priorFit, position = 4, base = "priorDistributionPlot")

  .bpcsIntervalTable(jaspResults, options, fit, position = 5)
  .bpcsIntervalPlot( jaspResults, options, fit, position = 6)

  .bpcsSequentialPointEstimatePlot(   jaspResults, dataset, options, fit, position = 7)
  .bpcsSequentialIntervalEstimatePlot(jaspResults, dataset, options, fit, position = 8)

  .bpcsPlotPredictive(jaspResults, options, fit,      position = 9, base = "posteriorPredictiveDistributionPlot")
  .bpcsPlotPredictive(jaspResults, options, priorFit, position = 10, base = "priorPredictiveDistributionPlot")

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
      "noIterations", "noWarmup", "noChains",
      # estimation method
      "estimationMethod"
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

.bpcsPlotLayoutDeps <- function(base, hasPrior = TRUE, hasEstimate = TRUE, hasCi = TRUE, hasType = FALSE, hasAxes = TRUE, hasRegions = FALSE, hasLegend = FALSE) {
  c(
    base,
    if (hasEstimate) .bpcsPlotLayoutEstimateDeps(base),
    if (hasCi)       .bpcsPlotLayoutCiDeps(base),
    if (hasType)     .bpcsPlotLayoutTypeDeps(base),
    if (hasAxes)     .bpcsPlotLayoutAxesDeps(base),
    if (hasPrior)    .bpcsPlotLayoutPriorDeps(base),
    if (hasRegions)  .bpcsPlotLayoutRegionsDeps(base),
    if (hasLegend)   .bpcsPlotLayoutLegendDeps(base)
  )
}

.bpcsPlotLayoutEstimateDeps <- function(base) { paste0(base, c("IndividualPointEstimate", "IndividualPointEstimateType")) }
.bpcsPlotLayoutCiDeps       <- function(base) { paste0(base, c("IndividualCi", "IndividualCiType", "IndividualCiMass", "IndividualCiLower", "IndividualCiUpper", "IndividualCiBf")) }
.bpcsPlotLayoutTypeDeps     <- function(base) { paste0(base, c("TypeLower", "TypeUpper")) }
.bpcsPlotLayoutAxesDeps     <- function(base) { paste0(base, c("PanelLayout", "Axes", "custom_x_min", "custom_x_max", "custom_y_min", "custom_y_max")) }
.bpcsPlotLayoutPriorDeps    <- function(base) { paste0(base, "PriorDistribution") }
.bpcsPlotLayoutRegionsDeps  <- function(base) { paste0(base, "ShowRegions") }
.bpcsPlotLayoutLegendDeps   <- function(base) { paste0(base, "ShowLegend") }

.bpcsProcessCriteriaDeps <- function() {
  c(paste0("interval", 1:4, "b"), paste0("interval", 1:4), paste0("intervalLabel", 1:5))
}

.bpcsFormatIntervalBounds <- function(intBounds, intNames) {
  n <- length(intBounds)
  fmt <- function(x) {
    if (is.infinite(x)) {
      if (x < 0) return("\u2212\u221e") else return("\u221e")
    }
    formatC(x, digits = 3, format = "f", drop0trailing = TRUE)
  }

  vapply(seq_len(n - 1L), function(i) {
    lhs <- "("
    rhs <- if (i == n - 1L) ")" else "]"
    sprintf("%s %s%s, %s%s", intNames[i], lhs, fmt(intBounds[i]), fmt(intBounds[i + 1L]), rhs)
  }, character(1L))
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

  method <- options[["estimationMethod"]]
  if (is.null(method))
    method <- "mcmc"
  if (identical(method, "integration") && options[["capabilityStudyType"]] == "tCapabilityAnalysis")
    stop("Integration method is available only for the normal distribution.")

  if (!is.null(jaspResults[[paste0(base, "ResultsObject")]]))
    return(jaspResults[[paste0(base, "ResultsObject")]]$object)

  rawfit <- jaspResults[[paste0(base, "State")]] %setOrRetrieve% (
    qc::bpc(
      dataset[[1L]],
      iter = options[["noIterations"]],
      warmup = options[["noWarmup"]],
      chains = options[["noChains"]],
      silent = TRUE,
      seed = 1, # TODO: use setSeed QML component
      target        = options[["targetValue"]],
      LSL           = options[["lowerSpecificationLimitValue"]],
      USL           = options[["upperSpecificationLimitValue"]],
      prior_mu      = .bpcsMuPriorFromOptions(options),
      prior_sigma   = .bpcsSigmaPriorFromOptions(options),
      prior_nu      = .bpcsTPriorFromOptions(options),
      sample_priors = prior,
      method        = method
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

  jaspResults[[paste0(base, "ResultsObject")]] <- createJaspState(
    resultsObject,
    dependencies = jaspDeps(options = c(.bpcsStateDeps(), "credibleIntervalWidth"))
  )

  return(resultsObject)
}

.bpcsCanSampleFromPriors <- function(options) {
  options$priorSettings != "default"
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
  allMetrics <- c("Cp", "CpU", "CpL", "Cpk", "Cpc", "Cpm")
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

  .bpcsCapabilityTableDivergenceFootnotes(table, resultsObject, selectedMetrics)

}

.bpcsCapabilityTableDivergenceFootnotes <- function(table, resultsObject, selectedMetrics) {

  flags <- qc::get_analytic_flags(resultsObject[["summaryObject"]])
  if (is.null(flags))
    return()

  flags <- flags[flags$metric %in% selectedMetrics, , drop = FALSE]
  if (nrow(flags) == 0L)
    return()

  meanRows <- flags$metric[flags$mean_divergent]
  sdRows   <- flags$metric[flags$sd_divergent]

  if (length(meanRows) > 0L)
    table$addFootnote(
      gettext("The mean is analytically infinite given the prior specification."),
      colNames = "mean", rowNames = meanRows
    )

  if (length(sdRows) > 0L)
    table$addFootnote(
      gettext("The standard deviation is analytically infinite given the prior specification."),
      colNames = "sd", rowNames = sdRows
    )

}

.bpcsIntervalTable <- function(jaspResults, options, fit, position) {

  if (!options[["intervalTable"]])
    return()

  table <- .bpcsIntervalTableMeta(jaspResults, options, position)
  if (!.bpcsIsReady(options) || is.null(fit))
    return()

  selectedMetrics <- .bpcsGetSelectedMetrics(options)
  tryCatch({

    # qc does c(-Inf, interval_probability, Inf)
    interval_probability <- unlist(options[paste0("interval", 1:4)], use.names = FALSE)
    interval_summary <- summary(fit[["rawfit"]], interval_probability = interval_probability)[["interval_summary"]]
    colnames(interval_summary) <- c("metric", paste0("interval", 1:5))
    interval_summary <- subset(interval_summary, metric %in% selectedMetrics)
    table$setData(interval_summary)

  }, error = function(e) {

    table$setError(gettextf("Unexpected error in interval table: %s", extractErrorMessage(e)))

  })

  return()
}

.bpcsIntervalTableMeta <- function(jaspResults, options, position) {

  table <- createJaspTable(title = gettext("Interval Table"), position = position)

  table$addColumnInfo(name = "metric", title = gettext("Capability\nMeasure"), type = "string")

  intervalBounds <- c(-Inf, unlist(options[paste0("interval",      1:4)], use.names = FALSE), Inf)
  intervalNames  <-         unlist(options[paste0("intervalLabel", 1:5)], use.names = FALSE)

  titles <- .bpcsFormatIntervalBounds(intervalBounds, intervalNames)
  for (i in seq_along(titles)) {
    table$addColumnInfo(name = paste0("interval", i), title = titles[i], type = "number")
  }
  table$dependOn(c("intervalTable", .bpcsDefaultDeps(), .bpcsProcessCriteriaDeps()))

  jaspResults[["bpcsIntervalTable"]] <- table
  return(table)
}

.bpcsPriorPosteriorTable <- function(jaspResults, options, fit, priorFit, position) {

  if (!options[["priorPosteriorTable"]])
    return()

  table <- createJaspTable(title = gettext("Prior and Posterior Probabilities"), position = position)
  table$dependOn(c("priorPosteriorTable", .bpcsDefaultDeps(), .bpcsProcessCriteriaDeps()))
  jaspResults[["bpcsPriorPosteriorTable"]] <- table

  if (!.bpcsIsReady(options) || is.null(fit))
    return()

  selectedMetrics <- .bpcsGetSelectedMetrics(options)
  if (length(selectedMetrics) == 0) return()

  hasProperPriors <- .bpcsCanSampleFromPriors(options) && !is.null(priorFit)

  intervalProbs <- unlist(options[paste0("interval", 1:4)], use.names = FALSE)
  intNames  <- unlist(options[paste0("intervalLabel", 1:5)], use.names = FALSE)
  intBounds <- c(-Inf, intervalProbs, Inf)

  tryCatch({

    getProbs <- function(obj) summary(obj[["rawfit"]], interval_probability = intervalProbs)[["interval_summary"]]
    postSum  <- as.data.frame(getProbs(fit))
    postSum  <- postSum[match(selectedMetrics, postSum$metric), , drop = FALSE]

    priorSum <- NULL
    if (hasProperPriors) {
      priorSum <- as.data.frame(getProbs(priorFit))
      priorSum <- priorSum[match(selectedMetrics, priorSum$metric), , drop = FALSE]
    }

    table$addColumnInfo(name = "metric", title = gettext("Measure"), type = "string")

    regionTitles <- .bpcsFormatIntervalBounds(intBounds, intNames)

    for (i in seq_along(intNames)) {
      regionTitle <- regionTitles[i]

      if (hasProperPriors) {
        table$addColumnInfo(name = paste0("prior_", i), title = gettext("Prior"),     type = "number", overtitle = regionTitle)
      }
      table$addColumnInfo(name = paste0("post_", i),  title = gettext("Posterior"), type = "number", overtitle = regionTitle)
      if (hasProperPriors) {
        table$addColumnInfo(name = paste0("bf_", i),    title = gettext("BF"),        type = "number", overtitle = regionTitle)
      }
    }

    df <- data.frame(metric = selectedMetrics)

    for (i in seq_along(intNames)) {
      p_post  <- postSum[[i + 1]]
      df[[paste0("post_", i)]]  <- p_post

      if (hasProperPriors) {
        p_prior <- priorSum[[i + 1]]

        odds_prior <- p_prior / (1 - p_prior)
        odds_post  <- p_post / (1 - p_post)

        odds_prior[p_prior >= 1] <- Inf
        odds_post[p_post >= 1]   <- Inf
        odds_prior[p_prior <= 0] <- 0
        odds_post[p_post <= 0]   <- 0

        bf <- odds_post / odds_prior
        bf[odds_prior == 0 & odds_post == 0] <- 1
        bf[odds_prior == 0 & odds_post > 0]  <- Inf

        df[[paste0("prior_", i)]] <- p_prior
        df[[paste0("bf_", i)]]    <- bf
      }
    }

    table$setData(df)

    if (!hasProperPriors) {
      table$addFootnote(gettext("Prior probabilities and Bayes factors are not available for improper priors. Specify proper priors in the Prior Settings to enable these columns."))
    }

  }, error = function(e) {
    table$setError(gettextf("Unexpected error in prior/posterior table: %s", extractErrorMessage(e)))
  })
}


# Plots ----
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
        .bpcsPlotLayoutDeps(base, hasType = FALSE, hasRegions = TRUE),
        .bpcsProcessCriteriaDeps(),
        "colorScheme"
      )
    )
  )
  jaspResults[[base]] <- jaspPlt

  if (!.bpcsIsReady(options) || (isPost && is.null(fit)))
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

      # Compute colors: grey = single grey, non-grey = jaspGraphs palette
      plotColors <- if (is.null(options[["colorScheme"]]) || identical(options[["colorScheme"]], "grey")) {
        rep("grey50", length(selectedMetrics))
      } else {
        jaspGraphs::JASPcolors(palette = jaspGraphs::getGraphOption("palette"), asFunction = TRUE)(length(selectedMetrics))
      }

      showRegions <- isTRUE(options[[paste0(base, "ShowRegions")]])
      regionCutoffs <- unlist(options[paste0("interval", 1:4)], use.names = FALSE)

      jaspPlt$plotObject <- qc::plot_density(
        summaryObject,
        what = selectedMetrics,
        point_estimate     = if (options[[paste0(base, "IndividualPointEstimate")]]) options[[paste0(base, "IndividualPointEstimateType")]] else "none",
        ci                 = if (!showRegions && options[[paste0(base, "IndividualCi")]]) options[[paste0(base, "IndividualCiType")]] else "none",
        ci_level           = options[[paste0(base, "IndividualCiMass")]],
        ci_custom_left     = options[[paste0(base, "IndividualCiLower")]],
        ci_custom_right    = options[[paste0(base, "IndividualCiUpper")]],
        bf_support         = options[[paste0(base, "IndividualCiBf")]],
        single_panel       = singlePanel,
        axes               = options[[paste0(base, "Axes")]],
        axes_custom        = getCustomAxisLimits(options, base),
        priorSummaryObject = priorSummaryObject,
        textsize           = .35 * jaspGraphs::getGraphOption("fontsize"),
        colorScheme        = plotColors,
        stripTextFontsize  = jaspGraphs::getGraphOption("fontsize"),
        show_regions       = showRegions,
        region_cutoffs     = regionCutoffs
      ) +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw()


    }
  }, error = function(e) {
    jaspPlt$width  <- 400
    jaspPlt$height <- 400
    jaspPlt$setError(
      if (isPost) gettextf("Unexpected error in posterior distribution plot: %s", extractErrorMessage(e))
      else gettextf("Unexpected error in prior distribution plot: %s", extractErrorMessage(e))
    )
  })

}

.bpcsIntervalPlot <- function(jaspResults, options, fit, position) {

  if (!options[["intervalPlot"]] || !is.null(jaspResults[["bpcsIntervalPlot"]]))
    return()

  jaspPlt <- createJaspPlot(
    title = gettext("Interval plot"),
    width  = 400 * 3,
    height = 400 * 2,
    position = position,
    dependencies = jaspDeps(options = c(.bpcsDefaultDeps(), .bpcsProcessCriteriaDeps(), "intervalPlot", "colorScheme"))
  )
  jaspResults[["bpcsIntervalPlot"]] <- jaspPlt

  if (!.bpcsIsReady(options) || is.null(fit))
    return()

  tryCatch({

    selectedMetrics <- .bpcsGetSelectedMetrics(options)
    if (length(selectedMetrics) == 0) {
      NULL
    } else {

      interval_probability <- unlist(options[paste0("interval", 1:4)], use.names = FALSE)
      interval_summary <- summary(fit[["rawfit"]], interval_probability = interval_probability)[["interval_summary"]]
      interval_summary <- interval_summary[interval_summary$metric %in% selectedMetrics, , drop = FALSE]
      intervalBounds <- c(-Inf, interval_probability, Inf)
      intervalNames  <- unlist(options[paste0("intervalLabel", 1:5)], use.names = FALSE)
      colnames(interval_summary)[-1] <- .bpcsFormatIntervalBounds(intervalBounds, intervalNames)
      jaspPlt$plotObject <- .bpcsMakeIntervalPlot(interval_summary, options)

    }
  }, error = function(e) {
    jaspPlt$width  <- 400
    jaspPlt$height <- 400
    jaspPlt$setError(gettextf("Unexpected error in interval plot: %s", extractErrorMessage(e)))
  })

  return()
}

.bpcsMakeIntervalPlot <- function(intervalSummary, options) {
  groups <- colnames(intervalSummary)[-1]
  colorScheme <- options[["colorScheme"]]
  if (is.null(colorScheme))
    colorScheme <- "grey"

  paletteName <- if (identical(colorScheme, "grey")) "gray" else jaspGraphs::getGraphOption("palette")
  legendColorsFun <- jaspGraphs::JASPcolors(paletteName, asFunction = TRUE)
  legendColors <- legendColorsFun(length(groups))
  plts <- lapply(seq_len(nrow(intervalSummary)), function(i) {
    jaspGraphs::plotPieChart(
      unlist(intervalSummary[i, -1]), groups, legendName = NULL, legendColors = legendColors
    ) + ggplot2::ggtitle(intervalSummary$metric[i])
  })
  return(patchwork::wrap_plots(plts) + patchwork::plot_layout(nrow = 2, guides = "collect")
  )
}

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
                          .bpcsPlotLayoutDeps(base, hasPrior = FALSE, hasLegend = TRUE),
                          "sequentialAnalysisPlotAdditionalInfo",
                          "colorScheme"
                        )))
  jaspResults[[base]] <- plt

  if (!.bpcsIsReady(options) || jaspResults$getError()) return()

  sequentialPlotData <- .bpcsGetSequentialAnalysis(jaspResults, dataset, options, fit)

  if (!is.null(sequentialPlotData$error)) {
    plt$setError(sequentialPlotData$error)
  } else {
    tryCatch({
      plt$plotObject <- .bpcsMakeSequentialPlot(sequentialPlotData$data, options, base, colorScheme = options[["colorScheme"]])
      failureCount <- attr(sequentialPlotData$data, "failures")
      if (!is.null(failureCount) && failureCount > 0L) {
        plt$setError(gettextf("%d sub-analyses failed.", failureCount))
      }
    }, error = function(e) {
      plt$setError(gettextf("Unexpected error in sequential analysis point estimate plot: %s", extractErrorMessage(e)))
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
                          .bpcsPlotLayoutDeps(base, hasPrior = FALSE, hasLegend = TRUE),
                          "colorScheme"
                        )))
  jaspResults[[base]] <- plt

  if (!.bpcsIsReady(options) || jaspResults$getError()) return()

  sequentialPlotData <- .bpcsGetSequentialAnalysis(jaspResults, dataset, options, fit)

  if (!is.null(sequentialPlotData$error)) {
    plt$setError(sequentialPlotData$error)
  } else {
    tryCatch({
      plt$plotObject <- .bpcsMakeSequentialPlot(sequentialPlotData$data, options, base, custom = TRUE, colorScheme = options[["colorScheme"]])
      failureCount <- attr(sequentialPlotData$data, "failures")
      if (!is.null(failureCount) && failureCount > 0L) {
        plt$setError(gettextf("%d sub-analyses failed.", failureCount))
      }
    }, error = function(e) {
      plt$setError(gettextf("Unexpected error in sequential analysis interval estimate plot: %s", extractErrorMessage(e)))
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
                      paste0(base2, c("TypeLower", "TypeUpper")),
                      "sequentialAnalysisMaxFailures")
          ))
    )

    return(list(data = sequentialPlotData, error = NULL))

  }, error = function(e) {

    return(list(data = NULL, error = extractErrorMessage(e)))

  })

}

.bpcsComputeSequentialAnalysis <- function(dataset, options, fit) {

  n <- nrow(dataset)
  nfrom <- min(n, 3L) # Gaussian could do 2, but let's not push it
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

  priorMu    <- .bpcsMuPriorFromOptions(options)
  priorSigma <- .bpcsSigmaPriorFromOptions(options)
  priorNu    <- .bpcsTPriorFromOptions(options)

  method <- options[["estimationMethod"]]
  if (is.null(method))
    method <- "mcmc"
  if (identical(method, "integration") && options[["capabilityStudyType"]] == "tCapabilityAnalysis")
    stop("Integration method is available only for the normal distribution.")

  maxFailures <- options[["sequentialAnalysisMaxFailures"]]
  if (is.null(maxFailures)) maxFailures <- 10L
  failureCount <- 0L
  failedIndices <- integer(0L)

  for (i in seq_along(nseq)) {

    result <- tryCatch({
      x_i <- x[1:nseq[i]]
      fit_i <- qc::bpc(
        x_i, chains = 1, warmup = 1000, iter = 5000, silent = TRUE, seed = 1,
        target      = options[["targetValue"]],
        LSL         = options[["lowerSpecificationLimitValue"]],
        USL         = options[["upperSpecificationLimitValue"]],
        prior_mu    = priorMu,
        prior_sigma = priorSigma,
        prior_nu    = priorNu,
        method      = method
      )

      sum_fit_i <- summary(fit_i, interval_probability = customBounds)
      sum_i <- sum_fit_i$summary
      custom_i <- sum_fit_i$interval_summary[, 3, drop = FALSE]
      colnames(custom_i) <- "custom"
      sum_i <- cbind(sum_i, custom_i)

      if (is.null(rownames(estimates)))
        rownames(estimates) <- sum_i$metric

      as.matrix(sum_i[keys])
    }, error = function(e) {
      NULL
    })

    if (is.null(result)) {
      failureCount <- failureCount + 1L
      failedIndices <- c(failedIndices, i)
      if (failureCount > maxFailures) {
        stop(gettextf("Sequential analysis aborted: more than %d sub-analyses failed. Last failure at n = %d.",
                      maxFailures, nseq[i]))
      }
    } else {
      estimates[, , i] <- result
    }

    jaspBase::progressbarTick()
  }

  # Remove failed indices from nseq and estimates
  if (length(failedIndices) > 0L) {
    estimates <- estimates[, , -failedIndices, drop = FALSE]
    nseq <- nseq[-failedIndices]
  }

  attr(estimates, "nseq") <- nseq
  attr(estimates, "failures") <- failureCount

  return(estimates)
}

.bpcsMakeSequentialPlot <- function(estimates, options, base, custom = FALSE, colorScheme = NULL) {

  # this function should move to qc, and these are the arguments that should be passed to the arguments of that function
  single_panel <- options[[paste0(base, "PanelLayout")]] != "multiplePanels"
  axes         <- options[[paste0(base, "Axes")]]
  axes_custom  <- getCustomAxisLimits(options, base)
  show_legend  <- isTRUE(options[[paste0(base, "ShowLegend")]])

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
      gettext("Estimate with 95% credible interval")
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
    # Single panel case
    if (axes == "custom") {
      # Apply custom x axis limits
      if (!is.null(axes_custom[["xmin"]]) && !is.null(axes_custom[["xmax"]])) {
        xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(axes_custom[["xmin"]], axes_custom[["xmax"]]))
        scale_x <- ggplot2::scale_x_continuous(breaks = xBreaks, limits = sort(c(axes_custom[["xmin"]], axes_custom[["xmax"]])))
      } else {
        xBreaks <- jaspGraphs::getPrettyAxisBreaks(tb$n)
        scale_x <- ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(tb$n))
      }
      # Apply custom y axis limits
      if (!is.null(axes_custom[["ymin"]]) && !is.null(axes_custom[["ymax"]])) {
        yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(axes_custom[["ymin"]], axes_custom[["ymax"]]))
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
        scale_facet <- ggplot2::scale_y_continuous(breaks = yBreaks, limits = leftLimits,
                                                   minor_breaks = gridLines, sec.axis = rightAxis)
      } else {
        scale_facet <- y_breaks_per_scale
      }
    } else {
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(tb$n)
      scale_x <- ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(tb$n))
      scale_facet <- y_breaks_per_scale
    }
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
        scale_x <- ggplot2::scale_x_continuous(breaks = xbreaks, limits = sort(c(axes_custom[["xmin"]], axes_custom[["xmax"]])))
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
    ggplot2::scale_color_manual(values = .bpcsPalette(values = unique(tb$metric), colorScheme = colorScheme, single_panel = single_panel)) +
    ggplot2::scale_fill_manual(values = .bpcsPalette(values = unique(tb$metric), colorScheme = colorScheme, single_panel = single_panel)) +
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
    jaspGraphs::themeJaspRaw(legend.position = if (single_panel && show_legend) "right" else "none") +
    ggplot2::theme(strip.text = ggplot2::element_text(size = jaspGraphs::getGraphOption("fontsize"))) +
    extraTheme

}

.bpcsPalette <- function(values, colorScheme = NULL, single_panel) {
  nColors <- length(values)

  # Grey scheme: use single grey for all metrics
  # Non-grey: use jaspGraphs palette
  if (is.null(colorScheme) || identical(colorScheme, "grey")) {
    if (single_panel) {
      # Single panel with grey: use jaspGraphs palette for distinguishability
      jaspGraphs::JASPcolors(palette = jaspGraphs::getGraphOption("palette"), asFunction = TRUE)(nColors)
    } else {
      # Multiple panels with grey: single grey for all
      rep("grey50", nColors)
    }
  } else {
    # Non-grey: use jaspGraphs palette
    jaspGraphs::JASPcolors(palette = jaspGraphs::getGraphOption("palette"), asFunction = TRUE)(nColors)
  }
}

# Additional plot functions ----
.bpcsPlotPredictive <- function(jaspResults, options, fit, position, base = c("posteriorPredictiveDistributionPlot", "priorPredictiveDistributionPlot")) {

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
    raw_samples       <- qc:::extract_samples(fit$rawfit, bootstrap = FALSE)
    samples           <- qc:::samples_to_mu_and_sigma(raw_samples)
    predictiveSamples <- qc:::samples_to_posterior_predictives(samples)

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
      if (isPrior) gettextf("Unexpected error in prior predictive distribution plot: %s", extractErrorMessage(e))
      else gettextf("Unexpected error in posterior predictive distribution plot: %s", extractErrorMessage(e))
    )
  })
}


# TODO: move this to jaspBase
#'@export
extractErrorMessage <- function(error) {
  UseMethod("extractErrorMessage")
}
#'@export
extractErrorMessage.default <- function(error) {
  stop("Do not know what to do with an object of class `",
       paste(class(error), collapse = ","),
       "`; The class of the `error` object should be `try-error` or `character`!",
       domain = NA)
}
#'@export
# method for simpleError (created by tryCatch)
extractErrorMessage.simpleError <- function(error) {
  extractErrorMessage(error$message)
}

# method for try-error (created by try)
#'@export
`extractErrorMessage.try-error` <- function(error) {
  extractErrorMessage(attr(error, "condition"))
}
# method for character strings
#'@export
extractErrorMessage.character <- function(error) {
  split <- strsplit(error, ":")[[1]]
  last <- split[[length(split)]]
  return(trimws(last))
}
