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

}

.bpcsIsReady <- function(options) {
  length(options[["measurementLongFormat"]]) > 0L && options[["measurementLongFormat"]] != "" &&
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

  table <- createJaspTable(title = gettext("Capability Table"))
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
    colnames(interval_summary) <- paste0("interval", 1:6)
    table$setData(interval_summary)

  }, error = function(e) {

    table$setError(gettextf("Unexpected error in interval table: %s", e$message))

  })

  return()
}

.bpcsIntervalTableMeta <- function(jaspResults, options) {

  table <- createJaspTable(title = gettext("Interval Table"))

  table$addColumnInfo(name = "metric",  title = gettext("Capability\nMeasure"), type = "string")

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
        qc::plot_density(fit$summaryObject,
                         what = c("Cp", "CpU", "CpL", "Cpk", "Cpm")) +
          jaspGraphs::geom_rangeframe() +
          jaspGraphs::themeJaspRaw()
      } else {
        NULL
      },
      width  = 320 * 6,
      height = 320,
      dependencies = jaspDeps(
        options = c(.bpcsDefaultDeps(), "posteriorDistributionPlot")
      )
    )
  )
}
