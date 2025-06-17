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

# Plots ----
.bpcsCapabilityPlot <- function(jaspResults, options, fit) {

  if (!options[["posteriorDistributionPlot"]])
    return()

  jaspResults[["posteriorDistributionPlot"]] %setOrRetrieve% (
    createJaspPlot(
      title = gettext("Posterior Distribution"),
      plot  = if (.bpcsIsReady(options) && !is.null(fit)) {
        qc::plot_density(fit$summaryObject) +
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
