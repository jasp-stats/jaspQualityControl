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


#'@export
bayesianProcessCapabilityStudies <- function(jaspResults, dataset, options) {

  fit <- .bpcsCapabilityTable(jaspResults, dataset, options)
  .bpcsCapabilityPlot(jaspResults, fit)

}

.bpcsIsReady <- function(options) {
  length(options[["measurementLongFormat"]]) > 0L && options[["measurementLongFormat"]] != ""
}

.bpcsCapabilityTable <- function(jaspResults, dataset, options) {

  if (!is.null(options[["bpcsCapabilityTable"]]))
    return()

  table <- .bpcsCapabilityTableMeta(jaspResults, options)
  if (!.bpcsIsReady(options))
    return()

  fit <- qc::bpc(dataset[[1L]], chains = 1, warmup = 1000, iter = 5000, silent = TRUE, seed = 1)

  .bpcsCapabilityTableFill(table, fit, options)
  return(fit)

}

.bpcsCapabilityTableMeta <- function(jaspResults, options) {

  table <- createJaspTable(title = gettext("Capability Table"))
  table$addColumnInfo(name = "metric",  title = gettext("Capability Measure"), type = "string")
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

.bpcsCapabilityTableFill <- function(table, fit, options) {

  df <- as.data.frame(qc::summarize_capability_metrics(fit, cri_width = options[["credibleIntervalWidth"]]))
  table$setData(df)

}

#'@importFrom jaspBase jaspDeps %setOrRetrieve%

.bpcsCapabilityPlot <- function(jaspResults, fit) {

  if (!options[["posteriorDistributionPlot"]])
    return()

  jaspResults[["posteriorDistributionPlot"]] %setOrRetrieve% (
    createJaspPlot(
      title = gettext("Posterior Distribution"),
      plot  = qc::plot_density(fit) + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw(),
      dependencies = jaspDeps(
        options = c("measurementLongFormat", "posteriorDistributionPlot")
      )
    )
  )
}
