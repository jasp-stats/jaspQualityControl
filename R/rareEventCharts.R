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
rareEventCharts <- function(jaspResults, dataset, options) {
  # reading variables in from the GUI
  variable <- unlist(options[["variable"]])
  stages <- unlist(options[["stage"]])
  variable <- variable[variable != ""]
  stages <- stages[stages != ""]

  if (options[["dataType"]] == "dataTypeDates" || options[["dataType"]] ==  "dataTypeInterval" && options[["dataTypeIntervalType"]] == "dataTypeIntervalTypeTime") {
    numericVariables <- NULL
    factorVariables <- c(variable, stages)
  } else {
    numericVariables <- variable
    factorVariables <- stages
  }

  ready <- length(variable) == 1

  dataset <- .readDataSetToEnd(columns.as.factor = factorVariables, columns.as.numeric = numericVariables)


  # remove NA
  dataset <- na.omit(dataset)


  # Checking for errors in the dataset
  .hasErrors(dataset, type = c('infinity'),
             infinity.target = c(options$variable),
             exitAnalysisIfErrors = TRUE)

  if (ready) {
    # If variable is date/time transform into day, hour and minute intervals
    if (options[["dataType"]] == "dataTypeDates") {
      timepoints <- dataset[[variable]]
      timeStructure <- options[["dataTypeDatesStructure"]]
      timeFormat <- switch(timeStructure,
                           "dateTime" = paste(options[["dataTypeDatesFormatDate"]], options[["dataTypeDatesFormatTime"]]),
                           "timeDate" = paste(options[["dataTypeDatesFormatTime"]], options[["dataTypeDatesFormatDate"]]),
                           "dateOnly" = options[["dataTypeDatesFormatDate"]],
                           "timeOnly" = options[["dataTypeDatesFormatTime"]])
      timepoints <- lubridate::parse_date_time(timepoints, orders = timeFormat) # returns all data in HMS format
      timepointsLag1 <- c(NA, timepoints[seq(1, length(timepoints) - 1)]) # because of the NA, everything is converted to seconds
      if (length(stages) == 1) { # remove the first value of each stage, as it is a new beginning
        stageSwitchPositions <- which(diff(as.numeric(dataset[[stages]])) != 0) + 1
        timepointsLag1[stageSwitchPositions] <- NA
      }
      intervalsMinutes <- as.numeric(timepoints - timepointsLag1)/60
      intervalsHours <- intervalsMinutes/60
      intervalsDays <- intervalsHours/24
    } else if (options[["dataType"]] ==  "dataTypeInterval" && options[["dataTypeIntervalType"]] == "time") {
      timepoints <- dataset[[variable]]
      timeFormat <- options[["dataTypeIntervalTimeFormat"]]
      timepoints <- lubridate::parse_date_time(timepoints, orders = timeFormat) # returns all data in HMS format
      intervalsMinutes <- as.numeric(timepoints - lubridate::as_datetime("0000-01-01 UTC")) # transform to minutes
      intervalsHours <- intervalsMinutes/60
    }

    # if intervals are all NA, throw error
    if ((options[["dataType"]] == "dataTypeDates" || options[["dataType"]] ==  "dataTypeInterval" && options[["dataTypeIntervalType"]] == "time") &&
        all(is.na(timepoints))) {
      errorPlot <-  createJaspPlot(title = gettext("Rare event charts"), width = 1200, height = 500)
      errorPlot$dependOn(c("variable", "stage", "dataType", "dataTypeDatesStructure", "dataTypeDatesFormatDate",
                           "dataTypeDatesFormatTime", "dataTypeIntervalType", "dataTypeIntervalTimeFormat"))
      errorPlot$setError(gettext("Date/time conversion returned no valid values. Did you select the correct date/time format?"))
      jaspResults[["errorPlot"]] <- errorPlot
      return()
    }

    # Get the interval type, depending on the input type and, if applicable, the calculated intervals
    if (options[["dataType"]] == "dataTypeInterval") {
      if (options[["dataTypeIntervalType"]] == "opportunities") {
        intervals <- as.numeric(dataset[[variable]])
        intervalType <- "opportunities"
      } else if (options[["dataTypeIntervalType"]] == "hours") {
        intervals <- as.numeric(dataset[[variable]])
        intervalType <- "hours"
      } else if (options[["dataTypeIntervalType"]] == "days") {
        intervals <- as.numeric(dataset[[variable]])
        intervalType <- "days"
      } else if (options[["dataTypeIntervalType"]] == "time") {
        intervals <- if (all(intervalsHours < 1, na.rm = TRUE)) intervalsMinutes else intervalsHours
        intervalType <- if (all(intervalsHours < 1, na.rm = TRUE)) "minutes" else "hours"
      }
    } else if (options[["dataType"]] == "dataTypeDates") {
      if (all(intervalsDays < 1, na.rm = TRUE) && all(intervalsHours < 1, na.rm = TRUE)) {
        intervals <- intervalsMinutes
        intervalType <- "minutes"
      } else if (all(intervalsDays < 1, na.rm = TRUE)) {
        intervals <- intervalsHours
        intervalType <- "hours"
      } else {
        intervals <- intervalsDays
        intervalType <- "days"
      }
    }

    # if intervals contains any negative values, throw error
    if (any(intervals < 0, na.rm = TRUE)) {
      errorPlot <-  createJaspPlot(title = gettext("Rare event charts"), width = 1200, height = 500)
      errorPlot$dependOn(c("variable", "stage", "dataType", "dataTypeDatesStructure", "dataTypeDatesFormatDate",
                           "dataTypeDatesFormatTime", "dataTypeIntervalType", "dataTypeIntervalTimeFormat"))
      errorPlot$setError(gettext("Negative values for time/intervals detected, calculation is not possible."))
      jaspResults[["errorPlot"]] <- errorPlot
      return()
    }

    if (length(stages) == 1) {
      dataset <- data.frame(x1 = intervals, x2 = dataset[[stages]])
      colnames(dataset) <- c(variable, stages)
    } else {
      dataset <- data.frame(x1 = intervals)
      colnames(dataset) <- variable
      stages <- ""
    }
  }

  # Create the rule list for the out-of-control signals
  if (ready)
    ruleList <- .getRuleListRareEventCharts(options)


  # G chart
  if (options[["gChart"]]) {
    gChart <- .gChart(dataset, variable, stages, intervalType, ruleList, options, ready)
  }

  # T chart
  if (options[["tChart"]]) {
    tChart <- .tChart(dataset, variable, stages, intervalType, ruleList, options, ready)
  }


  # Report
  if (options[["report"]]) {
    reportPlot <- createJaspPlot(title = gettext("Rare event charts report"), width = 1250, height = 1000)
    jaspResults[["report"]] <- reportPlot
    jaspResults[["report"]]$dependOn(c("variable", "stage", "dataType", "dataTypeDatesStructure", "dataTypeDatesFormatDate",
                                       "dataTypeDatesFormatTime", "dataTypeIntervalType", "dataTypeIntervalTimeFormat",
                                       "gChart", "gChartProportionSource", "gChartHistoricalProportion", "tChart",
                                       "tChartDistribution", "tChartDistributionParameterSource", "tChartHistoricalParametersWeibullShape",
                                       "tChartHistoricalParametersScale", "report", "reportMetaData",
                                       "reportTitle", "reportTitleText", "reportChartName", "reportChartNameText", "reportSubtitle",
                                       "reportSubtitleText", "reportMeasurementName", "reportMeasurementNameText", "reportFootnote",
                                       "reportFootnoteText", "reportLocation", "reportLocationText", "reportDate", "reportDateText",
                                       "reportPerformedBy", "reportPerformedByText", "reportPrintDate", "reportPrintDateText",
                                       .getDependenciesControlChartRules()))

    # Plot meta data
    if (options[["reportTitle"]] ) {
      title <- if (options[["reportTitleText"]] == "") gettext("Rare event charts report") else options[["reportTitleText"]]
    } else {
      title <- ""
    }

    if (options[["reportMetaData"]]) {
      text <- c()
      text <- if (options[["reportChartName"]]) c(text, gettextf("Chart name: %s", options[["reportChartNameText"]])) else text
      text <- if (options[["reportSubtitle"]]) c(text, gettextf("Sub-title: %s", options[["reportSubtitleText"]])) else text
      text <- if (options[["reportMeasurementName"]]) c(text, gettextf("Measurement name: %s", options[["reportMeasurementNameText"]])) else text
      text <- if (options[["reportFootnote"]]) c(text, gettextf("Footnote: %s", options[["reportFootnoteText"]])) else text
      text <- if (options[["reportLocation"]]) c(text, gettextf("Location: %s", options[["reportLocationText"]])) else text
      text <- if (options[["reportDate"]]) c(text, gettextf("Date: %s", options[["reportDateText"]])) else text
      text <- if (options[["reportPerformedBy"]]) c(text, gettextf("Performed by: %s", options[["reportPerformedByText"]])) else text
      text <- if (options[["reportPrintDate"]]) c(text, gettextf("Print date: %s", options[["reportPrintDateText"]])) else text
    } else {
      text <- NULL
    }

    plots <- list()
    if (options[["gChart"]])
      plots[["gChart"]] <- gChart[["plot"]]$plotObject
    if (options[["tChart"]])
      plots[["tChart"]] <- tChart[["plot"]]$plotObject
    reportPlotObject <- .qcReport(text = text, plots = plots, textMaxRows = 8,
                                  reportTitle = title)
    reportPlot$plotObject <- reportPlotObject

    ###
    ### If not report mode
    ###
  } else {
    if (options[["gChart"]]) {
      jaspResults[["gChart"]] <- createJaspContainer(position = 1)
      jaspResults[["gChart"]][["plot"]] <- gChart[["plot"]]
      jaspResults[["gChart"]][["table"]] <- gChart[["table"]]
    }
    if (options[["tChart"]]) {
      jaspResults[["tChart"]] <- createJaspContainer(position = 2)
      jaspResults[["tChart"]][["plot"]] <- tChart[["plot"]]
      jaspResults[["tChart"]][["table"]] <- tChart[["table"]]
    }
  }
}


.gChart <- function(dataset,
                    variable,
                    stages = NULL,
                    intervalType = c("days", "hours", "minutes", "opportunities"),
                    ruleList,
                    options,
                    ready) {
  plot <-  createJaspPlot(title = gettext("G chart"), width = 1200, height = 500)
  plot$dependOn(c("variable", "stage", "dataType", "dataTypeDatesStructure", "dataTypeDatesFormatDate",
                  "dataTypeDatesFormatTime", "dataTypeIntervalType", "dataTypeIntervalTimeFormat",
                  "gChart", "gChartProportionSource", "gChartHistoricalProportion", "report",
                  .getDependenciesControlChartRules()))

  if (!ready)
    return(plot)

  columnsToPass <- c(variable, stages)
  columnsToPass <- columnsToPass[columnsToPass != ""]
  phase2 <- options[["gChartProportionSource"]] == "historical"
  gChart <- .controlChart(dataset[columnsToPass], ruleList = ruleList, plotType = "g", stages = stages,
                          gAndtUnit = intervalType, phase2 = phase2,
                          phase2gChartProportion = options[["gChartHistoricalProportion"]])
  plot$plotObject <- gChart$plotObject
  table <- gChart$table
  return(list("plot" = plot, "table" = table))
}

.tChart <- function(dataset,
                    variable,
                    stages = NULL,
                    intervalType = c("days", "hours", "minutes", "opportunities"),
                    ruleList,
                    options,
                    ready) {
  plot <-  createJaspPlot(title = gettext("T chart"), width = 1200, height = 500)
  plot$dependOn(c("variable", "stage", "dataType", "dataTypeDatesStructure", "dataTypeDatesFormatDate",
                  "dataTypeDatesFormatTime", "dataTypeIntervalType", "dataTypeIntervalTimeFormat", "tChart",
                  "tChartDistribution", "tChartDistributionParameterSource", "tChartHistoricalParametersWeibullShape",
                  "tChartHistoricalParametersScale", "report", .getDependenciesControlChartRules()))

  if (!ready)
    return(plot)

  columnsToPass <- c(variable, stages)
  columnsToPass <- columnsToPass[columnsToPass != ""]
  phase2 <- options[["tChartDistributionParameterSource"]] == "historical"
  tChart <- .controlChart(dataset[columnsToPass], ruleList = ruleList, plotType = "t",
                          stages = stages, gAndtUnit = intervalType,
                          tChartDistribution = options[["tChartDistribution"]],
                          phase2tChartDistributionShape = options[["tChartHistoricalParametersWeibullShape"]],
                          phase2tChartDistributionScale = options[["tChartHistoricalParametersScale"]],
                          phase2 = phase2)
  plot$plotObject <- tChart$plotObject
  table <- tChart$table
  return(list("plot" = plot, "table" = table))
}

.getRuleListRareEventCharts <- function(options) {
  ruleSet <- options[["testSet"]]
  if (ruleSet == "jaspDefault") {
    ruleList <- list("rule1" = list("enabled" = TRUE),
                     "rule2" = NULL,
                     "rule3" = NULL,
                     "rule4" = NULL,
                     "rule5" = NULL,
                     "rule6" = NULL,
                     "rule7" = NULL,
                     "rule8" = NULL,
                     "rule9" = NULL
    )
  } else if (ruleSet == "custom") {
    ruleList <- list("rule1" = list("enabled" = options[["rule1"]]),
                     "rule2" = list("enabled" = options[["rule2"]], "k" = options[["rule2Value"]]),
                     "rule3" = list("enabled" = options[["rule3"]], "k" = options[["rule3Value"]]),
                     "rule4" = NULL,
                     "rule5" = NULL,
                     "rule6" = NULL,
                     "rule7" = NULL,
                     "rule8" = list("enabled" = options[["rule8"]], "k" = options[["rule8Value"]]),
                     "rule9" = list("enabled" = options[["rule9"]], "k" = options[["rule9Value"]])
    )
  }
  return(ruleList)
}
