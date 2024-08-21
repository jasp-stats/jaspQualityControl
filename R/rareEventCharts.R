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
  factorVariables <- c(variable, stages)
  ready <- length(variable) == 1



  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.factor = factorVariables)
  }


  # Checking for errors in the dataset
  .hasErrors(dataset, type = c('infinity'),
             infinity.target = c(options$variable),
             exitAnalysisIfErrors = TRUE)


  #
  #  ### TESTING #####
  #  # date and time
  #  timepoints <- c("01.04.2024, 9:11", "04.04.2024, 11:11", "09.04.2024, 3:11", "15.04.2024, 18:11", "30.04.2024, 1:11")
  #  dataset <- list()
  #  dataset[["variable"]] <- timepoints
  #  options <- list()
  #  options[["dataType"]] <- "dataTypeDates"
  #  options[["dataTypeDatesStructure"]] <- "dateTime"
  #  options[["dataTypeDatesFormatDate"]] <- "dmy"
  #  options[["dataTypeDatesFormatTime"]] <- "HM"
  #  # time and date
  #  timepoints <- c("9:11 01.04.2024", "11:11 04.04.2024", "3:11 09.04.2024", " 18:11: 15.04.2024", " 1:11 30.04.2024")
  #  dataset <- list()
  #  dataset[["variable"]] <- timepoints
  #  options <- list()
  #  options[["dataType"]] <- "dataTypeDates"
  #  options[["dataTypeDatesStructure"]] <- "timeDate"
  #  options[["dataTypeDatesFormatDate"]] <- "dmy"
  #  options[["dataTypeDatesFormatTime"]] <- "HM"
  #  # date only
  #  timepoints <- c("01.04.2024", "04.04.2024", "09.04.2024", "15.04.2024", " 30.04.2024")
  #  dataset <- list()
  #  dataset[["variable"]] <- timepoints
  #  options <- list()
  #  options[["dataType"]] <- "dataTypeDates"
  #  options[["dataTypeDatesStructure"]] <- "dateOnly"
  #  options[["dataTypeDatesFormatDate"]] <- "dmy"
  #  options[["dataTypeDatesFormatTime"]] <- "HM"
  #  # time only
  #  timepoints <- c("1:32", "3:24", "5:17", "9:22", "12:21")
  #  dataset <- list()
  #  dataset[["variable"]] <- timepoints
  #  options <- list()
  #  options[["dataType"]] <- "dataTypeDates"
  #  options[["dataTypeDatesStructure"]] <- "timeOnly"
  #  options[["dataTypeDatesFormatDate"]] <- "dmy"
  #  options[["dataTypeDatesFormatTime"]] <- "HM"
  # ###########################

#
#     # reproduce example g chart
#     dataset <- read.csv("c:/Users/Jonee/Desktop/Temporary Files/specialControlCharts/gchart.csv")
#      options <- list()
#      variable <- "Date.of.infection"
#      options[["dataType"]] <- "dataTypeDates"
#      options[["dataTypeDatesStructure"]] <- "dateOnly"
#      options[["dataTypeDatesFormatDate"]] <- "md"



    # # reproduce example t chart
    # dataset <- read.csv("c:/Users/Jonee/Desktop/Temporary Files/specialControlCharts/tchart.csv")
    #  options <- list()
    #  variable <- "Date.and.time.of.needlestick"
    #  options[["dataType"]] <- "dataTypeDates"
    #  options[["dataTypeDatesStructure"]] <- "dateTime"
    #  options[["dataTypeDatesFormatDate"]] <- "md"
    #  options[["dataTypeDatesFormatTime"]] <- "HM"




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
      intervalsMinutes <- as.numeric(timepoints - timepointsLag1)/60
      intervalsHours <- intervalsMinutes/60
      intervalsDays <- intervalsHours/24
    } else if (options[["dataType"]] ==  "dataTypeInterval" && options[["dataTypeIntervalType"]] == "dataTypeIntervalTypeTime") {
      timepoints <- dataset[[variable]]
      timeFormat <- options[["dataTypeIntervalTimeFormat"]]
      timepoints <- lubridate::parse_date_time(timepoints, orders = timeFormat) # returns all data in HMS format
      intervalsMinutes <- as.numeric(timepoints - lubridate::as_datetime("0000-01-01 UTC")) # transform to minutes
      intervalsHours <- intervalsMinutes/60
    }

    # if intervals are all NA, throw error
    if (all(is.na(timepoints))) {
      errorPlot <-  createJaspPlot(title = gettext("Rare event charts"), width = 1200, height = 500)
      errorPlot$setError(gettext("Date/time conversion returned no valid values. Did you select the correct date/time format?"))
      jaspResults[["errorPlot"]] <- errorPlot
      return()
    }

    # Get the interval type, depending on the input type and, if applicable, the calculated intervals
    if (options[["dataType"]] == "dataTypeInterval" && options[["dataTypeIntervalType"]] == "opportunities") {
      intervals <- dataset[[variable]]
      intervalType <- "opportunities"
    } else if (options[["dataType"]] == "dataTypeInterval" && options[["dataTypeIntervalType"]] == "hours") {
      intervals <- dataset[[variable]]
      intervalType <- "hours"
    } else if (options[["dataType"]] == "dataTypeInterval" && options[["dataTypeIntervalType"]] == "days") {
      intervals <- dataset[[variable]]
      intervalType <- "days"
    } else if (options[["dataType"]] == "dataTypeInterval" && options[["dataTypeIntervalType"]] == "time") {
      intervals <- if(all(intervalsHours < 1, na.rm = TRUE) ) intervalsMinutes else intervalsHours
      intervalType <- if(all(intervalsHours < 1, na.rm = TRUE)) "minutes" else "hours"
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

    if (length(stages) == 1) {
      dataset <- data.frame(x1 = intervals, x2 = dataset[[stages]])
      colnames(dataset) <- c(variable, stages)
    } else {
      dataset <- data.frame(x1 = intervals)
      colnames(dataset) <- variable
      stages <- ""
    }
  }

  # G chart
  if (options[["gChart"]] && is.null(jaspResults[["gChart"]])) {
    jaspResults[["gChart"]] <- .gChart(dataset, variable, stages, intervalType, options, ready)
  }

  # T chart
  if (options[["tChart"]] && is.null(jaspResults[["tChart"]])) {
    jaspResults[["tChart"]] <- .tChart(dataset, variable, stages, intervalType, options, ready)
  }
}


.gChart <- function(dataset,
                    variable,
                    stages = NULL,
                    intervalType = c("days", "hours", "minutes", "opportunities"),
                    options, ready) {
  plot <-  createJaspPlot(title = gettext("G chart"), width = 1200, height = 500)
  plot$dependOn(c(""))

  if (!ready)
    return(plot)

  columnsToPass <- c(variable, stages)
  columnsToPass <- columnsToPass[columnsToPass != ""]
  phase2 <- options[["gChartProportionSource"]] == "historical"
  plotObject <- .controlChart(dataset[columnsToPass], plotType = "g", stages = stages, gAndtUnit = intervalType,
                              phase2 = phase2, phase2gChartProportion = options[["gChartHistoricalProportion"]])$plotObject
  plot$plotObject <- plotObject

  return(plot)
}

.tChart <- function(dataset,
                    variable,
                    stages = NULL,
                    intervalType = c("days", "hours", "minutes", "opportunities"),
                    options, ready) {
  plot <-  createJaspPlot(title = gettext("T chart"), width = 1200, height = 500)
  plot$dependOn(c(""))

  if (!ready)
    return(plot)

  columnsToPass <- c(variable, stages)
  columnsToPass <- columnsToPass[columnsToPass != ""]
  phase2 <- options[["tChartDistributionParameterSource"]] == "historical"
  plotObject <- .controlChart(dataset[columnsToPass], plotType = "t", stages = stages, gAndtUnit = intervalType,
                              tChartDistribution = options[["tChartDistribution"]],
                              phase2tChartDistributionShape = options[["tChartHistoricalParametersWeibullShape"]],
                              phase2tChartDistributionScale = options[["tChartHistoricalParametersScale"]],
                              phase2 = phase2)$plotObject
  plot$plotObject <- plotObject

  return(plot)
}
