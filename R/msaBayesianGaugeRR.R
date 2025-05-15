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
msaBayesianGaugeRR <- function(jaspResults, dataset, options, ...) {
  # Reading the data in the correct format
  wideFormat <- options[["dataFormat"]] == "wideFormat"
  if (wideFormat) {
    measurements <- unlist(options[["measurementsWideFormat"]])
    parts <- unlist(options[["partWideFormat"]])
    operators <- unlist(options[["operatorWideFormat"]])
  } else {
    measurements <- unlist(options[["measurementLongFormat"]])
    parts <- unlist(options[["partLongFormat"]])
    operators <- unlist(options[["operatorLongFormat"]])
  }

  #ready statement
  if (wideFormat && !options[["type3"]]) {
    ready <- (length(measurements) > 1 && !identical(operators, "") && !identical(parts, ""))
  } else if (wideFormat && options[["type3"]]) {
    ready <- (length(measurements) > 1 && !identical(parts, ""))
  } else if (!wideFormat && !options[["type3"]]) {
    ready <- (measurements != "" && !identical(operators, "") && !identical(parts, ""))
  }  else if (!wideFormat && options[["type3"]]) {
    ready <- (!identical(measurements, "") && !identical(parts, ""))
  }


  numeric.vars <- measurements
  numeric.vars <- numeric.vars[numeric.vars != ""]
  factor.vars <- c(parts, operators)
  factor.vars <- factor.vars[factor.vars != ""]

  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = numeric.vars, columns.as.factor = factor.vars)
    if (options$type3){
      dataset$operators <- rep(1, nrow(dataset))
      operators <- "operators"
    }
  }

  # Checking for infinity and missingValues
  .hasErrors(dataset, type = c('infinity', 'missingValues'),
             infinity.target = measurements,
             missingValues.target = c(measurements, parts, operators),
             exitAnalysisIfErrors = TRUE)

  #Converting long to wide data
  # if (!wideFormat && ready) {
  #   dataset <- dataset[order(dataset[[operators]]),]
  #   dataset <- dataset[order(dataset[[parts]]),]
  #   nrep <- table(dataset[operators])[[1]]/length(unique(dataset[[parts]]))
  #   index <- rep(paste("V", 1:nrep, sep = ""), nrow(dataset)/nrep)
  #   dataset <- cbind(dataset, data.frame(index = index))
  #   dataset <- tidyr::spread(dataset, index, measurements)
  #   measurements <- unique(index)
  #   dataset <- dataset[,c(operators, parts, measurements)]
  # } else if (ready) {
  #   dataset <- dataset[order(dataset[[parts]]),]
  # }

  if(ready && !options[["type3"]]){
    crossed <- .checkIfCrossed(dataset, operators, parts, measurements)
    if(!crossed){
      plot <- createJaspPlot(title = gettext("Gauge r&R"), width = 700, height = 400)
      jaspResults[["plot"]] <- plot
      plot$setError(gettext("Design is not balanced: not every operator measured every part. Use non-replicable gauge r&R."))
      return()
    }
  }

  # Checking type 3
  Type3 <- c(length(unique(dataset[[operators]])) == 1 || options$type3)

  # Errors #
  # Checking whether type3 is used correctly
  .hasErrors(dataset,
             target = measurements,
             custom = function() {
               if (Type3 && !options$type3)
                 return("This dataset seems to have only a single unique operator. Please use the Type 3 study by checking the box below.")},
             exitAnalysisIfErrors = TRUE)
  # Checking whether the format wide is used correctly
  if (ready)
    .hasErrors(dataset,
               target = measurements,
               custom = function() {
                 dataToBeChecked <- dataset[dataset[[operators]] == dataset[[operators]][1],]
                 partsLevels <- length(levels(dataToBeChecked[[parts]]))
                 partsLength <- length(dataToBeChecked[[parts]])
                 if (wideFormat &&  partsLevels != partsLength && !Type3)
                   return(gettextf("The measurements selected seem to be in a 'Single Column' format as every operator's part is measured %d times.", partsLength/partsLevels))},
               exitAnalysisIfErrors = FALSE)


  saveRDS(options, "/Users/julian/Documents/Jasp files/options.rds")
  saveRDS(dataset, "/Users/julian/Documents/Jasp files/dataset.rds")
  saveRDS(measurements, "/Users/julian/Documents/Jasp files/measurements.rds")
  saveRDS(operators, "/Users/julian/Documents/Jasp files/operators.rds")
  saveRDS(parts, "/Users/julian/Documents/Jasp files/parts.rds")

  # BF table
  .createBFtable(jaspResults, dataset, options, measurements, parts, operators, ready)

}

.createBFtable <- function(jaspResults, dataset, options, measurements, parts, operators, ready) {
  if(!is.null(jaspResults[["BFtable"]])) {
    return()
  }

  BFtable <- createJaspTable(title = gettext("Model Comparison"))
  BFtable$position <- 1
  BFtable$dependOn(c("operatorWideFormat", "operatorLongFormat", "partWideFormat", "partLongFormat", "measurementsWideFormat",
                     "measurementLongFormat"))

  jaspResults[["BFtable"]] <- BFtable

  BFtable$addColumnInfo(name = "modelName", title = gettext("Model"),           type = "string")
  BFtable$addColumnInfo(name = "BF",        title = gettext("BF<sub>01</sub>"), type = "number")

  # return empty if no data is specified
  if(nrow(dataset) == 0) {
    return()
  }

  # set data
  if(ready) { # this could also be sth like if(ncol(dataset) == 3)
    BFtable$setData(.getBFinteraction(dataset, measurements, parts, operators))
    BFtable$addFootnote("The Bayes factor compares the model without the interaction term to the full model.")
  }

  return()
}

.getBFinteraction <- function(dataset, measurements, parts, operators) {
  # create formulae
  formula_int <- as.formula(paste(measurements, "~", parts, "*", operators))
  formula     <- as.formula(paste(measurements, "~", parts, "+", operators))

  # fit BayesFactor objects
  fit_int <- BayesFactor::lmBF(formula_int, whichRandom = c(parts, operators),
                               data = dataset)
  fit     <- BayesFactor::lmBF(formula, whichRandom = c(parts, operators),
                               data = dataset)

  # obtain BF
  bf <- fit / fit_int
  bf <- as.numeric(BayesFactor::extractBF(bf)["bf"])
  bf <- round(bf, 2)

  return(data.frame(modelName = "No interaction",
           BF = bf))
}
