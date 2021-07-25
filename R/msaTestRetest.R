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

msaTestRetest <- function(jaspResults, dataset, options, ...) {


  if (options[["testRetestDataFormat"]] == "testRetestWideFormat"){
    measurements <- unlist(options$measurements)
  }else{
    measurements <- unlist(options$measurementsLong)
  }
  parts <- unlist(options$parts)
  operators <- unlist(options$operators)

  numeric.vars <- measurements
  numeric.vars <- numeric.vars[numeric.vars != ""]
  factor.vars <- c(parts, operators)
  factor.vars <- factor.vars[factor.vars != ""]

  if (options[["testRetestDataFormat"]] == "testRetestWideFormat"){
    ready <- (length(measurements) != 0 && operators != "" && parts != "")
  }else{
    ready <- (measurements != "" && operators != "" && parts != "")
  }


  readyRangeMethod <- length(measurements) == 2

  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = numeric.vars, columns.as.factor = factor.vars,
                                         exclude.na.listwise = c(numeric.vars, factor.vars))
  }

  if (options[["testRetestDataFormat"]] == "testRetestLongFormat" && ready){
    dataset <- dataset[order(dataset[operators]),]
    nrep <- table(dataset[operators])[[1]]/length(unique(dataset[[parts]]))
    index <- rep(paste("V", 1:nrep, sep = ""), nrow(dataset)/nrep)
    dataset <- cbind(dataset, data.frame(index = index))
    dataset <- tidyr::spread(dataset, index, measurements)
    measurements <- unique(index)
    dataset <- dataset[,c(operators, parts, measurements)]
  }


  .msaCheckErrors(dataset, options)

  # Range Method

    # Range Method r and R table
    if (options[["rangeRr"]]) {
      .rAndRtableRange(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, jaspResults, ready = readyRangeMethod)
    }

    # Scatter Plot Operators vs Parts
    if (options[["rangeScatterPlotOperatorParts"]]) {
      if (is.null(jaspResults[["ScatterOperatorParts"]])) {
        jaspResults[["ScatterOperatorParts"]] <- createJaspContainer(gettext("Scatterplot Operators vs Parts"))
        jaspResults[["ScatterOperatorParts"]]$position <- 9
      }
      jaspResults[["ScatterOperatorParts"]] <- .ScatterPlotOperatorParts(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, ready = readyRangeMethod)
    }

    # Rchart Range method
    if (options[["rangeRchart"]]) {
      if (is.null(jaspResults[["rangeRchart"]])) {
        jaspResults[["rangeRchart"]] <- createJaspContainer(gettext("Range Method R Chart"))
        jaspResults[["rangeRchart"]]$position <- 11
      }
      plot <- createJaspPlot(title = gettext("Range Chart by Operator"), width = 600, height = 300)
      plot$dependOn(c("rangeRchart"))
      p <- .RchartNoId(dataset = dataset[measurements], options = options, warningLimits = FALSE)
      plot$plotObject <- p
      jaspResults[["rangeRchart"]] <- plot
    }


  return()
}





.ScatterPlotOperatorParts <- function(dataset, measurements, parts, operators, options, ready) {


  plot <- createJaspPlot(title = gettext("Scatterplot of Operator A, Operator B vs Part"), width = 500, height = 320)
  plot$dependOn(c("rangeScatterPlotOperatorParts", "gaugeRRmethod"))

  if (ready) {
    partIndex <- 1:length(dataset[[measurements[1]]])
    dataset <- cbind(dataset, Parts = factor(partIndex, partIndex))

    p <- ggplot2::ggplot() +
      jaspGraphs::geom_point(data = dataset, ggplot2::aes_string(x = "Parts", y = measurements[1]), fill = "red", size = 4) +
      jaspGraphs::geom_point(data = dataset, ggplot2::aes_string(x = "Parts", y = measurements[2]), fill = "green", size = 4) +
      ggplot2::ylab("Measurements")

    p <- jaspGraphs::themeJasp(p) + ggplot2::theme(legend.position = "right")

    plot$plotObject <- p
  }

  return(plot)
}

.rAndRtableRange <- function(dataset, measurements, parts, operators, options, jaspResults, ready) {

  table <- createJaspTable(title = gettext("r&R Table"))
  table$position <- 2
  table$dependOn(c("rangeRr", "gaugeRRmethod"))

  table$addColumnInfo(name = "Rbar", title = gettext("R-bar"), type = "number")
  table$addColumnInfo(name = "d2", title = gettext("d2"), type = "number")
  table$addColumnInfo(name = "PSD", title = gettext("Process Std. Dev."), type = "number")
  table$addColumnInfo(name = "GRR", title = gettext("GRR"), type = "number")
  table$addColumnInfo(name = "GRRpercent", title = gettext("%GRR"), type = "number")

  jaspResults[["rAndR2"]] <- table

  if (ready) {

    if (nrow(dataset[measurements]) > 20)
      table$setError("Range Method available for max. 20 unique Parts")

    n <- length(dataset[[measurements[1]]])
    Rbar <- sum(abs(dataset[measurements[1]] - dataset[measurements[2]]) / n)
    d2 <- .d2Value(n)
    SD <- options$rangePSD
    GRR <- Rbar/d2
    GRRpercent <- GRR/SD*100


    table$addRows(list(      "Rbar"       = Rbar,
                             "d2"         = d2,
                             "PSD"        = SD,
                             "GRR"        = GRR,
                             "GRRpercent" = GRRpercent))
  }
}
