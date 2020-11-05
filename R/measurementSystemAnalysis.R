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

measurementSystemAnalysis <- function(jaspResults, dataset, options, ...){

  measurements <- unlist(options$measurements)
  parts <- unlist(options$parts)
  operators <- unlist(options$operators)
  #  splitParts<- parts != ""

  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(all.columns = T)
    #dataset.factors <- .readDataSetToEnd(columns = measurements, columns.as.factor = parts)
  }


  # r and R table
  if (options[["rangeRr"]]) {
    if(is.null(jaspResults[["rAndR"]])) {
      jaspResults[["rAndR"]] <- createJaspContainer(gettext("r & R Table"))
      jaspResults[["rAndR"]]$position <- 1
    }

    jaspResults[["IsoPlot"]] <- .rAndRtable(dataset = dataset, measurements = measurements, parts = parts, options =  options)

  }


  # Iso Plot
  if (options[["IsoPlot"]]) {
    if(is.null(jaspResults[["IsoPlot"]])) {
      jaspResults[["IsoPlot"]] <- createJaspContainer(gettext("Iso Plot"))
      jaspResults[["IsoPlot"]]$position <- 2
    }

    jaspResults[["IsoPlot"]] <- .IsoPlot(dataset = dataset, measurements = measurements, parts = parts, options =  options)

  }


  # Scatter Plot Operators
  if (options[["rangeScatterPlotOperators"]]) {
    if(is.null(jaspResults[["ScatterOperators"]])) {
      jaspResults[["ScatterOperators"]] <- createJaspContainer(gettext("Scatterplot Operators"))
      jaspResults[["ScatterOperators"]]$position <- 3
    }

    jaspResults[["ScatterOperators"]] <- .ScatterPlotOperators(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)

  }


  # Scatter Plot Operators vs Parts
  if (options[["rangeScatterPlotOperatorParts"]]) {
    if(is.null(jaspResults[["ScatterOperatorParts"]])) {
      jaspResults[["ScatterOperatorParts"]] <- createJaspContainer(gettext("Scatterplot Operators vs Parts"))
      jaspResults[["ScatterOperatorParts"]]$position <- 4
    }

    jaspResults[["ScatterOperatorParts"]] <- .ScatterPlotOperatorParts(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)

  }
  return()
}

.IsoPlot <- function(dataset, measurements, parts, options){

  #restructure data

  measurement1 <- numeric()
  measurement2 <- numeric()

  for (i in unique(dataset[,parts])){
    d <- subset(dataset[measurements], dataset[parts] == i)
    measurement1 <- c(measurement1, d[1,])
    measurement2 <- c(measurement2, d[2,])
  }

  d <- data.frame("Measurement1" = measurement1, "Measurement2" = measurement2)

  #halfCirc <-  .circleFun(c(0,7.625), 9.8, start=1.5, end=2.5)

  IsoPlot <- createJaspPlot(title = "Iso Plot")

  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = Measurement1, y = Measurement2)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::geom_abline(intercept = 10, slope = 1) +
    ggplot2::geom_abline(intercept = -10, slope = 1)
  # ggplot2::geom_path(data = halfCirc, mapping = ggplot2::aes(x = x, y = y))

  p <- jaspGraphs::themeJasp(p)



  IsoPlot$plotObject <- p

  IsoTable <- createJaspTable(gettextf("Isoplot Calculations"))

  return(IsoPlot)
}


.ScatterPlotOperators <- function(dataset, measurements, parts, operators, options){

  byOperators <- split(dataset, dataset[operators])

  d <- data.frame("Operator1" = unlist(byOperators[[1]][measurements]),  # Assumes that parts are ordered!
                  "Operator2" = unlist(byOperators[[2]][measurements]))


  plot <- createJaspPlot(title = "Scatterplot of Operator A vs Operator B")

  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = Operator2, y = Operator1)) +
    ggplot2::geom_point()

  if (options[["rangeScatterPlotFitLine"]])
    p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE)

  if (options[["rangeScatterPlotOriginLine"]])
    p <- p + ggplot2::geom_abline(col = "gray", linetype = "dashed")

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return(plot)
}


.ScatterPlotOperatorParts <- function(dataset, measurements, parts, operators, options){

  byOperators <- split(dataset, dataset[operators])

  d1 <- data.frame("Parts" = unlist(byOperators[[1]][parts]),
                   "Measurement" = unlist(byOperators[[1]][measurements]))

  d2 <- data.frame("Parts" = unlist(byOperators[[2]][parts]),
                   "Measurement" = unlist(byOperators[[2]][measurements]))

  plot <- createJaspPlot(title = "Scatterplot of Operator A, Operator B vs Part")

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = d1, ggplot2::aes(x = Parts, y = Measurement, col = "c1", shape = "s1"),size = 3) +
    ggplot2::geom_point(data = d2, ggplot2::aes(x = Parts, y = Measurement, col = "c2", shape = "s2"), size = 3) +
    ggplot2::scale_color_manual(name = "Operators",
                                breaks = c("c1", "c2"),
                                values = c( "c1" = "black", "c2" = "red"),
                                labels = c("Operator A", "Operator B")) +
    ggplot2::scale_shape_manual(name = "Operators",
                                breaks = c("s1", "s2"),
                                values = c( "s1" = 19, "s2" = 15),
                                labels = c("Operator A", "Operator B"))


  p <- jaspGraphs::themeJasp(p) + ggplot2::theme(legend.position = "right")

  plot$plotObject <- p

  return(plot)
}

.rAndRtable <- function(dataset, measurements, parts, operators, options){

  byOperators <- split(dataset, dataset[operators])

  d1 <- data.frame("Parts" = unlist(byOperators[[1]][parts]),
                   "Measurement" = unlist(byOperators[[1]][measurements]))

  d2 <- data.frame("Parts" = unlist(byOperators[[2]][parts]),
                   "Measurement" = unlist(byOperators[[2]][measurements]))

  Rbar <- sum(abs(d1[["Measurement"]] - d2[["Measurement"]]) / length(d1[["Measurement"]]))

  table <- createJaspTable(gettext("r & R Table"))

  table$addColumnInfo(name="Rbar", title=gettext("R-bar"), type="number")
  table$addColumnInfo(name="d2", title=gettext("d2"), type="number")
  table$addColumnInfo(name="PSD", title=gettext("Process SD"), type="number")
  table$addColumnInfo(name="GRR", title=gettext("GRR"), type="number")
  table$addColumnInfo(name="GRRpercent", title=gettext("%GRR"), type="number")

  table$addRows(data.frame("Rbar"       = Rbar,
                           "d2"         = 2,
                           "PSD"        = 3,
                           "GRR"        = 4,
                           "GRRpercent" = 5))



  return(table)
}

#.circleFun <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2)
#{
#  tt <- seq(start*pi, end*pi, length.out=npoints)
#  data.frame(x = center[1] + diameter / 2 * cos(tt),
#             y = center[2] + diameter / 2 * sin(tt))
#}
