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
  #  splitParts<- parts != ""

  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric = measurements, columns.as.factor = parts)
    #dataset.factors <- .readDataSetToEnd(columns = measurements, columns.as.factor = parts)
  }



  # Iso Plot
  if (options[["rangeIsoPlot"]]) {
    if(is.null(jaspResults[["IsoPlot"]])) {
      jaspResults[["IsoPlot"]] <- createJaspContainer(gettext("Iso Plot"))
      jaspResults[["scatterPlots"]]$position <- 1
    }

    jaspResults[["IsoPlot"]] <- .IsoPlot(dataset = dataset, measurements = measurements, parts = parts, options =  options)

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


#.circleFun <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2)
#{
#  tt <- seq(start*pi, end*pi, length.out=npoints)
#  data.frame(x = center[1] + diameter / 2 * cos(tt),
#             y = center[2] + diameter / 2 * sin(tt))
#}
