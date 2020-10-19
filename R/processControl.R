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

processControl <- function(jaspResults, dataset, options){

  variables <- unlist(options$variables)

  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric=variables)
    dataset.factors <- .readDataSetToEnd(columns=variables)
  }

.XbarchartNoId <- function(jaspResults, dataset, options) {

  ready <- (length(options$variables) > 1)
  if (!ready)
    return()

  jaspResults[["XbarPlot"]] <- createJaspPlot(title = "X bar chart", width = 1100, height = 400)
  jaspResults[["XbarPlot"]]$dependOn(c("XbarRchart"))
  jaspResults[["XbarPlot"]]$position <- 11
  XbarPlot <- jaspResults[["XbarPlot"]]

  data1 <- na.omit(dataset)
  means <- rowMeans(data1)
  subgroups <- 1:length(means)
  data2 <- data.frame(subgroups = subgroups, means = means)
  sixsigma <- qcc::qcc(data1, type ='xbar', plot=FALSE)
  center <- sixsigma$center
  sd1 <- sixsigma$std.dev
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  dfLabel <- data.frame(
    x = length(subgroups) + 1.2,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("Mean = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
      )
   )
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(LCL, UCL))
  yLimits <- range(yBreaks)
  xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(subgroups))

  p <- ggplot2::ggplot(data2, ggplot2::aes(x = subgroups, y = means)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2, col = ifelse(data2$means > UCL | data2$means < LCL, 2, 1)) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(name = "Subgroup Mean" ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name = 'Subgroup', breaks = xBreaks, limits = c(1, length(subgroups) + 1.5)) +
    JASPgraphs::geom_rangeframe() +
    JASPgraphs::themeJaspRaw()

  XbarPlot$plotObject <- p
}

.Rchart <- function(jaspResults, dataset, options) {

  ready <- (length(options$variables) > 1)
  if (!ready)
    return()

  jaspResults[["RPlot"]] <- createJaspPlot(title = "R chart", width = 1100, height= 400)
  jaspResults[["RPlot"]]$dependOn(c("XbarRchart"))
  jaspResults[["RPlot"]]$position <- 11
  RPlot <- jaspResults[["RPlot"]]

  data1 <- na.omit(dataset)
  range <- apply(data1, 1, function(x) max(x) - min(x))
  subgroups <- 1:length(range)
  data2 <- data.frame(subgroups = subgroups, range = range)
  sixsigma <- qcc::qcc(data1, type= 'R', plot = FALSE)
  center <- sixsigma$center
  UCL <- max(sixsigma$limits)
  LCL <- min(sixsigma$limits)
  dfLabel <- data.frame(
    x = length(subgroups) + 1.2,
    y = c(center, UCL, LCL),
    l = c(
      gettextf("Range = %g", round(center, 3)),
      gettextf("UCL = %g",   round(UCL, 3)),
      gettextf("LCL = %g",   round(LCL, 3))
        )
      )
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(LCL, UCL))
  xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(subgroups))
  yLimits <- range(yBreaks)

  p <- ggplot2::ggplot(data2, ggplot2::aes(x = subgroups, y = range)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2, col = ifelse(data2$range > UCL | data2$range < LCL, 2, 1)) +
    ggplot2::geom_hline(yintercept =  center, color = 'black') +
    ggplot2::geom_hline(yintercept = c(UCL,LCL), color = "red") +
    ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(name = "Subgroup Range" ,limits = yLimits, breaks = yBreaks) +
    ggplot2::scale_x_continuous(name= "Subgroup" ,breaks = xBreaks, limits = c(1,length(subgroups) + 1.5)) +
    JASPgraphs::geom_rangeframe() +
    JASPgraphs::themeJaspRaw()

  RPlot$plotObject <- p
}

#X bar R Chart
  if(options$XbarRchart && is.null(jaspResults[["XbarRchart"]])){
    .XbarchartNoId(jaspResults = jaspResults, dataset = dataset, options = options)
    .Rchart(jaspResults = jaspResults, dataset = dataset, options = options)
  }
}


