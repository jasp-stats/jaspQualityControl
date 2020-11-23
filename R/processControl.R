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

  # Make the Process ID and clean the dataset
  variables <- unlist(options$variables)
  ID <- options$ProcessID
  makeID <- ID != ""
  numberMissing <- 0

  if (is.null(dataset)) {
    if (makeID) {
      dataset         <- .readDataSetToEnd(columns.as.numeric = variables, columns.as.factor = ID)
      dataset.factors <- .readDataSetToEnd(columns = variables, columns.as.factor=ID)
    } else {
      dataset         <- .readDataSetToEnd(columns.as.numeric = variables)
      dataset.factors <- .readDataSetToEnd(columns = variables)
    }
  }
  # Remove missing values
  if (makeID & length(variables) > 0) {
    processID <- dataset[[.v(ID)]]
    dataset <- dataset[!is.na(processID), ]
    dataset.factors <- dataset.factors[!is.na(processID), ]
    processID <- na.omit(processID)
    dataset <- na.omit(dataset)
  } else {dataset <- na.omit(dataset)
  }

  #X bar R Chart
  if(options$XbarRchart && is.null(jaspResults[["XbarRchart"]])){

    jaspResults[["XbarPlot"]] <- createJaspPlot(title = "X bar chart", width = 1100, height = 400)
    jaspResults[["XbarPlot"]]$dependOn(c("XbarRchart"))
    jaspResults[["XbarPlot"]]$position <- 11
    XbarPlot <- jaspResults[["XbarPlot"]]
    XbarPlot$plotObject <- .XbarchartNoId(dataset = dataset, options = options)

    jaspResults[["RPlot"]] <- createJaspPlot(title = "R chart", width = 1100, height= 400)
    jaspResults[["RPlot"]]$dependOn(c("XbarRchart"))
    jaspResults[["RPlot"]]$position <- 11
    RPlot<- jaspResults[["RPlot"]]
    RPlot$plotObject <- .RchartNoId(dataset = dataset, options = options)
  }

  #X bar S Chart
  if(options$Xbarschart && is.null(jaspResults[["Xbarschart"]])){

    jaspResults[["XbarPlot"]] <- createJaspPlot(title = "X bar chart", width = 1100, height = 400)
    jaspResults[["XbarPlot"]]$dependOn(c("Xbarschart"))
    jaspResults[["XbarPlot"]]$position <- 11
    XbarPlot <- jaspResults[["XbarPlot"]]
    XbarPlot$plotObject <- .XbarchartNoId(dataset = dataset, options = options)

    jaspResults[["SPlot"]] <- createJaspPlot(title = "S chart", width = 1100, height= 400)
    jaspResults[["SPlot"]]$dependOn(c("Xbarschart"))
    jaspResults[["SPlot"]]$position <- 11
    SPlot<- jaspResults[["SPlot"]]
    SPlot$plotObject <- .SchartNoId(dataset = dataset, options = options)
  }
}

.SchartNoId <- function(dataset, options) {

    ready <- (length(options$variables) > 1)
    if (!ready)
      return()

    data1 <- dataset
    Stdv <- apply(data1, 1, function(x) sd(x))
    subgroups <- 1:length(Stdv)
    data2 <- data.frame(subgroups = subgroups, Stdv = Stdv)
    sixsigma <- qcc::qcc(data1, type ='S', plot=FALSE)
    center <- sixsigma$center
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
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL-10, UCL+10))
    yLimits <- range(yBreaks)
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(subgroups))

    p <- ggplot2::ggplot(data2, ggplot2::aes(x = subgroups, y = Stdv)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 2, col = ifelse(data2$Stdv > UCL | data2$Stdv < LCL, 2, 1)) +
      ggplot2::geom_hline(yintercept =  center, color = 'black') +
      ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
      ggplot2::scale_y_continuous(name = "Subgroup Standard Deviation" ,limits = yLimits, breaks = yBreaks) +
      ggplot2::scale_x_continuous(name = 'Subgroup', breaks = xBreaks, limits = c(1, length(subgroups) + 1.5)) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()

    return(p)
  }
