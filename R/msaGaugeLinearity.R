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

msaGaugeLinearity <- function(jaspResults, dataset, options, ...){

  measurements <- unlist(options$measurements)
  parts <- unlist(options$parts)
  standards <- unlist(options$standard)


  ready <- (length(measurements) != 0)

  numeric.vars <- c(measurements, standards)
  numeric.vars <- numeric.vars[numeric.vars != ""]

  factor.vars <- parts

  #if(length(measurements) == 0)
  #  return()

  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = numeric.vars, columns.as.factor = factor.vars)
  }

  .msaCheckErrors(dataset, options)



  # Linearity and Bias Analysis

  if(is.null(jaspResults[["LB"]])) {
    jaspResults[["LB"]] <- createJaspContainer(gettext("Linearity and Bias"))
    jaspResults[["LB"]]$position <- 1
  }
  jaspResults[["LB"]] <- .linearityAndBias(dataset = dataset, options = options, measurements = measurements, parts = parts, standards = standards)


  return()
}

#dataset <- read.csv("C:/Users/Jonee/Google Drive/SKF Six Sigma/Dataset/MinitabExamples/BearingDiameter(LinearityAndBias).csv")
#colnames(dataset) <- c("Part", "Master", "Response")
#parts <- "Part"
#measurements <- "Response"
#standards <- "Master"

.linearityAndBias <- function(dataset, options, measurements, parts, standards){

  tablesAndGraphs <- createJaspContainer(gettext("Linearity and Bias"))

  table1 <- createJaspTable(title = gettext("Gauge Bias"))
  table1$dependOn(c(""))

  table1$addColumnInfo(name = "part",  title = gettext("Part"), type = "string")
  table1$addColumnInfo(name = "referenceValue",  title = gettext("Reference Value"), type = "number")
  table1$addColumnInfo(name = "observedMean", title = gettext("Observed Mean"), type = "number")
  table1$addColumnInfo(name = "bias",            title = gettext("Bias"), type = "number")
  table1$addColumnInfo(name = "pvalue",            title = gettext("p (t-test of Bias against 0)"), type = "pvalue")

  partValues <- unique(dataset[[parts]])
  df <- data.frame()
  biases <- vector()
  references <- vector()

  for (i in partValues){
    Part <- i
    partData <- subset.data.frame(dataset, dataset[[parts]] == i)
    Ref <- partData[[standards]][1]
    ObservedMean <- mean(partData[[measurements]])
    Bias <-  ObservedMean - Ref
    pvalue <- t.test(partData[[measurements]] - Ref, mu = 0)$p.value
    df <- rbind(df, list(Part = Part, Ref = Ref, ObservedMean = ObservedMean, Bias = Bias, pvalue = pvalue))
    biases <- c(biases, partData[[measurements]] - partData[[standards]][1])
    references <- c(references, partData[[standards]])
  }

  averageBias <- mean(df$Bias)
  averagePvalue <- t.test(biases, mu = 0)$p.value

  table1$setData(list("part" = c(partValues, gettext("Average")),
                      "referenceValue" = df$Ref,
                      "observedMean" = df$ObservedMean,
                      "bias" = c(df$Bias, averageBias),
                      "pvalue" = c(df$pvalue, averagePvalue)))
  df2 <- data.frame(Bias = biases, Ref = references)

  plot <- createJaspPlot(title = gettext("Bias and Linearity Graph"), width = 500, height = 500)

  p <- ggplot2::ggplot() + ggplot2::geom_hline(yintercept = 0, lty = 2, color = "grey") +
    ggplot2::geom_smooth(data = df2, mapping = ggplot2::aes(x = Ref, y = Bias), method = "lm", color = "red") +
    jaspGraphs::geom_point(data = df2, mapping = ggplot2::aes(x = Ref, y = Bias), fill = "blue",size = 2) +
    jaspGraphs::geom_point(data = df, mapping = ggplot2::aes(x = Ref, y = Bias), fill = "red",size = 4)


  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  tablesAndGraphs[["table1"]] <- table1
  tablesAndGraphs[["plot"]] <- plot


  return(tablesAndGraphs)

}

.msaCheckErrors <- function(dataset, options) {

  #if (options[["gaugeScatterPlotOperators"]]){
  #  .hasErrors(dataset = dataset, type = "factorLevels",
  #             factorLevels.target  = options$operators, factorLevels.amount  = "> 2",
  #             exitAnalysisIfErrors = TRUE)
  #}

}
