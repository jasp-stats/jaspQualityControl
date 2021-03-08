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

msaType1Gauge <- function(jaspResults, dataset, options, ...){

  measurements <- unlist(options$measurements)
  measurements <- measurements[measurements != ""]

  ready <- (length(measurements) != 0)

  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = measurements, exclude.na.listwise = measurements)
  }


  .msaCheckErrors(dataset, options)

  # Bias Run Chart
  if (options[["biasRun"]]) {
    if(is.null(jaspResults[["biasRun"]])) {
      jaspResults[["biasRun"]] <- createJaspContainer(gettext("Run Chart"))
    }

    jaspResults[["biasRun"]] <- .biasRunChart(dataset = dataset, measurements = measurements, options =  options, ready = ready)
    jaspResults[["biasRun"]]$position <- 1
  }

  # Determine Bias Table
  if (options[["biasTable"]]) {
    if(is.null(jaspResults[["biasTable"]])) {
      jaspResults[["biasTable"]] <- createJaspContainer(gettext("Bias Table"))
    }
    jaspResults[["biasTable"]] <- .biasTable(dataset = dataset, measurements = measurements, options = options, ready = ready)
    jaspResults[["biasTable"]]$position <- 2
  }

  # Determine Bias t-Test
  if (options[["biasTtest"]]) {
    if(is.null(jaspResults[["biasTtest"]])) {
      jaspResults[["biasTtest"]] <- createJaspContainer(gettext("t-Test Bias"))
    }
    jaspResults[["biasTtest"]] <- .biasTtest(dataset = dataset, measurements = measurements, options =  options, ready = ready)
    jaspResults[["biasTtest"]]$position <- 3
  }

  # Determine Bias Histogram
  if (options[["biasHistogram"]]) {
    if(is.null(jaspResults[["biasHistogram"]])) {
      jaspResults[["biasHistogram"]] <- createJaspContainer(gettext("Histogram Bias"))
    }
    jaspResults[["biasHistogram"]] <- .biasHistogram(dataset = dataset, measurements = measurements, options =  options, ready = ready)
    jaspResults[["biasHistogram"]]$position <- 4
  }

  return()
}

.biasTable <- function(dataset, measurements, options, ready){

  biasTables <- createJaspContainer(gettext("Bias and Capability Table"))

  data <- dataset

  tolerance <- options$biasTolerance
  reference <- options$biasReferenceValue

  if (options$BiasStudyVarMultiplierType == "svmSD"){
    studyVarMultiplier <- options$BiasStudyVarMultiplier
  }else{
    percent <- options$BiasStudyVarMultiplier/100
    q <- (1 - percent)/2
    studyVarMultiplier <- abs(2*qnorm(q))
  }

  table1 <- createJaspTable(title = gettext("Basic Statistics"))
  table1$dependOn(c("biasReferenceValue", "biastable", "biasTolerance"))

  table1$addColumnInfo(name = "referenceValue",  title = gettext("Reference value"), type = "number")
  table1$addColumnInfo(name = "observedMean", title = gettext("Mean"), type = "number")
  table1$addColumnInfo(name = "bias",            title = gettext("Bias"), type = "number")
  table1$addColumnInfo(name = "sd",            title = gettext("Std. Deviation (<i>s</i>)"), type = "number")
  table1$addColumnInfo(name = "SV",            title = gettext("Study variation"), type = "number")
  table1$addColumnInfo(name = "tolerance",       title = gettext("Tolerance"), type = "number")
  table1$addColumnInfo(name = "biasPercent",     title = gettext("% Bias"), type = "number")

  table1$addFootnote(gettextf("The study variation is calculated as %.2f * <i>s</i>.", studyVarMultiplier))

  table2 <- createJaspTable(title = gettext("Capability"))
  table2$dependOn(c("biasReferenceValue", "biastable", "biasTolerance"))


  table2$addColumnInfo(name = "Cg",  title = gettext("Cg"), type = "number")
  table2$addColumnInfo(name = "Cgk",  title = gettext("Cgk"), type = "number")
  table2$addColumnInfo(name = "percentRep",  title = gettext("% Var(Repeatability)"), type = "number")
  table2$addColumnInfo(name = "percentRepBias",  title = gettext("% Var(Repeatability and bias)"), type = "number")

  table1$setData(list(      "referenceValue"       = reference,
                            "tolerance"             = tolerance))

  if (ready){

    observedAverage <- mean(dataset[[measurements]])
    bias <- observedAverage - options$biasReferenceValue
    biasPercent <- abs(bias) / options$biasTolerance * 100
    sd <- sd(unlist(data))
    sv <- sd * studyVarMultiplier

    table1$setData(list(      "referenceValue"       = reference,
                              "observedMean"          = observedAverage,
                              "bias"                  = bias,
                              "sd"                    = sd,
                              "SV"                    =  sv,
                              "tolerance"             = tolerance,
                              "biasPercent"           = biasPercent))

    k <- options$biasPercentCG
    cg <- ((k / 100) * tolerance) / sv
    cgk <- (((k / 200) * tolerance) - abs(observedAverage - reference)) / (sv / 2)
    percentRep <-   k/cg
    percentRepBias <- k/cgk


    table2$setData(list(     "Cg"     = cg,
                             "Cgk"    = cgk,
                             "percentRep"  =  percentRep,
                             "percentRepBias"    = percentRepBias))

  }


  biasTables[["Basic"]] <- table1
  biasTables[["Capability"]] <- table2



  return(biasTables)
}

.biasTtest <- function(dataset, measurements, options, ready){

  data <- dataset

  table <- createJaspTable(title = gettext("t-Test of Observed Bias Against 0"))

  table$dependOn(c("biasReferenceValue", "biasTtest", "biasTolerance"))

  table$addColumnInfo(name = "df",              title = gettext("df"), type = "integer")
  table$addColumnInfo(name = "bias",            title = gettext("Bias"), type = "number")
  table$addColumnInfo(name = "lci",             title = gettext("Lower"), type = "number", overtitle = gettext("95% CI for Bias"))
  table$addColumnInfo(name = "uci",             title = gettext("Upper"), type = "number", overtitle = gettext("95% CI for Bias"))
  table$addColumnInfo(name = "t",               title = gettext("<i>t</i>"), type = "number")
  table$addColumnInfo(name = "p",               title = gettext("<i>p<i/>"), type = "pvalue")


  if (ready){
    bias <- data - options$biasReferenceValue
    tTest <- t.test(bias, mu = 0)

    table$addRows(list(      "df"                   = tTest$parameter,
                             "bias"                 = tTest$estimate,
                             "lci"                  = tTest$conf.int[1],
                             "uci"                  = tTest$conf.int[2],
                             "t"                    = tTest$statistic,
                             "p"                    = tTest$p.value))
  }

  return(table)
}

.biasHistogram <- function(dataset, measurements, options, ready){

  if (ready){

    dataForPart <- dataset[[measurements]]

    plot <- createJaspPlot(title = gettext("Bias Histogram"), width = 400, height = 400)

    p <- jaspDescriptives:::.plotMarginal(column = dataForPart, variableName = "Measurement")

    plot$dependOn(c("biasHistogram"))

    plot$plotObject <- p

    return(plot)

  }
}

.biasRunChart <- function(dataset, measurements, options, ready){

  if (ready){

    plot <- createJaspPlot(title = "Run Chart", width = 700, height = 300)

    dataset <- tidyr::gather(dataset, Repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

    index <- 1:length(dataset[["Measurement"]])

    dataset <- cbind(dataset, index = factor(index, index))

    plot$dependOn(c("biasRun"))

    toleranceLines <- c(options$biasReferenceValue + 0.1 * options$biasTolerance, options$biasReferenceValue - 0.1 * options$biasTolerance)
    yLimits <- range(c(dataset[["Measurement"]], toleranceLines))


    p <- ggplot2::ggplot() +
      jaspGraphs::geom_line(data = dataset, mapping = ggplot2::aes(x = index, y = Measurement, group = 1))

    if (options$biasRunDots)
      p <- p + jaspGraphs::geom_point(data = dataset, ggplot2::aes(x = index, y = Measurement))

    p <- p + ggplot2::geom_hline(yintercept = options$biasReferenceValue, data = dataset,
                                 mapping = ggplot2::aes(x = index, y = Measurement), color = "darkgreen") +
      ggplot2::geom_label(data = data.frame(x = max(index), y = options$biasReferenceValue, l = "Ref"),
                          ggplot2::aes(x = x, y = y, label = l), vjust="inward",hjust="inward", color = "darkgreen" ) +
      ggplot2::geom_hline(yintercept = toleranceLines[1], data = dataset,
                          mapping = ggplot2::aes(x = index, y = Measurement), color = "darkred") +
      ggplot2::geom_label(data = data.frame(x = max(index), y = toleranceLines[1], l = "Ref + 0.1 * Tol"),
                          ggplot2::aes(x = x, y = y, label = l), vjust="outward",hjust="inward", color = "darkred" ) +
      ggplot2::geom_hline(yintercept = toleranceLines[2], data = dataset,
                          mapping = ggplot2::aes(x = index, y = Measurement), color = "darkred") +
      ggplot2::geom_label(data = data.frame(x = max(index), y = toleranceLines[2], l = "Ref - 0.1 * Tol"),
                          ggplot2::aes(x = x, y = y, label = l), vjust="outward",hjust="inward", color = "darkred" ) +
      ggplot2::scale_x_discrete(name = "Index", breaks = c(seq(1, max(index), 5),max(index)))

    p <- jaspGraphs::themeJasp(p) + ggplot2::theme(plot.margin = ggplot2::unit(c(.5, .5, .5, .5), "cm"))

    plot$plotObject <- p

    return(plot)

  }

}

.msaCheckErrors <- function(dataset, options) {

  #if (options[["gaugeScatterPlotOperators"]]){
  #  .hasErrors(dataset = dataset, type = "factorLevels",
  #             factorLevels.target  = options$operators, factorLevels.amount  = "> 2",
  #             exitAnalysisIfErrors = TRUE)
  #}

}
