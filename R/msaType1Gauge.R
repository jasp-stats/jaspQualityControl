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
msaType1Gauge <- function(jaspResults, dataset, options, ...) {

  measurements <- unlist(options[["measurement"]])
  measurements <- measurements[measurements != ""]

  ready <- (length(measurements) != 0)

  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = measurements, exclude.na.listwise = measurements)
  }

  # Bias Run Chart
  if (options[["runChart"]]) {
    if (is.null(jaspResults[["biasRun"]])) {
      jaspResults[["biasRun"]] <- createJaspContainer(gettext("Run chart"))
    }

    jaspResults[["biasRun"]] <- .biasRunChart(dataset = dataset, measurements = measurements, options =  options, ready = ready)
    jaspResults[["biasRun"]]$position <- 1
  }

  # Determine Bias Table
  if (options[["biasTable"]]) {
    if (is.null(jaspResults[["biasTable"]])) {
      jaspResults[["biasTable"]] <- createJaspContainer(gettext("Bias table"))
    }
    jaspResults[["biasTable"]] <- .biasTable(dataset = dataset, measurements = measurements, options = options, ready = ready)
    jaspResults[["biasTable"]]$position <- 2
  }

  # Determine Bias t-Test
  if (options[["tTest"]]) {
    if (is.null(jaspResults[["biasTtest"]])) {
      jaspResults[["biasTtest"]] <- createJaspContainer(gettext("t-test bias"))
    }
    jaspResults[["biasTtest"]] <- .biasTtest(dataset = dataset, measurements = measurements, options =  options, ready = ready)
    jaspResults[["biasTtest"]]$position <- 3
  }

  # Determine Bias Histogram
  if (options[["histogram"]]) {
    if (is.null(jaspResults[["biasHistogram"]])) {
      jaspResults[["biasHistogram"]] <- createJaspContainer(gettext("Histogram bias"))
    }
    jaspResults[["biasHistogram"]] <- .biasHistogram(dataset = dataset, measurements = measurements, options =  options, ready = ready)
    jaspResults[["biasHistogram"]]$position <- 4
  }

  return()
}

.biasTable <- function(dataset, measurements, options, ready) {

  biasTables <- createJaspContainer(gettext("Bias and instrument capability table"))

  data <- dataset

  tolerance <- options[["toleranceRange"]]
  reference <- options[["referenceValue"]]

  studyVarMultiplier <- as.numeric(options[["studyVarianceMultiplier"]])

  table1 <- createJaspTable(title = gettext("Basic statistics"))
  table1$dependOn(c("referenceValue", "biasTable", "toleranceRange"))

  table1$addColumnInfo(name = "referenceValue",  title = gettext("Reference value"), type = "number")
  table1$addColumnInfo(name = "observedMean", title = gettext("Mean"), type = "number")
  table1$addColumnInfo(name = "bias",            title = gettext("Bias"), type = "number")
  table1$addColumnInfo(name = "sd",            title = gettext("Std. dev. (<i>s</i>)"), type = "number")
  table1$addColumnInfo(name = "SV",            title = gettext("Instrument variation"), type = "number")
  table1$addColumnInfo(name = "tolerance",       title = gettext("Tolerance"), type = "number")
  table1$addColumnInfo(name = "toleranceLB",       title = gettext("Lower"), overtitle = gettext("Tolerance bounds"), type = "number")
  table1$addColumnInfo(name = "toleranceUB",       title = gettext("Upper"), overtitle = gettext("Tolerance bounds"), type = "number")
  table1$addColumnInfo(name = "biasPercent",     title = gettextf("%% Bias"), type = "number")

  table1$addFootnote(gettextf("The instrument variation is calculated as %i * <i>s</i>.", studyVarMultiplier))

  table2 <- createJaspTable(title = gettext("Capability"))
  table2$dependOn(c("referenceValue", "biastable", "toleranceRange"))


  table2$addColumnInfo(name = "Cg",  title = gettext("Cg"), type = "string")
  table2$addColumnInfo(name = "Cgk",  title = gettext("Cgk"), type = "string")
  table2$addColumnInfo(name = "percentRep",  title = gettextf("%% Var(repeatability)"), type = "integer")
  table2$addColumnInfo(name = "percentRepBias",  title = gettextf("%% Var(repeatability and bias)"), type = "integer")

  table1$setData(list(      "referenceValue"       = reference,
                            "tolerance"             = tolerance))

  if (ready) {

    observedAverage <- mean(dataset[[measurements]])
    bias <- observedAverage - options[["referenceValue"]]
    biasPercent <- abs(bias) / tolerance * 100
    sd <- sd(unlist(data))
    sv <- sd * studyVarMultiplier
    k <- options[["percentToleranceForCg"]]

    table1$setData(list(      "referenceValue"       = reference,
                              "observedMean"          = observedAverage,
                              "bias"                  = bias,
                              "sd"                    = sd,
                              "SV"                    =  sv,
                              "tolerance"             = tolerance,
                              "toleranceLB"           = reference - (k/200)*tolerance,
                              "toleranceUB"           = reference + (k/200)*tolerance,
                              "biasPercent"           = biasPercent))

    cg <- ((k / 100) * tolerance) / sv
    cgk <- (((k / 200) * tolerance) - abs(observedAverage - reference)) / (sv / 2)
    percentRep <-   k/cg
    percentRepBias <- k/cgk


    table2$setData(list(     "Cg"     = round(cg, 2),
                             "Cgk"    = round(cgk, 2),
                             "percentRep"  =  round(percentRep,2),
                             "percentRepBias"    = round(percentRepBias,2)))

  }


  biasTables[["Basic"]] <- table1
  biasTables[["Capability"]] <- table2



  return(biasTables)
}

.biasTtest <- function(dataset, measurements, options, ready) {

  data <- dataset

  table <- createJaspTable(title = gettext("t-test of observed bias against 0"))

  table$dependOn(c("referenceValue", "tTest", "toleranceRange"))
  ciLevel <- options[["tTestCiLevel"]]
  ciLevelPercent <- ciLevel * 100
  table$addColumnInfo(name = "df",              title = gettext("df"), type = "integer")
  table$addColumnInfo(name = "bias",            title = gettext("Bias"), type = "number")
  table$addColumnInfo(name = "lci",             title = gettext("Lower"), type = "number", overtitle = gettextf("%s CI for bias", paste(ciLevelPercent, "%")))
  table$addColumnInfo(name = "uci",             title = gettext("Upper"), type = "number", overtitle = gettextf("%s CI for bias", paste(ciLevelPercent, "%")))
  table$addColumnInfo(name = "t",               title = gettext("<i>t</i>"), type = "number")
  table$addColumnInfo(name = "p",               title = gettext("<i>p</i>-value"), type = "pvalue")


  if (ready) {

    if (nrow(dataset[measurements]) < 2){
      table$setError(gettextf("t-test requires more than 1 measurement. %1$i valid measurement(s) detected in %2$s.", nrow(dataset[measurements]), measurements))
      return(table)
    }

    bias <- data - options[["referenceValue"]]
    tTest <- t.test(bias, mu = 0, conf.level = ciLevel)

    table$addRows(list(      "df"                   = tTest$parameter,
                             "bias"                 = tTest$estimate,
                             "lci"                  = tTest$conf.int[1],
                             "uci"                  = tTest$conf.int[2],
                             "t"                    = tTest$statistic,
                             "p"                    = tTest$p.value))
  }

  return(table)
}

.biasHistogram <- function(dataset, measurements, options, ready) {

  if (ready) {

    data <- dataset[[measurements]]

    plot <- createJaspPlot(title = gettext("Bias histogram"), width = 700, height = 400)
    dataForBreaks <- c(data)
    if (options[["histogramBinWidthType"]] == "freedmanDiaconis") {
      binWidthType <- "fd"
    } else if (options[["histogramBinWidthType"]] == "manual") {
      binWidthType <- options[["histogramManualNumberOfBins"]]
    } else {
      binWidthType <- options[["histogramBinWidthType"]]
    }

    h <- hist(data, plot = FALSE, breaks = binWidthType)
    binWidth <- (h$breaks[2] - h$breaks[1])
    plotData <- data.frame("x" = c(data))
    p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x)) +
      ggplot2::geom_histogram(fill = "grey", col = "black", linewidth = .7, binwidth = binWidth,
                              closed = options[["histogramBinBoundaryDirection"]], center = binWidth/2, na.rm = TRUE) +
      ggplot2::ylab(gettext("Count")) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()

    if (options[["histogramMeanLine"]]) {
      mean <- mean(data)
      observedsd <- sd(data, na.rm = TRUE)
      nSigma <- as.numeric(options[["studyVarianceMultiplier"]])/2
      p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = mean, color = "Mean"), lwd = 1.5) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = mean - nSigma*observedsd, color = "MeanMinusSigma"), lwd = 1.5, linetype = "dotted") +
        ggplot2::geom_vline(ggplot2::aes(xintercept = mean + nSigma*observedsd, color = "MeanPlusSigma"), lwd = 1.5, linetype = "dotted") +
        ggplot2::scale_color_manual(name = "", values = c("Mean" = "dodgerblue", "MeanMinusSigma" = "red", "MeanPlusSigma" = "red"),
                                    labels = c(gettext("Mean"), gettextf("Mean - %is", nSigma), gettextf("Mean + %is", nSigma)))
      dataForBreaks <- c(dataForBreaks, mean)
      if (options[["histogramMeanCi"]]) {
        CI <- t.test(data, mu = 0, conf.level = options[["histogramMeanCiLevel"]])$conf.int
        p <- p + ggplot2::geom_errorbarh(ggplot2::aes(y = .5, x = mean, xmin = CI[1], xmax = CI[2]), lwd = 1, color = "dodgerblue")
        dataForBreaks <- c(dataForBreaks, CI)
      }
    }
    if (options[["histogramReferenceValueLine"]]) {
      reference <- options[["referenceValue"]]
      percentTolerance <- options[["percentToleranceForCg"]]/100
      percentToleranceOneSided <- percentTolerance/2
      p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = reference, color = "Reference"), lwd = 1.5) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = reference - options[["toleranceRange"]] * percentToleranceOneSided,
                                         color = "RefMinusTol"), lwd = 1.5, linetype = "dashed") +
        ggplot2::geom_vline(ggplot2::aes(xintercept = reference + options[["toleranceRange"]] * percentToleranceOneSided,
                                         color = "RefPlusTol"), lwd = 1.5, linetype = "dashed") +
        ggplot2::scale_color_manual(name = "", values = c("Reference" = "darkgreen", "RefMinusTol" = "darkred", "RefPlusTol" = "darkred"),
                                    labels = c(gettext("Reference"), gettextf("Ref. - %.2f * tol.", percentToleranceOneSided),
                                               gettextf("Ref. + %.2f * tol.", percentToleranceOneSided)))
      dataForBreaks <- c(dataForBreaks, reference)
    }
    if (options[["histogramReferenceValueLine"]] && options[["histogramMeanLine"]])
      p <- p + ggplot2::scale_color_manual(name = "", values = c("Mean" = "dodgerblue", "MeanMinusSigma" = "red", "MeanPlusSigma" = "red",
                                                                 "Reference" = "darkgreen", "RefMinusTol" = "darkred", "RefPlusTol" = "darkred"),
                                           labels = c(gettext("Mean"), gettextf("Mean - %is", nSigma), gettextf("Mean + %is", nSigma),
                                                      gettext("Reference"),
                                                      gettextf("Ref. - %.2f * tol.", percentToleranceOneSided),
                                                      gettextf("Ref. + %.2f * tol.", percentToleranceOneSided)))


    xBreaks <- jaspGraphs::getPrettyAxisBreaks(dataForBreaks)
    p <- p + ggplot2::theme(legend.position = "right") + ggplot2::scale_x_continuous(breaks = xBreaks, name = measurements)

    plot$dependOn(c("histogram", "histogramBinWidthType", "histogramBinBoundaryDirection"))

    plot$plotObject <- p

    return(plot)

  }
}

.biasRunChart <- function(dataset, measurements, options, ready) {

  if (ready) {

    plot <- createJaspPlot(title = gettextf("Run chart of %s", measurements), width = 700, height = 400)

    dataset <- tidyr::gather(dataset, Repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

    if (nrow(dataset["Measurement"]) == 0){
      plot$setError(gettextf("No valid measurements in %s.", measurements))
      return(plot)
    }

    Observation <- 1:length(dataset[["Measurement"]])

    dataset <- cbind(dataset, Observation = Observation)

    plot$dependOn(c("runChart"))
    datayBreaks <- c(dataset[["Measurement"]], options[["referenceValue"]])
    p <- ggplot2::ggplot()
    if (options[["runChartToleranceLimitLines"]]) {
      percentTolerance <- options[["percentToleranceForCg"]]/100
      percentToleranceOneSided <- percentTolerance/2
      toleranceLines <- c(options[["referenceValue"]] + percentToleranceOneSided * options[["toleranceRange"]], options[["referenceValue"]] - percentToleranceOneSided * options[["toleranceRange"]])
      datayBreaks <- c(datayBreaks, toleranceLines)
      p <- p + ggplot2::geom_hline(yintercept = toleranceLines[1], color = "darkred", linetype = "dashed") +
        ggrepel::geom_label_repel(data = data.frame(x = max(Observation) * 1.15 , y = toleranceLines[1], l = gettextf("Ref. + %.2f * tol.", percentToleranceOneSided)),
                                  ggplot2::aes(x = x, y = y, label = l), vjust="top",hjust="inward", color = "darkred", size = 5) +
        ggplot2::geom_hline(yintercept = toleranceLines[2], color = "darkred", linetype = "dashed") +
        ggrepel::geom_label_repel(data = data.frame(x = max(Observation) * 1.15, y = toleranceLines[2], l = gettextf("Ref. - %.2f * tol.", percentToleranceOneSided)),
                                  ggplot2::aes(x = x, y = y, label = l), vjust="bottom",hjust="inward", color = "darkred", size = 5)
    }
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(datayBreaks)
    if (max(Observation) <= 15){
      nxBreaks <- max(Observation)
    }else{
      nxBreaks <- 5
    }
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(x = Observation, n = nxBreaks)
    xBreaks[1] <- 1
    observedMean <- mean(dataset[["Measurement"]], na.rm = TRUE)
    observedsd <- sd(dataset[["Measurement"]], na.rm = TRUE)
    nSigma <- as.numeric(options[["studyVarianceMultiplier"]])/2

    p <- p + jaspGraphs::geom_line(data = dataset, mapping = ggplot2::aes(x = Observation, y = Measurement, group = 1)) +
      ggplot2::geom_hline(yintercept = observedMean + nSigma*observedsd, color = "red", linetype = "dotted") +
      ggplot2::geom_hline(yintercept = observedMean - nSigma*observedsd, color = "red", linetype = "dotted") +
      ggplot2::geom_hline(yintercept = options[["referenceValue"]], color = "darkgreen") +
      ggplot2::geom_hline(yintercept = observedMean, color = "dodgerblue") +
      ggrepel::geom_label_repel(data = data.frame(x = max(Observation) * 1.15, y = observedMean + nSigma*observedsd, l = gettextf("Mean + %is", nSigma)),
                                ggplot2::aes(x = x, y = y, label = l), hjust="inward", color = "red", size = 5) +
      ggrepel::geom_label_repel(data = data.frame(x = max(Observation) * 1.15, y = observedMean - nSigma*observedsd, l = gettextf("Mean - %is", nSigma)),
                                ggplot2::aes(x = x, y = y, label = l), hjust="inward", color = "red", size = 5) +
      ggrepel::geom_label_repel(data = data.frame(x = max(Observation) * 1.15, y = options[["referenceValue"]], l = gettext("Ref.")),
                                ggplot2::aes(x = x, y = y, label = l), hjust="inward", color = "darkgreen", size = 5) +
      ggrepel::geom_label_repel(data = data.frame(x = max(Observation) * 1.05, y = observedMean, l = gettext("Mean")),
                                ggplot2::aes(x = x, y = y, label = l), hjust="inward", color = "dodgerblue", size = 5) +
      ggplot2::scale_x_continuous(name = "Observation", breaks = xBreaks, limits = c(min(xBreaks), max(xBreaks) * 1.2)) +
      ggplot2::scale_y_continuous(name = measurements, breaks = yBreaks, limits = range(yBreaks)) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(plot.margin = ggplot2::unit(c(.5, .5, .5, .5), "cm"))

    if (options[["runChartIndividualMeasurementDots"]])
      p <- p + jaspGraphs::geom_point(data = dataset, ggplot2::aes(x = Observation, y = Measurement))



    plot$plotObject <- p

    return(plot)

  }

}

