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

msaGaugeLinearity <- function(jaspResults, dataset, options, ...) {

  measurements <- unlist(options$measurements)
  parts <- unlist(options$parts)
  standards <- unlist(options$standard)

  ready <- (measurements != "" && parts != "" && standards != "")

  numeric.vars <- c(measurements, standards)
  numeric.vars <- numeric.vars[numeric.vars != ""]

  factor.vars <- parts
  factor.vars <- factor.vars[factor.vars != ""]


  #if (length(measurements) == 0)
  #  return()

  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = numeric.vars, columns.as.factor = factor.vars,
                                         exclude.na.listwise = c(numeric.vars, factor.vars))
  }

  # Linearity and Bias Analysis

  if (is.null(jaspResults[["LB"]])) {
    jaspResults[["LB"]] <- createJaspContainer(gettext("Linearity and Bias"))
    jaspResults[["LB"]]$position <- 1
  }
  jaspResults[["LB"]] <- .linearityAndBias(ready = ready, dataset = dataset, options = options, measurements = measurements, parts = parts, standards = standards)


  return()
}

#dataset <- read.csv("C:/Users/Jonee/Google Drive/SKF Six Sigma/Dataset/MinitabExamples/BearingDiameter(LinearityAndBias).csv")
#colnames(dataset) <- c("Part", "Master", "Response")
#parts <- "Part"
#measurements <- "Response"
#standards <- "Master"

.linearityAndBias <- function(ready, dataset, options, measurements, parts, standards) {

  tablesAndGraphs <- createJaspContainer(gettext("Linearity and Bias"))

  table1 <- createJaspTable(title = gettext("Gauge Bias"))

  table1$addColumnInfo(name = "part",  title = gettext("Part"), type = "string")
  table1$addColumnInfo(name = "referenceValue",  title = gettext("Reference value"), type = "number")
  table1$addColumnInfo(name = "observedMean", title = gettext("Mean per reference value"), type = "number")
  table1$addColumnInfo(name = "bias",            title = gettext("Bias per reference value"), type = "number")
  table1$addColumnInfo(name = "percentBias",            title = gettext("Percent bias per reference value"), type = "number")
  table1$addColumnInfo(name = "pvalue",            title = gettext("<i>p(t</i>-test of bias against 0)"), type = "pvalue")


  table2 <- createJaspTable(title = gettext("Regression Model"))

  table2$addColumnInfo(name = "predictor",  title = gettext("Predictor"), type = "string")
  table2$addColumnInfo(name = "coefficient", title = gettext("Coefficient"), type = "number")
  table2$addColumnInfo(name = "Tvalues", title = gettext("<i>t</i>-statistic"), type = "number")
  table2$addColumnInfo(name = "SEcoefficient", title = gettext("Std. Error coefficients"), type = "number")
  table2$addColumnInfo(name = "pvalue",            title = gettext("<i>p</i>"), type = "pvalue")

  table3 <- createJaspTable(title = gettext("Gauge Linearity"))

  table3$addColumnInfo(name = "S",  title = gettext("S"), type = "number")
  table3$addColumnInfo(name = "linearity", title = gettext("Linearity"), type = "number")
  table3$addColumnInfo(name = "rsq",       title = gettextf("R%1$s", "\u00B2"), type = "number")
  table3$addColumnInfo(name = "percentLin", title = gettext("% Linearity"), type = "number")

  Note <- gettext("Calculations are based on 6 times the historical standard variation unless specified otherwise.")
  table1$addFootnote(Note)
  table2$addFootnote(Note)
  table3$addFootnote(Note)

  plot1 <- createJaspPlot(title = gettext("Bias and Linearity"), width = 500, height = 500)

  plot2 <- createJaspPlot(title = gettext("Percentage Process Variation Graph"), width = 500, height = 500)

  if (ready) {
    ReferenceValues <- unique(dataset[[standards]])
    df <- data.frame()
    biases <- vector()
    Part <- vector()
    references <- vector()

    for (i in ReferenceValues) {
      ReferenceValue <- i
      ReferenceData <- subset.data.frame(dataset, dataset[[standards]] == i)
      Ref <- ReferenceData[[standards]][1]
      Part <- unique(ReferenceData[[parts]])
      ObservedMean <- mean(ReferenceData[[measurements]])
      Bias <-  ObservedMean - Ref
      pvalue <- t.test(ReferenceData[[measurements]] - Ref, mu = 0)$p.value
      df <- rbind(df, list(Part = Part,Ref = rep(Ref,length(Part)), ObservedMean =  rep(ObservedMean,length(Part)), Bias =  rep(Bias,length(Part)), pvalue =  rep(pvalue,length(Part))))
      biases <- c(biases, ReferenceData[[measurements]] - ReferenceData[[standards]][1])
      references <- c(references, ReferenceData[[standards]])
    }

    averageBias <- mean(df$Bias)
    averagePvalue <- t.test(biases, mu = 0)$p.value

    table1$setData(list("part" = c(df$Part,gettext("Average")),
                        "referenceValue" = df$Ref,
                        "observedMean" = df$ObservedMean,
                        "bias" = c(df$Bias, averageBias),
                        "percentBias" = (abs(c(df$Bias, averageBias))/options$linearityProcessVariation) * 100,
                        "pvalue" = c(df$pvalue, averagePvalue)))
    df2 <- data.frame(Bias = biases, Ref = references)



    p1 <- ggplot2::ggplot(data = df2, mapping = ggplot2::aes(x = Ref, y = Bias)) + ggplot2::geom_hline(yintercept = 0, lty = 2, color = "grey") +
      ggplot2::geom_smooth(data = df2, mapping = ggplot2::aes(x = Ref, y = Bias), method = "lm", color = "red") +
      ggplot2::scale_x_continuous(name = gettext("Reference")) +
      jaspGraphs::geom_point(data = df2, mapping = ggplot2::aes(x = Ref, y = Bias), fill = "blue", size = 4, shape = "X") +
      jaspGraphs::geom_point(data = df, mapping = ggplot2::aes(x = Ref, y = Bias), fill = "red",size = 4) +
      #ggplot2::geom_text(x = mean(df2$Ref), y = max(df2$Bias) * 1.5, label = eq(df2$Ref, df2$Bias), parse = TRUE) +
      ggpubr::stat_regline_equation(label.x = mean(df2$Ref),label.y = max(df2$Bias) * 1.5, ggplot2::aes(label = ..eq.label..), size = 5)+
      ggplot2::scale_y_continuous(limits = c(min(df2$Bias), max(df2$Bias) * 2))


    p1 <- jaspGraphs::themeJasp(p1)

    plot1$plotObject <- p1


    lm <- summary(lm(Bias ~ Ref, df2))

    coefficientConstant <- lm$coefficients[1]
    coefficientSlope <- lm$coefficients[2]
    coefficients <- c(coefficientConstant, coefficientSlope)
    SEcoefficients <- lm$coefficients[c(3,4)]
    Tvalues <- lm$coefficients[c(5,6)]
    pvalues <- lm$coefficients[c(7,8)]
    S <- lm$sigma
    rsq <- lm$r.squared
    linearity <- abs(coefficientSlope) * options$linearityProcessVariation
    percentLin <- (linearity / options$linearityProcessVariation) * 100

    table2$setData(list("predictor" = c("Intercept", "Slope"),
                        "coefficient" = coefficients,
                        "Tvalues" = Tvalues,
                        "SEcoefficient" = SEcoefficients,
                        "pvalue" = pvalues))

    table3$setData(list("S" = S,
                        "linearity" = linearity,
                        "rsq" = rsq,
                        "percentLin" = percentLin))

    df3 <- data.frame(Source = c("Linearity", "Bias"), Percent = c(percentLin, (abs(averageBias) / options$linearityProcessVariation) * 100))

    p2 <- ggplot2::ggplot() + ggplot2::geom_col(data = df3, mapping = ggplot2::aes(x = Source, y = Percent)) + ggplot2::xlab(ggplot2::element_blank())

    p2 <- jaspGraphs::themeJasp(p2)

    plot2$plotObject <- p2
  }

  if (options$LBtableBias)
    tablesAndGraphs[["table1"]] <- table1


  if (options$LBtableLinearity) {
    tablesAndGraphs[["table2"]] <- table2
    tablesAndGraphs[["table3"]] <- table3
  }

  if (options$LBgraph)
    tablesAndGraphs[["plot1"]] <- plot1

  if (options$LBpercentGraph)
    tablesAndGraphs[["plot2"]] <- plot2




  return(tablesAndGraphs)

}

eq <- function(x,y) {
  m <- lm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}
