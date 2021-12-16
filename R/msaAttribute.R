
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

msaAttribute <- function(jaspResults, dataset, options, ...) {

  if (options[["AAAdataFormat"]] == "AAAwideFormat"){
    measurements <- unlist(options$measurements)
  }else{
    measurements <- unlist(options$measurementsLong)
  }

  parts <- unlist(options$parts)
  operators <- unlist(options$operators)
  standards <- unlist(options$standard)

  numeric.vars <- measurements
  numeric.vars <- numeric.vars[numeric.vars != ""]
  factor.vars <- c(parts, operators, standards)
  factor.vars <- factor.vars[factor.vars != ""]

  # Ready
  if (options[["AAAdataFormat"]] == "AAAwideFormat"){
    ready <- (length(measurements) != 0 && operators != "" && parts != "")
  } else {
    ready <- (measurements != "" && operators != "" && parts != "")
  }


  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns  = c(numeric.vars, factor.vars),
                                         exclude.na.listwise = c(numeric.vars, factor.vars))
  }

  if (options[["AAAdataFormat"]] == "AAAlongFormat" && ready){
    dataset <- dataset[order(dataset[operators]),]
    nrep <- table(dataset[operators])[[1]]/length(unique(dataset[[parts]]))
    index <- rep(paste("V", 1:nrep, sep = ""), nrow(dataset)/nrep)
    dataset <- cbind(dataset, data.frame(index = index))
    dataset <- tidyr::spread(dataset, index, measurements)
    measurements <- unique(index)

    if (standards != "")
      dataset <- dataset[,c(operators, parts, measurements,standards)]
    else
      dataset <- dataset[,c(operators, parts, measurements)]
  }


  # Error handling
  if (standards == "" && options$PositiveRef != "" && options[["AAAcohensKappa"]]) {
    jaspResults[["tableReference"]] <- createJaspContainer(title = gettext("Reference Tables and Plots"))
    jaspResults[["tableReference"]]$position <- 10
    jaspResults[["tableReference"]]$dependOn(c("PositiveRef", "standard"))

    Container <- jaspResults[["tableReference"]]

    tableReference <- createJaspTable(title = gettext("Reference Tables and Plots"))
    tableReference$setError(gettext("Please insert a reference value before specifying a positive reference."))

    Container[["TableError"]] <- tableReference

    return()
  }


  # Attribute Agreement Analysis Table & Graph
  if (length(measurements) == 0) {
    if (is.null(jaspResults[["AAAtableGraphs"]])) {
      jaspResults[["AAAtableGraphs"]] <- createJaspContainer(gettext("Attributes Agreement Analysis"))
      jaspResults[["AAAtableGraphs"]]$position <- 16
    }
    jaspResults[["AAAtableGraphs"]] <- .aaaTableGraphs(ready = ready, dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, standards = standards)
  }else{
    if (is.null(jaspResults[["AAAtableGraphs"]])) {
      jaspResults[["AAAtableGraphs"]] <- createJaspContainer(gettext("Attributes Agreement Analysis"))
      jaspResults[["AAAtableGraphs"]]$position <- 16
    }
    jaspResults[["AAAtableGraphs"]] <- .aaaTableGraphs(ready = ready, dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, standards = standards)
  }

  # Cohen's Kappa Operator vs Standard
  if (options[["AAAcohensKappa"]] && ready) {
    if (is.null(jaspResults[["cohensKappa"]])) {
      jaspResults[["cohensKappa"]] <- createJaspContainer(gettext("Cohen's Kappa"))
      jaspResults[["cohensKappa"]]$position <- 18
    }

    jaspResults[["cohensKappa"]] <- .cohensKappa(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, standards = standards)
    jaspResults[["cohensKappaCor"]] <- .corCohenTable(ready = ready, dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }


  # Fleiss' Kappa
  if (options[["AAAfleissKappa"]] && ready) {
    if (is.null(jaspResults[["fleissKappa"]])) {
      jaspResults[["fleissKappa"]] <- createJaspContainer(gettext("Cohen's Kappa"))
      jaspResults[["fleissKappa"]]$position <- 19
    }

    jaspResults[["fleissKappa"]] <- .fleissKappa(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, standards = standards)

  }

  # Kendall Tau
  if (options[["AAAkendallTau"]] && ready) {
    if (is.null(jaspResults[["KendallTau"]])) {
      jaspResults[["KendallTau"]] <- createJaspContainer(gettext("Kendall's Tau"))
      jaspResults[["KendallTau"]]$position <- 20
    }

    jaspResults[["KendallTau"]] <- .kendallTau(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, standards = standards, ready = ready)


  }
  return()
}


.cohensKappa <- function(dataset, measurements, parts, operators, standards, options) {

  dataset <- tidyr::gather(dataset, Repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

  table <- createJaspTable(title = gettext("Cohen's Kappa for Appraiser vs Standard"))

  table$dependOn(c("AAAcohensKappa"))

  table$addColumnInfo(name = "Appraiser",  title = gettext("Appraiser"), type = "string")
  table$addColumnInfo(name = "CK", title = gettext("Cohen's Kappa"), type = "number")

  appraiserVector <- vector(mode = "character")
  kappaVector <- vector(mode = "numeric")
  for (i in 1:length(unique(dataset[[operators]]))) {
    appraiser <- as.character(unique(dataset[[operators]])[i])
    appraiserVector[i] <- appraiser
    onlyAppraiser <- subset(dataset, dataset[operators] == appraiser)
    kappaFrame <- data.frame(standard = onlyAppraiser[[standards]], measurement = onlyAppraiser[["Measurement"]])
    kappa <- psych::cohen.kappa(kappaFrame)
    kappaVector[i] <- kappa$kappa
  }

  allKappa <- psych::cohen.kappa(data.frame(standard = dataset[[standards]], measurement = dataset[["Measurement"]]))
  appraiserVector <- c(appraiserVector, "All")
  kappaVector <- c(kappaVector, allKappa$kappa)

  table$setData(list(      "Appraiser"       = appraiserVector,
                           "CK"              = kappaVector))

  return(table)
}

.corCohenTable <- function(dataset, ready, measurements, parts, operators, options) {

  table <- createJaspTable(title = gettext("Cohen's Kappa correlations summary"))
  table$dependOn(c("AAAcohensKappa"))
  table$addColumnInfo(name = "appraiserVector",  title = gettext("Appraiser"), type = "string")

  appraiserVector <- vector(mode = "character")
  listCor <- list()
  for (i in 1:length(unique(dataset[[operators]]))) {
    appraiser <- as.character(unique(dataset[[operators]])[i])
    appraiserVector[i] <- appraiser
    onlyAppraiser <- subset(dataset, dataset[operators] == appraiser)

    DataPerAppriaser <- unlist(onlyAppraiser[,measurements])

    if (!is.numeric(DataPerAppriaser))
      DataPerAppriaser <- ifelse(DataPerAppriaser == options$PositiveRef, 1, 0)


    listCor[[appraiser]] <- c(DataPerAppriaser)
    table$addColumnInfo(name = appraiser,  title = gettext(appraiser), type = "integer")

  }
  cors <- cbind(appraiserVector, round(cor(as.data.frame(listCor)), 2))

  if (!any(options$PositiveRef == as.character(unique(unlist(dataset[measurements])))) && options$PositiveRef != "" && !options$AAAkendallTau)
    table$setError(gettext("Please inseret a vaild Positive reference as used in the 'Results' variables."))


  table$setData(ifelse(cors == "1", "-", cors))
  return(table)
}

.fleissKappa <- function(dataset, measurements, parts, operators, standards, options) {

  table <- createJaspTable(title = gettext("Fleiss' Kappa"))

  table$dependOn(c("AAAfleissKappa"))

  table$addColumnInfo(name = "appraiser",  title = gettext("Appraiser"), type = "string")
  table$addColumnInfo(name = "within", title = gettext("Within Appraisers"), type = "number")
  if (standards != "")
    table$addColumnInfo(name = "vsStandard", title = gettext("Appraiser vs Standard"), type = "number")
  table$addColumnInfo(name = "between", title = gettext("Between Appraisers"), type = "number")

  appraiserVector <- vector(mode = "character")
  kappaWithinVector <- vector(mode = "numeric")
  kappaBetweenVector <- vector(mode = "numeric")
  kappaStandardVector <- vector(mode = "numeric")
  listBetween <- list()

  for (i in 1:length(unique(dataset[[operators]]))) {
    appraiser <- as.character(unique(dataset[[operators]])[i])
    appraiserVector[i] <- appraiser
    onlyAppraiser <- subset(dataset, dataset[operators] == appraiser)

    # Within
    fkappa <- irr::kappam.fleiss(onlyAppraiser[measurements])
    kappaWithinVector[i] <- fkappa$value

    # Versus Standard
    if (standards != "") {
      Kappa0 <- NULL
      count = 0
      for (j in measurements){
        count = count + 1
        Kappa0[count] <- irr::kappam.fleiss(cbind(onlyAppraiser[j], onlyAppraiser[[standards]]))$value
      }
      kappaStandardVector[i] <- mean(Kappa0)
    }

    # Between
    listBetween[[i]] <- onlyAppraiser[measurements]
  }

  kappaBetweenVector <- c(rep(NA,length(unique(dataset[[operators]]))), irr::kappam.fleiss(as.data.frame(listBetween))$value)
  appraiserVector <- c(appraiserVector, 'All')

  if (standards != "") {
    kappaStandardVector <- c(kappaStandardVector, mean(kappaStandardVector))
    table$setData(list(      "appraiser"       = appraiserVector,
                             "within"          = kappaWithinVector,
                             "vsStandard"      = kappaStandardVector,
                             "between"         = kappaBetweenVector))
  }
  else{
    table$setData(list(      "appraiser"       = appraiserVector,
                             "within"          = kappaWithinVector,
                             "between"         = kappaBetweenVector))
  }

  return(table)
}

.aaaTableGraphs <- function(ready, dataset, measurements, parts, operators, standards, options) {

  AAA <- createJaspContainer(gettext("Attributes Agreement Analysis"))
  AAA$dependOn(c("measurements", "parts", "operators", "standard"))

  if (standards != "") {
    tableWithin <- createJaspTable(title = gettext("Within Appraisers"))
    tableBetween <- createJaspTable(title = gettext("Between Appraisers"))
    tableEachVsStandard <- createJaspTable(title = gettext("Each Appraiser vs Standard"))
    tableAllVsStandard <- createJaspTable(title = gettext("All Appraisers vs Standard"))


    allTables <- list(tableWithin, tableBetween, tableEachVsStandard, tableAllVsStandard)

    for (table in allTables[c(1,3)]) {
      table$addColumnInfo(name = "Appraiser",  title = gettext("Appraiser"), type = "string")
    }

    for (table in allTables) {
      table$addColumnInfo(name = "Inspected", title = gettext("Inspected"), type = "integer")
      table$addColumnInfo(name = "Matched", title = gettext("Matched"), type = "integer")
      table$addColumnInfo(name = "Percent", title = gettext("Percent"), type = "number")
      table$addColumnInfo(name = "CIL", title = gettext("Lower"), type = "integer", overtitle = gettext("Confidence interval of 95%"))
      table$addColumnInfo(name = "CIU", title = gettext("Upper"), type = "integer", overtitle = gettext("Confidence interval of 95%"))
    }

    if (ready) {
      appraiserVector <- as.character(unique(dataset[[operators]]))
      numberInspected <- length(unique(dataset[[parts]]))

      if ((length(unique(unlist(dataset[measurements]))) != 2 | length(unique(dataset[[standards]])) != 2) && !options$AAAkendallTau && options$PositiveRef != "") {
        table$setError(gettext("Invalid Reference and/or Results were inserted."))
        return(table)
      }

      for (measurement in measurements) {
        if (is.numeric(dataset[[measurement]])) {
          dataset[measurement] <- as.character(dataset[[measurement]])
          dataset[standards] <- as.character(dataset[[standards]])
        }
      }

      matchesWithin <- vector(mode = "numeric")

      for (i in 1:length(appraiserVector)) {
        onlyAppraiser <- subset(dataset, dataset[operators] == appraiserVector[i])
        matchesWithin[i] <- .countRowMatches(onlyAppraiser[measurements])
      }

      percentWithin <- matchesWithin / numberInspected* 100

      matchesEachVsStandard <- vector(mode = "numeric")

      for (i in 1:length(appraiserVector)) {
        onlyAppraiser <- subset(dataset, dataset[operators] == appraiserVector[i])
        matchesEachVsStandard[i] <- .countRowMatches(onlyAppraiser[c(measurements, standards)])
      }


      percentEachVsStandard <- matchesEachVsStandard / numberInspected* 100

      reshapeData <- data.frame(subset(dataset, dataset[[operators]] == unique(dataset[[operators]])[1])[standards])
      for (i in 1:length(appraiserVector)) {
        appraiser <- as.character(unique(dataset[[operators]])[i])
        reshapeData <- cbind(reshapeData, subset(dataset, dataset[operators] == appraiser)[measurements])
      }

      matchesBetween <- .countRowMatches(reshapeData[2:ncol(reshapeData)])
      percentBetween <- matchesBetween / numberInspected* 100

      matchesAllVsStandard <- .countRowMatches(reshapeData)
      percentAllVsStandard <- matchesAllVsStandard / numberInspected* 100

      if (length(measurements) == 1 & !options$AAAkendallTau) {
        tableWithin$setError(gettext("More than 1 Measurement per Operator required."))
      }else{
        tableDecisions <- createJaspTable(title = gettext("Study effectiveness summary"))
        tableDecisions$addColumnInfo(name = "Appraiser", title = gettext("Appraiser"), type = "string")
        tableDecisions$addColumnInfo(name = "Effectiveness", title = gettext("Effectiveness"), type = "string")
        tableDecisions$addColumnInfo(name = "Miss", title = gettext("Miss rate"), type = "string")
        tableDecisions$addColumnInfo(name = "False", title = gettext("False alarm rate"), type = "string")

        if (!any(options$PositiveRef == as.character(unique(unlist(dataset[measurements])))) && options$PositiveRef != "" && !options$AAAkendallTau)
          tableDecisions$setError(gettext("Please inseret a vaild Positive reference as used in the 'Results' variables."))

        if (!options$AAAkendallTau && standards != "" && options$PositiveRef != "" && any(options$PositiveRef == dataset[measurements]))
        {
          PositiveRef <- options$PositiveRef
          Misses <- vector()
          Falses <- vector()
          for (i in 1:length(appraiserVector)) {
            #Miss Rate
            dat_Neg <- subset(dataset, dataset[operators] == appraiserVector[i] & dataset[standards] != PositiveRef)
            Misses[i] = sum(as.character(unlist(dat_Neg[measurements])) == PositiveRef) / length(unlist(dat_Neg[measurements])) * 100

            #False Rate
            dat_Pos <- subset(dataset, dataset[operators] == appraiserVector[i] & dataset[standards] == PositiveRef)
            Falses[i] = sum(as.character(unlist(dat_Pos[measurements])) != PositiveRef) / length(unlist(dat_Pos[measurements])) * 100
          }

          tableDecisions$setData(list("Appraiser"     = appraiserVector,
                                      "Effectiveness" = .decisionNote(percentEachVsStandard),
                                      "Miss" = .decisionNote(Misses, type = "Miss"),
                                      "False" = .decisionNote(Falses, type = "Falses")))
          tableDecisions$addFootnote(gettext("Acceptable: x >= 90% (Effectiveness), x =< 2% (Miss rate), x =< 5% (False alarm rate)"))
          tableDecisions$addFootnote(gettext("Marginally acceptable: x >= 80% (Effectiveness), x =< 5% (Miss rate), x =< 10% (False alarm rate)"))
          tableDecisions$addFootnote(gettext("Unacceptable: x < 80% (Effectiveness), x > 5% (Miss rate), x > 10% (False alarm rate)"))
        }

        CIWithin <- .AAACI(matchesWithin, rep(numberInspected, length(appraiserVector)))
        tableWithin$setData(list(      "Appraiser"       = appraiserVector,
                                       "Inspected"       = rep(numberInspected, length(appraiserVector)),
                                       "Matched"         = matchesWithin,
                                       "Percent"         = round(percentWithin, 2),
                                       "CIL" = CIWithin$lower,
                                       "CIU" = CIWithin$upper))
      }

      CIEachVsStandard <- .AAACI(matchesEachVsStandard, rep(numberInspected, length(appraiserVector)))
      tableEachVsStandard$setData(list("Appraiser"     = appraiserVector,
                                       "Inspected"       = rep(numberInspected, length(appraiserVector)),
                                       "Matched"         = matchesEachVsStandard,
                                       "Percent"         = round(percentEachVsStandard, 2),
                                       "CIL" = CIEachVsStandard$lower,
                                       "CIU" = CIEachVsStandard$upper))

      CIBetween <- .AAACI(matchesBetween, rep(numberInspected, length(appraiserVector)))
      tableBetween$setData(list(     "Inspected"       = numberInspected,
                                     "Matched"         = matchesBetween,
                                     "Percent"         = round(percentBetween, 2),
                                     "CIL" = unique(CIBetween$lower),
                                     "CIU" = unique(CIBetween$upper)))

      CIAllVsStandard <- .AAACI(matchesAllVsStandard, rep(numberInspected, length(appraiserVector)))
      tableAllVsStandard$setData(list("Inspected"      = numberInspected,
                                      "Matched"         = matchesAllVsStandard,
                                      "Percent"         = round(percentAllVsStandard,2),
                                      "CIL" = unique(CIAllVsStandard$lower),
                                      "CIU" = unique(CIAllVsStandard$upper)))
    }

    AAA[["Within"]] <- tableWithin
    AAA[["Between"]] <- tableBetween
    AAA[["EachVsStandard"]] <- tableEachVsStandard
    AAA[["AllVsStandard"]] <- tableAllVsStandard

    if (ready) {

      if (length(measurements) > 1) {
        plotWithin <- createJaspPlot(title = "Within Appraisers", width = 300, height = 400)

        withinDataframe <- data.frame(x = appraiserVector, y = percentWithin)

        pw <- ggplot2::ggplot(withinDataframe, ggplot2::aes(x = x, y = y)) +
          jaspGraphs::geom_point() +
          ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10))+
          ggplot2::geom_errorbar(ggplot2::aes(ymin = c(CIWithin$lower),
                                              ymax = c(CIWithin$upper)))


        pw <- jaspGraphs::themeJasp(pw) +
          ggplot2::ylab("Percent") +
          ggplot2::xlab("Appraiser")
        plotWithin$plotObject <- pw

        AAA[["PlotWithin"]] <- plotWithin
      }

      plotVs <- createJaspPlot(title = "Each Appraiser vs Standard", width = 300, height = 400)

      vsDataframe <- data.frame(x = appraiserVector, y = percentEachVsStandard)

      pvs <- ggplot2::ggplot(vsDataframe, ggplot2::aes(x = x, y = y)) +
        jaspGraphs::geom_point() +
        ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = c(CIEachVsStandard$lower),
                                            ymax = c(CIEachVsStandard$upper)))


      pvs <- jaspGraphs::themeJasp(pvs) +
        ggplot2::ylab("Percent") +
        ggplot2::xlab("Appraiser")

      plotVs$plotObject <- pvs

      AAA[["PlotVs"]] <- plotVs
    }

  } else{
    tableWithin <- createJaspTable(title = gettext("Within Appraisers"))
    tableBetween <- createJaspTable(title = gettext("Between Appraisers"))


    allTables <- list(tableWithin, tableBetween)

    for (table in allTables[1:2]) {
      table$addColumnInfo(name = "Appraiser",  title = gettext("Appraiser"), type = "string")
    }

    for (table in allTables) {
      table$addColumnInfo(name = "Inspected", title = gettext("Inspected"), type = "integer")
      table$addColumnInfo(name = "Matched", title = gettext("Matched"), type = "integer")
      table$addColumnInfo(name = "Percent", title = gettext("Percent"), type = "number")
      table$addColumnInfo(name = "CIL", title = gettext("Lower"), type = "integer", overtitle = gettext("Confidence interval of 95%"))
      table$addColumnInfo(name = "CIU", title = gettext("Upper"), type = "integer", overtitle = gettext("Confidence interval of 95%"))

    }

    if (ready) {
      appraiserVector <- as.character(unique(dataset[[operators]]))
      numberInspected <- length(unique(dataset[[parts]]))

      for (measurement in measurements) {
        if (is.numeric(dataset[[measurement]])) {
          dataset[measurement] <- as.character(dataset[[measurement]])
        }
      }

      matchesWithin <- vector(mode = "numeric")

      for (i in 1:length(appraiserVector)) {
        onlyAppraiser <- subset(dataset, dataset[operators] == appraiserVector[i])
        matchesWithin[i] <- .countRowMatches(onlyAppraiser[measurements])
      }

      percentWithin <- matchesWithin / numberInspected* 100


      reshapeData <- data.frame(subset(dataset, dataset[[operators]] == unique(dataset[[operators]])[1])[measurements])
      for (i in 1:length(appraiserVector)) {
        appraiser <- as.character(unique(dataset[[operators]])[i])
        reshapeData <- cbind(reshapeData, subset(dataset, dataset[operators] == appraiser)[measurements])
      }

      matchesBetween <- .countRowMatches(reshapeData[2:ncol(reshapeData)])
      percentBetween <- matchesBetween / numberInspected* 100

      if (length(measurements) == 1 && !options$AAAkendallTau) {
        tableWithin$setError(gettext("More than 1 Measurement per Operator required."))
      }else{
        CIWithin <- .AAACI(matchesWithin, rep(numberInspected, length(appraiserVector)))
        tableWithin$setData(list(      "Appraiser"       = appraiserVector,
                                       "Inspected"       = rep(numberInspected, length(appraiserVector)),
                                       "Matched"         = matchesWithin,
                                       "Percent"         = round(percentWithin, 2),
                                       "CIL" = CIWithin$lower,
                                       "CIU" = CIWithin$upper))
      }

      CIBetween <- .AAACI(matchesBetween, rep(numberInspected, length(appraiserVector)))
      tableBetween$setData(list(     "Inspected"       = c(numberInspected),
                                     "Matched"         = c(matchesBetween),
                                     "Percent"         = round(percentBetween, 2),
                                     "CIL" = unique(CIBetween$lower),
                                     "CIU" = unique(CIBetween$upper)))
    }

    AAA <- createJaspContainer(gettext("Attributes Agreement Analysis"))

    AAA$dependOn(c("measurements", "parts", "operators"))

    AAA[["Within"]] <- tableWithin
    AAA[["Between"]] <- tableBetween

    if (ready) {

      if (length(measurements) > 1) {
        plotWithin <- createJaspPlot(title = "Within Appraisers", width = 300, height = 400)

        withinDataframe <- data.frame(x = appraiserVector, y = percentWithin)

        pw <- ggplot2::ggplot(withinDataframe, ggplot2::aes(x = x, y = y)) + jaspGraphs::geom_point()

        pw <- jaspGraphs::themeJasp(pw) +
          ggplot2::ylab("Percent") +
          ggplot2::xlab("Appraiser") +
          ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = c(CIWithin$lower),
                                              ymax = c(CIWithin$upper)))

        plotWithin$plotObject <- pw

        AAA[["PlotWithin"]] <- plotWithin
      }
    }
  }

  if (options$AAAkendallTau) {
    AAA[["Within"]] <- NULL
    AAA[["Between"]] <- tableBetween

    if (options$standard != ""){
      AAA[["EachVsStandard"]] <- tableEachVsStandard
      AAA[["AllVsStandard"]] <- tableAllVsStandard
    }

  } else {
    AAA[["Within"]] <- tableWithin
    AAA[["Between"]] <- tableBetween

    if (options$standard != ""){
      AAA[["EachVsStandard"]] <- tableEachVsStandard
      AAA[["AllVsStandard"]] <- tableAllVsStandard
    }

    if (options$standard != "" & options$PositiveRef != "" & length(measurements) > 1)
      AAA[["StudyEffectiveness"]] <- tableDecisions
  }
  return(AAA)
}

.countRowMatches <- function(data) {
  count <- 0
  for (i in 1:nrow(data)) {
    matches <- unlist(data[i,]) == data[i,1]
    if (all(matches))
      count <- count + 1
  }
  return(count)
}


.kendallTau <- function(dataset, measurements, parts, operators, standards, options, ready) {

  operatorVector <- as.character(unique(dataset[[operators]]))

  table <- createJaspTable(title = gettext("Kendall's Tau"))

  table$dependOn(c("AAAkendallTau", "measurements", "parts", "operators", "standard"))

  if (!ready & options$AAAkendallTau)
    return(table)

  if (!is.numeric(dataset[[measurements[1]]])) {
    table$setError(gettext("Kendall's Tau is only available for numeric measurements."))
  }  else if (length(unique(unlist(dataset[measurements]))) <= 2) {
    table$setError(gettext("Kendall's Tau is only available for non-binary measurements."))
  } else{
    table$addColumnInfo(name = "Operator",  title = gettext("Operator"), type = "string")

    for (operator in operatorVector) {
      table$addColumnInfo(name = operator, title = gettext(operator), type = "number")
    }

    table$addColumnInfo(name = standards, title = gettext(standards), type = "number")

    standCorrVector <- vector(mode = "numeric")
    tableColumns <- list()
    for (i in 1:length(operatorVector)) {
      corrVector <- vector(mode = "numeric")
      operator1 <- subset(dataset, dataset[operators] == operatorVector[i])
      for (j in 1:length(operatorVector)) {
        if (j == i) {
          corrVector <- c(corrVector, 1)
        }else{
          operator2 <- subset(dataset, dataset[operators] == operatorVector[j])
          kt <- psych::corr.test(method = "kendall", x = operator1[[measurements]], y = operator2[[measurements]])
          corrVector <- c(corrVector, kt$r)
        }
      }
      tableColumns[[operatorVector[i]]] <- corrVector

      if (standards != ""){
        kt <- psych::corr.test(method = "kendall", x = operator1[[measurements]], y = as.numeric(operator1[[standards]]))
        standCorrVector <- c(standCorrVector, kt$r)
      }
    }

    tableColumns[["Operator"]] <- operatorVector
    if (standards != "")
      tableColumns[[standards]] <- standCorrVector
    table$setData(tableColumns)
  }

  return(table)
}

.msaCheckErrors <- function(dataset, options) {

  #if (options[["gaugeScatterPlotOperators"]]) {
  #  .hasErrors(dataset = dataset, type = "factorLevels",
  #             factorLevels.target  = options$operators, factorLevels.amount  = "> 2",
  #             exitAnalysisIfErrors = TRUE)
  #}

}

.AAACI <- function(m,N){
  v1_L <- m * 2
  v2_L <- 2 * (N - m + 1)

  v1_U <- 2*(m + 1)
  v2_U <- 2*(N - m)

  return(
    list(
      lower = round((v1_L * qf(0.025,v1_L,v2_L))/(v2_L + v1_L*qf(0.025,v1_L,v2_L)) * 100,3) ,
      upper = round((v1_U * qf(0.975, v1_U,v2_U))/(v2_U + v1_U*qf(0.975,v1_U,v2_U)) * 100,3)
    )
  )
}

.decisionNote <- function(vec, type = "Effectiveness") {

  if (type == "Effectiveness")
    decisionCriterions <- c(80,90)
  else if (type == "Miss")
    decisionCriterions <- c(5,2)
  else
    decisionCriterions <- c(10, 5)

  decisionVec = vector()
  if (type == "Effectiveness"){
    for (i in 1:length(vec)){
      if (vec[i] < decisionCriterions[1])
        decisionVec[i] = gettextf("%g (Unacceptable)", round(vec[i],2))
      else if (vec[i] >= decisionCriterions[1] & vec[i] <= decisionCriterions[2])
        decisionVec[i] = gettextf("%g (Marginally acceptable)", round(vec[i],2))
      else
        decisionVec[i] = gettextf("%g (Acceptable)", round(vec[i],2))
    }
  } else{
    for (i in 1:length(vec)){
      if (vec[i] > decisionCriterions[1])
        decisionVec[i] = gettextf("%g (Unacceptable)", round(vec[i],2))
      else if (vec[i] <= decisionCriterions[1] & vec[i] >= decisionCriterions[2])
        decisionVec[i] = gettextf("%g (Marginally acceptable)", round(vec[i],2))
      else
        decisionVec[i] = gettextf("%g (Acceptable)", round(vec[i],2))
    }
  }

  return(decisionVec)
}
