
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
msaAttributeInternal <- function(jaspResults, dataset, options, ...) {

  wideFormat <- options[["dataFormat"]] == "wideFormat"
  if (wideFormat){
    measurements <- unlist(options[["measurementsWideFormat"]])
    parts <- unlist(options[["partWideFormat"]])
    operators <- unlist(options[["operatorWideFormat"]])
    standards <- unlist(options[["standardWideFormat"]])
  } else {
    measurements <- unlist(options[["measurementLongFormat"]])
    parts <- unlist(options[["partLongFormat"]])
    operators <- unlist(options[["operatorLongFormat"]])
    standards <- unlist(options[["standardLongFormat"]])
  }

  numeric.vars <- measurements
  numeric.vars <- numeric.vars[numeric.vars != ""]
  factor.vars <- c(parts, operators, standards)
  factor.vars <- factor.vars[factor.vars != ""]

  # Ready
  if (wideFormat){
    ready <- (length(measurements) != 0 && !identical(operators, "") && !identical(parts, ""))
  } else {
    ready <- (!identical(measurements, "") && !identical(operators, "") && !identical(parts, ""))
  }


  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(c(numeric.vars, factor.vars))
  }

  # The results lists only accept nominal variables, so numeric ratings arrive as factors:
  # give them their numbers back, which Kendall's tau and the rating correlations need.
  for (measurement in numeric.vars)
    dataset[[measurement]] <- .msaAttributeAsNumberIfPossible(dataset[[measurement]])

  # Check for missing values and infinity
  .hasErrors(dataset, type = c('infinity', 'missingValues'),
             all.target = c(measurements, standards, operators, parts),
             exitAnalysisIfErrors = TRUE)

  if (ready)
    .msaAttributeCheckDesign(dataset, operators, parts, standards, wideFormat)

  if (!wideFormat && ready) {
    # Trial number of each rating within its appraiser and part, in data order.
    trial   <- stats::ave(seq_len(nrow(dataset)), .msaAttributeKey(dataset[[operators]]), .msaAttributeKey(dataset[[parts]]),
                          FUN = seq_along)
    dataset <- cbind(dataset, data.frame(index = paste0("V", trial)))
    dataset <- tidyr::spread(dataset, index, measurements)
    measurements <- paste0("V", seq_len(max(trial)))

    if (standards != "")
      dataset <- dataset[,c(operators, parts, measurements,standards)]
    else
      dataset <- dataset[,c(operators, parts, measurements)]
  }

  # Agreement between appraisers, Fleiss' kappa between appraisers, Kendall's tau and the
  # rating correlations compare the appraisers' rows position by position.
  if (ready)
    dataset <- .msaAttributeOrderByAppraiserAndPart(dataset, operators, parts)


  # Error handling

  if (identical(standards, "") && !identical(options[["positiveReference"]], "") && options[["cohensKappa"]]) {
    jaspResults[["tableReference"]] <- createJaspContainer(title = gettext("Reference tables and plots"))
    jaspResults[["tableReference"]]$position <- 10
    jaspResults[["tableReference"]]$dependOn(c("positiveReference", "standardLongFormat", "standardWideFormat"))

    Container <- jaspResults[["tableReference"]]

    tableReference <- createJaspTable(title = gettext("Reference tables and plots"))
    tableReference$setError(gettext("Please insert a reference value before specifying a positive reference."))

    Container[["TableError"]] <- tableReference

    return()
  }


  # Attribute Agreement Analysis Table & Graph
  if (length(measurements) == 0) {
    if (is.null(jaspResults[["AAAtableGraphs"]])) {
      jaspResults[["AAAtableGraphs"]] <- createJaspContainer(gettext("Attributes agreement analysis"))
      jaspResults[["AAAtableGraphs"]]$position <- 16
    }
    jaspResults[["AAAtableGraphs"]] <- .aaaTableGraphs(ready = ready, dataset = dataset, measurements = measurements,
                                                       parts = parts, operators = operators, options =  options, standards = standards)
  }else{
    if (is.null(jaspResults[["AAAtableGraphs"]])) {
      jaspResults[["AAAtableGraphs"]] <- createJaspContainer(gettext("Attributes agreement analysis"))
      jaspResults[["AAAtableGraphs"]]$position <- 16
    }
    jaspResults[["AAAtableGraphs"]] <- .aaaTableGraphs(ready = ready, dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, standards = standards)
  }

  # Cohen's Kappa Operator vs Standard
  if (options[["cohensKappa"]] && ready) {
    if (is.null(jaspResults[["cohensKappa"]])) {
      jaspResults[["cohensKappa"]] <- createJaspContainer(gettext("Cohen's kappa"))
      jaspResults[["cohensKappa"]]$position <- 18
    }

    jaspResults[["cohensKappa"]] <- .cohensKappa(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, standards = standards)
    jaspResults[["cohensKappaCor"]] <- .corCohenTable(ready = ready, dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options)
  }


  # Fleiss' Kappa
  if (options[["fleissKappa"]] && ready) {
    if (is.null(jaspResults[["fleissKappa"]])) {
      jaspResults[["fleissKappa"]] <- createJaspContainer(gettext("Cohen's kappa"))
      jaspResults[["fleissKappa"]]$position <- 19
    }

    jaspResults[["fleissKappa"]] <- .fleissKappa(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, standards = standards)

  }

  # Kendall Tau
  if (options[["kendallsTau"]] && ready) {
    if (is.null(jaspResults[["KendallTau"]])) {
      jaspResults[["KendallTau"]] <- createJaspContainer(gettext("Kendall's tau"))
      jaspResults[["KendallTau"]]$position <- 20
    }

    jaspResults[["KendallTau"]] <- .kendallTau(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, standards = standards, ready = ready)


  }
  return()
}


.cohensKappa <- function(dataset, measurements, parts, operators, standards, options) {

  dataset <- tidyr::gather(dataset, Repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

  table <- createJaspTable(title = gettext("Cohen's kappa for appraiser vs standard"))

  table$dependOn(c("cohensKappa"))

  table$addColumnInfo(name = "Appraiser",  title = gettext("Appraiser"), type = "string")
  table$addColumnInfo(name = "CK", title = gettext("Cohen's kappa"), type = "number")

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

  table <- createJaspTable(title = gettext("Correlations between appraisers' ratings"))
  table$dependOn(c("cohensKappa"))
  table$addColumnInfo(name = "appraiserVector",  title = gettext("Appraiser"), type = "string")

  appraiserVector <- vector(mode = "character")
  listRatings <- list()
  for (i in 1:length(unique(dataset[[operators]]))) {
    appraiser <- as.character(unique(dataset[[operators]])[i])
    appraiserVector[i] <- appraiser
    onlyAppraiser <- subset(dataset, dataset[operators] == appraiser)

    DataPerAppriaser <- as.matrix(onlyAppraiser[measurements])

    if (!is.numeric(DataPerAppriaser))
      DataPerAppriaser <- (DataPerAppriaser == options[["positiveReference"]]) * 1

    listRatings[[appraiser]] <- DataPerAppriaser
    table$addColumnInfo(name = appraiser,  title = gettext(appraiser), type = "integer")

  }

  corMatrix <- diag(length(appraiserVector))
  dimnames(corMatrix) <- list(appraiserVector, appraiserVector)
  for (i in seq_along(appraiserVector))
    for (j in seq_along(appraiserVector)[-i])
      corMatrix[i, j] <- .msaAttributeAllTrialPairsCor(listRatings[[i]], listRatings[[j]])

  cors <- round(corMatrix, 2)
  diag(cors) <- "-"
  cors <- cbind(appraiserVector, cors)

  if (!any(options[["positiveReference"]] == as.character(unique(unlist(dataset[measurements])))) && !identical(options[["positiveReference"]], "") && !options[["kendallsTau"]])
    table$setError(gettext("Please inseret a vaild positive reference as used in the results variables."))


  table$setData(cors)
  return(table)
}

# Correlation of two appraisers' ratings (parts x trials, rows matched by part), pairing every trial of
# one appraiser with every trial of the other on the same part. Trial numbers only label repeated ratings
# of a part, so pairing trial k with trial k would depend on them; with one trial both are the same.
.msaAttributeAllTrialPairsCor <- function(ratings1, ratings2) {
  pairs <- expand.grid(trial1 = seq_len(ncol(ratings1)), trial2 = seq_len(ncol(ratings2)))
  x     <- as.vector(ratings1[, pairs[["trial1"]], drop = FALSE])
  y     <- as.vector(ratings2[, pairs[["trial2"]], drop = FALSE])
  return(stats::cor(x, y))
}

# irr::kappam.fleiss (>= 0.85) drops a single-column data.frame to a vector in its
# internal NA-omit step, so ncol() is NULL and it errors on `1:nr`. Degenerate
# single-column input has no within-agreement; return NaN as older irr versions did.
.fleissKappaValue <- function(ratings) {
  value <- try(irr::kappam.fleiss(ratings)$value, silent = TRUE)
  if (jaspBase::isTryError(value))
    return(NaN)
  return(value)
}

.fleissKappa <- function(dataset, measurements, parts, operators, standards, options) {

  table <- createJaspTable(title = gettext("Fleiss' kappa"))

  table$dependOn(c("fleissKappa"))

  table$addColumnInfo(name = "appraiser",  title = gettext("Appraiser"), type = "string")
  table$addColumnInfo(name = "within", title = gettext("Within appraisers"), type = "number")
  if (standards != "")
    table$addColumnInfo(name = "vsStandard", title = gettext("Appraiser vs standard"), type = "number")
  table$addColumnInfo(name = "between", title = gettext("Between appraisers"), type = "number")

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
    kappaWithinVector[i] <- .fleissKappaValue(onlyAppraiser[measurements])

    # Versus Standard: as in Minitab, the kappa of each trial with the standard as another trial, averaged
    # over trials. It depends on which rating is trial k (long format: the k-th row of that appraiser and part).
    if (standards != "") {
      Kappa0 <- NULL
      count = 0
      for (j in measurements){
        count = count + 1
        Kappa0[count] <- .fleissKappaValue(cbind(onlyAppraiser[j], onlyAppraiser[[standards]]))
      }
      kappaStandardVector[i] <- mean(Kappa0)
    }

    # Between
    listBetween[[i]] <- onlyAppraiser[measurements]
  }

  kappaBetweenVector <- c(rep(NA,length(unique(dataset[[operators]]))), .fleissKappaValue(as.data.frame(listBetween)))
  appraiserVector <- c(appraiserVector, 'All')

  if (standards != "") {
    kappaStandardVector <- c(kappaStandardVector, mean(kappaStandardVector))
    table$setData(list(      "appraiser"       = appraiserVector,
                             "within"          = kappaWithinVector,
                             "vsStandard"      = kappaStandardVector,
                             "between"         = kappaBetweenVector))
    if (options[["dataFormat"]] == "longFormat" && length(measurements) > 1)
      table$addFootnote(gettext("Averaged over trials; trial k is the k-th row of an appraiser and part in the data."),
                        colNames = "vsStandard")
  }
  else{
    table$setData(list(      "appraiser"       = appraiserVector,
                             "within"          = kappaWithinVector,
                             "between"         = kappaBetweenVector))
  }

  return(table)
}

.aaaTableGraphs <- function(ready, dataset, measurements, parts, operators, standards, options) {

  AAA <- createJaspContainer(gettext("Attributes agreement analysis"))
  AAA$dependOn(c("measurementsWideFormat", "measurementLongFormat", "partWideFormat", "partLongFormat", "operatorWideFormat",
                 "operatorLongFormat", "standardLongFormat", "standardWideFormat"))

  if (standards != "") {
    tableWithin <- createJaspTable(title = gettext("Within appraisers"))
    tableBetween <- createJaspTable(title = gettext("Between appraisers"))
    tableEachVsStandard <- createJaspTable(title = gettext("Each appraiser vs standard"))
    tableAllVsStandard <- createJaspTable(title = gettext("All appraisers vs standard"))


    allTables <- list(tableWithin, tableBetween, tableEachVsStandard, tableAllVsStandard)

    for (table in allTables[c(1,3)]) {
      table$addColumnInfo(name = "Appraiser",  title = gettext("Appraiser"), type = "string")
    }

    for (table in allTables) {
      table$addColumnInfo(name = "Inspected", title = gettext("Inspected"), type = "integer")
      table$addColumnInfo(name = "Matched", title = gettext("Matched"), type = "integer")
      table$addColumnInfo(name = "Percent", title = gettext("Percent"), type = "number")
      table$addColumnInfo(name = "CIL", title = gettext("Lower"), type = "integer", overtitle = gettextf("Confidence interval of 95%%"))
      table$addColumnInfo(name = "CIU", title = gettext("Upper"), type = "integer", overtitle = gettextf("Confidence interval of 95%%"))
    }

    if (ready) {
      appraiserVector <- as.character(unique(dataset[[operators]]))
      numberInspected <- length(unique(dataset[[parts]]))

      if ((length(unique(unlist(dataset[measurements]))) != 2 | length(unique(dataset[[standards]])) != 2) && !options[["kendallsTau"]] && !identical(options[["positiveReference"]], "")) {
        table$setError(gettext("Invalid reference and/or results were inserted."))
        return(table)
      }

      for (measurement in measurements) {
        dataset[measurement] <- as.character(dataset[[measurement]])
        dataset[standards] <- as.character(dataset[[standards]])
      }

      matchesWithin <- vector(mode = "numeric")

      for (i in 1:length(appraiserVector)) {
        onlyAppraiser <- subset(dataset, dataset[operators] == appraiserVector[i])

        if (any(is.na(onlyAppraiser[measurements]))) {
          table$setError(gettextf("Invalid values (NA) found in appraiser %s.", appraiserVector[i]))
          return(table)
        }
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

      if (length(measurements) == 1 & !options[["kendallsTau"]]) {
        tableWithin$setError(gettext("More than 1 measurement per operator required."))
      }else{
        tableDecisions <- createJaspTable(title = gettext("Study effectiveness summary"))
        tableDecisions$addColumnInfo(name = "Appraiser", title = gettext("Appraiser"), type = "string")
        tableDecisions$addColumnInfo(name = "Effectiveness", title = gettext("Effectiveness"), type = "string")
        tableDecisions$addColumnInfo(name = "Miss", title = gettext("Miss rate"), type = "string")
        tableDecisions$addColumnInfo(name = "False", title = gettext("False alarm rate"), type = "string")

        if (!any(options[["positiveReference"]] == as.character(unique(unlist(dataset[measurements])))) && !identical(options[["positiveReference"]], "") && !options[["kendallsTau"]])
          tableDecisions$setError(gettext("Please inseret a vaild positive reference as used in the results variables."))

        if (!options[["kendallsTau"]] && !identical(standards, "") && !identical(options[["positiveReference"]], "") && any(options[["positiveReference"]] == dataset[measurements])) {
          PositiveRef <- options[["positiveReference"]]
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
          tableDecisions$addFootnote(gettextf("Acceptable: x >= 90%% (effectiveness), x =< 2%% (miss rate), x =< 5%% (false alarm rate)"))
          tableDecisions$addFootnote(gettextf("Marginally acceptable: x >= 80%% (effectiveness), x =< 5%% (miss rate), x =< 10%% (false alarm rate)"))
          tableDecisions$addFootnote(gettextf("Unacceptable: x < 80%% (effectiveness), x > 5%% (miss rate), x > 10%% (false alarm rate)"))
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
        plotWithin <- createJaspPlot(title = "Within Appraisers", width = 500, height = 500)

        withinDataframe <- data.frame(x = appraiserVector, y = percentWithin)

        pw <- ggplot2::ggplot(withinDataframe, ggplot2::aes(x = x, y = y)) +
          jaspGraphs::geom_point() +
          ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10))+
          ggplot2::geom_errorbar(ggplot2::aes(ymin = c(CIWithin$lower),
                                              ymax = c(CIWithin$upper))) +
          jaspGraphs::geom_rangeframe() +
          jaspGraphs::themeJaspRaw() +
          ggplot2::ylab("Percent") +
          ggplot2::xlab("Appraiser")
        plotWithin$plotObject <- pw

        AAA[["PlotWithin"]] <- plotWithin
      }

      plotVs <- createJaspPlot(title = "Each Appraiser vs Standard", width = 500, height = 500)

      vsDataframe <- data.frame(x = appraiserVector, y = percentEachVsStandard)

      pvs <- ggplot2::ggplot(vsDataframe, ggplot2::aes(x = x, y = y)) +
        jaspGraphs::geom_point() +
        ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = c(CIEachVsStandard$lower),
                                            ymax = c(CIEachVsStandard$upper))) +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw() +
        ggplot2::ylab("Percent") +
        ggplot2::xlab("Appraiser")

      plotVs$plotObject <- pvs

      AAA[["PlotVs"]] <- plotVs
    }

  } else{
    tableWithin <- createJaspTable(title = gettext("Within appraisers"))
    tableBetween <- createJaspTable(title = gettext("Between appraisers"))


    allTables <- list(tableWithin, tableBetween)

    for (table in allTables[1:2]) {
      table$addColumnInfo(name = "Appraiser",  title = gettext("Appraiser"), type = "string")
    }

    for (table in allTables) {
      table$addColumnInfo(name = "Inspected", title = gettext("Inspected"), type = "integer")
      table$addColumnInfo(name = "Matched", title = gettext("Matched"), type = "integer")
      table$addColumnInfo(name = "Percent", title = gettext("Percent"), type = "number")
      table$addColumnInfo(name = "CIL", title = gettext("Lower"), type = "integer", overtitle = gettextf("Confidence interval of 95%%"))
      table$addColumnInfo(name = "CIU", title = gettext("Upper"), type = "integer", overtitle = gettextf("Confidence interval of 95%%"))

    }

    if (ready) {
      appraiserVector <- as.character(unique(dataset[[operators]]))
      numberInspected <- length(unique(dataset[[parts]]))

      for (measurement in measurements) {
          dataset[measurement] <- as.character(dataset[[measurement]])
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

      if (length(measurements) == 1 && !options[["kendallsTau"]]) {
        tableWithin$setError(gettext("More than one measurement per operator required."))
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

    AAA <- createJaspContainer(gettext("Attributes agreement analysis"))

    AAA$dependOn(c("measurementsWideFormat", "measurementLongFormat", "partWideFormat", "partLongFormat", "operatorWideFormat",
                   "operatorLongFormat", "standardLongFormat", "standardWideFormat"))

    AAA[["Within"]] <- tableWithin
    AAA[["Between"]] <- tableBetween

    if (ready) {

      if (length(measurements) > 1) {
        plotWithin <- createJaspPlot(title = "Within Appraisers", width = 500, height = 500)

        withinDataframe <- data.frame(x = appraiserVector, y = percentWithin)

        pw <- ggplot2::ggplot(withinDataframe, ggplot2::aes(x = x, y = y)) +
          jaspGraphs::geom_point() +
          ggplot2::ylab("Percent") +
          ggplot2::xlab("Appraiser") +
          ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = c(CIWithin$lower),
                                              ymax = c(CIWithin$upper))) +
          jaspGraphs::geom_rangeframe() +
          jaspGraphs::themeJaspRaw()

        plotWithin$plotObject <- pw

        AAA[["PlotWithin"]] <- plotWithin
      }
    }
  }

  if (options[["kendallsTau"]]) {
    AAA[["Within"]] <- NULL
    AAA[["Between"]] <- tableBetween

    if (standards != ""){
      AAA[["EachVsStandard"]] <- tableEachVsStandard
      AAA[["AllVsStandard"]] <- tableAllVsStandard
    }

  } else {
    AAA[["Within"]] <- tableWithin
    AAA[["Between"]] <- tableBetween

    if (standards != ""){
      AAA[["EachVsStandard"]] <- tableEachVsStandard
      AAA[["AllVsStandard"]] <- tableAllVsStandard
    }

    if (!identical(options[["standard"]], "") && !identical(options[["positiveReference"]], "") && length(measurements) > 1)
      AAA[["StudyEffectiveness"]] <- tableDecisions
  }
  return(AAA)
}

# One integer per distinct ID, in sorted order. Checks, trial numbering and sorting all use this key, so
# they agree on which rows hold the same part (no locale collation ties, no 15-digit string rounding).
.msaAttributeKey <- function(x) {
  return(match(x, sort(unique(x))))
}

# Cross-appraiser agreement compares the appraisers' ratings part by part, so every appraiser must rate
# every part equally often (wide format: in one row), and each part needs one reference value. Checked on
# the raw data, as the long-to-wide reshape fails or hides missing trials as NA otherwise.
.msaAttributeCheckDesign <- function(dataset, operators, parts, standards, wideFormat) {
  keys <- data.frame(appraiser = .msaAttributeKey(dataset[[operators]]),
                     part      = .msaAttributeKey(dataset[[parts]]))

  if (!.checkIfCrossed(keys, "appraiser", "part"))
    .quitAnalysis(gettext("Each appraiser must rate each part, and each part the same number of times."))

  if (wideFormat && any(table(keys) != 1))
    .quitAnalysis(gettext("Each appraiser must have exactly one row per part."))

  if (standards != "" && any(tapply(dataset[[standards]], keys[["part"]], function(x) length(unique(x)) > 1)))
    .quitAnalysis(gettext("Each part must have a single reference value."))
}

# Rows ordered by appraiser (in order of appearance), then by part, so that row k of every
# appraiser holds the same part even when the data are in run order.
.msaAttributeOrderByAppraiserAndPart <- function(dataset, operators, parts) {
  appraiserKey <- match(dataset[[operators]], unique(dataset[[operators]]))
  partKey      <- .msaAttributeKey(dataset[[parts]])
  return(dataset[order(appraiserKey, partKey), , drop = FALSE])
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


# A factor (or character vector) whose values are all numbers, e.g. ratings 1 to 5, as those numbers;
# anything else unchanged. as.numeric() on the factor itself would give the level codes instead.
.msaAttributeAsNumberIfPossible <- function(x) {
  if (is.numeric(x))
    return(x)

  values  <- as.character(x)
  numbers <- suppressWarnings(as.numeric(values))

  if (all(is.na(numbers) == is.na(values)))
    return(numbers)

  return(x)
}

.kendallTau <- function(dataset, measurements, parts, operators, standards, options, ready) {

  operatorVector <- as.character(unique(dataset[[operators]]))

  table <- createJaspTable(title = gettext("Kendall's tau"))

  table$dependOn(c("kendallsTau", "measurementsWideFormat", "measurementLongFormat", "partWideFormat", "partLongFormat",
                   "operatorWideFormat", "operatorLongFormat", "standardLongFormat", "standardWideFormat"))

  if (!ready & options[["kendallsTau"]])
    return(table)

  if (!all(vapply(dataset[measurements], is.numeric, logical(1)))) {
    table$setError(gettext("Kendall's tau is only available for numeric measurements."))
  }  else if (length(unique(unlist(dataset[measurements]))) <= 2) {
    table$setError(gettext("Kendall's tau is only available for non-binary measurements."))
  } else{
    table$addColumnInfo(name = "Operator",  title = gettext("Operator"), type = "string")

    for (operator in operatorVector) {
      table$addColumnInfo(name = operator, title = gettext(operator), type = "number")
    }

    if (standards != "")
      table$addColumnInfo(name = standards, title = gettext(standards), type = "number")

    # Ratings and reference matched by part ID: row p of every matrix holds part p.
    partKey   <- .msaAttributeKey(dataset[[parts]])
    partIds   <- sort(unique(partKey))
    ratings   <- lapply(operatorVector, function(operator) {
      rows <- which(dataset[[operators]] == operator)
      as.matrix(dataset[rows[match(partIds, partKey[rows])], measurements, drop = FALSE])
    })
    if (standards != "")
      reference <- as.numeric(.msaAttributeAsNumberIfPossible(dataset[[standards]]))[match(partIds, partKey)]

    standCorrVector <- vector(mode = "numeric")
    tableColumns <- list()
    for (i in 1:length(operatorVector)) {
      corrVector <- vector(mode = "numeric")
      for (j in 1:length(operatorVector)) {
        if (j == i) {
          corrVector <- c(corrVector, 1)
        }else{
          corrVector <- c(corrVector, .kendallTauBetweenAppraisers(ratings[[i]], ratings[[j]]))
        }
      }
      tableColumns[[operatorVector[i]]] <- corrVector

      if (standards != ""){
        # Minitab: tau-b of each trial with the standard, averaged over trials.
        standCorrVector <- c(standCorrVector, mean(apply(ratings[[i]], 2, stats::cor, y = reference, method = "kendall")))
      }
    }

    tableColumns[["Operator"]] <- operatorVector
    if (standards != "")
      tableColumns[[standards]] <- standCorrVector
    table$setData(tableColumns)

    if (length(measurements) > 1) {
      if (length(operatorVector) > 1)
        table$addFootnote(gettext("Between appraisers: Kendall's tau-b over all combinations of the two appraisers' ratings of each part."))
      if (standards != "")
        table$addFootnote(gettext("Appraiser vs standard: Kendall's tau-b of each trial with the standard, averaged over trials."))
    }
  }

  return(table)
}

# Kendall's tau-b between two appraisers' ratings (parts x trials, rows matched by part). Every rating of a
# part by one appraiser is paired with every rating of that part by the other, so the value does not depend
# on trial labels or order; with one trial each it is the ordinary tau-b. Computed without expanding the
# pairs: pairs within a part add nothing to C - D, pairs of parts p, q add signSums(x)[p, q] * signSums(y)[p, q].
.kendallTauBetweenAppraisers <- function(x, y) {
  signSums <- function(m) {
    s <- matrix(0, nrow(m), nrow(m))
    for (i in seq_len(ncol(m))) for (k in seq_len(ncol(m)))
      s <- s + outer(m[, i], m[, k], ">") - outer(m[, i], m[, k], "<")
    return(s)
  }
  # pairs tied on one appraiser's ratings (exact equality); each rating occurs once per rating of the other appraiser
  tiedPairs <- function(m, copies) {
    counts <- as.numeric(tabulate(match(m, m))) * copies
    return(sum(counts * (counts - 1) / 2))
  }
  sx <- signSums(x)
  sy <- signSums(y)
  concordance <- sum((sx * sy)[upper.tri(sx)])
  pairs       <- choose(nrow(x) * ncol(x) * ncol(y), 2)
  denominator <- sqrt((pairs - tiedPairs(x, ncol(y))) * (pairs - tiedPairs(y, ncol(x))))
  if (denominator == 0)
    return(NA_real_)
  return(concordance / denominator)
}

.msaCheckErrors <- function(dataset, options) {

  #if (options[["gaugeScatterPlotOperators"]]) {
  #  .hasErrors(dataset = dataset, type = "factorLevels",
  #             factorLevels.target  = options$operators, factorLevels.amount  = "> 2",
  #             exitAnalysisIfErrors = TRUE)
  #}

}

.AAACI <- function(m, N) {
  count <- max(length(m), length(N))
  if (!length(m) || !length(N))
    return(list(lower = rep(NA_real_, count), upper = rep(NA_real_, count)))
  if (!(length(m) %in% c(1L, count)) || !(length(N) %in% c(1L, count)))
    stop(gettext("Agreement counts and totals must have compatible lengths."))
  m <- rep_len(m, count)
  N <- rep_len(N, count)
  valid <- is.finite(m) & is.finite(N) & N > 0 & N == floor(N) &
    m >= 0 & m <= N & m == floor(m)
  lower <- upper <- rep(NA_real_, count)

  # Fixed 95% Clopper-Pearson interval, equivalent to the interior F-quantile formula.
  # No interval is defined for zero observations or invalid binomial counts.
  lower[valid & m == 0] <- 0
  upper[valid & m == N] <- 1
  lowerIndex <- valid & m > 0
  upperIndex <- valid & m < N
  lower[lowerIndex] <- stats::qbeta(0.025, m[lowerIndex], N[lowerIndex] - m[lowerIndex] + 1)
  upper[upperIndex] <- stats::qbeta(0.975, m[upperIndex] + 1, N[upperIndex] - m[upperIndex])

  return(list(lower = round(lower * 100, 3), upper = round(upper * 100, 3)))
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
        decisionVec[i] = gettextf("%g (unacceptable)", round(vec[i],2))
      else if (vec[i] >= decisionCriterions[1] & vec[i] <= decisionCriterions[2])
        decisionVec[i] = gettextf("%g (marginally acceptable)", round(vec[i],2))
      else
        decisionVec[i] = gettextf("%g (acceptable)", round(vec[i],2))
    }
  } else{
    for (i in 1:length(vec)){
      if (vec[i] > decisionCriterions[1])
        decisionVec[i] = gettextf("%g (unacceptable)", round(vec[i],2))
      else if (vec[i] <= decisionCriterions[1] & vec[i] >= decisionCriterions[2])
        decisionVec[i] = gettextf("%g (marginally acceptable)", round(vec[i],2))
      else
        decisionVec[i] = gettextf("%g (acceptable)", round(vec[i],2))
    }
  }

  return(decisionVec)
}
