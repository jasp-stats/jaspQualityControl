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

msaAttribute <- function(jaspResults, dataset, options, ...){

  measurements <- unlist(options$measurements)
  parts <- unlist(options$parts)
  operators <- unlist(options$operators)
  standards <- unlist(options$standard)


  ready <- (length(measurements) != 0 & operators != "" & standards != "" & parts != "")
  numeric.vars <- measurements

  factor.vars <- c(parts, operators, standards)
  factor.vars <- factor.vars[factor.vars != ""]

  #if(length(measurements) == 0)
  #  return()

  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = numeric.vars, columns.as.factor = factor.vars)
  }

  .msaCheckErrors(dataset, options)



  # Cohen's Kappa Operator vs Standard
  if (options[["AAAcohensKappa"]]) {
    if(is.null(jaspResults[["cohensKappa"]])) {
      jaspResults[["cohensKappa"]] <- createJaspContainer(gettext("Cohen's Kappa"))
      jaspResults[["cohensKappa"]]$position <- 17
    }

    jaspResults[["cohensKappa"]] <- .cohensKappa(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, standards = standards)

  }


  # Fleiss' Kappa
  if (options[["AAAfleissKappa"]]) {
    if(is.null(jaspResults[["fleissKappa"]])) {
      jaspResults[["fleissKappa"]] <- createJaspContainer(gettext("Cohen's Kappa"))
      jaspResults[["fleissKappa"]]$position <- 18
    }

    jaspResults[["fleissKappa"]] <- .fleissKappa(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, standards = standards)

  }

  # Attribute Agreement Analysis Table & Graph
  if(length(measurements) == 0){
    if(is.null(jaspResults[["AAAtableGraphs"]])) {
      jaspResults[["AAAtableGraphs"]] <- createJaspContainer(gettext("Attribute Agreement Analysis"))
      jaspResults[["AAAtableGraphs"]]$position <- 19
    }
    jaspResults[["AAAtableGraphs"]] <- .aaaTableGraphs(ready = ready, dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, standards = standards)
  }else{
    if(is.null(jaspResults[["AAAtableGraphs"]])) {
      jaspResults[["AAAtableGraphs"]] <- createJaspContainer(gettext("Attribute Agreement Analysis"))
      jaspResults[["AAAtableGraphs"]]$position <- 19
    }
    jaspResults[["AAAtableGraphs"]] <- .aaaTableGraphs(ready = ready, dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, standards = standards)
  }

  # Kendall Tau
  if(options[["AAAkendallTau"]]){
    if(is.null(jaspResults[["KendallTau"]])) {
      jaspResults[["KendallTau"]] <- createJaspContainer(gettext("Kendall's Tau"))
      jaspResults[["KendallTau"]]$position <- 20
    }

    jaspResults[["KendallTau"]] <- .kendallTau(dataset = dataset, measurements = measurements, parts = parts, operators = operators, options =  options, standards = standards)


  }
  return()
}

.cohensKappa <- function(dataset, measurements, parts, operators, standards, options){

  dataset <- tidyr::gather(dataset, Repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

  table <- createJaspTable(title = gettext("Cohen's Kappa for Appraiser vs Standard"))

  table$dependOn(c("AAAcohensKappa"))

  table$addColumnInfo(name = "Appraiser",  title = gettext("Appraiser"), type = "string")
  table$addColumnInfo(name = "CK", title = gettext("Cohen's Kappa"), type = "number")

  appraiserVector <- vector(mode = "character")
  kappaVector <- vector(mode = "numeric")
  for(i in 1:length(unique(dataset[[operators]]))){
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

.fleissKappa <- function(dataset, measurements, parts, operators, standards, options){

  table <- createJaspTable(title = gettext("Fleiss' Kappa"))

  table$dependOn(c("AAAfleissKappa"))

  table$addColumnInfo(name = "appraiser",  title = gettext("Appraiser"), type = "string")
  table$addColumnInfo(name = "within", title = gettext("Within Appraisers"), type = "number")
  table$addColumnInfo(name = "vsStandard", title = gettext("Appraiser vs Standard"), type = "number")
  table$addColumnInfo(name = "between", title = gettext("Between Appraisers"), type = "number")

  appraiserVector <- vector(mode = "character")
  kappaWithinVector <- vector(mode = "numeric")
  kappaBetweenVector <- vector(mode = "numeric")
  kappaStandardVector <- vector(mode = "numeric")

  for(i in 1:length(unique(dataset[[operators]]))){
    appraiser <- as.character(unique(dataset[[operators]])[i])
    appraiserVector[i] <- appraiser
    onlyAppraiser <- subset(dataset, dataset[operators] == appraiser)
    fkappa <- irr::kappam.fleiss(onlyAppraiser[measurements])
    kappaWithinVector[i] <- fkappa$value
    kappaBetweenVector[i] <- NA
  }

  datasetLong <- tidyr::gather(dataset, Repetition, Measurement, measurements[1]:measurements[length(measurements)], factor_key=TRUE)

  for(i in 1:length(unique(datasetLong[[operators]]))){
    appraiser <- as.character(unique(datasetLong[[operators]])[i])
    onlyAppraiser <- subset(datasetLong, datasetLong[[operators]] == appraiser)
    kappaFrame <- data.frame(standard = onlyAppraiser[[standards]], measurement = onlyAppraiser[["Measurement"]])
    fkappa <- irr::kappam.fleiss(kappaFrame)
    kappaStandardVector[i] <- fkappa$value
  }

  reshapeData <- data.frame(rep(NA,nrow(subset(datasetLong, datasetLong[[operators]] == unique(datasetLong[[operators]])[1]))))
  for(i in 1:length(unique(datasetLong[[operators]]))){
    appraiser <- as.character(unique(datasetLong[[operators]])[i])
    reshapeData[,i] <- subset(datasetLong, datasetLong[operators] == appraiser)['Measurement']
  }
  betweenKappa <- irr::kappam.fleiss(reshapeData)
  kappaBetweenVector <- c(kappaBetweenVector, betweenKappa$value)
  allKappa <- irr::kappam.fleiss(data.frame(standard = datasetLong[standards], measurement = datasetLong["Measurement"]))
  kappaStandardVector <- c(kappaStandardVector, allKappa$value)
  appraiserVector <- c(appraiserVector, 'All')

  table$setData(list(      "appraiser"       = appraiserVector,
                           "within"          = kappaWithinVector,
                           "vsStandard"      = kappaStandardVector,
                           "between"         = kappaBetweenVector))

  return(table)
}

.aaaTableGraphs <- function(ready, dataset, measurements, parts, operators, standards, options){

  tableWithin <- createJaspTable(title = gettext("Within Appraisers"))
  tableEachVsStandard <- createJaspTable(title = gettext("Each Appraiser vs Standard"))
  tableBetween <- createJaspTable(title = gettext("Between Appraisers"))
  tableAllVsStandard <- createJaspTable(title = gettext("All Appraisers vs Standard"))

  allTables <- list(tableWithin, tableEachVsStandard, tableBetween, tableAllVsStandard)

  for(table in allTables[1:2]){
    table$addColumnInfo(name = "Appraiser",  title = gettext("Appraiser"), type = "string")
  }

  for(table in allTables){
    table$addColumnInfo(name = "Inspected", title = gettext("Inspected"), type = "integer")
    table$addColumnInfo(name = "Matched", title = gettext("Matched"), type = "integer")
    table$addColumnInfo(name = "Percent", title = gettext("Percent"), type = "number")

  }

  if(ready){
    appraiserVector <- as.character(unique(dataset[[operators]]))
    numberInspected <- length(unique(dataset[[parts]]))

    for(measurement in measurements){
      if(is.numeric(dataset[[measurement]])){
        dataset[measurement] <- as.character(dataset[[measurement]])
        dataset[standards] <- as.character(dataset[[standards]])
      }
    }

    matchesWithin <- vector(mode = "numeric")

    for(i in 1:length(appraiserVector)){
      onlyAppraiser <- subset(dataset, dataset[operators] == appraiserVector[i])
      matchesWithin[i] <- .countRowMatches(onlyAppraiser[measurements])
    }

    percentWithin <- matchesWithin / numberInspected* 100

    matchesEachVsStandard <- vector(mode = "numeric")

    for(i in 1:length(appraiserVector)){
      onlyAppraiser <- subset(dataset, dataset[operators] == appraiserVector[i])
      matchesEachVsStandard[i] <- .countRowMatches(onlyAppraiser[c(measurements, standards)])
    }


    percentEachVsStandard <- matchesEachVsStandard / numberInspected* 100

    reshapeData <- data.frame(subset(dataset, dataset[[operators]] == unique(dataset[[operators]])[1])[standards])
    for(i in 1:length(appraiserVector)){
      appraiser <- as.character(unique(dataset[[operators]])[i])
      reshapeData <- cbind(reshapeData, subset(dataset, dataset[operators] == appraiser)[measurements])
    }

    matchesBetween <- .countRowMatches(reshapeData[2:ncol(reshapeData)])
    percentBetween <- matchesBetween / numberInspected* 100

    matchesAllVsStandard <- .countRowMatches(reshapeData)
    percentAllVsStandard <- matchesAllVsStandard / numberInspected* 100

    if(length(measurements) == 1){
      tableWithin$setError(gettext("More than 1 Measurement per Operator required."))
    }else{
      tableWithin$setData(list(      "Appraiser"       = appraiserVector,
                                     "Inspected"       = rep(numberInspected, length(appraiserVector)),
                                     "Matched"         = matchesWithin,
                                     "Percent"         = percentWithin))
    }

    tableEachVsStandard$setData(list("Appraiser"     = appraiserVector,
                                     "Inspected"       = rep(numberInspected, length(appraiserVector)),
                                     "Matched"         = matchesEachVsStandard,
                                     "Percent"         = percentEachVsStandard))

    tableBetween$setData(list(     "Inspected"       = numberInspected,
                                   "Matched"         = matchesBetween,
                                   "Percent"         = percentBetween))

    tableAllVsStandard$setData(list("Inspected"      = numberInspected,
                                    "Matched"         = matchesAllVsStandard,
                                    "Percent"         = percentAllVsStandard))
  }

  AAA <- createJaspContainer(gettext("Attribute Agreement Analysis"))

  AAA$dependOn(c("measurements", "parts", "operators", "standard"))

  AAA[["Within"]] <- tableWithin
  AAA[["EachVsStandard"]] <- tableEachVsStandard
  AAA[["Between"]] <- tableBetween
  AAA[["AllVsStandard"]] <-tableAllVsStandard

  if(ready){

    if(length(measurements) > 1){
      plotWithin <- createJaspPlot(title = "Within Appraisers", width = 300, height = 400)

      withinDataframe <- data.frame(x = appraiserVector, y = percentWithin)

      pw <- ggplot2::ggplot(withinDataframe, ggplot2::aes(x = x, y = y)) + jaspGraphs::geom_point()

      pw <- jaspGraphs::themeJasp(pw) +
        ggplot2::ylab("Percent") +
        ggplot2::xlab("Appraiser") +
        ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10))

      plotWithin$plotObject <- pw

      AAA[["PlotWithin"]] <- plotWithin
    }

    plotVs <- createJaspPlot(title = "Each Appraiser vs Standard", width = 300, height = 400)

    vsDataframe <- data.frame(x = appraiserVector, y = percentEachVsStandard)

    pvs <- ggplot2::ggplot(vsDataframe, ggplot2::aes(x = x, y = y)) + jaspGraphs::geom_point()

    pvs <- jaspGraphs::themeJasp(pvs) +
      ggplot2::ylab("Percent") +
      ggplot2::xlab("Appraiser") +
      ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10))

    plotVs$plotObject <- pvs

    AAA[["PlotVs"]] <- plotVs
  }
  return(AAA)
}

.countRowMatches <- function(data){
  count <- 0
  for(i in 1:nrow(data)){
    matches <- unlist(data[i,]) == data[i,1]
    if(all(matches))
      count <- count + 1
  }
  return(count)
}


.kendallTau <- function(dataset, measurements, parts, operators, standards, options){

  operatorVector <- as.character(unique(dataset[[operators]]))

  table <- createJaspTable(title = gettext("Kendall's Tau"))

  table$dependOn(c("AAAkendallTau", "measurements", "parts", "operators", "standard"))

  if(!is.numeric(dataset[[measurements[1]]])){
    table$setError(gettext("Kendall's Tau is only available for numeric measurements."))
  }else{
    table$addColumnInfo(name = "Operator",  title = gettext("Operator"), type = "string")

    for(operator in operatorVector){
      table$addColumnInfo(name = operator, title = gettext(operator), type = "number")
    }

    table$addColumnInfo(name = standards, title = gettext(standards), type = "number")

    standCorrVector <- vector(mode = "numeric")
    tableColumns <- list()
    for(i in 1:length(operatorVector)){
      corrVector <- vector(mode = "numeric")
      operator1 <- subset(dataset, dataset[operators] == operatorVector[i])
      for(j in 1:length(operatorVector)){
        if(j == i){
          corrVector <- c(corrVector, 1)
        }else{
          operator2 <- subset(dataset, dataset[operators] == operatorVector[j])
          kt <- psych::corr.test(method = "kendall", x = operator1[[measurements]], y = operator2[[measurements]])
          corrVector <- c(corrVector, kt$r)
        }
      }
      kt <- psych::corr.test(method = "kendall", x = operator1[[measurements]], y = as.numeric(operator1[[standards]]))
      standCorrVector <- c(standCorrVector, kt$r)
      tableColumns[[operatorVector[i]]] <- corrVector
    }

    tableColumns[["Operator"]] <- operatorVector
    tableColumns[[standards]] <- standCorrVector
    table$setData(tableColumns)
  }

  return(table)
}

.msaCheckErrors <- function(dataset, options) {

  #if (options[["gaugeScatterPlotOperators"]]){
  #  .hasErrors(dataset = dataset, type = "factorLevels",
  #             factorLevels.target  = options$operators, factorLevels.amount  = "> 2",
  #             exitAnalysisIfErrors = TRUE)
  #}

}
