#
# Copyright (C) 2013-2021 University of Amsterdam
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

doeScreening <- function(jaspResults, dataset, options, ...){

  .doeScreeningSummarySelectedDesign(options, jaspResults, position = 1)

  .doeScreeningShowSelectedDesign(options, jaspResults, position = 2)

}

.doeScreeningSummarySelectedDesign <- function(options, jaspResults, position){

  if(is.null(jaspResults[["selectedDesignScreening"]])){
    table <- createJaspTable(gettext("Selected Design"))
    table$position <- position
    table$dependOn(options = c("screeningType",
                               "numberOfFactorsScreen3",
                               "numberOfFactorsScreen2",
                               "PBruns",
                               "screeningCenterPoints",
                               "screeningCornerReplicates",
                               "screeningRepeats"))

    table$addColumnInfo(name = 'type', title = "Design", type = 'string')
    table$addColumnInfo(name = 'factors', title = gettext("Factors"), type = 'integer')
    table$addColumnInfo(name = 'runs', title = gettext("Runs"), type = 'integer')
    table$addColumnInfo(name = 'resolution', title = gettext("Resolution"), type = 'string')
    table$addColumnInfo(name = 'centers', title = gettext("Centre points"), type = 'integer')
    if(options[["screeningType"]] == "PBdes")
      table$addColumnInfo(name = 'replicates', title = gettext("Replicates"), type = 'integer')

    rows <- data.frame(type = "Screening",
                       factors =
                         if(options[["screeningType"]] == "PBdes"){
                           options[["numberOfFactorsScreen2"]]
                         } else {
                           options[["numberOfFactorsScreen2"]] + options[["numberOfFactorsScreen3"]]
                         },
                       runs =
                         if(options[["screeningType"]] == "PBdes"){
                           (as.numeric(options[["PBruns"]]) + options[["screeningCenterPoints"]])*options[["screeningCornerReplicates"]]
                         } else {
                           nrow(daewr::DefScreen(options[["numberOfFactorsScreen3"]],
                                                 options[["numberOfFactorsScreen2"]],
                                                 center = if(options[["numberOfFactorsScreen2"]] == 0){ options[["screeningCenterPoints"]] } else { 0 }
                                                 )
                                )
                         },
                       resolution =
                         if(options[["screeningType"]] == "PBdes"){
                           "III"
                         } else { "IV" },
                       centers =
                         if(options[["screeningType"]] == "PBdes"){
                           options[["screeningCenterPoints"]]
                         } else if(options[["numberOfFactorsScreen2"]] == 0){
                           options[["screeningCenterPoints"]]
                           } else { 0 }
                       )
    if(options[["screeningType"]] == "PBdes")
      rows <- cbind.data.frame(rows, replicates = options[["screeningCornerReplicates"]])

    table$setData(rows)
    jaspResults[["selectedDesignScreening"]] <- table
  }
}

.doeScreeningShowSelectedDesign <- function(options, jaspResults, position){

  if(!options[["displayScreeningDesign"]]){
    jaspResults[["displayScreeningDesign"]] <- NULL
    return()
  }

  if(is.null(jaspResults[["displayScreeningDesign"]])){

    table <- createJaspTable(gettext("Design Preview"))
    table$position <- position

    table$dependOn(options = c("displayScreeningDesign",
                               "dataCodingScreen",
                               "runOrderScreen",
                               "screeningType",
                               "numberOfFactorsScreen3",
                               "numberOfFactorsScreen2",
                               "PBruns",
                               "factors3",
                               "factors2",
                               "screeningCenterPoints",
                               "screeningCornerReplicates",
                               "screeningRepeats",
                               "fileScreening",
                               "actualExporter"))

    if(options[["numberOfFactorsScreen2"]] > 0){
      twoLevels <- options[["factors2"]]
      factorNames2 <- factorLows2 <- factorHighs2 <- character()
      threePlus <- if(options[["screeningType"]] == "DSdes"){
        length(options[["factors3"]])
      } else {
        0
      }
      for(i in 1:length(twoLevels)){
        factorNames2[i] <- ifelse(twoLevels[[i]]$factorName2 == "", paste0(twoLevels[[i]]$factorName2, " (", (i+threePlus), ")"), twoLevels[[i]]$factorName2)
        factorLows2[i]  <- twoLevels[[i]]$low2
        factorHighs2[i] <- twoLevels[[i]]$high2
      }
    }

    if(options[["screeningType"]] == "DSdes"){
      threeLevels <- options[["factors3"]]
      factorNames3 <- factorLows3 <- factorCenters3 <- factorHighs3 <- character()
      for(i in 1:length(threeLevels)){
        factorNames3[i]   <- ifelse(threeLevels[[i]]$factorName3 == "", paste0(threeLevels[[i]]$factorName3, " (", i, ")"), threeLevels[[i]]$factorName3)
        factorLows3[i]    <- threeLevels[[i]]$low3
        factorCenters3[i] <- threeLevels[[i]]$center3
        factorHighs3[i]   <- threeLevels[[i]]$high3
      }
      if(options[["numberOfFactorsScreen2"]] > 0){
        factorNamesAll <- c(factorNames3, factorNames2)
      } else {
        factorNamesAll <- factorNames3
      }
    } else {
      factorNamesAll <- factorNames2
    }

    rep <- options[["screeningRepeats"]] > 0

    desScreen <- if(options[["screeningType"]] == "PBdes"){
      FrF2::pb(nruns = as.numeric(options[["PBruns"]]),
               nfactors = options[["numberOfFactorsScreen2"]],
               ncenter = options[["screeningCenterPoints"]],
               replications = options[["screeningCornerReplicates"]],
               repeat.only = rep,
               randomize = F)
    } else {
      daewr::DefScreen(m = options[["numberOfFactorsScreen3"]],
                       c = options[["numberOfFactorsScreen2"]],
                       center = if(options[["numberOfFactorsScreen2"]] == 0) { options[["screeningCenterPoints"]] } else { 0 }
                       )
    }

    set.seed(5)
    runOrder <- sample(nrow(desScreen), nrow(desScreen))
    standard <- if(options[["screeningType"]] == "PBdes"){
      DoE.base::run.order(desScreen)[,1]
    } else {
      1:nrow(desScreen)
    }

    #datacoding
    desScreen <- if(options[["screeningCenterPoints"]] > 0 || options[["screeningType"]] == "DSdes"){
      sapply(desScreen, as.numeric)
    } else {
      sapply(desScreen, as.numeric) * 2 - 3
    }

    # Check for empty levels
    allFactorLevels <- c(factorLows2, factorHighs2)
    emptyLevelsCheck <- sapply(1:length(allFactorLevels), function (x) {
        allFactorLevels[x] == ""
      })

    if(options[["dataCodingScreen"]] == "dataUncoded" && options[["screeningType"]] == "PBdes" && !any(emptyLevelsCheck)){
        for(i in 1:ncol(desScreen)){
          desScreen[,i][desScreen[,i] == 1] <- factorHighs2[i]
          if(options[["screeningCenterPoints"]] >= 1){
            desScreen[,i][desScreen[,i] == 0] <-
              if(!is.na(as.numeric(factorLows2[i]) + as.numeric(factorHighs2[i]))){
                (as.numeric(factorLows2[i]) + as.numeric(factorHighs2[i]))/2
              } else {
                "center"
              }
          }
          desScreen[,i][desScreen[,i] == -1] <- factorLows2[i]
        }
    }

    rows <- cbind.data.frame(runOrder, standard, desScreen)
    if(options[["runOrderScreen"]] == "runOrderStandard"){
      rows <- rows[order(rows$standard),]
    } else {
      rows <- rows[order(rows$runOrder),]
    }

    #table naming
    table$addColumnInfo(name = 'runOrder', title = gettext("Run order"), type = 'string')
    table$addColumnInfo(name = 'standard', title = gettext("Standard order"), type = 'string')
    for(i in 1:length(factorNamesAll)){
      colnames(rows)[i+2] <- factorNamesAll[i]
      table$addColumnInfo(name = factorNamesAll[i], title = factorNamesAll[i], type = 'string')
    }

    if(options[["actualExporter"]] == TRUE && options[["fileScreening"]] != ""){
      print("ready to print")
      exportDesign <- cbind.data.frame(rows, Response = rep(NA, nrow(rows)))
      utils::write.csv(x = exportDesign, file = options[["fileScreening"]], row.names = FALSE, na = "", quote = FALSE)
    }

    table$setData(rows)
    jaspResults[["displayScreeningDesign"]] <- table
  }
}
