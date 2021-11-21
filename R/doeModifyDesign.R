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

doeModifyDesign <- function(jaspResults, dataset, options, ...){

  ready <- (length(options[["MDassignedFactors"]]) >= 2 && !is.null(options[["MDresponse"]]))

  if(ready){

    dataset <- .modifyDesignReadData(dataset, options)

    inputDesign <- .modifyDesignShowInputDesign(jaspResults, dataset, options)

    .modifyDesignShowDesiredDesign(jaspResults, dataset, options, inputDesign)

  }
}

.modifyDesignReadData <- function(dataset, options){

  if(!is.null(dataset)){
    return(dataset)
  } else {
    dataset <-
      if(options[["MDrunOrder"]]!=""){
        .readDataSetToEnd(columns.as.numeric = c(options[["MDresponse"]],
                                                 options[["MDrunOrder"]],
                                                 options[["MDassignedFactors"]]))
      } else {
        .readDataSetToEnd(columns.as.numeric = c(options[["MDresponse"]],
                                                 options[["MDassignedFactors"]]))
      }

    return(dataset)
  }
}

.modifyDesignShowInputDesign <- function(jaspResults, dataset, options){

  if(is.null(jaspResults[["showInputDesign"]])){
    table <- createJaspTable(gettext("Input Design"))
    table$position <- 1
    table$dependOn(options = c("designBy",
                               "MDassignedFactors",
                               "MDresolution",
                               "MDruns",
                               "MDfraction",
                               "MDrunOrder",
                               "MDresponse",
                               "MDcenterPoints",
                               "MDrepeats",
                               "dataCoding",
                               "displayRunOrder",
                               "repeatRuns",
                               "showDesiredDesign"))

    factors <- unlist(dataset[,options[["MDassignedFactors"]]], use.names = FALSE)
    response <- unlist(dataset[,options[["MDresponse"]]], use.names = FALSE)

    perF <- length(factors) / length(options[["MDassignedFactors"]])
    factorsDF <- data.frame(split(factors, ceiling(seq_along(factors) / perF)))

    runOrder <-
      if(!options[["MDrunOrder"]] == ""){
        unlist(dataset[,options[["MDrunOrder"]]], use.names = FALSE)
      } else {
        1:nrow(factorsDF)
      }

    if(options[["dataCoding"]] == "dataCoded"){
      for(i in 1:ncol(factorsDF)){
        factorsDF[,i][factorsDF[,i] == min(factorsDF[,i])] <- -1
        factorsDF[,i][factorsDF[,i] == max(factorsDF[,i])] <- 1
      }
    }

    input <- cbind.data.frame(runOrder, factorsDF, response)

    table$setData(input)
    jaspResults[["showInputDesign"]] <- table

    return(factorsDF)

  }
}

.modifyDesignShowDesiredDesign <- function(jaspResults, dataset, options, inputDesign){

  if(!options[["showDesiredDesign"]]){
    jaspResults[["desiredDesign"]] <- NULL
    return()
  }

  if(is.null(jaspResults[["desiredDesign"]])){
    table <- createJaspTable(gettext("Desired Design"))
    table$position <- 2
    table$dependOn(options = c("designBy",
                               "MDassignedFactors",
                               "MDresolution",
                               "MDruns",
                               "MDfraction",
                               "MDresponse",
                               "MDcenterPoints",
                               "MDrepeats",
                               "dataCoding",
                               "displayRunOrder",
                               "showDesiredDesign"))

    if(options[["designBy"]] == "byRuns"){
      desiredDesign <- FrF2::FrF2(nfactors = length(options[["MDassignedFactors"]]),
                                  nruns = as.numeric(options[["MDruns"]]),
                                  ncenter = options[["MDcenterPoints"]])
    } else if(options[["designBy"]] == "byResolution"){
      desiredDesign <- FrF2::FrF2(nfactors = length(options[["MDassignedFactors"]]),
                                  resolution = ifelse(options[["MDresolution"]] != "Full",
                                                      as.numeric(as.roman(options[["MDresolution"]])),
                                                      999),
                                  ncenter = options[["MDcenterPoints"]])
    } else {
      desiredDesign <- FrF2::FrF2(nfactors = length(options[["MDassignedFactors"]]),
                                  nruns = (2^length(options[["MDassignedFactors"]]) * as.numeric(options[["MDfraction"]])),
                                  ncenter = options[["MDcenterPoints"]])
    }

    desiredStdOrder <- DoE.base::run.order(desiredDesign)[,1]
    print(1:100)
    print(desiredStdOrder)

    desiredDesign <- if(options[["MDcenterPoints"]] == 0){
      sapply(desiredDesign, as.numeric)*2-3
    } else {
      sapply(desiredDesign, as.numeric)
    }

    if(options[["dataCoding"]] == "dataUncoded"){
      for(i in 1:ncol(desiredDesign)){
        desiredDesign[,i][desiredDesign[,i] == -1] <- min(inputDesign[,i])
        desiredDesign[,i][desiredDesign[,i] == 1] <- max(inputDesign[,i])
      }
    }

    showDesiredDesign <- cbind.data.frame(1:nrow(desiredDesign), desiredStdOrder, desiredDesign)
    colnames(showDesiredDesign)[c(1,2)] <- c("Run order", "Standard order")

    if(options[["displayRunOrder"]] == "runOrderStandard"){
      showDesiredDesign <- showDesiredDesign[order(showDesiredDesign$`Standard order`),]
    }

    table$setData(showDesiredDesign)
    jaspResults[["desiredDesign"]] <- table

    .modifyDesignAntiJoin(jaspResults, inputDesign = inputDesign, desiredDesign = desiredDesign)
  }
}

.modifyDesignAntiJoin <- function(jaspResults, inputDesign, desiredDesign){

  if(is.null(jaspResults[["showMissingRuns"]])){
    table <- createJaspTable(gettext("Missing Runs"))
    table$position <- 3
    table$dependOn(options = c("designBy",
                               "MDassignedFactors",
                               "MDresolution",
                               "MDruns",
                               "MDfraction",
                               "dataCoding",
                               "showDesiredDesign"))

    colnames(inputDesign) <- LETTERS[1:ncol(inputDesign)]
    colnames(desiredDesign) <- LETTERS[1:ncol(desiredDesign)]

    missing <- dplyr::anti_join(as.data.frame(desiredDesign), as.data.frame(inputDesign))
    missing <- cbind.data.frame('Run order' = 1:nrow(missing), missing)
    table$setData(missing)
    jaspResults[["showMissingRuns"]] <- table

  }
}
