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

#' @export
doeModifyDesign <- function(jaspResults, dataset, options, ...){

  ready <- (length(options[["assignedFactors"]]) >= 2 && !is.null(options[["responseVariable"]]))

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
      if(options[["runOrder"]]!=""){
        .readDataSetToEnd(columns.as.numeric = c(options[["responseVariable"]],
                                                 options[["runOrder"]],
                                                 options[["assignedFactors"]]))
      } else {
        .readDataSetToEnd(columns.as.numeric = c(options[["responseVariable"]],
                                                 options[["assignedFactors"]]))
      }

    return(dataset)
  }
}

.modifyDesignShowInputDesign <- function(jaspResults, dataset, options){

  if(is.null(jaspResults[["showInputDesign"]])){
    table <- createJaspTable(gettext("Input Design"))
    table$position <- 1
    table$dependOn(options = c("designOptionsType",
                               "assignedFactors",
                               "designOptionsTypeResolutionValue",
                               "designOptionsTypeNumberOfRunsValue",
                               "designOptionsTypeFractionValue",
                               "runOrder",
                               "responseVariable",
                               "numberCenterPoints",
                               "MDrepeats",
                               "unitDisplay",
                               "displayedRunOrder",
                               "randomRunsNumberRepetitions",
                               "desiredDesignTable"))

    factors <- unlist(dataset[,options[["assignedFactors"]]], use.names = FALSE)
    response <- unlist(dataset[,options[["responseVariable"]]], use.names = FALSE)

    perF <- length(factors) / length(options[["assignedFactors"]])
    factorsDF <- data.frame(split(factors, ceiling(seq_along(factors) / perF)))

    runOrder <-
      if(!options[["runOrder"]] == ""){
        unlist(dataset[,options[["runOrder"]]], use.names = FALSE)
      } else {
        1:nrow(factorsDF)
      }

    if(options[["unitDisplay"]] == "coded"){
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

  if(!options[["desiredDesignTable"]]){
    jaspResults[["desiredDesign"]] <- NULL
    return()
  }

  if(is.null(jaspResults[["desiredDesign"]])){
    table <- createJaspTable(gettext("Desired Design"))
    table$position <- 2
    table$dependOn(options = c("designOptionsType",
                               "assignedFactors",
                               "designOptionsTypeResolutionValue",
                               "designOptionsTypeNumberOfRunsValue",
                               "designOptionsTypeFractionValue",
                               "responseVariable",
                               "numberCenterPoints",
                               "MDrepeats",
                               "unitDisplay",
                               "displayedRunOrder",
                               "desiredDesignTable"))

    if(options[["designOptionsType"]] == "numberOfRuns"){
      desiredDesign <- FrF2::FrF2(nfactors = length(options[["assignedFactors"]]),
                                  nruns = as.numeric(options[["designOptionsTypeNumberOfRunsValue"]]),
                                  ncenter = options[["numberCenterPoints"]])
    } else if(options[["designOptionsType"]] == "resolution"){
      desiredDesign <- FrF2::FrF2(nfactors = length(options[["assignedFactors"]]),
                                  resolution = if(options[["designOptionsTypeResolutionValue"]] != "Full"){
                                    as.numeric(as.roman(options[["designOptionsTypeResolutionValue"]]))
                                  } else {
                                    999
                                  },
                                  ncenter = options[["numberCenterPoints"]])
    } else {
      desiredDesign <- FrF2::FrF2(nfactors = length(options[["assignedFactors"]]),
                                  nruns = (2^length(options[["assignedFactors"]]) * as.numeric(options[["designOptionsTypeFractionValue"]])),
                                  ncenter = options[["numberCenterPoints"]])
    }

    desiredStdOrder <- DoE.base::run.order(desiredDesign)[,1]
    print(1:100)
    print(desiredStdOrder)

    desiredDesign <- if(options[["numberCenterPoints"]] == 0){
      sapply(desiredDesign, as.numeric)*2-3
    } else {
      sapply(desiredDesign, as.numeric)
    }

    if(options[["unitDisplay"]] == "uncoded"){
      for(i in 1:ncol(desiredDesign)){
        desiredDesign[,i][desiredDesign[,i] == -1] <- min(inputDesign[,i])
        desiredDesign[,i][desiredDesign[,i] == 1] <- max(inputDesign[,i])
      }
    }

    showDesiredDesign <- cbind.data.frame(1:nrow(desiredDesign), desiredStdOrder, desiredDesign)
    colnames(showDesiredDesign)[c(1,2)] <- c("Run order", "Standard order")

    if(options[["displayedRunOrder"]] == "standard"){
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
    table$dependOn(options = c("designOptionsType",
                               "assignedFactors",
                               "designOptionsTypeResolutionValue",
                               "designOptionsTypeNumberOfRunsValue",
                               "designOptionsTypeFractionValue",
                               "unitDisplay",
                               "desiredDesignTable"))

    colnames(inputDesign) <- LETTERS[1:ncol(inputDesign)]
    colnames(desiredDesign) <- LETTERS[1:ncol(desiredDesign)]

    missing <- dplyr::anti_join(as.data.frame(desiredDesign), as.data.frame(inputDesign))
    missing <- cbind.data.frame('Run order' = 1:nrow(missing), missing)
    table$setData(missing)
    jaspResults[["showMissingRuns"]] <- table

  }
}
