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
doeFull <- function(jaspResults, dataset, options, ...){

  .doeFullSummaryDesign(options, jaspResults, position = 1)

  .doeFullShowDesign(options, jaspResults, position = 2)

}

.doeFullSummaryDesign <- function(options, jaspResults, position){

  if(is.null(jaspResults[["selectedDesignFull"]])){
    table <- createJaspTable(gettext("Selected Design"))
    table$position <- position
    table$dependOn(options = c("numberOfFactors",
                               "factors",
                               "fullCornerReplicates"))

    table$addColumnInfo(name = 'type', title = "Design", type = 'string')
    table$addColumnInfo(name = 'factors', title = gettext("Factors"), type = 'integer')
    table$addColumnInfo(name = 'runs', title = gettext("Runs"), type = 'integer')
    table$addColumnInfo(name = 'resolution', title = gettext("Resolution"), type = 'string')
    table$addColumnInfo(name = 'replicates', title = gettext("Replicates"), type = 'integer')

    levels <- numeric(options[["numberOfFactors"]])
    for(i in 1:length(levels)){
      levels[i] <- options[["factors"]][[i]]$numberOfLevels
    }

    rows <- data.frame(type = "Factorial",
                       factors = options[["numberOfFactors"]],
                       runs = prod(levels) * options[["fullCornerReplicates"]],
                       resolution = "Full",
                       replicates = options[["fullCornerReplicates"]])

    table$setData(rows)
    jaspResults[["selectedDesignScreening"]] <- table
  }
}

.doeFullShowDesign <- function(options, jaspResults, position){

  if(!options[["displayFullDesign"]]){
    jaspResults[["displayFullDesign"]] <- NULL
    return()
  }

  if(is.null(jaspResults[["displayFullDesign"]])){

    table <- createJaspTable(gettext("Design Preview"))
    table$position <- position

    table$dependOn(options = c("displayFullDesign",
                               "numberOfFactors",
                               "runOrderFull",
                               "factors",
                               "fullCornerReplicates",
                               "fullRepeats",
                               "fullRepeatRuns",
                               "fileFull",
                               "actualExporter"))

    levels <- numeric(options[["numberOfFactors"]])
    factorNames <- character(options[["numberOfFactors"]])
    for(i in 1:length(levels)){
      levels[i] <- options[["factors"]][[i]]$numberOfLevels
      factorNames[i] <- options[["factors"]][[i]]$factorName
    }

    rep <- options[["fullRepeats"]] > 0

    fullDesign <- DoE.base::fac.design(nlevels = levels,
                                   factor.names = factorNames,
                                   replications = options[["fullCornerReplicates"]],
                                   repeat.only = rep)

    fullRunOrder <- 1:nrow(fullDesign)
    fullStandard <- DoE.base::run.order(fullDesign)[,1]

    rows <- cbind.data.frame(`Run order` = fullRunOrder, `Standard order` = fullStandard, fullDesign)
    if(options[["runOrderFull"]] == "runOrderStandard"){
      rows <- rows[order(rows$`Standard order`),]
    } else {
      rows <- rows[order(rows$`Run order`),]
    }

    if(options[["actualExporter"]] == TRUE && options[["fileFull"]] != ""){
      print("ready to print")
      exportDesign <- cbind.data.frame(rows, Response = rep(NA, nrow(rows)))
      utils::write.csv(x = exportDesign, file = options[["fileFull"]], row.names = FALSE, na = "", quote = FALSE)
    }

    table$setData(rows)
    jaspResults[["displayScreeningDesign"]] <- table
  }
}
