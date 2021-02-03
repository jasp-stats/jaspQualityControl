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

designOfExperiments <- function(jaspResults, dataset, options, ...){

  .qualityControlShowAvailableDesigns(options, jaspResults, position = 1)

  .qualityControlSummarySelectedDesign(options, jaspResults, position = 2)

  .qualityControlShowSelectedDesign(options, jaspResults, position = 3)

}

.qualityControlShowAvailableDesigns <- function(options, jaspResults, position){

  if(is.null(jaspResults[["displayDesigns"]])){

    tableTitle <- base::switch(options[["design"]],
                               "factorial" = gettext("Available Factorial Designs (with Resolution)"),
                               "screening" = gettext("Available Screening Designs"),
                               "response" = gettext("Available Response Surface Designs"),
                               "mixture" = gettext("Available Mixture Designs"))

    table <- createJaspTable(tableTitle)
    table$position <- position

    table$dependOn(options = "design")

    if(options[["design"]] == "factorial"){

      table$addColumnInfo(name = 'run', title = gettext("Runs"), type = 'integer')
      for (i in 2:15){
        table$addColumnInfo(name = paste0("factor", i), title = as.character(i), type = "string", overtitle = "Factors")
      }

      rows <- data.frame(run = c(4, 8, 16, 32, 64, 128),
                         factor2 = c("Full", "", "", "", "", ""),
                         factor3 = c("III", "Full", "", "", "", ""),
                         factor4 = c("", "IV", "Full", "", "", ""),
                         factor5 = c("", "III", "V", "Full", "", ""),
                         factor6 = c("", "III", "IV", "VI", "Full", ""),
                         factor7 = c("", "III", "IV", "IV", "VII", "Full"),
                         factor8 = c("", "", "IV", "IV", "V", "VIII"),
                         factor9 = c("", "", "III", "IV", "V", "VI"),
                         factor10 = c("", "", "III", "IV", "V", "V"),
                         factor11 = c("", "", "III", "IV", "V", "V"),
                         factor12 = c("", "", "III", "IV", "IV", "IV"),
                         factor13 = c("", "", "III", "IV", "IV", "IV"),
                         factor14 = c("", "", "III", "IV", "IV", "IV"),
                         factor15 = c("", "", "III", "IV", "IV", "IV"))

      jaspResults[["state"]] <- createJaspState(rows)
      jaspResults[["state"]]$dependOn("always_present")

    } else if(options[["design"]] == "screening"){

    } else if(options[["design"]] == "response"){

    } else if(options[["design"]] == "mixture"){

    }

    table$setData(rows)
    table$addFootnote(gettext("Design resolutions describe how much the effects in a fractional factorial design are aliased with other effects. See the help file for more information."))

    jaspResults[["displayDesigns"]] <- table
  }
}

.qualityControlSummarySelectedDesign <- function(options, jaspResults, position){

  if(is.null(jaspResults[["selectedDesign"]])){

    table <- createJaspTable(gettext("Selected Design"))
    table$position <- position

    table$dependOn(options = c("design",
                               "factors",
                               "factorialRuns",
                               "factorialCenterPoints",
                               "factorialCornerReplicates",
                               "factorialBlocks",
                               "designBy",
                               "factorialResolution",
                               "numberOfFactors"))

    table$addColumnInfo(name = 'type', title = gettext("Design"), type = 'string')
    table$addColumnInfo(name = 'factors', title = gettext("Factors"), type = 'integer')
    table$addColumnInfo(name = 'runs', title = gettext("Runs"), type = 'integer')
    table$addColumnInfo(name = 'resolution', title = gettext("Resolution"), type = 'string')
    table$addColumnInfo(name = 'measure', title = gettext("2^(k-p)"), type = 'number')
    table$addColumnInfo(name = 'centers', title = gettext("Center points"), type = 'integer')
    table$addColumnInfo(name = 'replicates', title = gettext("Replicates"), type = 'integer')
    table$addColumnInfo(name = 'blocks', title = gettext("Blocks"), type = 'integer')

    designs <- jaspResults[["state"]]$object

    if(options[["designBy"]] == "designByRuns"){
      rowIndex 	    <- which(designs[, 1] == options[["factorialRuns"]])
      resolution    <- designs[rowIndex, options[["numberOfFactors"]]]
      runs          <- options[["factorialRuns"]]
    } else {
      resolution    <- options[["factorialResolution"]]
      rowIndex      <- which(designs[,options[["numberOfFactors"]]] == resolution)[1]
      runs          <- designs[rowIndex, 1]
    }
    if(resolution == "")
      resolution <- "None"

    design <- base::switch(options[["factorialType"]],
                           "factorialTypeDefault" = gettext("2-level factorial"))

    rows <- data.frame(type = "Factorial",
                       factors = options[["numberOfFactors"]],
                       runs = runs,
                       resolution = resolution,
                       measure = as.numeric(runs),
                       centers = options[["factorialCenterPoints"]],
                       replicates = options[["factorialCornerReplicates"]],
                       blocks = options[["factorialBlocks"]])

    table$setData(rows)
    jaspResults[["selectedDesign"]] <- table
  }
}

.qualityControlShowSelectedDesign <- function(options, jaspResults, position){

  if(!options[["displayDesign"]])
    return()

  if(is.null(jaspResults[["displayDesign"]])){

    table <- createJaspTable(gettext("Design Preview"))
    table$position <- position

    table$dependOn(options = c("displayDesign",
                               "design",
                               "factors",
                               "factorialRuns",
                               "factorialCenterPoints",
                               "factorialCornerReplicates",
                               "factorialBlocks",
                               "designBy",
                               "factorialResolution",
                               "numberOfFactors"))

    results <- options[["factors"]]

    factorNames <- factorLows <- factorHighs <- character()
    factorVectors <- list()

    for(i in 1:length(results)){
      factorNames[i]      <- results[[i]]$factorName
      factorLows[i]       <- results[[i]]$low
      factorHighs[i]      <- results[[i]]$high1
      factorVectors[[i]]  <- c(factorLows[i], factorHighs[i])
    }

    table$addColumnInfo(name = 'runOrder', title = gettext("RunOrder"), type = 'string')
    runOrder <- 1:2^options[["numberOfFactors"]]

    rows <- data.frame(runOrder = runOrder)
    rows <- cbind.data.frame(rows,
                             matrix(0,
                                    ncol = options[["numberOfFactors"]],
                                    nrow = nrow(rows),
                                    dimnames = NULL))

    for(i in 1:as.numeric(options[["numberOfFactors"]])){
      rows[,i+1] <- rep(c(rep(1, 2^(options[["numberOfFactors"]]-i)), rep(-1, 2^(options[["numberOfFactors"]]-i))), 2^(i-1))
      colnames(rows)[i+1] <- factorNames[i]
      table$addColumnInfo(name = factorNames[i], title = factorNames[i], type = 'string')
    }

    jaspResults[["displayDesign"]] <- table
    colnames(rows) <- c("runOrder", factorNames)
    table$setData(rows)

  }
}
