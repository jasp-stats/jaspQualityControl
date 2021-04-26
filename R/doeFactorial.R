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

doeFactorial <- function(jaspResults, dataset, options, ...){

  .qualityControlShowAvailableDesignsFactorial(options, jaspResults, position = 1)

  .qualityControlSummarySelectedDesignFactorial(options, jaspResults, position = 2)

  .qualityControlShowSelectedDesignFactorial(options, jaspResults, position = 3)

}

.qualityControlShowAvailableDesignsFactorial <- function(options, jaspResults, position){

  if(is.null(jaspResults[["displayDesigns"]])){

    tableTitle <- gettext("Available Factorial Designs (with Resolution)")

    table <- createJaspTable(tableTitle)
    table$position <- position

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

    table$setData(rows)
    table$addFootnote(gettext("Design resolutions describe how much the effects in a fractional factorial design are aliased with other effects. See the help file for more information."))

    jaspResults[["displayDesigns"]] <- table
  }
}

.qualityControlSummarySelectedDesignFactorial <- function(options, jaspResults, position){

  if(is.null(jaspResults[["selectedDesign"]])){

    table <- createJaspTable(gettext("Selected Design"))
    table$position <- position

    table$dependOn(options = c("factors",
                               "factorialRuns",
                               "factorialCenterPoints",
                               "factorialCornerReplicates",
                               "factorialBlocks",
                               "factorialType",
                               "designBy",
                               "factorialResolution",
                               "numberOfFactors"))

    table$addColumnInfo(name = 'type', title = gettext("Design"), type = 'string')
    table$addColumnInfo(name = 'factors', title = gettext("Factors"), type = 'integer')
    table$addColumnInfo(name = 'runs', title = gettext("Runs"), type = 'integer')
    table$addColumnInfo(name = 'resolution', title = gettext("Resolution"), type = 'string')
    table$addColumnInfo(name = 'centers', title = gettext("Center points"), type = 'integer')
    table$addColumnInfo(name = 'replicates', title = gettext("Replicates"), type = 'integer')
    table$addColumnInfo(name = 'blocks', title = gettext("Blocks"), type = 'integer')

    designs <- jaspResults[["state"]]$object

    if(options[["factorialType"]] == "factorialTypeFull"){
      runs <- 2^as.numeric(options[["numberOfFactors"]])
      resolution <- "Full"
    } else { # not full factorial
      if(options[["designBy"]] == "designByRuns"){
      runs <- options[["factorialRuns"]]
      if(log2(as.numeric(options[["factorialRuns"]])) < options[["numberOfFactors"]]){
        resolution <- as.character(as.roman(DoE.base::design.info(FrF2::FrF2(nfactors = as.numeric(options[["numberOfFactors"]]),
                                                                             nruns = as.numeric(options[["factorialRuns"]])))$catlg.entry[[1]]$res))
      } else {
        resolution <- "Full"
        }
    } else {
      resolution <- options[["factorialResolution"]]
      if(options[["factorialResolution"]] != "Full"){
        runs <- DoE.base::design.info(FrF2::FrF2(nfactors = as.numeric(options[["numberOfFactors"]]),
                                                 resolution = as.numeric(as.roman(options[["factorialResolution"]]))))$nruns
      } else {
        runs <- DoE.base::design.info(FrF2::FrF2(nfactors = as.numeric(options[["numberOfFactors"]]),
                                                 resolution = 100))$nruns
        }
      }
    }


    design <- base::switch(options[["factorialType"]],
                           "factorialTypeDefault" = gettext("2-level factorial"))

    rows <- data.frame(type = "Factorial",
                       factors = options[["numberOfFactors"]],
                       runs = runs,
                       resolution = resolution,
                       # centers = "NA",
                       # replicates = "NA",
                       blocks = options[["factorialBlocks"]]
    )

    table$setData(rows)
    jaspResults[["selectedDesign"]] <- table
  }
}

.qualityControlShowSelectedDesignFactorial <- function(options, jaspResults, parentState, position){

  if(!options[["displayDesign"]])
    return()

  if(is.null(jaspResults[["displayDesign"]])){

    table <- createJaspTable(gettext("Design Preview"))
    table$position <- position

    table$dependOn(options = c("displayDesign",
                               "dataCoding",
                               "factors",
                               "factorialType",
                               "factorialRuns",
                               "factorialCenterPoints",
                               "factorialCornerReplicates",
                               "factorialBlocks",
                               "designBy",
                               "factorialResolution",
                               "numberOfFactors",
                               "factorialTypeSpecifyGenerators",
                               "numberHTCFactors",
                               "runOrder"))

    results <- options[["factors"]]

    factorNames <- factorLows <- factorHighs <- character()
    factorVectors <- list()

    for(i in 1:length(results)){
      factorNames[i]      <- paste(results[[i]]$factorName, " (", LETTERS[i], ")", sep = "")
      factorLows[i]       <- results[[i]]$low
      factorHighs[i]      <- results[[i]]$high1
      factorVectors[[i]]  <- c(factorLows[i], factorHighs[i])
    }

    ifelse(options[["runOrder"]] == "runOrderStandard",
           rnd <- FALSE,
           rnd <- TRUE)

    table$addColumnInfo(name = 'runOrder', title = gettext("RunOrder"), type = 'string')

    if(options[["factorialType"]] == "factorialTypeDefault"){
      if(options[["designBy"]] == "designByRuns"){
        runOrder <- 1:options[["factorialRuns"]]
        rows <- data.frame(runOrder = runOrder)
        rows <- cbind.data.frame(rows,
                                 FrF2::FrF2(nfactors = options[["numberOfFactors"]],
                                            nruns = length(runOrder),
                                            randomize = rnd))
      } else { #if by resolution
        byResolutionRuns <- FrF2::FrF2(nfactors = options[["numberOfFactors"]],
                                       resolution = ifelse(options[["factorialResolution"]] != "Full",
                                                           as.numeric(as.roman(options[["factorialResolution"]])),
                                                           999),
                                       randomize = rnd
        )
        runOrder <- 1:nrow(byResolutionRuns)
        rows <- data.frame(runOrder = runOrder)
        rows <- cbind.data.frame(rows,
                                 byResolutionRuns)
      }
    } #blocks YES

    if(options[["factorialType"]] == "factorialTypeSpecify"){
      specifyGeneratorsRuns <- FrF2::FrF2(nfactors = options[["numberOfFactors"]],
                                          nruns = as.numeric(options[["factorialRuns"]]),
                                          generators = strsplit(options[["factorialTypeSpecifyGenerators"]], "\\s+")[[1]],
                                          randomize = rnd)
      runOrder <- 1:nrow(specifyGeneratorsRuns)
      rows <- data.frame(runOrder = runOrder)
      rows <- cbind.data.frame(rows,
                               specifyGeneratorsRuns)
    } #blocks YES

    if(options[["factorialType"]] == "factorialTypeSplit"){
      if(options[["designBy"]] == "designByRuns"){
        runOrder <- 1:options[["factorialRuns"]]
        splitRuns <- FrF2::FrF2(nfactors = options[["numberOfFactors"]],
                                nruns = as.numeric(options[["factorialRuns"]]),
                                hard = options[["numberHTCFactors"]],
                                randomize = rnd)
        rows <- data.frame(runOrder = runOrder)
        rows <- cbind.data.frame(rows,
                                 splitRuns)
      } else {
        splitRuns <- FrF2::FrF2(nfactors = options[["numberOfFactors"]],
                                resolution = ifelse(options[["factorialResolution"]] != "Full",
                                                    as.numeric(as.roman(options[["factorialResolution"]])),
                                                    999),
                                hard = options[["numberHTCFactors"]],
                                randomize = rnd)
        runOrder <- 1:nrow(splitRuns)
        rows <- data.frame(runOrder = runOrder)
        rows <- cbind.data.frame(rows,
                                 splitRuns)
      }
    } #blocks NO

    if(options[["factorialType"]] == "factorialTypeFull"){
      runOrder <- 1:2^options[["numberOfFactors"]]
      rows <- data.frame(runOrder = runOrder)
      rows <- cbind.data.frame(rows,
                               FrF2::FrF2(nfactors = options[["numberOfFactors"]],
                                          nruns = 2^options[["numberOfFactors"]],
                                          randomize = rnd))
    } #blocks YES

    for(i in 1:as.numeric(options[["numberOfFactors"]])){
      colnames(rows)[i+1] <- factorNames[i]
      table$addColumnInfo(name = factorNames[i], title = factorNames[i], type = 'string')
    }

    jaspResults[["displayDesign"]] <- table

    if(options[["runOrder"]] == "runOrderStandard"){
      rows <- rows[-1]
      rows <- rows[do.call(order, rows),]
      # rows <- rows[rev(rownames(rows)),]
      rows <- cbind(runOrder = 1:nrow(rows), rows)
    }

    if(options[["dataCoding"]] == "dataUncoded"){
      table$setData(rows)
    } else {
      for(i in 1:as.numeric(options[["numberOfFactors"]])){
        rows[,i+1][rows[,i+1] == 1] <- factorHighs[i]
        rows[,i+1][rows[,i+1] == -1] <- factorLows[i]
      }
      table$setData(rows)
    }

    #export design
    if(options[["actualExporter"]] && options[["file"]] != ""){
      exportDesign <- data.frame(rows)
      exportDesign <- cbind(exportDesign, rep(NA, nrow(exportDesign)))
      colnames(exportDesign)[ncol(exportDesign)] <- "Response"
      utils::write.csv(x = exportDesign, file = options[["file"]], row.names = FALSE, na = "", quote = FALSE)
    }

  }
}
