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

doeFactorial <- function(jaspResults, dataset, options, ...) {

  .qualityControlShowAvailableDesignsFactorial(options, jaspResults, position = 1)

  .qualityControlSummarySelectedDesignFactorial(options, jaspResults, position = 2)

  .qualityControlShowSelectedDesignFactorial(options, jaspResults, position = 3)

}

.qualityControlShowAvailableDesignsFactorial <- function(options, jaspResults, position) {

  if(!options[["showAvailableDesigns"]]){
    jaspResults[["displayDesigns"]] <- NULL
    return()
  }

  if (is.null(jaspResults[["displayDesigns"]])) {

    tableTitle <- gettext("Available Factorial Designs (with Resolution)")

    table <- createJaspTable(tableTitle)
    table$position <- position

    table$addColumnInfo(name = 'run', title = gettext("Runs"), type = 'integer')
    for (i in 2:15) {
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

.qualityControlSummarySelectedDesignFactorial <- function(options, jaspResults, position) {

  if (is.null(jaspResults[["selectedDesign"]])) {

    table <- createJaspTable(gettext("Selected Design"))
    table$position <- position

    table$dependOn(options = c("factors",
                               "factorialRuns",
                               "PBruns",
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

    if(options[["factorialType"]] == "factorialPlackettBurman"){
      runs <- (as.numeric(options[["PBruns"]]) +
                 options[["factorialCenterPoints"]] *
                 options[["factorialBlocks"]]) *
        options[["factorialCornerReplicates"]]
      resolution <- "?"
    } else {
      if(options[["factorialType"]] == "factorialTypeFull"){
        runs <- (2^options[["numberOfFactors"]] +
                   options[["factorialCenterPoints"]] *
                   options[["factorialBlocks"]]) *
          options[["factorialCornerReplicates"]]
        resolution <- "Full"
      } else {
        if(options[["designBy"]] == "designByRuns"){
          runs <- (as.numeric(options[["factorialRuns"]]) +
                     options[["factorialCenterPoints"]] *
                     options[["factorialBlocks"]]) *
            options[["factorialCornerReplicates"]]

          resolution <- if(log2(as.numeric(options[["factorialRuns"]])) < options[["numberOfFactors"]]){
            as.character(as.roman(DoE.base::design.info(
              FrF2::FrF2(nfactors = options[["numberOfFactors"]],
                         nruns = as.numeric(options[["factorialRuns"]])))$catlg.entry[[1]]$res))
          } else {
            "Full"
          }

        } else {
          resolution <- if(options[["factorialResolution"]] == "Full"){
            100
          } else {
            as.numeric(as.roman(options[["factorialResolution"]]))
          }

          runs <- (DoE.base::design.info(FrF2::FrF2(nfactors = options[["numberOfFactors"]],
                                                    resolution = resolution))$nruns +
                     options[["factorialCenterPoints"]] *
                     options[["factorialBlocks"]]) *
            options[["factorialCornerReplicates"]]
        }
      }
    }

    design <- base::switch(options[["factorialType"]],
                           "factorialTypeDefault" = gettext("2-level factorial"))

    res <- if(resolution == 100){
      "Full"
    } else {
      resolution
    }
    rows <- data.frame(type = "Factorial",
                       factors = options[["numberOfFactors"]],
                       runs = runs,
                       resolution = res,
                       centers = options[["factorialCenterPoints"]],
                       replicates = options[["factorialCornerReplicates"]],
                       blocks = options[["factorialBlocks"]]
    )

    table$setData(rows)
    jaspResults[["selectedDesign"]] <- table
  }
}

.qualityControlShowSelectedDesignFactorial <- function(options, jaspResults, position) {

  if (!options[["displayDesign"]])
    return()

  if (is.null(jaspResults[["displayDesign"]])) {

    table <- createJaspTable(gettext("Design Preview"))
    table$position <- position

    table$dependOn(options = c("displayDesign",
                               "dataCoding",
                               "factors",
                               "factorialType",
                               "factorialRuns",
                               "PBruns",
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

    for (i in 1:length(results)) {
      factorNames[i]      <- paste0(results[[i]]$factorName, " (", i, ")")
      factorLows[i]       <- results[[i]]$low
      factorHighs[i]      <- results[[i]]$high1
      factorVectors[[i]]  <- c(factorLows[i], factorHighs[i])
    }

    ifelse(options[["runOrder"]] == "runOrderStandard",
           rnd <- FALSE,
           rnd <- TRUE)

    rep <- options[["factorialRepeats"]] > 0

    table$addColumnInfo(name = 'runOrder', title = gettext("Run order"), type = 'string')

    if (options[["factorialType"]] == "factorialTypeDefault") {
      if (options[["designBy"]] == "designByRuns") {
        rows <- FrF2::FrF2(nfactors = options[["numberOfFactors"]],
                           nruns = as.numeric(options[["factorialRuns"]]),
                           ncenter = options[["factorialCenterPoints"]],
                           replications = options[["factorialCornerReplicates"]],
                           repeat.only = rep,
                           blocks = options[["factorialBlocks"]],
                           randomize = rnd)
      } else {
        rows <- FrF2::FrF2(nfactors = options[["numberOfFactors"]],
                           resolution = ifelse(options[["factorialResolution"]] != "Full",
                                               as.numeric(as.roman(options[["factorialResolution"]])),
                                               999),
                           ncenter = options[["factorialCenterPoints"]],
                           replications = options[["factorialCornerReplicates"]],
                           repeat.only = rep,
                           randomize = rnd)
      }
      runOrder <- 1:nrow(rows)
      rows <- cbind.data.frame(runOrder, rows)
    } else if (options[["factorialType"]] == "factorialTypeSpecify") {
      specifyGeneratorsRuns <- FrF2::FrF2(nfactors = options[["numberOfFactors"]],
                                          nruns = as.numeric(options[["factorialRuns"]]),
                                          generators = strsplit(options[["factorialTypeSpecifyGenerators"]], "\\s+")[[1]],
                                          ncenter = options[["factorialCenterPoints"]],
                                          replications = options[["factorialCornerReplicates"]],
                                          repeat.only = rep,
                                          randomize = rnd)
      runOrder <- 1:nrow(specifyGeneratorsRuns)
      rows <- cbind.data.frame(runOrder, specifyGeneratorsRuns)
    } else if (options[["factorialType"]] == "factorialTypeSplit") {
      if (options[["designBy"]] == "designByRuns") {
        splitRuns <- FrF2::FrF2(nfactors = options[["numberOfFactors"]],
                                nruns = as.numeric(options[["factorialRuns"]]),
                                hard = options[["numberHTCFactors"]],
                                replications = options[["factorialCornerReplicates"]],
                                repeat.only = rep,
                                randomize = rnd)
      } else {
        splitRuns <- FrF2::FrF2(nfactors = options[["numberOfFactors"]],
                                resolution = ifelse(options[["factorialResolution"]] != "Full",
                                                    as.numeric(as.roman(options[["factorialResolution"]])),
                                                    999),
                                hard = options[["numberHTCFactors"]],
                                replications = options[["factorialCornerReplicates"]],
                                repeat.only = rep,
                                randomize = rnd)
      }
      runOrder <- 1:nrow(splitRuns)
      rows <- cbind.data.frame(runOrder, splitRuns)
      table$addFootnote(paste("Hard-to-change factors: ", paste(factorNames[1:options[["numberHTCFactors"]]], collapse = ", "), sep = ""))
    } else if (options[["factorialType"]] == "factorialTypeFull") {
      rows <- FrF2::FrF2(nfactors = options[["numberOfFactors"]],
                         nruns = 2^options[["numberOfFactors"]],
                         ncenter = options[["factorialCenterPoints"]],
                         replications = options[["factorialCornerReplicates"]],
                         repeat.only = rep,
                         blocks = options[["factorialBlocks"]],
                         randomize = rnd)
      runOrder <- 1:nrow(rows)
      rows <- cbind.data.frame(runOrder, rows)
    } else if(options[["factorialType"]] == "factorialPlackettBurman"){
      rows <- FrF2::pb(nruns = as.numeric(options[["PBruns"]]),
                       nfactors = options[["numberOfFactors"]],
                       ncenter = options[["factorialCenterPoints"]],
                       replications = options[["factorialCornerReplicates"]],
                       repeat.only = rep,
                       randomize = rnd)
      runOrder <- 1:nrow(rows)
      rows <- cbind.data.frame(runOrder, rows)
    }

    blocks <- rows$Blocks
    rows <- rows[,!names(rows) %in% "Blocks"]

    #filling in table
    for (i in 1:options[["numberOfFactors"]]) {
      colnames(rows)[i+1] <- factorNames[i]
      table$addColumnInfo(name = factorNames[i], title = factorNames[i], type = 'string')
    }

    if(options[["factorialBlocks"]] > 1 || options[["factorialCornerReplicates"]] > 1){
      blocks <- rows$Blocks
      rows <- rows[,!names(rows) %in% "Blocks"]
      # table$addColumnInfo(name = 'blocks', title = gettext("Blocks"), type = 'string')

      if(options[["factorialBlocks"]] == 1 && options[["factorialCornerReplicates"]] > 1 && options[["factorialCenterPoints"]] > 0){
        for(i in seq_len(options[["factorialCornerReplicates"]])){
          for(j in seq_len(options[["factorialCenterPoints"]])){
            blocks[is.na(blocks)][1] <- as.character(i)
          }
        }
      }

      # rows[ncol(rows) + 1] <- blocks
    }

    rows <- rows[,!names(rows) %in% "Blocks"]

    #run order randomization REPLACE
    # if (options[["runOrder"]] == "runOrderStandard") {
    #   rows <- rows[-1]
    #   rows <- rows[do.call(order, rows),]
    #   # rows <- rows[rev(rownames(rows)),]
    #   rows <- cbind(runOrder = 1:nrow(rows), rows)
    # }

    #DATA CODING
    if(options[["factorialCenterPoints"]] >= 1){
      rows[,2:(options[["numberOfFactors"]]+1)] <- sapply(rows[,2:(options[["numberOfFactors"]]+1)], as.numeric)
    } else {
      rows[,2:(options[["numberOfFactors"]]+1)] <- sapply(rows[,2:(options[["numberOfFactors"]]+1)], as.numeric) * 2 - 3
    }


    if(options[["dataCoding"]] == "dataUncoded"){
      for(i in 1:options[["numberOfFactors"]]){
        rows[,i+1][rows[,i+1] == -1] <- factorLows[i]
        if(options[["factorialCenterPoints"]] >= 1){
          rows[,i+1][rows[,i+1] == 0] <- if(!is.na(as.numeric(factorLows[i]) + as.numeric(factorHighs[i]))){
            (as.numeric(factorLows[i]) + as.numeric(factorHighs[i]))/2
          } else {
            "center"
          }
        }
        rows[,i+1][rows[,i+1] == 1] <- factorHighs[i]
      }
    }

    # for (i in 1:as.numeric(options[["numberOfFactors"]])) {
    #   rows[,i+1][rows[,i+1] == 1] <- ifelse (options[["dataCoding"]] == "dataCoded",
    #                                          -1,
    #                                          factorLows[i])
    #   rows[,i+1][rows[,i+1] == 2] <- ifelse (options[["dataCoding"]] == "dataCoded",
    #                                          1,
    #                                          factorHighs[i])
    # }

    #center points REPLACE
    # if (options[["factorialCenterPoints"]] > 0) {
    #   centerRuns <- sort(sample(1:(nrow(rows) + options[["factorialCenterPoints"]]), options[["factorialCenterPoints"]]))
    #   rows[(nrow(rows)+1):(nrow(rows) + options[["factorialCenterPoints"]]),] <- 0
    #   ifelse (options[["dataCoding"]] == "dataCoded",
    #           center <- rep(0, options[["numberOfFactors"]]),
    #           center <- (as.numeric(factorHighs)+as.numeric(factorLows))/2)
    #   for (i in centerRuns) {
    #     rows[(i+1):(nrow(rows)+1),] <- rows[i:nrow(rows),]
    #     rows[i,] <- append(center, i, 0)
    #   }
    #   rows <- head(rows, -options[["factorialCenterPoints"]])
    #   rows[,1] <- 1:nrow(rows)
    # }

    #replicates REPLACE
    # if (options[["factorialCornerReplicates"]] > 1 && options[["factorialType"]] != "factorialTypeSplit") {
    #   original <- rows
    #   if (rnd == FALSE) {
    #     for (i in 2:options[["factorialCornerReplicates"]]) {
    #       rows <- rbind.data.frame(rows, original)
    #     }
    #   } else {
    #     for (i in 2:options[["factorialCornerReplicates"]]) {
    #       add_index <- sample(1:nrow(original))
    #       add_df <- original[add_index,]
    #       rows <- rbind.data.frame(rows, add_df)
    #     }
    #   }
    #   rows[,1] <- 1:nrow(rows)
    # }

    table$setData(rows)
    jaspResults[["displayDesign"]] <- table

    #export design
    if(options[["actualExporter"]] && options[["file"]] != "") {
      exportDesign <- data.frame(rows)
      exportDesign <- cbind(exportDesign, rep(NA, nrow(exportDesign)))
      colnames(exportDesign)[ncol(exportDesign)] <- "Response"
      utils::write.csv(x = exportDesign, file = options[["file"]], row.names = FALSE, na = "", quote = FALSE)
    }

  }
}
