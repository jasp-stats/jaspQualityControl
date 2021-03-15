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

doeResponseSurfaceMethodology <- function(jaspResults, dataset, options, ...){
  if (options[["showDesign"]])

    .qualityControlDesignMainRSM(jaspResults,options, position = 1)

  if (options[["contour"]])
    .responseSurfaceContour(jaspResults, options, position = 2)
}

.qualityCOntrolResponseReadData <- function(dataset, options) {
  if(!is.null(dataset))
    return(dataset)
  else
    return(.readDataSetToEnd(columns.as.numeric = options[["rsmVariables"]]))

}

.qualityControlDesignMainRSM <- function(jaspResults,options,dataset,position) {
  ready <- T

  if (is.null(jaspResults[["showDesign"]])) {
    .qualityControlDesignChoice(jaspResults, options, dataset)
  }

}

.qualityControlResponseError <- function(dataset, options) {
  #Error check: Check for infinity, NA
  .hasErrors(dataset, type = c('infinity', 'missingValues','observations'),
             all.target = c(options[["rsmVariables"]],options[["rsmResponseVariables"]], observations.amount = c('<1')), exitAnalysisIfErrors = T)
}

.qualityControlDesignChoice <- function(jaspResults,options, dataset) {


  if (is.null(jaspResults[["showDesign"]]))

    DesignChoiceTable <- createJaspTable(gettext("Design Choice"))

    DesignChoiceTable$dependOn(options = c("factorResponse","showDesign", "responseSurfaceCenter","responseSurfaceBlocks",    "responseSurfaceCenterPointStar","responseSurfaceReplicationStar"))

    DesignChoiceTable$addCitation("Lenth, R.V. (2020). Response-Surface Methods in R, Using rsm.")

    DesignChoiceTable$addColumnInfo(name = "n.c",        title = "N Fac Points",                                type = "integer")
    DesignChoiceTable$addColumnInfo(name = "n0.c",       title = "N Center Points/Block",                       type = "integer")
    DesignChoiceTable$addColumnInfo(name = "blks.c",     title = "N Cube Blocks/Cube Portion",                  type = "integer")
    DesignChoiceTable$addColumnInfo(name = "n.s",        title = "N Design Star Points/Block",                  type = "integer")
    DesignChoiceTable$addColumnInfo(name = "n0.s",       title = "N Center Points/Star Block",                  type = "integer")
    DesignChoiceTable$addColumnInfo(name = "bbr.c",      title = "N Copies of Each Cube Block",                 type = "number")
    DesignChoiceTable$addColumnInfo(name = "wbr.s",      title = "N Replications of Each Star Point/Block",     type = "number")
    DesignChoiceTable$addColumnInfo(name = "bbr.s",      title = "N Copies of Each Star Block",                 type = "number")
    DesignChoiceTable$addColumnInfo(name = "N",          title = "Total Experiment Size",                       type = "integer")
    DesignChoiceTable$addColumnInfo(name = "alpha.rot",  title = "Rotational Alpha",                            type = "number")
    DesignChoiceTable$addColumnInfo(name = "alpha.orth", title = "Orthoganal Alpha",                            type = "number")

    message <- "n."
    
    jaspResults[["showDesign"]] <- DesignChoiceTable

    .qualityControlFillChoice(DesignChoiceTable, options)


    return()
}

.qualityControlFillChoice <- function(DesignChoiceTable, options) {

  k             <- options[["factorResponse"]]
  n0.c          <- options[["responseSurfaceCenter"]]
  blks.c        <- options[["responseSurfaceBlocks"]]
  n0.s          <- options[["responseSurfaceCenterPointStar"]] 
  wbr.s         <- options[["responseSurfaceReplicationStar"]]

  
  

  results       <- rsm::ccd.pick(k,
                                 n0.c   = n0.c,
                                 n.c    = c(2^k,2^(k+1)),
                                 blks.c = c(1,2,4),
                                 n0.s   = n0.s,
                                 wbr.s  = 1:wbr.s
  )
                                 
                                

  DesignChoiceTable$setData(list(n.c        = results$n.c,
                                 n0.c       = results$n0.c,
                                 blks.c     = results$blks.c,
                                 n.s        = results$n.s,
                                 n0.s       = results$n0.s,
                                 bbr.c      = results$bbr.c,
                                 wbr.s      = results$wbr.s,
                                 bbr.s      = results$bbr.s,
                                 N          = results$N,
                                 alpha.rot  = results$alpha.rot,
                                 alpha.orth = results$alpha.orth))

  return()

}


.responseSurfaceContour <- function(jaspResults, options, position, dataset) {
  ready <- 1

  if (is.null(jaspResults[["ContourPlot"]]))
    .responseSurfaceContourPlot(jaspResults, dataset, options)

}


.responseSurfaceContourPlot <- function(jaspResults, dataset, options, ready) {
  contourPlot <- createJaspPlot(title = "Contour Plot", width = 540, height = 540)

  contourPlot$dependOn(c("rsmVariables", "rsmResponseVariables", "rsmBlocks", "firstVar","secondVar", "contour"))

  jaspResults[["contourPlot"]] <- contourPlot
  
  if (length(options[["rsmVariables"]]) == 2 & length(options[["rsmResponseVariables"]]) >= 1 & length(options[["rsmBlocks"]]) >= 1)
    .responseSurfaceContourFill(contourPlot, options)

  return()
}


.responseSurfaceContourFill <- function(contourPlot, options, dataset) {
  val1 <- options[["firstVar"]]
  val2 <- options[["secondVar"]]

  data <- .readDataSetToEnd(columns.as.numeric = c(options[["rsmVariables"]],options[["rsmResponseVariables"]]), columns.as.factor = options[["rsmBlocks"]])
  
  name <- vector()
  op1  <- length(options[["rsmVariables"]]) 
  op2  <- length(options[["rsmResponseVariables"]]) 
  op3  <- length(options[["rsmBlocks"]]) 
  

  name <- paste("v", 1:(op1+op2+op3), sep = "")

  names(data) <- name 
  data$v1 <- (data$v1 - mean(data$v1))/val1
  data$v2 <- (data$v2 - mean(data$v1))/val2

  var.code <- rsm::coded.data(data, x1 ~ v1, x2 ~ v2)

  var.rsm <- rsm::rsm(v3 ~ v4 + SO(x1, x2), data = var.code)
  

  cont <- function () {
    contour(var.rsm, ~ x1 + x2 , image = TRUE,
            at = summary(var.rsm)$canonical$xs
            )
  }

  contourPlot[["plotObject"]] <- cont

  return()
}





