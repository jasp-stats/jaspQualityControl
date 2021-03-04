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

    DesignChoiceTable$dependOn(options = c("factorResponse","showDesign", "responseSurfaceCenterReplicates", "responseSurfaceStarReplicates"))

    DesignChoiceTable$addCitation("Lenth, R.V. (2020). Response-Surface Methods in R, Using rsm.")

    DesignChoiceTable$addColumnInfo(name = "n.c",        title = "n.c",                  type = "integer")
    DesignChoiceTable$addColumnInfo(name = "n0.c",       title = "n0.c",                 type = "integer")
    DesignChoiceTable$addColumnInfo(name = "blks.c",     title = "blks.c",               type = "integer")
    DesignChoiceTable$addColumnInfo(name = "n.s",        title = "n.s",                  type = "integer")
    DesignChoiceTable$addColumnInfo(name = "n0.s",       title = "n0.s",                 type = "integer")
    DesignChoiceTable$addColumnInfo(name = "bbr.c",      title = "bbr.c",                type = "number")
    DesignChoiceTable$addColumnInfo(name = "wbr.s",      title = "wbr.s",                type = "number")
    DesignChoiceTable$addColumnInfo(name = "bbr.s",      title = "bbr.s",                type = "number")
    DesignChoiceTable$addColumnInfo(name = "N",          title = "N",                    type = "integer")
    DesignChoiceTable$addColumnInfo(name = "alpha.rot",  title = "alpha.rot",            type = "number")
    DesignChoiceTable$addColumnInfo(name = "alpha.orth", title = "alpha.orth",           type = "number")

    jaspResults[["showDesign"]] <- DesignChoiceTable

    .qualityControlFillChoice(DesignChoiceTable, options)


    return()
}

.qualityControlFillChoice <- function(DesignChoiceTable, options) {

  oper          <- options[["factorResponse"]]
  n0.c          <- options[["responseSurfaceCenterReplicates"]]
  n0.s          <- options[["responseSurfaceStarReplicates"]]

  results       <- rsm::ccd.pick(oper,
                                 n0.c = n0.c,
                                 n.c = c(8, 16),
                                 n0.s = n0.s,
                                 blks.c = c(1, 2, 4),
                                 wbr.s = 1:2,
                                 restrict = "N<=65")

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

  .responseSurfaceContourFill(contourPlot, options)

  return()
}


.responseSurfaceContourFill <- function(contourPlot, options, dataset) {
  val1 <- options[["firstVar"]]
  val2 <- options[["secondVar"]]

  data <- .readDataSetToEnd(columns.as.numeric = c(options[["rsmVariables"]],options[["rsmResponseVariables"]]), columns.as.factor = options[["rsmBlocks"]])
  names(data) <- c("v1", "v2", "v3", "v4")
  data$v1 <- (data$v1 - mean(data$v1))/val1
  data$v2 <- (data$v2 - mean(data$v1))/val2

  var.code <- rsm::coded.data(data, x1 ~ v1, x2 ~ v2)

  var.rsm <- rsm::rsm(v3 ~ v4 + SO(x1, x2), data = var.code)

  cont <- function () {
    contour(var.rsm, ~ x1 + x2 , image = TRUE,
            at = summary(var.rsm)$canonical$xs)
  }

  contourPlot[["plotObject"]] <- cont

  return()
}





