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

doeResponseSurfaceMethodology <- function(jaspResults, dataset, options, ...) {
  if (options[["showDesign"]])

    .qualityControlDesignMainRSM(jaspResults,options, position = 1)

  if (options[["contour"]])
    .responseSurfaceContour(jaspResults, options, position = 2)
}

.qualityCOntrolResponseReadData <- function(dataset, options) {
  if (!is.null(dataset))
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
  DesignChoiceTable$addColumnInfo(name = "n0.c",       title = "N Centre Points/Block",                       type = "integer")
  DesignChoiceTable$addColumnInfo(name = "blks.c",     title = "N Cube Blocks/Cube Portion",                  type = "integer")
  DesignChoiceTable$addColumnInfo(name = "n.s",        title = "N Design Star Points/Block",                  type = "integer")
  DesignChoiceTable$addColumnInfo(name = "n0.s",       title = "N Centre Points/Star Block",                  type = "integer")
  DesignChoiceTable$addColumnInfo(name = "bbr.c",      title = "N Copies of Each Cube Block",                 type = "integer")
  DesignChoiceTable$addColumnInfo(name = "wbr.s",      title = "N Replications of Each Star Point/Block",     type = "integer")
  DesignChoiceTable$addColumnInfo(name = "bbr.s",      title = "N Copies of Each Star Block",                 type = "integer")
  DesignChoiceTable$addColumnInfo(name = "N",          title = "Total Experiment Size",                       type = "integer")
  DesignChoiceTable$addColumnInfo(name = "alpha.rot",  title = "Rotational Alpha",                            type = "number")
  DesignChoiceTable$addColumnInfo(name = "alpha.orth", title = "Orthogonal Alpha",                            type = "number")

  message <- "n."

  jaspResults[["showDesign"]] <- DesignChoiceTable

  .qualityControlFillChoice(DesignChoiceTable, options)


  return()
}

.qualityControlFillChoice <- function(DesignChoiceTable, options) {

  k             <- options[["factorResponse"]]
  n0.c          <- options[["responseSurfaceCentre"]]
  wbr.s         <- options[["responseSurfaceReplicationStar"]]




  results       <- rsm::ccd.pick(k,
                                 n0.c   = n0.c,
                                 n.c    = c(2^k,2^(k+1)),
                                 wbr.s  = 1:wbr.s
  )



  DesignChoiceTable$setData(list(n.c        = results$n.c,
                                 n0.c       = results$n0.c,
                                 blks.c     = results$blks.c,
                                 n.s        = results$n.s,
                                 n0.s       = results$n0.s,
                                 bbr.c      = as.integer(results$bbr.c),
                                 wbr.s      = as.integer(results$wbr.s),
                                 bbr.s      = as.integer(results$bbr.s),
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
  contourPlot <- createJaspContainer(title = "Contour Plot")

  op1  <- length(options[["rsmVariables"]])
  print(op1)
  op2  <- length(options[["rsmResponseVariables"]])
  op3  <- length(options[["rsmBlocks"]])

  print(op3)


  op_pair <- length(options[["pairs"]])

  jaspResults[["contourPlot"]] <- contourPlot

  if (op_pair == 0 & options[["contour"]]) {
    jaspResults[["contourPlot"]][["1"]] <- createJaspPlot()
    return ()
  }else {

    data <- .readDataSetToEnd(columns.as.numeric = c(options[["rsmVariables"]],
                                                     options[["rsmResponseVariables"]]),
                              columns.as.factor  = options[["rsmBlocks"]])


    print(data)


    pair <- options[["pairs"]]

    name <- vector()

    mean.col <- apply(data, 2, mean)

    opt1 <- colnames(data)[1:op1]
    opt2 <- colnames(data)[(op1+1)]
    if (op3 > 0) {
      opt3 <- colnames(data)[(op1+2)]
    }


    optio <- options[["rsmVariables"]]
    data.list <- list()


    for (i in 1:op1) {
      data.list[[i]] <- as.formula(paste0("x", i, " ~ ", "(", opt1[i], "-",
                                          mean(data[,i]), ")/",
                                          abs(data[1,i] - mean(data[,i]))))
      if (options[["coded"]])
        data[,i] <- (data[,i] - mean(data[,i]))/(abs(data[1,i] - mean(data[,i])))
    }

    var.code <- rsm::coded.data(data, formulas = data.list)
    print(var.code)

    if (op_pair > 0) {
      for (i in 1:op_pair) {


        plot <- createJaspPlot(
          title = paste0("Plot", i, sep = " "),
          width = 320, height = 320)
        plot$dependOn(c("rsmResponseVariables",
                        "rsmBlocks",
                        "contour", "psi", "theta","pairs"))
        contourPlot[[paste0("plotObject", i, sep = "")]] <- plot
      }
    }




    l_gen <- 1:op1

    col <- 1

    for (i in pair) {

      if (!(i[1] == "") & !(i[2] == "")) {
        str1 <- paste("x", l_gen[optio %in% i[1]],",","x",
                      l_gen[optio %in% i[2]], sep = "")
        str2 <-  paste("SO(", str1, ")", sep = "")
        if (length(unique(data[,(op1+2)])) > 1) {
          str3 <- as.formula(paste(opt2, "~",
                                   opt3,
                                   "+", str2, sep = ""))
        }else {
          str3 <- as.formula(paste(opt2, "~", str2, sep = ""))
        }
        po <- as.formula(paste("~x", l_gen[optio %in% i[1]],
                               "+", "x",
                               l_gen[optio %in% i[2]], sep = ""))
        heli.rsm1 <- rsm::rsm(str3, data = var.code)
        .responseSurfaceContourFill(contourPlot[[paste0("plotObject", col, sep = "")]],
                                    heli.rsm1, po, options)
        col <- col + 1
      }

    }




  }



  return()




}


.responseSurfaceContourFill <- function(contourPlot,heli.rsm1,po, options, dataset) {

  op1  <- length(options[["rsmVariables"]])
  op2  <- length(options[["rsmResponseVariables"]])
  op3  <- length(options[["rsmBlocks"]])

  opt2 <- options[["rsmResponseVariables"]]
  contour.fill <- function () {
    plot <- persp(heli.rsm1, po,
                  at = summary(heli.rsm1)$canonical$xs, contours = "colors",
                  col = rainbow(options["divide"]),
                  zlab = opt2,
                  ticktype = "detailed",
                  phi = options[["phi"]]*360,
                  theta = (options[["theta"]]*360 + 330))
    if (options[["legend"]]) {
      #Divide the difference between the upper and lower z-axis boundary by number of colours
      z_step <- (plot[[1]][[5]][[2]] - plot[[1]][[5]][[1]])/(length(rainbow(options["divide"])))
      #Create a string of intervals corresponding to each colour
      string <- c()
      for (i in 1:length(rainbow(options["divide"]))) {
        string[i] <- paste(as.character(round(plot[[1]][[5]][[1]]+(i-1)*z_step,1)), "-",
                           as.character(round(plot[[1]][[5]][[1]]+i*z_step,1)), sep = "")
      }
      legend(x = "topright", legend = string, fill = rainbow(options["divide"]), text.width = 0.1, cex = 0.9)
    }

  }


  contourPlot$plotObject <- contour.fill






  return()
}





