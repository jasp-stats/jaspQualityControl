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

  op1  <- length(options[["rsmVariables"]])
  op2  <- length(options[["rsmResponseVariables"]])
  op3  <- length(options[["rsmBlocks"]])


  if (op1 > 0 & op2 > 0) {
    rsm <- list()
    if (op2 == 1) {
      data <- .readDataSet(jaspResults, options, dataset, 1)
      rsm[[1]] <- .responseSurfaceCalculate(jaspResults, options, dataset, data)

      if (options[["showDesign"]])
        .qualityControlDesignMainRSM(jaspResults,options, position = 1)

      if (options[["contour"]])
        .responseSurfaceContour(jaspResults, options, position = 2, data, i = 1)

      if(options[["coef"]])
        .responseSurfaceTableCall(jaspResults, options, rsm[[1]], position = 3)

      if(options[["anova"]])
        .responseSurfaceTableAnovaCall(jaspResults, options, rsm[[1]], position = 4)

      # if(options[["eigen"]])
      #   .responseSurfaceTableEigenCall(jaspResults, options, rsm, position = 5)

      if(options[["res"]])
        .responsePlotResidualCall(jaspResults, options, rsm[[1]], position = 6)

      if(options[["pareto"]])
        .responsePlotPareto(jaspResults, options, rsm[[1]], position = 7)

      if(options[["resNorm"]])
        .responsePlotResNorm(jaspResults, options, rsm[[1]], position = 8)

      if(options[["ResFitted"]])
        .responsePlotResFitted(jaspResults, options, rsm[[1]], position = 9)
    }else {
      for (i in 1:op2) {
        data <- .readDataSet(jaspResults, options, dataset, i)
        rsm[[i]] <- .responseSurfaceCalculate(jaspResults, options, dataset, data)

        if (options[["showDesign"]])
          .qualityControlDesignMainRSM(jaspResults,options, position = 1)

        if (options[["contour"]])
          .responseSurfaceContour(jaspResults, options, position = 2, data, i)

        if(options[["coef"]])
          .responseSurfaceTableCall(jaspResults, options, rsm[[i]], position = 3)

        if(options[["anova"]])
          .responseSurfaceTableAnovaCall(jaspResults, options, rsm[[i]], position = 4)

        # if(options[["eigen"]])
        #   .responseSurfaceTableEigenCall(jaspResults, options, rsm, position = 5)

        if(options[["res"]])
          .responsePlotResidualCall(jaspResults, options, rsm[[i]], position = 6)

        if(options[["pareto"]])
          .responsePlotPareto(jaspResults, options, rsm[[i]], position = 7)

        if(options[["resNorm"]])
          .responsePlotResNorm(jaspResults, options, rsm[[i]], position = 8)

        if(options[["ResFitted"]])
          .responsePlotResFitted(jaspResults, options, rsm[[i]], position = 9)

      }
    }
  }



  #.responseSurfaceOptimize(jaspResults, options, rsm, data, position = 10, dataset)

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

.readDataSet <- function(jaspResults, options, dataset, i) {
  if (options[["rsmBlocks"]] != ""){
    data <- .readDataSetToEnd(columns.as.numeric =   c(matrix(unlist(options[["rsmVariables"]]),ncol=2,byrow=TRUE)[,2],
                                                       options[["rsmResponseVariables"]][[i]]),
                              columns.as.factor  =   options[["rsmBlocks"]])
  }else{
    data <- .readDataSetToEnd(columns.as.numeric = c(matrix(unlist(options[["rsmVariables"]]),ncol=2,byrow=TRUE)[,2],
                                                     options[["rsmResponseVariables"]][[i]]))
  }
}

.responseSurfaceCalculate <- function(jaspResults,options, dataset, data) {
  op1  <- length(options[["rsmVariables"]])
  op2  <- length(options[["rsmResponseVariables"]])
  op3  <- length(options[["rsmBlocks"]])


  name <- vector()

  mean.col <- apply(data, 2, mean)

  opt1 <- colnames(data)[1:op1]
  opt2 <- colnames(data)[(op1+1)]
  if (options[["rsmBlocks"]] != "") {
    opt3 <- colnames(data)[(op1+2)]
  }else{
    data[,(op1+2)] <- rep(1, times = nrow(data))
  }

  optio <- matrix(unlist(options[["rsmVariables"]]),ncol=2,byrow=TRUE)[,2]
  data.list <- list()


  for (i in 1:op1) {
    data.list[[i]] <- as.formula(paste0("x",i , " ~ ", "(", opt1[i], "-",
                                        mean(data[,i]), ")/",
                                        abs(data[1,i] - mean(data[,i]))))
  }

  var.code <- rsm::coded.data(data)

  vari <- matrix(unlist(options[["rsmVariables"]]),ncol = 2, byrow = T)[,2]
  len_mo <- length(options[["modelTerms"]])

  pq <- character()
  fo <- character()
  for (i in seq_along(options[["modelTerms"]])) {
    if (!any(options[["modelTerms"]][[i]][[2]] %in% vari == "FALSE")) {
      if (options[["modelTerms"]][[i]][[1]] == "fopq" & length(options[["modelTerms"]][[i]][[2]]) == 1) {
        pq[i] <- paste("x", which(vari %in% options[["modelTerms"]][[i]][[2]]), sep = "")
        fo[i] <- paste("x", which(vari %in% options[["modelTerms"]][[i]][[2]]), sep = "")

      }else if (options[["modelTerms"]][[i]][[1]] == "fo" & length(options[["modelTerms"]][[i]][[2]])) {
        fo[i] <- paste("x", which(vari %in% options[["modelTerms"]][[i]][[2]]), sep = "")

      }
    }
  }
  #print(pq)
  pq <- pq[!is.na(pq)]
  fo <- fo[!is.na(fo)]
  #print(fo)
  start <- ifelse(length(fo) == 0, 0, 1) + ifelse(length(pq) == 0, 0 ,1)
  formula_str <- rep("", times = start + len_mo - op1)
  if (length (fo) > 0){
    fo <- paste(fo, collapse = ",")
    formula_str[1] <- paste("FO(",fo,")", sep = "")
  }
  if (length (pq) > 0){
    pq <- paste(pq, collapse = ",")
    formula_str[2] <- paste("PQ(", pq, ")", sep = "")
  }
  #print(formula_str)

  op1 <- length(options[["rsmVariables"]])

  l_gen <- 1:op1

  for (i in seq_along(options[["modelTerms"]])) {

    if (length(options[["modelTerms"]][[i]][[2]]) > 1) {

      jo <- character(length = length(options[["modelTerms"]][[i]][[2]]))

      for (j in seq_along(formula_str)){

        if (formula_str[j] == "") {


          optio_2 <- options[["modelTerms"]][[i]][[2]][[1]]

          jo[1] <- paste("x",l_gen[vari %in% optio_2], sep = "")

          for (k in seq_along(jo)) {

            if (jo[k] == "") {

              optio_3 <- options[["modelTerms"]][[i]][[2]][[k]]

              jo[k] <- paste(",x", l_gen[vari %in% optio_3], sep = "")

            }
          }
          jo_2 <- paste(jo, collapse = "")
          jo_3 <- paste("TWI(",jo_2,")", sep = "")
          formula_str[j] <- jo_3
          break()
        }


      }
    }
  }
  # print(formula_str)
  # print(paste(formula_str, collapse = "+"))

  if (length(unique(data[,(op1+2)])) > 1){
    form <- paste(formula_str, collapse = "+")

    form_2 <- paste(opt2, "~", options[["rsmBlocks"]], "+", form, sep = "")

    form_3 <- as.formula(form_2)


     # str3 <- as.formula(paste(opt2, "~",
    #                          opt3,
    #                          "+", options[["Formula"]], sep = ""))
  }else {
    form <- paste(formula_str, collapse = "+")
    # print(form)
    form_2 <- paste(opt2, "~", form, sep = "")
    # print(paste(opt2, "~", form, sep = ""))
    form_3 <- as.formula(form_2)
    # print(form_3)

    # str3 <- as.formula(paste(opt2, "~", options[["Formula"]], sep = ""))
  }

  rsm <- rsm::rsm(form_3 , data = var.code)


}


.responseSurfaceContour <- function(jaspResults, options, position, data, i, dataset) {
  ready <- 1


  if (is.null(jaspResults[["ContourPlot"]]))
    .responseSurfaceContourPlot(jaspResults, dataset, options, data, lol = i)

}


.responseSurfaceCheckError <- function(dataset, options) {
  #Error 1: Does
}

.responseSurfaceContourPlot <- function(jaspResults, dataset, options, data, lol) {
  contourPlot <- createJaspContainer(title = paste(options[["rsmResponseVariables"]][[lol]],"Contour Plot", sep = ""))

  op1  <- length(options[["rsmVariables"]])
  op2  <- length(options[["rsmResponseVariables"]])
  op3  <- length(options[["rsmBlocks"]])



  op_pair <- length(options[["pairs"]])

  jaspResults[["contourPlot"]] <- contourPlot

  if (op_pair == 0 & options[["contour"]]) {
    jaspResults[["contourPlot"]][["1"]] <- createJaspPlot(width = 400, height = 400)

    return ()
  }else if(op2 >= 1) {

    pair <- options[["pairs"]]



    optio <- matrix(unlist(options[["rsmVariables"]]),ncol=2,byrow=TRUE)[,2]




    l_gen <- 1:op1

    col <- 1

    point_vec <- c()

    for (i in seq_along(matrix(unlist(options[["rsmVariables"]]),ncol=2,byrow=TRUE)[,1])) {
      point_vec[i] <- matrix(unlist(options[["rsmVariables"]]),ncol=2,byrow=TRUE)[i,1]
    }
    names(point_vec) <- paste("x", seq_along(options[["rsmVariables"]]), sep = "")



    for (i in pair){

      if (!(i[1] == "") & !(i[2] == "")) {

        plot <- createJaspPlot(
          title = paste(i[1], "-", i[2], " Plot", sep = ""),
          width = 400, height = 400)
        plot$dependOn(c("rsmResponseVariables",
                        "rsmBlocks",
                        "contour", "psi", "theta","pairs", "Component", "Point"))
        contourPlot[[paste0("plotObject", col, sep = "")]] <- plot



        po <- as.formula(paste("~x", l_gen[optio %in% i[1]],
                               "+", "x",
                               l_gen[optio %in% i[2]], sep = ""))

        point_spec_r <- c()
        k <- 1
        for (j in 1:op1) {
          if (!(l_gen[optio %in% i[1]] == j || l_gen[optio %in% i[2]] == j)){
            point_spec_r[k] <- point_vec[j]
            names(point_spec_r)[k] <- paste("x", j, sep = "")
            k <- k + 1
          }
        }

        if (is.null(point_spec_r)) {
          for (j in 1:op1) {
            if (l_gen[optio %in% i[1]] == j || l_gen[optio %in% i[2]] == j){
              point_spec_r[k] <- point_vec[j]
              names(point_spec_r)[k] <- paste("x", j, sep = "")
              k <- k + 1
            }
          }
        }
        # print(point_spec_r)

        heli.rsm  <- .responseSurfaceCalculate(jaspResults, options, dataset, data)
        .responseSurfaceContourFill(contourPlot[[paste0("plotObject", col, sep = "")]],
                                    heli.rsm, po, options, point_spec_r, lol)
        col <- col + 1
      }

    }

  }

  return()
}

.responsePlotResNorm <- function(jaspResults, options, rsm, position, dataset){
  if (!(is.null(jaspResults[["resNorm"]]))){
    return()
  }
  plot <- createJaspPlot(title = "Normal Probability Plot of Residuals",
                         width = 400, height = 400)
  jaspResults[["resNorm"]] <- plot

  plot$dependOn(c("resNorm", "rsmBlocks",
                  "rsmResponseVariables",
                  "rsmVariables","modelTerms"))
  p <- jaspGraphs::plotQQnorm(resid(rsm))

  plot$plotObject <- p

  return()
}



.responsePlotResidualCall <- function(jaspResults, options, rsm, position, dataset) {
  if (!(is.null(jaspResults[["Residual"]]))) {
    return()
  }
  plot <- createJaspPlot(
    title = "Residual Plot",
    width = 400, height = 400)
  plot$dependOn(c("rsmResponseVariables",
                  "rsmBlocks",
                  "res", "modelTerms"))


  jaspResults[["Residual"]] <- plot

  x <- resid(rsm)
  #print(x)
  h <- hist(x, plot = FALSE)
  #print(h)


  p <- ggplot2::ggplot(data.frame(x), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(binwidth = abs(h$breaks[1] - h$breaks[2])) +
    ggplot2::labs(y = "Count", x = "Residuals")


  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p
  return()



}

.responsePlotResFitted <- function(jaspResults, options, rsm, position, dataset){

  if (!(is.null(jaspResults[["ResFitted"]]))){
   return()

  }
  plot <- createJaspPlot(title = "Residuals vs. Fitted Value", width = 400, height = 400)
  jaspResults[["ResFitted"]] <- plot
  plot$dependOn(c("resFitted","rsmBlocks",
                "rsmResponseVariables",
                "rsmVariables","modelTerms"))

  df <- data.frame(x = fitted(rsm), y = resid(rsm))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df$x)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(df$y)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
    ggplot2::scale_x_continuous(name = gettext("Fitted values"), limits = c(min(xBreaks), max(xBreaks)), breaks = xBreaks) +
    ggplot2::scale_y_continuous(name = gettext("Residuals"),     limits = c(min(yBreaks), max(yBreaks)), breaks = yBreaks)

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return()
}

.responsePlotPareto <- function(jaspResults, options, rsm, position, dataset) {

  if (!(is.null(jaspResults[["pareto"]]))) {
    return()
  }

  plot <- createJaspPlot(title = "Pareto Plot of Standardized Effects",
                         width = 400, height = 400)
  jaspResults[["pareto"]] <- plot
  plot$dependOn(c("pareto","rsmBlocks",
                  "rsmResponseVariables",
                  "rsmVariables", "modelTerms"))

  t <- abs(data.frame(summary(rsm)[[4]][,4]))
  name <- rownames(summary(rsm)[[4]])
  par <- cbind.data.frame(name, t)
  names(par)[2] <- "t"
  par <- par[rev(order(par$t)),]

  df <- summary(rsm)$df[2]
  crit <- abs(qt(0.025, df))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(abs(summary(rsm)[[4]][,4]))

  p <- ggplot2::ggplot(par, ggplot2::aes(y = name)) +
    ggplot2::geom_bar(ggplot2::aes(x = t), stat = "identity") +
    ggplot2::geom_vline(xintercept = crit, linetype = "dashed", color = "red") +
    ggplot2::labs(x = 'Standardized Effect', y ='Term') +
    ggplot2::scale_x_continuous(name = gettext("Standardized Effect"), limits = c(min(xBreaks), max(xBreaks)), breaks = xBreaks)

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return()

}



.responseSurfaceContourFill <- function(contourPlot,heli.rsm,po, options, point_spec_r, lol, dataset) {

  op1  <- length(options[["rsmVariables"]])
  op2  <- length(options[["rsmResponseVariables"]])
  op3  <- length(options[["rsmBlocks"]])

  opt2 <- options[["rsmResponseVariables"]][[lol]]

  #print(options[["Formula"]])


  # print(point_spec_r)
  # print(heli.rsm1)
  # print(po)
  contour.fill <- function() {
    plot <- persp(heli.rsm, po,
                  at = point_spec_r, contours = "colors",
                  col = rainbow(options["divide"]),
                  zlab = opt2,
                  box = T,
                  decode = F,
                  ticktype = "detailed",
                  phi = options[["phi"]]*360,
                  theta = (options[["theta"]]*360 + 330))
    #mtext(plot[[1]][[4]][[5]])
    if (options[["legend"]]){
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


  plot.contour <- function() {
    contour(heli.rsm,
            po,
            image = T,
            at = point_spec_r,
            contours = terrain.colors(5),
            decode = ifelse(options[["coded"]], F,T),
            las = 1)
  }

  if (options[["cplot"]]) {
    contourPlot$plotObject <- plot.contour
  }else {
    contourPlot$plotObject <- contour.fill
  }


  return()

}







.responseSurfaceTableCall <- function(jaspResults, options, rsm, dataset, position) {

  rsm <- summary(rsm)

  .responseSurfaceTable(jaspResults, options, dataset, rsm)

  return()
}

.responseSurfaceTable <- function(jaspResults, options, dataset, rsm) {

  TableContainer <- createJaspContainer()
  jaspResults[["TableContainer"]] <- TableContainer

  if (is.null(jaspResults[["TableContainer"]][["coef"]]))
    CoefTable <- createJaspTable(gettext("RSM Coefficients"))
  if (is.null(jaspResults[["TableContainer"]][["RSQTable"]]))
    RSQTable  <- createJaspTable()
  TableContainer$dependOn(     options = c("coef","modelTerms", "rsmBlocks",
                                      "rsmResponseVariables",
                                      "rsmVariables"))


  CoefTable$addColumnInfo(name = "names",title = " ")
  CoefTable$addColumnInfo(name = "est",  title = "Estimate")
  CoefTable$addColumnInfo(name = "std",  title = "Standard Error")
  CoefTable$addColumnInfo(name = "tval", title = "t")
  CoefTable$addColumnInfo(name = "pval", title = "p")

  RSQTable$addColumnInfo( name = "RSQ",   title = "Multiple R-squared")
  RSQTable$addColumnInfo( name = "ARSQ",  title = "Adjusted R-squared")
  RSQTable$addColumnInfo( name = "DF1",   title = "DF1")
  RSQTable$addColumnInfo( name = "DF2",   title = "DF2")
  RSQTable$addColumnInfo( name = "FStat", title = "F")
  RSQTable$addColumnInfo( name = "pval_2",title = "p")

  jaspResults[["TableContainer"]][["coef"]] <- CoefTable
  jaspResults[["TableContainer"]][["RSQTable"]] <- RSQTable




  .responseSurfaceTableFill(TableContainer, options,rsm)




  return()
}


.responseSurfaceTableFill <- function(TableContainer, options, rsm) {


  TableContainer[["coef"]]$setData(list(names = rownames(rsm[[4]]),
                                        est  = round(rsm[[4]][,1],3),
                                        std  = round(rsm[[4]][,2],3),
                                        tval = round(rsm[[4]][,3],3),
                                        pval = ifelse(rsm[[4]][,4] > 0.001,round(rsm[[4]][,4],3), "< .001")))

  TableContainer[["RSQTable"]]$setData(list(RSQ    = round(rsm[[8]],3),
                                            ARSQ   = round(rsm[[9]],3),
                                            DF1    = rsm[[10]][[2]],
                                            DF2    = rsm[[10]][[3]],
                                            FStat  = round(rsm[[10]][[1]],3),
                                            pval_2 = ifelse((1 - pf(rsm[[10]][[1]], rsm[[10]][[2]], rsm[[10]][[3]])) > 0.001,
                                                            round(1 - pf(rsm[[10]][[1]], rsm[[10]][[2]], rsm[[10]][[3]]),3),
                                                            "<.001")))






  return()
}

.responseSurfaceTableAnovaCall <- function(jaspResults, options, rsm, position, dataset) {

  rsm <- summary(rsm)

  .responseSurfaceAnovaTable(jaspResults, options, dataset, rsm)

  return()
}


.responseSurfaceAnovaTable <- function(jaspResults, options, dataset,rsm) {


  AnovaTable <- createJaspTable(gettext("ANOVA"))


  AnovaTable$dependOn(      options = c("anova","modelTerms", "rsmBlocks",
                                       "rsmResponseVariables",
                                       "rsmVariables"))
  AnovaTable$addColumnInfo( name = "names",     title = " ")
  AnovaTable$addColumnInfo( name = "Df",       title = "DF")
  AnovaTable$addColumnInfo( name = "Sum",      title = "Sum of Squares")
  AnovaTable$addColumnInfo( name = "Mean",     title = "Mean of Squares")
  AnovaTable$addColumnInfo( name = "FValue",   title = "F")
  AnovaTable$addColumnInfo( name = "PValue",   title = "p")

  jaspResults[["anova"]] <- AnovaTable

  .responseSurfaceAnovaFill(AnovaTable, jaspResults, options,rsm)

  return()
}



.responseSurfaceAnovaFill <- function(AnovaTable, jaspResults, options,rsm) {

 jaspResults[["anova"]]$setData(list(    names  = rownames(rsm[[13]]),
                                         Df     = round(rsm[[13]][[1]],3),
                                         Sum    = round(rsm[[13]][[2]],3),
                                         Mean   = round(rsm[[13]][[3]],3),
                                         FValue = round(rsm[[13]][[4]],3),
                                         PValue = ifelse(rsm[[13]][[5]] > 0.001,
                                                         round(rsm[[13]][[5]],3),
                                                         "<.001")))

  return()
}

.responseSurfaceTableEigenCall <- function(jaspResults, options, rsm, position, dataset) {
  rsm <- summary(rsm)


  .responseSurfaceEigenTable(jaspResults, options, dataset, rsm)

  return()
}

.responseSurfaceEigenTable <- function(jaspResults, options, dataset, rsm) {

  eigen      <- createJaspContainer()
  jaspResults[["eigen"]] <- eigen

  if (is.null(eigen[["XTable"]]))
    XTable <- createJaspTable(title = gettext("Stationary Points of Response Surface (Coded)"))
  if (is.null(eigen[["EigenValue"]]))
    EigenValue <- createJaspTable(title = gettext("Eigenvalues"))

  if (is.null(eigen[["EigenVectors"]]))
    EigenVector <- createJaspTable(title = gettext("Eigenvectors"))

  eigen$dependOn(      options = c("eigen","modelTerms", "rsmBlocks",
                                        "rsmResponseVariables",
                                        "rsmVariables"))


  op1  <- length(options[["rsmVariables"]])
  EigenVector$addColumnInfo(name = "names", title = "")
  for (i in 1:op1) {
    XTable$addColumnInfo(name = paste0("x",i, sep = ""), title = paste0("x",i, sep = ""))
    EigenValue$addColumnInfo(name = paste0("x",i, sep = ""), title = "")
    EigenVector$addColumnInfo(name = paste0("x",i, sep = ""), title = "")
  }

  eigen[["XTable"]] <- XTable
  eigen[["EigenValue"]] <- EigenValue
  eigen[["EigenVectors"]] <- EigenVector

  .responseSurfaceTableEigenFill(eigen, jaspResults, options, rsm)

  return()
}

.responseSurfaceTableEigenFill <- function(eigen, jaspResults, options, rsm) {

  op1  <- length(options[["rsmVariables"]])

  eigen[["XTable"]]$addRows(round(rsm[[12]][[1]],3))
  eigen[["EigenValue"]]$setData(round(rsm[[12]][[2]][[1]],3))
  eigen[["EigenVectors"]]$setData(list(names = rownames(rsm[[12]][[2]][[2]])))

  m <- as.data.frame(matrix(0, ncol = op1 + 1, nrow = op1))
  m[,1] <- rownames(rsm[[12]][[2]][[2]])
  colnames(m) <- rep("", times = op1+1)

  for (i in 1:op1) {
    m[,i+1] <- round(rsm[[12]][[2]][[2]][,i],3)
  }

  eigen[["EigenVectors"]]$setData(m)

  return()
}

.responseSurfaceOptimize <- function(jaspResults, options, rsm, data, position, dataset) {

  desire_list <- list()
  k <- 1
  if (length(options[["rsmMin"]]) > 0) {
    for (i in 1:length(options[["rsmMin"]])) {
      desire_list[[k]] <- desirability::dMin(options[["rsmMin"]][[i]][[2]], options[["rsmMin"]][[i]][[1]])
      k <- k +1
    }
  }
  if (length(options[["rsmMax"]]) > 0) {
    for (i in 1:length(options[["rsmMax"]])) {
      desire_list[[k]] <- desirability::dMin(options[["rsmMax"]][[i]][[2]], options[["rsmMax"]][[i]][[1]])
      k <- k +1
    }
  }
  # if (length(options[["rsmTar"]]) > 0) {
  #   for (i in 1:length(options[["rsmTar"]])) {
  #     desire_list[[k]] <- desirability::dMin(options[["rsmMax"]][[i]][[2]], , options[["rsmMax"]][[i]][[1]])
  #     k <- k +1
  #   }
  # }

  desire_final <- do.call(desirability::dOverall, desire_list)

  if (length(options[["rsmMax"]]) + length(options[["rsmMin"]]) + length(options[["rsmTar"]]) == 1)
    desire_final <- desire_final[[1]][[1]]

  op  <- data.frame(x1 = seq(-1,1,0.25))
  op1 <- length(options[["rsmVariables"]])

  if (op1 > 1) {
    for (i in 2:op1) {
      op <- cbind(op, seq(-1,1,0.25))
    }
    names(op) <- paste("x",1:op1, sep = "")
  }

  var.code <- rsm::coded.data(data)

  you <- expand.grid(op)

  # print(desire_final)
  # print(length(desire_final))
  # print(predict(rsm, newdata = you[i,]))
  # print(desire_final)

  rsmOpt_2 <- function(you_2, rsm, dObject, space = "square") {
    conv <- predict(rsm, newdata = you[i,])
    out <- predict(desire_final, newdata = as.vector(conv))

    if (space == "circular") {
      if (sqrt(sum(you[i,]^2)) > 1.682)
        out <- 0
    }
    else if (space == "square")
      if (any(abs(you[i,]) > 1.682))
        out <- 0
    out
  }

  tmp <- list()
  for (i in 1:dim(you)[1]) {
    tmp[[i]] <- optim(as.vector(you[i,]), rsmOpt_2, dObject = desire_final,
                      rsm = rsm, space = "square", control = list(fnscale = -1))
    if (i == 1) {
      best_2 <- tmp[[i]]
    }else {
      if (tmp[[i]][["value"]] > best_2[["value"]])
        best_2 <- tmp[[i]]
    }
  }



  desirability_table <- createJaspContainer()
  jaspResults[["desirability_table"]] <- desirability_table
  desirability_table$dependOn(options = c("rsmMin", "rsmMax", "rsmVariables"))


  if (is.null(desirability_table[["predictor_value"]])) {
    predictor_value <- createJaspTable(title = "Predictor Values")
  }

  if (is.null(desirability_table[["value"]])) {
    value <- createJaspTable(title = "Overall Desirability")
    value$addColumnInfo(name = "Value", title = "Value")
  }

  for (i in 1:op1) {
    predictor_value$addColumnInfo(name = paste0("x",i, sep = ""), title = paste0("x",i, sep = ""))
  }


  desirability_table[["predictor_value"]] <- predictor_value
  desirability_table[["value"]] <- value

  .responseSurfaceOptimizeFill(predictor_value, value, desirability_table, best_2, jaspResults, options, dataset)

  return()
}

.responseSurfaceOptimizeFill <- function(predictor_value, value, desirability_table, best_2, jaspResults, options, dataset) {

  desirability_table[["predictor_value"]]$addRows(best_2[["par"]])
  desirability_table[["value"]]$setData(round(best_2[["value"]],3))

  return()
}
