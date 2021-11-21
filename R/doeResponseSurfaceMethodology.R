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
    #Placeholder table when the user inputs something. If an analysis gets picked, remove the table
    if(!(any(options[["showDesign"]], options[["contour"]], options[["coef"]], options[["anova"]],
             options[["res"]], options[["pareto"]], options[["resNorm"]], options[["ResFitted"]]))) {
      placeholder <- createJaspTable(title = gettext(" "))
      jaspResults[["placeholder"]] <- placeholder
    }else{
      jaspResults[["placeholder"]] <- NULL
    }

    for (i in 1:op2) {
      data <- .readDataSet(jaspResults, options, dataset, i)

      .dataErrorCheck(data)

      rsm[[i]] <- .responseSurfaceCalculate(jaspResults, options, dataset, data)

      if (options[["showDesign"]])
        .qualityControlDesignMainRSM(jaspResults,options, position = 1)

      if (options[["contour"]])
        .responseSurfaceContour(jaspResults, options, data, i, position = 2)

      if(options[["coef"]])
        .responseSurfaceTableCall(jaspResults, options, rsm[[i]], i, position = 3)

      if(options[["anova"]])
        .responseSurfaceTableAnovaCall(jaspResults, options, rsm[[i]], i, position = 4)

      # if(options[["eigen"]])
      #   .responseSurfaceTableEigenCall(jaspResults, options, rsm, position = 5)

      if(options[["res"]])
        .responsePlotResidualCall(jaspResults, options, rsm[[i]], i, position = 6)

      if(options[["pareto"]])
        .responsePlotPareto(jaspResults, options, rsm[[i]], i, position = 7)

      if(options[["resNorm"]])
        .responsePlotResNorm(jaspResults, options, rsm[[i]], i, position = 8)

      if(options[["ResFitted"]])
        .responsePlotResFitted(jaspResults, options, rsm[[i]],i, position = 9)

      }
  }


  if(options[["desirability"]])
    .responseSurfaceOptimize(jaspResults, options, rsm, data, position = 10, dataset)

}

.dataErrorCheck <- function(data) {
  .hasErrors(dataset = data,
             custom = function() {
               if(any(unique(data[,seq_along(options[["rsmVariables"]])]) > 5))
                 return(gettext("This analysis does not take predictor variables with more than 5 unique values."))
             },
             exitAnalysisIfErrors = T)
}

.qualityControlDesignMainRSM <- function(jaspResults,options,dataset,position) {
  ready <- T

  if (is.null(jaspResults[["showDesign"]])) {
    .qualityControlDesignChoice(jaspResults, options, dataset)
  }

}



.qualityControlDesignChoice <- function(jaspResults,options, dataset) {


  if (is.null(jaspResults[["showDesign"]]))

    DesignChoiceTable <- createJaspTable(gettextf("Design Choice"))

  DesignChoiceTable$dependOn(options = c("factorResponse","showDesign", "responseSurfaceCenter","responseSurfaceBlocks",    "responseSurfaceCenterPointStar","responseSurfaceReplicationStar"))

  DesignChoiceTable$addCitation("Lenth, R.V. (2020). Response-Surface Methods in R, Using rsm.")

  DesignChoiceTable$addColumnInfo(name = "n.c",        title = gettext("N Fac Points"),                                type = "integer")
  DesignChoiceTable$addColumnInfo(name = "n0.c",       title = gettext("N Centre Points/Block"),                       type = "integer")
  DesignChoiceTable$addColumnInfo(name = "blks.c",     title = gettext("N Cube Blocks/Cube Portion"),                  type = "integer")
  DesignChoiceTable$addColumnInfo(name = "n.s",        title = gettext("N Design Star Points/Block"),                  type = "integer")
  DesignChoiceTable$addColumnInfo(name = "n0.s",       title = gettext("N Centre Points/Star Block"),                  type = "integer")
  DesignChoiceTable$addColumnInfo(name = "bbr.c",      title = gettext("N Copies of Each Cube Block"),                 type = "integer")
  DesignChoiceTable$addColumnInfo(name = "wbr.s",      title = gettext("N Replications of Each Star Point/Block"),     type = "integer")
  DesignChoiceTable$addColumnInfo(name = "bbr.s",      title = gettext("N Copies of Each Star Block"),                 type = "integer")
  DesignChoiceTable$addColumnInfo(name = "N",          title = gettext("Total Experiment Size"),                       type = "integer")
  DesignChoiceTable$addColumnInfo(name = "alpha.rot",  title = gettext("Rotational Alpha"),                            type = "number")
  DesignChoiceTable$addColumnInfo(name = "alpha.orth", title = gettext("Orthogonal Alpha"),                            type = "number")

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


  op1  <- length(options[["modelTerms"]])
  length_rsmVariables <- length(options[["rsmVariables"]])
  op2  <- length(options[["rsmResponseVariables"]])
  op3  <- length(options[["rsmBlocks"]])

  name <- vector()

  mean.col <- colMeans(data)

  optio <- matrix(unlist(options[["rsmVariables"]]),ncol=2,byrow=TRUE)[,2]
  data.list <- list()


  opt1 <- colnames(data)[1:length_rsmVariables]
  opt2 <- colnames(data)[(length_rsmVariables+1)]
  if (options[["rsmBlocks"]] != "") {
    opt3 <- colnames(data)[(length_rsmVariables+2)]
  }


  #For some reason, if I call .dataErrorCheck(data) right before var.code,
  #the analysis does not stop even if the
  #logical condition for stopping is met.
  #Also, the .dataErrorCheck(data) in the doeResponseSurfaceMethodology function
  #does not catch a subsequently added predictor which has more than 5 unique
  #values, so this error check needs to be repeated here.
  #Makes me wonder whether .dataErrorCheck is even needed, but I left it in
  #as I believe it looks nicer with it.

  .hasErrors(dataset = data,
             custom = function() {
               if(any(unique(data[,seq_along(options[["rsmVariables"]])]) > 5))
                 return(gettext("This analysis does not take predictor variables with more than 5 unique values."))
             },
             exitAnalysisIfErrors = T)
  var.code <- rsm::coded.data(data)

  vari <- matrix(unlist(options[["rsmVariables"]]),ncol = 2, byrow = T)[,2]
  len_mo <- length(options[["modelTerms"]])

  pq <- character()
  fo <- character()
  for (i in seq_along(options[["modelTerms"]])) {
    if (!any(options[["modelTerms"]][[i]][[2]] %in% vari == "FALSE")) {
      if (options[["modelTerms"]][[i]][[1]] == "fopq" & length(options[["modelTerms"]][[i]][[2]]) == 1) {
        pq[i] <- paste0("x", which(vari %in% options[["modelTerms"]][[i]][[2]]))
        fo[i] <- paste0("x", which(vari %in% options[["modelTerms"]][[i]][[2]]))

      }else if (options[["modelTerms"]][[i]][[1]] == "fo" & length(options[["modelTerms"]][[i]][[2]])) {
        fo[i] <- paste0("x", which(vari %in% options[["modelTerms"]][[i]][[2]]))

      }
    }
  }

  pq <- pq[!is.na(pq)]
  fo <- fo[!is.na(fo)]

  start <- ifelse(length(fo) == 0, 0, 1) + ifelse(length(pq) == 0, 0 ,1)
  formula_str <- rep("", times = start + len_mo - length_rsmVariables)
  if (length (fo) > 0){
    fo <- paste(fo, collapse = ",")
    formula_str[1] <- paste0("FO(",fo,")")
  }
  if (length (pq) > 0){
    pq <- paste(pq, collapse = ",")
    formula_str[2] <- paste0("PQ(", pq, ")")
  }




  l_gen <- seq_along(options[["rsmVariables"]])

  for (i in seq_along(options[["modelTerms"]])) {

    if (length(options[["modelTerms"]][[i]][[2]]) > 1) {

      jo <- character(length = length(options[["modelTerms"]][[i]][[2]]))

      for (j in seq_along(formula_str)){

        if (formula_str[j] == "") {


          optio_2 <- options[["modelTerms"]][[i]][[2]][[1]]

          jo[1] <- paste0("x",l_gen[vari %in% optio_2])

          for (k in seq_along(jo)) {

            if (jo[k] == "") {

              optio_3 <- options[["modelTerms"]][[i]][[2]][[k]]

              jo[k] <- paste0(",x", l_gen[vari %in% optio_3])

            }
          }
          jo_2 <- paste(jo, collapse = "")
          jo_3 <- paste0("TWI(",jo_2,")")
          formula_str[j] <- jo_3
          break()
        }


      }
    }
  }

  if (options[["rsmBlocks"]] != "") {
    if (length(unique(data[,(op1+2)])) > 1){
      form <- paste(formula_str, collapse = "+")

      form_2 <- paste0(opt2, "~", options[["rsmBlocks"]], "+", form)

      form_3 <- as.formula(form_2)
    }
  }else {
    form <- paste(formula_str, collapse = "+")
    form_2 <- paste0(opt2, "~", form)
    form_3 <- as.formula(form_2)

  }


  rsm <- rsm::rsm(form_3 , data = var.code)


}


.responseSurfaceContour <- function(jaspResults, options, data, i, position, dataset) {
  ready <- 1


  if (is.null(jaspResults[[paste0("contourPlot", i)]]))
    .responseSurfaceContourPlot(jaspResults, dataset, options, data, counter_in_main_for_loop = i)

}


.responseSurfaceCheckError <- function(dataset, options) {
  #Error 1: Does
}

.responseSurfaceContourPlot <- function(jaspResults, dataset, options, data, counter_in_main_for_loop) {
  contourPlot <- createJaspContainer(title = paste(options[["rsmResponseVariables"]][[counter_in_main_for_loop]],"Contour Plots", sep = " "))

  op1  <- length(options[["rsmVariables"]])
  op2  <- length(options[["rsmResponseVariables"]])
  op3  <- length(options[["rsmBlocks"]])



  op_pair <- length(options[["pairs"]])

  jaspResults[[paste0("contourPlot", counter_in_main_for_loop)]] <- contourPlot

  if (op_pair == 0 & options[["contour"]]) {
    jaspResults[[paste0("contourPlot", counter_in_main_for_loop)]][["1"]] <- createJaspPlot(width = 400, height = 400)

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
    names(point_vec) <- paste0("x", seq_along(options[["rsmVariables"]]))



    for (i in pair){

      if (!(i[1] == "") & !(i[2] == "")) {

        plot <- createJaspPlot(
          title = paste0(i[1], "-", i[2], " Plot"),
          width = 400, height = 400)
        plot$dependOn(c("rsmResponseVariables",
                        "rsmBlocks",
                        "contour", "psi", "theta","pairs", "Component", "Point"))
        jaspResults[[paste0("contourPlot", counter_in_main_for_loop)]][[paste0("plotObject", col)]] <- plot



        po <- as.formula(paste0("~x", l_gen[optio %in% i[1]],
                               "+", "x",
                               l_gen[optio %in% i[2]]))

        point_spec_r <- c()
        k <- 1
        for (j in 1:op1) {
          if (!(l_gen[optio %in% i[1]] == j || l_gen[optio %in% i[2]] == j)){
            point_spec_r[k] <- point_vec[j]
            names(point_spec_r)[k] <- paste0("x", j)
            k <- k + 1
          }
        }

        if (is.null(point_spec_r)) {
          for (j in 1:op1) {
            if (l_gen[optio %in% i[1]] == j || l_gen[optio %in% i[2]] == j){
              point_spec_r[k] <- point_vec[j]
              names(point_spec_r)[k] <- paste0("x", j)
              k <- k + 1
            }
          }
        }


        heli.rsm  <- .responseSurfaceCalculate(jaspResults, options, dataset, data)
        .responseSurfaceContourFill(jaspResults[[paste0("contourPlot", counter_in_main_for_loop)]][[paste0("plotObject", col)]],
                                    heli.rsm, po, options, point_spec_r, counter_in_main_for_loop)
        col <- col + 1
      }

    }

  }

  return()
}

.responsePlotResNorm <- function(jaspResults, options, rsm, i, position, dataset){
  if (!(is.null(jaspResults[[paste0("resNorm", i)]]))){
    return()
  }
  plot <- createJaspPlot(title = paste0("Normal Probability Plot of Residuals for ",
                                       options[["rsmResponseVariables"]][[i]]
                                       ),
                         width = 400, height = 400)
  jaspResults[[paste0("resNorm", i)]] <- plot

  plot$dependOn(c("resNorm", "rsmBlocks",
                  "rsmResponseVariables",
                  "rsmVariables","modelTerms"))
  p <- jaspGraphs::plotQQnorm(resid(rsm))

  plot$plotObject <- p

  return()
}



.responsePlotResidualCall <- function(jaspResults, options, rsm, i, position, dataset) {
  if (!(is.null(jaspResults[[paste0("Residual", i)]]))) {
    return()
  }
  plot <- createJaspPlot(
    title = paste0("Residual Plot for ",
                  options[["rsmResponseVariables"]][[i]]),
    width = 400, height = 400)
  plot$dependOn(c("rsmResponseVariables",
                  "rsmBlocks",
                  "res", "modelTerms"))


  jaspResults[[paste0("Residual", i)]] <- plot

  x <- resid(rsm)

  h <- hist(x, plot = FALSE)



  p <- ggplot2::ggplot(data.frame(x), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(binwidth = abs(h$breaks[1] - h$breaks[2])) +
    ggplot2::labs(y = "Count", x = "Residuals")


  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p
  return()



}

.responsePlotResFitted <- function(jaspResults, options, rsm, position, i, dataset){

  if (!(is.null(jaspResults[[paste0("ResFitted", i)]]))){
   return()

  }
  plot <- createJaspPlot(title = paste0("Residuals vs. Fitted Value for ", options[["rsmResponseVariables"]][[i]]),
                         width = 400, height = 400)
  jaspResults[[paste0("ResFitted", i)]] <- plot
  plot$dependOn(c("resFitted","rsmBlocks",
                "rsmResponseVariables",
                "rsmVariables","modelTerms"))

  df <- data.frame(x = fitted(rsm), y = resid(rsm))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df$x)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(df$y)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
    ggplot2::scale_x_continuous(name = gettextf("Fitted values"), limits = c(min(xBreaks), max(xBreaks)), breaks = xBreaks) +
    ggplot2::scale_y_continuous(name = gettextf("Residuals"),     limits = c(min(yBreaks), max(yBreaks)), breaks = yBreaks)

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return()
}

.responsePlotPareto <- function(jaspResults, options, rsm, i, position, dataset) {

  if (!(is.null(jaspResults[[paste0("pareto", i)]]))) {
    return()
  }

  plot <- createJaspPlot(title = paste0("Pareto Plot of Standardized Effects for ",
                                       options[["rsmResponseVariables"]][[i]]
                                       ),
                         width = 400, height = 400)
  jaspResults[[paste0("pareto", i)]] <- plot
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
    ggplot2::scale_x_continuous(name = gettextf("Standardized Effect"), limits = c(min(xBreaks), max(xBreaks)), breaks = xBreaks)

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p

  return()

}



.responseSurfaceContourFill <- function(contourPlot,heli.rsm,po, options, point_spec_r, counter_in_main_for_loop, dataset) {

  op1  <- length(options[["rsmVariables"]])
  op2  <- length(options[["rsmResponseVariables"]])
  op3  <- length(options[["rsmBlocks"]])

  opt2 <- options[["rsmResponseVariables"]][[counter_in_main_for_loop]]


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
        string[i] <- paste0(as.character(round(plot[[1]][[5]][[1]]+(i-1)*z_step,1)), "-",
                           as.character(round(plot[[1]][[5]][[1]]+i*z_step,1)))
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







.responseSurfaceTableCall <- function(jaspResults, options, rsm, i, dataset, position) {

  rsm <- summary(rsm)

  .responseSurfaceTable(jaspResults, options, rsm, i, dataset)

  return()
}

.responseSurfaceTable <- function(jaspResults, options, rsm, i, dataset) {

  if (is.null(jaspResults[[paste0("TableContainer", i)]])) {
    TableContainer <- createJaspContainer()
    jaspResults[[paste0("TableContainer", i)]] <- TableContainer
  } else {
    TableContainer <- jaspResults[[paste0("TableContainer", i)]]
  }

  if (is.null(jaspResults[[paste0("TableContainer", i)]][["coef"]])) {
    CoefTable <- createJaspTable(gettextf("RSM Coefficients for %s",
                                               options[["rsmResponseVariables"]][[i]]))
    jaspResults[[paste0("TableContainer", i)]][["coef"]] <- CoefTable
  } else {
    CoefTable  <- jaspResults[[paste0("TableContainer", i)]][["coef"]]
  }

  if (is.null(jaspResults[[paste0("TableContainer", i)]][["RSQTable"]])){
    RSQTable  <- createJaspTable()
    jaspResults[[paste0("TableContainer", i)]][["RSQTable"]] <- RSQTable
  }else {
    RSQTable <- jaspResults[[paste0("TableContainer", i)]][["RSQTable"]]
  }
  TableContainer$dependOn(     options = c("coef","modelTerms", "rsmBlocks",
                                      "rsmResponseVariables",
                                      "rsmVariables"))


  CoefTable$addColumnInfo(name = "names",title = gettext(" "))
  CoefTable$addColumnInfo(name = "est",  title = gettext("Estimate"))
  CoefTable$addColumnInfo(name = "std",  title = gettext("Standard Error"))
  CoefTable$addColumnInfo(name = "tval", title = gettext("t"))
  CoefTable$addColumnInfo(name = "pval", title = gettext("p"))

  RSQTable$addColumnInfo( name = "RSQ",   title = gettext("Multiple R-squared"))
  RSQTable$addColumnInfo( name = "ARSQ",  title = gettext("Adjusted R-squared"))
  RSQTable$addColumnInfo( name = "DF1",   title = gettext("DF1"))
  RSQTable$addColumnInfo( name = "DF2",   title = gettext("DF2"))
  RSQTable$addColumnInfo( name = "FStat", title = gettext("F"))
  RSQTable$addColumnInfo( name = "pval_2",title = gettext("p"))

  # jaspResults[[paste0("TableContainer", i)]][["coef"]] <- CoefTable
  # jaspResults[[paste0("TableContainer", i)]][["RSQTable"]] <- RSQTable




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

.responseSurfaceTableAnovaCall <- function(jaspResults, options, rsm, i,  position, dataset) {

  rsm <- summary(rsm)

  .responseSurfaceAnovaTable(jaspResults, options, rsm, i, dataset)

  return()
}


.responseSurfaceAnovaTable <- function(jaspResults, options, rsm, i, dataset) {

  if (is.null(jaspResults[[paste0("anova", i)]])) {
    AnovaTable <- createJaspTable(gettextf("ANOVA for %s",
                                           options[["rsmResponseVariables"]][[i]]))

    jaspResults[[paste0("anova", i)]] <- AnovaTable
  }else{
    AnovaTable <- jaspResults[[paste0("anova", i)]]
  }




  AnovaTable$dependOn(      options = c("anova","modelTerms", "rsmBlocks",
                                       "rsmResponseVariables",
                                       "rsmVariables"))
  AnovaTable$addColumnInfo( name = "names",     title = " ")
  AnovaTable$addColumnInfo( name = "Df",       title = "DF")
  AnovaTable$addColumnInfo( name = "Sum",      title = "Sum of Squares")
  AnovaTable$addColumnInfo( name = "Mean",     title = "Mean of Squares")
  AnovaTable$addColumnInfo( name = "FValue",   title = "F")
  AnovaTable$addColumnInfo( name = "PValue",   title = "p")



  .responseSurfaceAnovaFill(AnovaTable, jaspResults, options,rsm, i)

  return()
}



.responseSurfaceAnovaFill <- function(AnovaTable, jaspResults, options,rsm, i) {

 jaspResults[[paste0("anova", i)]]$setData(list(    names  = rownames(rsm[[13]]),
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

  if (is.null(eigen[["XTable"]])){
    XTable <- createJaspTable(title = gettextf("Stationary Points of Response Surface (Coded)"))
    eigen[["XTable"]] <- XTable
  }else {
    XTable <- eigen[["XTable"]]
  }

  if (is.null(eigen[["EigenValue"]])){
    EigenValue <- createJaspTable(title = gettextf("Eigenvalues"))
    eigen[["EigenValue"]] <- EigenValue
  }else {
    EigenValue <- eigen[["EigenValue"]]
  }

  if (is.null(eigen[["EigenVectors"]])){
    EigenVector <- createJaspTable(title = gettextf("Eigenvectors"))
    eigen[["EigenVectors"]] <- EigenVector
  }else{
    EigenVector <- eigen[["EigenVectors"]]
  }

  eigen$dependOn(      options = c("eigen","modelTerms", "rsmBlocks",
                                        "rsmResponseVariables",
                                        "rsmVariables"))


  op1  <- length(options[["rsmVariables"]])
  EigenVector$addColumnInfo(name = "names", title = "")
  for (i in 1:op1) {
    XTable$addColumnInfo(name = paste0("x",i), title = paste0("x",i))
    EigenValue$addColumnInfo(name = paste0("x",i), title = "")
    EigenVector$addColumnInfo(name = paste0("x",i), title = "")
  }

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
  if (length(options[["rsmTar"]]) > 0) {
    for (i in 1:length(options[["rsmTar"]])) {
      desire_list[[k]] <- desirability::dMin(options[["rsmTar"]][[i]][[2]],options[["rsmTar"]][[i]][[3]], options[["rsmTar"]][[i]][[1]])
      k <- k +1
    }
  }

  desire_final <- do.call(desirability::dOverall, desire_list)

  if (length(options[["rsmMax"]]) + length(options[["rsmMin"]]) + length(options[["rsmTar"]]) == 1)
    desire_final <- desire_final[[1]][[1]]

  op  <- data.frame(x1 = seq(-1,1,0.25))
  op1 <- length(options[["rsmVariables"]])

  if (op1 > 1) {
    for (i in 2:op1) {
      op <- cbind(op, seq(-1,1,0.25))
    }
    names(op) <- paste0("x",1:op1)
  }


  possibleCodedCombinationsOfPredictors <- expand.grid(op)

  rsmOpt_2 <- function(predictorCombinationFunction, rsm, dObject, space = "square") {
    conv <- numeric(length(rsm))
    for (j in seq_along(rsm)) {
      conv[j] <- predict(rsm[[j]], newdata = possibleCodedCombinationsOfPredictors[i,])
    }
    out <- do.call(predict, list(object = dObject, newobject = conv))
    #Value of 1.682 taken from https://rdrr.io/rforge/desirability/f/inst/doc/desirability.pdf , 5. Maximizing Desirability
    if (space == "circular") {
      if (sqrt(sum(possibleCodedCombinationsOfPredictors[i,]^2)) > 1.682)
        out <- 0
    }
    else if (space == "square")
      if (any(abs(possibleCodedCombinationsOfPredictors[i,]) > 1.682))
        out <- 0
    out
  }

  tmp <- vector("list", dim(possibleCodedCombinationsOfPredictors)[1])
  for (i in seq_len(dim(possibleCodedCombinationsOfPredictors)[1])) {
    tmp[[i]] <- optim(as.vector(possibleCodedCombinationsOfPredictors[i,]), rsmOpt_2, dObject = desire_final,
                      rsm = rsm, space = "square", control = list(fnscale = -1))
    if (i == 1) {
      bestCombinedDesirability <- tmp[[i]]
    }else {
      if (tmp[[i]][["value"]] > bestCombinedDesirability[["value"]])
        bestCombinedDesirability <- tmp[[i]]
    }
  }



  if (is.null(jaspResults[["desirability_container"]])) {
    desirability_container <- createJaspContainer()
    jaspResults[["desirability_container"]] <- desirability_container
    desirability_container$dependOn(options = c("rsmMin", "rsmMax", "rsmVariables", "rsmTar"))
  } else {
     desirability_container <- jaspResults[["desirability_container"]]
  }


  if (is.null(desirability_container[["predictor_value"]])) {
    predictor_value <- createJaspTable(title = "Predictor Values")
    desirability_container[["predictor_value"]] <- predictor_value
  }else {
    predictor_value <- desirability_container[["predictor_value"]]
  }

  if (is.null(desirability_container[["value"]])) {
    value <- createJaspTable(title = "Overall Desirability")
    value$addColumnInfo(name = "Value", title = "Value")
    desirability_container[["value"]] <- value
  }else {
    value <- desirability_container[["value"]]
  }

  for (i in 1:op1) {
    predictor_value$addColumnInfo(name = paste0("x",i), title = paste0("x",i))
  }

  .responseSurfaceOptimizeFill(predictor_value, value, desirability_container, bestCombinedDesirability, jaspResults, options, dataset)

  return()
}

.responseSurfaceOptimizeFill <- function(predictor_value, value, desirability_container, bestCombinedDesirability, jaspResults, options, dataset) {

  desirability_container[["predictor_value"]]$addRows(bestCombinedDesirability[["par"]])
  desirability_container[["value"]]$setData(round(bestCombinedDesirability[["value"]],3))

  return()
}
