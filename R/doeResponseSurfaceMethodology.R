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

#' @export
doeResponseSurfaceMethodology <- function(jaspResults, dataset, options, ...) {

  op1 <- length(options[["modelTerms"]])
  op2 <- length(options[["rsmResponseVariables"]])
  op3 <- length(options[["rsmBlocks"]])

  ready <- (op1 > 0 && op2 > 0) && any(options[["contour"]], options[["coef"]], options[["anova"]],
                                       options[["res"]], options[["pareto"]], options[["resNorm"]], options[["ResFitted"]],
                                       options[["buildDesignInv"]], options[["desirability"]],
                                       options[["contour"]])



  placeholder <- createJaspTable(title = gettext("Response Surface Methodology"))
  jaspResults[["placeholder"]] <- placeholder
  placeholder$dependOn(options = c("rsmVariables","rsmResponseVariables", "contour","coef","anova","res","pareto","resNorm",
                                   "resFitted","buildDesignInv","desirability","contour"))
  rsm <- list()
  #Placeholder table when the user inputs something. If an analysis gets picked, remove the table
  if (!ready) {

    if (any(options[["contour"]], options[["coef"]], options[["anova"]],
                                                                options[["res"]], options[["pareto"]], options[["resNorm"]], options[["ResFitted"]],
                                                                options[["desirability"]],
                                                                options[["contour"]])) {

      text <- gettext("No analyses (except those under 'Design Specification') can be started until at least one model term and one response variable are inputed.")
      placeholder$addFootnote(text, symbol = gettext("<em>Warning: </em>"))

    }
  }


  if (options[["designType"]] == "cube" && options[["buildDesignInv"]]) {
    jaspResults[["placeholder"]] <- NULL
    .cubeDesign(jaspResults, options)
  }
  if (options[["designType"]] == "star" && options[["buildDesignInv"]]) {
    jaspResults[["placeholder"]] <- NULL
    .starDesign(jaspResults, options)
  }

  if (ready) {

    for (i in 1:op2) {
      jaspResults[["placeholder"]] <- NULL
      data <- .readDataSet(jaspResults, options, dataset, i)

      #check for more than 5 unique
      .dataErrorCheck(data, options)

      rsm[[i]] <- .responseSurfaceCalculate(jaspResults, options, dataset, data)

      # if (options[["showDesign"]])
      #   .qualityControlDesignMainRSM(jaspResults,options, position = 1)

      if (options[["contour"]])
        .responseSurfaceContour(jaspResults, options, data, rsm[[i]], i, position = 2)


      if (options[["coef"]])
        .responseSurfaceTableCall(jaspResults, options, rsm[[i]], i, position = 3)

      if (options[["anova"]])
        .responseSurfaceTableAnovaCall(jaspResults, options, rsm = rsm[[i]], i, position = 4)

      # if(options[["eigen"]])
      #   .responseSurfaceTableEigenCall(jaspResults, options, rsm, position = 5)

      if (options[["res"]])
        .responsePlotResidualCall(jaspResults, options, rsm[[i]], i, position = 6)

      if (options[["normalPlot"]])
        .responseNomralProbabilityPlot(data, jaspResults, options, rsm[[i]], i, position = 7)

      if (options[["pareto"]])
        .responsePlotPareto(jaspResults, options, rsm[[i]], i, position = 8)

      if (options[["resNorm"]])
        .responsePlotResNorm(jaspResults, options, rsm[[i]], i, position = 9)

      if (options[["ResFitted"]])
        .responsePlotResFitted(jaspResults, options, rsm[[i]],i, position = 10)

      if (options[["fourInOne"]])
        .responseFourInOnePlot(jaspResults, options, rsm[[i]],i, position = 11)


    }
    if (options[["desirability"]])
      .responseSurfaceOptimize(jaspResults, options, rsm, data, position = 11, dataset)
  }

}


.cubeDesign <- function(jaspResults, options) {

  # TODO: rename "ccd" in jaspResults[["ccd"]] to "ccdTable"
  if (!is.null(jaspResults[["ccd"]]))
    return()

  ccdTable <- createJaspTable(title = gettext("Central Composite Design"))
  ccdTable$dependOn(options = c(
    "noModel", "designModel", "runOrder", "inscribed", "block", "designBlock",
    "numberOfCubes", "numberOfGenerators", "generators", "factors", "coded_out", "numberOfFactors",
    "buildDesignInv", "designType"
  ))

  ccdTable$addColumnInfo(name = "run.order", title = gettext("Run Order"),      type = "integer")
  ccdTable$addColumnInfo(name = "std.order", title = gettext("Standard Order"), type = "integer")

  factorNames <- doersmGetFactorNamesUncoded(options)
  for (i in seq_along(factorNames))
    ccdTable$addColumnInfo(name = paste0("x", i), title = factorNames[i], type = "number")

  jaspResults[["ccd"]] <- ccdTable

  ready <- TRUE

  # FIXME: what should happen here?
  if (FALSE && options[["numberOfGenerators"]] > 0) {
    for (i in seq_len(options[["numberOfGenerators"]])) {
      if (!(is.na(as.numeric(options[["generators"]][[i]][["generatorName"]])))) {

        text <- gettextf("Generator name '%s' is a number, and number generator names are not supported.", options[["generators"]][[i]][["generatorName"]])
        ccdTable$addFootnote(text, symbol = gettext("<em>Warning: </em>"))
        ready <- FALSE
        next
      }

      ccdTable$addColumnInfo(name = options[["generators"]][[i]][["generatorName"]], title = gettext(options[["generators"]][[i]][["generatorName"]]),
                              type = "number")
    }
  }


  modelError <- !(options[["noModel"]]) && options[["designModel"]] == ""
  blockError <- options[["block"]] && options[["designBlock"]] == ""

  if (modelError || blockError) {

    ready <- FALSE

    if (modelError) {
      text <- gettext("The analysis will not run when the 'Specify Model for CCD' field is empty and the 'Use # of Variables instead of Model' is not ticked.")
      ccdTable$addFootnote(text, symbol = gettext("<em>Warning: </em>"))
    }

    if (blockError) {
      text <- gettext("The analysis will not run when the 'Specify Blocks for CCD' field is empty and the Introduce Blocking is ticked.")
      ccdTable$addFootnote(text, symbol = gettext("<em>Warning: </em>"))
    }

  }

  if (ready) {

    ccd <- doersmGenerateDesign(options)

    # convert between coded and uncoded form
    tableData <- if (options[["coded_out"]])
      rsm::code2val(ccd, rsm::codings(ccd))
    else
      rsm::val2code(ccd, rsm::codings(ccd))

    # assumes that that the order of colnames(tableData)[-(1:2)] matches that of options[["factors"]]
    colnames(tableData)[-(1:2)] <- factorNames

    ccdTable$setData(tableData)

  }
}

.starDesign <- function(jaspResults, options) {

  if (!is.null(jaspResults[["star"]]))
    return()

  # TODO: merge this function with .cubeDesign

  ccdTable <- createJaspTable(title = gettext("Central Composite Design with Star Points"))
  jaspResults[["star"]] <- ccdTable

  ccdTable$dependOn(options = c("runOrder", "numberOfStars","alpha",
                                  "numberOfCubes","numberOfGenerators","generators",
                                  "factors","numberOfFactors", "buildDesignInv", "coded_out",
                                  "designType"))

  ccdTable$addColumnInfo(name = "run.order", title = gettext("Run Order"),      type = "integer")
  ccdTable$addColumnInfo(name = "std.order", title = gettext("Standard Order"), type = "integer")

  factorNames <- doersmGetFactorNamesUncoded(options)
  for (i in seq_along(factorNames))
    ccdTable$addColumnInfo(name = paste0("x", i), title = factorNames[i], type = "number")


  # FIXME: what to do here?
  generatorError <- FALSE && options[["numberOfGenerators"]] > 0
  ready <- TRUE
  if (generatorError) {
    ready <- FALSE
    text  <- gettext("The analysis will not run when the 'Number of Generators' > 0")
    ccdTable$addFootnote(text, symbol = gettext("<em>Warning: </em>"))
  }

  if (ready) {

    alpha <- tolower(options[["alpha"]])
    ccd   <- doersmGenerateDesign(options)

    n0    <- options[["numberOfStars"]]

    star.ccd <- rsm::star(basis = ccd, n0 = n0, alpha = alpha)
    # convert between coded and uncoded form
    tableData <- if (options[["coded_out"]])
      rsm::code2val(ccd, rsm::codings(ccd))
    else
      rsm::val2code(ccd, rsm::codings(ccd))

    # assumes that that the order of colnames(tableData)[-(1:2)] matches that of options[["factors"]]
    colnames(tableData)[-(1:2)] <- factorNames

    ccdTable$setData(tableData)

  }
}


doersmGenerateDesign <- function(options) {

  if (options[["noModel"]])
    formula <- as.integer(options[["numberOfFactors"]])
  else {
    clean_designModel <- stringr::str_replace_all(options[["designModel"]], c(",", " "), "")
    formula <- as.formula(paste0("~",  clean_designModel))
  }

  inscribed <- options[["inscribed"]]
  oneblock  <- options[["oneBlock"]]

  # TODO: when is this not empty?
  block_formula <- vector()
  if (options[["block"]] && length(options[["designBlock"]]) > 0) {
    clean_Blocks  <- stringr::str_replace_all(options[["designBlock"]], " ", "")
    vector_blocks <- stringr::str_split(clean_Blocks, ",")
    block_formula <- as.formula(paste0("~", clean_Blocks))
  }

  n0 <- options[["numberOfCubes"]]
  generators <- vector()
  # FIXME: what should happen here?
  if (FALSE && options[["numberOfGenerators"]] > 0) {
    for (i in seq_along(options[["generators"]])) {
      if (!(options[["generators"]][[i]][["generatorName"]] == "" ||
            options[["generators"]][[i]][["generatorFormula"]] == "")) {
        if (!(is.na(as.numeric(options[["generators"]][[i]][["generatorName"]]))))
          next

        generators <- c(generators, as.formula(paste0(options[["generators"]][[i]][["generatorName"]],
                                                      "~", options[["generators"]][[i]][["generatorFormula"]])))
      }
    }
  }

  # TODO: unclear whether this codingList is a good idea.
  # it should also handle categorical levels and I don't think this supports that
  # alternatively, just create the design in coded form and then recode?
  # compare results to https://support.minitab.com/en-us/minitab/21/help-and-how-to/statistical-modeling/doe/supporting-topics/response-surface-designs/summary-of-central-composite-designs/
  codingList <- vector("list", length = length(options[["factors"]]))
  # rsm::cube does not allow e.g., x1 ~ (x1 - 0) / 1 so we add 1 at the end to get e.g., x1 ~ (x11 - 0) / 1
  codedFactorNames <- paste0(doersmGetFactorNamesCoded(options), "1")
  for (i in seq_along(options[["factors"]])) {

    x <- options[["factors"]][[i]]

    if (is.numeric(x[["low"]])) {

      values    <- sort(c(x[["low"]], x[["high"]]))
      center    <- mean(values)
      distance  <- (values[2L] - values[1L]) / 2
      newCoding <- as.formula(paste0("x", i, " ~ (", codedFactorNames[i], " - ", center, ") / ", distance))

    } else {
      # TODO:what
      values <- c(x[["low"]], x[["high"]])
      newCoding <- as.formula(paste0("x", i, " ~ (", codedFactorNames[i], " - ", center, ") / ", distance))

    }
    codingList[[i]] <- newCoding

  }

  ccd <- rsm::cube(basis = formula, n0 = n0, coding = codingList, randomize = TRUE,
            inscribed = inscribed)
  rsm::val2code(ccd, rsm::codings(ccd))

  # TODO: reduce the long if chain to nested ifs to avoid repetition, or use do.call
  # if (length(codingList) == 0L) {
  #
  #   if (length(generators) == 0 && length(block_formula) == 0) {
  #
  #   } else if ()
  #
  # } else if (length(generators) == 0L) {
  #
  # }

  if (length(codingList) == 0 && length(generators) == 0 && length(block_formula) == 0) {
    ccd <- rsm::cube(basis = formula, n0 = n0, randomize = TRUE,
                     inscribed = inscribed)


  } else if (length(codingList) == 0 && length(generators) == 0) {
    ccd <- rsm::cube(basis = formula, n0 = n0, blockgen = block_formula, randomize = TRUE,
                     inscribed = inscribed)


  } else if (length(codingList) == 0 && length(block_formula) == 0) {
    ccd <- rsm::cube(basis = formula, n0 = n0, generators = generators, randomize = TRUE,
                     inscribed = inscribed,
                     oneblock = oneblock)


  } else if (length(generators) == 0 && length(block_formula) == 0) {
    ccd <- rsm::cube(basis = formula, n0 = n0, coding = codingList, randomize = TRUE,
                     inscribed = inscribed)

  } else if (length(codingList) == 0) {
    ccd <- rsm::cube(basis = formula, n0 = n0, generators = generators, randomize = TRUE,
                     blockgen = block_formula, inscribed = inscribed)


  } else if (length(generators) == 0) {
    ccd <- rsm::cube(basis = formula, n0 = n0,
                     blockgen = block_formula, coding = codingList, randomize = TRUE,
                     inscribed = inscribed)


  } else if (length(block_formula) == 0) {
    ccd <- rsm::cube(basis = formula, n0 = n0,
                     generators = generators, coding = codingList, randomize = TRUE,
                     inscribed = inscribed)


  } else {
    ccd <- rsm::cube(basis = formula, n0 = n0,
                     blockgen = block_formula, generators = generators, randomize = TRUE,
                     coding = codingList, inscribed = inscribed)

  }

  o <- order(if (options[["runOrder"]] == "runOrderStandard") ccd[["std.order"]] else ccd[["run.order"]])
  ccd <- ccd[o, ]

  return(ccd)
}

.dataErrorCheck <- function(data, options) {

  .hasErrors(dataset = data, type = c("infinity","missingValues", "observations"),
             infinity.target = c(options[["rsmVariables"]],options[["rsmResponseVariables"]],
                            options[["rsmBlocks"]]),
             missingValues.target = c(options[["rsmVariables"]],options[["rsmResponseVariables"]],
                                      options[["rsmBlocks"]]),
             observations.target = c(options[["rsmVariables"]],options[["rsmResponseVariables"]]),
             observations.amount = c("< 2"),
             custom = function() {
               if (length(options[["rsmVariables"]]) == 1) {
                 if (length(unique(data[,1])) > 5)
                   return(gettext("This analysis does not take predictor variables with more than 5 unique values."))
               }else {
                 if(any(rapply(data[,seq_along(options[["rsmVariables"]])], function(x)length(unique(x)) > 5)))
                   return(gettext("This analysis does not take predictor variables with more than 5 unique values."))
               }
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
  blocks <- unlist(options$rsmBlocks)
  if (blocks != "")
    dat.blocks = data[,blocks] ;data <- data[, colnames(data) != blocks]

  name <- vector()
  mean.col <- colMeans(data)
  optio <- matrix(unlist(options[["rsmVariables"]]),ncol=2,byrow=TRUE)[,2]
  data.list <- list()

  opt1 <- colnames(data)[1:length_rsmVariables]

  opt2 <- colnames(data)[(length_rsmVariables+1)]

  if (opt1[1] != "x1")
    var.code <- rsm::coded.data(data)
  else
    var.code <- data

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
  start_le <- (length(fo) != 0) + (length(pq) != 0)
  if (len_mo > length_rsmVariables){
    formula_str <- character(start_le - length_rsmVariables + len_mo)
  }else{
    formula_str <- character(start_le)
  }

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
    if (length(unique(dat.blocks)) > 1){
      form <- paste(formula_str, collapse = "+")
      form_2 <- paste0(opt2, "~", options[["rsmBlocks"]], "+", form)
      form_3 <- as.formula(form_2)
      data.blcoks <- as.data.frame(dat.blocks); names(data.blcoks)[1] <- blocks
      var.code <- cbind(var.code, data.blcoks)
    }
  }else {
    form <- paste(formula_str, collapse = "+")
    form_2 <- paste0(opt2, "~", form)
    form_3 <- as.formula(form_2)

  }

  rsm <- rsm::rsm(form_3 , data = var.code)
}


.responseSurfaceContour <- function(jaspResults, options, data, rsm, i, position, dataset) {

  ready <- 1
  .responseSurfaceContourPlot(jaspResults, dataset, options, data, rsm, counter_in_main_for_loop = i)

}


.responseSurfaceCheckError <- function(dataset, options) {
  #Error 1: Does
}


.responseSurfaceContourPlot <- function(jaspResults, dataset, options, data, rsm, counter_in_main_for_loop) {

  op1  <- length(options[["rsmVariables"]])
  op2  <- length(options[["rsmResponseVariables"]])
  op3  <- length(options[["rsmBlocks"]])

  if (is.null(jaspResults[[paste0("contourPlot", counter_in_main_for_loop)]])) {
    contourPlot <- createJaspContainer(title = paste(options[["rsmResponseVariables"]][[counter_in_main_for_loop]],"Contour Plots"))
    jaspResults[[paste0("contourPlot", counter_in_main_for_loop)]] <- contourPlot
    contourPlot$dependOn(c("rsmResponseVariables",
               "rsmBlocks",
               "contour", "psi", "theta","pairs", "Component", "Point"))
  }else {
    contourPlot <- jaspResults[[paste0("contourPlot", counter_in_main_for_loop)]]
  }

  op_pair <- length(options[["pairs"]])




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
        if (is.null(jaspResults[[paste0("contourPlot", counter_in_main_for_loop)]][[paste0("plotObject", col)]])) {
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



          heli.rsm  <- rsm

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
            plot$plotObject <- plot.contour
          }else {
            plot$plotObject <- contour.fill
          }

          col <- col + 1
        }

      }

    }

  }


  return()
}

.responseFourInOnePlot <- function (jaspResults, options, rsm, i, position) {
  if (!(is.null(jaspResults[[paste0("fourInOne", i)]]))){

    return()
  }

  fourInOne <- createJaspContainer(gettextf("Matrix plot for %s",
                                            options[["rsmResponseVariables"]][[i]]))

  jaspResults[[paste0("fourInOne", i)]] <- fourInOne
  fourInOne$dependOn(c("resNorm", "rsmBlocks",
                       "rsmResponseVariables",
                       "rsmVariables","modelTerms", "fourInOne"))

  matrixPlot <- createJaspPlot(width = 1100, height = 800)
  plotMat <- matrix(list(), 1, 3)

  plotMat[[1, 1]] <- .responsePlotResidualCall(jaspResults, options, rsm, i, position = 1, ggPlot = TRUE)
  plotMat[[1, 2]] <-.responsePlotResNorm(jaspResults, options, rsm, i, position = 2, ggPlot = TRUE)
  plotMat[[1, 3]] <-.responsePlotResFitted(jaspResults, options, rsm, i, position = 3, ggPlot = TRUE)


  matrixPlot$plotObject <- jaspGraphs::ggMatrixPlot(plotMat)
  fourInOne[["plot"]] <- matrixPlot

  return()
}

.responsePlotResNorm <- function(jaspResults, options, rsm, i, position, dataset, ggPlot = FALSE){

  if (!(is.null(jaspResults[[paste0("resNorm", i)]]))){

    return()
  }
  plot <- createJaspPlot(title = paste0("Normal Probability Plot of Residuals for ",
                                       options[["rsmResponseVariables"]][[i]]
                                       ),
                         width = 400, height = 400)


  plot$dependOn(c("resNorm", "rsmBlocks",
                  "rsmResponseVariables",
                  "rsmVariables","modelTerms"))
  p <- jaspGraphs::plotQQnorm(resid(rsm))

  plot$plotObject <- p

  if (!ggPlot)
    jaspResults[[paste0("resNorm", i)]] <- plot
  else
    return(p)
}



.responsePlotResidualCall <- function(jaspResults, options, rsm, i, position, dataset, ggPlot = FALSE) {

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

  x <- resid(rsm)
  h <- hist(x, plot = FALSE)

  p <- ggplot2::ggplot(data.frame(x), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(binwidth = abs(h$breaks[1] - h$breaks[2])) +
    ggplot2::labs(y = "Count", x = "Residuals")


  p <- jaspGraphs::themeJasp(p)
  plot$plotObject <- p

  if (!ggPlot)
    jaspResults[[paste0("Residual", i)]] <- plot
  else
    return(p)
}

.responsePlotResFitted <- function(jaspResults, options, rsm, position, i, dataset, ggPlot = FALSE){


  if (!(is.null(jaspResults[[paste0("ResFitted", i)]]))){
   return()

  }
  plot <- createJaspPlot(title = paste0("Residuals vs. Fitted Value for ", options[["rsmResponseVariables"]][[i]]),
                         width = 400, height = 400)
  plot$dependOn(c("ResFitted","rsmBlocks",
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

  if (!ggPlot)
    jaspResults[[paste0("ResFitted", i)]] <- plot
  else
    return(p)
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

  plot$plotObject <- .factorialPareto(jaspResults, options, rsm, onlyPlot = TRUE)

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

    TableContainer$dependOn(options = c("coef","modelTerms", "rsmBlocks",
                                             "rsmResponseVariables",
                                             "rsmVariables"))
  } else {
    TableContainer <- jaspResults[[paste0("TableContainer", i)]]
  }

  if (is.null(TableContainer[["coef"]])) {

    # Index table
    indexTable <- createJaspTable(gettextf("Index coefficients for %s",
                                          options[["rsmResponseVariables"]][[i]]))
    TableContainer[["indexTable"]] <- indexTable
    indexTable$addColumnInfo(name = "coded", title = gettext("Coded coefficients"), type = "string")
    indexTable$addColumnInfo(name = "names",  title = gettext("Uncoded coefficients"), type ="string")

    n.factors <- length(unlist(options$rsmVariables))/2 # Exclude P points
    names <- sapply(1:n.factors, function(x) {unlist(options$rsmVariables[[x]][[2]])}) # index names
    coded <- names(rsm[[4]][,1])[-1][1:n.factors] # Exclude the intercept and interaction terms

    indexTable$setData(list(
      coded = coded,
      names = names
    ))

    # coefficients table
    CoefTable <- createJaspTable(gettextf("Index coefficients for %s",
                                          options[["rsmResponseVariables"]][[i]]))
    TableContainer[["coef"]] <- CoefTable
    CoefTable$addColumnInfo(name = "names", type = "string")
    CoefTable$addColumnInfo(name = "est",  title = gettext("Coefficient"), "number")
    CoefTable$addColumnInfo(name = "std",  title = gettext("Standard Error"), "number")
    CoefTable$addColumnInfo(name = "tval", title = gettext("t"), "number")
    CoefTable$addColumnInfo(name = "pval", title = gettext("<i>p</i>-value"), "pvalue")

    CoefTable$setData(list(names = rownames(rsm[[4]]),
                                          est  = round(rsm[[4]][,1],3),
                                          std  = round(rsm[[4]][,2],3),
                                          tval = round(rsm[[4]][,3],3),
                                          pval = ifelse(rsm[[4]][,4] > 0.001,round(rsm[[4]][,4],3), "< .001")))

    # Formula table
    rsmRegressionFormula <- createJaspTable(gettext("Regression equation in coded coefficients"))
    TableContainer[["formula"]] <- rsmRegressionFormula

    factors <- names(rsm[[4]][,1])
    coefs <- as.vector(rsm[[4]][,1])
    plusOrMin <- sapply(1:length(coefs), function(x) {if (coefs[x] > 0) "+" else "-"})

    formula <- sprintf("y = %.5g%s %s %.5g%s", coefs[1], factors[1],plusOrMin[2], abs(coefs[2]), factors[2])
    for (i in 3:length(coefs))
      formula <- sprintf("%s %s %.5g%s", formula, plusOrMin[i], abs(coefs[i]), factors[i])

    rsmRegressionFormula$setData(list(Formula = formula))

    # RSM model summary
    RSQTable  <- createJaspTable(title = "Model summary")
    TableContainer[["RSQTable"]] <- RSQTable

    RSQTable$addColumnInfo( name = "S",   title = gettext("S"))
    RSQTable$addColumnInfo( name = "RSQ",   title = gettext("Model R-squared"))
    RSQTable$addColumnInfo( name = "ARSQ",  title = gettext("Adjusted R-squared"))
    RSQTable$addColumnInfo( name = "DF1",   title = gettext("df1"))
    RSQTable$addColumnInfo( name = "DF2",   title = gettext("df2"))
    RSQTable$addColumnInfo( name = "FStat", title = gettext("F"))
    RSQTable$addColumnInfo( name = "pval_2",title = gettext("<i>p</i>-value"))


    RSQTable$setData(list(RSQ    = round(rsm[[8]],3),
                          S   = round(rsm$sigma,3),
                          ARSQ   = round(rsm[[9]],3),
                          DF1    = rsm[[10]][[2]],
                          DF2    = rsm[[10]][[3]],
                          FStat  = round(rsm[[10]][[1]],3),
                          pval_2 = ifelse((1 - pf(rsm[[10]][[1]], rsm[[10]][[2]], rsm[[10]][[3]])) > 0.001,
                                          round(1 - pf(rsm[[10]][[1]], rsm[[10]][[2]], rsm[[10]][[3]]),3),
                                          "<.001")))

  }

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

    AnovaTable$dependOn(c("anova","modelTerms", "rsmBlocks",
                                          "rsmResponseVariables",
                                          "rsmVariables"))

    AnovaTable$addColumnInfo( name = "names", "string")
    AnovaTable$addColumnInfo( name = "Df",       title = gettext("df"), "number")
    AnovaTable$addColumnInfo( name = "Sum",      title = gettext("SS"), "number")
    AnovaTable$addColumnInfo( name = "Mean",     title = gettext("MS"), "number")
    AnovaTable$addColumnInfo( name = "FValue",   title = gettext("F"), "number")
    AnovaTable$addColumnInfo( name = "PValue",   title = gettext("<i>p</i>-value"), "pvalue")

    .responseSurfaceAnovaFill(AnovaTable, jaspResults, options,rsm, i)
  }else{
    AnovaTable <- jaspResults[[paste0("anova", i)]]
  }

  return()
}




.responseSurfaceAnovaFill <- function(AnovaTable, jaspResults, options,rsm, i) {

  anova <- rsm[[13]]
  names <- c("Model", rownames(anova), "Total"); names[names == "Residuals"] <- "Error"

  errorIndex <- which(rownames(anova) == "Residuals")
  modelIndex <- errorIndex - 1
  model.df <- sum(anova$`Df`[1:modelIndex])
  model.SS <- sum(anova$`Sum Sq`[1:modelIndex])
  model.MS <- model.SS / model.df
  model.F <- model.MS / anova$`Mean Sq`[errorIndex]
  model.Pval <- pf(model.F, model.df, anova$Df[errorIndex], lower.tail = F)

  tota.df <- sum(anova$Df[errorIndex], model.df)
  tota.SS <- sum(anova$`Sum Sq`[errorIndex], model.SS)

  DF <- c(model.df, anova$`Df`, tota.df)
  SS <- round(c(model.SS, anova[[2]], tota.SS),3);       SS[SS == "NaN"] <- NA
  MS <- round(c(model.MS,anova[[3]]),3);        MS[MS == "NaN"] <- NA
  FValue <- round(c(model.F, anova[[4]]),3);    FValue[FValue == "NaN"] <- NA
  PValue <- round(c(model.Pval,anova[[5]]),3);  PValue[PValue == "NaN"] <- NA

 jaspResults[[paste0("anova", i)]]$setData(list(
  names  = names,
  Df     = DF,
  Sum    = SS,
  Mean   = MS,
  FValue = FValue,
  PValue = PValue))

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
    return()
  }

  if (is.null(eigen[["EigenValue"]])){
    EigenValue <- createJaspTable(title = gettextf("Eigenvalues"))
    eigen[["EigenValue"]] <- EigenValue
  }else {
    return()
  }


  if (is.null(eigen[["EigenVectors"]])){
    EigenVector <- createJaspTable(title = gettextf("Eigenvectors"))
    eigen[["EigenVectors"]] <- EigenVector
  }else{
    return()
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
  min_len <- length(options[["rsmMin"]])
  max_len <- length(options[["rsmMax"]])
  tar_len <- length(options[["rsmTar"]])

  if (length(rsm) == 0 || min_len + max_len + tar_len == 0) {
    error_table <- createJaspTable(gettext("Desirability"))
    jaspResults[["errorTable"]] <- error_table
    error_table$dependOn(options = c("desirability", "rsmVariables","rsmResponseVariables"))

    if (length(rsm) == 0){
      text  <- gettext("At least one predictor and response variable need to be specified before running the analysis.")
      error_table$addFootnote(text, symbol = gettext("<em>Warning: </em>"))
    }

    if (min_len + max_len + tar_len == 0) {
      text  <- gettext("At least one desirability function must be specified before running the analysis.")
      error_table$addFootnote(text, symbol = gettext("<em>Warning: </em>"))
    }


  } else {
    jaspResults[["errorTable"]] <- NULL

    desire_list <- list()
    k <- 1
    if (min_len > 0) {
      for (i in 1:min_len) {
        desire_list[[k]] <- desirability::dMin(options[["rsmMin"]][[i]][[2]], options[["rsmMin"]][[i]][[1]])
        k <- k +1
      }
    }
    if (max_len > 0) {
      for (i in 1:max_len) {
        desire_list[[k]] <- desirability::dMin(options[["rsmMax"]][[i]][[2]], options[["rsmMax"]][[i]][[1]])
        k <- k +1
      }

    }
    if (tar_len > 0) {
      for (i in 1:tar_len) {
        desire_list[[k]] <- desirability::dMin(options[["rsmTar"]][[i]][[2]],options[["rsmTar"]][[i]][[3]], options[["rsmTar"]][[i]][[1]])
        k <- k +1
      }
    }

    desire_final <- do.call(desirability::dOverall, desire_list)

    if (max_len + min_len + tar_len == 1)
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
      desirability_container$dependOn(options = c("rsmMin", "rsmMax", "rsmVariables", "rsmTar", "desirability"))
    }else {
      desirability_container <- jaspResults[["desirability_container"]]
    }


    if (is.null(desirability_container[["predictor_value"]])) {
      predictor_value <- createJaspTable(title = "Predictor Values")
      desirability_container[["predictor_value"]] <- predictor_value
      desirability_container[["predictor_value"]]$setData(bestCombinedDesirability[["par"]])

    }

    if (is.null(desirability_container[["value"]])) {
      value <- createJaspTable(title = "Overall Desirability")
      value$addColumnInfo(name = "Value", title = "Value")
      desirability_container[["value"]] <- value
      desirability_container[["value"]]$setData(round(bestCombinedDesirability[["value"]],3))
    }
  }

  return()
}

.responseNomralProbabilityPlot <- function(data, jaspResults, options, rsm, i, position){


  if (!(is.null(jaspResults[[paste0("NomralProbabilityPlot", i)]]))) {
    return()
  }

  Container <- createJaspContainer(title = paste0("Normal Plot of Standardized Effects for ",
                                        options[["rsmResponseVariables"]][[i]]))
  jaspResults[[paste0("NomralProbabilityPlot", i)]] <- Container
  Container$dependOn(c("pareto","rsmBlocks",
                  "rsmResponseVariables",
                  "rsmVariables", "modelTerms", "normalPlot", "addGridlines"))

  Container[["plot"]] <- .qcProbabilityPlot(dataset = data, options = options, fit = rsm)

  return()
}

# helpers ----
doersmGetFactorNamesCoded <- function(options) {
  return(paste0("x", seq_len(options[["numberOfFactors"]])))
}

doersmGetFactorNamesUncoded <- function(options) {
  factorNames <- purrr::map_chr(options[["factors"]], `[[`, "factorName")
  badNamesIdx <- which(factorNames == "")
  factorNames[badNamesIdx] <- sprintf("x%d", badNamesIdx)
  return(factorNames)
}
