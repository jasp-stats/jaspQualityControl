
# imports from packages
#' @importFrom jaspBase createJaspContainer createJaspPlot createJaspState createJaspTable createJaspHtml
#' .extractErrorMessage .hasErrors isTryError .readDataSetToEnd


#############################################################
## Common functions for plots ###############################
#############################################################
NelsonLaws <- function(data, allsix = FALSE, chart = "i", xLabels = NULL) {

  # Adjust Rules to SKF
  pars <- Rspc::SetParameters()
  pars$Rule2$nPoints = 7
  pars$Rule3$nPoints = 7
  pars$Rule3$convention = "minitab"
  pars$Rule4$convention = "minitab"

  #Evaluate all rules
  if (chart == "p") {
    n = length(data$statistics)
    warnings <- data.frame(x = rep(1,n), Rule1 = rep(1,n), Rule2 = rep(1,n), Rule3 = rep(1,n))
    for( i in 1:length(data$statistics)) {
      warningsRaw <- Rspc::EvaluateRules(x = c(data$statistics[i],0), type = "c", lcl = data$limits[i,1], ucl = data$limits[i,2], cl = data$center, parRules = pars,
                                         whichRules = c(1:3,5,7:8))[1,]
      warnings[i,] <- warningsRaw
    }
  } else {
      lcl <- ifelse(is.nan(data$limits[1,1]) || is.na(data$limits[1,1]), NA, data$limits[1,1])
      ucl <- ifelse(is.nan(data$limits[1,2])  || is.na(data$limits[1,2]), NA, data$limits[1,2])
      warnings <- Rspc::EvaluateRules(x = data$statistics, type = chart, lcl = lcl, ucl = ucl, cl = data$center[1], parRules = pars,
                        whichRules = c(1:3,5,7:8))
  }

  if (allsix) {
    if (length(xLabels) == 0) {
      Rules <- list(R1 = which(warnings[,2] == 1),
                    R2 = which(warnings[,3] == 1),
                    R3 = which(warnings[,4] == 1),
                    R4 = which(warnings[,5] == 1),
                    R5 = which(warnings[,6] == 1),
                    R6 = which(warnings[,7] == 1))
    }
    else {
      Rules <- list(R1 = xLabels[which(warnings[,2] == 1)],
                    R2 = xLabels[which(warnings[,3] == 1)],
                    R3 = xLabels[which(warnings[,4] == 1)],
                    R4 = xLabels[which(warnings[,5] == 1)],
                    R5 = xLabels[which(warnings[,6] == 1)],
                    R6 = xLabels[which(warnings[,7] == 1)])
    }
    red_points = apply(warnings[,-1], 1, sum) > 0
  }
  else {
    if (length(xLabels) == 0) {
      Rules <- list(R1 = which(warnings[,2] == 1),
                    R2 = which(warnings[,3] == 1),
                    R3 = which(warnings[,4] == 1))
    }
    else {
      Rules <- list(R1 = xLabels[which(warnings[,2] == 1)],
                    R2 = xLabels[which(warnings[,3] == 1)],
                    R3 = xLabels[which(warnings[,4] == 1)])
    }
    red_points = apply(warnings[,c(2,3,4)], 1, sum) > 0
  }

  return(list(red_points = red_points, Rules = Rules))
}

.sdXbar <- function(df, type = c("s", "r"), unbiasingConstantUsed = TRUE) {
  type <- match.arg(type)

  # exclude groups with single observation from calculation
  rowRemovalIndex <- which(apply(df, 1, function(x) sum(!is.na(x)) < 2)) # get index of rows with less than 2 obs.
   if (length(rowRemovalIndex) > 0)
    df <- df[-rowRemovalIndex, ]

  if (type == "r") {
    rowRanges <- .rowRanges(df, na.rm = TRUE)$ranges
    n <- .rowRanges(df)$n
    if (sum(n) < 2) {
      sdWithin <- 0
    } else {
      d2s <- sapply(n, function(x) return(KnownControlStats.RS(x, 0)$constants[1]))
      sdWithin <- sum((n - 1) * rowRanges / d2s) / sum(n - 1) # Burr (1969), equation 11
    }
  } else if (type == "s") {
    rowSd <- apply(df, 1, sd, na.rm = TRUE)
    if (sum(!is.na(rowSd)) == 0) {
      sdWithin <- NaN
    } else if (!unbiasingConstantUsed) {
      sdWithin <- mean(rowSd, na.rm = TRUE)
    } else if (unbiasingConstantUsed) {
      n <- apply(df, 1, function(x) return(sum(!is.na(x))))
      c4s <- sapply(n, function(x) return(KnownControlStats.RS(x, 0)$constants[3]))
      hs <- c4s^2/(1-c4s^2)
      sdWithin <- sum((hs*rowSd)/c4s)/sum(hs)
    }
  }
  return(sdWithin)
}

.rowRanges <- function(df, na.rm = FALSE) {
  nrow <- nrow(df)
  ranges <- c()
  n <- c()
  for (i in seq_len(nrow)) {
    rowVector <- df[i,]
    ranges <- c(ranges, max(rowVector, na.rm = na.rm) - min(rowVector, na.rm = na.rm))
    n <- c(n, sum(!is.na(rowVector)))
  }
  return(list(ranges = ranges, n = n))
}

KnownControlStats.RS <- function(N, sigma = 3) {

  Data.d3 <- data.frame(
    n = 0:25,
    d3 = c(NA, NA, 0.8525 ,0.8884, 0.8798, 0.8641, 0.8480, 0.8332, 0.8198, 0.8078, 0.7971, 0.7873, 0.7785, 0.7704, 0.7630,
           0.7562, 0.7499, 0.7441, 0.7386, 0.7335, 0.7287, 0.7242, 0.7199, 0.7159, 0.7121, 0.7084))

  Data.d2 <- data.frame(
    n = 0:50,
    d2 = c(NA, NA, 1.128, 1.693 ,2.059, 2.326, 2.534, 2.704, 2.847, 2.970, 3.078, 3.173, 3.258, 3.336, 3.407, 3.472, 3.532,
            3.588 ,3.640 ,3.689, 3.735, 3.778, 3.819, 3.858, 3.895, 3.931, 3.964, 3.997, 4.027, 4.057, 4.086, 4.113,
            4.139 ,4.165 ,4.189, 4.213, 4.236, 4.259, 4.280, 4.301, 4.322, 4.341, 4.361, 4.379, 4.398, 4.415, 4.433,
            4.450 ,4.466, 4.482, 4.498))

  if (N > 25 && N <= 50){
    d3 <- 0.80818 - 0.0051871 * N + 0.00005098 * N^2 - 0.00000019 * N^3
    d2 <- Data.d2[N == Data.d2$n,2]
  } else if (N > 50) {
    d3 <- 0.80818 - 0.0051871 * N + 0.00005098 * N^2 - 0.00000019 * N^3
    d2 <- 2.88606 + 0.051313 * N - 0.00049243 * N^2 + 0.00000188 * N^3
  } else {
    d2 <- Data.d2[N == Data.d2$n,2]
    d3 <- Data.d3[N == Data.d3$n,2]
  }

  if (N > 1) {
    c4 <- sqrt(2/(N-1)) * gamma(N/2) / gamma((N-1)/2)
    c5 <- sqrt(1 - c4^2)
  } else {
    c4 <- 0
    c5 <- 0
  }

  UCL <- d2 * sigma + 3 * d3 * sigma
  CL <- d2 * sigma
  LCL <- max(0, d2 * sigma - 3 * d3 * sigma)

  return(list(constants = c(d2, d3, c4, c5), limits = data.frame(LCL,UCL), center = CL))
}

.controlLimits <- function(mu = NA, sigma, n, k = 3, type = c("xbar", "r", "s"), unbiasingConstantUsed = TRUE) {
  type = match.arg(type)
  UCLvector <- c()
  LCLvector <- c()
  for (i in seq_along(n)) {
    if (type == "xbar") {
      UCL <- mu + (k * sigma) / sqrt(n[i])
      LCL <- mu - (k * sigma) / sqrt(n[i])
    } else  if (type == "r") {
      d2 <- KnownControlStats.RS(n[i], 0)$constants[1]
      d3 <- KnownControlStats.RS(n[i], 0)$constants[2]
      UCL <- d2 * sigma + k * d3 * sigma
      LCL <- d2 * sigma - k * d3 * sigma
      LCL <- max(0, LCL) # LCL in R-chart must be >= 0
    } else  if (type == "s") {
      if (n[i] > 1) {
        c4 <- KnownControlStats.RS(n[i], 0)$constants[3]
        c5 <- KnownControlStats.RS(n[i], 0)$constants[4]
        if (unbiasingConstantUsed) {
          UCL <- c4 * sigma + k * sigma * c5
          LCL <- c4 * sigma - k * sigma * c5
        } else {
          UCL <- sigma + k * (c5 / c4) * sigma
          LCL <- sigma - k * (c5 / c4) * sigma
        }
        LCL <- max(0, LCL) # LCL in S-chart must be >= 0
      } else {
        LCL <- NaN
        UCL <- NaN
      }
    }
    UCLvector <- c(UCLvector, UCL)
    LCLvector <- c(LCLvector, LCL)
  }
  return(list(LCL = LCLvector, UCL = UCLvector))
}

.controlChart <- function(dataset,  plotType                  = c("xBar", "R", "I", "MR", "MMR", "s"),
                                    stages                    = "",
                                    xBarSdType                = c("r", "s"),
                                    phase2                    = FALSE,
                                    phase2Mu                  = "",
                                    phase2Sd                  = "",
                                    fixedSubgroupSize         = "",
                                    warningLimits             = FALSE,
                                    xAxisLabels               = "",
                                    xAxisTitle                = gettext("Sample"),
                                    movingRangeLength         = 2,
                                    clLabelSize               = 4.5,
                                    stagesSeparateCalculation = TRUE,
                                    unbiasingConstantUsed     = TRUE
                          ) {
  plotType <- match.arg(plotType)

  # This function returns all the needed data for the plot and table: data for the points, the limits, the labels and a list of point violations for the table
  controlChartData <- .controlChart_calculations(dataset, plotType = plotType, stages = stages, xBarSdType = xBarSdType,
                                                 phase2 = phase2, phase2Mu = phase2Mu, phase2Sd = phase2Sd,
                                                 fixedSubgroupSize = fixedSubgroupSize, warningLimits = warningLimits,
                                                 movingRangeLength = movingRangeLength, stagesSeparateCalculation = stagesSeparateCalculation,
                                                 tableLabels = xAxisLabels, unbiasingConstantUsed = unbiasingConstantUsed)


  # This function turns the point violation list into a JASP table
  table <- .controlChart_table(controlChartData$violationTable, plotType = plotType, stages = stages)


  # This function turns the raw plot data into a ggPlot
  plotObject <- .controlChart_plotting(pointData = controlChartData$pointData, clData = controlChartData$clData,
                                       stageLabels = controlChartData$stageLabels, clLabels = controlChartData$clLabels,
                                       plotType = plotType, stages = stages, phase2 = phase2, warningLimits = warningLimits,
                                       xAxisLabels = xAxisLabels, xAxisTitle = xAxisTitle, clLabelSize = clLabelSize)


  return(list(plotObject = plotObject, table = table, controlChartData = controlChartData))
}

.controlChart_calculations <- function(dataset, plotType                  = c("xBar", "R", "I", "MR", "MMR", "s"),
                                                stages                    = "",
                                                xBarSdType                = c("r", "s"),
                                                phase2                    = FALSE,
                                                phase2Mu                  = "",
                                                phase2Sd                  = "",
                                                fixedSubgroupSize         = "",
                                                warningLimits             = FALSE,
                                                movingRangeLength         = 2,
                                                stagesSeparateCalculation = TRUE,
                                                tableLabels               = "",
                                                unbiasingConstantUsed     = TRUE
                                       ) {
  plotType <- match.arg(plotType)
  if (identical(stages, "")) {
    nStages <- 1
    dataset[["stage"]] <- 1
    stages <- "stage"
  } else if (!identical(stages, "")) {
    nStages <- length(unique(dataset[[stages]]))
  }

  ### Calculate plot values per stage and combine into single dataframe ###
  plotData <- data.frame(matrix(ncol = 4, nrow = 0))
  clData <- data.frame(matrix(ncol = 5, nrow = 0))
  tableList <- list()
  colnames(plotData) <- c("plotStatistic", "subgroup", "stage", "dotColor")
  colnames(clData) <- c("subgroup", "stage", "LCL", "UCL", "center")
  if (warningLimits) {
    warningLimitsDf <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(warningLimitsDf) <- c("UWL1", "LWL1", "UWL2", "LWL2")
    clData <- cbind(clData, warningLimitsDf)
  }
  dfLimitLabel <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(dfLimitLabel) <- c("x", "y", "label")
  dfStageLabels <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(dfStageLabels) <- c("x", "y", "label", "separationLine")
  ###
  ### Beginning of loop over all stages to calculate all values
  ###
  for (i in seq_len(nStages)) {
    stage <- unique(dataset[[stages]])[i]
    dataCurrentStage <- dataset[which(dataset[[stages]] == stage), ][!names(dataset) %in% stages]
    ###
    ### Calculations for I, MR and MMR chart
    ###
    if (plotType == "I" || plotType == "MR" || plotType == "MMR") {
      if (plotType == "MMR") {
        subgroupMeans <- apply(dataCurrentStage, 1, mean, na.rm = TRUE)
        dataCurrentStage <- data.frame("subgroupMeans" = subgroupMeans)
      }
      k <- movingRangeLength
      # qcc has no moving range plot, so we need to arrange data in a matrix with the observation + k future observation per row and calculate the range chart
      dataCurrentStageVector <- unlist(dataCurrentStage)
      mrMatrix <- matrix(dataCurrentStageVector[seq((k), length(dataCurrentStageVector))])   # remove first k - 1 elements
      for (j in seq(1, k-1)) {
        mrMatrix <- cbind(mrMatrix, matrix(dataCurrentStageVector[seq(k-j, length(dataCurrentStageVector)-j)]))
      }
      meanMovingRange <- mean(.rowRanges(mrMatrix)$ranges, na.rm = TRUE)
      d2 <- KnownControlStats.RS(k)$constants[1]
      sd <- meanMovingRange/d2
      if (plotType == "I") {
        processMean <- mean(dataCurrentStageVector, na.rm = TRUE) # manually calculate mean as package does not remove NAs
        qccObject <- qcc::qcc(dataCurrentStage, type ='xbar.one', plot = FALSE, std.dev = sd, center = processMean)
        plotStatistic <- qccObject$statistics
        limits <- qccObject$limits
      } else if (plotType == "MR" || plotType == "MMR" ) {
        qccObject <- qcc::qcc(mrMatrix, type = "R", plot = FALSE, std.dev = sd, center = meanMovingRange)
        limits <- unlist(.controlLimits(meanMovingRange, sd, n = k, type = "r"))
        # the qcc package calculates the ranges ignoring the NAs, but for the MR chart we want the range to be NA if there are any NAs in the moving range
        qccObject$statistics[which(!complete.cases(mrMatrix))] <- NA
        plotStatistic <- c(rep(NA, k-1), qccObject$statistics)
      }
      LCL <- limits[1]
      UCL <- limits[2]
      center <- qccObject$center
    ###
    ### Calculations for R chart
    ###
    } else if (plotType == "R") {
      n <- if (!identical(fixedSubgroupSize, "")) fixedSubgroupSize else apply(dataCurrentStage, 1, function(x) return(sum(!is.na(x)))) # returns the number of non NA values per row
      # manually calculate mean and sd as the package gives wrong results with NAs
      if(phase2) {
        sigma <- phase2Sd
      } else if (stagesSeparateCalculation) {
        sigma <- .sdXbar(df = dataCurrentStage, type = "r")
      } else if (!stagesSeparateCalculation) {
        # use the whole dataset for calculation
        sigma <- .sdXbar(df = dataset[!names(dataset) %in% stages], type = "r")
      }
      d2 <- sapply(n, function(x) KnownControlStats.RS(x, 0)$constants[1])
      mu <- sigma * d2
      qccObject <- qcc::qcc(dataCurrentStage, type ='R', plot = FALSE, center = mu, std.dev = sigma, sizes = ncol(dataCurrentStage))
      # the qcc package returns -Inf when all values are NA, which does not look good in ggplot. So we replace it with NA.
      qccObject$statistics[is.infinite(qccObject$statistics)] <- NA
      plotStatistic <- qccObject$statistics

      limits <- .controlLimits(mu, sigma, n = n, type = "r")
      center <- mu
      UCL <- limits$UCL
      LCL <- limits$LCL
    ###
    ### Calculations for X-bar chart
    ###
    } else if (plotType == "xBar") {
      xBarSdType <- match.arg(xBarSdType)
      if (phase2) {
        mu <- as.numeric(phase2Mu)
        sigma <- as.numeric(phase2Sd)
      } else if (stagesSeparateCalculation) {
        # manually calculate mean and sd as the package gives wrong results with NAs
        mu <- mean(unlist(dataCurrentStage), na.rm = TRUE)
        sigma <- .sdXbar(df = dataCurrentStage, type = xBarSdType, unbiasingConstantUsed = unbiasingConstantUsed)
      } else if (!stagesSeparateCalculation) {
        # use the whole dataset for calculation
        mu <- mean(unlist(dataset[!names(dataset) %in% stages]), na.rm = TRUE)
        sigma <- .sdXbar(df = dataset[!names(dataset) %in% stages], type = xBarSdType, unbiasingConstantUsed = unbiasingConstantUsed)
      }
      qccObject <- qcc::qcc(dataCurrentStage, type ='xbar', plot = FALSE, center = mu, sizes = ncol(dataCurrentStage), std.dev = sigma)
      plotStatistic <- qccObject$statistics
      n <- if (!identical(fixedSubgroupSize, "")) fixedSubgroupSize else apply(dataCurrentStage, 1, function(x) return(sum(!is.na(x)))) # returns the number of non NA values per row
      limits <- .controlLimits(mu, sigma, n = n, type = "xbar")
      center <- mu
      UCL <- limits$UCL
      LCL <- limits$LCL

      # upper and lower warning limits at 1 sd and 2 sd
      WL1 <- .controlLimits(mu, sigma, n = n, type = "xbar", k = 1)
      WL2 <- .controlLimits(mu, sigma, n = n, type = "xbar", k = 2)
      UWL1 <- WL1$UCL
      LWL1 <- WL1$LCL
      UWL2 <- WL2$UCL
      LWL2 <- WL2$LCL
    ###
    ### Calculations for S chart
    ###
    } else if (plotType == "s") {
      if(phase2) {
        sigma <- phase2Sd
      } else if (stagesSeparateCalculation) {
        sigma <- .sdXbar(df = dataCurrentStage, type = "s", unbiasingConstantUsed = unbiasingConstantUsed)
      } else if (!stagesSeparateCalculation) {
        # use the whole dataset for calculation
        sigma <- .sdXbar(df = dataset[!names(dataset) %in% stages], type = "s", unbiasingConstantUsed = unbiasingConstantUsed)
      }
      qccObject <- qcc::qcc(dataCurrentStage, type ='S', plot = FALSE, center = sigma, sizes = ncol(dataCurrentStage))
      plotStatistic <- qccObject$statistics
      n <- if (!identical(fixedSubgroupSize, "")) fixedSubgroupSize else apply(dataCurrentStage, 1, function(x) return(sum(!is.na(x)))) # returns the number of non NA values per row
      limits <- .controlLimits(sigma = sigma, n = n, type = "s", unbiasingConstantUsed = unbiasingConstantUsed)
      if (unbiasingConstantUsed) {
        c4s <- sapply(n, function(x) return(KnownControlStats.RS(x, 0)$constants[3]))
        center <- sigma * c4s
      } else {
        center <- sigma
      }
      UCL <- limits$UCL
      LCL <- limits$LCL
    }
    if (i != 1) {
        subgroups <- seq(max(plotData$subgroup) + 1, max(plotData$subgroup) + length(plotStatistic))
      dfStageLabels <- rbind(dfStageLabels, data.frame(x = max(plotData$subgroup) + length(subgroups)/2,
                                                       y = NA,  # the y value will be filled in later
                                                       label = stage,
                                                       separationLine = max(plotData$subgroup) + .5))
    } else {
      subgroups <- seq_along(plotStatistic)
      dfStageLabels <- rbind(dfStageLabels, data.frame(x = max(subgroups)/2 + 0.5,
                                                       y = NA, # the y value will be filled in later
                                                       label = stage,
                                                       separationLine = NA))
    }

    if (length(na.omit(plotStatistic)) > 1) {
      if (plotType == "MR" || plotType == "MMR") {
        dotColor <- ifelse(c(rep(NA, k-1), NelsonLaws(qccObject)$red_points), 'red', 'blue')
      } else {
        dotColor <- ifelse(NelsonLaws(qccObject, allsix = plotType == "I")$red_points, 'red', 'blue')
      }
    } else {
      dotColor <- ifelse(plotStatistic > UCL | plotStatistic < LCL, "red", "blue")
      dotColor[is.na(dotColor)] <- "blue"
    }
    # if more than half of the dots are violations, do not show red dots.
    nOutOfLimits <- sum(dotColor[!is.na(dotColor)] == "red")
    if (nOutOfLimits > length(qccObject$statistics)/2)
      dotColor <- "blue"

    stagePlotData <- data.frame("plotStatistic" = plotStatistic,
                                "subgroup" = subgroups,
                                "stage" = stage,
                                "dotColor" = dotColor)
    stageClData <- data.frame("subgroup" = subgroups,
                              "stage" = stage,
                              "LCL" = LCL,
                              "UCL" = UCL,
                              "center" = center)

    if (warningLimits) {
      stageClData <- cbind(stageClData,
                           data.frame("UWL1" = UWL1,
                                      "LWL1" = LWL1,
                                      "UWL2" = UWL2,
                                      "LWL2" = LWL2))
    }

    # offset to align geom_step lines with observations
    stageClData <- rbind(stageClData, stageClData[nrow(stageClData),])
    stageClData[["subgroup"]][nrow(stageClData)] <- stageClData[["subgroup"]][nrow(stageClData)] + 1
    stageClData[["subgroup"]] <- stageClData[["subgroup"]] - 0.5


    plotData <- rbind(plotData, stagePlotData)
    clData <- rbind(clData, stageClData)
    decimals <- .numDecimals
    # even if there are multiple different centers, LCL and UCL within a stage, only the last one is shown on the label
    lastCenter <- center[length(center)]
    lastLCL <- LCL[length(LCL)]
    lastUCL <- UCL[length(UCL)]
    if (i == nStages) { # the last label has more space available and hence can be longer
      labelXPos <- max(subgroups) * 1.1
      labelText <- c(
        gettextf("CL = %g",  round(lastCenter, decimals)),
        gettextf("LCL = %g", round(lastLCL, decimals)),
        gettextf("UCL = %g", round(lastUCL, decimals)))
    } else {
      labelXPos <- max(subgroups) + .5
      labelText <- c(
        round(lastCenter, decimals),
        round(lastLCL, decimals),
        round(lastUCL, decimals))
    }
    if (stagesSeparateCalculation || (!stagesSeparateCalculation && i == nStages))
      dfLimitLabel <- rbind(dfLimitLabel, data.frame(x = labelXPos,
                                                     y = c(lastCenter, lastLCL, lastUCL),
                                                     label = labelText))
    tableLabelsCurrentStage <- if (identical(tableLabels, "")) subgroups else as.character(tableLabels)[subgroups]
    if (plotType == "MR" || plotType == "MMR")
      tableLabelsCurrentStage <- tableLabelsCurrentStage[-seq(1, k-1)]
    tableList[[i]] <- .NelsonTableList(qccObject = qccObject, type = plotType, labels = tableLabelsCurrentStage)
    tableListLengths <- sapply(tableList[[i]], length)
    if (any(tableListLengths > 0)) {
      tableList[[i]][["stage"]] <- as.character(stage)
      tableList[[i]] <- lapply(tableList[[i]], "length<-", max(lengths(tableList[[i]]))) # this fills up all elements of the list with NAs so all elements are the same size
    }
  }
  return(list("pointData"      = plotData,
              "clData"         = clData,
              "clLabels"       = dfLimitLabel,
              "stageLabels"    = dfStageLabels,
              "violationTable" = tableList
              ))
}

.controlChart_table <- function(tableList, plotType = c("xBar", "R", "I", "MR", "MMR", "s"),
                                           stages   = "") {
  plotType <- match.arg(plotType)
  tableTitle <- switch (plotType,
                        "xBar" = "x-bar",
                        "R" = "range",
                        "I" = "individuals",
                        "MR" = "moving range",
                        "MMR" = "moving range",
                        "s" = "s"
  )
  table <- createJaspTable(title = gettextf("Test results for %1$s chart", tableTitle))
  tableListVectorized <- unlist(tableList, recursive = FALSE)
  tableLongestVector <- max(sapply(tableListVectorized, length))
  if (tableLongestVector > 0) {
    tableListCombined <- tapply(tableListVectorized, names(tableListVectorized), function(x) unlist(x, FALSE, FALSE))
    if (!identical(stages, ""))
      table$addColumnInfo(name = "stage",              title = stages,                                     type = "string")
    if (length(tableListCombined[["test1"]][!is.na(tableListCombined[["test1"]])]) > 0)
      table$addColumnInfo(name = "test1",              title = gettextf("Test 1: Beyond limit"),           type = "string")
    if (length(tableListCombined[["test2"]][!is.na(tableListCombined[["test2"]])]) > 0)
      table$addColumnInfo(name = "test2",              title = gettextf("Test 2: Shift"),                  type = "string")
    if (length(tableListCombined[["test3"]][!is.na(tableListCombined[["test3"]])]) > 0)
      table$addColumnInfo(name = "test3",              title = gettextf("Test 3: Trend"),                  type = "string")
    if (plotType == "I") {
      if (length(tableListCombined[["test4"]][!is.na(tableListCombined[["test4"]])]) > 0)
        table$addColumnInfo(name = "test4",              title = gettextf("Test 4: Increasing variation"),   type = "string")
      if (length(tableListCombined[["test5"]][!is.na(tableListCombined[["test5"]])]) > 0)
        table$addColumnInfo(name = "test5",              title = gettextf("Test 5: Reducing variation"),     type = "string")
      if (length(tableListCombined[["test6"]][!is.na(tableListCombined[["test6"]])]) > 0)
        table$addColumnInfo(name = "test6",              title = gettextf("Test 6: Bimodal distribution"),   type = "string")
    }
    tableData <- list(
      "stage" = tableListCombined[["stage"]],
      "test1" = tableListCombined[["test1"]],
      "test2" = tableListCombined[["test2"]],
      "test3" = tableListCombined[["test3"]]
    )
    if (plotType == "I") {
      tableData[["test4"]] <- tableListCombined[["test4"]]
      tableData[["test5"]] <- tableListCombined[["test5"]]
      tableData[["test6"]] <- tableListCombined[["test6"]]
    }
    table$setData(tableData)
    table$showSpecifiedColumnsOnly <- TRUE
    table$addFootnote(message = gettext("Points where a test failed."))
  }
  return(table)
}

.controlChart_plotting <- function(pointData, clData, stageLabels, clLabels,
                                   plotType = c("xBar", "R", "I", "MR", "MMR", "s"),
                                   stages = "",
                                   phase2 = FALSE,
                                   warningLimits = FALSE,
                                   xAxisLabels = "",
                                   xAxisTitle = "",
                                   clLabelSize = 4.5) {
  plotType <- match.arg(plotType)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(pointData$plotStatistic, clData$LCL, clData$UCL, clData$center))
  xBreaks <- unique(as.integer(jaspGraphs::getPrettyAxisBreaks(pointData$subgroup))) # we only want integers on the x-axis

  if (xBreaks[1] == 0)  # never start counting at 0 on x axis
    xBreaks[1] <- 1
  xLimits <- c(0.5, max(xBreaks) * 1.2 + 0.5) # add some buffer, but at least .5

  if (!identical(xAxisLabels, "")) {
    if (max(xBreaks) > length(xAxisLabels)) # sometimes pretty creates breaks that go beyond the labels that are given, this must be avoided else it will display an NA on this tick
      xBreaks[length(xBreaks)] <- length(xAxisLabels)
    xLabels <- xAxisLabels[xBreaks]
  } else {
    xLabels <- xBreaks
  }

  yTitle <- switch (plotType,
                    "xBar" = "Sample average",
                    "R"    = "Sample range",
                    "I"    = "Individual value",
                    "MR"   = "Moving range",
                    "MMR"  = "Moving range of subgroup mean",
                    "s"    = "Sample std. dev.")
  if (!identical(stages, ""))
    stageLabels$y <- max(yBreaks)
  lineType <- if (phase2) "solid" else "dashed"

  # Create plot
  plotObject <- ggplot2::ggplot(clData, ggplot2::aes(x = subgroup, group = stage)) +
    ggplot2::geom_step(mapping = ggplot2::aes(x = subgroup, y = center) , col = "green", linewidth = 1) +
    ggplot2::geom_step(mapping = ggplot2::aes(x = subgroup, y = UCL) , col = "red", linewidth = 1.5, linetype = lineType) +
    ggplot2::geom_step(mapping = ggplot2::aes(x = subgroup, y = LCL) , col = "red", linewidth = 1.5, linetype = lineType)
  if (!identical(stages, "")) {
    plotObject <- plotObject + ggplot2::geom_vline(xintercept = na.omit(stageLabels[["separationLine"]])) +
      ggplot2::geom_text(data = stageLabels, mapping = ggplot2::aes(x = x, y = y, label = label),
                                                  size = 6, fontface = "bold", inherit.aes = FALSE)
  }
  if (warningLimits) {
    plotObject <- plotObject + ggplot2::geom_step(data = clData, mapping = ggplot2::aes(x = subgroup, y = UWL1), col = "orange",
                                                  linewidth = 1, linetype = "dashed") +
      ggplot2::geom_step(data = clData, mapping = ggplot2::aes(x = subgroup, y = LWL1), col = "orange",
                         linewidth = 1, linetype = "dashed") +
      ggplot2::geom_step(data = clData, mapping = ggplot2::aes(x = subgroup, y = UWL2), col = "orange",
                         linewidth = 1, linetype = "dashed") +
      ggplot2::geom_step(data = clData, mapping = ggplot2::aes(x = subgroup, y = LWL2), col = "orange",
                         linewidth = 1, linetype = "dashed")
  }
  plotObject <- plotObject + ggplot2::geom_label(data = clLabels, mapping = ggplot2::aes(x = x, y = y, label = label),
                                                 inherit.aes = FALSE, size = clLabelSize) +
    ggplot2::scale_y_continuous(name = yTitle, breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(name = xAxisTitle, breaks = xBreaks, limits = xLimits, labels = xLabels) +
    jaspGraphs::geom_line(pointData, mapping = ggplot2::aes(x = subgroup, y = plotStatistic, group = stage), color = "blue") +
    jaspGraphs::geom_point(pointData, mapping = ggplot2::aes(x = subgroup, y = plotStatistic, group = stage),
                           size = 4, fill = pointData$dotColor, inherit.aes = TRUE) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plotObject)
}

.NelsonTableList <- function(qccObject, type = "xBar", phase2 = TRUE, labels = NULL) {
  violationsList <- list("test1" = NULL, "test2" = NULL, "test3" = NULL)

  if (length(na.omit(qccObject$statistics)) <= 1) # no need for table with only 1 group
    return(violationsList)

  if (!phase2 || type == "I") {
    Test <- NelsonLaws(data = qccObject, allsix = TRUE, xLabels = labels)
    violationsList[["test4"]] <- Test$Rules$R4
    violationsList[["test5"]] <- Test$Rules$R5
    violationsList[["test6"]] <- Test$Rules$R6
  } else if (type == "np" || type == "c" || type == "u" || type == "Laney p'" || type == "Laney u'") {
    Test <- NelsonLaws(data = qccObject, xLabels = labels, chart = "c")
  } else if (type == "P") {
    Test <- NelsonLaws(data = qccObject, xLabels = labels, chart = "p")
  } else {
    Test <- NelsonLaws(data = qccObject, xLabels = labels)
  }

  violationsList[["test1"]] <- Test$Rules$R1
  violationsList[["test2"]] <- Test$Rules$R2
  violationsList[["test3"]] <- Test$Rules$R3

  return(violationsList)
}
