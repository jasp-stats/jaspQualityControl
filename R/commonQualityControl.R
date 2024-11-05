
# imports from packages
#' @importFrom jaspBase createJaspContainer createJaspPlot createJaspState createJaspTable createJaspHtml
#' .extractErrorMessage .hasErrors isTryError .readDataSetToEnd


#############################################################
## Common functions for plots ###############################
#############################################################
.reshapeSubgroupDataLongToWide <- function(dataset,
                                           measurements,
                                           stages = "",
                                           subgroupVariable = "",
                                           subgroupSizeType = c("manual", "groupingVariable"),
                                           manualSubgroupSizeValue = 5,
                                           subgroupVariableMethod = c("sameLabel", "newLabel")) {
  subgroupSizeType <- match.arg(subgroupSizeType)
  # Rearrange data if not already wide format (one group per row)
  # if subgroup size is set manual, use that. Else determine subgroup size from largest level in subgroups variable
  if (subgroupSizeType == "manual") {
    k <- manualSubgroupSizeValue
    if (stages != "") {
      # Only take the first stage of each subgroup, to avoid multiple stages being defined
      stagesPerSubgroup <- dataset[[stages]][seq(1, length(dataset[[stages]]), k)]
    }
    # fill up with NA to allow all subgroup sizes
    if(length(dataset[[measurements]]) %% k != 0) {
      rest <- length(dataset[[measurements]]) %% k
      dataset_expanded <- c(dataset[[measurements]], rep(NA, k - rest))
      dataset <- as.data.frame(matrix(dataset_expanded, ncol = k, byrow = TRUE))
    } else {
      dataset <- as.data.frame(matrix(dataset[[measurements]], ncol = k, byrow = TRUE))
    }
    measurements <- colnames(dataset)
    axisLabels <- ""
    xAxisTitle <- gettext("Sample")
    if (stages != "") {
      dataset[[stages]] <- stagesPerSubgroup
    }
  } else if (subgroupSizeType == "groupingVariable") {
    subgroupVariableMethod <- match.arg(subgroupVariableMethod)
    subgroups <- dataset[[subgroupVariable]]
    subgroups <- na.omit(subgroups)
    if (subgroupVariableMethod == "sameLabel") {
      if (stages != "") {
        # Only take the first defined stage of each subgroup, to avoid multiple stages being defined
        stagesPerSubgroup <- dataset[[stages]][match(unique(subgroups), subgroups)]
      }
      # add sequence of occurence to allow pivot_wider
      occurenceVector <- with(dataset, ave(seq_along(subgroups), subgroups, FUN = seq_along))
      dataset$occurence <- occurenceVector
      # transform into one group per row
      dataset <- tidyr::pivot_wider(data = dataset[c(measurements, subgroupVariable, "occurence")],
                                    values_from = tidyr::all_of(measurements), names_from = occurence)
    } else if (subgroupVariableMethod == "newLabel") {
      runLengthVector <- rle(as.character(subgroups))
      occurenceVector <- c(unlist(sapply(runLengthVector$lengths, seq_len)))
      idVector <- seq_len(length(runLengthVector$lengths))
      newSubgroupsPosition <- cumsum(runLengthVector$lengths) - (runLengthVector$lengths - 1)
      subgroupVector <- subgroups[newSubgroupsPosition]
      if (stages != "") {
        # Only take the first defined stage of each subgroup, to avoid multiple stages being defined
        stagesPerSubgroup <- dataset[[stages]][newSubgroupsPosition]
      }

      dataset$occurence <- occurenceVector
      dataset$identifier <- c(unlist(sapply(idVector, function(x) rep(idVector[x], runLengthVector$lengths[x]))))
      dataset <- tidyr::pivot_wider(data = dataset[c(measurements, "identifier", "occurence")],
                                    values_from = tidyr::all_of(measurements), names_from = occurence)
      dataset <- dataset[ ,colnames(dataset) != "identifier"]
      dataset[[subgroupVariable]] <- subgroupVector
    }
    # arrange into dataframe
    dataset <- as.data.frame(dataset)
    measurements <- as.character(unique(occurenceVector))
    axisLabels <- dataset[[subgroupVariable]]
    xAxisTitle <- subgroupVariable
    if (stages != ""){
      dataset[[stages]] <- stagesPerSubgroup
      stageDataForOrdering <- factor(dataset[[stages]], levels = unique(dataset[[stages]]))
      axisLabels <- axisLabels[order(stageDataForOrdering)]
    }
  }
  return(list(dataset = dataset,
              measurements = measurements,
              axisLabels = axisLabels,
              xAxisTitle = xAxisTitle))
}

.qcReport <- function(text = NULL, # a string or vector of strings,
                      plots, # a list of ggplots. If the plots should stay on top of each other, use a nested list.
                      tables = NULL, # a list of dataframes. If tables should be in the same plot, use a nested list.
                      textMaxRows = 5,
                      tableTitles = "", # a list with the same layout as the tables list
                      reportTitle = "",
                      tableSize = 6) {
  lengthAllElements <- length(plots) + length(tables) + (!is.null(text)) + sum(!sapply(plots, ggplot2::is.ggplot)) # length of plots, tables, one for the text and addition tables of nested plots
  lengthAllElements <- if (lengthAllElements %% 2 != 0) lengthAllElements + 1 else lengthAllElements # always need even number
  lengthAllElements <- if (any(!sapply(plots, ggplot2::is.ggplot)) && lengthAllElements < 3) 4 else lengthAllElements # edge case if only a nested plot is given
  plotList <- list()
  plotList[1:lengthAllElements] <- NA
  if (!is.null(text))
    plotList[[2]] <- .ggplotWithText(text = text, maxRows = textMaxRows)
  for (i in seq_along(plots)) {
    currentPlot <- plots[[i]]
    if (ggplot2::is.ggplot(currentPlot)) {
      plotPos <- min(.indicesOfNAinList(plotList)) # smallest empty index
      plotList[[plotPos]] <- currentPlot
    } else { # it should be a list of ggplots
      plot1pos <- min(.indicesOfNAinList(plotList)) # smallest empty index
      plotList[[plot1pos]] <- currentPlot[[1]]
      plotList[[plot1pos + 2]] <- currentPlot[[2]] # plus two, so it's always below plot 1
    }
  }
  for (j in seq_along(tables)) {
    currentTable <- tables[[j]]
    currentTitle <- if (!identical(tableTitles, "")) tableTitles[[j]] else ""
    tablePlot <- .ggplotTable(currentTable, currentTitle, tableSize)
    tablePos <- min(.indicesOfNAinList(plotList)) # smallest empty index
    plotList[[tablePos]] <- tablePlot
  }

  plotList <- matrix(plotList, ncol = 2, byrow = TRUE)
  topLabel <- if (!identical(reportTitle, "")) c("", reportTitle) else NULL
  if (!identical(reportTitle, "") && all(is.na(plotList[,2])))
    topLabel <- c(reportTitle, "")

  # If there are still NA in the list, fill them with empty plots
  indicesRemainingNA <- .indicesOfNAinList(plotList)
  if (!is.null(indicesRemainingNA)) {
    for (k in indicesRemainingNA)
      plotList[[k]] <- ggplot2::ggplot() + ggplot2::theme_void()
  }

  plot <- jaspGraphs::ggMatrixPlot(plotList = plotList, topLabels = topLabel)
  return(plot)
}

.indicesOfNAinList <- function(list) {
  nElements <- length(list)
  indices <- c()
  for (i in seq_len(nElements)) {
    currentElement <- list[i]
    if (is.na(currentElement))
      indices <- c(indices, i)
  }
  return(indices)
}

.ggplotWithText <- function(text, maxRows = 5) {
  nText <- length(text)
  annotation <- data.frame(x = rep(0, nText), y = (maxRows:1)[1:nText], label = text)
  p <- ggplot2::ggplot() + ggplot2::theme_void() +
    ggplot2::annotate("rect", xmin = -.05, xmax = 1.05, ymin = 1 - .5, ymax = maxRows + .5, fill = 'lightgray', color = "black", linewidth = .7) +
    ggplot2::geom_text(data=annotation, ggplot2::aes(x = x, y = y, label = label), size = 6, hjust = 0) +
    ggplot2::scale_x_continuous(limits = c(-.05, 1.05)) +
    ggplot2::scale_y_continuous(limits = c(1 - .5,  maxRows + .5))
  return(p)
}

.ggplotTable <- function(tableObject, titles = "", tableSize = 6) {
  if (!is.data.frame(tableObject)) { # then it should be a list
    nTables <- length(tableObject)
    df <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(df) <- c("x", "y", "tb")
    tibble <- tibble::as_tibble(df)
    titleDf <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(titleDf) <- c("x", "y", "label")
    for (i in seq_along(tableObject)) {
      yPosTable <- if (i == 1) 1 else as.numeric(tibble[i-1,2]) - .08 - nrow(tableObject[[i - 1]]) * .08
      if (!identical(titles, "")) {
        yPosTitle <- yPosTable
        yPosTable <- yPosTable - .08 # distance between table and title
        titleDf <- rbind(titleDf, data.frame(x = 0, y = yPosTitle, label = titles[[i]]))
      }
      tibble <- rbind(tibble, tibble::tibble("x" = 0, "y" = yPosTable, tb = tableObject[i]))
    }
  } else {
    yPosTitle <- 1
    if (!identical(titles, "")) {
      titleDf <- data.frame(x = 0, y = 1 , label = titles)
      yPosTitle <- yPosTitle - .08
    }
    tibble <- tibble::tibble(x = 0, y = yPosTitle, tb = list(tableObject))

  }
  p <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggpp::geom_table(data = tibble, ggplot2::aes(x = x, y = y, label = tb),
                     table.colnames = TRUE, size = tableSize, hjust = 0, vjust = 1) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 1))
  if (!identical(titles, ""))
    p <- p + ggplot2::geom_text(titleDf, mapping = ggplot2::aes(x = x, y = y, label = label), hjust = 0, size = 7, vjust = 1)
  return(p)
}


ggplotTable <- function(dataframe, displayColNames = FALSE){
  df <- tibble::tibble(dataframe)
  p <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggpp::geom_table(data = data.frame(x = 1, y = 1), ggplot2::aes(x = x, y = y), label = list(df),
                     table.colnames = displayColNames, size = 7)

  return(p)
}

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



# Generalized function to check if there are at least k TRUE values within k + 1 elements
check_previous_k <- function(i, vec, k) {
  # Define the range: from max(1, i - k) to i
  range <- vec[max(1, i - k):i]
  # Check if at least k out of k + 1 (or fewer if near the start) are TRUE
  return(vec[i] == TRUE && sum(range) >= k)
}

.nelsonLaws <- function(plotStatistics,
                        sigma = NULL,
                        center = NULL,
                        UCL = NULL,
                        LCL = NULL,
                        ruleList) {
  if (length(LCL) == 1) {
    LCL <- rep(LCL, length(plotStatistics))
  }
  if (length(UCL) == 1) {
    UCL <- rep(UCL, length(plotStatistics))
  }

  redPoints <- c()
  violationList <- list()

  # Rule 1: Outside of control limits
  if (!is.null(ruleList[["rule1"]]) && ruleList[["rule1"]][["enabled"]] == TRUE) {
    r1 <- which(plotStatistics < LCL | plotStatistics > UCL)
    redPoints <- c(redPoints, r1)
    violationList[["test1"]] <- if (length(r1) > 0) r1 else numeric()
  }

  # Rule 2: k points in a row, on the same side of center line
  if (!is.null(ruleList[["rule2"]]) && ruleList[["rule2"]][["enabled"]] == TRUE) {
    k2 <- ruleList[["rule2"]][["k"]]
    sideVector <- ifelse(plotStatistics > center, 1, ifelse(plotStatistics < center, -1, 0))
    rleSides <- rle(sideVector)
    r2 <- c()
    # Track the current start index for runs
    currentIndex <- 1
    # Iterate over the lengths and values of rle
    for (i in seq_along(rleSides$lengths)) {
      runLength <- rleSides$lengths[i]
      runValue <- rleSides$values[i]
      # Check if the run is on the same side and the length is >= k
      if (runLength >= k2 && runValue != 0) {
        # Add indices of this run to the offending indices vector
        r2 <- c(r2, ((currentIndex + runLength - 1)-(runLength-k2)):(currentIndex + runLength - 1))
      }
      # Update the current index
      currentIndex <- currentIndex + runLength
    }
    redPoints <- c(redPoints, r2)
    violationList[["test2"]] <- if (length(r2) > 0) r2 else numeric()
  }

  # Rule 3: k points in a row, all increasing or decreasing
  if (!is.null(ruleList[["rule3"]]) && ruleList[["rule3"]][["enabled"]] == TRUE) {
    k3 <- ruleList[["rule3"]][["k"]]
    r3 <- c()

    # Loop through the points to find consecutive increases or decreases
    currentIndex <- 1
    consecutiveIncreaseCount <- 0
    consecutiveDecreaseCount <- 0

    # Iterate over the points, considering each point and the next for comparison
    for (i in 1:(length(plotStatistics) - 1)) {
      if (plotStatistics[i + 1] > plotStatistics[i]) {
        # Increment the consecutive increase count and reset decrease count
        consecutiveIncreaseCount <- consecutiveIncreaseCount + 1
        consecutiveDecreaseCount <- 0
      } else if (plotStatistics[i + 1] < plotStatistics[i]) {
        # Increment the consecutive decrease count and reset increase count
        consecutiveDecreaseCount <- consecutiveDecreaseCount + 1
        consecutiveIncreaseCount <- 0
      } else {
        # Reset counts if neither increasing nor decreasing
        consecutiveIncreaseCount <- 0
        consecutiveDecreaseCount <- 0
      }

      # Check if the count reaches k and record the offending sequence
      if (consecutiveIncreaseCount >= k3) {
        startIdx <- currentIndex - consecutiveIncreaseCount
        r3 <- c(r3, startIdx + 1:(consecutiveIncreaseCount + 1))
        consecutiveIncreaseCount <- 0  # Reset to find next potential sequence
      }
      if (consecutiveDecreaseCount >= k3) {
        startIdx <- currentIndex - consecutiveDecreaseCount
        r3 <- c(r3, startIdx + 1:(consecutiveDecreaseCount + 1))
        consecutiveDecreaseCount <- 0  # Reset to find next potential sequence
      }

      # Update currentIndex for the next iteration
      currentIndex <- currentIndex + 1
    }
    redPoints <- c(redPoints, r3)
    violationList[["test3"]] <- if (length(r3) > 0) r3 else numeric()
  }

  # Rule 4: k points in a row, alternating increase and decrease
  if (!is.null(ruleList[["rule4"]]) && ruleList[["rule4"]][["enabled"]] == TRUE) {
    k4 <- ruleList[["rule4"]][["k"]]
    r4 <- c()

    # Function to determine if two numbers alternate in increase/decrease pattern
    patternAlternates <- function(x, y) {
      return((x < 0 && y > 0) || (x > 0 && y < 0))
    }

    # Calculate differences between consecutive points
    differences <- diff(plotStatistics)


    for (i in 1:(length(differences)-(k4-1))) {
      currentSequence <- differences[i:(i+k4-1)]
      sequenceCount <- 1
      for (j in 1:(k4-1)) {
        if (patternAlternates(currentSequence[j], currentSequence[j + 1])) {
          sequenceCount <- sequenceCount + 1
        }
      }
      if (sequenceCount >= k4) {
        r4 <- c(r4, k4 + i)
      }
    }
    redPoints <- c(redPoints, r4)
    violationList[["test4"]] <- if (length(r4) > 0) r4 else numeric()
  }

  # Rule 5: k out of k+1 points > 2 std. dev. from center line (same side)
  if (!is.null(ruleList[["rule5"]]) && ruleList[["rule5"]][["enabled"]] == TRUE) {
    k5 <- ruleList[["rule5"]][["k"]]
    r5 <- c()

    aboveBolVector <- plotStatistics > (center + 2 * sigma)
    belowBolVector <- plotStatistics < center - 2 * sigma
    r5above <- which(sapply(seq_along(aboveBolVector), check_previous_k, vec = aboveBolVector, k = k5))
    r5below <- which(sapply(seq_along(belowBolVector), check_previous_k, vec = belowBolVector, k = k5))
    r5 <- sort(c(r5, r5above, r5below))
    redPoints <- c(redPoints, r5)
    violationList[["test5"]] <- if (length(r5) > 0) r5 else numeric()
  }

  # Rule 6: k out of k+1 points > 1 std. dev. from center line (same side)
  if (!is.null(ruleList[["rule6"]]) && ruleList[["rule6"]][["enabled"]] == TRUE) {
    k6 <- ruleList[["rule6"]][["k"]]
    r6 <- c()

    aboveBolVector <-  plotStatistics > (center + sigma)
    belowBolVector <- plotStatistics < center - sigma
    r6above <- which(sapply(seq_along(aboveBolVector), check_previous_k, vec = aboveBolVector, k = k6))
    r6below <- which(sapply(seq_along(belowBolVector), check_previous_k, vec = belowBolVector, k = k6))
    r6 <- sort(c(r6, r6above, r6below))

    redPoints <- c(redPoints, r6)
    violationList[["test6"]] <- if (length(r6) > 0) r6 else numeric()
  }

  # Rule 7: k points in a row within 1 std. dev from center line (either side)
  if (!is.null(ruleList[["rule7"]]) && ruleList[["rule7"]][["enabled"]] == TRUE) {
    k7 <- ruleList[["rule7"]][["k"]]
    r7 <- c()

    withinBolVector <- plotStatistics < (center + sigma) & plotStatistics > (center - sigma)

    # Use ave to create a cumulative counter for TRUE sequences, resetting at each FALSE
    withinSeqVector <- ave(withinBolVector, cumsum(!withinBolVector), FUN = function(x) ifelse(x, seq_along(x), 0))
    r7 <- c(r7, which(withinSeqVector > k7))
    redPoints <- c(redPoints, r7)
    violationList[["test7"]] <- if (length(r7) > 0) r7 else numeric()
  }

  # Rule 8: k points in a row > 1 std. dev. from center line (either side)
  if (!is.null(ruleList[["rule8"]]) && ruleList[["rule8"]][["enabled"]] == TRUE) {
    k8 <- ruleList[["rule8"]][["k"]]
    r8 <- c()

    outsideBolVector <- plotStatistics > (center + sigma) | plotStatistics < (center - sigma)

    # Use ave to create a cumulative counter for TRUE sequences, resetting at each FALSE
    outsideSeqVector <- ave(outsideBolVector, cumsum(!outsideBolVector), FUN = function(x) ifelse(x, seq_along(x), 0))
    r8 <- c(r8, which(outsideSeqVector > k8))
    redPoints <- c(redPoints, r8)
    violationList[["test8"]] <- if (length(r8) > 0) r8 else numeric()
  }

  # Rule 9: Benneyan test, k successive points equal to 0
  if (!is.null(ruleList[["rule9"]]) && ruleList[["rule9"]][["enabled"]] == TRUE) {
    k9 <- ruleList[["rule9"]][["k"]]
    r9 <- c()

    zeroBolVector <- plotStatistics == 0

    # Use ave to create a cumulative counter for TRUE sequences, resetting at each FALSE
    zeroSeqVector <- ave(zeroBolVector, cumsum(!zeroBolVector), FUN = function(x) ifelse(x, seq_along(x), 0))
    r9 <- c(r9, which(zeroSeqVector > k9))
    redPoints <- c(redPoints, r9)
    violationList[["test9"]] <- if (length(r9) > 0) r9 else numeric()
  }

  if (length(violationList) == 0)
    violationList[["test1"]] <- list()

  return(list(redPoints = redPoints, violationList = violationList))
  violationList <- list()}

.sdXbar <- function(df, type = c("s", "r"), unbiasingConstantUsed = TRUE) {
  type <- match.arg(type)

  # exclude groups with single observation from calculation
  rowRemovalIndex <- which(apply(df, 1, function(x) sum(!is.na(x)) < 2)) # get index of rows with less than 2 obs.
  if (length(rowRemovalIndex) > 0)
    df <- df[-rowRemovalIndex, ]

  # return sdWithin = 0 if no groups have more than 1 obs
  if (length(df) == 0)
    return(0)

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

  # d2 and d3 are unbiasing constants as reported in D. J. Wheeler and D. S. Chambers. (1992). Understanding Statistical Process Control, Second Edition, SPC Press, Inc.

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

.controlChart <- function(dataset,  plotType        = c("xBar", "R", "I", "MR", "MMR", "s", "cusum", "ewma", "g", "t"),
                          ruleList                  = list(),
                          stages                    = "",
                          xBarSdType                = c("r", "s"),
                          nSigmasControlLimits      = 3,
                          phase2                    = FALSE,
                          phase2Mu                  = "",
                          phase2Sd                  = "",
                          fixedSubgroupSize         = "",
                          warningLimits             = FALSE,
                          specificationLimits       = NA,
                          xAxisLabels               = "",
                          tableLabels               = "",
                          xAxisTitle                = gettext("Sample"),
                          movingRangeLength         = 2,
                          clLabelSize               = 4.5,
                          stagesSeparateCalculation = TRUE,
                          unbiasingConstantUsed     = TRUE,
                          cusumShiftSize            = 0.5,
                          cusumTarget               = 0,
                          ewmaLambda                = 0.3,
                          gAndtUnit                 = c("days", "hours", "minutes", "opportunities"),
                          phase2gChartProportion    = 0.5,
                          tChartDistribution        = c("weibull", "exponential"),
                          phase2tChartDistributionShape   = 1,
                          phase2tChartDistributionScale   = 3
) {
  plotType <- match.arg(plotType)

  # This function returns all the needed data for the plot and table: data for the points, the limits, the labels and a list of point violations for the table
  controlChartData <- .controlChart_calculations(dataset, ruleList = ruleList, plotType = plotType, stages = stages, xBarSdType = xBarSdType,
                                                 nSigmasControlLimits = nSigmasControlLimits, phase2 = phase2,
                                                 phase2Mu = phase2Mu, phase2Sd = phase2Sd, fixedSubgroupSize = fixedSubgroupSize,
                                                 warningLimits = warningLimits, movingRangeLength = movingRangeLength,
                                                 stagesSeparateCalculation = stagesSeparateCalculation, unbiasingConstantUsed = unbiasingConstantUsed,
                                                 cusumShiftSize = cusumShiftSize, cusumTarget = cusumTarget, ewmaLambda = ewmaLambda,
                                                 tChartDistribution = tChartDistribution, phase2tChartDistributionShape = phase2tChartDistributionShape,
                                                 phase2tChartDistributionScale = phase2tChartDistributionScale,
                                                 phase2gChartProportion = phase2gChartProportion)


  # This function turns the point violation list into a JASP table
  table <- .controlChart_table(controlChartData$violationTable, plotType = plotType, stages = stages, tableLabels = tableLabels)


  # This function turns the raw plot data into a ggPlot
  plotObject <- .controlChart_plotting(pointData = controlChartData$pointData, clData = controlChartData$clData,
                                       stageLabels = controlChartData$stageLabels, clLabels = controlChartData$clLabels,
                                       plotType = plotType, stages = stages, phase2 = phase2, warningLimits = warningLimits,
                                       xAxisLabels = xAxisLabels, xAxisTitle = xAxisTitle, clLabelSize = clLabelSize,
                                       specificationLimits = specificationLimits, gAndtUnit = gAndtUnit)


  return(list(plotObject = plotObject, table = table, controlChartData = controlChartData))
}

.controlChart_calculations <- function(dataset, plotType               = c("xBar", "R", "I", "MR", "MMR", "s", "cusum", "ewma", "g", "t"),
                                       ruleList                        = list(),
                                       stages                          = "",
                                       xBarSdType                      = c("r", "s"),
                                       nSigmasControlLimits            = 3,
                                       phase2                          = FALSE,
                                       phase2Mu                        = "",
                                       phase2Sd                        = "",
                                       fixedSubgroupSize               = "",
                                       warningLimits                   = FALSE,
                                       movingRangeLength               = 2,
                                       stagesSeparateCalculation       = TRUE,
                                       unbiasingConstantUsed           = TRUE,
                                       cusumShiftSize                  = 0.5,
                                       cusumTarget                     = 0,
                                       ewmaLambda                      = 0.3,
                                       phase2gChartProportion          = 0.5,
                                       tChartDistribution              = c("weibull", "exponential"),
                                       phase2tChartDistributionShape   = 1,
                                       phase2tChartDistributionScale   = 3
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
      sigma <- meanMovingRange/d2
      if (plotType == "I") {
        processMean <- mean(dataCurrentStageVector, na.rm = TRUE) # manually calculate mean as package does not remove NAs
        qccObject <- qcc::qcc(dataCurrentStage, type ='xbar.one', plot = FALSE, std.dev = sigma, center = processMean, nsigmas = nSigmasControlLimits)
        plotStatistic <- qccObject$statistics
        limits <- qccObject$limits
      } else if (plotType == "MR" || plotType == "MMR" ) {
        qccObject <- qcc::qcc(mrMatrix, type = "R", plot = FALSE, std.dev = sigma, center = meanMovingRange, nsigmas = nSigmasControlLimits)
        limits <- unlist(.controlLimits(meanMovingRange, sigma, n = k, k = nSigmasControlLimits, type = "r"))
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
      n <- if (!identical(fixedSubgroupSize, "")) fixedSubgroupSize else rowSums(!is.na(dataCurrentStage)) # returns the number of non NA values per row
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
      qccObject <- qcc::qcc(dataCurrentStage, type ='R', plot = FALSE, center = mu, std.dev = sigma, sizes = ncol(dataCurrentStage), nsigmas = nSigmasControlLimits)
      # the qcc package returns -Inf when all values are NA, which does not look good in ggplot. So we replace it with NA.
      qccObject$statistics[is.infinite(qccObject$statistics)] <- NA
      # the qcc package just returns the values instead of the ranges when there is only one column
      plotStatistic <- if (ncol(dataCurrentStage) == 1) rep(0, nrow(dataCurrentStage)) else qccObject$statistics
      limits <- .controlLimits(mu, sigma, n = n, type = "r", k = nSigmasControlLimits)
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
      qccObject <- qcc::qcc(dataCurrentStage, type ='xbar', plot = FALSE, center = mu, sizes = ncol(dataCurrentStage), std.dev = sigma, nsigmas = nSigmasControlLimits)
      plotStatistic <- qccObject$statistics
      n <- if (!identical(fixedSubgroupSize, "")) fixedSubgroupSize else rowSums(!is.na(dataCurrentStage)) # returns the number of non NA values per row
      limits <- .controlLimits(mu, sigma, n = n, type = "xbar", k = nSigmasControlLimits)
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
      if (phase2) {
        sigma <- phase2Sd
      } else if (stagesSeparateCalculation) {
        sigma <- .sdXbar(df = dataCurrentStage, type = "s", unbiasingConstantUsed = unbiasingConstantUsed)
      } else if (!stagesSeparateCalculation) {
        # use the whole dataset for calculation
        sigma <- .sdXbar(df = dataset[!names(dataset) %in% stages], type = "s", unbiasingConstantUsed = unbiasingConstantUsed)
      }
      qccObject <- qcc::qcc(dataCurrentStage, type ='S', plot = FALSE, center = sigma, sizes = ncol(dataCurrentStage), nsigmas = nSigmasControlLimits)
      plotStatistic <- qccObject$statistics
      n <- if (!identical(fixedSubgroupSize, "")) fixedSubgroupSize else rowSums(!is.na(dataCurrentStage)) # returns the number of non NA values per row
      limits <- .controlLimits(sigma = sigma, n = n, type = "s", unbiasingConstantUsed = unbiasingConstantUsed, k = nSigmasControlLimits)
      if (unbiasingConstantUsed) {
        c4s <- sapply(n, function(x) return(KnownControlStats.RS(x, 0)$constants[3]))
        center <- sigma * c4s
      } else {
        center <- sigma
      }
      UCL <- limits$UCL
      LCL <- limits$LCL
      ###
      ### Calculations for cusum chart
      ###
    } else if (plotType == "cusum") {
      n <- if (!identical(fixedSubgroupSize, "")) fixedSubgroupSize else rowSums(!is.na(dataCurrentStage)) # returns the number of non NA values per row
      # sigma for subgroup size = 1 is calculated as the average moving range sd
      if (phase2) {
        sigma <- as.numeric(phase2Sd)
      } else if (all(n <= 1)) {
        k <- movingRangeLength
        dataCurrentStageVector <- unlist(dataCurrentStage)
        mrMatrix <- matrix(dataCurrentStageVector[seq((k), length(dataCurrentStageVector))])   # remove first k - 1 elements
        for (j in seq(1, k-1)) {
          mrMatrix <- cbind(mrMatrix, matrix(dataCurrentStageVector[seq(k-j, length(dataCurrentStageVector)-j)]))
        }
        meanMovingRange <- mean(.rowRanges(mrMatrix)$ranges, na.rm = TRUE)
        d2 <- KnownControlStats.RS(k)$constants[1]
        sigma <- meanMovingRange/d2
        # sigma for subgroup size > 1
      } else {
        sigma <- .sdXbar(dataCurrentStage, type = xBarSdType, unbiasingConstantUsed = unbiasingConstantUsed)
      }
      plotStatisticUpper <- .cusumPoints(dataCurrentStage, sigma, n, cusumTarget, cusumShiftSize, cuType = "upper")
      plotStatisticLower <- .cusumPoints(dataCurrentStage, sigma, n, cusumTarget, cusumShiftSize, cuType = "lower")
      plotStatistic <- c(plotStatisticUpper, plotStatisticLower)
      UCL <- nSigmasControlLimits*sigma/sqrt(n)
      UCL[is.infinite(UCL)] <- 0  # looks better in the plot to display CLs at 0 if calculation fails
      LCL <- -UCL
      center <- 0 # not to be confused with the target, even if target != 0, the center line of the plot should be at 0
      ###
      ### Calculations for ewma chart
      ###
    } else if (plotType == "ewma") {
      n <- if (!identical(fixedSubgroupSize, "")) fixedSubgroupSize else rowSums(!is.na(dataCurrentStage)) # returns the number of non NA values per row
      if (phase2) {
        sigma <- as.numeric(phase2Sd)
      } else if (all(n == 1)) {
        k <- movingRangeLength
        dataCurrentStageVector <- unlist(dataCurrentStage)
        mrMatrix <- matrix(dataCurrentStageVector[seq((k), length(dataCurrentStageVector))])   # remove first k - 1 elements
        for (j in seq(1, k-1)) {
          mrMatrix <- cbind(mrMatrix, matrix(dataCurrentStageVector[seq(k-j, length(dataCurrentStageVector)-j)]))
        }
        meanMovingRange <- mean(.rowRanges(mrMatrix)$ranges, na.rm = TRUE)
        d2 <- KnownControlStats.RS(k)$constants[1]
        sigma <- meanMovingRange/d2
        # sigma for subgroup size > 1
      } else {
        sigma <- .sdXbar(dataCurrentStage, type = xBarSdType, unbiasingConstantUsed = unbiasingConstantUsed)
      }
      plotStatistic <- .ewmaPlotStatistic(data = dataCurrentStage, lambda = ewmaLambda)
      center <- mean(unlist(dataCurrentStage), na.rm = TRUE)
      individualPointSigmas <- .ewmaPointSigmas(n = n, sigma = sigma, lambda = ewmaLambda)
      UCL <- center + individualPointSigmas * nSigmasControlLimits
      LCL <- center - individualPointSigmas * nSigmasControlLimits
      ###
      ### Calculations for g chart
      ###
    } else if (plotType == "g") {
      plotStatistic <- unname(unlist(dataCurrentStage))
      gChartStatistics <- .gChartStatistics(intervals = plotStatistic, phase2Proportion = if(phase2) phase2gChartProportion else "")
      center <- gChartStatistics$CL
      UCL <- gChartStatistics$UCL
      LCL <- gChartStatistics$LCL
      p <- gChartStatistics$p
      ###
      ### Calculations for t chart
      ###
    } else if (plotType == "t") {
      plotStatistic <- unname(unlist(dataCurrentStage))
      if (any(plotStatistic[!is.na(plotStatistic)] == 0)) {
        zeroCorrectionIndices <- which(plotStatistic == 0, arr.ind = TRUE)
        plotStatistic[zeroCorrectionIndices] <- min(plotStatistic[plotStatistic > 0], na.rm = TRUE)/2
      }

      if (!phase2) {
        distributionPars <- .distributionParameters(plotStatistic[!is.na(plotStatistic)], distribution = tChartDistribution)
        shape <- distributionPars$beta
        scale <- distributionPars$theta
      } else {
        shape <- if (tChartDistribution == "exponential") 1 else phase2tChartDistributionShape
        scale <- phase2tChartDistributionScale
      }
      center <- qweibull(p = .5, shape = shape, scale = scale)
      UCL <- qweibull(p = pnorm(3), shape = shape, scale = scale)
      LCL <- qweibull(p = pnorm(-3), shape = shape, scale = scale)
    }
    if (i != 1) {
      if (plotType == "cusum") {
        subgroups <- rep(seq_along(plotStatisticUpper), 2) + max(plotData$subgroup)
      } else {
        subgroups <- seq(max(plotData$subgroup) + 1, max(plotData$subgroup) + length(plotStatistic))
      }

      dfStageLabels <- rbind(dfStageLabels, data.frame(x = max(plotData$subgroup) + length(subgroups)/2,
                                                       y = NA,  # the y value will be filled in later
                                                       label = stage,
                                                       separationLine = max(plotData$subgroup) + .5))
    } else {
      if (plotType == "cusum") {
        subgroups <- rep(seq_along(plotStatisticUpper), 2)
      } else {
        subgroups <- seq_along(plotStatistic)
      }
      dfStageLabels <- rbind(dfStageLabels, data.frame(x = max(subgroups)/2 + 0.5,
                                                       y = NA, # the y value will be filled in later
                                                       label = stage,
                                                       separationLine = NA))
    }

    if (length(na.omit(plotStatistic)) > 1) {
      if (plotType == "MR" || plotType == "MMR") {
        # redPoints <- .nelsonLaws(plotStatistic, sigma, center, UCL, LCL, ruleList) This gives a vector of indices, can probably be used to simplify the code below
        dotColor <- ifelse(c(rep(NA, k-1),  NelsonLaws(qccObject)$red_points), 'red', 'blue')
      } else if (plotType == "cusum" || plotType == "ewma" || plotType == "g" || plotType == "t") {
        dotColor <- ifelse(plotStatistic > UCL | plotStatistic < LCL, "red", "blue") # TODO: add proper tests here, other than test 1
        dotColor[is.na(dotColor)] <- "blue"
      } else {
        dotColor <- rep("blue", length(plotStatistic))
        redPoints <- .nelsonLaws(plotStatistic, sigma, center, UCL, LCL, ruleList)$redPoints
        dotColor[redPoints] <- "red"
          # ifelse(NelsonLaws(qccObject, allsix = (plotType == "I"))$red_points, 'red', 'blue')
      }
    } else {
      dotColor <- ifelse(plotStatistic > UCL | plotStatistic < LCL, "red", "blue") # TODO: try out if the new function can handle single value, if yes remove this whole logic
      dotColor[is.na(dotColor)] <- "blue"
    }
    # if more than half of the dots are violations, do not show red dots.
    nOutOfLimits <- sum(dotColor[!is.na(dotColor)] == "red")
    if (nOutOfLimits > length(plotStatistic)/2)
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
      labelXPos <- max(subgroups) * 1.06
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
    tableLabelsCurrentStage <- subgroups
    if (plotType == "MR" || plotType == "MMR")
      tableLabelsCurrentStage <- tableLabelsCurrentStage[-seq(1, k-1)]
    if (plotType == "cusum" || plotType == "ewma" || plotType == "g" || plotType == "t") {
      tableList[[i]] <- c() # TODO: update with new Nelson Function
    } else {
      tableList[[i]] <- .nelsonLaws(plotStatistic, sigma, center, UCL, LCL, ruleList)$violationList
      tableListLengths <- sapply(tableList[[i]], length)
      if (any(tableListLengths > 0)) {
        tableList[[i]][["stage"]] <- as.character(stage)
        tableList[[i]] <- lapply(tableList[[i]], "length<-", max(lengths(tableList[[i]]))) # this fills up all elements of the list with NAs so all elements are the same size
      }
    }
  }
  return(list("pointData"      = plotData,
              "clData"         = clData,
              "clLabels"       = dfLimitLabel,
              "stageLabels"    = dfStageLabels,
              "violationTable" = tableList,
              "sd"             = sigma
  ))
}

.controlChart_table <- function(tableList,
                                plotType = c("xBar", "R", "I", "MR", "MMR", "s", "cusum", "ewma", "g", "t"),
                                stages   = "",
                                tableLabels = "") {
  plotType <- match.arg(plotType)
  tableTitle <- switch (plotType,
                        "xBar" = "x-bar",
                        "R" = "range",
                        "I" = "individuals",
                        "MR" = "moving range",
                        "MMR" = "moving range",
                        "s" = "s",
                        "cusum" = "cumulative sum",
                        "ewma"  = "exponentially weighted moving average",
                        "g"     = "g",
                        "t"     = "t"
  )
  table <- createJaspTable(title = gettextf("Test results for %1$s chart", tableTitle))
  table$showSpecifiedColumnsOnly <- TRUE
  tableListVectorized <- unlist(tableList, recursive = FALSE)
  tableLongestVector <- if (is.null(tableListVectorized)) 0 else max(sapply(tableListVectorized, length))
  if (tableLongestVector > 0) {
    # combine the tests for different stages in same column
    tableListCombined <- tapply(tableListVectorized, names(tableListVectorized), function(x) unlist(x, FALSE, FALSE))
    # format
    for (test in names(tableListCombined)[names(tableListCombined) != "stage"]) {
      points <- as.numeric(tableListCombined[[test]])
      if (all(is.na(points)))
        next()
      formattedPoints <- c()
      for (point in points) {
        formattedPoint <- if (is.na(point)) NA else paste(gettext("Point"), point)
        if (!identical(tableLabels, ""))
          formattedPoint <- if (is.na(point)) NA else paste0(formattedPoint, " (", tableLabels[point], ")")
        formattedPoints <- c(formattedPoints, formattedPoint)
      }
      tableListCombined[[test]] <- formattedPoints
    }
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
    table$addFootnote(message = gettext("Points where a test failed."))
  } else {
    table$addColumnInfo(name = "noViolations", title = gettext("Tests"), type = "string")
    tableData <- list("noViolations" = gettext("No test violations occurred."))
  }
  table$setData(tableData)
  return(table)
}

.controlChart_plotting <- function(pointData, clData, stageLabels, clLabels,
                                   plotType            = c("xBar", "R", "I", "MR", "MMR", "s", "cusum", "ewma", "g", "t"),
                                   stages              = "",
                                   phase2              = FALSE,
                                   warningLimits       = FALSE,
                                   specificationLimits = NA,
                                   xAxisLabels         = "",
                                   xAxisTitle          = "",
                                   clLabelSize         = 4.5,
                                   gAndtUnit           = c("days", "hours", "minutes", "opportunities")) {
  plotType <- match.arg(plotType)
  yBreakDeterminants <- c(pointData$plotStatistic, clData$LCL, clData$UCL, clData$center)
  # if all statistics are 0, pretty will select c(-1, 0). But c(0, 1) is better
  yBreaks <- if (identical(unique(na.omit(yBreakDeterminants)), 0)) c(0, 1) else jaspGraphs::getPrettyAxisBreaks(yBreakDeterminants)
  yLimits <- range(yBreaks)
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

  if (plotType == "g" || plotType == "t") {
    unitString <- paste0(toupper(substr(gAndtUnit, 1, 1)), substr(gAndtUnit, 2, nchar(gAndtUnit)))
    yTitle <- gettextf("%1$s between events", unitString)
  } else {
    yTitle <- switch (plotType,
                      "xBar"  = gettext("Sample average"),
                      "R"     = gettext("Sample range"),
                      "I"     = gettext("Individual value"),
                      "MR"    = gettext("Moving range"),
                      "MMR"   = gettext("Moving range of subgroup mean"),
                      "s"     = gettext("Sample std. dev."),
                      "cusum" = gettext("Cumulative sum"),
                      "ewma"  = gettext("Exponentially weighted moving average"))
  }
  lineType <- if (phase2) "solid" else "dashed"
  # Create plot
  plotObject <- ggplot2::ggplot(clData, ggplot2::aes(x = subgroup, group = stage)) +
    ggplot2::geom_step(mapping = ggplot2::aes(x = subgroup, y = center) , col = "green", linewidth = 1, na.rm = TRUE) +
    ggplot2::geom_step(mapping = ggplot2::aes(x = subgroup, y = UCL) , col = "red", linewidth = 1.5, linetype = lineType, na.rm = TRUE) +
    ggplot2::geom_step(mapping = ggplot2::aes(x = subgroup, y = LCL) , col = "red", linewidth = 1.5, linetype = lineType, na.rm = TRUE)
  if (!all(is.na(specificationLimits))) {
    length(specificationLimits) <- 3
    xPosLabel <- 1 - max(xLimits) * 0.06
    xLimits[1] <- xPosLabel
    xRange <- range(clData$subgroup)
    lslYPos <- specificationLimits[1]
    targetYPos <- specificationLimits[2]
    uslYPos <- specificationLimits[3]
    yLimits <- range(c(yLimits, lslYPos, targetYPos, uslYPos), na.rm = TRUE)
    if (!is.na(lslYPos)) {
      lslLineDf <- data.frame(xPos = xRange, yPos = rep(lslYPos, each = 2))
      lslLabelDf <- data.frame(xPos = xPosLabel, yPos = lslYPos, label = gettextf("LSL = %g", round(lslYPos, .numDecimals)))
      plotObject <- plotObject + ggplot2::geom_line(data = lslLineDf, mapping = ggplot2::aes(x = xPos, y = yPos),
                                                    inherit.aes = FALSE, linewidth = 1.5, col = "darkred", na.rm = TRUE) +
        ggplot2::geom_label(data = lslLabelDf, mapping = ggplot2::aes(x = xPos, y = yPos, label = label),
                            inherit.aes = FALSE, size = clLabelSize, hjust = "inward", na.rm = TRUE)
    }
    if (!is.na(targetYPos)) {
      targetLineDf <- data.frame(xPos = xRange, yPos = rep(targetYPos, each = 2))
      targetLabelDf <- data.frame(xPos = xPosLabel, yPos = targetYPos, label = gettextf("Tar. = %g", round(targetYPos, .numDecimals)))
      plotObject <- plotObject + ggplot2::geom_line(data = targetLineDf, mapping = ggplot2::aes(x = xPos, y = yPos),
                                                    inherit.aes = FALSE, linewidth = 1.5, col = "darkgreen", na.rm = TRUE) +
        ggplot2::geom_label(data = targetLabelDf, mapping = ggplot2::aes(x = xPos, y = yPos, label = label),
                            inherit.aes = FALSE, size = clLabelSize, hjust = "inward", na.rm = TRUE)
    }
    if (!is.na(uslYPos)) {
      uslLineDf <- data.frame(xPos = xRange, yPos = rep(uslYPos, each = 2))
      uslLabelDf <- data.frame(xPos = xPosLabel, yPos = uslYPos, label = gettextf("USL = %g", round(uslYPos, .numDecimals)))
      plotObject <- plotObject + ggplot2::geom_line(data = uslLineDf, mapping = ggplot2::aes(x = xPos, y = yPos),
                                                    inherit.aes = FALSE, linewidth = 1.5, col = "darkred", na.rm = TRUE) +
        ggplot2::geom_label(data = uslLabelDf, mapping = ggplot2::aes(x = xPos, y = yPos, label = label),
                            inherit.aes = FALSE, size = clLabelSize, hjust = "inward", na.rm = TRUE)
    }
  }
  if (warningLimits) {
    plotObject <- plotObject + ggplot2::geom_step(data = clData, mapping = ggplot2::aes(x = subgroup, y = UWL1), col = "orange",
                                                  linewidth = 1, linetype = "dashed", na.rm = TRUE) +
      ggplot2::geom_step(data = clData, mapping = ggplot2::aes(x = subgroup, y = LWL1), col = "orange",
                         linewidth = 1, linetype = "dashed", na.rm = TRUE) +
      ggplot2::geom_step(data = clData, mapping = ggplot2::aes(x = subgroup, y = UWL2), col = "orange",
                         linewidth = 1, linetype = "dashed", na.rm = TRUE) +
      ggplot2::geom_step(data = clData, mapping = ggplot2::aes(x = subgroup, y = LWL2), col = "orange",
                         linewidth = 1, linetype = "dashed", na.rm = TRUE)
  }
  if (!identical(stages, "")) {
    stageLabels$y <- max(yLimits)
    plotObject <- plotObject + ggplot2::geom_vline(xintercept = na.omit(stageLabels[["separationLine"]])) +
      ggplot2::geom_text(data = stageLabels, mapping = ggplot2::aes(x = x, y = y, label = label),
                         size = 6, fontface = "bold", inherit.aes = FALSE)
  }
  plotObject <- plotObject + ggplot2::geom_label(data = clLabels, mapping = ggplot2::aes(x = x, y = y, label = label),
                                                 inherit.aes = FALSE, size = clLabelSize, na.rm = TRUE) +
    ggplot2::scale_y_continuous(name = yTitle, breaks = yBreaks, limits = yLimits) +
    ggplot2::scale_x_continuous(name = xAxisTitle, breaks = xBreaks, limits = xLimits, labels = xLabels)
  if (plotType != "cusum") {
    plotObject <- plotObject +
      jaspGraphs::geom_line(pointData, mapping = ggplot2::aes(x = subgroup, y = plotStatistic, group = stage), color = "blue",
                            na.rm = TRUE)
  } else {
    # since the upper and lower part of the cusum chart are in the same df, we split the first and second half; if there are stages, do this for each stage
    # loop over stages
    for (stage_i in unique(pointData[["stage"]])) {
      pointDataSubset <- subset.data.frame(pointData, pointData$stage == stage_i)
      if (length(pointDataSubset$subgroup) %% 2 != 0)
        stop("Data provided to cusum plot function is not symmetric, i.e., unequal amount of points above and below 0.")
      nSubgroupsStage <- length(pointDataSubset$subgroup)/2 # the data should always be symmetrical, hence we can divide by two
      firstHalf <- seq(1, nSubgroupsStage)
      secondHalf <- firstHalf + nSubgroupsStage
      pointDataFirstHalf <- pointDataSubset[firstHalf,]
      pointDataSecondHalf <- pointDataSubset[secondHalf,]
      plotObject <- plotObject +
        jaspGraphs::geom_line(pointDataFirstHalf, mapping = ggplot2::aes(x = subgroup, y = plotStatistic, group = stage), color = "blue",
                              na.rm = TRUE) +
        jaspGraphs::geom_line(pointDataSecondHalf, mapping = ggplot2::aes(x = subgroup, y = plotStatistic, group = stage), color = "blue",
                              na.rm = TRUE)
    }

  }
  plotObject <- plotObject  +
    jaspGraphs::geom_point(pointData, mapping = ggplot2::aes(x = subgroup, y = plotStatistic, group = stage),
                           size = 4, fill = pointData$dotColor, inherit.aes = TRUE, na.rm = TRUE) +
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

.cusumPoints <- function(data, sigma, n, target, shiftSize, cuType = c("lower", "upper")) {
  cuType <- match.arg(cuType)

  # Initialize vector and first point
  cuSumPoints <- c()
  rowMeanInitial <- if (length(data[1,]) == 1) data[1,] else rowMeans(data[1,], na.rm = T)
  if (cuType == "lower") {
    initialPoint <- rowMeanInitial - (target - shiftSize*(sigma/sqrt(n[1])))
    cuSumPoints[1] <-  min(0, initialPoint)
  } else {
    initialPoint <- rowMeanInitial - (target + shiftSize*(sigma/sqrt(n[1])))
    cuSumPoints[1] <-  max(0, initialPoint)
  }

  # Loop over remaining data
  for (i in seq(2, nrow(data))) {
    rowMean_i <- if (length(data[i,]) == 1) data[i,] else rowMeans(data[i,], na.rm = TRUE)
    if (cuType == "lower") {
      cuSumPoint <- cuSumPoints[i-1] + rowMean_i - (target - shiftSize*(sigma/sqrt(n[i])))
      cuSumPoints[i] <-  min(0, cuSumPoint)
    } else {
      cuSumPoint <- cuSumPoints[i-1] + rowMean_i - (target + shiftSize*(sigma/sqrt(n[i])))
      cuSumPoints[i] <-  max(0, cuSumPoint)
    }
  }
  return(cuSumPoints)
}

.ewmaPlotStatistic <- function(data, lambda) {
  ewmaPoints <- c()
  initialPoint <- mean(unlist(data), na.rm = TRUE)
  rowMeanVector <- if (ncol(data) == 1) unlist(data) else rowMeans(data, na.rm = TRUE)
  for (i in seq(1, nrow(data))) {
    previousPoint <- if (i == 1) initialPoint else ewmaPoints[i - 1]
    ewmaPoint <- lambda * rowMeanVector[i] + (1 - lambda) * previousPoint
    ewmaPoints[i] <- ewmaPoint
  }
  return(ewmaPoints)
}

.ewmaPointSigmas <- function(n = n, sigma = sigma, lambda = ewmaLambda) {
  ewmaPointSigmas <- c()
  initialPoint <- (lambda * sigma) / sqrt(n[1])
  ewmaPointSigmas[1] <- initialPoint
  for (i in seq(2, length(n))) {
    nVector <- n[seq(i-1, 1)]
    exponentVector <- 2*seq(1, i-1)
    exponentiallyWeightedVector <- ((1 - lambda)^(exponentVector)) / nVector
    ewmaPointSigma <- sigma * lambda * sqrt(sum(1/n[i], exponentiallyWeightedVector))
    ewmaPointSigmas[i] <- ewmaPointSigma
  }
  return(ewmaPointSigmas)
}

.gChartStatistics <- function(intervals, phase2Proportion = "") {
  intervalsMean <- mean(intervals, na.rm = TRUE)
  n <- sum(!is.na(intervals))
  p <- if (identical(phase2Proportion, "")) ((n - 1) / n) / (intervalsMean + 1) else phase2Proportion

  # calculate CL
  p2a <- pgeom(qgeom(0.5, prob = p) - 1, prob = p, lower.tail = T)      # p2a is the CDF at G2a
  p2b <- pgeom(qgeom(0.5, prob = p), prob = p, lower.tail = T)  # p2b is the CDF at G2b
  # Find G2a and G2b
  G2a <- qgeom(p2a, prob = p) + 1  # Add 1 to get trials count
  G2b <- G2a + 1                   # Since G2b = G2a + 1
  # Perform linear interpolation to find G2
  G2 <- G2a + (0.5 - p2a) / (p2b - p2a)
  # Calculate CL = G2 - 1
  CL <- G2 - 1

  # calculate LCL
  p1a <- pgeom(qgeom(0.99865, prob = p, lower.tail = F) - 1, prob = p, lower.tail = T)
  p1b <- pgeom(qgeom(0.99865, prob = p, lower.tail = F), prob = p, lower.tail = T)
  # Find G2a and G2b
  G1a <- qgeom(p1a, prob = p) + 1
  G1b <- G1a + 1
  # Perform linear interpolation to find G1
  G1 <- G1a + (0.00135  - p1a) / (p1b - p1a)
  LCL <- G1 - 1
  LCL <- max(0, LCL)

  # calculate UCL
  p3a <- pgeom(qgeom(0.00135, prob = p, lower.tail = F) - 1, prob = p, lower.tail = T)      # p2a is the CDF at G2a
  p3b <- pgeom(qgeom(0.00135, prob = p, lower.tail = F), prob = p, lower.tail = T)  # p2b is the CDF at G2b
  # Find G2a and G2b
  G3a <- qgeom(p3a, prob = p) + 1  # Add 1 to get trials count
  G3b <- G3a + 1                   # Since G2b = G2a + 1
  # Perform linear interpolation to find G2
  G3 <- G3a + (0.99865  - p3a) / (p3b - p3a)
  UCL <- G3 - 1

  return(list(p = p, CL = CL, UCL = UCL, LCL = LCL))
}

.distributionParameters <- function(data, distribution = c("lognormal", "weibull", "3ParameterLognormal", "3ParameterWeibull", "exponential")){
  if (distribution == "lognormal") {
    fit_Lnorm <- try(EnvStats::elnorm(data))
    if (jaspBase::isTryError(fit_Lnorm))
      stop(gettext("Parameter estimation failed. Values might be too extreme. Try a different distribution."), call. = FALSE)
    beta <- fit_Lnorm$parameters[1]
    theta <- fit_Lnorm$parameters[2]
  } else if (distribution == "weibull") {
    fit_Weibull <- try(fitdistrplus::fitdist(data, "weibull", method = "mle",
                                             control = list(maxit = 500, abstol = .Machine$double.eps, reltol = .Machine$double.eps)))
    if (jaspBase::isTryError(fit_Weibull))
      stop(gettext("Parameter estimation failed. Values might be too extreme. Try a different distribution."), call. = FALSE)
    beta <- fit_Weibull$estimate[[1]]
    theta <- fit_Weibull$estimate[[2]]
  } else if(distribution == "3ParameterLognormal") {
    temp <- try(EnvStats::elnorm3(data))
    if (jaspBase::isTryError(temp))
      stop(gettext("Parameter estimation failed. Values might be too extreme. Try a different distribution."), call. = FALSE)
    beta <- temp$parameters[[1]]
    theta <- temp$parameters[[2]]
    threshold <- temp$parameters[[3]]
  } else if(distribution == "3ParameterWeibull") {
    temp <- try(MASS::fitdistr(data, function(x, shape, scale, thres)
      dweibull(x-thres, shape, scale), list(shape = 0.1, scale = 1, thres = 0)))
    if (jaspBase::isTryError(temp))
      stop(gettext("Parameter estimation failed. Values might be too extreme. Try a different distribution."), call. = FALSE)
    beta <- temp$estimate[1]
    theta <- temp$estimate[2]
    threshold <- temp$estimate[3]
  } else if (distribution == "exponential") {
    fit_Weibull <- try(fitdistrplus::fitdist(data, "weibull", method = "mle", fix.arg = list("shape" = 1),
                                             control = list(maxit = 500, abstol = .Machine$double.eps, reltol = .Machine$double.eps)))
    if (jaspBase::isTryError(fit_Weibull))
      stop(gettext("Parameter estimation failed. Values might be too extreme. Try a different distribution."), call. = FALSE)
    beta <- 1
    theta <- fit_Weibull$estimate[[1]]
  }
  list <- list(beta = beta,
               theta = theta)
  if(distribution == '3ParameterWeibull' | distribution == "3ParameterLognormal")
    list['threshold'] <- threshold
  return(list)
}

.getRuleListSubgroupCharts <- function(options) {
  ruleSet <- options[["testSet"]]
  if (ruleSet == "jaspDefault") {
    ruleList <- list("rule1" = list("enabled" = TRUE),
                     "rule2" = list("enabled" = TRUE, "k" = 7),
                     "rule3" = list("enabled" = TRUE, "k" = 7),
                     "rule4" = NULL,
                     "rule5" = NULL,
                     "rule6" = NULL,
                     "rule7" = NULL,
                     "rule8" = NULL,
                     "rule9" = NULL
    )
  } else if (ruleSet == "nelsonLaws") {
    ruleList <- list("rule1" = list("enabled" = TRUE),
                     "rule2" = list("enabled" = TRUE, "k" = 9),
                     "rule3" = list("enabled" = TRUE, "k" = 6),
                     "rule4" = list("enabled" = TRUE, "k" = 14),
                     "rule5" = list("enabled" = TRUE, "k" = 2),
                     "rule6" = list("enabled" = TRUE, "k" = 4),
                     "rule7" = list("enabled" = TRUE, "k" = 15),
                     "rule8" = list("enabled" = TRUE, "k" = 8),
                     "rule9" = NULL
    )

  } else if (ruleSet == "westernElectric") {
    ruleList <- list("rule1" = list("enabled" = TRUE),
                     "rule2" = list("enabled" = TRUE, "k" = 8),
                     "rule3" = NULL,
                     "rule4" = NULL,
                     "rule5" = list("enabled" = TRUE, "k" = 2),
                     "rule6" = list("enabled" = TRUE, "k" = 4),
                     "rule7" = NULL,
                     "rule8" = NULL,
                     "rule9" = NULL
    )
  } else if (ruleSet == "custom") {
    ruleList <- list("rule1" = list("enabled" = options[["rule1"]]),
                     "rule2" = list("enabled" = options[["rule2"]], "k" = options[["rule2Value"]]),
                     "rule3" = list("enabled" = options[["rule3"]], "k" = options[["rule3Value"]]),
                     "rule4" = list("enabled" = options[["rule4"]], "k" = options[["rule4Value"]]),
                     "rule5" = list("enabled" = options[["rule5"]], "k" = options[["rule5Value"]]),
                     "rule6" = list("enabled" = options[["rule6"]], "k" = options[["rule6Value"]]),
                     "rule7" = list("enabled" = options[["rule7"]], "k" = options[["rule7Value"]]),
                     "rule8" = list("enabled" = options[["rule8"]], "k" = options[["rule8Value"]]),
                     "rule9" = NULL
    )
  }
  return(ruleList)
}


