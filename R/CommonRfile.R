# Common R file
.XbarchartNoId <- function(dataset, options) {


  if(!is.null(options$variables)){
    ready <- (length(options$variables) > 1)
    if (!ready)
      return()
  }

    data1 <- dataset
    means <- rowMeans(data1)
    subgroups <- 1:length(means)
    data2 <- data.frame(subgroups = subgroups, means = means)
    sixsigma <- qcc::qcc(data1, type ='xbar', plot=FALSE)
    center <- sixsigma$center
    sd1 <- sixsigma$std.dev
    UCL <- max(sixsigma$limits)
    LCL <- min(sixsigma$limits)
    dfLabel <- data.frame(
      x = length(subgroups) + 1.2,
      y = c(center, UCL, LCL),
      l = c(
        gettextf("Mean = %g", round(center, 3)),
        gettextf("UCL = %g",   round(UCL, 3)),
        gettextf("LCL = %g",   round(LCL, 3))
      )
    )
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL))
    yLimits <- range(yBreaks)
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(subgroups))

    p <- ggplot2::ggplot(data2, ggplot2::aes(x = subgroups, y = means)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 2, col = ifelse(data2$means > UCL | data2$means < LCL, 2, 1)) +
      ggplot2::geom_hline(yintercept =  center, color = 'black') +
      ggplot2::geom_hline(yintercept = c(UCL, LCL), color = "red") +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
      ggplot2::scale_y_continuous(name = "Subgroup Mean" ,limits = yLimits, breaks = yBreaks) +
      ggplot2::scale_x_continuous(name = 'Subgroup', breaks = xBreaks, limits = c(1, length(subgroups) + 1.5)) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()

    return(p)
  }

  .RchartNoId <- function(dataset, options) {

    if(!is.null(options$variables)){
    ready <- (length(options$variables) > 1)
      if (!ready)
        return()
    }

    #Arrange data and compute
    data1 <- dataset
    range <- apply(data1, 1, function(x) max(x) - min(x))
    subgroups <- 1:length(range)
    data2 <- data.frame(subgroups = subgroups, range = range)
    sixsigma <- qcc::qcc(data1, type= 'R', plot = FALSE)
    center <- sixsigma$center
    UCL <- max(sixsigma$limits)
    LCL <- min(sixsigma$limits)
    dfLabel <- data.frame(
      x = length(subgroups) + 1.2,
      y = c(center, UCL, LCL),
      l = c(
        gettextf("Range = %g", round(center, 3)),
        gettextf("UCL = %g",   round(UCL, 3)),
        gettextf("LCL = %g",   round(LCL, 3))
      )
    )
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(LCL, UCL))
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(subgroups))
    yLimits <- range(yBreaks)

    p <- ggplot2::ggplot(data2, ggplot2::aes(x = subgroups, y = range)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 2, col = ifelse(data2$range > UCL | data2$range < LCL, 2, 1)) +
      ggplot2::geom_hline(yintercept =  center, color = 'black') +
      ggplot2::geom_hline(yintercept = c(UCL,LCL), color = "red") +
      ggplot2::geom_label(data = dfLabel, mapping = ggplot2::aes(x = x, y = y, label = l),inherit.aes = FALSE) +
      ggplot2::scale_y_continuous(name = "Subgroup Range" ,limits = yLimits, breaks = yBreaks) +
      ggplot2::scale_x_continuous(name= "Subgroup" ,breaks = xBreaks, limits = c(1,length(subgroups) + 1.5)) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()

    return(p)
  }

.ProbabilityPlotNoId <- function(dataset, options, variable){
  title <- variable
  ppPlot <- createJaspPlot(width=600, aspectRatio=1, title=title)
  ppPlot$dependOn(optionContainsValue=list(variables=variable))

  #Arrange data
  x = dataset[[.v(variable)]]
  x <- x[order(x)]
  n <- length(x)
  i <- rank(x)
  p <- (i - 0.3) / (n + 0.4) # Median method
  y <- qnorm(p = p) # Normal distribution

  #Find out variance and covariance of mle's of mu and sigma
  lpdf <- quote(-log(sigma) - 0.5 / sigma ^ 2 * (x - mu) ^ 2)
  matrix <- mle.tools::observed.varcov(logdensity = lpdf, X = x, parms = c("mu", "sigma"), mle = c(mean(x), sd(x)))
  varMu <- matrix$varcov[1, 1]
  varSigma <- matrix$varcov[2,2]
  covarMuSigma <- matrix$varcov[1, 2]

  # Quantities
  pSeq <- seq(0.001, 0.999, 0.001)
  zp <- qnorm(p = pSeq)
  zalpha <- qnorm(0.975)
  percentileEstimate <- mean(x) + zp * sd(x)
  varPercentile <- varMu + zp^2 * varSigma + 2*zp * covarMuSigma
  percentileLower <- percentileEstimate - zalpha * sqrt(varPercentile)
  percentileUpper <- percentileEstimate + zalpha * sqrt(varPercentile)
  data1= data.frame(x = x, y = y)

  #Graphics
  ticks <- c(0.1, 1, 5, seq(10, 90, 10), 95, 99, 99.9)
  xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(min(x) - 2 , max(x) + 2))
  xlimits <- range(xBreaks)

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(y = qnorm(pSeq), x= percentileEstimate)) +
    ggplot2::geom_line(ggplot2::aes(y = qnorm(pSeq), x= percentileLower)) +
    ggplot2::geom_line(ggplot2::aes(y = qnorm(pSeq), x= percentileUpper)) +
    ggplot2::geom_point(ggplot2::aes(x= data1$x, y= data1$y), size = 2) +
    ggplot2::scale_x_continuous('candle_width',limits = xlimits, breaks = xBreaks) +
    ggplot2::scale_y_continuous('Percent', labels = ticks, breaks = qnorm(ticks / 100)) +
    JASPgraphs::themeJaspRaw() +
    JASPgraphs::geom_rangeframe()

  ppPlot$plotObject <-  p
  return(ppPlot)
}

.PPtable <- function( dataset, options, variable){

  pptable <- createJaspTable(title = gettextf("Descriptives and normality for %s", variable ))
  pptable$dependOn(optionContainsValue=list(variables=variable))
  dat = dataset[[.v(variable)]]

  pptable$addColumnInfo(name = "mean",   title = "Mean",         type = "integer", combine = FALSE)
  pptable$addColumnInfo(name = "sd",     title = "StDev",        type = "integer")
  pptable$addColumnInfo(name = "N",      title = "N",            type = "integer")
  pptable$addColumnInfo(name = "AD",     title = "AD",           type = "integer")
  pptable$addColumnInfo(name = "P_value",title = "P-value",      type = "integer")

  pptable$addRows(list(
    mean = round(mean(dat),3),
    sd         = round(sd(dat),3),
    N          = length(dat),
    AD         = round(nortest::ad.test(x = dat)$statistic,3),
    P_value    = round(nortest::ad.test(x = dat)$p.value,3)
  ))


  return(pptable)
}
