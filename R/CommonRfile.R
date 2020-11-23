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

.ProbabilityPlotNoId <- function(dataset, options, variable, dis){
  title <- variable
  ppPlot <- createJaspPlot(width = 600, aspectRatio = 1, title = title)
  ppPlot$dependOn(optionContainsValue = list(variables = variable))

  #Arrange data
  x <- dataset[[.v(variable)]]
  x <- x[order(x)]
  n <- length(x)
  i <- rank(x)

  #Method for rank
  Rank_funs <- matrix(
    list(
      function(x) {
      x <- x[order(x)]
      n <- length(x)
      i <- rank(x)
      p <- (i - 0.3) / (n + 0.4)
      return(p)},
      function(x){
        x <- x[order(x)]
        n <- length(x)
        i <- rank(x)
        p <- (i) / (n + 1)
        return(p)},
      function(x,i,n) {
        x <- x[order(x)]
        n <- length(x)
        i <- rank(x)
        p <- (i - 0.5) / (n)
        return(p)},
      function(x) {
        x <- x[order(x)]
        n <- length(x)
        i <- rank(x)
        p <- (i) / (n)
        return(p)}
    ),
    ncol = 1,
    dimnames = list(
      c("median", "mean", "KMmodif", 'KM'),
      c("p")
    ),
    byrow = TRUE
  )
  rankByUser <- options$rank
  p <- Rank_funs[[rankByUser, 'p']](x)

  #Functions for computing y
  y_funs <- matrix(
    list(
      qnorm,
      qnorm,
      function(p){log(-log(1 - p))}
    ),
    ncol = 1,
    dimnames = list(
      c('Normal', 'Lognormal', 'Weibull'),
      c('y')
    ),
    byrow = TRUE
  )
  DisByUser <- options$Nulldis
  y <- y_funs[[DisByUser, 'y']](p)
  data1 <- data.frame(x = x, y = y)

  #Quantities
  pSeq <- seq(0.001, 0.999, 0.001)
  ticks <- c(0.1, 1, 5, seq(10, 90, 10), 95, 99, 99.9)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(x), max(x)))
  xlimits <- range(xBreaks)

  #Computing according to the distribution
  if (dis == 'Normal'){
    lpdf <- quote(-log(sigma) - 0.5 / sigma ^ 2 * (x - mu) ^ 2)
    matrix <- mle.tools::observed.varcov(logdensity = lpdf, X = x, parms = c("mu", "sigma"), mle = c(mean(x), sd(x)))
    varMu <- matrix$varcov[1, 1]
    varSigma <- matrix$varcov[2,2]
    covarMuSigma <- matrix$varcov[1, 2]
    zp <- qnorm(p = pSeq)
    zalpha <- qnorm(0.975)
    percentileEstimate <- mean(x) + zp * sd(x)
    varPercentile <- varMu + zp^2 * varSigma + 2*zp * covarMuSigma
    percentileLower <- percentileEstimate - zalpha * sqrt(varPercentile)
    percentileUpper <- percentileEstimate + zalpha * sqrt(varPercentile)
    breaksY <- qnorm(ticks / 100)
  }
  else if (dis == 'Lognormal'){
    fit <- fitdistrplus::fitdist(x, 'lnorm')
    meanlog <- as.numeric(fit$estimate[1])
    sdlog <- as.numeric(fit$estimate[2])
    lpdf <- quote(log(1/(sqrt(2*pi)*x*sdlog) * exp(-(log(x)- meanlog)^2/(2*sdlog^2))))
    matrix <- mle.tools::observed.varcov(logdensity = lpdf, X = x, parms = c("meanlog", "sdlog"), mle = fit$estimate)
    varmeanlog <- matrix$varcov[1, 1]
    varsdlog <- matrix$varcov[2,2]
    covarSS <- matrix$varcov[1, 2]
    zp <- qnorm(p = pSeq)
    zalpha <- qnorm(0.975)
    percentileEstimate <- exp(meanlog + zp*sdlog)
    varPercentile <- percentileEstimate^2*( varmeanlog+zp^2*varsdlog + 2*zp * covarSS)
    percentileLower <- exp( log(percentileEstimate) - zalpha * (sqrt(varPercentile)/percentileEstimate))
    percentileUpper <- exp(log(percentileEstimate) + zalpha * (sqrt(varPercentile)/percentileEstimate))
    breaksY <- qnorm(ticks / 100)
  }
  else if (dis == 'Weibull'){
    fit <- fitdistrplus::fitdist(x, 'weibull')
    shape <- as.numeric(fit$estimate[1])
    scale <- as.numeric(fit$estimate[2])
    lpdf <- quote(log(shape) - shape * log(scale) + shape * log(x) - (x / scale)^ shape )
    matrix <- mle.tools::observed.varcov(logdensity = lpdf, X = x, parms = c("shape", "scale"), mle = fit$estimate)
    varShape <- matrix$varcov[1,1]
    varScale <- matrix$varcov[2,2]
    covarSS <- matrix$varcov[1,2]
    zp <- log(-1*log(1-pSeq))
    zalpha <- log(-1*log(1-0.975))
    percentileEstimate <- scale * (- log(1 - pSeq))^(1/shape)
    varPercentile <- (percentileEstimate^2 / scale^2) * varScale + (percentileEstimate^2/shape^4)*zp^2*varShape - 2*((zp*percentileEstimate^2) / (scale * shape^2))*covarSS
    percentileLower <- exp( log(percentileEstimate) - zalpha * (sqrt(varPercentile)/percentileEstimate))
    percentileUpper <- exp(log(percentileEstimate) + zalpha * (sqrt(varPercentile)/percentileEstimate))
    breaksY <- log(-1*log(1-(ticks / 100)))
  }


  p <- ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileEstimate)) +
      ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileLower)) +
      ggplot2::geom_line(ggplot2::aes(y = zp, x = percentileUpper)) +
      ggplot2::geom_point(ggplot2::aes(x= data1$x, y= data1$y), size = 2) +
      ggplot2::scale_x_continuous('candle_width',limits = xlimits, breaks = xBreaks) +
      ggplot2::scale_y_continuous('Percent', labels = ticks, breaks = breaksY) +
      jaspGraphs::themeJaspRaw() +
      jaspGraphs::geom_rangeframe()

  ppPlot$plotObject <-  p
  return(ppPlot)
  }

.PPtable <- function( dataset, options, variable, dis){
  pptable <- createJaspTable(title = gettextf("Descriptives for %s", variable ))
  pptable$dependOn(optionContainsValue=list(variables=variable))
  x = dataset[[.v(variable)]]

  if (dis == 'Normal') {
    pptable$addColumnInfo(name = "mean",   title = "Mean",         type = "integer", combine = FALSE)
    pptable$addColumnInfo(name = "sd",     title = "StDev",        type = "integer")
    pptable$addColumnInfo(name = "N",      title = "N",            type = "integer")
    pptable$addColumnInfo(name = "AD",     title = "AD",           type = "integer")
    pptable$addColumnInfo(name = "P_value",title = "P-value",      type = "integer")
    pptable$addColumnInfo(name = "Normality",title = "Reject the null-distribution?",      type = "string")

    pptable$addRows(list(
      mean = round(mean(x),3),
      sd         = round(sd(x),3),
      N          = length(x),
      AD         = round(goftest::ad.test(x = x, "norm", mean=mean(x), sd= sd(x))$statistic,3),
      P_value    = round(goftest::ad.test(x = x, "norm", mean=mean(x), sd= sd(x))$p.value,3),
      Normality  = ifelse(goftest::ad.test(x = x, "norm", mean=mean(x), sd= sd(x))$p.value >= 0.05, 'No', 'Yes')
    ))
  }
  else if (dis == 'Lognormal') {
    fit= fitdistrplus::fitdist(x, 'lnorm')
    pptable$addColumnInfo(name = "Location",   title = "Location",         type = "integer", combine = FALSE)
    pptable$addColumnInfo(name = "Scale",     title = "Scale",        type = "integer")
    pptable$addColumnInfo(name = "N",      title = "N",            type = "integer")
    pptable$addColumnInfo(name = "AD",     title = "AD",           type = "integer")
    pptable$addColumnInfo(name = "P_value",title = "P-value",      type = "integer")
    pptable$addColumnInfo(name = "Normality",title = "Reject the null-distribution?",      type = "string")

    pptable$addRows(list(
      Location   = round(as.numeric(fit$estimate[1]), 3),
      Scale      = round(as.numeric(fit$estimate[2]), 3),
      N          = length(x),
      AD         = round(goftest::ad.test(x = x, "plnorm", meanlog= as.numeric(fit$estimate[1]), sdlog=as.numeric(fit$estimate[2]) )$statistic,3),
      P_value    = round(goftest::ad.test(x = x, "plnorm", meanlog= as.numeric(fit$estimate[1]), sdlog=as.numeric(fit$estimate[2]) )$p.value,3),
      Normality  = ifelse(round(goftest::ad.test(x = x, "plnorm", meanlog= as.numeric(fit$estimate[1]), sdlog=as.numeric(fit$estimate[2]))$p.value,3) >= 0.05, 'No', 'Yes')
      ))
  }
  else if (dis == 'Weibull') {
    fit= fitdistrplus::fitdist(x, 'weibull')
    pptable$addColumnInfo(name = "Shape",   title = "Shape",         type = "integer", combine = FALSE)
    pptable$addColumnInfo(name = "Scale",   title = "Scale",        type = "integer")
    pptable$addColumnInfo(name = "N",      title = "N",            type = "integer")
    pptable$addColumnInfo(name = "AD",     title = "AD",           type = "integer")
    pptable$addColumnInfo(name = "P_value",title = "P-value",      type = "integer")
    pptable$addColumnInfo(name = "Normality",title = "Reject the null-distribution?",      type = "string")

    pptable$addRows(list(
      Shape      = round(as.numeric(fit$estimate[1]),3),
      Scale      = round(as.numeric(fit$estimate[2]),3),
      N          = length(x),
      AD         = round(goftest::ad.test(x = x, "pweibull", shape = as.numeric(fit$estimate[1]), scale= as.numeric(fit$estimate[2]))$statistic,3),
      P_value    = round(goftest::ad.test(x = x, "pweibull", shape = as.numeric(fit$estimate[1]), scale= as.numeric(fit$estimate[2]))$p.value,3),
      Normality  = ifelse(round(goftest::ad.test(x = x, "pweibull", shape = as.numeric(fit$estimate[1]), scale= as.numeric(fit$estimate[2]))$p.value,3) >= 0.05, 'No', 'Yes')
      ))

  }
  return(pptable)
}
