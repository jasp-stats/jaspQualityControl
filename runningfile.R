renv::install(".")
# renv::install("jasp-stats/jaspTools")
# renv::install(c("diffviewer", "shiny"))
library(jaspTools)
setPkgOption("module.dirs", ".")
setPkgOption("reinstall.modules", FALSE)

# options <- analysisOptions("~/GitHub/jasp/deleteable/qualitycontrol/doeRSM2.jasp")
# options <- analysisOptions("~/GitHub/jasp/deleteable/qualitycontrol/doeRsmTests.jasp")
options <- analysisOptions("~/../Downloads/doe_debug.jasp")

# options$designType <- "star"
# for (i in seq_along(options$factors)) {
#   options$factors[[i]]$low  <- options$factors[[i]]$centre   <- 900
#   options$factors[[i]]$high <- options$factors[[1]]$distance <- 1500
# }
# options$numberOfFactors <- 3
# options$factors[[3]] <- options$factors[[2]]

# options$factors[[1]]$low <- centre
# options$factors[[1]]$distance <- .5
#
# options$factors[[1]]$value

options$buildDesignInv <- TRUE

debugonce(jaspQualityControl:::doeResponseSurfaceMethodology)

# debugonce(jaspQualityControl:::.doeRsmGenerateDesign)

# debugonce(jaspQualityControl:::.cubeDesign)
result <- runAnalysis(data = "debug.csv", options = options)

# testAll()

# rsm::cube(basis = 3, n0 = 3,# coding = list(x1 ~ (x11 - 1200)/300, x2 ~ (x21 - 1200)/300, x3 ~ (x31 - 1200)/300),
#           randomize = FALSE, inscribed = FALSE)
#
#
# # identical to minitab with 3 continuous variables
# rsm::ccd(basis = 3, n0 = 3, alpha = "rotatable")
#
# rsm::ccd(basis = 4, n0 = 30, alpha = sqrt(2))
#
# A <- gl(3, 1)
# B <- gl(2, 1)
#
# debugonce(rsm::ccd)
# rsm::ccd(basis = 2, n0 = 3, alpha = sqrt(2), randomize = FALSE)
#
#
# rsm::cube(basis = 2, n0 = 3, randomize = FALSE)
# rsm::star()
#
# FOdes <- rsm::cube (3, n0 = 4, coding = list (
#   x1 ~ (Temp - 150)/10, x2 ~ (Pres - 50)/5, x3 ~ Feedrate - 4))
#
# rsm::djoin(FOdes, rsm::dupe(FOdes))
#
# rsm::ccd(basis = 3, n0 = c(10, 0), alpha = sqrt(2), randomize = FALSE)
#
# rsm::cube(2, n0 = 8, randomize = FALSE)
# rsm::star(basis = 2, n0 = 0, alpha = sqrt(2))
#
# rsm::ccd(basis = 2, n0 = c(8, 0), alpha = sqrt(2), randomize = FALSE)
#
# rsm::ccd(basis = 2, n0 = c(5, 0), alpha = "rotatable")
#
# rsm::ccd(basis = 3, n0 = c(10, 0), alpha = "rotatable")
#
# # This function replicates minitab output using rsm
# # perhaps it should call cube & start directly?
# # also order should match minitab exactly
doCCD <- function(noContinuous, centerPointsCube, centerPointsAxial = 0, alpha = "rotatable",
                  categoricalVariables = list()) {

  # totalLevels <- prod(noLevels)
  design <- rsm::ccd(
    basis = noContinuous,
    n0 = c(centerPointsCube, centerPointsAxial),# / totalLevels,
    alpha = "rotatable",
    oneblock = TRUE,
    randomize = FALSE
  )

  return(replicateDesignForCategoricalVariables(design, categoricalVariables))

}

doBBD <- function(noContinuous, centerPoints, categoricalVariables = list(), randomize = FALSE) {

  # totalLevels <- prod(lengths(categoricalVariables))
  design <- rsm::bbd(
    k         = noContinuous,
    n0        = centerPoints,# / totalLevels,
    block     = FALSE,#block
    randomize = randomize
  )

  return(replicateDesignForCategoricalVariables(design, categoricalVariables))

}

replicateDesignForCategoricalVariables <- function(design, categoricalVariables) {

  if (length(categoricalVariables) <= 0L)
    return(design)

  categoricalCombinations <- expand.grid(categoricalVariables)

  unrowname <- function(x) {
    rownames(x) <- NULL
    x
  }

  nr <- nrow(design)
  designCombined <- cbind(design, unrowname(categoricalCombinations[rep(1L, nr), , drop = FALSE]))

  for (i in 2L:nrow(categoricalCombinations)) {
    toAdd <- cbind(design, unrowname(categoricalCombinations[rep(i, nrow(design)), , drop = FALSE]))
    toAdd[["run.order"]] <- toAdd[["run.order"]] + nr * (i - 1L)
    toAdd[["std.order"]] <- toAdd[["std.order"]] + nr * (i - 1L)

    designCombined <- rbind(designCombined, toAdd)
  }

  return(designCombined)

}

# TODO: continue here
summarizeDesign <- function() {
  # factors
  # baseRuns
  # totalRuns
  # alpha
  # baseBlocks
  # totalBlocks
  #
  # pointTypes
  # cubePoints
  # centerPointsInCube
  # axialPoints
  # centerPointsInAxial
}
#
#
# # verified against minitab
doCCD(2, 3, 3)

# doCCD(3, centerPointsCube = 6)
# doCCD(3, centerPointsCube = 6, categoricalVariables = list(A = c("a", "b")))
# doCCD(3, centerPointsCube = 6, categoricalVariables = list(A = c("a", "b"), B = c("a", "b", "c")))
#
# doBBD(4, centerPoints = 3)
# doBBD(4, centerPoints = 3, categoricalVariables = list(A = c("a", "b")))
# doBBD(4, centerPoints = 3, categoricalVariables = list(A = c("a", "b"), B = c("a", "b", "c")))
#
