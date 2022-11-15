renv::install(".")
# renv::install("jasp-stats/jaspTools")
# renv::install(c("diffviewer", "shiny"))
library(jaspTools)
setPkgOption("module.dirs", ".")
setPkgOption("reinstall.modules", FALSE)

options <- analysisOptions("~/GitHub/jasp/deleteable/qualitycontrol/doeRSM2.jasp")

# options$designType <- "star"
for (i in seq_along(options$factors)) {
  options$factors[[i]]$low  <- options$factors[[i]]$centre   <- 900
  options$factors[[i]]$high <- options$factors[[1]]$distance <- 1500
}
options$numberOfFactors <- 3
options$factors[[3]] <- options$factors[[2]]

# options$factors[[1]]$low <- centre
# options$factors[[1]]$distance <- .5
#
# options$factors[[1]]$value

# debugonce(jaspProcessControl:::doeResponseSurfaceMethodology)

debugonce(jaspQualityControl:::.cubeDesign)
result <- runAnalysis(data = "debug.csv", options = options)

# testAll()

rsm::cube(basis = 3, n0 = 3,# coding = list(x1 ~ (x11 - 1200)/300, x2 ~ (x21 - 1200)/300, x3 ~ (x31 - 1200)/300),
          randomize = FALSE, inscribed = FALSE)


# identical to minitab with 3 continuous variables
rsm::ccd(basis = 3, n0 = 3, alpha = "rotatable")

rsm::ccd(basis = 4, n0 = 30, alpha = sqrt(2))

A <- gl(3, 1)
B <- gl(2, 1)

debugonce(rsm::ccd)
rsm::ccd(basis = 2, n0 = 3, alpha = sqrt(2), randomize = FALSE)


rsm::cube(basis = 2, n0 = 3, randomize = FALSE)


FOdes <- rsm::cube (3, n0 = 4, coding = list (
  x1 ~ (Temp - 150)/10, x2 ~ (Pres - 50)/5, x3 ~ Feedrate - 4))

rsm::djoin(FOdes, rsm::dupe(FOdes))

rsm::ccd(basis = 3, n0 = c(10, 0), alpha = sqrt(2), randomize = FALSE)

rsm::cube(2, n0 = 8, randomize = FALSE)
rsm::star(basis = 2, n0 = 0, alpha = sqrt(2))

rsm::ccd(basis = 2, n0 = c(8, 0), alpha = sqrt(2), randomize = FALSE)

rsm::ccd(basis = 2, n0 = c(5, 0), alpha = "rotatable")

rsm::ccd(basis = 3, n0 = c(10, 0), alpha = "rotatable")

# This function replicates minitab output using rsm
doRsm <- function(noContinuous = 2L, noCategorical = 0L, noLevels = integer(), centerPointsCube, centerPointsAxial = 0, alpha = "rotatable",
                  categoricalNames = letters[seq_len(noCategorical)]) {

  totalLevels <- prod(noLevels)
  des <- rsm::ccd(
    basis = noContinuous,
    n0 = c(centerPointsCube, centerPointsAxial) / totalLevels,
    alpha = "rotatable"
  )

  categoricalValues <- lapply(noLevels, seq_len)
  names(categoricalValues) <- categoricalNames
  categoricalCombinations <- expand.grid(categoricalValues)

  unrowname <- function(x) {
    rownames(x) <- NULL
    x
  }

  desCombined <- cbind(des, unrowname(categoricalCombinations[rep(1, nrow(des)), ]))

  for (i in seq_len(nrow(categoricalCombinations) - 1) + 1) {
    desCombined <- rbind(
      desCombined,
      cbind(des, unrowname(categoricalCombinations[rep(i, nrow(des)), ]))
    )
  }

  return(desCombined)

}

doRsm(2, 2, c(2, 3), centerPointsCube = 30)
