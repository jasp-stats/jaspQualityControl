# The tests for special causes are numbered as in Nelson (1984), AIAG and Minitab (jasp-issues #4671):
# 1 beyond limit, 2 shift (k on one side), 3 trend, 4 oscillation, 5 k of k+1 beyond 2 sigma (same side),
# 6 k of k+1 beyond 1 sigma (same side), 7 k within 1 sigma, 8 k beyond 1 sigma (either side)

nelsonViolations <- function(x, ruleList) {
  jaspQualityControl:::.nelsonLaws(plotStatistics = x, center = 0, UCL = 3, LCL = -3, nSigmasControlLimits = 3,
                                   ruleList = ruleList)$violationList
}

test_that("NL1.1 Each test number detects its own pattern", {
  expect_equal(nelsonViolations(c(0, 0, 3.5, 0), list(rule1 = list(enabled = TRUE))), list(test1 = 3))
  expect_equal(nelsonViolations(c(1, 1, 1, -1), list(rule2 = list(enabled = TRUE, k = 3))), list(test2 = 3))
  expect_equal(nelsonViolations(c(0, 1, 2, 3, 0), list(rule3 = list(enabled = TRUE, k = 3))), list(test3 = 4))
  expect_equal(nelsonViolations(c(0, 1, 0, 1, 0, 1), list(rule4 = list(enabled = TRUE, k = 4))), list(test4 = c(5, 6)))
  expect_equal(nelsonViolations(c(0, 2.5, 0, 2.5, 0), list(rule5 = list(enabled = TRUE, k = 2))), list(test5 = 4))
  expect_equal(nelsonViolations(c(1.5, 0, 1.5, 1.5, 1.5, 0), list(rule6 = list(enabled = TRUE, k = 4))), list(test6 = 5))
  expect_equal(nelsonViolations(rep(0.1, 5), list(rule7 = list(enabled = TRUE, k = 3))), list(test7 = c(4, 5)))
  expect_equal(nelsonViolations(c(1.5, -1.5, 1.5, 0), list(rule8 = list(enabled = TRUE, k = 3))), list(test8 = 3))
})

test_that("NL1.2 Test sets use Nelson's numbering", {
  # named vector with the k of each test that is applied (NA for test 1, which has no k)
  appliedTests <- function(ruleList) {
    applied <- Filter(Negate(is.null), ruleList)
    vapply(applied, function(rule) if (is.null(rule[["k"]])) NA_real_ else as.numeric(rule[["k"]]), numeric(1))
  }
  individuals <- function(testSet, type = "I") jaspQualityControl:::.getRuleListIndividualCharts(list(testSet = testSet), type = type)
  subgroups   <- function(testSet, type = "xBar") jaspQualityControl:::.getRuleListSubgroupCharts(list(testSet = testSet), type = type)

  expect_equal(appliedTests(individuals("nelsonLaws")),
               c(rule1 = NA, rule2 = 9, rule3 = 6, rule4 = 14, rule5 = 2, rule6 = 4, rule7 = 15, rule8 = 8))
  expect_equal(appliedTests(subgroups("nelsonLaws")),
               c(rule1 = NA, rule2 = 9, rule3 = 6, rule4 = 14, rule5 = 2, rule6 = 4, rule7 = 15, rule8 = 8))
  expect_equal(appliedTests(individuals("westernElectric")), c(rule1 = NA, rule2 = 8, rule5 = 2, rule6 = 4))
  expect_equal(appliedTests(subgroups("westernElectric")), c(rule1 = NA, rule2 = 8, rule5 = 2, rule6 = 4))
  expect_equal(appliedTests(individuals("jaspDefault")), c(rule1 = NA, rule2 = 7, rule3 = 7, rule5 = 2, rule7 = 15, rule8 = 8))
  expect_equal(appliedTests(subgroups("jaspDefault")), c(rule1 = NA, rule2 = 7, rule3 = 7))
  # only the tests that do not need zones apply to the moving range chart and to the s and R charts
  expect_equal(names(appliedTests(individuals("nelsonLaws", type = "MR"))), c("rule1", "rule2", "rule3", "rule4"))
  expect_equal(names(appliedTests(subgroups("nelsonLaws", type = "R"))), c("rule1", "rule2", "rule3", "rule4"))
  expect_equal(names(appliedTests(subgroups("nelsonLaws", type = "s"))), c("rule1", "rule2", "rule3", "rule4"))
})

test_that("NL1.3 The test table names the tests by Nelson's number", {
  options <- analysisOptions("variablesChartsIndividuals")
  options$measurement <- "V1"
  options$xmrChartMovingRangeLength <- 2
  options$testSet <- "custom"
  options$rule1 <- TRUE
  options$rule2 <- TRUE
  options$rule2Value <- 7
  options$rule3 <- TRUE
  options$rule3Value <- 7
  options$rule4 <- TRUE
  options$rule4Value <- 14
  options$rule5 <- TRUE
  options$rule5Value <- 2
  options$rule6 <- TRUE
  options$rule6Value <- 4
  options$rule7 <- TRUE
  options$rule7Value <- 15
  options$rule8 <- TRUE
  options$rule8Value <- 8
  results <- runAnalysis("variablesChartsIndividuals", "datasets/controlChartRules/violatingAllindividualRules.csv", options)
  fields <- results[["results"]][["Ichart"]][["collection"]][["Ichart_tableI"]][["schema"]][["fields"]]
  titles <- setNames(vapply(fields, function(field) field[["title"]], character(1)),
                     vapply(fields, function(field) field[["name"]], character(1)))
  expect_equal(titles, c(test1 = "Test 1: Beyond limit", test2 = "Test 2: Shift", test3 = "Test 3: Trend",
                         test4 = "Test 4: Oscillation", test5 = "Test 5: Beyond 2 sigma, same side",
                         test6 = "Test 6: Beyond 1 sigma, same side", test7 = "Test 7: Reducing variation",
                         test8 = "Test 8: Bimodal distribution"))
})

# Saved files: Upgrades.qml renames the options of the old numbering (old 4, 5, 6, 7, 8 are new 5, 7, 8, 6, 4) ####
# JASP applies the ChangeRename items one after the other (it fails if the new name exists, and skips an old name that is missing)

readNelsonUpgrade <- function(functionName, fromVersion = "0.98.3") {
  lines <- readLines(system.file("Upgrades.qml", package = "jaspQualityControl"))
  starts <- grep("^\\s*Upgrade\\s*$", lines)
  ends <- c(starts[-1] - 1, length(lines))
  for (i in seq_along(starts)) {
    block <- lines[starts[i]:ends[i]]
    isFunction <- any(grepl(sprintf("functionName:\\s*\"%s\"", functionName), block))
    isVersion <- any(grepl(sprintf("fromVersion:\\s*\"%s\"", fromVersion), block))
    if (isFunction && isVersion) {
      matches <- regmatches(block, regexec("ChangeRename\\s*\\{\\s*from:\\s*\"([^\"]+)\"\\s*;\\s*to:\\s*\"([^\"]+)\"", block))
      matches <- Filter(function(match) length(match) == 3, matches)
      return(lapply(matches, function(match) list(from = match[2], to = match[3])))
    }
  }
  stop("no upgrade found for ", functionName)
}

applyRenames <- function(options, renames) {
  for (rename in renames) {
    stopifnot(is.null(options[[rename$to]]))
    if (is.null(options[[rename$from]]))
      next
    options[[rename$to]] <- options[[rename$from]]
    options[[rename$from]] <- NULL
  }
  options
}

test_that("NL2.1 Saved options of the old numbering are renamed to the new numbering", {
  oldNames <- c("rule1", paste0(rep(paste0("rule", 2:8), each = 2), c("", "Value")))
  oldOptions <- setNames(as.list(paste("old", oldNames)), oldNames)
  newOfOld <- c(rule4 = "rule5", rule5 = "rule7", rule6 = "rule8", rule7 = "rule6", rule8 = "rule4")
  for (functionName in c("variablesChartsIndividuals", "variablesChartsSubgroups", "processCapabilityStudies",
                         "msaGaugeRR", "msaGaugeRRnonrep")) {
    upgraded <- applyRenames(oldOptions, readNelsonUpgrade(functionName))
    expect_setequal(names(upgraded), oldNames)
    for (old in names(newOfOld)) {
      expect_equal(upgraded[[newOfOld[[old]]]], paste("old", old), info = functionName)
      expect_equal(upgraded[[paste0(newOfOld[[old]], "Value")]], paste("old", paste0(old, "Value")), info = functionName)
    }
    for (unchanged in c("rule1", "rule2", "rule2Value", "rule3", "rule3Value"))
      expect_equal(upgraded[[unchanged]], paste("old", unchanged), info = functionName)
    # the upgraded options are exactly the rule options of the form
    expect_setequal(names(upgraded), grep("^rule", names(analysisOptions(functionName)), value = TRUE))
  }
})

test_that("NL2.2 Saved rare event charts options of the old numbering are renamed", {
  oldNames <- c("rule1", "rule2", "rule2Value", "rule3", "rule3Value", "rule8", "rule8Value", "rule9", "rule9Value")
  oldOptions <- setNames(as.list(paste("old", oldNames)), oldNames)
  upgraded <- applyRenames(oldOptions, readNelsonUpgrade("rareEventCharts"))
  expect_equal(upgraded[["rule4"]], "old rule8")
  expect_equal(upgraded[["rule4Value"]], "old rule8Value")
  expect_equal(upgraded[["rule9"]], "old rule9")
  expect_setequal(names(upgraded), grep("^rule", names(analysisOptions("rareEventCharts")), value = TRUE))
})
