context("[Quality Control] Rare Event Charts")
.numDecimals <- 2

# Date input ####

## MD (g chart verified with Minitab, T chart differs because of zero handling) ####
options <- analysisOptions("rareEventCharts")
options$variable <- "MD"
options$dataType <- "dataTypeDates"
options$dataTypeDatesStructure <- "dateOnly"
options$dataTypeDatesFormatDate <- "md"
options$gChart <- TRUE
options$tChart <- TRUE
results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options)

test_that("1.1 Test of G chart with MD date format", {
  plotName <- results[["results"]][["gChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart1")
})

test_that("1.2 Test of T chart with MD date format", {
  plotName <- results[["results"]][["tChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart1")
})

## MDHM (t chart verified with Minitab, g chart not possible in Minitab) ####
options <- analysisOptions("rareEventCharts")
options$variable <- "MDHM"
options$dataType <- "dataTypeDates"
options$dataTypeDatesStructure <- "dateTime"
options$dataTypeDatesFormatDate <- "md"
options$dataTypeDatesFormatTime <- "HM"
options$gChart <- TRUE
options$tChart <- TRUE
results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options)

test_that("2.1 Test of G chart with MDHM date format", {
  plotName <- results[["results"]][["gChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "g-chart2")
})

test_that("2.2 Test of T chart with MDHM date format", {
  plotName <- results[["results"]][["tChart"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "t-chart2")
})


## DM ####
# options <- analysisOptions("rareEventCharts")
# options$variable <- "DM"
# options$dataType <- "dataTypeDates"
# options$dataTypeDatesStructure <- "dateOnly"
# options$dataTypeDatesFormatDate <- "dm"
# options$gChart <- TRUE
# options$tChart <- TRUE
# results <- runAnalysis("rareEventCharts", "datasets/rareEventCharts/rareEventCharts.csv", options, makeTests = T)



## DMY ####

## MDY ####

## YMD ####

# Time input ####

## Decimal Hours ####

## Decimal Days ####

# Opportunities input ####

# Stages ####

# Historical options ####

# Distribution options ####

# Report ####
