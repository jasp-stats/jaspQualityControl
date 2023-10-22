context("[Quality Control] Variables Charts for Subgroups")
.numDecimals <- 2
set.seed(1)

# Long / Column format

## Basic tests (all verified with Minitab)

### x-bar & r chart with manual subgroup size (verified with Minitab)
options <- analysisOptions("variablesChartsSubgroups")
options$variablesLong <- "Diameter"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormat.csv",
                       options)

test_that("X-bar & R control chart plot matches", {
 plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-r-control-chart")
})

### x-bar & s chart with manual subgroup size (verified with Minitab) Note: JASP does not use the unbiasing constant.
options <- analysisOptions("variablesChartsSubgroups")
options$variablesLong <- "Diameter"
options$TypeChart <- "xBarSchart"
results <- runAnalysis("variablesChartsSubgroups",
                       "datasets/variableChartsSubgroups/variableChartsSubgroupsLongFormat.csv",
                       options)

test_that("X-bar & s control chart plot matches", {
plotName <- results[["results"]][["controlCharts"]][["collection"]][["controlCharts_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "x-bar-s-control-chart")
})

### x-bar & r chart with manual subgroup size and stages

### x-bar & s chart with manual subgroup size and stages

### x-bar & r chart with subgroup variable

### x-bar & s chart with subgroup variable

### x-bar & r chart with subgroup variable and stages

### x-bar & s chart with subgroup variable and stages

### x-bar & r chart with warning limits

### x-bar & s chart with warning limits

### x-bar & r chart with known parameters

### x-bar & s chart with known parameters

### Change manual subgroup size value

### Report function with x-bar & r chart

### Report function with x-bar & s chart


## Missing values handling

### Missing values in measurements variable

#### Single missing value

##### x-bar & r chart

##### x-bar & s chart

#### All but one value missing

##### x-bar & r chart

##### x-bar & s chart

#### All values missing

##### x-bar & r chart

##### x-bar & s chart

### Missing values in subgroup variable

#### x-bar & r chart

#### x-bar & s chart

### Missing values in stages variable

#### x-bar & r chart

#### x-bar & s chart


## Unequal subgroup sizes

### x-bar & r chart with actual sizes

### x-bar & s chart with actual sizes

### x-bar & r chart with fixed group calculation

### x-bar & s chart with fixed group calculation


## Edge cases

### Subgroup size larger than data or stage

### Multiple stages assigned within subgroup



# Wide / Row format

## Basic tests

### x-bar & r chart

### x-bar & s chart

### x-bar & r chart with stages

### x-bar & s chart with stages

### x-bar & r chart with warning limits

### x-bar & s chart with warning limits

### x-bar & r chart with known parameters

### x-bar & s chart with known parameters

### Report function with x-bar & r chart

### Report function with x-bar & s chart


## Missing values handling

### Missing values in measurements variable

#### Single missing value

##### x-bar & r chart

##### x-bar & s chart

#### All but one value missing

##### x-bar & r chart

##### x-bar & s chart

#### All values missing

##### x-bar & r chart

##### x-bar & s chart

### Missing values in stages variable

#### x-bar & r chart

#### x-bar & s chart


## Unequal subgroup sizes

### x-bar & r chart with actual sizes

### x-bar & s chart with actual sizes

### x-bar & r chart with fixed group calculation

### x-bar & s chart with fixed group calculation
