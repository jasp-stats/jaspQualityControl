context("[Quality Control] Time Weighted Charts")
.numDecimals <- 2

# Long Format ####

## Without Stages ####

### CUSUM ####
options <- analysisOptions("timeWeightedCharts")
options$measurementLongFormat <- "Measurement"
options$subgroupSizeType <- "manual"
options$manualSubgroupSizeValue <- 5
options$cumulativeSumChart <- TRUE
options$cumulativeSumChartSdSource <- "data"
options$cumulativeSumChartSdMethod <- "s"
results <- runAnalysis("timeWeightedCharts", "datasets/timeWeightedCharts/cumsumChartLong.csv", options, makeTests = T)

### EWMA  ####


## With Stages ####

### CUSUM ####

### EWMA  ####

## Subgrouping Mechanisms ####

### Grouping Variable ####

#### Value Change ####

##### CUSUM ####

##### EWMA  ####

#### Same Value ####

##### CUSUM ####

##### EWMA  ####

### No Subgroups ####

#### CUSUM ####

#### EWMA  ####

## Plotting Options ####

### Historical values ####

#### CUSUM ####

#### EWMA  ####

### Alternative SD Estimation ####

#### CUSUM ####

#### EWMA  ####

## Report ####

# Wide Format ####

## Without Stages ####

### CUSUM ####

## With Stages ####

### CUSUM ####

## Plotting Options ####

### Historical values ####

#### CUSUM ####

### Alternative SD Estimation ####

#### CUSUM ####


## Report ####
