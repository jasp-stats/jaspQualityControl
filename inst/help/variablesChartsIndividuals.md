Variable Charts Indviduals
==========================
For low volume production runs the best alternative for X-bar & R and X-bar & s control charts is an individual moving Range (I-mR) chart. 
An I-mR chart is also useful when there is no obvious source of variation related to a rational subgroup or when there is no practical subgroup.

The basic assumptions for individual and moving Range charts are:
- the measurements must be sequential or time sequenced.
- the measurements are independent of each other – one data point does not determine or impact the next data point.
- the data must approximate a normal distribution.

The basic assumptions for Autocorrelation are:
- each data point plotted is dependent on previously plotted points. 

#### Assignment Box 
-------
- Variables: the observations collected from a process.  

### Charts
-------
- X-mR chart: outlines the process (Individual) and process's moving range (MR) over time.
  - Moving range length: the size of the range between observations. 
- Autocorrelation: outlines the autocorrelation over the lags.
  - Number of lags: the number of lags to be used.
  - Confidence size: the size of the confidence interval used to calculate the limits. 

### R Packages
-------
- ggplot2
- qcc
- jaspGraphs
- ggrepel
- stats