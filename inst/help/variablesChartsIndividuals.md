Variable Charts Indviduals
==========================
Control charts are a set of tools used to model the variation of a process, thereby indicating its stability and establishing a state of statistical control. 

For low volume production runs the best alternative for X-bar & R and X-bar & s control charts is an individual moving Range (X-mR) chart. 
An X-mR chart is also useful when there is no obvious source of variation related to a rational subgroup or when there is no practical subgroup.
An individual moving Range (X-mR) chart is used in the absence of subgroups or/and the source of process variation is unclear. It is therefore an alternative to the X-bar & R and X-bar & s control charts when analyzing small-scale production data.

An Autocorrelation chart models the pairwise correlation of values returned from a certain function. The correlation can be based on values following one another (lag of 1) or
on every second value (lag of 2).

## Assumptions 
-------
The assumptions associated with the I-mR chart are: 
- sequential or time-sequenced measurements must be used. 
- the data points are independent of one another- a given data point is unrelated to the next. 
- the data must be approximately normally distributed. 

The assumptions associated with the Autocorrelation chart are:
- the data points are dependent on one another- a given data point is related to the next. 

## Input
-------
### Assignment Box 
- Variables: the observations collected from a process.  

### Options
- X-mR chart:
  - Moving range length: the size of the range between observations. 
- Autocorrelation:
  - Number of lags: the number of lags to be used.
  - Confidence size: the size of the confidence interval used to calculate the limits. 

## Output
-------
### Charts
-------
- X-mR chart: outlines the process's value and moving range (MR) over time.
- Autocorrelation: outlines the autocorrelation over the lags. 

### Out-of-control Signals 
-------
_Statistical process control handbook page 23:_

- Test 1: One point beyond the control limits (sporadic issue).
- Test 2: Seven consecutive points above or below the central line (mean shift).
- Test 3:  A run up or down of seven consecutive points (trend).
- Test 4: Two out of three consecutive points beyond the warning (two sigma) limits (increasing variation).
- Test 5:Hugging the central line, this is indicated when 15 consecutive points are within the one sigma limits (reducing variation).
- Test 6: Eight consecutive points are beyond the one sigma limits, regardless of which side of the central line (bimodal distribution).

## References 
-------
- Duncan, A.J. (1986), Quality control and industrial statistics, Richard D. Irwin, Inc., and Automotive Industry Action Group (July 2005), Statistical process control (SPC) – Reference manual, AIAG.
- Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009).*Statistical process control handbooks*. SKF group.

## R Packages
-------
- ggplot2
- qcc
- jaspGraphs
- ggrepel
- stats