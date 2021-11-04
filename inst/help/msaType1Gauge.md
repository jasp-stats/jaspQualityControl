Type 1 Gauge
==========================
Type 1 Gauge is performed before a Gauge r & R study to determine the measurement's capability using bias and repeatability. 

## Input
-------
### Assignment Box
- Measurements: the observations/data collected from a process.

## Options
### Analysis options 
- Reference value: value for reference. 
- Tolerance range: value for tolerance. 
- Percent of tolerance for Cg: value for Cg.
- Study var. (number of std. deviations): value for the number of standard deviations.
- Bias and instrument capability table: display the Basic statistics and Capability tables.
- One sample T-test: display the T-Test table.
 - Confidence interval for bias: value for the confidence interval's width.

### Plots
- Run chart: display the run chart of the measurement.
 - Display individual measurements: display the measurement's individual values. 
 - Display tolerance limits: display the tolerance limits.
- Histogram: display the bias histogram.
 - Bin width type: specify the type of bin width.
 - Number of bins: value for the number of bins.
 - Display mean: display the mean value of the measurement.
 - Confidence interval for mean: value for the mean confidence interval's width.
 - Display reference value: display the reference value.

## Output 
-------
- Run chart plot: plots the measurement's values across observations/samples.
- Basic statistics table: table of the reference value, mean, bias, standard deviation, study variation (SD of the measurement times the Study var.), tolerance, and percent of bias (bias's percentage of the tolerance).
- Capability table: Cg, CgK, percent of the variance of Repeatability and Repeatability and bias.
- T-Test of observed bias: degrees of freedom, Bias, Confidence interval limits, t-statistic, and p-value for the test of observed bias against a value of zero.
- Bias histogram: histogram of the measurement. 

## References 
-------
- Chrysler Group LLC, Ford Motor Company, & General Motors Corporation. (2010). *Measurement Systems Analysis* (4th ed.). http://www.aiag.orgt. 
- Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009).*Statistical process control handbooks*. SKF group. 

## R Packages
-------
- jaspGraphs
- jaspDescriptives
- tidyr
- ggplot2
- ggrepel