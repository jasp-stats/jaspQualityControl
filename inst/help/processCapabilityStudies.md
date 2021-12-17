Process capability Studies
==========================
Process capability studies are a set of tools used to model and determine the statistical control of a process using capability indices and limits specification. 

## Input
-------
### Data Format
Data can be in the form of all observations in one column ("Single column") or across rows with a subgroup index ("Across rows").

### Assignment Box
- Measurements: the observations/data collected from a process.
- Subgroups: the corresponding subgroups to each observation.

### Options 
#### Type of data distribution
- Type of data distribution: indicate whether the data approximates a normal distribution or another distribution (Weibull, Lognormal, 3-parameter Weibull, and 3-parameter Lognormal)
 - Specify a distribution: the non-normal distribution to be used. 
 - Non-normal capability statistics: the method used to calculate the capability statistics for non-normally distributed data.

#### Capability studies
- Specification limits:
 - Lower specification limit: the value used as the lower specification limit.
 - Target value: the value used as the target.
 - Upper specification limit: the value used as the upper specification limit.
- Process capability plot: 
 - Number of bins: the number of bins to be plotted. 
 - Process capability tables:
 - Confidence interval: the percentage of confidence used for calculating the intervals of process capability's statistics (Cp, Cpk, Pp, Ppk, and Cpm). 

#### Distribution of the process
- Histogram:
 - Fit distribution: display a line of the data's distribution.
 - Number of bins: number of bins to be used for the histogram.
- Probability table and plot
 - Null distribution: the distribution underlaying the data displayed in the probability plot. 
 - Display grid lines: add grid lines to the probability plot. 

### Advanced Options 
 - Rank method: the method used to calculate the rank of the data, displayed in the probability plot. 

## Output
-------
### Control charts:
- X-bar & R chart: plots the process average (x-bar) and process range (R) over time.
- X-mR chart: outlines the process's value and moving range (MR) over time.

### Histogram: 
- plots the density of the process's values in a given range, depending on the bin number.

### Probability table and plot
- Probability table: the number of observations, the process's mean, the process's standard deviation, the Anderson-Darling statistic, and the p-value associated with the former. 
- Probability plot: plots the percentage of the process's rank over the process's values. 

### Capability studies
- Process summary: the lower specification limit, target, upper specification limit, sample size, average, total standard deviation, and within standard deviation.
- Capability of the process: plots the density of the process's values in a given range in relation to the upper and lower specification limits (red lines) and within (red) and overall (blue) distributions' line. 
- Process capability (within): the Cp and its confidence interval's values, CpL, cpK, and its confidence interval's values.
- Process performance: the Pp and its confidence interval's values, PpL, PpU, PpK and its confidence interval's values, Cpm and its confidence interval's values.
- Non-conformance statistics: ppm and its relation to the specification limits across the observed, expected overall, and (only for normally distributed data) expected within statistics. 

## References
-------
- Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009).*Statistical process control*. SKF group. 

## R Packages
-------
- ggplot2
- qcc
- jaspGraphs
- ggrepel
- FAdist
- goftest
- fitdistrplus
- mle.tools
- tidyr
- tibble
- EnvStats
- weibullness
