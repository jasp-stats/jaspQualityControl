Test-retest
==========================
Test-retest aims to approximate an overall picture of the measurement system's variability as it does not distinguish the variability to its repeatability and reproducibility components. 

## Input
-------
### Data Format
Data can be in the form of all observations in one column ("Single column") or across rows with a subgroup index ("Across rows").

### Assignment Box
- operators: the operators of the measurement system.
- Parts: the parts of the measurement system.
- Measurements: the observations/data collected from a process.

## Options
### Range Method Options 
- Process Std. Deviation: historical process standard variation.
- Tolerance: value for tolerance.
- r&R table: outputs the short gauge study table.

### Plots
- Run chart of parts: plots the measurements' values agianst the parts.
- Scatter plot: plots a scatter plot of the measurements. 
 - Regression line: fits a regression line to the scatter plot.
 - jitter: adds jitter to the scatter plot.
- Range chart: plots the range chart by part. 
- Traffic light graph: plots the traffic light graph.

## Output 
-------
- Short gauge study table: sample size, R-bar, historical standard variation, tolerance, GRR, GRR's percentage of the standard variation and tolerance.  
- Traffic light graph: GRR's percentage of the standard variation and tolerance.  
- Run chart of parts: all measurements' values per part.
- Range chart by part: range chart for the measurements using the parts as subgroups.
- Scatter plot: a scatter plot of the measurements. 


## References 
-------
- Duncan, A.J. (1986), Quality control and industrial statistics, Richard D. Irwin, Inc., and Automotive Industry Action Group (July 2005), Statistical process control (SPC) – Reference manual, AIAG.
- Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009).*Statistical process control handbook*. SKF group. 

## R Packages
-------
- jaspGraphs
- tidyr
- ggplot2