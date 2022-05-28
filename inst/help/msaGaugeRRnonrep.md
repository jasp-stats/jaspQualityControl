Gauge r&R (Non-Replicable Measurements)
==========================
Gauge Repeatability and Reproducibility (Gauge R & R) is an analysis aimed at defining the amount of variation in measurements given a measurement system. This is the version
of the analysis for non-replicable measurements.
<br>
Both attribute and variable measurement data can be used in the analysis. 

## Input
### Data Format
-------
Data can be in the form of all observations in one column ("Single column") or across rows with a subgroup index ("Across rows").

### Assignment Box
- Operators: the operators in the measurement system. 
- Parts: the parts of the measurement system.
- Measurements: the observations/data collected from a process.



### Gauge r&R Options
#### Analysis options 
- Std.Deviation reference: either a historically known standard deviation (Historical process std.Deviation) or estimated from the data (Study std.Deviation).
- Tolerance: include a value for tolerance. 
- r&R tables: options for the Gauge r&R tables. 
 - Study Var. multiplier type: multiplier based on either Std.Deviation or Percent. 
 - Study Var. multiplier value: value for the multiplier. 
 - Graph variation components: display the components of variation (contribution, study variation, and tolerance) plot. 

#### Plots
- R chart by operator: display a plot of the range chart across operators by parts. 
- X-bar chart by operator: display a plot of the average chart across operators by parts. 
- Measurement by part plot: display a plot of the measurement's means across parts.
 - Display all measurements: display all measurement values across parts. 
- Measurement by operators plot: display a box plot of the measurement's values across operators.

## Output 
-------
- Gauge r&R (Nested): Nested ANOVA table for the input variables, repeatability, and total Gauge r&R.  
- Gauge r&R Variance Components: variance and contribution in percentage of the input variables, repeatability, reproducibility, and total Gauge r&R.  
- Gauge Evaluation: the standard deviations, study variations, and percent of study variation and tolerance for input variables, repeatability, reproducibility, and total Gauge r&R.  

## References 
-------
- Duncan, A.J. (1986), Quality control and industrial statistics, Richard D. Irwin, Inc., and Automotive Industry Action Group (July 2005), Statistical process control (SPC) – Reference manual, AIAG.
- Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009).*Statistical process control handbook*. SKF group. 

## R Packages
-------
- jaspGraphs
- ggplot2
- tidyr
- qcc
- cowplot