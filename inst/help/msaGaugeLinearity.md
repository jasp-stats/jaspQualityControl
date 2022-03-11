Gauge Linearity and Bias
==========================
Gauge Linearity and Bias study is used to investigate the linearity (accuracy across the expected range of measurements) and 
bias (matching between measurements and reference values) of a system.

## Input
-------
### Assignment Box
- Parts: the parts of the measurement system.
- Measurements: the observations/data collected from a process.
- Standard: the reference values. 

## Options
### Analysis options 
- Process variation: the number of standard deviations used to calculate the bias, by default 6.
- Linearity table: outputs the regression model and gauge linearity tables.
- Bias table: outputs the gauge bias table. 

### Plots
- Linearity and bias graph: plots the linear relationship between the bias and reference values.
- Percent process variation graph: plots the percentage of the gauge bias and linearity in relation to the process variation. 

## Output 
-------
- Gauge Bias table: per part the- reference values, mean per reference value, bias per reference value, and a t-test of the bias against a value of 0.  
- Regression Model table: the predictors' (intercept and slope) coefficients, t-statistics, standard errors, and p-values.  
- Gauge Linearity table: the regression model's sigma (S), absolute coefficient values multiplied by the process variation (linearity), r-squared, the percentage of linearity in relation to the process variation.
- Bias and linearity plot: linear relationship between the bias and reference values, reference values' means (red dots), and values (black axes) given the linear equation.
- Percentage Process Variation Graph: percentage of linearity and bias in relation to the process variation. 

## References 
-------
- Duncan, A.J. (1986), Quality control and industrial statistics, Richard D. Irwin, Inc., and Automotive Industry Action Group (July 2005), Statistical process control (SPC) – Reference manual, AIAG.
- Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009).*Statistical process control handbook*. SKF group. 

## R Packages
-------
- jaspGraphs
- ggplot2
- ggpubr