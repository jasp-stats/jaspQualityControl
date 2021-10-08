Gauge r&R
==========================
Gage Repeatability and Reproducibility (Gage R & R) is an anlysis aimed at defnining the amount of variation in meaasurements given a measuermnet system. 
The variation detected in the measuerment is sourced in two factors, repeatability (equipment variation) and reproducibility (operator variation).
<br>
Both attribute and variable measurement data can be used in the analysis. 

## Input
### Data Format
-------
Data can be in the form of all observations in one column ("Column" option) or across rows with a subgroup index ("Row" option).

### Assignment Box
- Operators: the operators in the measurement system. 
- Parts: the parts of the measurement system.
- Measurements: the observations/data collected from a process.

### Gauge r&R Method
The method used in the analysis. 
- ANOVA method: analysis is based on analysis of variance (ANOVA).
- Range method: analysis is based on the range. 

### Options (ANOVA method)
#### Analysis options 
- Std.Deviation reference: either a historically known standard deviation (Historical process std.Deviation) or estimated from the data (Study std.Deviation).
- Tolerance: include a value for tolerance. 
- r&R table ANOVA method: options for the ANOVA table. 
 - Alpha interaction removal: value for alpha level. 
 - Study Var. multiplier type: multiplier based on either Std.Deviation or Percent. 
 - Study Var. multiplier value: value for the multiplier. 
 - Components of variation: display the components of variation plot. 
- Descriptives table: display descriptive statistics for the operator. 

#### Plots
- R chart by operator: display a plot of the range chart across operators by parts. 
- X-bar chart by operator: display a plot of the average chart across operators by parts. 
- Scatter plots operators: display a matrix plot of the relationship between the operators.
 - Fit line: fit a linear line to the data points. 
 - Show origin line: displays the origin line of the data points.
- Measurement by part plot: display a plot of the measurement's means across parts.
 - Display all measurements: display all measurement values across parts. 
- Measurement by operators plot: display a box plot of the measurement's values across operators.
- Part x operator interaction plot: display the measurements' mean across parts per operator. 