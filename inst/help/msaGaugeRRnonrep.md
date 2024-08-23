Gauge r&R (Non-replicable Measurements)
==========================
Gauge repeatability and Reproducibility (Gauge r&R) is an analysis method aimed at defining the amount of variation in measurements given a measurement system.  The variation detected in the measurement is sourced in two factors, repeatability (equipment variation) and Reproducibility (operator variation). This is the version of the analysis for non-replicable measurements.

## Input
### Data format
-------
Data can be in the form of all observations in one column ("Single column") or across rows with a part identification ("Across rows").

### Assignment box
- Operators: the appraisers using the measurement system. 
- Parts: the parts selected from the process and representing its entire operating range.
- Measurements: the repeated measurements of each part.



### Gauge r&R analysis

#### Analysis options 
- Std.Deviation reference: either a historically known standard deviation (Historical process std.Deviation) or estimated from the data (Study std.Deviation).
- Tolerance: include a value for tolerance. 
- r&R tables: options for the Gauge r&R tables. 
 - Study Var. multiplier type: multiplier based on either Std.Deviation or Percent. 
 - Study Var. multiplier value: value for the multiplier. 
 - Graph variation components: display the components of variation (contribution, study variation, and tolerance) plot. 

#### Plots
- Range chart by operator: displays the variation in the measurements made by each operator, allowing you to compare operators to each other.
- Average chart by operator: displays the measurements in relation to the overall average for each operator, allowing you to compare operators to each other, and to the average.
- Scatter plots operators: displays a matrix plot of the relationship between the operators.
    - Fit line: fits a linear line to the data points.
    - Show origin line: displays the origin line of the data points.
- Measurement by part displays the main effect for the parts, so you can compare the average measurement for each part.
    - Display all measurements: displays all measurement values across parts.
- Measurement by operators: displays the main effect for the operators, so you can compare average measurement for each operator. If you have many replicates, boxplots are displayed on the By Operator graph.


## Output 
-------
- Gauge r&R (Nested): Nested ANOVA table for the input variables, repeatability, and total Gauge r&R.  
- Gauge r&R Variance Components: variance and contribution in percentage of the input variables, repeatability, reproducibility, and total Gauge r&R.  
- Gauge Evaluation: the standard deviations, study variations, and percent of study variation and tolerance for input variables, repeatability, reproducibility, and total Gauge r&R.

## General guideline for acceptance of measurement systems
If the total gauge r&R contribution in the %Study Var. column (%Tolerance, %Process) is:
- %r&R ≤ 10%: measurement system is generally considered to be acceptable
- 10% < %r&R ≤ 30%:	may be acceptable for some applications
- %r&R > 30%: measurement system is considered to be unacceptable.

If you are looking at the %Contribution column, the corresponding standards are: 
- Less than 1%: the measurement system is acceptable
- Between 1% and 9%: the measurement system is acceptable depending on the application, the cost of the measuring device, cost of repair, or other factors
- Greater than 9%: the measurement system is unacceptable and should be improved.

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