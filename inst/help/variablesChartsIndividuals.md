Variable Charts Indviduals
==========================
A control chart is also called a Shewhart control chart.  A control chart is a graph used to study how a process changes over time. The data are plotted in time order. Each chart always has a central line for the average, an upper line for the upper control limit, and a lower line for the lower control limit. These lines are determined based on historical data. By comparing current data to these lines, conclusions can be drawn about whether the process variation is consistent (in control) or is unpredictable (out of control, and affected by special causes of variation). Control charts for variables data are always used in pairs. The top chart monitors the average, or the centring of the data from the process. The bottom chart monitors the range, or the spread of the distribution.

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
- the data points are dependent on one another
- a given data point is related to the next. 

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

#### JASP Default Tests
_Statistical process control handbook page 23:_

- Test 1: Beyond limit - One point beyond the control limits (sporadic issue).
- Test 2: Shift - Seven consecutive points above or below the central line (mean shift).
- Test 3: Trend - A run up or down of seven consecutive points (trend).
- Test 4: Increasing variation - Two out of three consecutive points beyond the warning (two sigma) limits (increasing variation).
- Test 5: Reducing variation - Hugging the central line, this is indicated when 15 consecutive points are within the one sigma limits (reducing variation).
- Test 6: Bimodal distribution - Eight consecutive points are beyond the one sigma limits, regardless of which side of the central line (bimodal distribution).

#### Nelson Laws
- Test 1: Beyond limit - One point beyond the control limits (sporadic issue).
- Test 2: Shift - Nine consecutive points above or below the central line (mean shift).
- Test 3: Trend - A run up or down of six consecutive points (trend).
- Test 4: Increasing variation - Two out of three consecutive points beyond the warning (two sigma) limits (increasing variation).
- Test 5: Reducing variation - Hugging the central line, this is indicated when 15 consecutive points are within the one sigma limits (reducing variation).
- Test 6: Bimodal distribution - Eight consecutive points are beyond the one sigma limits, regardless of which side of the central line (bimodal distribution).
- Test 7: Slightly increasing variation - Four out of five consecutive points beyond the warning (one sigma) limits (slightly increasing variation).
- Test 8: Oscillation - Fourteen points in a row are alternating increase and decrease (oscillation).

#### Western Electric Rules
- Test 1: Beyond limit - One point beyond the control limits (sporadic issue).
- Test 2: Shift - Eight consecutive points above or below the central line (mean shift).
- Test 4: Increasing variation - Two out of three consecutive points beyond the warning (two sigma) limits (increasing variation).
- Test 7: Slightly increasing variation - Four out of five consecutive points beyond the warning (one sigma) limits (slightly increasing variation).

#### Custom Test Selection
Select manually which tests you want to apply and modify them as desired:

- Test 1: Beyond limit - One point beyond the control limits (sporadic issue).
- Test 2: Shift - N consecutive points above or below the central line (mean shift).
- Test 3: Trend - A run up or down of N consecutive points (trend).
- Test 4: Increasing variation - N out of N+1 consecutive points beyond the warning (two sigma) limits (increasing variation).
- Test 5: Reducing variation - Hugging the central line, this is indicated when N consecutive points are within the one sigma limits (reducing variation).
- Test 6: Bimodal distribution - N consecutive points are beyond the one sigma limits, regardless of which side of the central line (bimodal distribution).
- Test 7: Slightly increasing variation - N out of N+1 consecutive points beyond the warning (one sigma) limits (slightly increasing variation).
- Test 8: Oscillation - N points in a row are alternating increase and decrease (oscillation).

Only Test 1, Test 2, Test 3 and Test 8 will be applied to the moving range chart.


## References 
-------
- Duncan, A.J. (1986), *Quality control and industrial statistics*, Richard D. Irwin, Inc.
- Automotive Industry Action Group, *Statistical Process Control – Reference Manual* (July 2005, 2nd Edition)
-	SKF Quality Techniques, Klerx, R., Dodson, B., and Dumont, D., *QT 1 – Process capability studies*, (PUB GQ/P9 10347/1 EN – December 2021)
-	SKF Quality Techniques, Dodson, B., Lynch, D., Weidenbacher, M., & and Klerx, R. (2009).), *QT 2 – Statistical process control*, (PUB GQS/P9 18343 EN – April 2019)


## R Packages
-------
- ggplot2
- qcc
- jaspGraphs
- ggrepel
- stats