Variable Charts Subgroups
==========================
Control charts are a set of tools used to model the variation of a process, thereby indicating its stability and establishing a state of statistical control. 

Variable charts for subgroups are used to monitor the behavior of a dimensional variable. 
Depending on the data collected from a given process, the analysis can be executed using historical data (Phase 2) or new data (Phase 1).  

## Input
-------
### Data Format
Data can be in the form of all observations in one column ("Single column") or across rows with a subgroup index ("Across rows").

### Assignment Box 
-------
- Measurements: the observations collected from a process. the observations collected from a process. 
- Subgroups ("Row" option): Index for the observations over the rows. 
- Subgroup size ("Column " option): the number of subgroups. 

### Plotting options
-------
- Warning limits: plots one and two standard deviations from the central line. 
- Known parameters: enables the use of historic parameter values (Phase 2). 

## Output
### Charts
-------
- X-bar & R chart: plots the process mean (x-bar) and process range (R) over time.
- X-bar & s chart: plots the process mean (x-bar) and process standard deviation (s) over time.

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
- Duncan, A.J. (1986), Quality control and industrial statistics, Richard D. Irwin, Inc., and Automotive Industry Action Group (July 2005), Statistical process control (SPC) – Reference manual, AIAG
- Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009).*Statistical process control handbook*. SKF group.


## R Packages
-------
- ggplot2
- qcc
- jaspGraphs
- ggrepel
- tidyr
- tibble
- mle.tools
