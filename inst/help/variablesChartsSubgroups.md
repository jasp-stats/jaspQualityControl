Variable Charts Subgroups
==========================
Control charts are a set of tools used to model the variation of a process, thereby indicating its stability and establishing a state of statistical control. 

Variable charts for subgroups are used to monitor the behavior of a dimensional variable. 
Depending on the data collected from a given process, the analysis can be executed using historical data (Phase 2) or new data (Phase 1).  

## Input
### Data Format
-------
Data can be in the form of all observations in one column ("Column" option) or across rows with a subgroup index ("Row" option).

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

## References
- Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2019).*Statistical process control*. SKF group. 

## R Packages
-------
- ggplot2
- qcc
- jaspGraphs
- ggrepel
- tidyr
- tibble
- mle.tools
