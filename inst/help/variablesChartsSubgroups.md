Variable Charts Subgroups
==========================
Control charts monitor the behaviour of a process to determine whether it is stable. They display data in time sequence in which the data occurred or produced.

Control charts for variables are utilised for any dimensional variable. 
When a process is to be studied for the first time with the objective of bringing the process into a state of statistical control, one can either use historical data which have previously been obtained from the process (Phase 2) or undertake action to obtain new data from a series of samples before attempting to establish the control chart (Phase 1).

### Data Format
-------
Data can be in the form of all observations in one column ("Column" option) or across rows with a subgroup index ("Row" option).

#### Assignment Box 
-------
- Measurements: the observations collected from a process. the observations collected from a process. 
- Subgroups ("Row" option): Index for the observations over the rows. 
- Subgroup size ("Column " option): the number of subgroups. 

### Charts
-------
- X-bar & R chart: outlines the process mean (x-bar) and process range (R) over time.
- X-bar & s chart: outlines the process mean (x-bar) and process standard deviation (s) over time.

### Plotting options
-------
- Warning limits: plots one and two standard deviations from the central line. 
- Known parameters: allows the addition of historic parameter values (Phase 2). 

### R Packages
-------
- ggplot2
- qcc
- jaspGraphs
- ggrepel