Variable Charts Subgroups
==========================
Control charts are a set of tools used to model the variation of a process, thereby indicating its stability and establishing a state of statistical control. 

Variable charts for subgroups are used to monitor the behavior of a dimensional variable. 
Depending on the data collected from a given process, the analysis can be executed using historical data (Phase 2) or new data (Phase 1).  

## Input
-------
### Data Format
Data can be in the form of all observations in one column ("Single column") or across rows, with one group per row ("Across rows").

### Assignment Box 
-------
- Measurements: the observations collected from a process.
- Subgroups ("Column" option): the subgroup that each observation is assigned to, if all observations are in the same column.
- Timestamp ("Row" option): optinal subgroup names for each row of observations, that are used as x-axis labels. 
- Stages: a column to split the analysis into multiple parts by assigning a stage to each subgroup. In "row" format, only one stage per subgroup is possible. In "column" format, only the first specified stage for each subgroup is considered.

### Specifying Subgroups in "Column" Format
-------
- Through grouping variable: a single-column subgroup variable is specified that assigns each observation to a subgroup.
    - Grouping method:  if identical values of the subgroup variable are not adjacent in the dataset (top to bottom), there are two methods to handle this. If the grouping method is "Subgroup value change", only series of identical and adjacent subgroup values form a group. If the grouping method is "Same subgroup value", all identical subgroup values form a group, regardless of their adjacency. For instance, if the values of the subgroup variable are [A, A, B, B, A], the method "Subgroup value change" would yield three groups: [A, A], [B, B] and [A]. The method "Same subgroup value" would yield two groups, [A, A, A] and [B, B].
- Manual subgroup size: the observations are assigned in the order of appearence (from top to bottom) to subgroups of the specified size. If the number of observations cannot be equally divided into subgroups of the specified size, the last subgroup has the size of the remaining observations.

### Handling Unequal Subgroup Sizes
-------
The size of the subgroups is relevant for the calculation of the process variance and subsequently the calculation of the control limits. If not all subgroups are of the same size, there are two options to handle this:
- Assume equal subgroup sizes: the control limits are calculated with the assumption that all subgroups have the same size, and the same control limits are calculated for all groups. In this case, the size of the largest subgroup is used for the calculation.
- Calculate with actual size: the control limits are calculated per subgroup and the actual subgroup sizes are used for the calculation.

### Plotting Options
-------
- Warning limits: plots limits one and two standard deviations from the central line. 
- Known parameters: enables the use of historic parameter values (Phase 2). 

## Output
### Charts
-------
- X-bar & R chart: plots the process mean (x-bar) and process range (R) over time.
- X-bar & s chart: plots the process mean (x-bar) and process standard deviation (s) over time.

### Out-of-control Signals 
-------

#### JASP Default Tests
_Statistical process control handbook page 23:_

- Test 1: One point beyond the control limits (sporadic issue).
- Test 2: Seven consecutive points above or below the central line (mean shift).
- Test 3:  A run up or down of seven consecutive points (trend).

#### Nelson Laws
- Test 1: One point beyond the control limits (sporadic issue).
- Test 2: Nine consecutive points above or below the central line (mean shift).
- Test 3:  A run up or down of six consecutive points (trend).
- Test 4: Two out of three consecutive points beyond the warning (two sigma) limits (increasing variation).
- Test 5: Hugging the central line, this is indicated when 15 consecutive points are within the one sigma limits (reducing variation).
- Test 6: Eight consecutive points are beyond the one sigma limits, regardless of which side of the central line (bimodal distribution).
- Test 7: Four out of five consecutive points beyond the warning (one sigma) limits (slightly increasing variation).
- Test 8: Fourteen points in a row are alternating increase and decrease (oscillation).

#### Western Electric Rules
- Test 1: One point beyond the control limits (sporadic issue).
- Test 2: Eight consecutive points above or below the central line (mean shift).
- Test 4: Two out of three consecutive points beyond the warning (two sigma) limits (increasing variation).
- Test 7: Four out of five consecutive points beyond the warning (one sigma) limits (slightly increasing variation).

#### Custom Test Selection
Select manually which tests you want to apply and modify them as desired:

- Test 1: N points beyond the control limits (sporadic issue).
- Test 2: N consecutive points above or below the central line (mean shift).
- Test 3:  A run up or down of N consecutive points (trend).
- Test 4: N out of N+1 consecutive points beyond the warning (two sigma) limits (increasing variation).
- Test 5: Hugging the central line, this is indicated when N consecutive points are within the one sigma limits (reducing variation).
- Test 6: N consecutive points are beyond the one sigma limits, regardless of which side of the central line (bimodal distribution).
- Test 7: N out of N+1 consecutive points beyond the warning (one sigma) limits (slightly increasing variation).
- Test 8: N points in a row are alternating increase and decrease (oscillation).


## References 
-------
- Duncan, A.J. (1986), Quality control and industrial statistics, Richard D. Irwin, Inc., and Automotive Industry Action Group (July 2005), Statistical process control (SPC) - Reference manual, AIAG
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
