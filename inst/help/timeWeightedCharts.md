Time Weighted Charts
==========================
Two very effective alternatives to the Shewhart control chart may be used when small process shifts are of interest: the cumulative sum (CUSUM) control chart, and the exponentially weighted moving average (EWMA) control chart. CUSUM and EWMA control charts are excellent alternatives to the Shewhart control chart for phase II process monitoring situations. Collectively, the CUSUM and EWMA control chart are sometimes called time-weighted control charts.

The cumulative sum chart and the exponentially weighted moving average (EWMA) charts are also monitors the mean of the process, but the basic difference is unlike X-bar charts they consider the previous value means at each point. Moreover, these charts are considered as a reliable estimate when correct standard deviation exists.

## CUSUM control chart
--------
### Features of cumulative sum (CUSUM) control charts
--------
A CUSUM control chart is essentially a running total of deviations from some preselected reference value. The mean of any group of consecutive values is represented visually by the current slope of the graph. The basic advantage of a CUSUM chart is that it is more sensitive to small shifts of the process mean when compared to the Shewhart control charts.

### When to use
--------
- The false alarm rate of a Shewhart control chart is too high.
- The process rate is so high that false alarms are hardly acceptable.
- Small process changes (less than one sigma) must be detected quickly.
- Automation is available to update the chart in real time.

In general, the underlying distribution is supposed to be normal, but CUSUM control charts can be used as well for binomial and Poisson distributions for modelling nonconformities and fraction nonconforming.

### Assumptions
--------
The basic assumptions for a CUSUM control chart are:

1. the measurements must be sequential or time sequenced;
2. the measurements are independent of each other – one data point does not determine or impact the next data point;
3. the measurements are approximately normally distributed, or the sample size of each plotted point is large enough so the average yields an approximate normal distribution.

## Input
--------

### Assignment box
--------
- Measurements: the observations collected from a process.
- Subgroups ("Column" option): the subgroup that each observation is assigned to, if all observations are in the same column.
- Timestamp ("Row" option): optinal subgroup names for each row of observations, that are used as x-axis labels. 
- Stages: a column to split the analysis into multiple parts by assigning a stage to each subgroup. In "row" format, only one stage per subgroup is possible. In "column" format, only the first specified stage for each subgroup is considered.

### Specifying subgroups in "Column" format
-------
- Through grouping variable: a single-column subgroup variable is specified that assigns each observation to a subgroup.
    - Grouping method:  if identical values of the subgroup variable are not adjacent in the dataset (top to bottom), there are two methods to handle this. If the grouping method is "Subgroup value change", only series of identical and adjacent subgroup values form a group. If the grouping method is "Same subgroup value", all identical subgroup values form a group, regardless of their adjacency. For instance, if the values of the subgroup variable are [A, A, B, B, A], the method "Subgroup value change" would yield three groups: [A, A], [B, B] and [A]. The method "Same subgroup value" would yield two groups, [A, A, A] and [B, B].
- Subgroup size: the observations are assigned in the order of appearence (from top to bottom) to subgroups of the specified size. If the number of observations cannot be equally divided into subgroups of the specified size, the last subgroup has the size of the remaining observations.

### Chart options
--------
- Target: Value to use as a target

- Average run length (ARL): The number of plotted points until special cause variation is signalled is the average run length (ARL). It is a function of the process mean shift.

- Number of standard deviations (*h*): Standardised decision interval

- Shift size (*k*): *k* is usually called the reference value (or the allowance, or the slack value), and it is often chosen about halfway between the target (µ0) and the out‑of‑control value of the mean (µ1) that we are interested in detecting quickly.

The proper selection of these two parameters (*h* and *k*) is quite important, as it has substantial impact on the performance of the CUSUM. Calculate the reference value or allowable slack, since CUSUM chart is used to monitor the small shifts, generally 0.5 to 1 σ will be considered. *k* = 0.5 σ. Compute decision interval *h*, generally ± 4 σ will be considered (some place ± 5 σ also be used).

## Recommendations for CUSUM design
--------
The tabular CUSUM is designed by choosing values for the reference value *k* and the decision interval *h*. It is usually recommended that these parameters be selected to provide good average run length performance. There have been many analytical studies of CUSUM ARL performance. Based on these studies, we may give some general recommendations for selecting *h* and *k*. Define H = *h* σ and K = *k* σ, where σ is the standard deviation of the sample variable used in forming the CUSUM. Using *h* = 4 or *h* = 5 and *k* = ½ will generally provide a CUSUM that has good ARL properties against a shift of about 1 σ in the process mean.

## Output
--------
### Charts
--------
CUSUM statistics are cumulative deviations from the target, or nominal.

Graphical display for the tabular CUSUM. These charts are sometimes called CUSUM status charts. They are constructed by plotting C+i and C−i versus the sample number.

## References
--------
- Page, E.S. (1954), *Continuous inspection schemes*, Biometrika, Vol. 41, No. 1-2, pp. 100-115
- Roberts, S.W. (1959), *Control chart tests based on geometric moving averages*, Technometrics, Vol. 1, No. 3, pp. 239-250
- SKF Quality Techniques, Dodson, B., Lynch, D., Weidenbacher, M., and Klerx, R., *QT 2 – Statistical process control* (PUB GQS/P9 18343 EN – April 2019)
- International Organization for Standardization, *Control charts – Part 4: Cumulative sum charts*, ISO 7870-4:2021 (E)
- Montgomery D. C. (2013), *Introduction to statistical quality control (7th Ed.)*, John Wiley & Sons, Inc , Hoboken (NJ)

## R Packages
-------
- ggplot2
- qcc
- jaspGraphs
- ggrepel
