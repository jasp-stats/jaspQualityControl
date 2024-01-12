Process capability Studies
==========================
A process capability study is the analysis of a process in order to determine whether the process under study is capable of providing good quality products. This analysis uses data from an initial run of parts to predict whether a manufacturing process can repeatably produce parts that meet specifications.
A subgroup is a group of pars that are produced under the same set of conditions.
Rational subgroup: "A subgroup gathered in such a manner as to give the maximum chance for the measurements in each subgroup to be alike and the maximum chance for the subgroups to differ from the other. This subgrouping scheme enables a determination of whether the process variation includes special cause variation." [1]


## Input
-------
### Data Format
Data can be in the form of all observations in one column ("Single column") or across rows with a subgroup index ("Across rows").

### Assignment Box
- Measurements: the observations/data collected from a certain process parameter
- Subgroups ("Column" option): the subgroup that each observation is assigned to, if all observations are in the same column.
- Timestamp ("Row" option): optinal subgroup names for each row of observations, that are used as x-axis labels. 
- Stages: a column to split the analysis into multiple parts by assigning a stage to each subgroup. In "row" format, only one stage per subgroup is possible. In "column" format, only the first specified stage for each subgroup is considered.

### Specifying Subgroups in "Column" Format
-------
- Through grouping variable: a single-column subgroup variable is specified that assigns each observation to a subgroup.
- Through grouping variable: a single-column subgroup variable is specified that assigns each observation to a subgroup.
    - Grouping method: if identical values of the subgroup variable are not adjacent in the dataset (top to bottom), there are two methods to handle this. If the grouping method is "Subgroup value change", only series of identical and adjacent subgroup values form a group. If the grouping method is "Same subgroup value", all identical subgroup values form a group, regardless of their adjacency. For instance, if the values of the subgroup variable are [A, A, B, B, A], the method "Subgroup value change" would yield three groups: [A, A], [B, B] and [A]. The method "Same subgroup value" would yield two groups, [A, A, A] and [B, B].
- Manual subgroup size: the observations are assigned in the order of appearence (from top to bottom) to subgroups of the specified size. If the number of observations cannot be equally divided into subgroups of the specified size, the last subgroup has the size of the remaining observations.

### Handling Unequal Subgroup Sizes
-------
The size of the subgroups is relevant for the calculation of the process variance and subsequently the calculation of the control limits. If not all subgroups are of the same size, there are two options to handle this:
- Assume equal subgroup sizes: the control limits are calculated with the assumption that all subgroups have the same size, and the same control limits are calculated for all groups. In this case, the size of the largest subgroup is used for the calculation.
- Calculate with actual size: the control limits are calculated per subgroup and the actual subgroup sizes are used for the calculation.

### Options 
#### Type of data distribution
- Type of data distribution: indicate whether the data approximates a normal distribution or another distribution (the most commonly used distributions are: Weibull, Lognormal, 3-parameter Weibull, and 3-parameter lognorma)
    - Specify a distribution: the non-normal distribution to be used. 
    - Non-normal capability statistics: the method used to calculate the capability statistics for non-normally distributed data.

#### Capability studies
- Specification limits:
    - Lower specification limit: the value used as the lower tolerance limit.
        - Boundary: whether the lower specification limit is a physicial boundary that cannot be exceeded.
    - Target value: the value used as the target.
    - Upper specification limit: the value used as the upper tolerance limit.
        - Boundary: whether the upper specification limit is a physicial boundary that cannot be exceeded.
- Process capability plot: 
    - Number of bins: the number of classes to be plotted. 
    - Process capability tables.
    - Confidence interval: the percentage of confidence used for calculating the intervals of process capability's statistics (Cp, Cpk, Pp, Ppk, and Cpm). 

#### Distribution of the process
- Histogram:
    - Fit distribution: add a line to the data, which represents the fitted distribution.
    - Number of bins: number of bins to be used for the histogram.
- Probability table and plot
    - Null distribution: the distribution used to exam the data displayed in the probability plot to determine whether the data are distributed approximately normally or any other distribution.
    - Display grid lines: add grid lines to the probability plot. 

### Advanced Options 
- Rank method: the method used to calculate the rank of the data, displayed in the probability plot (Benard's median rank method is the most commonly used method, and is very close to the exact method).

## Output
-------
### Control charts:
- X-bar & R control chart: charts the process average (x-bar) and process range (R = maximum - minimum) over time.
- X-bar & s control chart: charts the process average (x-bar) and standard deviation (s) over time.
- X-mR contrl chart: charts the individual measurement and the moving range (mR) of two consecutive measurements over time.
- X-bar & mR control chart: charts the process average (x-bar) and the moving range (mR) of two consecutive subgroup means over time.

### Histogram: 
- plots the frequency of the values in classes. A histogram is a graphical representation of data points organized into user-specified ranges. Similar in appearance to a bar graph, the histogram condenses a data series into an easily interpreted visual by taking many data points and grouping them into logical classes or bins.

### Probability table and plot
- Probability table: the number of observations, the process's mean, the process's standard deviation, the Anderson-Darling statistic, and the *p*-value associated with the former. 
- Probability plot: plots the data against a theoretical distribution in such a way that the plotted points should form approximately a straight line. 

### Capability studies
- Process summary (only displayed if one or both specification limits are filled in): Produces a table of statistics consisting of the lower specification limit, target, upper specification limit, sample size, overall average, overall (total) standard deviation, and the within (subgroup) standard deviation.
- Capability of the process: plots the frequency distribution with the fitted distribution based on the overall (total) process variation and the inherent (within) process variation. It compares the process spread to the customer requirements (the upper and lower specification limits displayed as red vertical lines). The two curved lines indicate how well the selected distribution fits to the data. The two distributions are based on the within (red curve) and overall (blue curve)  process variation.
- Process capability (within): the Cp and its confidence interval's values, CpL, cpK, and its confidence interval's values.

    Process capability is determined by the variation that comes from common causes. It generally represents the best (potential) performance of the process. This is demonstrated when the process is being operated in a state of statistical control regardless the specifications [1].
    Capability index Cp compares the process capability to the maximum allowable variation as indicated by the tolerance.
    Cp is not impacted by the process location. This index can be calculated only for bilateral (two-sided) tolerances, it has no meaning for the one-sided situation (unilateral tolerance, with only one specification limit).
    Capability index Cpk takes the process location as well as the capability into account. Cpk will always be less than or equal to Cp.
    *Note*: Capability study reports contain meaningless numbers if processes are not in a state of statistical control.

- Process performance (total): the Pp and its confidence interval's values, PpL, PpU, PpK and its confidence interval's values, Cpm and its confidence interval's values.

    Process performance is the overall output of the process and how it relates to the requirements (defined by specifications).
    Performance index Pp compares the process performance to the maximum allowable variation as indicated by the tolerance [1].
    Pp is not impacted by the process location. This index can be calculated only for bilateral (two-sided) tolerances, it has no meaning for the one-sided situation (unilateral tolerance, with only one specification limit).
    Performance index Ppk takes the process location as well as the performance into account.
    Ppk will always be less than or equal to Pp.
    The Cpm index indicates how well the process can produce within specifications. Its calculation is similar to Cp, except that the standard deviation is calculated using the target value instead of the mean. The larger the Cpm, the more likely the process will produce output that meets specifications and is close to the target value.

- Non-conformance statistics: the observed of ppm outside the specification limits, the expected ppm outside the specifications limits based on the overall standard deviation and the within standard deviation.

    - ppm < LSL for observed performance is the actual number of parts per million (ppm) that have measurements that are less than the lower specification limit (LSL).
    - ppm > USL for observed performance is the actual number of parts per million (ppm) that have measurements that are greater than the upper specification limit (USL).
    - ppm total for observed performance is the actual number of parts per million (ppm) that have measurements that are outside the specification limits. ppm total equals the sum of ppm < LSL and ppm > USL.
    - ppm < LSL for expected overall performance is the expected number of parts per million (ppm) that have measurements that are less than the lower specification limit (LSL). Expected overall performance values are calculated using overall standard deviation. ppm < LSL for expected overall performance is 1,000,000 times the probability that the measurement of a randomly selected part from the overall process distribution is less than LSL.
    - ppm > USL for expected overall performance is the expected number of parts per million (ppm) that have measurements that are greater than the upper specification limit (USL). Expected overall performance values are calculated using overall standard deviation. ppm > USL for expected overall performance is 1,000,000 times the probability that the measurement of a randomly selected part from the overall process distribution is greater than USL.
    - ppm total for expected overall performance is the expected number of parts per million (ppm) that are outside the specification limits. Expected overall performance values are calculated using overall standard deviation. ppm total for expected overall performance is 1,000,000 times the probability that the measurement of a randomly selected part from the overall process distribution is outside the specification limits.
    - ppm < LSL for expected within performance is the expected number of parts per million (ppm) that have measurements that are less than the lower specification limit (LSL). Expected within performance values are calculated using the within-subgroup standard deviation. ppm < LSL for expected within performance is 1,000,000 times the probability that the measurement of a randomly selected part from the within-subgroup process distribution is less than LSL.
    - ppm > USL for expected within performance is the expected number of parts per million (ppm) that have measurements that are greater than the upper specification limit (USL). Expected within performance values are calculated using the within-subgroup standard deviation. ppm > USL for expected within performance is 1,000,000 times the probability that the measurement of a randomly selected part from the within-subgroup process distribution is greater than USL.
    - ppm total for expected within performance is the expected number of parts per million (ppm) that have measurements that are outside the specification limits. Expected within performance values are calculated using the within-subgroup standard deviation. ppm total for expected within performance is 1,000,000 times the probability that the measurement of a randomly selected part from the within-subgroup process distribution is outside the specification limits.



## References
-------
1.	Automotive Industry Action Group, *Statistical Process Control - Reference Manual* (July 2005, 2nd Edition)
2.	SKF Quality Techniques, Klerx, R., Dodson, B., and Dumont, D., QT 1 - *Process capability studies*. (PUB GQ/P9 10347/1 EN - December 2021)
3.	SKF Quality Techniques, Dodson, B., Lynch, D., Weidenbacher, M., and Klerx, R. (), *QT 2 - Statistical process control*, (PUB GQS/P9 18343 EN - April 2019)


## R Packages
-------
- ggplot2
- qcc
- jaspGraphs
- ggrepel
- FAdist
- goftest
- fitdistrplus
- mle.tools
- tidyr
- tibble
- EnvStats
- weibullness
