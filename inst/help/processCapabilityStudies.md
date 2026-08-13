Process capability Studies
==========================
A process capability study is the analysis of a process in order to determine whether the process under study is capable of providing good quality products. This analysis uses data from an initial run of parts to predict whether a manufacturing process can repeatably produce parts that meet specifications.
A subgroup is a group of pars that are produced under the same set of conditions.
Rational subgroup: "A subgroup gathered in such a manner as to give the maximum chance for the measurements in each subgroup to be alike and the maximum chance for the subgroups to differ from the other. This subgrouping scheme enables a determination of whether the process variation includes special cause variation." [1]


## Input
-------
### Data Type
- **Measurement data (variables)**: each observation is a continuous measurement that is compared against specification limits. This is the analysis described in the rest of this page and produces Cp/Cpk/Pp/Ppk.
- **Pass/fail counts (attributes)**: each inspected unit is either good or defective, and the data are counts of defective units per sample. There are no specification limits; capability is expressed as %Defective, PPM defective and Process Z. See "Binomial Capability Analysis" below.

Variables that were assigned in the other data type stay assigned when you switch, but they are not used by the active analysis.

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

#### Data transformation
- Transform data before any analysis.
  - **None**: The data is analysed as-is
  - **Box-Cox**: The data is transformed using the equation $y = (x+\text{shift})^\lambda$ if $\lambda \neq 0$, otherwise $y = \ln(x + \text{shift})$.
  - **Box-Cox (auto)**: The data is transformed using the Box-Cox transformation, but with the $\lambda$ parameter automatically estimated using one of the methods specified (see 'Method').
  - **Yeo-Johnson**: The data is transformed using the Yeo-Johnson transform as described in Yeo & Johnson (2000). It can be used for unbounded data.
  - **Yeo-Johnson (auto)**: The data is transformed using the Yeo-Johnson transform, but with the $\lambda$ parameter automatically estimated using the profile likelihood of a normal distribution. This procedure allows only process performance results (no process capability).
  - **Johnson**: The data is transformed using the Johnson transform. It can be used for unbounded data (but some forms of the transform will impose restrictions on the specification limits). The transform is fully automatic, as described in Chou, Polanski, & Mason (1998). This procedure allows only process performance results (no process capability).

- **Shift** numerical value of the shift parameter used for transforms that accept bounded data. This option is disabled for unbounded transforms (Yeo-Johnson, Johnson)
- **Lambda** numerical value of the $\lambda$ parameter of the transforms. This option is disabled for transforms which automatically estimate their parameter(s).
- **Method** method for selecting the best $\lambda$ value. 'Log-Lik' maximizes the normal-likelihood of the transformed variable. 'SD' minimizes the sums of squares of the power-transformed variable. 'Average moving range' minimizes the estimate of variabiliy based on the average moving range of the power-transformed variable. 'Log-Lik' and 'Sd' are appropriate for grouped data, 'Average moving range' is appropriate for individual's data.
- **Continuity Adjustment** if enabled, the Box-Cox transform includes the adjustment term $y = \frac{(x+\text{shift})^\lambda-1}{\lambda}$.

#### Type of data distribution
- Type of data distribution: indicate whether the data approximates a normal distribution or another distribution (the available distributions are: Weibull, Lognormal, Gamma, Exponential, Logistic, Log-logistic, 3-parameter Weibull, and 3-parameter lognormal)
    - Specify a distribution: the non-normal distribution to be used. 
    - Non-normal capability statistics: the method used to calculate the capability statistics for non-normally distributed data.
    - Historical parameters: Select which parameters should use fixed historical values instead of being estimated. Note that if not all parameters are set historically, JASP keeps the historical parameters fixed while estimating the remaining parameters freely.

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
- Process summary (only displayed if one or both specification limits are filled in): Produces a table of statistics consisting of the lower specification limit, target, upper specification limit, sample size, overall average, overall (total) standard deviation, and the within (subgroup) standard deviation. The within standard deviation is estimated according to the selected method (R-bar, S-bar, or Pooled). Pooled standard deviation is a common default for capability analysis and is the most efficient estimate of the within-subgroup variation, especially for larger or unequal subgroups.
- Capability of the process: plots the frequency distribution with the fitted distribution based on the overall (total) process variation and the inherent (within) process variation. It compares the process spread to the customer requirements (the upper and lower specification limits displayed as red vertical lines). The two curved lines indicate how well the selected distribution fits to the data. The two distributions are based on the within (red curve) and overall (blue curve)  process variation.
- Process capability (within): the Cp and its confidence interval's values, CpL, cpK, and its confidence interval's values.

    Process capability is determined by the variation that comes from common causes. It generally represents the best (potential) performance of the process. This is demonstrated when the process is being operated in a state of statistical control regardless the specifications [1].
    Capability index Cp compares the process capability to the maximum allowable variation as indicated by the tolerance.
    Cp is not impacted by the process location. This index can be calculated only for bilateral (two-sided) tolerances, it has no meaning for the one-sided situation (unilateral tolerance, with only one specification limit).
    Capability index Cpk takes the process location as well as the capability into account. Cpk will always be less than or equal to Cp. Z (ST) is equal to 3 * Cpk and corresponds to the distance of the process mean to the nearest bound in standard deviation units. Z.bench (ST) is the short-term sigma level computed from the total expected defect probability across both specification limits (based on the within standard deviation), i.e. the standard normal quantile of one minus that probability. It equals Z (ST) only for one-sided tolerances; for two-sided tolerances it is smaller because both tails contribute.
    *Note*: Capability study reports contain meaningless numbers if processes are not in a state of statistical control.

- Process performance (total): the Pp and its confidence interval's values, PpL, PpU, PpK and its confidence interval's values, Cpm and its confidence interval's values.

    Process performance is the overall output of the process and how it relates to the requirements (defined by specifications).
    Performance index Pp compares the process performance to the maximum allowable variation as indicated by the tolerance [1].
    Pp is not impacted by the process location. This index can be calculated only for bilateral (two-sided) tolerances, it has no meaning for the one-sided situation (unilateral tolerance, with only one specification limit).
    Performance index Ppk takes the process location as well as the performance into account.
    Ppk will always be less than or equal to Pp. Z (LT) is equal to 3 * Ppk and corresponds to the distance of the process mean to the nearest bound in standard deviation units. Z.bench (LT) is the long-term sigma level computed from the total expected defect probability across both specification limits (based on the overall standard deviation), i.e. the standard normal quantile of one minus that probability. It equals Z (LT) only for one-sided tolerances; for two-sided tolerances it is smaller because both tails contribute.
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



## Binomial Capability Analysis
-------
Selected with **Data type → Pass/fail counts (attributes)**. Every inspected unit is classified as either good or defective, and the data consist of the number of defective units $D_i$ found in a sample of $n_i$ inspected units. Because there is no measurement and no specification limit, capability is not expressed as Cp/Cpk/Pp/Ppk but as the percentage of defective units, the equivalent number of defectives per million and the corresponding sigma level.

Stages are not supported in this mode.

### Assignment Box
- **Defectives**: the number of defective units found in each sample.
- **Sample size (Total)**: the number of units inspected in each sample. Only used when the sample size is set to "Variable".
- **Timestamp (optional)**: labels for the samples, used on the x-axis of the p chart and in the test results table.

### Options
- **Sample size**: choose "Constant" and enter the number of units inspected in every sample, or choose "Variable" and assign a column holding the sample size. This choice also decides which of the two sample-size dependent panels is offered ("Rate of defectives" for variable sizes, "Distribution of defective (%)" for a constant size), so the label of the check box always matches what is drawn.
- **Historical proportion defective (%)**: use a known proportion as the centre line of the p chart instead of estimating it from the data. This changes the chart only. The summary statistics are always estimated from the observed data, so that the reported estimate stays inside its own confidence interval.
- **Target defective (%)**: a target percentage, reported in the summary table and drawn as a reference line in the cumulative plot and in the histogram.
- **Confidence interval** and **Interval method**: the level and the method used for the intervals in the summary table and for the band of the cumulative plot.

### Scale convention
Everything is reported in percent, with one deliberate exception: the y-axis of the p chart is a proportion, following the usual SPC convention and matching the p chart of the *Control Charts for Attributes* analysis.

### Output
- **p chart**: the observed proportion defective $\hat{p}_i = D_i / n_i$ per sample with the centre line at $\bar{p} = \sum D_i / \sum n_i$ and control limits at $\bar{p} \pm k\sqrt{\bar{p}(1-\bar{p})/n_i}$, where $k$ is the number of standard deviations set under Advanced options. Because the limits depend on $n_i$, they step up and down whenever the number of inspected units changes: a smaller sample gives a less precise estimate and therefore wider limits. Limits are clamped to the interval $[0, 1]$. Only the run based tests (beyond limit, shift, trend, oscillation) are applied; the zone based tests are not, because the zones of a clamped, asymmetric chart do not correspond to actual sigma multiples.
- **Cumulative defective (%)**: the running estimate $\sum_{j \le i} D_j / \sum_{j \le i} n_j$ with a confidence band, the overall estimate as a horizontal line and, if set, the target. The band shows whether enough samples were collected for the estimate to settle.
- **Binomial plot**: the observed number of defectives against the expected number, with the diagonal $y = x$ as reference. The expected number is $n_i$ times the centre line proportion of the p chart, that is $n_i \bar{p}$, or $n_i$ times the historical proportion when one is set. Points scattering around the diagonal support the binomial assumption.
- **Rate of defectives**: the percentage defective against the sample size. A trend indicates that the percentage defective depends on how many units were inspected.
- **Distribution of defective (%)**: a histogram of the percentage defective across samples. If a target is set, it is drawn as a dashed vertical line, so a distribution sitting to the right of the line marks samples worse than the target. **Number of bins** sets the suggested number of bins; the boundaries are rounded to readable values, so the histogram can end up with a slightly different number of bins.
- **Summary statistics**:
    - **Defective (%)** $= 100\,\bar{p}$
    - **PPM defective** $= 10^6\,\bar{p}$
    - **Process Z** $= \Phi^{-1}(1 - \bar{p})$, the standard normal quantile corresponding to the estimated proportion defective. Larger is better. It is unbounded when no defectives, or only defectives, were observed; in that case only the confidence bound is informative.
    - Confidence intervals are computed on $\bar{p}$ and transformed to the PPM and Z scale. Because Z decreases in $p$, the upper bound of $p$ gives the lower bound of Z. Three methods are available: **exact** (Clopper-Pearson, the default and the most conservative), **Wald** (normal approximation, unreliable for small counts) and **Wilson** score.
    - A footnote warns when the p chart shows out-of-control points, because a capability estimate from an unstable process is not representative of future output.

### Assumptions
- Units are inspected independently of one another.
- The probability that a unit is defective is constant within a sample.
- The number of inspected units per sample is known.
- The process is in statistical control. If the p chart flags points, the capability estimate describes the observed data but does not predict future output.

### Relation to *Control Charts for Attributes*
That analysis also produces a p chart, and the two do not have to agree:
- This analysis always draws stepped control limits computed from the individual sample size $n_i$.
- *Control Charts for Attributes* replaces the stepped limits with constant limits computed from the mean sample size whenever $\min(n)/\max(n) \ge 0.75$.
- The two use different out-of-control rule engines, so they can flag different points.

Use this analysis when you want capability statistics alongside the chart, and *Control Charts for Attributes* when you want np, c, u or Laney charts.

## References
-------
1.	Automotive Industry Action Group, *Statistical Process Control - Reference Manual* (July 2005, 2nd Edition)
2.  Yeo, I. K., & Johnson, R. A. (2000). A new family of power transformations to improve normality or symmetry. Biometrika, 87(4), 954-959.
3.  Chou, Y. M., Polansky, A. M., & Mason, R. L. (1998). Transforming non-normal data to normality in statistical process control. Journal of Quality Technology, 30(2), 133-141.


## R Packages
-------
- ggplot2
- qcc
- jaspGraphs
- ggrepel
- FAdist
- goftest
- fitdistrplus
- tidyr
- tibble
- EnvStats
- weibullness
