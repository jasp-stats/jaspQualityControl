Attribute Charts
==========================

A Shewhart control chart is a graph used to study how a process changes over time. The obtained measurements can be either variables or attributes data, and subgroups are taken from the process at regular intervals. From each subgroup one or more statistical measures (such as average, dispersion, or proportion) are plotted in time order. Each chart always has a central line usually the average of the subgroup statistic or may be based on past experience, if the process is in a state of statistical control. The control chart has two statistically determined boundaries, one on either side of the central line, which are called the upper control limit (UCL) and the lower control limit (LCL). They are typically based on and placed at a distance of three times the standard deviation of the statistic being plotted. By comparing current data to these lines, conclusions can be drawn about whether the process variation is consistent (i.e., in control) or is unpredictable (i.e., out of control, and affected by special causes of variation). Control charts for variables data are always used in pairs. The top chart monitors the averages i.e., the centring of the data from the process. The bottom chart monitors the ranges i.e., the spread of the distribution. Control charts for attributes data consist of a single chart. Attributes data represent observations obtained by noting the presence or absence of some characteristic (attributes) in each of the items in the subgroup under consideration, then counting how many items do or do not possess the attribute, or how many such events occur in the item, group or area. Attributes data are generally rapid and inexpensive to obtain and often do not require specialised collection skills.

The assumed distribution for attributes data has only one independent parameter, the average level. The p and np control charts are based on the binomial distribution (for defectives), while the u and c control charts are based on the Poisson distribution (for defects).There are many types of count based data that cannot be characterised by either a binomial (p and np) or a Poisson (u and c) distribution. When such data are placed on p, np, u, or c control charts the limits based on these models would be wrong. The Laney p′ (p prime) and u′ (u prime) charts  are used to monitor attributes data if the sample sizes are variable and often very large. In such cases, it is common for the average proportion or rate to vary over the sampling period, a phenomenon known as overdispersion. Overdispersion means the sample-to-sample variation (i.e., the between sample variation) is much larger than the variation within the sample. Underdispersion is the opposite.

Unlike the attributes control charts, the X-mR control chart makes no assumptions about the relationship between the location and the dispersion parameter. It directly measures the location with the average and the dispersion with the moving ranges. Thus, while the p, np, u, and c control charts use “theoretical” limits, the X-mR control chart uses “empirical” limits. X-mR control charts are used if the sample sizes are reasonable constant. Laney control charts are used if sample sizes vary; if the sample size is constant the Laney control chart is exactly the same as the X mR control chart.


## Assumptions 
-------
The assumptions for attributes control charts are:
- p/np control chart are used if the observed data are approximately binomial distributed;
- u/c control chart are used if the observed data are approximately Poisson distributed;
- np/c control chart: the subgroup size used for data collection is constant;
- p/u control chart: the subgroup size used for data collection is either variable or constant;
- Laney p'/u': the observed data  cannot be modelled by either a binomial or a Poisson distribution and the subgroup size is variable.

## Input
-------
### Assignment Box 
- Defectives/Defects: the number of observations collected from a process that do or do not possess the attribute.
- Sample: sample size of each observation. 

## Output
-------
### Charts
Defectives charts: Defectives charts are used for products that are either defective or not defective (binary).
    - p chart, which charts the proportion of non-conforming (defective) products, rejects or unacceptable outcomes, or conforming/accepted items in each subgroup.
    - np chart, which charts the number of non-conforming (defective) products or only the conforming items in each subgroup.
    - Laney p' chart, which charts the variation proportion of non-conforming (defective) products in each subgroup with adjusted control limits.

Defects charts: Defects charts are used for products that have multiple defects per unit (the possible occurrences could be infinite).
    - u chart, which charts the proportion of defects or non-conformities in each subgroup. Subgroup size is variable.
    - chart, which charts the number of defects or non-conformities in each subgroup. Subgroup size is constant.
    - Laney u' chart, which charts the variation proportion of defects or non-conformities in products in each subgroup with adjusted control limits.

X-mR chart, which charts the process values (individuals) and moving range (mR) over time.


### Out-of-control Signals 
-------
 To indicate special cause variation the following unnatural patterns are used:

- Signal 1: One point beyond the control limits (sporadic issue). One point more than three standard deviations from the central line equivalent to violation of UCL or LCL.
- Signal 2: Seven consecutive points above or below the central line (mean shift). Seven points in a row on same side of centre line equivalent to a run above or below centre line.
- Signal 3: A run up or down of seven consecutive points (trend). Seven points  in a row all increasing or all decreasing equivalent to a run downward or upward.
- Signal 4: Two out of three consecutive points beyond the warning (two sigma) limits (increasing variation).
- Signal 5:Hugging the central line, this is indicated when 15 consecutive points are within the one sigma limits (reducing variation).
- Signal 6: Eight consecutive points are beyond the one sigma limits, regardless of which side of the central line (bimodal distribution).

The lack of control in both average and variation charts is generally found in the initial stages of setting up of control charts, sometimes called Phase 1 control charts. Using all six signals is only applicable for the average chart during Phase 1 when the control chart is used as an analysis tool. The first three signals are used always, for variables as well as attributes control charts.

In the case that the data of the individual and moving Range chart (X-mR chart) violate the pre assumption (normality and independently distributed) only signal 1 is applied. Autocorrelation is an indication of that.


## References 
-------

- Duncan, A.J. (1986), *Quality control and industrial statistics*, Richard D. Irwin, Inc.
- Automotive Industry Action Group, *Statistical Process Control – Reference Manual* (July 2005, 2nd Edition)
- SKF Quality Techniques, Klerx, R., Dodson, B., and Dumont, D., QT 1 – *Process capability studies* (PUB GQ/P9 10347/1 EN – December 2021)
- SKF Quality Techniques, Dodson, B., Lynch, D., Weidenbacher, M., and Klerx, R., QT 2 – *Statistical process control* (PUB GQS/P9 18343 EN – April 2019)
- International Organization for Standardization, *Control charts – Part 2: Shewhart control charts*, ISO 7870-2:2023 (E)


## R Packages
-------
- ggplot2
- qcc
- jaspGraphs
- ggrepel
