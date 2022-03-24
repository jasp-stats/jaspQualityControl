Attribute Charts
==========================
Control charts are a set of tools used to model the variation of a process, thereby indicating its stability and establishing a state of statistical control.

Attribute charts are used for discrete data and model the stability of the process based on the sample size collected and the count of defectives\defects.

## Assumptions 
-------
The assumptions for attribute charts are:
- np/c chart: the sample size used for data collection is equal.
- p/u chart: the sample size used for data collection is inequal.
- Laney p'/u': the data are overdispersed or underdispersed.

## Input
-------
### Assignment Box 
- Defectives\Defects: the observations collected from a process.
- Sample: sample size of each observation. 

## Output
-------
### Charts
Defectives charts: Defectives charts are used in products that are either defective or not defective (binary).
- np chart: plots the count of non-conforming (defective) products. 
- p chart: displays changes in the proportion of non-conforming (defective) products, rejects or unacceptable outcomes. 
- Laney p' chart: plots the variation adjusted proportion of non-conforming (defective) products.

Defects charts: In the case of defectives charts the product may have multiple defects per unit (non-binary).
- c chart: plots the count of defects or non-conformities in a sample over time. 
- u chart: plots the proportion of defects or non-conformities in a sample over time. 
- Laney u' chart: plots the variation adjusted proportion of defects or non-conformities in products over time.

X-mR chart: plots the process's values (Individual) and moving range (MR) over time.

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
- Duncan, A.J. (1986), Quality control and industrial statistics, Richard D. Irwin, Inc., and Automotive Industry Action Group (July 2005), Statistical process control (SPC) – Reference manual, AIAG.
- Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009).*Statistical process control handbook*. SKF group. 

## R Packages
-------
- ggplot2
- qcc
- jaspGraphs
- ggrepel
