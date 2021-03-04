Attribute Charts
==========================
Control charts are a set of tools used to model the variation of a process, thereby indicating its stability and establishing a state of statistical control.

Attribute charts are used for discrete data and model the stability of the process based on the sample size collected and the count of defectives\defects.

## Assumptions 
The assumptions for attribute charts are:
- np/c chart: the sample size used for data collection is equal.
- p/u chart: the sample size used for data collection is inequal.
- Laney p'/u': the data are overdispersed or underdispersed.

## Input
### Assignment Box 
-------
- Defectives\Defects: the observations collected from a process.
- Sample: sample size of each observation. 

## Output
### Charts
-------
Defectives charts: Defectives charts are used in products that are either defective or not defective (binary).
- np chart: plots the count of non-conforming (defective) products. 
- p chart: displays changes in the proportion of non-conforming (defective) products, rejects or unacceptable outcomes. 
- Laney p' chart: plots the variation adjusted proportion of non-conforming (defective) products.

Defects charts: In the case of defectives charts the product may have multiple defects per unit (non-binary).
- c chart: plots the count of defects or non-conformities in a sample over time. 
- u chart: plots the proportion of defects or non-conformities in a sample over time. 
- Laney u' chart: plots the variation adjusted proportion of defects or non-conformities in products over time.

X-mR chart: plots the process's values (Individual) and moving range (MR) over time.

## References
- Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2019).*Statistical process control*. SKF group. 

## R Packages
-------
- ggplot2
- qcc
- jaspGraphs
- ggrepel
