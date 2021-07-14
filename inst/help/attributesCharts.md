Attribute Charts
==========================
Control charts monitor the behaviour of a process to determine whether it is stable. They display data in time sequence in which the data occurred or produced.

If the data derived from the process are of a discrete nature (e.g. go/no-go, acceptable/not acceptable) then an attributes control chart should be used.

#### Assignment Box 
-------
- Defectives\Defects: the observations collected from a process.
- Sample: sample size of each observation. 

### Charts
-------
Defectives charts: In the case of defectives charts the product is either defective or not defective (binary).
- np chart: displays changes in the number of non-conforming (defective) products, rejects or unacceptable outcomes. The np chart is only valid when the data are collected in samples with the same size.
- p chart: displays changes in the proportion of non-conforming (defective) products, rejects or unacceptable outcomes. The p chart is only valid when the data are collected in samples with the varying size.
- Laney p' chart: displays changes in the proportion of non-conforming (defective) products and adjusts for overdispersion or underdispersion in the data.

Defects charts: In the case of defectives charts the product may have multiple defect per unit (non-binary).
- c chart: displays changes in the number of defects or non-conformities in a sample over time. The c chart is only valid when the data are collected in samples with the same size.
- u chart: displays changes in the proportion of defects or non-conformities in a sample over time. The u chart is only valid when the data are collected in samples with the varying size.
- Laney u' chart: displays changes in the proportion of defects or non-conformities in products over time and adjusts for overdispersion or underdispersion in the data.

X-mR chart: outlines the process (Individual) and process's moving range (MR) over time.

### R Packages
-------
- ggplot2
- qcc
- jaspGraphs
- ggrepel