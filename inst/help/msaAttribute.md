Attribute Agreement Analysis
==========================
Attribute Agreement Analysis is used to deem the agreement between operators' and known standard's ratings. 
One of the aims is thus to determine the accuracy of the investigated operators. 

## Input
### Data Format
-------
Data can be in the form of all observations in one column ("Column" option) or across rows with a subgroup index ("Row" option).

### Assignment Box
- Operators: the operators in the measurement system. 
- Parts: the parts of the measurement system.
- Result: The ratings made by the operators. 
- Standard (optional): the ratings by the known standard. 

### Options
#### Kappa Studies (Binary Data)
- Cohen's kappa (interrater kappa): compute Cohen's kappa per operator. 
- Fleiss's kappa (multirater kappa): compute Fleiss's kappa per operator. 
- Positive reference: the positive reference used for the rating classifications (for example, "Yes", "fit", "Good").

#### Tau Studies (Ordinal Data)
- Kendall's tau: compute the correlations between the operators and their ratings.

## Output 
-------
### Tables 
- Study effectiveness summary: Effectiveness, Miss rate, and False alarm rate per operator with an evaluation of acceptance.
- Within Appraisers: matching between the inspected items and operators, with a 95% confidence interval. 
- Each Appraiser vs Standard: matching between the known standard and inspected items per operator, with a 95% confidence interval.    
- Between Appraisers: matching between the different operators, with a 95% confidence interval.  
- All Appraisers vs Standard: matching between all operators and known standard, with a 95% confidence interval.  

### Plots
- Within Appraisers: plotting the matching's percentage within appraiser and its confidence intervals. 
- Each Appraiser vs Standard: plotting the matching's percentage between the known standard and inspected items per operator and its confidence intervals. 

## R Packages
-------
- jaspGraphs
- ggplot2
- tidyr
- psych
- irr