Analyse Design
==========================

The purpose of analysing both factorial and response surface designs is to identify influential factors and optimal response conditions.  This analysis helps uncover which variables significantly impact the outcome and how they interact. It allows researchers to systematically optimize processes and improve product quality. Ultimately, it enables the achievement of better performance and efficiency.

## Design type
-------
Select the type of experimental design you want to analyse.

### Factorial design
A factorial design is an experimental setup used to study the effects of multiple factors by varying them simultaneously. Each factor is tested at different levels and in different combinations with other factors. This design allows for the examination of both the main effects of each factor and the interactions between factors. It provides a systematic approach to understanding how different variables influence the outcome.

### Response surface design
A response surface design is an experimental framework used to optimize and understand the relationships between several explanatory variables and one or more response variables. By systematically varying the input variables, researchers can explore the effects of these variables on the response, identify optimal conditions, and understand the interactions between variables. Response surface designs are particularly useful in situations where the goal is to optimize a process or product, as they provide a detailed map of the response landscape, enabling precise fine-tuning and improvement.

## Input
-------

### Assignment box
- Response: The measured outcome variable of the process under investigation.
- Discrete predictors: The columns corresponding to the discrete predictors in the design.
- Continuous predictors: The columns corresponding to the continuous predictors in the design.
- Covariates: The columns corresponding to the covariates in the design. The difference between a covariate and a continuous predictor is that the covariate will not be analysed for interaction effects and excluded from effect plots.
- Blocks: The columns corresponding to the blocks in the design. The difference between a block variable and a discrete predictor is that the blocks will not be analysed for interaction effects and excluded from effect plots.

## Analysis options
-------

### Predictor levels
For the ordering of contrasts and to identify the alpha points in a response surface design, the low and high levels of all predictors need to be specified. There are two options to handle this.

- Automatically detect low/high: Attempts to automatically detect the low and high levels by taking the minimum and maximum for continuous predictors, and by ordering discrete predictors alphabetically. For response surface designs, this might recognize alpha values as low/high levels. In this case, manual specification is needed.

- Manually specify low/high: Shows all predictors in the analysis to allow for manual specification of the low and high levels.

### Other analysis options

- Use alias names: Check to assign alias names to predictors. Useful when predictors have long names.
- Show regression equation: Check to display the regression coefficients as an equation predicting the response.
- Display result in coded units: Check to display the results in coded units. This means, all predictor levels are standardized between -1 and 1.
- Show optimal response: Check to display the predictor levels yielding the optimal response.

## Model options
-------

There are different options available to determine the terms that should be added to the model.

### Select predefined model / highest order interaction term
Check to use one of the predefined models offered for analysing RSM designs, or to define the model based on the highest order interaction terms to include.

### Define manually
Allows manually defining the terms in the model. When analysing RSM designs, a second assignment box for defining squared terms is available.

## Plots
-------

### Residual plots
- Normal probability plot: Check to show a normal probability plot of the residuals.
- Histogram: Check to show a histogram of the residuals.
- Residuals vs. fitted values: Check to show a plot of the residuals per fitted value.
- Residuals vs. run order: Check to show a plot of the residuals per run order value.
- Residuals four-in-one plot: Check to show all of the above plots in a 2x2 matrix.

### Other plots
- Pareto plot of effects: Check to show a pareto plot of the standardized effects.
- Normal plot of effects: Check to show a normal probability plot of the standardized effects.
- Contour/surface plot: Check to show a contour (2D) or surface (3D) plot.
 - Show legend: Check to show a legend next to the plot.
 - Divide surface into N parts: Select the number of division in the surface of the contour/surface plot.

## Advanced options
-------
- Histogram bin width type: Select the method used to calculate the bin-widths or specify a manual number of bins.
- Sums of squares type: Select the method use to calculate the sums of squares.