Bayesian Gauge r&R
==========================
Gauge Repeatability and Reproducibility (Gauge R & R) is an analysis aimed at defining the amount of variation in measurements given a measurement system. 
The variation detected in the measurement is sourced in two factors, repeatability (equipment variation) and reproducibility (operator variation). 
This analysis offers a Bayesian implementation of Gauge R & R. 

## Input
### Data Format
-------
Data can be in the form of all observations in one column ("Single column") or across rows with a subgroup index ("Across rows").

### Assignment Box
- Operators: the operators in the measurement system. 
- Parts: the parts of the measurement system.
- Measurements: the observations/data collected from a process.

### Gauge r&R Method
The method used in the analysis. 
- Linear random effects model: The analysis is based on a linear random effects model, also called random effects ANOVA (for more information about the model, see Rouder et al., 2012).

### Options
#### Analysis options 
- Estimation: either automatic based on a cut-off Bayes factor (BF) or a manual choice between main effects only or full model.
- Cut-off BF: cut-off for the BF in favor of the full model that needs to be reached for the full model to be chosen over the model only including main effects in automatic estimation.
- Process variation reference: either a historically known standard deviation (Historical standard deviation) or estimated from the data (Study variation).
- Tolerance: include a value for tolerance. 
- r&R table options: 
    - Study Var. multiplier type: multiplier based on either Std. Deviation or Percent. 
    - Study Var. multiplier value: value for the multiplier.

#### Plots
- Prior: g-prior used in the analysis. 
- Posterior: posterior distribution on variances, %Contribution, %Study variation or %Tolerance
    - Display histogram: plot histogram of Markov Chain Monte Carlo (MCMC) samples.
    - Point estimate: display posterior mean, median or mode.
    - CI: display central, highest posterior density or custom credible interval.
    - Mass: percentage of posterior mass that the credible interval covers. For custom intervals, bounds can be specified.
- Contour plot: 95% contour of a multivariate normal distribution based on sample measurement mean and average posterior part and total variation (Mader et al., 1999).
- Components of variation: bar chart for components of variation with 95% credible intervals.
- Range charts by operator: control chart on the range.
- Average chart by operator: control chart on the mean.
- Scatter plots by operators: scatterplot matrix for operators.
- Measurements by part plot: average measurement for each part.
    - Display all measurements: individual measurements are shown.
- Measurements by operator plot: boxplots of measurements by operator.
- Part x operator interaction plot: average measurement for each part with separate lines for operators.
- Traffic light chart: traffic light plots for %Study variation and %Tolerance with 95% credible intervals.

#### MCMC diagnostics
- Diagnostics table: table with statistics.
- Plots: diagnostic plots.
    - Traceplot
    - Autocorrelation
    - Density

#### Advanced options
- Priors
    - r scale prior: square root of the scale for the inverse chi-square g-prior.
- MCMC options
    - Chains: number of MCMC chains to run.
    - Iterations per chain: iterations for each chain. 
    - Burn-in per chain: number of initial samples discarded from each chain.
- Repeatability
    - Set seed: specify seed to keep results constant.
- Distribution fit to MCMC samples: distritbution that is fit to the MCMC samples for the posterior plots.
    - For variance posteriors, either a generalized inverse Gaussian or a metalog distribution.
    - For all other posteriors, the metalog is used.   

#### Report options
- Create a gauge R & R report. 

## Output 
-------
- Model Comparison: 
    - BF<sub>10</sub>: shows the Bayes factor in favor of the full model compared to the model in each row.
    - error %: percent error in BF estimation.
- Variance Components:
    - Mean: posterior mean
    - Std. Deviation: standard deviation of the posterior distribution.
- % Contribution to Total Variation: posterior summaries for percent contribution of each component to the total variation.
- Standard Deviation & Study Variation: posterior summaries for standard deviations and study variation.
- % Study variation & % Tolerance: posterior summaries for percent of study variation and tolerance.
- MCMC diagnostics:
    - ESS (Bulk): effective sample size in the bulk of the distribution.
    - ESS (Tail): effective sample size in the tails of the distribution.
    - Rhat: convergence diagnostic that compares between- to within-chain variance. Values larger than 1.01 indicate potential problems (Vehtari et al., 2021).
    - MCSE: markov chain standard error for the estimator indicated in parentheses.
- Posterior distributions
    - Posterior Summary: posterior summaries based on the distribution fit to the MCMC samples.
- Contour plot: posterior summaries for producer's and consumer's risk.

## References 
-------
- Duncan, A.J. (1986), Quality control and industrial statistics, Richard D. Irwin, Inc., and Automotive Industry Action Group (July 2005), Statistical process control (SPC) – Reference manual, AIAG.
- Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009). *Statistical process control handbook*. SKF group. 
- Mader, D. P., Prins, J., & Lampe, R. E. (1999). THE ECONOMIC EVIPACT OF MEASUREMENT ERROR. *Quality Engineering, 11*(4), 563–574. https://doi.org/10.1080/08982119908919276
- Montgomery, D. C. (2013). *Introduction to statistical quality control* (7th ed.). John Wiley
& Sons.
- Rouder, J. N., Morey, R. D., Speckman, P. L., & Province, J. M. (2012). Default Bayes factors for ANOVA designs. *Journal of Mathematical Psychology, 56*(5), 356–374. https://doi.org/10.1016/j.jmp.2012.08.001
- Stan Development Team. 2024. Stan Reference Manual, 2.36. https://mc-stan.org
- Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C. (2021). Rank-normalization, folding, and localization: An improved $\widehat{R}$ for assessing convergence of MCMC. *Bayesian Analysis, 16*(2). https://doi.org/10.1214/20-BA1221

## R Packages
-------
- jaspGraphs
- jaspBase
- ggplot2
- tidyr
- dplyr
- BayesFactor
- ellipse
- mvtnorm
- rmetalog
- GeneralizedHyperbolic
- HDInterval
- extraDistr
- posterior
- rstan
- bayesplot