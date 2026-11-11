# jaspQualityControl Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
> 
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspModuleTemplate (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.). 
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)`). 
> * **Format Categories:** >   * **Added:** New template features, QML examples, or build tools.
>   * **Changed:** Updates to default configurations, boilerplate code, or dependencies. 
>   * **Fixed:** Bug fixes in the build pipeline, R wrappers, or QML layouts.
>   * **Deprecated / Removed:** Outdated template components or legacy code.


---

# jaspQualityControl (development version)

## Added
* Process Capability: optional Z.bench (ST) / Z.bench (LT) columns, reporting the sigma level based on the total expected defect probability across both specification limits, alongside the existing Z (ST) / Z (LT) = 3·Cpk / 3·Ppk.
* Process Capability / control charts: added "Pooled" as a within-subgroup standard deviation estimation method (in addition to R-bar and S-bar).
* DOE Analysis: optional confidence intervals for the effects and coefficients in the coefficients table ([Issue #4640](https://github.com/jasp-stats/jasp-issues/issues/4640)).
* DOE Analysis: the ANOVA table splits the Error row into Lack of fit (with F-test) and Pure error when the design has replicated runs, e.g. center points ([Issue #4640](https://github.com/jasp-stats/jasp-issues/issues/4640)).
* DOE Analysis, response optimizer: optional table with the predicted responses at the optimal solution (and at manually set factor levels), with standard error, confidence interval for the mean response and prediction interval for a single new run ([Issue #4640](https://github.com/jasp-stats/jasp-issues/issues/4640)).

## Changed
* DOE Analysis, Factorial Design and Response Surface Design: help moved from markdown files into the QML `info` properties.
* DOE Analysis: the contour plot is now drawn with ggplot2 instead of base graphics, so it renders correctly in interactive (plotly) mode, and always shows its legend. The "Show legend next to graph" option was removed; surface plots no longer show a color legend.

## Fixed
* Time weighted charts: singleton subgroups across multiple measurement columns now use moving ranges in row order, preserving entirely missing rows as gaps.
* Process Capability: missing moving ranges no longer discard the capability tables or the report. A control chart that cannot be drawn is left out with a note below the plot (or the plot shows the error if neither chart can be drawn); on the report, it is replaced by its error. Unavailable within standard deviations remain missing while overall estimates are retained.
* Time weighted charts, variables charts for individuals: when the std. dev. is estimated from moving ranges and a stage (or the data) has no complete moving range (fewer non-missing observations than the moving range length, or every range interrupted by a missing value), the chart shows an error naming the stage instead of stopping the analysis or drawing NaN limits. In the reports, only the affected chart is replaced by its error.
* Time weighted charts: the EWMA chart estimates the std. dev. of individual observations with missing values from moving ranges, like the CUSUM chart (previously it stopped with an error, or, with a grouping variable or across-rows data, drew zero-width control limits).
* Time weighted charts: after a subgroup without observations, the CUSUM and EWMA charts continue from the last observed subgroup with finite control limits (previously both lines stopped at the gap and the EWMA limits became infinite).
* Time weighted charts: notes about removed observations (not assigned to a stage or subgroup) are shown below the charts.
* Time weighted charts: the chart tables no longer stay in the output when the report is shown.
* Time weighted charts: the CUSUM moving range length field is shown based on the CUSUM (not the EWMA) std. dev. source.
* DOE Analysis: with "Display results in coded units", discrete factors with more than two levels were coded as one numeric -1/0/+1 column (1 df instead of k-1), which gave wrong ANOVA, lack-of-fit test, S, R², p-values and effects. They now stay factors (sum-to-zero contrasts), as in uncoded units, and the manual low/high table shows "Not available" for them and for blocks (whose low/high levels were already ignored). Levels without runs (filtered out or with missing values) are named below the coefficients table. Also affected the response surface ANOVA with "Use coded data to calculate squared terms" and stepwise selection on coded data ([Issue #4638](https://github.com/jasp-stats/jasp-issues/issues/4638)).
* DOE Analysis: with type II/III sums of squares, the ANOVA table's Model and Total rows no longer just add up the individual term SS (which is only valid for type I). Model SS is now SST − SSE, with F/p from the overall regression test, and Total SS is SST. The grouped rows (Linear, Squared, Interaction terms, Covariates) are now tested jointly and report MS, F and p.
* Test–retest: limit across-rows input to two measurement columns, reject extra single-column measurement sets (previously the third set was silently ignored) and report duplicate or unmatched readings; rows with missing values are still excluded.
* Attribute agreement: return valid confidence-interval endpoints for zero or perfect agreement.
* Probability of detection: identify natural-log coordinates in covariate labels, reference tables and explanatory footnotes.
* Attribute charts: allow defect rates above one and reject defective-item counts exceeding the sample size.
* Process Capability: observed nonconformance rates and changes from baseline now use each stage's valid measurement count when stage sizes differ.
* Attribute charts: label u and Laney u axes as defects per unit instead of proportions.
* Multivariate control charts: keep exported T² values aligned with their original rows when incomplete observations are omitted.
* Multivariate control charts: fix Phase II errors and missing observation labels caused by automatic naming.
* Control charts and process capability: prevent overflow when calculating bias-corrected standard deviations.
* Time weighted charts: the exponentially weighted moving average chart now offers S-bar and R-bar as std. dev. estimation method for data in wide format ("Across rows"), instead of only X-mR, which stopped the analysis with two or more measurement columns.
* Rare event charts: the interval type "Time" no longer always fails with "Date/time conversion returned no valid values", and its intervals are always converted to minutes, so the chart no longer labels intervals given in hours as minutes.
* Stepwise model selection in the Response Surface design now respects marginality: a squared term is no longer retained without its corresponding main effect.
* Process Capability: distribution/fit density curves no longer render flat in interactive (plotly) plots; the curves are now drawn with `geom_line` instead of `stat_function`.
* Process Capability: the x-axis of the capability plot is no longer stretched towards the default historical scale (Weibull) or log mean (log-normal) when no historical parameter is set, which squeezed the histogram and fitted density into a narrow strip for data far from zero.
* Process Capability: the x-axis of the Weibull and log-normal probability plots no longer stretches far beyond the data, which squeezed the points into a thin strip and made the tick labels overlap. The axis now follows the data, and ticks that would crowd each other are dropped.
* Attributes agreement analysis: Fleiss' kappa no longer crashes with newer `irr` (>= 0.85) when a within-appraiser input has a single measurement column; returns NaN as before.
* DOE Analysis: the Effect column of the coefficients table is now 2 × coefficient (on the −1/+1 scale) for main effects and interactions of every order. Previously 2-way and 3-way interactions were inflated (4× and 6×), effects depended on the factor level labels, and uncoded continuous predictors and manual coding beyond ±1 gave wrong values. Effects are left blank for squared terms and factors with more than two levels, and a footnote explains the definition ([PR #525](https://github.com/jasp-stats/jaspQualityControl/pull/525)).
* DOE Analysis: no longer crashes when a discrete factor is fully removed from the model due to aliasing ([PR #525](https://github.com/jasp-stats/jaspQualityControl/pull/525)).
* DOE Analysis: the response optimizer now uses the observed maximum (maximize) or minimum (minimize) as the default target when "Set all bounds manually" is on and "Set all targets manually" is off. Previously the target silently became the value of the disabled Upper/Lower field (1 or 0), which distorted the desirability. Settings without a defined desirability (maximize with the lower bound above the target, minimize with the target above the upper bound, target outside manual bounds, lower above upper) now give an error instead of a misleading optimum. A target outside bounds taken from the data gets a footnote, and a bound equal to the target no longer gives NaN. The optimizer now starts from the first run and the three design points with the highest desirability instead of only the first run, so it no longer stalls at desirability 0 when a design point already reaches the bounds. A footnote flags a solution with desirability 0; acceptable settings that lie between the design points can still be missed. The optimizer is no longer rerun when unrelated options change. Analyses with a single predictor no longer crash, the settings footnote no longer reports an entered target as estimated from data, no empty "Optimization Plot" heading remains after a settings error, "Show optimal solution" now also hides the settings table, and an optimum on the edge of the design space is no longer flagged as extrapolation ([PR #541](https://github.com/jasp-stats/jaspQualityControl/pull/541)).
* DOE Analysis, response optimizer: with blocks or covariates in the model, the fitted responses and desirabilities added every block and covariate coefficient in full. Predictions are now averaged over the blocks and covariates are held at their mean, matching the new prediction table ([Issue #4665](https://github.com/jasp-stats/jasp-issues/issues/4665)).
* DOE Analysis, response optimizer: with two or more discrete factors, the optimization plot, its summary table and the solution table's fitted response gave some factor levels no contribution (e.g. the last level of the first factor), because the factor levels were merged. The optimal settings themselves were not affected ([Issue #4665](https://github.com/jasp-stats/jasp-issues/issues/4665)).
* DOE Analysis: block and discrete factor levels without runs (e.g. all their rows filtered out or with missing responses) are dropped. Such a level made the model fall back from sum-to-zero to treatment contrasts, which the effects and the response optimizer assume.
* Gauge r&R: the gauge evaluation table of the report shows the name of the operator variable instead of its internal (encoded) column name.
* Control charts: the CL, LCL and UCL labels of values of 1000 or more keep all their decimals (e.g. 1908.333 instead of 1908.33); the shortened labels could also be rounded wrong (2092.49 instead of 2092.495).
* Process Capability, stages: the Cpm confidence interval is no longer blanked for the stages after the baseline, and the change row no longer shows the difference of the interval bounds as an interval.
* Process Capability, stages: the change (vs. baseline) rows are computed from the unrounded values, so they are the rounded difference; the difference of the rounded values could be off in the last decimal.

---

# jaspModuleTemplate 0.2.0
## Added
* Added NEWS.md
* Added workflow to remind users to update their `NEWS.md`.
* Added workflow to auto-bump version when user does not do so.

---

# jaspModuleTemplate 0.1.0

## Added
* Initial examples to showcase JASP module development

## Changed
* Use best practices for checking input ([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)).
* The main results table now defaults to displaying 95% Confidence Intervals for effect sizes.

## Fixed
* Remove deprecated dependencies from qml files ([Issue #14](https://github.com/jasp-stats/jaspModuleTemplate/issues/14)).
