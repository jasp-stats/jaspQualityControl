# The Quality Control Module

Investigate if a manufactured product adheres to a defined set of quality criteria.

## Core Functionality

The Quality Control module bundles several `R` packages for quality control into a general interface.

## Module Structure

The analyses in the Quality Control module are structured in JASP in the following way:

```
--- Quality Control
    -- Measurement Systems Analysis
       - Type-1 Study
       - Linearity Studie
       - Gauge r&R
       - Gauge r&R (Non-Replicable Measurements)
       - Attributes Agreement Analysis
    -- Control Charts
       - Attributes Charts
       - Variables Charts for Subgroups
       - Variables Charts for Individuals
       - Time Weighted Charts
    -- Capability Studies
       - Process Capability Studies
    -- DOE
       - Two-level Factorial Design
       - Response Surface
```

<div align="right">

[![Unit Tests](https://github.com/jasp-stats/jaspQualityControl/actions/workflows/unittests.yml/badge.svg)](https://github.com/jasp-stats/jaspQualityControl/actions/workflows/unittests.yml)
[![codecov](https://codecov.io/gh/jasp-stats/jaspQualityControl/branch/master/graph/badge.svg)](https://codecov.io/gh/jasp-stats/jaspQualityControl)
<br>
<b>Maintainer:</b> <a href="https://github.com/JTPetter/">Jonas Petter</a>

</div>