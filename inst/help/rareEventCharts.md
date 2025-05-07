Rare Event Charts
==========================
Two very effective alternatives to the Shewhart control chart may be used when monitoring rare events or when small process shifts are of interest: the g chart and the t chart. These charts are particularly useful for situations where events occur infrequently and traditional Shewhart control charts may not be as effective. The g and t charts belong to the broader category of rare event control charts and are designed to handle scenarios where monitoring time between events or the number of opportunities between events is more meaningful than tracking continuous data points.

- **G Chart**: Used to monitor the number of opportunities (such as units produced or time intervals) between rare events. It is particularly effective in processes where defects are infrequent and the main interest is in the distance between these occurrences.
  
- **T Chart**: Used to monitor the time between rare events. This chart is suitable for processes where time intervals are of interest, such as monitoring the time between machine breakdowns, customer complaints, or other rare events.

The g and t charts are essential tools in Statistical Quality Control (SQC) when dealing with rare events, providing a more sensitive approach to detect shifts in processes where events do not occur frequently enough for traditional methods to be effective.

## Input
--------

### Assignment box
--------
- Variable: either the timepoint when the event took place or the intervals beetween events.
- Stages: a column to split the analysis into multiple parts by assigning a stage to each subgroup.

### Data type
-------
- Date/time: if your data specify a timepoint at which the event took place.
    - Structure: the structure of your timepoints, either only dates (e.g., 01/12), only time (e.g., 12:30), date and time (e.g., 01/12 12:30), or time and date (e.g., 12:30 01/12).
    - Date format: the date format in your data, where D = day, M = month, Y = year, thus, DMY = Day Month Year, for example 30/12/2024. The symbol used as separator does not matter. 
    - Time format: the time format in your data, where H = hour, M = minute, S = second, thus, HMS = Hour Minute Second, for example 01:02:03. The symbol used as separator does not matter. Ip and IpM refer to integer hours and integer hours with minutes, for example, 12pm or 12:30pm.
- Interval between events: if your data specify the time or number of opportunities between events.
    - Interval type: the unit in which your interval is expressed. Opportunities, hours (decimal) and days (decimal) are read as is, where times are treated as decimal, so 1.25 hours = 1 hour 15 minutes. If you select time as interval type, you will need to specify the time format, as explained above.

### Chart options
--------
- G chart: select to display a G chart.
    - Proportion estimated from data: the proportion is calculated from the data and used for all other calculations of the G chart. 
    - Proportion historical: a historical proportion value is used for all other calculations of the G chart.

- T chart: select to display a T chart.
    - Based on Weibull/exponential distribution: the distribution function that used to calculate the control limits in the T chart.
    - Distribution parameters estimed from data: the distribution parameters for the Weibull/exponentital distribution are calculated from the data (best fit) and used for all other calculations of the T chart. 
    - Distribution parameters historical: historical distribution parameters for the Weibull/exponentital distribution are provided and used for all other calculations of the T chart. 

## Output
--------
### Charts
--------
- G chart: A graphical representation showing the number of time units or opportunities between rare events, highlighting any deviations from expected intervals that might indicate a process shift.
  
- T chart: A graphical representation showing the time intervals between rare events, allowing for easy identification of shifts in the process that could signify increased risk or process degradation.

### Out-of-control Signals 
-------

#### JASP Default Tests

- Test 1: One point beyond the control limits (sporadic issue).

#### Custom Test Selection
Select manually which tests you want to apply and modify them as desired:

- Test 1: N points beyond the control limits (sporadic issue).
- Test 2: N consecutive points above or below the central line (mean shift).
- Test 3: A run up or down of N consecutive points (trend).
- Test 8: N points in a row are alternating increase and decrease (oscillation).
- Test 9: N points in a row are equal to 0 (Benneyan test).



## References
--------
- Montgomery, D. C. (2009). *Introduction to Statistical Quality Control*. John Wiley & Sons.

## R Packages
-------
- ggplot2
- qcc
- jaspGraphs
- ggrepel
