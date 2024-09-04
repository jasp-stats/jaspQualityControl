Factorial Design
==========================

A factorial design is an experimental setup used to study the effects of multiple factors by varying them simultaneously. Each factor is tested at different levels and in different combinations with other factors. This design allows for the examination of both the main effects of each factor and the interactions between factors. It provides a systematic approach to understanding how different variables influence the outcome.

## Display design
-------
Check to display the design table in the output. One of the pre-set designs needs to be selected in the "Design Table".

- Coded units: Whether to display the levels of the predictor in the given units or in coded units.
- Sort by run/standard order: Whether to sort the runs in the design by run order or standard order.

## Save design
-------
Option to save the design as a .csv file. Specify a name and path for the file or use the browse option. Check "Export design" to save the selected design.

## Design settings
-------

- Number of predictors: Set the total number of predictors in the desired design.

- Maximum predictor levels: Set the maximum number of levels for the predictors in the design. Only applicable for general full factorial designs, else predictors always have two levels. If a predictor has fewer levels than the maximum number of levels in the design, leave the remaining level cells empty or add a space.

- Design type:
    - 2-level factorial (default generator): Generate a two-level factorial design using the default generator. 
    - 2-level factorial (specify generator): Generate a two-level factorial design using a custom generator.
        - Design generator: Specify the generator for the design.
    - 2-level split-plot (hard-to-change factors): Generate a two-level split-plot design containing hard-to-change factors. 
        - Number of hard-to-change factors: Select the number of hard-to-change factors.
    - General full factorial: Generate a general full factorial design. 

- Predictor names and levels: Set the names of the predictors and give the values for each level.

- Design table: Select one of the pre-set designs by clicking on it in the table.

- Alias structure: Check to display alias structure of the generated design.

- Repeatability (Seed): Set a seed to reproduce a certain random order.

- Blocks: Select the number of blocks in the design.

- Centre points per block: Select the number of center points per block to add to the design.

- Replications: Select the number of replications of the whole design.

- Repetitions: Select the number of randomly selected runs to repeat.