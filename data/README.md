# Credit

Richardson et al. (2023) was published under the CC-BY-SA 4.0,
https://creativecommons.org/licenses/by-sa/4.0/.

# Figure

richardson_2023_fig1.png is fig. 1 from Richardson et al. (2023) and the file
was extracted from the article pdf.

# Data

richardson_data.csv was extracted from Richardson et al. (2023) tab. 1 and the following changes were made.

## Phosphate ##

what counts is the global boundary, the value before the transformation is
calculated with regional values according to

$$\frac{\text{global_bound}\text{regional_current_value}}{\text{regional_bound}}$$

## Novel ##

You can set safe boundary to 1, high risk to 4 and the current value to 4

## Blue/Green ##

The preindustrial values must be 0 to match the plot. This is either an error in
the plot or in the table.

## Genetic

The current value has been set to 110, the original graphics shows a value of
~100, the original table indicates a value >100.

## Transformation ##

scaling to get the variables on the plot is

$$ \log( (\text{current_value} - \text{holocene_value}) / \text{safe_boundary} + 1 )$$
