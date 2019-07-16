# res40 Loss Triangles and other

## Overview

Res40 is the grammar for actuarial loss reserving, providing the calculation of the loss ratio for chian-ladder or development method. This is and interactive program and can be used as an app. 

## Installation 
```javascript
Install.package(“Res40”)  
```
## Usage 
```javascript
Library (”app.R”)
```

Given the actuarial payment and reserve data in the corresponding accident and development year, the app generates the actuarial loss triangle, calculate the link ratio, and illustrate the numbers.

In the panel of `Triangle`, the column represents the accident year (`AY`, vary from 1982 to 2017), and the row represent the development year (`Dev`, vary from 1 to 50). The length of the column is great or equal the length of row in the display. 

`Loss Curves` is the dotplot of the numbers  in the triangle over the development year.

`Dev Factors` represents the ratio of loss amounts from one valuation date to another, and they are intended to capture growth patterns of losses over time. 

`Factor Boxplots` displays the distribution of the loss ratio based on the five number summary: minimum, first quartile, median, third quartile, and maximum
And the red points are the outliers. 

`Factor curves` is the dotplot of the summary of mean, meantrim, 50% quantile and 90% quantile with observations.

`Results` is the number of the four summaries above.

The filled number of the triangle is determined by the following variables.  

- `LOB`indicates different insurance business lines (mortgage, auto, etc).   

- `Triangle type` concludes case incurred (`inc`), case reserves or case outstandings (`os`) and case paid (`paid`) with the relation *case incurred = case outstandings + case paid.*

- `Digits` is the number of digits

- `Comulative` is the cumulative amount in the development year, without comulative is the incremental amount.

- `Calendar view` is the re-arrangement of the triangle  

- `Facet loss curves` is the seperate plots of each year of the `loss curves`.

- `Log loss` is the logarithm of numbers in the triangle

- `Loss ratio` represents the ratio of loss amounts from one valuation date to another, and they are intended to capture growth patterns of losses over time. 

## Getting help 

If you encounter a clear bug, please file a minimal reproducible example on github.


