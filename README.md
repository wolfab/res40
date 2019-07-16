# res40 Loss Triangles and other

## Overview

Res40 is the grammar for actuarial loss reserving, providing the calculation of the loss ratio for chian-ladder or development method. This is and interactive program and can be used as an app. 

## Installation 
```javascript
Install.package(“Res40”)  
Library (”app.R”)
```
##Usage 
Given the actuarial payment and reserve data in the corresponding accident and development year, the app generates the actuarial loss triangle, calculate the link ratio, and illustrate the numbers.

In the panel of triangle, the column represents the accident year (`AY`, vary from 1982 to 2017), and the row represent the development year (`Dev`, vary from 1 to 50). The length of the column is great or equal the length of row in the display. 

The filled number of the triangle is determined by the following variables.  

`LOB` means the business line, indicates the type of insurance (mortgage, auto, etc).   

Triangle type concludes: case incurred (`inc`), case reserves or case outstandings (`os`) and case paid (`paid`) with the relation that 

case incurred = case outstandings + case paid. 

`Digits` is the number of digits

`Comulative` is the cumulative amount in the development year, without comulative is the incremental amount.

In the panel of `Loss Curves`, the curves represent dot plot of the numbers in triangle.

`Dev Factors` is the loss ratio

`Factor Boxplots` displays the distribution of the loss ratio based on the five number summary: minimum, first quartile, median, third quartile, and maximum
And the red points are the outliers. 

`Factor curves` is the dotplot of the summary of mean, meantrim, 50% quantile and 90% quantile with observations.

`Results` is the number of the four summaries above.

`Calendar view`: representation of the triangle  

`Facet loss curves`: the indepent plot of the numbers in trianlge of each year 

`Log loss` : the logarithm of the numbers in triangle

`Loss ratio`: represent the ratio of loss amounts from one valuation date to another, and they are intended to capture growth patterns of losses over time. 
1122





