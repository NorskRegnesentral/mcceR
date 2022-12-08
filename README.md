
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mcceR

<!-- badges: start -->
<!-- badges: end -->

The goal of mcceR is to generate counterfactual explanations

## Installation

You can install mcceR from GitHub with:

``` r
remotes::install_github("NorskRegnesentral/mcceR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mcceR)
library(xgboost)
## basic example code

data("airquality")
airquality <- airquality[complete.cases(airquality), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_explain <- 1:6
x_train <- as.matrix(airquality[-ind_x_explain, x_var])
y_train <- airquality[-ind_x_explain, y_var]
x_explain <- as.matrix(airquality[ind_x_explain, x_var])

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

#predict(model,x_train)

explained <- explain_mcce(model = model,
                          x_explain = x_explain,
                          x_train = x_train,
                          c_int = c(-Inf,15),
                          predict_model=NULL,
                          fixed_features = "Wind")
#> [1] 1
#> [1] 2
#> [1] 3


explained$cf
#>    id_explain counterfactual_rank Solar.R Wind Temp Month
#> 1:          1                   1      49  7.4   77     6
#> 2:          2                   1      49  8.0   70     6
#> 3:          3                   1     191 12.6   75     7
#> 4:          4                   1     334 11.5   63     7
#> 5:          5                   1      49  8.6   70     9
#> 6:          6                   1      78 13.8   64     7
```
