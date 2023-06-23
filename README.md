
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mcceR

<!-- badges: start -->

[![R-CMD-check](https://github.com/NorskRegnesentral/mcceR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NorskRegnesentral/mcceR/actions/workflows/R-CMD-check.yaml)
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
                          fixed_features = "Wind",
                          fit.seed = 123,
                          generate.seed = 123)

explained
#>    id_explain counterfactual_rank  pred Solar.R Wind Temp Month
#> 1:          1                   1 14.23      49  7.4   76     6
#> 2:          2                   1 14.90      49  8.0   70     9
#> 3:          3                   1 14.36     191 12.6   73     7
#> 4:          4                   1 14.59     334 11.5   64     7
#> 5:          5                   1 14.47      49  8.6   76     7
#> 6:          6                   1 14.39      78 13.8   64     7
```
