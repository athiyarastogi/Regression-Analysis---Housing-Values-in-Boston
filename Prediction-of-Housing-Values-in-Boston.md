Prediction of Housing Values in Boston
================

**File name:** “housing.proper.csv” (given in the problem set) has
housing values in the suburbs of Boston

  - **506 observations** and **13 predictor variables** (independent
    variables or X or features)  
  - Median price is a more accurate measure of market prices as it is
    not affected by
    outliers

<!-- end list -->

``` r
library (tidyverse)  
```

    ## ── Attaching packages ────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✔ ggplot2 3.3.0     ✔ purrr   0.3.3
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ───────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library (ggplot2)  
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(dplyr)
library(broom)

housing_data = read.csv("housing.proper.csv") #loading the dataset
housing_data_df= data.frame(housing_data)
colnames(housing_data_df) <- c("X1","X2","X3","X4","X5","X6","X7","X8",
                            "X9","X10","X11","X12","X13", "Y") #transforming column names
lnY = log(housing_data_df$Y)
```

# Exploratory Data Analysis

**Objective of EDA** : To choose important variables or features of this
dataset Visualisations are used the explain relationship between
features and target variable

**Split the data into 70% training set (model building data set) and 30%
testing set**

``` r
set.seed(1234) #set seed to ensure same random numbers are generated
require(caTools)
```

    ## Loading required package: caTools

``` r
sample = sample.split(housing_data_df, SplitRatio = 0.7)
#Splits the dataset "housing_data" in the ratio mentioned. Marks rows as logical true and remaining as logical false.  

training_housing_data = subset(housing_data_df, sample == TRUE)  #creates training dataset with rows marked true
print 
```

    ## function (x, ...) 
    ## UseMethod("print")
    ## <bytecode: 0x7fa3ea36c760>
    ## <environment: namespace:base>

``` r
lny = log(training_housing_data$Y)
testing_housing_data = subset(housing_data_df, sample ==FALSE)
lny_testing = log(testing_housing_data)
```

**Choosing features through Summary Statistics, Correlation Plot, Heat
Map and Boxplot**

EDA is performed on the complete
dataset

``` r
summary (housing_data_df) # to get statistic of each variable to detect outliers  
```

    ##        X1                 X2               X3              X4         
    ##  Min.   : 0.00632   Min.   :  0.00   Min.   : 0.46   Min.   :0.00000  
    ##  1st Qu.: 0.08204   1st Qu.:  0.00   1st Qu.: 5.19   1st Qu.:0.00000  
    ##  Median : 0.25651   Median :  0.00   Median : 9.69   Median :0.00000  
    ##  Mean   : 3.61352   Mean   : 11.36   Mean   :11.14   Mean   :0.06917  
    ##  3rd Qu.: 3.67708   3rd Qu.: 12.50   3rd Qu.:18.10   3rd Qu.:0.00000  
    ##  Max.   :88.97620   Max.   :100.00   Max.   :27.74   Max.   :1.00000  
    ##        X5               X6              X7               X8        
    ##  Min.   :0.3850   Min.   :3.561   Min.   :  2.90   Min.   : 1.130  
    ##  1st Qu.:0.4490   1st Qu.:5.886   1st Qu.: 45.02   1st Qu.: 2.100  
    ##  Median :0.5380   Median :6.208   Median : 77.50   Median : 3.207  
    ##  Mean   :0.5547   Mean   :6.285   Mean   : 68.57   Mean   : 3.795  
    ##  3rd Qu.:0.6240   3rd Qu.:6.623   3rd Qu.: 94.08   3rd Qu.: 5.188  
    ##  Max.   :0.8710   Max.   :8.780   Max.   :100.00   Max.   :12.127  
    ##        X9              X10             X11             X12        
    ##  Min.   : 1.000   Min.   :187.0   Min.   :12.60   Min.   :  0.32  
    ##  1st Qu.: 4.000   1st Qu.:279.0   1st Qu.:17.40   1st Qu.:375.38  
    ##  Median : 5.000   Median :330.0   Median :19.05   Median :391.44  
    ##  Mean   : 9.549   Mean   :408.2   Mean   :18.46   Mean   :356.67  
    ##  3rd Qu.:24.000   3rd Qu.:666.0   3rd Qu.:20.20   3rd Qu.:396.23  
    ##  Max.   :24.000   Max.   :711.0   Max.   :22.00   Max.   :396.90  
    ##       X13              Y        
    ##  Min.   : 1.73   Min.   : 5.00  
    ##  1st Qu.: 6.95   1st Qu.:17.02  
    ##  Median :11.36   Median :21.20  
    ##  Mean   :12.65   Mean   :22.53  
    ##  3rd Qu.:16.95   3rd Qu.:25.00  
    ##  Max.   :37.97   Max.   :50.00

``` r
housing_data.cor = round(cor(housing_data_df), 3) #correlation between features and median housing values
housing_data.cor
```

    ##         X1     X2     X3     X4     X5     X6     X7     X8     X9    X10
    ## X1   1.000 -0.200  0.407 -0.056  0.421 -0.219  0.353 -0.380  0.626  0.583
    ## X2  -0.200  1.000 -0.534 -0.043 -0.517  0.312 -0.570  0.664 -0.312 -0.315
    ## X3   0.407 -0.534  1.000  0.063  0.764 -0.392  0.645 -0.708  0.595  0.721
    ## X4  -0.056 -0.043  0.063  1.000  0.091  0.091  0.087 -0.099 -0.007 -0.036
    ## X5   0.421 -0.517  0.764  0.091  1.000 -0.302  0.731 -0.769  0.611  0.668
    ## X6  -0.219  0.312 -0.392  0.091 -0.302  1.000 -0.240  0.205 -0.210 -0.292
    ## X7   0.353 -0.570  0.645  0.087  0.731 -0.240  1.000 -0.748  0.456  0.506
    ## X8  -0.380  0.664 -0.708 -0.099 -0.769  0.205 -0.748  1.000 -0.495 -0.534
    ## X9   0.626 -0.312  0.595 -0.007  0.611 -0.210  0.456 -0.495  1.000  0.910
    ## X10  0.583 -0.315  0.721 -0.036  0.668 -0.292  0.506 -0.534  0.910  1.000
    ## X11  0.290 -0.392  0.383 -0.122  0.189 -0.356  0.262 -0.232  0.465  0.461
    ## X12 -0.385  0.176 -0.357  0.049 -0.380  0.128 -0.274  0.292 -0.444 -0.442
    ## X13  0.456 -0.413  0.604 -0.054  0.591 -0.614  0.602 -0.497  0.489  0.544
    ## Y   -0.388  0.360 -0.484  0.175 -0.427  0.695 -0.377  0.250 -0.382 -0.469
    ##        X11    X12    X13      Y
    ## X1   0.290 -0.385  0.456 -0.388
    ## X2  -0.392  0.176 -0.413  0.360
    ## X3   0.383 -0.357  0.604 -0.484
    ## X4  -0.122  0.049 -0.054  0.175
    ## X5   0.189 -0.380  0.591 -0.427
    ## X6  -0.356  0.128 -0.614  0.695
    ## X7   0.262 -0.274  0.602 -0.377
    ## X8  -0.232  0.292 -0.497  0.250
    ## X9   0.465 -0.444  0.489 -0.382
    ## X10  0.461 -0.442  0.544 -0.469
    ## X11  1.000 -0.177  0.374 -0.508
    ## X12 -0.177  1.000 -0.366  0.333
    ## X13  0.374 -0.366  1.000 -0.738
    ## Y   -0.508  0.333 -0.738  1.000

``` r
heatmap(housing_data.cor, Rowv = NA, Colv = NA)
```

![](Prediction-of-Housing-Values-in-Boston_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
corrplot(cor(housing_data_df), method = "number", type = "upper", diag = FALSE)
```

![](Prediction-of-Housing-Values-in-Boston_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

**Removing Charles River Dummy Variable (X4)** as the Correlation
between Median House Value and X4 is low (0.18). Smaller value of
correlation signifies that the presence of Charles River has little to
no impact on the value median value of houses.

Strong positive correlation (0.91) between X9 (index of accessibility to
radial highways) and X10 (full-value property-tax rate per 10,000)
suggests Multicollinearity, which means we can drop one variable with a
lower correlation with Y. Here, higher accessibility to radial highways
is connected to a high tax value of a property. **I will drop X9 (index
of accessibility to radial highways)** as it has a lower correlation
with median housing values as compared to X10 (full-value property-tax
rate per 10,000). **X9 is a repetitive predictor.**

Strong Negative correlation (-0.75) between X7 (proportion of owner
occupied units built prior to 1940) and X8 (weighted distances to five
Boston employment
centres)

``` r
housing_data.box = boxplot(housing_data_df, xlab= "Features", ylab ="Frequency") #boxplot to detect outliers 
```

![](Prediction-of-Housing-Values-in-Boston_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
There are outliers in **X1(per capita crime rate by town)**,
**X2(proportion of residential land zoned for lots over 25,000 sq.ft.)**
and **X12 (proportion of African Americans by town)**

There is high variability in **X10 (full-value property-tax rate per
10,000)**

Plotting the model generates 4 plots: Normal QQ plot. Resdiuals vs
Fitted. Scale vs Location. Residuals vs
Leverage.

``` r
full_housing_model =lm(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 ,data=housing_data_df)
log_full_housing_model =lm(lnY~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 ,data=housing_data_df)


summary(full_housing_model)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + 
    ##     X10 + X11 + X12 + X13, data = housing_data_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.595  -2.730  -0.518   1.777  26.199 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.646e+01  5.103e+00   7.144 3.28e-12 ***
    ## X1          -1.080e-01  3.286e-02  -3.287 0.001087 ** 
    ## X2           4.642e-02  1.373e-02   3.382 0.000778 ***
    ## X3           2.056e-02  6.150e-02   0.334 0.738288    
    ## X4           2.687e+00  8.616e-01   3.118 0.001925 ** 
    ## X5          -1.777e+01  3.820e+00  -4.651 4.25e-06 ***
    ## X6           3.810e+00  4.179e-01   9.116  < 2e-16 ***
    ## X7           6.922e-04  1.321e-02   0.052 0.958229    
    ## X8          -1.476e+00  1.995e-01  -7.398 6.01e-13 ***
    ## X9           3.060e-01  6.635e-02   4.613 5.07e-06 ***
    ## X10         -1.233e-02  3.760e-03  -3.280 0.001112 ** 
    ## X11         -9.527e-01  1.308e-01  -7.283 1.31e-12 ***
    ## X12          9.312e-03  2.686e-03   3.467 0.000573 ***
    ## X13         -5.248e-01  5.072e-02 -10.347  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.745 on 492 degrees of freedom
    ## Multiple R-squared:  0.7406, Adjusted R-squared:  0.7338 
    ## F-statistic: 108.1 on 13 and 492 DF,  p-value: < 2.2e-16

``` r
tidy(full_housing_model) %>% arrange(p.value)
```

    ## # A tibble: 14 x 5
    ##    term          estimate std.error statistic  p.value
    ##    <chr>            <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 X13          -0.525      0.0507   -10.3    7.78e-23
    ##  2 X6            3.81       0.418      9.12   1.98e-18
    ##  3 X8           -1.48       0.199     -7.40   6.01e-13
    ##  4 X11          -0.953      0.131     -7.28   1.31e-12
    ##  5 (Intercept)  36.5        5.10       7.14   3.28e-12
    ##  6 X5          -17.8        3.82      -4.65   4.25e- 6
    ##  7 X9            0.306      0.0663     4.61   5.07e- 6
    ##  8 X12           0.00931    0.00269    3.47   5.73e- 4
    ##  9 X2            0.0464     0.0137     3.38   7.78e- 4
    ## 10 X1           -0.108      0.0329    -3.29   1.09e- 3
    ## 11 X10          -0.0123     0.00376   -3.28   1.11e- 3
    ## 12 X4            2.69       0.862      3.12   1.93e- 3
    ## 13 X3            0.0206     0.0615     0.334  7.38e- 1
    ## 14 X7            0.000692   0.0132     0.0524 9.58e- 1

``` r
plot(log_full_housing_model)
```

![](Prediction-of-Housing-Values-in-Boston_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->![](Prediction-of-Housing-Values-in-Boston_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->![](Prediction-of-Housing-Values-in-Boston_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->![](Prediction-of-Housing-Values-in-Boston_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

``` r
pairs(housing_data_df)
```

![](Prediction-of-Housing-Values-in-Boston_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->

``` r
#We have already removed X9 due to multicollinearity with X10, another reason to remove it is it's *high p-value*. 
#We will remove X8 too as it has the highest p-value as compared to all the other variables. High p-value suggests changes in X8 are not associated with changes in Y (median housing values)
```

We remove every variable with a pvalue greater than 0.05.  
Through EDA we have removed 4 variables (X3, X4, X7, X9, X12)  
Since X13 has the smallest p-value it becomes the first step for Forward
Stepwise Selection

# Model Selection and Validation

Fitting the model with selected
variables

``` r
log_full_housing_model =lm(lny~X1 + X2 + X5 + X6 + X8 + X10 + X11 + X13 ,data=training_housing_data)
s <- summary(log_full_housing_model)$sigma
# Selection criteria for the full model
SSres <- sum(log_full_housing_model$residuals^2)
Rsq <- summary(log_full_housing_model$r.squared)
Rsq_adj <- summary(log_full_housing_model)$adj.r.squared
p_prime <- length(log_full_housing_model$coefficients)
n <- nrow(training_housing_data)
C <- SSres/s^2 + 2*p_prime - n
AIC <- n*log(SSres) - n*log(n) + 2*p_prime


# Function that returns the selection criteria
select_criteria = function(model, n, s)
{
  SSres <- sum(model$residuals^2)
  Rsq <- summary(model)$r.squared
  Rsq_adj <- summary(model)$adj.r.squared
  p_prime <- length(model$coefficients)
  C <- SSres/s^2 + 2*p_prime - n
  AIC <- n*log(SSres) - n*log(n) + 2*p_prime
  res <- c(SSres, Rsq, Rsq_adj, C, AIC)
  names(res) <- c("SSres", "Rsq", "Rsq_adj", "C", "AIC")
  return(res)
}



rbind(
  select_criteria(lm(lny~X1+X2, data = training_housing_data), n, s),
  select_criteria(lm(lny~X1+X2+X6, data = training_housing_data), n, s),
  select_criteria(lm(lny~X1+X2+X3+X6+X8, data = training_housing_data), n, s),
  select_criteria(lm(lny~X1+X2+X5+X6+X8, data = training_housing_data), n, s),
  select_criteria(lm(lny~X1+X2+X3+X6+X9, data = training_housing_data), n, s),
  select_criteria(lm(lny~X1+X2+X5+X6+X8+X10, data = training_housing_data), n, s),
  select_criteria(lm(lny~X1+X2+X5+X6+X8+X10+X11, data = training_housing_data), n, s),
  select_criteria(lm(lny~X1+X2+X5+X6+X8+X10+X11+X13, data = training_housing_data), n, s),
  select_criteria(lm(lny~X1+X2+X3+X5+X6+X8+X10+X11+X13, data = training_housing_data), n, s))
```

    ##          SSres       Rsq   Rsq_adj         C        AIC
    ##  [1,] 34.04853 0.3386797 0.3345848 621.56550  -730.4700
    ##  [2,] 21.26331 0.5870054 0.5831576 270.00777  -881.9520
    ##  [3,] 19.74294 0.6165353 0.6105437 231.96399  -902.1370
    ##  [4,] 19.02593 0.6304617 0.6246877 212.13605  -914.1968
    ##  [5,] 19.84237 0.6146041 0.6085823 234.71358  -900.4993
    ##  [6,] 18.28503 0.6448521 0.6381722 193.64751  -925.1455
    ##  [7,] 16.57020 0.6781591 0.6710746 148.22610  -955.2491
    ##  [8,] 11.46323 0.7773511 0.7717322   9.00000 -1073.3673
    ##  [9,] 11.45943 0.7774250 0.7710859  10.89471 -1071.4756

``` r
sum((lny) - mean(lny)^2)
```

    ## [1] -2004.703

Model lny~X1+X2+X5+X6+X8+X10+X11+X13 has  
\- **Coefficient of Determination 0.777 and Adjusted coefficient of
determination is 0.771**. Adding more variables in the next model causes
a very minute change in both. Therefore, the current model is correct  
\- the smallest **Akaike’s Information Criterion, (AIC: -1073)**  
\- **Mallows’ Cp** is close to p’and is equal to **9**

\`\`\` We validate the reduced model against the boston housing testing
set and find that **Mean squared prediction error** (MSPE) & MSRes are
quite closer together indicating a good predictive ability of the model.

# Model Validation
