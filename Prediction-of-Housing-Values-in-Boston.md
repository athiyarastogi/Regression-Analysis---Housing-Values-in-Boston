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
housing_data = read.csv("housing.proper.csv") #loading the dataset
housing_data_df= data.frame(housing_data)
colnames(housing_data_df) <- c("X1","X2","X3","X4","X5","X6","X7","X8",
                            "X9","X10","X11","X12","X13", "Y") #transforming column names
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

training_housing_data = subset( housing_data_df, sample == TRUE)  #creates training dataset with rows marked true
testing_housing_data = subset(housing_data_df, sample ==FALSE)
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

``` r
housing_data.box = boxplot(housing_data_df, xlab= "Features", ylab ="Frequency") #boxplot to detect outliers   
```

![](Prediction-of-Housing-Values-in-Boston_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->
