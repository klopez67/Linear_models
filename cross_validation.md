Cross Validation
================
Kimberly Lopez
2024-11-14

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(modelr)
library(mgcv)
```

    ## Loading required package: nlme
    ## 
    ## Attaching package: 'nlme'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse
    ## 
    ## This is mgcv 1.9-1. For overview type 'help("mgcv-package")'.

``` r
library(SemiPar)

set.seed(1)
```

working with the Lidar dataset

``` r
data("lidar")

lidar_df = 
  lidar|>
  as_tibble()|>
  mutate( id= row_number())
```

Graphing the Lidar data:

``` r
lidar_df|>
  ggplot( aes(x= range, y = logratio))+ 
  geom_point()
```

![](cross_validation_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

**Is this model too complex to fit a linear relationship?**

# Cross Validation

We will compare 3 models, linear, smooth, wiggly

Frist, contruct training and testing data

``` r
train_df = sample_frac(lidar_df, size = .8)

test_df = anti_join(lidar_df, train_df, by = "id")
```

Visualizing Training data:

``` r
ggplot(train_df, aes(x= range, y = logratio))+ 
  geom_point()+ 
  geom_point(data = test_df, color = "red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Now we can start fitting one model: –\> one model will be too simple or
too complex and make incorrect predictions.

Three models:

``` r
linear_mod= lm(logratio ~ range, data = train_df)
smooth_mod= gam(logratio ~ s(range), data = train_df)
wiggly_mod = gam(logratio ~ s(range, k = 30, sp= 10e-6), data = train_df)
```

Visualize the model fits:

**Linear fit**

``` r
train_df|>
  add_predictions(linear_mod)|> 
  ggplot(aes(x=range, y = logratio))+ 
  geom_point() + 
  geom_line(aes(y = pred), color = "red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

- This is not complex enough since some of the datapoints are not linear

**Wiggly model fit**

``` r
train_df|>
  add_predictions(wiggly_mod)|> 
  ggplot(aes(x=range, y = logratio))+ 
  geom_point() + 
  geom_line(aes(y = pred), color = "red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

- this over fits the training data on the model, so we cannot generalize
  this for a different dataset

**Smooth model fit**

``` r
train_df|>
  add_predictions(smooth_mod)|> 
  ggplot(aes(x=range, y = logratio))+ 
  geom_point() + 
  geom_line(aes(y = pred), color = "red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

- this is better than linear since it captures the trends in the data,
  but not too flexible that will cause overfitting

**Compare these numerically using RMSE**

``` r
rmse(linear_mod, test_df)
```

    ## [1] 0.127317

``` r
rmse(smooth_mod, test_df)
```

    ## [1] 0.08302008

``` r
rmse(wiggly_mod, test_df)
```

    ## [1] 0.08848557

Smooth_mod has lower RMSE

We want to see if this differece is consistent for random data testing

# Repeat the Train/test split
