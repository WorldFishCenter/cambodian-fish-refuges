Alpha diversity
================

``` r
drake::loadd(catch_info)
drake::loadd(refuge_info)
drake::loadd(occasion_info)
```

Claculate shannon’s diversity index

``` r
catch_standard <- catch_info %>%
 # standardise replicate sampling
  group_by(refuge, occasion, gear, species) %>%
  summarise(
    # Mean not appropriate as some surveys came empty even when they were
    # performed. For analysis need to calculate number of samplings performed
    # and standardise using that rather than the mean
    across(c(no_fish, total_weight), ~round(mean(., na.rm = T)* 8)), 
    .groups = "drop")

catch_wide <- catch_standard %>%
  # Convert to a matrix
  pivot_wider(id_cols = c("refuge", "occasion", "gear"), 
              names_from = "species", 
              values_from = "no_fish") %>%
  # fill un-sampled species to zero
  mutate(across(where(is_numeric), tidyr::replace_na, replace = 0),
         across(where(is.numeric), log1p)) %>%
  select(!starts_with("sp"), starts_with("sp")) 

div <- catch_wide %>%
  select(starts_with("sp")) %>%
  as.matrix() %>% 
  vegan::diversity(index = "shannon", base = exp(1))

diversity <- catch_wide %>%
  select(!starts_with("sp")) %>%
  mutate(diversity_alpha = div) 
```

Having a look at the distribution we can see that the diversity of Line
fishing is lower than gill net and fike nets. Also it seems it looks
like need any special transformation for modelling; particularly if only
gill net diversity values are included.

``` r
diversity %>%
  ggplot(aes(x = diversity_alpha, fill = gear)) +
  geom_histogram(binwidth = 0.1, position = "stack", center = 0) +
  scale_fill_viridis_d() +
  facet_wrap("gear", scales = "free_y")
```

![](diversity_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

It also appears, as it was seen in previous data explorations, that the
diversity at the end of the wet season seems to be the highest while the
diversity at the end of the dry season or beginning of the wet season
seems to be the lowest.

``` r
diversity %>%
  ggplot(aes(x = occasion, y = diversity_alpha)) +
  geom_line(aes(group = refuge, colour = refuge), alpha = 0.25) +
  geom_boxplot() +
  scale_color_viridis_d() +
  facet_wrap("gear", ncol = 1)
```

![](diversity_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Covariates of diversity

We now integrate other data about sites and locations that can be useful
at explaining diversity values. This can be later used in a statistical
model.

``` r
occasion_cov <- occasion_info %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date), 
         season = if_else(month %in% c(2,5), "dry", "wet"), 
         water_level = (gauge_start + gauge_finish) / 2, 
         visibility = secchi_depth, 
         aquatic_plant_index = aquatic_plant_area * aquatic_plant_density, 
         across(c("month", "year"), as.character)) %>%
  select(refuge, occasion, year, season, month, water_level, water_temp, visibility,
         aquatic_plant_area, aquatic_plant_index, brush_park) %>%
  distinct()

refuge_cov <- refuge_info %>%
  mutate(dry_area = area_dry_ha, 
         dry_depth = depth_dry_m, 
         rf_area_connected = log(rf_area_connected_in_wet_season_ha), 
         across(category, as.character)) %>%
  select(refuge, category, dry_area, dry_depth, large_water_body)

diversity_cov <- diversity %>%
  left_join(refuge_cov, by = "refuge") %>%
  left_join(occasion_cov, by = c("occasion", "refuge")) %>%
  filter(gear == "GN")
```

### Missing values

Water temperature and the aquatic\_plant measurements appear to have
small levels of missing values. We might need to check wheter there is a
systematic bias on missing values before including these two variables
in a model.

``` r
inspectdf::inspect_na(diversity_cov) %>% filter(pcnt > 0) %>% knitr::kable()
```

| col\_name             | cnt |      pcnt |
| :-------------------- | --: | --------: |
| water\_temp           |   7 | 1.3513514 |
| aquatic\_plant\_index |   1 | 0.1930502 |

It appears that all cases of missing water temperature are within the
first occasion at the end of the wet season in 2012.

``` r
na_temp_div <- filter(diversity_cov, is.na(water_temp)) %>% select(-water_temp, -gear)
no_na_temp_div <- filter(diversity_cov, !is.na(water_temp)) %>% select(-water_temp, -gear)
inspectdf::inspect_cat(na_temp_div, no_na_temp_div) %>% inspectdf::show_plot()
```

![](diversity_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Variable distributions

Some variables might be better behaved with a log-transformation. These
include, the plant index, dry\_area, dry\_depth, and perhaps visibility
and water level.

``` r
inspectdf::inspect_num(diversity_cov) %>% inspectdf::show_plot()
```

![](diversity_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
diversity_cov %>%
  mutate(across(c(aquatic_plant_index, dry_area, dry_depth, visibility, water_level), log1p)) %>%
  inspectdf::inspect_num() %>% inspectdf::show_plot()
```

![](diversity_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### Categotries balance

Categories appear to be well balanced except for 2012 year which would
be potentially confounded with occasion 01.

``` r
inspectdf::inspect_cat(diversity_cov) %>% inspectdf::show_plot()
```

![](diversity_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Correlations

Larger refuges also tend to be deeper so maybe we can integrate these
two variables into one or just use one of them as an explanatory
variable.

Also there seems to be a positive relationship between visibility, water
level, aquatic plants and diversity. And a negative between those and
water temperature.

Dry depth and dry area appear to be non-seasonal factors that are
positively correlated with diversity.

These results generally agree with the multivariate analysis of
[seasonality](%22seasonality.md%22)

``` r
inspectdf::inspect_cor(diversity_cov, method = "spearman") %>% inspectdf::show_plot()
```

![](diversity_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

When using a Pearson correlation coefficient, patterns hold except for
the aquatic plant index which reverses. This shows the importance of the
very large values in this index which are reduced when using the
spearman coefficient.

``` r
inspectdf::inspect_cor(diversity_cov, method = "pearson") %>% inspectdf::show_plot()
```

![](diversity_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Statistical model

A quick exploration of the diversity parameters.

First, we are interested on investigating whether there are diversity
differences across seasons, and particularly the beginning and the end
of each of them. First, we use a extra simple model with seasons/months
as categorical values which is looking for significant differences among
them. Then, we look at a more mechanistic point of view which looks at
the environmental factors that promote diversity.

``` r
m_c <- lm(diversity_alpha ~ month + year, data = diversity_cov)
summary(m_c)
```

    ## 
    ## Call:
    ## lm(formula = diversity_alpha ~ month + year, data = diversity_cov)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.33628 -0.26363  0.05577  0.32932  1.07517 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.67534    0.07409  36.107  < 2e-16 ***
    ## month2      -0.04734    0.06063  -0.781    0.435    
    ## month5      -0.32535    0.06063  -5.367 1.22e-07 ***
    ## month8      -0.35442    0.06050  -5.858 8.38e-09 ***
    ## year2013    -0.01370    0.09079  -0.151    0.880    
    ## year2014     0.02037    0.09076   0.224    0.823    
    ## year2015    -0.01332    0.09076  -0.147    0.883    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4686 on 511 degrees of freedom
    ## Multiple R-squared:  0.1075, Adjusted R-squared:  0.09702 
    ## F-statistic: 10.26 on 6 and 511 DF,  p-value: 9.781e-11

``` r
m_m <- lm(diversity_alpha ~ water_level + aquatic_plant_area + brush_park + water_temp, 
           data = diversity_cov)
summary(m_m)
```

    ## 
    ## Call:
    ## lm(formula = diversity_alpha ~ water_level + aquatic_plant_area + 
    ##     brush_park + water_temp, data = diversity_cov)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.48739 -0.30476  0.04681  0.36366  1.10807 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         2.958e+00  2.569e-01  11.517   <2e-16 ***
    ## water_level         1.388e-04  2.893e-04   0.480   0.6316    
    ## aquatic_plant_area -9.713e-04  7.941e-04  -1.223   0.2218    
    ## brush_park          1.696e-04  9.406e-05   1.803   0.0719 .  
    ## water_temp         -1.639e-02  8.415e-03  -1.947   0.0521 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4864 on 506 degrees of freedom
    ##   (7 observations deleted due to missingness)
    ## Multiple R-squared:  0.01968,    Adjusted R-squared:  0.01193 
    ## F-statistic:  2.54 on 4 and 506 DF,  p-value: 0.03914

We found that the average difference between the beginning and end of
the seasons is fairly small and that the differences among years is also
relatively small.

The model with mechanistic variables show some significant relationships
but the R2 of that model is really small. Suggesting that they are not
as important on its own and account for only a small amount of
variability that can be explained by treating seasonality as a
categorical variable.

If we use models like this in the actual paper we should focus on the
categorical model and explain how environmental co-variates change
across seasons.

``` r
diversity_wide <- diversity_cov %>%
  mutate(year_s = (as.numeric(substr(occasion, 3, 4))-1) %/% 4, 
         month_s = paste0("m_", month)) %>% 
  pivot_wider(id_cols = c(refuge, year), 
              names_from = month_s, 
              values_from = diversity_alpha) 

diversity_wide %>%
  inspectdf::inspect_cor() %>%
  inspectdf::show_plot()
```

![](diversity_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
library(brms)
```

    ## Loading required package: Rcpp

    ## Loading 'brms' package (version 2.14.4). Useful instructions
    ## can be found by typing help('brms'). A more detailed introduction
    ## to the package is available through vignette('brms_overview').

    ## 
    ## Attaching package: 'brms'

    ## The following object is masked from 'package:stats':
    ## 
    ##     ar

``` r
m_dw <- brm(m_8 ~ m_5 + (1 | year) + (1 | refuge), data = diversity_wide)
```

    ## Compiling Stan program...

    ## Trying to compile a simple C file

    ## Running /usr/local/lib/R/bin/R CMD SHLIB foo.c
    ## make[1]: Entering directory '/tmp/RtmpZ9o6Ja'
    ## gcc -I"/usr/local/lib/R/include" -DNDEBUG   -I"/usr/local/lib/R/site-library/Rcpp/include/"  -I"/usr/local/lib/R/site-library/RcppEigen/include/"  -I"/usr/local/lib/R/site-library/RcppEigen/include/unsupported"  -I"/usr/local/lib/R/site-library/BH/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src/"  -I"/usr/local/lib/R/site-library/StanHeaders/include/"  -I"/usr/local/lib/R/site-library/RcppParallel/include/"  -I"/usr/local/lib/R/site-library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DBOOST_NO_AUTO_PTR  -include '/usr/local/lib/R/site-library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/usr/local/include   -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c foo.c -o foo.o
    ## In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:88,
    ##                  from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
    ##                  from /usr/local/lib/R/site-library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13,
    ##                  from <command-line>:
    ## /usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/util/Macros.h:613:1: error: unknown type name ‘namespace’
    ##   613 | namespace Eigen {
    ##       | ^~~~~~~~~
    ## /usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/util/Macros.h:613:17: error: expected ‘=’, ‘,’, ‘;’, ‘asm’ or ‘__attribute__’ before ‘{’ token
    ##   613 | namespace Eigen {
    ##       |                 ^
    ## In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Dense:1,
    ##                  from /usr/local/lib/R/site-library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13,
    ##                  from <command-line>:
    ## /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:96:10: fatal error: complex: No such file or directory
    ##    96 | #include <complex>
    ##       |          ^~~~~~~~~
    ## compilation terminated.
    ## make[1]: *** [/usr/local/lib/R/etc/Makeconf:167: foo.o] Error 1
    ## make[1]: Leaving directory '/tmp/RtmpZ9o6Ja'

    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL 'd29713d24a31c0a529f235cfe9de0b12' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 2.5e-05 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.25 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0.350817 seconds (Warm-up)
    ## Chain 1:                0.188193 seconds (Sampling)
    ## Chain 1:                0.53901 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL 'd29713d24a31c0a529f235cfe9de0b12' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 1.7e-05 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.17 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 0.432774 seconds (Warm-up)
    ## Chain 2:                0.369537 seconds (Sampling)
    ## Chain 2:                0.802311 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL 'd29713d24a31c0a529f235cfe9de0b12' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 1.8e-05 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.18 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 0.442173 seconds (Warm-up)
    ## Chain 3:                0.23461 seconds (Sampling)
    ## Chain 3:                0.676783 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL 'd29713d24a31c0a529f235cfe9de0b12' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 2.4e-05 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.24 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 0.594425 seconds (Warm-up)
    ## Chain 4:                1.01005 seconds (Sampling)
    ## Chain 4:                1.60448 seconds (Total)
    ## Chain 4:

``` r
summary(m_dw)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: m_8 ~ m_5 + (1 | year) + (1 | refuge) 
    ##    Data: diversity_wide (Number of observations: 119) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Group-Level Effects: 
    ## ~refuge (Number of levels: 40) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.27      0.06     0.15     0.40 1.00      859     1205
    ## 
    ## ~year (Number of levels: 3) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.11      0.15     0.00     0.51 1.01      870      761
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     1.56      0.25     1.08     2.09 1.01      797      535
    ## m_5           0.32      0.09     0.13     0.51 1.00     1160     1384
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.36      0.03     0.30     0.43 1.00      940      553
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).
