---
title: "cat3advice"
output: 
  rmarkdown::html_vignette:
    toc: true
    toc_depth: 2
    keep_md: true
bibliography: references.bib
vignette: >
  %\VignetteIndexEntry{cat3advice}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




```r
library(cat3advice)
```

# Introduction

This tutorial uses data from the ICES Western English Channel plaice stock (ple.27.7e) to illustrate the application of the rfb/rb/chr rules. The data is included in the `cat3advice` R package.

Before reading this vignette, please first read the ICES Technical Guidelines [@ICES2022_cat23_tech_guidelines].

For more details on the rfb rule, please refer to Fischer et al. [-@Fischer2020_catch_rule;-@Fischer2021_GA;@Fischer2021_GA_PA;-@Fischer2022_risk_equivalence] and for the chr rule, please refer to Fischer et al. [-@Fischer2022_hr;-@Fischer2022_risk_equivalence].

# The rfb rule

The rfb rule is an index adjusted harvest control rule that uses a biomass index and catch length data. The method is defined as Method 2.1 in the ICES Technical Guidelines [@ICES2022_cat23_tech_guidelines, page 9] as

$$
A_{y+1} = A_y \times r \times f \times b \times m
$$

where $A_{y+1}$ is the new catch advice, $A_y$ the previous advice, $r$ the biomass ratio from a biomass index, $f$ the fishing pressure proxy from catch length data, $b$ a biomass safeguard and $m$ a precautionary multiplier. Furthermore, the change in the advice is restricted by a stability clause that only allows changes of between $+20\%$ and $-30\%$ relative to the previous advice, but the application of the stability clause is conditional on $b=1$ and turned off when $b<1$.

The rfb rule should be applied biennially, i.e. the catch advice is valid for two years.

Please note that any change from the default configuration should be supported by case-specific simulations.

## Reference catch $A_y$

The reference catch $A_y$ is usually the most recently advised catch. In a typical ICES setting, an assessment is conducted in an assessment (intermediate) year $y$ to give advice for the following advice year $y+1$, this is the advice for year $y$:


```r
### load plaice catch and advice data
data("ple7e_catch")
tail(ple7e_catch)
#>    year advice landings discards catch
#> 31 2017   2714     2128      821  2949
#> 32 2018   3257     1880      633  2513
#> 33 2019   3648     1725      366  2091
#> 34 2020   2721     1373      514  1888
#> 35 2021   2177     1403      211  1615
#> 36 2022   1742       NA       NA    NA
### get reference catch
A <- A(ple7e_catch, units = "tonnes")
A
#> An object of class "A".
#> Value: 1742
```

The ICES Technical Guidelines [@ICES2022_cat23_tech_guidelines] specify that if the realised catch is very different from the advised catch, the reference catch could be replaced by an average of recent catches:


```r
### use 3-year average
A(ple7e_catch, units = "tonnes", basis = "average catch", avg_years = 3)
#> An object of class "A".
#> Value: 1864.66666666667
```

The reference catch can also be defined manually:


```r
### use 3-year average
A(2000, units = "tonnes")
#> An object of class "A".
#> Value: 2000
```

## Biomass index trend (ratio) $r$

The biomass index trend $r$ calculates the trend in the biomass index over last the five years, by dividing the mean of the last two years by the mean of the three preceding years:

$$
r = \Sigma_{i=y-2}^{y-1}(I_i/2)\ / \ \Sigma_{i=y-5}^{y-2}(I_i/3)
$$

The ratio is calculated with the function `r`. Index data should be provided as a `data.frame` with columns `year` and `index`.


```r
### load plaice data
data("ple7e_idx")
tail(ple7e_idx)
#>    year     index
#> 14 2016 1.3579990
#> 15 2017 1.3323659
#> 16 2018 1.1327596
#> 17 2019 0.8407277
#> 18 2020 0.5996326
#> 19 2021 1.0284297

### calculate biomass trend
r <- r(ple7e_idx, units = "kg/hr")
r
#> An object of class "rfb_r".
#> Value: 0.73871806243358

### ICES advice style table
advice(r)
#> --------------------------------------------------------------------------------
#> Stock biomass trend
#> --------------------------------------------------------------------------------
#> Index A (2020,2021)                              |                    0.81 kg/hr
#> Index B (2017,2018,2019)                         |                    1.10 kg/hr
#> r: stock biomass trend (index ratio A/B)         |                          0.74

### plot index
### horizontal orange lines correspond to the the 2/3-year averages
plot(r)
```

<img src="figure/unnamed-chunk-5-1.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" width="500" style="display: block; margin: auto;" />

```r

### when the value of r is known
r(1)
#> An object of class "r".
#> Value: 1
```

Biomass index data is usually available until the year before the advise year. More recent data can be used and the function automatically picks the most recent data provided to it.

## Biomass safeguard $b$

The biomass safeguard reduces the advice when the biomass index $I$ falls below a threshold value $I_{\text{trigger}}$:

$$
b = \text{min}\{1,\ I_{y-1}/I_{\text{trigger}}\}
$$
By default, the trigger value is defined based on the lowest observed index value $I_{\text{loss}}$ as $I_{\text{trigger}} = 1.4I_{\text{loss}}$. 

The biomass safeguard is calculated with the function `b`:

```r
### use same plaice data as before
### application in first year with new calculation of Itrigger
b <- b(ple7e_idx, units = "kg/hr")
b
#> An object of class "b".
#> Value: 1

### ICES advice style table
advice(b)
#> --------------------------------------------------------------------------------
#> Biomass safeguard
#> --------------------------------------------------------------------------------
#> Last index value (I2021)                         |                    1.03 kg/hr
#> Index trigger value (Itrigger = Iloss x 1.4)     |                    0.39 kg/hr
#> b: index relative to trigger value,              |                          1.00
#>    min{I2021/Itrigger, 1}                        |

### plot
plot(b)
```

<img src="figure/unnamed-chunk-6-1.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" width="500" style="display: block; margin: auto;" />

```r
### plot b and r in one figure
plot(r, b)
```

<img src="figure/unnamed-chunk-6-2.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" width="500" style="display: block; margin: auto;" />

**Please note that $I_{\text{trigger}}$ should only be defined once in the first year of the application of the rfb rule**. In the following years, the same value should be used. For this purpose, `b` allows the direct definition of $I_{\text{trigger}}$, or, more conveniently, $I_{\text{trigger}}$ can be based on the year in which $I_{\text{loss}}$ is defined:


```r
### in following years, Itrigger should NOT be updated
### i.e. provide value for Itrigger
b(ple7e_idx, units = "kg/hr", Itrigger = 0.3924721)
#> An object of class "b".
#> Value: 1
### alternatively, the reference year for Iloss can be used
b(ple7e_idx, units = "kg/hr", yr_ref = 2007)
#> An object of class "b".
#> Value: 1
```

## Fishing pressure proxy $f$

Catch length data are used to approximate the fishing pressure. The mean length of fish in the catch compared to a reference length is used as the indicator.

### Length data

The fishing pressure proxy requires length data from the catch. Ideally, length data for several years are provided in a `data.frame` with columns `year`, `length` and `numbers`. An additional column `catch_category` specifying the catch category, such as discards and landings, is optional.


```r
data("ple7e_length")
head(ple7e_length)
#>   year             catch_category length numbers
#> 1 2018                BMS landing    100    0.00
#> 2 2018                   Discards    100 5887.55
#> 3 2018 Logbook Registered Discard    100    0.00
#> 4 2015                   Discards    120  128.60
#> 5 2016                BMS landing    140    0.00
#> 6 2018                BMS landing    140    0.00
```

### Length at first capture $L_c$

Only length data above the length at first capture $L_c$ are used to avoid noisy data from fish that are not fully selected. $L_c$ is defined as the first length class where the abundance is at or above 50\% of the mode of the length distribution and can be calculated with the function `Lc()`:


```r
lc <- Lc(ple7e_length)
lc
#> 2014 2015 2016 2017 2018 2019 2020 2021 
#>  240  260  260  270  260  260  260  270
plot(lc)
#> Warning: Removed 404 rows containing missing values (position_stack).
```

<img src="figure/unnamed-chunk-9-1.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" width="500" style="display: block; margin: auto;" />

$L_c$ can change from year to year. Therefore, it is recommended to pool data from several (e.g. 5) years:

```r
lc <- Lc(ple7e_length, pool = 2017:2021)
lc
#> [1] 260
plot(lc)
#> Warning: Removed 56 rows containing missing values (position_stack).
```

<img src="figure/unnamed-chunk-10-1.png" title="plot of chunk unnamed-chunk-10" alt="plot of chunk unnamed-chunk-10" width="500" style="display: block; margin: auto;" />

If length data are noisy, the size of the length classes can be increased:

```r
### use 20mm length classes
plot(Lc(ple7e_length, pool = 2017:2021, lstep = 20))
#> Warning: Removed 29 rows containing missing values (position_stack).
```

<img src="figure/unnamed-chunk-11-1.png" title="plot of chunk unnamed-chunk-11" alt="plot of chunk unnamed-chunk-11" width="500" style="display: block; margin: auto;" />

Once defined, $L_c$ should be kept constant and used the same value used for all data years. $L_c$ should only be changed if there are strong changes in the fishery or data.

### Mean length

After defining $L_c$, the mean (annual) catch length $L_{\text{mean}}$ above $L_c$ can be calculated:

If length data are noisy, the size of the length classes can be increased:

```r
### use 20mm length classes
lmean <- Lmean(data = ple7e_length, Lc = lc, units = "mm")
lmean
#>     2014     2015     2016     2017     2018     2019     2020     2021 
#> 310.6955 322.8089 333.1876 326.9434 326.5741 339.8752 321.5979 319.1974
plot(lmean)
```

<img src="figure/unnamed-chunk-12-1.png" title="plot of chunk unnamed-chunk-12" alt="plot of chunk unnamed-chunk-12" width="700" style="display: block; margin: auto;" />

### Reference length

The reference length follows the concepts of @Beverton1957_dynamics_populations and is calculated as derived by @Jardim2015_HCR:

$$
L_{F=M} = 0.75L_c + 0.25L_\infty
$$
where $L_{F=M}$ is the MSY reference length, $L_c$ the length at first capture as defined above, and $L_\infty$ the von Bertalanffy asymptotic length. This simple equation assumes that fishing at $F=M$ can be used as a proxy for MSY and that $M/k=1.5$ (where $M$ is the natural mortality and $k$ the von Bertalanffy individual growth rate). The reference length can be calculated with


```r
lref <- Lref(Lc = 264, Linf = 585, units = "mm")
lref
#> [1] 344.25
```

The reference length $L_{F=M}$ should only be defined once in the first year of the application of the rfb rule. In the following years, the same value should be used.

Deviations from the assumptions of $F=M$ and $M/k=1.5$ are possible following Equation A.3 of @Jardim2015_HCR $L_{F=\gamma M, k = \theta M}=(\theta L_\infty + L_c(\gamma + 1))/(\theta + \gamma +1)$ and can be used by providing arguments `gamma` and `theta` to `Lref()`.

### Indicator

The length indicator $f$ is defined as

$$
f = L_{\text{mean}}/L_{F=M}
$$

and can be calculated with `f()`:


```r
f <- f(Lmean = lmean, Lref = lref, units = "mm")
f
#> An object of class "f".
#> Value: 0.927225455656142
### ICES advice style table
advice(f)
#> --------------------------------------------------------------------------------
#> Fishing pressure proxy
#> --------------------------------------------------------------------------------
#> Mean catch length (Lmean = L2021)                |                        320 mm
#> MSY proxy length (LF=M)                          |                        340 mm
#> f: Fishing pressure proxy relative to MSY proxy  | 
#>    (L2021/LF=M)                                  |                          0.93
### plot
plot(f)
```

<img src="figure/unnamed-chunk-14-1.png" title="plot of chunk unnamed-chunk-14" alt="plot of chunk unnamed-chunk-14" width="500" style="display: block; margin: auto;" />

In this case, the mean catch length (orange curve) is always below the MSY proxy reference length (blue horizontal line), indicating that the fishing pressure was above $F_{\text{MSY}}$.

## Multiplier $m$

The multiplier $m$ is a tuning parameter and ensures that the catch advice is precautionary in the long term. 

The value of $m$ is set depending on the von Bertalanffy parameter $k$ (individual growth rate), with $m=0.95$ for stocks with $k<0.2/\text{year}$ and $m=0.90$ for stocks with $0.2\text{year}^{-1} \leq k < 0.32\text{year}^{-1}$. The multiplier can be calculated with the function `m()`:


```r
# for k=0.1/year
m <- m(hcr = "rfb", k = 0.1)
#> Multiplier (m) for the rfb rule: selecting value based on k: m=0.95
m
#> An object of class "m".
#> Value: 0.95

### alias for rfb rule
rfb_m(k = 0.1)
#> Multiplier (m) for the rfb rule: selecting value based on k: m=0.95
#> An object of class "rfb_m".
#> Value: 0.95
```

Please note that the multiplier $m$ does not lead to a continuous reduction in the catch advice. The components of the rfb rule are multiplicative, this means that $m$ could be considered as part of component $f$ and essentially adjusts the reference length $L_{F=M}$ to $L'_{F=M}$:

$$
A_{y+1} = A_y\ r\ f\ b\ m = A_y\ r\ \frac{L_{\text{mean}}}{L_{F=M}}\ b\ m = A_y\ r\ \frac{L_{\text{mean}}}{L_{F=M}/m}\ b = A_y\ r\ \frac{L_{\text{mean}}}{L'_{F=M}}\ b
$$

## Application of rfb rule

Now we have all the components of the rfb rule and can apply it:


```r
advice <- rfb(A = A, r = r, f = f, b = b, m = m, discard_rate = 27)
advice
#> An object of class "rfb".
#> Value: 1219.4
```

A discard rate in \% can be provided to the argument `discard_rate` and this means the advice is provided for the catch and landings.

The rfb rule includes a stability clause that restricts changes to $+20%$ and $-30%$. This stability clause in conditional on the biomass safeguard and is only applied if $b=1$ but turned off when $b<1$.

`cat3advice` can print a table similar to the table presented in ICES advice sheets:

```r
advice(advice)
#> --------------------------------------------------------------------------------
#> Previous catch advice Ay (advised catch for 2022) |                   1742 tonnes
#> --------------------------------------------------------------------------------
#> Stock biomass trend
#> --------------------------------------------------------------------------------
#> Index A (2020,2021)                              |                      0.81 rfb
#> Index B (2017,2018,2019)                         |                      1.10 rfb
#> r: stock biomass trend (index ratio A/B)         |                          0.74
#> --------------------------------------------------------------------------------
#> Fishing pressure proxy
#> --------------------------------------------------------------------------------
#> Mean catch length (Lmean = L2021)                |                        320 mm
#> MSY proxy length (LF=M)                          |                        340 mm
#> f: Fishing pressure proxy relative to MSY proxy  | 
#>    (L2021/LF=M)                                  |                          0.93
#> --------------------------------------------------------------------------------
#> Biomass safeguard
#> --------------------------------------------------------------------------------
#> Last index value (I2021)                         |                    1.03 kg/hr
#> Index trigger value (Itrigger = Iloss x 1.4)     |                    0.39 kg/hr
#> b: index relative to trigger value,              |                          1.00
#>    min{I2021/Itrigger, 1}                        |                              
#> --------------------------------------------------------------------------------
#> Precautionary multiplier to maintain biomass above Blim with 95% probability
#> --------------------------------------------------------------------------------
#> m: multiplier                                    |                          0.95
#>    (generic multiplier based on life history)    |                              
#> RFB calculation (r*f*b*m)                        |                   1130 tonnes
#> Stability clause (+20%/-30% compared to Ay,      | 
#>    only applied if b=1)                          |       Applied |           0.7
#> Catch advice for 2023 and 2024                   | 
#>    (Ay * stability clause)                       |                   1220 tonnes
#> Discard rate                                     |                           27%
#> Projected landings corresponding to advice       |                    890 tonnes
#> % advice change                                  |                          -30%
```


# The rb rule

The rb rule is essentially a simplified version of the rfb rule without component f, i.e. applicable to stocks without reliable catch length data. This method is meant as a last resort and should be avoid if possible.

The rb rule is an index adjusted harvest control rule that adjusts the catch advice based on a biomass index but does not have a target. The method is defined as Method 2.3 in the ICES Technical Guidelines [@ICES2022_cat23_tech_guidelines, page 15] as

$$
A_{y+1} = A_y \times r \times b \times m
$$

where $A_{y+1}$ is the new catch advice, $A_y$ the previous advice, $r$ the biomass ratio from a biomass index, $b$ a biomass safeguard and $m$ a precautionary multiplier. Furthermore, the change in the advice is restricted by a stability clause that only allows changes of between $+20\%$ and $-30\%$ relative to the previous advice, but the application of the stability clause is conditional on $b=1$ and turned off when $b<1$.

The rb rule should be applied biennially, i.e. the catch advice is valid for two years.

Please note that any change from the default configuration should be supported by case-specific simulations.

## Reference catch $A_y$

The reference catch $A_y$ is usually the most recently advised catch. In a typical ICES setting, an assessment is conducted in an assessment (intermediate) year $y$ to give advice for the following advice year $y+1$, this is the advice for year $y$:


```r
### load plaice catch and advice data
data("ple7e_catch")
tail(ple7e_catch)
#>    year advice landings discards catch
#> 31 2017   2714     2128      821  2949
#> 32 2018   3257     1880      633  2513
#> 33 2019   3648     1725      366  2091
#> 34 2020   2721     1373      514  1888
#> 35 2021   2177     1403      211  1615
#> 36 2022   1742       NA       NA    NA
### get reference catch
A <- A(ple7e_catch, units = "tonnes")
A
#> An object of class "A".
#> Value: 1742
```

The ICES Technical Guidelines [@ICES2022_cat23_tech_guidelines] specify that if the realised catch is very different from the advised catch, the reference catch could be replaced by an average of recent catches:


```r
### use 3-year average
A(ple7e_catch, units = "tonnes", basis = "average catch", avg_years = 3)
#> An object of class "A".
#> Value: 1864.66666666667
```

The reference catch can also be defined manually:


```r
### use 3-year average
A(2000, units = "tonnes")
#> An object of class "A".
#> Value: 2000
```

## Biomass index trend (ratio) $r$

The biomass index trend $r$ calculates the trend in the biomass index over last the five years, by dividing the mean of the last two years by the mean of the three preceding years:

$$
r = \Sigma_{i=y-2}^{y-1}(I_i/2)\ / \ \Sigma_{i=y-5}^{y-2}(I_i/3)
$$

The ratio is calculated with the function `r`. Index data should be provided as a `data.frame` with columns `year` and `index`.


```r
### load plaice data
data("ple7e_idx")
tail(ple7e_idx)
#>    year     index
#> 14 2016 1.3579990
#> 15 2017 1.3323659
#> 16 2018 1.1327596
#> 17 2019 0.8407277
#> 18 2020 0.5996326
#> 19 2021 1.0284297

### calculate biomass trend
r <- r(ple7e_idx, units = "kg/hr")
r
#> An object of class "rfb_r".
#> Value: 0.73871806243358

### ICES advice style table
advice(r)
#> --------------------------------------------------------------------------------
#> Stock biomass trend
#> --------------------------------------------------------------------------------
#> Index A (2020,2021)                              |                    0.81 kg/hr
#> Index B (2017,2018,2019)                         |                    1.10 kg/hr
#> r: stock biomass trend (index ratio A/B)         |                          0.74

### plot index
### horizontal orange lines correspond to the the 2/3-year averages
plot(r)
```

<img src="figure/unnamed-chunk-21-1.png" title="plot of chunk unnamed-chunk-21" alt="plot of chunk unnamed-chunk-21" width="500" style="display: block; margin: auto;" />

```r

### when the value of r is known
r(1)
#> An object of class "r".
#> Value: 1
```

Biomass index data is usually available until the year before the advise year. More recent data can be used and the function automatically picks the most recent data provided to it.

## Biomass safeguard $b$

The biomass safeguard reduces the advice when the biomass index $I$ falls below a threshold value $I_{\text{trigger}}$:

$$
b = \text{min}\{1,\ I_{y-1}/I_{\text{trigger}}\}
$$
By default, the trigger value is defined based on the lowest observed index value $I_{\text{loss}}$ as $I_{\text{trigger}} = 1.4I_{\text{loss}}$. 

The biomass safeguard is calculated with the function `b`:

```r
### use same plaice data as before
### application in first year with new calculation of Itrigger
b <- b(ple7e_idx, units = "kg/hr")
b
#> An object of class "b".
#> Value: 1

### ICES advice style table
advice(b)
#> --------------------------------------------------------------------------------
#> Biomass safeguard
#> --------------------------------------------------------------------------------
#> Last index value (I2021)                         |                    1.03 kg/hr
#> Index trigger value (Itrigger = Iloss x 1.4)     |                    0.39 kg/hr
#> b: index relative to trigger value,              |                          1.00
#>    min{I2021/Itrigger, 1}                        |

### plot
plot(b)
```

<img src="figure/unnamed-chunk-22-1.png" title="plot of chunk unnamed-chunk-22" alt="plot of chunk unnamed-chunk-22" width="500" style="display: block; margin: auto;" />

```r
### plot b and r in one figure
plot(r, b)
```

<img src="figure/unnamed-chunk-22-2.png" title="plot of chunk unnamed-chunk-22" alt="plot of chunk unnamed-chunk-22" width="500" style="display: block; margin: auto;" />

**Please note that $I_{\text{trigger}}$ should only be defined once in the first year of the application of the rb rule**. In the following years, the same value should be used. For this purpose, `b` allows the direct definition of $I_{\text{trigger}}$, or, more conveniently, $I_{\text{trigger}}$ can be based on the year in which $I_{\text{loss}}$ is defined:


```r
### in following years, Itrigger should NOT be updated
### i.e. provide value for Itrigger
b(ple7e_idx, units = "kg/hr", Itrigger = 0.3924721)
#> An object of class "b".
#> Value: 1
### alternatively, the reference year for Iloss can be used
b(ple7e_idx, units = "kg/hr", yr_ref = 2007)
#> An object of class "b".
#> Value: 1
```


## Multiplier $m$

The multiplier $m$ is a tuning parameter and ensures that the catch advice is precautionary in the long term. 

The value of $m$ is set to $m=0.5$ for all stocks. The multiplier can be calculated with the function `m()`:


```r
m <- m(hcr = "rb")
m
#> An object of class "m".
#> Value: 0.5

### alias for rb rule
rb_m()
#> An object of class "rb_m".
#> Value: 0.5
```

Please note that for the rb rule, the multiplier $m$ does lead to a continuous reduction in the catch advice because the rb rule does not include a target.

## Application of rb rule

Now we have all the components of the rb rule and can apply it:


```r
advice <- rb(A = A, r = r, b = b, m = m, discard_rate = 27)
advice
#> An object of class "rb".
#> Value: 1219.4
```

A discard rate in \% can be provided to the argument `discard_rate` and this means the advice is provided for the catch and landings.

The rb rule includes a stability clause that restricts changes to $+20%$ and $-30%$. This stability clause in conditional on the biomass safeguard and is only applied if $b=1$ but turned off when $b<1$.

`cat3advice` can print a table similar to the table presented in ICES advice sheets:

```r
advice(advice)
#> --------------------------------------------------------------------------------
#> Previous catch advice Ay (advised catch for 2022) |                   1742 tonnes
#> --------------------------------------------------------------------------------
#> Stock biomass trend
#> --------------------------------------------------------------------------------
#> Index A (2020,2021)                              |                       0.81 rb
#> Index B (2017,2018,2019)                         |                       1.10 rb
#> r: stock biomass trend (index ratio A/B)         |                          0.74
#> --------------------------------------------------------------------------------
#> Biomass safeguard
#> --------------------------------------------------------------------------------
#> Last index value (I2021)                         |                    1.03 kg/hr
#> Index trigger value (Itrigger = Iloss x 1.4)     |                    0.39 kg/hr
#> b: index relative to trigger value,              |                          1.00
#>    min{I2021/Itrigger, 1}                        |                              
#> --------------------------------------------------------------------------------
#> Precautionary multiplier to maintain biomass above Blim with 95% probability
#> --------------------------------------------------------------------------------
#> m: multiplier                                    |                          0.50
#>    (generic multiplier based on life history)    |                              
#> RB calculation (r*b*m)                           |                    640 tonnes
#> Stability clause (+20%/-30% compared to Ay,      | 
#>    only applied if b=1)                          |       Applied |           0.7
#> Catch advice for 2023 and 2024                   | 
#>    (Ay * stability clause)                       |                   1220 tonnes
#> Discard rate                                     |                           27%
#> Projected landings corresponding to advice       |                    890 tonnes
#> % advice change                                  |                          -30%
```

# References