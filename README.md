
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmstats

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/specr)](https://CRAN.R-project.org/package=specr)
<!-- badges: end -->

This package includes some custom-made functions to facilitate some
common statistical procedures as well as extracting and reporting
results from various models in RMarkdown articles. Please note that most
functions are highly costumized to my personal workflow. They may hence
break in more general frameworks or when used in a different,
non-intended way…

## Dependencies

Most functions require the following packages:

  - For data wrangling procedures: `tidyverse` and `magrittr`
  - For printing and typesetting: `papaja`
  - For SEM procedures: `lavaan` and `semTools`

These packages should be installed prior to using this package.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("masurp/pmstats")
```

# Some examples

## Printing a zero order correlation table

``` r
library(pmstats)
(tab <- zero_order_corr(mtcars[,1:6], 
                        digits = 2, 
                        sig = T, 
                        print = T))
#>   Variables      M     SD     1     2     3     4     5
#> 1     1 mpg  20.09   6.03                              
#> 2     2 cyl   6.19   1.79 -.85*                        
#> 3    3 disp 230.72 123.94 -.85*  .90*                  
#> 4      4 hp 146.69  68.56 -.78*  .83*  .79*            
#> 5    5 drat   3.60   0.53  .68* -.70* -.71* -.45*      
#> 6      6 wt   3.22   0.98 -.87*  .78*  .89*  .66* -.71*
```

``` r
papaja::apa_table(tab, 
                  format = "html",
                  align = c("l", rep("r", 6)))
```



| Variables |      M |     SD |       1 |       2 |       3 |       4 | 5       |
| :-------- | -----: | -----: | ------: | ------: | ------: | ------: | :------ |
| 1 mpg     |  20.09 |   6.03 |         |         |         |         |         |
| 2 cyl     |   6.19 |   1.79 | \-.85\* |         |         |         |         |
| 3 disp    | 230.72 | 123.94 | \-.85\* |   .90\* |         |         |         |
| 4 hp      | 146.69 |  68.56 | \-.78\* |   .83\* |   .79\* |         |         |
| 5 drat    |   3.60 |   0.53 |   .68\* | \-.70\* | \-.71\* | \-.45\* |         |
| 6 wt      |   3.22 |   0.98 | \-.87\* |   .78\* |   .89\* |   .66\* | \-.71\* |

## Extracting result table from structural equation model

``` r
library(lavaan)

# Estimate SEM
model.sem <- '
  # latent variables
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
  # regressions
  dem60 ~ a*ind60
  dem65 ~ b*ind60 + c*dem60
  # residual covariances
  y1 ~~ y5
  y2 ~~ y4 + y6
  y3 ~~ y7
  y4 ~~ y8
  y6 ~~ y8
'
fit.sem <- sem(model.sem, 
               data = PoliticalDemocracy)

# Extracting results (only regression paths )
(results <- result_table(fit.sem, 
                         sem_regressions = TRUE, 
                         new_labels = c("H1", "H2", "H3"), 
                         print = TRUE))
#> # A tibble: 3 x 9
#>   outcome predictor label b     se    ll    ul    p      beta 
#>   <chr>   <chr>     <chr> <chr> <chr> <chr> <chr> <chr>  <chr>
#> 1 dem60   ind60     H1    1.48  0.40  0.70  2.27  < .001 .45  
#> 2 dem65   ind60     H2    0.57  0.22  0.14  1.01  .010   .18  
#> 3 dem65   dem60     H3    0.84  0.10  0.64  1.03  < .001 .89

# Print specific results
print_coeff(results, "H2", 
            se = FALSE, 
            beta = TRUE)
#> [1] "$b = 0.57$, 95\\% CI $[0.14, 1.01]$, $p = .010$, $\\beta = .18$"
```
