
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SIStree

<!-- badges: start -->
<!-- badges: end -->

The goal of SIStree is to provides a fast and efficient ranking by the
importance of variables for ultra-high dimensional data.

## Installation

You can install the development version of SIStree from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("liuyu-star/SIStree")
```

## Usage

We show how to use the SIStree package with examples.

- Simulation data with continuous response.

``` r
library(SIStree)
X = matrix(rnorm(100*1000), 100, 1000)
y = X[,1] + X[,2]^2 + 2*X[,4]*X[,5] + rnorm(100)
sis = SIStree(X, y)
# show the first 20 variables ranked by SIS
sis$VarImp$rank.SIS[1:20]
#>  [1]   2   1 847   4   5 293 501 318 908 456 447 636 852 735 783 713 591 460  11
#> [20] 756
# show the first 20 variables ranked by SIStree
sis$VarImp$rank.SIStree[1:20]
#>  [1]   2   1   4 293 636   5 447 847 366 456 460 852 908 129 501 916 783 404 756
#> [20] 735

set.seed(230826)
sis = SISforest(X, y, parallel=FALSE)
# show the first 20 variables ranked by SISforest
sis$VarImp$rank.SISforest[1:20]
#>  [1]   2   1   4   5 847 501 293  77 456 460 908 916 447 852  11 636 756 783 129
#> [20] 318
```

- Real data with categorical response.

``` r
data(breast_cancer)
sis <- SIStree(as.factor(diagnosis) ~ ., breast_cancer[,-1], criteria='Gini')
#show variables ranked by SIStree
sis$VarImp$rank.SIStree
#>  [1] 15 19 12 10 20  9 30  5 16 29 25 18 22 17  2 26  6 13 11 27 14  1  3  4  7
#> [26]  8 23 24 28 21

set.seed(230826)
sis <- SISforest(as.factor(diagnosis) ~ ., breast_cancer[,-1],
        criteria='Gini', parallel=FALSE)
#show variables ranked by SISforest
sis$VarImp$rank.SISforest
#>  [1] 22 18  2  5 27 14 10  9 17  3 24  1 30 21 12  4 23 25 19 11 26 28 15  7  8
#> [26] 13  6 29 20 16
```
