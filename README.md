# usefun

<!-- badges: start -->
[![R build status](https://github.com/bblodfon/usefun/workflows/R-CMD-check/badge.svg)](https://github.com/bblodfon/usefun/actions)
[![codecov](https://codecov.io/gh/bblodfon/usefun/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bblodfon/usefun)
[![CRAN status](https://www.r-pkg.org/badges/version/usefun)](https://cran.r-project.org/package=usefun)
[![Downloads](https://cranlogs.r-pkg.org/badges/usefun)](https://cran.r-project.org/package=usefun)
<!-- badges: end -->

A set of miscellaneous functions that I have used in various projects and in other R packages. 
Some of the most important are:

- `powerset_icounts()`: find common (patient) ids in incomplete multi-omic datasets
- `get_roc_stats()`: the ROC statistics the way you want it!
- `pr.test()`: compare two PR curves (AUCs) and get a p-value!
- `pr.boot()`: bootstrap CIs for PR curve
- `partial_permut()`: get a partially-scrambled vector (you choose how much!)
- `normalize_to_range()`: normalize a vector to a specified range
- `dec_to_bin()`: convert a decimal (base-10) number to its binary representation
- `get_percentage_of_matches()`: find the percentage of common elements between two vectors
- `outersect()`: find the non-common elements between two vectors (outer-section!)
- `colors.100`: a vector of 100 as much as possible *distinct* colors

## Install

CRAN version:
```
install.packages("usefun")
```

Development version:
```
devtools::install_github("bblodfon/usefun")
```

## Examples

See examples in the functions documentation and in the respective tests.
For a full biomarker analysis that uses various functions from this package, see [this report](https://druglogics.github.io/gitsbe-model-analysis/atopo/cell-lines-2500/).
