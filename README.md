# usefun

<!-- badges: start -->
[![codecov](https://codecov.io/gh/bblodfon/usefun/branch/master/graph/badge.svg)](https://codecov.io/gh/bblodfon/usefun)
[![CRAN status](https://www.r-pkg.org/badges/version/usefun)](https://cran.r-project.org/package=usefun)
[![Downloads](https://cranlogs.r-pkg.org/badges/usefun)](https://cran.r-project.org/package=usefun)
<!-- badges: end -->

A set of miscellaneous functions that I have used in various projects and in other R packages. 
Some of the most important are:

- `get_roc_stats`: the ROC statistics the way you want it!
- `partial_permut`: get a partially-scrambled vector (you choose how much!)
- `normalize_to_range`: normalize a vector to a specified range
- `dec_to_bin`: convert a decimal (base-10) number to its binary representation
- `get_percentage_of_matches`: find the percentage of common elements between two vectors
- `pretty_print_*`: pretty printing of vector names and values in an R Markdown document (as quotes)
- `outersect`: find the non-common elements between two vectors (outer-section!)
- `ldf_arrange_by_rownames`: rearrange a list of data frames by rownames
- `make_multiple_density_plot`: plotting multiple density estimations in one plot
- `usefun:::colors.100`: a vector of 100 as much as possible *distinct* colors

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
Also various functions from this package have been used in this [biomarker analysis](https://bblodfon.github.io/gitsbe-model-analysis/atopo/cell-lines-2500/).
