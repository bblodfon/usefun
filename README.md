# usefun

<!-- badges: start -->
  [![Travis build status](https://travis-ci.org/bblodfon/usefun.svg?branch=master)](https://travis-ci.org/bblodfon/usefun)
  [![codecov](https://codecov.io/gh/bblodfon/usefun/branch/master/graph/badge.svg)](https://codecov.io/gh/bblodfon/usefun)
<!-- badges: end -->

A set of general functions that I have used in various 
projects and in other R packages. They support some miscellaneous operations 
on data frames, matrices and vectors: adding a row on a ternary (3-value)
data.frame based on positive and negative vector-indicators, rearranging a 
list of data.frames by rownames, pruning rows or columns of a data.frame 
that contain only one specific value given by the user, checking for matrix equality,
pruning and reordering a vector according to the common elements between its 
names and elements of another given vector, finding the non-common elements 
between two vectors (outer-section), 
normalization of a vector, matrix or data.frame's numeric values in a specified range, 
pretty printing of vector names and values in an R notebook (common names and 
values between two vectors also supported), retrieving the parent directory of 
any string path, checking whether a numeric value is inside a given interval, 
trim the decimal points of a given numeric value, quick saving of data to a file, 
making a multiple densities plot and a color bar plot and executing a plot 
string expression while generating the result to the specified file format.

## Install

```
devtools::install_github("bblodfon/usefun")
```
## Examples

See examples in the functions documentation and in the respective tests. Also various 
functions from this package have been used in this [biomarker analysis](https://bblodfon.github.io/gitsbe-model-analysis/atopo/cell-lines-2500/).
