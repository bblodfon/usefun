## Test environments
* local OS X install, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.6.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes on email

I received from Martina Schmirl an email with the following points:

1. Most packages are a collection of useful functions. Please consider using a more meaningful title.
2. Please do not start the description with "This package", package name, title or similar.
3. The Description field is intended to be a (one paragraph) description of what the package does and why it may be useful. Please elaborate.
4. Please add \value to .Rd files regarding methods and explain the functions results in the documentation.
(See: Writing R Extensions <https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Documenting-functions>)
5. Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the
package directory and getwd()). That is not allowed by CRAN policies.
Please only write/save files if the user has specified a directory in
the function themselves. Therefore please omit any default path =
getwd() in writing functions.
In your examples/vignettes/tests you can write to tempdir().
6. \dontrun{} should be only used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user.
Does not seem necessary.
Please unwrap the example.

Here are my reply notes/comments:

1. I wrote this package to be used by another R package that I want to submit afterwards and in other R analyses that I am currenlty writing. Since it's just a collection of general functions useful at least to me, I really couldn't think of a more appropriate title than 'usefun' :) For the Title I took as template an already CRAN published 
package (see [xfun](https://cran.r-project.org/web/packages/xfun/index.html)).
2. Updated the Description so that it does not start the same as the Title.
3. Updated the Description field to give a full coverage of what my functions can do.
4. The only functions that I don't have the return result (generated from \value) 
is the plotting, printing and saving (to file) ones. But I think that is okay, 
because neither `plot()`, `print()` or `write.table` (sort of equivalent) have 
a return value in their respective function documentation.
5. None of my functions write to the package directory or use getwd().
6. Unwrapped example and used `tempdir()` to store the result.
