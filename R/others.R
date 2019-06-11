#' Range normalization
#'
#' Normalize a vector, matrix or data.frame of numeric values in a specified
#' range.
#'
#' @param x vector, matrix or data.frame with at least two different elements
#' @param range vector of two elements specifying the desired normalized range.
#' Default value is c(0,1)
#' @return the normalized data
#'
#' @examples
#' vec = 1:10
#' normalize_to_range(vec)
#' normalize_to_range(vec, range = c(-1,1))
#'
#' mat = matrix(c(0,2,1), ncol = 3, nrow = 4)
#' normalize_to_range(mat, range = c(-5,5))
#'
#' @export
normalize_to_range = function(x, range = c(0,1)) {
  stopifnot(length(unique(x)) >= 2)

  x.max = max(x)
  x.min = min(x)

  a = range[1]
  b = range[2]

  res = a + (x - x.min)*(b-a)/(x.max - x.min)
  return(res)
}

#' Specify decimal
#'
#' Use this function to transform a given decimal number to the desired
#' precision by choosing the number of digits after the decimal point.
#'
#' @param number numeric
#' @param digits.to.keep numeric. Refers to the digits to keep after decimal
#' point '.'
#'
#' @return the pruned number in string format
#'
#' @examples
#' # 0.123
#' specify_decimal(0.1233213, 3)
#'
#' @export
specify_decimal = function(number, digits.to.keep) {
  trimws(format(round(number, digits.to.keep), nsmall = digits.to.keep))
}

#' Remove commented and empty lines
#'
#' Removes empty or commented lines from a character vector (each element being
#' a line)
#'
#' @param lines a character vector, usually the result from using the
#' \code{\link{readLines}} function
#'
#' @return a character vector of the pruned lines
#'
#' @export
remove_commented_and_empty_lines = function(lines) {
  commented.or.empty.lines = character(0)
  for (line in lines) {
    if (startsWith(line, "#") || trimws(line) == "") {
      commented.or.empty.lines = c(commented.or.empty.lines, line)
    }
  }
  pruned.lines = lines[!lines %in% commented.or.empty.lines]
  return(pruned.lines)
}

#' Retrieve the parent directory
#'
#' Use this function to retrieve the parent directory from a string representing
#' the full path of a file or a directory.
#'
#' @param pathStr string. The name of the directory, can be a full path filename.
#'
#' @return a string representing the parent directory. When a non-file path is
#' used as input (or something along those lines :) then it returns the root
#' ("/") directory.
#'
#' @examples
#' get_parent_dir("/home/john")
#' get_parent_dir("/home/john/a.txt")
#' get_parent_dir("/home")
#'
#' @export
get_parent_dir = function(pathStr) {
  parts = unlist(strsplit(pathStr, "/"))
  parent.dir = do.call(file.path, as.list(parts[1:length(parts) - 1]))

  if (parent.dir == "" || is_empty(parent.dir)) return("/")
  else return(parent.dir)
}

#' Matrix equality
#'
#' Check if two matrices are equal. Equality is defined by both of them being
#' matrices in the first place, having the same dimensions as well as the same
#' elements.
#'
#' @param x,y matrices
#'
#' @return a logical specifying if the two matrices are equal or not.
#'
#' @export
mat_equal = function(x, y) {
  is.matrix(x) && is.matrix(y) && all(dim(x) == dim(y)) && all(x == y)
}

#' Is object empty?
#'
#' A function to test whether an object is \strong{NULL, empty} or something
#' similar :) It checks the length of the object, so it has different behaviour
#' than \code{\link{is.null}}.
#'
#' @param obj a general object
#'
#' @return a logical specifying if the object is NULL or not.
#'
#' @examples
#' # TRUE
#' is_empty(NULL)
#' is_empty(c())
#'
#' # FALSE
#' is_empty("")
#' is_empty(NA)
#' is_empty(NaN)
#'
#' @export
is_empty = function(obj) {
  if (length(obj)) return(FALSE) else return(TRUE)
}

#' Outersect
#'
#' Performs set outersection on two vectors. The opposite operation from
#' \code{intersect}!
#'
#' @param x,y vectors
#'
#' @return a vector of the non-common elements of x and y.
#'
#' @examples
#' x = 1:10
#' y = 2:11
#'
#' # c(1,11)
#' outersect(x,y)
#'
#' @seealso \code{\link{intersect}}
#'
#' @export
outersect = function(x, y) {
  sort(c(setdiff(x, y), setdiff(y, x)))
}

#' Is value between two others?
#'
#' This function checks if a given value is inside an interval specified by
#' two boundary values.
#'
#' @param value numeric
#' @param low.thres numeric. Lower boundary of the interval.
#' @param high.thres numeric. Upper boundary of the interval.
#' @param include.high.value logical. Whether the upper bound is included in the
#' interval or not. Default value: FALSE.
#'
#' @return a logical specifying if the \code{value} is inside the interval
#' \code{[low.thres,high.thres)} (default behaviour) or inside the interval
#' \code{[low.thres,high.thres]} if \code{include.high.value} is TRUE.
#'
#' @examples
#' is_between(3,2,4)
#' is_between(4,2,4)
#' is_between(4,2,4,include.high.value=TRUE)
#'
#' @export
is_between = function(value, low.thres, high.thres, include.high.value = FALSE) {
  if (include.high.value) return(value >= low.thres & value <= high.thres)
  else return(value >= low.thres & value < high.thres)
}
