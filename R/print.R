#' Pretty print a string
#'
#' Nice printing of a string in an R notebook (default behaviour). Otherwise,
#' it prints the string to the standard R output.
#'
#' @param string a string
#' @param with.gt logical. Determines if the ">" sign will be appended for nice
#' printing in an R notebook. Default value: TRUE.
#'
#' @seealso \code{\link{cat}}
#'
#' @export
pretty_print_string = function(string, with.gt = TRUE) {
  if (with.gt)
    cat(paste0("> ", string))
  else
    cat(string)
}

#' Pretty print a bold string
#'
#' Prints a bold string only when `html.output` is enabled. Otherwise, it prints
#' a normal string. The the ">" sign can be appended if nice output in an R
#' notebook is desired.
#'
#' @param string a string
#' @param with.gt logical. Determines if the ">" sign will be appended for nice
#' printing in an R notebook. Default value: TRUE.
#' @param html.output logical. If TRUE, it encapsulates the string with the bold
#' tags for an HTML document. Default value: TRUE.
#'
#' @seealso \code{\link{pretty_print_string}}
#'
#' @export
pretty_print_bold_string =
  function(string, with.gt = TRUE, html.output = TRUE) {
    if (html.output) {
      bold.string = paste0("<b>", string, "</b>")
      if (with.gt)
        cat(paste0("> ", bold.string))
      else
        cat(bold.string)
    } else {
      pretty_print_string(string, with.gt = with.gt)
    }
}

#' Print an empty line
#'
#' @param html.output logical. If TRUE, it outputs an empty line for an HTML
#' document, else an empty line for the standard R output. Default value:
#' FALSE.
#'
#' @seealso \code{\link{cat}}
#'
#' @export
print_empty_line = function(html.output = FALSE) {
  if (html.output)
    cat("<br/>")
  else
    cat("\n")
}

#' Pretty printing of a vector's names attribute
#'
#' @param vec vector
#' @param vector.names.str string. It tell us what are the names of the
#' vector (use plural form) in order to fill the print message. Default value:
#' "nodes".
#' @param sep string. The seperator character to use to distinguish between
#' the names values. Default value: ", ".
#' @param with.gt logical. Determines if the ">" sign will be appended for nice
#' printing in an R notebook. Default value: TRUE.
#'
#' @seealso \code{\link{pretty_print_string}}
#'
#' @export
pretty_print_vector_names = function(vec, vector.names.str = "nodes",
                                     sep = ", ", with.gt = TRUE) {
  if (length(vec) == 1) {
    vector.names.str = substr(vector.names.str, start = 1,
                              stop = nchar(vector.names.str) - 1)
  }
  pretty_print_string(paste0(length(vec), " ", vector.names.str, ": ",
                      paste0(names(vec), collapse = sep)), with.gt)
}

#' Pretty printing of a vector's values
#'
#' @param vec vector
#' @param vector.values.str string. It tell us what are the values of the
#' vector (use plural form) in order to fill the print message. Default value:
#' "nodes".
#' @param sep string. The seperator character to use to distinguish between
#' the vector values. Default value: ", ".
#' @param with.gt logical. Determines if the ">" sign will be appended for nice
#' printing in an R notebook. Default value: TRUE.
#'
#' @seealso \code{\link{pretty_print_string}}
#'
#' @export
pretty_print_vector_values = function(vec, vector.values.str = "nodes",
                                      sep = ", ", with.gt = TRUE) {
  if (length(vec) == 1) {
    vector.values.str = substr(vector.values.str, start = 1,
                              stop = nchar(vector.values.str) - 1)
  }
  pretty_print_string(paste0(length(vec), " ", vector.values.str, ": ",
                      paste0(vec, collapse = sep)), with.gt)
}

#' Pretty printing of a vector's names and values
#'
#' It outputs a vector's names and values in this format: \emph{name1: value1,
#' name2: value2,...}. You can choose how many elements to show in this format.
#'
#' @param vec vector with \code{names} attribute
#' @param n the number of elements that you want to print in a nice way. Default
#' value: -1 (pretty print all elements). For any n < 1, all elements are
#' printed.
#'
#' @seealso \code{\link{pretty_print_name_and_value}}
#'
#' @export
pretty_print_vector_names_and_values = function(vec, n = -1) {
  len = length(vec)
  stopifnot(len > 0)

  # print all elements by default
  if (n == -1) n = len

  vec.names = names(vec)
  if (len == 1) {
    pretty_print_name_and_value(vec.names, vec, with.gt = TRUE, with.comma = FALSE)
  } else {
    # limit elements to show
    if (n >= 1 & n < len)
      last.index = n
    else
      last.index = len

    for (index in 1:last.index) {
      name = vec.names[index]
      value = vec[index]
      if (index == 1 & index != last.index)
        pretty_print_name_and_value(name, value, with.gt = TRUE, with.comma = TRUE)
      if (index == 1 & index == last.index)
        pretty_print_name_and_value(name, value, with.gt = TRUE, with.comma = FALSE)
      if (index != 1 & index != last.index)
        pretty_print_name_and_value(name, value)
      if (index != 1 & index == last.index)
        pretty_print_name_and_value(name, value, with.comma = FALSE)
    }
  }
}

#' Pretty print a name and value
#'
#' @param name string
#' @param value string
#' @param with.gt logical. Determines if the ">" sign will be appended for nice
#' printing in an R notebook. Default value: FALSE.
#' @param with.comma logical. Determines if the comma (,) character will be
#' appended to the end of the output. Default value: TRUE.
#'
#' @examples
#' pretty_print_name_and_value("aName", "aValue", with.gt = TRUE)
#' pretty_print_name_and_value("aName", "aValue", with.comma = FALSE)
#' @export
pretty_print_name_and_value =
  function(name, value, with.gt = FALSE, with.comma = TRUE) {
    if (with.comma) {
      pretty_print_string(string = paste0(name, ": ", value, ", "),
                          with.gt = with.gt)
    }
    else
      pretty_print_string(string = paste0(name, ": ", value), with.gt = with.gt)
}
