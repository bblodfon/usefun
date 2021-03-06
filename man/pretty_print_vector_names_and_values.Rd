% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{pretty_print_vector_names_and_values}
\alias{pretty_print_vector_names_and_values}
\title{Pretty printing of a vector's names and values}
\usage{
pretty_print_vector_names_and_values(vec, n = -1)
}
\arguments{
\item{vec}{vector with \code{names} attribute}

\item{n}{the number of elements that you want to print in a nice way. Default
value: -1 (pretty print all elements). For any n < 1, all elements are
printed.}
}
\description{
It outputs a vector's names and values in this format: \emph{name1: value1,
name2: value2,...}. You can choose how many elements to show in this format.
Use with the chuck option \emph{results = 'asis'} to get a nice printing in
an R notebook.
}
\seealso{
\code{\link{pretty_print_name_and_value}}
}
