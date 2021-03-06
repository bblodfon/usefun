% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/operations.R
\name{partial_permut}
\alias{partial_permut}
\title{Get partial permutation of a vector}
\usage{
partial_permut(x, exp_sim = 0)
}
\arguments{
\item{x}{a vector with at least 2 elements}

\item{exp_sim}{a value between 0 and 1 indicating the level of \emph{expected
similarity} between the input and output vector. Default value is \strong{0}
(random permutation).}
}
\value{
a partially (random) permutated vector. If \code{exp_sim = 0} then
the result is equal to \code{sample(x)} (a random permutation). If
\code{exp_sim = 1} then the result is always the same as the input vector.
For \code{exp_sim} values between \emph{0} and \emph{1} we randomly sample
a subset of the input vector inversely proportionate to the \code{exp_sim}
value (e.g. \code{exp_sim = 0.8 => 20\%} of the elements) and randomly
permutate these elements only.
}
\description{
Get partial permutation of a vector
}
\examples{
set.seed(42)
partial_permut(x = LETTERS, exp_sim = 0)
partial_permut(x = LETTERS, exp_sim = 0.5)
partial_permut(x = LETTERS, exp_sim = 0.9)

}
