% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factors.R
\name{least_common_multiple}
\alias{least_common_multiple}
\title{Least Common Multiple of a Set of Numbers}
\usage{
least_common_multiple(...)
}
\arguments{
\item{\dots}{The numbers for which you want the least common multiple.}
}
\value{
A single integer value representing the least common multiple
of the set of inputs.
}
\description{
The \code{least_common_multiple} function takes a set of numbers and
calculates their least common multiple using the prime factorization
method.
}
\note{
The absolute values of the input is used in calculating the
least common multiple.
}
\examples{
least_common_multiple(4, 7, 11)
}
