% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factors.R
\name{prime_factors}
\alias{prime_factors}
\title{Prime Factors of a Number}
\usage{
prime_factors(x, unique = TRUE)
}
\arguments{
\item{x}{The number that you want the prime factors of.}

\item{unique}{Logical. Should the function return all prime factors
(where \code{prod(prime_factors(x)) == x}) or just the unique prime factors?
Defaults to \code{TRUE}.}
}
\value{
A numeric vector either with the repeated prime factorization
of the input or just the unique prime factors of the input.
}
\description{
The \code{prime_factors} function is used to calculate the prime factors of
a positive integer greater than 1.
}
\note{
Returns \code{NULL} for a value of 1, and generates an error for
values less than 1.
}
\examples{
prime_factors(100, unique = FALSE)
prime_factors(100)
}
