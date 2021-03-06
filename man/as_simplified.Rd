% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fractions.R
\name{as_simplified}
\alias{as_simplified}
\title{Simplifies Parsed Fractions to Proper or Mixed Fraction Form}
\usage{
as_simplified(fraction)
}
\arguments{
\item{fraction}{The input fraction, already parsed, or a string to
be parsed using \code{parse_fraction(fraction, improper = FALSE, reduce = TRUE)}.}
}
\description{
Given a string or a fraction parsed using \code{\link[=parse_fraction]{parse_fraction()}}, this
function will check to see whether it is in its simplified form,
and if not, it will convert it to a proper or mixed fraction, depending
on whether the parsed number has a whole number component.
}
\examples{
as_simplified("17/2")

}
