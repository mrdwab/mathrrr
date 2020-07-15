#' Basic Arithmetic with Fractions
#'
#' Functions for doing basic arithmetic with fractions.
#'
#' @param x1 The first fraction to be used.
#' @param x2 The second fraction to be used.
#'
#' @examples
#' # Use as a function
#' add_fractions("1/2", "1/4")
#'
#' # Use with the defined infix operator
#' "1/8" %f+% "2/3"
#'
#' "1/2" %f/% "1/2"
#'
#' # Infix operators can be chained
#' # Operations go from left to right
#' "-2" %f-% "1/2" %f+% "2 1/3"
#'
#' # Use parentheses to control order of operations
#' "-2" %f-% "1/2" %f+% "2 1/3" %f*% "2"
#'
#' "-2" %f-% "1/2" %f+% ("2 1/3" %f*% "2")
#'
#' @name fraction_arithmetic
NULL

#' @rdname fraction_arithmetic
#' @export
add_fractions <- function(x1, x2) {
  tmp <- .comparison(x1, x2)
  numerator <- tmp$num1 + tmp$num2
  out <- structure(list(
    whole = 0,
    numerator = abs(numerator),
    denominator = tmp$gcd,
    sign = sign(numerator)),
    class = c("list", "fraction", "improper"))
  as_simplified(out)
}
NULL

#' @rdname fraction_arithmetic
#' @export
subtract_fractions <- function(x1, x2) {
  tmp <- .comparison(x1, x2)
  numerator <- tmp$num1 - tmp$num2
  out <- structure(list(
    whole = 0,
    numerator = abs(numerator),
    denominator = tmp$gcd,
    sign = sign(numerator)),
    class = c("list", "fraction", "improper"))
  as_simplified(out)
}
NULL

#' @rdname fraction_arithmetic
#' @export
multiply_fractions <- function(x1, x2) {
  e1 <- as_improper(x1)
  e2 <- as_improper(x2)
  out <- structure(list(
    whole = 0,
    numerator = e1[["numerator"]] * e2[["numerator"]],
    denominator = e1[["denominator"]] * e2[["denominator"]],
    sign = e1[["sign"]] * e2[["sign"]]),
    class = c("list", "fraction", "improper"))
  as_simplified(out)
}
NULL

#' @rdname fraction_arithmetic
#' @export
divide_fractions <- function(x1, x2) {
  e1 <- as_improper(x1)
  e2 <- as_improper(x2)
  multiply_fractions(e1, .reciprocal(e2))
}
NULL

#' @rdname fraction_arithmetic
#' @export
`%f+%` <- add_fractions
NULL

#' @rdname fraction_arithmetic
#' @export
`%f-%` <- subtract_fractions
NULL

#' @rdname fraction_arithmetic
#' @export
`%f*%` <- multiply_fractions
NULL

#' @rdname fraction_arithmetic
#' @export
`%f/%` <- divide_fractions
NULL
