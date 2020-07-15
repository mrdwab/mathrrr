#' Basic Comparison of Fractions
#'
#' Operations for comparing pairs of fractions.
#'
#' @param x1 The first fraction to be used.
#' @param x2 The second fraction to be used.
#'
#' @return A logical `TRUE` or `FALSE`.
#'
#' @examples
#' "3 1/2" %f<% "14/4"
#' "3 1/2" %f==% "14/4"
#' "9/17" %f>% "10/19"
#' "-2/3" %f==% ("-1/3" %f*% "2")
#'
#' @name fraction_comparison
NULL

#' @rdname fraction_comparison
#' @export
`%f<%` <- function(x1, x2) {
  do.call("<", .comparison(x1, x2))
}
NULL

#' @rdname fraction_comparison
#' @export
`%f>%` <- function(x1, x2) {
  do.call(">", .comparison(x1, x2))
}
NULL

#' @rdname fraction_comparison
#' @export
`%f<=%` <- function(x1, x2) {
  do.call("<=", .comparison(x1, x2))
}
NULL

#' @rdname fraction_comparison
#' @export
`%f>=%` <- function(x1, x2) {
  do.call(">=", .comparison(x1, x2))
}
NULL

#' @rdname fraction_comparison
#' @export
`%f==%` <- function(x1, x2) {
  do.call("==", .comparison(x1, x2))
}
NULL

#' @rdname fraction_comparison
#' @export
`%f!=%` <- function(x1, x2) {
  do.call("!=", .comparison(x1, x2))
}
