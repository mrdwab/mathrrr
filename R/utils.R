# Fraction Helpers --------------------------------------------------------

#' Function to Reduce Fractional Part of Fractions
#'
#' @param who The whole number part of the fraction
#' @param num The numerator of the fraction
#' @param den The denominator of the fraction
#' @param cla The class of the fraction
#'
#' @noRd
.frac_reduce <- function(who, num, den, cla) {
  whole <- who
  cl <- cla
  if (any(c(num == 0, den == 0))) {
    cl <- "whole"
  } else {
    if (num == den) {
      whole <- whole + 1L
      cl <- "whole"
    } else {
      tmp <- .frac(num = num, den = den)
      if (tmp[[2]] == 1L) {
        whole <- whole + tmp[[1]]
        cl <- "whole"
      } else {
        numerator <- tmp[[1]]
        denominator <- tmp[[2]]
      }
    }
  }
  list(whole = whole,
       numerator = if (cl == "whole") NULL else numerator,
       denominator = if (cl == "whole") NULL else denominator, cl = cl)
}

#' Reduces an Integer Representing the Decimal Part of a Number to a Fraction
#'
#' @param num The initial integer
#' @param den The initial denominator. Defaults to `NULL`.
#'
#' @note This recursively reduces the numerator and denominator
#' of the initial integer. For example, starting with `num = 124`,
#' `den` will be automatically calculated to be 1000. This reduces
#' to `num = 62, den = 500`, which can further be reduced to
#' `num = 32, den = 250`,
#'
#' @noRd
.frac <- function(num, den = NULL) {
  if (is.null(den)) den <- 10^nchar(num)
  a <- prime_factors(num)
  b <- prime_factors(den)
  while (any(as.logical(intersect(a, b)))) {
    m <- prod(intersect(a, b))
    num <- num/m
    den <- den/m
    a <- prime_factors(num)
    b <- prime_factors(den)
  }
  list(num, den)
}

#' Checks Whether the Input is an Integer
#'
#' @param x The value to be checked
#'
#' @noRd
.isInteger <- function(x) all(as.numeric(x) == as.integer(x))

#' Returns the Reciprocal of a Fraction
#'
#' @param x The fraction object
#'
#' @noRd
.reciprocal <- function(x) {
  x[c("numerator", "denominator")] <- x[c("denominator", "numerator")]
  x
}

#' Prepares Two Fractions for Comparison
#'
#' Prepares two fractions for comparison by converting them using
#' their greatest common denominator.
#'
#' @param x1 The first fraction.
#' @param x2 The second fraction.
#'
#' @noRd
.comparison <- function(x1, x2) {
  e1 <- as_improper(x1)
  e2 <- as_improper(x2)
  gcd <- least_common_multiple(e1$denominator, e2$denominator)
  list(num1 = e1$numerator * (gcd / e1$denominator) * e1$sign,
       num2 = e2$numerator * (gcd / e2$denominator) * e2$sign,
       gcd = gcd)
}

#' Print Method for Fraction Objects
#'
#' @param x The input fraction object
#' @param \dots Not used
#'
#' @noRd
#' @export
print.fraction <- function(x, ...) {
  cl <- intersect(class(x), c("improper", "simplified", "whole"))
  out <- switch(
    cl,
    improper = sprintf("%s/%s", format(x[["numerator"]] * x[["sign"]], scientific = FALSE),
                       format(x[["denominator"]], scientific = FALSE)),
    simplified = if (x[["whole"]] == 0) {
      sprintf("%s/%s", format(x[["sign"]] * x[["numerator"]], scientific = FALSE),
              format(x[["denominator"]], scientific = FALSE))
    } else {
      sprintf("%s %s/%s", format(x[["sign"]] * x[["whole"]], scientific = FALSE),
              format(x[["numerator"]], scientific = FALSE),
              format(x[["denominator"]], scientific = FALSE))
    },
    whole = format(x[["whole"]] * x[["sign"]], scientific = FALSE))
  print(out)
}

# Grouped Function Helpers ------------------------------------------------

#' Splits Character Input Into Intervals
#'
#' @param intervals The input character vector
#' @param sep The delimiter
#' @param trim A pattern to be removed from the left and right of the input
#'
#' @noRd
.grp_intervals <- function(intervals, sep, trim) {
  if (!is.null(sep)) {
    if (is.null(trim)) {
      pattern <- ""
    } else if (trim == "cut") {
      pattern <- "\\[|\\]|\\(|\\)"
    } else {
      pattern <- trim
    }
    matrix(
      as.numeric(unlist(strsplit(gsub(pattern, "", intervals), sep), use.names = FALSE)),
      ncol = 2, byrow = TRUE)
  }
}

#' Lower Boundary and Width of Interval Class
#'
#' @param intervals Intervals, as created using `.grp_intervals`
#' @param ind The position of the desired interval class
#'
#' @noRd
.grp_lw <- function(intervals, ind) {
  if (ind == 1) {
    L <- intervals[ind, 1]
    w <- abs(diff(intervals[ind, ]))
  } else {
    if (intervals[ind, 1] == intervals[(ind-1), 2]) {
      L <- intervals[ind, 1]
      w <- abs(diff(intervals[ind, ]))
    } else {
      L <- mean(c(intervals[ind, 1], intervals[(ind-1), 2]))
      x <- abs(intervals[ind, 1] - L)
      w <- abs((intervals[ind, 2] + x) - L)
    }
  }
  list(L, w)
}

