#' Convert a Decimal to an Approximate Fraction
#'
#' @param number The decimal you want to convert to a fraction.
#' @param precision The number of digits to round the decimal to before trying
#' to convert the result to a fraction. Must be greater than 1 but less than 8.
#' @param improper Logical. Should the fraction be a returned as an improper
#' fraction or a proper fraction? Defaults to `TRUE`.
#' @return A formatted `list` printed with `print.fraction()`. The `list`
#' includes four elements:
#' * `whole`: The absolute value of the whole number part of the decimal. This
#' is `NULL` if `improper = TRUE`.
#' * `numerator`: The numerator of the resulting fraction.
#' * `denominator`: The denominator of the resulting fraction.
#' * `sign`: `-1` if the input is negative; `1` if the input is positive.
#' @examples
#' as_fraction(3.2454)
#' as_fraction(3.2454, 2, TRUE)
#' as_fraction(3.2454, 2, FALSE)
#' as_fraction(3.2454, 1, FALSE)
#' @export
as_fraction <- function(number, precision = 3, improper = TRUE) {
  if (as.integer(number) == as.numeric(number)) {
    structure(list(whole = abs(as.integer(number)),
                   numerator = NULL,
                   denominator = NULL,
                   sign = sign(number)),
              class = c("fraction", "whole", "list"))
  } else {
    if (precision <= 0) stop("frac is intended for decimals")
    if (precision >= 8) stop("precision is limited to truncating numbers 7 digits after the decimal")
    number <- round(number, precision)
    decimal <- as.integer(sub(".*\\.", "", number))
    whole <- abs(as.integer(sub("(.*)\\..*", "\\1", number)))
    whole_sign <- sign(number)

    fraction <- .frac(decimal, den = NULL)

    if (isTRUE(improper)) {
      structure(list(whole = NULL,
                     numerator = (whole * fraction[[2]]) + fraction[[1]],
                     denominator = fraction[[2]],
                     sign = whole_sign),
                class = c("fraction", "improper", "list"))
    } else {
      structure(list(whole = whole,
                     numerator = fraction[[1]],
                     denominator = fraction[[2]],
                     sign = whole_sign),
                class = c("fraction", "proper", "list"))
    }
  }
}
NULL

#' Parse a String as a Fraction
#'
#' @param string The input character to be parsed.
#' @param improper Logical. Should the result be kept as an improper fraction?
#' Defaults to `TRUE`.
#' @param reduce Logical. Should unreduced fractions be simplified? Defaults
#' to `TRUE`.
#'
#' @return A formatted `list` printed with `print.fraction()`. The `list`
#' includes four elements:
#' * `whole`: The absolute value of the whole number part of the decimal. This
#' is `NULL` if `improper = TRUE`.
#' * `numerator`: The numerator of the resulting fraction.
#' * `denominator`: The denominator of the resulting fraction.
#' * `sign`: `-1` if the input is negative; `1` if the input is positive.
#'
#' @note The string can be entered either as an improper fraction
#' (for example, `"5/2"`) or as a proper fraction (for example,
#' `"2 1/2"`). Depending on how it is entered, the resulting `list`
#' will have a value in `"whole"` or `"whole"` will be `NULL`.
#'
#' @examples
#' parse_fraction("4/4")                   # "1"
#' parse_fraction("4/4", reduce = FALSE)   # "4/4"
#'
#' parse_fraction("32/4")                  # "8"
#' parse_fraction("34/4", reduce = FALSE)  # "34/4"
#' parse_fraction("34/4", reduce = TRUE)   # "17/2"
#' parse_fraction("34/4", improper = FALSE)# "8 1/2"
#'
#' parse_fraction("4 2/4")                 # "9/2"
#' parse_fraction("4 2/4", TRUE, FALSE)    # "18/4"
#' parse_fraction("4 2/4", FALSE)          # "4 1/2"
#' @export
parse_fraction <- function(string, improper = TRUE, reduce = TRUE) {
  if (!grepl("[ /]", string)) {
    cl <- "whole"
    whole <- abs(as.integer(string))
    numerator <- NULL
    denominator <- NULL
    whole_sign <- sign(as.integer(string))
  } else {
    a <- strsplit(string, "[ /]")[[1]]
    b <- as.integer(a)
    whole_sign <- sign(b[1])
    cl <- if (improper) "improper" else "proper"

    if (length(b) == 3) {
      denominator <- b[3]
      numerator <- if (improper) (abs(b[1]) * b[3]) + b[2] else b[2]
      whole <- if (improper) 0L else abs(b[1])
      if (reduce) {
        tmp <- .frac_reduce(whole, numerator, denominator, cl)
        numerator <- tmp[["numerator"]]
        denominator <- tmp[["denominator"]]
        whole <- tmp[["whole"]]
        cl <- tmp[["cl"]]
      }
    } else {
      denominator <- b[2]
      numerator <- abs(b[1])
      whole <- 0L
      if (improper) {
        if (reduce) {
          tmp <- .frac_reduce(whole, numerator, denominator, cl)
          numerator <- tmp[["numerator"]]
          denominator <- tmp[["denominator"]]
          whole <- tmp[["whole"]]
          cl <- tmp[["cl"]]
        }
      } else {
        if (numerator > denominator) {
          whole <- whole + (numerator %/% denominator)
          numerator <- numerator %% denominator
        } else if (numerator < denominator) {
          numerator <- numerator
        } else if (numerator == denominator ) {
          whole <- whole + 1
          numerator <- 0L
          denominator <- 0L
        }
        if (reduce) {
          tmp <- .frac_reduce(whole, numerator, denominator, cl)
          numerator <- tmp[["numerator"]]
          denominator <- tmp[["denominator"]]
          whole <- tmp[["whole"]]
          cl <- tmp[["cl"]]
        }
      }
    }
  }

  structure(list(whole = whole,
                 numerator = numerator,
                 denominator = denominator,
                 sign = whole_sign),
            class = c("fraction", cl, "list"))
}
NULL