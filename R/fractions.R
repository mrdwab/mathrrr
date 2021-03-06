#' Convert a Decimal to an Approximate Fraction
#'
#' The `as_fraction` function takes a numeric input and converts it to a fraction.
#' The input is first rounded to the desired level of precision (entered as a
#' value between 1 and 7).
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
#' * `sign`: `-1` if the input is negative; `1` if the input is positive; `0`
#' if the input is zero.
#' @examples
#' as_fraction(3.2454)
#' as_fraction(3.2454, 2, TRUE)
#' as_fraction(3.2454, 2, FALSE)
#' as_fraction(3.2454, 1, FALSE)
#' @export
as_fraction <- function(number, precision = 3, improper = TRUE) {
  if (.isInteger(number)) {
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

    out <- structure(list(
      whole = NULL,
      numerator = (whole * fraction[[2]]) + fraction[[1]],
      denominator = fraction[[2]],
      sign = whole_sign),
      class = c("fraction", "improper", "list"))

    if (improper) out else as_simplified(out)
  }
}
NULL

#' Convert a Fraction to a Decimal
#'
#' The `as_decimal` function parses a string and evaluates the result to
#' return a numeric approximation.
#'
#' @param fraction The input fraction, already parsed, or a string to
#' be parsed using `parse_fraction(fraction, improper = TRUE, reduce = TRUE)`.
#'
#' @return A numeric (possible approximation) of the fraction converted
#' to a numeric value.
#'
#' @examples
#' # String as input
#' as_decimal("2 3/8")
#'
#' # Parsed fraction as input
#' x <- as_improper("-1 1/3")
#' as_decimal(x)
#' @export
as_decimal <- function(fraction) {
  tmp <- if (is.character(fraction)) {
    tmp <- parse_fraction(string = fraction, improper = TRUE, reduce = TRUE)
  } else {
    as_improper(fraction)
  }
  tmp[["sign"]] * (tmp[["numerator"]]/tmp[["denominator"]])
}
NULL

#' Parse a String as a Fraction
#'
#' When provided a string representing a fraction, `parse_fraction` will parse
#' the provided string to create a `list` with the class `fraction` applied to
#' it. This will allow the parsed fraction to be used in subsequent calculations.
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
#' is `0` if `improper = TRUE`.
#' * `numerator`: The numerator of the resulting fraction.
#' * `denominator`: The denominator of the resulting fraction.
#' * `sign`: `-1` if the input is negative; `1` if the input is positive.
#'
#' @note The string can be entered either as an improper fraction
#' (for example, `"5/2"`) or as a simplified fraction (for example,
#' `"2 1/2"`). Depending on how it is entered, the resulting `list`
#' will have a value in `"whole"` or `"whole"` will be `NULL`.
#'
#' @examples
#' parse_fraction("4/4")
#' parse_fraction("4/4", reduce = FALSE)
#'
#' parse_fraction("32/4")
#' parse_fraction("34/4", reduce = FALSE)
#' parse_fraction("34/4", reduce = TRUE)
#' parse_fraction("34/4", improper = FALSE)
#'
#' parse_fraction("4 2/4")
#' parse_fraction("4 2/4", TRUE, FALSE)
#' parse_fraction("4 2/4", FALSE)
#' @export
parse_fraction <- function(string, improper = TRUE, reduce = TRUE) {
  string <- trimws(string)
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
    cl <- if (improper) "improper" else "simplified"

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

#' Converts a Parsed Fraction to an Improper Fraction
#'
#' Given a string or a fraction parsed using [parse_fraction()], this
#' function will check to see whether it is in its improper form, and
#' if not, it will convert it to an improper fraction.
#'
#' @param fraction The input fraction, already parsed, or a string to
#' be parsed using `parse_fraction(fraction, improper = TRUE, reduce = FALSE)`.
#'
#' @examples
#' frac <- "-3 1/2"
#' as_improper(frac)
#'
#' p_frac <- parse_fraction(frac, improper = FALSE)
#' as_improper(p_frac)
#'
#' as_improper("3")
#'
#' @export
as_improper <- function(fraction) {
  fraction <- if (is.character(fraction)) {
    parse_fraction(string = fraction, improper = TRUE, reduce = FALSE)
  } else {
    fraction
  }
  if (!"fraction" %in% class(fraction)) stop("
This function is to be used for fractions only")
  type <- intersect(class(fraction), c("whole", "simplified", "improper"))
  cl <- c("fraction", "improper", "list")
  out <- switch(
    type,
    whole = {
      structure(list(
        whole = 0, numerator = fraction[["whole"]],
        denominator = 1, sign = fraction[["sign"]]), class = cl)
    },
    simplified = {
      if (fraction[["whole"]] == 0) {
        `class<-`(fraction, cl)
      } else {
        structure(list(
          whole = 0,
          numerator = (fraction[["denominator"]] * fraction[["whole"]]) + fraction[["numerator"]],
          denominator = fraction[["denominator"]], sign = fraction[["sign"]]), class = cl)
      }
    },
    improper = fraction)
  out
}
NULL

#' Simplifies Parsed Fractions to Proper or Mixed Fraction Form
#'
#' Given a string or a fraction parsed using [parse_fraction()], this
#' function will check to see whether it is in its simplified form,
#' and if not, it will convert it to a proper or mixed fraction, depending
#' on whether the parsed number has a whole number component.
#'
#' @param fraction The input fraction, already parsed, or a string to
#' be parsed using `parse_fraction(fraction, improper = FALSE, reduce = TRUE)`.
#'
#' @examples
#' as_simplified("17/2")
#'
#' @export
as_simplified <- function(fraction) {
  fraction <- if (is.character(fraction)) {
    parse_fraction(string = fraction, improper = TRUE, reduce = FALSE)
  } else {
    fraction
  }
  if (!"fraction" %in% class(fraction)) stop("
This function is to be used for fractions only")
  type <- intersect(class(fraction), c("whole", "simplified", "improper"))

  pf <- function(fraction) {
    denominator <- fraction[["denominator"]]
    numerator <- fraction[["numerator"]]
    whole <- 0L
    whole_sign <- fraction[["sign"]]
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
    tmp <- .frac_reduce(whole, numerator, denominator, "simplified")
    numerator <- tmp[["numerator"]]
    denominator <- tmp[["denominator"]]
    whole <- tmp[["whole"]]
    cl <- tmp[["cl"]]
    structure(list(whole = whole,
                   numerator = numerator,
                   denominator = denominator,
                   sign = whole_sign),
              class = c("fraction", cl, "list"))
  }

  out <- switch(
    type,
    whole = fraction,
    simplified = fraction,
    improper = pf(fraction))
  out
}
NULL



