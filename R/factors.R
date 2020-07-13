#' All Factors of a Number
#'
#' The `factors_of` function returns a numeric vector of the factors of a
#' provided number.
#'
#' @param x The number that you want to find the factors of.
#'
#' @return A numeric vector.
#' @examples
#' factors_of(8)
#' @export
factors_of <- function(x) which(!x %% seq_len(x))
NULL

#' Common Factors of Multiple Numbers
#'
#' The `common_factors` function takes a set of numbers and identifies
#' the common factors between them. It can also be used to identify the
#' greatest common factor of a set of numbers.
#'
#' @param \dots The numbers that you want to get the common factors of.
#' @param greatest Logical. Should the result be only the greatest
#' common factor? Defaults to `FALSE`.
#'
#' @return A numeric vector.
#' @examples
#' common_factors(18, 48)
#' common_factors(25, 50, 100, greatest = TRUE)
#' @export
common_factors <- function(..., greatest = FALSE) {
  out <- Reduce(intersect, lapply(
    unlist(list(...), use.names = FALSE), factors_of))
  if (isTRUE(greatest)) max(out) else out
}
NULL

#' Prime Factors of a Number
#'
#' The `prime_factors` function is used to calculate the prime factors of
#' a positive integer greater than 1.
#'
#' @param x The number that you want the prime factors of.
#' @param unique Logical. Should the function return all prime factors
#' (where `prod(prime_factors(x)) == x`) or just the unique prime factors?
#' Defaults to `TRUE`.
#'
#' @return A numeric vector either with the repeated prime factorization
#' of the input or just the unique prime factors of the input.
#'
#' @note Returns `NULL` for a value of 1, and generates an error for
#' values less than 1.
#'
#' @examples
#' prime_factors(100, unique = FALSE)
#' prime_factors(100)
#' @export
prime_factors <- function(x, unique = TRUE) {
  if (x < 1) stop("
This function is to be used on positive integers greater than 1")
  if (x %in% primes) {
    facs <- x
  } else {
    facs <- c()
    i <- 2
    rem <- x
    while (prod(facs) != x) {
      if (!rem %% i) {
        facs <- c(facs, i)
        rem <- rem/i
        i <- 1
      }
      i <- i + 1
    }
  }
  if (isTRUE(unique)) unique(facs) else facs
}
NULL

#' Least Common Multiple of a Set of Numbers
#'
#' The `least_common_multiple` function takes a set of numbers and
#' calculates their least common multiple using the prime factorization
#' method.
#'
#' @param \dots The numbers for which you want the least common multiple.
#'
#' @return A single integer value representing the least common multiple
#' of the set of inputs.
#'
#' @note The absolute values of the input is used in calculating the
#' least common multiple.
#'
#' @examples
#' least_common_multiple(4, 7, 11)
#' @export
least_common_multiple <- function(...) {
  L <- list(...)
  l <- sort(abs(unique(unlist(L, use.names = FALSE))))
  if (!.isInteger(l)) stop("
This function is only defined to be used on integer values")
  if (any(l == 0)) {
    0
  } else if (identical(l, 1)) {
    1
  } else {
    l <- l[l != 1]
    if (all(!max(l) %% l)) {
      max(l)
    } else {
      out <- lapply(l, prime_factors, unique = FALSE)
      out <- unique(do.call(rbind, lapply(
        out, function(y) data.frame(unclass(rle(y))))))
      out <- out[as.logical(with(
        out, ave(lengths, values, FUN = function(x) x == max(x)))), ]
      prod(do.call("^", rev(out)))
    }
  }
}
NULL
