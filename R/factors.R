#' All Factors of a Number
#'
#' @param x The number that you want to find the factors of.
#' @examples
#' factors_of(8)
#' @export
factors_of <- function(x) which(!x %% seq_len(x))
NULL

#' Common Factors of Multiple Numbers
#'
#' @param \dots The numbers that you want to get the common factors of.
#' @param greatest Logical. Should the result be only the greatest common factor? Defaults to `FALSE`.
#' @examples
#' common_factors(18, 48)
#' common_factors(25, 50, 100)
#' @export
common_factors <- function(..., greatest = FALSE) {
  out <- Reduce(intersect, lapply(
    unlist(list(...), use.names = FALSE), factors_of))
  if (isTRUE(greatest)) max(out) else out
}
NULL

#' Prime Factors of a Number
#'
#' @param x The number that you want the prime factors of.
#' @param unique Logical. Should the function return all prime factors
#' (where `prod(prime_factors(x)) == x`) or just the unique prime factors?
#' Defaults to `TRUE`.
#'
#' @examples
#' prime_factors(100, unique = FALSE)
#' prime_factors(100)
#' @export
prime_factors <- function(x, unique = TRUE) {
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
#' @param \dots The numbers for which you want the least common multiple.
#'
#' @examples
#' least_common_multiple(4, 7, 11)
#' @export
least_common_multiple <- function(...) {
  L <- list(...)
  l <- sort(unlist(L, use.names = FALSE))
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
NULL
