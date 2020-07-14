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

.isInteger <- function(x) all(as.numeric(x) == as.integer(x))

.reciprocal <- function(x) {
  x[c("numerator", "denominator")] <- x[c("denominator", "numerator")]
  x
}

#' @export
print.fraction <- function(x, ...) {
  cl <- intersect(class(x), c("improper", "proper", "whole"))
  out <- switch(
    cl,
    improper = sprintf("%s/%s", format(x[["numerator"]] * x[["sign"]], scientific = FALSE),
                       format(x[["denominator"]], scientific = FALSE)),
    proper = if (x[["whole"]] == 0) {
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
