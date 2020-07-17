#' Estimate Measures of Central Tendency for Already Grouped Data
#'
#' Estimates the mean, median, and mode of already grouped data
#' given the interval ranges and the frequencies of each group.
#'
#' @param frequencies A vector of frequencies.
#' @param intervals A 2-column `matrix` with the same number of
#' rows as the length of frequencies, with the first column being
#' the lower class boundary, and the second column being the upper
#' class boundary. Alternatively, `intervals` may be a character
#' vector, and you may specify `sep` (and possibly, `trim`) to
#' have the function automatically create the required `matrix`.
#' @param sep Optional character that separates lower and uppper
#' class boundaries if `intervals` is entered as a character vector.
#' @param trim Optional leading or trailing characters to trim from
#' the character vector being used for `intervals`. There is an
#' in-built pattern to trim the breakpoint labels created by
#' [base::cut()]. If you are using a `grouped_*` function on the
#' output of `cut` (where, for some reason, you no longer have
#' access to the original data), you can use `trim = "cut"`.
#' @param method A single value (1 or 2) determining which method
#' will be used to estimate the grouped mode. See the notes
#' section for the different approaches.
#' @return A single numeric value representing the grouped mean,
#' median, or mode, depending on which function was called.
#'
#' @details ## Calculation of Grouped Mean
#'
#' The following formula is used to calculate the grouped mean:
#'
#' \deqn{M = \frac{\sum f\times x}{n}}{M = (sum f * x)/n}
#'
#' Where:
#'
#' * f = The frequency of each class
#' * x = The width of each class
#' * n = The sum of the frequencies
#'
#' @details ## Calculation of Grouped Median
#'
#' The following forumla is used to calculate the grouped median:
#'
#' \deqn{M = L +\frac{\frac{n}{2}-cf}{f} \times c}{M = L + (n/2 - cf)/f * c}
#'
#' Where:
#'
#' * L = The lower boundary of the median class
#' * n = The sum of the frequencies
#' * cf = The cumulative frequency of the class below the median class
#' * f = The frequency of the median class
#' * c = The length of the median class
#'
#' @details ## Calculation of Grouped Mode
#'
#' The following formula is used to calculate the grouped mode if
#' `method = 1`:
#'
#' \deqn{M = L + \left ( \frac{f1-f0}{\left ( 2 \times f1 \right ) - f0 - f2} \right ) \times c}{Z = L + ((f1 - f0) / (2 * f1 - f0 - f2)) * c}
#'
#' Where:
#'
#' * L = The lower boundary of the mode class
#' * f1 = The frequency of the mode class
#' * f0 = The frequency of the class before the mode class
#' * f2 = The frequency of the class after the mode class
#' * c = The length of the mode class
#'
#' Keep in mind that while it might be easy to say which is the modal
#' group, the mode of the source data may not even be in that group.
#' Additionally, it is possible for data to have more than one mode
#' or conversely, no mode.
#'
#' The following formula is used to calculate the grouped mode if
#' `method = 2`:
#'
#' \deqn{M = (3 \times x) - (2 \times y)}{M = (3 * x) - (2 * y)}
#'
#' Where:
#'
#' * x = The group median
#' * y = The group mean
#'
#' @examples
#'
#' mydf <- structure(list(salary = c("1500-1600", "1600-1700", "1700-1800",
#'         "1800-1900", "1900-2000", "2000-2100", "2100-2200", "2200-2300",
#'         "2300-2400", "2400-2500"), number = c(110L, 180L, 320L, 460L,
#'         850L, 250L, 130L, 70L, 20L, 10L)), .Names = c("salary", "number"),
#'         class = "data.frame", row.names = c(NA, -10L))
#' mydf
#'
#' with(mydf, grouped_median(frequencies = number, intervals = salary, sep = "-"))
#'
#' ## Example with intervals manually specified
#' Freq <- mydf$number
#' X <- cbind(c(1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400),
#'            c(1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500))
#'
#' grouped_median(Freq, X)
#'
#' # Using `cut`
#' set.seed(1)
#' x <- sample(100, 100, replace = TRUE)
#' y <- data.frame(table(cut(x, 10)))
#'
#' with(y, grouped_mean(Freq, Var1, sep = ",", trim = "cut"))
#' mean(x)
#'
#' with(y, grouped_median(Freq, Var1, sep = ",", trim = "cut"))
#' median(x)
#'
#' ## Note that the mode might be really far off depending on the approach used
#' with(y, grouped_mode(Freq, Var1, sep = ",", trim = "cut"))
#' with(y, grouped_mode(Freq, Var1, sep = ",", trim = "cut", method = 2))
#' tail(sort(table(x)))
#'
#' @name grouped_functions
NULL

#' @rdname grouped_functions
#' @export
grouped_mean <- function(frequencies, intervals, sep = NULL, trim = NULL) {
  intervals <- if (!is.matrix(intervals)) .grp_intervals(intervals, sep, trim) else intervals
  sum(rowMeans(intervals) * frequencies) / sum(frequencies)
}
NULL

#' @rdname grouped_functions
#' @export
grouped_mode <- function(frequencies, intervals, sep = NULL, trim = NULL, method = 1) {
  method <- as.character(method)
  out <- switch(
    method,
    "1" = {
      intervals <- if (!is.matrix(intervals)) .grp_intervals(intervals, sep, trim) else intervals

      ind <- which.max(frequencies)
      if (length(ind) > 1L) stop("Only for use where there are no ties for highest frequencies across groups.")

      lw <- .grp_lw(intervals, ind)

      fm0 <- if (ind == 1) 0 else frequencies[(ind-1)]
      fm1 <- frequencies[ind]
      fm2 <- if (ind == length(frequencies)) 0 else frequencies[(ind+1)]

      lw[[1]] + ((fm1 - fm0) / (2*fm1 - fm0 - fm2)) * lw[[2]]
    },
    "2" = {
      (3*(grouped_median(frequencies = frequencies, intervals = intervals, sep = sep, trim = trim))) -
        (2*(grouped_mean(frequencies = frequencies, intervals = intervals, sep = sep, trim = trim)))
    },
    stop("method should be either '1' or '2'"))
  out
}
NULL

#' @rdname grouped_functions
#' @export
grouped_median <- function(frequencies, intervals, sep = NULL, trim = NULL) {
  intervals <- if (!is.matrix(intervals)) .grp_intervals(intervals, sep, trim) else intervals

  cf <- cumsum(frequencies)
  ind <- findInterval(max(cf)/2, cf) + 1
  lw <- .grp_lw(intervals, ind)
  f <- frequencies[ind]
  cf <- cf[(ind - 1)]
  n <- sum(frequencies)

  lw[[1]] + (n/2 - cf)/f * lw[[2]]
}
NULL
