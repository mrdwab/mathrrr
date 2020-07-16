#' Calculate the Median of Already Grouped Data
#'
#' Calculates the median of already grouped data given the interval ranges and
#' the frequencies of each group.
#'
#' @param frequencies A vector of frequencies.
#' @param intervals A 2-column `matrix` with the same number of rows as
#' the length of frequencies, with the first column being the lower class
#' boundary, and the second row being the upper class boundary. Alternatively,
#' `intervals` may be a character vector, and you may specify `sep` (and
#' possibly, `trim` function automatically create the required `matrix`.
#' @param sep Optional character that separates lower and uppper class
#' boundaries if `intervals` is entered as a character vector.
#' @param trim Optional leading or trailing characters to trim from the
#' character vector being used for `intervals`. There is one in-built pattern
#' in `grouped_median` to trim the breakpoint labels created by [base::cut()].
#' If you are using `grouped_median` on the output of `cut` (where, for some
#' reason, you no longer have access to the original data), you can use
#' `trim = "cut"`.
#' @return A single numeric value representing the grouped median.
#' @note
#' The following forumla is used to calculate the grouped median:
#'
#' \deqn{M = L +\frac{\frac{n}{2}-cf}{f} \times c}{M = L + (n/2 - cf)/f * c}
#'
#' * L = The lower boundary of the median class
#' * n = The sum of the frequencies
#' * cf = The cumulative frequency of the class below the median class
#' * f = The frequency of the median class
#' * c = The length of the median class
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
#' with(y, grouped_median(Freq, Var1, sep = ",", trim = "cut"))
#'
#' @export grouped_median
grouped_median <- function(frequencies, intervals, sep = NULL, trim = NULL) {
  if (!is.null(sep)) {
    if (is.null(trim)) pattern <- ""
    else if (trim == "cut") pattern <- "\\[|\\]|\\(|\\)"
    else pattern <- trim
    intervals <- matrix(
      as.numeric(unlist(strsplit(gsub(pattern, "", intervals), sep), use.names = FALSE)),
      ncol = 2, byrow = TRUE)
  }

  cf <- cumsum(frequencies)
  Midrow <- findInterval(max(cf)/2, cf) + 1
  L <- intervals[Midrow, 1]
  h <- diff(intervals[Midrow, ])
  f <- frequencies[Midrow]
  cf2 <- cf[Midrow - 1]
  n_2 <- max(cf)/2

  unname(L + (n_2 - cf2)/f * h)
}
