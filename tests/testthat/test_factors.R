library(mathrrr)
context("factors functions")

test_that("correct results are returned", {
  expect_equal(factors_of(8), c(1, 2, 4, 8))
})
