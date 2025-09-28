library(testthat)
library(k3k2)


x <- c(1, 2, 70, 3, 4)
obj <- k3k2(list(x))

test_that("negative inputs are rejected", {
  expect_error(k3k2(c(1,2,-3)), "All input values must be positive for log")
  expect_error(k3k2(c(NA, 4, 5)), "samples must not contain NA values")
})
