library(testthat)
library(k3k2)


x1 <- c(1,2,3)
x2 <- c(2,3,4)
obj <- k3k2(list(x1, x2))


test_that("k3k2 object structure", {
  expect_s3_class(obj, "k3k2")
  expect_length(obj, 2)
  expect_true(all(vapply(obj, function(el) all(c("data","kappa2","kappa3") %in% names(el)), logical(1))))
})

test_that("k3k2 rejects invalid input", {
  x_bad <- c(-1, 2, 3)
  expect_error(k3k2(x_bad), "All input values must be positive for log")

  x_na <- c(1, NA, 3)
  expect_error(k3k2(x_na), "samples must not contain NA values")
})
